!=====================================================================
!
! MARCHINGCUBE:
!
! For generating triangulated iso-surfaces using the "marching cubes"
! algorithm. This represents a single marching cube. The data
! statements below contain details of the algorithm.
!
! This version can also cope to a limited extent with non-cubic
! marching cubes. In this case the gradient and hessian information is
! not worked out.
!
! CREDIT: This is a modified version of "conscript.f" written by:
!
! (C) Mike Lawrence February 2000
!     Biomolecular Research Institute
!     343 Royal Parade
!     Parkville 3052
!     Victoria
!     Australia
!
! (C) Stephen K Wolff, Dylan Jayatilaka, 2002
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
module MARCHINGCUBE_MODULE

#  include "macros"

   include "marchingcube.use"

   implicit none

   include "marchingcube.int"


!  ===============
!  Data Statements
!  ===============

!  Every cube has 6 faces each face has 4 vertices v
!  z  y
!  ^ /
!  |/
!  +---> x
!              f2(+z)    f6(+y)
!           +--|-----+  /
!          /|  V    /|L
!         / |      / |
!        +--------+  | <-f4
!        |  |_____|_ +   (+x)
! f3->   |  /f5   | /
! (x=0)  | /      |/
!        +--------+
!            ^
!            |
!            f1 (z=0)
!
   MAT(INT,1:4,1:6), private :: vertices_of_face
!                                f1 (z=0)   f2(+z)  f3(x=0)   f4(+x)  f5(y=0)  f6(+y)
   data vertices_of_face(1:4,1:6)/3,2,1,0, 4,5,6,7, 3,0,4,7, 1,2,6,5, 0,1,5,4, 2,3,7,6/
   MAT(INT,1:4,1:6), private :: edges_of_face
!                              f1 (z=0)   f2(+z)  f3(x=0)   f4(+x)  f5(y=0)  f6(+y)
   data edges_of_face(1:4,1:6)/2,1,0,3, 4,5,6,7, 3,8,7,11, 1,10,5,9, 0,9,4,8, 2,11,6,10/

!  Every cube has 8 vertices. "cube" lists the eight vertices of a cube in the
!  marching cubes standard order. The vertex indices are in the range 0:7
   MAT(INT,1:3,0:7), private :: cube
!                        v0     v1     v2     v3     v4     v5     v6     v7
   data cube(1:3,0:7)/0,0,0, 1,0,0, 1,1,0, 0,1,0, 0,0,1, 1,0,1, 1,1,1, 0,1,1/

!  Tube is a transposed cube, for efficiency
   MAT(INT,0:7,1:3), private :: tube
   data tube(0:7,1:3)/0,1,1,0,0,1,1,0, 0,0,1,1,0,0,1,1, 0,0,0,0,1,1,1,1/

!  There are 12 cube edges which can be defined in a standard order.
!  "vertex_1_of_edge(e)" gives the 1-st vertex index of the e-th cube edge.
!  "vertex_2_of_edge(e)" gives the 2-nd vertex index of the e-th cube edge.
!  The edge indices are in the range 0:11.
   VEC(INT,0:11), private :: vertex_1_of_edge
   VEC(INT,0:11), private :: vertex_2_of_edge
!                                   e0  e1  e2  e3  e4  e5  e6  e7  e8  e9 e10 e11
   data vertex_1_of_edge(0:11)/      0,  1,  2,  3,  4,  5,  6,  7,  0,  1,  2,  3/
   data vertex_2_of_edge(0:11)/      1,  2,  3,  0,  5,  6,  7,  4,  4,  5,  6,  7/

!  The "xyz_axis_of_edge(e)" array defines which axis (1-3 or x-z) is changing
!  for every edge "e" for the 12 standard cube edges.
   VEC(INT,0:11), private :: xyz_axis_of_edge

!                                   e0  e1  e2  e3  e4  e5  e6  e7  e8  e9 e10 e11
   data xyz_axis_of_edge(0:11)/      1,  2,  1,  2,  1,  2,  1,  2,  3,  3,  3,  3/

!  The following data is the heart of the module. In the marching
!  cubes algorithm, every cube vertex is classified either less than
!  or greater than the iso value for the isosurface. Since there are 8
!  cube vertices, there are only 256 possibile cases that can occur.
!  Each case can be encoded as a bit string. For example, if vertex 0
!  and 7 are ON (i.e. LESS than the iso value of the isosurface) and
!  all other vertex are OFF (i.e. GREATER than the iso value for the
!  isosurface), then the case # is: 010000001 = 129.  For each case, a
!  number of triangles may be drawn which approximates the isosurface
!  through the cube. The "edge_table" maps each case # to another bit
!  string which describes the *edges* to be used to construct each
!  triangle.  The midpoints along the edges (or some other
!  interpolated value along the edge) are used to make each vertex of
!  the triangle. For example, for case #129, only two triangle are
!  drawn, involving edges 3, 0, 8, 11, 7 and 6 (as described below for
!  traingle_table(:,129)). The bits string encoding these edges is:
!  000100111001001 = 9(12)9 = 9c9 base 16.  i.e. edge_table(129) =
!  "9c9", as shown below.  Note that the opposite case # 101111110
!  also involves the same triangles, thus the edge_table is symmetric
!  in two halves. However, the edge bit string should not be reversed,
!  so that ON stands for GREATER than the iso value, because otherwise
!  the the arrays vertex_1_of_edge and vertex_2_of_edge would have to
!  be swapped to be consistent with edge_table, in order that the sign
!  of the function gradients are correctly calculated.
   VEC(INT,0:255), private :: edge_table
   data edge_table(  0:127)/ &
      Z'0'  , Z'109', Z'203', Z'30a', Z'406', Z'50f', Z'605', Z'70c', &
      Z'80c', Z'905', Z'a0f', Z'b06', Z'c0a', Z'd03', Z'e09', Z'f00', &
      Z'190', Z'99' , Z'393', Z'29a', Z'596', Z'49f', Z'795', Z'69c', &
      Z'99c', Z'895', Z'b9f', Z'a96', Z'd9a', Z'c93', Z'f99', Z'e90', &
      Z'230', Z'339', Z'33' , Z'13a', Z'636', Z'73f', Z'435', Z'53c', &
      Z'a3c', Z'b35', Z'83f', Z'936', Z'e3a', Z'f33', Z'c39', Z'd30', &
      Z'3a0', Z'2a9', Z'1a3', Z'aa' , Z'7a6', Z'6af', Z'5a5', Z'4ac', &
      Z'bac', Z'aa5', Z'9af', Z'8a6', Z'faa', Z'ea3', Z'da9', Z'ca0', &
      Z'460', Z'569', Z'663', Z'76a', Z'66' , Z'16f', Z'265', Z'36c', &
      Z'c6c', Z'd65', Z'e6f', Z'f66', Z'86a', Z'963', Z'a69', Z'b60', &
      Z'5f0', Z'4f9', Z'7f3', Z'6fa', Z'1f6', Z'ff' , Z'3f5', Z'2fc', &
      Z'dfc', Z'cf5', Z'fff', Z'ef6', Z'9fa', Z'8f3', Z'bf9', Z'af0', &
      Z'650', Z'759', Z'453', Z'55a', Z'256', Z'35f', Z'55' , Z'15c', &
      Z'e5c', Z'f55', Z'c5f', Z'd56', Z'a5a', Z'b53', Z'859', Z'950', &
      Z'7c0', Z'6c9', Z'5c3', Z'4ca', Z'3c6', Z'2cf', Z'1c5', Z'cc' , &
      Z'fcc', Z'ec5', Z'dcf', Z'cc6', Z'bca', Z'ac3', Z'9c9', Z'8c0'/

!  The bottom half is a reflection of the top half
   data edge_table(128:255)/ &
      Z'8c0', Z'9c9', Z'ac3', Z'bca', Z'cc6', Z'dcf', Z'ec5', Z'fcc', &
      Z'cc' , Z'1c5', Z'2cf', Z'3c6', Z'4ca', Z'5c3', Z'6c9', Z'7c0', &
      Z'950', Z'859', Z'b53', Z'a5a', Z'd56', Z'c5f', Z'f55', Z'e5c', &
      Z'15c', Z'55' , Z'35f', Z'256', Z'55a', Z'453', Z'759', Z'650', &
      Z'af0', Z'bf9', Z'8f3', Z'9fa', Z'ef6', Z'fff', Z'cf5', Z'dfc', &
      Z'2fc', Z'3f5', Z'ff' , Z'1f6', Z'6fa', Z'7f3', Z'4f9', Z'5f0', &
      Z'b60', Z'a69', Z'963', Z'86a', Z'f66', Z'e6f', Z'd65', Z'c6c', &
      Z'36c', Z'265', Z'16f', Z'66' , Z'76a', Z'663', Z'569', Z'460', &
      Z'ca0', Z'da9', Z'ea3', Z'faa', Z'8a6', Z'9af', Z'aa5', Z'bac', &
      Z'4ac', Z'5a5', Z'6af', Z'7a6', Z'aa' , Z'1a3', Z'2a9', Z'3a0', &
      Z'd30', Z'c39', Z'f33', Z'e3a', Z'936', Z'83f', Z'b35', Z'a3c', &
      Z'53c', Z'435', Z'73f', Z'636', Z'13a', Z'33' , Z'339', Z'230', &
      Z'e90', Z'f99', Z'c93', Z'd9a', Z'a96', Z'b9f', Z'895', Z'99c', &
      Z'69c', Z'795', Z'49f', Z'596', Z'29a', Z'393', Z'99' , Z'190', &
      Z'f00', Z'e09', Z'd03', Z'c0a', Z'b06', Z'a0f', Z'905', Z'80c', &
      Z'70c', Z'605', Z'50f', Z'406', Z'30a', Z'203', Z'109', Z'0'/

!  The "triangle_table(n)" lists the edges used to form each triangle,
!  for the n-th possible case number. They come in groups of three,
!  and -1 is the null value indicating no edge, e.g. the fourth data
!  item below is:

!  This contains two triangles.  The first is formed from points on
!  edges 1, 8, 3, while the second is formed from points on edges 9,
!  8, 1.
   MAT(INT,0:15,0:255), private :: triangle_table
   data triangle_table(0:15,  0: 24)/                  &
      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 8, 3, 9, 8, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 1, 2,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 2,10, 0, 2, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 8, 3, 2,10, 8,10, 9, 8,-1,-1,-1,-1,-1,-1,-1, &
       3,11, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0,11, 2, 8,11, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 9, 0, 2, 3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1,11, 2, 1, 9,11, 9, 8,11,-1,-1,-1,-1,-1,-1,-1, &
       3,10, 1,11,10, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0,10, 1, 0, 8,10, 8,11,10,-1,-1,-1,-1,-1,-1,-1, &
       3, 9, 0, 3,11, 9,11,10, 9,-1,-1,-1,-1,-1,-1,-1, &
       9, 8,10,10, 8,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 7, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 3, 0, 7, 3, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 1, 9, 4, 7, 1, 7, 3, 1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 4, 7, 3, 0, 4, 1, 2,10,-1,-1,-1,-1,-1,-1,-1, &
       9, 2,10, 9, 0, 2, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1, &
       2,10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4,-1,-1,-1,-1, &
       8, 4, 7, 3,11, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15, 25: 49)/                  &
      11, 4, 7,11, 2, 4, 2, 0, 4,-1,-1,-1,-1,-1,-1,-1, &
       9, 0, 1, 8, 4, 7, 2, 3,11,-1,-1,-1,-1,-1,-1,-1, &
       4, 7,11, 9, 4,11, 9,11, 2, 9, 2, 1,-1,-1,-1,-1, &
       3,10, 1, 3,11,10, 7, 8, 4,-1,-1,-1,-1,-1,-1,-1, &
       1,11,10, 1, 4,11, 1, 0, 4, 7,11, 4,-1,-1,-1,-1, &
       4, 7, 8, 9, 0,11, 9,11,10,11, 0, 3,-1,-1,-1,-1, &
       4, 7,11, 4,11, 9, 9,11,10,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 4, 0, 8, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 5, 4, 1, 5, 0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 5, 4, 8, 3, 5, 3, 1, 5,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 9, 5, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 8, 1, 2,10, 4, 9, 5,-1,-1,-1,-1,-1,-1,-1, &
       5, 2,10, 5, 4, 2, 4, 0, 2,-1,-1,-1,-1,-1,-1,-1, &
       2,10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8,-1,-1,-1,-1, &
       9, 5, 4, 2, 3,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0,11, 2, 0, 8,11, 4, 9, 5,-1,-1,-1,-1,-1,-1,-1, &
       0, 5, 4, 0, 1, 5, 2, 3,11,-1,-1,-1,-1,-1,-1,-1, &
       2, 1, 5, 2, 5, 8, 2, 8,11, 4, 8, 5,-1,-1,-1,-1, &
      10, 3,11,10, 1, 3, 9, 5, 4,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 5, 0, 8, 1, 8,10, 1, 8,11,10,-1,-1,-1,-1, &
       5, 4, 0, 5, 0,11, 5,11,10,11, 0, 3,-1,-1,-1,-1, &
       5, 4, 8, 5, 8,10,10, 8,11,-1,-1,-1,-1,-1,-1,-1, &
       9, 7, 8, 5, 7, 9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 3, 0, 9, 5, 3, 5, 7, 3,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15, 50: 74)/                  &
       0, 7, 8, 0, 1, 7, 1, 5, 7,-1,-1,-1,-1,-1,-1,-1, &
       1, 5, 3, 3, 5, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 7, 8, 9, 5, 7,10, 1, 2,-1,-1,-1,-1,-1,-1,-1, &
      10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3,-1,-1,-1,-1, &
       8, 0, 2, 8, 2, 5, 8, 5, 7,10, 5, 2,-1,-1,-1,-1, &
       2,10, 5, 2, 5, 3, 3, 5, 7,-1,-1,-1,-1,-1,-1,-1, &
       7, 9, 5, 7, 8, 9, 3,11, 2,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7,11,-1,-1,-1,-1, &
       2, 3,11, 0, 1, 8, 1, 7, 8, 1, 5, 7,-1,-1,-1,-1, &
      11, 2, 1,11, 1, 7, 7, 1, 5,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 8, 8, 5, 7,10, 1, 3,10, 3,11,-1,-1,-1,-1, &
       5, 7, 0, 5, 0, 9, 7,11, 0, 1, 0,10,11,10, 0,-1, &
      11,10, 0,11, 0, 3,10, 5, 0, 8, 0, 7, 5, 7, 0,-1, &
      11,10, 5, 7,11, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      10, 6, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 5,10, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9, 0, 1, 5,10, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 8, 3, 1, 9, 8, 5,10, 6,-1,-1,-1,-1,-1,-1,-1, &
       1, 6, 5, 2, 6, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 6, 5, 1, 2, 6, 3, 0, 8,-1,-1,-1,-1,-1,-1,-1, &
       9, 6, 5, 9, 0, 6, 0, 2, 6,-1,-1,-1,-1,-1,-1,-1, &
       5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8,-1,-1,-1,-1, &
       2, 3,11,10, 6, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      11, 0, 8,11, 2, 0,10, 6, 5,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9, 2, 3,11, 5,10, 6,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15, 75: 99)/                  &
       5,10, 6, 1, 9, 2, 9,11, 2, 9, 8,11,-1,-1,-1,-1, &
       6, 3,11, 6, 5, 3, 5, 1, 3,-1,-1,-1,-1,-1,-1,-1, &
       0, 8,11, 0,11, 5, 0, 5, 1, 5,11, 6,-1,-1,-1,-1, &
       3,11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9,-1,-1,-1,-1, &
       6, 5, 9, 6, 9,11,11, 9, 8,-1,-1,-1,-1,-1,-1,-1, &
       5,10, 6, 4, 7, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 3, 0, 4, 7, 3, 6, 5,10,-1,-1,-1,-1,-1,-1,-1, &
       1, 9, 0, 5,10, 6, 8, 4, 7,-1,-1,-1,-1,-1,-1,-1, &
      10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4,-1,-1,-1,-1, &
       6, 1, 2, 6, 5, 1, 4, 7, 8,-1,-1,-1,-1,-1,-1,-1, &
       1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7,-1,-1,-1,-1, &
       8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6,-1,-1,-1,-1, &
       7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9,-1, &
       3,11, 2, 7, 8, 4,10, 6, 5,-1,-1,-1,-1,-1,-1,-1, &
       5,10, 6, 4, 7, 2, 4, 2, 0, 2, 7,11,-1,-1,-1,-1, &
       0, 1, 9, 4, 7, 8, 2, 3,11, 5,10, 6,-1,-1,-1,-1, &
       9, 2, 1, 9,11, 2, 9, 4,11, 7,11, 4, 5,10, 6,-1, &
       8, 4, 7, 3,11, 5, 3, 5, 1, 5,11, 6,-1,-1,-1,-1, &
       5, 1,11, 5,11, 6, 1, 0,11, 7,11, 4, 0, 4,11,-1, &
       0, 5, 9, 0, 6, 5, 0, 3, 6,11, 6, 3, 8, 4, 7,-1, &
       6, 5, 9, 6, 9,11, 4, 7, 9, 7,11, 9,-1,-1,-1,-1, &
      10, 4, 9, 6, 4,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4,10, 6, 4, 9,10, 0, 8, 3,-1,-1,-1,-1,-1,-1,-1, &
      10, 0, 1,10, 6, 0, 6, 4, 0,-1,-1,-1,-1,-1,-1,-1, &
       8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1,10,-1,-1,-1,-1/
   data triangle_table(0:15,100:124)/                  &
       1, 4, 9, 1, 2, 4, 2, 6, 4,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4,-1,-1,-1,-1, &
       0, 2, 4, 4, 2, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 3, 2, 8, 2, 4, 4, 2, 6,-1,-1,-1,-1,-1,-1,-1, &
      10, 4, 9,10, 6, 4,11, 2, 3,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 2, 2, 8,11, 4, 9,10, 4,10, 6,-1,-1,-1,-1, &
       3,11, 2, 0, 1, 6, 0, 6, 4, 6, 1,10,-1,-1,-1,-1, &
       6, 4, 1, 6, 1,10, 4, 8, 1, 2, 1,11, 8,11, 1,-1, &
       9, 6, 4, 9, 3, 6, 9, 1, 3,11, 6, 3,-1,-1,-1,-1, &
       8,11, 1, 8, 1, 0,11, 6, 1, 9, 1, 4, 6, 4, 1,-1, &
       3,11, 6, 3, 6, 0, 0, 6, 4,-1,-1,-1,-1,-1,-1,-1, &
       6, 4, 8,11, 6, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7,10, 6, 7, 8,10, 8, 9,10,-1,-1,-1,-1,-1,-1,-1, &
       0, 7, 3, 0,10, 7, 0, 9,10, 6, 7,10,-1,-1,-1,-1, &
      10, 6, 7, 1,10, 7, 1, 7, 8, 1, 8, 0,-1,-1,-1,-1, &
      10, 6, 7,10, 7, 1, 1, 7, 3,-1,-1,-1,-1,-1,-1,-1, &
       1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7,-1,-1,-1,-1, &
       2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9,-1, &
       7, 8, 0, 7, 0, 6, 6, 0, 2,-1,-1,-1,-1,-1,-1,-1, &
       7, 3, 2, 6, 7, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 3,11,10, 6, 8,10, 8, 9, 8, 6, 7,-1,-1,-1,-1, &
       2, 0, 7, 2, 7,11, 0, 9, 7, 6, 7,10, 9,10, 7,-1, &
       1, 8, 0, 1, 7, 8, 1,10, 7, 6, 7,10, 2, 3,11,-1, &
      11, 2, 1,11, 1, 7,10, 6, 1, 6, 7, 1,-1,-1,-1,-1, &
       8, 9, 6, 8, 6, 7, 9, 1, 6,11, 6, 3, 1, 3, 6,-1/
   data triangle_table(0:15,125:149)/                  &
       0, 9, 1,11, 6, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7, 8, 0, 7, 0, 6, 3,11, 0,11, 6, 0,-1,-1,-1,-1, &
       7,11, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7, 6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 8,11, 7, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 1, 9,11, 7, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 1, 9, 8, 3, 1,11, 7, 6,-1,-1,-1,-1,-1,-1,-1, &
      10, 1, 2, 6,11, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 3, 0, 8, 6,11, 7,-1,-1,-1,-1,-1,-1,-1, &
       2, 9, 0, 2,10, 9, 6,11, 7,-1,-1,-1,-1,-1,-1,-1, &
       6,11, 7, 2,10, 3,10, 8, 3,10, 9, 8,-1,-1,-1,-1, &
       7, 2, 3, 6, 2, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       7, 0, 8, 7, 6, 0, 6, 2, 0,-1,-1,-1,-1,-1,-1,-1, &
       2, 7, 6, 2, 3, 7, 0, 1, 9,-1,-1,-1,-1,-1,-1,-1, &
       1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6,-1,-1,-1,-1, &
      10, 7, 6,10, 1, 7, 1, 3, 7,-1,-1,-1,-1,-1,-1,-1, &
      10, 7, 6, 1, 7,10, 1, 8, 7, 1, 0, 8,-1,-1,-1,-1, &
       0, 3, 7, 0, 7,10, 0,10, 9, 6,10, 7,-1,-1,-1,-1, &
       7, 6,10, 7,10, 8, 8,10, 9,-1,-1,-1,-1,-1,-1,-1, &
       6, 8, 4,11, 8, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 6,11, 3, 0, 6, 0, 4, 6,-1,-1,-1,-1,-1,-1,-1, &
       8, 6,11, 8, 4, 6, 9, 0, 1,-1,-1,-1,-1,-1,-1,-1, &
       9, 4, 6, 9, 6, 3, 9, 3, 1,11, 3, 6,-1,-1,-1,-1, &
       6, 8, 4, 6,11, 8, 2,10, 1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 3, 0,11, 0, 6,11, 0, 4, 6,-1,-1,-1,-1/
   data triangle_table(0:15,150:174)/                  &
       4,11, 8, 4, 6,11, 0, 2, 9, 2,10, 9,-1,-1,-1,-1, &
      10, 9, 3,10, 3, 2, 9, 4, 3,11, 3, 6, 4, 6, 3,-1, &
       8, 2, 3, 8, 4, 2, 4, 6, 2,-1,-1,-1,-1,-1,-1,-1, &
       0, 4, 2, 4, 6, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8,-1,-1,-1,-1, &
       1, 9, 4, 1, 4, 2, 2, 4, 6,-1,-1,-1,-1,-1,-1,-1, &
       8, 1, 3, 8, 6, 1, 8, 4, 6, 6,10, 1,-1,-1,-1,-1, &
      10, 1, 0,10, 0, 6, 6, 0, 4,-1,-1,-1,-1,-1,-1,-1, &
       4, 6, 3, 4, 3, 8, 6,10, 3, 0, 3, 9,10, 9, 3,-1, &
      10, 9, 4, 6,10, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 5, 7, 6,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 4, 9, 5,11, 7, 6,-1,-1,-1,-1,-1,-1,-1, &
       5, 0, 1, 5, 4, 0, 7, 6,11,-1,-1,-1,-1,-1,-1,-1, &
      11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5,-1,-1,-1,-1, &
       9, 5, 4,10, 1, 2, 7, 6,11,-1,-1,-1,-1,-1,-1,-1, &
       6,11, 7, 1, 2,10, 0, 8, 3, 4, 9, 5,-1,-1,-1,-1, &
       7, 6,11, 5, 4,10, 4, 2,10, 4, 0, 2,-1,-1,-1,-1, &
       3, 4, 8, 3, 5, 4, 3, 2, 5,10, 5, 2,11, 7, 6,-1, &
       7, 2, 3, 7, 6, 2, 5, 4, 9,-1,-1,-1,-1,-1,-1,-1, &
       9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7,-1,-1,-1,-1, &
       3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0,-1,-1,-1,-1, &
       6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8,-1, &
       9, 5, 4,10, 1, 6, 1, 7, 6, 1, 3, 7,-1,-1,-1,-1, &
       1, 6,10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4,-1, &
       4, 0,10, 4,10, 5, 0, 3,10, 6,10, 7, 3, 7,10,-1/
   data triangle_table(0:15,175:199)/                  &
       7, 6,10, 7,10, 8, 5, 4,10, 4, 8,10,-1,-1,-1,-1, &
       6, 9, 5, 6,11, 9,11, 8, 9,-1,-1,-1,-1,-1,-1,-1, &
       3, 6,11, 0, 6, 3, 0, 5, 6, 0, 9, 5,-1,-1,-1,-1, &
       0,11, 8, 0, 5,11, 0, 1, 5, 5, 6,11,-1,-1,-1,-1, &
       6,11, 3, 6, 3, 5, 5, 3, 1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,10, 9, 5,11, 9,11, 8,11, 5, 6,-1,-1,-1,-1, &
       0,11, 3, 0, 6,11, 0, 9, 6, 5, 6, 9, 1, 2,10,-1, &
      11, 8, 5,11, 5, 6, 8, 0, 5,10, 5, 2, 0, 2, 5,-1, &
       6,11, 3, 6, 3, 5, 2,10, 3,10, 5, 3,-1,-1,-1,-1, &
       5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2,-1,-1,-1,-1, &
       9, 5, 6, 9, 6, 0, 0, 6, 2,-1,-1,-1,-1,-1,-1,-1, &
       1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8,-1, &
       1, 5, 6, 2, 1, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 3, 6, 1, 6,10, 3, 8, 6, 5, 6, 9, 8, 9, 6,-1, &
      10, 1, 0,10, 0, 6, 9, 5, 0, 5, 6, 0,-1,-1,-1,-1, &
       0, 3, 8, 5, 6,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      10, 5, 6,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      11, 5,10, 7, 5,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      11, 5,10,11, 7, 5, 8, 3, 0,-1,-1,-1,-1,-1,-1,-1, &
       5,11, 7, 5,10,11, 1, 9, 0,-1,-1,-1,-1,-1,-1,-1, &
      10, 7, 5,10,11, 7, 9, 8, 1, 8, 3, 1,-1,-1,-1,-1, &
      11, 1, 2,11, 7, 1, 7, 5, 1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2,11,-1,-1,-1,-1, &
       9, 7, 5, 9, 2, 7, 9, 0, 2, 2,11, 7,-1,-1,-1,-1, &
       7, 5, 2, 7, 2,11, 5, 9, 2, 3, 2, 8, 9, 8, 2,-1/
   data triangle_table(0:15,200:224)/                  &
       2, 5,10, 2, 3, 5, 3, 7, 5,-1,-1,-1,-1,-1,-1,-1, &
       8, 2, 0, 8, 5, 2, 8, 7, 5,10, 2, 5,-1,-1,-1,-1, &
       9, 0, 1, 5,10, 3, 5, 3, 7, 3,10, 2,-1,-1,-1,-1, &
       9, 8, 2, 9, 2, 1, 8, 7, 2,10, 2, 5, 7, 5, 2,-1, &
       1, 3, 5, 3, 7, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 8, 7, 0, 7, 1, 1, 7, 5,-1,-1,-1,-1,-1,-1,-1, &
       9, 0, 3, 9, 3, 5, 5, 3, 7,-1,-1,-1,-1,-1,-1,-1, &
       9, 8, 7, 5, 9, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       5, 8, 4, 5,10, 8,10,11, 8,-1,-1,-1,-1,-1,-1,-1, &
       5, 0, 4, 5,11, 0, 5,10,11,11, 3, 0,-1,-1,-1,-1, &
       0, 1, 9, 8, 4,10, 8,10,11,10, 4, 5,-1,-1,-1,-1, &
      10,11, 4,10, 4, 5,11, 3, 4, 9, 4, 1, 3, 1, 4,-1, &
       2, 5, 1, 2, 8, 5, 2,11, 8, 4, 5, 8,-1,-1,-1,-1, &
       0, 4,11, 0,11, 3, 4, 5,11, 2,11, 1, 5, 1,11,-1, &
       0, 2, 5, 0, 5, 9, 2,11, 5, 4, 5, 8,11, 8, 5,-1, &
       9, 4, 5, 2,11, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 5,10, 3, 5, 2, 3, 4, 5, 3, 8, 4,-1,-1,-1,-1, &
       5,10, 2, 5, 2, 4, 4, 2, 0,-1,-1,-1,-1,-1,-1,-1, &
       3,10, 2, 3, 5,10, 3, 8, 5, 4, 5, 8, 0, 1, 9,-1, &
       5,10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2,-1,-1,-1,-1, &
       8, 4, 5, 8, 5, 3, 3, 5, 1,-1,-1,-1,-1,-1,-1,-1, &
       0, 4, 5, 1, 0, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5,-1,-1,-1,-1, &
       9, 4, 5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4,11, 7, 4, 9,11, 9,10,11,-1,-1,-1,-1,-1,-1,-1/
   data triangle_table(0:15,225:255)/                  &
       0, 8, 3, 4, 9, 7, 9,11, 7, 9,10,11,-1,-1,-1,-1, &
       1,10,11, 1,11, 4, 1, 4, 0, 7, 4,11,-1,-1,-1,-1, &
       3, 1, 4, 3, 4, 8, 1,10, 4, 7, 4,11,10,11, 4,-1, &
       4,11, 7, 9,11, 4, 9, 2,11, 9, 1, 2,-1,-1,-1,-1, &
       9, 7, 4, 9,11, 7, 9, 1,11, 2,11, 1, 0, 8, 3,-1, &
      11, 7, 4,11, 4, 2, 2, 4, 0,-1,-1,-1,-1,-1,-1,-1, &
      11, 7, 4,11, 4, 2, 8, 3, 4, 3, 2, 4,-1,-1,-1,-1, &
       2, 9,10, 2, 7, 9, 2, 3, 7, 7, 4, 9,-1,-1,-1,-1, &
       9,10, 7, 9, 7, 4,10, 2, 7, 8, 7, 0, 2, 0, 7,-1, &
       3, 7,10, 3,10, 2, 7, 4,10, 1,10, 0, 4, 0,10,-1, &
       1,10, 2, 8, 7, 4,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 1, 4, 1, 7, 7, 1, 3,-1,-1,-1,-1,-1,-1,-1, &
       4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1,-1,-1,-1,-1, &
       4, 0, 3, 7, 4, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       4, 8, 7,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       9,10, 8,10,11, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 9, 3, 9,11,11, 9,10,-1,-1,-1,-1,-1,-1,-1, &
       0, 1,10, 0,10, 8, 8,10,11,-1,-1,-1,-1,-1,-1,-1, &
       3, 1,10,11, 3,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 2,11, 1,11, 9, 9,11, 8,-1,-1,-1,-1,-1,-1,-1, &
       3, 0, 9, 3, 9,11, 1, 2, 9, 2,11, 9,-1,-1,-1,-1, &
       0, 2,11, 8, 0,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       3, 2,11,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 3, 8, 2, 8,10,10, 8, 9,-1,-1,-1,-1,-1,-1,-1, &
       9,10, 2, 0, 9, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       2, 3, 8, 2, 8,10, 0, 1, 8, 1,10, 8,-1,-1,-1,-1, &
       1,10, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       1, 3, 8, 9, 1, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 9, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
       0, 3, 8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, &
      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(MARCHINGCUBE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"MARCHINGCUBE:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(MARCHINGCUBE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MARCHINGCUBE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine set_defaults(self)
   ! Set up default isosurface information.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%vertex_positions = ZERO
      self%vertex_fn_value = ZERO
      self%vertex_gradient = ZERO
      self%vertex_hessian = ZERO
      self%x_side_length = ZERO
      self%y_side_length = ZERO
      self%z_side_length = ZERO
      call to_unit_mx_(self%axes)
      call to_unit_mx_(self%inverse_axes)
      self%iso_value = ONE
      self%accuracy = TOL(3)
      self%case = -1  ! This is not a real case
      self%interior_case = 0  ! Interior function values assumed larger than exterior
      self%exterior_case = 255  ! 0 means all the vertices are larger than iso_value
      self%edge_bit_string = 0
      self%skip_bit_string = 0
      self%cube_bit_string = 0
      self%n_edge = 0
      self%n_active_edge = 0
      self%n_triangle = 0
      self%n_pt = 0
      self%triangle_edge_index = 0
      self%triangle_vertex_index = 0
      self%edge_vertex_positions = ZERO
      self%edge_vertex_value = ZERO
      self%edge_vertex_gradient = ZERO
      self%edge_vertex_hessian = ZERO
      self%edge_mean_curvature = ZERO
      self%edge_gaussian_curvature = ZERO
      self%edge_vertex_index = 0
      self%vertex_edge_index = 0
   end subroutine

   PURE subroutine reset(self)
   ! Reset defaults. Compared to set_defaults, .iso_value is
   ! unchanged, and .accuracy is unchanged.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%vertex_positions = ZERO
      self%vertex_gradient = ZERO
      self%vertex_fn_value = ZERO
      self%case = -1  ! This is not a real case
      self%edge_bit_string = 0
      self%skip_bit_string = 0
      self%n_edge = 0
      self%n_active_edge = 0
      self%n_triangle = 0
      self%n_pt = 0
      self%edge_vertex_positions = ZERO
      self%edge_vertex_gradient = ZERO
      self%edge_vertex_index = 0
      self%vertex_edge_index = 0
      self%triangle_edge_index = 0
      self%triangle_vertex_index = 0
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","MARCHINGCUBE:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"MARCHINGCUBE:read_keywords ... unexpected end of file while reading list keywords")
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
      type(MARCHINGCUBE_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                ")  ! exit case
      case ("accuracy=        "); call read_accuracy_(self)
      case ("interior_is_big= "); call read_interior_is_big_(self)
      case ("iso_value=       "); call read_iso_value_(self)
      case ("put              "); call put_(self)
      case ("units=           "); call read_units_(self)
      case default; allocate(tonto%known_keywords(6))
      tonto%known_keywords(1) = "}                "
      tonto%known_keywords(2) = "accuracy=        "
      tonto%known_keywords(3) = "interior_is_big= "
      tonto%known_keywords(4) = "iso_value=       "
      tonto%known_keywords(5) = "put              "
      tonto%known_keywords(6) = "units=           "
      call unknown_(tonto,word,"MARCHINGCUBE:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_accuracy(self)
   ! Read a number which tells to what accuracy each generated
   ! isosurface is determined
      type(MARCHINGCUBE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%accuracy)
   end subroutine

   subroutine read_interior_is_big(self)
   ! Read a switch which tells whether the interior of the isosurface
   ! which the cube is mapping is larger than the exterior. This sets
   ! the interior and exterior case numbers, and would be determined
   ! by, for example, whether the surface gradients are required to be
   ! reversed (since in this case the gradients would be pointing
   ! inwards from the surface toward increasing values).  i.e.
   ! interior_is_big = reverse_normals
      type(MARCHINGCUBE_TYPE), INOUT :: self
      BIN :: big_interior
      call read_(stdin,big_interior)
      call set_big_interior_(self,big_interior)
   end subroutine

   subroutine read_iso_value(self)
   ! Read the defining iso_value for the isosurface
      type(MARCHINGCUBE_TYPE), INOUT :: self
      call read_quantity_(stdin,self%iso_value)
   end subroutine


!  ===========
!  Set methods
!  ===========

   subroutine set_axes(self,axes)
   ! Set the marching cube axes. The y and z axis lengths are scaled
   ! so that we can apply a single matrix correction to get the
   ! vertex gradients and hessians.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT(REAL,3,3), IN :: axes
      REAL :: facx
      self%axes(:,1) = axes(:,1)
      facx = ONE/self%x_side_length
      self%axes(:,2) = axes(:,2)*self%y_side_length*facx
      self%axes(:,3) = axes(:,3)*self%z_side_length*facx
      call to_inverse_of_(self%inverse_axes,self%axes)  ! NOT PURE
   end subroutine


!   correct_for_oblique_axes
!   ! Correct the vertex gradient and hessian for the use of oblique axes.
!     v :: VEC{REAL}(3)
!     v = .vertex_gradient
!     .vertex_gradient = matmul(.vertex_gradient,.inverse_axes)

!                               matmul(.vertex_hessian,.inverse_axes))
!   end

!   set_vertex_info(px,py,pz,f)
!   ! Set the marching cube vertex info, i.e. x, y z, coordinates of each vertex
!   ! "px", "py", "pz", and the function values "f" at each vertex. Vertex
!   ! gradient information is *not" set.
!      px,py,pz,f :: MAT3{REAL}(0:1,0:1,0:1), IN
!      v,x,y,z :: INT
!      do v = 0, 7 ! loop over vertices
!          x = tube(v,1)
!          y = tube(v,2)
!          z = tube(v,3)
!          .vertex_positions(1,v)    = px(x,y,z)
!          .vertex_positions(2,v)    = py(x,y,z)
!          .vertex_positions(3,v)    = pz(x,y,z)
!          .vertex_fn_value(v) = f(x,y,z)
!      end
!   end

   PURE subroutine set_vertex_info_0(self,p,f)
   ! Set the marching cube vertex info, i.e. x, y z, coordinates of
   ! each vertex "p" and the function values "f" at each vertex.
   ! Vertex gradient information is *not* set.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT4(REAL,3,0:1,0:1,0:1), IN :: p
      MAT3(REAL,0:1,0:1,0:1), IN :: f
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         x = cube(1,v)
         y = cube(2,v)
         z = cube(3,v)
         self%vertex_positions(:,v) = p(:,x,y,z)
         self%vertex_fn_value(v) = f(x,y,z)
      end do
   end subroutine

   PURE subroutine set_vertex_info_1(self,p,f,r)
   ! Set the marching cube vertex info from a Bader basin index array,
   ! i.e. the x, y, z coordinates of each vertex "p", and a vertex
   ! function value which is ZERO where the basin index "f" equals the
   ! basin of interest "r" and ONE elsewhere. Used with an .iso_value
   ! of one half, this triangulates the boundary of basin "r".
   ! Vertex gradient information is *not* set: a basin index is a step
   ! function and has no useful gradient.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT4(REAL,3,0:1,0:1,0:1), IN :: p
      MAT3(INT,0:1,0:1,0:1), IN :: f
      INT, IN :: r
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         x = cube(1,v)
         y = cube(2,v)
         z = cube(3,v)
         self%vertex_positions(:,v) = p(:,x,y,z)
         if (f(x,y,z)==r) then; self%vertex_fn_value(v) = ZERO
         else; self%vertex_fn_value(v) = ONE
         end if
      end do
   end subroutine

   PURE subroutine set_vertex_info3(self,p,f,ox,oy,oz)
   ! Set the marching cube vertex info from a 3-cube, i.e. x, y z,
   ! coordinates of each vertex "p" and the function values "f" at
   ! each vertex where each is a three cube and using the offsets
   ! "ox", "oy", "oz" to get the appropriate 2-cube.  Vertex gradient
   ! information is *not* set. This is a specialist routine for
   ! recursive marching cubes.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT4(REAL,3,3,3,3), IN :: p
      MAT3(REAL,3,3,3), IN :: f
      INT, IN :: ox,oy,oz
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         x = ox+cube(1,v)+1
         y = oy+cube(2,v)+1
         z = oz+cube(3,v)+1
         self%vertex_positions(:,v) = p(:,x,y,z)
         self%vertex_fn_value(v) = f(x,y,z)
      end do
   end subroutine

   PURE subroutine set_vertex_info_2(self,p,f,g)
   ! Set the marching cube vertex info, i.e. x, y z, coordinates of
   ! each vertex "p", the function values "f" at each vertex, and the
   ! gradients "g" at each vertex.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT4(REAL,3,0:1,0:1,0:1), IN :: p
      MAT3(REAL,0:1,0:1,0:1), IN :: f
      MAT4(REAL,3,0:1,0:1,0:1), IN :: g
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         x = cube(1,v)
         y = cube(2,v)
         z = cube(3,v)
         self%vertex_positions(:,v) = p(:,x,y,z)
         self%vertex_fn_value(v) = f(x,y,z)
         self%vertex_gradient(:,v) = g(:,x,y,z)
      end do
   end subroutine

   PURE subroutine set_vertex_info_3(self,p,f,g,h)
   ! Set the marching cube vertex info, i.e. x, y z, coordinates of
   ! each vertex "p", the function values "f" at each vertex, the
   ! gradients "g" at each vertex, and the hessians "h" at each vertex
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT4(REAL,3,0:1,0:1,0:1), IN :: p
      MAT3(REAL,0:1,0:1,0:1), IN :: f
      MAT4(REAL,3,0:1,0:1,0:1), IN :: g
      MAT5(REAL,3,3,0:1,0:1,0:1), IN :: h
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         x = cube(1,v)
         y = cube(2,v)
         z = cube(3,v)
         self%vertex_positions(:,v) = p(:,x,y,z)
         self%vertex_fn_value(v) = f(x,y,z)
         self%vertex_gradient(:,v) = g(:,x,y,z)
         self%vertex_hessian(:,:,v) = h(:,:,x,y,z)
      end do
   end subroutine

   PURE subroutine set_gradient_info(self,f)
   ! Set the marching cube vertex gradient information, calculated by
   ! finite differences, from a cube of values "f". The "f" array
   ! indices 2 & 3 correspond to the actual cube, indices 1 & 4 are
   ! the surrounding cube edges, required to evaluate the gradients.
   ! NOTE: only those corners for which .cube_bit_string is set have
   ! the gradients computed.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT3(REAL,4,4,4), IN :: f
      INT :: v,x,y,z
      REAL :: fac
      fac = ONE/(TWO*self%x_side_length)

      ! This is for reversing the sign of f
      if (self%iso_value>=ZERO) fac = -fac
      do v = 0,7  ! loop over vertices
         if (NOT BIT_SET_AT_POS_IN(self%cube_bit_string,v)) cycle
         x = cube(1,v)+2
         y = cube(2,v)+2
         z = cube(3,v)+2
         self%vertex_gradient(1,v) = (f(x+1,y,z)-f(x-1,y,z))*fac
         self%vertex_gradient(2,v) = (f(x,y+1,z)-f(x,y-1,z))*fac
         self%vertex_gradient(3,v) = (f(x,y,z+1)-f(x,y,z-1))*fac
      end do
   end subroutine

   PURE subroutine set_hessian_info(self,f)
   ! Set the marching cube vertex gradient and hessian information,
   ! calculated by finite differences, from a cube of values "f". The
   ! "f" array indices 2 & 3 correspond to the actual cube, indices 1
   ! & 4 are the surrounding cube edges, required to evaluate the
   ! gradients. NOTE: only those corners for which .cube_bit_string is
   ! set have the gradients computed.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT3(REAL,4,4,4), IN :: f
      INT :: v,x,y,z
      REAL :: fac,fac2,fac4
      fac = ONE/(TWO*self%x_side_length)
      fac2 = fac*fac
      fac4 = fac2*FOUR

      ! This code should be executed when .iso_value >= ZERO
      ! but NOT when .iso_value < ZERO
      if (self%iso_value>=ZERO) then  ! This is for reversing the sign of f
         fac = -fac
         fac2 = -fac2
         fac4 = -fac4
      end if
      do v = 0,7  ! loop over vertices
         if (NOT BIT_SET_AT_POS_IN(self%cube_bit_string,v)) cycle
         x = cube(1,v)+2
         y = cube(2,v)+2
         z = cube(3,v)+2
         self%vertex_gradient(1,v) = (f(x+1,y,z)-f(x-1,y,z))*fac
         self%vertex_gradient(2,v) = (f(x,y+1,z)-f(x,y-1,z))*fac
         self%vertex_gradient(3,v) = (f(x,y,z+1)-f(x,y,z-1))*fac
         self%vertex_hessian(1,1,v) = (f(x+1,y,z)+f(x-1,y,z)-TWO*f(x,y,z))*fac4
         self%vertex_hessian(2,2,v) = (f(x,y+1,z)+f(x,y-1,z)-TWO*f(x,y,z))*fac4
         self%vertex_hessian(3,3,v) = (f(x,y,z+1)+f(x,y,z-1)-TWO*f(x,y,z))*fac4
         self%vertex_hessian(2,1,v) = (f(x+1,y+1,z)-f(x+1,y-1,z)-f(x-1,y+1,z)+f(x-1,y-1,z))*fac2
         self%vertex_hessian(3,1,v) = (f(x+1,y,z+1)-f(x+1,y,z-1)-f(x-1,y,z+1)+f(x-1,y,z-1))*fac2
         self%vertex_hessian(3,2,v) = (f(x,y+1,z+1)-f(x,y+1,z-1)-f(x,y-1,z+1)+f(x,y-1,z-1))*fac2
         self%vertex_hessian(1,2,v) = self%vertex_hessian(2,1,v)
         self%vertex_hessian(1,3,v) = self%vertex_hessian(3,1,v)
         self%vertex_hessian(2,3,v) = self%vertex_hessian(3,2,v)
      end do
   end subroutine

   PURE subroutine set_hessian_info5(self,f,ox,oy,oz)
   ! Specialist routine for resursive marching cubes. It takes into
   ! account actual size of "f" with offsets "ox", "oy", and "oz".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT3(REAL,5,5,5), IN :: f
      INT, IN :: ox,oy,oz
      INT :: v,x,y,z
      REAL :: fac,fac2,fac4
      fac = ONE/(TWO*self%x_side_length)
      fac2 = fac*fac
      fac4 = fac2*FOUR

      ! This is for reversing the sign of f
      if (self%iso_value>=ZERO) then
         fac = -fac
         fac2 = -fac2
         fac4 = -fac4
      end if
      do v = 0,7  ! loop over vertices
         if (NOT BIT_SET_AT_POS_IN(self%cube_bit_string,v)) cycle
         x = ox+cube(1,v)+2
         y = oy+cube(2,v)+2
         z = oz+cube(3,v)+2
         self%vertex_gradient(1,v) = (f(x+1,y,z)-f(x-1,y,z))*fac
         self%vertex_gradient(2,v) = (f(x,y+1,z)-f(x,y-1,z))*fac
         self%vertex_gradient(3,v) = (f(x,y,z+1)-f(x,y,z-1))*fac
         self%vertex_hessian(1,1,v) = (f(x+1,y,z)+f(x-1,y,z)-TWO*f(x,y,z))*fac4
         self%vertex_hessian(2,2,v) = (f(x,y+1,z)+f(x,y-1,z)-TWO*f(x,y,z))*fac4
         self%vertex_hessian(3,3,v) = (f(x,y,z+1)+f(x,y,z-1)-TWO*f(x,y,z))*fac4
         self%vertex_hessian(2,1,v) = (f(x+1,y+1,z)-f(x+1,y-1,z)-f(x-1,y+1,z)+f(x-1,y-1,z))*fac2
         self%vertex_hessian(3,1,v) = (f(x+1,y,z+1)-f(x+1,y,z-1)-f(x-1,y,z+1)+f(x-1,y,z-1))*fac2
         self%vertex_hessian(3,2,v) = (f(x,y+1,z+1)-f(x,y+1,z-1)-f(x,y-1,z+1)+f(x,y-1,z-1))*fac2
         self%vertex_hessian(1,3,v) = self%vertex_hessian(3,1,v)
         self%vertex_hessian(1,2,v) = self%vertex_hessian(2,1,v)
         self%vertex_hessian(2,3,v) = self%vertex_hessian(3,2,v)
      end do
   end subroutine

   PURE subroutine set_hessian_eval_array(self,eval,ox,oy,oz)
   ! This routine is used only in conjunction with the recursive
   ! marching cube algorithm. It sets an element of array "eval" TRUE
   ! if the corresponding cube point is required for the evaluation of
   ! the gradient or hessian by finite differences. Armed with this
   ! information, the number of function calls to the isosurface
   ! routine can be reduced. The numbers "ox", "oy" and "oz" are
   ! offsets telling how much to offset the x, y, and z indices of the
   ! 4x4x4 cube. As before, indices 2 & 3 correspond to the actual
   ! cube, indices 1 & 4 are the surrounding cube edges, required to
   ! evaluate the gradients.  NOTE: only those corners for which
   ! .cube_bit_string is set have the gradients computed.
      type(MARCHINGCUBE_TYPE), IN :: self
      MAT3(BIN,5,5,5), INOUT :: eval
      INT, IN :: ox,oy,oz
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         if (NOT BIT_SET_AT_POS_IN(self%cube_bit_string,v)) cycle
         x = cube(1,v)+2+ox
         y = cube(2,v)+2+oy
         z = cube(3,v)+2+oz
         eval(x,y,z) = TRUE
         eval(x+1,y,z) = TRUE
         eval(x-1,y,z) = TRUE
         eval(x,y+1,z) = TRUE
         eval(x,y-1,z) = TRUE
         eval(x,y,z+1) = TRUE
         eval(x,y,z-1) = TRUE
         eval(x+1,y+1,z) = TRUE
         eval(x+1,y-1,z) = TRUE
         eval(x-1,y+1,z) = TRUE
         eval(x-1,y-1,z) = TRUE
         eval(x+1,y,z+1) = TRUE
         eval(x+1,y,z-1) = TRUE
         eval(x-1,y,z+1) = TRUE
         eval(x-1,y,z-1) = TRUE
         eval(x,y+1,z+1) = TRUE
         eval(x,y+1,z-1) = TRUE
         eval(x,y-1,z+1) = TRUE
         eval(x,y-1,z-1) = TRUE
      end do
   end subroutine

   PURE subroutine set_gradient_eval_array(self,eval,ox,oy,oz)
   ! This routine is used only in conjunction with the recursive
   ! marching cube algorithm. It sets an element of array "eval" TRUE
   ! if the corresponding cube point is required for the evaluation of
   ! the gradient or hessian by finite differences. Armed with this
   ! information, the number of function calls to the isosurface
   ! routine can be reduced. The numbers "ox", "oy" and "oz" are
   ! offsets telling how much to offset the x, y, and z indices of the
   ! 4x4x4 cube. As before, indices 2 & 3 correspond to the actual
   ! cube, indices 1 & 4 are the surrounding cube edges, required to
   ! evaluate the gradients.  NOTE: only those corners for which
   ! .cube_bit_string is set have the gradients computed.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT3(BIN,5,5,5), INOUT :: eval
      INT, IN :: ox,oy,oz
      INT :: v,x,y,z
      do v = 0,7  ! loop over vertices
         if (NOT BIT_SET_AT_POS_IN(self%cube_bit_string,v)) cycle
         x = cube(1,v)+2+ox
         y = cube(2,v)+2+oy
         z = cube(3,v)+2+oz
         eval(x,y,z) = TRUE
         eval(x+1,y,z) = TRUE
         eval(x-1,y,z) = TRUE
         eval(x,y+1,z) = TRUE
         eval(x,y-1,z) = TRUE
         eval(x,y,z+1) = TRUE
         eval(x,y,z-1) = TRUE
      end do
   end subroutine

   PURE subroutine set_vertex_and_gradient_info(self,p,f)
   ! Set the marching cube vertex info, i.e. x, y z, coordinates of
   ! each vertex "p" and the function values "f" at each vertex, as
   ! well as the vertex gradient information calculated by finite
   ! differences. The "f" array indices 2 & 3 correspond to the
   ! current cube, indices 1 & 4 are the surrounding cube edges,
   ! required to evaluate the gradients.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      MAT4(REAL,3,0:1,0:1,0:1), IN :: p
      MAT3(REAL,-1:2,-1:2,-1:2), IN :: f
      INT :: v,x,y,z
      REAL :: fac
      fac = ONE/(TWO*self%x_side_length)
      if (self%iso_value>=ZERO) fac = -fac
      do v = 0,7  ! loop over vertices
         x = cube(1,v)
         y = cube(2,v)
         z = cube(3,v)
         self%vertex_positions(:,v) = p(:,x,y,z)
         self%vertex_fn_value(v) = f(x,y,z)
         self%vertex_gradient(1,v) = (f(x+1,y,z)-f(x-1,y,z))*fac
         self%vertex_gradient(2,v) = (f(x,y+1,z)-f(x,y-1,z))*fac
         self%vertex_gradient(3,v) = (f(x,y,z+1)-f(x,y,z-1))*fac
      end do
   end subroutine

   PURE subroutine set_n_pt(self,n)
   ! Set the number of unique points found for all cubes up till this one.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT, IN :: n
   ENSURE(n>=0,"MARCHINGCUBE:set_n_pt ... must have non-negative number of points")
      self%n_pt = n
   end subroutine

   PURE subroutine set_iso_value(self,iso_value)
   ! Set the defining "iso_value"
      type(MARCHINGCUBE_TYPE), INOUT :: self
      REAL, IN :: iso_value
      self%iso_value = iso_value
   end subroutine

   PURE subroutine set_side_length_0(self,length)
   ! Set the cube side length to "length"
      type(MARCHINGCUBE_TYPE), INOUT :: self
      REAL, IN :: length
      self%x_side_length = length
      self%y_side_length = length
      self%z_side_length = length
   end subroutine

   PURE subroutine set_side_length_1(self,x_length,y_length,z_length)
   ! Set the cube side lengths
      type(MARCHINGCUBE_TYPE), INOUT :: self
      REAL, IN :: x_length,y_length,z_length
      self%x_side_length = x_length
      self%y_side_length = y_length
      self%z_side_length = z_length
   end subroutine

   PURE subroutine set_accuracy(self,accuracy)
   ! Set a number "accuracy" tells to what accuracy each generated
   ! isosurface is determined
      type(MARCHINGCUBE_TYPE), INOUT :: self
      REAL, IN :: accuracy
      self%accuracy = accuracy
   end subroutine

   PURE subroutine set_big_interior(self,big_interior)
   ! Set a switch which tells whether the interior of the isosurface
   ! which the cube is mapping is larger than the exterior. This sets
   ! the interior and exterior case numbers, and would be determined
   ! by, for example, whether the surface gradients are required to be
   ! reversed (since in this case the gradients would be pointing
   ! inwards from the surface toward increasing values).  i.e.
   ! interior_is_big = reverse_normals
   ! NOTE: 0 means all the vertices are larger than iso_value
   !       1 means all the vertices are less than iso_value
      type(MARCHINGCUBE_TYPE), INOUT :: self
      BIN, IN :: big_interior
      select case (big_interior)

      case (TRUE)
         self%interior_case = 0
         self%exterior_case = 255

      case (FALSE)
         self%interior_case = 255
         self%exterior_case = 0
      end select
   end subroutine

   PURE subroutine set_case_info(self)
   ! Set the marching cube case information.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%case = case_number_(self)
   end subroutine

   PURE subroutine set_triangulation_info(self)
   ! Set the marching cube triangulation information i.e. which edges of the
   ! cube are supposed to be interpolated, how many triangle faces are needed.
   ! NOTE: the case number is assumed to be correctly set by a call to
   ! .set_case_info
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT :: i,e

      ! Set which edges cut the surface
      call set_edge_bit_string_(self)

      ! Set which vetices are needed;
      ! WARNING: this routine notices .skip_bit_string
      call set_cube_bit_string_(self)
      self%n_triangle = no_of_triangles_(self,self%case)
      self%triangle_edge_index = reshape(triangle_table(0:14,self%case),[3,5])

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

   PURE subroutine set_left_info(self,left)
   ! Set the .skip_bit_string based on when there is information from the
   ! adjoing cube to the left this cube. The actual .edge_vertex_index values
   ! for the left cube are given in the array "left".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      VEC(INT,0:12), IN :: left
      INT :: ind

      ! This skips the left face edges: 3,7,8,11
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,3)) then
         ind = left(1)
         self%edge_vertex_index(3) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,3)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,7)) then
         ind = left(5)
         self%edge_vertex_index(7) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,7)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,8)) then
         ind = left(9)
         self%edge_vertex_index(8) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,8)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,11)) then
         ind = left(10)
         self%edge_vertex_index(11) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,11)
      end if
   end subroutine

   PURE subroutine set_front_info(self,front)
   ! Set the .skip_bit_string based on when there is information from the
   ! adjoing cube to the front this cube. The actual .edge_vertex_index values
   ! for the front cube are given in the array "front".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      VEC(INT,0:12), IN :: front
      INT :: ind

      ! This skips the front face edges: 0,4,8,9
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,0)) then
         ind = front(2)
         self%edge_vertex_index(0) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,0)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,4)) then
         ind = front(6)
         self%edge_vertex_index(4) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,4)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,8)) then
         ind = front(11)
         self%edge_vertex_index(8) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,8)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,9)) then
         ind = front(10)
         self%edge_vertex_index(9) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,9)
      end if
   end subroutine

   PURE subroutine set_below_info(self,below)
   ! Set the .skip_bit_string based on when there is information from the
   ! adjoing cube below this cube. The actual .edge_vertex_index values for the
   ! cube below are given in the array "below".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      VEC(INT,0:12), IN :: below
      INT :: ind

      ! This skips the bottom face edges: 0,1,2,3
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,0)) then
         ind = below(4)
         self%edge_vertex_index(0) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,0)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,1)) then
         ind = below(5)
         self%edge_vertex_index(1) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,1)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,2)) then
         ind = below(6)
         self%edge_vertex_index(2) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,2)
      end if
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,3)) then
         ind = below(7)
         self%edge_vertex_index(3) = ind
         self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,3)
      end if
   end subroutine

   PURE subroutine set_left_skip_bit_string(self)
   ! Set the .skip_bit_string based on when there is information from
   ! the adjoing cube to the left this cube.
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! This skips the left face edges: 3,7,8,11
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,3)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,3)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,7)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,7)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,8)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,8)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,11)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,11)
   end subroutine

   PURE subroutine set_front_skip_bit_string(self)
   ! Set the .skip_bit_string based on when there is information from
   ! the adjoing cube to the front this cube.
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! This skips the front face edges: 0,4,8,9
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,0)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,0)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,4)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,4)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,8)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,8)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,9)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,9)
   end subroutine

   PURE subroutine set_below_skip_bit_string(self)
   ! Set the .skip_bit_string based on when there is information from
   ! the adjoing cube below this cube.
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! This skips the bottom face edges: 0,1,2,3
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,0)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,0)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,1)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,1)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,2)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,2)
      if (BIT_SET_AT_POS_IN(self%edge_bit_string,3)) self%skip_bit_string = SET_BIT_AT_POS_IN(self%skip_bit_string,3)
   end subroutine

   PURE subroutine set_edge_bit_string(self)
   ! Set the marching cube edge_bit_string which tells which edges
   ! cross the isosurface and therefore are to be interpolated. NOTE:
   ! the case number is assumed to be correctly set e.g. by a call to
   ! .set_case_info.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = edge_table(self%case)
   end subroutine

   PURE subroutine set_left_face_edges(self)
   ! Explicitly set the left face edge "bits".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = 0
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,3)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,7)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,8)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,11)
      self%edge_bit_string = not(self%edge_bit_string)
   end subroutine

   PURE subroutine set_right_face_edges(self)
   ! Explicitly set the right face edge "bits".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = 0
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,1)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,5)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,9)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,10)
      self%edge_bit_string = not(self%edge_bit_string)
   end subroutine

   PURE subroutine set_front_face_edges(self)
   ! Explicitly set the front face edge "bits".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = 0
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,0)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,4)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,8)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,9)
      self%edge_bit_string = not(self%edge_bit_string)
   end subroutine

   PURE subroutine set_back_face_edges(self)
   ! Explicitly set the back face edge "bits".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = 0
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,2)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,6)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,10)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,11)
      self%edge_bit_string = not(self%edge_bit_string)
   end subroutine

   PURE subroutine set_below_face_edges(self)
   ! Explicitly set the below face edge "bits".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = 0
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,0)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,1)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,2)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,3)
      self%edge_bit_string = not(self%edge_bit_string)
   end subroutine

   PURE subroutine set_top_face_edges(self)
   ! Explicitly set the below face edge "bits".
      type(MARCHINGCUBE_TYPE), INOUT :: self
      self%edge_bit_string = 0
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,4)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,5)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,6)
      self%edge_bit_string = SET_BIT_AT_POS_IN(self%edge_bit_string,7)
      self%edge_bit_string = not(self%edge_bit_string)
   end subroutine

   PURE subroutine set_cube_bit_string(self)
   ! Set the .cube_bit_string which tells which cube corners lie on
   ! either side of the isosurface. The .cube_bit_string is used to
   ! save work when calculating the gradients and hessians on the cube
   ! corners. 
   ! NOTE: the edge_bit_string is assumed to be correctly set e.g. by
   ! a call to .set_edge_bit_string. NOTE: if some edges are to be
   ! explicitly skipped then those vertices do not appear in
   ! .cube_bit_string.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT :: e
      self%cube_bit_string = 0
      do e = 0,11

         ! Do this edge?
         if (NOT BIT_SET_AT_POS_IN(self%edge_bit_string,e)) cycle

         ! Explicitly skipped?
         if (BIT_SET_AT_POS_IN(self%skip_bit_string,e)) cycle
         self%cube_bit_string = SET_BIT_AT_POS_IN(self%cube_bit_string,vertex_1_of_edge(e))
         self%cube_bit_string = SET_BIT_AT_POS_IN(self%cube_bit_string,vertex_2_of_edge(e))
      end do
   end subroutine

   PURE subroutine set_capping_square(self,face_number,cappingsquare)
   ! Copies the relevant info from this marching cube to the cappingsquare for the face given by face_number
      type(MARCHINGCUBE_TYPE), IN :: self
      INT, IN :: face_number
      type(CAPPING_SQUARE_TYPE), INOUT :: cappingsquare
      VEC(INT,1:4) :: square_vertex,square_edge,edge_index
      VEC(REAL,1:4) :: values
      MAT(REAL,1:3,1:4) :: pos
      INT :: v
   ENSURE(face_number>=1,"MARCHINGCUBE:set_capping_square ... face_number outside range 1-6")
   ENSURE(face_number<=6,"MARCHINGCUBE:set_capping_square ... face_number outside range 1-6")
      square_vertex = vertices_of_face(:,face_number)
      square_edge = edges_of_face(:,face_number)
      call set_iso_value_(cappingsquare,self%iso_value)
      call set_face_(cappingsquare,face_number)

      ! For each vertex of the square...
      do v = 1,size(square_vertex)
         pos(:,v) = self%vertex_positions(:,square_vertex(v))
         values(v) = self%vertex_fn_value(square_vertex(v))
         edge_index(v) = self%edge_vertex_index(square_edge(v))
      end do
      call set_vertex_positions_(cappingsquare,pos)
      call set_vertex_values_(cappingsquare,values)
      call set_edge_vertex_index_(cappingsquare,edge_index)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine get_edge_vertex_positions(self,pos)
   ! Set the new edge vertex positions found on this marching cube
      type(MARCHINGCUBE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: pos
      INT :: i
   ENSURE(size(pos,1)==3,"MARCHINGCUBE:get_edge_vertex_positions ... wrong 1st dimension, pos")
   ENSURE(size(pos,2)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_vertex_positions ... pos too small")
      do i = 1,self%n_active_edge
         pos(:,i) = self%edge_vertex_positions(:,self%vertex_edge_index(i))
      end do
   end subroutine

   PURE subroutine get_edge_vertex_gradients(self,g)
   ! Get the new edge vertex gradient vectors "g" on this marching cube
      type(MARCHINGCUBE_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: g
      INT :: i
   ENSURE(size(g,1)==3,"MARCHINGCUBE:get_edge_vertex_gradients ... wrong 1st dimension, g")
   ENSURE(size(g,2)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_vertex_gradients ... g array too small")
      do i = 1,self%n_active_edge
         g(:,i) = self%edge_vertex_gradient(:,self%vertex_edge_index(i))
      end do
   end subroutine

   PURE subroutine get_edge_mean_curvatures(self,c)
   ! Get the new edge vertex mean curvatures "c" on this marching cube
      type(MARCHINGCUBE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: c
      INT :: i
   ENSURE(size(c)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_mean_curvatures ... c array too small")
      do i = 1,self%n_active_edge
         c(i) = self%edge_mean_curvature(self%vertex_edge_index(i))
      end do
   end subroutine

   PURE subroutine get_edge_gaussian_curvatures(self,c)
   ! Get the new edge vertex gaussian curvatures "c" on this marching cube
      type(MARCHINGCUBE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: c
      INT :: i
   ENSURE(size(c)>=no_of_active_edges_(self),"MARCHINGCUBE:get_edge_gaussian_curvatures ... c array too small")
      do i = 1,self%n_active_edge
         c(i) = self%edge_gaussian_curvature(self%vertex_edge_index(i))
      end do
   end subroutine

   PURE subroutine get_triangle_vertex_indices(self,ind)
   ! Set the new edge vertex positions found on this marching cube
      type(MARCHINGCUBE_TYPE), IN :: self
      MAT(INT,:,:), OUT :: ind
   ENSURE(size(ind,1)==3,"MARCHINGCUBE:get_triangle_vertex_indices ... wrong 1st dimension, ind")
   ENSURE(size(ind,2)==self%n_triangle,"MARCHINGCUBE:get_triangle_vertex_indices ... wrong 2nd dimension, ind")
      ind = self%triangle_vertex_index(:,1:self%n_triangle)
   end subroutine


!  =======
!  Inquiry
!  =======

   PURE function no_of_triangles(self,case) result (res)
   ! Return the number of triangles for a marching cube with a
   ! particular "case" number.
      type(MARCHINGCUBE_TYPE), IN :: self
      INT, optional, IN :: case
      INT :: res
      INT :: e,c
      VEC(INT,0:15) :: triangle_edge_index
      c = self%case
      if (present(case)) c = case
      ! ENSURE(c>=0 AND c<=255,"case is out of range")
      if (c<=0 OR c>=255) then
         res = 0
      else
         triangle_edge_index = triangle_table(:,c)

         ! Loop over triangle edge triples
         res = 0
         do e = 0,15,3
            if (triangle_edge_index(e)==-1) exit
            res = res+1
         end do
      end if
   end function

   PURE function no_of_edges(self) result (res)
   ! Return the number of edges to be interpolated on this cube
      type(MARCHINGCUBE_TYPE), IN :: self
      INT :: res

   ! ENSURE(.case>=0 AND .case<=255,"case is out of range")
      INT :: e
      res = 0
      do e = 0,11
         if (NOT BIT_SET_AT_POS_IN(self%edge_bit_string,e)) cycle
         res = res+1
      end do
   end function

   PURE function no_of_active_edges(self) result (res)
   ! Return the number of edges to be interpolated on this cube not including
   ! those that are skipped.
      type(MARCHINGCUBE_TYPE), IN :: self
      INT :: res

   ! ENSURE(.case>=0 AND .case<=255,"case is out of range")
      INT :: e
      res = 0
      do e = 0,11
         if (NOT BIT_SET_AT_POS_IN(self%edge_bit_string,e)) cycle
         if (BIT_SET_AT_POS_IN(self%skip_bit_string,e)) cycle
         res = res+1
      end do
   end function


!  =====================================
!  Triangulation: this is the main thing
!  =====================================

   PURE subroutine interpolate_Bader_faces(self)
   ! As for .interpolate_faces, but for a Bader basin boundary. Only
   ! the edge vertex *positions* are interpolated: the vertex function
   ! value is a basin-membership flag, so the gradients, hessians and
   ! curvatures .interpolate_edge_info produces have no meaning here.
   ! NOTE: the .case number must already have been set.
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! Skip some edges
      call interpolate_Bader_edge_info_(self)

      ! Set interpolated vertices
      call set_triangle_vertex_info_(self)
   end subroutine

   PURE subroutine interpolate_Bader_edge_info(self)
   ! Interpolate .edge_vertex_positions on each edge of the cube which
   ! crosses the basin boundary. The edge vertex positions later become
   ! the triangle vertices.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT :: e,v1,v2
      REAL :: f1,f2,del,bot
      VEC(REAL,3) :: p1,p2,P
      self%n_active_edge = 0
      do e = 0,11

         ! Skip? This edge does not intersect
         if (NOT BIT_SET_AT_POS_IN(self%edge_bit_string,e)) cycle

         ! Skip? This edge is explicitly skipped
         if (BIT_SET_AT_POS_IN(self%skip_bit_string,e)) cycle
         self%n_active_edge = self%n_active_edge+1

         ! Vertices
         v1 = vertex_1_of_edge(e)
         v2 = vertex_2_of_edge(e)

         ! Values at vertices
         f1 = self%vertex_fn_value(v1)
         f2 = self%vertex_fn_value(v2)

         ! Edge end points
         p1 = self%vertex_positions(:,v1)
         p2 = self%vertex_positions(:,v2)

         ! Fractional distance along the edge
         del = self%iso_value-f1
         bot = f2-f1
         if (bot/=ZERO) then; del = del/bot
         else; del = ZERO
         end if

         ! Interpolate
         P = p1+del*(p2-p1)
         self%edge_vertex_positions(:,e) = P
      end do
   end subroutine

   PURE subroutine interpolate_faces(self)
   ! This routine interpolates the triangle coordinates that result from the
   ! intersection of this cube with the isosurface. Any previously obtained
   ! information abount adjoining cubes to the left, infront, or below this cube
   ! should have already been set, e.g. using .set_left_info. This routine
   ! assumes that cubes are generated in an order from left to right (along the
   ! x-axis), front-to back (along the y-axis), and bottom to top (along the
   ! z-axis). NOTE: the .case number must already have been set.
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! Skip some edges
      call interpolate_edge_info_(self)

      ! Set interpolated vertices
      call set_triangle_vertex_info_(self)
   end subroutine

   PURE subroutine interpolate_edge_info(self)
   ! This routine interpolates edge_vertex_pos, edge_vertex_gradient,
   ! edge_vertex_hessian, edge_mean_curvature, edge_gaussian_curvature.
   ! The edge vertex positions are later to become the triangle vertices.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT :: e,v1,v2
      REAL :: f1,f2,del,bot,L
      VEC(REAL,3) :: p1,p2,P,g1,g2,G,n,u,v
      MAT(REAL,3,3) :: h1,h2,H
      MAT(REAL,2,2) :: S
      MAT(REAL,3,2) :: UV
      self%n_active_edge = 0
      do e = 0,11

         ! Skip? This edge does not intersect
         if (NOT BIT_SET_AT_POS_IN(self%edge_bit_string,e)) cycle

         ! Skip? this edge is explicitly skipped
         if (BIT_SET_AT_POS_IN(self%skip_bit_string,e)) cycle
         self%n_active_edge = self%n_active_edge+1

         ! Vertices
         v1 = vertex_1_of_edge(e)
         v2 = vertex_2_of_edge(e)

         ! Values at vertices
         f1 = self%vertex_fn_value(v1)
         f2 = self%vertex_fn_value(v2)
         L = min(f1,f2)
         ENSURE(self%iso_value>=L,"MARCHINGCUBE:interpolate_edge_info ... iso value LT endpoint values, "//trim(to_str_(L)))
         L = max(f1,f2)

         ! Oops ...
         if (self%iso_value>L) then

         ! stdout.text("edge_bit_string:")
         ! do v1 = 0,11
         !    if (BIT_SET_AT_POS_IN(.edge_bit_string,e)) then; stdout.put("1")
         !    else;                                            stdout.put("0")
         !    end
         ! end
         ! stdout.flush
         ! stdout.show("e         =",e)
         ! stdout.show("iso_value =",.iso_value)
         ! stdout.show("e         =",e)
         ! stdout.show("f1        =",f1)
         ! stdout.show("f2        =",f2)
         end if
         ENSURE(self%iso_value<=L,"MARCHINGCUBE:interpolate_edge_info ... iso value GT endpoint values, "//trim(to_str_(L)))

         ! Edge values
         p1 = self%vertex_positions(:,v1)
         p2 = self%vertex_positions(:,v2)

         ! The gradient and hessian are calculated w.r.t. 
         ! the canonical box axes, NOT the true xyz axes.
         g1 = self%vertex_gradient(:,v1)
         g2 = self%vertex_gradient(:,v2)
         h1 = self%vertex_hessian(:,:,v1)
         h2 = self%vertex_hessian(:,:,v2)

         ! Isovalue difference
         del = self%iso_value-f1

         ! Get del
         bot = f2-f1
         if (bot/=ZERO) then; del = del/bot
         else; del = ZERO
         end if

         ! Interpolate true P & hessian values
         P = p1+del*(p2-p1)
         G = g1+del*(g2-g1)
         H = h1+del*(h2-h1)
         self%edge_vertex_positions(:,e) = P
         self%edge_vertex_gradient(:,e) = G
         self%edge_vertex_hessian(:,:,e) = H
         L = norm_(G)
         ENSURE(L/=ZERO,"MARCHINGCUBE:interpolate_edge_info ... zero gradient, edge: "//trim(to_str_(e)))
         n = G/L

         ! Evaluate surface tangents u,v
         u = [n(2),-n(1),ZERO]
         if (is_zero_(u)) u = [-n(3),ZERO,n(1)]
         call normalize_(u)

         ! If we have reversed the normal, v must be reversed.
         ! Otherwise we would have a left handed system
         v = cross_(n,u)
         UV(:,1) = u
         UV(:,2) = v
         S = -matmul(transpose(UV),matmul(H,UV))/L

         ! Set curvatures
         self%edge_mean_curvature(e) = HALF*(S(1,1)+S(2,2))
         self%edge_gaussian_curvature(e) = S(1,1)*S(2,2)-S(1,2)*S(1,2)
      end do
   end subroutine

   PURE subroutine set_triangle_vertex_info(self)
   ! This routine sets the triangle vertex indices. The coordinates of the
   ! vertices, gradients, etc. are extracted using the get routines above.
   ! If some vertics have been explicitly skipped then the .edge_vertex_indices
   ! had better be supplied! The new vertices are incremented from the current
   ! value of .n_pt which can be externally set before calling this routine.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT :: n,e,v,skip,i
      skip = self%skip_bit_string
      i = 0
      do n = 1,self%n_triangle
         do v = 1,3
            e = self%triangle_edge_index(v,n)
            if (BIT_SET_AT_POS_IN(skip,e)) then

            ! Edge vertex info exists, so use it
               self%triangle_vertex_index(v,n) = self%edge_vertex_index(e)
               ENSURE(self%edge_vertex_index(e)/=0,"MARCHINGCUBE:set_triangle_vertex_info ... zero index, edge: "//trim(to_str_(e)))
            else

            ! No edge vertex info, new point
               i = i+1
               self%n_pt = self%n_pt+1
               self%triangle_vertex_index(v,n) = self%n_pt
               self%edge_vertex_index(e) = self%n_pt
               self%vertex_edge_index(i) = e

            ! Skip this edge next time
               skip = SET_BIT_AT_POS_IN(skip,e)
            end if
         end do
      end do
   end subroutine

   PURE subroutine interpolate_oblique_faces(self)
   ! This routine interpolates the triangle coordinates that result from the
   ! intersection of this cube with the isosurface. Any previously obtained
   ! information abount adjoining cubes to the left, infront, or below this cube
   ! should have already been set, e.g. using .set_left_info. This routine
   ! assumes that cubes are generated in an order from left to right (along the
   ! x-axis), front-to back (along the y-axis), and bottom to top (along the
   ! z-axis). NOTE: the .case number must already have been set.
      type(MARCHINGCUBE_TYPE), INOUT :: self

      ! Skip some edges
      call interpolate_oblique_edge_info_(self)

      ! Set interpolated vertices
      call set_triangle_vertex_info_(self)
   end subroutine

   PURE subroutine interpolate_oblique_edge_info(self)
   ! This routine interpolates edge_vertex_pos, edge_vertex_gradient,
   ! edge_vertex_hessian, edge_mean_curvature, edge_gaussian_curvature.
   ! The edge vertex positions are later to become the triangle vertices.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      INT :: e,v1,v2
      REAL :: f1,f2,del
      VEC(REAL,3) :: p1,p2,P,g1,g2,G
      self%n_active_edge = 0
      do e = 0,11

         ! Skip? This edge does not intersect
         if (NOT BIT_SET_AT_POS_IN(self%edge_bit_string,e)) cycle

         ! Skip? This edge is explicitly skipped
         if (BIT_SET_AT_POS_IN(self%skip_bit_string,e)) cycle
         self%n_active_edge = self%n_active_edge+1

         ! Vertices
         v1 = vertex_1_of_edge(e)
         v2 = vertex_2_of_edge(e)

         ! Values at vertices
         f1 = self%vertex_fn_value(v1)
         f2 = self%vertex_fn_value(v2)
         ENSURE(self%iso_value>=min(f1,f2),"MARCHINGCUBE:interpolate_oblique_edge_info ... iso_value LT endpoint values")
         ENSURE(self%iso_value<=max(f1,f2),"MARCHINGCUBE:interpolate_oblique_edge_info ... iso_value GT endpoint values")

         ! Edge values
         p1 = self%vertex_positions(:,v1)
         p2 = self%vertex_positions(:,v2)

         ! The gradient and hessian are calculated
         ! w.r.t. the canonical box axes
         g1 = self%vertex_gradient(:,v1)
         g2 = self%vertex_gradient(:,v2)

         ! Get del: f2 cannot equal f1
         del = (self%iso_value-f1)/(f2-f1)

         ! Interpolate P & G
         P = p1+del*(p2-p1)
         G = g1+del*(g2-g1)

         ! Change axes
         G = matmul(G,self%inverse_axes)

         ! Set surface values
         self%edge_vertex_positions(:,e) = P
         self%edge_vertex_gradient(:,e) = G
         self%edge_mean_curvature(e) = ZERO
         self%edge_gaussian_curvature(e) = ZERO
      end do
   end subroutine

   PURE subroutine triangulate(self,below,left,front)
   ! This routine generates the triangle coordinates that result from the
   ! intersection of this cube with the isosurface, using information from
   ! the adjoining cube "below", to the "left" of, and "front" of this cube,
   ! where that information is supplied. This assumes that cubes are generated
   ! in an order from left to right (along the x-axis), then from front-to back
   ! (along the y-axis), and finally from bottom to top (along the z-axis).
   ! NOTE: the .case number must have already been set by .set_case_info
      type(MARCHINGCUBE_TYPE), INOUT :: self
      type(EVEC_INT_TYPE), optional, IN :: below,left,front

      ! Skip if cube is entirely in/out of the surface
      if (NOT is_on_surface_(self)) return

      ! Initialize
      call set_triangulation_info_(self)
      call set_skip_bit_string_(self,below,left,front)

      ! Fill in details for skipped edges
      call set_edge_vertex_info_(self,below,left,front)

      ! Interpolate 
      call interpolate_faces_(self)
   end subroutine

   PURE subroutine set_skip_bit_string(self,below,left,front)
   ! Set the skip bit string based on whether there is any information from
   ! the adjoining cube "below", to the "left", or in "front" of this cube.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      type(EVEC_INT_TYPE), optional, IN :: below,left,front
      INT :: skip
      skip = 0

      ! This skips the bottom face edges: 0,1,2,3
      if (present(below)) skip = 15

      ! This skips the left face edges: 3,7,8,11
      if (present(left)) skip = ior(skip,2440)

      ! This skips the front face edges: 0,4,8,9
      if (present(front)) skip = ior(skip,785)
      self%skip_bit_string = skip
   end subroutine

   PURE subroutine set_edge_vertex_info(self,below,left,front)
   ! This routine gets the triangle vertex positions using information from
   ! the adjoining cube "below", to the "left", or in "front" of this cube, if
   ! that information is available.
   ! NOTE: this only sets the edge_vertex index information.
      type(MARCHINGCUBE_TYPE), INOUT :: self
      type(EVEC_INT_TYPE), optional, IN :: below,left,front
      INT :: edge
      edge = self%edge_bit_string
      if (has_any_bits_set_at_(edge,[0,1,2,3]) AND present(below)) then
         self%edge_vertex_index(0) = below%element(4)
         self%edge_vertex_index(1) = below%element(5)
         self%edge_vertex_index(2) = below%element(6)
         self%edge_vertex_index(3) = below%element(7)
      end if
      if (has_any_bits_set_at_(edge,[3,7,8,11]) AND present(left)) then
         self%edge_vertex_index(3) = left%element(1)
         self%edge_vertex_index(7) = left%element(5)
         self%edge_vertex_index(8) = left%element(9)
         self%edge_vertex_index(11) = left%element(10)
      end if
      if (has_any_bits_set_at_(edge,[0,4,8,9]) AND present(front)) then
         self%edge_vertex_index(0) = front%element(2)
         self%edge_vertex_index(4) = front%element(6)
         self%edge_vertex_index(8) = front%element(11)
         self%edge_vertex_index(9) = front%element(10)
      end if
   end subroutine


!   interpolate_zero_flux_faces(axes)
!   ! This routine interpolates the triangle coordinates that result from the
!   ! intersection of this cube with the isosurface. Any previously obtained
!   ! information abount adjoining cubes to the left, infront, or below this cube
!   ! should have already been set, e.g. using .set_left_info. This routine
!   ! assumes that cubes are generated in an order from left to right (along the
!   ! x-axis), front-to back (along the y-axis), and bottom to top (along the
!   ! z-axis). NOTE: the .case number must already have been set.
!      axes :: MAT{REAL}(3,3)
!      i,e,v :: INT
!      .interpolate_zero_flux_edges(axes)
!      .case = .case_number(.edge_bit_string)
!      if (.case<0) then
!         n_fail = n_fail + 1
!           if (once==1) once = 0
!         WARN("Failed triangulation #"//n_fail.to_str.trim)
!         stdout.show_bit_string("edge_bit_string =",.edge_bit_string)
!       ! .put
!         .edge_bit_string = 0
!       ! return
!         .n_triangle = .edge_bit_string.bit_count
!         i = 0
!         do e = 0,11
!            if (NOT BIT_SET_AT_POS_IN(.edge_bit_string,e)) cycle
!            .n_pt = .n_pt + 1
!            i = i + 1
!            .vertex_edge_index(i) = e
!            .edge_vertex_index(e) = 0 ! not used anyway
!            do v = 1,3
!               .triangle_vertex_index(v,i) = .n_pt
!            end
!         end
!      else if (.case>0) then
!         .n_triangle = .no_of_triangles(.case)
!         .triangle_edge_index = reshape(triangle_table(0:14,.case), [3,5])
!         .set_triangle_vertex_info
!           if (once==1) then
!              once = 2
!              .put
!           end
!      end
!   end

!   interpolate_zero_flux_edges(axes)
!   ! This routine interpolates the required edge_vetrices to get the points
!   ! where the hessian has a true inflection point, for use in obtaining the
!   ! zero flux surfaces. These points are later to become the triangle vertices.
!   ! All edges must be tested unless explicitly requested to skip. If it passes
!   ! the test, then the two vertices are set in the .case string. After that, we
!   ! can set the .edge_bit_string. This is a bit backwards compared to the
!   ! normal isosurface case where we can tell right away whether a certain cube
!   ! is on the surface or not. NOTE that we must know what the edge_bit_string
!   ! is, so we cannot skip any interpolation calculations. However, if the
!   ! .skip_bit_string is set, then we should use it to extract an earlier
!   ! interpolated result.
!      axes :: MAT{REAL}(3,3)
!      e,v1,v2 :: INT
!      f1,f2,n11,n22,n12,n21, a2,b,c,r,l1,l2,t1,t2 :: REAL
!      p1,p2,g1,g2,g,n,n1,n2,h11,h22,h12,h21 :: VEC{REAL}(3)
!      h1,h2,h :: MAT{REAL}(3,3)
!      l1_found,l2_found,found :: BIN
!  ! stdout.text("MARCHING CUBE")
!      .edge_bit_string = 0
!      do e = 0,11
!        v1 = vertex_1_of_edge(e)
!        v2 = vertex_2_of_edge(e)
!        p1 = .vertex_positions(:,v1)
!        p2 = .vertex_positions(:,v2)
!        f1 = .vertex_fn_value(v1)
!        f2 = .vertex_fn_value(v2)
!        g1 = .vertex_gradient(:,v1)   ! The gradient and hessian are NOT
!        g2 = .vertex_gradient(:,v2)   ! w.r.t. the canonical box axes here
!        h1 = .vertex_hessian(:,:,v1)
!        h2 = .vertex_hessian(:,:,v2)
!        if (f1<TOL(7)) cycle
!        if (f2<TOL(7)) cycle
!        if (all(abs(g1)<TOL(7))) cycle
!        if (all(abs(g2)<TOL(7))) cycle
!        if (all(abs(h1)<TOL(7))) cycle
!        if (all(abs(h2)<TOL(7))) cycle
!      ! if (any(abs(p1)>FIVE)) cycle
!      ! if (any(abs(p2)>FIVE)) cycle
!        h11 = matmul(h1,g1)
!        h22 = matmul(h2,g2)
!        h12 = matmul(h1,g2)
!        h21 = matmul(h2,g1)
!        n = g1.cross(g2) ! always orthogonal to any g interpolant
!        n.normalize
!        n11 = dot_product(n,h11); n22 = dot_product(n,h22)
!        n12 = dot_product(n,h12); n21 = dot_product(n,h21)
!        a2= TWO*(n11 + n22 - n12 - n21)
!        b = -TWO*n11 + n12 + n21
!        c = n11
!        r  = b*b - TWO*a2*c
!        if (a2==ZERO) cycle
!        if (r<ZERO) cycle
!        r  = sqrt(r)
!        l1 = (-b+r)/a2
!        l2 = (-b-r)/a2
!        l1_found = l1>=ZERO AND l1<=ONE
!        l2_found = l2>=ZERO AND l2<=ONE
!        found = FALSE
!        if (l1_found) then
!           WARN_IF(l2_found,"Both found")
!           t1 = (ONE-l1); t2 = l1
!           ! check if second direction is zero
!           g = t1*g1 + t2*g2
!           h = t1*h1 + t2*h2
!           n1 = g
!           n1.normalize
!           n2 = n.cross(n1)
!           n11 = dot_product(n1,matmul(h,n1))
!           n21 = dot_product(n2,matmul(h,n1))
!           if (n21<=TOL(1)*n11) then
!            ! h.solve_eigenproblem(eval,evec)
!            ! if (all(eval<ZERO)) cycle
!              found = TRUE
!           end
!        end
!        if (NOT found AND l2_found) then
!           t1 = (ONE-l2); t2 = l2
!           ! check if second direction is zero
!           g = t1*g1 + t2*g2
!           h = t1*h1 + t2*h2
!           n1 = g
!           n1.normalize
!           n2 = n.cross(n1)
!           n11 = dot_product(n1,matmul(h,n1))
!           n21 = dot_product(n2,matmul(h,n1))
!           if (n21<=TOL(1)*n11) then
!            ! h.solve_eigenproblem(eval,evec)
!            ! if (all(eval<ZERO)) cycle
!              found = TRUE
!           end
!        end
!        if (found) then
!           .edge_bit_string = ibset(.edge_bit_string,e)
!           .edge_vertex_positions(:,e)       = t1*p1 + t2*p2
!           .edge_vertex_value(e)       = t1*f1 + t2*f2
!           .edge_vertex_gradient(:,e)  = g
!           .edge_vertex_hessian(:,:,e) = h
!        end
!      end
!   end

!   interpolate_zero_flux_edges(axes)
!   ! This routine interpolates the required edge_vetrices to get the points
!   ! where the hessian has a true inflection point, for use in obtaining the
!   ! zero flux surfaces. These points are later to become the triangle vertices.
!   ! All edges must be tested unless explicitly requested to skip. If it passes
!   ! the test, then the two vertices are set in the .case string. After that, we
!   ! can set the .edge_bit_string. This is a bit backwards compared to the
!   ! normal isosurface case where we can tell right away whether a certain cube
!   ! is on the surface or not. NOTE that we must know what the edge_bit_string
!   ! is, so we cannot skip any interpolation calculations. However, if the
!   ! .skip_bit_string is set, then we should use it to extract an earlier
!   ! interpolated result.
!      axes :: MAT{REAL}(3,3)
!      e,v1,v2, i,im  :: INT
!      f1,f2,s,sm,t1,t2 :: REAL
!      p1,p2,g1,g2,g,d, eval :: VEC{REAL}(3)
!      h1,h2,h, evec :: MAT{REAL}(3,3)
!  ! stdout.text("MARCHING CUBE")
!      .edge_bit_string = 0
!      do e = 0,11
!        v1 = vertex_1_of_edge(e)
!        v2 = vertex_2_of_edge(e)
!        p1 = .vertex_positions(:,v1)
!        p2 = .vertex_positions(:,v2)
!        f1 = .vertex_fn_value(v1)
!        f2 = .vertex_fn_value(v2)
!        g1 = .vertex_gradient(:,v1)   ! The gradient and hessian are NOT
!        g2 = .vertex_gradient(:,v2)   ! w.r.t. the canonical box axes here
!        h1 = .vertex_hessian(:,:,v1)
!        h2 = .vertex_hessian(:,:,v2)
!        if (f1<TOL(7)) cycle
!        if (f2<TOL(7)) cycle
!        if (all(abs(g1)<TOL(7))) cycle
!        if (all(abs(g2)<TOL(7))) cycle
!        if (all(abs(h1)<TOL(7))) cycle
!        if (all(abs(h2)<TOL(7))) cycle
!      ! if (any(abs(p1)>FIVE)) cycle
!      ! if (any(abs(p2)>FIVE)) cycle
!        d  = cube(:,v2) - cube(:,v1)
!        d = d*.side_length
!        d  = matmul(axes,d)           ! Change cube edge to real axis frame
!        sm = huge(ONE)
!        do i = 0,50
!           s = .hessian_x_gradient(i/(FIVE*TEN),g1,g2,h1,h2,d)
!           if (s>=sm) cycle
!           sm = s
!           im = i
!        end
!        if (sm>TOL(5)) cycle
!        t2 = im/(FIVE*TEN)
!        t1 = ONE - t2
!        .edge_bit_string = ibset(.edge_bit_string,e)
!        .edge_vertex_positions(:,e)       = t1*p1 + t2*p2
!        .edge_vertex_value(e)       = t1*f1 + t2*f2
!        .edge_vertex_gradient(:,e)  = t1*g1 + t2*g2
!        .edge_vertex_hessian(:,:,e) = t1*h1 + t2*h2
!      end
!   end

!   hessian_x_gradient(lambda,g1,g2,h1,h2,d) result (res)
!   ! For testing if the gardient is an eigenvector of the hessian
!      lambda,res :: REAL
!      g1,g2,d :: VEC{REAL}(3)
!      h1,h2 :: MAT{REAL}(3,3)
!      t1,t2 :: REAL
!      g,dg,t :: VEC{REAL}(3)
!      h :: MAT{REAL}(3,3)
!      t2 = lambda
!      t1 = ONE - t2
!      h  = t1*h1 + t2*h2
!      dg = matmul((h1-h2),d)
!      g  = t1*g1 + t2*g2 + t1*t2*dg
!      t  = matmul(h,g)/g
!      res = (t(2)-t(1))==2 + (t(3)-t(2))==2 + (t(1)-t(3))==2
!   end

!   interpolate_zero_flux_edges(axes)
!   ! This routine interpolates the required edge_vetrices to get the points
!   ! where the hessian has a true inflection point, for use in obtaining the
!   ! zero flux surfaces. These points are later to become the triangle vertices.
!   ! All edges must be tested unless explicitly requested to skip. If it passes
!   ! the test, then the two vertices are set in the .case string. After that, we
!   ! can set the .edge_bit_string. This is a bit backwards compared to the
!   ! normal isosurface case where we can tell right away whether a certain cube
!   ! is on the surface or not. NOTE that we must know what the edge_bit_string
!   ! is, so we cannot skip any interpolation calculations. However, if the
!   ! .skip_bit_string is set, then we should use it to extract an earlier
!   ! interpolated result.
!      axes :: MAT{REAL}(3,3)
!      e,v1,v2, w,fnd :: INT
!      f1,f2,df,fa,fb,ga,gb,dg,ha,hb,dh, a,b,c,a2,r,l1,l2,f :: REAL
!      p1,p2,g1,g2,d,da,db :: VEC{REAL}(3)
!      h1,h2  :: MAT{REAL}(3,3)
!      l1_found,l2_found,is_minimum :: BIN
!  ! stdout.text("MARCHING CUBE")
!      .edge_bit_string = 0
!      do e = 0,11
!        v1 = vertex_1_of_edge(e)
!        v2 = vertex_2_of_edge(e)
!        p1 = .vertex_positions(:,v1)
!        p2 = .vertex_positions(:,v2)
!        f1 = .vertex_fn_value(v1)
!        f2 = .vertex_fn_value(v2)
!        g1 = .vertex_gradient(:,v1)   ! The gradient and hessian are NOT
!        g2 = .vertex_gradient(:,v2)   ! w.r.t. the canonical box axes here
!        h1 = .vertex_hessian(:,:,v1)
!        h2 = .vertex_hessian(:,:,v2)
!        if (f1<TOL(4)) cycle
!        if (f2<TOL(4)) cycle
!        if (all(abs(g1)<TOL(5))) cycle
!        if (all(abs(g2)<TOL(5))) cycle
!        if (all(abs(h1)<TOL(6))) cycle
!        if (all(abs(h2)<TOL(6))) cycle
!        if (any(abs(p1)>FIVE)) cycle
!        if (any(abs(p2)>FIVE)) cycle
!        d  = cube(:,v2) - cube(:,v1)
!        d = d*.side_length
!        d  = matmul(axes,d)           ! Change cube edge to real axis frame
!        fa = f1
!        fb = f2
!        ga = dot_product(g1,d)
!        gb = dot_product(g2,d)
!        da = matmul(h1,d)
!        db = matmul(h2,d)
!        ha = dot_product(d,da)
!        hb = dot_product(d,db)
!        df = fb - fa
!        dg = gb - ga
!        dh = hb - ha
!        c  = df - dg + HALF*hb
!        b  = TWO*dg - dh - hb
!        a2 = THREE*dh
!        r  = b*b - TWO*a2*c
!        if (a2==ZERO) cycle
!        if (r<ZERO) cycle
!        r  = sqrt(r)
!        l1 = (-b+r)/a2
!        l2 = (-b-r)/a2
!        l1_found = l1>=ZERO AND l1<=ONE
!        l2_found = l2>=ZERO AND l2<=ONE
!        if (l1_found AND l2_found) then
!           a = ONE-l1; b = l1
!           r = a*fa + b*fb + a*b*dg + HALF*a*b*(b*ha+a*hb)
!           a = ONE-l2; b = l2
!           f = a*fa + b*fb + a*b*dg + HALF*a*b*(b*ha+a*hb)
!           if (r<f) then
!           a = ONE-l1; b = l1
!           f = a*fa + b*fb + a*b*dg + HALF*a*b*(b*ha+a*hb)
!           end
!        else if (l1_found) then
!           a = ONE-l1; b = l1; f = a*f1 + b*f2
!           f = a*fa + b*fb + a*b*dg + HALF*a*b*(b*ha+a*hb)
!        else if (l2_found) then
!           a = ONE-l2; b = l2; f = a*f1 + b*f2
!           f = a*fa + b*fb + a*b*dg + HALF*a*b*(b*ha+a*hb)
!        end
!        is_minimum = f<f1 AND f<f2
!        if (is_minimum AND (l1_found OR l2_found)) then
!           .edge_bit_string = ibset(.edge_bit_string,e)
!           .edge_vertex_positions(:,e)       = a*p1 + b*p2
!           .edge_vertex_value(e)       = f
!           .edge_vertex_gradient(:,e)  = a*(g1+b*da) + b*(g2-a*db)
!           .edge_vertex_hessian(:,:,e) = a*h1 + b*h2
!           if (once==0) then
!              once = 1
!           end
!         ! H.solve_eigenproblem(eval,evec)
!         ! if (any(eval)>ZERO) then
!         ! else
!         ! end
!        end
!      end
!   end

!   interpolate_zero_flux_edges
!   ! This routine interpolates the required edge_vetrices to get the points
!   ! where the hessian has a true inflection point, for use in obtaining the
!   ! zero flux surfaces. These points are later to become the triangle vertices.
!   ! All edges must be tested unless explicitly requested to skip. If it passes
!   ! the test, then the two vertices are set in the .case string. After that, we
!   ! can set the .edge_bit_string. This is a bit backwards compared to the
!   ! normal isosurface case where we can tell right away whether a certain cube
!   ! is on the surface or not. NOTE that we must know what the edge_bit_string
!   ! is, so we cannot skip any interpolation calculations. However, if the
!   ! .skip_bit_string is set, then we should use it to extract an earlier
!   ! interpolated result.
!      e,v1,v2, w,fnd :: INT
!      del, L :: REAL
!      d1,d2,e1,e2 :: REAL
!      p1,p2,P,g1,g2,G,n,u,v :: VEC{REAL}(3)
!      h1,h2, H  :: MAT{REAL}(3,3)
!      S  :: MAT{REAL}(2,2)
!      UV :: MAT{REAL}(3,2)
!  ! stdout.text("MARCHING CUBE")
!      .edge_bit_string = 0
!      do e = 0,11
!        v1 = vertex_1_of_edge(e)
!        v2 = vertex_2_of_edge(e)
!        p1 = .vertex_positions(:,v1)
!        p2 = .vertex_positions(:,v2)
!        g1 = .vertex_gradient(:,v1)   ! The gradient and hessian are calculated
!        g2 = .vertex_gradient(:,v2)   ! w.r.t. the canonical box axes, NOT the
!        h1 = .vertex_hessian(:,:,v1)  ! true xyz axes.
!        h2 = .vertex_hessian(:,:,v2)
!        w  = xyz_axis_of_edge(e)
!        d1 = g1(w)
!        d2 = g2(w)
!        e1 = .vertex_hessian(w,w,v1)
!        e2 = .vertex_hessian(w,w,v2)
!        fnd = 0
!        if (d1*d2<ZERO AND e1>ZERO AND e2>ZERO) then
!           .edge_bit_string = ibset(.edge_bit_string,e)
!           del = -d1/(d2-d1)
!           fnd = 1
!        end
!  ! if (fnd>0) then
!  ! stdout.show("edge =",e)
!  ! stdout.show("fnd  =",fnd)
!  ! stdout.show("p1 =",p1)
!  ! stdout.show("p2 =",p2)
!  ! stdout.show("g1 =",g1)
!  ! stdout.show("g2 =",g2)
!  ! stdout.show("w  =",w)
!  ! stdout.show("d1 =",d1)
!  ! stdout.show("d2 =",d2)
!  ! stdout.show("e1 =",e1)
!  ! stdout.show("e2 =",e2)
!  ! end
!        P = p1 + del*(p2-p1)          ! Interpolate true P
!        H = h1 + del*(h2-h1)          ! Interpolate hessian values
!        G = g1 + del*(g2-g1)          ! ... and gradient G
!        .edge_vertex_positions(:,e)       = P
!        .edge_vertex_gradient(:,e)  = G
!        .edge_vertex_hessian(:,:,e) = H
!        if (G.is_zero) then
!           .edge_mean_curvature(e) = ZERO
!           .edge_gaussian_curvature(e) = ZERO
!        else
!           L = G.norm
!           n = G/L
!           u = [ n(2),-n(1), ZERO]       ! Evaluate surface tangents u,v
!           if (u.is_zero) u = [-n(3), ZERO, n(1)]
!           u.normalize
!           v = n.cross(u)
!           UV(:,1) = u
!           UV(:,2) = v
!           S = -matmul(transpose(UV),matmul(H,UV))/L
!           .edge_mean_curvature(e) = HALF*(S(1,1)+S(2,2))
!           .edge_gaussian_curvature(e) = S(1,1)*S(2,2)-S(1,2)*S(1,2)
!        end
!      end
!   end

!  ================
!  Inquiry routines
!  ================

   PURE function case_number(self) result (res)
   ! From the vertex values on the cube evaluate and return the case number
   ! telling us which vertices are larger than the .iso_value.
      type(MARCHINGCUBE_TYPE), IN :: self
      INT :: res
      res = 0
      if (self%vertex_fn_value(0)<self%iso_value) res = SET_BIT_AT_POS_IN(res,0)
      if (self%vertex_fn_value(1)<self%iso_value) res = SET_BIT_AT_POS_IN(res,1)
      if (self%vertex_fn_value(2)<self%iso_value) res = SET_BIT_AT_POS_IN(res,2)
      if (self%vertex_fn_value(3)<self%iso_value) res = SET_BIT_AT_POS_IN(res,3)
      if (self%vertex_fn_value(4)<self%iso_value) res = SET_BIT_AT_POS_IN(res,4)
      if (self%vertex_fn_value(5)<self%iso_value) res = SET_BIT_AT_POS_IN(res,5)
      if (self%vertex_fn_value(6)<self%iso_value) res = SET_BIT_AT_POS_IN(res,6)
      if (self%vertex_fn_value(7)<self%iso_value) res = SET_BIT_AT_POS_IN(res,7)
   end function

   PURE function is_on_surface(self) result (res)
   ! Return TRUE if the cube is on the surface. NOTE: .case number must be
   ! properly set beforehand with .set_case_info
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res
      res = self%case/=0 AND self%case/=255
   end function

   PURE function is_outside_surface(self) result (res)
   ! Return TRUE if the cube is wholly outside the surface. Usually the interior
   ! of the isosurface has function values *smaller* than the .iso_value.
   ! NOTE: .case number must be properly set beforehand with .set_case_info
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res
      res = self%case==self%exterior_case
   end function

   PURE function is_inside_surface(self) result (res)
   ! Return TRUE if the cube is wholly inside the surface. Usually the interior
   ! of the isosurface has function values *bigger* than the .iso_value.
   ! NOTE: .case number must be properly set beforehand with .set_case_info
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res
      res = self%case==self%interior_case
   end function

   PURE function is_nearly_on_surface(self,tol) result (res)
   ! Return TRUE if the cube is nearly on the surface. This is worked out by
   ! taking the maximum difference between any two vertices of the cube, and
   ! then seeing if the .iso_value is within any one vertex value, plus or minus
   ! this maximum difference, times a factor "tol" for good measure.
      type(MARCHINGCUBE_TYPE), IN :: self
      REAL, IN :: tol
      BIN :: res
      REAL :: diff
   ENSURE(tol>ZERO,"MARCHINGCUBE:is_nearly_on_surface ... tol must be positive")
      res = is_on_surface_(self)
      if (res) return
      diff = maxval(self%vertex_fn_value)-minval(self%vertex_fn_value)
      diff = diff*tol
      res = any(abs(self%vertex_fn_value-self%iso_value)<diff)
   end function

   PURE function is_nearly_on_surface_old(self,tol) result (res)
   ! Return TRUE if the cube is nearly on the surface, to within a factor "tol"
   ! of the .iso_value.
      type(MARCHINGCUBE_TYPE), IN :: self
      REAL, IN :: tol
      BIN :: res
   ENSURE(tol>ZERO,"MARCHINGCUBE:is_nearly_on_surface_old ... tol must be positive")
      res = is_on_surface_(self)
      if (res) return
      res = any(abs(self%vertex_fn_value-self%iso_value)<tol)
   end function

   PURE function has_left_face_on_surface(self) result (res)
   ! Returns TRUE if the left face is on the surface.
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res

      ! This tests left face edges: 3,7,8,11
      res = has_any_bits_set_at_(self%edge_bit_string,[3,7,8,11])
   end function

   PURE function has_front_face_on_surface(self) result (res)
   ! Returns TRUE if the front face is on the surface.
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res

      ! This tests front face edges: 0,4,8,9
      res = has_any_bits_set_at_(self%edge_bit_string,[0,4,8,9])
   end function

   PURE function has_lower_face_on_surface(self) result (res)
   ! Returns TRUE if the bottom face is on the surface.
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res

      ! This tests bottom face edges: 0,1,2,3
      res = has_any_bits_set_at_(self%edge_bit_string,[0,1,2,3])
   end function

   PURE function has_right_face_on_surface(self) result (res)
   ! Returns TRUE if the right face is on the surface.
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res

      ! This tests right face edges: 1,5,9,10
      res = has_any_bits_set_at_(self%edge_bit_string,[1,5,9,10])
   end function

   PURE function has_back_face_on_surface(self) result (res)
   ! Returns TRUE if the back face is on the surface.
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res

      ! this tests back face edges: 2,6,10,11
      res = has_any_bits_set_at_(self%edge_bit_string,[2,6,10,11])
   end function

   PURE function has_upper_face_on_surface(self) result (res)
   ! Returns TRUE if the upper face is on the surface.
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res

      ! This tests upper face edges: 4,5,6,7
      res = has_any_bits_set_at_(self%edge_bit_string,[4,5,6,7])
   end function

   PURE function has_reusable_edge_vertices(self) result (res)
   ! Returns there are any reasuable vertices
      type(MARCHINGCUBE_TYPE), IN :: self
      BIN :: res
      res = any(self%edge_vertex_index>0)
   end function


!   has_reusable_edge_vertices(indices) result (res)
!   ! Returns TRUE if the right, back, or upper faces are on the surface.
!      indices :: VEC{INT}(0:11), IN
!      res :: BIN
!      res = any(indices>0)
!   end
!
!   no_back_face_indices_in(edge_indices) result (res)
!   ! Returns TRUE if the "edge_indices" array has zero values in positions
!   ! corresponding to back face edges of the cube.
!      edge_indices :: VEC{INT}(0:11)
!      res :: BIN
!      ! this tests back face edges: 2,6,10,11

!            edge_indices(11) == 0
!   end
!
!   no_upper_face_indices_in(edge_indices) result (res)
!   ! Returns TRUE if the "edge_indices" array has zero values in positions
!   ! corresponding to upper face edges of the cube.
!      edge_indices :: VEC{INT}(0:11)
!      res :: BIN
!      ! this tests upper face edges: 4,5,6,7

!            edge_indices(7) == 0
!   end

!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put the list of vertices for the object
      type(MARCHINGCUBE_TYPE), IN :: self
      call put_bitmask_info_(self)
      call put_vertex_index_info_(self)
      call put_positional_info_(self)
   end subroutine

   subroutine put_bitmask_info(self)
   ! Put the bit mask related info for the object.
      type(MARCHINGCUBE_TYPE), IN :: self
      call flush_(stdout)
      call show_(stdout,"iso_value       =",self%iso_value)
      call show_(stdout,"accuracy        =",self%accuracy)
      call show_(stdout,"case #          =",self%case)
      call show_bit_string_(stdout,"case bit_string =",self%case)
      call show_bit_string_(stdout,"edge_bit_string =",self%edge_bit_string)
      call show_bit_string_(stdout,"cube_bit_string =",self%cube_bit_string)
      call show_bit_string_(stdout,"skip_bit_string =",self%skip_bit_string)
      call show_(stdout,"left  face on?  =",has_left_face_on_surface_(self))
      call show_(stdout,"right face on?  =",has_right_face_on_surface_(self))
      call show_(stdout,"front face on?  =",has_front_face_on_surface_(self))
      call show_(stdout,"back  face on?  =",has_back_face_on_surface_(self))
      call show_(stdout,"lower face on?  =",has_lower_face_on_surface_(self))
      call show_(stdout,"upper face on?  =",has_upper_face_on_surface_(self))
      call show_(stdout,"n_edge          =",no_of_edges_(self))
      call show_(stdout,"n_active_edges  =",no_of_active_edges_(self))
      call show_(stdout,"n_triangle      =",self%n_triangle)
      call show_(stdout,"no_of_triangles =",no_of_triangles_(self))
      call show_(stdout,"n_pt            =",self%n_pt)
   end subroutine

   subroutine put_vertex_index_info(self)
   ! Put the list of indexc related entites.
      type(MARCHINGCUBE_TYPE), IN :: self
      if (self%n_triangle>0) then
         call flush_(stdout)
         call text_(stdout,"Triangle edge index:")
         call put_(stdout,self%triangle_edge_index(:,1:self%n_triangle),by_column=TRUE)
         call text_(stdout,"Triangle vertex index:")
         call put_(stdout,self%triangle_vertex_index(:,1:self%n_triangle),by_column=TRUE)
         call text_(stdout,"Edge vertex indices:")
         call put_(stdout,self%edge_vertex_index)
      end if
   end subroutine

   subroutine put_positional_info(self)
   ! Put the list of position and function related entities.
      type(MARCHINGCUBE_TYPE), IN :: self
      INT :: i
      call flush_(stdout)
      call text_(stdout,"Vertex positons:")
      call put_(stdout,self%vertex_positions,by_column=TRUE)
      call text_(stdout,"Values at each vertex:")
      call put_(stdout,self%vertex_fn_value)
      call text_(stdout,"Vertex gradients:")
      call put_(stdout,self%vertex_gradient,by_column=TRUE)
      call text_(stdout,"Vertex hessians:")
      do i = 0,7
         call show_(stdout,"edge =",i+1)
         call put_(stdout,self%vertex_hessian(:,:,i))
      end do
      if (self%n_triangle>0) then
         call text_(stdout,"Edge vertex positions:")
         call put_(stdout,self%edge_vertex_positions,by_column=TRUE)
         call text_(stdout,"Edge vertex values:")
         call put_(stdout,self%edge_vertex_value)
         call text_(stdout,"Edge vertex gradients:")
         call put_(stdout,self%edge_vertex_gradient,by_column=TRUE)
         call text_(stdout,"Edge vertex hessians:")
         do i = 0,11
            call show_(stdout,"edge =",i+1)
            call put_(stdout,self%edge_vertex_hessian(:,:,i))
         end do
      end if
   end subroutine

end module
