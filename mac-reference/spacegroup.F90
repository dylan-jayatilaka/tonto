!=====================================================================
!
! SPACEGROUP: Crystal space group object
!
! Synopsis
!
! Read in the international symmetry symbol (an integer or qualified
! integer) or Hermann-Mauguin symbol, or Hall symbol and make the
! Seitz matrices for the specified spacegroup. This modules provide
! other crystal symmetry information as well. Use the method in: S. R.
! Hall, Acta Cryst A37, 517 (1981) and better, International Tables B
! section A1.4.2.3, p. 112
!
! The code has been updated to the latest version of the Hall notation
! described in the international tables, including explicit origin
! notation.
!
!
! Copyright (C) S K Wolff, 1995
! Copyright (C) D Jayatilaka, 1998-2005
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
module SPACEGROUP_MODULE

#  include "macros"

   include "spacegroup.use"

   implicit none

   include "spacegroup.int"


!  ===============
!  Data statements
!  ===============

   ! Implied translation vectors T_implied(:,v,latt)
   ! International tables B, table 1.4
   ! . The first index gives the actual translation vector.
   ! . The second index "v" runs over the number of implied vectors.
   !   The actual number of these is stored in
   !   "n_implied_translations" below.  
   ! . The third index "latt" is the index of the lattice type.
   !   See "lattice_symbols". There are 8.
   STR(len=8), private :: lattice_symbols = "PABCIRHF"
   STR(len=1), private :: bar_symbol = "-"  ! Bar symbol, precedes axis orders
   STR(len=1), private :: dash_symbol = "'"  !
   STR(len=1), private :: double_dash_symbol = '"'  !
   STR(len=1), private :: star_symbol = "*"  !
   VEC(INT,8), private :: n_implied_translations
   data n_implied_translations/ 1, 2, 2, 2, 2, 3, 3, 4/
   MAT3(REAL,3,4,8), private :: T_implied
   data T_implied/ &
        ZERO,      ZERO,      ZERO, &  ! "P" lattice
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "A" lattice
         ZERO,      HALF,      HALF, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "B" lattice
         HALF,      ZERO,      HALF, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "C" lattice
         HALF,      HALF,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "I" lattice
         HALF,      HALF,      HALF, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "R" lattice
   TWOTHIRD,     THIRD,     THIRD, &  !
      THIRD,  TWOTHIRD,  TWOTHIRD, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "H" lattice
   TWOTHIRD,     THIRD,      ZERO, &  !
      THIRD,  TWOTHIRD,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  !
         ZERO,      ZERO,      ZERO, &  ! "F" lattice
         ZERO,      HALF,      HALF, &  !
         HALF,      ZERO,      HALF, &  !
         HALF,      HALF,      ZERO/

   ! Translation vectors. 
   ! See International Tables B, table A1.4.2.3.
   ! . The first index gives the actual vector. 
   ! . The second index is linked t othe translation symbol. 
   ! . NOTE: The numerical symbols are not stored, they can be
   !   calculated from the subscript index and the order. 
   !   See second column of the table mentioned.
   STR(len=6), private :: fractional_translation_symbols = "123456"
   STR(len=8), private :: alpha_translation_symbols = "abcnuvwd"
   STR(len=6), private :: A_axis_direction_symbols = "xyz""'*"  ! Axis setting symbols
   MAT(REAL,3,8), private :: T_translation_vector
   data T_translation_vector/ &
         HALF,    ZERO,    ZERO, &  ! "a" translation
         ZERO,    HALF,    ZERO, &  ! "b" translation
         ZERO,    ZERO,    HALF, &  ! "c" translation
         HALF,    HALF,    HALF, &  ! "n" translation
      QUARTER,    ZERO,    ZERO, &  ! "u" translation
         ZERO, QUARTER,    ZERO, &  ! "v" translation
         ZERO,    ZERO, QUARTER, &  ! "w" translation
      QUARTER, QUARTER, QUARTER/

   ! Principal rotation matrices NN(:,:,axis,i). 
   ! From International Tables B, tables A1.4.2.4, A1.4.2.5, A1.4.2.6.
   ! The "axis" symbol is either 1, 2, or 3 and stands for x, y, or z.
   ! The "i" symbol is the axis order, except when i=5,7 or 8. 
   ! . If i=5, then the matrices are for a twofold
   !   rotation around a dash axis, and "axis" now represents the
   !   *preceding* axis symbol. 
   ! . If i=7 then the matrices are for a twofold rotation around a
   !   double dash axes, with "axis" again representing the
   !   *preceding* axis symbol.  
   ! . If i=8 the rotation is in the * direction, and "axis" should be
   !   a+b+c, and should be set to 1. See NN table below.
   ! Checked 5/5/13 - Dylan
   STR(len=6), private :: N_rotation_order_symbols = "1234#6"
   MAT4(REAL,3,3,3,8), private :: NN
   data NN/ &
       1, 0, 0, 0, 1, 0, 0, 0, 1, & ! N1(:,:,x,1)
      1, 0, 0, 0, 1, 0, 0, 0, 1, & ! N1(:,:,y,1)
      1, 0, 0, 0, 1, 0, 0, 0, 1, & ! N1(:,:,z,1)
      1, 0, 0, 0,-1, 0, 0, 0,-1, & ! N2(:,:,x,2)
      -1, 0, 0, 0, 1, 0, 0, 0,-1, & ! N2(:,:,y,2)
      -1, 0, 0, 0,-1, 0, 0, 0, 1, & ! N2(:,:,z,2)
      -1, 0, 0, 0, 0, 1, 0,-1,-1, & ! N3(:,:,x,3)
      -1, 0,-1, 0, 1, 0, 1, 0, 0, & ! N3(:,:,y,3)
      0, 1, 0,-1,-1, 0, 0, 0, 1, & ! N3(:,:,z,3)
      1, 0, 0, 0, 0, 1, 0,-1, 0, & ! N4(:,:,x,4)
      0, 0,-1, 0, 1, 0, 1, 0, 0, & ! N4(:,:,y,4)
      0, 1, 0,-1, 0, 0, 0, 0, 1, & ! N4(:,:,z,4)
      -1, 0, 0, 0, 0,-1, 0,-1, 0, & ! N'(:,:,x,5) -- dash rotations
      0, 0,-1, 0,-1, 0,-1, 0, 0, & ! N'(:,:,y,5)
      0,-1, 0,-1, 0, 0, 0, 0,-1, & ! N'(:,:,z,5)
      1, 0, 0, 0, 1, 1, 0,-1, 0, & ! N6(:,:,x,6)
      0, 0,-1, 0, 1, 0, 1, 0, 1, & ! N6(:,:,y,6)
      1, 1, 0,-1, 0, 0, 0, 0, 1, & ! N6(:,:,z,6)
      -1, 0, 0, 0, 0, 1, 0, 1, 0, & ! N"(:,:,x,7) -- double dash rotation
      0, 0, 1, 0,-1, 0, 1, 0, 0, & ! N"(:,:,y,7)
      0, 1, 0, 1, 0, 0, 0, 0,-1, & ! N"(:,:,z,7) 
      0, 1, 0, 0, 0, 1, 1, 0, 0, & ! N*(:,:,1,8) -- star rotation
      0, 0, 0, 0, 0, 0, 0, 0, 0, & ! N*(:,:,y,8) -- The last two not used
      0, 0, 0, 0, 0, 0, 0, 0, 0/

   ! Spacegroup conversion table. The first element is the International Table
   ! spacegroup number, followed by an optional colon and specific setting
   ! index. The second column is the Schoenflies pointgroup symbol. The third
   ! column is the Hermann-Mauguin spacegroup symbol. The fourth and final
   ! column is the Hall notation for the spacegroup. That is the only notation
   ! which provides a unique spacegroup specification that distinuishes all
   ! possible settings.
   MAT(STR(len=14),4,593), public :: spacegroup_conversion_table
   data spacegroup_conversion_table(1:4,  1: 30)/ &
      "1             ", "c1^1          ", "p 1           ", "p 1           ", & ! deprecated Hall symbols
      "2             ", "ci^1          ", "p -1          ", "-p 1          ", &
      "3:b           ", "c2^1          ", "p 1 2 1       ", "p 2y          ", &
      "3:b           ", "c2^1          ", "p 2           ", "p 2y          ", &
      "3:c           ", "c2^1          ", "p 1 1 2       ", "p 2           ", &
      "3:a           ", "c2^1          ", "p 2 1 1       ", "p 2x          ", &
      "4:b           ", "c2^2          ", "p 1 21 1      ", "p 2yb         ", &
      "4:b           ", "c2^2          ", "p 1 21 1      ", "p 2y1         ", &
      "4:b           ", "c2^2          ", "p 21          ", "p 2yb         ", &
      "4:c           ", "c2^2          ", "p 1 1 21      ", "p 2c          ", &
      "4:c           ", "c2^2          ", "p 1 1 21      ", "p 21          ", &
      "4:a           ", "c2^2          ", "p 21 1 1      ", "p 2xa         ", &
      "4:a           ", "c2^2          ", "p 21 1 1      ", "p 2x1         ", &
      "5:b1          ", "c2^3          ", "c 1 2 1       ", "c 2y          ", &
      "5:b1          ", "c2^3          ", "c 2           ", "c 2y          ", &
      "5:b2          ", "c2^3          ", "a 1 2 1       ", "a 2y          ", &
      "5:b3          ", "c2^3          ", "i 1 2 1       ", "i 2y          ", &
      "5:c1          ", "c2^3          ", "a 1 1 2       ", "a 2           ", &
      "5:c2          ", "c2^3          ", "b 1 1 2       ", "b 2           ", &
      "5:c3          ", "c2^3          ", "i 1 1 2       ", "i 2           ", &
      "5:a1          ", "c2^3          ", "b 2 1 1       ", "b 2x          ", &
      "5:a2          ", "c2^3          ", "c 2 1 1       ", "c 2x          ", &
      "5:a3          ", "c2^3          ", "i 2 1 1       ", "i 2x          ", &
      "6:b           ", "cs^1          ", "p 1 m 1       ", "p -2y         ", &
      "6:b           ", "cs^1          ", "p m           ", "p -2y         ", &
      "6:c           ", "cs^1          ", "p 1 1 m       ", "p -2          ", &
      "6:a           ", "cs^1          ", "p m 1 1       ", "p -2x         ", &
      "7:b1          ", "cs^2          ", "p 1 c 1       ", "p -2yc        ", &
      "7:b1          ", "cs^2          ", "p c           ", "p -2yc        ", &
      "7:b2          ", "cs^2          ", "p 1 n 1       ", "p -2yac       "/
   data spacegroup_conversion_table(1:4, 31: 60)/ &
      "7:b2          ", "cs^2          ", "p n           ", "p -2yac       ", &
      "7:b3          ", "cs^2          ", "p 1 a 1       ", "p -2ya        ", &
      "7:b3          ", "cs^2          ", "p a           ", "p -2ya        ", &
      "7:c1          ", "cs^2          ", "p 1 1 a       ", "p -2a         ", &
      "7:c2          ", "cs^2          ", "p 1 1 n       ", "p -2ab        ", &
      "7:c3          ", "cs^2          ", "p 1 1 b       ", "p -2b         ", &
      "7:a1          ", "cs^2          ", "p b 1 1       ", "p -2xb        ", &
      "7:a2          ", "cs^2          ", "p n 1 1       ", "p -2xbc       ", &
      "7:a3          ", "cs^2          ", "p c 1 1       ", "p -2xc        ", &
      "8:b1          ", "cs^3          ", "c 1 m 1       ", "c -2y         ", &
      "8:b1          ", "cs^3          ", "c m           ", "c -2y         ", &
      "8:b2          ", "cs^3          ", "a 1 m 1       ", "a -2y         ", &
      "8:b3          ", "cs^3          ", "i 1 m 1       ", "i -2y         ", &
      "8:b3          ", "cs^3          ", "i m           ", "i -2y         ", &
      "8:c1          ", "cs^3          ", "a 1 1 m       ", "a -2          ", &
      "8:c2          ", "cs^3          ", "b 1 1 m       ", "b -2          ", &
      "8:c3          ", "cs^3          ", "i 1 1 m       ", "i -2          ", &
      "8:a1          ", "cs^3          ", "b m 1 1       ", "b -2x         ", &
      "8:a2          ", "cs^3          ", "c m 1 1       ", "c -2x         ", &
      "8:a3          ", "cs^3          ", "i m 1 1       ", "i -2x         ", &
      "9:b1          ", "cs^4          ", "c 1 c 1       ", "c -2yc        ", &
      "9:b1          ", "cs^4          ", "c c           ", "c -2yc        ", &
      "9:b2          ", "cs^4          ", "a 1 n 1       ", "a -2yab       ", & ! a -2yac
      "9:b3          ", "cs^4          ", "i 1 a 1       ", "i -2ya        ", &
      "9:-b1         ", "cs^4          ", "a 1 a 1       ", "a -2ya        ", &
      "9:-b2         ", "cs^4          ", "c 1 n 1       ", "c -2yac       ", & ! c -2ybc
      "9:-b3         ", "cs^4          ", "i 1 c 1       ", "i -2yc        ", &
      "9:c1          ", "cs^4          ", "a 1 1 a       ", "a -2a         ", &
      "9:c2          ", "cs^4          ", "b 1 1 n       ", "b -2ab        ", & ! b -2bc
      "9:c3          ", "cs^4          ", "i 1 1 b       ", "i -2b         "/
   data spacegroup_conversion_table(1:4, 61: 90)/ &
      "9:-c1         ", "cs^4          ", "b 1 1 b       ", "b -2b         ", &
      "9:-c2         ", "cs^4          ", "a 1 1 n       ", "a -2ab        ", & ! a -2ac
      "9:-c3         ", "cs^4          ", "i 1 1 a       ", "i -2a         ", &
      "9:a1          ", "cs^4          ", "b b 1 1       ", "b -2xb        ", &
      "9:a2          ", "cs^4          ", "c n 1 1       ", "c -2xac       ", & ! c -2xbc
      "9:a3          ", "cs^4          ", "i c 1 1       ", "i -2xc        ", &
      "9:-a1         ", "cs^4          ", "c c 1 1       ", "c -2xc        ", &
      "9:-a2         ", "cs^4          ", "b n 1 1       ", "b -2xab       ", & ! b -2xbc
      "9:-a3         ", "cs^4          ", "i b 1 1       ", "i -2xb        ", &
      "10:b          ", "c2h^1         ", "p 1 2/m 1     ", "-p 2y         ", &
      "10:b          ", "c2h^1         ", "p 2/m         ", "-p 2y         ", &
      "10:c          ", "c2h^1         ", "p 1 1 2/m     ", "-p 2          ", &
      "10:a          ", "c2h^1         ", "p 2/m 1 1     ", "-p 2x         ", &
      "11:b          ", "c2h^2         ", "p 1 21/m 1    ", "-p 2yb        ", &
      "11:b          ", "c2h^2         ", "p 1 21/m 1    ", "-p 2y1        ", &
      "11:b          ", "c2h^2         ", "p 21/m        ", "-p 2yb        ", &
      "11:c          ", "c2h^2         ", "p 1 1 21/m    ", "-p 2c         ", &
      "11:c          ", "c2h^2         ", "p 1 1 21/m    ", "-p 21         ", &
      "11:a          ", "c2h^2         ", "p 21/m 1 1    ", "-p 2xa        ", &
      "11:a          ", "c2h^2         ", "p 21/m 1 1    ", "-p 2x1        ", &
      "12:b1         ", "c2h^3         ", "c 1 2/m 1     ", "-c 2y         ", &
      "12:b1         ", "c2h^3         ", "c 2/m         ", "-c 2y         ", &
      "12:b2         ", "c2h^3         ", "a 1 2/m 1     ", "-a 2y         ", &
      "12:b3         ", "c2h^3         ", "i 1 2/m 1     ", "-i 2y         ", &
      "12:b3         ", "c2h^3         ", "i 2/m         ", "-i 2y         ", &
      "12:c1         ", "c2h^3         ", "a 1 1 2/m     ", "-a 2          ", &
      "12:c2         ", "c2h^3         ", "b 1 1 2/m     ", "-b 2          ", &
      "12:c3         ", "c2h^3         ", "i 1 1 2/m     ", "-i 2          ", &
      "12:a1         ", "c2h^3         ", "b 2/m 1 1     ", "-b 2x         ", &
      "12:a2         ", "c2h^3         ", "c 2/m 1 1     ", "-c 2x         "/
   data spacegroup_conversion_table(1:4, 91:120)/ &
      "12:a3         ", "c2h^3         ", "i 2/m 1 1     ", "-i 2x         ", &
      "13:b1         ", "c2h^4         ", "p 1 2/c 1     ", "-p 2yc        ", &
      "13:b1         ", "c2h^4         ", "p 2/c         ", "-p 2yc        ", &
      "13:b2         ", "c2h^4         ", "p 1 2/n 1     ", "-p 2yac       ", &
      "13:b2         ", "c2h^4         ", "p 2/n         ", "-p 2yac       ", &
      "13:b3         ", "c2h^4         ", "p 1 2/a 1     ", "-p 2ya        ", &
      "13:b3         ", "c2h^4         ", "p 2/a         ", "-p 2ya        ", &
      "13:c1         ", "c2h^4         ", "p 1 1 2/a     ", "-p 2a         ", &
      "13:c2         ", "c2h^4         ", "p 1 1 2/n     ", "-p 2ab        ", &
      "13:c3         ", "c2h^4         ", "p 1 1 2/b     ", "-p 2b         ", &
      "13:a1         ", "c2h^4         ", "p 2/b 1 1     ", "-p 2xb        ", &
      "13:a2         ", "c2h^4         ", "p 2/n 1 1     ", "-p 2xbc       ", &
      "13:a3         ", "c2h^4         ", "p 2/c 1 1     ", "-p 2xc        ", &
      "14:b1         ", "c2h^5         ", "p 1 21/c 1    ", "-p 2ybc       ", &
      "14:b1         ", "c2h^5         ", "p 21/c        ", "-p 2ybc       ", &
      "14:b2         ", "c2h^5         ", "p 1 21/n 1    ", "-p 2yn        ", &
      "14:b2         ", "c2h^5         ", "p 21/n        ", "-p 2yn        ", &
      "14:b3         ", "c2h^5         ", "p 1 21/a 1    ", "-p 2yab       ", &
      "14:b3         ", "c2h^5         ", "p 21/a        ", "-p 2yab       ", &
      "14:c1         ", "c2h^5         ", "p 1 1 21/a    ", "-p 2ac        ", &
      "14:c2         ", "c2h^5         ", "p 1 1 21/n    ", "-p 2n         ", &
      "14:c3         ", "c2h^5         ", "p 1 1 21/b    ", "-p 2bc        ", &
      "14:a1         ", "c2h^5         ", "p 21/b 1 1    ", "-p 2xab       ", &
      "14:a2         ", "c2h^5         ", "p 21/n 1 1    ", "-p 2xn        ", &
      "14:a3         ", "c2h^5         ", "p 21/c 1 1    ", "-p 2xac       ", &
      "15:b1         ", "c2h^6         ", "c 1 2/c 1     ", "-c 2yc        ", &
      "15:b1         ", "c2h^6         ", "c 2/c         ", "-c 2yc        ", &
      "15:b2         ", "c2h^6         ", "a 1 2/n 1     ", "-a 2yab       ", & ! -a 2yac
      "15:b3         ", "c2h^6         ", "i 1 2/a 1     ", "-i 2ya        ", &
      "15:b3         ", "c2h^6         ", "i 2/a         ", "-i 2ya        "/
   data spacegroup_conversion_table(1:4,121:150)/ &
      "15:-b1        ", "c2h^6         ", "a 1 2/a 1     ", "-a 2ya        ", &
      "15:-b2        ", "c2h^6         ", "c 1 2/n 1     ", "-c 2yac       ", & ! -c 2ybc
      "15:-b2        ", "c2h^6         ", "c 2/n         ", "-c 2yac       ", & ! -c 2ybc
      "15:-b3        ", "c2h^6         ", "i 1 2/c 1     ", "-i 2yc        ", &
      "15:-b3        ", "c2h^6         ", "i 2/c         ", "-i 2yc        ", &
      "15:c1         ", "c2h^6         ", "a 1 1 2/a     ", "-a 2a         ", &
      "15:c2         ", "c2h^6         ", "b 1 1 2/n     ", "-b 2ab        ", & ! -b 2bc
      "15:c3         ", "c2h^6         ", "i 1 1 2/b     ", "-i 2b         ", &
      "15:-c1        ", "c2h^6         ", "b 1 1 2/b     ", "-b 2b         ", &
      "15:-c2        ", "c2h^6         ", "a 1 1 2/n     ", "-a 2ab        ", & ! -a 2ac
      "15:-c3        ", "c2h^6         ", "i 1 1 2/a     ", "-i 2a         ", &
      "15:a1         ", "c2h^6         ", "b 2/b 1 1     ", "-b 2xb        ", &
      "15:a2         ", "c2h^6         ", "c 2/n 1 1     ", "-c 2xac       ", & ! -c 2xbc
      "15:a3         ", "c2h^6         ", "i 2/c 1 1     ", "-i 2xc        ", &
      "15:-a1        ", "c2h^6         ", "c 2/c 1 1     ", "-c 2xc        ", &
      "15:-a2        ", "c2h^6         ", "b 2/n 1 1     ", "-b 2xab       ", & ! -b 2xbc
      "15:-a3        ", "c2h^6         ", "i 2/b 1 1     ", "-i 2xb        ", &
      "16            ", "d2^1          ", "p 2 2 2       ", "p 2 2         ", &
      "17:           ", "d2^2          ", "p 2 2 21      ", "p 2c 2        ", &
      "17:           ", "d2^2          ", "p 2 2 21      ", "p 21 2        ", &
      "17:cab        ", "d2^2          ", "p 21 2 2      ", "p 2a 2a       ", &
      "17:bca        ", "d2^2          ", "p 2 21 2      ", "p 2 2b        ", &
      "18:           ", "d2^3          ", "p 21 21 2     ", "p 2 2ab       ", &
      "18:cab        ", "d2^3          ", "p 2 21 21     ", "p 2bc 2       ", &
      "18:bca        ", "d2^3          ", "p 21 2 21     ", "p 2ac 2ac     ", &
      "19            ", "d2^4          ", "p 21 21 21    ", "p 2ac 2ab     ", &
      "20:           ", "d2^5          ", "c 2 2 21      ", "c 2c 2        ", &
      "20:           ", "d2^5          ", "c 2 2 21      ", "c 21 2        ", &
      "20:cab        ", "d2^5          ", "a 21 2 2      ", "a 2a 2a       ", &
      "20:cab        ", "d2^5          ", "a 21 2 2      ", "a 2a 21       "/
   data spacegroup_conversion_table(1:4,151:180)/ &
      "20:bca        ", "d2^5          ", "b 2 21 2      ", "b 2 2b        ", &
      "21:           ", "d2^6          ", "c 2 2 2       ", "c 2 2         ", &
      "21:cab        ", "d2^6          ", "a 2 2 2       ", "a 2 2         ", &
      "21:bca        ", "d2^6          ", "b 2 2 2       ", "b 2 2         ", &
      "22            ", "d2^7          ", "f 2 2 2       ", "f 2 2         ", &
      "23            ", "d2^8          ", "i 2 2 2       ", "i 2 2         ", &
      "24            ", "d2^9          ", "i 21 21 21    ", "i 2b 2c       ", &
      "25:           ", "c2v^1         ", "p m m 2       ", "p 2 -2        ", &
      "25:cab        ", "c2v^1         ", "p 2 m m       ", "p -2 2        ", &
      "25:bca        ", "c2v^1         ", "p m 2 m       ", "p -2 -2       ", &
      "26:           ", "c2v^2         ", "p m c 21      ", "p 2c -2       ", &
      "26:           ", "c2v^2         ", "p m c 21      ", "p 21 -2       ", &
      "26:ba-c       ", "c2v^2         ", "p c m 21      ", "p 2c -2c      ", &
      "26:ba-c       ", "c2v^2         ", "p c m 21      ", "p 21 -2c      ", &
      "26:cab        ", "c2v^2         ", "p 21 m a      ", "p -2a 2a      ", &
      "26:-cba       ", "c2v^2         ", "p 21 a m      ", "p -2 2a       ", &
      "26:bca        ", "c2v^2         ", "p b 21 m      ", "p -2 -2b      ", &
      "26:a-cb       ", "c2v^2         ", "p m 21 b      ", "p -2b -2      ", &
      "27:           ", "c2v^3         ", "p c c 2       ", "p 2 -2c       ", &
      "27:cab        ", "c2v^3         ", "p 2 a a       ", "p -2a 2       ", &
      "27:bca        ", "c2v^3         ", "p b 2 b       ", "p -2b -2b     ", &
      "28:           ", "c2v^4         ", "p m a 2       ", "p 2 -2a       ", &
      "28:           ", "c2v^4         ", "p m a 2       ", "p 2 -21       ", &
      "28:ba-c       ", "c2v^4         ", "p b m 2       ", "p 2 -2b       ", &
      "28:cab        ", "c2v^4         ", "p 2 m b       ", "p -2b 2       ", &
      "28:-cba       ", "c2v^4         ", "p 2 c m       ", "p -2c 2       ", &
      "28:-cba       ", "c2v^4         ", "p 2 c m       ", "p -21 2       ", &
      "28:bca        ", "c2v^4         ", "p c 2 m       ", "p -2c -2c     ", &
      "28:a-cb       ", "c2v^4         ", "p m 2 a       ", "p -2a -2a     ", &
      "29:           ", "c2v^5         ", "p c a 21      ", "p 2c -2ac     "/
   data spacegroup_conversion_table(1:4,181:210)/ &
      "29:ba-c       ", "c2v^5         ", "p b c 21      ", "p 2c -2b      ", &
      "29:cab        ", "c2v^5         ", "p 21 a b      ", "p -2b 2a      ", &
      "29:-cba       ", "c2v^5         ", "p 21 c a      ", "p -2ac 2a     ", &
      "29:bca        ", "c2v^5         ", "p c 21 b      ", "p -2bc -2c    ", &
      "29:a-cb       ", "c2v^5         ", "p b 21 a      ", "p -2a -2ab    ", &
      "30:           ", "c2v^6         ", "p n c 2       ", "p 2 -2bc      ", &
      "30:ba-c       ", "c2v^6         ", "p c n 2       ", "p 2 -2ac      ", &
      "30:cab        ", "c2v^6         ", "p 2 n a       ", "p -2ac 2      ", &
      "30:-cba       ", "c2v^6         ", "p 2 a n       ", "p -2ab 2      ", &
      "30:bca        ", "c2v^6         ", "p b 2 n       ", "p -2ab -2ab   ", &
      "30:a-cb       ", "c2v^6         ", "p n 2 b       ", "p -2bc -2bc   ", &
      "31:           ", "c2v^7         ", "p m n 21      ", "p 2ac -2      ", &
      "31:ba-c       ", "c2v^7         ", "p n m 21      ", "p 2bc -2bc    ", &
      "31:cab        ", "c2v^7         ", "p 21 m n      ", "p -2ab 2ab    ", &
      "31:-cba       ", "c2v^7         ", "p 21 n m      ", "p -2 2ac      ", &
      "31:bca        ", "c2v^7         ", "p n 21 m      ", "p -2 -2bc     ", &
      "31:a-cb       ", "c2v^7         ", "p m 21 n      ", "p -2ab -2     ", &
      "32:           ", "c2v^8         ", "p b a 2       ", "p 2 -2ab      ", &
      "32:cab        ", "c2v^8         ", "p 2 c b       ", "p -2bc 2      ", &
      "32:bca        ", "c2v^8         ", "p c 2 a       ", "p -2ac -2ac   ", &
      "33:           ", "c2v^9         ", "p n a 21      ", "p 2c -2n      ", &
      "33:           ", "c2v^9         ", "p n a 21      ", "p 21 -2n      ", &
      "33:ba-c       ", "c2v^9         ", "p b n 21      ", "p 2c -2ab     ", &
      "33:ba-c       ", "c2v^9         ", "p b n 21      ", "p 21 -2ab     ", &
      "33:cab        ", "c2v^9         ", "p 21 n b      ", "p -2bc 2a     ", &
      "33:cab        ", "c2v^9         ", "p 21 n b      ", "p -2bc 21     ", &
      "33:-cba       ", "c2v^9         ", "p 21 c n      ", "p -2n 2a      ", &
      "33:-cba       ", "c2v^9         ", "p 21 c n      ", "p -2n 21      ", &
      "33:bca        ", "c2v^9         ", "p c 21 n      ", "p -2n -2ac    ", &
      "33:a-cb       ", "c2v^9         ", "p n 21 a      ", "p -2ac -2n    "/
   data spacegroup_conversion_table(1:4,211:240)/ &
      "34:           ", "c2v^10        ", "p n n 2       ", "p 2 -2n       ", &
      "34:cab        ", "c2v^10        ", "p 2 n n       ", "p -2n 2       ", &
      "34:bca        ", "c2v^10        ", "p n 2 n       ", "p -2n -2n     ", &
      "35:           ", "c2v^11        ", "c m m 2       ", "c 2 -2        ", &
      "35:cab        ", "c2v^11        ", "a 2 m m       ", "a -2 2        ", &
      "35:bca        ", "c2v^11        ", "b m 2 m       ", "b -2 -2       ", &
      "36:           ", "c2v^12        ", "c m c 21      ", "c 2c -2       ", &
      "36:           ", "c2v^12        ", "c m c 21      ", "c 21 -2       ", &
      "36:ba-c       ", "c2v^12        ", "c c m 21      ", "c 2c -2c      ", &
      "36:ba-c       ", "c2v^12        ", "c c m 21      ", "c 21 -2c      ", &
      "36:cab        ", "c2v^12        ", "a 21 m a      ", "a -2a 2a      ", &
      "36:cab        ", "c2v^12        ", "a 21 m a      ", "a -2a 21      ", &
      "36:-cba       ", "c2v^12        ", "a 21 a m      ", "a -2 2a       ", &
      "36:-cba       ", "c2v^12        ", "a 21 a m      ", "a -2 21       ", &
      "36:bca        ", "c2v^12        ", "b b 21 m      ", "b -2 -2b      ", &
      "36:a-cb       ", "c2v^12        ", "b m 21 b      ", "b -2b -2      ", &
      "37:           ", "c2v^13        ", "c c c 2       ", "c 2 -2c       ", &
      "37:cab        ", "c2v^13        ", "a 2 a a       ", "a -2a 2       ", &
      "37:bca        ", "c2v^13        ", "b b 2 b       ", "b -2b -2b     ", &
      "38:           ", "c2v^14        ", "a m m 2       ", "a 2 -2        ", &
      "38:ba-c       ", "c2v^14        ", "b m m 2       ", "b 2 -2        ", &
      "38:cab        ", "c2v^14        ", "b 2 m m       ", "b -2 2        ", &
      "38:-cba       ", "c2v^14        ", "c 2 m m       ", "c -2 2        ", &
      "38:bca        ", "c2v^14        ", "c m 2 m       ", "c -2 -2       ", &
      "38:a-cb       ", "c2v^14        ", "a m 2 m       ", "a -2 -2       ", &
      "39:           ", "c2v^15        ", "a b m 2       ", "a 2 -2c       ", &
      "39:ba-c       ", "c2v^15        ", "b m a 2       ", "b 2 -2a       ", & ! b 2 -2c
      "39:cab        ", "c2v^15        ", "b 2 c m       ", "b -2a 2       ", & ! b -2c 2
      "39:-cba       ", "c2v^15        ", "c 2 m b       ", "c -2a 2       ", & ! c -2b 2
      "39:bca        ", "c2v^15        ", "c m 2 a       ", "c -2a -2a     "/
   data spacegroup_conversion_table(1:4,241:270)/ &
      "39:a-cb       ", "c2v^15        ", "a c 2 m       ", "a -2c -2c     ", &
      "40:           ", "c2v^16        ", "a m a 2       ", "a 2 -2a       ", &
      "40:ba-c       ", "c2v^16        ", "b b m 2       ", "b 2 -2b       ", &
      "40:cab        ", "c2v^16        ", "b 2 m b       ", "b -2b 2       ", &
      "40:-cba       ", "c2v^16        ", "c 2 c m       ", "c -2c 2       ", &
      "40:bca        ", "c2v^16        ", "c c 2 m       ", "c -2c -2c     ", &
      "40:a-cb       ", "c2v^16        ", "a m 2 a       ", "a -2a -2a     ", &
      "41:           ", "c2v^17        ", "a b a 2       ", "a 2 -2ab      ", & ! a 2 -2ac
      "41:ba-c       ", "c2v^17        ", "b b a 2       ", "b 2 -2ab      ", & ! b 2 -2bc
      "41:cab        ", "c2v^17        ", "b 2 c b       ", "b -2ab 2      ", & ! b -2bc 2
      "41:-cba       ", "c2v^17        ", "c 2 c b       ", "c -2ac 2      ", & ! c -2bc 2
      "41:bca        ", "c2v^17        ", "c c 2 a       ", "c -2ac -2ac   ", & ! c -2bc -2bc
      "41:a-cb       ", "c2v^17        ", "a c 2 a       ", "a -2ab -2ab   ", & ! a -2ac -2ac
      "42:           ", "c2v^18        ", "f m m 2       ", "f 2 -2        ", &
      "42:cab        ", "c2v^18        ", "f 2 m m       ", "f -2 2        ", &
      "42:bca        ", "c2v^18        ", "f m 2 m       ", "f -2 -2       ", &
      "43:           ", "c2v^19        ", "f d d 2       ", "f 2 -2d       ", &
      "43:cab        ", "c2v^19        ", "f 2 d d       ", "f -2d 2       ", &
      "43:bca        ", "c2v^19        ", "f d 2 d       ", "f -2d -2d     ", &
      "44:           ", "c2v^20        ", "i m m 2       ", "i 2 -2        ", &
      "44:cab        ", "c2v^20        ", "i 2 m m       ", "i -2 2        ", &
      "44:bca        ", "c2v^20        ", "i m 2 m       ", "i -2 -2       ", &
      "45:           ", "c2v^21        ", "i b a 2       ", "i 2 -2c       ", &
      "45:cab        ", "c2v^21        ", "i 2 c b       ", "i -2a 2       ", &
      "45:bca        ", "c2v^21        ", "i c 2 a       ", "i -2b -2b     ", &
      "46:           ", "c2v^22        ", "i m a 2       ", "i 2 -2a       ", &
      "46:ba-c       ", "c2v^22        ", "i b m 2       ", "i 2 -2b       ", &
      "46:cab        ", "c2v^22        ", "i 2 m b       ", "i -2b 2       ", &
      "46:-cba       ", "c2v^22        ", "i 2 c m       ", "i -2c 2       ", &
      "46:bca        ", "c2v^22        ", "i c 2 m       ", "i -2c -2c     "/
   data spacegroup_conversion_table(1:4,271:300)/ &
      "46:a-cb       ", "c2v^22        ", "i m 2 a       ", "i -2a -2a     ", &
      "47            ", "d2h^1         ", "p m m m       ", "-p 2 2        ", &
      "48:1          ", "d2h^2         ", "p n n n:1     ", "p 2 2 -1n     ", &
      "48:2          ", "d2h^2         ", "p n n n:2     ", "-p 2ab 2bc    ", &
      "49:           ", "d2h^3         ", "p c c m       ", "-p 2 2c       ", &
      "49:cab        ", "d2h^3         ", "p m a a       ", "-p 2a 2       ", &
      "49:bca        ", "d2h^3         ", "p b m b       ", "-p 2b 2b      ", &
      "50:1          ", "d2h^4         ", "p b a n:1     ", "p 2 2 -1ab    ", &
      "50:2          ", "d2h^4         ", "p b a n:2     ", "-p 2ab 2b     ", &
      "50:1cab       ", "d2h^4         ", "p n c b:1     ", "p 2 2 -1bc    ", &
      "50:2cab       ", "d2h^4         ", "p n c b:2     ", "-p 2b 2bc     ", &
      "50:1bca       ", "d2h^4         ", "p c n a:1     ", "p 2 2 -1ac    ", &
      "50:2bca       ", "d2h^4         ", "p c n a:2     ", "-p 2a 2c      ", &
      "51:           ", "d2h^5         ", "p m m a       ", "-p 2a 2a      ", &
      "51:ba-c       ", "d2h^5         ", "p m m b       ", "-p 2b 2       ", &
      "51:cab        ", "d2h^5         ", "p b m m       ", "-p 2 2b       ", &
      "51:-cba       ", "d2h^5         ", "p c m m       ", "-p 2c 2c      ", &
      "51:bca        ", "d2h^5         ", "p m c m       ", "-p 2c 2       ", &
      "51:a-cb       ", "d2h^5         ", "p m a m       ", "-p 2 2a       ", &
      "52:           ", "d2h^6         ", "p n n a       ", "-p 2a 2bc     ", &
      "52:ba-c       ", "d2h^6         ", "p n n b       ", "-p 2b 2n      ", &
      "52:cab        ", "d2h^6         ", "p b n n       ", "-p 2n 2b      ", &
      "52:-cba       ", "d2h^6         ", "p c n n       ", "-p 2ab 2c     ", &
      "52:bca        ", "d2h^6         ", "p n c n       ", "-p 2ab 2n     ", &
      "52:a-cb       ", "d2h^6         ", "p n a n       ", "-p 2n 2bc     ", &
      "53:           ", "d2h^7         ", "p m n a       ", "-p 2ac 2      ", &
      "53:ba-c       ", "d2h^7         ", "p n m b       ", "-p 2bc 2bc    ", &
      "53:cab        ", "d2h^7         ", "p b m n       ", "-p 2ab 2ab    ", &
      "53:-cba       ", "d2h^7         ", "p c n m       ", "-p 2 2ac      ", &
      "53:bca        ", "d2h^7         ", "p n c m       ", "-p 2 2bc      "/
   data spacegroup_conversion_table(1:4,301:330)/ &
      "53:a-cb       ", "d2h^7         ", "p m a n       ", "-p 2ab 2      ", &
      "54:           ", "d2h^8         ", "p c c a       ", "-p 2a 2ac     ", &
      "54:ba-c       ", "d2h^8         ", "p c c b       ", "-p 2b 2c      ", &
      "54:cab        ", "d2h^8         ", "p b a a       ", "-p 2a 2b      ", &
      "54:-cba       ", "d2h^8         ", "p c a a       ", "-p 2ac 2c     ", &
      "54:bca        ", "d2h^8         ", "p b c b       ", "-p 2bc 2b     ", &
      "54:a-cb       ", "d2h^8         ", "p b a b       ", "-p 2b 2ab     ", &
      "55:           ", "d2h^9         ", "p b a m       ", "-p 2 2ab      ", &
      "55:cab        ", "d2h^9         ", "p m c b       ", "-p 2bc 2      ", &
      "55:bca        ", "d2h^9         ", "p c m a       ", "-p 2ac 2ac    ", &
      "56:           ", "d2h^10        ", "p c c n       ", "-p 2ab 2ac    ", &
      "56:cab        ", "d2h^10        ", "p n a a       ", "-p 2ac 2bc    ", &
      "56:bca        ", "d2h^10        ", "p b n b       ", "-p 2bc 2ab    ", &
      "57:           ", "d2h^11        ", "p b c m       ", "-p 2c 2b      ", &
      "57:ba-c       ", "d2h^11        ", "p c a m       ", "-p 2c 2ac     ", &
      "57:cab        ", "d2h^11        ", "p m c a       ", "-p 2ac 2a     ", &
      "57:-cba       ", "d2h^11        ", "p m a b       ", "-p 2b 2a      ", &
      "57:bca        ", "d2h^11        ", "p b m a       ", "-p 2a 2ab     ", &
      "57:a-cb       ", "d2h^11        ", "p c m b       ", "-p 2bc 2c     ", &
      "58:           ", "d2h^12        ", "p n n m       ", "-p 2 2n       ", &
      "58:cab        ", "d2h^12        ", "p m n n       ", "-p 2n 2       ", &
      "58:bca        ", "d2h^12        ", "p n m n       ", "-p 2n 2n      ", &
      "59:1          ", "d2h^13        ", "p m m n:1     ", "p 2 2ab -1ab  ", &
      "59:2          ", "d2h^13        ", "p m m n:2     ", "-p 2ab 2a     ", &
      "59:1cab       ", "d2h^13        ", "p n m m:1     ", "p 2bc 2 -1bc  ", &
      "59:2cab       ", "d2h^13        ", "p n m m:2     ", "-p 2c 2bc     ", &
      "59:1bca       ", "d2h^13        ", "p m n m:1     ", "p 2ac 2ac -1ac", &
      "59:2bca       ", "d2h^13        ", "p m n m:2     ", "-p 2c 2a      ", &
      "60:           ", "d2h^14        ", "p b c n       ", "-p 2n 2ab     ", &
      "60:ba-c       ", "d2h^14        ", "p c a n       ", "-p 2n 2c      "/
   data spacegroup_conversion_table(1:4,331:360)/ &
      "60:cab        ", "d2h^14        ", "p n c a       ", "-p 2a 2n      ", &
      "60:-cba       ", "d2h^14        ", "p n a b       ", "-p 2bc 2n     ", &
      "60:bca        ", "d2h^14        ", "p b n a       ", "-p 2ac 2b     ", &
      "60:a-cb       ", "d2h^14        ", "p c n b       ", "-p 2b 2ac     ", &
      "61:           ", "d2h^15        ", "p b c a       ", "-p 2ac 2ab    ", &
      "61:ba-c       ", "d2h^15        ", "p c a b       ", "-p 2bc 2ac    ", &
      "62:           ", "d2h^16        ", "p n m a       ", "-p 2ac 2n     ", &
      "62:ba-c       ", "d2h^16        ", "p m n b       ", "-p 2bc 2a     ", &
      "62:cab        ", "d2h^16        ", "p b n m       ", "-p 2c 2ab     ", &
      "62:-cba       ", "d2h^16        ", "p c m n       ", "-p 2n 2ac     ", &
      "62:bca        ", "d2h^16        ", "p m c n       ", "-p 2n 2a      ", &
      "62:a-cb       ", "d2h^16        ", "p n a m       ", "-p 2c 2n      ", &
      "63:           ", "d2h^17        ", "c m c m       ", "-c 2c 2       ", &
      "63:ba-c       ", "d2h^17        ", "c c m m       ", "-c 2c 2c      ", &
      "63:cab        ", "d2h^17        ", "a m m a       ", "-a 2a 2a      ", &
      "63:-cba       ", "d2h^17        ", "a m a m       ", "-a 2 2a       ", &
      "63:bca        ", "d2h^17        ", "b b m m       ", "-b 2 2b       ", &
      "63:a-cb       ", "d2h^17        ", "b m m b       ", "-b 2b 2       ", &
      "64:           ", "d2h^18        ", "c m c a       ", "-c 2ac 2      ", & ! -c 2bc 2
      "64:ba-c       ", "d2h^18        ", "c c m b       ", "-c 2ac 2ac    ", & ! -c 2bc 2bc
      "64:cab        ", "d2h^18        ", "a b m a       ", "-a 2ab 2ab    ", & ! -a 2ac 2ac
      "64:-cba       ", "d2h^18        ", "a c a m       ", "-a 2 2ab      ", & ! -a 2 2ac
      "64:bca        ", "d2h^18        ", "b b c m       ", "-b 2 2ab      ", & ! -b 2 2bc
      "64:a-cb       ", "d2h^18        ", "b m a b       ", "-b 2ab 2      ", & ! -b 2bc 2
      "65:           ", "d2h^19        ", "c m m m       ", "-c 2 2        ", &
      "65:cab        ", "d2h^19        ", "a m m m       ", "-a 2 2        ", &
      "65:bca        ", "d2h^19        ", "b m m m       ", "-b 2 2        ", &
      "66:           ", "d2h^20        ", "c c c m       ", "-c 2 2c       ", &
      "66:cab        ", "d2h^20        ", "a m a a       ", "-a 2a 2       ", &
      "66:bca        ", "d2h^20        ", "b b m b       ", "-b 2b 2b      "/
   data spacegroup_conversion_table(1:4,361:390)/ &
      "67:           ", "d2h^21        ", "c m m a       ", "-c 2a 2       ", & ! -c 2b 2
      "67:ba-c       ", "d2h^21        ", "c m m b       ", "-c 2a 2a      ", & ! -c 2b 2b
      "67:cab        ", "d2h^21        ", "a b m m       ", "-a 2b 2b      ", & ! -a 2c 2c
      "67:-cba       ", "d2h^21        ", "a c m m       ", "-a 2 2c       ", &
      "67:bca        ", "d2h^21        ", "b m c m       ", "-b 2 2a       ", & ! -b 2 2c
      "67:a-cb       ", "d2h^21        ", "b m a m       ", "-b 2a 2       ", & ! -b 2c 2
      "68:1          ", "d2h^22        ", "c c c a:1     ", "c 2 2 -1ac    ", & ! c 2 2 -1bc
      "68:2          ", "d2h^22        ", "c c c a:2     ", "-c 2a 2ac     ", & ! -c 2b 2bc
      "68:1ba-c      ", "d2h^22        ", "c c c b:1     ", "c 2 2 -1ac    ", & ! c 2 2 -1bc
      "68:2ba-c      ", "d2h^22        ", "c c c b:2     ", "-c 2a 2c      ", & ! -c 2b 2c
      "68:1cab       ", "d2h^22        ", "a b a a:1     ", "a 2 2 -1ab    ", & ! a 2 2 -1ac
      "68:2cab       ", "d2h^22        ", "a b a a:2     ", "-a 2a 2c      ", &
      "68:1-cba      ", "d2h^22        ", "a c a a:1     ", "a 2 2 -1ab    ", & ! a 2 2 -1ac
      "68:2-cba      ", "d2h^22        ", "a c a a:2     ", "-a 2ab 2b     ", & ! -a 2ac 2c
      "68:1bca       ", "d2h^22        ", "b b c b:1     ", "b 2 2 -1ab    ", & ! b 2 2 -1bc
      "68:2bca       ", "d2h^22        ", "b b c b:2     ", "-b 2ab 2b     ", & ! -b 2bc 2b
      "68:1a-cb      ", "d2h^22        ", "b b a b:1     ", "b 2 2 -1ab    ", & ! b 2 2 -1bc
      "68:2a-cb      ", "d2h^22        ", "b b a b:2     ", "-b 2b 2ab     ", & ! -b 2b 2bc
      "69            ", "d2h^23        ", "f m m m       ", "-f 2 2        ", &
      "70:1          ", "d2h^24        ", "f d d d:1     ", "f 2 2 -1d     ", &
      "70:2          ", "d2h^24        ", "f d d d:2     ", "-f 2uv 2vw    ", &
      "71            ", "d2h^25        ", "i m m m       ", "-i 2 2        ", &
      "72:           ", "d2h^26        ", "i b a m       ", "-i 2 2c       ", &
      "72:cab        ", "d2h^26        ", "i m c b       ", "-i 2a 2       ", &
      "72:bca        ", "d2h^26        ", "i c m a       ", "-i 2b 2b      ", &
      "73:           ", "d2h^27        ", "i b c a       ", "-i 2b 2c      ", &
      "73:ba-c       ", "d2h^27        ", "i c a b       ", "-i 2a 2b      ", &
      "74:           ", "d2h^28        ", "i m m a       ", "-i 2b 2       ", &
      "74:ba-c       ", "d2h^28        ", "i m m b       ", "-i 2a 2a      ", &
      "74:cab        ", "d2h^28        ", "i b m m       ", "-i 2c 2c      "/
   data spacegroup_conversion_table(1:4,391:420)/ &
      "74:-cba       ", "d2h^28        ", "i c m m       ", "-i 2 2b       ", &
      "74:bca        ", "d2h^28        ", "i m c m       ", "-i 2 2a       ", &
      "74:a-cb       ", "d2h^28        ", "i m a m       ", "-i 2c 2       ", &
      "75            ", "c4^1          ", "p 4           ", "p 4           ", &
      "76:           ", "c4^2          ", "p 41          ", "p 4w          ", &
      "76:           ", "c4^2          ", "p 41          ", "p 41          ", &
      "77:           ", "c4^3          ", "p 42          ", "p 4c          ", &
      "77:           ", "c4^3          ", "p 42          ", "p 42          ", &
      "78:           ", "c4^4          ", "p 43          ", "p 4cw         ", &
      "78:           ", "c4^4          ", "p 43          ", "p 43          ", &
      "79            ", "c4^5          ", "i 4           ", "i 4           ", &
      "80            ", "c4^6          ", "i 41          ", "i 4bw         ", &
      "81            ", "s4^1          ", "p -4          ", "p -4          ", &
      "82            ", "s4^2          ", "i -4          ", "i -4          ", &
      "83            ", "c4h^1         ", "p 4/m         ", "-p 4          ", &
      "84:           ", "c4h^2         ", "p 42/m        ", "-p 4c         ", &
      "84:           ", "c4h^2         ", "p 42/m        ", "-p 42         ", &
      "85:1          ", "c4h^3         ", "p 4/n:1       ", "p 4ab -1ab    ", &
      "85:2          ", "c4h^3         ", "p 4/n:2       ", "-p 4a         ", &
      "86:1          ", "c4h^4         ", "p 42/n:1      ", "p 4n -1n      ", &
      "86:2          ", "c4h^4         ", "p 42/n:2      ", "-p 4bc        ", &
      "87            ", "c4h^5         ", "i 4/m         ", "-i 4          ", &
      "88:1          ", "c4h^6         ", "i 41/a:1      ", "i 4bw -1bw    ", &
      "88:2          ", "c4h^6         ", "i 41/a:2      ", "-i 4ad        ", &
      "89            ", "d4^1          ", "p 4 2 2       ", "p 4 2         ", &
      "90            ", "d4^2          ", "p 4 21 2      ", "p 4ab 2ab     ", &
      "91:           ", "d4^3          ", "p 41 2 2      ", "p 4w 2c       ", &
      "91:           ", "d4^3          ", "p 41 2 2      ", "p 41 2c       ", &
      "92            ", "d4^4          ", "p 41 21 2     ", "p 4abw 2nw    ", &
      "93:           ", "d4^5          ", "p 42 2 2      ", "p 4c 2        "/
   data spacegroup_conversion_table(1:4,421:450)/ &
      "93:           ", "d4^5          ", "p 42 2 2      ", "p 42 2        ", &
      "94            ", "d4^6          ", "p 42 21 2     ", "p 4n 2n       ", &
      "95:           ", "d4^7          ", "p 43 2 2      ", "p 4cw 2c      ", &
      "95:           ", "d4^7          ", "p 43 2 2      ", "p 43 2c       ", &
      "96            ", "d4^8          ", "p 43 21 2     ", "p 4nw 2abw    ", &
      "97            ", "d4^9          ", "i 4 2 2       ", "i 4 2         ", &
      "98            ", "d4^10         ", "i 41 2 2      ", "i 4bw 2bw     ", &
      "99            ", "c4v^1         ", "p 4 m m       ", "p 4 -2        ", &
      "100           ", "c4v^2         ", "p 4 b m       ", "p 4 -2ab      ", &
      "101:          ", "c4v^3         ", "p 42 c m      ", "p 4c -2c      ", &
      "101:          ", "c4v^3         ", "p 42 c m      ", "p 42 -2c      ", &
      "102           ", "c4v^4         ", "p 42 n m      ", "p 4n -2n      ", &
      "103           ", "c4v^5         ", "p 4 c c       ", "p 4 -2c       ", &
      "104           ", "c4v^6         ", "p 4 n c       ", "p 4 -2n       ", &
      "105:          ", "c4v^7         ", "p 42 m c      ", "p 4c -2       ", &
      "105:          ", "c4v^7         ", "p 42 m c      ", "p 42 -2       ", &
      "106:          ", "c4v^8         ", "p 42 b c      ", "p 4c -2ab     ", &
      "106:          ", "c4v^8         ", "p 42 b c      ", "p 42 -2ab     ", &
      "107           ", "c4v^9         ", "i 4 m m       ", "i 4 -2        ", &
      "108           ", "c4v^10        ", "i 4 c m       ", "i 4 -2c       ", &
      "109           ", "c4v^11        ", "i 41 m d      ", "i 4bw -2      ", &
      "110           ", "c4v^12        ", "i 41 c d      ", "i 4bw -2c     ", &
      "111           ", "d2d^1         ", "p -4 2 m      ", "p -4 2        ", &
      "112           ", "d2d^2         ", "p -4 2 c      ", "p -4 2c       ", &
      "113           ", "d2d^3         ", "p -4 21 m     ", "p -4 2ab      ", &
      "114           ", "d2d^4         ", "p -4 21 c     ", "p -4 2n       ", &
      "115           ", "d2d^5         ", "p -4 m 2      ", "p -4 -2       ", &
      "116           ", "d2d^6         ", "p -4 c 2      ", "p -4 -2c      ", &
      "117           ", "d2d^7         ", "p -4 b 2      ", "p -4 -2ab     ", &
      "118           ", "d2d^8         ", "p -4 n 2      ", "p -4 -2n      "/
   data spacegroup_conversion_table(1:4,451:480)/ &
      "119           ", "d2d^9         ", "i -4 m 2      ", "i -4 -2       ", &
      "120           ", "d2d^10        ", "i -4 c 2      ", "i -4 -2c      ", &
      "121           ", "d2d^11        ", "i -4 2 m      ", "i -4 2        ", &
      "122           ", "d2d^12        ", "i -4 2 d      ", "i -4 2bw      ", &
      "123           ", "d4h^1         ", "p 4/m m m     ", "-p 4 2        ", &
      "124           ", "d4h^2         ", "p 4/m c c     ", "-p 4 2c       ", &
      "125:1         ", "d4h^3         ", "p 4/n b m:1   ", "p 4 2 -1ab    ", &
      "125:2         ", "d4h^3         ", "p 4/n b m:2   ", "-p 4a 2b      ", &
      "126:1         ", "d4h^4         ", "p 4/n n c:1   ", "p 4 2 -1n     ", &
      "126:2         ", "d4h^4         ", "p 4/n n c:2   ", "-p 4a 2bc     ", &
      "127           ", "d4h^5         ", "p 4/m b m     ", "-p 4 2ab      ", &
      "128           ", "d4h^6         ", "p 4/m n c     ", "-p 4 2n       ", &
      "129:1         ", "d4h^7         ", "p 4/n m m:1   ", "p 4ab 2ab -1ab", &
      "129:2         ", "d4h^7         ", "p 4/n m m:2   ", "-p 4a 2a      ", &
      "130:1         ", "d4h^8         ", "p 4/n c c:1   ", "p 4ab 2n -1ab ", &
      "130:2         ", "d4h^8         ", "p 4/n c c:2   ", "-p 4a 2ac     ", &
      "131           ", "d4h^9         ", "p 42/m m c    ", "-p 4c 2       ", &
      "132           ", "d4h^10        ", "p 42/m c m    ", "-p 4c 2c      ", &
      "133:1         ", "d4h^11        ", "p 42/n b c:1  ", "p 4n 2c -1n   ", &
      "133:2         ", "d4h^11        ", "p 42/n b c:2  ", "-p 4ac 2b     ", &
      "134:1         ", "d4h^12        ", "p 42/n n m:1  ", "p 4n 2 -1n    ", &
      "134:2         ", "d4h^12        ", "p 42/n n m:2  ", "-p 4ac 2bc    ", &
      "135:          ", "d4h^13        ", "p 42/m b c    ", "-p 4c 2ab     ", &
      "135:          ", "d4h^13        ", "p 42/m b c    ", "-p 42 2ab     ", &
      "136           ", "d4h^14        ", "p 42/m n m    ", "-p 4n 2n      ", &
      "137:1         ", "d4h^15        ", "p 42/n m c:1  ", "p 4n 2n -1n   ", &
      "137:2         ", "d4h^15        ", "p 42/n m c:2  ", "-p 4ac 2a     ", &
      "138:1         ", "d4h^16        ", "p 42/n c m:1  ", "p 4n 2ab -1n  ", &
      "138:2         ", "d4h^16        ", "p 42/n c m:2  ", "-p 4ac 2ac    ", &
      "139           ", "d4h^17        ", "i 4/m m m     ", "-i 4 2        "/
   data spacegroup_conversion_table(1:4,481:510)/ &
      "140           ", "d4h^18        ", "i 4/m c m     ", "-i 4 2c       ", &
      "141:1         ", "d4h^19        ", "i 41/a m d:1  ", "i 4bw 2bw -1bw", &
      "141:2         ", "d4h^19        ", "i 41/a m d:2  ", "-i 4bd 2      ", &
      "142:1         ", "d4h^20        ", "i 41/a c d:1  ", "i 4bw 2aw -1bw", &
      "142:2         ", "d4h^20        ", "i 41/a c d:2  ", "-i 4bd 2c     ", &
      "143           ", "c3^1          ", "p 3           ", "p 3           ", &
      "144           ", "c3^2          ", "p 31          ", "p 31          ", &
      "145           ", "c3^3          ", "p 32          ", "p 32          ", &
      "146:h         ", "c3^4          ", "r 3:h         ", "r 3           ", &
      "146:r         ", "c3^4          ", "r 3:r         ", "p 3*          ", &
      "147           ", "c3i^1         ", "p -3          ", "-p 3          ", &
      "148:h         ", "c3i^2         ", "r -3:h        ", "-r 3          ", &
      "148:r         ", "c3i^2         ", "r -3:r        ", "-p 3*         ", &
      "149           ", "d3^1          ", "p 3 1 2       ", "p 3 2         ", &
      "150           ", "d3^2          ", "p 3 2 1       ", "p 3 2""       ", &
      "151           ", "d3^3          ", "p 31 1 2      ", "p 31 2 (0 0 4)", & ! p 31 2c (0 0 1)
      "152           ", "d3^4          ", "p 31 2 1      ", "p 31 2""      ", &
      "153           ", "d3^5          ", "p 32 1 2      ", "p 32 2 (0 0 2)", & ! p 32 2c (0 0 -1)
      "154           ", "d3^6          ", "p 32 2 1      ", "p 32 2""      ", &
      "155:h         ", "d3^7          ", "r 3 2:h       ", "r 3 2""       ", &
      "155:r         ", "d3^7          ", "r 3 2:r       ", "p 3* 2        ", &
      "156           ", "c3v^1         ", "p 3 m 1       ", "p 3 -2""      ", &
      "157           ", "c3v^2         ", "p 3 1 m       ", "p 3 -2        ", &
      "158           ", "c3v^3         ", "p 3 c 1       ", "p 3 -2""c     ", &
      "159           ", "c3v^4         ", "p 3 1 c       ", "p 3 -2c       ", &
      "160:h         ", "c3v^5         ", "r 3 m:h       ", "r 3 -2""      ", &
      "160:r         ", "c3v^5         ", "r 3 m:r       ", "p 3* -2       ", &
      "161:h         ", "c3v^6         ", "r 3 c:h       ", "r 3 -2""c     ", &
      "161:r         ", "c3v^6         ", "r 3 c:r       ", "p 3* -2n      ", &
      "162           ", "d3d^1         ", "p -3 1 m      ", "-p 3 2        "/
   data spacegroup_conversion_table(1:4,511:540)/ &
      "163           ", "d3d^2         ", "p -3 1 c      ", "-p 3 2c       ", &
      "164           ", "d3d^3         ", "p -3 m 1      ", "-p 3 2""      ", &
      "165           ", "d3d^4         ", "p -3 c 1      ", "-p 3 2""c     ", &
      "166:h         ", "d3d^5         ", "r -3 m:h      ", "-r 3 2""      ", &
      "166:r         ", "d3d^5         ", "r -3 m:r      ", "-p 3* 2       ", &
      "167:h         ", "d3d^6         ", "r -3 c:h      ", "-r 3 2""c     ", &
      "167:r         ", "d3d^6         ", "r -3 c:r      ", "-p 3* 2n      ", &
      "168           ", "c6^1          ", "p 6           ", "p 6           ", &
      "169           ", "c6^2          ", "p 61          ", "p 61          ", &
      "170           ", "c6^3          ", "p 65          ", "p 65          ", &
      "171           ", "c6^4          ", "p 62          ", "p 62          ", &
      "172           ", "c6^5          ", "p 64          ", "p 64          ", &
      "173:          ", "c6^6          ", "p 63          ", "p 6c          ", &
      "173:          ", "c6^6          ", "p 63          ", "p 63          ", &
      "174           ", "c3h^1         ", "p -6          ", "p -6          ", &
      "175           ", "c6h^1         ", "p 6/m         ", "-p 6          ", &
      "176:          ", "c6h^2         ", "p 63/m        ", "-p 6c         ", &
      "176:          ", "c6h^2         ", "p 63/m        ", "-p 63         ", &
      "177           ", "d6^1          ", "p 6 2 2       ", "p 6 2         ", &
      "178           ", "d6^2          ", "p 61 2 2      ", "p 61 2 (0 0 5)", & ! p 61 2 (0 0 -1)
      "179           ", "d6^3          ", "p 65 2 2      ", "p 65 2 (0 0 1)", &
      "180           ", "d6^4          ", "p 62 2 2      ", "p 62 2 (0 0 4)", & ! p 62 2c (0 0 1)
      "181           ", "d6^5          ", "p 64 2 2      ", "p 64 2 (0 0 2)", & ! p 64 2c (0 0 -1)
      "182:          ", "d6^6          ", "p 63 2 2      ", "p 6c 2c       ", &
      "182:          ", "d6^6          ", "p 63 2 2      ", "p 63 2c       ", &
      "183           ", "c6v^1         ", "p 6 m m       ", "p 6 -2        ", &
      "184           ", "c6v^2         ", "p 6 c c       ", "p 6 -2c       ", &
      "185:          ", "c6v^3         ", "p 63 c m      ", "p 6c -2       ", &
      "185:          ", "c6v^3         ", "p 63 c m      ", "p 63 -2       ", &
      "186:          ", "c6v^4         ", "p 63 m c      ", "p 6c -2c      "/
   data spacegroup_conversion_table(1:4,541:570)/ &
      "186:          ", "c6v^4         ", "p 63 m c      ", "p 63 -2c      ", &
      "187           ", "d3h^1         ", "p -6 m 2      ", "p -6 2        ", &
      "188           ", "d3h^2         ", "p -6 c 2      ", "p -6c 2       ", &
      "189           ", "d3h^3         ", "p -6 2 m      ", "p -6 -2       ", &
      "190           ", "d3h^4         ", "p -6 2 c      ", "p -6c -2c     ", &
      "191           ", "d6h^1         ", "p 6/m m m     ", "-p 6 2        ", &
      "192           ", "d6h^2         ", "p 6/m c c     ", "-p 6 2c       ", &
      "193:          ", "d6h^3         ", "p 63/m c m    ", "-p 6c 2       ", &
      "193:          ", "d6h^3         ", "p 63/m c m    ", "-p 63 2       ", &
      "194:          ", "d6h^4         ", "p 63/m m c    ", "-p 6c 2c      ", &
      "194:          ", "d6h^4         ", "p 63/m m c    ", "-p 63 2c      ", &
      "195           ", "t^1           ", "p 2 3         ", "p 2 2 3       ", &
      "196           ", "t^2           ", "f 2 3         ", "f 2 2 3       ", &
      "197           ", "t^3           ", "i 2 3         ", "i 2 2 3       ", &
      "198           ", "t^4           ", "p 21 3        ", "p 2ac 2ab 3   ", &
      "199           ", "t^5           ", "i 21 3        ", "i 2b 2c 3     ", &
      "200           ", "th^1          ", "p m -3        ", "-p 2 2 3      ", &
      "201:1         ", "th^2          ", "p n -3:1      ", "p 2 2 3 -1n   ", &
      "201:2         ", "th^2          ", "p n -3:2      ", "-p 2ab 2bc 3  ", &
      "202           ", "th^3          ", "f m -3        ", "-f 2 2 3      ", &
      "203:1         ", "th^4          ", "f d -3:1      ", "f 2 2 3 -1d   ", &
      "203:2         ", "th^4          ", "f d -3:2      ", "-f 2uv 2vw 3  ", &
      "204           ", "th^5          ", "i m -3        ", "-i 2 2 3      ", &
      "205           ", "th^6          ", "p a -3        ", "-p 2ac 2ab 3  ", &
      "206           ", "th^7          ", "i a -3        ", "-i 2b 2c 3    ", &
      "207           ", "o^1           ", "p 4 3 2       ", "p 4 2 3       ", &
      "208           ", "o^2           ", "p 42 3 2      ", "p 4n 2 3      ", &
      "209           ", "o^3           ", "f 4 3 2       ", "f 4 2 3       ", &
      "210           ", "o^4           ", "f 41 3 2      ", "f 4d 2 3      ", &
      "211           ", "o^5           ", "i 4 3 2       ", "i 4 2 3       "/
   data spacegroup_conversion_table(1:4,571:593)/ &
      "212           ", "o^6           ", "p 43 3 2      ", "p 4acd 2ab 3  ", &
      "213           ", "o^7           ", "p 41 3 2      ", "p 4bd 2ab 3   ", &
      "214           ", "o^8           ", "i 41 3 2      ", "i 4bd 2c 3    ", &
      "215           ", "td^1          ", "p -4 3 m      ", "p -4 2 3      ", &
      "216           ", "td^2          ", "f -4 3 m      ", "f -4 2 3      ", &
      "217           ", "td^3          ", "i -4 3 m      ", "i -4 2 3      ", &
      "218           ", "td^4          ", "p -4 3 n      ", "p -4n 2 3     ", &
      "219           ", "td^5          ", "f -4 3 c      ", "f -4a 2 3     ", & ! f -4c 2 3
      "220           ", "td^6          ", "i -4 3 d      ", "i -4bd 2c 3   ", &
      "221           ", "oh^1          ", "p m -3 m      ", "-p 4 2 3      ", &
      "222:1         ", "oh^2          ", "p n -3 n:1    ", "p 4 2 3 -1n   ", &
      "222:2         ", "oh^2          ", "p n -3 n:2    ", "-p 4a 2bc 3   ", &
      "223           ", "oh^3          ", "p m -3 n      ", "-p 4n 2 3     ", &
      "224:1         ", "oh^4          ", "p n -3 m:1    ", "p 4n 2 3 -1n  ", &
      "224:2         ", "oh^4          ", "p n -3 m:2    ", "-p 4bc 2bc 3  ", &
      "225           ", "oh^5          ", "f m -3 m      ", "-f 4 2 3      ", &
      "226           ", "oh^6          ", "f m -3 c      ", "-f 4a 2 3     ", & ! -f 4c 2 3
      "227:1         ", "oh^7          ", "f d -3 m:1    ", "f 4d 2 3 -1d  ", &
      "227:2         ", "oh^7          ", "f d -3 m:2    ", "-f 4vw 2vw 3  ", &
      "228:1         ", "oh^8          ", "f d -3 c:1    ", "f 4d 2 3 -1ad ", & ! f 4d 2 3 -1cd
      "228:2         ", "oh^8          ", "f d -3 c:2    ", "-f 4ud 2vw 3  ", & ! -f 4cvw 2vw 3
      "229           ", "oh^9          ", "i m -3 m      ", "-i 4 2 3      ", &
      "230           ", "oh^10         ", "i a -3 d      ", "-i 4bd 2c 3   "/

contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create(self)
   ! Allocate an object
      type(SPACEGROUP_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SPACEGROUP:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SPACEGROUP_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SPACEGROUP:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of object
      type(SPACEGROUP_TYPE), INOUT :: self
      call destroy_(self%seitz)
      call destroy_(self%product_of_symops)
      call destroy_(self%inverse_of_symops)
      call destroy_(self%inverted_symop)
      call destroy_(self%translated_symop)
      call destroy_(self%unique_SF_symop)
   end subroutine


!  ===========
!  Set methods
!  ===========

   subroutine set_IT_symbol(self,IT_symbol)
   ! Set the international table symbol to be "IT_symbol"
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: IT_symbol
      STR(len=STR_SIZE) :: symbol
      INT :: ind,pos

      ! Lower case the symbol
      symbol = IT_symbol
      call to_lower_case_(symbol)
      ind = 0

      ! Is the IT symbol in the table?
      if (symbol==" " OR symbol=="?") then
         WARN("SPACEGROUP:set_IT_symbol ... IT_symbol is blank or ?")
         return
      else if (any(symbol==spacegroup_conversion_table(1,:))) then

         ! Matching index
         ind = index_of_(spacegroup_conversion_table(1,:),symbol)

         ! Set IT symbol
         self%IT_symbol = trim_blanks_from_start_(symbol)

         ! Set the IT group no.
         pos = index(self%IT_symbol,":")
         if (pos==0) then; self%IT_group_number = to_int_(self%IT_symbol)
         else; self%IT_group_number = to_int_(self%IT_symbol(1:pos-1))
         end if

         ! Set the other two symbols
         self%Schoenflies_symbol = spacegroup_conversion_table(2,ind)
         self%HM_symbol = spacegroup_conversion_table(3,ind)

         ! Set the Hall symbol
         call set_Hall_symbol_(self,spacegroup_conversion_table(4,ind))
      else if (has_any_including_(spacegroup_conversion_table(1,:),symbol)) then
         ind = index_of_first_that_includes_(spacegroup_conversion_table(1,:),trim(symbol))
         self%IT_symbol = spacegroup_conversion_table(1,ind)
         WARN("SPACEGROUP:set_IT_symbol ... Symbol "//trim(symbol)//" does not match any specific IT symbol")
         WARN("SPACEGROUP:set_IT_symbol ... Matching symbols are:")
         call put_matching_IT_symbols(symbol)
      end if
      self%analyzed = FALSE
   end subroutine

   subroutine set_HM_symbol(self,HM_symbol)
   ! Set the Herman-Mauguin symbol to be "HM_symbol"
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: HM_symbol
      STR(len=STR_SIZE) :: symbol
      INT :: ind,pos

      ! Set the HM symbol
      self%HM_symbol = trim_blanks_from_start_(HM_symbol)

      ! Replace underscores with spaces
      if (index(trim(self%HM_symbol),"_")/=0) call replace_all_(self%HM_symbol,"_"," ")

      ! Lower case it
      symbol = self%HM_symbol
      call to_lower_case_(symbol)
      ind = 0

      ! Is the IT symbol in the table?
      if (symbol==" " OR symbol=="?") then
         WARN("SPACEGROUP:set_HM_symbol ... IT_symbol is blank or ?")
         return
      else if (any(symbol==spacegroup_conversion_table(3,:))) then

         ! Matching index
         ind = index_of_(spacegroup_conversion_table(3,:),symbol)

         ! Set HM symbol
         self%HM_symbol = trim_blanks_from_start_(symbol)

         ! Set the IT symbol
         self%IT_symbol = spacegroup_conversion_table(1,ind)

         ! Set the IT groutp no.
         pos = index(self%IT_symbol,":")
         if (pos==0) then; self%IT_group_number = to_int_(self%IT_symbol)
         else; self%IT_group_number = to_int_(self%IT_symbol(1:pos-1))
         end if

         ! Set Schonflies
         self%Schoenflies_symbol = spacegroup_conversion_table(2,ind)

         ! Set Hall symbol
         call set_Hall_symbol_(self,spacegroup_conversion_table(4,ind))
      else if (has_any_including_(spacegroup_conversion_table(1,:),symbol)) then
         WARN("SPACEGROUP:set_HM_symbol ... Symbol "//trim(symbol)//" does not match any specific HM symbol")

         ! Set unknown spacegroup
         self%IT_symbol = "?"
         self%IT_group_number = 0
         self%Schoenflies_symbol = "?"
         self%HM_symbol = "?"

         ! Report near matches
         ind = index_of_first_that_includes_(spacegroup_conversion_table(3,:),trim(symbol))
         if (ind>0) then
            WARN("SPACEGROUP:set_HM_symbol ... There were part-matching symbols are:")
            call put_matching_HM_symbols(symbol)
         end if
      end if
      self%analyzed = FALSE
   end subroutine

   subroutine set_Hall_symbol(self,Hall_symbol)
   ! Set the Hall symbol to be "Hall_symbol"
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: Hall_symbol
      STR(len=STR_SIZE) :: symbol
      INT :: ind,pos
      self%Hall_symbol = trim_blanks_from_start_(Hall_symbol)

      ! Replace underscores with spaces
      if (index(trim(self%Hall_symbol),"_")/=0) call replace_all_(self%Hall_symbol,"_"," ")

      ! Lower case it
      call to_lower_case_(self%Hall_symbol)

      ! Convert to equivalent notations
      symbol = self%Hall_symbol
      if (any(symbol==spacegroup_conversion_table(4,:))) then
         ind = index_of_(spacegroup_conversion_table(4,:),symbol)
         self%IT_symbol = spacegroup_conversion_table(1,ind)
         pos = index(self%IT_symbol,":")
         if (pos==0) then; self%IT_group_number = to_int_(self%IT_symbol)
         else; self%IT_group_number = to_int_(self%IT_symbol(1:pos-1))
         end if
         self%Schoenflies_symbol = spacegroup_conversion_table(2,ind)
         self%HM_symbol = spacegroup_conversion_table(3,ind)
      else
         WARN("SPACEGROUP:set_Hall_symbol ... Non-standard Hall symbol, "//trim(symbol))
         self%IT_symbol = "?"
         self%IT_group_number = 0
         self%Schoenflies_symbol = "?"
         self%HM_symbol = "?"
      end if
      self%analyzed = FALSE
   end subroutine

   subroutine set_from_symops(self,mat)
   ! This will go through every spacegroup and setting to see if we
   ! can match the symops in "mat".
      type(SPACEGROUP_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), IN :: mat
      STR(len=STR_SIZE) :: symbol
      BIN :: found
      type(SPACEGROUP_TYPE), allocatable :: sg
      INT :: g
   ENSURE(size(mat,1)==4,"SPACEGROUP:set_from_symops ... wrong 1st dimension, mat")
   ENSURE(size(mat,2)==4,"SPACEGROUP:set_from_symops ... wrong 1st dimension, mat")
      found = FALSE
      call flush_(stdout)
      call text_(stdout,"Trying to correct wrong/missing spacegroup symbol.")

      ! Loop over all spacegroups settings
      do g = 1,size(spacegroup_conversion_table,2)

         ! Get the symbol
         symbol = spacegroup_conversion_table(4,g)

         ! Create and analyze
         call create_(sg)
         call set_Hall_symbol_(sg,trim(symbol))
         call analyze_(sg)

         ! Are the symops the same?
         if (seitz_same_as_(sg,mat,warn=FALSE)) then
            call flush_(stdout)
            call text_(stdout,"Found spacegroup Hall symbol: "//trim(symbol))
            found = TRUE
            exit
         end if

         ! Clean
         call destroy_(sg)
      end do
      if (found) then

         ! Set the Hall symbol
         call set_Hall_symbol_(self,symbol)
      else

         ! Unfound
         call flush_(stdout)
         call text_(stdout,"Unable to correct wrong/missing spacegroup symbol")
      end if

      ! Clean
      call destroy_(sg)
   end subroutine


!  =============
!  Input methods
!  =============

   ! NOTE: always analyze the spacegroup after reading keywords

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","SPACEGROUP:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"SPACEGROUP:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                      ")  ! exit case
      case ("analyze                "); call analyze_(self)
      case ("hall_symbol=           "); call read_Hall_symbol_(self)
      case ("hermann_mauguin_symbol="); call read_HM_symbol_(self)
      case ("hm_symbol=             "); call read_HM_symbol_(self)
      case ("it_symbol=             "); call read_IT_symbol_(self)
      case ("jones_faithful_symbols="); call read_jones_faithful_symbols_(self)
      case ("put                    "); call put_(self)
      case default; allocate(tonto%known_keywords(8))
      tonto%known_keywords(1) = "}                      "
      tonto%known_keywords(2) = "analyze                "
      tonto%known_keywords(3) = "hall_symbol=           "
      tonto%known_keywords(4) = "hermann_mauguin_symbol="
      tonto%known_keywords(5) = "hm_symbol=             "
      tonto%known_keywords(6) = "it_symbol=             "
      tonto%known_keywords(7) = "jones_faithful_symbols="
      tonto%known_keywords(8) = "put                    "
      call unknown_(tonto,word,"SPACEGROUP:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_IT_symbol(self)
   ! Read the internation table symbol
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: symbol
      call read_(stdin,symbol)
      call set_IT_symbol_(self,symbol)
   end subroutine

   subroutine read_Hall_symbol(self)
   ! Read the Hall symbol
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: symbol
      call read_(stdin,symbol)
      call set_Hall_symbol_(self,symbol)
   end subroutine

   subroutine read_HM_symbol(self)
   ! Read the Hermann-Mauguin symbol
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: symbol
      call read_(stdin,symbol)
      call set_HM_symbol_(self,symbol)
   end subroutine

   subroutine read_CIF(self,cif)
   ! Read information from a CIF file, "cif". For a valid spacegroup
   ! either the Hall symbol or the symmetry equivalent positions must
   ! be present. The Hermann-Maiguin symbol is only used if the Hall
   ! symbol is not found.
      type(SPACEGROUP_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (NOT cif%is_mmCIF) then; call read_smCIF_(self,cif)
      else; call read_mmCIF_(self,cif)
      end if
   end subroutine

   subroutine read_smCIF(self,cif)
   ! Read information from a small-molecule CIF file, "cif". For a
   ! valid spacegroup either the Hall symbol or the symmetry
   ! equivalent positions must be present. The Hermann-Maiguin symbol
   ! is only used if the Hall symbol is not found.
      type(SPACEGROUP_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: found,jf_found,matched
      STR(len=STR_SIZE) :: item,symbol,IT_symbol
      VEC(STR(len=STR_SIZE),:), allocatable :: jf_symbols,itemvec
      MAT3(REAL,:,:,:), allocatable :: mat
      MAT(STR(len=STR_SIZE),:,:), allocatable :: list

      ! Initialise
      self%Hall_symbol = " "
      self%HM_symbol = " "
      self%analyzed = FALSE

      ! First look for the Hall symbol, trying old and new labels
      item = "_symmetry_space_group_name_Hall"
      call find_item_(cif,trim(item),found)
      if (NOT found) then  ! cif v 1.1
         item = "_space_group_name_Hall"
         call find_item_(cif,trim(item),found)
      end if

      ! Read the Hall symbol if there
      if (found) then
         call read_item_(cif,trim(item),symbol,itemvec)
         if (symbol(1:1)/="?") then
            call set_Hall_symbol_(self,symbol)
            ENSURE(NOT allocated(itemvec),"SPACEGROUP:read_smCIF ... multi-line Hall symbol")
         else  ! "?" is not a valid symbol
            found = FALSE
         end if
      end if

      ! The Hall symbol was not there ...
      if (NOT found) then

         ! Look for a HM symbol
         item = "_symmetry_space_group_name_H-M"
         call find_item_(cif,trim(item),found)
         if (NOT found) then  ! cif v 1.1
            item = "_space_group_name_H-M_alt"
            call find_item_(cif,trim(item),found)
         end if

         ! Read the Hermann-Mauguin symbol
         if (found) then
            call read_item_(cif,trim(item),symbol,itemvec)
            if (symbol(1:1)/="?") then
               call set_HM_symbol_(self,symbol)
               ENSURE(NOT allocated(itemvec),"SPACEGROUP:read_smCIF ... multi-line HM symbol")
               if (self%IT_group_number==0) found = FALSE
            else
               found = FALSE
            end if
         end if
      end if

      ! The HM symbol was not there either ... 
      if (NOT found) then

         ! In desperation, try Int Tables number
         item = "_symmetry_Int_Tables_number"
         call find_item_(cif,trim(item),found)

         ! Read the IT symbol
         if (found) then
            call read_item_(cif,trim(item),IT_symbol,itemvec)
            call set_IT_symbol_(self,IT_symbol)
         else
            call set_IT_symbol_(self,"?")
         end if
      end if

      ! Now look for Jones faithful symbols, if there ...
      item = "_symmetry_equiv_pos_as_xyz"
      call find_looped_item_(cif,trim(item),jf_found)
      if (NOT jf_found) then  ! cif v 1.1
         item = "_space_group_symop_operation_xyz"
         call find_item_(cif,trim(item),jf_found)
      end if

      ! Always try and read the Jones-Faithful symbol
      if (jf_found) then
         call create_(list,6,2)
         list = transpose(reshape(["/2 +","/2+ ","/2 -","/2- ","/3 +","/3+ ","/3 -","/3- ","/4 +","/4+ ","/4 -","/4- "],[2,6]))
         call set_replacement_list_(cif%file,list)
         call read_looped_item_(cif,trim(item),jf_symbols)
         call destroy_replacement_list_(cif%file)
         call destroy_(list)
         call create_(mat,4,4,size(jf_symbols))
         call decode_Jones_Faithful_symbols_(jf_symbols,mat)
         call destroy_(jf_symbols)
      end if

      ! Get spacegroup symbol brute-force
      if (((NOT found) OR is_one_of_(self%Hall_symbol,[" ","?"])) AND jf_found) then
         call set_from_symops_(self,mat)
      end if

      ! Analyze the spacegroup symbol
      if (self%Hall_symbol/=" " AND self%Hall_symbol/="?") call analyze_(self)

      ! Compare with JF matrices, if there
      if (self%analyzed AND allocated(mat)) then
         matched = seitz_same_as_(self,mat,warn=TRUE)
         if (NOT matched) then
            call flush_(stdout)
            call text_(stdout,"WARNING: inconsistent seitz matrices, using Jones-Faithful matrices")
            self%seitz = mat
            self%n_seitz = size(mat,3)
            call destroy_(self%product_of_symops)
            call destroy_(self%inverse_of_symops)
            call create_(self%product_of_symops,self%n_seitz,self%n_seitz)
            call create_(self%inverse_of_symops,self%n_seitz)
            call make_multiplication_table_(self)
            call make_inverted_symops_(self)
            call make_translated_symops_(self)
            call make_unique_SF_symops_(self)

            ! This counts as being analyzed ...
            self%analyzed = TRUE
         else
            call destroy_(mat)
         end if
      end if

      ! Use JF matrices is spacegroup not there
      if (NOT self%analyzed AND allocated(mat)) then
         self%seitz = mat
         self%n_seitz = size(mat,3)
         call destroy_(self%product_of_symops)
         call destroy_(self%inverse_of_symops)
         call create_(self%product_of_symops,self%n_seitz,self%n_seitz)
         call create_(self%inverse_of_symops,self%n_seitz)
         call make_multiplication_table_(self)
         call make_inverted_symops_(self)
         call make_translated_symops_(self)
         call make_unique_SF_symops_(self)

         ! This counts as being analyzed ...
         self%analyzed = TRUE
      end if

      ! Use JF matrices is spacegroup not there
      if (NOT self%analyzed) then
         DIE("SPACEGROUP:read_smCIF ... Can't find spacegroup information")
      end if
   end subroutine

   subroutine read_mmCIF(self,cif)
   ! Read information from a macromolecular CIF file, "cif". For a
   ! valid spacegroup either the Hall symbol or the symmetry
   ! equivalent positions must be present. The Hermann-Maiguin symbol
   ! is only used if the Hall symbol is not found.
      type(SPACEGROUP_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: found,jf_found,matched
      STR(len=STR_SIZE) :: item,symbol,IT_symbol
      VEC(STR(len=STR_SIZE),:), allocatable :: jf_symbols,itemvec
      MAT3(REAL,:,:,:), allocatable :: mat
      MAT(STR(len=STR_SIZE),:,:), allocatable :: list

      ! Initialise
      self%Hall_symbol = " "
      self%HM_symbol = " "
      self%analyzed = FALSE

      ! First look for the Hall symbol, trying old and new labels
      item = "_symmetry.space_group_name_Hall"
      call find_item_(cif,trim(item),found)
      if (NOT found) then  ! cif v 1.1
         item = "_space_group.name_Hall"
         call find_item_(cif,trim(item),found)
      end if

      ! Read the Hall symbol if there
      if (found) then
         call read_item_(cif,trim(item),symbol,itemvec)
         if (symbol(1:1)/="?") then
            call set_Hall_symbol_(self,symbol)
            ENSURE(NOT allocated(itemvec),"SPACEGROUP:read_mmCIF ... multi-line Hall symbol")
         else  ! "?" is not a valid symbol
            found = FALSE
         end if
      end if

      ! The Hall symbol was not there ...
      if (NOT found) then

         ! Look for a HM symbol
         item = "_symmetry.space_group_name_H-M"
         call find_item_(cif,trim(item),found)
         if (NOT found) then  ! cif v 1.1
            item = "_space_group.name_H-M_alt"
            call find_item_(cif,trim(item),found)
         end if

         ! Read the Hermann-Mauguin symbol
         if (found) then
            call read_item_(cif,trim(item),symbol,itemvec)
            if (symbol(1:1)/="?") then
               call set_HM_symbol_(self,symbol)
               ENSURE(NOT allocated(itemvec),"SPACEGROUP:read_mmCIF ... multi-line HM symbol")
               if (self%IT_group_number==0) found = FALSE
            else
               found = FALSE
            end if
         end if
      end if

      ! The HM symbol was not there either ... 
      if (NOT found) then

         ! In desperation, try Int Tables number
         item = "_symmetry.Int_Tables_number"
         call find_item_(cif,trim(item),found)

         ! Read the IT symbol
         if (found) then
            call read_item_(cif,trim(item),IT_symbol,itemvec)
            call set_IT_symbol_(self,IT_symbol)
         else
            call set_IT_symbol_(self,"?")
         end if
      end if

      ! Now look for Jones faithful symbols, if there ...
      item = "_symmetry_equiv.pos_as_xyz"
      call find_looped_item_(cif,trim(item),jf_found)
      if (NOT jf_found) then  ! cif v 1.1
         item = "_space_group_symop.operation_xyz"
         call find_item_(cif,trim(item),jf_found)
      end if

      ! Always try and read the Jones-Faithful symbol
      if (jf_found) then
         call create_(list,6,2)
         list = transpose(reshape(["/2 +","/2+ ","/2 -","/2- ","/3 +","/3+ ","/3 -","/3- ","/4 +","/4+ ","/4 -","/4- "],[2,6]))
         call set_replacement_list_(cif%file,list)
         call read_looped_item_(cif,trim(item),jf_symbols)
         call destroy_replacement_list_(cif%file)
         call destroy_(list)
         call create_(mat,4,4,size(jf_symbols))
         call decode_Jones_Faithful_symbols_(jf_symbols,mat)
         call destroy_(jf_symbols)
      end if

      ! Get spacegroup symbol brute-force
      if (((NOT found) OR is_one_of_(self%Hall_symbol,[" ","?"])) AND jf_found) then
         call set_from_symops_(self,mat)
      end if

      ! Analyze the spacegroup symbol
      if (self%Hall_symbol/=" " AND self%Hall_symbol/="?") call analyze_(self)

      ! Compare with JF matrices, if there
      if (self%analyzed AND allocated(mat)) then
         matched = seitz_same_as_(self,mat,warn=TRUE)
         if (NOT matched) then
            call flush_(stdout)
            call text_(stdout,"WARNING: inconsistent seitz matrices, using Jones-Faithful matrices")
            self%seitz = mat
            self%n_seitz = size(mat,3)
            call destroy_(self%product_of_symops)
            call destroy_(self%inverse_of_symops)
            call create_(self%product_of_symops,self%n_seitz,self%n_seitz)
            call create_(self%inverse_of_symops,self%n_seitz)
            call make_multiplication_table_(self)
            call make_inverted_symops_(self)
            call make_translated_symops_(self)
            call make_unique_SF_symops_(self)

            ! This counts as being analyzed ...
            self%analyzed = TRUE
         else
            call destroy_(mat)
         end if
      end if

      ! Use JF matrices is spacegroup not there
      if (NOT self%analyzed AND allocated(mat)) then
         self%seitz = mat
         self%n_seitz = size(mat,3)
         call destroy_(self%product_of_symops)
         call destroy_(self%inverse_of_symops)
         call create_(self%product_of_symops,self%n_seitz,self%n_seitz)
         call create_(self%inverse_of_symops,self%n_seitz)
         call make_multiplication_table_(self)
         call make_inverted_symops_(self)
         call make_translated_symops_(self)
         call make_unique_SF_symops_(self)

         ! This counts as being analyzed ...
         self%analyzed = TRUE
      end if

      ! Use JF matrices is spacegroup not there
      if (NOT self%analyzed) then
         DIE("SPACEGROUP:read_mmCIF ... Can't find spacegroup information")
      end if
   end subroutine

   subroutine read_jones_faithful_symbols(self)
   ! Read the jones faithful symbols.
      type(SPACEGROUP_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: jf_symbols
   ! list :: MAT{STR}@
      call read_all_(stdin,jf_symbols)

   ! jf_symbols.remove_blanks
   ! list.create(6,2) 

    !                 "/4 -","/4- "],[2,6]))
   ! list.destroy
   ! do i = 1,list.dim
   !    jf_symbols.replace_all(list(1,i),list(2,i)) 
   ! end

      ! Generate the Seitz operators
      self%n_seitz = size(jf_symbols)
      call create_(self%seitz,4,4,self%n_seitz)
      call decode_Jones_Faithful_symbols_(jf_symbols,self%seitz)
      call destroy_(jf_symbols)

      ! These are used to save time in SF calcs
      call destroy_(self%product_of_symops)
      call destroy_(self%inverse_of_symops)
      call create_(self%product_of_symops,self%n_seitz,self%n_seitz)
      call create_(self%inverse_of_symops,self%n_seitz)
      call make_multiplication_table_(self)
      call make_inverted_symops_(self)
      call make_translated_symops_(self)
      call make_unique_SF_symops_(self)

      ! This counts as being analyzed ...
      self%analyzed = TRUE
   end subroutine


!  =======================
!  Spacegroup construction
!  =======================

   subroutine update(self)
   ! Analyze the spacegroup symbol
      type(SPACEGROUP_TYPE), INOUT :: self
      call analyze_(self)
   end subroutine

   subroutine analyze(self)
   ! Analyze the spacegroup symbol
      type(SPACEGROUP_TYPE), INOUT :: self

      ! Do we need to anlayze it?
      if (self%analyzed) return

      ! Clean up
      call destroy_ptr_part_(self)

      ! Decode the Hall symbol
      call decode_Hall_symbol_(self)
      call set_lattice_type_(self)
      call set_spacegroup_order_(self)

      ! Make the Seitz operators
      call create_(self%seitz,4,4,self%n_seitz)
      self%seitz = ZERO
      call form_L_seitz_(self)
      call form_S_seitz_(self)
      call generate_seitz_(self)
      call shift_origin_(self)  ! NOT PURE

      ! Group things
      call create_(self%product_of_symops,self%n_seitz,self%n_seitz)
      call create_(self%inverse_of_symops,self%n_seitz)
      call make_multiplication_table_(self)

      ! These are used to save time in SF calcs
      call make_inverted_symops_(self)
      call make_translated_symops_(self)
      call make_unique_SF_symops_(self)

      ! Analyzed
      self%analyzed = TRUE
   end subroutine

   PURE subroutine decode_Hall_symbol(self)
   ! Decode the Hall notation symbol for the spacegroup
      type(SPACEGROUP_TYPE), INOUT :: self
      type(BUFFER_TYPE) :: buffer
      STR(len=STR_SIZE) :: symbol,lattice,generator
      INT :: g

      ! Replace explicit origin brackets (if any)
      symbol = trim(self%Hall_symbol)
      if (index(trim(symbol),"(")/=0) call replace_all_(symbol,"(","( ")
      if (index(trim(symbol),")")/=0) call replace_all_(symbol,")"," )")

      ! Analyze the symbol into items
      call set_and_analyze_(buffer,symbol)

      ! Get lattice symbol
      call get_item_(buffer,lattice)

      ! === Decode lattice symbol ===
      call decode_Hall_lattice_symbol_(self,lattice)

      ! Defaults for crystal axis "N" symbols
      self%has_axis_bar = FALSE
      self%axis_order = 1
      self%axis_direction_symbol = "_"  ! this means a blank
      self%axis_direction_index = 0
      self%translation_symbol = "_"
      self%origin_shift = 0

      ! Analyze the crystal axis "N" symbols
      ! Loop over crystal axis generators
      do g = 1,4

         ! Anything in buffer?
         if (exhausted_(buffer)) return

         ! Get the generator
         call get_item_(buffer,generator)

         ! Explicit origin present?
         if (generator(1:1)=="(") exit

         ! === Decode generator symbol ===
         call decode_Hall_generator_symbol_(self,generator,g)
      end do

      ! Set origin shift
      if (NOT exhausted_(buffer)) then

         ! Extract change of origin vector
         call get_int_(buffer,self%origin_shift(1))
         call get_int_(buffer,self%origin_shift(2))
         call get_int_(buffer,self%origin_shift(3))
         call get_item_(buffer,generator)

         ! Sanity check
         ENSURE(generator==")","SPACEGROUP:decode_Hall_symbol ... missing origin-shift closing parenthesis")
         ENSURE(exhausted_(buffer),"SPACEGROUP:decode_Hall_symbol ... extra items at end of Hall symbol")
      end if
   end subroutine

   PURE subroutine decode_Hall_generator_symbol(self,generator,g)
   ! Decode the "g"-th Hall symbol "generator" which has form N^A_T
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: generator
      INT, IN :: g
      INT :: i
      STR(len=1) :: gen1,as
      STR(len=STR_SIZE) :: gen
      BIN :: impossible,alpha_used,frac_used
   ENSURE(generator/=" ","SPACEGROUP:decode_Hall_generator_symbol ... generator "//trim(to_str_(g))//" symbol is blank")

      ! Next character
      gen = generator
      gen1 = generator(1:1)

      ! Centrosymmetric?
      self%has_axis_bar(g) = FALSE
      if (gen1==bar_symbol) then
         self%has_axis_bar(g) = TRUE

         ! Next character
         gen = gen(2:)
         gen1 = gen(1:1)
      end if

      ! Must have first generator's axis order
      ENSURE(g/=1 OR gen1/=" ","SPACEGROUP:decode_Hall_generator_symbol ... missing first axis order symbol")

      ! Get axis order
      self%axis_order(g) = 1
      if (includes_(N_rotation_order_symbols,gen1)) then
         self%axis_order(g) = index(N_rotation_order_symbols,gen1)

         ! Next character
         gen = gen(2:)
         gen1 = gen(1:1)
      end if

      ! Set default axis symbol (underscore is blank)
      call set_default_axis_directions_(self,g)
      if (gen1==" ") return

      ! Any non-default axis symbol?
      if (includes_(A_axis_direction_symbols,gen1)) then
         as = gen1
         self%axis_direction_symbol(g) = as
         self%axis_direction_index(g) = index(A_axis_direction_symbols,as)

         ! Sanity checks
         impossible = (as==dash_symbol OR as==double_dash_symbol) AND (self%axis_order(g)/=2 OR g==1)
         ENSURE(NOT impossible,"SPACEGROUP:decode_Hall_generator_symbol ... ' or "" is only for 2nd-order rotation, after 1st axis")
         impossible = as==star_symbol AND self%axis_order(g)/=3
         ENSURE(NOT impossible,"SPACEGROUP:decode_Hall_generator_symbol ... * axis only for 3rd-order rotation")

         ! Next character
         gen = gen(2:)
         gen1 = gen(1:1)
      end if

      ! Check for translation symbols: a,b,c,u,v,w,n and 1 .. 6
      self%translation_symbol(:,g) = "_"  ! this means a blank
      if (gen1==" ") return
      alpha_used = FALSE
      frac_used = FALSE

      ! Loop over (at most 3) translation symbols ...
      do i = 1,3
         if (gen1==" ") exit
         if (includes_(alpha_translation_symbols,gen1)) then

         ! WARN_IF(frac_used,"mixed alphabetical/numeric translation symbols")
            self%translation_symbol(i,g) = gen1
            gen = gen(2:)
            gen1 = gen(1:1)
            alpha_used = TRUE
         else if (includes_(fractional_translation_symbols,gen1)) then
            alpha_used = alpha_used
         ! WARN_IF(alpha_used,"mixed alphabetical/numeric translation symbols")
            self%translation_symbol(i,g) = gen1
            gen = gen(2:)
            gen1 = gen(1:1)
            frac_used = TRUE
      ! else 
      !    DIE("Unknown translation symbol, "//gen)
         end if
      end do
   end subroutine

   PURE subroutine set_default_axis_directions(self,g)
   ! Set the default axis directions for generator "g".
   ! NOTE: this only works if the rotation axis order is up to date.
      type(SPACEGROUP_TYPE), INOUT :: self
      INT, IN :: g
   ENSURE(is_in_range_(g,[1,4]),"SPACEGROUP:set_default_axis_directions ... g must be in range [1,4]")
      self%axis_direction_symbol(g) = "_"
      self%axis_direction_index(g) = 0
      select case (g)

      case (1)
         self%axis_direction_symbol(g) = "z"
         self%axis_direction_index(g) = 3
      !  .axis_direction_index(g)  = index(A_axis_direction_symbols,"z")

      case (2)
         if (self%axis_order(2)==2) then

         ! See: International Tables B 1.4.2.3.1 (ii)
         ! Implicit a axis
            if (self%axis_order(1)==2 OR self%axis_order(1)==4) then
               self%axis_direction_symbol(g) = "x"
               self%axis_direction_index(g) = 1
      !  .axis_direction_index(g)  = index(A_axis_direction_symbols,"x")
            end if

         ! Implicit a-b axis symbol. 
         ! See: International Tables B Table 1.4.2.5 for: N^z 2' a-b
            if (self%axis_order(1)==3 OR self%axis_order(1)==6) then
               self%axis_direction_symbol(g) = "'"
               self%axis_direction_index(g) = 3
            end if
         end if

      case (3)
         if (self%axis_order(3)==3) then
            self%axis_direction_symbol(g) = "*"
            self%axis_direction_index(g) = 6
      !  .axis_direction_index(g)  = index(A_axis_direction_symbols,"*")
         end if
      end select
   end subroutine

   PURE subroutine decode_Hall_lattice_symbol(self,lattice)
   ! Decode the Hall "lattice" symbol
      type(SPACEGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: lattice
      STR(len=STR_SIZE) :: latt
      STR(len=1) :: latt1

      ! Store lattice
      latt = lattice

      ! Centrosymmetric lattice? 
      self%centrosymmetric = FALSE
      if (latt(1:1)==bar_symbol) then
         self%centrosymmetric = TRUE
         latt = latt(2:)
      end if

      ! Get lattice symbol
      call to_upper_case_(latt)
      latt1 = latt(1:1)
      ENSURE(includes_(lattice_symbols,latt1),"SPACEGROUP:decode_Hall_lattice_symbol ... unknown lattice, "//latt1)

      ! Set the lattice symbol & lattice symbol index
      self%lattice_symbol = latt1
      self%lattice_symbol_index = index(lattice_symbols,latt1)
   end subroutine

   PURE subroutine set_lattice_type(self)
   ! Set the lattice type name
      type(SPACEGROUP_TYPE), INOUT :: self
   ENSURE(all_in_range_(self%axis_order,[1,6]),"SPACEGROUP:set_lattice_type ... wrong axis orders")
      if (self%axis_order(3)==3) then; self%lattice_type = "Cubic"
      else if (self%axis_order(1)==4 AND self%axis_order(3)/=3) then; self%lattice_type = "Tetragonal"
      else if (self%axis_order(1)==2 AND self%axis_order(2)==2) then; self%lattice_type = "Orthorhombic"
      else if (self%axis_order(1)==2 AND self%axis_order(2)==1) then; self%lattice_type = "Monoclinic"
      else if (self%axis_order(1)==6) then; self%lattice_type = "Hexagonal"
      else if (self%axis_order(1)==3) then; self%lattice_type = "Trigonal"
      else if (self%axis_order(1)==1) then; self%lattice_type = "Triclinic"
      end if
   end subroutine

   PURE subroutine set_spacegroup_order(self)
   ! Work out the spacegroup orders.
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: g
   ENSURE(is_in_range_(self%lattice_symbol_index,[1,8]),"SPACEGROUP:set_spacegroup_order ... wrong lattice index")

      ! No. of translation generators
      self%nL = n_implied_translations(self%lattice_symbol_index)
      if (self%centrosymmetric) self%nL = 2*self%nL

      ! No. of seitz matrices
      self%n_seitz = self%nL*product(self%axis_order)

      ! No. of generators
      self%nG = self%nL
      do g = 1,4
         if (self%axis_order(g)>1) self%nG = self%nG+1
      end do
   end subroutine

   PURE subroutine form_L_seitz(self)
   ! Form the seitz generators corresponding to the lattice symbol L
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: n,v
      n = self%nL
      if (self%centrosymmetric) n = self%nL/2
      do v = 1,n
         self%seitz(1:3,1:3,v) = NN(:,:,1,1)  ! unit matrix
         self%seitz(1:3,4,v) = T_implied(1:3,v,self%lattice_symbol_index)
         self%seitz(4,1:3,v) = ZERO
         self%seitz(4,4,v) = ONE
      end do
      if (NOT self%centrosymmetric) return
      do v = 1,n
         self%seitz(1:3,1:3,n+v) = -NN(:,:,1,1)  ! inversion matrix
         self%seitz(1:3,4,n+v) = T_implied(1:3,v,self%lattice_symbol_index)
         self%seitz(4,1:3,n+v) = ZERO
         self%seitz(4,4,n+v) = ONE
      end do
   end subroutine

   PURE subroutine form_S_seitz(self)
   ! Add the rotational seitz matrices to the generator list
      type(SPACEGROUP_TYPE), INOUT :: self
      VEC(REAL,3) :: t
      REAL :: fac
      STR(len=1) :: as,ts
      INT :: g,N,i,v,ax,aw,sgn
      self%nR = 0
      do g = 1,4

         ! Axis order & axis direction index
         N = self%axis_order(g)
         as = self%axis_direction_symbol(g)
         ax = self%axis_direction_index(g)
         if (N<=1) cycle
         t = ZERO

         ! Loop over translation symbols
         do i = 1,3

            ! Translational symbol
            ts = self%translation_symbol(i,g)
            if (ts=="_") exit

            ! ===Alphabetical translation===
            v = index(alpha_translation_symbols,ts)
            if (v/=0) then
               t = t+T_translation_vector(:,v)
            end if

            ! ===Fractional (numerical) translation===
            v = index(fractional_translation_symbols,ts)
            if (v/=0) then

               ! This is the fractional translation
               fac = REALIFY(v)/REALIFY(N)

               ! This is the direction
               select case (ax)
               case (1); t(1) = t(1)+fac  ! x axis
               case (2); t(2) = t(2)+fac  ! y axis
               case (3); t(3) = t(3)+fac  ! z axis
               case (6)  ! * axis
                  t(1) = t(1)+fac  ! * a+b+c
                  t(2) = t(2)+fac
                  t(3) = t(3)+fac
               case (4)  ! " axis
                  ENSURE(g/=1,"SPACEGROUP:form_S_seitz ... must have previous axis")
                  aw = self%axis_direction_index(g-1)
                  select case (aw)
                  case (1)
                     t(2) = t(2)+fac  ! " b+c
                     t(3) = t(3)+fac
                  case (2)
                     t(1) = t(1)+fac  ! " a+c
                     t(3) = t(3)+fac
                  case (3)
                     t(1) = t(1)+fac  ! " a+b
                     t(2) = t(2)+fac
                  end select
               case (5)  ! ' axis
               end select
            end if
         end do  ! loop on translations

         ! Implicit a-b dash symbol. See:
         ! International Tables B 1.4.2.3.1 (ii) and
         ! International Tables B Table 1.4.2.5 for: N^z 2' a-b
         if (as=="'") then
            ENSURE(g/=1,"SPACEGROUP:form_S_seitz ... cannot have dash symbols on 1st generator")
            N = 5  ! see comments for NN matrices above
            ax = self%axis_direction_index(g)  ! NOTE: must be z
            ENSURE(ax==3,"SPACEGROUP:form_S_seitz ... axis index index must be 3")
         end if

         ! Double-dash axis symbol
         if (as=='"') then
            ENSURE(g/=1,"SPACEGROUP:form_S_seitz ... cannot have dash symbols on 1st generator")
            N = 7  ! see comments for NN matrices above
            ax = self%axis_direction_index(g-1)  ! NOTE: axis of previous N symbol
            ENSURE(is_in_range_(ax,[1,3]),"SPACEGROUP:form_S_seitz ... previous generator rotation axis must be in range [1,3]")
         end if

         ! Star axis symbol
         if (as=="*") then
            ENSURE(N==3,"SPACEGROUP:form_S_seitz ... star operation is only for order 3 axis")
            N = 8
            ax = 1
         end if

         ! Final sanity check
         ENSURE(is_in_range_(N,[1,8]),"SPACEGROUP:form_S_seitz ... Wrong axis for rotation matrix NN")
         ENSURE(is_in_range_(ax,[1,3]),"SPACEGROUP:form_S_seitz ... Wrong axis symbol index")

         ! Set sign
         sgn = 1
         if (self%has_axis_bar(g)) sgn = -1

         ! Add the generators to the list here
         self%nR = self%nR+1
         self%seitz(1:3,1:3,self%nL+self%nR) = sgn*NN(:,:,ax,N)
         self%seitz(1:3,4,self%nL+self%nR) = t(:)
         self%seitz(4,1:3,self%nL+self%nR) = ZERO
         self%seitz(4,4,self%nL+self%nR) = ONE
      end do  ! loop over axis
   end subroutine

   PURE subroutine generate_seitz(self)
   ! Generate all the Seitz matrices from the minimal generating set.
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: m,i,j,k
      BIN :: equal
   ENSURE(self%nG>0,"SPACEGROUP:generate_seitz ... number of generators must be positive")
      m = self%nG

      ! Anything to do?
      if (m==self%n_seitz) return
      i = 1
      main: do
         i = i+1
         j = 1

         ! Loop over pair products
         pair_products: do
            j = j+1

            ! Get new pair-product
            call to_product_of_(self%seitz(:,:,m+1),self%seitz(:,:,j),self%seitz(:,:,i))

            ! Put translation part to unit cell
            self%seitz(1,4,m+1) = mod(self%seitz(1,4,m+1)+TWO+TOL(8),ONE)-TOL(8)
            self%seitz(2,4,m+1) = mod(self%seitz(2,4,m+1)+TWO+TOL(8),ONE)-TOL(8)
            self%seitz(3,4,m+1) = mod(self%seitz(3,4,m+1)+TWO+TOL(8),ONE)-TOL(8)

            ! Pair-product equal to previous?
            equal = FALSE
            do k = 1,m
               equal = same_as_(self%seitz(:,:,m+1),self%seitz(:,:,k))
               if (equal) exit
            end do

            ! A new seitz matrix found
            if (NOT equal) m = m+1

            ! Finished everything?
            if (m==self%n_seitz) exit main

            ! Next pair?
            if (j==m) exit pair_products
         end do pair_products

         ! No more pairs -- should die here?
         if (i==m) exit main
      end do main
   end subroutine

   PURE subroutine make_multiplication_table(self)
   ! Make the multiplication table
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: i,j,k
      BIN :: equal
      MAT(REAL,4,4) :: prod
   ENSURE(self%n_seitz>0,"SPACEGROUP:make_multiplication_table ... no seitz matrices")

      ! Make the multiplication table
      do i = 1,self%n_seitz
         do j = 1,self%n_seitz

         ! Get product
            call to_product_of_(prod,self%seitz(:,:,j),self%seitz(:,:,i))

         ! Put translation part to unit cell
            prod(1,4) = mod(prod(1,4)+TWO+TOL(8),ONE)-TOL(8)
            prod(2,4) = mod(prod(2,4)+TWO+TOL(8),ONE)-TOL(8)
            prod(3,4) = mod(prod(3,4)+TWO+TOL(8),ONE)-TOL(8)

         ! Product equal to previous?
            equal = FALSE
            do k = 1,self%n_seitz
               equal = same_as_(prod,self%seitz(:,:,k))
               if (NOT equal) cycle
               self%product_of_symops(j,i) = k
               exit
            end do

         ! A new seitz matrix found
            if (NOT equal) then

         ! stdout.text("prod:")
         ! stdout.put(prod)
         ! stdout.flush
         ! stdout.text("Seitz matrices:")
         ! do k = 1,.n_seitz
         !    stdout.text("Symop "//trim(k.to_str)//":")
         !    stdout.put(.seitz(:,:,k))
         ! end
         ! stdout.flush
         ! DIE("product prod not found")
            end if
         end do
      end do

      ! Get the (unique!) inverse symop
      do i = 1,self%n_seitz
         do j = 1,self%n_seitz
            if (self%product_of_symops(i,j)/=1) cycle
            self%inverse_of_symops(i) = j
            exit
         end do
      end do
   end subroutine

   subroutine shift_origin(self)
   ! Shift the origin of the Seitz matrices, by doing a similarity transform
      type(SPACEGROUP_TYPE), INOUT :: self
      MAT(REAL,4,4) :: V
      VEC(REAL,3) :: t
      INT :: i
      if (same_as_(self%origin_shift,[0,0,0])) return
      call to_unit_mx_(V)
      V(1:3,4) = self%origin_shift/TWELVE
      call similarity_transform_12_(self%seitz,V)

      ! Make sure translations do not exceed one unit cell
      do i = 1,self%n_seitz
         t = self%seitz(1:3,4,i)
         if (all(abs(t)<ONE-TOL(8))) cycle
         t = mod(t+TWO+TOL(8),ONE)-TOL(8)
         self%seitz(1:3,4,i) = t
      end do
   end subroutine


!  =======================================
!  Translation and inversion unqiue symops
!  =======================================

   PURE subroutine make_inverted_symops(self)
   ! Determine which of the .seitz symops are related by inversion.
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: n,i,j
      BIN :: inverted
   ENSURE(allocated(self%seitz),"SPACEGROUP:make_inverted_symops ... Seitz matrices not initialized")
      call destroy_(self%inverted_symop)
      call create_(self%inverted_symop,self%n_seitz)
      self%inverted_symop = 0
      self%n_inverted_symops = 0
      n = 0

      ! Loop on symop pairs i>j
      do i = 2,self%n_seitz
         do j = 1,i-1
            inverted = equals_(self%seitz(1:3,1:3,i),-self%seitz(1:3,1:3,j))
            if (NOT inverted) cycle
            n = n+1
            self%inverted_symop(i) = j
            exit
         end do
      end do
      self%n_inverted_symops = n
   end subroutine

   PURE subroutine make_translated_symops(self)
   ! Determine which of the .seitz symops are related by translation,
   ! (including translation by the zero vector) but *not* inversion.
   ! Unlike inversions, a symop is at least a (zero) translation of
   ! itself.
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: i,j
      BIN :: translated
   ENSURE(allocated(self%inverted_symop),"SPACEGROUP:make_translated_symops ... no inverted_symop array")
   ENSURE(NOT allocated(self%translated_symop),"SPACEGROUP:make_translated_symops ... translated_symop exists!")
   ENSURE(allocated(self%seitz),"SPACEGROUP:make_translated_symops ... Seitz matrices not initialized")
      call destroy_(self%translated_symop)
      call create_(self%translated_symop,self%n_seitz)
      self%translated_symop = 0
      do i = 1,self%n_seitz

         ! Symop i is at least a translation 
         ! of itself
         self%translated_symop(i) = i

         ! Skip inversions
         if (self%inverted_symop(i)>0) cycle

         ! Loop on symop pairs i>=j
         do j = 1,i-1
            translated = equals_(self%seitz(1:3,1:3,i),self%seitz(1:3,1:3,j))
            if (NOT translated) cycle
            self%translated_symop(i) = j
            exit
         end do
      end do
   end subroutine

   PURE subroutine make_unique_SF_symops(self)
   ! Determine which are the structure-factor unique symops, i.e.
   ! those which generate fragment geometries which are different than
   ! a translation or inversion. This list should be a superset of the
   ! reduced symops.
      type(SPACEGROUP_TYPE), INOUT :: self
      INT :: n,i
   ENSURE(allocated(self%inverted_symop),"SPACEGROUP:make_unique_SF_symops ... no inverted_symop array")
   ENSURE(allocated(self%translated_symop),"SPACEGROUP:make_unique_SF_symops ... no translated_symop array")

      ! How many unique symops?
      n = 0
      do i = 1,self%n_seitz
         if (self%inverted_symop(i)>0) cycle
         if (self%translated_symop(i)<i) cycle  ! Only true translations count
         n = n+1
      end do

      ! Set the # of unique symops
      self%n_unique_SF_symops = n

      ! Make the unique_SF_symop index array
      call create_(self%unique_SF_symop,n)
      n = 0
      do i = 1,self%n_seitz
         if (self%inverted_symop(i)>0) cycle
         if (self%translated_symop(i)<i) cycle
         n = n+1
         self%unique_SF_symop(n) = i
      end do
   end subroutine

   PURE function unique_SF_symop_mat(self,u) result (res)
   ! Return the "u"-th reduced symop matrix in the unique list made by routine
   ! ".make_reduced_symops".
      type(SPACEGROUP_TYPE), IN :: self
      INT, IN :: u
      MAT(REAL,3,3) :: res
      INT :: s
   ENSURE(self%analyzed,"SPACEGROUP:unique_SF_symop_mat ... spacegroup not analyzed")
   ENSURE(allocated(self%unique_SF_symop),"SPACEGROUP:unique_SF_symop_mat ... no unique_SF_symops!")
   ENSURE(u<=self%n_unique_SF_symops,"SPACEGROUP:unique_SF_symop_mat ... symop index out of range")
   ENSURE(u>0,"SPACEGROUP:unique_SF_symop_mat ... symop index out of range")
      s = self%unique_SF_symop(u)
      res = self%seitz(1:3,1:3,s)
   end function


!  ============================
!  Jones-Faithful decomposition
!  ============================

   PURE subroutine decode_Jones_Faithful_symbols(symbols,mat)
   ! Decode a series of Jones-Faithful symbols and place in the Seitz matrices "mat".
      VEC(STR(len=*),:), IN :: symbols
      MAT3(REAL,4,4,size(symbols)), OUT :: mat
      INT :: i
      do i = 1,size(symbols)
         call decode_Jones_Faithful_symbol_(symbols(i),mat(:,:,i))
      end do
   end subroutine

   PURE subroutine decode_Jones_Faithful_symbol(symbol,mat)
   ! Decode a single Jones-Faithful symbol, e.g. "x-y+z+1/2,y,z-x",
   ! and place in the Seitz matrix in "mat".
      STR(len=*), IN :: symbol
      MAT(REAL,4,4), OUT :: mat
      type(BUFFER_TYPE) :: buffer
      STR(len=STR_SIZE) :: word,item
      INT :: pos,comma,row
      REAL :: t
      mat = ZERO
      mat(4,4) = ONE

      ! Position in symbol
      pos = 0

      ! Loop over rows
      ! Note: the row is the product of "mat" with (x,y,z,1)
      do row = 1,3
         word = symbol(pos+1:)
         if (row<3) then

            ! Extract a comma or space part of the JF symbol
            comma = index(word,",")
            if (comma==0) comma = index(word," ")
            ENSURE(comma>1,"SPACEGROUP:decode_Jones_Faithful_symbol ... missing comma or space separator")

            ! Get the word for this row
            word = word(1:comma-1)
            ENSURE(word/=" ","SPACEGROUP:decode_Jones_Faithful_symbol ... empty row, "//trim(to_str_(row)))

            ! Increment pos to comma/space separator
            pos = pos+comma

            ! Increment pos to just before next symbol 
            comma = verify(symbol(pos+1:)," ")
            pos = pos+comma-1
         end if

         ! Separate out items before +/-
         call to_lower_case_(word)
         call remove_blanks_(word)
         call separate_before_(word,"+-")

         ! Heavier guns
         call set_defaults_(buffer)
         call set_and_analyze_(buffer,word)

         ! Extract all symbols
         do
            if (exhausted_(buffer)) exit
            call get_(buffer,item)
            select case (item)

            ! Simple cases
            case ("x","+x"); mat(row,1) = 1
            case ("y","+y"); mat(row,2) = 1
            case ("z","+z"); mat(row,3) = 1
            case ("-x"); mat(row,1) = -1
            case ("-y"); mat(row,2) = -1
            case ("-z"); mat(row,3) = -1

            ! Fractions and integers
            case default
               if (includes_(item,"/")) then

                  ! Should be a fraction
                  t = frac_to_real_(item)
                  t = modulo(t,ONE)  ! translate to first unit cell.
                  mat(row,4) = mat(row,4)+t
               else if (is_int_(item)) then  ! ignore any integers

            ! else
            !    DIE("unknown item, "//item.trim)
               end if
            end select  ! case
         end do
      end do
   end subroutine

   PURE subroutine recode_Jones_Faithful_symbol(jf,mat)
   ! Recode the seitz matrix "mat" into "jf"
      STR(len=*), OUT :: jf
      MAT(REAL,4,4), IN :: mat
      INT :: i
      STR(len=STR_SIZE) :: jj
      REAL :: val

      ! Build up the symbol jf for each row i
      do i = 1,3
         jj = " "
         val = mat(i,1)
         if (equals_(val,ZERO)) then
         else if (equals_(val,ONE)) then; jj = trim(jj)//"+x"
         else if (equals_(val,-ONE)) then; jj = trim(jj)//"-x"

      ! else; DIE("unknown value at col 1: "//trim(val.to_str))
         end if
         val = mat(i,2)
         if (equals_(val,ZERO)) then
         else if (equals_(val,ONE)) then; jj = trim(jj)//"+y"
         else if (equals_(val,-ONE)) then; jj = trim(jj)//"-y"

      ! else; DIE("unknown value at col 2: "//trim(val.to_str))
         end if
         val = mat(i,3)
         if (equals_(val,ZERO)) then
         else if (equals_(val,ONE)) then; jj = trim(jj)//"+z"
         else if (equals_(val,-ONE)) then; jj = trim(jj)//"-z"

      ! else; DIE("unknown value at col 3: "//trim(val.to_str))
         end if
         val = mat(i,4)
         if (equals_(val,ZERO)) then
         else if (equals_(val,HALF)) then; jj = trim(jj)//"+1/2"
         else if (equals_(val,-HALF)) then; jj = trim(jj)//"-1/2"
         else if (equals_(val,THIRD)) then; jj = trim(jj)//"+1/3"
         else if (equals_(val,-THIRD)) then; jj = trim(jj)//"-1/3"
         else if (equals_(val,TWOTHIRD)) then; jj = trim(jj)//"+2/3"
         else if (equals_(val,-TWOTHIRD)) then; jj = trim(jj)//"-2/3"
         else if (equals_(val,QUARTER)) then; jj = trim(jj)//"+1/4"
         else if (equals_(val,-QUARTER)) then; jj = trim(jj)//"-1/4"
         else if (equals_(val,THREEQUARTERS)) then; jj = trim(jj)//"+3/4"
         else if (equals_(val,-THREEQUARTERS)) then; jj = trim(jj)//"-3/4"
         else if (equals_(val,SIXTH)) then; jj = trim(jj)//"+1/6"
         else if (equals_(val,-SIXTH)) then; jj = trim(jj)//"-1/6"
         else if (equals_(val,FIVESIXTH)) then; jj = trim(jj)//"+5/6"
         else if (equals_(val,-FIVESIXTH)) then; jj = trim(jj)//"-5/6"

      ! else; DIE("unknown value at col 4: "//trim(val.to_str))
         end if
         if (i==1) then; jf = trim(jj)
         else if (jf==" ") then; jf = trim(jj)
         else; jf = trim(jf)//", "//trim(jj)
         end if
      end do
   end subroutine

   function seitz_same_as(self,mat,warn) result (res)
   ! Determine if the Seitz matrices are the same as those in "mat", except
   ! for a rearrangement of order.
      type(SPACEGROUP_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: mat
      BIN, optional, IN :: warn
      BIN :: res
      VEC(BIN,self%n_seitz) :: found,fownd
      MAT(REAL,4,4) :: tmp
      BIN :: out
      INT :: i,j
      STR(len=STR_SIZE) :: jf
   ENSURE(size(mat,1)==4,"SPACEGROUP:seitz_same_as ... wrong 1st dimension, mat")
   ENSURE(size(mat,2)==4,"SPACEGROUP:seitz_same_as ... wrong 1st dimension, mat")
      out = TRUE
      if (present(warn)) out = warn
      if (size(mat,3)/=self%n_seitz) then
         WARN_IF(out,"SPACEGROUP:seitz_same_as ... Group orders are not the same!")
         res = FALSE
      else
         found = FALSE
         fownd = FALSE
         do i = 1,self%n_seitz
            do j = 1,self%n_seitz
               if (found(j)) cycle
               if (same_as_(self%seitz(:,:,i),mat(:,:,j))) then
                  fownd(i) = TRUE
                  found(j) = TRUE
                  exit
               end if
            end do
         end do
         res = all(found)
         if (res) return

         ! Oops ...
         if (self%centrosymmetric) then
            do i = 1,self%n_seitz
               do j = 1,self%n_seitz
                  if (found(j)) cycle
                  tmp = -mat(:,:,j)  ! try inverting and rescaling
                  tmp(1,4) = mod(tmp(1,4)+TWO+TOL(8),ONE)-TOL(8)
                  tmp(2,4) = mod(tmp(2,4)+TWO+TOL(8),ONE)-TOL(8)
                  tmp(3,4) = mod(tmp(3,4)+TWO+TOL(8),ONE)-TOL(8)
                  tmp(4,4) = 1
                  if (same_as_(self%seitz(:,:,i),tmp)) then
                     fownd(i) = TRUE
                     found(j) = TRUE  ! found inverse ... we hope that
                     exit  ! its inverse pair also matches
                  end if
               end do
            end do
            res = all(found)
            if (res) return
         end if
      end if

      ! This is a debugging message
      if (out) then
         call flush_(stdout)
         call text_(stdout,"There were unmatched seitz matrices!")
         call text_(stdout,"Unmatched spacegroup-symbol matrices:")
         do i = 1,self%n_seitz
            if (fownd(i)) cycle
            call text_(stdout,"Matrix "//trim(to_str_(i))//":")
            call recode_Jones_Faithful_symbol_(jf,self%seitz(:,:,i))
            call put_(stdout,trim(jf))
            call flush_(stdout)
         end do
         call flush_(stdout)
         call text_(stdout,"Unmatched JF-symbol matrices:")
         do j = 1,size(mat,3)
            if (found(j)) cycle
            call recode_Jones_Faithful_symbol_(jf,mat(:,:,i))
            call put_(stdout,trim(jf))
            call flush_(stdout)
         end do
         call text_(stdout,"================WARNING====================")
         call text_(stdout,"Possible wrong spacegroup information below")
         call put_(self)
         call text_(stdout,"==============END WARNING==================")
      end if
   end function


!  ===================================
!  Spacegroup geometry transformations
!  ===================================

!   put_geometry_to_unit_cell(g)
!   ! Transform the geometry "g" in fractional coordinates into the
!   ! (1,1,1) unit cell
!       g :: MAT{REAL}
!
!   ENSURE(.seitz.allocated,"No Seitz matrices!")
!   ENSURE(g.dim1==3,"incorrect size for array g")
!
!      n,n_atom :: INT
!
!      n_atom = g.dim2
!      do n = 1,n_atom
!         .put_position_to_unit_cell(g(:,n))
!      end
!
!   end

!   put_position_to_unit_cell(p)
!   ! Transform the position "p" in fractional coordinates into the
!   ! (1,1,1) unit cell
!      p :: VEC{REAL}(3)
!
!   ENSURE(size(p)==3,"position p must have size 3")
!
!      if (FALSE) self = self
!      p = mod(p+TWO,ONE)
!
!   end

!   get_partition_factors(pfac,geometry,full) :: leaky
!   ! Given a "geometry" array in fractional coordinates for a
!   ! molecule, generate the partition factors or repetition factors
!   ! "pfac" to be used for each atom in a structure factor
!   ! calculation.  If present, the "full" geometry in fractional
!   ! coordinates will be returned, where all possible symmetry
!   ! distinct atom positions in the unit cell have been generated.
!      geometry :: MAT{REAL}
!      pfac :: VEC{REAL}
!      full :: MAT{REAL}*, optional
!      pa,pb :: VEC{REAL}(3)
!      a,b,n,n_atom,n_same :: INT
!      same :: BIN
!      n_atom = size(geometry,2)
!
!   ENSURE(geometry.dim1==3,"Wrong shape for geometry array")
!   ENSURE(pfac.dim==n_atom,"Incompatible shape for p array")
!   ENSURE(.seitz.allocated,  "Seitz matrices not initialized")
!
!      do a = 1,n_atom
!         pa = geometry(:,a)
!         .put_position_to_unit_cell(pa)
!         n_same = 0
!         do n = 1,.n_seitz
!         do b = 1,n_atom
!            pb = geometry(:,b)
!            .transform_position(pb,n)
!            .put_position_to_unit_cell(pb)
!            same = pa.same_as(pb)
!            if (same) n_same = n_same + 1
!         end
!         end
!         pfac(a) = n_same
!      end
!
!      if (present(full)) .get_full_geometry(full,geometry,pfac)
!
!   end

!   get_full_geometry(full,geometry,pfac) :: leaky
!   ! Get the "full" set of distinct atom positions in the unit cell,
!   ! given a (possibly) partial "geometry", and a set of partition
!   ! factors "pfac".
!      full :: MAT{REAL}@
!      geometry :: MAT{REAL}
!      pfac :: VEC{REAL}
!
!   ENSURE(geometry.dim1==3,"Wrong shape for geometry array")
!   ENSURE(geometry.dim2==pfac.dim,"Incompatible shape for pfac array")
!   ENSURE(.seitz.allocated,  "Seitz matrices not initialized")
!
!      pa,pb :: VEC{REAL}(3)
!      a,b,n,n_atom,n_full :: INT
!      same :: BIN
!      rep :: REAL
!
!      n_atom = geometry.dim2
!
!      n_full = 0
!      do a = 1,n_atom
!         rep = .n_seitz/pfac(a)
!         DIE_IF(NOT rep.is_int,"rep factor for atom "//trim(a.to_str)//" not integral!")
!         n_full = n_full + int(.n_seitz/pfac(a))
!      end
!      full.create(3,n_full)
!
!      n_full = 0
!      do a = 1,n_atom
!      do n = 1,.n_seitz
!         pa = geometry(:,a)
!         .put_position_to_unit_cell(pa)
!         .transform_position(pa,n)
!         .put_position_to_unit_cell(pa)
!         same = FALSE
!         do b = 1,n_full
!            pb = full(:,b)
!            same = pa.same_as(pb)
!            if (same) exit
!         end
!         if (NOT same) then       ! New symmetry generated position
!            n_full = n_full+1
!            full(:,n_full) = pa
!         end
!      end
!      end
!
!   end

!   transform_geometry(g,op)
!   ! Transform the geometry "g" in fractional coordinates with the
!   ! Seitz operator with index "op"
!      g :: MAT{REAL}
!      op :: INT
!
!   ENSURE(.seitz.allocated,"Seitz matrices not initialized")
!   ENSURE(g.dim1==3,"incorrect size for array g")
!   ENSURE(op>0,         "operator index out of bounds")
!   ENSURE(op<=.n_seitz, "operator index out of bounds")
!
!      n,n_atom :: INT
!
!      n_atom = g.dim2
!      do n = 1,n_atom
!         .transform_position(g(:,n),op)
!      end
!
!   end

   PURE subroutine transform_position(self,pos,op)
   ! Transform the position "pos" in fractional coordinates with the
   ! Seitz operator with index "op"
      type(SPACEGROUP_TYPE), IN :: self
      VEC(REAL,3), INOUT :: pos
      INT, IN :: op
   ENSURE(allocated(self%seitz),"SPACEGROUP:transform_position ... Seitz matrices not initialized")
   ENSURE(op>0,"SPACEGROUP:transform_position ... operator index out of bounds")
   ENSURE(op<=self%n_seitz,"SPACEGROUP:transform_position ... operator index out of bounds")
      pos = matmul(self%seitz(1:3,1:3,op),pos)+self%seitz(1:3,4,op)
   end subroutine


!   is_same_geometry(geom_i,geom_j) result (res)
!   ! Return TRUE if the geometries "geom_i" and "geom_j" in fractional
!   ! coordinates are the same, to within *traslational* symmetry
!      geom_i,geom_j :: MAT{REAL}
!      res :: BIN
!      gi,gj :: MAT{REAL}@
!      i,j,n_atom :: INT
!      same :: BIN
!      skip :: VEC{BIN}@
!      n_atom = size(geom_i,2)
!
!   ENSURE(.seitz.allocated, "Seitz matrices not initialized")
!   ENSURE(geom_i.dim1==3, "incorrect size for array geom_i")
!   ENSURE(geom_j.dim1==3, "incorrect size for array geom_j")
!   ENSURE(n_atom==size(geom_j,2), "incompatible sizes for geom_i, geom_j")
!
!      gi.create(3,n_atom); gi = geom_i; .put_geometry_to_unit_cell(gi)
!      gj.create(3,n_atom); gj = geom_j; .put_geometry_to_unit_cell(gj)
!      skip.create(n_atom); skip(:) = FALSE
!      do i = 1,n_atom
!         do j = 1,n_atom
!            same = gi(:,i).same_as(gj(:,j))
!            if (NOT same OR skip(j)) cycle
!            skip(j) = TRUE
!            exit
!         end
!      end
!      res = all(skip) ! True if all atoms in i were matched (skipped) in j
!      skip.destroy
!      gj.destroy
!      gi.destroy
!   end

!   parent_inversion_symop_for(s) result (res) :: PURE
!   ! For a given symop with index "s", return the index of the
!   ! parent translation symop in the list of unique symops. If it does
!   ! not have a parent, return 0.
!      self :: IN
!      s :: INT, IN
!      res :: INT
!
!      n :: INT
!
!      res = 0
!
!      do n = 1,.n_seitz
!          if (s/=n) then
!          if (s==.inverted_symop(n)) then
!             res = n
!             exit
!          end
!          end
!      end
!
!   end

!   parent_translation_symop_for(s) result (res) :: PURE
!   ! For a given symop with index "s", return the index of the parent
!   ! translation symop in the list of unique symops. If it does not
!   ! have a parent, return 0.
!      self :: IN
!      s :: INT, IN
!      res :: INT
!
!      n :: INT
!
!      res = 0
!
!      do n = 1,.n_seitz
!          if (s/=n) then
!          if (s==.translated_symop(n)) then
!             res = n
!             exit
!          end
!          end
!      end
!
!   end

!   is_translated_symop(s) result (res) :: PURE
!   ! Return TRUE if "s" is the index of an translated symop
!      self :: IN
!      s :: INT, IN
!      res :: BIN
!      res = .parent_translation_symop_for(s) /= 0
!   end

!   is_inverted_symop(s) result (res) :: PURE
!   ! Return TRUE if "s" is the index of an inverted symop
!      self :: IN
!      s :: INT, IN
!      res :: BIN
!      res = NOT .is_translated_symop(s)
!   end

   PURE function is_inverted_unique_SF_symop(self,u) result (res)
   ! Return TRUE if "u" is the index of an inverted symop from
   ! the list of unique_SF_symops.
      type(SPACEGROUP_TYPE), IN :: self
      INT, IN :: u
      BIN :: res
      res = any(self%inverted_symop==self%unique_SF_symop(u))
   end function


!  ===========================================
!  Sum structure factors over unique SF symops
!  ===========================================

! To understand this code see Jayatilaka & Dittrich (2008)
! Acta Cryst. A 64, p383 section 2.5 comments (i) and (ii)
! NOTE: These were checked on KHMal with HBB 01/03/18

   PURE subroutine make_phase_for_symop(self,s,t_phase,refl)
   ! For each reflection in "refl", return in "t_phase" the sum of the
   ! *exponentiated* screw/glide phase shifts from each glide vector
   ! for all symops which are equivalent to the "u"-th unique symmetry
   ! operation, .unique_SF_symop(u), as determined by the "mask"
   ! array.
      type(SPACEGROUP_TYPE), IN :: self
      INT, IN :: s
      VEC(CPX,:), OUT :: t_phase
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      REAL :: pi2,t1,t2,t3,dot
      INT :: n,n_refl,h1,h2,h3
   ENSURE(self%analyzed,"SPACEGROUP:make_phase_for_symop ... spacegroup not analyzed")
   ENSURE(s<=self%n_seitz,"SPACEGROUP:make_phase_for_symop ... symop index out of range")
   ENSURE(size(t_phase)==size(refl),"SPACEGROUP:make_phase_for_symop ... wrong length for t_phase array")
      n_refl = size(refl)
      pi2 = TWO*PI
      t1 = pi2*self%seitz(1,4,s)
      t2 = pi2*self%seitz(2,4,s)
      t3 = pi2*self%seitz(3,4,s)
      do n = 1,n_refl
         h1 = refl(n)%h1
         h2 = refl(n)%h2
         h3 = refl(n)%h3
         dot = h1*t1+h2*t2+h3*t3

         ! Sum of exponentiated phase !
         t_phase(n) = exp(IMAGIFY(dot))
      end do
   end subroutine

   PURE subroutine make_phases_for_symop_0(self,u,t_phase,mask,refl)
   ! For each reflection in "refl", return in "t_phase" the sum of the
   ! *exponentiated* screw/glide phase shifts from each glide vector
   ! for all symops which are equivalent to the "u"-th unique symmetry
   ! operation, .unique_SF_symop(u), as determined by the "mask"
   ! array.
      type(SPACEGROUP_TYPE), IN :: self
      INT, IN :: u
      VEC(CPX,:), OUT :: t_phase
      VEC(INT,:), IN :: mask
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      REAL :: pi2,t1,t2,t3,dot
      INT :: s,n,n_refl,h1,h2,h3
   ENSURE(self%analyzed,"SPACEGROUP:make_phases_for_symop_0 ... spacegroup not analyzed")
   ENSURE(allocated(self%unique_SF_symop),"SPACEGROUP:make_phases_for_symop_0 ... no unique_SF_symop array!")
   ENSURE(u<=self%n_unique_SF_symops,"SPACEGROUP:make_phases_for_symop_0 ... symop index out of range")
   ENSURE(size(t_phase)==size(refl),"SPACEGROUP:make_phases_for_symop_0 ... wrong length for t_phase array")
   ENSURE(size(mask)==self%n_seitz,"SPACEGROUP:make_phases_for_symop_0 ... wrong size, mask")
      n_refl = size(refl)
      pi2 = TWO*PI
      t_phase = ZERO

      ! Symops
      do s = 1,self%n_seitz

         ! Perform phase sum over symops in mask(s) 
         ! which are the same as .unique_SF_symop(u)
         ! i.e. those which have the same 3x3 rotational part
         ! of the seitz matrix but different translation part
         if (mask(s)/=self%unique_SF_symop(u)) cycle
         t1 = pi2*self%seitz(1,4,s)
         t2 = pi2*self%seitz(2,4,s)
         t3 = pi2*self%seitz(3,4,s)
         do n = 1,n_refl
            h1 = refl(n)%h1
            h2 = refl(n)%h2
            h3 = refl(n)%h3
            dot = h1*t1+h2*t2+h3*t3

            ! Sum of exponentiated phase !
            t_phase(n) = t_phase(n)+exp(IMAGIFY(dot))
         end do
      end do
   end subroutine

   PURE subroutine make_phases_for_symop_1(self,s,t_phase,refl)
   ! For each reflection in "refl", return in "t_phase" the
   ! exponential of the screw/glide phase shifts from each glide
   ! vector for the "s"-th symmetry operation.
      type(SPACEGROUP_TYPE), IN :: self
      INT, IN :: s
      VEC(CPX,:), OUT :: t_phase
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      REAL :: pi2,t1,t2,t3,dot
      INT :: n,n_refl,h1,h2,h3
   ENSURE(self%analyzed,"SPACEGROUP:make_phases_for_symop_1 ... spacegroup not analyzed")
   ENSURE(s<=self%n_seitz,"SPACEGROUP:make_phases_for_symop_1 ... symop index out of range")
   ENSURE(size(t_phase)==size(refl),"SPACEGROUP:make_phases_for_symop_1 ... wrong length for t_phase array")
      n_refl = size(refl)
      pi2 = TWO*PI

      ! Glide/screw part
      t1 = pi2*self%seitz(1,4,s)
      t2 = pi2*self%seitz(2,4,s)
      t3 = pi2*self%seitz(3,4,s)
      do n = 1,n_refl
         h1 = refl(n)%h1
         h2 = refl(n)%h2
         h3 = refl(n)%h3
         dot = h1*t1+h2*t2+h3*t3

         ! Exponential of glide/screw part of symop "s"
         t_phase(n) = exp(IMAGIFY(dot))
      end do
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf(self,sf,unique_sf,refl)
   ! Form the structure factors "sf" from the sum of the list of
   ! unique-symop-generated structure factors, "unique_sf".
      type(SPACEGROUP_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf
      VEC(CPX,:), IN :: unique_sf
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      INT :: u,uf,ul,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf ... spacegroup not analyzed")
   ENSURE(size(sf)==size(refl),"SPACEGROUP:sum_unique_sf ... sf: wrong size")
   ENSURE(size(unique_sf)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf ... unique_sf: wrong size")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      sf = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf")
         uf = n_refl*(u-1)+1
         ul = n_refl*u

         ! Add up translated symop contribution
         call make_phases_for_symop_(self,u,t_phase,self%translated_symop,refl)
         sf = sf+t_phase*unique_sf(uf:ul)
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle

         ! Add up inverted symop contribution, if any
         ! Inversions conjugate the structure factor
         call make_phases_for_symop_(self,u,t_phase,self%inverted_symop,refl)
         sf = sf+t_phase*conjg(unique_sf(uf:ul))
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf")
      PARALLEL_SUM(sf)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine get_full_sd(self,sf,full_sf,refl)
   ! *GET* the *anomalous* structure factors into "sf" from the sum of
   ! the list of symop-generated structure factors, "full_sf".
   ! NOTE: this does not use translational or inversion symmetry.
      type(SPACEGROUP_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf
      VEC(CPX,:), IN :: full_sf
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      INT :: s,f,l,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:get_full_sd ... spacegroup not analyzed")
   ENSURE(size(sf)==size(refl),"SPACEGROUP:get_full_sd ... sf: wrong size")
   ENSURE(size(full_sf)==self%n_seitz*size(refl),"SPACEGROUP:get_full_sd ... full_sf: wrong size")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      sf = ZERO
      do s = PARALLEL_DO_START(1,1),self%n_seitz,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:get_full_sd")
         f = n_refl*(s-1)+1
         l = n_refl*s

         ! Add up translated symop contribution
         ! These are the same as in sum_unique_sf
         call make_phases_for_symop_(self,s,t_phase,refl)
         sf = sf+t_phase*full_sf(f:l)
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:get_full_sd")
      PARALLEL_SUM(sf)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine add_full_sd(self,sf,full_sf,refl)
   ! *Add* the *anomalous* structure factors into "sf" from the sum of
   ! the list of symop-generated structure factors, "full_sf".
   ! NOTE: this does not use translational or inversion symmetry.
      type(SPACEGROUP_TYPE), IN :: self
      VEC(CPX,:), INOUT :: sf
      VEC(CPX,:), IN :: full_sf
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      VEC(CPX,:), allocatable :: s_t
      INT :: s,f,l,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:add_full_sd ... spacegroup not analyzed")
   ENSURE(size(sf)==size(refl),"SPACEGROUP:add_full_sd ... sf: wrong size")
   ENSURE(size(full_sf)==self%n_seitz*size(refl),"SPACEGROUP:add_full_sd ... full_sf: wrong size")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      call create_(s_t,n_refl)
      s_t = ZERO
      do s = PARALLEL_DO_START(1,1),self%n_seitz,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:add_full_sd")
         f = n_refl*(s-1)+1
         l = n_refl*s

         ! Add up translated symop contribution
         ! These are the same as in sum_unique_sf
         call make_phases_for_symop_(self,s,t_phase,refl)
         s_t = s_t+t_phase*full_sf(f:l)
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:add_full_sd")
      PARALLEL_SUM(s_t)
      sf = sf+s_t
      call destroy_(s_t)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf_ints(self,sf_ints,unique_sf_ints,refl)
   ! Form the structure factor integrals "sf_ints" from a sum of the list
   ! of unique-symop-generated structure factors integrals "unique_sf_ints".
      type(SPACEGROUP_TYPE), IN :: self
      MAT3(CPX,:,:,:), OUT :: sf_ints
      MAT3(CPX,:,:,:), IN :: unique_sf_ints
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      INT :: u,uf,n,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf_ints ... spacegroup not analyzed")
   ENSURE(size(sf_ints,1)==size(refl),"SPACEGROUP:sum_unique_sf_ints ... sf_ints: wrong size")
   ENSURE(size(unique_sf_ints,1)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf_ints ... unique_sf_ints: wrong size")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      sf_ints = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_ints")
         uf = n_refl*(u-1)

         ! Add up translated symop contribution
         call make_phases_for_symop_(self,u,t_phase,self%translated_symop,refl)
         do n = 1,n_refl
            sf_ints(n,:,:) = sf_ints(n,:,:)+t_phase(n)*unique_sf_ints(uf+n,:,:)
         end do

         ! Add up inverted symop contribution, if any
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle
         call make_phases_for_symop_(self,u,t_phase,self%inverted_symop,refl)
         do n = 1,n_refl
            sf_ints(n,:,:) = sf_ints(n,:,:)+t_phase(n)*conjg(unique_sf_ints(uf+n,:,:))
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_ints")
      PARALLEL_SUM(sf_ints)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf_derivs_0(self,sf_d,unique_sf_d,refl)
   ! Form the structure factor derivatives "sf_d" from a sum of the
   ! list of unique-symop-generated structure factor derivatives
   ! "unique_sf_d".
      type(SPACEGROUP_TYPE), IN :: self
      MAT(CPX,:,:), OUT :: sf_d
      MAT(CPX,:,:), IN :: unique_sf_d
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      INT :: u,uf,n,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf_derivs_0 ... spacegroup not analyzed")
   ENSURE(size(sf_d,1)==size(refl),"SPACEGROUP:sum_unique_sf_derivs_0 ... sf_d: wrong dim1")
   ENSURE(size(unique_sf_d,1)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf_derivs_0 ... unique_sf_d: wrong dim1")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      sf_d = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_derivs_0")
         uf = n_refl*(u-1)

         ! Add up translated symop contribution
         call make_phases_for_symop_(self,u,t_phase,self%translated_symop,refl)
         do n = 1,n_refl
            sf_d(n,:) = sf_d(n,:)+t_phase(n)*unique_sf_d(uf+n,:)
         end do

         ! Add up inverted symop contribution, if any
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle
         call make_phases_for_symop_(self,u,t_phase,self%inverted_symop,refl)
         do n = 1,n_refl
            sf_d(n,:) = sf_d(n,:)+t_phase(n)*conjg(unique_sf_d(uf+n,:))
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_derivs_0")
      PARALLEL_SUM(sf_d)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf_derivs_1(self,sf_d,unique_sf_d,refl)
   ! Form the structure factor derivatives "sf_d" from a sum of the
   ! list of unique-symop-generated structure factor derivatives
   ! "unique_sf_d".
      type(SPACEGROUP_TYPE), IN :: self
      VEC(CPX,:), OUT :: sf_d
      VEC(CPX,:), IN :: unique_sf_d
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      INT :: u,uf,n,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf_derivs_1 ... spacegroup not analyzed")
   ENSURE(size(sf_d,1)==size(refl),"SPACEGROUP:sum_unique_sf_derivs_1 ... sf_d: wrong dim1")
   ENSURE(size(unique_sf_d,1)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf_derivs_1 ... unique_sf_d: wrong dim1")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      sf_d = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_derivs_1")
         uf = n_refl*(u-1)

         ! Add up translated symop contribution
         call make_phases_for_symop_(self,u,t_phase,self%translated_symop,refl)
         do n = 1,n_refl
            sf_d(n) = sf_d(n)+t_phase(n)*unique_sf_d(uf+n)
         end do

         ! Add up inverted symop contribution, if any
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle
         call make_phases_for_symop_(self,u,t_phase,self%inverted_symop,refl)
         do n = 1,n_refl
            sf_d(n) = sf_d(n)+t_phase(n)*conjg(unique_sf_d(uf+n))
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_derivs_1")
      PARALLEL_SUM(sf_d)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf_derivs_atom(self,sf_d,unique_sf_d,refl)
   ! Form the structure factor derivatives "sf_d" from a sum of the
   ! list of unique-symop-generated structure factor derivatives
   ! "unique_sf_d".
      type(SPACEGROUP_TYPE), IN :: self
      MAT(CPX,:,:), OUT :: sf_d
      MAT(CPX,:,:), IN :: unique_sf_d
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      INT :: u,uf,n,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf_derivs_atom ... spacegroup not analyzed")
   ENSURE(size(sf_d,1)==size(refl),"SPACEGROUP:sum_unique_sf_derivs_atom ... sf_d: wrong dim1")
   ENSURE(size(unique_sf_d,1)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf_derivs_atom ... unique_sf_d: wrong dim1")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      sf_d = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_derivs_atom")
         uf = n_refl*(u-1)

         ! Add up translated symop contribution
         call make_phases_for_symop_(self,u,t_phase,self%translated_symop,refl)
         do n = 1,n_refl
            sf_d(n,:) = sf_d(n,:)+t_phase(n)*unique_sf_d(uf+n,:)
         end do

         ! Add up inverted symop contribution, if any
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle
         call make_phases_for_symop_(self,u,t_phase,self%inverted_symop,refl)
         do n = 1,n_refl
            sf_d(n,:) = sf_d(n,:)+t_phase(n)*conjg(unique_sf_d(uf+n,:))
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_derivs_atom")
      PARALLEL_SUM(sf_d)
      call destroy_(t_phase)
   end subroutine

   NON_MPI_PURE subroutine add_full_sf_derivs_d(self,sf_d,full_sf_d,refl)
   ! *Add* the *anomalous* structure factor *derivatives* into "sf_d"
   ! from a sum of symop-generated structure factor derivatives
   ! "full_sf_d". NOTE: do not use translational or inversion symmetry.
      type(SPACEGROUP_TYPE), IN :: self
      MAT(CPX,:,:), INOUT :: sf_d
      MAT(CPX,:,:), IN :: full_sf_d
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: t_phase
      MAT(CPX,:,:), allocatable :: sf_t
      INT :: s,f,n,n_refl
   ENSURE(self%analyzed,"SPACEGROUP:add_full_sf_derivs_d ... spacegroup not analyzed")
   ENSURE(size(sf_d,1)==size(refl),"SPACEGROUP:add_full_sf_derivs_d ... sf_d: wrong dim1")
   ENSURE(size(full_sf_d,1)==self%n_seitz*size(refl),"SPACEGROUP:add_full_sf_derivs_d ... unique_sf_d: wrong dim1")
      n_refl = size(refl)
      call create_(t_phase,n_refl)
      call create_(sf_t,size(sf_d,1),size(sf_d,2))
      sf_t = ZERO
      do s = PARALLEL_DO_START(1,1),self%n_seitz,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:add_full_sf_derivs_d")
         f = n_refl*(s-1)

         ! Add up translated symop contribution
         call make_phases_for_symop_(self,s,t_phase,refl)
         do n = 1,n_refl
            sf_t(n,:) = sf_t(n,:)+t_phase(n)*full_sf_d(f+n,:)
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:add_full_sf_derivs_d")
      PARALLEL_SUM(sf_t)
      sf_d = sf_d+sf_t
      call destroy_(sf_t)
      call destroy_(t_phase)
   end subroutine

   PURE subroutine make_unique_sf_coefficients(self,coef,tf,Fr,Fc,refl)
   ! Return "coef", one complex number for each symmetry-generated k
   ! point, such that the "X" of sum_unique_sf_pts at a point r is
   !    X(r) = Re sum_j coef(j) exp(i k_j.r)
   ! that is, the symmetry phase times the temperature factor "tf" times
   ! Fr - i Fc, with the conjugate added for an inverted unique symop.
   ! Used where the sum over k is done some other way than point by
   ! point, as for the fitted Hirshfeld atoms.
      type(SPACEGROUP_TYPE), IN :: self
      VEC(CPX,:), OUT :: coef
      VEC(CPX,:), IN :: tf
      VEC(REAL,:), IN :: Fr,Fc
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: phase
      CPX :: c
      INT :: n_refl,k,u,uf
   ENSURE(self%analyzed,"SPACEGROUP:make_unique_sf_coefficients ... spacegroup not analyzed")
   ENSURE(size(coef)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:make_unique_sf_coefficients ... coef: wrong size")
   ENSURE(size(tf)==size(coef),"SPACEGROUP:make_unique_sf_coefficients ... tf: wrong size")
   ENSURE(size(Fr)==size(refl),"SPACEGROUP:make_unique_sf_coefficients ... Fr: wrong size")
   ENSURE(size(Fc)==size(refl),"SPACEGROUP:make_unique_sf_coefficients ... Fc: wrong size")
      n_refl = size(refl)
      call create_(phase,n_refl)
      do u = 1,self%n_unique_SF_symops
         uf = n_refl*(u-1)
         call make_phases_for_symop_(self,u,phase,self%translated_symop,refl)
         do k = 1,n_refl
            c = phase(k)*tf(uf+k)*COMPLEXIFY(Fr(k),-Fc(k))
            if (is_inverted_unique_SF_symop_(self,u)) c = c+conjg(c)
            coef(uf+k) = c
         end do
      end do
      call destroy_(phase)
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf_pts_0(self,X,k_pts,pts,tf,Fr,Fc,refl)
   ! Form "X", the sum of the FT phase factor e^(i k.r) times the
   ! symmetry phases (which arise from copying the molecule
   ! around the unit cell) for a series of "k_pts" and space "pts",
   ! multiplied by a given k-space factor "(Fr,Fc)". The result "X" is
   ! of length of the number of space points
   ! NOTE: This version has a *real* temperature factor "tf".
      type(SPACEGROUP_TYPE), IN :: self
      VEC(REAL,:), OUT :: X
      MAT(REAL,:,:), IN :: k_pts,pts
      VEC(REAL,:), target, IN :: tf
      VEC(REAL,:), IN :: Fr,Fc
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: phase
      VEC(REAL,:), allocatable :: T,k1,k2,k3,r1,r2,r3
      CPX :: I,ph,val
      REAL :: k1k,k2k,k3k,kr,facr,facc,valr,valc
      INT :: n_refl,n_p,k,p,u,uf,ul
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf_pts_0 ... spacegroup not analyzed")
   ENSURE(size(X)==size(pts,1),"SPACEGROUP:sum_unique_sf_pts_0 ... X: wrong size")
   ENSURE(size(k_pts,1)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf_pts_0 ... k_pts: wrong dim1")
   ENSURE(size(k_pts,2)==3,"SPACEGROUP:sum_unique_sf_pts_0 ... k_pts: wrong dim2")
   ENSURE(size(Fr)==size(refl),"SPACEGROUP:sum_unique_sf_pts_0 ... Fr: wrong size")
   ENSURE(size(Fc)==size(refl),"SPACEGROUP:sum_unique_sf_pts_0 ... Fc: wrong size")
      I = IMAGIFY(ONE)
      n_refl = size(refl)
      n_p = size(pts,1)
      call create_(r1,n_p)
      call create_(r2,n_p)
      call create_(r3,n_p)
      call create_(phase,n_refl)
      call create_(k1,n_refl)
      call create_(k2,n_refl)
      call create_(k3,n_refl)
      call create_(T,n_refl)
      r1 = pts(:,1)
      r2 = pts(:,2)
      r3 = pts(:,3)
      X = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_pts_0")
         uf = n_refl*(u-1)+1
         ul = n_refl*u
         k1 = k_pts(uf:ul,1)
         k2 = k_pts(uf:ul,2)
         k3 = k_pts(uf:ul,3)
         T = tf(uf:ul)

         ! Make symmetry phases
         call make_phases_for_symop_(self,u,phase,self%translated_symop,refl)

         ! Loop over the points
         do k = 1,n_refl
            k1k = k1(k)
            k2k = k2(k)
            k3k = k3(k)
            ph = phase(k)*T(k)
            facr = Fr(k)
            facc = Fc(k)
            do p = 1,n_p
               kr = k1k*r1(p)+k2k*r2(p)+k3k*r3(p)
               val = exp(I*kr)*ph
               valr = real(val)
               valc = aimag(val)
               X(p) = X(p)+valr*facr+valc*facc
            end do
         end do

         ! Add up inverted symop contribution, if any
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle

         ! Loop over the points
         do k = 1,n_refl
            k1k = k1(k)
            k2k = k2(k)
            k3k = k3(k)
            ph = phase(k)*T(k)
            facr = Fr(k)
            facc = Fc(k)
            do p = 1,n_p
               kr = k1k*r1(p)+k2k*r2(p)+k3k*r3(p)
               val = exp(-I*kr)*ph
               valr = real(val)
               valc = aimag(val)
               X(p) = X(p)+valr*facr+valc*facc
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_pts_0")
      PARALLEL_SUM(X)
      call destroy_(T)
      call destroy_(k3)
      call destroy_(k2)
      call destroy_(k1)
      call destroy_(phase)
      call destroy_(r3)
      call destroy_(r2)
      call destroy_(r1)
   end subroutine

   NON_MPI_PURE subroutine sum_unique_sf_pts_1(self,X,k_pts,pts,tf,Fr,Fc,refl)
   ! Form "X", the sum of the FT phase factor e^(i k.r) times the
   ! symmetry phases (which arise from copying the molecule
   ! around the unit cell) for a series of "k_pts" and space "pts",
   ! multiplied by a given k-space factor "(Fr,Fc)". The result "X" is
   ! of length of the number of space points
   ! NOTE: This version has a complex temperature factor "tf", which
   ! can be the case when anharmonity is present.
      type(SPACEGROUP_TYPE), IN :: self
      VEC(REAL,:), OUT :: X
      MAT(REAL,:,:), IN :: k_pts,pts
      VEC(CPX,:), IN :: tf
      VEC(REAL,:), IN :: Fr,Fc
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      VEC(CPX,:), allocatable :: T,phase
      VEC(REAL,:), allocatable :: k1,k2,k3,r1,r2,r3
      CPX :: I,ph,val
      REAL :: k1k,k2k,k3k,kr,facr,facc,valr,valc
      INT :: n_refl,n_p,k,p,u,uf,ul
   ENSURE(self%analyzed,"SPACEGROUP:sum_unique_sf_pts_1 ... spacegroup not analyzed")
   ENSURE(size(X)==size(pts,1),"SPACEGROUP:sum_unique_sf_pts_1 ... X: wrong size")
   ENSURE(size(k_pts,1)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:sum_unique_sf_pts_1 ... k_pts: wrong dim1")
   ENSURE(size(k_pts,2)==3,"SPACEGROUP:sum_unique_sf_pts_1 ... k_pts: wrong dim2")
   ENSURE(size(Fr)==size(refl),"SPACEGROUP:sum_unique_sf_pts_1 ... Fr: wrong size")
   ENSURE(size(Fc)==size(refl),"SPACEGROUP:sum_unique_sf_pts_1 ... Fc: wrong size")
      I = IMAGIFY(ONE)
      n_refl = size(refl)
      n_p = size(pts,1)
      call create_(r1,n_p)
      call create_(r2,n_p)
      call create_(r3,n_p)
      call create_(phase,n_refl)
      call create_(k1,n_refl)
      call create_(k2,n_refl)
      call create_(k3,n_refl)
      call create_(T,n_refl)
      r1 = pts(:,1)
      r2 = pts(:,2)
      r3 = pts(:,3)
      X = ZERO
      do u = PARALLEL_DO_START(1,1),self%n_unique_SF_symops,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_pts_1")
         uf = n_refl*(u-1)+1
         ul = n_refl*u
         k1 = k_pts(uf:ul,1)
         k2 = k_pts(uf:ul,2)
         k3 = k_pts(uf:ul,3)
         T = tf(uf:ul)

         ! Make symmetry phases
         call make_phases_for_symop_(self,u,phase,self%translated_symop,refl)

         ! Loop over the points
         do k = 1,n_refl
            k1k = k1(k)
            k2k = k2(k)
            k3k = k3(k)
            ph = phase(k)*T(k)
            facr = Fr(k)
            facc = Fc(k)
            do p = 1,n_p
               kr = k1k*r1(p)+k2k*r2(p)+k3k*r3(p)
               val = exp(I*kr)*ph
               valr = real(val)
               valc = aimag(val)
               X(p) = X(p)+valr*facr+valc*facc
            end do
         end do

         ! Add up inverted symop contribution, if any
         if (NOT is_inverted_unique_SF_symop_(self,u)) cycle

         ! Loop over the points
         do k = 1,n_refl
            k1k = k1(k)
            k2k = k2(k)
            k3k = k3(k)
            ph = phase(k)*T(k)
            facr = Fr(k)
            facc = Fc(k)
            do p = 1,n_p
               kr = k1k*r1(p)+k2k*r2(p)+k3k*r3(p)
               val = exp(-I*kr)*ph
               valr = real(val)
               valc = aimag(val)
               X(p) = X(p)+valr*facr+valc*facc
            end do
         end do
      end do
      UNLOCK_PARALLEL_DO("SPACEGROUP:sum_unique_sf_pts_1")
      PARALLEL_SUM(X)
      call destroy_(T)
      call destroy_(k3)
      call destroy_(k2)
      call destroy_(k1)
      call destroy_(phase)
      call destroy_(r3)
      call destroy_(r2)
      call destroy_(r1)
   end subroutine

   subroutine symmetrize_unique_SFs(self,sf,stabilizer,refl,diff,diffmax,diffmax_hkl)
   ! Symmetrize an atom's form factors "sf" over its site "stabilizer"
   ! symops, so they obey the local symmetry a poor wavefunction may break.
   ! "sf" holds one block of n_refl values per unique SF symop. Each block is
   ! replaced by the average of the blocks its left coset maps it to, with a
   ! phase factor where the mapping symop is not itself a unique one, and a
   ! block whose symop is a stabilizer is copied straight over. (Kang, 2025.)
      type(SPACEGROUP_TYPE), IN :: self
      VEC(CPX,:), INOUT :: sf
      VEC(INT,:), IN :: stabilizer
      VEC(type(REFLECTION_TYPE),:), IN :: refl
      REAL, optional, OUT :: diff
      REAL, optional, OUT :: diffmax
      VEC(INT,3), optional, OUT :: diffmax_hkl
      VEC(REAL,:), allocatable :: absdiff
      VEC(CPX,:), allocatable :: average,t_phase,T_Phase_0
!      af, res_sf :: VEC{CPX}@
      VEC(CPX,:), allocatable :: tmp_ff
      INT :: n_refl,n_stab
      INT :: u,us,n,s,s_parent,v
      INT :: f,l,f_v,l_v
      BIN :: Db  ! For debugging purpose. if TRUE, show the stabilizers and coset
   ENSURE(self%analyzed,"SPACEGROUP:symmetrize_unique_SFs ... spacegroup not analyzed")
   ENSURE(size(sf)==self%n_unique_SF_symops*size(refl),"SPACEGROUP:symmetrize_unique_SFs ... unique_sf_ints: wrong size")
      Db = FALSE  ! Set Db=TRUE to see all the chains of indices comprising the stabilizer subgroup and its cosets.
      n_refl = size(refl)
      n_stab = size(stabilizer)

      ! Anything to do?

      ! Initialize whichever residuals were asked for. Each is tested
      ! separately: they are independently optional, and the branch version
      ! of this code assigned all three under "present(diff)" alone.
      if (present(diff)) diff = ZERO
      if (present(diffmax)) diffmax = ZERO
      if (present(diffmax_hkl)) diffmax_hkl = 0
      if (n_stab<=1) return

   ! Phases
      call create_(t_phase,n_refl)
      call create_(T_Phase_0,n_refl)  ! T_Phase_0 is the phase factor of the symmetry operator of interest
      call create_(average,n_refl)
      call create_(absdiff,n_refl)
!      af.create(n_refl)

!      res_sf.create(sf.dim) 
      call create_(tmp_ff,size(sf))  ! at the end sf :: INOUT will be overwritten by this
      tmp_ff = ZERO  ! initialize tmp_ff

      ! Every block (indexed by u) is symmetized by STABILIZERS or left COSETs 
      if (Db) call text_(stdout,"block_u ; UNIQ_SYMOP_us ; STAB./COSET_s ; S_PARENT ; block_v")
      if (Db) call text_(stdout,"lower case: among unique symops. UPPER CASE: AMONG ALL SYMOPs ")
      do u = 1,self%n_unique_SF_symops  ! u-th block in ff_u. blockwise do-loop ( no parallel for the moment ) 
         if (Db) call flush_(stdout)  !! for [u, us, s, o, v] printing's better visual
         us = self%unique_SF_symop(u)  !  unique symmetry operator's sym.op. index, ==> symop Seitz(:,:,us) 
         f = n_refl*(u-1)+1  ! u-th block in sf(f:l) (or refer to ff(f:l,a) )
         l = n_refl*u
         average = ZERO  ! Initialize the n_refl vector to save the symmetrized form factor of an atom
                        !  created by the symmetry operator Seitz(:,:,us).
                        !  for us = 1 or us = stabilizer(n), it is the original asymmetric atom.  
         do n = 1,n_stab  ! run over every element in the Stabilizer subgroup or the left coset.
            s = self%product_of_symops(us,stabilizer(n))  ! Kang's note below.

            ! Kang's notes: 
            !      case us = 1 (identity)      -- s = stabilizer subgroup elements's indices
            !      case us = one of stabilizers-- s = same as above (but different in the order) 
            !     [Note: There is a room for improving efficiency by skipping the loop of 
            !     "unique operator AND stabilizer of another uniq.symop", using calculated values from the previous unique symop's symmetrization.]
            !      otherwise                   -- s = corresponding left coset element's indices [Note: similarly, there is a room for improving efficiency]
            !
            ! Now, find the corresponding index of the unique symop "o" for "s" symmetry operator.
            !   ( Which unique symmetry operator is responsible for the "s"-th symop? 
            !   ( The answer is "o"-th symmetry operator )
            s_parent = self%inverted_symop(s)  ! If s-th symop is NOT inverted_symop, then o=0 at this stage.
            if (s_parent==0) s_parent = self%translated_symop(s)  ! if symop "s" has no inversion parent, find its translation parent.

            ! Kang's note: 
            !     "s_parent" is index from [all symmetry operators].
            !     now find the [block index v] among the uniq.sym.op (.unique_SF_symop)
            !     
            do v = 1,self%n_unique_SF_symops  ! Look up unique SYMOP "s_parent" . find "v"-th blocks
               if (s_parent==self%unique_SF_symop(v)) exit
            end do
            ! Kang's note: Try to replace above do-loop with "index_for_column" function 
            ! in the next update
            !  v = .n_unique_SF_symops.index_fol_column( s_parent )    
            if (Db) then
               call flush_(stdout)  !! Kang debugging
               call put_(stdout,u)  ! unique symop running index = block index                                 (unq.symop index "u" runs from  1 to .n_unique_SF_symops)
               call put_(stdout,us)  ! unique symop's index among the full set of symops                        (ALL symop index "us"      /    
               call put_(stdout,s)  ! index of a stabilizer(or coset element) among the full set of symops     (ALL symop index "s"       /> is one of {1,2,3, ... ,n_seitz} )
               call put_(stdout,s_parent)  ! index of corresponding unique symop(parent) among the full set of symops (ALL symop index "s_parent"/                                 
               call put_(stdout,v)  ! unique symop index of the o-th symop among the unique symops             (unq.symop index "v" is one of {1,...,.n_unique_SF_symops})
            end if
            f_v = n_refl*(v-1)+1  ! first/last index of o-th block in sf
            l_v = n_refl*v
            call make_phase_for_symop_(self,s,t_phase,refl)
            if (self%inverted_symop(s)/=0) then  ! if there is inversion parent
               average = average+conjg(sf(f_v:l_v))*t_phase
            else  ! else there is translation parent only
               average = average+sf(f_v:l_v)*t_phase
            end if
         end do  ! of "n = 1,n_stabilizer"-loop
         call make_phase_for_symop_(self,us,T_Phase_0,refl)  !T_Phase_0 is the phase factor corresponds to seitz(1:3,4,us) 
         tmp_ff(f:l) = average/n_stab*conjg(T_Phase_0)  ! undo the original symop's phase part
      end do  !of "u = 1,.n_unique SF symops"-loop

      !! PARALLEL_SUM(tmp_ff) ! One can make it be compatible with parallel computing?

      ! Kang's note. At this stage, 
      !               "sf" is as an input
      !           "tmp_ff" is symmetrized (averaged)
      ! At the end---after the calculation of "diff"---"sf" will be overwritten by "tmp_ff"

! If any residual is wanted, do the following section. else skip.
      if (present(diff) OR present(diffmax) OR present(diffmax_hkl)) then  ! Kang added 2024.05.29
         absdiff = abs(tmp_ff(1:n_refl)-sf(1:n_refl))  ! First block only = FF of original unique atom in the asym. unit
         ! The residual SUM OF SQUARES, which is what the caller stores in
         ! .asymmetric_FF_symmetrization_rss and what that component's
         ! description says it holds. This used to return sqrt(sum(...)),
         ! while CRYSTAL:put_asymmetric_FF_symmetrization_rss printed
         ! sqrt(rss/n_refl) -- so the reported "RMS asymmetry" was rooted
         ! twice, S^(1/4)/sqrt(n) rather than sqrt(S/n), overstating it by
         ! seven orders of magnitude for urea. The Max column added below
         ! is what exposed it: a maximum cannot be smaller than the RMS of
         ! the same set, and it was, by a factor of a million.
         if (present(diff)) diff = sum(absdiff**2)
         ! The largest single asymmetry, and the reflection that produced it.
         ! Kang 2025.04.25: an RMS says how asymmetric the form factors are on
         ! average, but not which reflection is worst, which is what tells you
         ! whether the cause is one bad k-point or a systematic effect.
         if (present(diffmax) OR present(diffmax_hkl)) then
            n = index_of_largest_value_(absdiff)
            if (present(diffmax)) diffmax = absdiff(n)
            if (present(diffmax_hkl)) diffmax_hkl = [refl(n)%h1,refl(n)%h2,refl(n)%h3]
         end if
      end if

      ! Update sf
      sf = tmp_ff  ! sf is now ready for roll out.

      ! Clean
      call destroy_(absdiff)
      call destroy_(average)
      call destroy_(tmp_ff)

       ! To see the old Form factor symmetrization, check out "2b3dd9c"
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump for debugging or reading back in
      type(SPACEGROUP_TYPE), IN :: self
      call dump_(stdout,"IT_symbol",self%IT_symbol)
      call dump_(stdout,"IT_group_number",self%IT_group_number)
      call dump_(stdout,"Hall_symbol",self%Hall_symbol)
      call dump_(stdout,"HM_symbol",self%HM_symbol)
      call dump_(stdout,"Schoenflies_symbol",self%Schoenflies_symbol)
      call dump_(stdout,"lattice_symbol",self%lattice_symbol)
      call dump_(stdout,"lattice_symbol_index",self%lattice_symbol_index)
      call dump_(stdout,"lattice_type",self%lattice_type)
      call dump_(stdout,"centrosymmetric",self%centrosymmetric)
      call dump_(stdout,"axis_order",self%axis_order)
      call dump_(stdout,"axis_direction_symbol",self%axis_direction_symbol)
      call dump_(stdout,"axis_direction_index",self%axis_direction_index)
      call dump_(stdout,"has_axis_bar",self%has_axis_bar)
      call dump_(stdout,"translation_symbol",self%translation_symbol)
      call dump_(stdout,"origin_shift",self%origin_shift)
      call dump_(stdout,"nL",self%nL)
      call dump_(stdout,"nR",self%nR)
      call dump_(stdout,"nG",self%nG)
      call dump_(stdout,"n_seitz",self%n_seitz)
      call dump_(stdout,"seitz",self%seitz)
      call dmpa_(stdout,"product_of_symops",self%product_of_symops)
      call dmpa_(stdout,"inverse_of_symops",self%inverse_of_symops)
      call dump_(stdout,"n_inverted_symops",self%n_inverted_symops)
      call dmpa_(stdout,"inverted_symop",self%inverted_symop)
      call dmpa_(stdout,"translated_symop",self%translated_symop)
      call dump_(stdout,"n_unique_SF_symops",self%n_unique_SF_symops)
      call dmpa_(stdout,"unique_SF_symop",self%unique_SF_symop)
      call dump_(stdout,"analyzed",self%analyzed)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(SPACEGROUP_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":SPACEGROUP@ = { ")
      else; call text_(stdout,trim(object_name)//":SPACEGROUP = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(SPACEGROUP_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":SPACEGROUP@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put out the spacegroup information
      type(SPACEGROUP_TYPE), IN :: self
   ENSURE(self%analyzed,"SPACEGROUP:put ... not yet analyzed")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"======================")
      call text_(stdout,"Spacegroup information")
      call text_(stdout,"======================")
      call flush_(stdout)
      call put_spacegroup_name_info_(self)
      call put_seitz_as_Jones_Faithful_(self)
      call put_seitz_as_4x4_matrices_(self)
      call put_group_multiplication_table_(self)
      call put_symop_inverse_table_(self)
      call put_inv_trans_symop_data_(self)
   end subroutine

   subroutine put_spacegroup_name_info(self)
   ! Put out the spacegroup name information (not the seitz matrices)
      type(SPACEGROUP_TYPE), IN :: self
      STR(len=STR_SIZE) :: subscripts,HM_symbol,Hall_symbol
      MAT(STR(len=1),3,4) :: sub
   ENSURE(self%analyzed,"SPACEGROUP:put_spacegroup_name_info ... spacegroup not analyzed")
      HM_symbol = self%HM_symbol
      call to_upper_case_(HM_symbol(1:1))
      Hall_symbol = self%Hall_symbol
      call to_upper_case_(Hall_symbol(1:1))
      call show_(stdout,"International Table no. =",self%IT_group_number)
      call show_(stdout,"IT symbol               =",trim(self%IT_symbol))
      call show_(stdout,"Hermann-Mauguin symbol  =",trim(HM_symbol))
      call show_(stdout,"Hall symbol             =",trim(Hall_symbol))
      call show_(stdout,"Schoenflies symbol      =",trim(self%Schoenflies_symbol))
      call show_(stdout,"Lattice symbol          =",trim(self%lattice_symbol))
      call show_(stdout,"Lattice type            =",trim(self%lattice_type))
      call flush_(stdout)
      call show_(stdout,"Spacegroup order        =",self%n_seitz)
      call show_(stdout,"Centro-symmetric?       =",self%centrosymmetric)
      sub = self%translation_symbol
      subscripts = sub(1,1)//sub(2,1)//sub(3,1)//" "//sub(1,2)//sub(2,2)//sub(3,2)//" "//sub(1,3)//sub(2,3)//sub(3,3)
      call show_(stdout,"Axis orders             =",self%axis_order,width=4)
      call show_(stdout,"Axis direction symbols  =",self%axis_direction_symbol,width=4)
      call show_(stdout,"Axis direction indices  =",self%axis_direction_index,width=4)
      call show_(stdout,"Translation subscripts  =",subscripts,width=4)
      call show_(stdout,"No of T generators      =",self%nL)
      call show_(stdout,"No of R generators      =",self%nR)
      call show_(stdout,"No of Generators        =",self%nG)
      call show_(stdout,"Origin shift in 12-ths  =",self%origin_shift,width=4)
   end subroutine

   subroutine put_seitz_as_Jones_Faithful(self)
   ! Put out the seitz matrices in Jones-Faithful notation.
      type(SPACEGROUP_TYPE), IN :: self
      INT :: n
      VEC(STR(len=STR_SIZE),:), allocatable :: jf
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Symmetry operations")
      call text_(stdout,"===================")
      call flush_(stdout)

      ! Data
      call create_(jf,size(self%seitz,3))
      do n = 1,size(self%seitz,3)
         call recode_Jones_Faithful_symbol_(jf(n),self%seitz(:,:,n))
      end do

      ! Table
      call create_(table,1)
      call set_column_spacing_(table(1),4)
      call set_heading_(table(1),"Symop")
      call set_center_(table(1),TRUE)
      call set_values_(table(1),jf)
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(jf)
   end subroutine

   subroutine put_seitz_as_4x4_matrices(self)
   ! Put out the seitz matrices in 4x4 matrix notation.
      type(SPACEGROUP_TYPE), IN :: self
      call save_style_(stdout)
      call set_real_precision_(stdout,3)
      call set_real_width_(stdout,7)
      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=====================================")
      call text_(stdout,"Seitz matrices in crystal axis system")
      call text_(stdout,"=====================================")
      call flush_(stdout)
      if (size(self%seitz,3)>48) then
         call text_(stdout,"Too many matrices: look at the Jones-Faithful table")
      else
         call put_(stdout,self%seitz,auto_width=TRUE)
      end if
      call unsave_style_(stdout)
   end subroutine

   subroutine put_group_multiplication_table(self)
   ! Put the group multiplcation table
      type(SPACEGROUP_TYPE), IN :: self
      INT :: n,g
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===============================")
      call text_(stdout,"Spacegroup multiplication table")
      call text_(stdout,"===============================")
      call flush_(stdout)
      call text_(stdout,". The element in the table is product(i,j) where:")
      call flush_(stdout)
      call text_(stdout,"     product(i,j) = symop(i) x symop(j)")
      call flush_(stdout)
      call text_(stdout,"  where i and j are row and column indices, respectively")
      call text_(stdout,"  and the product is modulo an integer shift vector.")
      call flush_(stdout)
      n = size(self%product_of_symops,1)
      if (n>48) then
         call text_(stdout,"Too many elements to show the table!")
         return
      end if

      ! Table
      call create_(table,n)
      do g = 1,n
         call set_heading_(table(g),trim(to_str_(g)))
         call set_values_(table(g),self%product_of_symops(:,g))
      end do
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_symop_inverse_table(self)
   ! Put the inverse symops
      type(SPACEGROUP_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"==============================")
      call text_(stdout,"Spacegroup symop inverse table")
      call text_(stdout,"==============================")

      ! Table
      call create_(table,1)
      call set_heading_(table(1),"Inverse")
      call set_subhead_(table(1),"symop")
      call set_values_(table(1),self%inverse_of_symops(:))
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_inv_trans_symop_data(self)
   ! Put out the inverted translated symop data
      type(SPACEGROUP_TYPE), IN :: self
      INT :: n
      VEC(STR(len=STR_SIZE),:), allocatable :: unique
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
   ENSURE(allocated(self%inverted_symop),"SPACEGROUP:put_inv_trans_symop_data ... no inverted_symop")
   ENSURE(allocated(self%translated_symop),"SPACEGROUP:put_inv_trans_symop_data ... no translated_symop")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"====================================")
      call text_(stdout,"Inversion-translation-related symops")
      call text_(stdout,"====================================")
      call flush_(stdout)
      call text_(stdout,". The information in this table is used to speed up")
      call text_(stdout,"  structure factor calculations since the Fourier transform")
      call text_(stdout,"  of densities related by such related symops are related by")
      call text_(stdout,"  complex conjugation and/or a multiplicative phase factor")
      call flush_(stdout)

      ! Table
      call create_(table,3)
      call set_heading_(table(1)," Related to")
      call set_subhead_(table(1),"  Inversion")
      call set_sb3head_(table(1)," of symop #")
      call set_center_(table(1),TRUE)
      call set_heading_(table(2)," Related to")
      call set_subhead_(table(2),"Translation")
      call set_sb3head_(table(2)," of symop #")
      call set_center_(table(2),TRUE)
      call set_heading_(table(3),"Unique")
      call set_subhead_(table(3),"symop?")
      call set_center_(table(3),TRUE)

      ! Set data
      call create_(unique,self%n_seitz)
      do n = 1,self%n_seitz
         unique(n) = "no"
         if (self%inverted_symop(n)==0 AND self%translated_symop(n)==n) unique(n) = "yes"
      end do

      ! Set table columns
      call set_values_(table(1),self%inverted_symop)
      call set_values_(table(2),self%translated_symop)
      call set_values_(table(3),unique)

      ! Put the table
      call put_(table)

      ! Clear
      call destroy_(table)
      call destroy_(unique)
   end subroutine

   subroutine put_matching_IT_symbols(symbol)
   ! Put out all the IT symbols which match "symbol".
      STR(len=*), IN :: symbol
      VEC(BIN,:), allocatable :: mask
      VEC(STR(len=STR_SIZE),:), allocatable :: IT,HM,Hall
      INT :: n,i
      call create_(mask,593)
      mask = includes_(spacegroup_conversion_table(1,:),trim(symbol),at_start=TRUE)
      n = count(mask)
      call create_(IT,n); IT = pack(spacegroup_conversion_table(1,:),mask)
      call create_(HM,n); HM = pack(spacegroup_conversion_table(3,:),mask)
      call create_(Hall,n); Hall = pack(spacegroup_conversion_table(4,:),mask)
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      call put_(stdout,"IT symbol")
      call put_(stdout,"HM symbol")
      call put_(stdout,"Hall symbol")
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      do i = 1,n
         call put_(stdout,IT(i))
         call put_(stdout,HM(i))
         call put_(stdout,Hall(i))
         call flush_(stdout)
      end do
      call dash_(stdout,real_fields=3)

      ! Clean
      call destroy_(Hall)
      call destroy_(HM)
      call destroy_(IT)
      call destroy_(mask)
   end subroutine

   subroutine put_matching_HM_symbols(symbol)
   ! Put out all the HM symbols which match "symbol".
      STR(len=*), IN :: symbol
      VEC(BIN,:), allocatable :: mask
      VEC(STR(len=STR_SIZE),:), allocatable :: IT,HM,Hall
      INT :: n,i
      call create_(mask,593)
      mask = includes_(spacegroup_conversion_table(3,:),trim(symbol),at_start=TRUE)
      n = count(mask)
      call create_(IT,n); IT = pack(spacegroup_conversion_table(1,:),mask)
      call create_(HM,n); HM = pack(spacegroup_conversion_table(3,:),mask)
      call create_(Hall,n); Hall = pack(spacegroup_conversion_table(4,:),mask)
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      call put_(stdout,"IT symbol")
      call put_(stdout,"HM symbol")
      call put_(stdout,"Hall symbol")
      call flush_(stdout)
      call dash_(stdout,real_fields=3)
      do i = 1,n
         call put_(stdout,IT(i))
         call put_(stdout,HM(i))
         call put_(stdout,Hall(i))
         call flush_(stdout)
      end do
      call dash_(stdout,real_fields=3)

      ! Clean
      call destroy_(Hall)
      call destroy_(HM)
      call destroy_(IT)
      call destroy_(mask)
   end subroutine

   subroutine put_CIF(self)
   ! Put out the spacegroup info in CIF format
      type(SPACEGROUP_TYPE), IN :: self
   ENSURE(allocated(self%seitz),"SPACEGROUP:put_CIF ... no seitz matrices")
      call flush_(stdout)
      call text_(stdout,"# ========")
      call text_(stdout,"# Symmetry")
      call text_(stdout,"# ========")
      call flush_(stdout)
      call text_(stdout,"_symmetry_cell_setting              "//trim(self%lattice_type))
      call text_(stdout,"_symmetry_space_group_name_H-M      '"//trim(self%HM_symbol)//"'")
      call text_(stdout,"_symmetry_space_group_name_Hall     '"//trim(self%Hall_symbol)//"'")
      call text_(stdout,"_symmetry_Int_Tables_number         '"//trim(to_str_(self%IT_group_number))//"'")
      call put_CIF_jf_symbols_(self)
   end subroutine

   subroutine put_CIF_jf_symbols(self)
   ! Put out the Jones-Faithful sumbols in cif notation
      type(SPACEGROUP_TYPE), IN :: self
      INT :: n
      STR(len=STR_SIZE) :: jf
   ENSURE(allocated(self%seitz),"SPACEGROUP:put_CIF_jf_symbols ... no seitz matrices")

      ! Jones-Faiful symbols
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"    _symmetry_equiv_pos_as_xyz")

      ! Build up the symbol for each symop n
      do n = 1,size(self%seitz,3)

         ! Get the JF symbol
         call recode_Jones_Faithful_symbol_(jf,self%seitz(:,:,n))

         ! Put quotes around the symbol, and put it out
         jf = "'"//trim(jf)//"'"
         call put_(stdout,trim(jf),left=TRUE)
         call flush_(stdout)
      end do  ! -- next symop
   end subroutine

end module
