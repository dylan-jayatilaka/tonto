!===================================================================
!
! UNIT_CELL: Data structure for a crystal unit cell
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1999
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
!===================================================================
module UNIT_CELL_MODULE

#  include "macros"

   include "unit_cell.use"

   implicit none

   include "unit_cell.int"

   REAL, private :: angle_tolerance = UNIT_CELL_ANGLE_TOLERANCE
   ! A cell angle this close to a right angle (in radians) is treated as
   ! exactly right. See "cell_cosines".

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(UNIT_CELL_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"UNIT_CELL:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(UNIT_CELL_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"UNIT_CELL:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ===========
!  Set methods
!  ===========

   subroutine set_defaults(self)
   ! Set up a default crystal object
   ! NOTE: all in bohr/radian units
      type(UNIT_CELL_TYPE), INOUT :: self
      self%angle = HALF_PI
      self%angle_error = ZERO
      self%length = ONE
      self%length_error = ZERO
      self%volume = -ONE
      self%volume_error = ZERO
      self%info_made = FALSE
      call make_info_(self)
   end subroutine

   pure subroutine set_lengths(self,length)
   ! Set cell lengths
   ! NOTE: all in bohr/radian units
      type(UNIT_CELL_TYPE), INOUT :: self
      VEC(REAL,3), IN :: length
      self%length = length
      self%length_error = ZERO
   end subroutine

   pure subroutine set_angles(self,angle)
   ! Set cell angles
      type(UNIT_CELL_TYPE), INOUT :: self
      VEC(REAL,3), IN :: angle
      self%angle = angle
      self%angle_error = ZERO
   end subroutine

   PURE subroutine set_units_to(self,kind)
   ! Set the units of all parameters to the "kind" system
      type(UNIT_CELL_TYPE), INOUT :: self
      STR(len=*), IN :: kind
      REAL :: fd,fa,f1,f3
   ENSURE(is_one_of_(kind,["cif","au "]),"UNIT_CELL:set_units_to ... unknown kind, "//trim(kind))
      fd = conversion_factor_("degree")
      fa = conversion_factor_("angstrom")
      f3 = conversion_factor_("angstrom^3")
      f1 = conversion_factor_("angstrom^-1")
      select case (kind)

      case ("cif")
         self%angle = self%angle*fd
         self%angle_error = self%angle_error*fd
         self%length = self%length*fa
         self%length_error = self%length_error*fa
         self%volume = self%volume*f3
         self%volume_error = self%volume_error*f3
         self%direct_mx = self%direct_mx*fa
         self%inverse_mx = self%inverse_mx*f1
         self%reciprocal_mx = self%reciprocal_mx*f1

      case ("au")
         self%angle = self%angle/fd
         self%angle_error = self%angle_error/fd
         self%length = self%length/fa
         self%length_error = self%length_error/fa
         self%volume = self%volume/f3
         self%volume_error = self%volume_error/f3
         self%direct_mx = self%direct_mx/fa
         self%inverse_mx = self%inverse_mx/f1
         self%reciprocal_mx = self%reciprocal_mx/f1
      end select
   end subroutine


!  ===============
!  Basic cell info
!  ===============

   pure function alpha_angle(self) result (res)
   ! Return the alpha angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%angle(1)
   end function

   pure function beta_angle(self) result (res)
   ! Return the beta angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%angle(2)
   end function

   pure function gamma_angle(self) result (res)
   ! Return the gamma angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%angle(3)
   end function

   pure function alpha_star_angle(self) result (res)
   ! Return the alpha reciprocal lattice angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      REAL :: tmp,alpha,beta,gamma
      alpha = self%angle(1)
      beta = self%angle(2)
      gamma = self%angle(3)
      tmp = (cos(beta)*cos(gamma)-cos(alpha))/(sin(beta)*sin(gamma))
      res = arccos_(tmp)
   end function

   pure function beta_star_angle(self) result (res)
   ! Return the beta reciprocal lattice angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      REAL :: tmp,alpha,beta,gamma
      alpha = self%angle(1)
      beta = self%angle(2)
      gamma = self%angle(3)
      tmp = (cos(gamma)*cos(alpha)-cos(beta))/(sin(gamma)*sin(alpha))
      res = arccos_(tmp)
   end function

   pure function gamma_star_angle(self) result (res)
   ! Return the gamma reciprocal lattice angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      REAL :: tmp,alpha,beta,gamma
      alpha = self%angle(1)
      beta = self%angle(2)
      gamma = self%angle(3)
      tmp = (cos(alpha)*cos(beta)-cos(gamma))/(sin(alpha)*sin(beta))
      res = arccos_(tmp)
   end function

   pure function a(self) result (res)
   ! Return the a cell length, in bohr.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%length(1)
   end function

   pure function b(self) result (res)
   ! Return the b cell length, in bohr.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%length(2)
   end function

   pure function c(self) result (res)
   ! Return the c cell length, in bohr.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%length(3)
   end function

   pure function a_star(self) result (res)
   ! Return the a reciprocal lattice length, in bohr.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%length(2)*self%length(3)*sin(self%angle(1))/self%volume
   end function

   pure function b_star(self) result (res)
   ! Return the b reciprocal lattice length, in bohr.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%length(3)*self%length(1)*sin(self%angle(2))/self%volume
   end function

   pure function c_star(self) result (res)
   ! Return the c reciprocal lattice length, in bohr.
      type(UNIT_CELL_TYPE), IN :: self
      REAL :: res
      res = self%length(1)*self%length(2)*sin(self%angle(3))/self%volume
   end function


!  ==========================
!  Make cell axis/volume info
!  ==========================

   subroutine update(self)
   ! Calculate the various unit cell axis matrices.
      type(UNIT_CELL_TYPE), INOUT :: self
      call make_info_(self)
   end subroutine

   subroutine make_info(self)
   ! Calculate the various unit cell axis matrices.
      type(UNIT_CELL_TYPE), INOUT :: self
      call make_volume_(self)
      call make_direct_mx_(self)
      call make_inverse_and_reciprocal_mx_(self)
      call make_direct_U_mx_(self)
      call make_reciprocal_U_mx_(self)
      self%info_made = TRUE
   end subroutine

   subroutine cell_cosines(self,ca,cb,cg)
   ! The cosines of the three cell angles, with anything below
   ! "angle_tolerance" set to exactly zero.
   ! NOTE: cos(HALF_PI) is 6.1d-17, not zero, however exactly PI/2 is
   ! stored -- PI/2 is not representable. Left alone that residue makes
   ! an orthogonal cell very slightly non-orthogonal, and a congruence
   ! that should be the identity then leaks round-off of arbitrary sign
   ! into tensor components which are exactly zero by symmetry. It
   ! produced negative ADP esds; see TASKS_AND_HISTORY.md.
   ! For a small deviation d from a right angle |cos(PI/2-d)| = sin(d) ~ d,
   ! so testing the cosine is testing the angle, in radians.
      type(UNIT_CELL_TYPE), IN :: self
      REAL, OUT :: ca,cb,cg
      ca = cos(self%angle(1))
      cb = cos(self%angle(2))
      cg = cos(self%angle(3))
      if (abs(ca)<angle_tolerance) ca = ZERO
      if (abs(cb)<angle_tolerance) cb = ZERO
      if (abs(cg)<angle_tolerance) cg = ZERO
   end subroutine

   subroutine make_volume(self)
   ! Calculate the cell volume
      type(UNIT_CELL_TYPE), INOUT :: self
      REAL :: a,b,c,ca,cb,cg
      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      call cell_cosines_(self,ca,cb,cg)
      self%volume = a*b*c*sqrt(ONE-ca*ca-cb*cb-cg*cg+TWO*ca*cb*cg)
   end subroutine

   subroutine make_direct_mx(self)
   ! Calculate the direct cell matrices "A" (i.e. cell axes) in units
   ! of BOHRS.
      type(UNIT_CELL_TYPE), INOUT :: self
      REAL :: v,a,b,c,ca,cb,cg,sg
      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      call cell_cosines_(self,ca,cb,cg)
      sg = sin(self%angle(3))

      ! Keep sin consistent with the snapped cosine
      if (cg==ZERO) sg = ONE
      v = self%volume

      ! Direct cell matrix
      ! From Giacovazzo, C., Fundamentals of Crystallography, p.68
      ! Note this is the transpose of "A" since "a", "b". "c"
      ! should be columns not rows.
      self%direct_mx(1,1) = a
      self%direct_mx(1,2) = b*cg
      self%direct_mx(1,3) = c*cb
      self%direct_mx(2,1) = ZERO
      self%direct_mx(2,2) = b*sg
      self%direct_mx(2,3) = c*(ca-cb*cg)/sg
      self%direct_mx(3,1) = ZERO
      self%direct_mx(3,2) = ZERO
      self%direct_mx(3,3) = v/(a*b*sg)
   end subroutine

   subroutine make_direct_mx_from(self,a,b,c)
   ! Set direct cell matrix axis vectors
   ! Then set everyting else.
   ! This is notthe usual ay it is done in crystallography,
   ! but the CRYSTALxx program seems to do it this way.
   ! NOTE: all in bohr/radian units
      type(UNIT_CELL_TYPE), INOUT :: self
      VEC(REAL,3), IN :: a,b,c
      REAL :: ca,cb,cg
      self%direct_mx(:,1) = a
      self%direct_mx(:,2) = b
      self%direct_mx(:,3) = c
      self%length(1) = norm_(a)
      self%length(2) = norm_(b)
      self%length(3) = norm_(c)
      ca = dot_product(b,c)/(self%length(2)*self%length(3))
      cb = dot_product(c,a)/(self%length(3)*self%length(1))
      cg = dot_product(a,b)/(self%length(1)*self%length(2))
      self%angle(1) = acos(ca)
      self%angle(2) = acos(cb)
      self%angle(3) = acos(cg)
      self%volume = product(self%length)*sqrt(ONE-ca*ca-cb*cb-cg*cg+TWO*ca*cb*cg)

      ! Kanghyun Chu 2024.Feb.2
      ! The rest from make_info
      call make_inverse_and_reciprocal_mx_(self)
      call make_direct_U_mx_(self)
      call make_reciprocal_U_mx_(self)
      self%info_made = TRUE
   end subroutine

   subroutine make_inverse_and_reciprocal_mx(self)
   ! Calculate the reciprocal cell matrix "(A^{-1})^T (i.e. reciprocal
   ! cell axes) in units of 1/BOHRS. Also calculate the inverse direct 
   ! cell matrix "A^-1" as the transpose of this.
      type(UNIT_CELL_TYPE), INOUT :: self

      ! Inverse direct cell matrix
      call to_inverse_of_(self%inverse_mx,self%direct_mx)

      ! Recriprocal cell matrix
      self%reciprocal_mx = transpose(self%inverse_mx)
   end subroutine


!   make_inverse_mx_and_reciprocal_mx :: private pure 
!   ! Kanghyun 2024.Feb.2
!   ! Calculate the inverse matrix from the direct cell matrix
!   ! Reciprocal matrix = transpose( inverse_mx )
!      self :: INOUT
!
!      v, A11, A12, A13, A21, A22, A23, A31, A32, A33 :: REAL
!      
!      v   = .volume ! same as determinant
!      A11 = .direct_mx(1,1)
!      A12 = .direct_mx(1,2)
!      A13 = .direct_mx(1,3)
!      A21 = .direct_mx(2,1)
!      A22 = .direct_mx(2,2)
!      A23 = .direct_mx(2,3)
!      A31 = .direct_mx(3,1)
!      A32 = .direct_mx(3,2)
!      A33 = .direct_mx(3,3)
!
!      ! The following code is the transpose of adjoint matrix //  
!      .reciprocal_mx(1,1) = A22 * A33 - A32 * A23 ! 23 , 23
!      .reciprocal_mx(1,2) = A23 * A31 - A21 * A33 ! 23 , 31
!      .reciprocal_mx(1,3) = A21 * A32 - A22 * A31 ! 23 , 12
!      .reciprocal_mx(2,1) = A32 * A13 - A33 * A12 ! 31 , 23
!      .reciprocal_mx(2,2) = A33 * A11 - A31 * A13 ! 31 , 31
!      .reciprocal_mx(2,3) = A31 * A12 - A32 * A11 ! 31 , 12
!      .reciprocal_mx(3,1) = A12 * A23 - A13 * A22 ! 12 , 23
!      .reciprocal_mx(3,2) = A13 * A21 - A11 * A23 ! 12 , 31
!      .reciprocal_mx(3,3) = A11 * A22 - A12 * A21 ! 12 , 12
!      ! //
!
!      .reciprocal_mx = .reciprocal_mx / v 
!      ! Kanghyun Note : 
!      !        inverse_mx = adjoint matrix / determinant
!
!      .inverse_mx    = transpose(.reciprocal_mx)
!      ! Kanghyun Note : 
!      !        inverse_mx * direct_mx = identity matrix
!      !        reciprocal_mx  = transpose( inverse_mx )   
!   end

   subroutine make_direct_U_mx(self)
   ! Return the transformation matrix (D L*)^T which changes the
   ! thermal ADP tensor from the crystal axis system (the usual U
   ! reported in the cif file) into the cartesian axis system:  
   !
   !     U_cart = (D L*) U_cif (D L*)^T
   !
   ! where D is the direct cell matrix and L* is the diagonal of
   ! reciprocal matrix lengths.
   ! See also comments for reciprocal_U_tensor_mx below.
   ! See also Grosse-Kuntsleve and Adams (2002) J. Appl. Cryst 35 p.477-480
      type(UNIT_CELL_TYPE), INOUT :: self
      REAL :: len
      INT :: i
      do i = 1,3
         len = norm_(self%reciprocal_mx(:,i))
         ! The transpose below is correct
         self%direct_U_mx(i,:) = self%direct_mx(:,i)*len
      end do
   end subroutine

   subroutine make_reciprocal_U_mx(self)
   ! Return the transformation matrix which changes the thermal ADP
   ! tensor from the cartesian axis system into the crystal axis
   ! system.
   !
   !    U_cif = (D* L*^-1)^T U_cart (D* L*^-1)
   !
   ! where D*=(D^-1)^T is the reciprocal cell matrix and L* is the
   ! diagonal of reciprocal matrix lengths.  The thermal tensor in the
   ! crystal axis system U_{ij} is defined by the temperature factor
   ! expansion:
   !
   !         TF = exp ( -2\pi^2 U_{ij} h_i h_j a^*_i a^*_j )
   !
   ! where h are the Miller indices and a^* are the reciprocal lattice
   ! constants (in bohr^{-2}). This is as used by systems like Xtal.
   ! The thermal tensor in the cartesian axis system U_{ij} is defined
   ! by the temperature factor expansion:
   !
   !         TF = exp ( -0.5 U_{ij} k_i k_j )
   !
   ! where k = 2\pi D* h.
   ! See also Grosse-Kuntsleve and Adams (2002) J. Appl. Cryst 35 p.477-480
      type(UNIT_CELL_TYPE), INOUT :: self
      REAL :: len
      INT :: i
      do i = 1,3
         len = ONE/norm_(self%reciprocal_mx(:,i))
         self%reciprocal_U_mx(:,i) = self%reciprocal_mx(:,i)*len
      end do
   end subroutine


!  ==========================
!  Geometry altering routines
!  ==========================

   PURE subroutine change_from_fractional_0(self,g)
   ! Change the columns of geometry array "g" *from* crystal fractional
   ! coordinates into standard cartesian coordiantes
      type(UNIT_CELL_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: g
   ENSURE(size(g,1)==3,"UNIT_CELL:change_from_fractional_0 ... dim1 is not 3")
      g = matmul(self%direct_mx,g)
   end subroutine

   PURE subroutine change_into_fractional_0(self,g)
   ! Change the columns of geometry array "g" from standard cartesian
   ! coordinates *into* crystal fractional coordinates
      type(UNIT_CELL_TYPE), IN :: self
      MAT(REAL,:,:), INOUT :: g
   ENSURE(size(g,1)==3,"UNIT_CELL:change_into_fractional_0 ... dim1 is not 3")
      g = matmul(self%inverse_mx,g)
   end subroutine

   PURE subroutine change_from_fractional_1(self,p)
   ! Change the position "p" *from* crystal fractional coordinates into standard
   ! cartesian coordiantes
      type(UNIT_CELL_TYPE), IN :: self
      VEC(REAL,3), INOUT :: p
      p = matmul(self%direct_mx,p)
   end subroutine

   PURE subroutine change_into_fractional_1(self,p)
   ! Change the position "p" from standard cartesian coordinates *into* crystal
   ! fractional coordinates
      type(UNIT_CELL_TYPE), IN :: self
      VEC(REAL,3), INOUT :: p
      p = matmul(self%inverse_mx,p)
   end subroutine

   PURE subroutine change_ADP_from_fractional(self,ADP_vector)
   ! Change thermal parameters in the cartesian axis system to the
   ! crystal axis system
      type(UNIT_CELL_TYPE), IN :: self
      VEC(REAL,6), INOUT :: ADP_vector
      MAT(REAL,3,3) :: U
      U(1,1) = ADP_vector(1)
      U(2,2) = ADP_vector(2)
      U(3,3) = ADP_vector(3)
      U(1,2) = ADP_vector(4)
      U(1,3) = ADP_vector(5)
      U(2,3) = ADP_vector(6)
      U(2,1) = ADP_vector(4)
      U(3,1) = ADP_vector(5)
      U(3,2) = ADP_vector(6)
      call change_basis_using_(U,self%direct_U_mx)
      ADP_vector(1) = U(1,1)
      ADP_vector(2) = U(2,2)
      ADP_vector(3) = U(3,3)
      ADP_vector(4) = U(1,2)
      ADP_vector(5) = U(1,3)
      ADP_vector(6) = U(2,3)
      ADP_vector(4) = U(2,1)
      ADP_vector(5) = U(3,1)
      ADP_vector(6) = U(3,2)
   end subroutine

   PURE subroutine change_ADP_into_fractional(self,ADP_vector)
   ! Change thermal parameters in the cartesian axis system to the
   ! crystal axis system
      type(UNIT_CELL_TYPE), IN :: self
      VEC(REAL,6), INOUT :: ADP_vector
      MAT(REAL,3,3) :: U
      U(1,1) = ADP_vector(1)
      U(2,2) = ADP_vector(2)
      U(3,3) = ADP_vector(3)
      U(1,2) = ADP_vector(4)
      U(1,3) = ADP_vector(5)
      U(2,3) = ADP_vector(6)
      U(2,1) = ADP_vector(4)
      U(3,1) = ADP_vector(5)
      U(3,2) = ADP_vector(6)
      call change_basis_using_(U,self%reciprocal_U_mx)
      ADP_vector(1) = U(1,1)
      ADP_vector(2) = U(2,2)
      ADP_vector(3) = U(3,3)
      ADP_vector(4) = U(1,2)
      ADP_vector(5) = U(1,3)
      ADP_vector(6) = U(2,3)
      ADP_vector(4) = U(2,1)
      ADP_vector(5) = U(3,1)
      ADP_vector(6) = U(3,2)
   end subroutine


!  ============
!  Read methods
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(UNIT_CELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","UNIT_CELL:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"UNIT_CELL:read_keywords ... unexpected end of file while reading list keywords")
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
      type(UNIT_CELL_TYPE), INOUT :: self
      STR(len=*) :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                ")  ! exit read_loop
      case ("a=               "); call read_a_(self)
      case ("alpha=           "); call read_alpha_(self)
      case ("angles=          "); call read_angles_(self)
      case ("b=               "); call read_b_(self)
      case ("beta=            "); call read_beta_(self)
      case ("c=               "); call read_c_(self)
      case ("dimensions=      "); call read_lengths_(self)
      case ("gamma=           "); call read_gamma_(self)
      case ("junk=            "); call read_junk_(self)
      case ("lengths=         "); call read_lengths_(self)
      case ("put              "); call put_(self)
      case ("units=           "); call read_units_(self)
      case default; allocate(tonto%known_keywords(13))
      tonto%known_keywords(1) = "}                "
      tonto%known_keywords(2) = "a=               "
      tonto%known_keywords(3) = "alpha=           "
      tonto%known_keywords(4) = "angles=          "
      tonto%known_keywords(5) = "b=               "
      tonto%known_keywords(6) = "beta=            "
      tonto%known_keywords(7) = "c=               "
      tonto%known_keywords(8) = "dimensions=      "
      tonto%known_keywords(9) = "gamma=           "
      tonto%known_keywords(10) = "junk=            "
      tonto%known_keywords(11) = "lengths=         "
      tonto%known_keywords(12) = "put              "
      tonto%known_keywords(13) = "units=           "
      call unknown_(tonto,word,"UNIT_CELL:process_keyword")
      deallocate(tonto%known_keywords)
      end select
      call update_(self)
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(UNIT_CELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_junk(self)
   ! Read in a junk string, useful for ignoring a field
      type(UNIT_CELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call skip_next_item_(stdin)
   end subroutine

   subroutine read_lengths(self)
   ! Read the unit cell axis lengths
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%length,self%length_error)
   end subroutine

   subroutine read_a(self)
   ! Read the a length
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%length(1))
   end subroutine

   subroutine read_b(self)
   ! Read the b length
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%length(2))
   end subroutine

   subroutine read_c(self)
   ! Read the c length
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%length(3))
   end subroutine

   subroutine read_angles(self)
   ! Read the unit cell angles
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%angle,self%angle_error)
   end subroutine

   subroutine read_alpha(self)
   ! Read the alpha angle
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%angle(1))
   end subroutine

   subroutine read_beta(self)
   ! Read the beta angle
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%angle(2))
   end subroutine

   subroutine read_gamma(self)
   ! Read the gamma angle
      type(UNIT_CELL_TYPE), INOUT :: self
      call read_quantity_(stdin,self%angle(3))
   end subroutine

   subroutine read_CIF(self,cif)
   ! Read cell information from a CIF file "cif"
      type(UNIT_CELL_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (NOT cif%is_mmCIF) then
         call read_smCIF_(self,cif)
      else
         call read_mmCIF_(self,cif)
      end if
   end subroutine

   subroutine read_smCIF(self,cif)
   ! Read cell information from a small-molecule CIF file "cif"
      type(UNIT_CELL_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: found

      ! Set defaults
      call set_defaults_(self)
      call set_units_to_(self,"cif")

      ! Read it in
      call read_item_(cif,"_cell_angle_alpha",self%angle(1),self%angle_error(1),found)
      call read_item_(cif,"_cell_angle_beta",self%angle(2),self%angle_error(2),found)
      call read_item_(cif,"_cell_angle_gamma",self%angle(3),self%angle_error(3),found)
      call read_item_(cif,"_cell_length_a",self%length(1),self%length_error(1),found)
      call read_item_(cif,"_cell_length_b",self%length(2),self%length_error(2),found)
      call read_item_(cif,"_cell_length_c",self%length(3),self%length_error(3),found)
      call read_item_(cif,"_cell_volume",self%volume,self%volume_error,found)

      ! Convert to standard units
      call set_units_to_(self,"au")

      ! Calculate associated information
      call update_(self)
   end subroutine

   subroutine read_mmCIF(self,cif)
   ! Read cell information from a macromolecular CIF file "cif"
      type(UNIT_CELL_TYPE), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: found

      ! Set defaults
      call set_defaults_(self)
      call set_units_to_(self,"cif")

      ! Read it in
      call read_item_(cif,"_cell.angle_alpha",self%angle(1),self%angle_error(1),found)
      call read_item_(cif,"_cell.angle_beta",self%angle(2),self%angle_error(2),found)
      call read_item_(cif,"_cell.angle_gamma",self%angle(3),self%angle_error(3),found)
      call read_item_(cif,"_cell.length_a",self%length(1),self%length_error(1),found)
      call read_item_(cif,"_cell.length_b",self%length(2),self%length_error(2),found)
      call read_item_(cif,"_cell.length_c",self%length(3),self%length_error(3),found)

      ! Seems for protein guiys, they can't be bothered
   ! cif.read_item("_cell.volume",.volume,.volume_error,found)

      ! Convert to standard units
      call set_units_to_(self,"au")

      ! Calculate associated information
      call update_(self)
   end subroutine


!  ==========================
!  Local field factor tensors
!  ==========================

   PURE subroutine make_LFF_tensors_0(self,L2,pos)
   ! Make the local field factor tensor "L2" at a series of unit cell
   ! positions "pos" (in cartesian coordinates). These tell what the
   ! electric field is at a certain position k1 when there is a dipole
   ! at position k2 in the unit cell and all translationally
   ! equivalent cells, F = L2(:,:,k1k2) mu(:). The last index of "L2"
   ! is a triangle index. This uses formula in ref:
   ! Cummins et al (1976), Acta Cryst A32, p. 847
      type(UNIT_CELL_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: L2
      MAT(REAL,:,:), IN :: pos
      MAT(REAL,3,3) :: L,L0,G,H
      VEC(REAL,3) :: r,r0,q,hv
      INT :: k,k1,k2,m,h1,h2,h3
      REAL :: v13,vr,vq,dot
   ENSURE(size(L2,1)==3 AND size(L2,2)==3,"UNIT_CELL:make_LFF_tensors_0 ... L2 must be 3x3")
   ENSURE(size(pos,1)==3,"UNIT_CELL:make_LFF_tensors_0 ... pos must have first dimension 3")
      v13 = self%volume**(THIRD)
      vr = SQRT_PI/v13
      vq = SQRT_PI*v13
      k = 0
      do k1 = 1,size(pos,2)
         do k2 = 1,k1
            k = k+1
            r0 = vr*(pos(:,k2)-pos(:,k1))
            if (k1==k2) then
               call to_unit_mx_(L0)
               L0 = THIRD*L0
            else
               call make_LFF_h_tensor(r0,H)
               L0 = H
            end if
            m = 0
            do  ! until converged
               m = m+1
               L = ZERO
               do h1 = -m,m,2*m
                  do h2 = -m,m
                     do h3 = -m,m
                        hv = [h1,h2,h3]
                        call to_scaled_product_of_(r,self%direct_mx,hv,vr)
                        r = r+r0
                        call to_scaled_product_of_(q,self%reciprocal_mx,hv,vq)
                        call make_LFF_h_tensor(r,H)
                        call make_LFF_g_tensor(q,G)
                        dot = q(1)*r0(1)+q(2)*r0(2)+q(3)*r0(3)
                        L = L+H-G*cos(TWO*dot)
                     end do
                  end do
               end do
               do h1 = -m+1,m-1
                  do h2 = -m,m,2*m
                     do h3 = -m,m
                        hv = [h1,h2,h3]
                        call to_scaled_product_of_(r,self%direct_mx,hv,vr)
                        r = r+r0
                        call to_scaled_product_of_(q,self%reciprocal_mx,hv,vq)
                        call make_LFF_h_tensor(r,H)
                        call make_LFF_g_tensor(q,G)
                        dot = q(1)*r0(1)+q(2)*r0(2)+q(3)*r0(3)
                        L = L+H-G*cos(TWO*dot)
                     end do
                  end do
               end do
               do h1 = -m+1,m-1
                  do h2 = -m+1,m-1
                     do h3 = -m,m,2*m
                        hv = [h1,h2,h3]
                        call to_scaled_product_of_(r,self%direct_mx,hv,vr)
                        r = r+r0
                        call to_scaled_product_of_(q,self%reciprocal_mx,hv,vq)
                        call make_LFF_h_tensor(r,H)
                        call make_LFF_g_tensor(q,G)
                        dot = q(1)*r0(1)+q(2)*r0(2)+q(3)*r0(3)
                        L = L+H-G*cos(TWO*dot)
                     end do
                  end do
               end do
               L0 = L0+L
               if (maxval(abs(L))<TOL(10)) exit
            end do
            L2(:,:,k) = L0
         end do
      end do
   end subroutine

   PURE subroutine make_LFF_h_tensor(r,h2)
   ! Make the local field factor H tensors. See appendix of:
   ! Cummins et al (1976), Acta Cryst A32, p. 847
      VEC(REAL,3), IN :: r
      MAT(REAL,3,3), OUT :: h2
      INT :: a,b
      REAL :: r1,r2,rm1,rm2,rm5,faa,fab,f1,f2
      REAL :: exmr2,erfcr,sqpi2,ra,rb,rab

      ! Temporary variables related to R
      r2 = r(1)*r(1)+r(2)*r(2)+r(3)*r(3)
      r1 = sqrt(r2)
      rm1 = ONE/r1
      rm2 = rm1*rm1
      rm5 = rm2*rm2*rm1

      ! Inverse R sums, including exponential term
      f1 = rm2
      f2 = rm2*(3*f1+2)
      exmr2 = HALF*exp(-r2)
      f1 = f1*exmr2
      f2 = f2*exmr2

      ! Complementary erf term
      erfcr = erfc(r1)
      sqpi2 = SQRT_PI*QUARTER*erfcr*rm5

      ! These are for h2
      faa = -f1-r2*sqpi2
      fab = f2+3*sqpi2

      ! Now make the h tensors ...
      h2 = ZERO
      do a = 1,3
         ra = r(a)
         do b = 1,3
            rb = r(b)
            rab = ra*rb
            h2(a,b) = h2(a,b)+fab*rab
         end do
         h2(a,a) = h2(a,a)+faa
      end do
   end subroutine

   PURE subroutine make_LFF_g_tensor(q,g2)
   ! Make the local field factor tensor. See appendix of:
   ! Cummins et al (1976), Acta Cryst A32, p. 847
      VEC(REAL,3), IN :: q
      MAT(REAL,3,3), OUT :: g2
      INT :: a,b
      REAL :: q2,qm2,exmq2,qa,qb,fab
      q2 = q(1)*q(1)+q(2)*q(2)+q(3)*q(3)
      qm2 = ONE/q2
      exmq2 = exp(-q2)
      fab = qm2*exmq2
      do a = 1,3
         qa = q(a)
         do b = 1,3
            qb = q(b)
            g2(a,b) = qa*qb*fab
         end do
      end do
   end subroutine


!   make_LFF_h_tensors(r,h2,h3,h4)
!   ! Make the local field factor tensors. 
!      r :: VEC{REAL}(3), IN
!      h2 :: MAT{REAL}(3,3), OUT
!      h3 :: MAT3{REAL}(3,3,3), OUT
!      h4 :: MAT4{REAL}(3,3,3,3), OUT
!
!      a,b,c,d :: INT
!      r1,r2,rm1,rm2,rm5,v13,v23,r13,r23 :: REAL
!      faa,fab,fabb,fabc,fabab,fabcc,fabcd :: REAL
!      f1,f2,f3,f4,exmr2,erfcr,sqpi2,val,tmp :: REAL
!      ra,rb,rc,rd,rab,rabc,rabcd :: REAL
!
!      ! Temporary variables related to R
!      r2  = r(1)*r(1) + r(2)*r(2) + r(3)*r(3)
!      r1  = sqrt(r2)
!      rm1 = ONE/r1
!      rm2 = rm1*rm1
!      rm5 = rm2*rm2*rm1
!
!      ! Temporary variables relatd to V
!      v13 = SQRT_PI*.volume**(-THIRD)
!      v23 = v13*v13
!      r13 = v13*rm2
!      r23 = r13*r13
!
!      ! Inverse R sums, including exponential term
!      f1 =  rm2
!      f2 =  rm2*(3*f1 + 2)
!      f3 =  rm2*(5*f2 + 4)
!      f4 =  rm2*(7*f3 + 8)
!      exmr2 = HALF*exp(-r2)
!      f1 = f1*exmr2
!      f2 = f2*exmr2
!      f3 = f3*exmr2
!      f4 = f4*exmr2
!
!      ! Complementary erf term
!      erfcr = r1.erfc
!      sqpi2 = SQRT_PI*QUARTER*erfcr*rm5
!
!      ! These are for h2
!      faa = -f1
!      fab =  f2
!      val = sqpi2
!      faa = faa - r2*val
!      fab = fab +  3*val
!
!      ! These are for h3
!      fabb =  f2*v13
!      fabc = -f3*v13
!      val = r13*sqpi2
!      fabb = fabb +  3*r2*val
!      fabc = fabc - 15*val
!
!      ! These are for h4
!      fabab =  f2*v23
!      fabcc = -f3*v23
!      fabcd =  f4*v23
!      val = r23*sqpi2
!      fabab = fabab +   3*r2*r2*val
!      fabcc = fabcc -  15*r2*val
!      fabcd = fabcd + 105*val
!
!      ! Now make the h tensors ...
!      h2 = ZERO
!      h3 = ZERO
!      h4 = ZERO
!
!      do a = 1,3
!         ra = r(a)
!         do b = 1,3
!            rb = r(b)
!            rab = ra*rb
!            h2(a,b) = h2(a,b) + fab*rab
!            do c = 1,3
!               rc = r(c)
!               rabc = rab*rc
!               h3(a,b,c) = h3(a,b,c) + fabc*rabc
!               do d = 1,3
!                  rd = r(d)
!                  rabcd = rabc*rd
!                  h4(a,b,c,d) = h4(a,b,c,d) + fabcd*rabcd
!               end
!               tmp = fabcc*rab
!               h4(a,b,c,c) = h4(a,b,c,c) + tmp
!               h4(a,c,b,c) = h4(a,c,b,c) + tmp
!               h4(a,c,c,b) = h4(a,c,c,b) + tmp
!               h4(c,a,b,c) = h4(c,a,b,c) + tmp
!               h4(c,a,c,b) = h4(c,a,c,b) + tmp
!               h4(c,c,a,b) = h4(c,c,a,b) + tmp
!            end
!            tmp = fabb*ra
!            h3(a,b,b) = h3(a,b,b) + tmp
!            h3(b,b,a) = h3(b,b,a) + tmp
!            h3(b,a,b) = h3(b,a,b) + tmp
!            h4(a,b,a,b) = h4(a,b,a,b) + fabab
!            h4(a,a,b,b) = h4(a,a,b,b) + fabab
!            h4(a,b,b,a) = h4(a,b,b,a) + fabab
!         end
!         h2(a,a) = h2(a,a) + faa
!      end
!      
!   end

!   make_LFF_g_tensors(q,g2,g3,g4)
!   ! Make the local field factor tensor.
!      q :: VEC{REAL}(3), IN
!      g2 :: MAT{REAL}(3,3), OUT
!      g3 :: MAT3{REAL}(3,3,3), OUT
!      g4 :: MAT4{REAL}(3,3,3,3), OUT
!
!      a,b,c,d :: INT
!      q2,qm2,exmq2,qa,qb,qc,qd,v13,fab,gab,gabc,gabcd :: REAL
!
!      q2  = q(1)*q(1) + q(2)*q(2) + q(3)*q(3)
!      qm2 = ONE/q2
!
!      v13 = TWO*SQRT_PI*.volume**(-THIRD)
!
!      exmq2 = exp(-q2)
!
!      fab = qm2*exmq2
!
!      do a = 1,3
!         qa = q(a)
!         do b = 1,3
!            qb = q(b)
!            gab = qa*qb*fab
!            g2(a,b) = gab
!            do c = 1,3
!               qc = q(c)
!               gabc = v13*qc*gab
!               g3(a,b,c) = gabc
!               do d = 1,3
!                  qd = q(d)
!                  gabcd = v13*qd*gabc
!                  g4(a,b,c,d) = gabc
!               end
!            end
!         end
!      end
!      
!   end

!   make_LFF_h1_tensor(h2,x,n)
!   ! Make the local field factor tensors from the formula in 
!   ! Reis (2000), JCP 112 p. 6161, Appendix B.
!      h2 :: MAT{REAL}(3,3), OUT
!      x :: VEC{REAL}(3), IN
!      n :: INT, IN
!
!      i,j :: INT
!      x1,x2,xm1,xm2,xm3,erfx,expx,fij,fii :: REAL
!
!      ! Temporary variables related to R
!      x2  = x(1)*x(1) + x(2)*x(2) + x(3)*x(3)
!      x1  = sqrt(x2)
!      xm1 = ONE/x1
!      xm3 = xm1*xm1*xm1
!
!      ! Complementary erf term
!      if (n==0) then; erfx = -x1.erf
!      else;           erfx =  x1.erfc
!      end
!
!      erfx  = xm3*erfx
!      expx  = xm2*TWO*exp(-x2)/SQRT_PI
!      fii   = -erfx - expx
!      fij   = THREE*xm2*erfx + (THREE*xm2+TWO)*expx
!
!      ! Now make the h tensors ...
!      h2 = ZERO
!      do i = 1,3
!         xi = x(i)
!         do j = 1,3
!            xj = x(j)
!            h2(i,j) = h2(i,j) + fij*xi*xj
!         end
!         h2(i,i) = h2(i,i) + fii
!      end
!      
!   end

   PURE subroutine make_LFF_tensors_1(self,L0,L1,pos,mol_for,sphere_sum)
   ! Make the charge local field factor (LFF) tensor "L0" and the
   ! dipole LFF tensor "L1" at a series of cartesian cell positions
   ! "pos" (usually the atom positions).  "mol_for" gives the molecule
   ! that the position is attached to. These tensors give the electric
   ! field at position p1, due to a charge and dipole at another
   ! position p2 in the unit cell *and* at all translationally
   ! equivalent points. Thus:
   !           F(:) = L0(:,:) q(:) + L1(:,:) mu(:).  
   ! where F(:) are the fields at all the positions as a flattened
   ! vector, and q(:) and mu(:) are charges and dipoles also as
   ! flattened vectors.
   ! >>>> See: Reis (2000) JCP 112 p. 6161 Appendix B.
      type(UNIT_CELL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: L0,L1
      MAT(REAL,:,:), IN :: pos
      VEC(INT,:), IN :: mol_for
      BIN, optional, IN :: sphere_sum
      MAT(REAL,3,3) :: W1,H1,G1
      VEC(REAL,3) :: W0,r0,rt,yt,H0,G0,t
      INT :: p1,p2,k1,k2,i,j,m,t1,t2,t3
      REAL :: v,v13,R,R2,iso,fd0,fd1,fy0,fy1,dot
      BIN :: same_mol,same_site,add_iso,add_H0,add_H1,sphere
   ENSURE(size(L0,1)==3*size(pos,2) AND size(L0,2)==size(pos,2),"UNIT_CELL:make_LFF_tensors_1 ... L0 wrong size")
   ENSURE(size(L1,1)==3*size(pos,2) AND is_square_(L1),"UNIT_CELL:make_LFF_tensors_1 ... L1 wrong size")
   ENSURE(size(pos,1)==3,"UNIT_CELL:make_LFF_tensors_1 ... pos must have first dimension 3")
   ENSURE(size(pos,2)==size(mol_for),"UNIT_CELL:make_LFF_tensors_1 ... pos and mol_for, incompatible")
   ENSURE(size(mol_for,1)==size(pos,2),"UNIT_CELL:make_LFF_tensors_1 ... mol_for wrong size")

      ! See: Reis (2000) JCP 112 p. 6161 Appendix B.
      sphere = FALSE
      if (present(sphere_sum)) sphere = sphere_sum

      ! Cell volume constants
      v = self%volume
      v13 = v**(THIRD)

      ! Arbitrary convergence factor
      R = SQRT_PI/v13
      R2 = R*R

      ! Constants
      iso = v*R2*R/(THREE*PI*SQRT_PI)
      fd0 = v*R2/(FOUR*PI)
      fd1 = R*fd0
      fy0 = HALF/R
      fy1 = PI/R
      i = -3
      do p1 = 1,size(pos,2)

         ! Supermatrix first index
         i = i+3

         ! Molecule point p1 belongs to
         k1 = mol_for(p1)
         j = -3
         do p2 = 1,size(pos,2)

            ! Supermatrix second index (L1 only)
            j = j+3

            ! Molecule point p2 belongs to
            k2 = mol_for(p2)

            ! Delta functions 
            same_mol = k1==k2
            same_site = p1==p2
            add_iso = (same_mol AND same_site) AND NOT sphere
            add_H0 = same_mol AND NOT same_site
            add_H1 = NOT same_mol

            ! Unit cell position difference
            r0 = R*(pos(:,p2)-pos(:,p1))

            ! The 3x1 and 3x3 submatrices
            L0(i+1:i+3,p2) = ZERO
            L1(i+1:i+3,j+1:j+3) = ZERO

            ! The isotropic part, only for L1
            if (add_iso) then
               call to_unit_mx_(L1(i+1:i+3,j+1:j+3))
               L1(i+1:i+3,j+1:j+3) = iso*L1(i+1:i+3,j+1:j+3)
            end if

            ! The n=0 part
            if (add_H0) then
               call make_LFF_h_tensors(H0,H1,r0,n=0)
               L0(i+1:i+3,p2) = L0(i+1:i+3,p2)-fd0*H0
               L1(i+1:i+3,j+1:j+3) = L1(i+1:i+3,j+1:j+3)+fd1*H1
            end if

            ! The l=0, n=1 part
            if (add_H1) then
               call make_LFF_h_tensors(H0,H1,r0,n=1)
               L0(i+1:i+3,p2) = L0(i+1:i+3,p2)-fd0*H0
               L1(i+1:i+3,j+1:j+3) = L1(i+1:i+3,j+1:j+3)+fd1*H1
            end if

            ! The extent of the cubic lattice
            m = 0
            do  ! until converged
               m = m+1
               W0 = ZERO
               W1 = ZERO
               do t1 = -m,m,2*m
                  do t2 = -m,m
                     do t3 = -m,m
                        t = [t1,t2,t3]
                        call to_scaled_product_of_(rt,self%direct_mx,t,R)
                        rt = rt+r0
                        call make_LFF_h_tensors(H0,H1,rt,n=1)
                        call to_scaled_product_of_(yt,self%reciprocal_mx,t,fy1)
                        call make_LFF_g_tensors(G0,G1,yt)
                        dot = yt(1)*r0(1)+yt(2)*r0(2)+yt(3)*r0(3)
                        W0 = W0-fd0*H0-fy0*G0*sin(TWO*dot)
                        W1 = W1+fd1*H1-G1*cos(TWO*dot)
                     end do
                  end do
               end do
               do t1 = -m+1,m-1
                  do t2 = -m,m,2*m
                     do t3 = -m,m
                        t = [t1,t2,t3]
                        call to_scaled_product_of_(rt,self%direct_mx,t,R)
                        rt = rt+r0
                        call make_LFF_h_tensors(H0,H1,rt,n=1)
                        call to_scaled_product_of_(yt,self%reciprocal_mx,t,fy1)
                        call make_LFF_g_tensors(G0,G1,yt)
                        dot = yt(1)*r0(1)+yt(2)*r0(2)+yt(3)*r0(3)
                        W0 = W0-fd0*H0-fy0*G0*sin(TWO*dot)
                        W1 = W1+fd1*H1-G1*cos(TWO*dot)
                     end do
                  end do
               end do
               do t1 = -m+1,m-1
                  do t2 = -m+1,m-1
                     do t3 = -m,m,2*m
                        t = [t1,t2,t3]
                        call to_scaled_product_of_(rt,self%direct_mx,t,R)
                        rt = rt+r0
                        call make_LFF_h_tensors(H0,H1,rt,n=1)
                        call to_scaled_product_of_(yt,self%reciprocal_mx,t,fy1)
                        call make_LFF_g_tensors(G0,G1,yt)
                        dot = yt(1)*r0(1)+yt(2)*r0(2)+yt(3)*r0(3)
                        W0 = W0-fd0*H0-fy0*G0*sin(TWO*dot)
                        W1 = W1+fd1*H1-G1*cos(TWO*dot)
                     end do
                  end do
               end do
               L0(i+1:i+3,p2) = L0(i+1:i+3,p2)+W0
               L1(i+1:i+3,j+1:j+3) = L1(i+1:i+3,j+1:j+3)+W1
               if (m>=5 AND maxval(abs(W0))<TOL(10) AND maxval(abs(W1))<TOL(10)) exit
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_LFF_h_tensors(H0,H1,x,n)
   ! Make the local field factor h tensors from the formula in 
   ! Reis (2000), JCP 112 p. 6161, Appendix B.
      VEC(REAL,3), OUT :: H0
      MAT(REAL,3,3), OUT :: H1
      VEC(REAL,3), IN :: x
      INT, IN :: n
      INT :: i,j
      REAL :: x1,x2,xm1,xm2,xm3,erfx,expx,fij,fii,fi,xi,xj

      ! Temporary variables related to R
      x2 = x(1)*x(1)+x(2)*x(2)+x(3)*x(3)
      x1 = sqrt(x2)
      xm1 = ONE/x1
      xm2 = xm1*xm1
      xm3 = xm2*xm1

      ! Complementary erf term
      if (n==0) then; erfx = -erf(x1)
      else; erfx = erfc(x1)
      end if
      erfx = xm3*erfx
      expx = xm2*TWO*exp(-x2)/SQRT_PI
      fii = -erfx-expx
      fij = THREE*xm2*erfx+(THREE*xm2+TWO)*expx
      fi = erfx+expx

      ! Now make the h tensors ...
      H0 = ZERO
      H1 = ZERO
      do i = 1,3
         xi = x(i)
         do j = 1,3
            xj = x(j)
            H1(i,j) = H1(i,j)+fij*xi*xj
         end do
         H1(i,i) = H1(i,i)+fii
         H0(i) = H0(i)+fi*xi
      end do
   end subroutine

   PURE subroutine make_LFF_g_tensors(G0,G1,x)
   ! G(x) function from
   ! Reis (2000), JCP 112 p. 6161, Appendix B.
      VEC(REAL,3), OUT :: G0
      MAT(REAL,3,3), OUT :: G1
      VEC(REAL,3), IN :: x
      INT :: i,j
      REAL :: x2,xi,xj,g

      ! Temporary variables related to R
      x2 = x(1)*x(1)+x(2)*x(2)+x(3)*x(3)
      g = exp(-x2)/x2
      do i = 1,3
         xi = x(i)
         do j = 1,3
            xj = x(j)
            G1(i,j) = xi*xj*g
         end do
         G0(i) = xi*g  ! need to be multiplied by 1/2R
      end do
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(UNIT_CELL_TYPE), IN :: self
      call dump_(stdout,"angle",self%angle)
      call dump_(stdout,"length",self%length)
      call dump_(stdout,"volume",self%volume)
      call dump_(stdout,"direct_mx",self%direct_mx)
      call dump_(stdout,"inverse_mx",self%inverse_mx)
      call dump_(stdout,"reciprocal_mx",self%reciprocal_mx)
      call dump_(stdout,"direct_U_mx",self%direct_U_mx)
      call dump_(stdout,"reciprocal_U_mx",self%reciprocal_U_mx)
      call dump_(stdout,"info_made",self%info_made)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(UNIT_CELL_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":UNIT_CELL@ = { ")
      else; call text_(stdout,trim(object_name)//":UNIT_CELL = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(UNIT_CELL_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":UNIT_CELL@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ===========
!  Put methods
!  ===========

   subroutine put(self)
   ! Put unit cell information 
      type(UNIT_CELL_TYPE), INOUT :: self
      REAL :: fac

      ! Convert to CIF units
      call set_units_to_(self,"cif")

      ! Change defaults
      call save_style_(stdout)
      call set_real_width_(stdout,14)
      call set_real_precision_(stdout,6)

      ! Print
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Unit cell information")
      call text_(stdout,"=====================")
      call flush_(stdout)
      call text_(stdout,". Lengths are in Angstrom")
      call text_(stdout,". Angles  are in degrees")
      call flush_(stdout)
      call show_(stdout,"a cell parameter         =",self%length(1),self%length_error(1))
      call show_(stdout,"b cell parameter         =",self%length(2),self%length_error(2))
      call show_(stdout,"c cell parameter         =",self%length(3),self%length_error(3))
      call show_(stdout,"Cell volume              =",self%volume,self%volume_error)
      call flush_(stdout)
      call show_(stdout,"alpha angle              =",self%angle(1),self%angle_error(1))
      call show_(stdout,"beta  angle              =",self%angle(2),self%angle_error(2))
      call show_(stdout,"gamma angle              =",self%angle(3),self%angle_error(3))

      ! Need to go back to au here
      call set_units_to_(self,"au")
      fac = conversion_factor_("angstrom^-1")
      call flush_(stdout)
      call show_(stdout,"a* cell parameter (A^-1) =",a_star_(self)*fac)
      call show_(stdout,"b* cell parameter (A^-1) =",b_star_(self)*fac)
      call show_(stdout,"c* cell parameter (A^-1) =",c_star_(self)*fac)
      fac = conversion_factor_("degree")
      call flush_(stdout)
      call show_(stdout,"alpha* angle (degree)    =",alpha_star_angle_(self)*fac)
      call show_(stdout,"beta*  angle (degree)    =",beta_star_angle_(self)*fac)
      call show_(stdout,"gamma* angle (degree)    =",gamma_star_angle_(self)*fac)

      ! Back to cif units
      call set_units_to_(self,"cif")
      call flush_(stdout)
      call text_(stdout,"Direct cell matrix")
      call put_(stdout,self%direct_mx,auto_width=TRUE)
      call flush_(stdout)
      call text_(stdout,"Reciprocal cell matrix")
      call put_(stdout,self%reciprocal_mx,auto_width=TRUE)

!      stdout.text("Inverse matrix")
!      stdout.flush
!      stdout.put(.inverse_mx, auto_width=TRUE)

      ! Put back defaults
      call unsave_style_(stdout)

      ! Convert back
      call set_units_to_(self,"au")
   end subroutine

   subroutine put_CX(self,label,formula,spacegroup)
   ! Output some information for the Crystal Explorer program.
      type(UNIT_CELL_TYPE), INOUT :: self
      STR(len=*), IN :: label,formula,spacegroup

      ! Convert to CIF units
      call set_units_to_(self,"cif")

      ! Output
      call flush_(stdout)
      call text_(stdout,"begin crystalcell "//trim(label))
      call text_(stdout,"   formula = "//'"'//trim(formula)//'"')
      call text_(stdout,"   spacegroup = "//'"'//trim(spacegroup)//'"')
      call show_(stdout,"   a =",self%length(1),dots=FALSE)
      call show_(stdout,"   b =",self%length(2),dots=FALSE)
      call show_(stdout,"   c =",self%length(3),dots=FALSE)
      call show_(stdout,"   alpha =",self%angle(1),dots=FALSE)
      call show_(stdout,"   beta  =",self%angle(2),dots=FALSE)
      call show_(stdout,"   gamma =",self%angle(3),dots=FALSE)
      call text_(stdout,"end crystalcell")

      ! Convert back
      call set_units_to_(self,"au")
   end subroutine

   subroutine put_CIF(self,Z,cart,har)
   ! Put unit cell information in CIF format
      type(UNIT_CELL_TYPE), INOUT :: self
      REAL, IN :: Z
      BIN, optional, IN :: cart
      BIN, optional, IN :: har
      REAL :: a,b,c,da,db,dc,v,dv
      BIN :: dohar

      ! Convert to CIF units
      call set_units_to_(self,"cif")

      ! Change defaults
      call save_style_(stdout)
      call set_real_width_(stdout,14)
      call set_real_precision_(stdout,6)

      ! Output
      call flush_(stdout)
      call text_(stdout,"# =========")
      call text_(stdout,"# Unit cell")
      call text_(stdout,"# =========")
      call flush_(stdout)
      call show_(stdout,"_cell_measurement_temperature ","?",dots=FALSE)
      call show_(stdout,"_cell_measurement_reflns_used ","?",dots=FALSE)
      call show_(stdout,"_cell_measurement_theta_min   ","?",dots=FALSE)
      call show_(stdout,"_cell_measurement_theta_max   ","?",dots=FALSE)
      a = self%length(1)
      b = self%length(2)
      c = self%length(3)
      da = self%length_error(1)
      db = self%length_error(2)
      dc = self%length_error(3)
      call flush_(stdout)
      call show_(stdout,"_cell_length_a                ",a,da,dots=FALSE)
      call show_(stdout,"_cell_length_b                ",b,db,dots=FALSE)
      call show_(stdout,"_cell_length_c                ",c,dc,dots=FALSE)
      a = self%angle(1)
      b = self%angle(2)
      c = self%angle(3)
      da = self%angle_error(1)
      db = self%angle_error(2)
      dc = self%angle_error(3)
      call show_(stdout,"_cell_angle_alpha             ",a,da,dots=FALSE)
      call show_(stdout,"_cell_angle_beta              ",b,db,dots=FALSE)
      call show_(stdout,"_cell_angle_gamma             ",c,dc,dots=FALSE)
      v = self%volume
      dv = self%volume_error
      call show_(stdout,"_cell_volume                  ",v,dv,dots=FALSE)
      call flush_(stdout)
      call show_(stdout,"_cell_formula_units_Z              ",to_str_(Z,"f11.6"),dots=FALSE)
      if (cart) then
         call flush_(stdout)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_11",self%direct_mx(1,1),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_21",self%direct_mx(2,1),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_31",self%direct_mx(3,1),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_12",self%direct_mx(1,2),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_22",self%direct_mx(2,2),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_32",self%direct_mx(3,2),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_13",self%direct_mx(1,3),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_23",self%direct_mx(2,3),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_matrix_33",self%direct_mx(3,3),dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_vector_1 ",ZERO,dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_vector_2 ",ZERO,dots=FALSE)
         call show_(stdout,"_atom_sites_Cartn_tran_vector_3 ",ZERO,dots=FALSE)
      end if
      dohar = FALSE
      if (present(har)) dohar = har
      if (dohar) then
         call flush_(stdout)
         call show_(stdout,"_atom_sites_solution_hydrogens     ","HAR",dots=FALSE)
      end if

      ! Put back defaults
      call unsave_style_(stdout)

      ! Convert back
      call set_units_to_(self,"au")
   end subroutine

end module
