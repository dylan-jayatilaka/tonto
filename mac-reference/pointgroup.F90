!===================================================================
!
!  POINTGROUP : for deriving and manipulating pointgroup objects
!
!  Input required is the Schonflies symbol. The data generated are
!  The 3x3 rotation matrices, all the irreducible representations,
!  The character table, the irreducible representation lables,
!  the group multiplication table, and the list of inverse operators
!
! Copyright (C) 1994-1997, Anthony J. Russell
! Copyright (C) 1998, Dylan Jayatilaka
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
module POINTGROUP_MODULE

#  include "macros"

   include "pointgroup.use"

   implicit none

   include "pointgroup.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(POINTGROUP_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"POINTGROUP:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,symbol,use_spherical_basis)
   ! Create a pointgroup object, optionally with Schoenflies "symbol"
      type(POINTGROUP_TYPE), allocatable, OUT :: self
      STR(len=*), IN :: symbol
      BIN, optional, IN :: use_spherical_basis
      call create_(self)
      call set_symbol_(self,symbol)
      if (present(use_spherical_basis)) self%use_spherical_basis = use_spherical_basis
      call update_(self)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(POINTGROUP_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"POINTGROUP:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of the object
      type(POINTGROUP_TYPE), INOUT :: self
      call destroy_(self%table)
      call destroy_(self%mx)
      call destroy_(self%ptr)
      call destroy_(self%dtr)
      call destroy_(self%ftr)
      call destroy_(self%gtr)
      call destroy_(self%inverse)
      call destroy_(self%irrep)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set the default pointgroup, "C1"
      type(POINTGROUP_TYPE), INOUT :: self
      call set_symbol_(self,"C1")
      self%use_spherical_basis = FALSE
      call update_(self)
   end subroutine

   pure subroutine set_symbol(self,symbol)
   ! Set the pointgroup Schonflies symbol
      type(POINTGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: symbol
      self%symbol = symbol
      self%ID_symbol = symbol
      call to_lower_case_(self%ID_symbol)
   end subroutine


!  =====
!  Input
!  =====

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(POINTGROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","POINTGROUP:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"POINTGROUP:read_keywords ... unexpected end of file while reading list keywords")
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
      type(POINTGROUP_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                  ")  ! exit case
      case ("put                "); call put_(self)
      case ("schoenflies_symbol="); call read_symbol_(self)
      case ("schonflies_symbol= "); call read_symbol_(self)
      case ("symbol=            "); call read_symbol_(self)
      case default; allocate(tonto%known_keywords(5))
      tonto%known_keywords(1) = "}                  "
      tonto%known_keywords(2) = "put                "
      tonto%known_keywords(3) = "schoenflies_symbol="
      tonto%known_keywords(4) = "schonflies_symbol= "
      tonto%known_keywords(5) = "symbol=            "
      call unknown_(tonto,word,"POINTGROUP:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_symbol(self)
   ! Read the pointgroup Schonflies symbol
      type(POINTGROUP_TYPE), INOUT :: self
      call read_(stdin,self%symbol)
      self%ID_symbol = self%symbol
      select case (self%symbol)
      case ("c1   ","cs   ","ci   ")
      case ("c2   ","c3   ","c4   ","c5   ","c6   ","c7   ","c8   ","c9   ","c10  ","c11  ","c12  ")
      case ("s2   ","s4   ","s6   ","s8   ","s10  ","s12  ")
      case ("c2h  ","c3h  ","c4h  ","c5h  ","c6h  ","c7h  ","c8h  ","c9h  ","c10h ","c11h ","c12h ")
      case ("c2v  ","c3v  ","c4v  ","c5v  ","c6v  ","c7v  ","c8v  ","c9v  ","c10v ","c11v ","c12v ")
      case ("d2   ","d3   ","d4   ","d5   ","d6   ","d7   ","d8   ","d9   ","d10  ","d11  ","d12  ")
      case ("d2h  ","d3h  ","d4h  ","d5h  ","d6h  ","d7h  ","d8h  ","d9h  ","d10h ","d11h ","d12h ")
      case ("d2d  ","d3d  ","d4d  ","d5d  ","d6d  ","d7d  ","d8d  ","d9d  ","d10d ","d11d ","d12d ")
      case ("cinfv","dinfh","t    ","th   ","td   ","o    ","oh   ","i    ","ih   ")
      case default; allocate(tonto%known_keywords(84))
      tonto%known_keywords(1) = "c1   "
      tonto%known_keywords(2) = "cs   "
      tonto%known_keywords(3) = "ci   "
      tonto%known_keywords(4) = "c2   "
      tonto%known_keywords(5) = "c3   "
      tonto%known_keywords(6) = "c4   "
      tonto%known_keywords(7) = "c5   "
      tonto%known_keywords(8) = "c6   "
      tonto%known_keywords(9) = "c7   "
      tonto%known_keywords(10) = "c8   "
      tonto%known_keywords(11) = "c9   "
      tonto%known_keywords(12) = "c10  "
      tonto%known_keywords(13) = "c11  "
      tonto%known_keywords(14) = "c12  "
      tonto%known_keywords(15) = "s2   "
      tonto%known_keywords(16) = "s4   "
      tonto%known_keywords(17) = "s6   "
      tonto%known_keywords(18) = "s8   "
      tonto%known_keywords(19) = "s10  "
      tonto%known_keywords(20) = "s12  "
      tonto%known_keywords(21) = "c2h  "
      tonto%known_keywords(22) = "c3h  "
      tonto%known_keywords(23) = "c4h  "
      tonto%known_keywords(24) = "c5h  "
      tonto%known_keywords(25) = "c6h  "
      tonto%known_keywords(26) = "c7h  "
      tonto%known_keywords(27) = "c8h  "
      tonto%known_keywords(28) = "c9h  "
      tonto%known_keywords(29) = "c10h "
      tonto%known_keywords(30) = "c11h "
      tonto%known_keywords(31) = "c12h "
      tonto%known_keywords(32) = "c2v  "
      tonto%known_keywords(33) = "c3v  "
      tonto%known_keywords(34) = "c4v  "
      tonto%known_keywords(35) = "c5v  "
      tonto%known_keywords(36) = "c6v  "
      tonto%known_keywords(37) = "c7v  "
      tonto%known_keywords(38) = "c8v  "
      tonto%known_keywords(39) = "c9v  "
      tonto%known_keywords(40) = "c10v "
      tonto%known_keywords(41) = "c11v "
      tonto%known_keywords(42) = "c12v "
      tonto%known_keywords(43) = "d2   "
      tonto%known_keywords(44) = "d3   "
      tonto%known_keywords(45) = "d4   "
      tonto%known_keywords(46) = "d5   "
      tonto%known_keywords(47) = "d6   "
      tonto%known_keywords(48) = "d7   "
      tonto%known_keywords(49) = "d8   "
      tonto%known_keywords(50) = "d9   "
      tonto%known_keywords(51) = "d10  "
      tonto%known_keywords(52) = "d11  "
      tonto%known_keywords(53) = "d12  "
      tonto%known_keywords(54) = "d2h  "
      tonto%known_keywords(55) = "d3h  "
      tonto%known_keywords(56) = "d4h  "
      tonto%known_keywords(57) = "d5h  "
      tonto%known_keywords(58) = "d6h  "
      tonto%known_keywords(59) = "d7h  "
      tonto%known_keywords(60) = "d8h  "
      tonto%known_keywords(61) = "d9h  "
      tonto%known_keywords(62) = "d10h "
      tonto%known_keywords(63) = "d11h "
      tonto%known_keywords(64) = "d12h "
      tonto%known_keywords(65) = "d2d  "
      tonto%known_keywords(66) = "d3d  "
      tonto%known_keywords(67) = "d4d  "
      tonto%known_keywords(68) = "d5d  "
      tonto%known_keywords(69) = "d6d  "
      tonto%known_keywords(70) = "d7d  "
      tonto%known_keywords(71) = "d8d  "
      tonto%known_keywords(72) = "d9d  "
      tonto%known_keywords(73) = "d10d "
      tonto%known_keywords(74) = "d11d "
      tonto%known_keywords(75) = "d12d "
      tonto%known_keywords(76) = "cinfv"
      tonto%known_keywords(77) = "dinfh"
      tonto%known_keywords(78) = "t    "
      tonto%known_keywords(79) = "th   "
      tonto%known_keywords(80) = "td   "
      tonto%known_keywords(81) = "o    "
      tonto%known_keywords(82) = "oh   "
      tonto%known_keywords(83) = "i    "
      tonto%known_keywords(84) = "ih   "
      call unknown_(tonto,self%symbol,"POINTGROUP:read_symbol")
      deallocate(tonto%known_keywords)
      end select
      call update_(self)
   end subroutine


!  =======
!  Analyze
!  =======

   PURE subroutine update(self)
   ! Analyze the pointgroup
      type(POINTGROUP_TYPE), INOUT :: self
      call analyze_symbol_(self)
      call make_rep_matrices_(self)
      call make_xyz_matrices_(self)
      call make_inverse_(self)
      call make_table_(self)
      call make_irrep_matrices_(self)
      call make_irrep_labels_(self)
      call make_character_table_(self)
   end subroutine

   PURE subroutine analyze_symbol(self)
   ! Analyze the pointgroup symbol
      type(POINTGROUP_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: axis
      INT :: n
      BIN :: temp_bin
      select case (self%ID_symbol)

      case ("c1")
         self%ID_number = 1
         self%ID_symbol = "c1 "

      case ("cs")
         self%ID_number = 2
         self%ID_symbol = "cs "

      case ("ci")
         self%ID_number = 3
         self%ID_symbol = "ci "

      case ("c2 ","c3 ","c4 ","c5 ","c6 ","c7 ","c8 ","c9 ","c10 ","c11 ","c12 ")
         self%ID_number = 4
         self%ID_symbol = "cn "

      case ("s2 ","s4 ","s6 ","s8 ","s10","s12")
         self%ID_number = 5
         self%ID_symbol = "s2n"

      case ("c2h","c3h","c4h","c5h","c6h","c7h","c8h","c9h","c10h","c11h","c12h")
         self%ID_number = 6
         self%ID_symbol = "cnh"

      case ("c2v","c3v","c4v","c5v","c6v","c7v","c8v","c9v","c10v","c11v","c12v")
         self%ID_number = 7
         self%ID_symbol = "cnv"

      case ("d2 ","d3 ","d4 ","d5 ","d6 ","d7 ","d8 ","d9 ","d10 ","d11 ","d12 ")
         self%ID_number = 8
         self%ID_symbol = "dn "

      case ("d2h","d3h","d4h","d5h","d6h","d7h","d8h","d9h","d10h","d11h","d12h")
         self%ID_number = 9
         self%ID_symbol = "dnh"

      case ("d2d","d3d","d4d","d5d","d6d","d7d","d8d","d9d","d10d","d11d","d12d")
         self%ID_number = 10
         self%ID_symbol = "dnd"

      case ("cinfv")
         self%ID_number = 11
         self%ID_symbol = "cinf"

      case ("dinfh")
         self%ID_number = 12
         self%ID_symbol = "dinf"

      case ("t ")
         self%ID_number = 13
         self%ID_symbol = "t  "

      case ("th")
         self%ID_number = 14
         self%ID_symbol = "th "

      case ("td")
         self%ID_number = 15
         self%ID_symbol = "td "

      case ("o ")
         self%ID_number = 16
         self%ID_symbol = "o  "

      case ("oh")
         self%ID_number = 17
         self%ID_symbol = "oh "

      case ("i ")
         self%ID_number = 18
         self%ID_symbol = "i  "

      case ("ih")
         self%ID_number = 19
         self%ID_symbol = "ih "

      case default
         self%ID_number = 20
      end select
      temp_bin = any(self%ID_symbol==["i   ","ih  ","cinf","dinf"])
      ENSURE(NOT temp_bin,"POINTGROUP:analyze_symbol ... Group"//trim(self%symbol)//"not implemented")
      ENSURE(NOT self%ID_number==20,"POINTGROUP:analyze_symbol ... Unknown group symbol: "//trim(self%symbol))
      select case (self%ID_number)

      case (1:3)
         self%axis_order = 1

      case (4:10)
         axis = self%symbol(2:)
         n = verify(axis,"123456789")-1
         axis = axis(1:n)
         self%axis_order = to_int_(axis)
         if (self%ID_number==5) self%axis_order = self%axis_order/2

      case (13:15)
         self%axis_order = 2

      case (16:17)
         self%axis_order = 4
      end select
      select case (self%ID_symbol)
      case ("c1 "); self%order = 1
      case ("cs "); self%order = 2
      case ("ci "); self%order = 2
      case ("cn "); self%order = self%axis_order
      case ("s2n"); self%order = 2*self%axis_order
      case ("cnh"); self%order = 2*self%axis_order
      case ("cnv"); self%order = 2*self%axis_order
      case ("dn "); self%order = 2*self%axis_order
      case ("dnh"); self%order = 4*self%axis_order
      case ("dnd"); self%order = 4*self%axis_order
      case ("t  "); self%order = 12
      case ("th "); self%order = 24
      case ("td "); self%order = 24
      case ("o  "); self%order = 24
      case ("oh "); self%order = 48
      end select
   end subroutine

   PURE subroutine make_rep_matrices(self)
   ! Make the 3x3 point group representation matrices
      type(POINTGROUP_TYPE), INOUT :: self
      INT :: n
      n = self%axis_order
      call create_(self%mx,3,3,self%order)
      call to_unit_mx_(self%mx(:,:,1))
      select case (self%ID_symbol)

      case ("c1")  ! C1

      case ("cs")
         ! Cs  = C1 x sigma-h
         call times_sigma_h_(self,1)

      case ("ci")
         ! Ci  = C1 x Ci
         call times_ci_(self,1)

      case ("cn")
         ! Cn
         call make_cn_matrices_(self)

      case ("s2n")
         ! Sn
         call make_cn_matrices_(self)
         call make_sn_matrices_(self,n)

      case ("cnh")
         ! Cnh = Cn x sigma-h(xy)
         call make_cn_matrices_(self)
         call times_sigma_h_(self,n)

      case ("cnv")
         ! Cnv = Cn x sigma-v(xz)
         call make_cn_matrices_(self)
         call times_sigma_v_(self,n)

      case ("dn")
         ! Dn  = Cn  x C2(x)
         call make_cn_matrices_(self)
         call times_c2x_(self,n)

      case ("dnh")
         ! Dnh = Cn x sigma-h(xy) x C2(x)
         call make_cn_matrices_(self)
         call times_sigma_h_(self,n)
         n = 2*n
         call times_c2x_(self,n)

      case ("dnd")
         ! Dnd = Cn x C2(x) x sigma-d
         call make_cn_matrices_(self)
         call times_c2x_(self,n)
         n = 2*n
         call times_sigma_d_(self,n)

      case ("t")
         ! T
         call make_t_matrices_(self)

      case ("th")
         ! Th  = T x Ci
         call make_t_matrices_(self)
         call times_ci_(self,12)

      case ("td")
         ! Td  = T x sigma(x=y)
         call make_t_matrices_(self)
         self%mx(:,1,13:24) = self%mx(:,2,1:12)
         self%mx(:,2,13:24) = self%mx(:,1,1:12)
         self%mx(:,3,13:24) = self%mx(:,3,1:12)

      case ("o")
         ! O  = T x C4(z)
         call make_t_matrices_(self)
         call times_c4z_(self,12)

      case ("oh")
         ! Oh = T x C4(z) x Ci
         call make_t_matrices_(self)
         call times_c4z_(self,12)
         call times_ci_(self,24)
      end select
   end subroutine

   PURE subroutine times_ci(self,n)
   ! Direct product with inversion operator
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      self%mx(1,:,n+1:n+n) = -self%mx(1,:,1:n)
      self%mx(2,:,n+1:n+n) = -self%mx(2,:,1:n)
      self%mx(3,:,n+1:n+n) = -self%mx(3,:,1:n)
   end subroutine

   PURE subroutine times_sigma_h(self,n)
   ! Direct product with sigma-h, which is the xy plane
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      self%mx(1,:,n+1:n+n) = self%mx(1,:,1:n)
      self%mx(2,:,n+1:n+n) = self%mx(2,:,1:n)
      self%mx(3,:,n+1:n+n) = -self%mx(3,:,1:n)
   end subroutine

   PURE subroutine times_sigma_v(self,n)
   ! Direct product with sigma-v, which is the xz plane
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      self%mx(1,:,n+1:n+n) = self%mx(1,:,1:n)
      self%mx(2,:,n+1:n+n) = -self%mx(2,:,1:n)
      self%mx(3,:,n+1:n+n) = self%mx(3,:,1:n)
   end subroutine

   PURE subroutine times_sigma_d(self,n)
   ! Direct product with sigma-d. Equation of sigma-d is sin(alph/4)*x-cos(alph/4)*y=0
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      REAL :: beta,cosb,sinb
      beta = PI/self%axis_order
      cosb = cos(beta)
      sinb = sin(beta)
      self%mx(1,:,n+1:n+n) = cosb*self%mx(1,:,1:n)+sinb*self%mx(2,:,1:n)
      self%mx(2,:,n+1:n+n) = sinb*self%mx(1,:,1:n)-cosb*self%mx(2,:,1:n)
      self%mx(3,:,n+1:n+n) = self%mx(3,:,1:n)
   end subroutine

   PURE subroutine times_c2x(self,n)
   ! Direct product with C2x, for Dn groups
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      self%mx(1,:,n+1:n+n) = self%mx(1,:,1:n)
      self%mx(2,:,n+1:n+n) = -self%mx(2,:,1:n)
      self%mx(3,:,n+1:n+n) = -self%mx(3,:,1:n)
   end subroutine

   PURE subroutine times_c4z(self,n)
   ! Direct product with the C4z matrix
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      self%mx(:,1,n+1:n+n) = -self%mx(:,2,1:n)
      self%mx(:,2,n+1:n+n) = self%mx(:,1,1:n)
      self%mx(:,3,n+1:n+n) = self%mx(:,3,1:n)
   end subroutine

   PURE subroutine make_cn_matrices(self)
   ! Make the Cn rotation matrices
      type(POINTGROUP_TYPE), INOUT :: self
      REAL :: alph,alpha,cosa,sina
      INT :: n
      alph = TWO*PI/self%axis_order
      alpha = ZERO
      do n = 2,self%axis_order
         alpha = alpha+alph
         cosa = cos(alpha)
         sina = sin(alpha)
         self%mx(:,:,n) = reshape([cosa,-sina,ZERO,sina,cosa,ZERO,ZERO,ZERO,ONE],[3,3])
      end do
   end subroutine

   PURE subroutine make_sn_matrices(self,n)
   ! Make the Sn rotation matrices
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: n
      REAL :: beta,cosb,sinb
      beta = PI/self%axis_order
      cosb = cos(beta)
      sinb = sin(beta)
      self%mx(1,:,n+1:n+n) = cosb*self%mx(1,:,1:n)+sinb*self%mx(2,:,1:n)
      self%mx(2,:,n+1:n+n) = -sinb*self%mx(1,:,1:n)+cosb*self%mx(2,:,1:n)
      self%mx(3,:,n+1:n+n) = -self%mx(3,:,1:n)
   end subroutine

   PURE subroutine make_t_matrices(self)
   ! Make the Tetrahedral group 3x3 representation matrices
      type(POINTGROUP_TYPE), INOUT :: self
      INT :: n
      n = 1
      self%mx(:,:,2) = reshape([ONE,ZERO,ZERO,ZERO,-ONE,ZERO,ZERO,ZERO,-ONE],[3,3])  ! C2x
      self%mx(:,:,3) = reshape([-ONE,ZERO,ZERO,ZERO,ONE,ZERO,ZERO,ZERO,-ONE],[3,3])  ! C2y
      self%mx(:,:,4) = reshape([-ONE,ZERO,ZERO,ZERO,-ONE,ZERO,ZERO,ZERO,ONE],[3,3])  ! C2z
      do n = 5,12
         self%mx(:,1,n) = self%mx(:,3,n-4)
         self%mx(:,2,n) = self%mx(:,1,n-4)
         self%mx(:,3,n) = self%mx(:,2,n-4)
      end do
   end subroutine

   PURE subroutine make_xyz_matrices(self)
   ! Make the representation matrices for xyz products found in
   ! gaussian shells, i.e. work out matrix R, where the shell row vector
   ! p'(r) = p(S^{-1}r) = p(r)R, and S is the symop.
   ! WARNING: this routine is tied to an explicit ordering of the cartesian
   ! gaussian basis functions in a shell.
      type(POINTGROUP_TYPE), INOUT :: self
      self%ptr = self%mx
      self%dtr = d_xyz_rep_matrices_for_(self%mx,self%use_spherical_basis)
      self%ftr = f_xyz_rep_matrices_for_(self%mx,self%use_spherical_basis)
      self%gtr = g_xyz_rep_matrices_for_(self%mx,self%use_spherical_basis)
   end subroutine

   PURE subroutine make_inverse(self)
   ! Make the inverse operator list
      type(POINTGROUP_TYPE), INOUT :: self
      INT :: m,n
      REAL :: test
      call create_(self%inverse,self%order)
      do m = 1,self%order
         do n = 1,self%order
            test = trace_product_with_(self%mx(:,:,m),self%mx(:,:,n))-THREE
            if (abs(test)>TOL(3)) cycle
            self%inverse(m) = n
            exit
         end do
      end do
   end subroutine

   PURE subroutine make_table(self)
   ! Make the group multiplication table
      type(POINTGROUP_TYPE), INOUT :: self
      INT :: l,m,n
      BIN :: same
      MAT(REAL,3,3) :: prod
      call create_(self%table,self%order,self%order)
      do l = 1,self%order
         do m = 1,self%order
            call to_product_of_(prod,self%mx(:,:,l),self%mx(:,:,m))
            do n = 1,self%order
               same = equals_(self%mx(:,:,n),prod)
               if (same) exit
            end do
            self%table(l,m) = n
         end do
      end do
   end subroutine

   PURE subroutine make_irrep_matrices(self)
   ! Make the Irreps for the group
      type(POINTGROUP_TYPE), INOUT :: self
      if (self%ID_number<=10) call make_C_type_irreps_(self)
      if (self%ID_number>=13) call make_T_type_irreps_(self)
   end subroutine

   PURE subroutine make_C_type_irreps(self)
   ! Make irrep matrices for the groups C1, Cs, Ci, Cn, Cnh, Cnv, S2n,
   ! Dn, Dnh, Dnd
      type(POINTGROUP_TYPE), INOUT :: self
      BIN :: odd_axis
      INT :: e,i,n,n_max,dim
      REAL :: theta
      MAT3(REAL,2,2,14), target :: gen1,gen2,gen3  ! generator matrices
      MAT(REAL,:,:), PTR :: g1,g2,g3
      odd_axis = NOT self%axis_order==2*(self%axis_order/2)
      select case (self%ID_symbol)

      case ("c1")
         self%has_complex_irreps = FALSE
         self%n_irrep = 1
         call create_(self%irrep,self%n_irrep)
         call set_dimension_(self%irrep(1),1)
         e = 2
         self%n_gen = 1
         gen1(1,1,1) = ONE

      case ("cs","ci")
         self%has_complex_irreps = FALSE
         self%n_irrep = 2
         call create_(self%irrep,self%n_irrep)
         call set_dimensions_(self%irrep(1:2),1)
         e = 3
         self%n_gen = 1
         gen1(1,1,1) = ONE
         gen1(1,1,2) = -ONE

      case ("cn")
         if (self%axis_order>2) self%has_complex_irreps = TRUE
         self%n_gen = 1
         gen1(1,1,1) = ONE
         if (odd_axis) then
            self%n_irrep = (self%axis_order+1)/2
            call create_(self%irrep,self%n_irrep)
            call set_dimension_(self%irrep(1),1)
            e = 2
         else
            self%n_irrep = self%axis_order/2+1
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:2),1)
            e = 3
            gen1(1,1,2) = -ONE
         end if
         call make_cn_gen_(self,e,gen1,self%axis_order)

      case ("s2n")
         if (self%axis_order>=2) self%has_complex_irreps = TRUE
         self%n_irrep = self%axis_order+1
         call create_(self%irrep,self%n_irrep)
         call set_dimensions_(self%irrep(1:2),1)
         e = 3
         self%n_gen = 2
         gen1(1,1,1) = ONE
         gen1(1,1,2) = ONE
         gen2(1,1,1) = ONE
         gen2(1,1,2) = -ONE
         call make_cn_gen_(self,e,gen2,2*self%axis_order)
         call make_cn_gen_(self,e,gen1,self%axis_order)

      case ("cnh")
         if (self%axis_order>2) self%has_complex_irreps = TRUE
         self%n_gen = 2
         gen1(1,1,1) = ONE
         gen1(1,1,2) = ONE
         gen2(1,1,1) = ONE
         gen2(1,1,2) = -ONE
         if (odd_axis) then
            self%n_irrep = self%axis_order+1
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:2),1)
            e = 3
         else
            self%n_irrep = self%axis_order+2
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:4),1)
            e = 5
            gen1(1,1,3) = -ONE
            gen1(1,1,4) = -ONE
            gen2(1,1,3) = ONE
            gen2(1,1,4) = -ONE
         end if
         call make_cnh_gen_(self,e,gen1,gen2)

      case ("cnv","dn")
         self%has_complex_irreps = FALSE
         self%n_gen = 2
         gen1(1,1,1) = ONE
         gen1(1,1,2) = ONE
         gen2(1,1,1) = ONE
         gen2(1,1,2) = -ONE
         if (odd_axis) then
            self%n_irrep = (self%axis_order+3)/2
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:2),1)
            e = 3
         else
            self%n_irrep = self%axis_order/2+3
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:4),1)
            e = 5
            gen1(1,1,3) = -ONE
            gen1(1,1,4) = -ONE
            gen2(1,1,3) = ONE
            gen2(1,1,4) = -ONE
         end if
         call make_sigma_x_gen_(self,e,gen2)
         call make_cn_gen_(self,e,gen1,self%axis_order)

      case ("dnh")
         self%has_complex_irreps = FALSE
         self%n_gen = 3
         gen1(1,1,1) = ONE
         gen1(1,1,2) = ONE
         gen2(1,1,1) = ONE
         gen2(1,1,2) = -ONE
         gen2(1,1,3) = ONE
         gen2(1,1,4) = -ONE
         gen3(1,1,1) = ONE
         gen3(1,1,2) = ONE
         if (odd_axis) then
            self%n_irrep = self%axis_order+3
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:4),1)
            e = 5
            gen3(1,1,3) = -ONE
            gen3(1,1,4) = -ONE
            gen1(1,1,3) = ONE  ! Warning, possible error, Changed from gen3
                                 ! to gen1; the first two could be gen1
            gen1(1,1,4) = ONE  ! Warning, possible error, Changed from gen3
                                 ! to gen1
         else
            self%n_irrep = self%axis_order+6
            call create_(self%irrep,self%n_irrep)
            call set_dimensions_(self%irrep(1:8),1)
            e = 9
            gen1(1,1,3) = -ONE
            gen1(1,1,4) = -ONE
            gen1(1,1,5) = ONE
            gen1(1,1,6) = ONE
            gen1(1,1,7) = -ONE
            gen1(1,1,8) = -ONE
            gen2(1,1,5) = ONE
            gen2(1,1,6) = -ONE
            gen2(1,1,7) = ONE
            gen2(1,1,8) = -ONE
            gen3(1,1,3) = ONE
            gen3(1,1,4) = ONE
            gen3(1,1,5) = -ONE
            gen3(1,1,6) = -ONE
            gen3(1,1,7) = -ONE
            gen3(1,1,8) = -ONE
         end if
         do i = e,self%n_irrep
            gen3(1,1,i) = ONE
            gen3(2,2,i) = -ONE
            gen3(2,1,i) = ZERO  ! Transposed
            gen3(1,2,i) = ZERO
         end do
         call make_cnh_gen_(self,e,gen1,gen2)

      case ("dnd")
         self%has_complex_irreps = FALSE
         self%n_irrep = self%axis_order+3
         call create_(self%irrep,self%n_irrep)
         call set_dimensions_(self%irrep(1:4),1)
         e = 5
         self%n_gen = 3
         gen1(1,1,1) = ONE
         gen1(1,1,2) = ONE
         gen1(1,1,3) = ONE
         gen1(1,1,4) = ONE
         gen2(1,1,1) = ONE
         gen2(1,1,2) = -ONE
         gen2(1,1,3) = ONE
         gen2(1,1,4) = -ONE
         gen3(1,1,1) = ONE
         gen3(1,1,2) = ONE
         gen3(1,1,3) = -ONE
         gen3(1,1,4) = -ONE
         do i = e,self%n_irrep
            theta = (i-e+1)*PI/self%axis_order
            gen3(1,1,i) = cos(theta)
            gen3(2,2,i) = -cos(theta)
            gen3(2,1,i) = sin(theta)  ! Transposed
            gen3(1,2,i) = sin(theta)
         end do
         call make_sigma_x_gen_(self,e,gen2)
         call make_cn_gen_(self,e,gen1,self%axis_order)
      end select
      do i = 1,self%n_irrep
         dim = self%irrep(i)%dim
         call create_(self%irrep(i)%mx,dim,dim,self%order)
         g1 => gen1(1:dim,1:dim,i)
         g2 => gen2(1:dim,1:dim,i)
         g3 => gen3(1:dim,1:dim,i)
         call to_unit_mx_(self%irrep(i)%mx(:,:,1))
         n_max = self%axis_order
         if (self%ID_symbol=="c1") cycle
         if (self%ID_symbol=="cs") n_max = 2
         if (self%ID_symbol=="ci") n_max = 2
         do n = 2,n_max
            call to_product_of_(self%irrep(i)%mx(:,:,n),g1,self%irrep(i)%mx(:,:,n-1))
         end do
         if (self%n_gen==1) cycle
         do n = 1,n_max
            call to_product_of_(self%irrep(i)%mx(:,:,n_max+n),g2,self%irrep(i)%mx(:,:,n))
         end do
         if (self%n_gen==2) cycle
         n_max = 2*n_max
         do n = 1,n_max
            call to_product_of_(self%irrep(i)%mx(:,:,n_max+n),g3,self%irrep(i)%mx(:,:,n))
         end do
      end do
   end subroutine

   PURE subroutine make_cnh_gen(self,e,gen1,gen2)
   ! Make the Cnh generator matrices for the irreps
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: e
      MAT3(REAL,2,2,14), OUT :: gen1,gen2
      INT :: i,m
      REAL :: sign,theta
      do i = e,self%n_irrep
         call set_dimension_(self%irrep(i),2)
         m = (i-e+2)/2
         theta = m*TWO*PI/self%axis_order
         gen1(1,1,i) = cos(theta)
         gen1(2,2,i) = cos(theta)
         gen1(2,1,i) = -sin(theta)  ! Transposed
         gen1(1,2,i) = sin(theta)
      end do
      sign = ONE
      do i = e,self%n_irrep
         sign = -sign
         gen2(1,1,i) = sign
         gen2(2,2,i) = sign
         gen2(2,1,i) = ZERO  ! Transposed
         gen2(1,2,i) = ZERO
      end do
   end subroutine

   PURE subroutine make_sigma_x_gen(self,e,gen2)
   ! Make the sigma-x generator matrices for the irreps
      type(POINTGROUP_TYPE), IN :: self
      INT, IN :: e
      MAT3(REAL,2,2,14), OUT :: gen2
      INT :: i
      do i = e,self%n_irrep
         gen2(1,1,i) = ONE
         gen2(2,2,i) = -ONE
         gen2(2,1,i) = ZERO  ! Transposed
         gen2(1,2,i) = ZERO
      end do
   end subroutine

   PURE subroutine make_cn_gen(self,e,gen1,axis_order)
   ! Make the Cn generator matrices for the irreps
      type(POINTGROUP_TYPE), INOUT :: self
      INT, IN :: e,axis_order
      MAT3(REAL,2,2,14), OUT :: gen1
      INT :: i
      REAL :: theta
      do i = e,self%n_irrep
         call set_dimension_(self%irrep(i),2)
         theta = (i-e+1)*TWO*PI/axis_order
         gen1(1,1,i) = cos(theta)
         gen1(2,2,i) = cos(theta)
         gen1(2,1,i) = -sin(theta)  ! Transposed
         gen1(1,2,i) = sin(theta)
      end do
   end subroutine

   PURE subroutine make_T_type_irreps(self)
   ! Make irrep matrices for the groups T, Th, Td, O, and Oh
   ! Must have already generated the 3x3 rep matrices
      type(POINTGROUP_TYPE), INOUT :: self
      MAT(INT,10,5) :: irrep_dimension
      VEC(INT,5) :: n_irrep
      VEC(INT,5) :: ab_irrep
      VEC(INT,5) :: ea_irrep
      VEC(INT,5) :: eb_irrep
      VEC(INT,5) :: fa_irrep
      VEC(INT,5) :: fb_irrep
      INT :: i,n,id,dim
      INT :: ab,ea,eb,fa,fb
      REAL :: pt5,r32
      irrep_dimension = reshape([1,2,3,0,0,0,0,0,0,0,1,2,3,1,2,3,0,0,0,0,1,1,2,3,3,0,0,0,0,0,1,1,2,3,3,0,0,0,0,0,1,1,2,3,3,1,1,2,3,3],[10,5])
      n_irrep = [3,6,5,5,10]  ! No of irreps
      ab_irrep = [0,4,2,2,2]  ! A irreps
      ea_irrep = [2,2,3,3,3]  ! E irreps
      eb_irrep = [0,5,0,0,8]  ! E irreps
      fa_irrep = [3,6,5,4,9]  ! F irreps
      fb_irrep = [0,0,4,5,10]  ! F irreps
      self%has_complex_irreps = FALSE
      if (self%ID_symbol=="t") self%has_complex_irreps = TRUE
      if (self%ID_symbol=="td") self%has_complex_irreps = TRUE
      id = self%ID_number-12
      self%n_irrep = n_irrep(id)
      call create_(self%irrep,self%n_irrep)
      call set_dimensions_(self%irrep,irrep_dimension(1:self%n_irrep,id))
      do i = 1,self%n_irrep
         dim = self%irrep(i)%dim
         call create_(self%irrep(i)%mx,dim,dim,self%order)
      end do
      self%irrep(1)%mx(1,1,:) = ONE
      pt5 = HALF
      r32 = sqrt(THREE)/TWO
      ea = ea_irrep(id)
      self%irrep(ea)%mx(1,1,1:4) = ONE
      self%irrep(ea)%mx(2,1,1:4) = ZERO
      self%irrep(ea)%mx(1,2,1:4) = ZERO
      self%irrep(ea)%mx(2,2,1:4) = ONE
      self%irrep(ea)%mx(1,1,5:8) = -pt5
      self%irrep(ea)%mx(2,1,5:8) = -r32
      self%irrep(ea)%mx(1,2,5:8) = r32
      self%irrep(ea)%mx(2,2,5:8) = -pt5
      self%irrep(ea)%mx(1,1,9:12) = -pt5
      self%irrep(ea)%mx(2,1,9:12) = r32
      self%irrep(ea)%mx(1,2,9:12) = -r32
      self%irrep(ea)%mx(2,2,9:12) = -pt5
      fa = fa_irrep(id)
      self%irrep(fa)%mx(:,:,:) = self%mx(:,:,:)
      if (self%ID_symbol=="t") return
      ab = ab_irrep(id)
      self%irrep(ab)%mx(1,1,1:12) = ONE
      self%irrep(ab)%mx(1,1,13:24) = -ONE
      if (self%ID_symbol=="oh") then
         self%irrep(2)%mx(1,1,25:48) = self%irrep(2)%mx(1,1,1:24)
         self%irrep(6)%mx(1,1,1:24) = ONE
         self%irrep(6)%mx(1,1,25:48) = -ONE
         self%irrep(7)%mx(1,1,1:24) = self%irrep(2)%mx(1,1,1:24)
         self%irrep(7)%mx(1,1,25:48) = -self%irrep(2)%mx(1,1,1:24)
      end if
      self%irrep(ea)%mx(:,:,13:24) = self%irrep(ea)%mx(:,:,1:12)
      n = self%order/2
      if (self%ID_symbol=="th" OR self%ID_symbol=="oh") then
         self%irrep(ea)%mx(:,:,n+1:n+n) = self%irrep(ea)%mx(:,:,1:n)
         eb = eb_irrep(id)
         self%irrep(eb)%mx(:,:,1:n) = self%irrep(ea)%mx(:,:,1:n)
         self%irrep(eb)%mx(:,:,n+1:n+n) = -self%irrep(ea)%mx(:,:,n+1:n+n)
      end if
      if (self%ID_symbol=="th") then
         self%irrep(3)%mx(:,:,1:12) = self%irrep(6)%mx(:,:,1:12)
         self%irrep(3)%mx(:,:,13:24) = self%irrep(6)%mx(:,:,1:12)
      else
         fb = fb_irrep(id)
         fa = fa_irrep(id)
         self%irrep(fb)%mx(:,:,1:12) = self%irrep(fa)%mx(:,:,1:12)
         self%irrep(fb)%mx(:,:,13:24) = -self%irrep(fa)%mx(:,:,13:24)
         if (self%ID_symbol=="oh") then
            self%irrep(10)%mx(:,:,25:48) = -self%irrep(10)%mx(:,:,1:24)
            self%irrep(4)%mx(:,:,1:24) = self%irrep(9)%mx(:,:,1:24)
            self%irrep(5)%mx(:,:,1:24) = self%irrep(10)%mx(:,:,1:24)
            self%irrep(4)%mx(:,:,25:48) = self%irrep(9)%mx(:,:,1:24)
            self%irrep(5)%mx(:,:,25:48) = self%irrep(10)%mx(:,:,1:24)
         end if
      end if
   end subroutine

   PURE subroutine make_character_table(self)
   ! MAke the character table
      type(POINTGROUP_TYPE), INOUT :: self
      INT :: n,i
      do i = 1,self%n_irrep
         call create_(self%irrep(i)%chi,self%order)
         do n = 1,self%order
            self%irrep(i)%chi(n) = trace_(self%irrep(i)%mx(:,:,n))
         end do
      end do
   end subroutine

   PURE subroutine make_irrep_labels(self)
   ! Make the names for the symmetry irreducible representations
   ! The irrep object must already be created and dimensioned correctly.
      type(POINTGROUP_TYPE), INOUT :: self

   ! warn_str :: STR(len=41)

   ! warn_str = "can't label irreps for groups larger than"
   ! WARN_IF(.ID_symbol=="cn"  AND .axis_order>9, warn_str // " C9")
   ! WARN_IF(.ID_symbol=="s2n" AND .axis_order>8, warn_str // " S8")
   ! WARN_IF(.ID_symbol=="cnh" AND .axis_order>6, warn_str // " C6h")
   ! WARN_IF(.ID_symbol=="cnv" AND .axis_order>6, warn_str // " C6v")
   ! WARN_IF(.ID_symbol=="dn"  AND .axis_order>6, warn_str // " D6")
   ! WARN_IF(.ID_symbol=="dnh" AND .axis_order>8, warn_str // " D8h")
   ! WARN_IF(.ID_symbol=="dnd" AND .axis_order>6, warn_str // " D6d")
      select case (self%ID_symbol)

      case ("c1")
         call set_labels_(self%irrep,["A   "])

      case ("cs")
         call set_labels_(self%irrep,["A'  ","A'' "])

      case ("ci")
         call set_labels_(self%irrep,["Ag  ","Au  "])

      case ("cn")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["A   "])
         case (2)
            call set_labels_(self%irrep,["A   ","B   "])
         case (3)
            call set_labels_(self%irrep,["A   ","E   "])
         case (4)
            call set_labels_(self%irrep,["A   ","B   ","E   "])
         case (5)
            call set_labels_(self%irrep,["A   ","E1  ","E2  "])
         case (6)
            call set_labels_(self%irrep,["A   ","B   ","E1  ","E2  "])
         case (7)
            call set_labels_(self%irrep,["A   ","E1  ","E2  ","E3  "])
         case (8)
            call set_labels_(self%irrep,["A   ","B   ","E1  ","E2  ","E3  "])
         case (9)
            call set_labels_(self%irrep,["A   ","E1  ","E2  ","E3  ","E4  "])
         end select

      case ("s2n")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["Ag  ","Au  "])
         case (2)
            call set_labels_(self%irrep,["A   ","B   ","E   "])
         case (3)
            call set_labels_(self%irrep,["Ag  ","Au  ","Eu  ","Eg  "])
         case (4)
            call set_labels_(self%irrep,["A   ","B   ","E1  ","E2  ","E3  "])
         end select

      case ("cnh")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["A'  ","A'' "])
         case (2)
            call set_labels_(self%irrep,["Ag  ","Au  ","Bu  ","Bg  "])
         case (3)
            call set_labels_(self%irrep,["A'  ","A'' ","E'' ","E'  "])
         case (4)
            call set_labels_(self%irrep,["Ag  ","Au  ","Bg  ","Bu  ","Eg  ","Eu  "])
         case (5)
            call set_labels_(self%irrep,["A'  ","A'' ","E1''","E1' ","E2''","E2' "])
         case (6)
            call set_labels_(self%irrep,["Ag  ","Au  ","Bu  ","Bg  ","E1g ","E1u ","E2u ","E2g "])
         end select

      case ("cnv")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["A'  ","A'' "])
         case (2)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  "])
         case (3)
            call set_labels_(self%irrep,["A1  ","A2  ","E   "])
         case (4)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  ","E   "])
         case (5)
            call set_labels_(self%irrep,["A1  ","A2  ","E1  ","E2  "])
         case (6)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  ","E1  ","E2  "])
         end select

      case ("dn")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["A1  ","B   "])
         case (2)
            call set_labels_(self%irrep,["A   ","B1  ","B3  ","B2  "])  ! Is this right?
         case (3)
            call set_labels_(self%irrep,["A1  ","A2  ","E   "])
         case (4)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  ","E   "])
         case (5)
            call set_labels_(self%irrep,["A1  ","A2  ","E1  ","E2  "])
         case (6)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  ","E1  ","E2  "])
         end select

      case ("dnh")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  "])
         case (2)
            call set_labels_(self%irrep,["Ag  ","Au  ","B3u ","B3g ","B1g ","B1u ","B2u ","B2g "])
         case (3)
            call set_labels_(self%irrep,["A1' ","A1''","A2' ","A2''","E'' ","E'  "])
         case (4)
            call set_labels_(self%irrep,["A1g ","A1u ","B1g ","B1u ","A2g ","A2u ","B2g ","B2u ","Eg  ","Eu  "])
         case (5)
            call set_labels_(self%irrep,["A1' ","A1''","A2' ","A2''","E1''","E1' ","E2''","E2' "])
         case (6)
            call set_labels_(self%irrep,["A1g ","A1u ","B1u ","B1g ","A2g ","A2u ","B2u ","B2g ","E1g ","E1u ","E2u ","E2g "])
         case (8)
            call set_labels_(self%irrep,["A1g ","A1u ","A2g ","A2u ","B1g ","B1u ","B2g ","B2u ","E1g ","E1u ","E2u ","E2g ","E3g ","E3u "])
         end select

      case ("dnd")
         select case (self%axis_order)
         case (1)
            call set_labels_(self%irrep,["Ag  ","Au  ","Bu  ","Bg  "])
         case (2)
            call set_labels_(self%irrep,["A1  ","B2  ","B1  ","A2  ","E   "])
         case (3)
            call set_labels_(self%irrep,["A1g ","A2u ","A1u ","A2g ","Eu  ","Eg  "])
         case (4)
            call set_labels_(self%irrep,["A1  ","A2  ","B1  ","B2  ","E1  ","E2  ","E3  "])
         case (5)
            call set_labels_(self%irrep,["A1g ","A1u ","A2u ","A2g ","E2u ","E1g ","E1u ","E2g "])
         case (6)
            call set_labels_(self%irrep,["A1  ","B2  ","B1  ","A2  ","E1  ","E2  ","E3  ","E4  ","E5  "])
         end select

      case ("t")
         call set_labels_(self%irrep,["A   ","E   ","F   "])

      case ("th")
         call set_labels_(self%irrep,["Ag  ","Eg  ","Fg  ","Au  ","Eu  ","Fu  "])
      case ("td")
         call set_labels_(self%irrep,["A1  ","A2  ","E   ","F1  ","F2  "])

      case ("o")
         call set_labels_(self%irrep,["A1  ","A2  ","E   ","F1  ","F2  "])

      case ("oh")
         call set_labels_(self%irrep,["A1g ","A2g ","Eg  ","F1g ","F2g ","A1u ","A2u ","Eu  ","F1u ","F2u "])
      case ("i")
         call set_labels_(self%irrep,["A   ","F1  ","F2  ","G   ","H   "])

      case ("ih")
         call set_labels_(self%irrep,["Ag  ","F1g ","F2g ","Gg  ","Hg  ","Au  ","F1u ","F2u ","Gu  ","Hu  "])
      end select
   end subroutine


!  =======
!  Inquiry
!  =======

   PURE function xyz_mx(self,n,l) result (res)
   ! Return the "n"-th representation matrix for xyz product found in
   ! a gaussian shell of angular momentum "l"
      type(POINTGROUP_TYPE), IN :: self
      INT, IN :: n,l
      MAT(REAL,:,:), allocatable :: res
   ENSURE(l>=0,"POINTGROUP:xyz_mx ... l must be non-negative")
   ENSURE(l<5,"POINTGROUP:xyz_mx ... l must be less than 5 (no h functions or higher)")
      select case (l)
      case (0); res = self%irrep(1)%mx(:,:,n)
      case (1); res = self%ptr(:,:,n)
      case (2); res = self%dtr(:,:,n)
      case (3); res = self%ftr(:,:,n)
      case (4); res = self%gtr(:,:,n)
      end select
   end function


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Display the pointgroup data to output file "out".
      type(POINTGROUP_TYPE), IN :: self
      INT :: i,n,dim,f,l
      INT :: block,n_fields,n_block
      call flush_(stdout)
      call text_(stdout,"POINTGROUP output:")
      call flush_(stdout)
      call show_(stdout,"Symbol                  =",self%symbol)
      call show_(stdout,"ID_symbol               =",self%ID_symbol)
      call show_(stdout,"ID_number               =",self%ID_number)
      call show_(stdout,"Principal axis order    =",self%axis_order)
      call show_(stdout,"Order                   =",self%order)
      call show_(stdout,"No. of Irreps           =",self%n_irrep)
      call show_(stdout,"No. of Irrep generators =",self%n_gen)
      call show_(stdout,"Has complex irreps?     =",self%has_complex_irreps)
      call flush_(stdout)
      call text_(stdout,"List of inverse group elements :")
      call flush_(stdout)
      call put_(stdout,self%inverse)
      call flush_(stdout)
      call text_(stdout,"Group multiplication table :")
      call flush_(stdout)
      call put_(stdout,self%table)
      call flush_(stdout)
      call text_(stdout,"Character Table :")
      n_fields = stdout%style%n_fields_per_line
      n_block = (self%order-1)/n_fields+1
      do block = 1,n_block
         f = 1+(block-1)*n_fields
         l = min(f+n_fields-1,self%order)
         call flush_(stdout)  ! Banner
         call dash_(stdout,int_fields=1,real_fields=min(n_fields,self%order))
         call tab_(stdout,int_fields=1)
         do n = f,l
            call put_(stdout,n,real_width=TRUE)
         end do
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=min(n_fields,self%order))
         do i = 1,self%n_irrep  ! The actual table
            call put_(stdout,self%irrep(i)%label,int_width=TRUE)
            do n = f,l
               call put_(stdout,self%irrep(i)%chi(n))
            end do
            call flush_(stdout)
         end do
         call dash_(stdout,int_fields=1,real_fields=min(n_fields,self%order))
      end do
      call flush_(stdout)
      call text_(stdout,"3x3 Representation matrices :")
      call flush_(stdout)
      call put_(stdout,self%mx,by_dim3_matrices=TRUE)
      call flush_(stdout)
      call text_(stdout,"Irrep matrices :")
      call flush_(stdout)
      do i = 1,self%n_irrep
         call flush_(stdout)
         dim = self%irrep(i)%dim
         call text_(stdout,"Irrep "//trim(self%irrep(i)%label)//", dimension "//trim(to_str_(dim))//":")
         call flush_(stdout)
         if (dim==1) then
            call put_(stdout,self%irrep(i)%mx(1,1,:))
         else
            do n = 1,self%order
               call text_(stdout,"Matrix number "//to_str_(n))
               call flush_(stdout)
               call put_(stdout,self%irrep(i)%mx(:,:,n))
            end do
         end if
      end do
   end subroutine

end module
