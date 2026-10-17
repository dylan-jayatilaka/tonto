!===================================================================
!
! SLATERSHELL: used to describe contracted slater shells.
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
!===================================================================
module SLATERSHELL_MODULE

#  include "macros"

   include "slatershell.use"

   implicit none

   include "slatershell.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(SLATERSHELL_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SLATERSHELL:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SLATERSHELL_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SLATERSHELL:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(SLATERSHELL_TYPE), INOUT :: self
      call destroy_(self%n)
      call destroy_(self%z)
      call destroy_(self%c)
      call destroy_(self%orb_kind)
      call destroy_(self%occupancy)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_l(self,l)
   ! Set the l value
      type(SLATERSHELL_TYPE), INOUT :: self
      INT, IN :: l
      self%l = l
      self%l_chr = l_chr(self%l)
      call set_n_comp_(self)
   end subroutine

   PURE subroutine set_n_comp(self)
   ! Set the number of components
      type(SLATERSHELL_TYPE), INOUT :: self
      self%n_comp = 2*self%l+1
   end subroutine


! ===========
! I/O methods
! ===========

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(SLATERSHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","SLATERSHELL:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"SLATERSHELL:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process command "keyword". Data is inputted from "stdin", unless
   ! "keyword" is a sequence of blank separated strings.  In this
   ! case, the sequence is processed as if it were a separate file.
      type(SLATERSHELL_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                  ")  ! exit case
      case ("c=                 "); call read_c_(self)
      case ("junk=              "); call read_junk_(self)
      case ("orb_kinds=         "); call read_orb_kinds_(self)
      case ("l=                 "); call read_l_(self)
      case ("l_int=             "); call read_l_int_(self)
      case ("l_chr=             "); call read_l_chr_(self)
      case ("l,orb_kinds,n,z,c*="); call read_l_orb_kinds_n_z_c_ptr_(self)
      case ("n=                 "); call read_n_(self)
      case ("n_prim=            "); call read_n_prim_(self)
      case ("n_orb=             "); call read_n_orb_(self)
      case ("n,z,c*=            "); call read_n_z_c_ptr_(self)
      case ("occupancy=         "); call read_occupancy_(self)
      case ("units=             "); call read_units_(self)
      case ("z=                 "); call read_z_(self)
      case default; allocate(tonto%known_keywords(15))
      tonto%known_keywords(1) = "}                  "
      tonto%known_keywords(2) = "c=                 "
      tonto%known_keywords(3) = "junk=              "
      tonto%known_keywords(4) = "orb_kinds=         "
      tonto%known_keywords(5) = "l=                 "
      tonto%known_keywords(6) = "l_int=             "
      tonto%known_keywords(7) = "l_chr=             "
      tonto%known_keywords(8) = "l,orb_kinds,n,z,c*="
      tonto%known_keywords(9) = "n=                 "
      tonto%known_keywords(10) = "n_prim=            "
      tonto%known_keywords(11) = "n_orb=             "
      tonto%known_keywords(12) = "n,z,c*=            "
      tonto%known_keywords(13) = "occupancy=         "
      tonto%known_keywords(14) = "units=             "
      tonto%known_keywords(15) = "z=                 "
      call unknown_(tonto,word,"SLATERSHELL:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(SLATERSHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_junk(self)
   ! Read in a junk string, useful for ignoring a field
      type(SLATERSHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call skip_next_item_(stdin)
   end subroutine

   subroutine read_l(self)
   ! Read in the l symbol
      type(SLATERSHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word

      ! The following code is inherited from SHELL
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else; call read_l_chr_(self)
      end if
   end subroutine

   subroutine read_l_int(self)
   ! Read in the l integer. No way to get "L" shells in this input mode.
      type(SLATERSHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word

      ! The following code is inherited from SHELL
      call read_(stdin,word)
      ENSURE(is_int_(word),"SLATERSHELL:read_l_int ... expecting an integer for L")
      call set_l_(self,to_int_(word))
   end subroutine

   subroutine read_l_chr(self)
   ! Read in the l symbol
      type(SLATERSHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      STR(len=1) :: l_c
      INT :: l

      ! The following code is inherited from SHELL
      call read_(stdin,word)
      DIE_IF(len_trim(word)/=1,"SLATERSHELL:read_l_chr ... unknown L symbol")
      self%l_chr = word(1:1)
      if (self%l_chr/="L") call to_lower_case_(self%l_chr)
      l_c = self%l_chr(1:1)
      l = 0
      select case (l_c)
      case ("s"); l = 0
      case ("p"); l = 1
      case ("L"); l = 1  ! This is the maximum l in the shell
      case ("d"); l = 2
      case ("f"); l = 3
      case ("g"); l = 4
      case ("h"); l = 5
      case ("i"); l = 6
      case default; l = 6+iachar(l_c)-iachar("i")
      end select
      call set_l_(self,l)
   end subroutine

   subroutine read_n_prim(self)
   ! Read in the number of contraction coefficients
      type(SLATERSHELL_TYPE), INOUT :: self
      call read_(stdin,self%n_prim)
      ENSURE(self%n_prim>0,"SLATERSHELL:read_n_prim ... n_prim must be positive")
   end subroutine

   subroutine read_n_orb(self)
   ! Read in the number of generally contracted orbitals
      type(SLATERSHELL_TYPE), INOUT :: self
      call read_(stdin,self%n_orb)
      ENSURE(self%n_orb>0,"SLATERSHELL:read_n_orb ... n_orb must be positive")
   end subroutine

   subroutine read_n(self)
   ! Read in the "n" quantum numbers. NOTE: n_prim must already have been input.
      type(SLATERSHELL_TYPE), INOUT :: self
   ENSURE(self%n_prim>0,"SLATERSHELL:read_n ... n_prim not set; use n_prim= before this command")
      call destroy_(self%n)
      call create_(self%n,self%n_prim)
      call read_(stdin,self%n)
   end subroutine

   subroutine read_c(self)
   ! Read in the "c" contraction coefficients. NOTE: n_orb must
   ! already have been input.
      type(SLATERSHELL_TYPE), INOUT :: self
   ENSURE(self%n_prim>0,"SLATERSHELL:read_c ... n_prim not set; use n_prim= before this command")
   ENSURE(self%n_orb>0,"SLATERSHELL:read_c ... n_orb not set; use n_orb= before this command")
      call destroy_(self%c)
      call create_(self%c,self%n_prim,self%n_orb)
      call read_(stdin,self%c)
   end subroutine

   subroutine read_z(self)
   ! Read in the "z" slater function exponents. NOTE: n_prim must
   ! already have been input.
      type(SLATERSHELL_TYPE), INOUT :: self
   ENSURE(self%n_prim>0,"SLATERSHELL:read_z ... n_prim not set; use n_prim= before this command")
      call destroy_(self%z)
      call create_(self%z,self%n_prim)
      call read_(stdin,self%z)
   end subroutine

   subroutine read_orb_kinds(self)
   ! Read in the orbital labels ("1s", "2s", "2p" ....); also set zero
   ! occupancies, if kinds are not set
      type(SLATERSHELL_TYPE), INOUT :: self
      call destroy_(self%orb_kind)
      call read_all_(stdin,self%orb_kind)
      if (self%n_orb>0) then
         ENSURE(self%n_orb==size(self%orb_kind),"SLATERSHELL:read_orb_kinds ... n_orb is not orb_kind.dim")
      else
         self%n_orb = size(self%orb_kind)
      end if
      if (allocated(self%occupancy)) then
         ENSURE(self%n_orb==size(self%occupancy),"SLATERSHELL:read_orb_kinds ... n_orb is not occupancy.dim")
      else
         call create_(self%occupancy,self%n_orb)
         self%occupancy = 0
      end if
   end subroutine

   subroutine read_occupancy(self)
   ! Read in the occupancies; also set blank kinds, if kinds are not set.
      type(SLATERSHELL_TYPE), INOUT :: self
      call destroy_(self%occupancy)
      call read_all_(stdin,self%occupancy)
      if (self%n_orb>0) then
         ENSURE(self%n_orb==size(self%orb_kind),"SLATERSHELL:read_occupancy ... n_orb is not orb_kind.dim")
      else
         self%n_orb = size(self%orb_kind)
      end if
      if (allocated(self%orb_kind)) then
         ENSURE(self%n_orb==size(self%orb_kind),"SLATERSHELL:read_occupancy ... n_orb is not orb_kind.dim")
      else
         call create_(self%orb_kind,self%n_orb)
         self%orb_kind = " "
      end if
   end subroutine

   subroutine read_l_orb_kinds_n_z_c_ptr(self)
   ! Read in everything: l, kind, and then n,z,c in a table, as in an
   ! Ajit Thakkar table
      type(SLATERSHELL_TYPE), INOUT :: self
      call read_l_chr_(self)
      call read_orb_kinds_(self)
      call read_n_z_c_ptr_(self)
   end subroutine

   subroutine read_n_z_c_ptr(self)
   ! Read in the "n", "z" and then "c" coefficients, across a line.
      type(SLATERSHELL_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: tmp
      INT :: i,k
   ENSURE(self%n_orb>0,"SLATERSHELL:read_n_z_c_ptr ... n_orb not set; use n_orb= before this command")
   ENSURE(self%n_prim==0,"SLATERSHELL:read_n_z_c_ptr ... n_prim already defined!")
      call read_all_(stdin,tmp)
      ENSURE(mod(size(tmp),(self%n_orb+2))==0,"SLATERSHELL:read_n_z_c_ptr ... number of data not divisible by n_orb")
      self%n_prim = size(tmp)/(self%n_orb+2)
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_prim)
      call create_(self%z,self%n_prim)
      call create_(self%c,self%n_prim,self%n_orb)
      k = 0
      do i = 1,self%n_prim
         ENSURE(is_int_(tmp(k+1)),"SLATERSHELL:read_n_z_c_ptr ... n number is not integer")
         self%n(i) = nint(tmp(k+1))
         self%z(i) = tmp(k+2)
         self%c(i,:) = tmp(k+2+1:k+2+self%n_orb)
         k = k+2+self%n_orb
      end do
      call destroy_(tmp)
   end subroutine


! ====================
! Key related routines
! ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(SLATERSHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(SLATERSHELL_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"SLATERSHELL:process_keys ... no keys")

      ! The number of keys

      ! The following code is inherited from OBJECT
      n_key = size(keys)

      ! Keyword counter
      k = 0
      do  ! Loop over keywords in "keys"

         ! Get the next keyword
         k = k+1
         keyword = keys(k)

         ! Exit if close brace
         if (keyword=="}") exit
         if (keyword=="{") then  ! embedded keyword

            ! Turn off keyword echo
            keyword_echo = tonto%keyword_echo
            call set_keyword_echo_(tonto,FALSE)

            ! Find closing brace of embedded section
            l = index_of_matching_bracket_(keys(k:),"{")
            DIE_IF(l==0,"SLATERSHELL:process_keys ... no matching closing brace in embedded keyword")

            ! Process internalized keywords
            call redirect_(stdin,keys(k:k+l-1))
            call read_keywords_(self)
            call revert_(stdin)

            ! Reset key counter
            k = k+l-1

            ! Reset keyword echo
            call set_keyword_echo_(tonto,keyword_echo)
         else  ! plain-old keyword

            ! Process plain-old keyword
            call process_keyword_(self,keyword)
         end if

         ! Exit if no keys left
         if (k==n_key) exit
      end do
   end subroutine

   function keys_created(self) result (res)
   ! Return TRUE if the list-element keys are created.
      type(SLATERSHELL_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(SLATERSHELL_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(SLATERSHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


! ===============
! Inquiry Methods
! ===============

   PURE function l_chr(l) result (res)
   ! Return a character representation for angular mtm "l"
   ! Of course, "L" shells are never returned: they have mixed mtm.
      INT, IN :: l
      STR(len=1) :: res
   ENSURE(l>=0 AND l<=23,"SLATERSHELL:l_chr ... angular momentum too large:"//trim(to_str_(l)))

      ! The following code is inherited from SHELL
      select case (l)
      case (0); res = "s"
      case (1); res = "p"
      case (2); res = "d"
      case (3); res = "f"
      case (4); res = "g"
      case default; res = achar(l-4+iachar("g"))
      end select
   end function

   pure function same_as(self,b) result (res)
   ! Return TRUE if "self" is the same as "b".
      type(SLATERSHELL_TYPE), IN :: self
      type(SLATERSHELL_TYPE), IN :: b
      BIN :: res

   ! ENSURE(.n.allocated, "no n quantum numbers")
   ! ENSURE(.c.allocated, "no contraction coefficients")
   ! ENSURE(.z.allocated, "no exponents")
   ! ENSURE(b.n.allocated,"no n quantum numbers to compare to")
   ! ENSURE(b.c.allocated,"no contraction coefficients to compare to")
   ! ENSURE(b.z.allocated,"no exponents to compare to")
      res = self%l==b%l AND same_as_(self%n,b%n) AND same_as_(self%c,b%c) AND same_as_(self%z,b%z)
      if (res AND allocated(self%orb_kind)) res = same_as_(self%orb_kind,b%orb_kind)
      if (res AND allocated(self%occupancy)) res = same_as_(self%occupancy,b%occupancy)
   end function

   pure function no_of_basis_functions(self) result (res)
   ! Work out and return the TOTAL number of generally contracted basis
   ! functions , counting the agular part also.
      type(SLATERSHELL_TYPE), IN :: self
      INT :: res
      res = self%n_comp*self%n_orb
   end function

   pure function no_of_primitives(self) result (res)
   ! Work out and return the TOTAL number of primitives, counting the angular
   ! part also.
      type(SLATERSHELL_TYPE), IN :: self
      INT :: res
      res = self%n_comp*self%n_prim
   end function


! ==============================
! Density and derivative density
! ==============================

   PURE function density_at_radius(self,R) result (res)
   ! Returns the value of the coppens orbital at radius "R".
   ! SOme work can be save if the prefactor array is made.
      type(SLATERSHELL_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      INT :: i
      REAL :: orb
      VEC(REAL,:), allocatable :: val
      call create_(val,self%n_prim)
      where (self%n==1); val = exp(-self%z*R)
      elsewhere; val = R**(self%n-1)*exp(-self%z*R)
      end where
      res = ZERO
      do i = 1,self%n_orb
         orb = sum(self%c(:,i)*val)
         res = res+self%occupancy(i)*orb*orb
      end do

      ! Divide by 4*pi since the radial wavefunction
      ! is multiplied by Y(lm) which is (4pi)^(1/2)
      res = res/(FOUR*PI)
      call destroy_(val)
   end function

   PURE function densities_at_radii(self,R) result (res)
   ! Returns the values of the coppens density at all the radii "R".
      type(SLATERSHELL_TYPE), IN :: self
      VEC(REAL,:), IN :: R
      VEC(REAL,size(R)) :: res
      INT :: i,p
      REAL :: orb
      VEC(REAL,:), allocatable :: val
      call create_(val,self%n_prim)
      do p = 1,size(R)
         where (self%n==1); val = exp(-self%z*R(p))
         elsewhere; val = R(p)**(self%n-1)*exp(-self%z*R(p))
         end where
         res(p) = ZERO
         do i = 1,self%n_orb
            orb = sum(self%c(:,i)*val)
            res(p) = res(p)+self%occupancy(i)*orb*orb
         end do
      end do

      ! Divide by 4*pi since the radial wavefunction
      ! is multiplied by Y(lm) which is (4pi)^(1/2)
      res = res/(FOUR*PI)
      call destroy_(val)
   end function

   PURE function density_d1_at_radius(self,R) result (res)
   ! Returns the 1st radial derivative of the orbital at all the radii "R".
      type(SLATERSHELL_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      INT :: i
      REAL :: Rp,orb,dorb
      VEC(REAL,:), allocatable :: val1,val2,val3

   ! f(R)  = \sum_i n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))^2
   ! f'(R) = \sum_i 2 n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
      call create_(val1,self%n_prim)
      call create_(val2,self%n_prim)
      call create_(val3,self%n_prim)
      Rp = R
      val3 = -self%z*Rp
      where (self%n==2); val2 = exp(val3)
      elsewhere; val2 = Rp**(self%n-2)*exp(val3)
      end where
      val1 = val2*Rp
      val2 = val2*(self%n-ONE-val3)
      res = ZERO
      do i = 1,self%n_orb
         orb = sum(self%c(:,i)*val1)
         dorb = sum(self%c(:,i)*val2)
         res = res+TWO*self%occupancy(i)*orb*dorb
      end do
      call destroy_(val3)
      call destroy_(val2)
      call destroy_(val1)
   end function

   PURE function densities_d1_at_radii(self,R) result (res)
   ! Returns the 1st radial derivative of the orbital at all the radii "R".
      type(SLATERSHELL_TYPE), IN :: self
      VEC(REAL,:), IN :: R
      VEC(REAL,size(R)) :: res
      INT :: i,p
      REAL :: Rp,orb,dorb
      VEC(REAL,:), allocatable :: val1,val2,val3

   ! f(R)  = \sum_i n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))^2
   ! f'(R) = \sum_i 2 n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
      call create_(val1,self%n_prim)
      call create_(val2,self%n_prim)
      call create_(val3,self%n_prim)
      do p = 1,size(R)
         Rp = R(p)
         val3 = -self%z*Rp
         where (self%n==2); val2 = exp(val3)
         elsewhere; val2 = Rp**(self%n-2)*exp(val3)
         end where
         val1 = val2*Rp
         val2 = val2*(self%n-ONE-val3)
         res(p) = ZERO
         do i = 1,self%n_orb
            orb = sum(self%c(:,i)*val1)
            dorb = sum(self%c(:,i)*val2)
            res(p) = res(p)+TWO*self%occupancy(i)*orb*dorb
         end do
      end do
      call destroy_(val3)
      call destroy_(val2)
      call destroy_(val1)
   end function

   PURE function density_d2_at_radius(self,R) result (res)
   ! Returns the 2nd radial derivative of the orbital at all the radii "R".
      type(SLATERSHELL_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      INT :: i
      REAL :: Rp,Rp2,orb,dorb,ddorb
      VEC(REAL,:), allocatable :: tmp3,val1,val2,val3

   ! f(R)   = \sum_i n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))^2
   ! f'(R)  = \sum_i 2 n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
   ! f''(R) = \sum_i 2 n_i {
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
   !         +
   !                       (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} ( (
   !                       (n_i-2) R^(n_i-3) exp(-z_i R)
   !                       -z_i R^(n_i-2) exp(-z_i R)
   !                       ) [(n_i-1) - z_i R ]
   !                       -
   !                        z_i R^(n_i-2) exp(-z_i R) )
   ! f''(R) = \sum_i 2 n_i {
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])^2
   !         +
   !         (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} R^(n_i-3) exp(-z_i R) ((n_i-2 - z_i R ) [n_i-1 - z_i R ] - z_i R )
      call create_(val1,self%n_prim)
      call create_(val2,self%n_prim)
      call create_(val3,self%n_prim)
      call create_(tmp3,self%n_prim)
      Rp = R
      Rp2 = Rp*Rp
      val3 = -self%z*Rp
      where (self%n==3); tmp3 = exp(val3)
      elsewhere; tmp3 = Rp**(self%n-3)*exp(val3)
      end where
      val1 = (self%n-ONE-val3)
      val2 = tmp3*Rp*val1
      val3 = tmp3*(val1*val1-val1-val3)
      val1 = tmp3*Rp2
      res = ZERO
      do i = 1,self%n_orb
         orb = sum(self%c(:,i)*val1)
         dorb = sum(self%c(:,i)*val2)
         ddorb = sum(self%c(:,i)*val3)
         res = res+TWO*self%occupancy(i)*(dorb*dorb+orb*ddorb)
      end do
      call destroy_(tmp3)
      call destroy_(val3)
      call destroy_(val2)
      call destroy_(val1)
   end function

   PURE function densities_d2_at_radii(self,R) result (res)
   ! Returns the 2nd radial derivative of the orbital at all the radii "R".
      type(SLATERSHELL_TYPE), IN :: self
      VEC(REAL,:), IN :: R
      VEC(REAL,size(R)) :: res
      INT :: i,p
      REAL :: Rp,Rp2,orb,dorb,ddorb
      VEC(REAL,:), allocatable :: tmp3,val1,val2,val3

   ! f(R)   = \sum_i n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))^2
   ! f'(R)  = \sum_i 2 n_i (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
   ! f''(R) = \sum_i 2 n_i {
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])
   !         +
   !                       (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} ( (
   !                       (n_i-2) R^(n_i-3) exp(-z_i R)
   !                       -z_i R^(n_i-2) exp(-z_i R)
   !                       ) [(n_i-1) - z_i R ]
   !                       -
   !                        z_i R^(n_i-2) exp(-z_i R) )
   ! f''(R) = \sum_i 2 n_i {
   !         (\sum_j c_{ji} R^(n_i-2) exp(-z_i R)[(n_i-1) - z_i R ])^2
   !         +
   !         (\sum_j c_{ji} R^(n_i-1) exp(-z_i R))
   !         (\sum_j c_{ji} R^(n_i-3) exp(-z_i R) ((n_i-2 - z_i R ) [n_i-1 - z_i R ] - z_i R )
      call create_(val1,self%n_prim)
      call create_(val2,self%n_prim)
      call create_(val3,self%n_prim)
      call create_(tmp3,self%n_prim)
      do p = 1,size(R)
         Rp = R(p)
         Rp2 = Rp*Rp
         val3 = -self%z*Rp
         where (self%n==3); tmp3 = exp(val3)  ! save this
         elsewhere; tmp3 = Rp**(self%n-3)*exp(val3)  ! save this
         end where
         val1 = (self%n-ONE-val3)
         val2 = tmp3*Rp*val1
         val3 = tmp3*(val1*val1-val1-val3)
         val1 = tmp3*Rp2
         res(p) = ZERO
         do i = 1,self%n_orb
            orb = sum(self%c(:,i)*val1)
            dorb = sum(self%c(:,i)*val2)
            ddorb = sum(self%c(:,i)*val3)
            res(p) = res(p)+TWO*self%occupancy(i)*(dorb*dorb+orb*ddorb)
         end do
      end do
      call destroy_(tmp3)
      call destroy_(val3)
      call destroy_(val2)
      call destroy_(val1)
   end function


! =============
! Normalisation
! =============

   pure subroutine unnormalize(self)
   ! Set the value of the coefficient to correspond to un-normalized Slater
   ! functions -- assuming they are normalized. This saves in computation time.
      type(SLATERSHELL_TYPE), INOUT :: self
      INT :: i,n2
      REAL :: fac
      do i = 1,self%n_prim
         n2 = 2*self%n(i)
         fac = sqrt(TWO*self%z(i)/factorial_(n2))*(TWO*self%z(i))**self%n(i)
         self%c(i,:) = self%c(i,:)*fac
      end do
   end subroutine

   pure subroutine renormalize(self)
   ! Set the value of the coefficient to correspond to normalized slater
   ! functions --- assuming they are w.r.t. unnormalized functions. 
      type(SLATERSHELL_TYPE), INOUT :: self
      INT :: i,n2
      do i = 1,self%n_prim
         n2 = 2*self%n(i)
         self%c(i,:) = self%c(i,:)/(sqrt(TWO*self%z(i)/factorial_(n2))*(TWO*self%z(i))**self%n(i))
      end do
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(SLATERSHELL_TYPE), IN :: self
      call dump_(stdout,"l",self%l)
      call dump_(stdout,"l_chr",self%l_chr)
      call dump_(stdout,"n_comp",self%n_comp)
      call dump_(stdout,"n_orb",self%n_orb)
      call dump_(stdout,"n_prim",self%n_prim)
      call dmpa_(stdout,"n",self%n)
      call dmpa_(stdout,"z",self%z)
      call dmpa_(stdout,"c",self%c)
      call dmpa_(stdout,"orb_kind",self%orb_kind)
      call dmpa_(stdout,"occupancy",self%occupancy)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(SLATERSHELL_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":SLATERSHELL@ = { ")
      else; call text_(stdout,trim(object_name)//":SLATERSHELL = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(SLATERSHELL_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":SLATERSHELL@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


! ======
! Output
! ======

   subroutine put(self)
   ! Put the orbital information to "stdout"
      type(SLATERSHELL_TYPE), IN :: self
      call flush_(stdout)
      call show_(stdout,"L quantum no.     =",self%l)
      call show_(stdout,"No. of orbitals   =",self%n_orb)
      call show_(stdout,"No. of primitives =",self%n_prim)
   end subroutine

end module
