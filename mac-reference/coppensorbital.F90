!=================================================================
! COPPENSORBITAL: 
!
! Used to describe contracted slater orbitals fitted to
! relativistic Hartree Fock calculations, as described 
! by Coppens and Liu, and MacchI and Coppens..
!
! Copyright (C) Mark Hore, Dylan Jayatilaka, 2002
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
!=================================================================
module COPPENSORBITAL_MODULE

#  include "macros"

   include "coppensorbital.use"

   implicit none

   include "coppensorbital.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys
   REAL, private :: values_cutoff = COPPENSORBITAL_VALUES_CUTOFF

contains

! ==================
! Allocation methods
! ==================

   PURE subroutine create(self)
   ! Allocate an object
      type(COPPENSORBITAL_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"COPPENSORBITAL:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(COPPENSORBITAL_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"COPPENSORBITAL:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(COPPENSORBITAL_TYPE), INOUT :: self
      call destroy_(self%n)
      call destroy_(self%c)
      call destroy_(self%z)
   end subroutine


!  =======
!  Inquiry
!  =======

   pure function ex(self,i) result (res)
   ! Return the exponent of the "i" th slater orbital
      type(COPPENSORBITAL_TYPE), IN :: self
      INT, IN :: i
      REAL :: res
      res = self%z(i)
   end function

   PURE function cc(self,i) result (res)
   ! Return the contraction coefficient of the "i" th slater orbital
      type(COPPENSORBITAL_TYPE), IN :: self
      INT, IN :: i
      REAL :: res
      res = self%c(i)
   end function


! ===========
! I/O methods
! ===========

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","COPPENSORBITAL:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"COPPENSORBITAL:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process command "keyword". Data is inputted from "stdin", unless "keyword"
   ! is a sequence of blank separated strings.  In this case, the sequence is
   ! processed as if it were a separate file.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                            ")  ! exit case
      case ("c=                           "); call read_c_(self)
      case ("orb_kind=                    "); call read_orb_kind_(self)
      case ("orb_kind,occupancy,num,n,c,z="); call read_orb_kind_occ_num_n_c_z_(self)
      case ("n=                           "); call read_n_(self)
      case ("n,c,z=                       "); call read_n_c_z_(self)
      case ("n,c,z*=                      "); call read_n_c_z_ptr_(self)
      case ("n_fun=                       "); call read_n_fun_(self)
      case ("num=                         "); call read_n_fun_(self)
      case ("occupancy=                   "); call read_occupancy_(self)
      case ("values_cutoff=               "); call read_values_cutoff
      case ("z=                           "); call read_z_(self)
      case default; allocate(tonto%known_keywords(12))
      tonto%known_keywords(1) = "}                            "
      tonto%known_keywords(2) = "c=                           "
      tonto%known_keywords(3) = "orb_kind=                    "
      tonto%known_keywords(4) = "orb_kind,occupancy,num,n,c,z="
      tonto%known_keywords(5) = "n=                           "
      tonto%known_keywords(6) = "n,c,z=                       "
      tonto%known_keywords(7) = "n,c,z*=                      "
      tonto%known_keywords(8) = "n_fun=                       "
      tonto%known_keywords(9) = "num=                         "
      tonto%known_keywords(10) = "occupancy=                   "
      tonto%known_keywords(11) = "values_cutoff=               "
      tonto%known_keywords(12) = "z=                           "
      call unknown_(tonto,word,"COPPENSORBITAL:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_orb_kind(self)
   ! Read in the orbital kind ("1s", "2s", "2p" ....)
      type(COPPENSORBITAL_TYPE), INOUT :: self
      call read_(stdin,self%orb_kind)
   end subroutine

   subroutine read_occupancy(self)
   ! Read in the orbital occupancy
      type(COPPENSORBITAL_TYPE), INOUT :: self
      call read_(stdin,self%occupancy)
   end subroutine

   subroutine read_n_fun(self)
   ! Read in the number of contracted functions.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      call read_(stdin,self%n_fun)
   end subroutine

   subroutine read_n(self)
   ! Read in the "n" quantum numbers. NOTE: n_fun must already have been input.
      type(COPPENSORBITAL_TYPE), INOUT :: self
   ENSURE(self%n_fun>0,"COPPENSORBITAL:read_n ... n_fun is negative; use n_fun= before this command")
      call destroy_(self%n)
      call create_(self%n,self%n_fun)
      call read_(stdin,self%n)
   end subroutine

   subroutine read_c(self)
   ! Read in the "c" contraction coefficients. NOTE: n_fun must already have
   ! been input.
      type(COPPENSORBITAL_TYPE), INOUT :: self
   ENSURE(self%n_fun>0,"COPPENSORBITAL:read_c ... n_fun is negative; use n_fun= before this command")
      call destroy_(self%c)
      call create_(self%c,self%n_fun)
      call read_(stdin,self%c)
   end subroutine

   subroutine read_z(self)
   ! Read in the "z" slater function exponents. NOTE: n_fun must already have
   ! been input.
      type(COPPENSORBITAL_TYPE), INOUT :: self
   ENSURE(self%n_fun>0,"COPPENSORBITAL:read_z ... n_fun is negative; use n_fun= before this command")
      call destroy_(self%z)
      call create_(self%z,self%n_fun)
      call read_(stdin,self%z)
   end subroutine

   subroutine read_n_c_z(self)
   ! Read in the "n", "c" and "z" vectors. NOTE: .n_fun must previously have been inputted.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      INT :: i
      REAL :: val
   ENSURE(self%n_fun>0,"COPPENSORBITAL:read_n_c_z ... n_fun is negative; use n_fun= before this command")
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_fun)
      call create_(self%c,self%n_fun)
      call create_(self%z,self%n_fun)
      do i = 1,self%n_fun
         call read_(stdin,val)
         ENSURE(is_int_(val),"COPPENSORBITAL:read_n_c_z ... n number is not integer")
         self%n(i) = nint(val)
         call read_(stdin,self%c(i))
         call read_(stdin,self%z(i))
      end do
   end subroutine

   subroutine read_n_c_z_ptr(self)
   ! Read in the "n", "c" and "z" vectors. NOTE: it is not necessary to read in
   ! n_fun, it will be defined from the vector length.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      VEC(REAL,:), allocatable :: tmp
      INT :: i,k
      call read_all_(stdin,tmp)
      ENSURE(mod(size(tmp),3)==0,"COPPENSORBITAL:read_n_c_z_ptr ... number of data not divisible by three")
      self%n_fun = size(tmp)/3
      call destroy_(self%n); call destroy_(self%c); call destroy_(self%z)
      call create_(self%n,self%n_fun)
      call create_(self%c,self%n_fun)
      call create_(self%z,self%n_fun)
      k = 1
      do i = 1,3
         ENSURE(is_int_(tmp(k)),"COPPENSORBITAL:read_n_c_z_ptr ... n number is not integer")
         self%n(k) = nint(tmp(k))
         self%c(k) = tmp(k+1)
         self%z(k) = tmp(k+2)
         k = k+3
      end do
      call destroy_(tmp)
   end subroutine

   subroutine read_orb_kind_occ_num_n_c_z(self)
   ! Read in everything: the "kind", "occupnacy", "n_fun", and "n", "c" and "z".
      type(COPPENSORBITAL_TYPE), INOUT :: self
      call read_orb_kind_(self)
      call read_occupancy_(self)
      call read_n_fun_(self)
      call read_n_c_z_(self)
   end subroutine

   subroutine read_values_cutoff()
   ! Read in a cutoff below which values of the the coppensorbital on
   ! a grid are set to zero.
      call read_(stdin,values_cutoff)
   end subroutine


! ====================
! Key related routines
! ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(COPPENSORBITAL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"COPPENSORBITAL:process_keys ... no keys")

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
            DIE_IF(l==0,"COPPENSORBITAL:process_keys ... no matching closing brace in embedded keyword")

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
      type(COPPENSORBITAL_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(COPPENSORBITAL_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(COPPENSORBITAL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


! =======
! Methods
! =======

   pure function same_as(self,b) result (res)
   ! Return TRUE if "self" is the same as "b".
      type(COPPENSORBITAL_TYPE), IN :: self
      type(COPPENSORBITAL_TYPE), IN :: b
      BIN :: res
      res = self%orb_kind==b%orb_kind AND self%occupancy==b%occupancy AND same_as_(self%n,b%n) AND same_as_(self%c,b%c) AND same_as_(self%z,b%z)
   end function


! ==============================
! Density and derivative density
! ==============================

   PURE function density_at_radius(self,R) result (res)
   ! Returns the value of the coppens orbital density  at radius "R".
      type(COPPENSORBITAL_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      res = value_at_radius_(self,R)
      res = self%occupancy*res*res
      ! Divide by 4*pi since the radial wavefunction
      ! is multiplied by Y(lm) which is (4pi)^(1/2)
      res = res/(FOUR*PI)
   end function

   PURE function densities_at_radii(self,R) result (res)
   ! Returns the values of the coppens density at all the radii "R".
      type(COPPENSORBITAL_TYPE), IN :: self
      VEC(REAL,:), IN :: R
      VEC(REAL,size(R)) :: res
      res = values_at_radii_(self,R)
      res = self%occupancy*res*res
      ! Divide by 4*pi since the radial wavefunction
      ! is multiplied by Y(lm) which is (4pi)^(1/2)
      res = res/(FOUR*PI)
   end function

   PURE function value_at_radius(self,R) result (res)
   ! Returns the value of the coppens orbital at radius "R".
      type(COPPENSORBITAL_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      INT :: j
      REAL :: val
      res = ZERO
      do j = 1,size(self%n)
         val = self%c(j)*exp(-self%z(j)*R)
         if (self%n(j)==1) then; res = res+val
         else; res = res+val*R**(self%n(j)-1)
         end if
      end do
   end function

   PURE function values_at_radii(self,R) result (res)
   ! Returns the values of the coppens orbital at all the radii "R".
      type(COPPENSORBITAL_TYPE), IN :: self
      VEC(REAL,:), IN :: R
      VEC(REAL,size(R)) :: res
      INT :: i,j
      REAL :: sm,val,Ri
      do i = 1,size(R)
         Ri = R(i)
         sm = ZERO
         do j = 1,size(self%n)
            val = self%c(j)*exp(-self%z(j)*Ri)
            if (self%n(j)==1) then; sm = sm+val
            else; sm = sm+val*Ri**(self%n(j)-1)
            end if
         end do
         res(i) = sm
      end do
   end function

   PURE function radial_density_at_radius(self,R) result (res)
   ! Returns the radial value of the coppens orbital density at radius "R".
      type(COPPENSORBITAL_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      res = value_at_radius_(self,R)
      res = self%occupancy*res*res
      res = FOUR*PI*R*R*res
   end function


! =============
! Normalisation
! =============

   pure subroutine unnormalize(self)
   ! Set the value of the coefficient to correspond to un-normalized Slater
   ! functions -- assuming they are normalized. This saves in computation time.
   ! We got this from Bunge (1993) At. Data Nucl. Data Tables 53, p113
   ! (p 118)
      type(COPPENSORBITAL_TYPE), INOUT :: self
      INT :: i,n2
      do i = 1,self%n_fun
         n2 = 2*self%n(i)
         self%c(i) = self%c(i)*sqrt(TWO*self%z(i)/factorial_(n2))*(TWO*self%z(i))**self%n(i)
      end do
   end subroutine

   pure subroutine renormalize(self)
   ! Set the value of the coefficient to correspond to normalized slater
   ! functions --- assuming they are w.r.t. unnormalized functions. 
      type(COPPENSORBITAL_TYPE), INOUT :: self
      INT :: i,n2
      do i = 1,self%n_fun
         n2 = 2*self%n(i)
         self%c(i) = self%c(i)/(sqrt(TWO*self%z(i)/factorial_(n2))*(TWO*self%z(i))**self%n(i))
      end do
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(COPPENSORBITAL_TYPE), IN :: self
      call dump_(stdout,"orb_kind",self%orb_kind)
      call dump_(stdout,"occupancy",self%occupancy)
      call dump_(stdout,"n_fun",self%n_fun)
      call dmpa_(stdout,"n",self%n)
      call dmpa_(stdout,"c",self%c)
      call dmpa_(stdout,"z",self%z)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(COPPENSORBITAL_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":COPPENSORBITAL@ = { ")
      else; call text_(stdout,trim(object_name)//":COPPENSORBITAL = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(COPPENSORBITAL_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":COPPENSORBITAL@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


! ======
! Output
! ======

   subroutine put(self)
   ! Put the orbital information to "stdout"
      type(COPPENSORBITAL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=======================")
      call text_(stdout,"Su-Coppens type orbital")
      call text_(stdout,"=======================")
      call flush_(stdout)
      call show_(stdout,"Kind                 =",self%orb_kind)
      call show_(stdout,"Electron occupancy   =",self%occupancy)
      call show_(stdout,"No. of fit functions =",self%n_fun)
      call flush_(stdout)

      ! Table headings
      call create_(table,3)
      call set_heading_(table(1),"N quantum")
      call set_subhead_(table(1),"  number ")
      call set_heading_(table(2),"Contraction")
      call set_heading_(table(3),"Exponent")
      call set_subhead_(table(3),"/au")

      ! Set table column data
      call set_values_(table(1),self%n)
      call set_values_(table(2),self%c)
      call set_values_(table(3),self%z)

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

end module
