!==================================================================
!
! REFLECTION: Reflection data for crystals
!
! Copyright (C) Daniel Grimwood, 2000
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
module REFLECTION_MODULE

#  include "macros"

   include "reflection.use"

   implicit none

   include "reflection.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(REFLECTION_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"REFLECTION:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(REFLECTION_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"REFLECTION:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_miller_indices(self,vec)
   ! Set (h1 h2 h3) to "vec"
      type(REFLECTION_TYPE), INOUT :: self
      VEC(INT,3), IN :: vec
      self%h1 = vec(1)
      self%h2 = vec(2)
      self%h3 = vec(3)
   end subroutine

   elemental subroutine set_h1(self,val)
   ! Set h1 in the miller indices (h1 h2 h3)
      type(REFLECTION_TYPE), INOUT :: self
      INT, IN :: val
      self%h1 = val
   end subroutine

   elemental subroutine set_h2(self,val)
   ! Set h2 in the miller indices (h1 h2 h3)
      type(REFLECTION_TYPE), INOUT :: self
      INT, IN :: val
      self%h2 = val
   end subroutine

   elemental subroutine set_h3(self,val)
   ! Set h3 in the miller indices (h1 h2 h3)
      type(REFLECTION_TYPE), INOUT :: self
      INT, IN :: val
      self%h3 = val
   end subroutine

   pure subroutine set_equivalence_factor(self,val)
   ! Set how many symmetry equivalents there are for this reflection
      type(REFLECTION_TYPE), INOUT :: self
      INT, IN :: val
      self%equivalence_factor = val
   end subroutine

   pure subroutine set_stl(self,val)
   ! Set sin(theta)/lambda for this reflection
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%stl = val
   end subroutine

   pure subroutine set_theta(self,val)
   ! Set the theta angle for this reflection
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%theta = val
   end subroutine

   pure subroutine set_d(self,val)
   ! Set the d spacing for this Miller plane
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%d = val
   end subroutine

   pure subroutine set_F_pred(self,val)
   ! Set predicted structure factor magnitude F_pred to "val".
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%F_pred = val
   end subroutine

   pure subroutine set_I_pred(self,val)
   ! Set predicted structure factor intensity I_pred to "val".
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%I_pred = val
   end subroutine

   pure subroutine set_F_calc(self,val)
   ! Set calculated structure factor magnitude F_calc to "val".
      type(REFLECTION_TYPE), INOUT :: self
      CPX, IN :: val
      self%F_calc = val
      self%I_calc = CONJG(val)*val
   end subroutine

   pure subroutine set_I_calc(self,val)
   ! Set calculated structure factor intensity I_calc to "val".
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%I_calc = val
   end subroutine

   pure subroutine set_F_disp(self,val)
   ! Set anomalous dispersion conntribution to F_calc.
      type(REFLECTION_TYPE), INOUT :: self
      CPX, IN :: val
      self%F_disp = val
   end subroutine

   pure subroutine set_F_corr(self,val)
   ! Set the "corrected" experimental structure factor magnitudes
   ! .F_corr to "val".
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val
      self%F_corr = val
   end subroutine

   pure subroutine reset_F_corr_to_F_exp(self)
   ! Reset F_corr values to the original F_exp values.
      type(REFLECTION_TYPE), INOUT :: self
      self%F_corr = self%F_exp
   end subroutine

   pure subroutine set_I_exp(self,val)
   ! Set the experimental F2 value, called I_exp.
   ! NOTE: Assumes I_sigma has not yet been inputted!
   ! Do no sanity checks.
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val

! ENSURE(NOT .I_exp_set,"oh dear!")
! ENSURE(NOT .F_exp_set,"oh dear!")
      self%I_exp = val
      self%I_exp_set = TRUE  ! <<<<<
   end subroutine

   pure subroutine set_I_sigma(self,val)
   ! Read in the experimental F2_sigma i.e. I_sigma
   ! NOTE: Assumes I_exp has been inputted!
   ! Do not perform any sanity checks.
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val

! ENSURE(    .I_exp_set,"oh dear!")
! ENSURE(NOT .F_exp_set,"oh dear!")

   ! .I_sigma = val
      self%I_sigma = abs(val)
      call reset_I_exp_I_sigma_(self)
   end subroutine

   pure subroutine reset_I_exp_I_sigma(self)
   ! Reset I_exp and I_sigma to sensible values.
   ! NOTE: -ve I_sigma must have been previously pruned.
      type(REFLECTION_TYPE), INOUT :: self
      if (self%I_exp<ZERO) then
         self%I_sigma = abs(self%I_exp)+self%I_sigma
         self%I_exp = ZERO
      end if
      call derive_Fs_from_Is_(self)
   end subroutine

   pure subroutine derive_Fs_from_Is(self)
   ! The F_sigma value is calculated by solving the quadratic which
   ! gives the *largest* sigma value, which when added to .F_exp and
   ! squared gives .I_exp+.I_sigma.
   ! NOTE: I_exp must be positive at this stage
      type(REFLECTION_TYPE), INOUT :: self
      REAL :: s_p,s_m
      self%F_exp = sqrt(self%I_exp)
      self%F_corr = self%F_exp  ! Don't forget this !

      ! s_p/s_m are upper/lower error ranges
      if (self%I_exp>self%I_sigma) then
         s_p = -self%F_exp+sqrt(self%I_exp+self%I_sigma)
         s_m = self%F_exp-sqrt(self%I_exp-self%I_sigma)
      else
         s_p = -self%F_exp+sqrt(self%I_exp+self%I_sigma)
         s_m = self%F_exp
      end if
      self%F_sigma = max(s_p,s_m)

      !!!!!!!!!!!!!!!!!!!
      self%F_exp_set = TRUE  ! <<<<<
      !!!!!!!!!!!!!!!!!!!
   end subroutine

   pure subroutine set_F_exp(self,val)
   ! Set experimental structure factor magnitude F_exp to "val".
   ! Also set F_corr in case we do not want to add dispersion.
   ! NOTE: once F_exp is set it should NEVER change!
   ! NOTE: Assumes F_sigma has not yet been inputted!
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val

! ENSURE(NOT .F_exp_set,"oh dear!")
! ENSURE(NOT .I_exp_set,"oh dear!")
      self%F_exp = val
      self%F_corr = val
      self%F_exp_set = TRUE
   end subroutine

   pure subroutine set_F_sigma(self,val)
   ! Set the experimentally determined F_sigma to "val".
   ! NOTE: Assumes F_exp has been inputted!
   ! Do not do any sanity checks.
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: val

! ENSURE(    .F_exp_set,"oh dear!")
! ENSURE(NOT .I_exp_set,"oh dear!")
      self%F_sigma = val
      self%F_exp_set = TRUE
      call reset_F_exp_F_sigma_(self)
      call derive_Is_from_Fs_(self)
   end subroutine

   pure subroutine reset_F_exp_F_sigma(self)
   ! Reset F_exp and F_sigma to sensible values.
   ! NOTE: -ve F_sigma must have been previously pruned.
      type(REFLECTION_TYPE), INOUT :: self
      if (self%F_exp<ZERO) then
         self%F_sigma = self%F_sigma+abs(self%F_exp)
         self%F_exp = ZERO
         self%F_corr = ZERO
      end if
   end subroutine

   pure subroutine derive_Is_from_Fs(self)
   ! Call this after derive_fs_from_Is.
   ! For the moment ...
      type(REFLECTION_TYPE), INOUT :: self
      self%I_exp = self%F_exp*self%F_exp
      self%I_sigma = TWO*self%F_exp*self%F_sigma
   end subroutine


!  ============
!  I/O Routines
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(REFLECTION_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","REFLECTION:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"REFLECTION:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process command "keyword". Any required data needed by the
   ! "keyword" is inputted from "stdin".
   ! NOTE: These need reflections need pruning & finalizing
   !       to remove negative values
      type(REFLECTION_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}       ")  ! exit surrounding loop
      case ("f_exp=  "); call read_F_exp_(self)
      case ("f_pred= "); call read_F_pred_(self)
      case ("f_sigma="); call read_F_sigma_(self)
      case ("group=  "); call read_group_(self)
      case ("h=      "); call read_h1_(self)
      case ("h1=     "); call read_h1_(self)
      case ("h2=     "); call read_h2_(self)
      case ("h3=     "); call read_h3_(self)
      case ("indices="); call read_indices_(self)
      case ("i_exp=  "); call read_I_exp_(self)
      case ("i_pred= "); call read_I_pred_(self)
      case ("i_sigma="); call read_I_sigma_(self)
      case ("junk=   "); call read_junk_(self)
      case ("k=      "); call read_h2_(self)
      case ("l=      "); call read_h3_(self)
      case ("units=  "); call read_units_(self)
      case default; allocate(tonto%known_keywords(17))
      tonto%known_keywords(1) = "}       "
      tonto%known_keywords(2) = "f_exp=  "
      tonto%known_keywords(3) = "f_pred= "
      tonto%known_keywords(4) = "f_sigma="
      tonto%known_keywords(5) = "group=  "
      tonto%known_keywords(6) = "h=      "
      tonto%known_keywords(7) = "h1=     "
      tonto%known_keywords(8) = "h2=     "
      tonto%known_keywords(9) = "h3=     "
      tonto%known_keywords(10) = "indices="
      tonto%known_keywords(11) = "i_exp=  "
      tonto%known_keywords(12) = "i_pred= "
      tonto%known_keywords(13) = "i_sigma="
      tonto%known_keywords(14) = "junk=   "
      tonto%known_keywords(15) = "k=      "
      tonto%known_keywords(16) = "l=      "
      tonto%known_keywords(17) = "units=  "
      call unknown_(tonto,word,"REFLECTION:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


! units= and junk=

   subroutine docu_units(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"units= STR")
      call flush_(stdout)
      call text_(stdout,". The units symbol for the next numerical item to be input or output.")
      call text_(stdout,"  Atomic units are assumed after the next numerical item is processed.")
      call text_(stdout,"  Note that an item may be a single number, or a whole array.")
      call flush_(stdout)
      call text_(stdout,". The value of default_units is temporarily set in the OUTPUT_STYLE of")
      call text_(stdout,"  the stdin or stdout TEXTFILE objects.")
      call flush_(stdout)
      call text_(stdout,". Often used as embedded input in the keys= part of list input")
      call text_(stdout,".    e.g. like:  keys= { ... { junk= } ... } ")
      call text_(stdout,". See manual for more details.")
      call flush_(stdout)
      call text_(stdout,". The following unit symbols are recognised:")
      call flush_(stdout)
      call text_(stdout,"     amu")
      call text_(stdout,"     angstrom angstroms")
      call text_(stdout,"     angstrom^2 angstrom^3 angstrom^4")
      call text_(stdout,"     angstrom^-1 angstrom^-3 angstrom^-5")
      call text_(stdout,"     bohr bohrs")
      call text_(stdout,"     cm^-1 wavenumber")
      call text_(stdout,"     debye debye-angstrom debye-angstrom^2")
      call text_(stdout,"     degrees radian radians")
      call text_(stdout,"     ev")
      call text_(stdout,"     joule")
      call text_(stdout,"     kcal/mol kcal.mol^-1")
      call text_(stdout,"     kelvin")
      call text_(stdout,"     kilojoule kjoule kj kj/mol kj.mol^-1")
      call text_(stdout,"     meter meters")
      call text_(stdout,"     mb mw gb")
      if (present(die)) stop
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(REFLECTION_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine docu_junk(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"junk= STR")
      call flush_(stdout)
      call text_(stdout,". The next word or number to be input or output is ignored.")
      call flush_(stdout)
      call text_(stdout,". Often used as embedded input in the keys= part of list input")
      call text_(stdout,".    e.g. like:  keys= { ... { junk= } ... } ")
      call text_(stdout,". See manual for more details.")
      if (present(die)) stop
   end subroutine

   subroutine read_junk(self)
   ! Read in a junk string, useful for ignoring a field
      type(REFLECTION_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call skip_next_item_(stdin)
   end subroutine


! F

   subroutine docu_F_pred(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"f_pred= REAL")
      call flush_(stdout)
      call text_(stdout,". The predicted tructure factor magnitude for the reflection.")
      call text_(stdout,"  It should include a scale factor, perhaps also an extinction")
      call text_(stdout,"  correction. Values not on an absolute scale.")
      call flush_(stdout)
      if (present(die)) stop
   end subroutine

   subroutine read_F_pred(self)
   ! Read in a predicted (real) structure factor
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%F_pred)
   end subroutine


! G

   subroutine docu_group(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"group= INT")
      call flush_(stdout)
      call text_(stdout,". The measurement or frame group index that the reflection")
      call text_(stdout,"  belongs to.")
      if (present(die)) stop
   end subroutine

   subroutine read_group(self)
   ! Read the group index that this reflection belongs to.
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%group)
   end subroutine


! H

   subroutine docu_h1(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"h1= INT")
      call flush_(stdout)
      call text_(stdout,". The h1 value of this reflection's indices [h1 h2 h3]")
      if (present(die)) stop
   end subroutine

   subroutine read_h1(self)
   ! Read in the "h1" Miller index
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%h1)
   end subroutine

   subroutine docu_h2(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"h2= INT")
      call flush_(stdout)
      call text_(stdout,". The h2 value of this reflection's indices [h1 h2 h3]")
      if (present(die)) stop
   end subroutine

   subroutine read_h2(self)
   ! Read in the "h2" Miller index
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%h2)
   end subroutine

   subroutine docu_h3(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"h3= INT")
      call flush_(stdout)
      call text_(stdout,". The h3 value of this reflection's indices [h1 h2 h3]")
      if (present(die)) stop
   end subroutine

   subroutine read_h3(self)
   ! Read in the "h3" Miller index
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%h3)
   end subroutine


! I

   subroutine docu_I_exp(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"I_exp= REAL")
      call flush_(stdout)
      call text_(stdout,". The measured (experimental) squared structure factor magnitude")
      call text_(stdout,"  or this reflection, on a non-absolute scale. These are intensities,")
      call text_(stdout,"  except for the Lorentz & polarization factor.")
      call flush_(stdout)
      call text_(stdout,". Only symmetry unique reflections should be input.")
      call flush_(stdout)
      if (present(die)) stop
   end subroutine

   subroutine read_I_exp(self)
   ! Read in the experimentay F2 i.e. I_exp 
   ! NOTE: F_exp is overwritten!
   ! NOTE: The sign of I_exp is preserved on F_exp
      type(REFLECTION_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_I_exp_(self,val)
   end subroutine

   subroutine docu_I_pred(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"i_pred= REAL")
      call flush_(stdout)
      call text_(stdout,". The predicted squared structure factor magnitude for the")
      call text_(stdout,"  reflection. It should include a scale factor, perhaps also")
      call text_(stdout,"  an extinction correction. Values are not on an absolute scale.")
      call flush_(stdout)
      if (present(die)) stop
   end subroutine

   subroutine read_I_pred(self)
   ! Read in a predicted intensity
   ! NOTE: if it is negative, the sign is preserved on F_pred, for
   ! later rejection.
   ! NOTE: F_pred is overwritten!
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%I_pred)
      if (self%I_pred<ZERO) then; self%F_pred = -sqrt(-self%I_pred)
      else; self%F_pred = sqrt(self%I_pred)
      end if
   end subroutine

   subroutine docu_I_sigma(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"i_sigma= REAL")
      call flush_(stdout)
      call text_(stdout,". The estimated standard deviation for the measured squared")
      call text_(stdout,"  structure factor magnitude, for this reflection.")
      if (present(die)) stop
   end subroutine

   subroutine read_I_sigma(self)
   ! Read in the experimentay F2_sigma i.e. I_ sigma
   ! You must have already inputted the I_exp= value. The sigma value is
   ! calculated by solving the quadratic which gives the *largest* sigma
   ! value, which when added to .F_exp and squared gives .I_exp+.I_sigma.
      type(REFLECTION_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_I_sigma_(self,val)
   end subroutine

   subroutine docu_indices(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"indices= VEC{INT}(3)")
      call flush_(stdout)
      call text_(stdout,". The the three indices h1 h2 h3 of this reflection.")
      call text_(stdout,"  Braces not required.")
      if (present(die)) stop
   end subroutine

   subroutine read_indices(self)
   ! Read in the h k l Miller indices as a triple
      type(REFLECTION_TYPE), INOUT :: self
      call read_(stdin,self%h1)
      call read_(stdin,self%h2)
      call read_(stdin,self%h3)
   end subroutine


! F

   subroutine docu_F_exp(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"f_exp= REAL")
      call flush_(stdout)
      call text_(stdout,". The measured (experimental) structure factor magnitude on a")
      call text_(stdout,"  non-absolute scale, for this reflection.")
      if (present(die)) stop
   end subroutine

   subroutine read_F_exp(self)
   ! Read in the experimentally determined structure factor
      type(REFLECTION_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_exp_(self,val)
   end subroutine

   subroutine docu_F_sigma(die)
   ! Documentation.
      BIN, optional, IN :: die
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"f_sigma= REAL")
      call flush_(stdout)
      call text_(stdout,". The estimated standard deviation for the measured structure")
      call text_(stdout,"  factor magnitude, for this reflection.")
      if (present(die)) stop
   end subroutine

   subroutine read_F_sigma(self)
   ! Read in the experimentally determined standard devaiation in
   ! the structure factor
      type(REFLECTION_TYPE), INOUT :: self
      REAL :: val
      call read_(stdin,val)
      call set_F_sigma_(self,val)
   end subroutine


!  ====================
!  Key related routines
!  ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(REFLECTION_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(REFLECTION_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"REFLECTION:process_keys ... no keys")

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
            DIE_IF(l==0,"REFLECTION:process_keys ... no matching closing brace in embedded keyword")

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
      type(REFLECTION_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(REFLECTION_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(REFLECTION_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


!  ================
!  Inquiry Routines
!  ================

   pure function Miller_indices(self) result (res)
   ! The Miller indices
      type(REFLECTION_TYPE), IN :: self
      VEC(INT,3) :: res
      res = [self%h1,self%h2,self%h3]
   end function

   pure function concatenated_miller_indices(self) result (res)
   ! The (h k l) indices as a string
      type(REFLECTION_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = trim(to_str_(self%h1))//" "//trim(to_str_(self%h2))//" "//trim(to_str_(self%h3))
   end function

   elemental function delta_F(self) result (res)
   ! Return the Delta F for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_corr-self%F_pred
   end function

   elemental function delta_I(self) result (res)
   ! Return the Delta I for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      REAL :: z
      z = self%I_exp-self%I_pred
      res = z*z/self%I_sigma
   end function

   elemental function abs_delta_F(self) result (res)
   ! Return the abs(Delta F) for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = abs(self%F_pred-self%F_exp)
   end function

   elemental function abs_delta_F2(self) result (res)
   ! Return the abs(Delta (F2)) for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = abs(self%F_pred*self%F_pred-self%F_exp*self%F_exp)
   end function

   elemental function F_z(self) result (res)
   ! Return the z statistic for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_corr-self%F_pred)/self%F_sigma
   end function

   elemental function I_z(self) result (res)
   ! Return the z statistic for the predicted intensity.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%I_pred-self%I_exp)/self%I_sigma
   end function

   elemental function abs_F_z(self) result (res)
   ! Return the abs of the z statistic for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_pred-self%F_exp)/self%F_sigma
      res = abs(res)
   end function

   elemental function abs_I_z(self) result (res)
   ! Return the abs of the z statistic for the predicted intensity.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%I_pred-self%I_exp)/self%I_sigma
      res = abs(res)
   end function

   elemental function F_z2(self) result (res)
   ! Return the z**2 statistic for the predicted structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      REAL :: z
      z = (self%F_pred-self%F_exp)/self%F_sigma
      res = z*z
   end function

   elemental function I_z2(self) result (res)
   ! Return the z**2 statistic for the predicted intensity.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      REAL :: z
      z = (self%I_pred-self%I_exp)/self%I_sigma
      res = z*z
   end function

   elemental function F2_z(self) result (res)
   ! Return the z statistic for F2, the structure factor squared.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_pred*self%F_pred-self%F_exp*self%F_exp)/(self%F_sigma*self%F_sigma)
   end function

   elemental function F2_z2(self) result (res)
   ! Return the square of the z statistic for F2.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_pred*self%F_pred-self%F_exp*self%F_exp)/(self%F_sigma*self%F_sigma)
      res = res*res
   end function

   elemental function F_ratio(self) result (res)
   ! Return the ratio F_exp/F_pred
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_exp/self%F_pred
   end function

   elemental function I_ratio(self) result (res)
   ! Return the ratio I_exp/FIpred
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%I_exp/self%I_pred
   end function

   elemental function F_rel_dev(self) result (res)
   ! Return the relative deviation of the predicted structure factor,
   ! (F_pred - F_exp)/F_exp. Not the atom's f_r, the real part of f'.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_pred-self%F_exp)/self%F_exp
   end function

   elemental function I_rel_dev(self) result (res)
   ! Return the relative deviation of the predicted intensity,
   ! (I_pred - I_exp)/I_exp.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%I_pred-self%I_exp)/self%I_exp
   end function

   elemental function F_exp(self) result (res)
   ! Return the F_exp ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_exp
   end function

   elemental function I_exp(self) result (res)
   ! Return the I_exp ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%I_exp
   end function

   elemental function F2_exp(self) result (res)
   ! Return the F_exp^2 ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_exp
      res = res*res
   end function

   elemental function F_exp_on_sigma(self) result (res)
   ! Return the F_exp/F_sigma
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = abs(self%F_exp/self%F_sigma)
   end function

   elemental function I_exp_on_sigma(self) result (res)
   ! Return the I_exp/I_sigma
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = abs(self%I_exp/self%I_sigma)
   end function

   elemental function F_exp_on_sigma_2(self) result (res)
   ! Return the F_exp/F_sigma
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_exp/self%F_sigma)
      res = res*res
   end function

   elemental function F_exp_on_sigma_4(self) result (res)
   ! Return the F_exp/F_sigma
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = (self%F_exp/self%F_sigma)
      res = res*res*res*res
   end function

   elemental function F_sigma(self) result (res)
   ! Return the F_sigma ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_sigma
   end function

   elemental function F2_sigma(self) result (res)
   ! Return the F2_sigma
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_sigma*self%F_sigma
   end function

   elemental function I_sigma(self) result (res)
   ! Return the F_sigma ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%I_sigma
   end function

   elemental function F_phase(self) result (res)
   ! Return the complex unit phase of F_calc.
      type(REFLECTION_TYPE), IN :: self
      CPX :: res
      res = self%F_calc/abs(self%F_calc)
   end function

   elemental function F_phase_without_disp(self) result (res)
   ! Return the complex unit phase of F_calc.
      type(REFLECTION_TYPE), IN :: self
      CPX :: res
      CPX :: val

      ! Remove dispersion.
      ! If there is none, or it was removed using
      ! remove_disp_from_F_exp, then below does nothing.
      val = self%F_calc-self%F_disp
      res = val/abs(val)
   end function

   elemental function F_phase_angle(self) result (res)
   ! Return the phase angle of the calculated complex structure factor.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = atan2(aimag(self%F_calc),real(self%F_calc))
   end function

   elemental function F_phase_angle_in_degrees(self) result (res)
   ! Return the phase angle of the calculated complex structure factor.
   ! In degrres.
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = atan2(aimag(self%F_calc),real(self%F_calc))*DEGREE_PER_RADIAN
!     if (res<ZERO) res = 360.0d0 + res
   end function

   elemental function stl(self) result (res)
   ! Return the value of sin(theta)/lambda in au^-1 for reflection n.
   ! NOTE: wavelength and stl must have been previously set!
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%stl
   end function

   elemental function F2_calc_abs(self) result (res)
   ! Return the F_pred ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = abs(self%F_calc)
      res = res*res
   end function

   elemental function F_calc_abs(self) result (res)
   ! Return the magnitude of F_calc
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = abs(self%F_calc)
   end function

   elemental function F_pred(self) result (res)
   ! Return the F_pred ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%F_pred
   end function

   elemental function I_pred(self) result (res)
   ! Return the I_pred ... for get_from
      type(REFLECTION_TYPE), IN :: self
      REAL :: res
      res = self%I_pred
   end function


! For pruning

   elemental function has_small_F_calc_abs(self,cutoff) result (res)
   ! Return TRUE if the reflection has a small/zero F_calc w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = abs(self%F_calc)<=cutoff
   end function

   elemental function has_zero_F_calc_abs(self,cutoff) result (res)
   ! Return TRUE if the reflection has a zero F_calc w.r.t. "cutoff"
   ! For pruning systematic absences.
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = abs(self%F_calc)<=cutoff
   end function

   elemental function has_small_F_pred(self,cutoff) result (res)
   ! Return TRUE if the reflection has a small/zero F_pred w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = abs(self%F_pred)<=cutoff
   end function

   elemental function has_weak_F_exp(self,cutoff) result (res)
   ! Return TRUE if the reflection is weak in F_exp w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = F_exp_on_sigma_(self)<=cutoff
   end function

   elemental function has_weak_I_exp(self,cutoff) result (res)
   ! Return TRUE if the reflection is weak in I_exp w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = I_exp_on_sigma_(self)<=cutoff
   end function

   elemental function has_small_F_exp(self,cutoff) result (res)
   ! Return TRUE if the reflection has a small F_exp w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = self%F_exp<=cutoff
   end function

   elemental function has_small_I_exp(self,cutoff) result (res)
   ! Return TRUE if the reflection has a small I_exp w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = self%I_exp<=cutoff
   end function

   elemental function has_small_F_sigma(self,cutoff) result (res)
   ! Return TRUE if the reflection has a small F_sigma w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = self%F_sigma<=cutoff
   end function

   elemental function has_small_I_sigma(self,cutoff) result (res)
   ! Return TRUE if the reflection has a zero I_sigma w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = self%I_sigma<=cutoff
   end function

   elemental function has_large_F_z(self,cutoff) result (res)
   ! Return TRUE if the reflection has a large F_z w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = F_z_(self)>=cutoff
   end function

   elemental function has_large_abs_F_z(self,cutoff) result (res)
   ! Return TRUE if the reflection has a large |F_z| w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = abs(F_z_(self))>=cutoff
   end function

   elemental function has_large_I_z(self,cutoff) result (res)
   ! Return TRUE if the reflection has a large I_z w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = I_z_(self)>=cutoff
   end function

   elemental function has_large_abs_I_z(self,cutoff) result (res)
   ! Return TRUE if the reflection has a large |I_z| w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = abs(I_z_(self))>=cutoff
   end function

   elemental function has_high_stl(self,cutoff) result (res)
   ! Return TRUE if the reflection has a high s.t.l w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = self%stl>=cutoff
   end function

   elemental function has_low_stl(self,cutoff) result (res)
   ! Return TRUE if the reflection has a low s.t.l w.r.t. "cutoff"
      type(REFLECTION_TYPE), IN :: self
      REAL, IN :: cutoff
      BIN :: res
      res = self%stl<cutoff
   end function


!  =========================
!  Methods that do something
!  =========================

   subroutine remove_anom_from_F_exp(self,scale_factor)
   ! Remove the anomalous dispersion correction from F_exp
   ! assuming the phase from F_calc is correct.
      type(REFLECTION_TYPE), INOUT :: self
      REAL, IN :: scale_factor
      CPX :: phase,F_corr,II

      ! Corrected complex F_corr
      II = IMAGIFY(ONE)
      phase = F_phase_(self)
      F_corr = self%F_exp*phase-self%F_disp*scale_factor

      ! Magnitude of complex F_corr, the experimentally
      ! corrected F_exp.
      self%F_corr = abs(F_corr)
   end subroutine

   PURE subroutine remove_anom_from_F_calc(self)
   ! Remove the anomalous dispersion correction from F_calc
   ! assuming the phase from F_calc is correct.
      type(REFLECTION_TYPE), INOUT :: self
      self%F_calc = self%F_calc-self%F_disp
   end subroutine

end module
