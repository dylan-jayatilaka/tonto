!==================================================================
!
! SHELL: used to describe contracted cartesian gaussian shells.
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
!==================================================================
module SHELL_MODULE

#  include "macros"

   include "shell.use"

   implicit none

   include "shell.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(SHELL_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SHELL:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SHELL_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SHELL:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy pointer part of a shell object
      type(SHELL_TYPE), INOUT :: self
      call destroy_(self%exponent)
      call destroy_(self%contraction)
   end subroutine

!  ===========
!  Set methods
!  ===========

   PURE subroutine set_l(self,l)
   ! Set the l value
      type(SHELL_TYPE), INOUT :: self
      INT, IN :: l

      ! The following code is inherited from SHELL
      self%l = l
      self%l_chr = l_chr(self%l)
      self%first_gaussian = n_comp_up_to_(l-1)+1
      self%last_gaussian = n_comp_up_to_(l)
      call set_n_comp_(self)
   end subroutine

   pure subroutine set_n_comp(self)
   ! Set the number of components. "L" shells are a special case.
      type(SHELL_TYPE), INOUT :: self

      ! The following code is inherited from SHELL
      if (self%l_chr=="L") then; self%n_comp = 4
      else; self%n_comp = n_comp_(self%l)
      end if
      self%n_sph = 2*self%l+1
      if (self%is_spherical) then; self%n_bf = self%n_sph
      else; self%n_bf = self%n_comp
      end if
   end subroutine

   pure subroutine set_spherical(self,val)
   ! Set sphericals to "val"
      type(SHELL_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from SHELL
      self%is_spherical = val
      call set_n_comp_(self)
   end subroutine


!  ============
!  I/O Routines
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(SHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","SHELL:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"SHELL:read_keywords ... unexpected end of file while reading list keywords")
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
      type(SHELL_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}            ")  ! exit surrounding loop
      case ("contractions="); call read_contractions_(self)
      case ("exponents=   "); call read_exponents_(self)
      case ("ex,cc=       "); call read_ex_cc_(self)
      case ("junk=        "); call read_junk_(self)
      case ("junk,ex,cc=  "); call read_junk_ex_cc_(self)
      case ("l=           "); call read_l_(self)
      case ("l_chr=       "); call read_l_chr_(self)
      case ("n_cc=        "); call read_n_cc_(self)
      case ("put          "); call put_(self)
      case ("spherical=   "); call read_spherical_(self)
      case ("units=       "); call read_units_(self)
      case default; allocate(tonto%known_keywords(12))
      tonto%known_keywords(1) = "}            "
      tonto%known_keywords(2) = "contractions="
      tonto%known_keywords(3) = "exponents=   "
      tonto%known_keywords(4) = "ex,cc=       "
      tonto%known_keywords(5) = "junk=        "
      tonto%known_keywords(6) = "junk,ex,cc=  "
      tonto%known_keywords(7) = "l=           "
      tonto%known_keywords(8) = "l_chr=       "
      tonto%known_keywords(9) = "n_cc=        "
      tonto%known_keywords(10) = "put          "
      tonto%known_keywords(11) = "spherical=   "
      tonto%known_keywords(12) = "units=       "
      call unknown_(tonto,word,"SHELL:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(SHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_junk(self)
   ! Read in a junk string, useful for ignoring a field
      type(SHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call skip_next_item_(stdin)
   end subroutine

   subroutine read_l(self)
   ! Read in the l symbol
      type(SHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else; call read_l_chr_(self)
      end if
   end subroutine

   subroutine read_l_int(self)
   ! Read in the l integer. No way to get "L" shells in this input mode.
      type(SHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      ENSURE(is_int_(word),"SHELL:read_l_int ... expecting an integer for L")
      call set_l_(self,to_int_(word))
   end subroutine

   subroutine read_l_chr(self)
   ! Read in the l symbol
      type(SHELL_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      STR(len=1) :: l_c
      INT :: l
      call read_(stdin,word)
      DIE_IF(len_trim(word)/=1,"SHELL:read_l_chr ... unknown L symbol")
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

   subroutine read_spherical(self)
   ! Read in if this is a spherical shell
      type(SHELL_TYPE), INOUT :: self
      call read_(stdin,self%is_spherical)
   end subroutine

   subroutine read_n_cc(self)
   ! Read in the number of contraction coefficients
      type(SHELL_TYPE), INOUT :: self
      call read_(stdin,self%n_cc)
      ENSURE(self%n_cc>0,"SHELL:read_n_cc ... n_cc must be positive")
   end subroutine

   subroutine read_exponents(self)
   ! Read in the exponents
      type(SHELL_TYPE), INOUT :: self
   ENSURE(self%n_cc>0,"SHELL:read_exponents ... n_cc must be entered first")
   ENSURE(NOT allocated(self%exponent),"SHELL:read_exponents ... ex already entered")
      call create_(self%exponent,self%n_cc)
      call read_(stdin,self%exponent)
   end subroutine

   subroutine read_contractions(self)
   ! Read in the contraction coefficients
      type(SHELL_TYPE), INOUT :: self
   ENSURE(self%n_cc>0,"SHELL:read_contractions ... n_cc must be entered first")
   ENSURE(NOT allocated(self%contraction),"SHELL:read_contractions ... cc already entered")
      call create_(self%contraction,self%n_cc)
      call read_(stdin,self%contraction)
   end subroutine

   subroutine read_ex_cc(self)
   ! Read in the exponents and contractions
      type(SHELL_TYPE), INOUT :: self
   ENSURE(self%n_cc>0,"SHELL:read_ex_cc ... n_cc must be entered first")
   ENSURE(NOT allocated(self%exponent),"SHELL:read_ex_cc ... ex already entered")
   ENSURE(NOT allocated(self%contraction),"SHELL:read_ex_cc ... ex already entered")
      call create_(self%exponent,self%n_cc)
      call create_(self%contraction,self%n_cc)
      call read_pair_(stdin,self%exponent,self%contraction)
   end subroutine

   subroutine read_junk_ex_cc(self)
   ! Read in the exponents and contractions preceded by a junk string
      type(SHELL_TYPE), INOUT :: self
      INT :: i
   ENSURE(self%n_cc>0,"SHELL:read_junk_ex_cc ... n_cc must be entered first")
   ENSURE(NOT allocated(self%exponent),"SHELL:read_junk_ex_cc ... ex already entered")
   ENSURE(NOT allocated(self%contraction),"SHELL:read_junk_ex_cc ... ex already entered")
      call create_(self%exponent,self%n_cc)
      call create_(self%contraction,self%n_cc)
      do i = 1,self%n_cc
         call skip_next_item_(stdin)
         call read_(stdin,self%exponent(i))
         call read_(stdin,self%contraction(i))
      end do
   end subroutine


!  ====================
!  Key related routines
!  ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(SHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(SHELL_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"SHELL:process_keys ... no keys")

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
            DIE_IF(l==0,"SHELL:process_keys ... no matching closing brace in embedded keyword")

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
      type(SHELL_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(SHELL_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(SHELL_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


!  =====================
!  Miscellaneous methods
!  =====================

   PURE function l_chr(l) result (res)
   ! Return a character representation for angular mtm "l"
   ! Of course, "L" shells are never returned: they have mixed mtm.
      INT, IN :: l
      STR(len=1) :: res
   ENSURE(l>=0 AND l<=23,"SHELL:l_chr ... angular momentum too large:"//trim(to_str_(l)))
      select case (l)
      case (0); res = "s"
      case (1); res = "p"
      case (2); res = "d"
      case (3); res = "f"
      case (4); res = "g"
      case default; res = achar(l-4+iachar("g"))
      end select
   end function

   PURE function xyz_basis_fn_part(self) result (res)
   ! Return a character representation for the cartesian
   ! product part of the gaussians in this shell.
      type(SHELL_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),self%n_comp) :: res
      INT :: i,width,spaces
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
   ENSURE(NOT self%is_spherical,"SHELL:xyz_basis_fn_part ... not yet available for sphericals")
      labels = xyz_product_str_for_(self%l)
      width = maxval(len_trim(labels))
      do i = 1,self%n_comp
         spaces = self%l+1-width
         res(i) = trim(self%l_chr)//repeat(" ",spaces)//trim(labels(i))
      end do
   end function

   pure function same_as(self,sh) result (same)
   ! Return TRUE if the shell "self" is the same as "sh".
      type(SHELL_TYPE), IN :: self
      type(SHELL_TYPE), IN :: sh
      BIN :: same

      ! Check if basic arrays are created identically
      same = self%l_chr==sh%l_chr AND (self%is_spherical EQV sh%is_spherical) AND (allocated(self%exponent) EQV allocated(sh%exponent)) AND (allocated(self%contraction) EQV allocated(sh%contraction))

      ! If not, we are done
      if (NOT same) return

      ! Check exponents
      if (allocated(self%exponent) AND allocated(sh%exponent)) same = same_as_(self%exponent,sh%exponent)
      if (NOT same) return

      ! Check contractions ...
      if (allocated(self%contraction) AND allocated(sh%contraction)) same = same_as_(self%contraction,sh%contraction)
   end function


!  ============
!  Contractions
!  ============

   PURE subroutine make_contraction_mx(self,ccm)
   ! Return the contraction coefficient matrix "ccm" which converts
   ! primitives to basis functions. It is with respect to uniform
   ! normalisation i.e. all components are normalized to the first
   ! component, x^l. 
      type(SHELL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ccm
      INT :: b,p,i
   ENSURE(size(ccm,1)==n_prim_(self),"SHELL:make_contraction_mx ... wrong 1st dimension, ccm")
   ENSURE(size(ccm,2)==self%n_comp,"SHELL:make_contraction_mx ... wrong 2nd dimension, ccm")
   ENSURE(self%l_chr/="L","SHELL:make_contraction_mx ... must not be an L=S+P shell")
   ENSURE(self%n_cc>0,"SHELL:make_contraction_mx ... no contraction coefficients")
   ENSURE(allocated(self%contraction),"SHELL:make_contraction_mx ... no contraction coefficients")
      do b = 1,self%n_comp  ! do over basis components
         p = b  ! primitive p
         do i = 1,self%n_cc  ! do over contractions
            ccm(p,b) = self%contraction(i)
            p = p+self%n_comp
         end do
      end do
   end subroutine

   PURE subroutine make_normalized_contraction_mx(self,ccm,sph)
   ! Return the contraction coefficient matrix "ccm". All the
   ! primitives are assumed correctly normalized.
      type(SHELL_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ccm
      BIN, optional, IN :: sph
      INT :: b,p,i
      BIN :: is_sph
   ENSURE(self%n_cc>0,"SHELL:make_normalized_contraction_mx ... no contraction coefficients")
   ENSURE(self%l_chr/="L","SHELL:make_normalized_contraction_mx ... the angular momentum is wrong, L=S+P does not works")
   ENSURE(allocated(self%contraction),"SHELL:make_normalized_contraction_mx ... no contraction coefficients")
      is_sph = FALSE
      if (present(sph)) is_sph = sph
      if (is_sph AND self%n_sph/=self%n_comp) then
         ENSURE(size(ccm,1)==n_sph_prim_(self),"SHELL:make_normalized_contraction_mx ... wrong 1st dimension, ccm")
         ENSURE(size(ccm,2)==self%n_sph,"SHELL:make_normalized_contraction_mx ... wrong 2nd dimension, ccm")
         do b = 1,self%n_sph  ! do over basis components
            p = b  ! primitive p
            do i = 1,self%n_cc  ! do over contractions
               ccm(p,b) = self%contraction(i)
               p = p+self%n_sph
            end do
         end do
      else
         ENSURE(size(ccm,1)==n_prim_(self),"SHELL:make_normalized_contraction_mx ... wrong 1st dimension, ccm")
         ENSURE(size(ccm,2)==self%n_comp,"SHELL:make_normalized_contraction_mx ... wrong 2nd dimension, ccm")
         do b = 1,self%n_comp  ! do over basis components
            p = b  ! primitive p
            do i = 1,self%n_cc  ! do over contractions
               ccm(p,b) = self%contraction(i)
               p = p+self%n_comp
            end do
         end do
      end if
   end subroutine


!  =============
!  Normalization
!  =============

   pure function norm(self) result (res)
   ! Return the norm of the shell, assuming that the existing contraction
   ! coefficients are with respect to NORMALISED primitive gaussians.
      type(SHELL_TYPE), IN :: self
      REAL :: res
      INT :: i,j
      REAL :: sm,a,b,ab
      sm = ZERO
      do i = 1,self%n_cc
         a = self%exponent(i)
         do j = 1,i-1
            b = self%exponent(j)
            ab = TWO*sqrt(a*b)/(a+b);
            sm = sm+TWO*self%contraction(i)*self%contraction(j)*ab**(self%l+ONE+HALF);
         end do

         ! This is the normalized diagonal term
         sm = sm+self%contraction(i)*self%contraction(i)
      end do
      res = sqrt(sm)*((TWO*PI)**(THREE/FOUR))
   end function

   pure subroutine unnormalize(self)
   ! Assuming the existing contraction coefficients are initially with respect
   ! to NORMALISED primitive gaussians, take out this normalisation factor of
   ! each primitive and put it in the contraction coefficient. The normalisation
   ! factor removed is the one appropriate for the cartesian component x^l.
   ! NOTE: It is up to you to correct this factor with appropriate double
   ! factorial square roots for other components. The factor should be: 
   !   fac = 1/sqrt(df(nx)*df(ny)*df(nz))
   ! where nx,ny,nz are the cartesian powers of the basis component
      type(SHELL_TYPE), INOUT :: self
      self%contraction(:) = self%contraction(:)*(FOUR*self%exponent(:))**(HALF*self%l+HALF+QUARTER)*(ONE/(norm_(self)*sqrt(double_factorial_(self%l))))
   end subroutine

   pure subroutine renormalize(self)
   ! Assuming the existing contraction coefficients are with respect to raw
   ! UNNORMALISED primitive gaussians, put back this normalisation factor for
   ! each primitive gaussian, by removing this factor from from the contraction
   ! coefficients. The result will be contraction coefficients relative to
   ! normalized primitive gausian. This will undo routine "unnormalize" routine
   ! above.  The ((TWO*PI)**(THREE/FOUR)) / .norm factor is supposed to make the
   ! contraction coefficient equal to 1 for a shell with one primitive i.e. the
   ! shell is not *really* normalized. This seems to be the usual convention.
      type(SHELL_TYPE), INOUT :: self
      self%contraction(:) = self%contraction(:)/((FOUR*self%exponent(:))**(HALF*self%l+HALF+QUARTER))
      self%contraction(:) = self%contraction(:)*((TWO*PI)**(THREE/FOUR))/norm_(self)
   end subroutine


!  =================
!  No. of primitives
!  =================

   pure function n_prim(self) result (res)
   ! Return the number of primitive gaussians in the shell
      type(SHELL_TYPE), IN :: self
      INT :: res
      res = self%n_comp*self%n_cc
   end function

   pure function n_sph_prim(self) result (res)
   ! Return the number of primitive gaussians in the shell
      type(SHELL_TYPE), IN :: self
      INT :: res
      res = self%n_sph*self%n_cc
   end function


!  =======
!  Cutoffs
!  =======

   pure function r_max(self,cutoff) result (res)
   ! Return the radius where all basis functions becomes smaller than "cutoff".
      type(SHELL_TYPE), IN :: self
      REAL, IN :: cutoff
      REAL :: res
      REAL :: alpha
      alpha = ONE/minval(self%exponent)
      if (cutoff<=ZERO) then; res = huge(ONE)
      else; res = sqrt(-alpha*log(cutoff/sum(abs(self%contraction))))
      end if
   end function

   pure function r2_max(self,cutoff) result (res)
   ! Return the radius where all basis functions becomes smaller than "cutoff".
      type(SHELL_TYPE), IN :: self
      REAL, IN :: cutoff
      REAL :: res
      REAL :: alpha
      alpha = ONE/minval(self%exponent)
      if (cutoff<=ZERO) then; res = huge(ONE)
      else; res = -alpha*log(cutoff/sum(abs(self%contraction)))
      end if
   end function


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(SHELL_TYPE), IN :: self
      call dump_(stdout,"l",self%l)
      call dump_(stdout,"l_chr",self%l_chr)
      call dump_(stdout,"is_spherical",self%is_spherical)
      call dump_(stdout,"n_comp",self%n_comp)
      call dump_(stdout,"n_sph",self%n_sph)
      call dump_(stdout,"n_bf",self%n_bf)
      call dump_(stdout,"first_gaussian",self%first_gaussian)
      call dump_(stdout,"last_gaussian",self%last_gaussian)
      call dump_(stdout,"n_cc",self%n_cc)
      call dmpa_(stdout,"exponent",self%exponent)
      call dmpa_(stdout,"contraction",self%contraction)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(SHELL_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":SHELL@ = { ")
      else; call text_(stdout,trim(object_name)//":SHELL = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(SHELL_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":SHELL@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put the shell information to "stdout"
      type(SHELL_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Gaussian type shell")
      call text_(stdout,"===================")
      call flush_(stdout)
      call show_(stdout,"Shell type                  =",self%l_chr)
      call show_(stdout,"L quatum no.                =",self%l)
      call show_(stdout,"is_spherical                =",self%is_spherical)
      call show_(stdout,"No. of cartesian components =",self%n_comp)
      call show_(stdout,"No. of spherical components =",self%n_sph)
      call show_(stdout,"Lexical starting index      =",self%first_gaussian)
      call show_(stdout,"Lexical end index           =",self%last_gaussian)
      call show_(stdout,"No. of contractions         =",self%n_cc)
      call flush_(stdout)
      if (NOT allocated(self%exponent)) return
      if (NOT allocated(self%contraction)) return

      ! Table headings
      call create_(table,2)
      call set_heading_(table(1),"Contraction")
      call set_subhead_(table(1),"coeff.")
      call set_heading_(table(2),"Exponent")
      call set_subhead_(table(2),"/au")

      ! Set table column data
      call set_values_(table(1),self%contraction)
      call set_values_(table(2),self%exponent)

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

end module
