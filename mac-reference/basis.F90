!==================================================================
! BASIS: For gaussian basis sets
!
! Copyright (C) Dylan Jayatilaka, 1998
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
module BASIS_MODULE

#  include "macros"

   include "basis.use"

   implicit none

   include "basis.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create(self)
   ! Allocate an object
      type(BASIS_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"BASIS:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(BASIS_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"BASIS:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the shell parts of self
      type(BASIS_TYPE), INOUT :: self
      call destroy_(self%shell)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_spherical(self,val)
   ! Set sphericals to "val" defaults
      type(BASIS_TYPE), INOUT :: self
      BIN, IN :: val
      self%is_spherical = val
      if (allocated(self%shell)) call set_spherical_(self%shell,val)
      call update_(self)
   end subroutine

   pure subroutine update(self)
   ! Update the shell data, if it exists
      type(BASIS_TYPE), INOUT :: self
      if (NOT allocated(self%shell)) return
      self%n_shell = no_of_shells_(self)
      self%n_bf = no_of_basis_functions_(self)
      self%n_prim = no_of_primitives_(self)
   end subroutine

   subroutine set_label(self,label)
   ! Set the basis label
      type(BASIS_TYPE), INOUT :: self
      STR(len=*), IN :: label
      self%label = label
   end subroutine


!  ============
!  I/O Routines
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(BASIS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","BASIS:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"BASIS:read_keywords ... unexpected end of file while reading list keywords")
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
      type(BASIS_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}            ")  ! exit surrounding loop
      case ("gamess-us=   "); call read_gamess_us_(self)
      case ("junk=        "); call read_junk_(self)
      case ("label=       "); call read_label_(self)
      case ("shells=      "); call read_shell_(self)
      case ("units=       "); call read_units_(self)
      case ("put          "); call put_(self)
      ! These are only for making custom tables for the list type
      case ("put_label    "); call put_(stdout,self%label,int_width=TRUE)
      case ("put_n_shells "); call put_(stdout,self%n_shell)
      case ("put_n_bf     "); call put_(stdout,self%n_bf)
      case ("put_n_prim   "); call put_(stdout,self%n_prim)
      case ("tonto_style= "); call read_tonto_style_(self)
      case ("turbomole=   "); call read_turbomole_(self)
      case ("flush        "); call flush_(stdout)
      case default; allocate(tonto%known_keywords(14))
      tonto%known_keywords(1) = "}            "
      tonto%known_keywords(2) = "gamess-us=   "
      tonto%known_keywords(3) = "junk=        "
      tonto%known_keywords(4) = "label=       "
      tonto%known_keywords(5) = "shells=      "
      tonto%known_keywords(6) = "units=       "
      tonto%known_keywords(7) = "put          "
      tonto%known_keywords(8) = "put_label    "
      tonto%known_keywords(9) = "put_n_shells "
      tonto%known_keywords(10) = "put_n_bf     "
      tonto%known_keywords(11) = "put_n_prim   "
      tonto%known_keywords(12) = "tonto_style= "
      tonto%known_keywords(13) = "turbomole=   "
      tonto%known_keywords(14) = "flush        "
      call unknown_(tonto,word,"BASIS:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_gamess_us(self)
   ! Create and read a GAMESS-US style basis set
      type(BASIS_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: the_keys
      call read_label_(self)
      the_keys = split_(("l_chr= n_cc= junk,ex,cc="))
      call set_keys_(self%shell,the_keys)
      call destroy_(the_keys)
      call read_data_(self%shell)
      call update_(self)
   end subroutine

   subroutine read_junk(self)
   ! Read in a junk string, useful for ignoring a field
      type(BASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call skip_next_item_(stdin)
   end subroutine

   subroutine read_label(self)
   ! Read only the basis label
      type(BASIS_TYPE), INOUT :: self
      call read_(stdin,self%label)
      ENSURE(is_alphabetical_(self%label(1:1)),"BASIS:read_label ... label must begin with alphabetical character")
   end subroutine

   subroutine read_shell(self)
   ! Read a shell
      type(BASIS_TYPE), INOUT :: self
      call read_list_keywords_(self%shell)
      call update_(self)
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(BASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_tonto_style(self)
   ! Create and read a tonto style basis set
      type(BASIS_TYPE), INOUT :: self
      call read_label_(self)
      call set_keys_(self%shell,["l_chr=","n_cc= ","ex,cc="])
      call read_data_(self%shell)
      call update_(self)
   end subroutine

   subroutine read_turbomole(self)
   ! Create and read a turbomole style basis set
      type(BASIS_TYPE), INOUT :: self
      call read_label_(self)
      call set_keys_(self%shell,["n_cc= ","l_chr=","ex,cc="])
      call read_data_(self%shell)
      call update_(self)
   end subroutine

   subroutine read_molden(self)
   ! Create and read a Molden style basis set
      type(BASIS_TYPE), INOUT :: self
      call read_junk_(self)
      call read_junk_(self)
      call set_keys_(self%shell,["l_chr=","n_cc= ","junk= ","ex,cc="])
      ! Note BL termination
      call read_data_BL_terminated_(self%shell)
      call update_(self)
   end subroutine


!  ====================
!  Key related routines
!  ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(BASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(BASIS_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"BASIS:process_keys ... no keys")

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
            DIE_IF(l==0,"BASIS:process_keys ... no matching closing brace in embedded keyword")

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
      type(BASIS_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(BASIS_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(BASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


!  =======
!  Methods
!  =======

   PURE subroutine resolve_by_label(self,label,basis,clobber,minmatch,found)
   ! Resolve "self" by *pointer* assigning it to the element in
   ! "basis" which has a label which matches "label". 
   ! . If "clobber" is present and FALSE, and if "self" is already
   !   assigned to a basis with non-blank label, then nothing is done.
   !   Otherwise (by default) "self" is pointer assigned regardless.
   ! . If "minmatch" is present, then a match is made to the number of
   !   characters in "label" even if there are more characters in
   !   "basis.label". This is useful for matching e.g. label "Mg:" 
   !   to basis.label "Mg:gaussian"
   ! . If "found" is present, it is set TRUE if "self" is resolved (or
   !   was already resolved if clobber was set FALSE). Otherwise it is
   !   set FALSE. If "found" is not present, and a match has not been
   !   found, the routine dies.
      type(BASIS_TYPE), allocatable, INOUT :: self
      STR(len=*), IN :: label
      VEC(type(BASIS_TYPE),:), allocatable, IN :: basis
      BIN, optional, IN :: clobber
      BIN, optional, IN :: minmatch
      BIN, optional, OUT :: found
      INT :: b
      VEC(BIN,:), allocatable :: check
      BIN :: bash,fnd,minmtch
   ENSURE(allocated(basis),"BASIS:resolve_by_label ... no basis set")
      bash = TRUE
      if (present(clobber)) bash = clobber

      ! If we don't clobber and the basis is created and it has a
      ! label, then it is already resolved ... so return.
      if (NOT bash) then
         if (allocated(self)) then
            if (self%label/=" ") then
               if (present(found)) found = TRUE
               return
            end if
         end if
      end if

      ! Find out if we are doing a minimal match
      minmtch = FALSE
      if (present(minmatch)) minmtch = minmatch

      ! Find where the matching label is ...
      call create_(check,size(basis))
      if (minmtch) then; check = basis%label(1:len_trim(label))==label
      else; check = basis%label==label
      end if
      b = index_of_first_true_element_(check)
      call destroy_(check)

      ! If there was a match, pointer assign the basis
      ! otherwise clobber/nullify it.
      fnd = b>0
      if (fnd) self = basis(b)

      ! Assign "found" or die
      if (present(found)) then; found = fnd
      else; ENSURE(fnd,"BASIS:resolve_by_label ... unknown basis label, "//trim(label))
      end if
   end subroutine

   PURE subroutine unnormalize(self)
   ! Take the normalisation factors out of the primitives, assuming
   ! that the contraction coeff's refer to normalized basis functions
      type(BASIS_TYPE), INOUT :: self
      if (allocated(self%shell)) call unnormalize_(self%shell)
   end subroutine

   PURE subroutine renormalize(self)
   ! Put back in the normalisation factors of the primitives, assuming
   ! that the contraction coeff's refer to unnormalized basis functions
      type(BASIS_TYPE), INOUT :: self
      if (allocated(self%shell)) call renormalize_(self%shell)
   end subroutine

   PURE subroutine get_lc_bf_ex_cc(self,lc,bf,ex,cc)
   ! Get the l character value symbols, exponents and contractions for
   ! each *primitive* shell
      type(BASIS_TYPE), IN :: self
      VEC(STR(len=*),:), OUT :: lc,bf
      VEC(REAL,:), OUT :: ex,cc
      INT :: s,c,i,b
   ENSURE(size(lc)==no_of_primitive_shells_(self),"BASIS:get_lc_bf_ex_cc ... wrong size, lc")
   ENSURE(size(bf)==size(lc),"BASIS:get_lc_bf_ex_cc ... wrong size, bf")
   ENSURE(size(ex)==size(lc),"BASIS:get_lc_bf_ex_cc ... wrong size, ex")
   ENSURE(size(cc)==size(lc),"BASIS:get_lc_bf_ex_cc ... wrong size, cc")
      lc = " "
      bf = " "
      i = 1
      b = 1
      do s = 1,self%n_shell
         lc(i) = self%shell(s)%l_chr
         bf(i) = to_str_(b)
         do c = 1,self%shell(s)%n_cc
            ex(i) = self%shell(s)%exponent(c)
            cc(i) = self%shell(s)%contraction(c)
            i = i+1
         end do
         b = b+self%shell(s)%n_bf
      end do
   end subroutine

   PURE function contraction_mx(self) result (ccm)
   ! Set the contraction coefficient matrix. Its dimensions are (.n_prim,.n_bf).
      type(BASIS_TYPE), IN :: self
      MAT(REAL,self%n_prim,self%n_bf) :: ccm
      INT :: b,p,n_p,n_b,s
      p = 0  ! no of primitives in all previous shells
      b = 0  ! basis number
      ccm = ZERO
      do s = 1,self%n_shell  ! For segmented basis sets !
         n_b = self%shell(s)%n_comp
         n_p = n_prim_(self%shell(s))
         call make_contraction_mx_(self%shell(s),ccm(p+1:p+n_p,b+1:b+n_b))
         b = b+n_b
         p = p+n_p
      end do
   end function

   PURE subroutine make_contraction_mx(self,ccm)
   ! Set the contraction coefficient matrix "ccm".
   ! Primitives are assumed to have uniform normalisation.
      type(BASIS_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ccm
      INT :: b,p,n_p,n_b,s
   ENSURE(size(ccm,1)==self%n_prim,"BASIS:make_contraction_mx ... wrong 1st dimension, ccm")
   ENSURE(size(ccm,2)==self%n_bf,"BASIS:make_contraction_mx ... wrong 2nd dimension, ccm")
      p = 0  ! no of primitives in all previous shells
      b = 0  ! basis number
      ccm = ZERO
      do s = 1,self%n_shell  ! For segmented basis sets !
         n_b = self%shell(s)%n_comp
         n_p = n_prim_(self%shell(s))
         call make_contraction_mx_(self%shell(s),ccm(p+1:p+n_p,b+1:b+n_b))
         b = b+n_b
         p = p+n_p
      end do
   end subroutine

   PURE subroutine make_normalized_contraction_mx(self,ccm,spherical)
   ! Set the normalized contraction coefficient matrix "ccm".
   ! i.e. primitive components are properly normalized.
      type(BASIS_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: ccm
      BIN, optional, IN :: spherical
      INT :: b,p,n_p,n_b,s
      BIN :: is_sph
      is_sph = self%is_spherical
      if (present(spherical)) is_sph = spherical
      if (is_sph) then
         ENSURE(size(ccm,1)==no_of_sph_primitives_(self),"BASIS:make_normalized_contraction_mx ... wrong 1st dimension, ccm")
         ENSURE(size(ccm,2)==no_of_sph_basis_functions_(self),"BASIS:make_normalized_contraction_mx ... wrong 2nd dimension, ccm")
      else
         ENSURE(size(ccm,1)==self%n_prim,"BASIS:make_normalized_contraction_mx ... wrong 1st dimension, ccm")
         ENSURE(size(ccm,2)==self%n_bf,"BASIS:make_normalized_contraction_mx ... wrong 2nd dimension, ccm")
      end if
      p = 0  ! no of primitives in all previous shells
      b = 0  ! basis number
      ccm = ZERO
      do s = 1,self%n_shell  ! For segmented basis sets !
         if (is_sph) then
            n_b = self%shell(s)%n_sph
            n_p = n_sph_prim_(self%shell(s))
         else
            n_b = self%shell(s)%n_comp
            n_p = n_prim_(self%shell(s))
         end if
         call make_normalized_contraction_mx_(self%shell(s),ccm(p+1:p+n_p,b+1:b+n_b),is_sph)
         b = b+n_b
         p = p+n_p
      end do
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   pure function same_as(self,b) result (res)
   ! Return TRUE if the basis set "self" is the same as "b". Only the
   ! shell vector is compared to see if they are "really" the same.
      type(BASIS_TYPE), IN :: self
      type(BASIS_TYPE), IN :: b
      BIN :: res
      res = same_as_(self%shell,b%shell)
   end function

   pure function is_resolved(self) result (res)
   ! Return TRUE if self is resolved i.e. has shells
      type(BASIS_TYPE), IN :: self
      BIN :: res
      res = allocated(self%shell)
   end function

   pure function is_created_and_resolved(self) result (res)
   ! Return TRUE if self is resolved
      type(BASIS_TYPE), allocatable, IN :: self
      BIN :: res
      res = allocated(self)
      if (res) res = allocated(self%shell)
   end function

   pure function is_created_and_has_label(self) result (res)
   ! Return TRUE if self has a non-blank label
      type(BASIS_TYPE), allocatable, IN :: self
      BIN :: res
      res = allocated(self)
      if (res) res = self%label/=" "
   end function

   pure function no_of_shells(self) result (res)
   ! Work out and return the number of shells in the basis set
      type(BASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")
      res = size(self%shell)
   end function

   pure function no_of_primitive_shells(self) result (res)
   ! Work out and return the number of primitive shells in the basis set
      type(BASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")
      res = no_of_primitive_shells_(self%shell)
   end function

   pure function no_of_basis_functions(self) result (res)
   ! Work out and return the number of basis functions in the basis set
      type(BASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")
      res = no_of_basis_functions_(self%shell)
   end function

   pure function no_of_sph_basis_functions(self) result (res)
   ! Work out and return the number of spherical basis functions in the basis set
      type(BASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")
      res = no_of_sph_basis_functions_(self%shell)
   end function

   pure function no_of_primitives(self) result (res)
   ! Work out and return the number of primitives in the basis set
      type(BASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")
      res = no_of_primitives_(self%shell)
   end function

   pure function no_of_sph_primitives(self) result (res)
   ! Work out and return the number of spherical primitives in the basis set
      type(BASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")
      res = no_of_sph_primitives_(self%shell)
   end function

   PURE function xyz_basis_fn_part(self) result (res)
   ! Return a character representation for the cartesian product part
   ! of the gaussians in this shell.
      type(BASIS_TYPE), IN :: self
      VEC(STR(len=STR_SIZE),no_of_basis_functions(self)) :: res
      INT :: s,f,l
   ENSURE(NOT self%is_spherical,"BASIS:xyz_basis_fn_part ... not yet available for sphericals")
   ENSURE(allocated(self%shell),"BASIS:xyz_basis_fn_part ... no shells")
      l = 0
      do s = 1,size(self%shell)
         f = l+1
         l = l+self%shell(s)%n_comp
         res(f:l) = xyz_basis_fn_part_(self%shell(s))
      end do
   end function


!  ==============================
!  Exponents and cutoff distances
!  ==============================

!   min_exponent result (res) :: PURE
!   ! Return the minimum exponent in the basis.
!      self :: IN
!      res :: REAL
!
!   ENSURE(.shell.allocated,"no shells")
!
!      res = .shell.min_exponent
!
!   end

   pure function r_max(self,cutoff) result (res)
   ! Return the radius where all basis functions becomes smaller than "cutoff".
      type(BASIS_TYPE), IN :: self
      REAL, IN :: cutoff
      REAL :: res
      res = r_max_(self%shell,cutoff)
   end function

   pure function r2_max(self,cutoff) result (res)
   ! Return the radius where all basis functions becomes smaller than "cutoff".
      type(BASIS_TYPE), IN :: self
      REAL, IN :: cutoff
      REAL :: res
      res = r2_max_(self%shell,cutoff)
   end function


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(BASIS_TYPE), IN :: self
      call dump_(stdout,"label",self%label)
      call dump_(stdout,"is_spherical",self%is_spherical)
      call dump_(stdout,"n_shell",self%n_shell)
      call dump_(stdout,"n_bf",self%n_bf)
      call dump_(stdout,"n_prim",self%n_prim)
      call dmpa_(self%shell,"shell")
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(BASIS_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":BASIS@ = { ")
      else; call text_(stdout,trim(object_name)//":BASIS = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(BASIS_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":BASIS@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ===
!  Put
!  ===

   subroutine put(self)
   ! Put out the basis information to file "stdout"
      type(BASIS_TYPE), IN :: self
      VEC(REAL,:), allocatable :: ex,cc
      VEC(STR(len=STR_SIZE),:), allocatable :: lc,bf
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: ns
      call flush_(stdout)
      call text_(stdout,"Basis set "//trim(self%label))
      call flush_(stdout)
      if (self%is_spherical) call show_(stdout,"Spherical harmonics?   =",self%is_spherical)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)

      ! Get table column data
      ns = no_of_primitive_shells_(self)
      call create_(lc,ns)
      call create_(bf,ns)
      call create_(ex,ns)
      call create_(cc,ns)
      call get_lc_bf_ex_cc_(self,lc,bf,ex,cc)

      ! Table headings
      call create_(table,4)
      call set_heading_(table(1),"-L-")
      call set_center_(table(1),TRUE)
      call set_heading_(table(2),"Fn")
      call set_subhead_(table(2),"#")
      call set_heading_(table(3),"Exponent")
      call set_subheading_(table(3),"/au")
      call set_heading_(table(4),"Contraction")
      call set_subheading_(table(4),"/au")

      ! Set table column data
      call set_values_(table(1),lc)
      call set_values_(table(2),bf)
      call set_values_(table(3),ex)
      call set_values_(table(4),cc)

      ! Make the table
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(cc)
      call destroy_(ex)
      call destroy_(bf)
      call destroy_(lc)
   end subroutine

end module
