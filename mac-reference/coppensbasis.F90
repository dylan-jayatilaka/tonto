!==================================================================
!
! COPPENSBASIS: 
!
! For a Coppens style fitted relativistic atomic orbital basis.
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
module COPPENSBASIS_MODULE

#  include "macros"

   include "coppensbasis.use"

   implicit none

   include "coppensbasis.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!   ===========
!   Allocation
!   ===========

   PURE subroutine create(self)
   ! Allocate an object
      type(COPPENSBASIS_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"COPPENSBASIS:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(COPPENSBASIS_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"COPPENSBASIS:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(COPPENSBASIS_TYPE), INOUT :: self
      call destroy_(self%orbital)
      call destroy_(self%interpolator)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine update(self)
   ! Update the shell data, if it exists
      type(COPPENSBASIS_TYPE), INOUT :: self
      if (NOT allocated(self%orbital)) return
      self%n_orb = size(self%orbital)
      self%n_prim = no_of_primitives_(self%orbital)
   end subroutine

   PURE subroutine set_label(self,label)
   ! Set the basis label
      type(COPPENSBASIS_TYPE), INOUT :: self
      STR(len=*), IN :: label
      self%label = label
   end subroutine

   pure subroutine set_use_interpolators(self,val)
   ! Set whether to use interpolation table to speed up evaulation.
      type(COPPENSBASIS_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_interpolator = val
   end subroutine

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
      type(COPPENSBASIS_TYPE), allocatable, INOUT :: self
      STR(len=*), IN :: label
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, IN :: basis
      BIN, optional, IN :: clobber
      BIN, optional, IN :: minmatch
      BIN, optional, OUT :: found
      INT :: b
      VEC(BIN,:), allocatable :: check
      BIN :: bash,fnd,minmtch
   ENSURE(allocated(basis),"COPPENSBASIS:resolve_by_label ... no basis set")

      ! The following code is inherited from BASIS
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
      else; ENSURE(fnd,"COPPENSBASIS:resolve_by_label ... unknown basis label, "//trim(label))
      end if
   end subroutine


!  ============
!  I/O Routines
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(COPPENSBASIS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","COPPENSBASIS:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"COPPENSBASIS:read_keywords ... unexpected end of file while reading list keywords")
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
      type(COPPENSBASIS_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                       ")  ! exit surrounding loop
      case ("label=                  "); call read_label_(self)
      case ("orbitals=               "); call read_orbitals_(self)
      case ("put                     "); call put_(self)
      case ("tonto-style=            "); call read_tonto_style_(self)
      case ("units=                  "); call read_units_(self)
      case default; allocate(tonto%known_keywords(6))
      tonto%known_keywords(1) = "}                       "
      tonto%known_keywords(2) = "label=                  "
      tonto%known_keywords(3) = "orbitals=               "
      tonto%known_keywords(4) = "put                     "
      tonto%known_keywords(5) = "tonto-style=            "
      tonto%known_keywords(6) = "units=                  "
      call unknown_(tonto,word,"COPPENSBASIS:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(COPPENSBASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_label(self)
   ! Read only the basis label
      type(COPPENSBASIS_TYPE), INOUT :: self
      call read_(stdin,self%label)
   end subroutine

   subroutine read_orbitals(self)
   ! Read a list of orbitals
      type(COPPENSBASIS_TYPE), INOUT :: self
      call read_list_keywords_(self%orbital)
      call update_(self)
   end subroutine

   subroutine read_tonto_style(self)
   ! Create and read a tonto style basis set
      type(COPPENSBASIS_TYPE), INOUT :: self
      call read_label_(self)
      call set_keys_(self%orbital,["kind= ","occ=  ","n_fun=","n,c,z="])
      call read_data_(self%orbital)
      call update_(self)
   end subroutine


!  ====================
!  Key related routines
!  ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(COPPENSBASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(COPPENSBASIS_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"COPPENSBASIS:process_keys ... no keys")

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
            DIE_IF(l==0,"COPPENSBASIS:process_keys ... no matching closing brace in embedded keyword")

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
      type(COPPENSBASIS_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(COPPENSBASIS_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(COPPENSBASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   pure function is_resolved(self) result (res)
   ! Return TRUE if self is resolved i.e. has shells
      type(COPPENSBASIS_TYPE), IN :: self
      BIN :: res

      ! The following code is inherited from BASIS
      res = allocated(self%orbital)
   end function

   pure function is_created_and_resolved(self) result (res)
   ! Return TRUE if self is resolved
      type(COPPENSBASIS_TYPE), allocatable, IN :: self
      BIN :: res

      ! The following code is inherited from BASIS
      res = allocated(self)
      if (res) res = allocated(self%orbital)
   end function

   pure function is_created_and_has_label(self) result (res)
   ! Return TRUE if self has a non-blank label
      type(COPPENSBASIS_TYPE), allocatable, IN :: self
      BIN :: res

      ! The following code is inherited from BASIS
      res = allocated(self)
      if (res) res = self%label/=" "
   end function

   pure function same_as(self,b) result (res)
   ! Return TRUE if the basis set "self" is the same as "b". Only the
   ! orbital vector is compared to see if they are "really" the same.
      type(COPPENSBASIS_TYPE), IN :: self
      type(COPPENSBASIS_TYPE), IN :: b
      BIN :: res
      res = same_as_(self%orbital,b%orbital)
   end function

   pure function no_of_orbitals(self) result (res)
   ! Work out and return the number of orbitals in the basis set
      type(COPPENSBASIS_TYPE), IN :: self
      INT :: res
      if (allocated(self%orbital)) then; res = size(self%orbital)
      else; res = 0
      end if
   end function

   pure function no_of_primitives(self) result (res)
   ! Work out and return the number of primitive fitting functions in the basis ! set
      type(COPPENSBASIS_TYPE), IN :: self
      INT :: res
      INT :: i
      res = 0
      do i = 1,size(self%orbital)
         res = res+self%orbital(i)%n_fun
      end do
   end function

   pure function maximum_basis_n_value(self) result (res)
   ! Returns the maximum n value over all orbitals in every basis set of the
   ! vector
      type(COPPENSBASIS_TYPE), IN :: self
      INT :: res
      if (allocated(self%orbital)) then; res = maximum_orbital_n_value_(self%orbital)
      else; res = 0
      end if
   end function


!  ==================
!  Density evaluation
!  ==================

   PURE subroutine make_ED_grid(self,density_grid,pt,pos,occ)
   ! Make the electron "density_grid" on a set of points "pt", assuming the
   ! orbitals are at position "pos".
      type(COPPENSBASIS_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ
   ENSURE(size(density_grid)==size(pt,1),"COPPENSBASIS:make_ED_grid ... inconsistent number of points")
      if (self%use_interpolator AND allocated(self%interpolator)) then
         call make_interpolated_ED_grid_(self,density_grid,pt,pos,occ)
      else
         call make_normal_ED_grid_(self,density_grid,pt,pos,occ)
      end if
   end subroutine

   PURE subroutine add_ED_grid(self,density_grid,pt,pos,occ)
   ! Add the electron "density_grid" on a set of points "pt", assuming the
   ! orbitals are at position "pos".
      type(COPPENSBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ
   ENSURE(size(density_grid)==size(pt,1),"COPPENSBASIS:add_ED_grid ... inconsistent number of points")
      if (self%use_interpolator AND allocated(self%interpolator)) then
         call add_interpolated_density_(self,density_grid,pt,pos,occ)
      else
         call add_normal_density_grid_(self,density_grid,pt,pos,occ)
      end if
   end subroutine

   PURE subroutine make_normal_ED_grid(self,density_grid,pt,pos,occ)
   ! Make the normal (uninterpolated) "density_grid" for the supplied points
   ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
   ! the orbital is at position "pos".
      type(COPPENSBASIS_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density_grid.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.orbital.associated,"no orbital vector")
      VEC(REAL,:), allocatable :: R
      INT :: n_pt,n
      REAL :: x,y,z
      BIN :: do_occ
      n_pt = size(pt,1)
      call create_(R,n_pt)
      do n = 1,n_pt
         x = pt(n,1)-pos(1)
         y = pt(n,2)-pos(2)
         z = pt(n,3)-pos(3)
         R(n) = sqrt(x*x+y*y+z*z)
      end do

      ! Do occupation factor?
      do_occ = FALSE
      if (present(occ)) then
         if (NOT equals_(occ,ONE)) then
            do_occ = TRUE
         end if
      end if

      ! Now get the density values
      if (do_occ) then; density_grid = occ*densities_at_radii_(self%orbital,R)
      else; density_grid = densities_at_radii_(self%orbital,R)
      end if

      ! Clean up
      call destroy_(R)
   end subroutine

   PURE subroutine add_normal_density_grid(self,density_grid,pt,pos,occ)
   ! Add the normal (uninterpolated) "density_grid" for the supplied points
   ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
   ! the orbital is at position "pos".
      type(COPPENSBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density_grid.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.orbital.associated,"no orbital vector")
      VEC(REAL,:), allocatable :: R
      INT :: n_pt,n
      REAL :: x,y,z
      BIN :: do_occ
      n_pt = size(pt,1)
      call create_(R,n_pt)
      do n = 1,n_pt
         x = pt(n,1)-pos(1)
         y = pt(n,2)-pos(2)
         z = pt(n,3)-pos(3)
         R(n) = sqrt(x*x+y*y+z*z)
      end do

      ! Do occupation factor?
      do_occ = FALSE
      if (present(occ)) then
         if (NOT equals_(occ,ONE)) then
            do_occ = TRUE
         end if
      end if

      ! Now get the density values
      if (do_occ) then; density_grid = density_grid+occ*densities_at_radii_(self%orbital,R)
      else; density_grid = density_grid+densities_at_radii_(self%orbital,R)
      end if

      ! Clean up
      call destroy_(R)
   end subroutine

   PURE subroutine make_interpolated_ED_grid(self,density,pt,pos,occ)
   ! Make the interpolated "density" for the supplied points "pt" from the real
   ! slater atomic orbitals assuming the orbitals are at position "pos".
      type(COPPENSBASIS_TYPE), IN :: self
      VEC(REAL,:), OUT :: density
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.interpolator.associated,"no interpolator defined!")
      VEC(REAL,:), allocatable :: R2
      INT :: n_pt,n
      REAL :: x,y,z

      ! Space for R2
      n_pt = size(pt,1)
      call create_(R2,n_pt)

      ! Now get the interpolated density values
      if (self%interpolator%domain_is_sqrt OR self%interpolator%domain_is_sqrt_x1x) then

         ! Get squared distances R2
         do n = 1,n_pt
            x = pt(n,1)-pos(1)
            y = pt(n,2)-pos(2)
            z = pt(n,3)-pos(3)
            R2(n) = x*x+y*y+z*z
         end do
         call values_for_(self%interpolator,R2,density,occ)
      else

         ! Get distances R2
         do n = 1,n_pt
            x = pt(n,1)-pos(1)
            y = pt(n,2)-pos(2)
            z = pt(n,3)-pos(3)
            R2(n) = sqrt(x*x+y*y+z*z)
         end do
         call values_for_(self%interpolator,R2,density,occ)
      end if

      ! Clean up
      call destroy_(R2)
   end subroutine

   PURE subroutine add_interpolated_density(self,density,pt,pos,occ)
   ! Make the interpolated "density" for the supplied points "pt" from the real
   ! slater atomic orbitals assuming the orbitals are at position "pos". 
      type(COPPENSBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.interpolator.associated,"no interpolator defined!")
      VEC(REAL,:), allocatable :: R2
      INT :: n_pt,n
      REAL :: x,y,z

      ! Space for R2
      n_pt = size(pt,1)
      call create_(R2,n_pt)

      ! Now get the interpolated density values
      if (self%interpolator%domain_is_sqrt OR self%interpolator%domain_is_sqrt_x1x) then

         ! Get squared distances R2
         do n = 1,n_pt
            x = pt(n,1)-pos(1)
            y = pt(n,2)-pos(2)
            z = pt(n,3)-pos(3)
            R2(n) = x*x+y*y+z*z
         end do
         call add_values_for_(self%interpolator,R2,density,occ)
      else

         ! Get distances R2
         do n = 1,n_pt
            x = pt(n,1)-pos(1)
            y = pt(n,2)-pos(2)
            z = pt(n,3)-pos(3)
            R2(n) = sqrt(x*x+y*y+z*z)
         end do
         call add_values_for_(self%interpolator,R2,density,occ)
      end if

      ! Clean up
      call destroy_(R2)
   end subroutine

   PURE function density_at_radius(self,R) result (res)
   ! Work out the electron density at radius "R" from the orbitals.
      type(COPPENSBASIS_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      if (allocated(self%interpolator)) then; res = value_for_(self%interpolator,R)
      else; res = density_at_radius_(self%orbital,R)
      end if
   end function


!  ============
!  Interpolator
!  ============

   subroutine make_interpolator(self,interpolator)
   ! Make the interpolator for the coppens atom density. If present,
   ! "interpolator" is used as a template to get the settings needed.
   ! Otherwise a default interpolator is used.
      type(COPPENSBASIS_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable, optional, IN :: interpolator
   ENSURE(allocated(self%orbital),"COPPENSBASIS:make_interpolator ... no coppens orbitals defined!")

      ! Create a copy of the template if it exists
      if (present(interpolator)) then
         if (allocated(interpolator)) then
            ENSURE(allocated(interpolator),"COPPENSBASIS:make_interpolator ... no interpolator defined!")
            call destroy_(self%interpolator)
            self%interpolator = interpolator
         end if
      end if

      ! If no template, create a default interpolator
      if (NOT allocated(self%interpolator)) call create_(self%interpolator)

      ! Set the interpolation table
      call set_saved_self_(self%orbital)  ! Used by function below, IMPURE
      call set_even_spaced_data_(self%interpolator,density_value_at_radius)
   end subroutine

   function interpolator_table_length(self,tol) result (length)
   ! Return the interpolator table "length" for a given tolerance "tol" for the
   ! atom density. The interpolator is a default one.
      type(COPPENSBASIS_TYPE), IN :: self
      REAL, IN :: tol
      REAL :: length
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
   ENSURE(allocated(self%orbital),"COPPENSBASIS:interpolator_table_length ... no coppens orbitals defined!")
      call create_(interpolator)
      call set_saved_self_(self%orbital)  ! Used by function below, IMPURE
      length = the_table_length_(interpolator,density_value_at_radius,tol)
      call destroy_(interpolator)
   end function


!  =============
!  Normalisation
!  =============

   PURE subroutine unnormalize(self)
   ! Set the value of the orbital coefficients to correspond to un-normalized
   ! Slater functions -- assuming they are normalized. This saves computation.
      type(COPPENSBASIS_TYPE), INOUT :: self
      if (allocated(self%orbital)) call unnormalize_(self%orbital)
   end subroutine

   PURE subroutine renormalize(self)
   ! Set the value of the orbitals coefficients to correspond to normalized
   ! Slater functions --- assuming they are w.r.t. unnormalized functions. 
      type(COPPENSBASIS_TYPE), INOUT :: self
      if (allocated(self%orbital)) call renormalize_(self%orbital)
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(COPPENSBASIS_TYPE), IN :: self
      call dump_(stdout,"label",self%label)
      call dump_(stdout,"n_orb",self%n_orb)
      call dump_(stdout,"n_prim",self%n_prim)
      call dmpa_(self%orbital,"orbital")
      call dump_(stdout,"use_interpolator",self%use_interpolator)
      call dump_(self%interpolator,"interpolator")
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(COPPENSBASIS_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":TYPE?@ = { ")
      else; call text_(stdout,trim(object_name)//":TYPE? = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(COPPENSBASIS_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":TYPE?@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put out the basis information to file "stdout"
      type(COPPENSBASIS_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"================")
      call text_(stdout,"Su-Coppens basis")
      call text_(stdout,"================")
      call flush_(stdout)
      call show_(stdout,"Label                  =",self%label)
      call show_(stdout,"No. of orbitals        =",self%n_orb)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      call show_(stdout,"Use interpolator?      =",self%use_interpolator)
      if (allocated(self%orbital)) call put_(self%orbital)
   end subroutine

end module
