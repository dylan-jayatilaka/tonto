!==================================================================
!
! SLATERBASIS: For Slater basis sets
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
module SLATERBASIS_MODULE

#  include "macros"

   include "slaterbasis.use"

   implicit none

   include "slaterbasis.int"

   VEC(STR(len=STR_SIZE),:), allocatable, private :: keys

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(SLATERBASIS_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SLATERBASIS:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SLATERBASIS_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SLATERBASIS:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the shell parts of self
      type(SLATERBASIS_TYPE), INOUT :: self
      call destroy_(self%shell)
      call destroy_(self%interpolator)
      call destroy_(self%d1_interpolator)
      call destroy_(self%d2_interpolator)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine update(self)
   ! Update the shell data, if it exists
      type(SLATERBASIS_TYPE), INOUT :: self

      ! The following code is inherited from BASIS
      if (NOT allocated(self%shell)) return
      self%n_shell = no_of_shells_(self)
      self%n_bf = no_of_basis_functions_(self)
      self%n_prim = no_of_primitives_(self)
   end subroutine

   pure subroutine set_label(self,val)
   ! Set the basis label to "val"
      type(SLATERBASIS_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%label = val
   end subroutine

   pure subroutine set_use_interpolators(self,val)
   ! Set whether to use interpolation table to speed up evaulation.
      type(SLATERBASIS_TYPE), INOUT :: self
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
      type(SLATERBASIS_TYPE), allocatable, INOUT :: self
      STR(len=*), IN :: label
      VEC(type(SLATERBASIS_TYPE),:), allocatable, IN :: basis
      BIN, optional, IN :: clobber
      BIN, optional, IN :: minmatch
      BIN, optional, OUT :: found
      INT :: b
      VEC(BIN,:), allocatable :: check
      BIN :: bash,fnd,minmtch
   ENSURE(allocated(basis),"SLATERBASIS:resolve_by_label ... no basis set")

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
      else; ENSURE(fnd,"SLATERBASIS:resolve_by_label ... unknown basis label, "//trim(label))
      end if
   end subroutine


!  ============
!  I/O Routines
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(SLATERBASIS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","SLATERBASIS:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"SLATERBASIS:read_keywords ... unexpected end of file while reading list keywords")
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
      type(SLATERBASIS_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("-- Regular options --   ")
      case ("}                       ")  ! exit surrounding loop
      case ("analyze                 "); call analyze_configuration_(self)
      case ("analyze_configuration   "); call analyze_configuration_(self)
      case ("conf=                   "); call read_configuration_(self)
      case ("configuration=          "); call read_configuration_(self)
      case ("label=                  "); call read_label_(self)
      case ("put                     "); call put_(self)
      case ("shells=                 "); call read_shell_(self)
      case ("tonto_style=            "); call read_tonto_style_(self)
      case ("units=                  "); call read_units_(self)
      ! These are only for making custom tables for the list type
      case ("-- Options for tables --")
      case ("flush                   "); call flush_(stdout)
      case ("put_label               "); call put_(stdout,self%label,int_width=TRUE)
      case ("put_n_shells            "); call put_(stdout,self%n_shell)
      case ("put_n_bf                "); call put_(stdout,self%n_bf)
      case ("put_n_prim              "); call put_(stdout,self%n_prim)
      case default; allocate(tonto%known_keywords(17))
      tonto%known_keywords(1) = "-- Regular options --   "
      tonto%known_keywords(2) = "}                       "
      tonto%known_keywords(3) = "analyze                 "
      tonto%known_keywords(4) = "analyze_configuration   "
      tonto%known_keywords(5) = "conf=                   "
      tonto%known_keywords(6) = "configuration=          "
      tonto%known_keywords(7) = "label=                  "
      tonto%known_keywords(8) = "put                     "
      tonto%known_keywords(9) = "shells=                 "
      tonto%known_keywords(10) = "tonto_style=            "
      tonto%known_keywords(11) = "units=                  "
      tonto%known_keywords(12) = "-- Options for tables --"
      tonto%known_keywords(13) = "flush                   "
      tonto%known_keywords(14) = "put_label               "
      tonto%known_keywords(15) = "put_n_shells            "
      tonto%known_keywords(16) = "put_n_bf                "
      tonto%known_keywords(17) = "put_n_prim              "
      call unknown_(tonto,word,"SLATERBASIS:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(SLATERBASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_label(self)
   ! Read only the basis label
      type(SLATERBASIS_TYPE), INOUT :: self

      ! The following code is inherited from BASIS
      call read_(stdin,self%label)
      ENSURE(is_alphabetical_(self%label(1:1)),"SLATERBASIS:read_label ... label must begin with alphabetical character")
   end subroutine

   subroutine read_shell(self)
   ! Read a shell
      type(SLATERBASIS_TYPE), INOUT :: self

      ! The following code is inherited from BASIS
      call read_list_keywords_(self%shell)
      call update_(self)
   end subroutine

   subroutine read_configuration(self)
   ! Read in the configuration string
      type(SLATERBASIS_TYPE), INOUT :: self
      call read_(stdin,self%configuration)
   end subroutine

   subroutine read_interpolator(self)
   ! Read the interpolator
      type(SLATERBASIS_TYPE), INOUT :: self
   ENSURE(allocated(self%shell),"SLATERBASIS:read_interpolator ... no shells defined!")
      call destroy_(self%interpolator)
      call create_(self%interpolator)
      call read_keywords_(self%interpolator)
   end subroutine

   subroutine read_tonto_style(self)
   ! Create and read a tonto style basis set
      type(SLATERBASIS_TYPE), INOUT :: self
      call read_label_(self)
      call read_configuration_(self)
      call set_keys_(self%shell,["l_chr=    ","orb_kinds=","n,z,c*=   "])
      call read_data_(self%shell)
      call analyze_configuration_(self)
      call update_(self)
   end subroutine

   PURE subroutine analyze_configuration(self)
   ! Analyze the orbital configuration and set the shell occupancies
      type(SLATERBASIS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: configuration,conf_kind
      VEC(STR(len=STR_SIZE),:), allocatable :: split,conf
      VEC(BIN,:), allocatable :: match
      BIN :: found,keep
      INT :: i,j,s,occ,ind
   ENSURE(self%configuration/=" ","SLATERBASIS:analyze_configuration ... no configuration")
   ENSURE(allocated(self%shell),"SLATERBASIS:analyze_configuration ... no shells")
   ENSURE(shell_kinds_created_(self),"SLATERBASIS:analyze_configuration ... not all orbital kinds are there")

      ! Get configuration
      configuration = self%configuration

      ! Replacements
      call replace_all_(configuration,"("," ")
      call replace_all_(configuration,")"," ")
      call replace_all_(configuration,"[Xe]","X 54 ")
      call replace_all_(configuration,"[XE]","X 54 ")
      call replace_all_(configuration,"[Rn]","R 86 ")
      call replace_all_(configuration,"[RN]","R 86 ")

      ! Split configuration into separate fields
      split = split_(configuration)
      i = 1
      do  ! loop over shell/orbital configurations
         if (i>size(split)) exit

         ! Now get orbital configurations only
         keep = FALSE
         select case (split(i))
         case ("K"); conf = split_(("1S 2"))
         case ("L"); conf = split_(("2S 2 2P 6"))
         case ("M"); conf = split_(("3S 2 3P 6 3D 10"))
         case ("X"); conf = split_(("1S 2 2S 2 2P 6 3S 2 3P 6 3D 10 4S 2 4P 6 5S 2 4D 10 5P 6"))
         case ("R"); conf = split_(("1S 2 2S 2 2P 6 3S 2 3P 6 3D 10 4S 2 4P 6 5S 2 4D 10 5P 6 4F 14 6S 2 5D 10 6P 6"))
         case default; conf = split(i:i+1); keep = TRUE
         end select
         j = 1
         do  ! Loop over orbital configurations 
            if (j>size(conf)) exit
            conf_kind = conf(j)
            occ = to_int_(conf(j+1))

            ! Now find the shell with the correct orbital kind
            if (occ>0) then
               found = FALSE
               do s = 1,size(self%shell)
                  if (all(self%shell(s)%orb_kind/=conf_kind)) cycle
                  call create_(match,self%shell(s)%n_orb)
                  match = self%shell(s)%orb_kind==conf_kind
                  ind = index_of_first_true_element_(match)
                  call destroy_(match)
                  ENSURE(allocated(self%shell(s)%occupancy),"SLATERBASIS:analyze_configuration ... no occupancies")
                  self%shell(s)%occupancy(ind) = occ
                  found = TRUE
                  exit
               end do
               ENSURE(found,"SLATERBASIS:analyze_configuration ... orbital kind "//trim(conf_kind)//" not found")
            end if
            j = j+2
         end do
         if (NOT keep) call destroy_(conf)
         i = i+2
      end do
      call destroy_(split)
   end subroutine


!  ====================
!  Key related routines
!  ====================

   subroutine read_keys(self)
   ! Read the "keys".
      type(SLATERBASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call read_all_(stdin,keys)
   end subroutine

   subroutine process_keys(self)
   ! Process each of the words in the "keys" list.
      type(SLATERBASIS_TYPE), INOUT :: self
      INT :: k,l,n_key
      STR(len=STR_SIZE) :: keyword
      BIN :: keyword_echo
   ENSURE(allocated(keys),"SLATERBASIS:process_keys ... no keys")

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
            DIE_IF(l==0,"SLATERBASIS:process_keys ... no matching closing brace in embedded keyword")

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
      type(SLATERBASIS_TYPE), INOUT :: self
      BIN :: res

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      res = allocated(keys)
      if (res) res = size(keys)>0
   end function

   subroutine set_keys(self,the_keys)
   ! This is for setting the "keys" externally.
      type(SLATERBASIS_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: the_keys

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      keys = the_keys
   end subroutine

   subroutine clear_keys(self)
   ! This is for destroying the "keys" externally.
      type(SLATERBASIS_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call destroy_(keys)
      call create_(keys,0)
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   pure function same_as(self,b) result (res)
   ! Return TRUE if the basis set "self" is the same as "b". Only the
   ! shell vector is compared to see if they are "really" the same.
      type(SLATERBASIS_TYPE), IN :: self
      type(SLATERBASIS_TYPE), IN :: b
      BIN :: res

      ! The following code is inherited from BASIS
      res = same_as_(self%shell,b%shell)
   end function

   pure function is_resolved(self) result (res)
   ! Return TRUE if self is resolved i.e. has shells
      type(SLATERBASIS_TYPE), IN :: self
      BIN :: res

      ! The following code is inherited from BASIS
      res = allocated(self%shell)
   end function

   pure function is_created_and_resolved(self) result (res)
   ! Return TRUE if self is resolved
      type(SLATERBASIS_TYPE), allocatable, IN :: self
      BIN :: res

      ! The following code is inherited from BASIS
      res = allocated(self)
      if (res) res = allocated(self%shell)
   end function

   pure function is_created_and_has_label(self) result (res)
   ! Return TRUE if self has a non-blank label
      type(SLATERBASIS_TYPE), allocatable, IN :: self
      BIN :: res

      ! The following code is inherited from BASIS
      res = allocated(self)
      if (res) res = self%label/=" "
   end function

   pure function max_n_orb(self) result (res)
   ! Return the maximum number of contracted orbitals in any one shell.
      type(SLATERBASIS_TYPE), IN :: self
      INT :: res
      res = maxval(self%shell%n_orb)
   end function

   pure function no_of_shells(self) result (res)
   ! Work out and return the number of shells in the basis set
      type(SLATERBASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")

      ! The following code is inherited from BASIS
      res = size(self%shell)
   end function

   pure function no_of_basis_functions(self) result (res)
   ! Work out and return the number of basis functions in the basis set
      type(SLATERBASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")

      ! The following code is inherited from BASIS
      res = no_of_basis_functions_(self%shell)
   end function

   pure function no_of_primitives(self) result (res)
   ! Work out and return the number of primitives in the basis set
      type(SLATERBASIS_TYPE), IN :: self
      INT :: res

   ! ENSURE(.shell.allocated,"no shells")

      ! The following code is inherited from BASIS
      res = no_of_primitives_(self%shell)
   end function


!   min_exponent result (res) :: get_from(BASIS), pure
!   ! Return the minimum exponent in the basis.
!   end

   pure function shell_kinds_created(self) result (res)
   ! Return TRUE if all the .shell.orb_kind vectors are there.
      type(SLATERBASIS_TYPE), IN :: self
      BIN :: res
      INT :: i
      if (NOT allocated(self%shell)) then
         res = FALSE
      else
         res = TRUE
         do i = 1,size(self%shell)
            if (allocated(self%shell(i)%orb_kind)) cycle
            res = FALSE
            exit
         end do
      end if
   end function


!  ==================
!  Density evaluation
!  ==================

   PURE subroutine make_ED_grid(self,density_grid,pt,pos,occ)
   ! Make the electron "density_grid" on a set of points "pt", assuming the
   ! orbitals are at position "pos".
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ
   ENSURE(size(density_grid)==size(pt,1),"SLATERBASIS:make_ED_grid ... inconsistent number of points")
      if (self%use_interpolator AND allocated(self%interpolator)) then
         call make_interpolated_ED_grid_(self,density_grid,pt,pos,occ)
      else
         call make_normal_ED_grid_(self,density_grid,pt,pos,occ)
      end if
   end subroutine

   PURE subroutine add_ED_grid(self,density_grid,pt,pos,occ,power_factor)
   ! Add the electron "density_grid" on a set of points "pt", assuming the
   ! orbitals are at position "pos".
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ
      INT, optional, IN :: power_factor
      BIN :: use_power_factor
   ENSURE(size(density_grid)==size(pt,1),"SLATERBASIS:add_ED_grid ... inconsistent number of points")
      use_power_factor = FALSE
      if (present(power_factor)) then
         if (power_factor/=1) then
            use_power_factor = TRUE
         end if
      end if
      if (self%use_interpolator AND allocated(self%interpolator)) then
         if (use_power_factor) then
            call add_interpolated_power_density_(self,density_grid,pt,pos,power_factor,occ)
         else
            call add_interpolated_density_(self,density_grid,pt,pos,occ)
         end if
      else
         call add_normal_density_grid_(self,density_grid,pt,pos,occ)
      end if
   end subroutine

   PURE subroutine make_normal_ED_grid(self,density_grid,pt,pos,occ)
   ! Make the normal (uninterpolated) "density_grid" for the supplied points
   ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
   ! the orbital is at position "pos".
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), OUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density_grid.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.shell.allocated,"no shell vector")
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
      if (do_occ) then; density_grid = occ*densities_at_radii_(self%shell,R)
      else; density_grid = densities_at_radii_(self%shell,R)
      end if

      ! Clean up
      call destroy_(R)
   end subroutine

   PURE subroutine add_normal_density_grid(self,density_grid,pt,pos,occ)
   ! Add the normal (uninterpolated) "density_grid" for the supplied points
   ! "pt" from the real slater atomic orbitals, as fitted by coppens, assuming
   ! the orbital is at position "pos".
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density_grid
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density_grid.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.shell.allocated,"no shell vector")
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
      if (do_occ) then; density_grid = density_grid+occ*densities_at_radii_(self%shell,R)
      else; density_grid = density_grid+densities_at_radii_(self%shell,R)
      end if
      call destroy_(R)
   end subroutine

   PURE subroutine make_interpolated_ED_grid(self,density,pt,pos,occ)
   ! Make the interpolated "density" for the supplied points "pt" from the real
   ! slater atomic orbitals assuming the orbitals are at position "pos". 
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), OUT :: density
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.interpolator.allocated,"no interpolator defined!")
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

   PURE subroutine add_interpolated_power_density(self,density,pt,pos,power_factor,occ)
   ! Make the interpolated "power density" for the supplied points
   ! "pt" from the real slater atomic orbitals assuming the orbitals
   ! are at position "pos" ... same as add_interpolated_density except
   ! the densities are raised by the "power factor"
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      INT, IN :: power_factor
      REAL, optional, IN :: occ
      VEC(REAL,:), allocatable :: temp_density
      call create_(temp_density,size(pt,1))
      temp_density = ZERO
      call make_interpolated_ED_grid_(self,temp_density,pt,pos,occ)
      density = density+temp_density**power_factor
      call destroy_(temp_density)
   end subroutine

   PURE subroutine add_interpolated_density(self,density,pt,pos,occ)
   ! Make the interpolated "density" for the supplied points "pt" from the real
   ! slater atomic orbitals assuming the orbitals are at position "pos". 
      type(SLATERBASIS_TYPE), IN :: self
      VEC(REAL,:), INOUT :: density
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,3), IN :: pos
      REAL, optional, IN :: occ

! ENSURE(pt.dim2==3,"wrong dimension for points array")
! ENSURE(density.dim==pt.dim1,"inconsistent number of points")
! ENSURE(.interpolator.allocated,"no interpolator defined!")
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
      type(SLATERBASIS_TYPE), IN :: self
      REAL, IN :: R
      REAL :: res
      if (allocated(self%interpolator)) then; res = value_for_(self%interpolator,R)
      else; res = density_at_radius_(self%shell,R)
      end if
   end function


!   make_d_density_grid(density,d1_density,d2_density,pt,pos)
!   ! Work out the electron "density", its 1st derivative "d1_density" and 2nd
!   ! derivative "d2_density" on a set of points "pt", assuming the orbitals are
!   ! at position "pos".
!      self :: IN
!      density :: VEC{REAL}, OUT
!      d1_density :: MAT{REAL}, OUT
!      d2_density :: MAT3{REAL}, OUT
!      pt :: MAT{REAL}, IN
!      pos :: VEC{REAL}(3), IN
!
!   ENSURE(density.dim==pt.dim1,"inconsistent number of points")
!   ENSURE(d1_density.dim2==pt.dim1,"inconsistent number of points")
!   ENSURE(d2_density.dim3==pt.dim1,"inconsistent number of points")
!   ENSURE(d1_density.dim1==3,"wrong dim1 for d_density")
!   ENSURE(d2_density.dim1==3,"wrong dim1 for d2_density")
!   ENSURE(d2_density.dim2==3,"wrong dim1 for d2_density")
!
!      if (.interpolator.allocated) then
!         .interpolated_d_density(density,d1_density,d2_density,pt,pos)
!      else
!         DIE("not implemented yet")
!      end
!   end

!   interpolated_d_density(density,d1_density,d2_density,pt,pos)
!   ! Make the interpolated "density", its 1st derivative "d1_density", and 2nd
!   ! derivative "d2_density" for the supplied points "pt" from the real slater
!   ! atomic orbitals assuming the orbitals are at position "pos". This fast
!   ! version skips tests and makes assumptions.  
!     self :: IN
!     density :: VEC{REAL}, OUT
!     d1_density :: MAT{REAL}, OUT
!     d2_density :: MAT3{REAL}, OUT
!     pt :: MAT{REAL}, IN
!     pos :: VEC{REAL}(3), IN
!
!   ! ENSURE(pt.dim2==3,"wrong dimension for points array")
!   ! ENSURE(density_grid.dim==pt.dim1,"inconsistent number of points")
!   ! ENSURE(.interpolator.allocated,"no interpolator defined!")
!
!     R,D1,D2 :: VEC{REAL}@
!     n_pt,n :: INT
!     x,y,z,r1,r2,r3,xx,yy,zz,xy,xz,yz, fac1,fac2,val :: REAL
!     n_pt = pt.dim1
!     D1.create(n_pt)
!     D2.create(n_pt)
!     R.create(n_pt)
!     do n = 1,n_pt
!        x = pt(n,1) - pos(1)
!        y = pt(n,2) - pos(2)
!        z = pt(n,3) - pos(3)
!        R(n) = sqrt(x*x + y*y + z*z)
!     end
!     ! Now get the interpolated density values
!     density = .interpolator.fast_values_for(R)
!     D1 = .d1_interpolator.fast_d_values_for(R) ! include minus sign, maybe
!     D2 = .d2_interpolator.fast_values_for(R)  ! minus sign not needed
!     do n = 1,n_pt
!        x = pt(n,1) - pos(1)
!        y = pt(n,2) - pos(2)
!        z = pt(n,3) - pos(3)
!        r1 = ONE/R(n)
!        r2 = r1*r1
!        r3 = r2*r1
!        xx = x*x; yy = y*y; zz = z*z
!        xy = x*y; xz = x*z; yz = y*z
!        fac1 = D1(n)
!        fac2 = D2(n)*r2 - fac1*r3
!        val  = fac1*r1
!        d2_density(1,1,n) = fac2*xx + val
!        d2_density(2,2,n) = fac2*yy + val
!        d2_density(3,3,n) = fac2*zz + val
!        d2_density(1,2,n) = fac2*xy; d2_density(2,1,n) = d2_density(1,2,n)
!        d2_density(1,3,n) = fac2*xz; d2_density(3,1,n) = d2_density(1,3,n)
!        d2_density(2,3,n) = fac2*yz; d2_density(3,2,n) = d2_density(2,3,n)
!        fac1 = fac1*r1
!        d1_density(1,n) = fac1*x
!        d1_density(2,n) = fac1*y
!        d1_density(3,n) = fac1*z
!     end
!     R.destroy
!     D2.destroy
!     D1.destroy
!   end

!  ============
!  Interpolator
!  ============

   subroutine make_interpolator(self,interpolator)
   ! Make the interpolator for the slater atom density. If present,
   ! "interpolator" is used as a template to get the settings needed. 
      type(SLATERBASIS_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable, optional, IN :: interpolator
   ENSURE(allocated(self%shell),"SLATERBASIS:make_interpolator ... no slater orbitals defined!")

      ! Create a copy of the template if it exists
      if (present(interpolator)) then
         if (allocated(interpolator)) then
            call destroy_(self%interpolator)
            self%interpolator = interpolator
         end if
      end if

      ! If no template, create a default interpolator
      if (NOT allocated(self%interpolator)) call create_(self%interpolator)

      ! Set the interpolation table
      call set_saved_self_(self%shell)  ! Used by function below
      call set_even_spaced_data_(self%interpolator,density_value_at_radius)
   end subroutine

   function interpolator_table_length(self,tol) result (length)
   ! Return the interpolator table "length" for a given tolerance "tol" for the
   ! atom density. The interpolator is a default one.
      type(SLATERBASIS_TYPE), IN :: self
      REAL, IN :: tol
      REAL :: length
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
   ENSURE(allocated(self%shell),"SLATERBASIS:interpolator_table_length ... no slater orbitals defined!")
      call create_(interpolator)
      call set_saved_self_(self%shell)  ! Used by function below
      length = the_table_length_(interpolator,density_value_at_radius,tol)
      call destroy_(interpolator)
   end function


!   make_d_interpolators(interpolator,tol) :: leaky
!   ! Make the DERIVATIVE interpolators as well for the slater atom density. If
!   ! present, "interpolator" is used as a template to get the settings needed.
!   ! Otherwise a default interpolator is used.
!      interpolator :: INTERPOLATOR*, optional
!      tol :: REAL, optional
!
!   ENSURE(.shell.allocated,"no slater orbitals defined!")
!
!      len :: REAL
!
!      if (present(interpolator)) then
!      if (interpolator.allocated) then
!         .interpolator.destroy
!         .interpolator.create_copy(interpolator)
!         .d1_interpolator.destroy
!         .d1_interpolator.create_copy(interpolator)
!         .d2_interpolator.destroy
!         .d2_interpolator.create_copy(interpolator)
!      end
!      end
!
!      if (.interpolator.deallocated) then
!         .interpolator.create
!         .d1_interpolator.create
!         .d2_interpolator.create
!      end
!
!      .shell.set_saved_self ! Used by function below
!      .interpolator.set_even_spaced_data(VEC{SLATERSHELL}::density_value_at_radius,tol=tol)
!      len = .interpolator.table_length
!      .d1_interpolator.set_even_spaced_data(VEC{SLATERSHELL}::density_d1_value_at_radius,length=len)
!      .d2_interpolator.set_even_spaced_data(VEC{SLATERSHELL}::density_d2_value_at_radius,length=len)
!
!   end

!  =============
!  Normalisation
!  =============

   PURE subroutine unnormalize(self)
   ! Set the value of the orbital coefficients to correspond to un-normalized
   ! Slater functions -- assuming they are normalized. This saves computation.
      type(SLATERBASIS_TYPE), INOUT :: self
      if (allocated(self%shell)) call unnormalize_(self%shell)
   end subroutine

   PURE subroutine renormalize(self)
   ! Set the value of the orbitals coefficients to correspond to normalized
   ! Slater functions --- assuming they are w.r.t. unnormalized functions. 
      type(SLATERBASIS_TYPE), INOUT :: self
      if (allocated(self%shell)) call renormalize_(self%shell)
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(SLATERBASIS_TYPE), IN :: self
      call dump_(stdout,"label",self%label)
      call dump_(stdout,"configuration",self%configuration)
      call dump_(stdout,"n_shell",self%n_shell)
      call dump_(stdout,"n_bf",self%n_bf)
      call dump_(stdout,"n_prim",self%n_prim)
      call dmpa_(self%shell,"shell")
      call dump_(stdout,"use_interpolator",self%use_interpolator)
      call dump_(self%interpolator,"interpolator")
      call dump_(self%d1_interpolator,"d1_interpolator")
      call dump_(self%d2_interpolator,"d2_interpolator")
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(SLATERBASIS_TYPE), IN :: self
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
      type(SLATERBASIS_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":TYPE?@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put out the basis information to file "stdout"
      type(SLATERBASIS_TYPE), IN :: self
      call flush_(stdout)
      call show_(stdout,"Slater basis set =",trim(self%label))
      call flush_(stdout)
      call show_(stdout,"Configuration          =",self%configuration)
      call show_(stdout,"No. of shells          =",self%n_shell)
      call show_(stdout,"No. of basis functions =",self%n_bf)
      call show_(stdout,"No. of primitives      =",self%n_prim)
      if (allocated(self%shell)) call put_(self%shell)
   end subroutine

end module
