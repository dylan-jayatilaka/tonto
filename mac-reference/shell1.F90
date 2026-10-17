!=====================================================================
!
! SHELL1 
!
! For describing contracted cartesian gaussian shells with a position
! coordinate. Largely inherited from SHELL.
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
!=====================================================================
module SHELL1_MODULE

#  include "macros"

   include "shell1.use"

   implicit none

   include "shell1.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(SHELL1_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SHELL1:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SHELL1_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SHELL1:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy pointer part of a shell object
      type(SHELL1_TYPE), INOUT :: self

      ! The following code is inherited from SHELL
      call destroy_(self%exponent)
      call destroy_(self%contraction)
   end subroutine


!  ===
!  Set
!  ===

   PURE subroutine set_0(self,shell,pos)
   ! Make a shell1 object from a shell1 object.
   ! The position "pos" overides "shell.position".
      type(SHELL1_TYPE), OUT :: self
      type(SHELL1_TYPE), IN :: shell
      VEC(REAL,3), IN :: pos
      self = shell
      self%position = pos
   end subroutine

   PURE subroutine set_reusing_storage(self,shell,pos)
   ! As set(shell,pos) but "self" is not destroyed first, so its arrays
   ! are reused when they are the right size. For integral loops.
      type(SHELL1_TYPE), INOUT :: self
      type(SHELL1_TYPE), IN :: shell
      VEC(REAL,3), IN :: pos
      self%l = shell%l
      self%l_chr = shell%l_chr
      self%is_spherical = shell%is_spherical
      self%n_bf = shell%n_bf
      self%n_comp = shell%n_comp
      self%n_sph = shell%n_sph
      self%first_gaussian = shell%first_gaussian
      self%last_gaussian = shell%last_gaussian
      self%n_cc = shell%n_cc
      self%position = pos
      self%exponent = shell%exponent
      self%contraction = shell%contraction
   end subroutine

   PURE subroutine set_1(self,shell,pos)
   ! Make a shell1 object from a shell1 object.
   ! The position "pos" overides "shell.position".
      type(SHELL1_TYPE), OUT :: self
      type(SHELL_TYPE), IN :: shell
      VEC(REAL,3), IN :: pos
      call set_(self,shell)
      call set_(self,pos)
   end subroutine

   PURE subroutine set_2(self,shell)
   ! Make a shell1 object from the shell. The position is *not* set.
   ! NOTE : ensure the ptr parts have been destroyed beforehand.
      type(SHELL1_TYPE), OUT :: self
      type(SHELL_TYPE), IN :: shell
   ENSURE(NOT allocated(self%exponent),"SHELL1:set_2 ... ex not destroyed")
   ENSURE(NOT allocated(self%contraction),"SHELL1:set_2 ... cc not destroyed")
      self%l = shell%l
      self%l_chr = shell%l_chr
      self%is_spherical = shell%is_spherical
      self%n_bf = shell%n_bf
      self%n_comp = shell%n_comp
      self%n_sph = shell%n_sph
      self%first_gaussian = shell%first_gaussian
      self%last_gaussian = shell%last_gaussian
      self%n_cc = shell%n_cc
      self%exponent = shell%exponent
      self%contraction = shell%contraction
   end subroutine

   pure subroutine set_3(self,pos)
   ! Set a shell1 object. Pointer parts are assigned not copied.
      type(SHELL1_TYPE), INOUT :: self
      VEC(REAL,:), IN :: pos
      self%position = pos
   end subroutine

   PURE subroutine set_l(self,l)
   ! Set the l value
      type(SHELL1_TYPE), INOUT :: self
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
      type(SHELL1_TYPE), INOUT :: self

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
      type(SHELL1_TYPE), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from SHELL
      self%is_spherical = val
      call set_n_comp_(self)
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(SHELL1_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","SHELL1:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"SHELL1:read_keywords ... unexpected end of file while reading list keywords")
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
      type(SHELL1_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}            ")  ! exit surrounding loop
      case ("contractions="); call read_contractions_(self)
      case ("exponents=   "); call read_exponents_(self)
      case ("ex,cc=       "); call read_ex_cc_(self)
      case ("junk,ex,cc=  "); call read_junk_ex_cc_(self)
      case ("l=           "); call read_l_(self)
      case ("l_chr=       "); call read_l_chr_(self)
      case ("n_cc=        "); call read_n_cc_(self)
      case ("position=    "); call read_position_(self)
      case ("put          "); call put_(self)
      case ("spherical=   "); call read_spherical_(self)
      case ("units=       "); call read_units_(self)
      case default; allocate(tonto%known_keywords(12))
      tonto%known_keywords(1) = "}            "
      tonto%known_keywords(2) = "contractions="
      tonto%known_keywords(3) = "exponents=   "
      tonto%known_keywords(4) = "ex,cc=       "
      tonto%known_keywords(5) = "junk,ex,cc=  "
      tonto%known_keywords(6) = "l=           "
      tonto%known_keywords(7) = "l_chr=       "
      tonto%known_keywords(8) = "n_cc=        "
      tonto%known_keywords(9) = "position=    "
      tonto%known_keywords(10) = "put          "
      tonto%known_keywords(11) = "spherical=   "
      tonto%known_keywords(12) = "units=       "
      call unknown_(tonto,word,"SHELL1:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(SHELL1_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_l(self)
   ! Read in the l symbol
      type(SHELL1_TYPE), INOUT :: self
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
      type(SHELL1_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word

      ! The following code is inherited from SHELL
      call read_(stdin,word)
      ENSURE(is_int_(word),"SHELL1:read_l_int ... expecting an integer for L")
      call set_l_(self,to_int_(word))
   end subroutine

   subroutine read_l_chr(self)
   ! Read in the l symbol
      type(SHELL1_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      STR(len=1) :: l_c
      INT :: l

      ! The following code is inherited from SHELL
      call read_(stdin,word)
      DIE_IF(len_trim(word)/=1,"SHELL1:read_l_chr ... unknown L symbol")
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
      type(SHELL1_TYPE), INOUT :: self

      ! The following code is inherited from SHELL
      call read_(stdin,self%is_spherical)
   end subroutine

   subroutine read_n_cc(self)
   ! Read in the number of contraction coefficients
      type(SHELL1_TYPE), INOUT :: self

      ! The following code is inherited from SHELL
      call read_(stdin,self%n_cc)
      ENSURE(self%n_cc>0,"SHELL1:read_n_cc ... n_cc must be positive")
   end subroutine

   subroutine read_exponents(self)
   ! Read in the exponents
      type(SHELL1_TYPE), INOUT :: self
   ENSURE(self%n_cc>0,"SHELL1:read_exponents ... n_cc must be entered first")
   ENSURE(NOT allocated(self%exponent),"SHELL1:read_exponents ... ex already entered")

      ! The following code is inherited from SHELL
      call create_(self%exponent,self%n_cc)
      call read_(stdin,self%exponent)
   end subroutine

   subroutine read_contractions(self)
   ! Read in the contraction coefficients
      type(SHELL1_TYPE), INOUT :: self
   ENSURE(self%n_cc>0,"SHELL1:read_contractions ... n_cc must be entered first")
   ENSURE(NOT allocated(self%contraction),"SHELL1:read_contractions ... cc already entered")

      ! The following code is inherited from SHELL
      call create_(self%contraction,self%n_cc)
      call read_(stdin,self%contraction)
   end subroutine

   subroutine read_ex_cc(self)
   ! Read in the exponents and contractions
      type(SHELL1_TYPE), INOUT :: self
   ENSURE(self%n_cc>0,"SHELL1:read_ex_cc ... n_cc must be entered first")
   ENSURE(NOT allocated(self%exponent),"SHELL1:read_ex_cc ... ex already entered")
   ENSURE(NOT allocated(self%contraction),"SHELL1:read_ex_cc ... ex already entered")

      ! The following code is inherited from SHELL
      call create_(self%exponent,self%n_cc)
      call create_(self%contraction,self%n_cc)
      call read_pair_(stdin,self%exponent,self%contraction)
   end subroutine

   subroutine read_junk_ex_cc(self)
   ! Read in the exponents and contractions preceded by a junk string
      type(SHELL1_TYPE), INOUT :: self
      INT :: i
   ENSURE(self%n_cc>0,"SHELL1:read_junk_ex_cc ... n_cc must be entered first")
   ENSURE(NOT allocated(self%exponent),"SHELL1:read_junk_ex_cc ... ex already entered")
   ENSURE(NOT allocated(self%contraction),"SHELL1:read_junk_ex_cc ... ex already entered")

      ! The following code is inherited from SHELL
      call create_(self%exponent,self%n_cc)
      call create_(self%contraction,self%n_cc)
      do i = 1,self%n_cc
         call skip_next_item_(stdin)
         call read_(stdin,self%exponent(i))
         call read_(stdin,self%contraction(i))
      end do
   end subroutine

   subroutine read_position(self)
   ! Read in the position
      type(SHELL1_TYPE), INOUT :: self
      call read_quantity_(stdin,self%position)
   end subroutine


!  =====================
!  Miscellaneous methods
!  =====================

   PURE function l_chr(l) result (res)
   ! Return a character representation for angular mtm "l"
   ! Of course, "L" shells are never returned: they have mixed mtm.
      INT, IN :: l
      STR(len=1) :: res
   ENSURE(l>=0 AND l<=23,"SHELL1:l_chr ... angular momentum too large:"//trim(to_str_(l)))

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

   pure function same_as(self,sh) result (same)
   ! Return TRUE if the shell "self" is the same as "sh".
      type(SHELL1_TYPE), IN :: self
      type(SHELL1_TYPE), IN :: sh
      BIN :: same

      ! Check if basic arrays are created identically
      same = self%l_chr==sh%l_chr AND (self%is_spherical EQV sh%is_spherical) AND (allocated(self%exponent) EQV allocated(sh%exponent)) AND (allocated(self%contraction) EQV allocated(sh%contraction))

      ! If not, we are done
      if (NOT same) return

      ! Check position ...
      same = same_as_(self%position,sh%position)
      if (NOT same) return

      ! Check exponents ..
      if (allocated(self%exponent) AND allocated(sh%exponent)) same = same_as_(self%exponent,sh%exponent)
      if (NOT same) return

      ! Check contractions ...
      if (allocated(self%contraction) AND allocated(sh%contraction)) same = same_as_(self%contraction,sh%contraction)
   end function

   pure function norm(self) result (res)
   ! Return the norm of the shell, assuming that the existing contraction
   ! coefficients are with respect to NORMALISED primitive gaussians.
      type(SHELL1_TYPE), IN :: self
      REAL :: res
      INT :: i,j
      REAL :: sm,a,b,ab

      ! The following code is inherited from SHELL
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
      type(SHELL1_TYPE), INOUT :: self

      ! The following code is inherited from SHELL
      self%contraction(:) = self%contraction(:)*(FOUR*self%exponent(:))**(HALF*self%l+HALF+QUARTER)*(ONE/(norm_(self)*sqrt(double_factorial_(self%l))))
   end subroutine

   PURE subroutine renormalize(self)
   ! Assuming the existing contraction coefficients are with respect to raw
   ! UNNORMALISED primitive gaussians, put back this normalisation factor for
      type(SHELL1_TYPE), INOUT :: self

      ! The following code is inherited from SHELL
      self%contraction(:) = self%contraction(:)/((FOUR*self%exponent(:))**(HALF*self%l+HALF+QUARTER))
      self%contraction(:) = self%contraction(:)*((TWO*PI)**(THREE/FOUR))/norm_(self)
   end subroutine

   pure function n_prim(self) result (res)
   ! Return the number of primitive gaussians in the shell
      type(SHELL1_TYPE), IN :: self
      INT :: res

      ! The following code is inherited from SHELL
      res = self%n_comp*self%n_cc
   end function


!  ============
!  Grid methods
!  ============

   pure function r_max(self,cutoff) result (res)
   ! Return the radius where all basis functions becomes smaller than "cutoff".
      type(SHELL1_TYPE), IN :: self
      REAL, IN :: cutoff
      REAL :: res
      REAL :: alpha

      ! The following code is inherited from SHELL
      alpha = ONE/minval(self%exponent)
      if (cutoff<=ZERO) then; res = huge(ONE)
      else; res = sqrt(-alpha*log(cutoff/sum(abs(self%contraction))))
      end if
   end function

   pure function r2_max(self,cutoff) result (res)
   ! Return the radius where all basis functions becomes smaller than "cutoff".
      type(SHELL1_TYPE), IN :: self
      REAL, IN :: cutoff
      REAL :: res
      REAL :: alpha

      ! The following code is inherited from SHELL
      alpha = ONE/minval(self%exponent)
      if (cutoff<=ZERO) then; res = huge(ONE)
      else; res = -alpha*log(cutoff/sum(abs(self%contraction)))
      end if
   end function

   pure subroutine make_skip_list(self,skip,n_keep,r2,preskip,n_pt,cutoff)
   ! From a list of squared distances "r2" and preskipped points
   ! "preskip" return those distances where the basis function has a
   ! significant value above "cutoff".
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_pt
      VEC(BIN,n_pt), OUT :: skip
      INT, OUT :: n_keep
      VEC(REAL,n_pt), IN :: r2
      VEC(BIN,n_pt), IN :: preskip
      REAL, IN :: cutoff
      REAL :: r2_max
      BIN :: s
      INT :: n
      r2_max = r2_max_(self,cutoff)
      n_keep = 0
      do n = 1,n_pt
         s = preskip(n)
         skip(n) = s
         if (s) cycle
         s = r2(n)>r2_max
         skip(n) = s
         if (s) cycle
         n_keep = n_keep+1
      end do
   end subroutine

   PURE subroutine make_significant_points(self,x,y,z,r2,skip,pt,cutoff)
   ! From a given list of points "pt" return the significant displacements "x",
   ! "y", "z" and their square "r2". These are comprised of those points for
   ! which the basis functions of the atom have a significant value above
   ! "cutoff" i.e. those points for which "skip" is FALSE.
      type(SHELL1_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: x,y,z,r2
      VEC(BIN,:), allocatable, OUT :: skip
      MAT(REAL,:,:), IN :: pt
      REAL, IN :: cutoff
      REAL :: xx,yy,zz,rr,x0,y0,z0,r2_max
      INT :: n_pt,n,k
      n_pt = size(pt,1)
      call create_(skip,n_pt)
      call create_(x,n_pt)
      call create_(y,n_pt)
      call create_(z,n_pt)
      call create_(r2,n_pt)

      ! Square cutoff distance
      r2_max = r2_max_(self,cutoff)

      ! Origin
      x0 = self%position(1)
      y0 = self%position(2)
      z0 = self%position(3)
      k = 0
      do n = 1,n_pt
         xx = pt(n,1)-x0
         yy = pt(n,2)-y0
         zz = pt(n,3)-z0
         rr = xx*xx+yy*yy+zz*zz
         skip(n) = rr>r2_max
         if (skip(n)) cycle
         k = k+1
         x(k) = xx
         y(k) = yy
         z(k) = zz
         r2(k) = rr
      end do
      if (k==0) then
         call destroy_(skip)
         call destroy_(x)
         call destroy_(y)
         call destroy_(z)
         call destroy_(r2)
      else
         call shrink_(x,k)
         call shrink_(y,k)
         call shrink_(z,k)
         call shrink_(r2,k)
      end if
   end subroutine

   PURE subroutine make_grid_0(self,F,pt)
   ! Return "F(i,n)", the value of the basis function "n" on grid
   ! point "i" given a set of grid points "pt(i,1:3)"
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      MAT(REAL,:,:), IN :: pt
      call make_grid_(self,F,pt(:,1),pt(:,2),pt(:,3))
   end subroutine

   PURE subroutine make_grid_1(self,F,x,y,z)
   ! Return "F(i,n)", the value of the basis function "n" on grid
   ! point "i" given a set of grid points "pt(i,1:3)"
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z
      MAT(REAL,:,:), allocatable :: Fc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_grid_c_(self,F,x,y,z)
      else if (self%l<2) then
         call make_grid_c_(self,F,x,y,z)
      else
         n_pt = size(x)
         call create_(Fc,n_pt,self%n_comp)
         call make_grid_c_(self,Fc,x,y,z)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_grid_c_0(self,f,x,y,z)
   ! Return "f(i,n)", the value of the *cartesian* shell component "n"
   ! on grid point "i" given a set of grid points "(x(i),y(i),z(i))"
   ! Note: it is assumed that the shell is normalized to the component
   ! x^2; the appropriate double factorial normalising factors are
   ! introduced for each component (if required) so that each
   ! component is properly normalized.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: f
      VEC(REAL,:), IN :: x,y,z
      VEC(REAL,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,ff,lf
      REAL :: rr,xx,yy,zz,x0,y0,z0,f0,bxb,byb,bzb
      REAL :: xx2,yy2,zz2,xx3,yy3,zz3,zz_f0
   ENSURE(size(f,1)==size(x),"SHELL1:make_grid_c_0 ... inconsistent f and x")
   ENSURE(size(f,1)==size(y),"SHELL1:make_grid_c_0 ... inconsistent f and x")
   ENSURE(size(f,1)==size(z),"SHELL1:make_grid_c_0 ... inconsistent f and x")
      n_pt = size(x)

      ! Origin
      x0 = self%position(1)
      y0 = self%position(2)
      z0 = self%position(3)
      select case (self%l)

      case (0)  ! S function ...........
         do n = 1,n_pt
            xx = x(n)-x0
            yy = y(n)-y0
            zz = z(n)-z0
            rr = xx*xx+yy*yy+zz*zz
            f(n,1) = sum(self%contraction*exp(-rr*self%exponent))
         end do

      case (1)  ! P function ...........
         do n = 1,n_pt
            xx = x(n)-x0
            yy = y(n)-y0
            zz = z(n)-z0
            rr = xx*xx+yy*yy+zz*zz
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            f(n,1) = xx*f0
            f(n,2) = yy*f0
            f(n,3) = zz*f0
         end do

      case (2)  ! D function ...........
         do n = 1,n_pt
            xx = x(n)-x0
            yy = y(n)-y0
            zz = z(n)-z0
            rr = xx*xx+yy*yy+zz*zz
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            zz_f0 = zz*f0
            f(n,1) = xx*xx*f0
            f(n,2) = yy*yy*f0
            f(n,3) = zz*zz*f0
            f(n,4) = xx*yy*f0
            f(n,5) = xx*zz_f0
            f(n,6) = yy*zz_f0
         end do

      case (3)  ! F function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         do n = 1,n_pt
            xx = x(n)-x0
            yy = y(n)-y0
            zz = z(n)-z0
            xx2 = xx*xx
            yy2 = yy*yy
            zz2 = zz*zz
            xx3 = xx2*xx
            yy3 = yy2*yy
            zz3 = zz2*zz
            rr = xx2+yy2+zz2
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            f(n,1) = f0*xx3
            f(n,2) = f0*yy3
            f(n,3) = f0*zz3
            f(n,4) = f0*xx2*yy
            f(n,5) = f0*xx2*zz
            f(n,6) = f0*xx*yy2
            f(n,7) = f0*yy2*zz
            f(n,8) = f0*xx*zz2
            f(n,9) = f0*yy*zz2
            f(n,10) = f0*xx*yy*zz
         end do

      case default  ! General l function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l)
         call create_(by,0,self%l)
         call create_(bz,0,self%l)
         do n = 1,n_pt
            xx = x(n)-x0
            yy = y(n)-y0
            zz = z(n)-z0
            rr = xx*xx+yy*yy+zz*zz
            f0 = sum(self%contraction*exp(-rr*self%exponent))

            ! Cartesian orbital part
            ! x**b,y**b,z**b for b=0,.l
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            do b = 2,self%l
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            f(n,:) = f0*bx(lx(:))*by(ly(:))*bz(lz(:))
         end do
         call destroy_(bz)
         call destroy_(by)
         call destroy_(bx)
      end select
   end subroutine

   PURE subroutine make_grid_s(self,F,x,y,z)
   ! Return "F(i,n)", the value of the *spherical* shell component "n"
   ! on grid point "i" given a set of grid points "(x(i),y(i),z(i))"
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z
      INT :: n_pt,n,m,c
      VEC(REAL,:), allocatable :: xx,yy,zz,rr
      MAT(REAL,:,:), allocatable :: angular
      REAL :: radial
      type(SPHERICAL_HARMONIC_TYPE) :: SH
   ENSURE(size(F,1)==size(x),"SHELL1:make_grid_s ... inconsistent f and x")
   ENSURE(size(F,1)==size(y),"SHELL1:make_grid_s ... inconsistent f and x")
   ENSURE(size(F,1)==size(z),"SHELL1:make_grid_s ... inconsistent f and x")
      xx = x-self%position(1)
      yy = y-self%position(2)
      zz = z-self%position(3)
      rr = xx*xx+yy*yy+zz*zz
      n_pt = size(xx)
      call create_(angular,n_pt,2*self%l+1)
      call set_L_(SH,self%l)  ! Don't need M value
      call values_at_(SH,xx,yy,zz,rr,angular)
      do m = 1,2*self%l+1
         do n = 1,n_pt
            radial = ZERO
            do c = 1,size(self%exponent)
               radial = radial+self%contraction(c)*exp(-rr(n)*self%exponent(c))
            end do
            F(n,m) = angular(n,m)*radial
         end do
      end do
   end subroutine

   PURE subroutine make_grid_2(self,F,x,y,z,r2,cutoff)
   ! Return "F(i,n)", the value of the basis function "n" on grid
   ! point "i" given a set of grid points "pt(i,1:3)"
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      MAT(REAL,:,:), allocatable :: Fc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_grid_c_(self,F,x,y,z,r2,cutoff)
      else if (self%l<2) then
         call make_grid_c_(self,F,x,y,z,r2,cutoff)
      else
         n_pt = size(x,1)
         call create_(Fc,n_pt,self%n_comp)
         call make_grid_c_(self,Fc,x,y,z,r2,cutoff)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_grid_c_1(self,F,x,y,z,r2,cutoff)
   ! Return "F(i,n)", the value of the shell component "n" on grid point "i".
   ! "x", "y", "z" are the differences between the points "pt" and the shell
   ! center, "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated under some
   ! circumstances, when shell belonging to a whole atom are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      VEC(REAL,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,ff,lf
      REAL :: rr,xx,yy,zz,f0,bxb,byb,bzb
      REAL :: r2_max,xx2,yy2,zz2,xx3,yy3,zz3,zz_f0
   ENSURE(size(f,1)==size(x),"SHELL1:make_grid_c_1 ... inconsistent f and x")
   ENSURE(size(f,1)==size(y),"SHELL1:make_grid_c_1 ... inconsistent f and x")
   ENSURE(size(f,1)==size(z),"SHELL1:make_grid_c_1 ... inconsistent f and x")
      n_pt = size(x)
      F = ZERO
      r2_max = r2_max_(self,cutoff)
      select case (self%l)

      case (0)  ! S function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            F(n,1) = sum(self%contraction*exp(-rr*self%exponent))
         end do

      case (1)  ! P function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(n,1) = xx*f0
            F(n,2) = yy*f0
            F(n,3) = zz*f0
         end do

      case (2)  ! D function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            zz_f0 = zz*f0
            F(n,1) = xx*xx*f0
            F(n,2) = yy*yy*f0
            F(n,3) = zz*zz*f0
            F(n,4) = xx*yy*f0
            F(n,5) = xx*zz_f0
            F(n,6) = yy*zz_f0
         end do

      case (3)  ! F function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            xx2 = xx*xx
            yy2 = yy*yy
            zz2 = zz*zz
            xx3 = xx2*xx
            yy3 = yy2*yy
            zz3 = zz2*zz
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(n,1) = f0*xx3
            F(n,2) = f0*yy3
            F(n,3) = f0*zz3
            F(n,4) = f0*xx2*yy
            F(n,5) = f0*xx2*zz
            F(n,6) = f0*xx*yy2
            F(n,7) = f0*yy2*zz
            F(n,8) = f0*xx*zz2
            F(n,9) = f0*yy*zz2
            F(n,10) = f0*xx*yy*zz
         end do

      case default  ! General l function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l)
         call create_(by,0,self%l)
         call create_(bz,0,self%l)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            f0 = sum(self%contraction*exp(-rr*self%exponent))

            ! Cartesian orbital part
            ! x**b,y**b,z**b for b=0,.l
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            do b = 2,self%l
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            F(n,:) = f0*bx(lx(:))*by(ly(:))*bz(lz(:))
         end do
         call destroy_(bz)
         call destroy_(by)
         call destroy_(bx)
      end select
   end subroutine

   PURE subroutine make_grid_3(self,F,n_pt,x,y,z,r2,lg)
   ! Return "F(i,n)", the value of the shell component "n" on grid
   ! point "i".  "x", "y", "z" are precomputed differences between the
   ! points "pt" and the shell center, "r2" is a precomputed array for
   ! the square of the distance from the points "pt" to ".position".
   ! "lg" is the log of the maximum value for this shell, used to
   ! skip elements too small ...  see make_skip_list.
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_pt
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(INT,:), OUT :: lg
      MAT(REAL,:,:), allocatable :: Fc
      if (NOT self%is_spherical) then
         call make_grid_c_(self,F,n_pt,x,y,z,r2,lg)
      else if (self%l<2) then
         call make_grid_c_(self,F,n_pt,x,y,z,r2,lg)
      else
         call create_(Fc,n_pt,self%n_comp)
         call make_grid_c_(self,Fc,n_pt,x,y,z,r2,lg)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_grid_c_2(self,F,n_pt,x,y,z,r2,lg)
   ! Return "F(i,n)", the value of the shell component "n" on grid
   ! point "i".  "x", "y", "z" are precomputed differences between the
   ! points "pt" and the shell center, "r2" is a precomputed array for
   ! the square of the distance from the points "pt" to "position".
   ! "lg" is the log of the maximum value for this shell, used to
   ! skip elements too small ...  see make_skip_list.
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_pt
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(INT,:), OUT :: lg
      VEC(REAL,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n,b,ff,lf
      REAL :: rr,xx,yy,zz,f0,bxb,byb,bzb
      REAL :: xx2,yy2,zz2,xx3,yy3,zz3,zz_f0
      select case (self%l)

      case (0)  ! S function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(n,1) = f0
            lg(n) = ceiling(log10(f0))
         end do

      case (1)  ! P function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(n,1) = xx*f0
            F(n,2) = yy*f0
            F(n,3) = zz*f0
            lg(n) = ceiling(log10(maxval(F(n,:))))
         end do

      case (2)  ! D function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            zz_f0 = zz*f0
            F(n,1) = xx*xx*f0
            F(n,2) = yy*yy*f0
            F(n,3) = zz*zz*f0
            F(n,4) = xx*yy*f0
            F(n,5) = xx*zz_f0
            F(n,6) = yy*zz_f0
            lg(n) = ceiling(log10(maxval(F(n,:))))
         end do

      case (3)  ! F function ...........
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            xx2 = xx*xx
            yy2 = yy*yy
            zz2 = zz*zz
            xx3 = xx2*xx
            yy3 = yy2*yy
            zz3 = zz2*zz
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(n,1) = f0*xx3
            F(n,2) = f0*yy3
            F(n,3) = f0*zz3
            F(n,4) = f0*xx2*yy
            F(n,5) = f0*xx2*zz
            F(n,6) = f0*xx*yy2
            F(n,7) = f0*yy2*zz
            F(n,8) = f0*xx*zz2
            F(n,9) = f0*yy*zz2
            F(n,10) = f0*xx*yy*zz
            lg(n) = ceiling(log10(maxval(F(n,:))))
         end do

      case default  ! General l function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l)
         call create_(by,0,self%l)
         call create_(bz,0,self%l)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))

            ! Cartesian orbital part
            ! x**b,y**b,z**b for b=0,.l
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            do b = 2,self%l
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            F(n,:) = f0*bx(lx(:))*by(ly(:))*bz(lz(:))
            lg(n) = ceiling(log10(maxval(F(n,:))))
         end do
         call destroy_(bz)
         call destroy_(by)
         call destroy_(bx)
      end select
   end subroutine

   PURE subroutine make_skip_grid(self,F,n_keep,x,y,z,r2,skip,n_pt)
   ! Return "F(i,n)", the value of the shell component "n" on grid
   ! point "i".  "x", "y", "z" are precomputed differences between the
   ! points "pt" and the shell center, "r2" is a precomputed array for
   ! the square of the distance from the points "pt" to ".position". "skip"
   ! is an array to skip elements too small ...  see make_skip_list.
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_keep,n_pt
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(BIN,:), IN :: skip
      MAT(REAL,:,:), allocatable :: Fc
      if (NOT self%is_spherical) then
         call make_skip_grid_c_(self,F,x,y,z,r2,skip,n_pt)
      else if (self%l<2) then
         call make_skip_grid_c_(self,F,x,y,z,r2,skip,n_pt)
      else
         call create_(Fc,n_keep,self%n_comp)
         call make_skip_grid_c_(self,Fc,x,y,z,r2,skip,n_pt)
         F = matmul(Fc,spherical_harmonics_for(self%l)%element)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_skip_grid_c(self,F,x,y,z,r2,skip,n_pt)
   ! Return "F(i,n)", the value of the shell component "n" on grid
   ! point "i".  "x", "y", "z" are precomputed differences between the
   ! points "pt" and the shell center, "r2" is a precomputed array for
   ! the square of the distance from the points "pt" to "position". "skip"
   ! is an array to skip elements too small ...  see make_skip_list.
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_pt
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(BIN,:), IN :: skip

! ENSURE(f.dim1==x.dim,"inconsistent f and x")
! ENSURE(f.dim1==y.dim,"inconsistent f and x")
! ENSURE(f.dim1==z.dim,"inconsistent f and x")
      VEC(REAL,:), allocatable :: bx,by,bz
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n,i,b,ff,lf
      REAL :: rr,xx,yy,zz,f0,bxb,byb,bzb
      REAL :: xx2,yy2,zz2,xx3,yy3,zz3,zz_f0
      select case (self%l)

      case (0)  ! S function ...........
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            F(i,1) = sum(self%contraction*exp(-rr*self%exponent))
         end do

      case (1)  ! P function ...........
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(i,1) = xx*f0
            F(i,2) = yy*f0
            F(i,3) = zz*f0
         end do

      case (2)  ! D function ...........
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            zz_f0 = zz*f0
            F(i,1) = xx*xx*f0
            F(i,2) = yy*yy*f0
            F(i,3) = zz*zz*f0
            F(i,4) = xx*yy*f0
            F(i,5) = xx*zz_f0
            F(i,6) = yy*zz_f0
         end do

      case (3)  ! F function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            xx2 = xx*xx
            yy2 = yy*yy
            zz2 = zz*zz
            xx3 = xx2*xx
            yy3 = yy2*yy
            zz3 = zz2*zz
            f0 = sum(self%contraction*exp(-rr*self%exponent))
            F(i,1) = f0*xx3
            F(i,2) = f0*yy3
            F(i,3) = f0*zz3
            F(i,4) = f0*xx2*yy
            F(i,5) = f0*xx2*zz
            F(i,6) = f0*xx*yy2
            F(i,7) = f0*yy2*zz
            F(i,8) = f0*xx*zz2
            F(i,9) = f0*yy*zz2
            F(i,10) = f0*xx*yy*zz
         end do

      case default  ! General l function ...........
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l)
         call create_(by,0,self%l)
         call create_(bz,0,self%l)
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            f0 = sum(self%contraction*exp(-rr*self%exponent))

            ! Cartesian orbital part
            ! x**b,y**b,z**b for b=0,.l
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            do b = 2,self%l
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            F(i,:) = f0*bx(lx(:))*by(ly(:))*bz(lz(:))
         end do
         call destroy_(bz)
         call destroy_(by)
         call destroy_(bx)
      end select
   end subroutine

   PURE subroutine make_skip_nabla_grid(self,Gx,Gy,Gz,F0,n_keep,x,y,z,r2,skip,n_pt)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F0(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_keep,n_pt
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F0
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(BIN,:), IN :: skip
      MAT(REAL,:,:), allocatable :: Gxc,Gyc,Gzc,Fc
      if (NOT self%is_spherical) then
         call make_skip_nabla_grid_c_(self,Gx,Gy,Gz,F0,x,y,z,r2,skip,n_pt)
      else if (self%l<2) then
         call make_skip_nabla_grid_c_(self,Gx,Gy,Gz,F0,x,y,z,r2,skip,n_pt)
      else
         call create_(Fc,n_keep,self%n_comp)
         call create_(Gxc,n_keep,self%n_comp)
         call create_(Gyc,n_keep,self%n_comp)
         call create_(Gzc,n_keep,self%n_comp)
         call make_skip_nabla_grid_c_(self,Gxc,Gyc,Gzc,Fc,x,y,z,r2,skip,n_pt)
         call to_product_of_(F0,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gx,Gxc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gy,Gyc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gz,Gzc,spherical_harmonics_for(self%l)%element)
         call destroy_(Gzc)
         call destroy_(Gyc)
         call destroy_(Gxc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_skip_nabla_grid_c(self,Gx,Gy,Gz,F0,x,y,z,r2,skip,n_pt)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F0(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      INT, IN :: n_pt
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F0
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(BIN,:), IN :: skip

! ENSURE(Gx.is_same_shape_as(Gy),"Gx and Gy incompatible")
! ENSURE(Gx.is_same_shape_as(Gz),"Gx and Gz incompatible")
! ENSURE(Gx.is_same_shape_as(F0),"Gx and F0 incompatible")
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n,i,b,nxb,nyb,nzb,ff,lf
      REAL :: xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb,by_bz
      F0 = ZERO
      Gx = ZERO
      Gy = ZERO
      Gz = ZERO
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)  ! S function ....
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            F0(i,1) = g0
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            Gx(i,1) = g1*xx
            Gy(i,1) = g1*yy
            Gz(i,1) = g1*zz
         end do

      case (1)  ! P function ....
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1x = xx*g0
            g1y = yy*g0
            g1z = zz*g0
            F0(i,1) = g1x
            F0(i,2) = g1y
            F0(i,3) = g1z
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            Gx(i,1) = g0+xx*g1x
            Gy(i,1) = xx*g1y
            Gz(i,1) = xx*g1z
            Gx(i,2) = yy*g1x
            Gy(i,2) = g0+yy*g1y
            Gz(i,2) = yy*g1z
            Gx(i,3) = zz*g1x
            Gy(i,3) = zz*g1y
            Gz(i,3) = g0+zz*g1z
         end do

      case default  ! General case
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         i = 0
         do n = 1,n_pt
            if (skip(n)) cycle
            i = i+1
            xx = x(n)
            yy = y(n)
            zz = z(n)

            ! Radial part, summed over primitives
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               g1 = bxb*by_bz*g0
               F0(i,b) = g1
               Gx(i,b) = gxb(nxb)*by_bz
               Gy(i,b) = bxb*gyb(nyb)*bzb
               Gz(i,b) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_0(self,G,pt)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)".
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), IN :: pt
      MAT3(REAL,:,:,:), allocatable :: Gc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,G,pt)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,G,pt)
      else
         n_pt = size(pt,1)
         call create_(Gc,n_pt,self%n_comp,3)
         call make_nabla_grid_c_(self,Gc,pt)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call destroy_(Gc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_0(self,G,pt)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)".
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: xx,yy,zz,rr
      REAL :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb
   ENSURE(size(pt,2)==3,"SHELL1:make_nabla_grid_c_0 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%position(1)
      y = self%position(2)
      z = self%position(3)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            g1 = -TWO*sum(self%exponent*self%contraction*exp(-self%exponent*rr))
            G(n,1,1) = g1*xx
            G(n,1,2) = g1*yy
            G(n,1,3) = g1*zz
         end do

      case (1)
         call create_(cc_exp_rr,self%n_cc)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            G(n,1,1) = g0+xx*g1x
            G(n,1,2) = xx*g1y
            G(n,1,3) = xx*g1z
            G(n,2,1) = yy*g1x
            G(n,2,2) = g0+yy*g1y
            G(n,2,3) = yy*g1z
            G(n,3,1) = zz*g1x
            G(n,3,2) = zz*g1y
            G(n,3,3) = g0+zz*g1z
         end do
         call destroy_(cc_exp_rr)

      case default
         call create_(cc_exp_rr,self%n_cc)
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz

            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx*g1x; gyb(1) = g0+yy*g1y; gzb(1) = g0+zz*g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx*g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy*g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz*g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Loop over basis functions
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               G(n,b,1) = gxb(nxb)*by(nyb)*bz(nzb)
               G(n,b,2) = bx(nxb)*gyb(nyb)*bz(nzb)
               G(n,b,3) = bx(nxb)*by(nyb)*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
         call destroy_(cc_exp_rr)
      end select
   end subroutine

   PURE subroutine make_nabla_grid_1(self,G,x,y,z,r2,cutoff)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell component "n"
   ! on grid point "i" given a set of grid points "pt(i,1:3)".
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      MAT3(REAL,:,:,:), allocatable :: Gc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,G,x,y,z,r2,cutoff)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,G,x,y,z,r2,cutoff)
      else
         n_pt = size(x,1)
         call create_(Gc,n_pt,self%n_comp,3)
         call make_nabla_grid_c_(self,Gc,x,y,z,r2,cutoff)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call destroy_(Gc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_1(self,G,x,y,z,r2,cutoff)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell component "n"
   ! on grid point "i" given a set of grid points "pt(i,1:3)".
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb
      REAL :: r2_max,xx,yy,zz,rr
      n_pt = size(x)
      G = ZERO
      r2_max = r2_max_(self,cutoff)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            g1 = -TWO*sum(self%exponent*self%contraction*exp(-self%exponent*rr))
            G(n,1,1) = g1*xx
            G(n,1,2) = g1*yy
            G(n,1,3) = g1*zz
         end do

      case (1)
         call create_(cc_exp_rr,self%n_cc)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            G(n,1,1) = g0+xx*g1x
            G(n,1,2) = xx*g1y
            G(n,1,3) = xx*g1z
            G(n,2,1) = yy*g1x
            G(n,2,2) = g0+yy*g1y
            G(n,2,3) = yy*g1z
            G(n,3,1) = zz*g1x
            G(n,3,2) = zz*g1y
            G(n,3,3) = g0+zz*g1z
         end do
         call destroy_(cc_exp_rr)

      case default
         call create_(cc_exp_rr,self%n_cc)
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz

            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx*g1x; gyb(1) = g0+yy*g1y; gzb(1) = g0+zz*g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx*g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy*g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz*g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Loop over basis functions
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               G(n,b,1) = gxb(nxb)*by(nyb)*bz(nzb)
               G(n,b,2) = bx(nxb)*gyb(nyb)*bz(nzb)
               G(n,b,3) = bx(nxb)*by(nyb)*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
         call destroy_(cc_exp_rr)
      end select
   end subroutine

   PURE subroutine make_nabla_grid_2(self,G,F,pt)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), OUT :: F
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: Fc
      MAT3(REAL,:,:,:), allocatable :: Gc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,G,F,pt)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,G,F,pt)
      else
         n_pt = size(pt,1)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gc,n_pt,self%n_comp,3)
         call make_nabla_grid_c_(self,Gc,Fc,pt)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call destroy_(Gc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_2(self,G,F,pt)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), OUT :: F
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb,by_bz
   ENSURE(size(pt,2)==3,"SHELL1:make_nabla_grid_c_2 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%position(1)
      y = self%position(2)
      z = self%position(3)
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            F(n,1) = sum(cc_exp_rr)
            G(n,1,1) = g1*xx
            G(n,1,2) = g1*yy
            G(n,1,3) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            F(n,1) = xx*g0
            F(n,2) = yy*g0
            F(n,3) = zz*g0
            G(n,1,1) = g0+xx*g1x
            G(n,1,2) = xx*g1y
            G(n,1,3) = xx*g1z
            G(n,2,1) = yy*g1x
            G(n,2,2) = g0+yy*g1y
            G(n,2,3) = yy*g1z
            G(n,3,1) = zz*g1x
            G(n,3,2) = zz*g1y
            G(n,3,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z

            ! Radial part, summed over primitives
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bxb = xx; byb = yy; bzb = zz
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               F(n,b) = bxb*by_bz*g0
               G(n,b,1) = gxb(nxb)*by_bz
               G(n,b,2) = bxb*gyb(nyb)*bzb
               G(n,b,3) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_3(self,G,F,x,y,z,r2)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.  "x", "y", "z" are the differences between the
   ! points "pt" and the shell center, "r2" is an array for the square
   ! of the distance from the points "pt" to ".position". These arrays can
   ! be efficiently precalculated under some circumstances, when shell
   ! belonging to a whole atom are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      MAT(REAL,:,:), allocatable :: Fc
      MAT3(REAL,:,:,:), allocatable :: Gc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,G,F,x,y,z,r2)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,G,F,x,y,z,r2)
      else
         n_pt = size(x,1)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gc,n_pt,self%n_comp,3)
         call make_nabla_grid_c_(self,Gc,Fc,x,y,z,r2)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call destroy_(Gc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_3(self,G,F,x,y,z,r2)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.  "x", "y", "z" are the differences between the
   ! points "pt" and the shell center, "r2" is an array for the square
   ! of the distance from the points "pt" to ".position". These arrays can
   ! be efficiently precalculated under some circumstances, when shell
   ! belonging to a whole atom are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb,by_bz
      n_pt = size(x)
      G = ZERO
      F = ZERO
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            F(n,1) = sum(cc_exp_rr)
            G(n,1,1) = g1*xx
            G(n,1,2) = g1*yy
            G(n,1,3) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            F(n,1) = xx*g0
            F(n,2) = yy*g0
            F(n,3) = zz*g0
            G(n,1,1) = g0+xx*g1x
            G(n,1,2) = xx*g1y
            G(n,1,3) = xx*g1z
            G(n,2,1) = yy*g1x
            G(n,2,2) = g0+yy*g1y
            G(n,2,3) = yy*g1z
            G(n,3,1) = zz*g1x
            G(n,3,2) = zz*g1y
            G(n,3,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bxb = xx; byb = yy; bzb = zz
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               F(n,b) = bxb*by_bz*g0
               G(n,b,1) = gxb(nxb)*by_bz
               G(n,b,2) = bxb*gyb(nyb)*bzb
               G(n,b,3) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_4(self,G,F,x,y,z,r2,cutoff)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.  "x", "y", "z" are the differences between the
   ! points "pt" and the shell center, "r2" is an array for the square
   ! of the distance from the points "pt" to ".position". These arrays can
   ! be efficiently precalculated under some circumstances, when shell
   ! belonging to a whole atom are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      MAT(REAL,:,:), allocatable :: Fc
      MAT3(REAL,:,:,:), allocatable :: Gc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,G,F,x,y,z,r2,cutoff)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,G,F,x,y,z,r2,cutoff)
      else
         n_pt = size(x,1)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gc,n_pt,self%n_comp,3)
         call make_nabla_grid_c_(self,Gc,Fc,x,y,z,r2,cutoff)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call destroy_(Gc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_4(self,G,F,x,y,z,r2,cutoff)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.  "x", "y", "z" are the differences between the
   ! points "pt" and the shell center, "r2" is an array for the square
   ! of the distance from the points "pt" to ".position". These arrays can
   ! be efficiently precalculated under some circumstances, when shell
   ! belonging to a whole atom are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), OUT :: F
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: r2_max,xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb,by_bz
      n_pt = size(x)
      G = ZERO
      F = ZERO
      r2_max = r2_max_(self,cutoff)
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            F(n,1) = sum(cc_exp_rr)
            G(n,1,1) = g1*xx
            G(n,1,2) = g1*yy
            G(n,1,3) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            F(n,1) = xx*g0
            F(n,2) = yy*g0
            F(n,3) = zz*g0
            G(n,1,1) = g0+xx*g1x
            G(n,1,2) = xx*g1y
            G(n,1,3) = xx*g1z
            G(n,2,1) = yy*g1x
            G(n,2,2) = g0+yy*g1y
            G(n,2,3) = yy*g1z
            G(n,3,1) = zz*g1x
            G(n,3,2) = zz*g1y
            G(n,3,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         allocate(lx(lf-ff))
         allocate(ly(lf-ff))
         allocate(lz(lf-ff))
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = x(n)
            yy = y(n)
            zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bxb = xx; byb = yy; bzb = zz
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               F(n,b) = bxb*by_bz*g0
               G(n,b,1) = gxb(nxb)*by_bz
               G(n,b,2) = bxb*gyb(nyb)*bzb
               G(n,b,3) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
         deallocate(lx)
         deallocate(ly)
         deallocate(lz)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_5(self,Gx,Gy,Gz,F,pt)
   ! Return "G(i,n,1:3)", the value of the gradient of the shell
   ! component "n" on grid point "i" given a set of grid points
   ! "pt(i,1:3)". Also return the "F(i,n)" the value of the shell
   ! component "n" on grid point "i". It is more efficient to compute
   ! both at once.  "x", "y", "z" are the differences between the
   ! points "pt" and the shell center, "r2" is an array for the square
   ! of the distance from the points "pt" to ".position". These arrays can
   ! be efficiently precalculated under some circumstances, when shell
   ! belonging to a whole atom are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,:,:), allocatable :: Gxc,Gyc,Gzc,Fc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F,pt)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F,pt)
      else
         n_pt = size(pt,1)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gxc,n_pt,self%n_comp)
         call create_(Gyc,n_pt,self%n_comp)
         call create_(Gzc,n_pt,self%n_comp)
         call make_nabla_grid_c_(self,Gxc,Gyc,Gzc,Fc,pt)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gx,Gxc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gy,Gyc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gz,Gzc,spherical_harmonics_for(self%l)%element)
         call destroy_(Gzc)
         call destroy_(Gyc)
         call destroy_(Gxc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_5(self,Gx,Gy,Gz,F,pt)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,x,y,z,bxb,byb,bzb,by_bz
   ENSURE(is_same_shape_as_(Gx,Gy),"SHELL1:make_nabla_grid_c_5 ... Gx and Gy incompatible")
   ENSURE(is_same_shape_as_(Gx,Gz),"SHELL1:make_nabla_grid_c_5 ... Gx and Gz incompatible")
   ENSURE(is_same_shape_as_(Gx,F),"SHELL1:make_nabla_grid_c_5 ... Gx and F incompatible")
   ENSURE(size(pt,2)==3,"SHELL1:make_nabla_grid_c_5 ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)
      x = self%position(1)
      y = self%position(2)
      z = self%position(3)
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            F(n,1) = sum(cc_exp_rr)
            Gx(n,1) = g1*xx
            Gy(n,1) = g1*yy
            Gz(n,1) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            F(n,1) = xx*g0
            F(n,2) = yy*g0
            F(n,3) = zz*g0
            Gx(n,1) = g0+xx*g1x
            Gy(n,1) = xx*g1y
            Gz(n,1) = xx*g1z
            Gx(n,2) = yy*g1x
            Gy(n,2) = g0+yy*g1y
            Gz(n,2) = yy*g1z
            Gx(n,3) = zz*g1x
            Gy(n,3) = zz*g1y
            Gz(n,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         allocate(lx(lf-ff))
         allocate(ly(lf-ff))
         allocate(lz(lf-ff))
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = pt(n,1)-x
            yy = pt(n,2)-y
            zz = pt(n,3)-z

            ! Radial part, summed over primitives
            rr = xx*xx+yy*yy+zz*zz
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               F(n,b) = bxb*by_bz*g0
               Gx(n,b) = gxb(nxb)*by_bz
               Gy(n,b) = bxb*gyb(nyb)*bzb
               Gz(n,b) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
         deallocate(lx)
         deallocate(ly)
         deallocate(lz)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_6(self,Gx,Gy,Gz,F,x,y,z,r2)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F
      VEC(REAL,:), IN :: x,y,z,r2
      MAT(REAL,:,:), allocatable :: Gxc,Gyc,Gzc,Fc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F,x,y,z,r2)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F,x,y,z,r2)
      else
         n_pt = size(x,1)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gxc,n_pt,self%n_comp)
         call create_(Gyc,n_pt,self%n_comp)
         call create_(Gzc,n_pt,self%n_comp)
         call make_nabla_grid_c_(self,Gxc,Gyc,Gzc,Fc,x,y,z,r2)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gx,Gxc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gy,Gyc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gz,Gzc,spherical_harmonics_for(self%l)%element)
         call destroy_(Gzc)
         call destroy_(Gyc)
         call destroy_(Gxc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_6(self,Gx,Gy,Gz,F,x,y,z,r2)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F
      VEC(REAL,:), IN :: x,y,z,r2
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb,by_bz
   ENSURE(is_same_shape_as_(Gx,Gy),"SHELL1:make_nabla_grid_c_6 ... Gx and Gy incompatible")
   ENSURE(is_same_shape_as_(Gx,Gz),"SHELL1:make_nabla_grid_c_6 ... Gx and Gz incompatible")
   ENSURE(is_same_shape_as_(Gx,F),"SHELL1:make_nabla_grid_c_6 ... Gx and F incompatible")
      n_pt = size(x)
      F = ZERO
      Gx = ZERO
      Gy = ZERO
      Gz = ZERO
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = x(n); yy = y(n); zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            F(n,1) = sum(cc_exp_rr)
            Gx(n,1) = g1*xx
            Gy(n,1) = g1*yy
            Gz(n,1) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            xx = x(n); yy = y(n); zz = z(n)
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            F(n,1) = xx*g0
            F(n,2) = yy*g0
            F(n,3) = zz*g0
            Gx(n,1) = g0+xx*g1x
            Gy(n,1) = xx*g1y
            Gz(n,1) = xx*g1z
            Gx(n,2) = yy*g1x
            Gy(n,2) = g0+yy*g1y
            Gz(n,2) = yy*g1z
            Gx(n,3) = zz*g1x
            Gy(n,3) = zz*g1y
            Gz(n,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = x(n); yy = y(n); zz = z(n)

            ! Radial part, summed over primitives
            rr = r2(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               F(n,b) = bxb*by_bz*g0
               Gx(n,b) = gxb(nxb)*by_bz
               Gy(n,b) = bxb*gyb(nyb)*bzb
               Gz(n,b) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_7(self,Gx,Gy,Gz,F0,skip_all,x,y,z,r2,cutoff)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F0(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F0
      VEC(BIN,:), OUT :: skip_all
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      MAT(REAL,:,:), allocatable :: Gxc,Gyc,Gzc,Fc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F0,skip_all,x,y,z,r2,cutoff)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F0,skip_all,x,y,z,r2,cutoff)
      else
         n_pt = size(x)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gxc,n_pt,self%n_comp)
         call create_(Gyc,n_pt,self%n_comp)
         call create_(Gzc,n_pt,self%n_comp)
         call make_nabla_grid_c_(self,Gxc,Gyc,Gzc,Fc,skip_all,x,y,z,r2,cutoff)
         call to_product_of_(F0,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gx,Gxc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gy,Gyc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gz,Gzc,spherical_harmonics_for(self%l)%element)
         call destroy_(Gzc)
         call destroy_(Gyc)
         call destroy_(Gxc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_7(self,Gx,Gy,Gz,F0,skip_all,x,y,z,r2,cutoff)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F0(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F0
      VEC(BIN,:), OUT :: skip_all
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: r2_max,xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb,by_bz
   ENSURE(is_same_shape_as_(Gx,Gy),"SHELL1:make_nabla_grid_c_7 ... Gx and Gy incompatible")
   ENSURE(is_same_shape_as_(Gx,Gz),"SHELL1:make_nabla_grid_c_7 ... Gx and Gz incompatible")
   ENSURE(is_same_shape_as_(Gx,F0),"SHELL1:make_nabla_grid_c_7 ... Gx and F0 incompatible")
      n_pt = size(x)
      r2_max = r2_max_(self,cutoff)
      F0 = ZERO
      Gx = ZERO
      Gy = ZERO
      Gz = ZERO
      skip_all = TRUE
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = x(n); yy = y(n); zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            skip_all(1) = FALSE
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            F0(n,1) = g0
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            Gx(n,1) = g1*xx
            Gy(n,1) = g1*yy
            Gz(n,1) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            xx = x(n); yy = y(n); zz = z(n)
            rr = r2(n)
            if (rr>r2_max) cycle
            skip_all(1:3) = FALSE
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1x = xx*g0
            g1y = yy*g0
            g1z = zz*g0
            F0(n,1) = g1x
            F0(n,2) = g1y
            F0(n,3) = g1z
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            Gx(n,1) = g0+xx*g1x
            Gy(n,1) = xx*g1y
            Gz(n,1) = xx*g1z
            Gx(n,2) = yy*g1x
            Gy(n,2) = g0+yy*g1y
            Gz(n,2) = yy*g1z
            Gx(n,3) = zz*g1x
            Gy(n,3) = zz*g1y
            Gz(n,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            xx = x(n); yy = y(n); zz = z(n)

            ! Radial part, summed over primitives
            rr = r2(n)
            if (rr>r2_max) cycle
            skip_all(1:self%n_comp) = FALSE
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               g1 = bxb*by_bz*g0
               F0(n,b) = g1
               Gx(n,b) = gxb(nxb)*by_bz
               Gy(n,b) = bxb*gyb(nyb)*bzb
               Gz(n,b) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_nabla_grid_8(self,Gx,Gy,Gz,F,x,y,z,r2,cutoff)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      MAT(REAL,:,:), allocatable :: Gxc,Gyc,Gzc,Fc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F,x,y,z,r2,cutoff)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,Gx,Gy,Gz,F,x,y,z,r2,cutoff)
      else
         n_pt = size(x)
         call create_(Fc,n_pt,self%n_comp)
         call create_(Gxc,n_pt,self%n_comp)
         call create_(Gyc,n_pt,self%n_comp)
         call create_(Gzc,n_pt,self%n_comp)
         call make_nabla_grid_c_(self,Gxc,Gyc,Gzc,Fc,x,y,z,r2,cutoff)
         call to_product_of_(F,Fc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gx,Gxc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gy,Gyc,spherical_harmonics_for(self%l)%element)
         call to_product_of_(Gz,Gzc,spherical_harmonics_for(self%l)%element)
         call destroy_(Gzc)
         call destroy_(Gyc)
         call destroy_(Gxc)
         call destroy_(Fc)
      end if
   end subroutine

   PURE subroutine make_nabla_grid_c_8(self,Gx,Gy,Gz,F,x,y,z,r2,cutoff)
   ! Return "Gj(i,n)", the value of the gradient with respect to
   ! coordinate "j" of the shell component "n", evaluated at grid
   ! point "i", given a set of grid points "pt(i,1:3)". Likewise,
   ! "F(i,n)" is the value of the shell component "n" evaluated at
   ! grid point "i". It is more eficient to compute both "Gn" and "F"
   ! at once. The "Gn" are presented as separate components to allow
   ! contiguous memory access for the calling routine.  "x", "y", "z"
   ! are the differences between the points "pt" and the shell center,
   ! "r2" is an array for the square of the distance from the points
   ! "pt" to ".position". These arrays can be efficiently precalculated
   ! under some circumstances, when shell belonging to a whole atom
   ! are being calculated.
      type(SHELL1_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Gx,Gy,Gz,F
      VEC(REAL,:), IN :: x,y,z,r2
      REAL, IN :: cutoff
      VEC(REAL,:), allocatable :: cc_exp_rr,bx,by,bz,gxb,gyb,gzb
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,nxb,nyb,nzb,ff,lf
      REAL :: r2_max,xx,yy,zz,rr,xx_g1x,yy_g1y,zz_g1z
      REAL :: g0,g1,g1x,g1y,g1z,bxb,byb,bzb,by_bz
   ENSURE(is_same_shape_as_(Gx,Gy),"SHELL1:make_nabla_grid_c_8 ... Gx and Gy incompatible")
   ENSURE(is_same_shape_as_(Gx,Gz),"SHELL1:make_nabla_grid_c_8 ... Gx and Gz incompatible")
   ENSURE(is_same_shape_as_(Gx,F),"SHELL1:make_nabla_grid_c_8 ... Gx and F incompatible")
      n_pt = size(x)
      r2_max = r2_max_(self,cutoff)
      F = ZERO
      Gx = ZERO
      Gy = ZERO
      Gz = ZERO
      call create_(cc_exp_rr,self%n_cc)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            rr = r2(n)
            if (rr>r2_max) cycle
            xx = x(n); yy = y(n); zz = z(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            F(n,1) = sum(cc_exp_rr)
            Gx(n,1) = g1*xx
            Gy(n,1) = g1*yy
            Gz(n,1) = g1*zz
         end do

      case (1)
         do n = 1,n_pt
            rr = r2(n)
            if (rr>r2_max) cycle
            xx = x(n); yy = y(n); zz = z(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            F(n,1) = xx*g0
            F(n,2) = yy*g0
            F(n,3) = zz*g0
            Gx(n,1) = g0+xx*g1x
            Gy(n,1) = xx*g1y
            Gz(n,1) = xx*g1z
            Gx(n,2) = yy*g1x
            Gy(n,2) = g0+yy*g1y
            Gz(n,2) = yy*g1z
            Gx(n,3) = zz*g1x
            Gy(n,3) = zz*g1y
            Gz(n,3) = g0+zz*g1z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         call create_(bx,0,self%l); call create_(by,0,self%l); call create_(bz,0,self%l)
         call create_(gxb,0,self%l); call create_(gyb,0,self%l); call create_(gzb,0,self%l)
         do n = 1,n_pt
            rr = r2(n)
            if (rr>r2_max) cycle
            xx = x(n); yy = y(n); zz = z(n)
            cc_exp_rr = self%contraction*exp(-self%exponent*rr)
            g0 = sum(cc_exp_rr)
            g1 = -TWO*sum(self%exponent*cc_exp_rr)

            ! Cartesian orbital part
            ! gxb(b)=x**(b-1),  bx(b)=x**b for b=0,.l
            g1x = g1*xx
            g1y = g1*yy
            g1z = g1*zz
            xx_g1x = xx*g1x
            yy_g1y = yy*g1y
            zz_g1z = zz*g1z
            bx(0) = ONE; by(0) = ONE; bz(0) = ONE
            bx(1) = xx; by(1) = yy; bz(1) = zz
            bxb = xx; byb = yy; bzb = zz
            gxb(0) = g1x; gyb(0) = g1y; gzb(0) = g1z
            gxb(1) = g0+xx_g1x; gyb(1) = g0+yy_g1y; gzb(1) = g0+zz_g1z
            do b = 2,self%l
               gxb(b) = (b*g0+xx_g1x)*bxb  ! (n-1)th power
               gyb(b) = (b*g0+yy_g1y)*byb  ! of the xyz part
               gzb(b) = (b*g0+zz_g1z)*bzb
               bxb = bxb*xx; byb = byb*yy; bzb = bzb*zz
               bx(b) = bxb; by(b) = byb; bz(b) = bzb
            end do

            ! Combine cartesian orbital powers with the exponential part
            do b = 1,self%n_comp
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               bxb = bx(nxb)
               byb = by(nyb)
               bzb = bz(nzb)
               by_bz = byb*bzb
               F(n,b) = bxb*by_bz*g0
               Gx(n,b) = gxb(nxb)*by_bz
               Gy(n,b) = bxb*gyb(nyb)*bzb
               Gz(n,b) = bxb*byb*gzb(nzb)
            end do
         end do
         call destroy_(gzb); call destroy_(gyb); call destroy_(gxb)
         call destroy_(bz); call destroy_(by); call destroy_(bx)
      end select
      call destroy_(cc_exp_rr)
   end subroutine

   PURE subroutine make_laplacian_grid_0(self,G,pt)
   ! Return "G(i,n,1:3)", the value of the second derivatives
   ! (d/dx2,d/dy2,d/dz2) of the shell component "n" on grid point "i"
   ! given a set of grid points "pt(i,1:3)".
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT(REAL,:,:), IN :: pt
      MAT3(REAL,:,:,:), allocatable :: Gc
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_nabla_grid_c_(self,G,pt)
      else if (self%l<2) then
         call make_nabla_grid_c_(self,G,pt)
      else
         n_pt = size(pt,1)
         call create_(Gc,n_pt,self%n_comp,3)
         call make_nabla_grid_c_(self,Gc,pt)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call destroy_(Gc)
      end if
   end subroutine

   PURE subroutine make_laplacian_grid_1(self,G,H,I,pt)
   ! Return "G(j,n,1:3)", the value of the second derivatives
   ! (d/dx2,d/dy2,d/dz2) of the shell component n on grid point j
   ! given a set of grid points "pt(j,1:3)". Also return "H(j,n,1:3)",
   ! the value of the first derivative, and return "I(n)", the value
   ! of the gaussian
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G
      MAT3(REAL,:,:,:), OUT :: H
      MAT(REAL,:,:), OUT :: I
      MAT(REAL,:,:), IN :: pt
      MAT3(REAL,:,:,:), allocatable :: Gc,Hc
      MAT(REAL,:,:), allocatable :: Ic
      INT :: n_pt
      if (NOT self%is_spherical) then
         call make_laplacian_grid_c_(self,G,H,I,pt)
      else if (self%l<2) then
         call make_laplacian_grid_c_(self,G,H,I,pt)
      else
         n_pt = size(pt,1)
         call create_(Gc,n_pt,self%n_comp,3)
         call create_(Hc,n_pt,self%n_comp,3)
         call create_(Ic,n_pt,self%n_comp)
         call make_laplacian_grid_c_(self,Gc,Hc,Ic,pt)
         call to_product_of_(G(:,:,1),Gc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,2),Gc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(G(:,:,3),Gc(:,:,3),spherical_harmonics_for(self%l)%element)
         call to_product_of_(H(:,:,1),Hc(:,:,1),spherical_harmonics_for(self%l)%element)
         call to_product_of_(H(:,:,2),Hc(:,:,2),spherical_harmonics_for(self%l)%element)
         call to_product_of_(H(:,:,3),Hc(:,:,3),spherical_harmonics_for(self%l)%element)
         call to_product_of_(I,Ic,spherical_harmonics_for(self%l)%element)
         call destroy_(Ic)
         call destroy_(Hc)
         call destroy_(Gc)
      end if
   end subroutine

   PURE subroutine make_laplacian_grid_c(self,G,H,I,pt)
   ! Return "G(j,n,1:3)", the value of the second derivatives
   ! (d/dx2,d/dy2,d/dz2) of the shell component n on grid point j
   ! given a set of grid points "pt(j,1:3)". Also return "H(j,n,1:3)",
   ! the value of the first derivative, and return "I(n)", the value
   ! of the gaussian
      type(SHELL1_TYPE), IN :: self
      MAT3(REAL,:,:,:), OUT :: G,H
      MAT(REAL,:,:), OUT :: I
      MAT(REAL,:,:), IN :: pt
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,p,j,nxb,nyb,nzb,ff,lf
      REAL :: aa,x,y,z,x0,y0,z0,rr,val
      REAL :: g0,g1,g2,g2x,g2y,g2z
      REAL :: gnbx,gnby,gnbz,x2,y2,z2
      REAL :: gx0,gx1,gy0,gy1,gz0,gz1,inb
      REAL :: hnbx,hnby,hnbz,hx1,hy1,hz1,h1x,h1y,h1z
      REAL :: tn,tn1,tn2,twog0
   ENSURE(size(pt,2)==3,"SHELL1:make_laplacian_grid_c ... pt matrix incorrectly dimensioned")
      n_pt = size(pt,1)

      ! Origin
      x0 = self%position(1)
      y0 = self%position(2)
      z0 = self%position(3)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            x = pt(n,1)-x0
            y = pt(n,2)-y0
            z = pt(n,3)-z0
            x2 = x*x; y2 = y*y; z2 = z*z
            rr = x2+y2+z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc  ! Loop over primitives
               aa = self%exponent(p)
               val = self%contraction(p)*exp(-aa*rr)
               g0 = g0+val
               g1 = g1-aa*val
               g2 = g2+aa*aa*val
            end do
            g1 = g1*TWO
            g2 = g2*FOUR
            G(n,1,1) = g1+g2*x2
            G(n,1,2) = g1+g2*y2
            G(n,1,3) = g1+g2*z2
            H(n,1,1) = g1*x
            H(n,1,2) = g1*y
            H(n,1,3) = g1*z
            I(n,1) = g0
         end do

      case (1)
         do n = 1,n_pt
            x = pt(n,1)-x0
            y = pt(n,2)-y0
            z = pt(n,3)-z0
            x2 = x*x; y2 = y*y; z2 = z*z
            rr = x2+y2+z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc  ! Loop over primitives
               aa = self%exponent(p)
               val = self%contraction(p)*exp(-aa*rr)
               g0 = g0+val
               g1 = g1-aa*val
               g2 = g2+aa*aa*val
            end do
            g1 = g1*TWO
            g2 = g2*FOUR
            g2x = g2*x2
            g2y = g2*y2
            g2z = g2*z2
            G(n,1,1) = x*(3*g1+g2x)
            G(n,1,2) = x*(g1+g2y)
            G(n,1,3) = x*(g1+g2z)
            G(n,2,1) = y*(g1+g2x)
            G(n,2,2) = y*(3*g1+g2y)
            G(n,2,3) = y*(g1+g2z)
            G(n,3,1) = z*(g1+g2x)
            G(n,3,2) = z*(g1+g2y)
            G(n,3,3) = z*(3*g1+g2z)
            h1x = g1*x
            h1y = g1*y
            h1z = g1*z
            H(n,1,1) = g0+x2*g1
            H(n,1,2) = x*h1y
            H(n,1,3) = x*h1z
            H(n,2,1) = y*h1x
            H(n,2,2) = g0+y2*g1
            H(n,2,3) = y*h1z
            H(n,3,1) = z*h1x
            H(n,3,2) = z*h1y
            H(n,3,3) = g0+z2*g1
            I(n,1) = g0*x
            I(n,2) = g0*y
            I(n,3) = g0*z
         end do

      case default
         ff = self%first_gaussian
         lf = self%last_gaussian
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         do n = 1,n_pt
            x = pt(n,1)-x0
            y = pt(n,2)-y0
            z = pt(n,3)-z0
            x2 = x*x; y2 = y*y; z2 = z*z
            rr = x2+y2+z2
            g0 = ZERO
            g1 = ZERO
            g2 = ZERO
            do p = 1,self%n_cc  ! Loop over primitives
               aa = self%exponent(p)
               val = self%contraction(p)*exp(-aa*rr)
               g0 = g0+val
               g1 = g1-aa*val
               g2 = g2+aa*aa*val
            end do
            g1 = g1*TWO
            g2 = g2*FOUR
            g2x = g2*x2
            g2y = g2*y2
            g2z = g2*z2

            ! Some that are used multiple times.
            ! Taken out of loop over n_comp.
            gx0 = g1+g2x
            gy0 = g1+g2y
            gz0 = g1+g2z
            gx1 = (THREE*g1+g2x)*x  ! *x^1
            gy1 = (THREE*g1+g2y)*y  ! *y^1
            gz1 = (THREE*g1+g2z)*z  ! *z^1
            h1x = g1*x
            h1y = g1*y
            h1z = g1*z
            hx1 = g0+x2*g1
            hy1 = g0+y2*g1
            hz1 = g0+z2*g1

            ! Loop over basis functions
            do b = 1,self%n_comp
               inb = g0
               nxb = lx(b)
               nyb = ly(b)
               nzb = lz(b)
               select case (nxb)
               case (0); gnbx = gx0
                  hnbx = h1x
               case (1); gnbx = gx1
                  hnbx = hx1
               case (2)
                  tn = x*x
                  twog0 = g0+g0
                  gnbx = twog0+(5*g1+g2x)*tn
                  hnbx = twog0*x+h1x*tn
               case default
                  tn2 = x**(nxb-2)
                  tn1 = tn2*x
                  tn = tn2*x*x
                  gnbx = nxb*(nxb-1)*g0*tn2+((2*nxb+1)*g1+g2x)*tn
                  hnbx = nxb*g0*tn1+h1x*tn
               end select
               select case (nyb)
               case (0); gnby = gy0
                  hnby = h1y
               case (1); gnby = gy1
                  hnby = hy1
               case (2)
                  tn = y*y
                  twog0 = g0+g0
                  gnby = twog0+(5*g1+g2y)*tn
                  hnby = twog0*y+h1y*tn
               case default
                  tn2 = y**(nyb-2)
                  tn1 = tn2*y
                  tn = tn2*y*y
                  gnby = nyb*(nyb-1)*g0*tn2+((2*nyb+1)*g1+g2y)*tn
                  hnby = nyb*g0*tn1+h1y*tn
               end select
               select case (nzb)
               case (0); gnbz = gz0
                  hnbz = h1z
               case (1); gnbz = gz1
                  hnbz = hz1
               case (2)
                  tn = z*z
                  twog0 = g0+g0
                  gnbz = twog0+(5*g1+g2z)*tn
                  hnbz = twog0*z+h1z*tn
               case default
                  tn2 = z**(nzb-2)
                  tn1 = tn2*z
                  tn = tn2*z*z
                  gnbz = nzb*(nzb-1)*g0*tn2+((2*nzb+1)*g1+g2z)*tn
                  hnbz = nzb*g0*tn1+h1z*tn
               end select
               do j = 1,nxb
                  gnby = x*gnby  ! Do the cartesian (x_i)^j, i/=k part of
                  gnbz = x*gnbz  ! gaussian power of the xyz part not equal
                  hnby = x*hnby
                  hnbz = x*hnbz
                  inb = x*inb
               end do  ! to deriv. component k
               do j = 1,nyb
                  gnbx = y*gnbx
                  gnbz = y*gnbz
                  hnbx = y*hnbx
                  hnbz = y*hnbz
                  inb = y*inb
               end do
               do j = 1,nzb
                  gnbx = z*gnbx
                  gnby = z*gnby
                  hnbx = z*hnbx
                  hnby = z*hnby
                  inb = z*inb
               end do
               G(n,b,1) = gnbx
               G(n,b,2) = gnby
               G(n,b,3) = gnbz
               H(n,b,1) = hnbx
               H(n,b,2) = hnby
               H(n,b,3) = hnbz
               I(n,b) = inb
            end do
         end do
      end select
   end subroutine

   subroutine make_oscillator_orbital_mask(self,f,c,l,n,pt)
   ! Return "f(i)", the value of the cartesian part of an oscillator function
   ! centered at "c", of angular momentum "l" and component "n", evaluated on
   ! grid point "i", which is "pt(i,1:3)". 
      type(SHELL1_TYPE), IN :: self
      VEC(REAL,:) :: f
      MAT(REAL,:,:) :: pt
      VEC(REAL,3) :: c
      INT :: l,n
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: nxn,nyn,nzn,ff,lf
   ENSURE(is_in_range_(n,[1,n_comp_(l)]),"SHELL1:make_oscillator_orbital_mask ... inconsistent l, n value")
      select case (l)

      case (0)
         f = ONE

      case (1)
         f = pt(:,n)-c(n)

   ! case (2)
   !    if      (n<=3) then; f = pt(:,n)-c(n)*c(n)
   !    else if (n==4) then; f = pt(:,n)-c(1)*c(2)
   !    else if (n==5) then; f = pt(:,n)-c(1)*c(3)
   !    else if (n==6) then; f = pt(:,n)-c(2)*c(3)
   !    end

   ! case (3)
   !    if      (n<=3)  then; f = pt(:,n)-c(n)*c(n)*c(n)
   !    else if (n==4)  then; f = pt(:,n)-c(1)*c(1)*c(2)
   !    else if (n==5)  then; f = pt(:,n)-c(1)*c(1)*c(3)
   !    else if (n==6)  then; f = pt(:,n)-c(2)*c(2)*c(1)
   !    else if (n==7)  then; f = pt(:,n)-c(2)*c(2)*c(3)
   !    else if (n==8)  then; f = pt(:,n)-c(3)*c(3)*c(1)
   !    else if (n==9)  then; f = pt(:,n)-c(3)*c(3)*c(2)
   !    else if (n==10) then; f = pt(:,n)-c(1)*c(2)*c(3)
   !    end

      case default
         call set_indices_(self%l)  ! Make sure indices are there
         ff = n_comp_up_to_(l-1)+1
         lf = n_comp_up_to_(l)
         lx = px(ff:lf)
         ly = py(ff:lf)
         lz = pz(ff:lf)
         nxn = lx(n); nyn = ly(n); nzn = lz(n)
         f = (pt(:,1)-c(1))**nxn*(pt(:,2)-c(2))**nyn*(pt(:,3)-c(3))**nzn
      end select
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(SHELL1_TYPE), IN :: self
      call dump_(stdout,"l",self%l)
      call dump_(stdout,"l_chr",self%l_chr)
      call dump_(stdout,"is_spherical",self%is_spherical)
      call dump_(stdout,"n_comp",self%n_comp)
      call dump_(stdout,"n_sph",self%n_sph)
      call dump_(stdout,"n_bf",self%n_bf)
      call dump_(stdout,"first_gaussian",self%first_gaussian)
      call dump_(stdout,"last_gaussian",self%last_gaussian)
      call dump_(stdout,"n_cc",self%n_cc)
      call dump_(stdout,"position",self%position)
      call dmpa_(stdout,"exponent",self%exponent)
      call dmpa_(stdout,"contraction",self%contraction)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(SHELL1_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":SHELL1@ = { ")
      else; call text_(stdout,trim(object_name)//":SHELL1 = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(SHELL1_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":SHELL1@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put the shell information to "stdout" 
      type(SHELL1_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
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
      call show_(stdout,"Position                    =",self%position(1),self%position(2),self%position(3))
      call flush_(stdout)
      if (NOT allocated(self%exponent)) return
      if (NOT allocated(self%contraction)) return

      ! Table headings
      call create_(table,2)
      call set_heading_(table(2),"Contraction")
      call set_subhead_(table(2),"coeff.")
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
