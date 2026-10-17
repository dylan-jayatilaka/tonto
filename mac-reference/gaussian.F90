!==================================================================
! For describing a single gaussian function :: GAUSSIAN.
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
module GAUSSIAN_MODULE

#  include "macros"

   include "gaussian.use"

   implicit none

   include "gaussian.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(GAUSSIAN_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(GAUSSIAN_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set(self,l,pos,ex)
   ! Set the gaussian to "G"
      type(GAUSSIAN_TYPE), INOUT :: self
      INT, IN :: l
      VEC(REAL,3), IN :: pos
      REAL, IN :: ex
      call set_l_(self,l)
      call set_position_(self,pos)
      call set_exponent_(self,ex)
   end subroutine

   pure subroutine set_l(self,l)
   ! Set the l value
      type(GAUSSIAN_TYPE), INOUT :: self
      INT, IN :: l
      self%l = l
      self%n_sph = n_sph_(self%l)
      self%n_comp = n_comp_(self%l)
      self%first_gaussian = n_comp_up_to_(l-1)+1
      self%last_gaussian = self%first_gaussian+self%n_comp-1
   end subroutine

   pure subroutine set_position(self,pos)
   ! Set the position
      type(GAUSSIAN_TYPE), INOUT :: self
      VEC(REAL,3), IN :: pos
      self%position = pos
   end subroutine

   pure subroutine set_exponent(self,ex)
   ! Set a gaussian object
      type(GAUSSIAN_TYPE), INOUT :: self
      REAL, IN :: ex
      self%exponent = ex
   end subroutine

   PURE function l_chr(self) result (res)
   ! Return a character representation for the angular mtm
      type(GAUSSIAN_TYPE), IN :: self
      STR(len=1) :: res
      INT :: l
      l = self%l
      select case (l)
      case (0); res = "s"
      case (1); res = "p"
      case (2); res = "d"
      case (3); res = "f"
      case (4); res = "g"
      case default
         ENSURE(l<=23,"GAUSSIAN:l_chr ... angular momentum too large:"//trim(to_str_(l)))
         res = achar(l-4+iachar("g"))
      end select
   end function


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put the gaussian information to file "stdout"
      type(GAUSSIAN_TYPE), IN :: self
      call show_(stdout,"l        =",self%l)
      call show_(stdout,"l symbol =",l_chr_(self))
      call show_(stdout,"pos      =",self%position)
      call show_(stdout,"exponent =",self%exponent)
      call show_(stdout,"n_sph    =",self%n_sph)
      call show_(stdout,"n_comp   =",self%n_comp)
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(GAUSSIAN_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","GAUSSIAN:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"GAUSSIAN:read_keywords ... unexpected end of file while reading list keywords")
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
      type(GAUSSIAN_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}        ")  ! exit case
      case ("exponent="); call read_exponent_(self)
      case ("l=       "); call read_l_(self)
      case ("l_chr=   "); call read_l_chr_(self)
      case ("l_int=   "); call read_l_int_(self)
      case ("position="); call read_position_(self)
      case ("put      "); call put_(self)
      case default; allocate(tonto%known_keywords(7))
      tonto%known_keywords(1) = "}        "
      tonto%known_keywords(2) = "exponent="
      tonto%known_keywords(3) = "l=       "
      tonto%known_keywords(4) = "l_chr=   "
      tonto%known_keywords(5) = "l_int=   "
      tonto%known_keywords(6) = "position="
      tonto%known_keywords(7) = "put      "
      call unknown_(tonto,word,"GAUSSIAN:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_l(self)
   ! Read in the l symbol
      type(GAUSSIAN_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      call move_to_previous_item_(stdin)
      if (is_int_(word)) then; call read_l_int_(self)
      else; call read_l_chr_(self)
      end if
   end subroutine

   subroutine read_l_int(self)
   ! Read in the l integer
      type(GAUSSIAN_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(stdin,word)
      ENSURE(is_int_(word),"GAUSSIAN:read_l_int ... expecting an integer for L")
      call set_l_(self,to_int_(word))
   end subroutine

   subroutine read_l_chr(self)
   ! Read in the l symbol
      type(GAUSSIAN_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      STR(len=1) :: l_c
      INT :: l
      call read_(stdin,word)
      DIE_IF(len_trim(word)/=1,"GAUSSIAN:read_l_chr ... unknown L symbol")
      l_c = word(1:1)
      call to_lower_case_(l_c)
      l = 0
      select case (l_c)
      case ("s"); l = 0
      case ("p"); l = 1
      case ("d"); l = 2
      case ("f"); l = 3
      case ("g"); l = 4
      case default
         DIE_IF(l_c<"g","GAUSSIAN:read_l_chr ... unknown angular momentum character: "//l_c)
         l = 4+iachar(l_c)-iachar("g")
      end select
      call set_l_(self,l)
   end subroutine

   subroutine read_exponent(self)
   ! Read in the exponents
      type(GAUSSIAN_TYPE), INOUT :: self
      call read_(stdin,self%exponent)
   end subroutine

   subroutine read_position(self)
   ! Read in the position
      type(GAUSSIAN_TYPE), INOUT :: self
      call read_quantity_(stdin,self%position)
   end subroutine


! =====================
! Miscellaneous methods
! =====================

   pure function normalisation_factor(self) result (res)
   ! Return the normaliston factor assuming it is x^l
      type(GAUSSIAN_TYPE), IN :: self
      REAL :: res
      res = PI**(-HALF-QUARTER)*TWO**(real(self%l,kind=REAL_KIND)+HALF+QUARTER)*self%exponent**(HALF*self%l+HALF+QUARTER)*(ONE/(sqrt(double_factorial_(self%l))))
   end function

   pure function same_as(self,G) result (same)
   ! Return TRUE if the gaussian "self" is the same as "G".
      type(GAUSSIAN_TYPE), IN :: self
      type(GAUSSIAN_TYPE), IN :: G
      BIN :: same
      same = self%l==G%l AND self%exponent==G%exponent AND self%position(1)==G%position(1) AND self%position(2)==G%position(2) AND self%position(3)==G%position(3)
   end function

   PURE subroutine make_grid_0(self,g,pt)
   ! Return "g(i,n)", the value of the gaussian component "n" on grid point "i"
   ! given a set of grid points "pt(i,1:3)"
      type(GAUSSIAN_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: g
      MAT(REAL,:,:), IN :: pt
   ENSURE(size(g,2)==self%n_comp,"GAUSSIAN:make_grid_0 ... incorrectly dimensioned")
      call make_grid_(self,g,pt(:,1),pt(:,2),pt(:,3))
   end subroutine

   PURE subroutine make_grid_1(self,g,x,y,z)
   ! Return "g(i,n)", the value of the gaussian component "n" on grid point "i"
   ! given a set of grid points "(x(i),y(i),z(i))"
      type(GAUSSIAN_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: g
      VEC(REAL,:), IN :: x,y,z
      VEC(INT,:), allocatable :: lx,ly,lz
      INT :: n_pt,n,b,j,f,l
      REAL :: rr,xx,yy,zz,posx,posy,posz,g0,gbn
   ENSURE(size(g,2)==self%n_comp,"GAUSSIAN:make_grid_1 ... incorrectly dimensioned")
      n_pt = size(x)
      posx = self%position(1)
      posy = self%position(2)
      posz = self%position(3)
      select case (self%l)

      case (0)
         do n = 1,n_pt
            xx = x(n)-posx; yy = y(n)-posy; zz = z(n)-posz
            rr = xx*xx+yy*yy+zz*zz
            g(n,1) = exp(-rr*self%exponent)
         end do

      case (1)
         do n = 1,n_pt
            xx = x(n)-posx; yy = y(n)-posy; zz = z(n)-posz
            rr = xx*xx+yy*yy+zz*zz
            g0 = exp(-rr*self%exponent)
            g(n,1) = xx*g0
            g(n,2) = yy*g0
            g(n,3) = zz*g0
         end do

      case (2)
         do n = 1,n_pt
            xx = x(n)-posx; yy = y(n)-posy; zz = z(n)-posz
            rr = xx*xx+yy*yy+zz*zz
            g0 = exp(-rr*self%exponent)
            g(n,1) = xx*xx*g0
            g(n,2) = yy*yy*g0
            g(n,3) = zz*zz*g0
            g(n,4) = xx*yy*g0
            g(n,5) = xx*zz*g0
            g(n,6) = yy*zz*g0
         end do

      case default
         f = self%first_gaussian; l = self%last_gaussian
         lx = px(f:l)
         ly = py(f:l)
         lz = pz(f:l)
         do n = 1,n_pt
            xx = x(n)-posx; yy = y(n)-posy; zz = z(n)-posz
            rr = xx*xx+yy*yy+zz*zz  ! Do exponential part of the gaussian
            g0 = exp(-rr*self%exponent)
            do b = 1,self%n_comp  ! Loop over all basis functions
               gbn = g0  ! Combine exponential and cartesian
               do j = 1,lx(b)  ! power of the x^j part
                  gbn = xx*gbn
               end do
               do j = 1,ly(b)
                  gbn = yy*gbn
               end do
               do j = 1,lz(b)
                  gbn = zz*gbn
               end do
               g(n,b) = gbn
            end do
         end do
      end select
   end subroutine

end module
