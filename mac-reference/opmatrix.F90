!===================================================================
!
! OPMATRIX: Operator matrices.
!
! Provide a basis set (matrix) representation of a one-electron
! quantum mechanical operator. Can cope with polymorphic types of
! basis representations, including restricted, unrestricted, and
! general basis orbital matrices.  Also, complex types for all these
! matrices.
!
! Basically, its a polymorphic matrix type.
!
! Copyright (C) Dylan Jayatilaka 1998
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
module OPMATRIX_MODULE

#  include "macros"

   include "opmatrix.use"

   implicit none

   include "opmatrix.int"

   VEC(STR(len=2),3) :: allowed_genres
   data allowed_genres/"r ","u ","gc"/

contains

! ==========
! Allocation
! ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(OPMATRIX_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"OPMATRIX:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,n_bf)
   ! Create an opmatrix object, in a basis set with "n_bf" spatial orbitals
      type(OPMATRIX_TYPE), allocatable, OUT :: self
      INT, IN :: n_bf
      call create_(self)
      self%n_bf = n_bf
      self%n_mo = n_bf
   end subroutine

   PURE subroutine create_2(self,genre)
   ! Create the "genre" of the opmatrix
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), IN :: genre
   ENSURE(self%n_bf>0,"OPMATRIX:create_2 ... bad number of basis functions.")
   ENSURE(is_one_of_(genre,allowed_genres),"OPMATRIX:create_2 ... unknown genre, "//trim(genre))
      select case (genre)
      case ("r "); call create_(self%r,self%n_bf,self%n_bf)
      case ("u "); call create_(self%a,self%n_bf,self%n_bf)
         call create_(self%b,self%n_bf,self%n_bf)
      case ("gc"); call create_(self%gc,2*self%n_bf,2*self%n_bf)
      end select
   end subroutine

   PURE subroutine create_3(self,n_bf,genre)
   ! Create an opmatrix object, set with "n_bf" spatial orbitals
   ! and with "genre".
      type(OPMATRIX_TYPE), allocatable, OUT :: self
      INT, IN :: n_bf
      STR(len=*), IN :: genre
      call create_(self,n_bf)
      call create_(self,genre)
   end subroutine

   PURE subroutine destroy_0(self)
   ! Deallocate "self"
      type(OPMATRIX_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"OPMATRIX:destroy_0 ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_1(self,genre)
   ! Destroy the appropriate spinorbital kind of the opmatrix
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), IN :: genre
   ENSURE(is_one_of_(genre,allowed_genres),"OPMATRIX:destroy_1 ... unknown genre, "//trim(genre))
      select case (genre)
      case ("r "); call destroy_(self%r)
      case ("u "); call destroy_(self%a); call destroy_(self%b)
      case ("gc"); call destroy_(self%gc)
      end select
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of the opmatrix object
      type(OPMATRIX_TYPE), INOUT :: self
      call destroy_(self%r)
      call destroy_(self%a)
      call destroy_(self%b)
      call destroy_(self%gc)
      call destroy_(self%tri)
      call destroy_(self%sqr)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_defaults(self)
   ! Set defaults
      type(OPMATRIX_TYPE), INOUT :: self
      self%n_bf = 0
      self%n_mo = 0
   end subroutine

   pure subroutine set(self,n_bf)
   ! Set with "n_bf" spatial orbitals
      type(OPMATRIX_TYPE), INOUT :: self
      INT, IN :: n_bf
      self%n_bf = n_bf
   end subroutine

   pure subroutine set_to(self,mx)
   ! Set "self" to "mx".
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: mx
      self = mx
   end subroutine

   pure subroutine set_to_zero(self)
   ! Set self to zero
      type(OPMATRIX_TYPE), INOUT :: self
      if (allocated(self%r)) self%r = ZERO
      if (allocated(self%a)) self%a = ZERO
      if (allocated(self%b)) self%b = ZERO
      if (allocated(self%gc)) self%gc = (ZERO,ZERO)
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   pure function genre(self) result (res)
   ! Return genre as a string length 2.
      type(OPMATRIX_TYPE), IN :: self
      STR(len=2) :: res
      res = " "
      if (allocated(self%r)) then; res = "r "
      else if (allocated(self%a) AND allocated(self%b)) then; res = "u "
      else if (allocated(self%gc)) then; res = "gc"
      end if
   end function

   pure function has_any_genre(self) result (res)
   ! Returns true if any component of self has been created
   ! NOTE: the beta parts are NOT tested for creation.
      type(OPMATRIX_TYPE), IN :: self
      BIN :: res
      VEC(BIN,3) :: c
      c(1) = allocated(self%r)
      c(2) = allocated(self%a) AND allocated(self%b)
      c(3) = allocated(self%gc)
      res = any(c)
   end function

   pure function has_real_genre(self) result (res)
   ! Returns true if any component of self has been created
   ! NOTE: the beta parts are NOT tested for creation.
      type(OPMATRIX_TYPE), IN :: self
      BIN :: res
      res = allocated(self%r) OR (allocated(self%a) AND allocated(self%b))
   end function

   PURE function is_allocated_with_genre(self,genre) result (res)
   ! Returns FALSE if self is not allocated
   ! Returns FALSE, unless subkind "genre" of self has been created.
      type(OPMATRIX_TYPE), allocatable, IN :: self
      STR(len=*), IN :: genre
      BIN :: res
      if (NOT allocated(self)) then; res = FALSE
      else; res = has_genre_(self,genre)
      end if
   end function

   pure function is_deallocated_or_no_genre(self) result (res)
   ! Returns true if self is deallocated OR has all components of
   ! self have been deallocated.
      type(OPMATRIX_TYPE), allocatable, IN :: self
      BIN :: res
      VEC(BIN,3) :: d
      if (NOT allocated(self)) then
         res = TRUE
      else
         d(1) = NOT allocated(self%r)
         d(2) = NOT allocated(self%a) AND NOT allocated(self%b)
         d(3) = NOT allocated(self%gc)
         res = all(d)
      end if
   end function

   PURE function has_genre(self,genre) result (res)
   ! Returns FALSE, unless subkind "genre" of self has been created.
      type(OPMATRIX_TYPE), IN :: self
      STR(len=*), IN :: genre
      BIN :: res
   ENSURE(is_one_of_(genre,allowed_genres),"OPMATRIX:has_genre ... unknown genre, "//trim(genre))
      res = FALSE
      select case (genre)
      case ("r "); res = allocated(self%r)
      case ("u "); res = allocated(self%a) AND allocated(self%b)
      case ("gc"); res = allocated(self%gc)
      end select
   end function

   pure function is_compatible_with(self,X) result (res)
   ! Returns TRUE if the "self" and "X" are compatible
      type(OPMATRIX_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: X
      BIN :: res
      VEC(BIN,8) :: cs,cx
      cs(1) = allocated(self%r)
      cs(2) = allocated(self%a) AND allocated(self%b)
      cs(3) = allocated(self%gc)
      cx(1) = allocated(X%r)
      cx(2) = allocated(X%a) AND allocated(X%b)
      cx(3) = allocated(X%gc)
      res = all(cs EQV cx)
   end function


!   is_compatible_with(X,genre) result (res) :: PURE
!   ! Returns TRUE if the "self" and "X" are compatible
!      self :: IN
!      X :: OPMATRIX, IN
!      genre :: STR, IN
!      res :: BIN
!      res = .has_genre(genre) AND X.has_genre(genre)
!   end

   pure function number_kind(self) result (res)
   ! Return the kind of numbers used in the representation
      type(OPMATRIX_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      select case (genre_(self))
      case ("r "); res = "real"
      case ("u "); res = "real"
      case ("gc"); res = "complex"
      end select
   end function


!   same_spinorbital_kind_as(X) result (res) :: pure
!   ! Return TRUE if "self" has the genre as "self" 
!   ! NOTE: Use as a last resort. Use the SCF_data.SCF_kind if possible.
!      self :: IN
!      X :: OPMATRIX, IN
!      res :: BIN
!      res = .genre==X.genre
!   end

!   same_spinorbital_kind_as(X,genre) result (res) :: PURE
!   ! Return TRUE if "X" has the same "genre" as "self" 
!   ! NOTE: Use as a last resort. Use SCF_data.SCF_kind if possible.
!      self :: IN
!      X :: OPMATRIX, IN
!      genre :: STR, IN
!      res :: BIN
!
!      res = .has_genre(genre) AND X.has_genre(genre)
!
!   end

!   spinorbital_kind(genre) result (res) :: PURE
!   ! Return the kind of spinorbitals used in the representation
!   ! NOTE: The *simplest* spinorbital kind in use is the one returned.
!   ! NOTE: Use as a last resort. Use SCF_data.SCF_kind if possible.
!      self :: IN
!      genre :: STR, IN, optional
!      res :: STR
!
!   ENSURE(.has_any_genre OR present(genre),"no component created")
!
!      if (present(genre)) then
!
!         ENSURE(genre.is_one_of(allowed_kinds), "unknown genre, "//trim(genre))
!         res = genre
!
!      else
!
!         res = " "
!              if (.r.allocated)         then; res = "r "
!         else if (.a.allocated)         then; res = "u "
!         else if (.gc.allocated) then; res = "gc"
!         end
!
!      end
!
!   end

   pure function occ_nos_genre(self) result (res)
   ! Return the kind of OPVECTOR used for the occupation numbers.
      type(OPMATRIX_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = " "
      if (allocated(self%r)) then; res = "r "
      else if (allocated(self%a) AND allocated(self%b)) then; res = "u "
      else if (allocated(self%gc)) then; res = "gc"
      end if
   end function

   pure function guess_SCF_kind(self) result (res)
   ! Guess the kind of SCF to be used from the spinorbitals used in the
   ! representation. WARNING: use as a last resort.
      type(OPMATRIX_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      res = " "
      select case (genre_(self))
      case ("r "); res = "rhf"
      case ("u "); res = "uhf"
      case ("gc"); res = "gchf"
      end select
   end function


!  ==========
!  Conversion
!  ==========

   subroutine convert_to_0(self,new,factor,destroy_original)
   ! Convert self to a "new" genre. To convert MO's please use the
   ! next routine.
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), IN :: new
      REAL, optional, IN :: factor
      BIN, optional, IN :: destroy_original
      STR(len=STR_SIZE) :: old
      BIN :: destroy_old,fail
      destroy_old = TRUE
      if (present(destroy_original)) destroy_old = destroy_original

      ! Done?
      old = genre_(self)
      if (new==old) return
      call create_(self,new)

      ! Convert old -> ne
      fail = FALSE
      select case (old)

      case ("r ")
         select case (new)
         case ("u ")
            if (present(factor)) then
               self%a = self%r*factor
               self%b = self%r*factor
            else
               self%a = self%r
               self%b = self%r
            end if
         case ("gc")
            self%gc = ZERO
            if (present(factor)) then
               call set_aa_block_to_(self%gc,self%r,factor)
               call set_bb_block_to_(self%gc,self%r,factor)
            else
               call set_aa_block_to_(self%gc,self%r)
               call set_bb_block_to_(self%gc,self%r)
            end if
         case default
            fail = TRUE
         end select

      case ("u ")
         select case (new)
         case ("r ")
            if (present(factor)) then
               self%r = self%a*factor
            else
               self%r = self%a
            end if
         case ("gc")
            self%gc = ZERO
            call set_aa_block_to_(self%gc,self%a)
            call set_bb_block_to_(self%gc,self%b)
         case default
            fail = TRUE
         end select

      case default
         fail = TRUE
      end select

      ! Clean
      if (destroy_old) call destroy_(self,old)
      if (fail) then
         DIE("OPMATRIX:convert_to_0 ... can't convert genre "//trim(old)//" to genre "//trim(new))
      end if
   end subroutine

   subroutine convert_to_1(self,new,na,nb,quantization_axis)
   ! Convert self (a set of MO's) to a new genre "new"
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), IN :: new
      INT, IN :: na,nb
      VEC(REAL,3), optional, IN :: quantization_axis
      STR(len=STR_SIZE) :: old
      INT :: d,ne
      BIN :: quantize,fail
      REAL :: lam,tmp,ar,br,bi
      d = self%n_bf
      ne = na+nb

      ! Done?
      old = genre_(self)
      if (new==old) return

      ! Create the new (leaky)
      call create_(self,new)
      fail = FALSE
      select case (old)

      case ("r ")
         select case (new)
         case ("u ")
            self%a = self%r
            self%b = self%r
         case ("gc")
            self%gc = ZERO
            self%gc(1:d,1:2*nb-1:2) = self%r(:,1:nb)
            self%gc(d+1:,2:2*nb:2) = self%r(:,1:nb)
            self%gc(1:d,2*nb+1:ne) = self%r(:,nb+1:na)
            self%gc(d+1:,ne+1:2*na) = self%r(:,nb+1:na)
            self%gc(1:d,2*na+1:2*d-1:2) = self%r(:,na+1:d)
            self%gc(d+1:,2*na+2:2*d:2) = self%r(:,na+1:d)
         case default
            fail = TRUE
         end select

      case ("u ")
         select case (new)
         case ("r ")
            self%r = self%a  ! Loss of information here
         case ("gc")
            self%gc = ZERO
            quantize = FALSE
            if (present(quantization_axis)) then
               quantize = NOT equals_(quantization_axis,[ZERO,ZERO,ONE])
            end if
            if (quantize) then
               lam = norm_(quantization_axis)
               tmp = abs(quantization_axis(3)+lam)  ! always positive anyway
               if (equals_(tmp,ZERO)) then
                  ar = ZERO
                  br = ONE
                  bi = ZERO
               else
                  ar = sqrt(tmp/(TWO*lam))
                  br = ONE/sqrt(TWO*lam*tmp)
                  bi = br
                  br = br*quantization_axis(1)
                  bi = bi*quantization_axis(2)
               end if
               self%gc(d+1:,1:na) = cmplx(br,bi,kind=CPX_KIND)*self%a(:,1:na)
               self%gc(1:d,1:na) = ar*self%a(:,1:na)
               self%gc(d+1:,ne+1:nb+d) = cmplx(br,bi,kind=CPX_KIND)*self%a(:,na+1:d)
               self%gc(1:d,ne+1:nb+d) = ar*self%a(:,na+1:d)
               br = -br
               self%gc(1:d,na+1:ne) = cmplx(br,bi,kind=CPX_KIND)*self%b(:,1:nb)
               self%gc(d+1:,na+1:ne) = ar*self%b(:,1:nb)
               self%gc(1:d,nb+d+1:) = cmplx(br,bi,kind=CPX_KIND)*self%b(:,nb+1:d)
               self%gc(d+1:,nb+d+1:) = ar*self%b(:,nb+1:d)
            else
               self%gc(1:d,1:na) = self%a(:,1:na)
               self%gc(d+1:,na+1:ne) = self%b(:,1:nb)
               self%gc(1:d,ne+1:nb+d) = self%a(:,na+1:)
               self%gc(d+1:,nb+d+1:) = self%b(:,nb+1:)
            end if

         case default
            fail = TRUE
         end select

      case default
         fail = TRUE
      end select

      ! Clean?
      call destroy_(self,old)
      if (fail) then
         DIE("OPMATRIX:convert_to_1 ... can't convert genre "//trim(old)//" to genre "//trim(new))
      end if
   end subroutine


!  ===========
!  Compression
!  ===========

   PURE function l_compress(self,genre) result (res)
   ! Return the length of the compressed object
      type(OPMATRIX_TYPE), IN :: self
      STR(len=*), optional, IN :: genre
      INT :: res
      STR(len=STR_SIZE) :: k
      if (present(genre)) then; k = genre
      else; k = genre_(self)
      end if
      res = 0
      select case (k)
      case ("r "); res = int(HALF*self%n_bf*(self%n_bf+1))
      case ("u "); res = self%n_bf*(self%n_bf+1)
      case ("gc"); res = 4*self%n_bf*self%n_bf
      end select
   end function

   PURE subroutine compress(self,genre)
   ! Compress the opmatrix into a triangle or square vector 
   ! to save space
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: k
      INT :: l
      VEC(REAL,:), allocatable :: W
      k = genre_(self)
      if (present(genre)) k = genre
      select case (k)

      case ("r ")
         call destroy_(self%tri)
         call create_(self%tri,l_compress_(self,k))
         call zip_lower_triangle_to_(self%r,self%tri)
         call destroy_(self%r)

      case ("u ")
         l = l_compress_(self,k)
         call destroy_(self%tri)
         call create_(self%tri,l)
         call create_(W,l/2)
         call zip_lower_triangle_to_(self%a,W); call set_a_block_to_(self%tri,W)
         call zip_lower_triangle_to_(self%b,W); call set_b_block_to_(self%tri,W)
         call destroy_(W)
         call destroy_(self%a)
         call destroy_(self%b)

      case ("gc")
         call destroy_(self%sqr)
         call create_(self%sqr,l_compress_(self,k))
         call zip_lower_triangles_to_(self%gc,self%sqr)
         call destroy_(self%gc)
      end select
   end subroutine

   PURE subroutine uncompress(self)
   ! Uncompress the opmatrix
      type(OPMATRIX_TYPE), INOUT :: self
      INT :: l
      l = -1
      if (allocated(self%tri)) l = size(self%tri)
      if (allocated(self%sqr)) l = size(self%sqr)
      if (l==l_compress_(self,"r ")) then
         call destroy_(self,"r ")
         call create_(self,"r ")
         call symmetric_unzip_triangle_(self%r,self%tri)
         call destroy_(self%tri)
      else if (l==l_compress_(self,"u ")) then
         call destroy_(self,"u ")
         call create_(self,"u ")
         call symmetric_unzip_triangle_(self%a,a_block_(self%tri))
         call symmetric_unzip_triangle_(self%b,b_block_(self%tri))
         call destroy_(self%tri)
      else if (l==l_compress_(self,"gc")) then
         call destroy_(self,"gc")
         call create_(self,"gc")
         call hermitian_unzip_triangles_(self%gc,self%sqr)
         call destroy_(self%sqr)
      end if
   end subroutine


!  ===============
!  Math operations
!  ===============

   PURE subroutine plus(self,X,genre)
   ! Add "X" to "self", only the right genre
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: X
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: k
      k = genre_(self)
      if (present(genre)) k = genre
      select case (k)
      case ("r "); self%r = self%r+X%r
      case ("u "); self%a = self%a+X%a
         self%b = self%b+X%b
      case ("gc"); self%gc = self%gc+X%gc
      end select
   end subroutine

   PURE subroutine minus(self,X,genre)
   ! Subtract "X" from "self", only the right genre
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: X
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: k
      k = genre_(self)
      if (present(genre)) k = genre
      select case (k)
      case ("r "); self%r = self%r-X%r
      case ("u "); self%a = self%a-X%a
         self%b = self%b-X%b
      case ("gc"); self%gc = self%gc-X%gc
      end select
   end subroutine

   PURE subroutine scale_by(self,fac)
   ! Set "self" to a scaled version of itself.
      type(OPMATRIX_TYPE), INOUT :: self
      REAL, IN :: fac
      select case (genre_(self))
      case ("r "); self%r = fac*self%r
      case ("u "); self%a = fac*self%a
         self%b = fac*self%b
      case ("gc"); self%gc = fac*self%gc
      end select
   end subroutine

   PURE subroutine to_scaled(self,X,fac)
   ! Set "self" to a scaled version of "X". The two must be compatible.
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: X
      REAL, IN :: fac
      select case (genre_(self))
      case ("r "); self%r = fac*X%r
      case ("u "); self%a = fac*X%a
         self%b = fac*X%b
      case ("gc"); self%gc = fac*X%gc
      end select
   end subroutine

   PURE subroutine plus_scaled(self,X,fac)
   ! Add to "self" a scaled version of "X".
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: X
      REAL, IN :: fac
      select case (genre_(self))
      case ("r "); self%r = self%r+fac*X%r
      case ("u "); self%a = self%a+fac*X%a
         self%b = self%b+fac*X%b
      case ("gc"); self%gc = self%gc+fac*X%gc
      end select
   end subroutine

   PURE subroutine to_product_of_0(self,X,Y)
   ! Set "self" to product of "X" times "self", for all genres. 
      type(OPMATRIX_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: X
      type(OPMATRIX_TYPE), IN :: Y
      if (allocated(Y%r)) self%r = matmul(X,Y%r)
      if (allocated(Y%a)) self%a = matmul(X,Y%a)
      if (allocated(Y%b)) self%b = matmul(X,Y%b)
      if (allocated(Y%gc)) self%gc = matmul(X,Y%gc)
   end subroutine

   PURE subroutine to_product_of_1(self,X,Y)
   ! Set "self" to product of "X" times "self", for all genres. 
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: X,Y
      if (allocated(X%r)) self%r = matmul(X%r,Y%r)
      if (allocated(X%a)) self%a = matmul(X%a,Y%a)
      if (allocated(X%b)) self%b = matmul(X%b,Y%b)
      if (allocated(X%gc)) self%gc = matmul(X%gc,Y%gc)
   end subroutine

   PURE subroutine damp(self,old,mix)
   ! Constructs the new self matrix by combining it with the "old" one
   ! using a "mix" factor, i.e.  self = (1-mix)*self + mix*old.
      type(OPMATRIX_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: old
      REAL, IN :: mix
      call scale_by_(self,ONE-mix)
      call plus_scaled_(self,old,mix)
   end subroutine

   function expectation_0(self,X) result (res)
   ! Expectation value of the matrix "X": res = Trace (X.self)
      type(OPMATRIX_TYPE), IN :: self
      type(OPMATRIX_TYPE), IN :: X
      REAL :: res
   ENSURE(X%n_bf==self%n_bf,"OPMATRIX:expectation_0 ... wrong X dimension")
      res = ZERO
      select case (genre_(self))
      case ("r "); res = trace_product_with_(self%r,X%r)
      case ("u "); res = trace_product_with_(self%a,X%a)+trace_product_with_(self%b,X%b)
      case ("gc"); res = RE(trace_product_with_(self%gc,X%gc))
      end select
   end function

   function expectation_1(self,X) result (res)
   ! Expectation value of the matrix "X": res = Trace (X.self)
      type(OPMATRIX_TYPE), IN :: self
      MAT(REAL,:,:), IN :: X
      REAL :: res
   ENSURE(size(X,1)==self%n_bf,"OPMATRIX:expectation_1 ... wrong X dimension")
   ENSURE(is_square_(X),"OPMATRIX:expectation_1 ... X is not square")
      res = ZERO
      select case (genre_(self))
      case ("r "); res = trace_product_with_(self%r,X)
      case ("u "); res = trace_product_with_(self%a,X)+trace_product_with_(self%b,X)
      case ("gc"); res = RE(trace_product_with_(self%gc,X))
      end select
   end function


!  ==================
!  Orthonormalization
!  ==================

   PURE subroutine Schmidt_orthonormalize(self,S,scale)
   ! Schmidt Orthonormalize self.  (For molecular orbitals).
      type(OPMATRIX_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: S
      REAL, optional, OUT :: scale
      MAT(REAL,:,:), allocatable :: SS
      INT :: n
      select case (genre_(self))

      case ("r ")
         call Schmidt_orthonormalize_(self%r,S,scale)

      case ("u ")
         call Schmidt_orthonormalize_(self%a,S)
         call Schmidt_orthonormalize_(self%b,S)

      case ("gc")
         n = 2*size(S,1)
         call create_(SS,n,n)
         SS = ZERO
         call set_aa_block_to_(SS,S)
         call set_bb_block_to_(SS,S)
         call Schmidt_orthonormalize_(self%gc,SS,scale)
         call destroy_(SS)
      end select
   end subroutine

   PURE subroutine symmetrically_orthonormalize(self,S)
   ! Symmetrically orthonormalize self. (For molecular orbitals).
   ! "S" is the AO overlap matrix.
      type(OPMATRIX_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: S
      MAT(REAL,:,:), allocatable :: SS
      INT :: n
      select case (genre_(self))

      case ("r ")
         call symmetrically_orthonormalize_(self%r,S)

      case ("u ")
         call symmetrically_orthonormalize_(self%a,S)
         call symmetrically_orthonormalize_(self%b,S)

      case ("gc")
         n = 2*size(S,1)
         call create_(SS,n,n)
         SS = ZERO
         ENSURE(size(SS,1)==2*size(S,1),"OPMATRIX:symmetrically_orthonormalize ... wrong size, S")
         call set_aa_block_to_(SS,S)
         call set_bb_block_to_(SS,S)
         call symmetrically_orthonormalize_(self%gc,SS)
         call destroy_(SS)
      end select
   end subroutine


!  ===============================
!  Normalisation & unnormalisation
!  ===============================

   PURE subroutine unnormalize_0(self,first,last,angmom)
   ! *Remove* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      type(OPMATRIX_TYPE), INOUT :: self
      VEC(INT,:), IN :: first,last,angmom
      select case (genre_(self))

      case ("r ")
         call unnormalize_(self%r,first,last,angmom)

      case ("u ")
         call unnormalize_(self%a,first,last,angmom)
         call unnormalize_(self%b,first,last,angmom)

      case ("gc")
         call unnormalize_(self%gc(1:self%n_bf,1:self%n_bf),first,last,angmom)
         call unnormalize_(self%gc(1:self%n_bf,self%n_bf+1:2*self%n_bf),first,last,angmom)
         call unnormalize_(self%gc(self%n_bf+1:self%n_bf,1:self%n_bf),first,last,angmom)
         call unnormalize_(self%gc(self%n_bf+1:self%n_bf,self%n_bf+1:2*self%n_bf),first,last,angmom)
      end select
   end subroutine

   PURE subroutine unnormalize_1(self,ind,first,last,angmom)
   ! *Remove* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: first,last,angmom
      select case (genre_(self))

      case ("r ")
         call unnormalize_(self%r,ind,first,last,angmom)

      case ("u ")
         call unnormalize_(self%a,ind,first,last,angmom)
         call unnormalize_(self%b,ind,first,last,angmom)

      case ("gc")
         call unnormalize_(self%gc(1:self%n_bf,1:self%n_bf),ind,first,last,angmom)
         call unnormalize_(self%gc(1:self%n_bf,self%n_bf+1:2*self%n_bf),ind,first,last,angmom)
         call unnormalize_(self%gc(self%n_bf+1:2*self%n_bf,1:self%n_bf),ind,first,last,angmom)
         call unnormalize_(self%gc(self%n_bf+1:2*self%n_bf,self%n_bf+1:2*self%n_bf),ind,first,last,angmom)
      end select
   end subroutine

   PURE subroutine renormalize_0(self,first,last,angmom)
   ! *Include* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      type(OPMATRIX_TYPE), INOUT :: self
      VEC(INT,:), IN :: first,last,angmom
      select case (genre_(self))

      case ("r ")
         call renormalize_(self%r,first,last,angmom)

      case ("u ")
         call renormalize_(self%a,first,last,angmom)
         call renormalize_(self%b,first,last,angmom)

      case ("gc")
         call renormalize_(self%gc(1:self%n_bf,1:self%n_bf),first,last,angmom)
         call renormalize_(self%gc(1:self%n_bf,self%n_bf+1:2*self%n_bf),first,last,angmom)
         call renormalize_(self%gc(self%n_bf+1:2*self%n_bf,1:self%n_bf),first,last,angmom)
         call renormalize_(self%gc(self%n_bf+1:2*self%n_bf,self%n_bf+1:2*self%n_bf),first,last,angmom)
      end select
   end subroutine

   PURE subroutine renormalize_1(self,ind,first,last,angmom)
   ! *Include* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      type(OPMATRIX_TYPE), INOUT :: self
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: first,last,angmom
      select case (genre_(self))

      case ("r ")
         call renormalize_(self%r,ind,first,last,angmom)

      case ("u ")
         call renormalize_(self%a,ind,first,last,angmom)
         call renormalize_(self%b,ind,first,last,angmom)

      case ("gc")
         call renormalize_(self%gc(1:self%n_bf,1:self%n_bf),ind,first,last,angmom)
         call renormalize_(self%gc(1:self%n_bf,self%n_bf+1:2*self%n_bf),ind,first,last,angmom)
         call renormalize_(self%gc(self%n_bf+1:2*self%n_bf,1:self%n_bf),ind,first,last,angmom)
         call renormalize_(self%gc(self%n_bf+1:2*self%n_bf,self%n_bf+1:2*self%n_bf),ind,first,last,angmom)
      end select
   end subroutine

end module
