!=====================================================================
!
! GAUSSIAN_DATA
!
! This module is used to store precalculated information pertaining to
! gaussian functions, including gaussian powers an gaussian indices,
! data for the reduced multiplication scheme (RMS) used in electron
! repulsion integral evaluation, data for the transfer relation, and
! binomial coefficients for the reverse transfer relation. The type is
! a dummy: the data is all held as module arrays.
!
! Copyright (C) Dylan Jayatilaka, 2006
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
module GAUSSIAN_DATA_MODULE

#  include "macros"

   include "gaussian_data.use"

   implicit none

   include "gaussian_data.int"

   INT, public :: l_max_saved = -1

   ! Standard angular momentum character symbols
   VEC(STR(len=STR_SIZE),:), allocatable, public :: l_symbol

   ! List of cartesian powers for gaussian functions in increasing angular
   ! momentum. This defines the lexical order. 
   VEC(INT,:), allocatable, public :: px
   VEC(INT,:), allocatable, public :: py
   VEC(INT,:), allocatable, public :: pz
   MAT(INT,:,:), allocatable, public :: pp

   ! String representations of the gaussian powers
   VEC(STR(len=STR_SIZE),:), allocatable, public :: pxyz_rep  ! e.g. xxyy
   VEC(STR(len=STR_SIZE),:), allocatable, public :: pxyz_pow  ! e.g. x2y2
   VEC(STR(len=STR_SIZE),:), allocatable, public :: p123_rep  ! e.g. 1122
   VEC(STR(len=STR_SIZE),:), allocatable, public :: b123_rep  ! e.g. [1][1][2][2]
! pLM_real :: VEC{STR}@, public  ! e.g. 21+

   ! The *un* normalizing factors apply to cartesian gaussian basis
   ! functions (i.e. components) in an angular momentum L-shell
   ! such that they all have the normalization factor for
   ! the componenents x^L, y^L or z^L.
   ! IMPORTANT: Tonto uses cartesian gaussian basis functions for
   ! which *every* component in an L-shell is multiplied by the
   ! normalization factor for x^L (or y^l or z^L). Thus, cartesian
   ! gaussian basis functions different from these in an L-shell are
   ! *NOT* normalized to unity. The "unnormalizing factors" returned
   ! by this procedure multiply L-shell basis function components so
   ! that each component would be normalized.  This is needed to
   ! ensure spherical harmonic components are all normalized. The
   ! factors are introduced into the spherical harmonic transformation
   ! matrices, see below. They are also used when talking to other
   ! programs which usually have every cartesian gaussian normalized.
   ! i.e. tonto-norm = unit-norm * unnormalizing_factors
   VEC(REAL,:), allocatable, public :: unnormalizing_factors

   ! List of cartesian indices for gaussian functions in increasing angular
   ! momentum. The indices are the powers plus 1.
   VEC(INT,:), allocatable, public :: nx
   VEC(INT,:), allocatable, public :: ny
   VEC(INT,:), allocatable, public :: nz
   MAT(INT,:,:), allocatable, public :: nn

   ! Set TRUE if Ishida's ordering is to be used
   ! This turns of special code in SHELL1QUARTET
   BIN, public :: Ishida_ordering = FALSE

   ! Set TRUE if Ishida's ordering is to be used
   ! This turns of special code in SHELL1QUARTET
   BIN, public :: CRYSTAL23_ordering = FALSE

   ! These are indexing arrays used for the horizontal recursion relation.
   ! They are set using the set_gaussian_indices routine.
   VEC(INT,:), allocatable, public :: first_nonzero
   MAT3(INT,:,:,:), allocatable, public :: index_of
   VEC(INT,:), allocatable, public :: index_m1
   MAT(INT,:,:), allocatable, public :: index_p1

   ! The binomial coefficients, plus 1, for reverse HRR
   VEC(type(EVEC_INT_TYPE),:), allocatable, public :: binomial_p1

   ! List of arrays used for the reduced multiplication scheme (RMS)
   MAT(type(RMS_INDICES_TYPE),:,:), allocatable, public :: rms_indices

   ! Array of spherical harmonic coefficients
   VEC(type(EMAT_REAL_TYPE),:), allocatable, public :: spherical_harmonics_for

contains

! =================
! Memory allocation
! =================

   subroutine create(self)
   ! Create an object. This version does not nullify point parts.
      type(GAUSSIAN_DATA_TYPE), PTR :: self
      INT :: status

      ! The following code is inherited from OBJECT
      nullify(self)
      allocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN_DATA:create ... error in allocating memory")
      status = status
      status = 0
      call set_defaults_(self)
   end subroutine

   subroutine destroy(self)
   ! Destroy an object. This version does not destroy pointer parts.
      type(GAUSSIAN_DATA_TYPE), PTR :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT associated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"GAUSSIAN_DATA:destroy ... error in allocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   subroutine destroy_data()
   ! Destroy all module data and reset l_max_saved.
      call destroy_(spherical_harmonics_for)
      call destroy_(rms_indices)
      call destroy_(binomial_p1)
      call destroy_(index_p1)
      call destroy_(index_m1)
      call destroy_(first_nonzero)
      call destroy_(index_of)
      call destroy_(unnormalizing_factors)
      call destroy_(nn); call destroy_(nz); call destroy_(ny); call destroy_(nx)
      call destroy_(pp); call destroy_(pz); call destroy_(py); call destroy_(px)
      call destroy_(b123_rep)
      call destroy_(p123_rep)
      call destroy_(pxyz_pow)
      call destroy_(pxyz_rep)
      call destroy_(l_symbol)
   end subroutine


! ===========
! Set methods
! ===========

   subroutine set_defaults(self)
   ! Set defaults
      type(GAUSSIAN_DATA_TYPE), INOUT :: self
      self%l_max = 0
   end subroutine

   subroutine set_CRYSTAL23_ordering(val)
   ! Set defaults
      BIN, IN :: val
      CRYSTAL23_ordering = val
   end subroutine


! =========================================
! Boundaries of each angular momentum range
! =========================================

   pure function n_comp(l) result (res)
   ! No. of cartesian components in a gaussian shell of momentum "l".
      INT, IN :: l
      INT :: res
      res = (l+1)*(l+2)/2
   end function

   pure function n_comp_up_to(l) result (res)
   ! No. of gaussian cartesian component functions up to and including
   ! the shell with momentum "l".
      INT, IN :: l
      INT :: res
      res = (l+1)*(l+2)*(l+3)/6
   end function

   pure function n_sph(l) result (res)
   ! No. of spherical components in a gaussian shell of momentum "l".
      INT, IN :: l
      INT :: res
      res = 2*l+1
   end function

   pure function n_sph_up_to(l) result (res)
   ! No. of spherical component functions up to and including the
   ! shell with momentum "l".
      INT, IN :: l
      INT :: res
      res = (l+1)*(l+1)
   end function


! This is the main routine: all others are private.

   subroutine set_indices(l_max)
   ! Set the gaussian power indices, hrr indices, and the rms indices. 
      INT, IN :: l_max
      INT :: l,l2,n
   ENSURE(l_max<=23,"GAUSSIAN_DATA:set_indices ... angular momentum too large:"//trim(to_str_(l_max)))

      ! Already made? Then return
      if (l_max<=l_max_saved) return

      ! Set l_lax_saved
      l_max_saved = l_max
      l2 = 2*l_max

      ! Set l symbols
      call create_(l_symbol,0,l_max)
      do l = 0,l_max
         select case (l)
         case (0); l_symbol(l) = "s"
         case (1); l_symbol(l) = "p"
         case (2); l_symbol(l) = "d"
         case (3); l_symbol(l) = "f"
         case default; l_symbol(l) = achar(l-4+iachar("g"))
         end select
      end do

      ! Set no of components up to 2*l
      n = n_comp_up_to(l2)

      ! Destroy all data (leaky)
      call destroy_data

      ! Set the gaussian data
      call create_(px,n); call create_(py,n); call create_(pz,n); call create_(pp,3,n)
      call create_(nx,n); call create_(ny,n); call create_(nz,n); call create_(nn,3,n)
      call create_(unnormalizing_factors,n)
      call create_(index_of,l2+1,l2+1,l2+1)
      call create_(first_nonzero,n)
      call create_(index_m1,n)
      call create_(index_p1,n,3)
      call set_gaussian_indices(l2,nx,ny,nz,first_nonzero,index_of,index_m1,index_p1)
   ! GAUSSIAN_DATA::set_Ishida_indices(l2,nx,ny,nz,first_nonzero,index_of,index_m1,index_p1)

      ! Set nn convenience arrays
      nn(1,:) = nx
      nn(2,:) = ny
      nn(3,:) = nz

      ! Set px,py,pz and pp convenience arrays
      px = nx-1
      py = ny-1
      pz = nz-1
      pp = nn-1

      ! Set the string representations
      call create_(pxyz_rep,n)
      call create_(pxyz_pow,n)
      call create_(p123_rep,n)
      call create_(b123_rep,n)
      call set_gaussian_power_str(pxyz_rep,pxyz_pow,p123_rep,b123_rep,px,py,pz)

      ! Set normalizing factors
      call set_unnormalizing_factors(l2,unnormalizing_factors,px,py,pz)

      ! Set binomial coefficients via Pascal's triangle
      call set_binomial_p1_coefficients(l_max,binomial_p1)

      ! Reduced multiplication scheme (rms) indices
      call create_(rms_indices,0,l_max,0,l_max)
      call set_indices_(rms_indices,l_max,nx,ny,nz,px,py,pz)

      ! Make spherical harmonic coefficients -- created in here
      call make_xyz_to_solid_harmonics(spherical_harmonics_for)
   end subroutine

   subroutine set_gaussian_indices(l_max,nx,ny,nz,first_nonzero,index_of,index_m1,index_p1)
   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
   ! momentum up to "l_max". (Actually adds one to each power to avoid 0 as
   ! lower bound). Return "first_nonzero(i)", the cartesian index (between 1 and
   ! 3) of the first non-zero power for the gaussian with lexical index "i".
   ! Also return "index_m1(i)" and "index_p1(i)", the lexical indices of the
   ! gaussians with the first nonzero component decremented by one, and
   ! incrementred by one, respectively. In the latter case we must store all
   ! possible increments.  These are used to determine which gaussian are to be
   ! used in the HRR recursion relations.  
      INT, IN :: l_max
      VEC(INT,:), OUT :: nx,ny,nz
      VEC(INT,:), OUT :: first_nonzero
      MAT3(INT,:,:,:), OUT :: index_of
      VEC(INT,:), OUT :: index_m1
      MAT(INT,:,:), OUT :: index_p1
      INT :: L,a,b,c,i,a1,b1,c1,f
      VEC(INT,3) :: p,m
   ENSURE(size(nx)==n_comp_up_to(l_max),"GAUSSIAN_DATA:set_gaussian_indices ... wrong size, nx")
   ENSURE(size(ny)==size(nx),"GAUSSIAN_DATA:set_gaussian_indices ... wrong size, ny")
   ENSURE(size(nz)==size(nx),"GAUSSIAN_DATA:set_gaussian_indices ... wrong size, nz")
   ENSURE(size(first_nonzero)==size(nx),"GAUSSIAN_DATA:set_gaussian_indices ... wrong dimension, first_nonzero")
   ENSURE(size(index_m1)==size(nx),"GAUSSIAN_DATA:set_gaussian_indices ... wrong dimension, index_m1")
   ENSURE(size(index_p1,1)==size(nx),"GAUSSIAN_DATA:set_gaussian_indices ... wrong 1st dimension, index_p1")
   ENSURE(size(index_p1,2)==3,"GAUSSIAN_DATA:set_gaussian_indices ... wrong 2nd dimension, index_p1")
   ENSURE(size(index_of,1)==(l_max+1),"GAUSSIAN_DATA:set_gaussian_indices ... wrong 1st dimension, index_of")
   ENSURE(size(index_of,2)==(l_max+1),"GAUSSIAN_DATA:set_gaussian_indices ... wrong 2nd dimension, index_of")
   ENSURE(size(index_of,3)==(l_max+1),"GAUSSIAN_DATA:set_gaussian_indices ... wrong 3rd dimension, index_of")
      index_of = 0
      i = 1

      ! Loop over angular momentum
      do L = 0,l_max  ! loop on L
         do a = L,floor((L+2)*THIRD),-1  ! 1st highest power
            do b = min(L-a,a),floor((L-a+1)*HALF),-1  ! 2nd highest power

         ! Get the third power
               c = L-a-b

         ! All powers equal
               if (a==b AND b==c) then
                  a1 = a+1
                  nx(i) = a1; ny(i) = a1; nz(i) = a1
                  index_of(a1,a1,a1) = i
                  first_nonzero(i) = 1
                  i = i+1

         ! 2nd & 3rd pwers equal
               else if (a>b AND b==c) then
                  a1 = a+1
                  b1 = b+1
                  nx(i) = a1; ny(i) = b1; nz(i) = b1
                  nx(i+1) = b1; ny(i+1) = a1; nz(i+1) = b1
                  nx(i+2) = b1; ny(i+2) = b1; nz(i+2) = a1
                  index_of(a1,b1,b1) = i
                  index_of(b1,a1,b1) = i+1
                  index_of(b1,b1,a1) = i+2
                  first_nonzero(i) = 1
                  first_nonzero(i+1) = 2
                  first_nonzero(i+2) = 3
                  if (b>0) then
                     first_nonzero(i+1) = 1
                     first_nonzero(i+2) = 1
                  end if
                  i = i+3

         ! 1st and 2nd powers equal
               else if (a==b AND b>c) then
                  a1 = a+1
                  c1 = c+1
                  nx(i) = a1; ny(i) = a1; nz(i) = c1
                  nx(i+1) = a1; ny(i+1) = c1; nz(i+1) = a1
                  nx(i+2) = c1; ny(i+2) = a1; nz(i+2) = a1
                  index_of(a1,a1,c1) = i
                  index_of(a1,c1,a1) = i+1
                  index_of(c1,a1,a1) = i+2
                  first_nonzero(i) = 1
                  first_nonzero(i+1) = 1
                  first_nonzero(i+2) = 2
                  if (c>0) then
                     first_nonzero(i+2) = 1
                  end if
                  i = i+3

         ! No power equal
               else
                  a1 = a+1
                  b1 = b+1
                  c1 = c+1
                  nx(i) = a1; ny(i) = b1; nz(i) = c1
                  nx(i+1) = a1; ny(i+1) = c1; nz(i+1) = b1
                  nx(i+2) = b1; ny(i+2) = a1; nz(i+2) = c1
                  nx(i+3) = c1; ny(i+3) = a1; nz(i+3) = b1
                  nx(i+4) = b1; ny(i+4) = c1; nz(i+4) = a1
                  nx(i+5) = c1; ny(i+5) = b1; nz(i+5) = a1
                  index_of(a1,b1,c1) = i
                  index_of(a1,c1,b1) = i+1
                  index_of(b1,a1,c1) = i+2
                  index_of(c1,a1,b1) = i+3
                  index_of(b1,c1,a1) = i+4
                  index_of(c1,b1,a1) = i+5
                  if (a>0) then
                     first_nonzero(i) = 1
                     first_nonzero(i+1) = 1
                     first_nonzero(i+2) = 2
                     first_nonzero(i+3) = 2
                     first_nonzero(i+4) = 3
                     first_nonzero(i+5) = 3
                  else
                     first_nonzero(i) = 3
                     first_nonzero(i+1) = 3
                     first_nonzero(i+2) = 3
                     first_nonzero(i+3) = 3
            ! i+4,i+5 must have b or c nonzero.
                  end if
                  if (b>0) then
                     first_nonzero(i+2) = 1
                     first_nonzero(i+4) = 1
                     first_nonzero(i+5) = 2
                  end if
                  if (c>0) then
                     first_nonzero(i+3) = 1
                     first_nonzero(i+5) = 1
                  end if
                  i = i+6
               end if
            end do
         end do
      end do

      ! Now make the index_p1, index_m1 arrays
      do i = 1,size(nx)

         ! Powers plus one
         p = [nx(i),ny(i),nz(i)]
         m = p

         ! First non-zero index
         f = first_nonzero(i)

         ! Subtract one power from first nonzero index
         m(f) = m(f)-1

         ! Get index of first non-zero subtract one power
         if (any(m<1)) then; index_m1(1) = -1
         else; index_m1(i) = index_of(m(1),m(2),m(3))
         end if

         ! Now add one to each power amd get index
         do f = 1,3
            p(f) = p(f)+1
            if (any(p>l_max+1)) then; index_p1(i,f) = -1
            else; index_p1(i,f) = index_of(p(1),p(2),p(3))
            end if
            p(f) = p(f)-1
         end do
      end do

      ! Fix up s function which has no non-zero
      first_nonzero(1) = -1
   end subroutine

   subroutine set_gaussian_power_str(pxyz_rep,pxyz_pow,p123_rep,b123_rep,px,py,pz)
   ! Make the xyz powers in a string representation from the integer
   ! powers in "px", "py", "pz". Useful for output.
   ! "pxyz_rep" are the repeated string-components e.g. xxyy
   ! "p123_rep" are the repeated number-components e.g. 1122
   ! "pxyz_pow" have the string-components as powers e.g. x2y2
      VEC(STR(len=*),:), OUT :: pxyz_rep
      VEC(STR(len=*),:), OUT :: pxyz_pow
      VEC(STR(len=*),:), OUT :: p123_rep
      VEC(STR(len=*),:), OUT :: b123_rep
      VEC(INT,:), IN :: px,py,pz
      INT :: dim,i
      STR(len=STR_SIZE) :: val
   ENSURE(size(pxyz_rep,1)==size(px,1),"GAUSSIAN_DATA:set_gaussian_power_str ... pxyz_rep: wrong dim1")
   ENSURE(size(pxyz_pow,1)==size(px,1),"GAUSSIAN_DATA:set_gaussian_power_str ... pxyz_pow: wrong dim1")
   ENSURE(size(p123_rep,1)==size(px,1),"GAUSSIAN_DATA:set_gaussian_power_str ... p123_rep: wrong dim1")
      dim = size(px,1)
      do i = 1,dim

         ! Make pxyz_rep
         pxyz_rep(i) = repeat("x",px(i))//repeat("y",py(i))//repeat("z",pz(i))

         ! Make p123_rep
         p123_rep(i) = repeat("1",px(i))//repeat("2",py(i))//repeat("3",pz(i))

         ! Make p123_rep
         b123_rep(i) = repeat("[1]",px(i))//repeat("[2]",py(i))//repeat("[3]",pz(i))

         ! Make pxyz_pow
         val = " "
         if (px(i)==1) val = "x"
         if (px(i)>1) val = "x"//trim(to_str_(px(i)))
         if (py(i)==1) val = trim(val)//"y"
         if (py(i)>1) val = trim(val)//"y"//trim(to_str_(py(i)))
         if (pz(i)==1) val = trim(val)//"z"
         if (pz(i)>1) val = trim(val)//"z"//trim(to_str_(pz(i)))
         pxyz_pow(i) = val
      end do
   end subroutine


! Below is Ishida's original (corrected) ordering
! To use this change make_esfs -> make_esfs_general
! in SHELL1QUARTET

!   set_Ishida_indices(l_max,nx,ny,nz,first_nonzero,index_of,index_m1,index_p1) :: selfless, private
!   ! Same as set_indices except using Ishida's ordering scheme.
!   ! See Ishida (1993), JCP 98 p2176.
!   ! NOTE: NOT WORKING
!   ! Make the xyz powers "nx", "ny", "nz" for a cartesian gaussian with angular
!   ! momentum up to "l_max". (Actually adds one to each power to avoid 0 as
!   ! lower bound). Return "first_nonzero(i)", the cartesian index (between 1 and
!   ! 3) of the first non-zero power for the gaussian with lexical index "i".
!   ! Also return "index_m1(i)" and "index_p1(i)", the lexical indices of the
!   ! gaussians with the first nonzero component decremented by one, and
!   ! incrementred by one, respectively. In the latter case we must store all
!   ! possible increments.  These are used to determine which gaussian are to be
!   ! used in the HRR recursion relations.  
!      l_max :: INT, IN
!      nx,ny,nz :: VEC{INT}, OUT
!      first_nonzero :: VEC{INT}, OUT
!      index_of :: MAT3{INT}, OUT
!      index_m1 :: VEC{INT}, OUT
!      index_p1 :: MAT{INT}, OUT
!
!   ENSURE(nx.dim==GAUSSIAN_DATA::n_comp_up_to(l_max),"wrong size, nx")
!   ENSURE(ny.dim==nx.dim,"wrong size, ny")
!   ENSURE(nz.dim==nx.dim,"wrong size, nz")
!   ENSURE(first_nonzero.dim==nx.dim,"wrong dimension, first_nonzero")
!   ENSURE(index_m1.dim==nx.dim,"wrong dimension, index_m1")
!   ENSURE(index_p1.dim1==nx.dim,"wrong 1st dimension, index_p1")
!   ENSURE(index_p1.dim2==3,"wrong 2nd dimension, index_p1")
!   ENSURE(index_of.dim1==(l_max+1),"wrong 1st dimension, index_of")
!   ENSURE(index_of.dim2==(l_max+1),"wrong 2nd dimension, index_of")
!   ENSURE(index_of.dim3==(l_max+1),"wrong 3rd dimension, index_of")
!
!      i,i1,i2,L,M,N,a1,b1,c1,f :: INT
!      p,q :: VEC{INT}(3)
!
!      Ishida_ordering = TRUE
!      index_of = -1
!      index_p1 = -1
!
!      ! S-type gaussian
!      nx(1 ) = 1; ny(1 ) =  1; nz(1 ) =  1
!      index_of( 1, 1, 1) = 1
!      first_nonzero(1 )  = 1
!
!      ! P-type gaussians and greater
!      i = 2
!      do L = 1,l_max
!
!        a1 = L + 1
!
!        i1 = i + 1
!        i2 = i + 2
!
!        nx(i ) = a1; ny(i ) =  1; nz(i ) =  1
!        nx(i1) =  1; ny(i1) = a1; nz(i1) =  1
!        nx(i2) =  1; ny(i2) =  1; nz(i2) = a1
!
!        index_of(a1, 1, 1) = i
!        index_of( 1,a1, 1) = i1
!        index_of( 1, 1,a1) = i2
!
!        first_nonzero(i )  = 1
!        first_nonzero(i1)  = 2
!        first_nonzero(i2)  = 3
!
!        i = i + 3
!
!        if (L<2) cycle
!
!        do M = 1,L/2
!
!           a1 = M + 1
!           b1 = L - M + 1
!
!           i1 = i + 1
!           i2 = i + 2
!
!           nx(i ) = a1; ny(i ) = b1; nz(i ) =  1
!           nx(i1) =  1; ny(i1) = a1; nz(i1) = b1
!           nx(i2) = b1; ny(i2) =  1; nz(i2) = a1
!
!           index_of(a1,b1, 1) = i
!           index_of( 1,a1,b1) = i1
!           index_of(b1, 1,a1) = i2
!
!           first_nonzero(i )  = 1   ! These are not first non-zero
!           first_nonzero(i1)  = 2   ! but good enough for recurrence
!           first_nonzero(i2)  = 3
!
!           i = i + 3
!
!           if (a1==b1) cycle
!
!           i1 = i + 1
!           i2 = i + 2
!
!           nx(i ) = a1; ny(i ) =  1; nz(i ) = b1
!           nx(i1) = b1; ny(i1) = a1; nz(i1) =  1
!           nx(i2) =  1; ny(i2) = b1; nz(i2) = a1
!
!           index_of(a1, 1,b1) = i
!           index_of(b1,a1, 1) = i1
!           index_of( 1,b1,a1) = i2
!
!           first_nonzero(i )  = 1   ! These are not first non-zero
!           first_nonzero(i1)  = 2   ! but good enough for recurrence
!           first_nonzero(i2)  = 3
!
!           i = i + 3
!
!        end
!
!        if (L<3) cycle
!
!        do M = 2,(2*L)/3
!        do N = max(1,2*M-L),M/2     ! <<< Mistake from Ishida?
!
!           a1 = N + 1
!           b1 = M - N + 1
!           c1 = L - M + 1
!
!           nx(i  ) = a1; ny(i  ) = b1; nz(i  ) = c1
!           index_of(a1,b1,c1) = i
!           first_nonzero(i )  = 1  
!
!           i = i + 1
!
!           if (a1==b1 AND b1==c1) cycle
!
!           i1 = i + 1
!
!           nx(i ) = c1; ny(i ) = a1; nz(i ) = b1
!           nx(i1) = b1; ny(i1) = c1; nz(i1) = a1
!
!           index_of(c1,a1,b1) = i
!           index_of(b1,c1,a1) = i1
!
!           first_nonzero(i )  = 2  
!           first_nonzero(i1)  = 3  
!
!           i = i + 2
!
!           if (a1==b1 OR b1==c1 OR a1==c1) cycle
!
!           i1 = i + 1
!           i2 = i + 2
!
!           nx(i ) = a1; ny(i ) = c1; nz(i ) = b1
!           nx(i1) = b1; ny(i1) = a1; nz(i1) = c1
!           nx(i2) = c1; ny(i2) = b1; nz(i2) = a1
!
!           index_of(a1,c1,b1) = i
!           index_of(b1,a1,c1) = i1
!           index_of(c1,b1,a1) = i2
!
!           first_nonzero(i )  = 1  
!           first_nonzero(i1)  = 2   
!           first_nonzero(i2)  = 3
!
!           i = i + 3
!
!        end
!        end
!     
!      end
!
!      ! Now make the index_p1, index_m1 arrays
!      do i = 1,nx.dim         
!
!         ! Powers plus one
!         p = [nx(i),ny(i),nz(i)]
!         q = p        
!
!         ! First non-zero index
!         f = first_nonzero(i)
!
!         ! Subtract one power from first nonzero index
!         q(f) = q(f) - 1
!
!         ! Get index of first non-zero subtract one power
!         if (any(q<1)) then; index_m1(1) = -1
!         else;               index_m1(i) = index_of(q(1),q(2),q(3))
!         end
!
!         ! Now add one to each power amd get index
!         do f = 1,3
!            p(f) = p(f) + 1
!            if (any(p>l_max+1)) then; index_p1(i,f) = -1
!            else;                     index_p1(i,f) = index_of(p(1),p(2),p(3))
!            end
!            p(f) = p(f) - 1
!         end
!
!      end
!
!      ! Fix up s function which has no non-zero
!      first_nonzero(1) = -1   
!
!   end

! ========================================
! Cartesian product string representations
! ========================================

   PURE function xyz_product_str_for(l) result (res)
   ! Return the string representation of the cartesian part of a
   ! gaussian of angular momentum "l"
      INT, IN :: l
      VEC(STR(len=STR_SIZE),n_comp(l)) :: res
      INT :: f,n
   ENSURE(l>=0,"GAUSSIAN_DATA:xyz_product_str_for ... l is negative")
   ENSURE(l<=l_max_saved,"GAUSSIAN_DATA:xyz_product_str_for ... l too large, "//trim(to_str_(l)))
      f = n_comp_up_to(l-1)
      n = n_comp(l)
      res = pxyz_rep(f+1:f+n)
   end function


! ==============================
! Normalising & binomial factors
! ==============================

   PURE subroutine set_unnormalizing_factors(l_max,unnormalizing_factors,px,py,pz)
   ! IMPORTANT: Tonto uses cartesian gaussian basis functions for
   ! which *every* component in an L-shell is multiplied by the
   ! normalization factor for x^L (or y^l or z^L). Thus, cartesian
   ! gaussian basis functions different from these in an L-shell are
   ! *NOT* normalized to unity. The "normalising factors" returned by
   ! this procedure multiply L-shell basis function components so that
   ! each component would be normalized.  This is needed to ensure
   ! spherical harmonic components are all normalized. The factors are
   ! introduced into the spherical harmonic transformation matrices,
   ! see below. They are also used when talking to other programs
   ! which usually have every cartesian gaussian normalized.
   ! i.e. tonto-norm = unit-norm * unnormalizing_factors
      INT, IN :: l_max
      VEC(REAL,:), OUT :: unnormalizing_factors
      VEC(INT,:), IN :: px,py,pz
      REAL :: val
      INT :: l,s,f,i
   ENSURE(size(px)==n_comp_up_to(l_max),"GAUSSIAN_DATA:set_unnormalizing_factors ... wrong size, px")
   ENSURE(size(py)==size(px),"GAUSSIAN_DATA:set_unnormalizing_factors ... wrong size, py")
   ENSURE(size(pz)==size(px),"GAUSSIAN_DATA:set_unnormalizing_factors ... wrong size, pz")
   ENSURE(size(unnormalizing_factors)==size(px),"GAUSSIAN_DATA:set_unnormalizing_factors ... wrong dimension,")
      f = 0
      do l = 0,l_max
         s = f+1
         f = f+n_comp(l)
         do i = s,f
            val = double_factorial_(l)/(double_factorial_(px(i))*double_factorial_(py(i))*double_factorial_(pz(i)))
            unnormalizing_factors(i) = sqrt(val)
         end do
      end do
   end subroutine

   PURE subroutine set_binomial_p1_coefficients(l_max,binomial_p1)
   ! Set the binomial coefficients (plus 1) using Pascal's triangle.
   ! In this version one is added to all indices to avoid indexing from 0.
      INT, IN :: l_max
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: binomial_p1
      INT :: n,i
   ENSURE(l_max>=0,"GAUSSIAN_DATA:set_binomial_p1_coefficients ... l_max must be non-negative")

      ! Create
      call create_(binomial_p1,l_max+1)
      call create_(binomial_p1(1)%element,1)

      ! Initialize peak of triangle
      binomial_p1(1)%element(1) = 1

      ! Do Pascal's triangle
      do n = 2,l_max+1
         call create_(binomial_p1(n)%element,n)
         do i = 1,n
            if (i==1) then; binomial_p1(n)%element(i) = 1
            else if (i==n) then; binomial_p1(n)%element(i) = 1
            else; binomial_p1(n)%element(i) = binomial_p1(n-1)%element(i-1)+binomial_p1(n-1)%element(i)
            end if
         end do
      end do
   end subroutine

!  ===============================
!  Normalisation & unnormalisation
!  ===============================

! Tonto works with unnormalized cartesian L-shell gaussians
! So  when reading from other programs you must *unnormalize*
! And when printing out, remember to *renormalize*

   PURE subroutine unnormalize_0(X,first,last,angmom)
   ! *Remove* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      MAT(REAL,:,:), INOUT :: X
      VEC(INT,:), IN :: first,last,angmom

      ! The following code is inherited from GAUSSIAN_DATA
      call unnormalize_(X,"1",first,last,angmom)
      call unnormalize_(X,"2",first,last,angmom)
   end subroutine

   PURE subroutine unnormalize_1(X,first,last,angmom)
   ! *Remove* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      MAT(CPX,:,:), INOUT :: X
      VEC(INT,:), IN :: first,last,angmom

      ! The following code is inherited from GAUSSIAN_DATA
      call unnormalize_(X,"1",first,last,angmom)
      call unnormalize_(X,"2",first,last,angmom)
   end subroutine

   PURE subroutine unnormalize_2(X,ind,first,last,angmom)
   ! *Remove* the intra-shell basis function normalisation factors
   ! from matrix X (such factors are not used in Tonto). Works
   ! separatelty for ind equal to "row", or "1"; or "column" or "2".
   ! Requires baisis-fn shell arrays, as well as angular momentum.
   ! NOTE: do this before using foreign expansion coefficients.
   !
   ! Add normalization factors so that every shell component
   ! becomes unit-normalized. Tonto does not use these factors.
   ! You should do this before printing out Tonto operator
   ! cartesian-gaussian matrix elements
      MAT(REAL,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: first,last,angmom
      INT :: s,f,l,Ls
   ENSURE(all(angmom<=4),"GAUSSIAN_DATA:unnormalize_2 ... Cannot handle higher than G functions")

      ! The following code is inherited from GAUSSIAN_DATA
      select case (ind)

      case ("row","1")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call unnormalize_block_L_(X(f:l,:),Ls)
            end if
         end do

      case ("column","2")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call unnormalize_block_R_(X(:,f:l),Ls)
            end if
         end do
      end select
   end subroutine

   PURE subroutine unnormalize_3(X,ind,first,last,angmom)
   ! *Remove* the intra-shell basis function normalisation factors
   ! from matrix X (such factors are not used in Tonto). Works
   ! separatelty for ind equal to "row", or "1"; or "column" or "2".
   ! Requires baisis-fn shell arrays, as well as angular momentum.
   ! NOTE: do this before using foreign expansion coefficients.
   !
   ! Add normalization factors so that every shell component
   ! becomes unit-normalized. Tonto does not use these factors.
   ! You should do this before printing out Tonto operator
   ! cartesian-gaussian matrix elements
      MAT(CPX,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: first,last,angmom
      INT :: s,f,l,Ls
   ENSURE(all(angmom<=4),"GAUSSIAN_DATA:unnormalize_3 ... Cannot handle higher than G functions")

      ! The following code is inherited from GAUSSIAN_DATA
      select case (ind)

      case ("row","1")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call unnormalize_block_L_(X(f:l,:),Ls)
            end if
         end do

      case ("column","2")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call unnormalize_block_R_(X(:,f:l),Ls)
            end if
         end do
      end select
   end subroutine

   pure subroutine unnormalize_block_L_0(B,Ls)
   ! Unnormalize the left index of the integral block "B".
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: j,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do j = 1,size(B,2)
         B(:,j) = B(:,j)*unnormalizing_factors(fg:lg)
      end do
   end subroutine

   pure subroutine unnormalize_block_L_1(B,Ls)
   ! Unnormalize the left index of the integral block "B".
      MAT(CPX,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: j,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do j = 1,size(B,2)
         B(:,j) = B(:,j)*unnormalizing_factors(fg:lg)
      end do
   end subroutine

   pure subroutine unnormalize_block_R_0(B,Ls)
   ! Unnormalize the right index of the integral block "B".
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: i,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do i = 1,size(B,1)
         B(i,:) = B(i,:)*unnormalizing_factors(fg:lg)
      end do
   end subroutine

   pure subroutine unnormalize_block_R_1(B,Ls)
   ! Unnormalize the right index of the integral block "B".
      MAT(CPX,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: i,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do i = 1,size(B,1)
         B(i,:) = B(i,:)*unnormalizing_factors(fg:lg)
      end do
   end subroutine

   PURE subroutine renormalize_0(X,first,last,angmom)
   ! *Include* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      MAT(REAL,:,:), INOUT :: X
      VEC(INT,:), IN :: first,last,angmom

      ! The following code is inherited from GAUSSIAN_DATA
      call renormalize_(X,"1",first,last,angmom)
      call renormalize_(X,"2",first,last,angmom)
   end subroutine

   PURE subroutine renormalize_1(X,first,last,angmom)
   ! *Include* the intra-shell basis function normalisation factors
   ! from density matrices or operators "X".
   ! NOTE: do this before using foreign expansion coefficients.
      MAT(CPX,:,:), INOUT :: X
      VEC(INT,:), IN :: first,last,angmom

      ! The following code is inherited from GAUSSIAN_DATA
      call renormalize_(X,"1",first,last,angmom)
      call renormalize_(X,"2",first,last,angmom)
   end subroutine

   PURE subroutine renormalize_2(X,ind,first,last,angmom)
   ! *Include* the intra-shell basis function normalisation factors
   ! from matrix X (such factors are not used in Tonto). Works
   ! separatelty for ind equal to "row", or "1"; or "column" or "2".
   ! Requires baisis-fn shell arrays, as well as angular momentum.
   ! NOTE: do this before printing foreign Tonto coefficients.
   !
   ! Remove normalization factors so that every shell component
   ! becomes Tonto-normalized.
      MAT(REAL,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: first,last,angmom
      INT :: s,f,l,Ls
   ENSURE(all(angmom<=4),"GAUSSIAN_DATA:renormalize_2 ... Cannot handle higher than G functions")

      ! The following code is inherited from GAUSSIAN_DATA
      select case (ind)

      case ("row","1")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call renormalize_block_L_(X(f:l,:),Ls)
            end if
         end do

      case ("column","2")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call renormalize_block_R_(X(:,f:l),Ls)
            end if
         end do
      end select
   end subroutine

   PURE subroutine renormalize_3(X,ind,first,last,angmom)
   ! *Include* the intra-shell basis function normalisation factors
   ! from matrix X (such factors are not used in Tonto). Works
   ! separatelty for ind equal to "row", or "1"; or "column" or "2".
   ! Requires baisis-fn shell arrays, as well as angular momentum.
   ! NOTE: do this before printing foreign Tonto coefficients.
   !
   ! Remove normalization factors so that every shell component
   ! becomes Tonto-normalized.
      MAT(CPX,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: first,last,angmom
      INT :: s,f,l,Ls
   ENSURE(all(angmom<=4),"GAUSSIAN_DATA:renormalize_3 ... Cannot handle higher than G functions")

      ! The following code is inherited from GAUSSIAN_DATA
      select case (ind)

      case ("row","1")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call renormalize_block_L_(X(f:l,:),Ls)
            end if
         end do

      case ("column","2")
         do s = 1,size(first)
            f = first(s)
            l = last(s)
            Ls = angmom(s)
            if (Ls>1) then
               call renormalize_block_R_(X(:,f:l),Ls)
            end if
         end do
      end select
   end subroutine

   pure subroutine renormalize_block_L_0(B,Ls)
   ! Renormalize the left index of the integral block "B".
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: j,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do j = 1,size(B,2)
         B(:,j) = B(:,j)/unnormalizing_factors(fg:lg)
      end do
   end subroutine

   pure subroutine renormalize_block_L_1(B,Ls)
   ! Renormalize the left index of the integral block "B".
      MAT(CPX,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: j,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do j = 1,size(B,2)
         B(:,j) = B(:,j)/unnormalizing_factors(fg:lg)
      end do
   end subroutine

   pure subroutine renormalize_block_R_0(B,Ls)
   ! Renormalize the right index of the integral block "B".
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: i,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do i = 1,size(B,1)
         B(i,:) = B(i,:)/unnormalizing_factors(fg:lg)
      end do
   end subroutine

   pure subroutine renormalize_block_R_1(B,Ls)
   ! Renormalize the right index of the integral block "B".
      MAT(CPX,:,:), INOUT :: B
      INT, IN :: Ls
      INT :: i,nc,fg,lg

      ! The following code is inherited from GAUSSIAN_DATA
      nc = n_comp_(Ls)
      lg = n_comp_up_to_(Ls-1)
      fg = lg+1
      lg = lg+nc
      do i = 1,size(B,1)
         B(i,:) = B(i,:)/unnormalizing_factors(fg:lg)
      end do
   end subroutine


!  =======================================================
!  Cartesian to spherical harmonic transformation matrices
!  =======================================================

! For simplicity I've changed the ordering of the basis functions
! to be indentical to that in Molden & the Gaussian program.

   PURE subroutine make_xyz_to_solid_harmonics(sph)
   ! Make the cartesian gaussian to *real* solid harmonic gaussian
   ! transformation matrices up to angular momentum "l_max". The
   ! particular transformation matrix for angular momentum "L" is
   ! S^(LM)_c = sph(L).element(c,M) where "c" is the lexical index of
   ! the cartesian gaussian and "M" is the spherical component index,
   ! as defined in the book by Helgaker, Olsen, and Simons, p.218. 
   ! NOTE: sph is created. 
   ! NOTE: the M index is generated as -L:L
   !       but is changed to 0,+1,-1, ... ,+L,-L
   ! ALSO: S^(L 0 ) =        (2 pi/2L+1)^(1/2) Re [    Y_(L 0)] 
   !       S^(L +M) = (-1)^M (8 pi/2L+1)^(1/2) Re [r^L Y_(L M)] M>0
   !       S^(L -M) = (-1)^M (8 pi/2L+1)^(1/2) Im [r^L Y_(L M)] M>0
   ! where Y_(L M) are the *complex* solid harminics also known as
   !                   the (complex) *harmonic polynomials*
   ! These are the same thge normal spherical harmonics but just
   ! multiplied by the factor r^L . See inside square brackets.
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: sph
      INT :: l_max,two
      INT :: L,M,N,i
      INT :: x,y,z,del,nc
      INT :: lm1,lm0,lm2
      REAL :: top,bot,fac
      REAL :: val,valp,valm
      VEC(INT,3) :: ix,iy,iz,pow
      VEC(INT,:), allocatable :: std,stp,stm  ! standard S order
   ENSURE(l_max_saved>=0,"GAUSSIAN_DATA:make_xyz_to_solid_harmonics ... l_max_saved must be non-negative")

      ! Set l_max
      l_max = l_max_saved

      ! Initialising the tranformation matrices
      two = 2
      call create_(sph,0,l_max)
      do L = 0,l_max
         nc = n_comp(L)
         call create_(sph(L)%element,nc,2*L+1)
         sph(L)%element = ZERO
      end do

      ! Initialising S function
      sph(0)%element(1,1) = ONE

      ! Recurrence for the other functions
      do L = 0,l_max-1
         del = delta_(L,0)
         top = 2*L+1
         fac = sqrt((raised_to_(two,del))*top/(top+1))
         del = 1-del  ! <=== NOTE
         nc = n_comp(L)
         lm1 = n_comp_up_to(L-1)
         lm0 = n_comp_up_to(L)

         ! Standard Molden/Gaussian order of harmonics
         call create_(std,-L,L)  ! Std order for M = [ L  , L  ]
         std = into_std_S_order_(L)
         call create_(stp,-L-1,L+1)  ! Std order for M = [-L-1, L+1]
         stp = into_std_S_order_(L+1)
         do i = 1,nc  ! loop over cartesian gaussian in L-shell
            pow = pp(:,lm1+i)
            ix = pow+[1,0,0]+1
            iy = pow+[0,1,0]+1
            iz = pow+[0,0,1]+1

            ! Subtract lm0 for start of L+1 shell
            x = index_of(ix(1),ix(2),ix(3))-lm0  ! multipy by x
            y = index_of(iy(1),iy(2),iy(3))-lm0  ! multipy by y
            z = index_of(iz(1),iz(2),iz(3))-lm0  ! multipy by z
            N = std(L)
            valp = fac*sph(L)%element(i,N)
            N = std(-L)
            valm = fac*sph(L)%element(i,N)*del
            N = stp(L+1)
            sph(L+1)%element(x,N) = sph(L+1)%element(x,N)+valp
            sph(L+1)%element(y,N) = sph(L+1)%element(y,N)-valm
            N = stp(-L-1)
            sph(L+1)%element(x,N) = sph(L+1)%element(x,N)+valm
            sph(L+1)%element(y,N) = sph(L+1)%element(y,N)+valp

            ! First term of 6.4.73)
            do M = -L,L
               bot = (L+M+1)*(L-M+1)
               N = stp(M)
               sph(L+1)%element(z,N) = sph(L+1)%element(z,N)+(top/sqrt(bot))*sph(L)%element(i,std(M))
            end do
         end do
         if (L>=1) then

         ! Second "r2" term of (6.4.73). Must have L >= 1.
            nc = n_comp(L-1)
            lm2 = n_comp_up_to(L-2)  ! This works for L=1

         ! Std order for M = [-L+1, L-1]
            call create_(stm,-L+1,L-1)  ! Std order for M = [ L  , L  ]
            stm = into_std_S_order_(L-1)
            do i = 1,nc  ! loop over cartesian gaussians in (L-1)-shell
               pow = pp(:,lm2+i)
               ix = pow+[2,0,0]+1
               iy = pow+[0,2,0]+1
               iz = pow+[0,0,2]+1

            ! Subtract lm0 for start of L+1 shell
               x = index_of(ix(1),ix(2),ix(3))-lm0  ! multiply by x^2
               y = index_of(iy(1),iy(2),iy(3))-lm0  ! multiply by y^2
               z = index_of(iz(1),iz(2),iz(3))-lm0  ! multiply by y^2
               do M = -L+1,L-1
                  bot = (L+M+1)*(L-M+1)
                  val = sqrt((L+M)*(L-M)/bot)*sph(L-1)%element(i,stm(M))
                  N = stp(M)
                  sph(L+1)%element(x,N) = sph(L+1)%element(x,N)-val
                  sph(L+1)%element(y,N) = sph(L+1)%element(y,N)-val
                  sph(L+1)%element(z,N) = sph(L+1)%element(z,N)-val
               end do
            end do
            call destroy_(std)
         end if
         call destroy_(stp)
         call destroy_(std)
      end do
   end subroutine


! Molden & Gaussian cartesian gaussian basis function order

   pure function into_std_S_order(L) result (res)
   ! This generates the Molden/Gaussian order corresponding to the
   ! canonical tonto order. For *gathering* molden -> tonto arrays.
   ! i.e. tonto(:) = molden(res(:)) e.g. for L=2 and L=3 :
   ! NOTE: Tonto now uses Molden/Gaussian/Horton order.
   !
   ! Tonto : -1, 0, 1 
   ! Molden:  0, 1,-1 
   ! res   :  3, 1, 2
   !
   ! Tonto : -2,-1, 0, 1, 2 
   ! Molden:  0, 1,-1, 2,-2 
   ! res   :  5, 3, 1, 2, 4
   !
   ! Tonto : -3,-2,-1, 0, 1, 2, 3 
   ! Molden:  0, 1,-1, 2,-2, 3,-3 
   ! Horton:  0, c, s, c, s, c, s
   ! res   :  7, 5, 3, 1, 2, 4, 6
   !
   ! i.e. molden(res(:)) = tonto(:)
   !
   ! NOTE: M>0 corresponds to Re [r^L Y_(L M)] or "cos like"
   !       M<0 corresponds to Im [r^L Y_(L M)] or "sin like"
      INT, IN :: L
      VEC(INT,2*L+1) :: res
      VEC(INT,:), allocatable :: mold,tont
      INT :: L2,n,s,i
      L2 = 2*L
      select case (L)

      case (0)
         res = [1]

      case (1)
         if (NOT CRYSTAL23_ordering) then
            res = [3,1,2]  ! Pz, Px, Py
         else
            res = [2,3,1]  ! Px, Py, Pz
         end if

      case default
         allocate(mold(L2+1))
         allocate(tont(L2+1))

         ! Standard order mold
         n = 1
         mold(n) = 0
         do i = 1,L
            do s = 1,-1,-2
               n = n+1
               mold(n) = s*i
            end do
         end do

         ! Tonto starts from +1 not -L
         n = 0
         do i = -L,L
            n = n+1
            tont(n) = i
         end do

         ! This method from Horton
         do n = 1,L2+1
            res(n) = findloc(mold,value=tont(n),dim=1)
         end do
      end select
   end function


!  ========================================================
!  Gaussian-program cartesian gaussian basis function order
!  ========================================================

   PURE subroutine swap_from_Gaussian_C_order(X,ind,fbf,lbf,ang)
   ! Swap the order of F and G cartesian gaussian basis functions in
   ! matrix "X" from the Gaussian order into a Tonto order. "ind"
   ! is either "row" or "1", or "coloumn" or "2" if the first or
   ! second indices are to be reordered. "fbf" and "lbf" are the first
   ! and last basis function indices, and "ang" the angular momentum,
   ! for each cartesian L-shell.
   ! i.e. X(tonto) = swap_from_Gaussian_C_order(X(gaussian) ...))
      MAT(REAL,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: fbf
      VEC(INT,:), IN :: lbf
      VEC(INT,:), IN :: ang
      INT :: s,Ls,f,l
   ! fac :: VEC{REAL}@
      VEC(INT,10) :: Fo
      VEC(INT,15) :: Go
   ENSURE(all(ang<=4),"GAUSSIAN_DATA:swap_from_Gaussian_C_order ... Cannot handle higher than G functions")

      ! Tonto order:    xxx yyy zzz xxy xxz xyy yyz xzz yzz xyz
      ! Gaussian order: xxx yyy zzz xyy xxy xxz xzz yzz yyz xyz
      ! Yes, this is indeed the same as Molden ordering
      Fo = [1,2,3,5,6,4,9,7,8,10]

      ! Tonto order:    
      ! xxxx yyyy zzzz xxxy xxxz xyyy yyyz xzzz yzzz xxyy xxzz yyzz xxyz xyyz xyzz
      ! Gaussian order: 
      ! zzzz yzzz yyzz yyyz yyyy xzzz xyzz xyyz xyyy xxzz xxyz xxyy xxxz xxxy xxxx
      ! This agrees with the HORTON2 convention in IOData
      Go = [15,5,1,14,13,9,4,6,2,12,10,3,11,8,7]
      select case (ind)

      case ("row","1")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_from_Gaussian_C_shell_L_(X(f:l,:),Ls,Fo,Go)
            call unnormalize_block_L_(X(f:l,:),Ls)
         end do

      case ("column","2")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_from_Gaussian_C_shell_R_(X(:,f:l),Ls,Fo,Go)
            call unnormalize_block_R_(X(:,f:l),Ls)
         end do
      end select
   end subroutine

   pure subroutine swap_from_Gaussian_C_shell_L(B,Ls,Fo,Go)
   ! Swap the left-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Gaussian order
   ! into a Tonto order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      VEC(INT,15), IN :: Go
      select case (Ls)
      case (3); B = B(Fo,:)
      case (4); B = B(Go,:)
      end select
   end subroutine

   pure subroutine swap_from_Gaussian_C_shell_R(B,Ls,Fo,Go)
   ! Swap the left-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Gaussian order
   ! into a Tonto order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      VEC(INT,15), IN :: Go
      select case (Ls)
      case (3); B = B(:,Fo)
      case (4); B = B(:,Go)
      end select
   end subroutine

   PURE subroutine swap_into_Gaussian_C_order(X,ind,fbf,lbf,ang)
   ! Swap the order of F and G cartesian gaussian basis functions in
   ! matrix "X" FROM the Tonto order INTO a Tonto order. "ind"
   ! is either "row" or "1", or "coloumn" or "2" if the first or
   ! second indices are to be reordered. "fbf" and "lbf" are the first
   ! and last basis function indices, and "ang" the angular momentum,
   ! for each cartesian L-shell.
   ! i.e. X(Gaussian) = swap_from_Tonto_C_order(X(Tonto) ...))
      MAT(REAL,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: fbf
      VEC(INT,:), IN :: lbf
      VEC(INT,:), IN :: ang
      INT :: s,Ls,f,l
      VEC(INT,10) :: Fo
      VEC(INT,15) :: Go
   ENSURE(all(ang<=4),"GAUSSIAN_DATA:swap_into_Gaussian_C_order ... Cannot handle higher than G functions")

      ! Tonto order:    xxx yyy zzz xxy xxz xyy yyz xzz yzz xyz
      ! Gaussian order: xxx yyy zzz xyy xxy xxz xzz yzz yyz xyz
      ! (Yes, this is indeed the same as Molden ordering)
      Fo = [1,2,3,5,6,4,9,7,8,10]

      ! Tonto order:    
      ! xxxx yyyy zzzz xxxy xxxz xyyy yyyz xzzz yzzz 
      ! xxyy xxzz yyzz xxyz xyyz xyzz
      ! Gaussian order: 
      Go = [15,5,1,14,13,9,4,6,2,12,10,3,11,8,7]
      select case (ind)

      case ("row","l","1")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_into_Gaussian_C_shell_L_(X(f:l,:),Ls,Fo,Go)
            call renormalize_block_L_(X(f:l,:),Ls)
         end do

      case ("column","r","2")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_into_Gaussian_C_shell_R_(X(:,f:l),Ls,Fo,Go)
            call renormalize_block_R_(X(:,f:l),Ls)
         end do
      end select
   end subroutine

   pure subroutine swap_into_Gaussian_C_shell_L(B,Ls,Fo,Go)
   ! Swap the left-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Tonto order
   ! into a Gaussian order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      VEC(INT,15), IN :: Go
      select case (Ls)
      case (3); B(Fo,:) = B
      case (4); B(Go,:) = B
      end select
   end subroutine

   pure subroutine swap_into_Gaussian_C_shell_R(B,Ls,Fo,Go)
   ! Swap the right-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Tonto order
   ! into a Gaussian order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      VEC(INT,15), IN :: Go
      select case (Ls)
      case (3); B(:,Fo) = B
      case (4); B(:,Go) = B
      end select
   end subroutine


!  ==============================================
!  Molden cartesian gaussian basis function order
!  ==============================================

   PURE subroutine swap_from_Molden_C_order(X,ind,fbf,lbf,ang)
   ! Swap the order of F and G cartesian gaussian basis functions in
   ! matrix "X" from the Molden order into a Tonto order. "ind"
   ! is either "row" or "1", or "coloumn" or "2" if the first or
   ! second indices are to be reordered. "fbf" and "lbf" are the first
   ! and last basis function indices, and "ang" the angular momentum,
   ! for each cartesian L-shell.
      MAT(REAL,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: fbf
      VEC(INT,:), IN :: lbf
      VEC(INT,:), IN :: ang
      INT :: s,Ls,f,l
      VEC(INT,10) :: Fo
   ENSURE(all(ang<=4),"GAUSSIAN_DATA:swap_from_Molden_C_order ... Cannot handle higher than G functions")

      ! Tonto order:  xxx yyy zzz xxy xxz xyy yyz xzz yzz xyz
      ! Molden order: xxx yyy zzz xyy xxy xxz xzz yzz yyz xyz
      ! Yes, this is indeed the same as Gaussian ordering
      Fo = [1,2,3,5,6,4,9,7,8,10]
      select case (ind)

      case ("row","1")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_from_Molden_C_shell_L_(X(f:l,:),Ls,Fo)
            call unnormalize_block_L_(X(f:l,:),Ls)
         end do

      case ("column","2")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_from_Molden_C_shell_R_(X(:,f:l),Ls,Fo)
            call unnormalize_block_R_(X(:,f:l),Ls)
         end do
      end select
   end subroutine

   pure subroutine swap_from_Molden_C_shell_L(B,Ls,Fo)
   ! Swap the left-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Molden order
   ! into a Tonto order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      select case (Ls)
      case (3); B = B(Fo,:)
      end select
   end subroutine

   pure subroutine swap_from_Molden_C_shell_R(B,Ls,Fo)
   ! Swap the left-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Molden order
   ! into a Tonto order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      select case (Ls)
      case (3); B = B(:,Fo)
      end select
   end subroutine

   PURE subroutine swap_into_Molden_C_order(X,ind,fbf,lbf,ang)
   ! Swap the order of F and G cartesian gaussian basis functions in
   ! matrix "X" FROM the Tonto order INTO a Tonto order. "ind"
   ! is either "row" or "1", or "coloumn" or "2" if the first or
   ! second indices are to be reordered. "fbf" and "lbf" are the first
   ! and last basis function indices, and "ang" the angular momentum,
   ! for each cartesian L-shell.
      MAT(REAL,:,:), INOUT :: X
      STR(len=*), IN :: ind
      VEC(INT,:), IN :: fbf
      VEC(INT,:), IN :: lbf
      VEC(INT,:), IN :: ang
      INT :: s,Ls,f,l
      VEC(INT,10) :: Fo
   ENSURE(all(ang<=4),"GAUSSIAN_DATA:swap_into_Molden_C_order ... Cannot handle higher than G functions")

      ! Corresponding F function order
      ! G function order requires no change!
      Fo = [1,2,3,5,6,4,9,7,8,10]
      select case (ind)

      case ("row","l","1")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_into_Molden_C_shell_L_(X(f:l,:),Ls,Fo)
            call renormalize_block_L_(X(f:l,:),Ls)
         end do

      case ("column","r","2")
         do s = 1,size(fbf)
            f = fbf(s)
            l = lbf(s)
            Ls = ang(s)
            call swap_into_Molden_C_shell_R_(X(:,f:l),Ls,Fo)
            call renormalize_block_R_(X(:,f:l),Ls)
         end do
      end select
   end subroutine

   pure subroutine swap_into_Molden_C_shell_L(B,Ls,Fo)
   ! Swap the left-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Tonto order
   ! into a Molden order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      select case (Ls)
      case (3); B(Fo,:) = B
      end select
   end subroutine

   pure subroutine swap_into_Molden_C_shell_R(B,Ls,Fo)
   ! Swap the right-index order of F and G cartesian gaussian basis
   ! functions in matrix element block "B" from the Tonto order
   ! into a Molden order. 
      MAT(REAL,:,:), INOUT :: B
      INT, IN :: Ls
      VEC(INT,10), IN :: Fo
      select case (Ls)
      case (3); B(:,Fo) = B
      end select
   end subroutine


! ==================================================
! Cartesian-product rotation representation matrices
! ==================================================

   PURE function d_xyz_rep_matrices_for(R,is_spherical) result (res)
   ! Return the representation matrices for d xyz products found in
   ! gaussian shells from the p xyz matrices.
      MAT3(REAL,:,:,:), IN :: R
      BIN, IN :: is_spherical
      MAT3(REAL,:,:,:), allocatable :: res
      INT :: n,order
      MAT(REAL,:,:), allocatable :: dtr
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:d_xyz_rep_matrices_for ... wrong 1st dimension, self")
   ENSURE(size(R,2)==3,"GAUSSIAN_DATA:d_xyz_rep_matrices_for ... wrong 2nd dimension, self")
   ENSURE(size(R,3)>0,"GAUSSIAN_DATA:d_xyz_rep_matrices_for ... no p type matrices")
      if (l_max_saved<2) return
      order = size(R,3)
      if (NOT is_spherical) then
         call create_(res,6,6,order)
         do n = 1,order
            res(:,:,n) = d_xyz_rep_mx_for_(R(:,:,n))
         end do
      else
         call create_(res,5,5,order)
         call create_(dtr,6,6)
         do n = 1,order
            dtr = d_xyz_rep_mx_for_(R(:,:,n))
            call to_spherical_rep_mx(res(:,:,n),dtr,2)
         end do
         call destroy_(dtr)
      end if
   end function

   PURE subroutine to_spherical_rep_mx(res,ctr,l)
   ! Set "res" to the representation matrix for spherical shells of angular
   ! momentum "l", from the matrix "ctr" for the cartesian shells:
   !    res = (U^T U)^-1 U^T ctr U,  U = spherical_harmonics_for(l).
   ! U^T U is not the unit matrix, so U^T ctr U alone is not a rotation.
      MAT(REAL,:,:), OUT :: res
      MAT(REAL,:,:), IN :: ctr
      INT, IN :: l
      MAT(REAL,:,:), allocatable :: M,W
      INT :: n
   ENSURE(is_square_(res),"GAUSSIAN_DATA:to_spherical_rep_mx ... res not square")
   ENSURE(size(res,1)==2*l+1,"GAUSSIAN_DATA:to_spherical_rep_mx ... wrong size, res")
      n = 2*l+1
      call create_(M,n,n)
      call to_product_of_(M,spherical_harmonics_for(l)%element(:,:),spherical_harmonics_for(l)%element(:,:),transpose_a=TRUE)
      call to_pseudo_inverse_of_(M,M)
      call create_(W,n,n)
      call change_basis_to_(ctr,W,spherical_harmonics_for(l)%element(:,:))
      call to_product_of_(res,M,W)
      call destroy_(W)
      call destroy_(M)
   end subroutine

   PURE function f_xyz_rep_matrices_for(R,is_spherical) result (res)
   ! Return the representation matrices for f xyz products found in
   ! gaussian shells from the p xyz matrices
      MAT3(REAL,:,:,:), IN :: R
      BIN, IN :: is_spherical
      MAT3(REAL,:,:,:), allocatable :: res
      INT :: n,order
      MAT(REAL,:,:), allocatable :: ftr
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:f_xyz_rep_matrices_for ... wrong 1st dimension, self")
   ENSURE(size(R,2)==3,"GAUSSIAN_DATA:f_xyz_rep_matrices_for ... wrong 2nd dimension, self")
   ENSURE(size(R,3)>0,"GAUSSIAN_DATA:f_xyz_rep_matrices_for ... no p type matrices")
      if (l_max_saved<3) return
      order = size(R,3)
      if (NOT is_spherical) then
         call create_(res,10,10,order)
         do n = 1,order
            res(:,:,n) = f_xyz_rep_mx_for_(R(:,:,n))
         end do
      else
         call create_(res,7,7,order)
         call create_(ftr,10,10)
         do n = 1,order
            ftr = f_xyz_rep_mx_for_(R(:,:,n))
            call to_spherical_rep_mx(res(:,:,n),ftr,3)
         end do
         call destroy_(ftr)
      end if
   end function

   PURE function g_xyz_rep_matrices_for(R,is_spherical) result (res)
   ! Return the representation matrices for g xyz products found in
   ! gaussian shells from the p xyz matrices
      MAT3(REAL,:,:,:), IN :: R
      BIN, IN :: is_spherical
      MAT3(REAL,:,:,:), allocatable :: res
      INT :: n,order
      MAT(REAL,:,:), allocatable :: gtr
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:g_xyz_rep_matrices_for ... wrong 1st dimension, self")
   ENSURE(size(R,2)==3,"GAUSSIAN_DATA:g_xyz_rep_matrices_for ... wrong 2nd dimension, self")
   ENSURE(size(R,3)>0,"GAUSSIAN_DATA:g_xyz_rep_matrices_for ... no p type matrices")
      order = size(R,3)
      if (l_max_saved<4) return
      if (NOT is_spherical) then
         call create_(res,15,15,order)
         do n = 1,order
            res(:,:,n) = g_xyz_rep_mx_for_(R(:,:,n))
         end do
      else
         call create_(res,9,9,order)
         call create_(gtr,15,15)
         do n = 1,order
            gtr = g_xyz_rep_mx_for_(R(:,:,n))
            call to_spherical_rep_mx(res(:,:,n),gtr,4)
         end do
         call destroy_(gtr)
      end if
   end function

   PURE subroutine make_gaussian_xyz_matrices(tr,R)
   ! Return representation matrices for the s, p, d, f, and g (l = 0
   ! ... 4) xyz cartesian gaussian shell components from a single p
   ! xyz representation matrix "ptr".
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: tr
      MAT(REAL,:,:), IN :: R
   ENSURE(is_square_(R),"GAUSSIAN_DATA:make_gaussian_xyz_matrices ... ptr matrix is not square")
   ENSURE(size(R,2)==3,"GAUSSIAN_DATA:make_gaussian_xyz_matrices ... wrong 1st dimension, self")
      call create_(tr,0,4)
      call create_(tr(0)%element,1,1)
      call create_(tr(1)%element,3,3)
      call create_(tr(2)%element,6,6)
      call create_(tr(3)%element,10,10)
      call create_(tr(4)%element,15,15)

      ! Now assign the transformation matrices
      tr(0)%element = ONE
      tr(1)%element = R
      tr(2)%element = d_xyz_rep_mx_for(R)
      tr(3)%element = f_xyz_rep_mx_for(R)
      tr(4)%element = g_xyz_rep_mx_for(R)
   end subroutine

   PURE function d_xyz_rep_mx_for(R) result (dtr)
   ! Return the representation matrix for a d-type xyz product found
   ! in gaussian shells, induced by an xyz rotation matrix "R". 
   ! The matrix representation induced is: d = d' * dtr, when the
   ! coordinates r of the gaussian shell functions are written in
   ! terms of new coordinates r' as: r = R^T * r'.
   ! ALTERNATIVELY:
   ! If R is used to *change basis* of symmetric matrix D -> D':
   !        D' = R^T * D * R    .... (1)
   ! and if Dv is the row composed of the lower triangle of D in the
   ! order given by "d1" and "d2" below, then this routine returns the
   ! 6 x 6 tensor product matrix "dtr" which performs:
   !       D'v = Dv * dtr       .... (2)
   ! That is, dtr *post multiplies* the row Dv. You can see this is
   ! correct because the (same) off-diagonal elements of D in (1)
   ! contribute twice to every element of D', hence D'v also. You can
   ! see these two contributions in the line below. 
   ! NOTE: If you want the rep matrix which *pre multiplies* a
   ! *column* Dv you need to transpose "dtr"; or else, supply R^T instead
   ! of R to this routine.
      MAT(REAL,:,:), IN :: R
      MAT(REAL,6,6) :: dtr
      INT :: i,i1,i2
      VEC(INT,6) :: d1,d2
   ENSURE(is_square_(R),"GAUSSIAN_DATA:d_xyz_rep_mx_for ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:d_xyz_rep_mx_for ... wrong size, R")

   ! sqrt3  = sqrt(THREE)
      d1 = [1,2,3,1,1,2]
      d2 = [1,2,3,2,3,3]
      do i = 1,6  ! loop on old coordinates
         i1 = d1(i)
         i2 = d2(i)
         dtr(1,i) = R(1,i1)*R(1,i2)
         dtr(2,i) = R(2,i1)*R(2,i2)
         dtr(3,i) = R(3,i1)*R(3,i2)
         dtr(4,i) = R(1,i1)*R(2,i2)+R(2,i1)*R(1,i2)
         dtr(5,i) = R(1,i1)*R(3,i2)+R(3,i1)*R(1,i2)
         dtr(6,i) = R(2,i1)*R(3,i2)+R(3,i1)*R(2,i2)
      end do

   ! dtr(1:6,4:6) = dtr(1:6,4:6)*sqrt3 ! Put in correct normalization for old primitives
   ! dtr(4:6,1:6) = dtr(4:6,1:6)/sqrt3 ! Put in wrong   normalization for new primitives
   end function

   PURE function f_xyz_rep_mx_for(R) result (ftr)
   ! Return the representation matrix for an f xyz product found in
   ! gaussian shells from a p-type xyz matrix. See comments above.
      MAT(REAL,:,:), IN :: R
      MAT(REAL,10,10) :: ftr
      INT :: i,i1,i2,i3
      VEC(INT,10) :: f1,f2,f3
   ENSURE(is_square_(R),"GAUSSIAN_DATA:f_xyz_rep_mx_for ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:f_xyz_rep_mx_for ... wrong size, R")

   ! sqrt5  = sqrt(FIVE)
   ! sqrt15 = sqrt(15d0)
      f1 = [1,2,3,1,1,2,2,3,3,1]
      f2 = [1,2,3,1,1,2,2,3,3,2]
      f3 = [1,2,3,2,3,1,3,1,2,3]
      do i = 1,10
         i1 = f1(i)
         i2 = f2(i)
         i3 = f3(i)
         ftr(1,i) = R(1,i1)*R(1,i2)*R(1,i3)
         ftr(2,i) = R(2,i1)*R(2,i2)*R(2,i3)
         ftr(3,i) = R(3,i1)*R(3,i2)*R(3,i3)
         ftr(4,i) = R(1,i1)*R(1,i2)*R(2,i3)+R(1,i1)*R(2,i2)*R(1,i3)+R(2,i1)*R(1,i2)*R(1,i3)
         ftr(5,i) = R(1,i1)*R(1,i2)*R(3,i3)+R(1,i1)*R(3,i2)*R(1,i3)+R(3,i1)*R(1,i2)*R(1,i3)
         ftr(6,i) = R(1,i1)*R(2,i2)*R(2,i3)+R(2,i1)*R(1,i2)*R(2,i3)+R(2,i1)*R(2,i2)*R(1,i3)
         ftr(7,i) = R(3,i1)*R(2,i2)*R(2,i3)+R(2,i1)*R(3,i2)*R(2,i3)+R(2,i1)*R(2,i2)*R(3,i3)
         ftr(8,i) = R(1,i1)*R(3,i2)*R(3,i3)+R(3,i1)*R(1,i2)*R(3,i3)+R(3,i1)*R(3,i2)*R(1,i3)
         ftr(9,i) = R(2,i1)*R(3,i2)*R(3,i3)+R(3,i1)*R(2,i2)*R(3,i3)+R(3,i1)*R(3,i2)*R(2,i3)
         ftr(10,i) = R(1,i1)*R(2,i2)*R(3,i3)+R(1,i1)*R(3,i2)*R(2,i3)+R(2,i1)*R(1,i2)*R(3,i3)+R(2,i1)*R(3,i2)*R(1,i3)+R(3,i1)*R(1,i2)*R(2,i3)+R(3,i1)*R(2,i2)*R(1,i3)
      end do

   ! ftr(1:10, 4:9 ) = ftr(1:10, 4:9 )*sqrt5
   ! ftr(1:10,10:10) = ftr(1:10,10:10)*sqrt15
   ! ftr(4:9 , 1:10) = ftr(4:9 , 1:10)/sqrt5
   ! ftr(10:10,1:10) = ftr(10:10,1:10)/sqrt15
   end function

   PURE function g_xyz_rep_mx_for(R) result (gtr)
   ! Return the representation matrix for a g xyz product found in
   ! gaussian shells from a p-type xyz matrix. See commenst above.
      MAT(REAL,:,:), IN :: R
      MAT(REAL,15,15) :: gtr
      INT :: i,i1,i2,i3,i4
      VEC(INT,15) :: g1,g2,g3,g4
   ENSURE(is_square_(R),"GAUSSIAN_DATA:g_xyz_rep_mx_for ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:g_xyz_rep_mx_for ... wrong size, R")

   ! sqrt7  = sqrt(SEVEN)
   ! sqrt35 = sqrt(35d0)         ! = sqrt(35)
   ! sqrt353= sqrt35/sqrt(THREE) ! = sqrt(35)/sqrt(3)
      g1 = [1,2,3,1,1,2,2,3,3,1,1,2,1,2,3]
      g2 = [1,2,3,1,1,2,2,3,3,1,1,2,1,2,3]
      g3 = [1,2,3,1,1,2,2,3,3,2,3,3,2,1,1]
      g4 = [1,2,3,2,3,1,3,1,2,2,3,3,3,3,2]
      do i = 1,15
         i1 = g1(i)
         i2 = g2(i)
         i3 = g3(i)
         i4 = g4(i)
         gtr(1,i) = R(1,i1)*R(1,i2)*R(1,i3)*R(1,i4)
         gtr(2,i) = R(2,i1)*R(2,i2)*R(2,i3)*R(2,i4)
         gtr(3,i) = R(3,i1)*R(3,i2)*R(3,i3)*R(3,i4)
         gtr(4,i) = R(1,i1)*R(1,i2)*R(1,i3)*R(2,i4)+R(1,i1)*R(1,i2)*R(2,i3)*R(1,i4)+R(1,i1)*R(2,i2)*R(1,i3)*R(1,i4)+R(2,i1)*R(1,i2)*R(1,i3)*R(1,i4)
         gtr(5,i) = R(1,i1)*R(1,i2)*R(1,i3)*R(3,i4)+R(1,i1)*R(1,i2)*R(3,i3)*R(1,i4)+R(1,i1)*R(3,i2)*R(1,i3)*R(1,i4)+R(3,i1)*R(1,i2)*R(1,i3)*R(1,i4)
         gtr(6,i) = R(1,i1)*R(2,i2)*R(2,i3)*R(2,i4)+R(2,i1)*R(1,i2)*R(2,i3)*R(2,i4)+R(2,i1)*R(2,i2)*R(1,i3)*R(2,i4)+R(2,i1)*R(2,i2)*R(2,i3)*R(1,i4)
         gtr(7,i) = R(3,i1)*R(2,i2)*R(2,i3)*R(2,i4)+R(2,i1)*R(3,i2)*R(2,i3)*R(2,i4)+R(2,i1)*R(2,i2)*R(3,i3)*R(2,i4)+R(2,i1)*R(2,i2)*R(2,i3)*R(3,i4)
         gtr(8,i) = R(1,i1)*R(3,i2)*R(3,i3)*R(3,i4)+R(3,i1)*R(1,i2)*R(3,i3)*R(3,i4)+R(3,i1)*R(3,i2)*R(1,i3)*R(3,i4)+R(3,i1)*R(3,i2)*R(3,i3)*R(1,i4)
         gtr(9,i) = R(2,i1)*R(3,i2)*R(3,i3)*R(3,i4)+R(3,i1)*R(2,i2)*R(3,i3)*R(3,i4)+R(3,i1)*R(3,i2)*R(2,i3)*R(3,i4)+R(3,i1)*R(3,i2)*R(3,i3)*R(2,i4)
         gtr(10,i) = R(1,i1)*R(1,i2)*R(2,i3)*R(2,i4)+R(1,i1)*R(2,i2)*R(1,i3)*R(2,i4)+R(1,i1)*R(2,i2)*R(2,i3)*R(1,i4)+R(2,i1)*R(1,i2)*R(1,i3)*R(2,i4)+R(2,i1)*R(1,i2)*R(2,i3)*R(1,i4)+R(2,i1)*R(2,i2)*R(1,i3)*R(1,i4)
         gtr(11,i) = R(1,i1)*R(1,i2)*R(3,i3)*R(3,i4)+R(1,i1)*R(3,i2)*R(1,i3)*R(3,i4)+R(1,i1)*R(3,i2)*R(3,i3)*R(1,i4)+R(3,i1)*R(1,i2)*R(1,i3)*R(3,i4)+R(3,i1)*R(1,i2)*R(3,i3)*R(1,i4)+R(3,i1)*R(3,i2)*R(1,i3)*R(1,i4)
         gtr(12,i) = R(2,i1)*R(2,i2)*R(3,i3)*R(3,i4)+R(2,i1)*R(3,i2)*R(2,i3)*R(3,i4)+R(2,i1)*R(3,i2)*R(3,i3)*R(2,i4)+R(3,i1)*R(2,i2)*R(2,i3)*R(3,i4)+R(3,i1)*R(2,i2)*R(3,i3)*R(2,i4)+R(3,i1)*R(3,i2)*R(2,i3)*R(2,i4)
         gtr(13,i) = R(1,i1)*R(1,i2)*R(2,i3)*R(3,i4)+R(1,i1)*R(1,i2)*R(3,i3)*R(2,i4)+R(1,i1)*R(2,i2)*R(1,i3)*R(3,i4)+R(1,i1)*R(2,i2)*R(3,i3)*R(1,i4)+R(1,i1)*R(3,i2)*R(1,i3)*R(2,i4)+R(1,i1)*R(3,i2)*R(2,i3)*R(1,i4)+R(2,i1)*R(1,i2)*R(1,i3)*R(3,i4)+R(2,i1)*R(1,i2)*R(3,i3)*R(1,i4)+R(2,i1)*R(3,i2)*R(1,i3)*R(1,i4)+R(3,i1)*R(1,i2)*R(1,i3)*R(2,i4)+R(3,i1)*R(1,i2)*R(2,i3)*R(1,i4)+R(3,i1)*R(2,i2)*R(1,i3)*R(1,i4)
         gtr(14,i) = R(2,i1)*R(2,i2)*R(1,i3)*R(3,i4)+R(2,i1)*R(2,i2)*R(3,i3)*R(1,i4)+R(2,i1)*R(1,i2)*R(2,i3)*R(3,i4)+R(2,i1)*R(1,i2)*R(3,i3)*R(2,i4)+R(2,i1)*R(3,i2)*R(2,i3)*R(1,i4)+R(2,i1)*R(3,i2)*R(1,i3)*R(2,i4)+R(1,i1)*R(2,i2)*R(2,i3)*R(3,i4)+R(1,i1)*R(2,i2)*R(3,i3)*R(2,i4)+R(1,i1)*R(3,i2)*R(2,i3)*R(2,i4)+R(3,i1)*R(2,i2)*R(2,i3)*R(1,i4)+R(3,i1)*R(2,i2)*R(1,i3)*R(2,i4)+R(3,i1)*R(1,i2)*R(2,i3)*R(2,i4)
         gtr(15,i) = R(3,i1)*R(3,i2)*R(1,i3)*R(2,i4)+R(3,i1)*R(3,i2)*R(2,i3)*R(1,i4)+R(3,i1)*R(1,i2)*R(3,i3)*R(2,i4)+R(3,i1)*R(1,i2)*R(2,i3)*R(3,i4)+R(3,i1)*R(2,i2)*R(3,i3)*R(1,i4)+R(3,i1)*R(2,i2)*R(1,i3)*R(3,i4)+R(1,i1)*R(3,i2)*R(3,i3)*R(2,i4)+R(1,i1)*R(3,i2)*R(2,i3)*R(3,i4)+R(1,i1)*R(2,i2)*R(3,i3)*R(3,i4)+R(2,i1)*R(3,i2)*R(3,i3)*R(1,i4)+R(2,i1)*R(3,i2)*R(1,i3)*R(3,i4)+R(2,i1)*R(1,i2)*R(3,i3)*R(3,i4)
      end do

   ! gtr(1:15, 4:9 ) = gtr(1:15, 4:9 )*sqrt7
   ! gtr(1:15,10:12) = gtr(1:15,10:12)*sqrt353
   ! gtr(1:15,13:15) = gtr(1:15,13:15)*sqrt35
   ! gtr(4:9 , 1:15) = gtr(4:9 , 1:15)/sqrt7
   ! gtr(10:12,1:15) = gtr(10:12,1:15)/sqrt353
   ! gtr(13:15,1:15) = gtr(13:15,1:15)/sqrt35
   end function


! Symmetric tensor product representation matrices

   PURE subroutine symmetric_tensor_2_product_mx(res,R)
   ! If R transforms the symmetric matrix D -> D' as
   !        D' = R * D * R^T    .... (1)
   ! and if Dv is the row composed of the lower triangle of D in the
   ! order given by "l1" and "l2" below, then this routine returns the
   ! 6 x 6 cartesian tensor product matrix "res" which performs:
   !       D'v = res * Dv       .... (2)
   ! That is, res *pre multiplies* the column Dv. You can see this is
   ! correct because the (same) off-diagonal elements of D in (1)
   ! contribute twice to every element of D', hence D'v also. You can
   ! see these two contributions in the line below.
   ! NOTE: (1) is the "back transform" sense, matching
   ! MAT:back_transform_to and MAT:to_tensor_back_transform_of, and it
   ! is what x' = R.x implies for an ADP -- which is why
   ! ATOM:transform_pADP_vector_with passes the same R for both.
   ! NOTE: this is the transpose of d_xyz_rep_mx_for(R^T)
   ! NOTE: MAT:change_basis_using(R) is the OTHER sense, R^T * D * R.
   ! To get the matching 6 x 6 map for that you must pass transpose(R)
   ! here, or use MAT:to_tensor_change_basis_of(R,reorder=TRUE). Getting
   ! this backwards is silent whenever R is symmetric -- in particular
   ! for every orthogonal cell, where the cell matrices are diagonal --
   ! so it will not show up in a tetragonal or orthorhombic test.
      MAT(REAL,6,6), OUT :: res
      MAT(REAL,:,:), IN :: R
      VEC(INT,6) :: l1,l2
      INT :: i,i1,i2
   ENSURE(is_square_(R),"GAUSSIAN_DATA:symmetric_tensor_2_product_mx ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:symmetric_tensor_2_product_mx ... wrong size, R")

      ! Lexical order: read as columns
      l1 = [1,2,3,1,1,2]
      l2 = [1,2,3,2,3,3]
      do i = 1,6
         i1 = l1(i)
         i2 = l2(i)
         res(i,1) = R(i1,1)*R(i2,1)
         res(i,2) = R(i1,2)*R(i2,2)
         res(i,3) = R(i1,3)*R(i2,3)
         res(i,4) = R(i1,1)*R(i2,2)+R(i1,2)*R(i2,1)
         res(i,5) = R(i1,1)*R(i2,3)+R(i1,3)*R(i2,1)
         res(i,6) = R(i1,2)*R(i2,3)+R(i1,3)*R(i2,2)
      end do
   end subroutine

   PURE subroutine symmetric_tensor_3_product_mx(res,R)
   ! Return the third-order cartesian tensor product matrix for the
   ! symmetric lower pyramid. See routine above.
   ! As there, the map returned is for  T' = R ... R  applied on the LEFT
   ! of every index, i.e. exactly MAT3:back_transform_to(new,R), and it
   ! PRE-multiplies the packed column. Verified numerically against the
   ! tensor contraction to 1e-15.
   ! NOTE: MAT3:change_basis_to(new,R) is the OTHER sense, R on the right of
   ! every index. For that, pass transpose(R) here. The two agree only
   ! when R is symmetric -- for a cell matrix, only when the cell is
   ! orthogonal -- so getting it backwards is silent in every test in this
   ! repository except tests/short/quartz_read_CIF_put_CIF.
      MAT(REAL,10,10), OUT :: res
      MAT(REAL,:,:), IN :: R
      VEC(INT,10) :: l1,l2,l3
      INT :: i,i1,i2,i3
   ENSURE(is_square_(R),"GAUSSIAN_DATA:symmetric_tensor_3_product_mx ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:symmetric_tensor_3_product_mx ... wrong size, R")

      ! Lexical order: read as columns
      l1 = [1,2,3,1,1,2,2,3,3,1]
      l2 = [1,2,3,1,1,2,2,3,3,2]
      l3 = [1,2,3,2,3,1,3,1,2,3]
      do i = 1,10
         i1 = l1(i)
         i2 = l2(i)
         i3 = l3(i)
         res(i,1) = R(i1,1)*R(i2,1)*R(i3,1)
         res(i,2) = R(i1,2)*R(i2,2)*R(i3,2)
         res(i,3) = R(i1,3)*R(i2,3)*R(i3,3)
         res(i,4) = R(i1,1)*R(i2,1)*R(i3,2)+R(i1,1)*R(i2,2)*R(i3,1)+R(i1,2)*R(i2,1)*R(i3,1)
         res(i,5) = R(i1,1)*R(i2,1)*R(i3,3)+R(i1,1)*R(i2,3)*R(i3,1)+R(i1,3)*R(i2,1)*R(i3,1)
         res(i,6) = R(i1,1)*R(i2,2)*R(i3,2)+R(i1,2)*R(i2,1)*R(i3,2)+R(i1,2)*R(i2,2)*R(i3,1)
         res(i,7) = R(i1,3)*R(i2,2)*R(i3,2)+R(i1,2)*R(i2,3)*R(i3,2)+R(i1,2)*R(i2,2)*R(i3,3)
         res(i,8) = R(i1,1)*R(i2,3)*R(i3,3)+R(i1,3)*R(i2,1)*R(i3,3)+R(i1,3)*R(i2,3)*R(i3,1)
         res(i,9) = R(i1,2)*R(i2,3)*R(i3,3)+R(i1,3)*R(i2,2)*R(i3,3)+R(i1,3)*R(i2,3)*R(i3,2)
         res(i,10) = R(i1,1)*R(i2,2)*R(i3,3)+R(i1,1)*R(i2,3)*R(i3,2)+R(i1,2)*R(i2,1)*R(i3,3)+R(i1,2)*R(i2,3)*R(i3,1)+R(i1,3)*R(i2,1)*R(i3,2)+R(i1,3)*R(i2,2)*R(i3,1)
      end do
   end subroutine

   PURE subroutine symmetric_tensor_4_product_mx(res,R)
   ! Return the fourth-order cartesian tensor product matrix for the
   ! symmetric lower pyramid. See routine above.
   ! As there, the map returned is for  T' = R ... R  applied on the LEFT
   ! of every index, i.e. exactly MAT4:back_transform_to(new,R), and it
   ! PRE-multiplies the packed column. Verified numerically against the
   ! tensor contraction to 1e-15.
   ! NOTE: MAT4:change_basis_to(new,R) is the OTHER sense, R on the right of
   ! every index. For that, pass transpose(R) here. The two agree only
   ! when R is symmetric -- for a cell matrix, only when the cell is
   ! orthogonal -- so getting it backwards is silent in every test in this
   ! repository except tests/short/quartz_read_CIF_put_CIF.
      MAT(REAL,15,15), OUT :: res
      MAT(REAL,:,:), IN :: R
      VEC(INT,15) :: l1,l2,l3,l4
      INT :: i,i1,i2,i3,i4
   ENSURE(is_square_(R),"GAUSSIAN_DATA:symmetric_tensor_4_product_mx ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:symmetric_tensor_4_product_mx ... wrong size, R")

      ! Lexical order: read as columns
      l1 = [1,2,3,1,1,2,2,3,3,1,1,2,1,2,3]
      l2 = [1,2,3,1,1,2,2,3,3,1,1,2,1,2,3]
      l3 = [1,2,3,1,1,2,2,3,3,2,3,3,2,1,1]
      l4 = [1,2,3,2,3,1,3,1,2,2,3,3,3,3,2]
      do i = 1,15
         i1 = l1(i)
         i2 = l2(i)
         i3 = l3(i)
         i4 = l4(i)
         res(i,1) = R(i1,1)*R(i2,1)*R(i3,1)*R(i4,1)
         res(i,2) = R(i1,2)*R(i2,2)*R(i3,2)*R(i4,2)
         res(i,3) = R(i1,3)*R(i2,3)*R(i3,3)*R(i4,3)
         res(i,4) = R(i1,1)*R(i2,1)*R(i3,1)*R(i4,2)+R(i1,1)*R(i2,1)*R(i3,2)*R(i4,1)+R(i1,1)*R(i2,2)*R(i3,1)*R(i4,1)+R(i1,2)*R(i2,1)*R(i3,1)*R(i4,1)
         res(i,5) = R(i1,1)*R(i2,1)*R(i3,1)*R(i4,3)+R(i1,1)*R(i2,1)*R(i3,3)*R(i4,1)+R(i1,1)*R(i2,3)*R(i3,1)*R(i4,1)+R(i1,3)*R(i2,1)*R(i3,1)*R(i4,1)
         res(i,6) = R(i1,1)*R(i2,2)*R(i3,2)*R(i4,2)+R(i1,2)*R(i2,1)*R(i3,2)*R(i4,2)+R(i1,2)*R(i2,2)*R(i3,1)*R(i4,2)+R(i1,2)*R(i2,2)*R(i3,2)*R(i4,1)
         res(i,7) = R(i1,3)*R(i2,2)*R(i3,2)*R(i4,2)+R(i1,2)*R(i2,3)*R(i3,2)*R(i4,2)+R(i1,2)*R(i2,2)*R(i3,3)*R(i4,2)+R(i1,2)*R(i2,2)*R(i3,2)*R(i4,3)
         res(i,8) = R(i1,1)*R(i2,3)*R(i3,3)*R(i4,3)+R(i1,3)*R(i2,1)*R(i3,3)*R(i4,3)+R(i1,3)*R(i2,3)*R(i3,1)*R(i4,3)+R(i1,3)*R(i2,3)*R(i3,3)*R(i4,1)
         res(i,9) = R(i1,2)*R(i2,3)*R(i3,3)*R(i4,3)+R(i1,3)*R(i2,2)*R(i3,3)*R(i4,3)+R(i1,3)*R(i2,3)*R(i3,2)*R(i4,3)+R(i1,3)*R(i2,3)*R(i3,3)*R(i4,2)
         res(i,10) = R(i1,1)*R(i2,1)*R(i3,2)*R(i4,2)+R(i1,1)*R(i2,2)*R(i3,1)*R(i4,2)+R(i1,1)*R(i2,2)*R(i3,2)*R(i4,1)+R(i1,2)*R(i2,1)*R(i3,1)*R(i4,2)+R(i1,2)*R(i2,1)*R(i3,2)*R(i4,1)+R(i1,2)*R(i2,2)*R(i3,1)*R(i4,1)
         res(i,11) = R(i1,1)*R(i2,1)*R(i3,3)*R(i4,3)+R(i1,1)*R(i2,3)*R(i3,1)*R(i4,3)+R(i1,1)*R(i2,3)*R(i3,3)*R(i4,1)+R(i1,3)*R(i2,1)*R(i3,1)*R(i4,3)+R(i1,3)*R(i2,1)*R(i3,3)*R(i4,1)+R(i1,3)*R(i2,3)*R(i3,1)*R(i4,1)
         res(i,12) = R(i1,2)*R(i2,2)*R(i3,3)*R(i4,3)+R(i1,2)*R(i2,3)*R(i3,2)*R(i4,3)+R(i1,2)*R(i2,3)*R(i3,3)*R(i4,2)+R(i1,3)*R(i2,2)*R(i3,2)*R(i4,3)+R(i1,3)*R(i2,2)*R(i3,3)*R(i4,2)+R(i1,3)*R(i2,3)*R(i3,2)*R(i4,2)
         res(i,13) = R(i1,1)*R(i2,1)*R(i3,2)*R(i4,3)+R(i1,1)*R(i2,1)*R(i3,3)*R(i4,2)+R(i1,1)*R(i2,2)*R(i3,1)*R(i4,3)+R(i1,1)*R(i2,2)*R(i3,3)*R(i4,1)+R(i1,1)*R(i2,3)*R(i3,1)*R(i4,2)+R(i1,1)*R(i2,3)*R(i3,2)*R(i4,1)+R(i1,2)*R(i2,1)*R(i3,1)*R(i4,3)+R(i1,2)*R(i2,1)*R(i3,3)*R(i4,1)+R(i1,2)*R(i2,3)*R(i3,1)*R(i4,1)+R(i1,3)*R(i2,1)*R(i3,1)*R(i4,2)+R(i1,3)*R(i2,1)*R(i3,2)*R(i4,1)+R(i1,3)*R(i2,2)*R(i3,1)*R(i4,1)
         res(i,14) = R(i1,2)*R(i2,2)*R(i3,1)*R(i4,3)+R(i1,2)*R(i2,2)*R(i3,3)*R(i4,1)+R(i1,2)*R(i2,1)*R(i3,2)*R(i4,3)+R(i1,2)*R(i2,1)*R(i3,3)*R(i4,2)+R(i1,2)*R(i2,3)*R(i3,2)*R(i4,1)+R(i1,2)*R(i2,3)*R(i3,1)*R(i4,2)+R(i1,1)*R(i2,2)*R(i3,2)*R(i4,3)+R(i1,1)*R(i2,2)*R(i3,3)*R(i4,2)+R(i1,1)*R(i2,3)*R(i3,2)*R(i4,2)+R(i1,3)*R(i2,2)*R(i3,2)*R(i4,1)+R(i1,3)*R(i2,2)*R(i3,1)*R(i4,2)+R(i1,3)*R(i2,1)*R(i3,2)*R(i4,2)
         res(i,15) = R(i1,3)*R(i2,3)*R(i3,1)*R(i4,2)+R(i1,3)*R(i2,3)*R(i3,2)*R(i4,1)+R(i1,3)*R(i2,1)*R(i3,3)*R(i4,2)+R(i1,3)*R(i2,1)*R(i3,2)*R(i4,3)+R(i1,3)*R(i2,2)*R(i3,3)*R(i4,1)+R(i1,3)*R(i2,2)*R(i3,1)*R(i4,3)+R(i1,1)*R(i2,3)*R(i3,3)*R(i4,2)+R(i1,1)*R(i2,3)*R(i3,2)*R(i4,3)+R(i1,1)*R(i2,2)*R(i3,3)*R(i4,3)+R(i1,2)*R(i2,3)*R(i3,3)*R(i4,1)+R(i1,2)*R(i2,3)*R(i3,1)*R(i4,3)+R(i1,2)*R(i2,1)*R(i3,3)*R(i4,3)
      end do
   end subroutine

   PURE subroutine symmetric_tensor_product_mx(res,R)
   ! This routine returns blocks of the above matrices in a direct
   ! sum, appropriate to the case when parameters are ordered in a
   ! pADP type vector.
      MAT(REAL,:,:), OUT :: res
      MAT(REAL,:,:), IN :: R
      INT :: dim
   ENSURE(is_square_(res),"GAUSSIAN_DATA:symmetric_tensor_product_mx ... res not square")
   ENSURE(is_square_(R),"GAUSSIAN_DATA:symmetric_tensor_product_mx ... R not square")
   ENSURE(size(R,1)==3,"GAUSSIAN_DATA:symmetric_tensor_product_mx ... wrong dim1, R")
   ENSURE(size(res,1)<=34,"GAUSSIAN_DATA:symmetric_tensor_product_mx ... wrong dim2, R")
      dim = size(res,1)
      select case (dim)

      case (3)
         res = ZERO
         res = R

      case (9)
         res = ZERO
         res(1:3,1:3) = R
         call symmetric_tensor_2_product_mx(res(4:9,4:9),R)

      case (19)
         res = ZERO
         res(1:3,1:3) = R
         call symmetric_tensor_2_product_mx(res(4:9,4:9),R)
         call symmetric_tensor_3_product_mx(res(10:19,10:19),R)

      case (34)
         res = ZERO
         res(1:3,1:3) = R
         call symmetric_tensor_2_product_mx(res(4:9,4:9),R)
         call symmetric_tensor_3_product_mx(res(10:19,10:19),R)
         call symmetric_tensor_4_product_mx(res(20:34,20:34),R)
      end select
   end subroutine


! ==================================================
! Reorder lexical order to lower pyramid for tensors
! ==================================================

   PURE subroutine lexical_2_to_lower_pyramid(inp,res)
   ! The gaussian lexical order for L=2 is reordered to the lower
   ! pyramid for a 3x3 tensor. 
   !. Input  order "inp" is the lexical order
   !. Output order "res" is lower pyramid
      VEC(REAL,:), IN :: inp
      VEC(REAL,:), OUT :: res
      INT :: i
      VEC(INT,6) :: old
   ENSURE(size(inp)==6,"GAUSSIAN_DATA:lexical_2_to_lower_pyramid ... wrong size, inp")
   ENSURE(size(res)==size(inp),"GAUSSIAN_DATA:lexical_2_to_lower_pyramid ... res and inp are inconsistent")

      ! Lower pyramid order translator
      old = [1,4,2,5,6,3]
      do i = 1,6
         res(i) = inp(old(i))
      end do
   end subroutine

   PURE subroutine lexical_3_to_lower_pyramid(inp,res)
   ! The gaussian lexical order for L=3 is reordered to the lower
   ! pyramid for a 3x3x3 tensor. 
   ! . Input  order "inp" is the lexical order
   ! . Output order "res" is lower pyramid
   ! Checked by hand 22/06/16
      VEC(REAL,:), IN :: inp
      VEC(REAL,:), OUT :: res
      INT :: i
      VEC(INT,10) :: old
   ENSURE(size(inp)==10,"GAUSSIAN_DATA:lexical_3_to_lower_pyramid ... wrong size, inp")
   ENSURE(size(res)==size(inp),"GAUSSIAN_DATA:lexical_3_to_lower_pyramid ... res and inp are inconsistent")

      ! Lower pyramid order translator
      old = [1,4,6,2,5,10,7,8,9,3]
      do i = 1,10
         res(i) = inp(old(i))
      end do
   end subroutine

   PURE subroutine lexical_4_to_lower_pyramid(inp,res)
   ! The gaussian lexical order for L=4 is reordered to the lower
   ! pyramid for a 3x3x3x3 tensor. 
   ! . Input  order "inp" is the lexical order
   ! . Output order "res" is lower pyramid
   ! Checked by hand 22/06/16
      VEC(REAL,:), IN :: inp
      VEC(REAL,:), OUT :: res
      INT :: i
      VEC(INT,15) :: old
   ENSURE(size(inp)==15,"GAUSSIAN_DATA:lexical_4_to_lower_pyramid ... wrong size, inp")
   ENSURE(size(res)==size(inp),"GAUSSIAN_DATA:lexical_4_to_lower_pyramid ... res and inp are inconsistent")

      ! Lower pyramid order translator
      old = [1,4,10,6,2,5,13,14,7,11,15,12,8,9,3]
      do i = 1,15
         res(i) = inp(old(i))
      end do
   end subroutine

   PURE subroutine lower_pyramid_to_lexical_2(inp,res)
   ! The gaussian lexical order for L=2 is reordered from the lower
   ! pyramid *to* a 3x3 tensor. 
   !. Input  order "res" is the lexical order
   !. Output order "inp" is lower pyramid
      VEC(REAL,:), IN :: inp
      VEC(REAL,:), OUT :: res
      INT :: i
      VEC(INT,6) :: old
   ENSURE(size(inp)==6,"GAUSSIAN_DATA:lower_pyramid_to_lexical_2 ... wrong size, inp")
   ENSURE(size(res)==size(inp),"GAUSSIAN_DATA:lower_pyramid_to_lexical_2 ... res and inp are inconsistent")

      ! Lower pyramid order translator
      old = [1,4,2,5,6,3]
      do i = 1,6
         res(old(i)) = inp(i)
      end do
   end subroutine

   PURE subroutine lower_pyramid_to_lexical_3(inp,res)
   ! The gaussian lexical order for L=3 is reordered from the lower
   ! pyramid *to* a 3x3x3 tensor. 
   !. Input  order "inp" is the lexical order
   !. Output order "res" is lower pyramid
      VEC(REAL,:), IN :: inp
      VEC(REAL,:), OUT :: res
      INT :: i
      VEC(INT,10) :: old
   ENSURE(size(inp)==10,"GAUSSIAN_DATA:lower_pyramid_to_lexical_3 ... wrong size, inp")
   ENSURE(size(res)==size(inp),"GAUSSIAN_DATA:lower_pyramid_to_lexical_3 ... res and inp are inconsistent")

      ! Lower pyramid order translator
      old = [1,4,6,2,5,10,7,8,9,3]
      do i = 1,10
         res(old(i)) = inp(i)
      end do
   end subroutine

   PURE subroutine lower_pyramid_to_lexical_4(inp,res)
   ! The gaussian lexical order for L=4 is reordered from the lower
   ! pyramid *to* a 3x3x3x3 tensor. 
   !. Input  order "res" is the lexical order
   !. Output order "inp" is lower pyramid
      VEC(REAL,:), IN :: inp
      VEC(REAL,:), OUT :: res
      INT :: i
      VEC(INT,15) :: old
   ENSURE(size(inp)==15,"GAUSSIAN_DATA:lower_pyramid_to_lexical_4 ... wrong size, inp")
   ENSURE(size(res)==size(inp),"GAUSSIAN_DATA:lower_pyramid_to_lexical_4 ... res and inp are inconsistent")

      ! Lower pyramid order translator
      old = [1,4,10,6,2,5,13,14,7,11,15,12,8,9,3]
      do i = 1,15
         res(old(i)) = inp(i)
      end do
   end subroutine


! ================
! Gaussian ordering
! ================

   pure function tonto_to_gaussian_spherical_indices(L) result (res)
   ! This generates the gaussian/molden order for e.g. L=3
   ! M =  0, 1,-1, 2,-2, 3,-3 ... from the tonto order
   ! M = -3,-2,-1, 0, 1, 2, 3 ... 
   ! Checked 22/9/2020 by Dylan!
      INT, IN :: L
      VEC(INT,:), allocatable :: res
      INT :: n,M
      allocate(res(2*L+1))

      ! Gaussian BF index which
      ! points to the tonto index
      n = 1
      do M = 0,L

         ! The M=0 case is first
         res(n) = L+M+1
         n = n+1

         ! Add M<0 case immediately after
         if (M>0) then
            res(n) = L-M+1
            n = n+1
         end if
      end do
   end function

   pure function gaussian_to_tonto_spherical_indices(l) result (res)
   ! ??? Is this Kosher
      INT, IN :: l
      VEC(INT,:), allocatable :: res
      INT :: lm,i
      allocate(res(2*l+1))
      lm = 1
      do i = 2*l,-1,-2
         res(lm) = i+1
         lm = lm+1
      end do
      do i = 1,2*l,2
         res(lm) = i+1
         lm = lm+1
      end do
   end function

end module
