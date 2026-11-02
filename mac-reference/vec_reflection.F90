!===================================================================
!
!  VEC{REFLECTION}: a vector of crystal reflection data
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
!===================================================================
module VEC_REFLECTION_MODULE

#  include "macros"

   include "vec{reflection}.use"

   implicit none

   include "vec{reflection}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(type(REFLECTION_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{REFLECTION}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{REFLECTION}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(type(REFLECTION_TYPE),:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{REFLECTION}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(REFLECTION_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{REFLECTION}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine


!  =======================
!  Shrinking and expansion
!  =======================

   PURE subroutine shrink(self,dim,chop_start)
   ! Shrink allocatable "self" to dimension "dim" from the end.
   ! Contents are retained. If "chop_start" is present and TRUE,
   ! "self" is chopped from the start, and the contents from the end
   ! are retained.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(type(REFLECTION_TYPE),:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{REFLECTION}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{REFLECTION}:shrink ... dim too large")

      ! Chop from start or end?

      ! The following code is inherited from VEC{OBJECT}
      chop_end = TRUE
      if (present(chop_start)) chop_end = NOT chop_start

      ! Bounds
      lb = lbound(self,dim=1)
      ub = ubound(self,dim=1)
      call create_(new,lb,lb+dim-1)
      if (chop_end) then
         new(lb:) = self(lb:lb+dim-1)
      else
         new(lb:) = [self(ub-dim+1:ub)]
      end if
      call destroy_(self)
      self = new
   end subroutine

   PURE subroutine expand(self,dim,grow_start)
   ! Expand allocatable "self" to dimension "dim" adding to the end.
   ! If "grow_start" is present and TRUE, then "self" is expanded from
   ! the start, keeping the same lower bound.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: grow_start
      INT :: nc
      VEC(type(REFLECTION_TYPE),:), allocatable :: new
      BIN :: start

      ! Expand from start?

      ! The following code is inherited from VEC{OBJECT}
      start = FALSE
      if (present(grow_start)) start = grow_start
      if (NOT allocated(self)) then
         call create_(self,dim)
      else if (size(self)==0) then
         call destroy_(self)
         call create_(self,dim)
      else
         ENSURE(dim>size(self),"VEC{REFLECTION}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{REFLECTION}:expand ... dim is not +ve")
         call create_(new,dim)
         if (start) then
            nc = dim-size(self)+1
            new(nc:dim) = self
         else
            new(1:size(self)) = self
         end if

         ! Move it
         call move_alloc(new,self)
      end if
   end subroutine

   PURE subroutine append_0(self,v)
   ! Append the contents of vector "v" to allocatable "self".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      VEC(type(REFLECTION_TYPE),:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   PURE subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(REFLECTION_TYPE), IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine


!  ============
!  Set routines
!  ============

   pure subroutine reset_F_exp_F_sigma(self)
   ! Finalize the reflections
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      INT :: n
      do n = 1,size(self)
         call reset_F_exp_F_sigma_(self(n))
      end do
   end subroutine

   pure subroutine reset_I_exp_I_sigma(self)
   ! Finalize the reflections
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      INT :: n
      do n = 1,size(self)
         call reset_I_exp_I_sigma_(self(n))
      end do
   end subroutine


!   set_h1(h1) :: pure
!   ! Set the h1 indices
!      self :: INOUT
!      h1 :: VEC{INT}, IN
!      REFLECTION:set_h1(self,h1)
!   end
!
!   set_h2(h2) :: pure
!   ! Set the h2 indices
!      self :: INOUT
!      h2 :: VEC{INT}, IN
!      REFLECTION:set_h2(self,h2)
!   end
!
!   set_h3(h3) :: pure
!   ! Set the h3 indices
!      self :: INOUT
!      h3 :: VEC{INT}, IN
!      REFLECTION:set_h3(self,h3)
!   end

   pure subroutine set_h(self,h)
   ! Set the h indices
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(INT,:), IN :: h
      call set_h1_(self,h)
   end subroutine

   pure subroutine set_k(self,k)
   ! Set the k indices
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(INT,:), IN :: k
      call set_h2_(self,k)
   end subroutine

   pure subroutine set_l(self,l)
   ! Set the l indices
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(INT,:), IN :: l
      call set_h3_(self,l)
   end subroutine

   pure subroutine set_F_calc(self,F_calc)
   ! Set the calculated structure factors
   ! Also set calculated intensities.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(CPX,:), IN :: F_calc
      INT :: n
      do n = 1,size(self)
         call set_F_calc_(self(n),F_calc(n))
      end do
   end subroutine

   pure subroutine set_I_calc(self,I_calc)
   ! Set the calculated intensities
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: I_calc
      INT :: n
      do n = 1,size(self)
         call set_I_calc_(self(n),I_calc(n))
      end do
   end subroutine

   pure subroutine set_F_pred(self,F_pred)
   ! Set the predicted structure factors
   ! Also set predicted intensities.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: F_pred
      INT :: n
      do n = 1,size(self)
         call set_F_pred_(self(n),F_pred(n))
      end do
   end subroutine

   pure subroutine set_I_pred(self,I_pred)
   ! Set the predicted predicted intensities.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: I_pred
      INT :: n
      do n = 1,size(self)
         call set_I_pred_(self(n),I_pred(n))
      end do
   end subroutine

   pure subroutine set_F_exp(self,F_exp)
   ! Set the experimental structure factors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: F_exp
      INT :: n
      do n = 1,size(self)
         call set_F_exp_(self(n),F_exp(n))
      end do
   end subroutine

   pure subroutine set_I_exp(self,I_exp)
   ! Set the experimental F2 structure factors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: I_exp
      INT :: n
      do n = 1,size(self)
         call set_I_exp_(self(n),I_exp(n))
      end do
   end subroutine

   subroutine set_F_sigma(self,F_sigma)
   ! Set the experimental structure factor errors
   ! NOTE: must call after set_F_exp
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: F_sigma
      INT :: n
      do n = 1,size(self)
         call set_F_sigma_(self(n),F_sigma(n))
      end do
   end subroutine

   subroutine set_I_sigma(self,I_sigma)
   ! Set the experimental F2 structure factor errors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: I_sigma
      INT :: n
      do n = 1,size(self)
         call set_I_sigma_(self(n),I_sigma(n))
      end do
   end subroutine

   pure subroutine set_F_disp(self,F_disp)
   ! Set the anomalous dispersion continuation.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(CPX,:), IN :: F_disp
      INT :: n
      do n = 1,size(self)
         call set_F_disp_(self(n),F_disp(n))
      end do
   end subroutine

   pure subroutine set_F_corr(self,values)
   ! Set .F_corr values to "values".
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: values
      INT :: n
      do n = 1,size(self)
         call set_F_corr_(self(n),values(n))
      end do
   end subroutine

   pure subroutine reset_F_corr_to_F_exp(self)
   ! Reset F_corr values to the original F_exp values.
   ! Leastt do any sanity checks.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      INT :: n
      do n = 1,size(self)
         call reset_F_corr_to_F_exp_(self(n))
      end do
   end subroutine

   subroutine remove_anom_from_F_exp(self,scale_factor)
   ! Remove anomalous dispersion from F_exp to make F_corr
   ! Only used when using the "XD" method.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: scale_factor
      INT :: n
      do n = 1,size(self)
         call remove_anom_from_F_exp_(self(n),scale_factor)
      end do
   end subroutine

   PURE subroutine remove_anom_from_F_calc(self)
   ! Remove anomalous dispersion from F_calc.
   ! Only used when using the "XD" method.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      INT :: n
      do n = 1,size(self)
         call remove_anom_from_F_calc_(self(n))
      end do
   end subroutine

   PURE subroutine set_equivalence_factors(self,spacegroup)
   ! Set the equivalence factors, the number of distinct reflections
   ! which are symmetry equivalent to a particular (hkl) triple, for all
   ! the reflections.
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      type(SPACEGROUP_TYPE), IN :: spacegroup
      VEC(INT,3) :: hkl,new
      INT :: n,s,u
      do n = 1,size(self)
         hkl = Miller_indices_(self,n)
         u = 1
         do s = 2,spacegroup%n_seitz
            new = int(matmul(REALIFY(hkl),spacegroup%seitz(1:3,1:3,s)))
            if (hkl(1)/=new(1) OR hkl(2)/=new(2) OR hkl(3)/=new(3)) u = u+1
         end do
         call set_equivalence_factor_(self(n),u)
      end do
   end subroutine

   PURE subroutine set_d_and_theta(self,cell,lambda)
   ! Called after the reflections are read in, in order to set all the
   ! .d and .theta values, which are used later all over the place ...
   ! NOTE: Make sure the wavelength is correct, else angles are wrong.
   ! NOTE: see Dunitz "X-ray Analysis" (1995) p. 35
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      type(UNIT_CELL_TYPE), IN :: cell
      REAL, IN :: lambda
      VEC(INT,3) :: h
      VEC(REAL,3) :: hv
      REAL :: stl,d,st,th
      INT :: n
      do n = 1,size(self)
         h = Miller_indices_(self,n)
         hv = matmul(cell%reciprocal_mx,h)
         d = ONE/norm_(hv)  ! with HBB 6/2/16
         stl = HALF/d

         ! Take care, numerical accuracy issues. The clamp is for a reflection
         ! a rounding error outside the limiting sphere; a wavelength that is
         ! wholly inconsistent with the data is caught before this, in
         ! DIFFRACTION_DATA.SET:update.
         st = stl*lambda
         if (st>ONE) st = ONE
         if (st<-ONE) st = -ONE
         th = asin(st)
         call set_stl_(self(n),stl)
         call set_theta_(self(n),th)
         call set_d_(self(n),d)
      end do
   end subroutine


!  ================
!  Scaling routines
!  ================

   pure subroutine scale_F_exp_0(self,fac)
   ! Scale the experimental structure factors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_F_exp_(self(n),self(n)%F_exp*fac)
      end do
   end subroutine

   pure subroutine scale_F_exp_1(self,fac)
   ! Scale the experimental structure factors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_F_exp_(self(n),self(n)%F_exp*fac(n))
      end do
   end subroutine

   pure subroutine scale_F_corr_0(self,fac)
   ! Scale the "corrected" experimental structure factors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_F_corr_(self(n),self(n)%F_corr*fac)
      end do
   end subroutine

   pure subroutine scale_F_corr_1(self,fac)
   ! Scale the "corrected" experimental structure factors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_F_corr_(self(n),self(n)%F_corr*fac(n))
      end do
   end subroutine

   pure subroutine scale_F_sigma_0(self,fac)
   ! Scale the experimental structure factor errors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_F_sigma_(self(n),self(n)%F_sigma*fac)
      end do
   end subroutine

   pure subroutine scale_F_sigma_1(self,fac)
   ! Scale the experimental structure factor errors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_F_sigma_(self(n),self(n)%F_sigma*fac(n))
      end do
   end subroutine

   pure subroutine scale_I_exp(self,fac)
   ! Scale the experimental intensities
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_I_exp_(self(n),self(n)%I_exp*fac)
      end do
   end subroutine

   pure subroutine scale_I_sigma(self,fac)
   ! Scale the experimental intensity errors
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: fac
      INT :: n
      do n = 1,size(self)
         call set_I_sigma_(self(n),self(n)%I_sigma*fac)
      end do
   end subroutine


!  ================
!  Inquiry Routines
!  ================

   pure function have_F_calc(self) result (res)
   ! Whether we have any calculated structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%F_calc)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_F_pred(self) result (res)
   ! Whether we have any predicted structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%F_pred)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_nonzero_F_exp(self) result (res)
   ! Whether we have any experimental structure factors.
   ! NOTE: If we have F_exp then we have F_corr also.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%F_exp)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_nonzero_F_sigma(self) result (res)
   ! Whether we have any errors in the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%F_sigma)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_I_calc(self) result (res)
   ! Whether we have any F2
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%I_calc)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_I_pred(self) result (res)
   ! Whether we have any predicted intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%I_pred)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_nonzero_I_exp(self) result (res)
   ! Whether we have any experimental intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      do n = 1,size(self)
         if (abs(self(n)%I_exp)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_nonzero_I_sigma(self) result (res)
   ! Whether we have any errors in the intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (abs(self(n)%I_sigma)>TOL(10)) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function have_indices(self) result (res)
   ! Whether we have the Miller indices
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      INT :: n
      res = FALSE
      do n = 1,size(self)
         if (self(n)%h1/=0 OR self(n)%h2/=0 OR self(n)%h3/=0) then
            res = TRUE
            exit
         end if
      end do
   end function

   pure function Miller_indices(self,n) result (res)
   ! Return the miller indices of reflection n.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      INT, IN :: n
      VEC(INT,3) :: res
      res = [self(n)%h1,self(n)%h2,self(n)%h3]
   end function

   pure function use_multiple_scale_factors(self) result (res)
   ! Return TRUE if we are supposed to use multiple scale factors.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      res = any(self%group>0)
   end function

   pure function d_max(self) result (res)
   ! The maximum d spacing/resolution in au
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      res = maxval(self(:)%d)
   end function

   pure function d_min(self) result (res)
   ! The minimum d spacing/resolution in au
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      res = minval(self(:)%d)
   end function

   pure function max_stl(self,cell) result (res)
   ! The largest sin(theta)/lambda in the list, in au^-1, worked out from the
   ! Miller indices and the reciprocal cell. Unlike .d_min this needs no prior
   ! call to .set_d_and_theta, so it can be used to test a wavelength before
   ! the angles that wavelength implies have been formed.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      type(UNIT_CELL_TYPE), IN :: cell
      REAL :: res
      VEC(INT,3) :: h
      VEC(REAL,3) :: hv
      INT :: n
      res = ZERO
      do n = 1,size(self)
         h = Miller_indices_(self,n)
         hv = matmul(cell%reciprocal_mx,h)
         res = max(res,HALF*norm_(hv))
      end do
   end function

   pure function h_max(self) result (res)
   ! Return the maximum Miller indices
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,3) :: res
      res(1) = maxval(self(:)%h1)
      res(2) = maxval(self(:)%h2)
      res(3) = maxval(self(:)%h3)
   end function

   pure function h_min(self) result (res)
   ! Return the minimum Miller indices
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,3) :: res
      res(1) = minval(self(:)%h1)
      res(2) = minval(self(:)%h2)
      res(3) = minval(self(:)%h3)
   end function

   PURE subroutine get_all_Friedel_pairs(self,mult)
   ! Returns TRUE if the list of reflections is Friedel unmerged.
   ! *Every* reflection must have matching Friedel pair.
   ! "mult" is set to 1 if there is a matching Friedel, 2 otherwise.
   ! It can be used as a scaling factor when calculating residual
   ! or other electron densities.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,:), OUT :: mult
      VEC(INT,3) :: p,q
      INT :: i,j,n_refl
      BIN :: same
   ENSURE(size(mult)==size(self),"VEC{REFLECTION}:get_all_Friedel_pairs ... wrong size, match array")

      ! No. of reflections
      n_refl = size(self)

      ! Loop reflections
      mult = 2
      same = FALSE
      do i = 1,n_refl

         ! Skip those already matched
         if (mult(i)==1) cycle
         p = Miller_indices_(self(i))

         ! Find the matching pair
         same = FALSE
         do j = i+1,n_refl

            ! Skip those already matched
            if (mult(j)==1) cycle
            q = -Miller_indices_(self(j))
            same = same_as_(p,q)
            if (same) then
               mult(i) = 1
               mult(j) = 1
               exit  ! i is paired with j; a later j must find its own
            end if
         end do
      end do
   end subroutine

   PURE function has_all_Friedel_pairs(self) result (res)
   ! Returns TRUE if the list of reflections is Friedel unmerged.
   ! *Every* reflection must have matching Friedel pair.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      VEC(INT,3) :: p,q
      VEC(INT,:), allocatable :: match
      INT :: i,j,n_refl
      BIN :: same

      ! No. of reflections
      n_refl = size(self)

      ! Can't be unmerged if it is not even
      res = is_even_(n_refl)
      if (NOT res) return

      ! Matches
      call create_(match,n_refl)
      match = 0

      ! Loop reflections
      same = FALSE
      do i = 1,n_refl

         ! Skip those already matched
         if (match(i)>0) cycle
         p = Miller_indices_(self(i))

         ! Find the matching pair
         same = FALSE
         do j = i+1,n_refl

            ! Skip those already matched
            if (match(j)>0) cycle
            q = -Miller_indices_(self(j))
            same = same_as_(p,q)
            if (same) then
               match(i) = j
               match(j) = i
            end if
         end do
      end do
      res = all(match>0)
      call destroy_(match)
   end function

   PURE function has_Friedel_pairs(self) result (res)
   ! Returns TRUE if there is at least *one* Friedel pair.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN :: res
      VEC(INT,3) :: p,q
      INT :: i,j,n_refl
      BIN :: same

      ! No. of reflections
      n_refl = size(self)

      ! Loop reflections
      same = FALSE
      refl: do i = 1,n_refl
         p = Miller_indices_(self(i))
         same = FALSE
         do j = i+1,n_refl
            q = -Miller_indices_(self(j))
            same = same_as_(p,q)
            ! Is Friedel?
            if (same) exit refl
         end do
      end do refl
      res = same
   end function

   PURE function n_unmatched_Friedel_pairs(self) result (res)
   ! This is also one way to test if the reflections are
   ! Friedel-merged ....
      VEC(type(REFLECTION_TYPE),:), IN :: self
      INT :: res
      VEC(INT,:), allocatable :: mult
      call create_(mult,size(self))
      call get_all_Friedel_pairs_(self,mult)
      res = count(mult==2)
      call destroy_(mult)
   end function


!  ==================
!  Enquiry statistics
!  ==================

   subroutine add_noise(self,dsigma)
   ! Reset experimental data to reflect addition of noise "dsigma" in
   ! units of sigma. The noise is assumed normally distributed.
   ! NOTE: F_sig is also reset! So GoF2(F) should be fit to the same level
   ! NOTE: Noise is added 
   !       after  small F_sig's are removed, but
   !       before small F_exp/F_sig
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      REAL, IN :: dsigma
      INT :: n
      REAL :: dF
      do n = 1,size(self)

         ! dF has mean zero, standard deviation one
         call to_random_normal_(dF)
         dF = dF*dsigma

         ! Reset
         call set_F_corr_(self(n),self(n)%F_corr+dF*self(n)%F_sigma)
         call set_F_sigma_(self(n),self(n)%F_sigma+abs(dsigma)*self(n)%F_sigma)
      end do
   end subroutine

   PURE function F_GoF2(self) result (res)
   ! GoF2 for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: z
      INT :: n
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:F_GoF2 ... no predicted structure factors")
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F_GoF2 ... no experimental structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"VEC{REFLECTION}:F_GoF2 ... no experimental structure factor errors")
      res = ZERO
      do n = 1,size(self)
         z = F_z_(self(n))
         res = res+z*z
      end do
      res = res/max(size(self)-1,1)
   end function

   PURE function I_GoF2(self) result (res)
   ! GoF2 for the intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: z
      INT :: n
   ENSURE(have_I_pred_(self),"VEC{REFLECTION}:I_GoF2 ... no predicted intensities")
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:I_GoF2 ... no experimental intensities")
   ENSURE(have_nonzero_I_sigma_(self),"VEC{REFLECTION}:I_GoF2 ... no experimental intensity errors")
      res = ZERO
      do n = 1,size(self)
         z = I_z_(self(n))
         res = res+z*z
      end do
      res = res/max(size(self)-1,1)
   end function

   PURE function F_goodness_of_fit(self) result (res)
   ! goodness_of_fit for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      res = sqrt(F_GoF2_(self))
   end function

   PURE function I_goodness_of_fit(self) result (res)
   ! goodness_of_fit for the intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      res = sqrt(I_GoF2_(self))
   end function

   PURE function F_r_factor(self) result (res)
   ! r factor for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot
      INT :: n
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:F_r_factor ... no predicted structure factors")
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F_r_factor ... no experimental structure factors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         top = top+abs(self(n)%F_pred-self(n)%F_corr)
         bot = bot+abs(self(n)%F_corr)
      end do
      res = top/bot
   end function

   PURE function F2_r_factor(self) result (res)
   ! r factor for the structure factors *squared*
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot,Fp2,Fe2
      INT :: n
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:F2_r_factor ... no predicted structure factors")
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F2_r_factor ... no experimental structure factors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         Fp2 = self(n)%F_pred; Fp2 = Fp2*Fp2
         Fe2 = self(n)%F_corr; Fe2 = Fe2*Fe2
         top = top+abs(Fp2-Fe2)
         bot = bot+abs(Fe2)
      end do
      res = top/bot
   end function

   PURE function I_r_factor(self) result (res)
   ! R factor for the intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot
      INT :: n
   ENSURE(have_I_pred_(self),"VEC{REFLECTION}:I_r_factor ... no predicted structure factors")
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:I_r_factor ... no experimental structure factors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         top = top+abs(self(n)%I_pred-self(n)%I_exp)
         bot = bot+abs(self(n)%I_exp)
      end do
      res = top/bot
   end function

   PURE function F_r_sigma(self) result (res)
   ! The R_sigma(F) for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot
      INT :: n
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F_r_sigma ... no experimental structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"VEC{REFLECTION}:F_r_sigma ... no experimental structure factor errors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         top = top+abs(self(n)%F_sigma)
         bot = bot+abs(self(n)%F_corr)
      end do
      res = top/bot
   end function

   PURE function F2_r_sigma(self) result (res)
   ! The R_sigma(F2) for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot
      INT :: n
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F2_r_sigma ... no experimental structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"VEC{REFLECTION}:F2_r_sigma ... no experimental structure factor errors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         top = top+abs(self(n)%F_sigma)**2
         bot = bot+abs(self(n)%F_corr)**2
      end do
      res = top/bot
   end function

   PURE function I_r_sigma(self) result (res)
   ! The R_sigma(I) for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot
      INT :: n
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:I_r_sigma ... no experimental intensities")
   ENSURE(have_nonzero_I_sigma_(self),"VEC{REFLECTION}:I_r_sigma ... no experimental intensity errors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         top = top+abs(self(n)%I_sigma)
         bot = bot+abs(self(n)%I_exp)
      end do
      res = top/bot
   end function

   PURE function F_weighted_r_factor(self) result (res)
   ! weighted r factor for the structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot,z,b
      INT :: n
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F_weighted_r_factor ... no experimental structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"VEC{REFLECTION}:F_weighted_r_factor ... no experimental structure factor errors")
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:F_weighted_r_factor ... no predicted structure factors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         z = F_z_(self(n))
         b = self(n)%F_corr/self(n)%F_sigma
         top = top+z*z
         bot = bot+b*b
      end do
      res = sqrt(top/bot)
   end function

   PURE function F2_weighted_r_factor(self) result (res)
   ! Weighted r factor for the structure factors *squared*
   ! NOTE: sigma is equal to (2 F_exp * F_sigma) 
   !       *not* F_sigma or F2_sigma and this is only
   !       appropriate for F refinement.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot,Fe,Fp,sg,z,b,Fe2,Fp2
      INT :: n
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:F2_weighted_r_factor ... no experimental structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"VEC{REFLECTION}:F2_weighted_r_factor ... no experimental structure factor errors")
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:F2_weighted_r_factor ... no predicted structure factors")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         Fp = self(n)%F_pred; Fp2 = Fp*Fp
         Fe = self(n)%F_corr; Fe2 = Fe*Fe
         sg = TWO*self(n)%F_corr*self(n)%F_sigma
         z = (Fe2-Fp2)/sg
         b = Fe2/sg
         top = top+z*z
         bot = bot+b*b
      end do
      res = sqrt(top/bot)
   end function

   PURE function I_weighted_r_factor(self) result (res)
   ! weighted r factor for the intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot,z,b
      INT :: n
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:I_weighted_r_factor ... no experimental intensities")
   ENSURE(have_nonzero_I_sigma_(self),"VEC{REFLECTION}:I_weighted_r_factor ... no experimental intensity errors")
   ENSURE(have_I_pred_(self),"VEC{REFLECTION}:I_weighted_r_factor ... no predicted intensities")
      top = ZERO
      bot = ZERO
      do n = 1,size(self)
         z = I_z_(self(n))
         b = self(n)%I_exp/self(n)%I_sigma
         top = top+z*z
         bot = bot+b*b
      end do
      res = sqrt(top/bot)
   end function

   pure function F2_calc_abs(self) result (res)
   ! Return the square of the magnitude of the calculated structure
   ! factors, F_calc
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res = abs(self(:)%F_calc)
      res = res*res
   end function

   pure function F_calc_abs(self) result (res)
   ! Return the magnitude of the calculated structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = abs(self(:)%F_calc)
   end function

   pure function F_calc(self) result (res)
   ! return the calculated structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(CPX,size(self)) :: res
      res(:) = self(:)%F_calc
   end function

   PURE function F_pred(self) result (res)
   ! return the predicted structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_pred
   end function

   PURE function F_exp(self) result (res)
   ! Return the experimental structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_exp
   end function

   PURE function F2_exp(self) result (res)
   ! Return the experimental structure factors *squared*
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_exp*self(:)%F_exp
   end function

   PURE function F_corr(self) result (res)
   ! Return the "corrected" experimental structure factors
   ! The correction may be because of e.g. removal of dispersion.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_corr
   end function

   PURE function F2_corr(self) result (res)
   ! Return the "corrected" experimental structure factors *squared*
   ! The correction may be because of e.g. removal of dispersion.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_corr*self(:)%F_corr
   end function

   PURE function I_pred(self) result (res)
   ! return the predicted intensities
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%I_pred
   end function

   PURE function F_sigma(self) result (res)
   ! return the errors in the experimental structure factors
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_sigma
   end function

   PURE function F2_sigma(self) result (res)
   ! Return the errors in the experimental structure factors *squared*
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      res(:) = self(:)%F_sigma*self(:)%F_sigma
   end function

   PURE function sigma2_eff(self) result (res)
   ! Return the effective (mean) value of GoF2 maybe useful
   ! for starting and ending constrained fit.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL :: res
      REAL :: top,bot,del,sig
      INT :: r
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:sigma2_eff ... no predicted structure factors")
   ENSURE(have_nonzero_F_sigma_(self),"VEC{REFLECTION}:sigma2_eff ... no experimental structure factor errors")
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:sigma2_eff ... no experimental structure factors")
      res = ZERO
      if (NOT have_nonzero_F_exp_(self)) return
      top = ZERO
      bot = ZERO
      do r = 1,size(self)
         sig = self(r)%F_sigma
         sig = sig*sig
         del = self(r)%F_corr-self(r)%F_pred
         del = del*del
         top = top+del
         bot = bot+del/sig
      end do
      res = top/bot
   end function


!  =============
!  Make k points
!  =============

   pure subroutine make_k_pts(self,k_pt,unit_cell)
   ! Make the reciprocal lattice vectors
   ! Dimension of "k_pt" is [.n_refl,3]
      VEC(type(REFLECTION_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: k_pt
      type(UNIT_CELL_TYPE), IN :: unit_cell
      INT :: n
      VEC(INT,3) :: hkl
      MAT(REAL,3,3) :: rcm
      rcm = TWO*PI*unit_cell%reciprocal_mx
      do n = 1,size(self)
         hkl = Miller_indices_(self(n))
         k_pt(n,1) = dot_product(rcm(1,:),hkl)
         k_pt(n,2) = dot_product(rcm(2,:),hkl)
         k_pt(n,3) = dot_product(rcm(3,:),hkl)
      end do
   end subroutine

   pure function n_unique_SF_k_pts(self,spacegroup) result (res)
   ! The number of unique k-points for an SF calculation
      VEC(type(REFLECTION_TYPE),:), IN :: self
      type(SPACEGROUP_TYPE), IN :: spacegroup
      INT :: res
      res = spacegroup%n_unique_SF_symops*size(self)
   end function

   PURE subroutine make_unique_SF_k_pts_0(self,k,spacegroup,unit_cell)
   ! Make the non-translational non-inverted symmetry generated
   ! reciprocal lattice vectors "k" required for structure factor
   ! calculations.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: k
      type(SPACEGROUP_TYPE), IN :: spacegroup
      type(UNIT_CELL_TYPE), IN :: unit_cell
   ENSURE(size(k,1)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_k_pts_0 ... k: wrong dim1")
   ENSURE(size(k,2)==3,"VEC{REFLECTION}:make_unique_SF_k_pts_0 ... k: wrong dim2")
      call make_unique_SF_k_pts_(self,k(:,1),k(:,2),k(:,3),spacegroup,unit_cell)
   end subroutine

   PURE subroutine make_unique_SF_k_pts_1(self,k1,k2,k3,spacegroup,unit_cell)
   ! Make the non-translational non-inverted symmetry generated
   ! reciprocal lattice vectors "k" required for structure factor
   ! calculations.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: k1,k2,k3
      type(SPACEGROUP_TYPE), IN :: spacegroup
      type(UNIT_CELL_TYPE), IN :: unit_cell
      MAT(REAL,3,3) :: B,rcm,symop
      INT :: h1,h2,h3,n,p,u
   ENSURE(size(k1)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_k_pts_1 ... k1: wrong dim")
   ENSURE(size(k2)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_k_pts_1 ... k2: wrong dim")
   ENSURE(size(k3)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_k_pts_1 ... k3: wrong dim")

      ! Reciprocal cell matrix
      rcm = TWO*PI*unit_cell%reciprocal_mx

      ! Make the
      do u = 1,spacegroup%n_unique_SF_symops
         symop = unique_SF_symop_mat_(spacegroup,u)
         B = matmul(rcm,transpose(symop))
         p = (u-1)*size(self)
         do n = 1,size(self)
            p = p+1
            h1 = self(n)%h1
            h2 = self(n)%h2
            h3 = self(n)%h3
            k1(p) = B(1,1)*h1+B(1,2)*h2+B(1,3)*h3
            k2(p) = B(2,1)*h1+B(2,2)*h2+B(2,3)*h3
            k3(p) = B(3,1)*h1+B(3,2)*h2+B(3,3)*h3
         end do
      end do
   end subroutine

   PURE subroutine make_unique_SF_Miller_indices(self,s1,s2,s3,spacegroup)
   ! Make the non-translational non-inverted symmetry generated
   ! reciprocal lattice MIller indices "h" required for structure
   ! factor calculations.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,:), OUT :: s1,s2,s3
      type(SPACEGROUP_TYPE), IN :: spacegroup
      MAT(REAL,3,3) :: B,symop
      INT :: h1,h2,h3,n,p,u
   ENSURE(size(s1)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_Miller_indices ... s1: wrong dim")
   ENSURE(size(s2)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_Miller_indices ... s2: wrong dim")
   ENSURE(size(s3)==spacegroup%n_unique_SF_symops*size(self),"VEC{REFLECTION}:make_unique_SF_Miller_indices ... s3: wrong dim")

      ! Make the
      do u = 1,spacegroup%n_unique_SF_symops
         symop = unique_SF_symop_mat_(spacegroup,u)
         B = transpose(symop)
         p = (u-1)*size(self)
         do n = 1,size(self)
            p = p+1
            h1 = self(n)%h1
            h2 = self(n)%h2
            h3 = self(n)%h3
            s1(p) = nint(B(1,1)*h1+B(1,2)*h2+B(1,3)*h3)
            s2(p) = nint(B(2,1)*h1+B(2,2)*h2+B(2,3)*h3)
            s3(p) = nint(B(3,1)*h1+B(3,2)*h2+B(3,3)*h3)
         end do
      end do
   end subroutine

   PURE subroutine make_symop_generated_SF_k_pts(self,k,spacegroup,unit_cell)
   ! Make the full set of symmetry generated reciprocal lattice
   ! vectors "k" required for structure factor calculations.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: k
      type(SPACEGROUP_TYPE), IN :: spacegroup
      type(UNIT_CELL_TYPE), IN :: unit_cell
      MAT(REAL,3,3) :: B,rcm,symop
      INT :: h1,h2,h3,n,p,s
   ENSURE(size(k,1)==spacegroup%n_seitz*size(self),"VEC{REFLECTION}:make_symop_generated_SF_k_pts ... k: wrong dim1")
   ENSURE(size(k,2)==3,"VEC{REFLECTION}:make_symop_generated_SF_k_pts ... k: wrong dim2")

      ! Reciprocal cell matrix
      rcm = TWO*PI*unit_cell%reciprocal_mx

      ! Make the symmetry-generated k points
      do s = 1,spacegroup%n_seitz
         symop = spacegroup%seitz(1:3,1:3,s)
         call to_product_of_(B,rcm,symop,transpose_b=TRUE)
         p = (s-1)*size(self)
         do n = 1,size(self)
            p = p+1
            h1 = self(n)%h1
            h2 = self(n)%h2
            h3 = self(n)%h3
            k(p,1) = B(1,1)*h1+B(1,2)*h2+B(1,3)*h3
            k(p,2) = B(2,1)*h1+B(2,2)*h2+B(2,3)*h3
            k(p,3) = B(3,1)*h1+B(3,2)*h2+B(3,3)*h3
         end do
      end do
   end subroutine


!  =========
!  Set hkl's
!  =========

   PURE subroutine set_hkl_up_to_stl_limit(self,limit,cell)
   ! Set (h k l) reflections as a half-sphere up to an STL "limit".
   ! NOTE: See Dunitz "Crystal Analysis" (1995) p. 35
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      REAL, IN :: limit
      type(UNIT_CELL_TYPE), IN :: cell
      VEC(INT,3) :: h
      VEC(REAL,3) :: hv
      REAL :: d,stl
      VEC(type(REFLECTION_TYPE),:), allocatable :: new
      type(REFLECTION_TYPE) :: ref
      INT :: h1,h2,h3,h_max,h1_max,h2_max,h3_max

      ! Get h_max
      h1_max = ceiling(cell%length(1)*2.5*limit)
      h2_max = ceiling(cell%length(2)*2.5*limit)
      h3_max = ceiling(cell%length(3)*2.5*limit)
      h_max = max(h1_max,h2_max,h3_max)

      ! Add the list of reflections
      call create_(new,0)
      do h1 = -h_max,h_max
         do h2 = -h_max,h_max
            do h3 = 0,h_max
               if (h1==0 AND h2==0 AND h3==0) cycle
               h = [h1,h2,h3]
               hv = matmul(cell%reciprocal_mx,h)
               d = ONE/norm_(hv)  ! with HBB 6/2/16
               stl = HALF/d
               if (stl>limit) cycle

         ! Append
               call set_miller_indices_(ref,h)
               call append_(new,ref)
            end do
         end do
      end do

      ! Reassign
      self = new
   end subroutine

   PURE subroutine set_hkl_range(self,hkl_range)
   ! Set a h,k,l range ... useful for structure factor calculations
   ! without having to set every reflection individually.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: hkl_range
      INT :: f1,f2,f3
      INT :: l1,l2,l3
      INT :: h1,h2,h3
      INT :: dim,i
   ENSURE(size(hkl_range)==3,"VEC{REFLECTION}:set_hkl_range ... hkl range must have three ranges")
   ENSURE(size(hkl_range(1)%element)==2,"VEC{REFLECTION}:set_hkl_range ... hkl range 1 must have two limits")
   ENSURE(size(hkl_range(2)%element)==2,"VEC{REFLECTION}:set_hkl_range ... hkl range 2 must have two limits")
   ENSURE(size(hkl_range(3)%element)==2,"VEC{REFLECTION}:set_hkl_range ... hkl range 3 must have two limits")

      ! Get limits
      f1 = hkl_range(1)%element(1)
      f2 = hkl_range(2)%element(1)
      f3 = hkl_range(3)%element(1)
      l1 = hkl_range(1)%element(2)
      l2 = hkl_range(2)%element(2)
      l3 = hkl_range(3)%element(2)
      dim = (l1-f1+1)*(l2-f2+1)*(l3-f3+1)

      ! Create the array
      call destroy_(self)
      call create_(self,dim)

      ! Report
      i = 0
      do h1 = f1,l1
         do h2 = f2,l2
            do h3 = f3,l3
               i = i+1
               call set_h1_(self(i),h1)
               call set_h2_(self(i),h2)
               call set_h3_(self(i),h3)
            end do
         end do
      end do
   end subroutine


!  =======================
!  List-based I/O Routines
!  =======================

   recursive subroutine read_list_keywords(self)
   ! This version is for allocatable arrays.
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{REFLECTION}:read_list_keywords ... expecting open bracket symbol, {")

      ! Read the open brace

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)

      ! Echo the brace if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input list-type keywords

         ! Read a keyword
         call read_(stdin,word)

         ! Unexpected end of file?
         DIE_IF(reverted_(stdin),"VEC{REFLECTION}:read_list_keywords ... unexpected end of file while reading list keywords")

         ! Lower case the word
         call to_lower_case_(word)

         ! Is there an equals next? Then join it on
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo the keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if it is a close brace
         if (word=="}") exit

         ! Process the list keyword
         call process_list_keyword_(self,word)
      end do
   end subroutine

   subroutine process_list_keyword(self,keyword)
   ! This version is for allocatable arrays.
   ! Process a list-type "keyword", common to all list-type objects.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word

      ! The following code is inherited from VEC{OBJECT}
      word = keyword
      call to_lower_case_(word)
      select case (word)

      ! Standard list-input keywords
      case ("altered_data=    "); call read_altered_data_(self)
      case ("append_data=     "); call read_append_data_(self)
      case ("data=            "); call read_data_(self)
      case ("do               "); call read_keywords_(self)
      case ("keys=            "); call read_keys_(self)
      case ("new_data=        "); call destroy_(self); call read_data_(self)
      case ("process_keys     "); call process_keys_(self)
      case ("process_keys_once"); call process_keys_once_(self)
      case ("redirect         "); call redirect_(self)
      case ("revert           "); call revert_(self)

      ! Backtrack & assume data= sans braces
      case default
         call move_to_previous_item_(stdin)
         call read_data_sans_braces_(self)
      end select
   end subroutine

   subroutine read_data(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. 
   ! Opening and closing braces are needed.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word

      ! Read open braces

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)  ! move past open brace
      ENSURE(word=="{","VEC{REFLECTION}:read_data ... expecting open bracket symbol, {")

      ! Read data (process keys)
      call read_data_sans_braces_(self)

      ! Read close brace
      call read_(stdin,word)
      ENSURE(word=="}","VEC{REFLECTION}:read_data ... expecting close bracket symbol, }")
   end subroutine

   subroutine read_data_sans_braces(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. 
   ! Opening and closing braces are not needed.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT :: length

      ! Get the vector length

      ! The following code is inherited from VEC{OBJECT}
      length = data_length_(self)
      if (allocated(self)) then

         ! If created, is dimension the same
         ENSURE(length==size(self),"VEC{REFLECTION}:read_data_sans_braces ... Mismatching new and old data lists")
      else

         ! If not created, create it
         call create_(self,length)
      end if

      ! Process keys
      call process_keys_(self)
   end subroutine

   subroutine read_data_BL_terminated(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. Opening
   ! braces are not needed, but a blank line (BL) is used as an
   ! end-of-list terminator.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT :: length

      ! Get the vector length

      ! The following code is inherited from VEC{OBJECT}
      length = data_length_BL_terminated_(self)
      if (allocated(self)) then

         ! If created, is dimension the same
         ENSURE(length==size(self),"VEC{REFLECTION}:read_data_BL_terminated ... Mismatching new and old data lists")
      else

         ! If not created, create it
         call create_(self,length)
      end if

      ! Process keys
      call process_keys_(self)

      ! Empty line there? Read it.
      call read_line_(stdin)
      ENSURE(stdin%buffer%string==" ","VEC{REFLECTION}:read_data_BL_terminated ... expecting blank line")
   end subroutine

   function data_length_BL_terminated(self) result (length)
   ! This version is for allocatable arrays.
   ! Read ahead in stdin to get the "length" of the data list. The
   ! data list begins with the first data item, *not* a "{" symbol.
   ! The data list must be terminated by a blank line (BL).
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT :: length
      type(REFLECTION_TYPE), allocatable :: tmp
      STR(len=STR_SIZE) :: word
      INT :: line,item
   ENSURE(next_item_(stdin)/="}","VEC{REFLECTION}:data_length_BL_terminated ... empty data list!")

      ! Remove warning

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if

      ! Initialize result
      length = 0

      ! Read first item, initialize line no & item no.
      call read_(stdin,word)
      line = line_number_(stdin)
      item = previous_line_item_(stdin)

      ! Get the length
      do

         ! Move just before
         call move_to_previous_item_(stdin)

         ! Read a scalar & increment
         call create_(tmp)
         call process_keys_(tmp)
         call destroy_(tmp)
         length = length+1

         ! Blank-line terminated?
         call read_line_(stdin)
         if (stdin%buffer%string==" ") exit
         if (at_EOF_(stdin)) exit

         ! No blank line ... read first item
         call read_(stdin,word)
         if (at_EOF_(stdin)) exit  ! Oops
      end do

      ! Go back to start
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
   end function

   function data_length(self) result (length)
   ! This version is for allocatable arrays.
   ! Read ahead in stdin to get the "length" of the data list, i.e. the number
   ! of data items in the list. The data must begin with the first data item,
   ! *not* a "{" symbol.  The order of data items comprising the list is given
   ! by keys defined in the associated list-element type module. The data list
   ! must be terminated by a "}" symbol.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT :: length
      type(REFLECTION_TYPE), allocatable :: tmp
      STR(len=STR_SIZE) :: word
      INT :: line,item
   ENSURE(next_item_(stdin)/="}","VEC{REFLECTION}:data_length ... empty data list!")

      ! Remove warning

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if

      ! Initialize result
      length = 0

      ! Read first item, initialize line no & item no.
      call read_(stdin,word)
      line = line_number_(stdin)
      item = previous_line_item_(stdin)

      ! Get the length
      do

         ! Move just before
         call move_to_previous_item_(stdin)

         ! Read a scalar & increment
         call create_(tmp)
         call process_keys_(tmp)
         call destroy_(tmp)
         length = length+1

         ! Closing-brace there?
         call read_(stdin,word)
         call to_lower_case_(word)
         if (word=="}") exit
         if (at_EOF_(stdin)) exit
      end do

      ! Go back to start
      call move_to_line_(stdin,line)
      call move_to_line_item_(stdin,item)
   end function

   subroutine read_altered_data(self)
   ! This version is for allocatable arrays.
   ! Read in a sublist of the complete list, and alter the data for that
   ! sublist.  The order of the data items in the sublist is given by the "keys"
   ! defined in the associated list-element type module.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
      INT :: s
   ENSURE(allocated(self),"VEC{REFLECTION}:read_altered_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","VEC{REFLECTION}:read_altered_data ... expecting open bracket symbol: {")

      ! Read brace {

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)
      do

         ! Read index
         call read_(stdin,word)

         ! End of indices
         if (word=="}") exit

         ! Get index
         ENSURE(is_int_(word),"VEC{REFLECTION}:read_altered_data ... expecting integer list-element index")
         s = to_int_(word)
         ENSURE(s<=size(self),"VEC{REFLECTION}:read_altered_data ... list-element too large")
         ENSURE(s>0,"VEC{REFLECTION}:read_altered_data ... list-element must be positive")

         ! Altered data for this index
         call process_keys_(self(s))
      end do
   end subroutine

   subroutine read_append_data(self)
   ! This version is for allocatable arrays.
   ! Read in a set of data to append to an existing set.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      VEC(type(REFLECTION_TYPE),:), allocatable :: tmp
   ENSURE(allocated(self),"VEC{REFLECTION}:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","VEC{REFLECTION}:read_append_data ... expecting open bracket symbol: {")

      ! Read data

      ! The following code is inherited from VEC{OBJECT}
      call read_data_(tmp)

      ! Append it
      call append_(self,tmp)

      ! Clean
      call destroy_(tmp)
   end subroutine

   subroutine process_keys(self)
   ! This version is for allocatable arrays.
   ! Process the "keys" on each element of the list.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      INT :: s

      ! The following code is inherited from VEC{OBJECT}
      if (allocated(self)) then

         ! Process keys on each element
         do s = 1,size(self)
            call process_keys_(self(s))
         end do
      else

         ! Process keys once on anonymous element
         call process_keys_once_(self)
      end if
   end subroutine

   subroutine process_keys_once(self)
   ! This version is for allocatable arrays.
   ! Process the "keys" just once on an anonymous object. This is useful if the
   ! "keys" set global variables in the list module (e.g. the default order of
   ! lists *within* the list data) as opposed to being keys pertaining to each
   ! element of the list.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(REFLECTION_TYPE) :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call process_keys_(tmp)
   end subroutine

   function keys_created(self) result (res)
   ! This version is for allocatable arrays.
   ! Return TRUE if the list-element keys are created.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      BIN :: res
      type(REFLECTION_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      res = keys_created_(tmp)
      call destroy_(tmp)
   end function

   subroutine set_keys(self,the_keys)
   ! This version is for allocatable arrays.
   ! This is for setting the "keys" externally.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      VEC(STR(len=*),:) :: the_keys
      type(REFLECTION_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      call set_keys_(tmp,the_keys)
      call destroy_(tmp)
   end subroutine

   subroutine clear_keys(self)
   ! This version is for allocatable arrays.
   ! This is for destroying the "keys" externally.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(REFLECTION_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      call clear_keys_(tmp)
      call destroy_(tmp)
   end subroutine

   subroutine read_keys(self)
   ! This version is for allocatable arrays.
   ! Read a new set of keys
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(REFLECTION_TYPE), allocatable :: tmp

      ! The following code is inherited from VEC{OBJECT}
      if (FALSE) then
         if (allocated(self)) then
            self = self
         end if
      end if
      call create_(tmp)
      call read_keys_(tmp)
      call destroy_(tmp)
   end subroutine

   subroutine redirect(self)
   ! Redirect input. (This code is for non-pointer self objects).
      VEC(type(REFLECTION_TYPE),:), INOUT :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file. (This code is for non-pointer self
   ! objects).
      VEC(type(REFLECTION_TYPE),:), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine


!  ===========================
!  Non-list based I/O routines
!  ===========================

   subroutine read_keywords(self)
   ! This version is for allocatable arrays.
   ! Read in and process normal (non list-type) keywords from "stdin".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{REFLECTION}:read_keywords ... expecting open bracket symbol, {")

      ! Read the opening brace

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)

      ! Echo the brace if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over keywords within brace

         ! Read a keyword
         call read_(stdin,word)

         ! Unexpected end of file?
         DIE_IF(reverted_(stdin),"VEC{REFLECTION}:read_keywords ... unexpected end of file while reading list keywords")

         ! Lower case word
         call to_lower_case_(word)

         ! Is there an equals next? Then join it on
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo the keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if it is a close brace
         if (word=="}") exit

         ! Process the keyword
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process a normal (non list-type) "keyword".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}")  ! do nothing.
      case ("put                      "); call put_(self)
      case ("put_f_exp_data           "); call put_F_exp_data_(self)
      case ("put_f_pred_data          "); call put_F_pred_data_(self)
      case ("put_f_statistics         "); call put_F_statistics_(self)
      case ("put_i_exp_data           "); call put_I_exp_data_(self)
      case ("put_i_statistics         "); call put_I_statistics_(self)
      case ("put_intensity_data       "); call put_intensity_data_(self)
      case ("put_structure_factor_data"); call put_structure_factor_data_(self)
      case ("redirect                 "); call redirect_(self)
      case ("revert                   "); call revert_(self)
      case default; allocate(tonto%known_keywords(11))
      tonto%known_keywords(1) = "}"
      tonto%known_keywords(2) = "put                      "
      tonto%known_keywords(3) = "put_f_exp_data           "
      tonto%known_keywords(4) = "put_f_pred_data          "
      tonto%known_keywords(5) = "put_f_statistics         "
      tonto%known_keywords(6) = "put_i_exp_data           "
      tonto%known_keywords(7) = "put_i_statistics         "
      tonto%known_keywords(8) = "put_intensity_data       "
      tonto%known_keywords(9) = "put_structure_factor_data"
      tonto%known_keywords(10) = "redirect                 "
      tonto%known_keywords(11) = "revert                   "
      call unknown_(tonto,word,"VEC{REFLECTION}:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


!  ========
!  Read CIF
!  ========

   subroutine read_CIF(self,cif)
   ! Read reflections from a "cif" file.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (NOT cif%is_mmCIF) then
         call read_smCIF_(self,cif)
      else
         call read_mmCIF_(self,cif)
      end if
   end subroutine


! smCIF

   subroutine read_smCIF(self,cif)
   ! Read reflections from a small-molecule "cif" file.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (has_smCIF_F_meas_F_calc(cif)) then; call read_smCIF_F_meas_F_calc_(self,cif)
      else if (has_smCIF_F_meas_only(cif)) then; call read_smCIF_F_meas_only_(self,cif)
      else if (has_smCIF_F2_reflections(cif)) then; call read_smCIF_F2_reflections_(self,cif)
      end if
   end subroutine

   function has_smCIF_F_meas_F_calc(cif) result (res)
   ! Does small-molecule "cif" have F_meas, F_sigma and F_calc
   ! reflections?
      type(CIF_TYPE), INOUT :: cif
      BIN :: res
      VEC(BIN,6) :: there

      ! Find reflections
      call find_looped_item_(cif,"_refln_index_h",there(1))
      call find_looped_item_(cif,"_refln_index_k",there(2))
      call find_looped_item_(cif,"_refln_index_l",there(3))
      call find_looped_item_(cif,"_refln_F_calc",there(4))
      call find_looped_item_(cif,"_refln_F_meas",there(5))
      call find_looped_item_(cif,"_refln_F_sigma",there(6))
      res = all(there)
   end function

   subroutine read_smCIF_F_meas_F_calc(self,cif)
   ! Read F reflections from small-molecule "cif".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(INT,:), allocatable :: h,k,l
      VEC(REAL,:), allocatable :: Fp,Fm,Sg

      ! Read
      call read_looped_item_(cif,"_refln_index_h",h)
      call read_looped_item_(cif,"_refln_index_k",k)
      call read_looped_item_(cif,"_refln_index_l",l)
      call read_looped_item_(cif,"_refln_F_calc",Fp)
      call read_looped_item_(cif,"_refln_F_meas",Fm)
      call read_looped_item_(cif,"_refln_F_sigma",Sg)

      ! Create and assign
      call destroy_(self)
      call create_(self,size(h))
      call set_h_(self,h)
      call set_k_(self,k)
      call set_l_(self,l)
      call set_F_pred_(self,Fp)
      call set_F_exp_(self,Fm)
      call set_F_sigma_(self,Sg)

      ! Clean
      call destroy_(Sg)
      call destroy_(Fm)
      call destroy_(Fp)
      call destroy_(l)
      call destroy_(k)
      call destroy_(h)
   end subroutine

   function has_smCIF_F_meas_only(cif) result (res)
   ! Does small-molecule "cif" have F_meas & F_sigma reflections?
      type(CIF_TYPE), INOUT :: cif
      BIN :: res
      VEC(BIN,5) :: there

      ! Find reflections
      call find_looped_item_(cif,"_refln_index_h",there(1))
      call find_looped_item_(cif,"_refln_index_k",there(2))
      call find_looped_item_(cif,"_refln_index_l",there(3))
      call find_looped_item_(cif,"_refln_F_meas",there(4))
      call find_looped_item_(cif,"_refln_F_sigma",there(5))
      res = all(there)
   end function

   subroutine read_smCIF_F_meas_only(self,cif)
   ! Read F_meas and F_sigma from small-molecule "cif".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(INT,:), allocatable :: h,k,l
      VEC(REAL,:), allocatable :: Fm,Sg

      ! Read
      call read_looped_item_(cif,"_refln_index_h",h)
      call read_looped_item_(cif,"_refln_index_k",k)
      call read_looped_item_(cif,"_refln_index_l",l)
      call read_looped_item_(cif,"_refln_F_meas",Fm)
      call read_looped_item_(cif,"_refln_F_sigma",Sg)

      ! Create and assign
      call destroy_(self)
      call create_(self,size(h))
      call set_h_(self,h)
      call set_k_(self,k)
      call set_l_(self,l)
      call set_F_exp_(self,Fm)
      call set_F_sigma_(self,Sg)

      ! Clean
      call destroy_(Sg)
      call destroy_(Fm)
      call destroy_(l)
      call destroy_(k)
      call destroy_(h)
   end subroutine

   function has_smCIF_F2_reflections(cif) result (res)
   ! Does small-molecule "cif" have F2 reflections?
      type(CIF_TYPE), INOUT :: cif
      BIN :: res
      VEC(BIN,5) :: there

      ! Find reflections
      call find_looped_item_(cif,"_refln_index_h",there(1))
      call find_looped_item_(cif,"_refln_index_k",there(2))
      call find_looped_item_(cif,"_refln_index_l",there(3))
      call find_looped_item_(cif,"_refln_F_squared_meas",there(4))
      call find_looped_item_(cif,"_refln_F_squared_sigma",there(5))
      res = all(there)
   end function

   subroutine read_smCIF_F2_reflections(self,cif)
   ! Read F2 reflections from small-molecule "cif".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(INT,:), allocatable :: h,k,l
      VEC(REAL,:), allocatable :: Fm,Sg

      ! Read
      call read_looped_item_(cif,"_refln_index_h",h)
      call read_looped_item_(cif,"_refln_index_k",k)
      call read_looped_item_(cif,"_refln_index_l",l)
      call read_looped_item_(cif,"_refln_F_squared_meas",Fm)
      call read_looped_item_(cif,"_refln_F_squared_sigma",Sg)

      ! Create and assign
      call destroy_(self)
      call create_(self,size(h))
      call set_h_(self,h)
      call set_k_(self,k)
      call set_l_(self,l)
      call set_I_exp_(self,Fm)
      call set_I_sigma_(self,Sg)

      ! Clean
      call destroy_(Sg)
      call destroy_(Fm)
      call destroy_(l)
      call destroy_(k)
      call destroy_(h)
   end subroutine


! mmCIF

   subroutine read_mmCIF(self,cif)
   ! Read reflections from a macromolecular "cif" file.
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (has_mmCIF_F_meas_F_calc(cif)) then; call read_mmCIF_F_meas_F_calc_(self,cif)
      else if (has_mmCIF_F_meas_only(cif)) then; call read_mmCIF_F_meas_only_(self,cif)
      else if (has_mmCIF_F2_reflections(cif)) then; call read_mmCIF_F2_reflections_(self,cif)
      end if
   end subroutine

   function has_mmCIF_F_meas_F_calc(cif) result (res)
   ! Does macromolecular "cif" have F_meas, F_sigma and F_calc
   ! reflections?
      type(CIF_TYPE), INOUT :: cif
      BIN :: res
      VEC(BIN,6) :: there

      ! Find reflections
      call find_looped_item_(cif,"_refln.index_h",there(1))
      call find_looped_item_(cif,"_refln.index_k",there(2))
      call find_looped_item_(cif,"_refln.index_l",there(3))
      call find_looped_item_(cif,"_refln.F_calc",there(4))
      call find_looped_item_(cif,"_refln.F_meas",there(5))
      call find_looped_item_(cif,"_refln.F_sigma",there(6))
      res = all(there)
   end function

   subroutine read_mmCIF_F_meas_F_calc(self,cif)
   ! Read F reflections from macromolecular "cif".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(INT,:), allocatable :: h,k,l
      VEC(REAL,:), allocatable :: Fp,Fm,Sg

      ! Read
      call read_looped_item_(cif,"_refln.index_h",h)
      call read_looped_item_(cif,"_refln.index_k",k)
      call read_looped_item_(cif,"_refln.index_l",l)
      call read_looped_item_(cif,"_refln.F_calc",Fp)
      call read_looped_item_(cif,"_refln.F_meas",Fm)
      call read_looped_item_(cif,"_refln.F_sigma",Sg)

      ! Create and assign
      call destroy_(self)
      call create_(self,size(h))
      call set_h_(self,h)
      call set_k_(self,k)
      call set_l_(self,l)
      call set_F_pred_(self,Fp)
      call set_F_exp_(self,Fm)
      call set_F_sigma_(self,Sg)

      ! Clean
      call destroy_(Sg)
      call destroy_(Fm)
      call destroy_(Fp)
      call destroy_(l)
      call destroy_(k)
      call destroy_(h)
   end subroutine

   function has_mmCIF_F_meas_only(cif) result (res)
   ! Does macromolecular "cif" have F_meas & F_sigma reflections?
      type(CIF_TYPE), INOUT :: cif
      BIN :: res
      VEC(BIN,5) :: there

      ! Find reflections
      call find_looped_item_(cif,"_refln.index_h",there(1))
      call find_looped_item_(cif,"_refln.index_k",there(2))
      call find_looped_item_(cif,"_refln.index_l",there(3))
      call find_looped_item_(cif,"_refln.F_meas",there(4))
      call find_looped_item_(cif,"_refln.F_sigma",there(5))
      res = all(there)
   end function

   subroutine read_mmCIF_F_meas_only(self,cif)
   ! Read F_meas and F_sigma from macromolecular "cif".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(INT,:), allocatable :: h,k,l
      VEC(REAL,:), allocatable :: Fm,Sg

      ! Read
      call read_looped_item_(cif,"_refln.index_h",h)
      call read_looped_item_(cif,"_refln.index_k",k)
      call read_looped_item_(cif,"_refln.index_l",l)
      call read_looped_item_(cif,"_refln.F_meas",Fm)
      call read_looped_item_(cif,"_refln.F_sigma",Sg)

      ! Create and assign
      call destroy_(self)
      call create_(self,size(h))
      call set_h_(self,h)
      call set_k_(self,k)
      call set_l_(self,l)
      call set_F_exp_(self,Fm)
      call set_F_sigma_(self,Sg)

      ! Clean
      call destroy_(Sg)
      call destroy_(Fm)
      call destroy_(l)
      call destroy_(k)
      call destroy_(h)
   end subroutine

   function has_mmCIF_F2_reflections(cif) result (res)
   ! Does macromolecular "cif" have F2 reflections?
      type(CIF_TYPE), INOUT :: cif
      BIN :: res
      VEC(BIN,6) :: there

      ! Find reflections
      call find_looped_item_(cif,"_refln.index_h",there(1))
      call find_looped_item_(cif,"_refln.index_k",there(2))
      call find_looped_item_(cif,"_refln.index_l",there(3))
      call find_looped_item_(cif,"_refln.F_squared_meas",there(5))
      call find_looped_item_(cif,"_refln.F_squared_sigma",there(6))
      res = all(there)
   end function

   subroutine read_mmCIF_F2_reflections(self,cif)
   ! Read F2 reflections from macromolecular "cif".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(INT,:), allocatable :: h,k,l
      VEC(REAL,:), allocatable :: Fm,Sg

      ! Read
      call read_looped_item_(cif,"_refln.index_h",h)
      call read_looped_item_(cif,"_refln.index_k",k)
      call read_looped_item_(cif,"_refln.index_l",l)
      call read_looped_item_(cif,"_refln.F_squared_meas",Fm)
      call read_looped_item_(cif,"_refln.F_squared_sigma",Sg)

      ! Create and assign
      call destroy_(self)
      call create_(self,size(h))
      call set_h_(self,h)
      call set_k_(self,k)
      call set_l_(self,l)
      call set_I_exp_(self,Fm)
      call set_I_sigma_(self,Sg)

      ! Clean
      call destroy_(Sg)
      call destroy_(Fm)
      call destroy_(l)
      call destroy_(k)
      call destroy_(h)
   end subroutine

!  ==================
!  Read shelx/std hkl
!  ==================

   subroutine read_shelx_F_hkl_file(self,file)
   ! Read Sheldrick's hkl F/F2 reflections from "file".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: file
      INT :: i,n_refl,h,k,l,n
      REAL :: value,sigma

      ! The following code is inherited from VEC{REFLECTION}
      n_refl = no_of_shelx_hkl_reflections(file)

      ! Create reflections
      call destroy_(self)
      call create_(self,n_refl)

      ! Read them
      n = 0
      do i = 1,file%no_of_lines
         call move_to_line_(file,i)
         if (NOT exhausted_(file%buffer)) then

            ! Fixed format
            h = to_int_(file%buffer%string(1:4))
            k = to_int_(file%buffer%string(5:8))
            l = to_int_(file%buffer%string(9:12))
            if (h==0 AND k==0 AND l==0) cycle

            ! Fixed format
            value = to_real_(file%buffer%string(13:20))
            sigma = to_real_(file%buffer%string(21:28))
            n = n+1
            call set_miller_indices_(self(n),[h,k,l])
            call set_F_exp_(self(n),value)
            call set_F_sigma_(self(n),sigma)
         end if
      end do
   end subroutine

   subroutine read_shelx_F2_hkl_file(self,file)
   ! Read Sheldrick's hkl F/F2 reflections from "file".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: file
      INT :: i,n_refl,h,k,l,n
      REAL :: value,sigma

      ! The following code is inherited from VEC{REFLECTION}
      n_refl = no_of_shelx_hkl_reflections(file)

      ! Create reflections
      call destroy_(self)
      call create_(self,n_refl)

      ! Read them
      n = 0
      do i = 1,file%no_of_lines
         call move_to_line_(file,i)
         if (NOT exhausted_(file%buffer)) then

            ! Fixed format
            h = to_int_(file%buffer%string(1:4))
            k = to_int_(file%buffer%string(5:8))
            l = to_int_(file%buffer%string(9:12))
            if (h==0 AND k==0 AND l==0) cycle

            ! Fixed format
            value = to_real_(file%buffer%string(13:20))
            sigma = to_real_(file%buffer%string(21:28))
            n = n+1
            call set_miller_indices_(self(n),[h,k,l])
            call set_I_exp_(self(n),value)
            call set_I_sigma_(self(n),sigma)
         end if
      end do
   end subroutine

   function no_of_shelx_hkl_reflections(file) result (res)
   ! Return the no. of (h k l) reflections in SHELX "file".
   ! - Blank lines are ignored
   ! - (0 0 0) reflection ignored too ...
      type(TEXTFILE_TYPE), INOUT :: file
      INT :: res
      INT :: i,h,k,l
      res = 0
      do i = 1,file%no_of_lines
         call move_to_line_(file,i)
         if (NOT exhausted_(file%buffer)) then

            ! Fixed format
            h = to_int_(file%buffer%string(1:4))
            k = to_int_(file%buffer%string(5:8))
            l = to_int_(file%buffer%string(9:12))
            if (h==0 AND k==0 AND l==0) cycle
            res = res+1
         end if
      end do
   end function

   subroutine read_std_F_hkl_file(self,file)
   ! Read normal free-form hkl F/F2 reflections from "file".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: file
      INT :: i,n_refl,h,k,l,n
      REAL :: value,sigma

      ! The following code is inherited from VEC{REFLECTION}
      n_refl = no_of_hkl_reflections(file)

      ! Create reflections
      call destroy_(self)
      call create_(self,n_refl)

      ! Read them
      n = 0
      do i = 1,file%no_of_lines
         call move_to_line_(file,i)
         if (NOT exhausted_(file%buffer)) then
            call read_(file,h)
            call read_(file,k)
            call read_(file,l)
            if (h==0 AND k==0 AND l==0) cycle
            call read_(file,value)
            call read_(file,sigma)
            n = n+1
            call set_miller_indices_(self(n),[h,k,l])
            call set_F_exp_(self(n),value)
            call set_F_sigma_(self(n),sigma)
         end if
      end do
   end subroutine

   subroutine read_std_F2_hkl_file(self,file)
   ! Read normal free-form hkl F/F2 reflections from "file".
      VEC(type(REFLECTION_TYPE),:), allocatable, INOUT :: self
      type(TEXTFILE_TYPE), INOUT :: file
      INT :: i,n_refl,h,k,l,n
      REAL :: value,sigma

      ! The following code is inherited from VEC{REFLECTION}
      n_refl = no_of_hkl_reflections(file)

      ! Create reflections
      call destroy_(self)
      call create_(self,n_refl)

      ! Read them
      n = 0
      do i = 1,file%no_of_lines
         call move_to_line_(file,i)
         if (NOT exhausted_(file%buffer)) then
            call read_(file,h)
            call read_(file,k)
            call read_(file,l)
            if (h==0 AND k==0 AND l==0) cycle
            call read_(file,value)
            call read_(file,sigma)
            n = n+1
            call set_miller_indices_(self(n),[h,k,l])
            call set_I_exp_(self(n),value)
            call set_I_sigma_(self(n),sigma)
         end if
      end do
   end subroutine

   function no_of_hkl_reflections(file) result (res)
   ! Return the no. of (h k l) reflections in "file".
   ! - Blank lines are ignored
   ! - (0 0 0) reflection ignored too ...
      type(TEXTFILE_TYPE), INOUT :: file
      INT :: res
      INT :: i,h,k,l
      res = 0
      do i = 1,file%no_of_lines
         call move_to_line_(file,i)
         if (NOT exhausted_(file%buffer)) then

            ! Fixed format
            call read_(file,h)
            call read_(file,k)
            call read_(file,l)
            if (h==0 AND k==0 AND l==0) cycle
            res = res+1
         end if
      end do
   end function


!  ===============
!  Output routines
!  ===============

   subroutine put(self)
   ! Output the reflection data.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      call flush_(stdout)
      call show_(stdout,"Number of reflections =",size(self))

      ! Structure factors
      call flush_(stdout)
      call text_(stdout,"Structure factors:")
      if (have_F_pred_(self)) then
         if (have_nonzero_F_exp_(self)) then; call put_structure_factor_data_(self)
         else; call put_F_pred_data_(self)
         end if
      else
         if (have_nonzero_F_exp_(self)) then; call put_F_exp_data_(self)
         else if (have_F_calc_(self)) then; call put_F_calc_data_(self)
         else; call text_(stdout,"There are no structure factors yet")
         end if
      end if
   end subroutine

   subroutine put_intensities(self)
   ! Output the reflection data.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      call flush_(stdout)
      call show_(stdout,"Number of reflections =",size(self))

      ! Structure factors
      call flush_(stdout)
      call text_(stdout,"Structure factors:")
      call flush_(stdout)
      call text_(stdout,"Intensities:")
      if (have_I_pred_(self)) then
         if (have_nonzero_I_exp_(self)) then; call put_intensity_data_(self)
         else; call put_I_pred_data_(self)
         end if
      else
         if (have_nonzero_I_exp_(self)) then; call put_I_exp_data_(self)
         else; call text_(stdout,"There are no intensities yet")
         end if
      end if
   end subroutine

   subroutine put_intensities_XD(self)
   ! Output only the experimental intensity data in XD format
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(INT,:), allocatable :: group
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:put_intensities_XD ... no experimental intensities")

      ! Precision
      call save_style_(stdout)
      call set_int_width_(stdout,4)
      call set_real_precision_(stdout,3)

      ! Get data
      call create_(group,size(self))
      group = 1

      ! Make table
      call create_(table,6)
      call set_values_(table(1),self(:)%h1)
      call set_values_(table(2),self(:)%h2)
      call set_values_(table(3),self(:)%h3)
      call set_values_(table(4),group)
      call set_values_(table(5),self(:)%I_exp)
      call set_values_(table(6),self(:)%I_sigma)
      call set_heading_(table(1),"h")
      call force_width_(table(1),4)
      call set_heading_(table(2),"k")
      call force_width_(table(2),4)
      call set_heading_(table(3),"l")
      call force_width_(table(3),4)
      call set_heading_(table(4),"g")
      call force_width_(table(4),3)
      call set_heading_(table(5),"I")
      call force_width_(table(5),13)
      call set_heading_(table(6),"sigma")
      call force_width_(table(6),13)

      ! Put
      call put_body_(table)

      ! Clean
      call destroy_(table)
      call destroy_(group)

      ! Restore
      call unsave_style_(stdout)
   end subroutine

   subroutine put_Miller_indices(self,Fc)
   ! Output the Miller indices for the complex structure factors "Fc".
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(CPX,:), IN :: Fc
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: Fr,Fi
   ENSURE(size(Fc)==size(self),"VEC{REFLECTION}:put_Miller_indices ... Fc does not have the right dimension")

      ! Get data
      call create_(Fr,size(self)); Fr = RE(Fc)
      call create_(Fi,size(self)); Fi = IM(Fc)

      ! Make table
      call create_(table,5)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"F"); call set_values_(table(4),Fr)
      call set_heading_(table(5),"F"); call set_values_(table(5),Fi)

      ! Subheadings
      call set_subhead_(table(4),"Re")
      call set_subhead_(table(5),"Im")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(Fi)
      call destroy_(Fr)
   end subroutine

   subroutine put_structure_factor_data(self)
   ! Output the structure factor data
      VEC(type(REFLECTION_TYPE),:), IN :: self
      call put_F_statistics_(self)
      call put_structure_factor_table_(self)
   end subroutine

   subroutine put_structure_factor_table(self,index)
   ! Output the structure factor table. If "index" is present it is the
   ! position of each reflection in the full data set, and is shown as the
   ! first column in place of the row number.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,:), optional, IN :: index
      if (use_multiple_scale_factors_(self)) then; call put_SF_group_data_(self,index)
      else; call put_SF_nogrp_data_(self,index)
      end if
   end subroutine

   subroutine put_F_statistics(self,show_GoF2)
   ! Output the structure factor statistics
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN, optional, IN :: show_GoF2
      REAL :: GoF2
      BIN :: show
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:put_F_statistics ... No F_exp data!")
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:put_F_statistics ... No F_pred data!")
      call flush_(stdout)
      call text_(stdout,"Model statistics based on structure factors:")
      call flush_(stdout)
      call show_(stdout,"R_sigma(F)                     =",F_r_sigma_(self))
      call show_(stdout,"R_sigma(F2)                    =",F2_r_sigma_(self))
      call show_(stdout,"R(F)                           =",F_r_factor_(self))
      call show_(stdout,"R(F2)                          =",F2_r_factor_(self))
      call show_(stdout,"Rw(F)                          =",F_weighted_r_factor_(self))
      call show_(stdout,"Rw(F2)                         =",F2_weighted_r_factor_(self))
      show = TRUE
      if (present(show_GoF2)) show = show_GoF2
      if (show) then
         GoF2 = F_GoF2_(self)
         call show_(stdout,"GoF^2(N_p=1)(F)                =",GoF2)
         call show_(stdout,"GoF  (N_p=1)(F)                =",sqrt(GoF2))
      end if
   end subroutine

   subroutine put_F_free_stats(self,show_GoF2)
   ! Output the structure factor statistics
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN, optional, IN :: show_GoF2
      REAL :: GoF2
      BIN :: show
   ENSURE(have_nonzero_F_exp_(self),"VEC{REFLECTION}:put_F_free_stats ... No F_exp data!")
   ENSURE(have_F_pred_(self),"VEC{REFLECTION}:put_F_free_stats ... No F_pred data!")
      call flush_(stdout)
      call text_(stdout,"Statistics based on free structure factors:")
      call flush_(stdout)
      call show_(stdout,"Free R_sigma(F)              =",F_r_sigma_(self))
      call show_(stdout,"Free R(F)                    =",F_r_factor_(self))
      call show_(stdout,"Free Rw(F)                   =",F_weighted_r_factor_(self))
      show = TRUE
      if (present(show_GoF2)) show = show_GoF2
      if (show) then
         GoF2 = F_GoF2_(self)
         call show_(stdout,"Free chi^2(N_p=1)(F)         =",GoF2)
         call show_(stdout,"Free GOF(N_p=1)(F)           =",sqrt(GoF2))
      end if
   end subroutine

   subroutine put_SF_group_data(self,index)
   ! Output the structure factor data, with multiple scale factor
   ! groups. "index" is as in put_structure_factor_table.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,:), optional, IN :: index
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_z,stl,d
      INT :: c
      BIN :: number_rows

      ! Get data
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR
      call create_(F_cr,size(self)); F_cr = RE(self%F_calc)
      call create_(F_ci,size(self)); F_ci = IM(self%F_calc)
      call create_(F_z,size(self)); F_z = F_z_(self)

      ! Index column in place of the row number?
      number_rows = NOT present(index)
      c = 0
      if (present(index)) c = 1

      ! Make table
      call create_(table,12+c)
      if (c==1) then
         call set_heading_(table(1),"#"); call set_values_(table(1),index)
      end if
      call set_heading_(table(c+1),"h"); call set_values_(table(c+1),self(:)%h1)
      call set_heading_(table(c+2),"k"); call set_values_(table(c+2),self(:)%h2)
      call set_heading_(table(c+3),"l"); call set_values_(table(c+3),self(:)%h3)
      call set_heading_(table(c+4),"Scale"); call set_values_(table(c+4),self(:)%group)
      call set_heading_(table(c+5),"stl"); call set_values_(table(c+5),stl)
      call set_heading_(table(c+6),"d"); call set_values_(table(c+6),d)
      call set_heading_(table(c+7),"F_calc"); call set_values_(table(c+7),F_cr)
      call set_heading_(table(c+8),"F_calc"); call set_values_(table(c+8),F_ci)
      call set_heading_(table(c+9),"F_pred"); call set_values_(table(c+9),self(:)%F_pred)
      call set_heading_(table(c+10),"F_exp"); call set_values_(table(c+10),self(:)%F_exp)
      call set_heading_(table(c+11),"F_sigma"); call set_values_(table(c+11),self(:)%F_sigma)
      call set_heading_(table(c+12),"F_z"); call set_values_(table(c+12),F_z)

      ! Subheadings
      call set_subhead_(table(c+4),"group")
      call set_subhead_(table(c+5),"/A^-1")
      call set_subhead_(table(c+6),"/A")
      call set_subhead_(table(c+7),"Re")
      call set_subhead_(table(c+8),"Im")
      call put_(table,label_rows=number_rows)

      ! Clean
      call destroy_(table)
      call destroy_(F_z)
      call destroy_(F_ci)
      call destroy_(F_cr)
      call destroy_(d)
      call destroy_(stl)
   end subroutine

   subroutine put_SF_nogrp_data(self,index)
   ! Output the structure factor data, single scale factor group.
   ! "index" is as in put_structure_factor_table.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(INT,:), optional, IN :: index
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_z,stl,d
      INT :: c
      BIN :: number_rows

      ! Get data
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR
      call create_(F_cr,size(self)); F_cr = RE(self%F_calc)
      call create_(F_ci,size(self)); F_ci = IM(self%F_calc)
      call create_(F_z,size(self)); F_z = F_z_(self)

      ! Index column in place of the row number?
      number_rows = NOT present(index)
      c = 0
      if (present(index)) c = 1

      ! Make table
      call create_(table,11+c)
      if (c==1) then
         call set_heading_(table(1),"#"); call set_values_(table(1),index)
      end if
      call set_heading_(table(c+1),"h"); call set_values_(table(c+1),self(:)%h1)
      call set_heading_(table(c+2),"k"); call set_values_(table(c+2),self(:)%h2)
      call set_heading_(table(c+3),"l"); call set_values_(table(c+3),self(:)%h3)
      call set_heading_(table(c+4),"stl"); call set_values_(table(c+4),stl)
      call set_heading_(table(c+5),"d"); call set_values_(table(c+5),d)
      call set_heading_(table(c+6),"F_calc"); call set_values_(table(c+6),F_cr)
      call set_heading_(table(c+7),"F_calc"); call set_values_(table(c+7),F_ci)
      call set_heading_(table(c+8),"F_pred"); call set_values_(table(c+8),self(:)%F_pred)
      call set_heading_(table(c+9),"F_exp"); call set_values_(table(c+9),self(:)%F_exp)
      call set_heading_(table(c+10),"F_sigma"); call set_values_(table(c+10),self(:)%F_sigma)
      call set_heading_(table(c+11),"F_z"); call set_values_(table(c+11),F_z)

      ! Subheadings
      call set_subhead_(table(c+4),"/A^-1")
      call set_subhead_(table(c+5),"/A")
      call set_subhead_(table(c+6),"Re")
      call set_subhead_(table(c+7),"Im")
      call put_(table,label_rows=number_rows)

      ! Clean
      call destroy_(table)
      call destroy_(F_z)
      call destroy_(F_ci)
      call destroy_(F_cr)
      call destroy_(d)
      call destroy_(stl)
   end subroutine

   subroutine put_F_calc_data(self)
   ! Output only the calculated structure factor data.
   ! Predicted structure factors are the magnitude of F_calc.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_dr,F_di,F_pred,stl,d

      ! Get data
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR
      call create_(F_cr,size(self)); F_cr = RE(self%F_calc)
      call create_(F_ci,size(self)); F_ci = IM(self%F_calc)
      call create_(F_dr,size(self)); F_dr = RE(self%F_disp)
      call create_(F_di,size(self)); F_di = IM(self%F_disp)
      call create_(F_pred,size(self)); F_pred = abs(self%F_calc)

      ! Make table
      call create_(table,10)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"stl"); call set_values_(table(4),stl)
      call set_heading_(table(5),"d"); call set_values_(table(5),d)
      call set_heading_(table(6),"F_calc"); call set_values_(table(6),F_cr)
      call set_heading_(table(7),"F_calc"); call set_values_(table(7),F_ci)
      call set_heading_(table(8),"F_disp"); call set_values_(table(8),F_dr)
      call set_heading_(table(9),"F_disp"); call set_values_(table(9),F_di)
      call set_heading_(table(10),"F_pred"); call set_values_(table(10),F_pred)

      ! Subheadings
      call set_subhead_(table(4),"/A^-1")
      call set_subhead_(table(5),"/A")
      call set_subhead_(table(6),"Re")
      call set_subhead_(table(7),"Im")
      call set_subhead_(table(8),"Re")
      call set_subhead_(table(9),"Im")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(F_pred)
      call destroy_(F_di)
      call destroy_(F_dr)
      call destroy_(F_ci)
      call destroy_(F_cr)
      call destroy_(d)
      call destroy_(stl)
   end subroutine

   subroutine put_F_pred_data(self)
   ! Output only the predicted structure factor data
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: F_cr,F_ci,stl,d

      ! Get data
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR
      call create_(F_cr,size(self)); F_cr = RE(self%F_calc)
      call create_(F_ci,size(self)); F_ci = IM(self%F_calc)

      ! Make table
      call create_(table,8)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"stl"); call set_values_(table(4),stl)
      call set_heading_(table(5),"d"); call set_values_(table(5),d)
      call set_heading_(table(6),"F_calc"); call set_values_(table(6),F_cr)
      call set_heading_(table(7),"F_calc"); call set_values_(table(7),F_ci)
      call set_heading_(table(8),"F_pred"); call set_values_(table(8),self(:)%F_pred)

      ! Subheadings
      call set_subhead_(table(4),"/A^-1")
      call set_subhead_(table(5),"/A")
      call set_subhead_(table(6),"Re")
      call set_subhead_(table(7),"Im")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(F_ci)
      call destroy_(F_cr)
      call destroy_(d)
      call destroy_(stl)
   end subroutine

   subroutine put_F_exp_data(self)
   ! Output only the experimental structure factor data
      VEC(type(REFLECTION_TYPE),:), IN :: self
      if (use_multiple_scale_factors_(self)) then; call put_F_exp_group_data_(self)
      else; call put_F_exp_nogrp_data_(self)
      end if
   end subroutine

   subroutine put_F_exp_nogrp_data(self)
   ! Output only the experimental structure factor data, where only
   ! one scale factor (scale group) is used.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: stl,d

      ! Get data
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR

      ! Make table
      call create_(table,7)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"stl"); call set_values_(table(4),stl)
      call set_heading_(table(5),"d"); call set_values_(table(5),d)
      call set_heading_(table(6),"F_exp"); call set_values_(table(6),self(:)%F_exp)
      call set_heading_(table(7),"F_sigma"); call set_values_(table(7),self(:)%F_sigma)

      ! Subheadings
      call set_subhead_(table(4),"/A^-1")
      call set_subhead_(table(5),"/A")

      ! Body
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(d)
      call destroy_(stl)
   end subroutine

   subroutine put_F_exp_group_data(self)
   ! Output only the experimental structure factor data where multiple
   ! scale factors (scale groups) are used.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(INT,:), allocatable :: g
      VEC(REAL,:), allocatable :: stl,d

      ! Get data
      call create_(g,size(self)); g = self%group
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR

      ! Make table
      call create_(table,8)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"Scale"); call set_values_(table(4),g)
      call set_heading_(table(5),"stl"); call set_values_(table(5),stl)
      call set_heading_(table(6),"d"); call set_values_(table(6),d)
      call set_heading_(table(7),"F_exp"); call set_values_(table(7),self(:)%F_exp)
      call set_heading_(table(8),"F_sigma"); call set_values_(table(8),self(:)%F_sigma)

      ! Subheadings
      call set_subhead_(table(4),"group")
      call set_subhead_(table(5),"/A^-1")
      call set_subhead_(table(6),"/A")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(d)
      call destroy_(stl)
      call destroy_(g)
   end subroutine

   subroutine put_F_all_data(self)
   ! Output all the structure factor date.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_ph,F_dr,F_di,stl,d

      ! Get data
      call create_(stl,size(self)); stl = self(:)%stl*BOHR_PER_ANGSTROM
      call create_(d,size(self)); d = self(:)%d*ANGSTROM_PER_BOHR
      call create_(F_cr,size(self)); F_cr = RE(self%F_calc)
      call create_(F_ci,size(self)); F_ci = IM(self%F_calc)
      call create_(F_ph,size(self)); F_ph = F_phase_angle_in_degrees_(self)
      call create_(F_dr,size(self)); F_dr = RE(self%F_disp)
      call create_(F_di,size(self)); F_di = IM(self%F_disp)

      ! Make table
      call create_(table,14)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"stl"); call set_values_(table(4),stl)
      call set_heading_(table(5),"d"); call set_values_(table(5),d)
      call set_heading_(table(6),"F_calc"); call set_values_(table(6),F_cr)
      call set_heading_(table(7),"F_calc"); call set_values_(table(7),F_ci)
      call set_heading_(table(8),"Phase"); call set_values_(table(8),F_ph)
      call set_heading_(table(9),"F_disp"); call set_values_(table(9),F_dr)
      call set_heading_(table(10),"F_disp"); call set_values_(table(10),F_di)
      call set_heading_(table(11),"F_pred"); call set_values_(table(11),self(:)%F_pred)
      call set_heading_(table(12),"F_corr"); call set_values_(table(12),self(:)%F_corr)
      call set_heading_(table(13),"F_exp"); call set_values_(table(13),self(:)%F_exp)
      call set_heading_(table(14),"F_sigma"); call set_values_(table(14),self(:)%F_sigma)

      ! Subheadings
      call set_subhead_(table(4),"/A^-1")
      call set_subhead_(table(5),"/A")
      call set_subhead_(table(6),"Re")
      call set_subhead_(table(7),"Im")
      call set_subhead_(table(8),"/deg")
      call set_subhead_(table(9),"Re")
      call set_subhead_(table(10),"Im")
      call set_subhead_(table(11),"(scaled)")
      call set_subhead_(table(12),"(exp)")
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(F_di)
      call destroy_(F_dr)
      call destroy_(F_ph)
      call destroy_(F_ci)
      call destroy_(F_cr)
      call destroy_(d)
      call destroy_(stl)
   end subroutine

   subroutine put_intensity_data(self)
   ! Output the intensity data
      VEC(type(REFLECTION_TYPE),:), IN :: self
      call put_I_statistics_(self)
      if (use_multiple_scale_factors_(self)) then; call put_I_group_data_(self)
      else; call put_I_nogrp_data_(self)
      end if
   end subroutine

   subroutine put_I_statistics(self,show_GoF2)
   ! Output the intensity goodness of fit statistics
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN, optional, IN :: show_GoF2
      REAL :: GoF2
      BIN :: show
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:put_I_statistics ... No I_exp data!")
   ENSURE(have_I_pred_(self),"VEC{REFLECTION}:put_I_statistics ... No I_pred data!")
      call flush_(stdout)
      call text_(stdout,"Model statistics based on intensities:")
      call flush_(stdout)
      call text_(stdout,"Note: be careful when using these chi^2 values")
      call text_(stdout,"after a Hirshfeld-atom refinement, the number of")
      call text_(stdout,"parameters N_p will not be correct.")
      call flush_(stdout)
      call show_(stdout,"R_sigma(I)                   =",I_r_sigma_(self))
      call show_(stdout,"R(I)                         =",I_r_factor_(self))
      call show_(stdout,"Rw(I)                        =",I_weighted_r_factor_(self))
      show = TRUE
      if (present(show_GoF2)) show = show_GoF2
      if (show) then
         GoF2 = I_GoF2_(self)
         call show_(stdout,"chi^2(N_p=1)(I)              =",GoF2)
         call show_(stdout,"Goodness of fit(N_p=1)(I)    =",sqrt(GoF2))
      end if
   end subroutine

   subroutine put_I_free_stats(self,show_GoF2)
   ! Output the intensity goodness of fit statistics
      VEC(type(REFLECTION_TYPE),:), IN :: self
      BIN, optional, IN :: show_GoF2
      REAL :: GoF2
      BIN :: show
   ENSURE(have_nonzero_I_exp_(self),"VEC{REFLECTION}:put_I_free_stats ... No I_exp data!")
   ENSURE(have_I_pred_(self),"VEC{REFLECTION}:put_I_free_stats ... No I_pred data!")
      call flush_(stdout)
      call text_(stdout,"Model statistics based on free intensities:")
      call flush_(stdout)
      call text_(stdout,"Note: be careful when using these chi^2 values")
      call text_(stdout,"after a Hirshfeld-atom refinement, the number of")
      call text_(stdout,"parameters N_p will not be correct.")
      call flush_(stdout)
      call show_(stdout,"Free R_sigma(I)              =",I_r_sigma_(self))
      call show_(stdout,"Free R(I)                    =",I_r_factor_(self))
      call show_(stdout,"Free Rw(I)                   =",I_weighted_r_factor_(self))
      show = TRUE
      if (present(show_GoF2)) show = show_GoF2
      if (show) then
         GoF2 = I_GoF2_(self)
         call show_(stdout,"Free chi^2(N_p=1)(I)         =",GoF2)
         call show_(stdout,"Free GOF(N_p=1)(I)           =",sqrt(GoF2))
      end if
   end subroutine

   subroutine put_I_nogrp_data(self)
   ! Output the intensity data, with only one scale factor groups
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: I_z

      ! Get data
      call create_(I_z,size(self)); I_z = I_z_(self)

      ! Make table
      call create_(table,8)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"I_calc"); call set_values_(table(4),self(:)%I_calc)
      call set_heading_(table(5),"I_pred"); call set_values_(table(5),self(:)%I_pred)
      call set_heading_(table(6),"I_exp"); call set_values_(table(6),self(:)%I_exp)
      call set_heading_(table(7),"I_sigma"); call set_values_(table(7),self(:)%I_sigma)
      call set_heading_(table(8),"I_z"); call set_values_(table(8),I_z)
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(I_z)
   end subroutine

   subroutine put_I_group_data(self)
   ! Output the intensity data, with multiple scale factor groups
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: I_z

      ! Get data
      call create_(I_z,size(self)); I_z = I_z_(self)

      ! Make table
      call create_(table,9)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"Scale"); call set_values_(table(4),self(:)%group)
      call set_heading_(table(5),"I_calc"); call set_values_(table(5),self(:)%I_calc)
      call set_heading_(table(6),"I_pred"); call set_values_(table(6),self(:)%I_pred)
      call set_heading_(table(7),"I_exp"); call set_values_(table(7),self(:)%I_exp)
      call set_heading_(table(8),"I_sigma"); call set_values_(table(8),self(:)%I_sigma)
      call set_heading_(table(9),"I_z"); call set_values_(table(9),I_z)
      call put_(table)

      ! Subheadings
      call set_subhead_(table(4),"group")

      ! Clean
      call destroy_(table)
      call destroy_(I_z)
   end subroutine

   subroutine put_I_pred_data(self)
   ! Output only the predicted intensity data
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Make table
      call create_(table,5)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"I_calc"); call set_values_(table(4),self(:)%I_calc)
      call set_heading_(table(5),"I_pred"); call set_values_(table(5),self(:)%I_pred)
      call put_(table)

      ! Subheadings
      call set_subhead_(table(4),"Re")
      call set_subhead_(table(5),"Im")

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_I_exp_data(self)
   ! Output only the experimental intensity data
      VEC(type(REFLECTION_TYPE),:), IN :: self
      if (use_multiple_scale_factors_(self)) then; call put_I_exp_group_data_(self)
      else; call put_I_exp_nogrp_data_(self)
      end if
   end subroutine

   subroutine put_I_exp_nogrp_data(self)
   ! Output only the experimental intensity data, where only
   ! one scale factor (scale group) is used.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Make table
      call create_(table,5)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"I_exp"); call set_values_(table(4),self(:)%I_exp)
      call set_heading_(table(5),"I_sigma"); call set_values_(table(5),self(:)%I_sigma)
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_I_exp_group_data(self)
   ! Output only the experimental intensity data where multiple scale
   ! factors (scale groups) are used.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Make table
      call create_(table,6)
      call set_heading_(table(1),"h"); call set_values_(table(1),self(:)%h1)
      call set_heading_(table(2),"k"); call set_values_(table(2),self(:)%h2)
      call set_heading_(table(3),"l"); call set_values_(table(3),self(:)%h3)
      call set_heading_(table(4),"Scale"); call set_values_(table(4),self(:)%group)
      call set_heading_(table(5),"I_exp"); call set_values_(table(5),self(:)%I_exp)
      call set_heading_(table(6),"I_sigma"); call set_values_(table(6),self(:)%I_sigma)
      call put_(table)

      ! Subheadings
      call set_subhead_(table(4),"group")

      ! Clean
      call destroy_(table)
   end subroutine


!  =======
!  Put CIF
!  =======

   subroutine put_CIF(self,threshold,wavelength)
   ! Put reflection information in CIF format
      VEC(type(REFLECTION_TYPE),:), IN :: self
      STR(len=*), IN :: threshold
      REAL, IN :: wavelength
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: F_cr,F_ci,F_ph,F_z,stl,d

      ! Put wavelength and other refln data
      call put_CIF_refln_data_(self,threshold,wavelength)

      ! Actual hkl's ...
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"    _refln_index_h")
      call text_(stdout,"    _refln_index_k")
      call text_(stdout,"    _refln_index_l")
      call text_(stdout,"    _refln_index_group")
      call text_(stdout,"    _refln_stl")
      call text_(stdout,"    _refln_d_spacing")
      call text_(stdout,"    _refln_A_calc")
      call text_(stdout,"    _refln_B_calc")
      call text_(stdout,"    _refln_phase_calc")
      call text_(stdout,"    _refln_F_calc")
      call text_(stdout,"    _refln_F_meas")
      call text_(stdout,"    _refln_F_sigma")
      call text_(stdout,"    _refln_delta_F_on_sigma")

      ! Get data
      stl = self(:)%stl*BOHR_PER_ANGSTROM
      d = self(:)%d*ANGSTROM_PER_BOHR
      F_cr = RE(self%F_calc)
      F_ci = IM(self%F_calc)
      F_ph = F_phase_angle_in_degrees_(self)
      F_z = F_z_(self)

      ! Make table
      call create_(table,13)
      call set_values_(table(1),self(:)%h1)
      call set_values_(table(2),self(:)%h2)
      call set_values_(table(3),self(:)%h3)
      call set_values_(table(4),self(:)%group)
      call set_values_(table(5),stl)
      call set_values_(table(6),d)
      call set_values_(table(7),F_cr)
      call set_values_(table(8),F_ci)
      call set_values_(table(9),F_ph)
      call set_values_(table(10),self(:)%F_pred)
      call set_values_(table(11),self(:)%F_exp)
      call set_values_(table(12),self(:)%F_sigma)
      call set_values_(table(13),F_z)

      ! Body
      call put_body_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_CIF_refln_data(self,threshold,wavelength)
   ! Put reflection information in CIF format
      VEC(type(REFLECTION_TYPE),:), IN :: self
      STR(len=*), IN :: threshold
      REAL, IN :: wavelength
      REAL :: lambda
      REAL :: fcmax
      call flush_(stdout)
      call text_(stdout,"# ===========")
      call text_(stdout,"# Reflections")
      call text_(stdout,"# ===========")
      lambda = wavelength
      call convert_to_(lambda,"angstrom")
      call flush_(stdout)
      call show_(stdout,"_diffrn_radiation_wavelength       ",lambda,dots=FALSE)
      fcmax = maxval(abs(self(:)%F_calc))
      call flush_(stdout)
      call show_(stdout,"_shelx_F_calc_maximum              ",fcmax,dots=FALSE)
      call flush_(stdout)
      call show_(stdout,"_reflns_d_resolution_low           ",d_min_(self)*ANGSTROM_PER_BOHR,dots=FALSE)
      call show_(stdout,"_reflns_d_resolution_high          ",d_max_(self)*ANGSTROM_PER_BOHR,dots=FALSE)
      call show_(stdout,"_reflns_limit_h_min                ",minval(self(:)%h1),dots=FALSE)
      call show_(stdout,"_reflns_limit_h_max                ",maxval(self(:)%h1),dots=FALSE)
      call show_(stdout,"_reflns_limit_k_min                ",minval(self(:)%h2),dots=FALSE)
      call show_(stdout,"_reflns_limit_k_max                ",maxval(self(:)%h2),dots=FALSE)
      call show_(stdout,"_reflns_limit_l_min                ",minval(self(:)%h3),dots=FALSE)
      call show_(stdout,"_reflns_limit_l_max                ",maxval(self(:)%h3),dots=FALSE)
      call show_(stdout,"_reflns_number_gt                  ",size(self),dots=FALSE)
      call show_(stdout,"_reflns_number_total               ",size(self),dots=FALSE)
      call show_(stdout,"_reflns_threshold_expression       ",threshold,dots=FALSE)
      call flush_(stdout)
      call text_(stdout,"_reflns_special_details            ")
      call text_(stdout,";")
      call text_(stdout,"* Specify symmetry and Friedel pair averaging *")
      call flush_(stdout)
      call text_(stdout,". Only reflections which satisfy the threshold expression")
      call text_(stdout,"  are listed below, and only they are considered observed,")
      call text_(stdout,"  thus the *_gt, *_all, & *_total data are always the same")
      call text_(stdout,";")
   end subroutine

   subroutine put_shelx_hkl3(self)
   ! Put reflection information in shelx hkl3 format
      VEC(type(REFLECTION_TYPE),:), IN :: self
      INT :: i
      do i = 1,size(self)
         call put_(stdout,self(i)%h1,width=4)
         call put_(stdout,self(i)%h2,width=4)
         call put_(stdout,self(i)%h3,width=4)
         call put_(stdout,self(i)%F_exp)
         call put_(stdout,self(i)%F_sigma)
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_shelx_hkl4(self)
   ! Put reflection information in shelx hkl4 format
      VEC(type(REFLECTION_TYPE),:), IN :: self
      INT :: i
      do i = 1,size(self)
         call put_(stdout,self(i)%h1,width=4)
         call put_(stdout,self(i)%h2,width=4)
         call put_(stdout,self(i)%h3,width=4)
         call put_(stdout,F2_exp_(self(i)))
         call put_(stdout,F2_sigma_(self(i)))
         call flush_(stdout)
      end do
   end subroutine


!  =============
!  Gnuplot files
!  =============
!  Shared by the QQ plot below and by the F_calc plots in DIFFRACTION_DATA.PUT,
!  which calls them on its own .reflections. Neither uses self -- they are here
!  because this is the module both callers can already reach, and because a
!  cross-module call needs an ordinary method rather than a selfless one.
!
!  Every data file these describe has the same layout: column 1 = x,
!  column 2 = y, columns 3-5 = h, k, l. Carrying the Miller indices alongside
!  the point is what lets gnuplot label an outlier with the reflection that
!  caused it -- which is the whole point of looking at these plots.

   function outlier_threshold(self,y,n_worst) result (res)
   ! Return the "n_worst"-th largest absolute value in "y", i.e. the cutoff at
   ! or above which exactly "n_worst" points lie. "n_worst" defaults to six.
   !
   ! Used as the threshold above which a point is labelled with its (h k l).
   ! Deliberately data-adaptive rather than a fixed number of sigma: only some
   ! of these plots have a y axis in units of sigma (F_z does, Delta_F does
   ! not), so no single constant is meaningful across all of them. This always
   ! labels the worst few, whatever the units.
   !
   ! NOTE: the default lives here, in the one routine that uses it, rather than
   ! as a macro in include/macros.in -- a constant there is #included by every
   ! module and so costs a whole-tree recompile whenever it is touched.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      VEC(REAL,:), IN :: y
      INT, IN, optional :: n_worst
      REAL :: res
      VEC(REAL,:), allocatable :: a
      INT :: n,k,n_lab
      n_lab = 6
      if (present(n_worst)) n_lab = n_worst
      n = size(y)
      if (n==0) then
         res = ZERO
         return
      end if
      call create_(a,n)
      a = abs(y)
      call sort_(a)  ! ascending
      k = min(n_lab,n)

      ! Midway between the k-th and (k+1)-th largest, NOT the k-th itself.
      !
      ! The threshold is written into the gnuplot script as a decimal, and the
      ! script tests abs(y)>=thr. If thr were exactly the k-th largest value,
      ! rounding it to a printable number can push it a hair ABOVE that value --
      ! and the very point which defines the threshold then fails its own test.
      ! Measured, not theorised: with thr = the 6th largest, nh3 labelled five
      ! points on the QQ plot instead of six. Halfway between neighbours cannot
      ! round the wrong side of either.
      if (n>k) then
         res = HALF*(a(n-k+1)+a(n-k))
      else
         res = a(1)  ! everything is labelled anyway
      end if
      call destroy_(a)
   end function

   subroutine nice_axis_range(self,lo,hi,a0,a1,non_negative)
   ! Return in "a0" and "a1" an axis range covering [lo,hi]: padded by 6% and
   ! then rounded outward to a whole tic on the 1-2-2.5-5 ladder, which is the
   ! ladder gnuplot's own autoscaling walks. So the axis reads -3 .. 3 rather
   ! than -2.83 .. 2.83.
   !
   ! Tonto has to fix the range itself instead of leaving gnuplot to autoscale,
   ! because .put_hkl_label_file computes label positions as fractions of the
   ! plot area -- and that mapping is only known once the range is known.
   !
   ! If "non_negative", the lower bound is clamped at zero. Padding below the
   ! smallest F^o would otherwise start that axis at -10, and a negative
   ! structure factor amplitude is meaningless.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      REAL, IN :: lo,hi
      REAL, OUT :: a0,a1
      BIN, IN, optional :: non_negative
      REAL :: span,pad,step,e,m
      BIN :: nn
      nn = FALSE
      if (present(non_negative)) nn = non_negative
      span = hi-lo

      ! Degenerate: every point at the same value. Invent a span so the
      ! arithmetic below stays finite and the single point is visible.
      if (span<=ZERO) span = max(abs(hi),ONE)
      pad = 0.06d0*span
      a0 = lo-pad
      a1 = hi+pad
      span = (a1-a0)/EIGHT  ! aim for about eight tics
      e = TEN**floor(log10(span))
      m = span/e
      if (m<=ONE) then
         step = e
      else if (m<=TWO) then
         step = TWO*e
      else if (m<=2.5d0) then
         step = 2.5d0*e
      else if (m<=FIVE) then
         step = FIVE*e
      else
         step = TEN*e
      end if
      a0 = floor(a0/step)*step
      a1 = ceiling(a1/step)*step
      if (nn AND a0<ZERO) a0 = ZERO
   end subroutine

   subroutine put_hkl_label_file(self,file,x,y,hkl,threshold,x0,x1,y0,y1,n_placed)
   ! Write "file", which says where each outlier's (h k l) label goes and where
   ! its leader line runs. Columns are
   !
   !    1,2  the data point            (x,y)
   !    3,4  the centre of the label   (x,y)
   !    5,6  the end of the leader     (x,y)  -- on the label's box edge
   !    7    the label text, quoted
   !
   ! A point is labelled when abs(y) >= "threshold". "n_placed" returns how many
   ! were, so the caller can leave the label passes out of the plot command
   ! altogether when there are none -- gnuplot aborts a whole plot if any one of
   ! its data files turns out to hold no points.
   !
   ! WHY THIS EXISTS. gnuplot has no label auto-placement: "with labels" takes
   ! one fixed offset for every label in the pass, so two outliers close
   ! together print on top of each other and neither can be read. (Checked, not
   ! assumed -- "offset variable" is not a thing, and "set jitter" moves points,
   ! not text.) Tonto is better placed to do it anyway, since it has the data.
   !
   ! THE ALGORITHM. Worst point first, so the worst outlier gets the best spot.
   ! For each, try twelve directions at three short radii and score every
   ! candidate: a heavy penalty for falling outside the frame or landing on a
   ! label already placed, a light one for covering a data point, and a mild
   ! preference for a short leader and for sideways over vertical. Take the
   ! cheapest, remember its box, move on. Greedy and deterministic; with six
   ! labels there is nothing to be gained by iterating.
   !
   ! The label box is sized from the character count and the font size, so it is
   ! an estimate -- gnuplot alone knows the true text extent. Good enough at six
   ! labels, and the leader line makes an imperfect guess obvious rather than
   ! wrong.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      STR(len=*), IN :: file
      VEC(REAL,:), IN :: x,y
      MAT(INT,:,:), IN :: hkl
      REAL, IN :: threshold
      REAL, IN :: x0,x1,y0,y1
      INT, OUT :: n_placed
      VEC(REAL,:), allocatable :: gx,gy
      VEC(REAL,:), allocatable :: bx,by,bw,bh
      VEC(INT,:), allocatable :: idx
      VEC(REAL,:), allocatable :: a
      VEC(REAL,12) :: ux,uy
      VEC(REAL,3) :: rad
      STR(len=STR_SIZE) :: txt
      STR(len=1) :: dq
      INT :: n,i,j,m,ia,ir
      REAL :: cw,ch,hw,hh,px,py,cx,cy,ex,ey
      REAL :: r,cost,best_cost,best_x,best_y,ddx,ddy,t
      n = size(x)

      ! Unit directions: sideways first, then the diagonals, then vertical.
      ! Listed rather than derived from an angle so no trigonometry is needed.
      ux = [ONE,-ONE,0.866d0,-0.866d0,0.866d0,-0.866d0,HALF,-HALF,HALF,-HALF,ZERO,ZERO]
      uy = [ZERO,ZERO,HALF,HALF,-HALF,-HALF,0.866d0,0.866d0,-0.866d0,-0.866d0,ONE,-ONE]
      rad = [0.045d0,0.075d0,0.110d0]

      ! Label box, as a fraction of the plot side, for the ",8" font below.
      cw = 0.0057d0  ! width of one character
      ch = 0.016d0  ! height of one line

      ! A double quote. The label "(h k l)" contains spaces, and gnuplot splits
      ! a data line on whitespace unless the field is quoted -- with DOUBLE
      ! quotes specifically. Single quotes do NOT work there, whatever they do
      ! elsewhere in gnuplot: "(1 1 1)" reads as one field, '(1 1 1)' reads as
      ! the three fields '(1, 1, 1', and the label draws as "'(1". Measured on
      ! gnuplot 6.0.2 by rendering it, not by watching for an error -- there is
      ! no error, just a truncated label.
      dq = """"

      ! Worst first
      call create_(a,n)
      a = abs(y)
      call create_(idx,n)
      call quick_sort_(a,idx,decreasing_order=TRUE)
      n_placed = count(a>=threshold)
      call create_(gx,n); call create_(gy,n)
      gx = (x-x0)/(x1-x0)
      gy = (y-y0)/(y1-y0)
      call create_(bx,n_placed); call create_(by,n_placed)
      call create_(bw,n_placed); call create_(bh,n_placed)
      call redirect_(stdout,file)
      call text_(stdout,"# Where the (h k l) labels go, placed by Tonto.")
      call text_(stdout,"# 1st,2nd columns: the data point")
      call text_(stdout,"# 3rd,4th columns: the centre of the label")
      call text_(stdout,"# 5th,6th columns: the end of its leader line")
      call text_(stdout,"# 7th column:      the label")
      call text_(stdout,"#")
      call text_(stdout,"# Move a label by editing columns 3 and 4; the leader follows.")
      do i = 1,n_placed
         j = idx(i)
         txt = "("//trim(to_str_(hkl(j,1)))//" "//trim(to_str_(hkl(j,2)))//" "//trim(to_str_(hkl(j,3)))//")"
         hw = HALF*len_trim(txt)*cw
         hh = HALF*ch
         px = gx(j)
         py = gy(j)
         best_cost = TEN**30
         best_x = px
         best_y = py
         do ir = 1,3
            do ia = 1,12
               r = rad(ir)
               cx = px+r*ux(ia)
               cy = py+r*uy(ia)
               cost = ZERO

            ! Stay inside the frame
               if (cx-hw<0.01d0 OR cx+hw>0.99d0 OR cy-hh<0.01d0 OR cy+hh>0.99d0) cost = cost+HUNDRED

            ! Never cover a label already placed
               do m = 1,i-1
                  ddx = (bw(m)+hw+0.008d0)-abs(cx-bx(m))
                  ddy = (bh(m)+hh+0.004d0)-abs(cy-by(m))
                  if (ddx>ZERO AND ddy>ZERO) cost = cost+50.0d0+TEN*ddx*ddy
               end do

            ! Prefer not to sit on the data
               do m = 1,n
                  if (abs(cx-gx(m))<hw+0.004d0 AND abs(cy-gy(m))<hh+0.004d0) cost = cost+ONE
               end do

            ! Prefer a short leader, and sideways over vertical
               cost = cost+THREE*r+0.6d0*abs(uy(ia))
               if (cost<best_cost) then
                  best_cost = cost
                  best_x = cx
                  best_y = cy
               end if
            end do
         end do
         cx = best_x
         cy = best_y
         bx(i) = cx; by(i) = cy
         bw(i) = hw; bh(i) = hh

         ! The leader stops on the box edge nearest the point, not at its
         ! centre, so the line does not strike through the text.
         ddx = px-cx
         ddy = py-cy
         t = ONE
         if (ddx/=ZERO) t = min(t,hw/abs(ddx))
         if (ddy/=ZERO) t = min(t,hh/abs(ddy))
         ex = cx+ddx*t
         ey = cy+ddy*t

         ! Back to data coordinates, which is what gnuplot plots in
         call put_(stdout,x0+px*(x1-x0))
         call put_(stdout,y0+py*(y1-y0))
         call put_(stdout,x0+cx*(x1-x0))
         call put_(stdout,y0+cy*(y1-y0))
         call put_(stdout,x0+ex*(x1-x0))
         call put_(stdout,y0+ey*(y1-y0))
         call put_(stdout," "//dq//trim(txt)//dq)
         call flush_(stdout)
      end do
      call revert_(stdout)
      call destroy_(a)
      call destroy_(idx)
      call destroy_(gx); call destroy_(gy)
      call destroy_(bx); call destroy_(by)
      call destroy_(bw); call destroy_(bh)
   end subroutine

   subroutine put_gnuplot_script(self,script,data,labels,xlabel,ylabel,x0,x1,y0,y1,n_labels,with_fit)
   ! Write the gnuplot input file "script", which plots the data file "data"
   ! over the axis range [x0,x1] by [y0,y1], and draws the "n_labels" outlier
   ! labels described by the file "labels".
   !
   ! The plot is square (1:1), as in Abrahams & Keve style figures -- see e.g.
   ! Fig. S8 of Davidson et al. (2022) Acta Cryst B 78 397.
   !
   ! If "with_fit", a least-squares line is fitted and its equation is printed
   ! centred along the bottom of the plot.
   !
   ! NOTE: the script sets no terminal and no output, on purpose. Run by hand it
   ! draws to your screen:
   !
   !    gnuplot -persist 'script'
   !
   ! and SYSTEM:call_gnuplot forces a PNG terminal on the command line instead.
   ! One script, both uses -- rather than a script that can only ever do one.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      STR(len=*), IN :: script,data,labels
      STR(len=*), IN :: xlabel,ylabel
      REAL, IN :: x0,x1,y0,y1
      INT, IN :: n_labels
      BIN, IN, optional :: with_fit
      STR(len=STR_SIZE) :: cmd
      BIN :: fit
      fit = FALSE
      if (present(with_fit)) fit = with_fit
      call redirect_(stdout,script)
      call text_(stdout,"# ==================")
      call text_(stdout,"# Gnuplot input file")
      call text_(stdout,"# ==================")
      call text_(stdout,"#")
      call text_(stdout,"# To see this plot type:")
      call text_(stdout,"#")
      call text_(stdout,"#    gnuplot -persist '"//trim(script)//"'")
      call text_(stdout,"#")
      call text_(stdout,"# Alter the lines below to get the plot you want.")

      ! Check
      call flush_(stdout)
      call text_(stdout,"# Check version")
      call text_(stdout,"if (GPVAL_VERSION<6.0) print 'Use gnuplot 6.0 or greater'; exit")

      ! Axis labels
      call flush_(stdout)
      call text_(stdout,"set xlabel '"//trim(xlabel)//"'")
      call text_(stdout,"set ylabel '"//trim(ylabel)//"'")

      ! Plot options. "set size square" is the 1:1 aspect ratio.
      call flush_(stdout)
      call text_(stdout,"# Plot options")
      call text_(stdout,"set size square")
      call text_(stdout,"unset key")
      call text_(stdout,"set grid xtics")
      call text_(stdout,"set grid ytics")

      ! Axis ranges.
      !
      ! Fixed here rather than left to gnuplot's autoscaling because the label
      ! positions in the "labels" file were computed against them: they are only
      ! meaningful if the mapping from data to plot area is the one Tonto
      ! assumed. .nice_axis_range picks bounds on the same 1-2-2.5-5 tic ladder
      ! gnuplot would have, so this costs nothing in appearance.
      call flush_(stdout)
      call text_(stdout,"# Axis ranges. The (h k l) labels were placed against these,")
      call text_(stdout,"# so widen them and the labels drift; rerun Tonto to replace.")
      call text_(stdout,"set xrange ["//trim(to_str_(x0,"e14.6"))//":"//trim(to_str_(x1,"e14.6"))//"]")
      call text_(stdout,"set yrange ["//trim(to_str_(y0,"e14.6"))//":"//trim(to_str_(y1,"e14.6"))//"]")

      ! Line of best fit
      if (fit) then
         call flush_(stdout)
         call text_(stdout,"# Line of best fit")
         call text_(stdout,"set fit quiet")
         call text_(stdout,"l(x) = a*x + b")
         ! NOTE: b starts at 0.1, not 0. gnuplot cannot use a parameter whose
         ! start value is exactly zero -- its step is taken relative to that
         ! value, so the normal matrix is singular in b. It then reports
         ! "Singular matrix in Invert_RtR", pins b at 1e-30 and fits the slope
         ! alone. The line is silently forced through the origin and the label
         ! prints a fitted-looking "+ 0.000". Measured on nh3: the true
         ! intercept is 0.035. The error message never reaches the user either,
         ! since SYSTEM:call_gnuplot sends stderr to /dev/null.
         call text_(stdout,"a = 1")
         call text_(stdout,"b = 0.1")
         call text_(stdout,"fit l(x) '"//trim(data)//"' using 1:2 via a,b")
         call flush_(stdout)
         call text_(stdout,"# Its equation, centred along the bottom of the plot")
         cmd = "set label 1 sprintf('y = %.3f x + %.3f', a, b)"
         cmd = trim(cmd)//" at graph 0.5, graph 0.045 center front"
         call text_(stdout,trim(cmd))
      end if

      ! Plot.
      !
      ! Emitted as several gnuplot continuation lines rather than one long one,
      ! because NO line handed to stdout.text may exceed BSTR_SIZE (256): the
      ! TEXTFILE buffer it is copied into is STR(len=BSTR_SIZE), and overrunning
      ! it segfaults inside memmove with no diagnostic. The single-line version
      ! of this command was 265 characters and did exactly that.
      !
      ! The trailing backslashes are safe. The C preprocessor still runs over the
      ! generated Fortran (CLAUDE.md S1), but it only splices a backslash that is
      ! the last character on a *source* line -- here each one is inside a string
      ! literal, followed by the closing quote and bracket.
      !
      ! Which component carries the trailing comma depends on which comes last,
      ! since a plot command may not end on ", \".
      call flush_(stdout)
      call text_(stdout,"# Plot")
      call text_(stdout,"plot '"//trim(data)//"' using 1:2 \")
      if (fit OR n_labels>0) then
         call text_(stdout,"        with points pt 7 ps 0.3 lc rgb 'dark-violet', \")
      else
         call text_(stdout,"        with points pt 7 ps 0.3 lc rgb 'dark-violet'")
      end if
      if (fit) then
         if (n_labels>0) then
            call text_(stdout,"     l(x) with lines lw 2 lc rgb 'skyblue', \")
         else
            call text_(stdout,"     l(x) with lines lw 2 lc rgb 'skyblue'")
         end if
      end if

      ! The leader lines, then the labels they lead to. Two passes over the
      ! same file: gnuplot draws one style per pass.
      if (n_labels>0) then
         call text_(stdout,"     '"//trim(labels)//"' using 1:2:($5-$1):($6-$2) \")
         call text_(stdout,"        with vectors nohead lw 1 lc rgb 'gray50', \")
         call text_(stdout,"     '"//trim(labels)//"' using 3:4:7 \")
         call text_(stdout,"        with labels center font ',8' tc rgb 'red'")
      end if

      ! PDF
      call flush_(stdout)
      call text_(stdout,"# PDF output")
      call text_(stdout,"# Uncomment, or do it from the Qt window")
      call text_(stdout,"# set terminal pdfcairo size 5,5")
      call text_(stdout,"# set output '"//trim(script)//".pdf'")
      call text_(stdout,"# replot")
      call revert_(stdout)
   end subroutine

   subroutine put_xy_plot_files(self,data,x,y,hkl,xlabel,ylabel,stem,with_fit,x_non_negative)
   ! Given a data file "data" that has already been written -- x, y, h, k, l in
   ! five columns -- make everything else the plot needs, and then draw it:
   !
   !    <stem>.labels    where each outlier's (h k l) goes, and its leader
   !    <stem>.gnuplot   the script
   !    <stem>.png       the picture, by actually running gnuplot
   !
   ! "stem" defaults to "data", which is what every plot here wants except the
   ! QQ plot, whose data file has been called <prefix>.QQ_plot_with_hkl since
   ! long before there was a script to go with it.
   !
   ! One call per plot, so adding a plot is a data file plus a line. Set
   ! "x_non_negative" for a quantity that cannot go below zero, such as F^o or
   ! sin(theta)/lambda, so the axis is not padded into negative territory.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      STR(len=*), IN :: data
      VEC(REAL,:), IN :: x,y
      MAT(INT,:,:), IN :: hkl
      STR(len=*), IN :: xlabel,ylabel
      STR(len=*), IN, optional :: stem
      BIN, IN, optional :: with_fit
      BIN, IN, optional :: x_non_negative
      STR(len=STR_SIZE) :: base,labels,script,image
      REAL :: x0,x1,y0,y1,thr
      INT :: n_lab
      base = data
      if (present(stem)) base = stem
      labels = trim(base)//".labels"
      script = trim(base)//".gnuplot"
      image = trim(base)//".png"
      call nice_axis_range_(self,minval(x),maxval(x),x0,x1,non_negative=x_non_negative)
      call nice_axis_range_(self,minval(y),maxval(y),y0,y1)

      ! The worst few points get labelled with the reflection responsible
      thr = outlier_threshold_(self,y)
      call put_hkl_label_file_(self,labels,x,y,hkl,thr,x0,x1,y0,y1,n_lab)
      call put_gnuplot_script_(self,script,data,labels,xlabel,ylabel,x0,x1,y0,y1,n_lab,with_fit=with_fit)
      call call_gnuplot_(tonto,script,image)
   end subroutine


!  ========
!  QQ plots
!  ========

   subroutine put_labelled_F_qq_plot(self,prefix)
   ! Output a qq plot to the text file.
   ! It is a plot of the experimental quantile vs expected quantile.
   !
   ! "prefix" heads every file written here. CRYSTAL passes the job name, so two
   ! jobs run in one directory no longer overwrite each other's plots.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      STR(len=*), optional, IN :: prefix
      MAT(REAL,:,:), allocatable :: grid
      MAT(INT,:,:), allocatable :: hkl
      STR(len=STR_SIZE) :: head,stem,data
      INT :: n
      head = "stdout"
      if (present(prefix)) head = prefix

      ! The data file keeps its historical name; everything else hangs off the
      ! stem, as it does for the F_calc plots in DIFFRACTION_DATA.PUT.
      stem = trim(head)//".QQ_plot"
      data = trim(head)//".QQ_plot_with_hkl"
      call create_(hkl,size(self),3)
      do n = 1,size(self)
         hkl(n,:) = Miller_indices_(self,n)
      end do
      call create_(grid,size(self),2)
      call make_F_qq_plot_grid_(self,grid,hkl)
      call redirect_(stdout,data)
      call text_(stdout,"# Labelled QQ plot:")
      call text_(stdout,"# ")
      call text_(stdout,"# 1st column: F_z(normal)")
      call text_(stdout,"# 2nd column: F_z(exp)")
      call text_(stdout,"# 3rd column: h")
      call text_(stdout,"# 4th column: k")
      call text_(stdout,"# 5th column: l")
      call text_(stdout,"# F_z = (F_exp - F_pred)/F_sigma")
      call text_(stdout,"# Note: Fexp & F_pred may have dispersion removed")
      call text_(stdout,"# ")
      call text_(stdout,"# Abrahams & Keve (1971) Acta Cryst A 27 p. 157 ")
      call text_(stdout,"# ")
      do n = 1,size(self)
         call put_(stdout,grid(n,1))
         call put_(stdout,grid(n,2))
         call put_(stdout,hkl(n,1))
         call put_(stdout,hkl(n,2))
         call put_(stdout,hkl(n,3))
         call flush_(stdout)
      end do
      call revert_(stdout)

      ! The label file, the gnuplot input file, then the picture itself
      call put_xy_plot_files_(self,data,grid(:,1),grid(:,2),hkl,"F_z (Gaussian)","F_z (exp) = ( F^{o} - F^{c} ) / {/Symbol s}",stem=stem,with_fit=TRUE)
      call destroy_(grid)
      call destroy_(hkl)
   end subroutine

   subroutine make_F_qq_plot_grid(self,grid,hkl)
   ! Make the grid for the Q-Q plot, which is a plot of the expected
   ! deviation in standard deviation units versus the actual
   ! deviations, assuming that the expected devaitions are normally
   ! distributed. grid(1,:) contains the expected deviation d0j,
   ! grid(2,:) contains actual deviation dj.
      VEC(type(REFLECTION_TYPE),:), IN :: self
      MAT(REAL,:,:), INOUT :: grid
      MAT(INT,:,:), optional, OUT :: hkl
      REAL :: p
      INT :: i,j
   ENSURE(size(grid,1)==size(self),"VEC{REFLECTION}:make_F_qq_plot_grid ... grid: wrong dim1")
   ENSURE(size(grid,2)==2,"VEC{REFLECTION}:make_F_qq_plot_grid ... grid: wrong dim2")

      ! grid(:,1) ! theoretical z's
      ! grid(:,2) ! calculated z's
      do i = 1,size(self)
         grid(i,2) = F_z_(self(i))
      end do

      ! Sort array from lowest z to highest
      if (present(hkl)) then
         ENSURE(size(hkl,1)==size(self),"VEC{REFLECTION}:make_F_qq_plot_grid ... size of index array incorrect")
         ENSURE(size(hkl,2)==3,"VEC{REFLECTION}:make_F_qq_plot_grid ... size of index array incorrect")
         do i = 1,size(self)-1
            do j = i+1,size(self)
               if (grid(j,2)<grid(i,2)) then
                  call swap_elements_(grid(:,2),i,j)
                  call swap_rows_(hkl,i,j)
               end if
            end do
         end do
      else
         call sort_(grid(:,2))
      end if
      do j = 1,size(self)  ! expected quantile, d0j
         p = (TWO*(size(self)-j)+ONE)/(TWO*size(self))  ! cumulative probability
         grid(size(self)-j+1,1) = z_from_cumulative_p_(p)
      end do
   end subroutine

end module
