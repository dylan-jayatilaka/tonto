!===================================================================
!
! VEC{ATOM}: ATOM vectors
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
!===================================================================
module VEC_ATOM_MODULE

#  include "macros"

   include "vec{atom}.use"

 ! USE dftd3_api

   implicit none

   include "vec{atom}.int"


   ! Atom connection table
   VEC(type(EVEC_INT_TYPE),:), allocatable, private :: connections_for

   ! Atom group connection table, to save work
! group_connections_for :: VEC{EVEC{INT}}@, private

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self,dim)
   ! Allocate vector
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{ATOM}:create_0 ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{ATOM}:create_0 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
   end subroutine

   PURE subroutine create_1(self,lb,ub)
   ! Allocate the vector with lower bound "lb", upper bound "ub"
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      INT, IN :: lb,ub

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(lb:ub),stat=status)
      ENSURE(status==0,"VEC{ATOM}:create_1 ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(lb:ub))
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{ATOM}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_ptr_part_(self(a))
      end do
   end subroutine

   PURE subroutine destroy_bases(self)
   ! Destroy all the bases
      VEC(type(ATOM_TYPE),:), INOUT :: self
      call destroy_basis_part_(self)
      call destroy_slaterbasis_part_(self)
      call destroy_coppensbasis_part_(self)
   end subroutine

   PURE subroutine destroy_basis_part(self)
   ! Destroy the gaussian basis parts of self
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_(self(a)%basis)
      end do
   end subroutine

   PURE subroutine destroy_slaterbasis_part(self)
   ! Destroy the slaterbasis parts of self
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_(self(a)%slaterbasis)
      end do
   end subroutine

   PURE subroutine destroy_coppensbasis_part(self)
   ! Destroy the coppensbasis parts of self
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_(self(a)%coppensbasis)
      end do
   end subroutine

   PURE subroutine destroy_NAO_matrices(self)
   ! Destroy the (unique-atom) NAOs
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_NAO_matrices_(self(a))
      end do
   end subroutine

   PURE subroutine destroy_ANO_matrices(self)
   ! Destroy the (unique-atom) ANOs
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_ANO_matrices_(self(a))
      end do
   end subroutine

   PURE subroutine destroy_interpolators(self)
   ! Destroy the (unique-atom) ANO interpolators
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_interpolators_(self(a))
      end do
   end subroutine

   PURE subroutine destroy_shell_info(self)
   ! Destroy the (unique-atom) shell info
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_shell_info_(self(a))
      end do
   end subroutine

   PURE subroutine destroy_atom_data(self)
   ! Destroy the (non-unique) ANO data
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call destroy_atom_data_(self(a))
      end do
   end subroutine


!  ======================
!  Shrink, expand, append
!  ======================

   PURE subroutine shrink(self,dim,chop_start)
   ! Shrink allocatable "self" to dimension "dim" from the end.
   ! Contents are retained. If "chop_start" is present and TRUE,
   ! "self" is chopped from the start, and the contents from the end
   ! are retained.
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: chop_start
      VEC(type(ATOM_TYPE),:), allocatable :: new
      INT :: lb,ub
      BIN :: chop_end
   ENSURE(allocated(self),"VEC{ATOM}:shrink ... no self array")
   ENSURE(dim<=size(self),"VEC{ATOM}:shrink ... dim too large")

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT, IN :: dim
      BIN, optional, IN :: grow_start
      INT :: nc
      VEC(type(ATOM_TYPE),:), allocatable :: new
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
         ENSURE(dim>size(self),"VEC{ATOM}:expand ... dim not large enough")
         ENSURE(dim>0,"VEC{ATOM}:expand ... dim is not +ve")
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

   pure subroutine append_0(self,v)
   ! Append the contents of vector "v" to allocatable "self".
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      VEC(type(ATOM_TYPE),:), IN :: v

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = v
      else
         self = [self,v]
      end if
   end subroutine

   pure subroutine append_1(self,value)
   ! Appends the scalar "value" onto the end of allocatable "self".
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      type(ATOM_TYPE), IN :: value

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) then
         self = [value]
      else
         self = [self,value]
      end if
   end subroutine

   PURE subroutine prune_element(self,k)
   ! Removes the element at position "k" from allocatable "self".
   ! Elements after that position are moved downward.
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT, IN :: k
      VEC(type(ATOM_TYPE),:), allocatable :: old
      INT :: lb,ub

      ! Bounds

      ! The following code is inherited from VEC{OBJECT}
      lb = lbound(self,dim=1)
      ub = ubound(self,dim=1)

      ! Save
      call create_(old,lb,ub)
      old = self

      ! Transfer
      call create_(self,lb,ub-1)
      self(lb:k-1) = old(lb:k-1)
      self(k:ub-1) = old(k+1:ub)
   end subroutine


!  ==========
!  Set labels
!  ==========

   PURE subroutine set_atom_data_from_labels(self,labels)
   ! Set "labels" for the atoms and also their atomic numbers.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(STR(len=*),:), IN :: labels
      INT :: a
   ENSURE(size(labels)==size(self),"VEC{ATOM}:set_atom_data_from_labels ... wrong length for labels")
      do a = 1,size(self)
         call set_atom_data_from_label_(self(a),labels(a))
      end do
   end subroutine

   pure subroutine set_is_cap_atom(self,val)
   ! Set whether the atom is a "capping" atom for fragmantation
   ! calculations.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      BIN, IN :: val
      self(:)%is_cap_atom = val
   end subroutine

   PURE subroutine set_labels(self,labels)
   ! Set the "labels" ONLY for the atoms; use the above routine to set the
   ! atomic numbers as well.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(STR(len=*),:), IN :: labels
   ENSURE(size(labels)==size(self),"VEC{ATOM}:set_labels ... wrong length for labels")
      self%label = labels
   end subroutine

   PURE subroutine set_recognised_labels(self,list)
   ! Set "list" to be the list of recognised labels. If all labels 
   ! are recognised then "list" is not created.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(INT,:), allocatable, OUT :: list
      INT :: n
      do n = 1,size(self)
         if (NOT has_recognised_label_(self(n))) cycle
         call append_(list,n)
      end do
      if (size(list)==size(self)) call destroy_(list)
   end subroutine

   pure subroutine set_use_interpolators(self,use_interp)
   ! Read whether to use interpolators for spherical atom densities
      VEC(type(ATOM_TYPE),:), INOUT :: self
      BIN, IN :: use_interp
      INT :: a
      self%use_interpolator = use_interp
      if (has_all_slater_bases_(self)) then
         do a = 1,size(self)
            call set_use_interpolators_(self(a)%slaterbasis,use_interp)
         end do
      end if
      if (has_all_coppens_bases_(self)) then
         do a = 1,size(self)
            call set_use_interpolators_(self(a)%coppensbasis,use_interp)
         end do
      end if
   end subroutine

   PURE subroutine set_unique_tags(self)
   ! Set the unique tags.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: a,k,n,nk

      ! Get the atom Z list
      call make_atom_Z_list_(self,Z_list)

      ! Label atoms consecutively Xn, X=symbol, n=1,...
      do k = 1,size(Z_list)
         nk = size(Z_list(k)%element)
         do n = 1,nk

            ! Atom index
            a = Z_list(k)%element(n)

            ! Skip dummy
            if (is_a_dummy_atom_(self(a))) cycle

            ! Set tag
            if (nk==1) then; self(a)%tag = trim(chemical_symbol_(self(a)))
            else; self(a)%tag = trim(chemical_symbol_(self(a)))//trim(to_str_(n))
            end if
         end do
      end do

      ! Clean
      call destroy_(Z_list)
   end subroutine

   PURE subroutine set_basis_suffix(self,suffix)
   ! Set the "basis_label" to be a library-style label using "suffix"
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: suffix
      STR(len=STR_SIZE) :: val
      INT :: a
      do a = 1,size(self)
         val = library_basis_label_(self(a),suffix)
         call set_basis_label_(self(a),val)
      end do
   end subroutine


!  =========
!  Set kinds
!  =========

   PURE subroutine update_kinds(self,n_kind)
   ! Set the kinds and whether the atoms are basis-unique.
   ! . They are the same kind if they have the same kind of basis
   !   and same atomic number.
   ! . This is used for ANO or other QM type calculations.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, optional, OUT :: n_kind
      INT :: k,a,b

      ! Initially all different kind
      do a = 1,size(self)
         call set_kind_(self(a),a)
      end do

      ! Kind index
      k = 0

      ! Loop on atoms "a"
      do a = 1,size(self)

         ! Already found this kind?
         if (self(a)%kind<=k) cycle

         ! New kind
         k = k+1
         call set_kind_(self(a),k)
         call set_is_unique_kind_(self(a),TRUE)

         ! Set non-unique kinds
         do b = a+1,size(self)
            if (self(b)%kind<=k) cycle
            if (NOT same_kind_of_atoms_(self,a,b)) cycle
            call set_kind_(self(b),k)
            call set_is_unique_kind_(self(b),FALSE)
         end do
      end do
      if (present(n_kind)) n_kind = k
   end subroutine

   PURE subroutine set_is_unique_kind(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      BIN, IN :: val
      INT :: a
      do a = 1,size(self)
         call set_is_unique_kind_(self(a),val)
      end do
   end subroutine

   PURE subroutine set_basis_kind(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: val
      INT :: a
      do a = 1,size(self)
         call set_basis_kind_(self(a),val)
      end do
   end subroutine


!  ==========================
!  Set whether data/info made
!  ==========================

   subroutine set_atom_data_made(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      BIN, IN :: val

      ! The following code is inherited from OBJECT
      self%atom_data_made = val
   end subroutine


!  =============
!  Set positions
!  =============

   PURE subroutine set_positions_from_vector(self,v)
   ! Set the positions from "v", a flat vector, where the xyz
   ! positions increment fastest
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: v
   ENSURE(size(v)==3*size(self),"VEC{ATOM}:set_positions_from_vector ... wrong size, v")
      call set_positions_to_(self,reshape(v,[3,size(self)]))
   end subroutine

   PURE subroutine set_positions_to(self,mx)
   ! Set the positions from matrix "mx". The shape must be (3xN).
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,:,:), IN :: mx
      INT :: a
   ENSURE(size(mx,1)==3,"VEC{ATOM}:set_positions_to ... dim1 is not 3")
   ENSURE(size(mx,2)==size(self),"VEC{ATOM}:set_positions_to ... dim2 is incompatible")
      do a = 1,size(self)
         call set_position_(self(a),mx(:,a))
      end do
   end subroutine

   PURE subroutine set_position_errors_to(self,me)
   ! Set the position_erroros from matrix "mx". The shape must be (3xN).
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,:,:), IN :: me
      INT :: a
   ENSURE(size(me,1)==3,"VEC{ATOM}:set_position_errors_to ... dim1 is not 3")
   ENSURE(size(me,2)==size(self),"VEC{ATOM}:set_position_errors_to ... dim2 is incompatible")
      do a = 1,size(self)
         call set_position_error_(self(a),me(:,a))
      end do
   end subroutine


!  ====================
!  Set site occupancies
!  ====================

   PURE subroutine set_site_occupancies(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: val
      INT :: a
   ENSURE(size(val)==size(self),"VEC{ATOM}:set_site_occupancies ... wrong size for occupancies")
      do a = 1,size(self)
         call set_site_occupancy_(self(a),val(a))
      end do
   end subroutine

   PURE subroutine set_site_disorder_groups(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(INT,:), IN :: val
      INT :: a
   ENSURE(size(val)==size(self),"VEC{ATOM}:set_site_disorder_groups ... wrong size for occupancies")
      do a = 1,size(self)
         call set_site_disorder_group_(self(a),val(a))
      end do
   end subroutine


!  ===============
!  Set axis system
!  ===============

   PURE subroutine set_axis_system_to(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: val
      INT :: a
      do a = 1,size(self)
         call set_axis_system_to_(self(a),val)
      end do
   end subroutine

   PURE subroutine set_pos_axis_system_to(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: val
      INT :: a
      do a = 1,size(self)
         call set_pos_axis_system_to_(self(a),val)
      end do
   end subroutine

   PURE subroutine set_ADP_axis_system_to(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: val
      INT :: a
      do a = 1,size(self)
         call set_ADP_axis_system_to_(self(a),val)
      end do
   end subroutine

   PURE subroutine set_ADPn_axis_system_to(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: val
      INT :: a
      do a = 1,size(self)
         call set_ADPn_axis_system_to_(self(a),val)
      end do
   end subroutine


!  ===============
!  Use IAM ITC FFs
!  ===============

   pure subroutine set_use_IAM_ITC_FFs(self,val)
   ! Set whether to use Tabulated or density_mx form factors
      VEC(type(ATOM_TYPE),:), INOUT :: self
      BIN, IN :: val
      self%use_IAM_ITC_FFs = val
   end subroutine


!  ============
!  pADP/ADP set
!  ============

   PURE subroutine set_ADP_tensor(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      REAL, IN :: val
      INT :: a
      do a = 1,size(self)
         call set_ADP_tensor_(self(a),val)
      end do
   end subroutine


! Set U_iso

   PURE subroutine set_isotropic_H_ADP(self)
   ! Set the isotropic hydrogen ADPs
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         if (self(a)%atomic_number==1) then
            call set_isotropic_ADP_(self(a))
         end if
      end do
   end subroutine

   PURE subroutine set_isotropic_ADPs(self,tags)
   ! Set isotropic ADPs for the atoms wich have tags given in "tags"
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(STR(len=*),:), IN :: tags
      INT :: a
      do a = 1,size(self)
         if (any(self(a)%tag==tags)) then
            call set_isotropic_ADP_(self(a))
         end if
      end do
   end subroutine

   PURE subroutine set_U_iso_0(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      REAL, IN :: val
      INT :: a
      do a = 1,size(self)
         call set_U_iso_(self(a),val)
      end do
   end subroutine

   PURE subroutine set_U_iso_1(self,val)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: val
      INT :: a
      do a = 1,size(self)
         call set_U_iso_(self(a),val(a))
      end do
   end subroutine

   PURE subroutine set_U_iso_error(self,err)
   ! Generic set
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: err
      INT :: a
   ENSURE(size(err)==size(self),"VEC{ATOM}:set_U_iso_error ... wrong size for occupancies")
      do a = 1,size(self)
         call set_U_iso_error_(self(a),err(a))
      end do
   end subroutine


! Reset pADPs

   PURE subroutine reset_pADPs_and_errors_0(self,dim)
   ! Allocate the positional-ADP parameters to a set size "dim".
   ! NOTE: assigning 4th order tags implies 3rd order.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, IN :: dim
      INT :: n,a

      ! Make sure at least 9 long.
      n = max(dim,9)

      ! Reset each pADP
      do a = 1,size(self)
         call reset_pADPs_and_errors_(self(a),n)
      end do
   end subroutine

   PURE subroutine reset_pADPs_and_errors_1(self,anharm3_tags,anharm4_tags)
   ! Allocate the positional-ADP parameters based on the given tags.
   ! NOTE: assigning 4th order tags implies 3rd order.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(STR(len=*),:), allocatable, IN :: anharm3_tags
      VEC(STR(len=*),:), allocatable, IN :: anharm4_tags
      INT :: t,a
      STR(len=STR_SIZE) :: tag

      ! Find the 4th-order tags, resize pADP_vector, and set to 0
      ! If the atoms are already fourth order do nothing.
      if (allocated(anharm4_tags)) then
         do t = 1,size(anharm4_tags)
            tag = anharm4_tags(t)
            a = index_of_(self(:)%tag,tag)
            ENSURE(a>0,"VEC{ATOM}:reset_pADPs_and_errors_1 ... no atom for 4th-order tag: "//trim(tag))
            call reset_pADPs_and_errors_(self(a),34)
         end do
      end if

      ! Find the 3rd-order tags, resize pADP_vector, and set to 0
      ! If they were already 3rd or 4th order, do nothing
      if (allocated(anharm3_tags)) then
         do t = 1,size(anharm3_tags)
            tag = anharm3_tags(t)
            a = index_of_(self(:)%tag,tag)
            ENSURE(a>0,"VEC{ATOM}:reset_pADPs_and_errors_1 ... no atom for 3rd-order tag: "//trim(tag))
            call reset_pADPs_and_errors_(self(a),19)
         end do
      end if

      ! Make sure at least 9 long.
      ! For positions (3) & ADPs (6)
      do a = 1,size(self)
         call reset_pADPs_and_errors_(self(a),9)
      end do
   end subroutine


! Set pADPs

   PURE subroutine set_pADP_vector_to(self,X)
   ! Set the pADP vector from vector "X".
   ! NOTE: positions and ADP_tensors are set!
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,:), IN :: X
      INT :: a,f,l
   ENSURE(size(X)==no_of_pADPs_(self),"VEC{ATOM}:set_pADP_vector_to ... wrong size, X")
      l = 0
      do a = 1,size(self)

         ! Limits
         f = l+1
         l = l+no_of_pADPs_(self(a))

         ! Leaky
         call set_pADP_vector_to_(self(a),X(f:l))
      end do
   end subroutine

   PURE subroutine set_pADP_errors_to(self,covariance_mx,H_U_iso)
   ! Set the pADP errors from vector "dX".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,:,:), IN :: covariance_mx
      BIN, IN :: H_U_iso
      INT :: a,f,l,n
   ENSURE(size(covariance_mx,1)==no_of_pADPs_(self),"VEC{ATOM}:set_pADP_errors_to ... wrong size, cov")
      l = 0
      do a = 1,size(self)

         ! Limits; make sure we have refinement setting correct!
         n = no_of_pADPs_(self(a))
         f = l+1
         l = l+n

         ! Not leaky
         call set_pADP_errors_to_(self(a),covariance_mx(f:l,f:l),H_U_iso)
      end do
   end subroutine

   PURE subroutine zero_pADP_errors(self)
   ! Set the pADP_errors
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call zero_pADP_error_(self(a))
      end do
   end subroutine

   subroutine check_position_and_pADP(self)
   ! Check if the .position and .pADP vector are consistent
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: a
      do a = 1,size(self)
         call check_position_and_pADP_(self(a))
      end do
   end subroutine


!  =====
!  Modes
!  =====

   subroutine initialize_local_H_modes(self,L)
   ! Initialize local H modes as columns. These come in triples.
   ! The first is the XH stretch. The next two are the two XH bends.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,:,:), OUT :: L
      MAT(REAL,3,3) :: unit
      VEC(REAL,3) :: r1,r2,r3
      INT :: h,i,x,fh,lh
   ENSURE(size(L,1)==3*size(self),"VEC{ATOM}:initialize_local_H_modes ... wrong size, dim1")
   ENSURE(size(L,2)==3*count(self%atomic_number==1),"VEC{ATOM}:initialize_local_H_modes ... wrong size, dim2")

      ! Make the connection table
      call make_connection_table_(self)  ! NOT PURE

      ! Initialise L
      L = ZERO

      ! Set unit vectors
      call to_unit_mx_(unit)

      ! Make the H modes
      i = 0
      lh = 0
      do h = 1,size(self)
         fh = lh+1
         lh = lh+3

         ! Only look for a = H atom
         if (self(h)%atomic_number>1) cycle

         ! Make sure H atom connects one other
         if (size(connections_for(h)%element)>1) then
            DIE("VEC{ATOM}:initialize_local_H_modes ... H atom connected tomore than one other")
         end if

         ! Get XH vector
         x = connections_for(h)%element(1)
         r1 = self(h)%position-self(x)%position
         call normalize_(r1)

         ! Get the two perpendicular vectors
         call to_cross_product_of_(r2,r1,unit(:,1))
         if (is_zero_(r2)) call to_cross_product_of_(r2,r1,unit(:,2))
         call normalize_(r2)
         call to_cross_product_of_(r3,r1,r2)

         ! Set L
         L(fh:lh,i+1) = r1
         L(fh:lh,i+2) = r2
         L(fh:lh,i+3) = r3

         ! Next H mode
         i = i+3
      end do

      ! Clean
      call destroy_(connections_for)
   end subroutine


!  ============
!  pADP/ADP put
!  ============

   PURE subroutine put_pADP_vector_to(self,X,fac)
   ! Put the whole pADP vector into vector "X".
   ! If present, include length-conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: X
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(X)==no_of_pADPs_(self),"VEC{ATOM}:put_pADP_vector_to ... wrong size, X")
      l = 0
      do a = 1,size(self)

         ! Limits
         f = l+1
         l = l+no_of_pADPs_(self(a))

         ! Set
         call put_pADP_vector_to_(self(a),X(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_pADP_errors_to(self,dX,fac)
   ! Put all the pADP errors into vector "dX".
   ! If present, include length-conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: dX
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(dX)==no_of_pADPs_(self),"VEC{ATOM}:put_pADP_errors_to ... wrong size, dX")
      l = 0
      do a = 1,size(self)

         ! Limits
         f = l+1
         l = l+no_of_pADPs_(self(a))

         ! Set
         call put_pADP_errors_to_(self(a),dX(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_pos_vector_to(self,X,fac)
   ! Put the position vector into matrix "X".
   ! If present, include length-conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: X
      REAL, optional, IN :: fac
      INT :: a
   ENSURE(size(X,1)==size(self),"VEC{ATOM}:put_pos_vector_to ... wrong size, X")
   ENSURE(size(X,2)==3,"VEC{ATOM}:put_pos_vector_to ... wrong size, X")
      do a = 1,size(self)
         call put_pos_vector_to_(self(a),X(a,:),fac)
      end do
   end subroutine

   PURE subroutine put_pos_errors_to_0(self,dX,fac)
   ! Put the position errors into matrix "dX".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dX
      REAL, optional, IN :: fac
      INT :: a
   ENSURE(size(dX,1)==size(self),"VEC{ATOM}:put_pos_errors_to_0 ... wrong size, dX")
   ENSURE(size(dX,2)==3,"VEC{ATOM}:put_pos_errors_to_0 ... wrong size, dX")
      do a = 1,size(self)
         call put_pos_errors_to_(self(a),dX(a,:),fac)
      end do
   end subroutine

   PURE subroutine put_pos_errors_to_1(self,dX,esd,fac)
   ! Put the position errors into matrix "dX" from the supplied
   ! "esd" vector made from a external refinement.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dX
      VEC(REAL,:), IN :: esd
      REAL, optional, IN :: fac
      INT :: a,k
   ENSURE(size(dX,1)==size(self),"VEC{ATOM}:put_pos_errors_to_1 ... wrong size, dX")
   ENSURE(size(dX,2)==3,"VEC{ATOM}:put_pos_errors_to_1 ... wrong size, dX")
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_pos_errors_to_1 ... wrong size, esd")
      k = 0
      do a = 1,size(self)
         dX(a,1) = esd(k+1)
         dX(a,2) = esd(k+2)
         dX(a,3) = esd(k+3)
         k = k+no_of_pADPs_(self(a))
      end do
      if (present(fac)) dX = fac*dX
   end subroutine

   PURE subroutine put_pADP2_vector_to(self,X,fac)
   ! Put the second-order pADP vector into matrix "X".
   ! If present, include length-conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: X
      REAL, optional, IN :: fac
      INT :: a
   ENSURE(size(X,1)==size(self),"VEC{ATOM}:put_pADP2_vector_to ... wrong size, X")
   ENSURE(size(X,2)==9,"VEC{ATOM}:put_pADP2_vector_to ... wrong size, X")
      do a = 1,size(self)
         call put_pADP2_vector_to_(self(a),X(a,:),fac)
      end do
   end subroutine

   PURE subroutine put_pADP2_errors_to(self,dX,fac)
   ! Put the pADP errors into matrix "dX".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dX
      REAL, optional, IN :: fac
      INT :: a
   ENSURE(size(dX,1)==size(self),"VEC{ATOM}:put_pADP2_errors_to ... wrong size, dX")
   ENSURE(size(dX,2)==9,"VEC{ATOM}:put_pADP2_errors_to ... wrong size, dX")
      do a = 1,size(self)
         call put_pADP2_errors_to_(self(a),dX(a,:),fac)
      end do
   end subroutine

   PURE subroutine put_ADP2_errors_to_0(self,dU,fac,esd)
   ! Put the ADP2 errors into matrix "dU" from the supplied
   ! "esd" vector made from a external refinement.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dU
      REAL, optional, IN :: fac
      VEC(REAL,:), IN :: esd
      INT :: a,k
   ENSURE(size(dU,1)==size(self),"VEC{ATOM}:put_ADP2_errors_to_0 ... wrong dim1, dU")
   ENSURE(size(dU,2)==6,"VEC{ATOM}:put_ADP2_errors_to_0 ... wrong dim2, dU")
      k = 0
      do a = 1,size(self)
         dU(a,1) = esd(k+4)
         dU(a,2) = esd(k+5)
         dU(a,3) = esd(k+6)
         dU(a,4) = esd(k+7)
         dU(a,5) = esd(k+8)
         dU(a,6) = esd(k+9)
         k = k+no_of_pADPs_(self(a))
      end do

      ! Change units
      if (present(fac)) dU = fac*fac*dU
   end subroutine

   PURE subroutine put_ADP2_errors_to_1(self,dU,list,fac,esd)
   ! Put the ADP2 errors into matrix "dU" from the supplied
   ! "esd" vector made from a external refinement. "list" is
   ! the sub list of atoms to use
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dU
      VEC(INT,:), IN :: list
      REAL, optional, IN :: fac
      VEC(REAL,:), IN :: esd
      INT :: i,a,k
   ENSURE(size(dU,1)==size(list),"VEC{ATOM}:put_ADP2_errors_to_1 ... wrong dim1, dU")
   ENSURE(size(dU,2)==6,"VEC{ATOM}:put_ADP2_errors_to_1 ... wrong dim2, dU")
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP2_errors_to_1 ... wrong size, esd")
      k = 0
      do i = 1,size(list)
         a = list(i)
         k = no_of_pADPs_up_to_atom_(self,a)
         dU(i,1) = esd(k+4)
         dU(i,2) = esd(k+5)
         dU(i,3) = esd(k+6)
         dU(i,4) = esd(k+7)
         dU(i,5) = esd(k+8)
         dU(i,6) = esd(k+9)
      end do

      ! Change units
      if (present(fac)) dU = fac*fac*dU
   end subroutine

   PURE subroutine put_ADP2_vector_to_0(self,U,fac)
   ! Put the second-order ADP parameters into vector "U".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: U
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(U)==6*size(self),"VEC{ATOM}:put_ADP2_vector_to_0 ... wrong size, U")
      l = 0
      do a = 1,size(self)
         f = l+1
         l = l+6
         call put_ADP2_vector_to_(self(a),U(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_ADP2_vector_to_1(self,U,list,fac)
   ! Put the second-order ADP parameters into vector "U".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: U
      VEC(INT,:), IN :: list
      REAL, optional, IN :: fac
      INT :: i,a
   ENSURE(size(U,1)==size(list),"VEC{ATOM}:put_ADP2_vector_to_1 ... wrong dim1, U")
   ENSURE(size(U,2)==6,"VEC{ATOM}:put_ADP2_vector_to_1 ... wrong dim2, U")
      do i = 1,size(list)
         a = list(i)
         call put_ADP2_vector_to_(self(a),U(i,1:6),fac)
      end do
   end subroutine

   PURE subroutine put_ADP2_errors_to_2(self,dU,fac)
   ! Put the ADP2 errors into vector "dU".
   ! If present, include length-conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: dU
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(dU)==6*size(self),"VEC{ATOM}:put_ADP2_errors_to_2 ... wrong size, dU")
      l = 0
      do a = 1,size(self)
         f = l+1
         l = l+6
         call put_ADP2_errors_to_(self(a),dU(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_ADP2_errors_to_3(self,dU,list,fac)
   ! Put the ADP2 errors into vector "dU".
   ! If present, include length-conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dU
      VEC(INT,:), IN :: list
      REAL, optional, IN :: fac
      INT :: i,a
   ENSURE(size(dU,1)==size(list),"VEC{ATOM}:put_ADP2_errors_to_3 ... wrong dim1, dU")
   ENSURE(size(dU,2)==6,"VEC{ATOM}:put_ADP2_errors_to_3 ... wrong dim2, dU")
      do i = 1,size(list)
         a = list(i)
         call put_ADP2_errors_to_(self(a),dU(i,1:6),fac)
      end do
   end subroutine

   PURE subroutine put_ADP2_vector_to_2(self,U,fac)
   ! Put the second-order ADP vector into matrix "U".
   ! If present, include length-conversion factor "fac".
   ! This routine used for making tables
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: U
      REAL, optional, IN :: fac
      INT :: a
   ENSURE(size(U,1)==size(self),"VEC{ATOM}:put_ADP2_vector_to_2 ... wrong size, U")
   ENSURE(size(U,2)==6,"VEC{ATOM}:put_ADP2_vector_to_2 ... wrong size, U")
      do a = 1,size(self)
         call put_ADP2_vector_to_(self(a),U(a,:),fac)
      end do
   end subroutine

   PURE subroutine put_ADP2_errors_to_4(self,dU,fac)
   ! Put the ADP2 errors into matrix "dU".
   ! If present, include length conversion factor "fac".
   ! This routine used for making tables
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dU
      REAL, optional, IN :: fac
      INT :: a
   ENSURE(size(dU,1)==size(self),"VEC{ATOM}:put_ADP2_errors_to_4 ... wrong size, dU")
   ENSURE(size(dU,2)==6,"VEC{ATOM}:put_ADP2_errors_to_4 ... wrong size, dU")
      do a = 1,size(self)
         call put_ADP2_errors_to_(self(a),dU(a,:),fac)
      end do
   end subroutine

   PURE subroutine put_ADP3_vector_to_0(self,U,fac)
   ! Put the ADP3 parameters into vector "U".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: U
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(U)==no_of_ADP3s_(self),"VEC{ATOM}:put_ADP3_vector_to_0 ... wrong size, X")
      l = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! Set ADP3's
         f = l+1
         l = l+10
         call put_ADP3_vector_to_(self(a),U(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_ADP3_errors_to_0(self,dU,fac)
   ! Put the ADP3 errors into vector "dU".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: dU
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(dU)==no_of_ADP3s_(self),"VEC{ATOM}:put_ADP3_errors_to_0 ... wrong size, dU")
      l = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! Set ADP3's
         f = l+1
         l = l+10
         call put_ADP3_errors_to_(self(a),dU(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_ADP3_vector_to_1(self,U,fac)
   ! Put the ADP3 parameters into matrix "U".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: U
      REAL, optional, IN :: fac
      INT :: a,i
   ENSURE(size(U,1)==no_of_ADP3_atoms_(self),"VEC{ATOM}:put_ADP3_vector_to_1 ... wrong size, U")
   ENSURE(size(U,2)==10,"VEC{ATOM}:put_ADP3_vector_to_1 ... wrong size, U")
      i = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! Set ADP3's
         i = i+1
         call put_ADP3_vector_to_(self(a),U(i,:),fac)
      end do
   end subroutine

   PURE subroutine put_ADP3_errors_to_1(self,dU,fac)
   ! Put the ADP3 errors into matrix "dU".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dU
      REAL, optional, IN :: fac
      INT :: a,i
   ENSURE(size(dU,1)==no_of_ADP3_atoms_(self),"VEC{ATOM}:put_ADP3_errors_to_1 ... wrong size, dU")
   ENSURE(size(dU,2)==10,"VEC{ATOM}:put_ADP3_errors_to_1 ... wrong size, U")
      i = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! Set ADP3's
         i = i+1
         call put_ADP3_errors_to_(self(a),dU(i,:),fac)
      end do
   end subroutine

   PURE subroutine put_ADP4_vector_to_0(self,U,fac)
   ! Put the ADP4 parameters into vector "U".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: U
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(U)==no_of_ADP4s_(self),"VEC{ATOM}:put_ADP4_vector_to_0 ... wrong size, X")
      l = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! Set ADP4's
         f = l+1
         l = l+15
         call put_ADP4_vector_to_(self(a),U(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_ADP4_errors_to_0(self,dU,fac)
   ! Put the ADP4 errors into vector "dU".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: dU
      REAL, optional, IN :: fac
      INT :: a,f,l
   ENSURE(size(dU)==no_of_ADP4s_(self),"VEC{ATOM}:put_ADP4_errors_to_0 ... wrong size, dU")
      l = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! Set ADP4's
         f = l+1
         l = l+10
         call put_ADP4_errors_to_(self(a),dU(f:l),fac)
      end do
   end subroutine

   PURE subroutine put_ADP4_vector_to_1(self,U,fac)
   ! Put the ADP4 parameters into matrix "U".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: U
      REAL, optional, IN :: fac
      INT :: a,i
   ENSURE(size(U,1)==no_of_ADP4_atoms_(self),"VEC{ATOM}:put_ADP4_vector_to_1 ... wrong size, U")
   ENSURE(size(U,2)==15,"VEC{ATOM}:put_ADP4_vector_to_1 ... wrong size, U")
      i = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! Set ADP4's
         i = i+1
         call put_ADP4_vector_to_(self(a),U(i,:),fac)
      end do
   end subroutine

   PURE subroutine put_ADP4_errors_to_1(self,dU,fac)
   ! Put the ADP4 errors into matrix "dU".
   ! If present, include length conversion factor "fac".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: dU
      REAL, optional, IN :: fac
      INT :: a,i
   ENSURE(size(dU,1)==no_of_ADP4_atoms_(self),"VEC{ATOM}:put_ADP4_errors_to_1 ... wrong size, dU")
   ENSURE(size(dU,2)==15,"VEC{ATOM}:put_ADP4_errors_to_1 ... wrong size, dU")
      i = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! Set ADP4's
         i = i+1
         call put_ADP4_errors_to_(self(a),dU(i,:),fac)
      end do
   end subroutine


! Put pADPx atom list

   PURE subroutine put_ADP3_atom_list_to(self,list)
   ! Put the ADP3 atom list to "list".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), OUT :: list
      INT :: a,i
   ENSURE(size(list)==no_of_ADP3_atoms_(self),"VEC{ATOM}:put_ADP3_atom_list_to ... wrong size, list")
      i = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! List of atoms
         i = i+1
         list(i) = a
      end do
   end subroutine

   PURE subroutine put_ADP4_atom_list_to(self,list)
   ! Put the ADP4 atom list to "list".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), OUT :: list
      INT :: a,i
   ENSURE(size(list)==no_of_ADP4_atoms_(self),"VEC{ATOM}:put_ADP4_atom_list_to ... wrong size, list")
      i = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! List of atoms
         i = i+1
         list(i) = a
      end do
   end subroutine


!  ===============
!  Coordinate info
!  ===============

   PURE function coordinates(self,skip_dummies) result (res)
   ! Create the 3 x .dim coordinate matrix pointer
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: skip_dummies
      MAT(REAL,:,:), allocatable :: res
      INT :: n_atom
      BIN :: skip
      skip = FALSE
      if (present(skip_dummies)) skip = skip_dummies
      if (skip) then
         n_atom = no_of_nondummy_atoms_(self)
         call create_(res,3,n_atom)
         call put_coordinates_to_(self,res,positions_as_rows=FALSE,skip_dummies=TRUE)
      else
         call create_(res,3,size(self))
         call put_coordinates_to_(self,res,positions_as_rows=FALSE)
      end if
   end function

   PURE subroutine put_coordinates_to(self,mx,positions_as_rows,skip_dummies)
   ! Put the atom coordinates in a matrix "mx".
   ! If positions_as_rows=TRUE  then "mx" is dim x 3
   ! If positions_as_rows=FALSE then "mx" is 3 x dim
   ! If skip_dummies=FALSE then dim = .dim1 (default)
   ! If skip_dummies=TRUE  then dim = .no_of_nondummy_atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: mx
      BIN, IN :: positions_as_rows
      BIN, optional, IN :: skip_dummies
      INT :: i,a
      BIN :: skip
      skip = FALSE
      if (present(skip_dummies)) skip = skip_dummies
      if (NOT skip) then

         ! Put all atoms
         if (positions_as_rows) then
            ENSURE(size(mx,1)==size(self) AND size(mx,2)==3,"VEC{ATOM}:put_coordinates_to ... wrong shape, mx")
            do a = 1,size(self)
               mx(a,:) = self(a)%position
            end do
         else
            ENSURE(size(mx,2)==size(self) AND size(mx,1)==3,"VEC{ATOM}:put_coordinates_to ... wrong shape, mx")
            do a = 1,size(self)
               mx(:,a) = self(a)%position
            end do
         end if
      else

         ! Put non-dummy atoms
         if (positions_as_rows) then
            ENSURE(size(mx,1)==no_of_nondummy_atoms_(self) AND size(mx,2)==3,"VEC{ATOM}:put_coordinates_to ... wrong shape, mx")
            i = 0
            do a = 1,size(self)
               if (is_a_dummy_atom_(self(a))) cycle
               i = i+1
               mx(i,:) = self(a)%position
            end do
         else
            ENSURE(size(mx,2)==no_of_nondummy_atoms_(self) AND size(mx,1)==3,"VEC{ATOM}:put_coordinates_to ... wrong shape, mx")
            i = 0
            do a = 1,size(self)
               if (is_a_dummy_atom_(self(a))) cycle
               i = i+1
               mx(:,i) = self(a)%position
            end do
         end if
      end if
   end subroutine

   PURE function coordinate_vector(self) result (res)
   ! Return the coordinates as a flat vector "res" where the xyz
   ! positions increment fastest, useful for optimisations
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3*size(self)) :: res
      call put_coordinate_vector_to_(self,res)
   end function

   PURE subroutine put_coordinate_vector_to(self,v)
   ! Put the coordinates as a flat vector in "v" where the xyz
   ! positions increment fastest, useful for optimisations
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: v
      INT :: k,i
   ENSURE(size(v)==3*size(self),"VEC{ATOM}:put_coordinate_vector_to ... wrong size, v")
      k = 0
      do i = 1,size(self)
         v(k+1) = self(i)%position(1)
         v(k+2) = self(i)%position(2)
         v(k+3) = self(i)%position(3)
         k = k+3
      end do
   end subroutine


!  ========
!  Finalize
!  ========

   PURE subroutine update(self)
   ! Update/finalize, as far as possible.
      VEC(type(ATOM_TYPE),:), INOUT :: self

      ! Update the labels?
      call update_labels_(self)

      ! Make atom kinds/unique atoms
      ! Should we always set the kinds?
      call update_kinds_(self)

      ! Set groups
      ! Probs here? --dylan 5/14
      if (any(self(:)%group==0)) call set_connected_groups_(self)

      ! Set any dispersion coefficients
      if (xray_dispersion_entered_()) call set_tabular_dispersion_(self)
   end subroutine

   PURE subroutine update_labels(self)
   ! Update the labels and/or tags.
   ! NOTE: Only update the labels if the label data are not
   ! unique, so as to preserve previous unique labels (and
   ! hopefully kinds) which can happen if a subset of a
   ! processed atom list is being updated.
   ! NOTE: It is assumed that is you set a different label
   ! you really mean that the atom is a different kind.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      BIN :: unique_labels,unique_tags

      ! Unique labels?
      unique_labels = has_unique_labels_(self)
      unique_tags = has_unique_tags_(self)

      ! Set unique labels/tags
      if (unique_labels) then

         ! Set unique tags from labels
         if (NOT unique_tags) self(:)%tag = self(:)%label
      else  ! not unique labels

         ! Set unique tags if needed
         if (NOT unique_tags) call set_unique_tags_(self)

         ! Set labels from tags
         self(:)%label = self(:)%tag
      end if
   end subroutine


!  ==============================
!  Basis size information methods
!  ==============================

   pure function no_of_shells(self) result (res)
   ! Work out and return the number of gaussian shells in the basis
   ! set for the molecule
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+size(self(a)%basis%shell)
      end do
   end function

   PURE function no_of_shell_pairs(self) result (res)
   ! Return the number of shell pairs in the concatenated  basis set
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: n_shell
      n_shell = no_of_shells_(self)
      ENSURE(n_shell<=sqrt(TWO)*(huge(1))**HALF,"VEC{ATOM}:no_of_shell_pairs ... too many shells")
      res = n_shell*(n_shell+1)/2
   end function

   PURE function no_of_shell_quartets(self) result (res)
   ! Return the number of shell quartets in the b concatenated asis set 
   ! Note, "res" will die at 22    shells for integer(2).
   !       "res" will die at 361   shells for integer(4).
   !       "res" will die at 92681 shells for integer(8).
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: n_pairs
      n_pairs = no_of_shell_pairs_(self)
      ENSURE(n_pairs<=sqrt(TWO)*(huge(1))**HALF,"VEC{ATOM}:no_of_shell_quartets ... too many shells")
      res = n_pairs*(n_pairs+1)/2
   end function

   pure function no_of_primitive_shells(self) result (res)
   ! Work out and return the number of primitive gaussian shells in
   ! the concatenated basis sets
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+no_of_primitive_shells_(self(a)%basis)
      end do
   end function

   pure function no_of_basis_functions(self) result (res)
   ! Work out and return the number of basis functions in the concatenated
   ! basis set for the atom list.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+no_of_basis_functions_(self(a)%basis)
      end do
   end function

   pure function no_of_sph_basis_functions(self) result (res)
   ! Work out and return the number of spherical basis functions in
   ! the concatenated basis set for the atom list.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+no_of_sph_basis_functions_(self(a)%basis)
      end do
   end function

   pure function no_of_primitives(self) result (res)
   ! Work out and return the number of primitives in the concatenated
   ! basis sets.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+no_of_primitives_(self(a)%basis)
      end do
   end function

   pure function no_of_sph_primitives(self) result (res)
   ! Work out and return the number of spherical primitives in the
   ! concatenated basis sets.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+no_of_sph_primitives_(self(a)%basis)
      end do
   end function


!  =======================
!  List-based I/O Routines
!  =======================

   recursive subroutine read_list_keywords(self)
   ! This version is for allocatable arrays.
   ! Read in and process list-based keywords from "stdin". List-based keywords
   ! are those that are intended to apply to each individual element of the list
   ! through a list of "keys" stored in the associated list-element type module.
   ! NOTE: this routine will create the list, if required.
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{ATOM}:read_list_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"VEC{ATOM}:read_list_keywords ... unexpected end of file while reading list keywords")

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word

      ! Read open braces

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)  ! move past open brace
      ENSURE(word=="{","VEC{ATOM}:read_data ... expecting open bracket symbol, {")

      ! Read data (process keys)
      call read_data_sans_braces_(self)

      ! Read close brace
      call read_(stdin,word)
      ENSURE(word=="}","VEC{ATOM}:read_data ... expecting close bracket symbol, }")
   end subroutine

   subroutine read_data_sans_braces(self)
   ! This version is for allocatable arrays.
   ! Repeatedly process the "keys" to read in the data list. 
   ! Opening and closing braces are not needed.
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT :: length

      ! Get the vector length

      ! The following code is inherited from VEC{OBJECT}
      length = data_length_(self)
      if (allocated(self)) then

         ! If created, is dimension the same
         ENSURE(length==size(self),"VEC{ATOM}:read_data_sans_braces ... Mismatching new and old data lists")
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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT :: length

      ! Get the vector length

      ! The following code is inherited from VEC{OBJECT}
      length = data_length_BL_terminated_(self)
      if (allocated(self)) then

         ! If created, is dimension the same
         ENSURE(length==size(self),"VEC{ATOM}:read_data_BL_terminated ... Mismatching new and old data lists")
      else

         ! If not created, create it
         call create_(self,length)
      end if

      ! Process keys
      call process_keys_(self)

      ! Empty line there? Read it.
      call read_line_(stdin)
      ENSURE(stdin%buffer%string==" ","VEC{ATOM}:read_data_BL_terminated ... expecting blank line")
   end subroutine

   function data_length_BL_terminated(self) result (length)
   ! This version is for allocatable arrays.
   ! Read ahead in stdin to get the "length" of the data list. The
   ! data list begins with the first data item, *not* a "{" symbol.
   ! The data list must be terminated by a blank line (BL).
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT :: length
      type(ATOM_TYPE), allocatable :: tmp
      STR(len=STR_SIZE) :: word
      INT :: line,item
   ENSURE(next_item_(stdin)/="}","VEC{ATOM}:data_length_BL_terminated ... empty data list!")

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      INT :: length
      type(ATOM_TYPE), allocatable :: tmp
      STR(len=STR_SIZE) :: word
      INT :: line,item
   ENSURE(next_item_(stdin)/="}","VEC{ATOM}:data_length ... empty data list!")

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
      INT :: s
   ENSURE(allocated(self),"VEC{ATOM}:read_altered_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","VEC{ATOM}:read_altered_data ... expecting open bracket symbol: {")

      ! Read brace {

      ! The following code is inherited from VEC{OBJECT}
      call read_(stdin,word)
      do

         ! Read index
         call read_(stdin,word)

         ! End of indices
         if (word=="}") exit

         ! Get index
         ENSURE(is_int_(word),"VEC{ATOM}:read_altered_data ... expecting integer list-element index")
         s = to_int_(word)
         ENSURE(s<=size(self),"VEC{ATOM}:read_altered_data ... list-element too large")
         ENSURE(s>0,"VEC{ATOM}:read_altered_data ... list-element must be positive")

         ! Altered data for this index
         call process_keys_(self(s))
      end do
   end subroutine

   subroutine read_append_data(self)
   ! This version is for allocatable arrays.
   ! Read in a set of data to append to an existing set.
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      VEC(type(ATOM_TYPE),:), allocatable :: tmp
   ENSURE(allocated(self),"VEC{ATOM}:read_append_data ... list does not exist yet")
   ENSURE(next_item_(stdin)=="{","VEC{ATOM}:read_append_data ... expecting open bracket symbol: {")

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      type(ATOM_TYPE) :: tmp

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      BIN :: res
      type(ATOM_TYPE), allocatable :: tmp

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      VEC(STR(len=*),:) :: the_keys
      type(ATOM_TYPE), allocatable :: tmp

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      type(ATOM_TYPE), allocatable :: tmp

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      type(ATOM_TYPE), allocatable :: tmp

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
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=STR_SIZE) :: filename

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      filename = next_str_(stdin)
      call redirect_(stdin,trim(filename))
   end subroutine

   subroutine revert(self)
   ! Revert back to previous stdin file. (This code is for non-pointer self
   ! objects).
      VEC(type(ATOM_TYPE),:), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call revert_(stdin)
   end subroutine


!  ===========================
!  Non-list based I/O routines
!  ===========================

   recursive subroutine read_keywords(self)
   ! This version is for allocatable arrays.
   ! Read in and process normal (non list-type) keywords from "stdin".
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","VEC{ATOM}:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"VEC{ATOM}:read_keywords ... unexpected end of file while reading list keywords")

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
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}")  ! do nothing.
      case ("dump                          "); call dump_(self,"atoms")
      case ("find_and_put_n_terminal_atoms "); call find_and_put_N_terminal_atoms_(self)
      case ("find_and_put_protein_backbone "); call find_and_put_protein_backbone_(self)
      case ("put                           "); call put_(self)
      case ("put_bond_angle_table          "); call put_bond_angle_table_(self)
      case ("put_bond_length_table         "); call put_bond_length_table_(self)
      case ("put_coordinates               "); call put_coordinates_(self)
      case ("put_coordinates_and_charges   "); call put_coordinates_and_charges_(self)
      case ("put_invariom_labels           "); call put_invariom_labels_(self)
      case ("put_smiles_invariom_labels    "); call put_SMILES_invariom_labels_(self)
   ! case("put_mm_info                   "); .put_mm_info
      case ("put_torsion_angle_table       "); call put_torsion_angle_table_(self)
   ! case("put_protein_sequence          "); .put_protein_sequence
   ! case("put_restraint_atoms           "); .put_restraint_atoms
      case ("read_cif                      "); call read_CIF_(self)
      case ("redirect                      "); call redirect_(self)
      case ("revert                        "); call revert_(self)
      case ("set_covalent_radii_ccdc       "); call set_covalent_radii_ccdc_
      case ("use_interpolators=            "); call read_use_interpolators_(self)
      case default; allocate(tonto%known_keywords(17))
      tonto%known_keywords(1) = "}"
      tonto%known_keywords(2) = "dump                          "
      tonto%known_keywords(3) = "find_and_put_n_terminal_atoms "
      tonto%known_keywords(4) = "find_and_put_protein_backbone "
      tonto%known_keywords(5) = "put                           "
      tonto%known_keywords(6) = "put_bond_angle_table          "
      tonto%known_keywords(7) = "put_bond_length_table         "
      tonto%known_keywords(8) = "put_coordinates               "
      tonto%known_keywords(9) = "put_coordinates_and_charges   "
      tonto%known_keywords(10) = "put_invariom_labels           "
      tonto%known_keywords(11) = "put_smiles_invariom_labels    "
      tonto%known_keywords(12) = "put_torsion_angle_table       "
      tonto%known_keywords(13) = "read_cif                      "
      tonto%known_keywords(14) = "redirect                      "
      tonto%known_keywords(15) = "revert                        "
      tonto%known_keywords(16) = "set_covalent_radii_ccdc       "
      tonto%known_keywords(17) = "use_interpolators=            "
      call unknown_(tonto,word,"VEC{ATOM}:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


! CIF

   subroutine read_CIF_0(self)
   ! Read information from a Crystallographic Information File whose
   ! name is read from "stdin".
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=STR_SIZE) :: name

      ! Read name cif name from stdin
      call read_(stdin,name)

      ! Read CIF
      call read_CIF_(self,name)
   end subroutine

   subroutine read_CIF_1(self,name)
   ! Read information from a Crystallographic Information File "name"
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      STR(len=*), IN :: name
      type(CIF_TYPE), allocatable :: cif
      BIN :: found

      ! Create the CIF and find data
      call create_(cif,name)
      call open_(cif)
      call find_crystal_data_block_(cif,found)
      ENSURE(found,"VEC{ATOM}:read_CIF_1 ... no crystal data block found")

      ! Read
      call read_CIF_(self,cif)

      ! Clean
      call destroy_(cif)
   end subroutine

   subroutine read_CIF_2(self,cif)
   ! Read information from a Crystallographic Information File "name"
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      if (NOT cif%is_mmCIF) then
         call read_smCIF_(self,cif)
      else
         call read_mmCIF_(self,cif)
      end if
   end subroutine


! smCIF

   subroutine read_smCIF(self,cif)
   ! Read information from a small-molecule Crystallographic
   ! Information File "name"
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      type(CIF_TYPE), INOUT :: cif

      ! Read atom positions
      call read_smCIF_atoms_(self,cif)

      ! Read ADPs including U_iso/B_iso
      call read_smCIF_ADPs_(self,cif)

      ! Read dispersion
      call read_smCIF_dispersion(cif)
   end subroutine

   subroutine read_smCIF_atoms(self,cif)
   ! Read information from a small-molecule Crystallographic
   ! Information File, "cif".
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: fail
      fail = FALSE
      call read_smCIF_atoms_cart_(self,cif,fail)
      if (NOT fail) return
      fail = FALSE
      call read_smCIF_atoms_xtal_(self,cif,fail)
      ENSURE(NOT fail,"VEC{ATOM}:read_smCIF_atoms ... failed")
   end subroutine

   subroutine read_smCIF_atoms_cart(self,cif,fail)
   ! Read Tonto information from a small-molecule "cif".  Cartesian
   ! format extends existing items in the standard CIF dictionary.
   ! NOTE: self is destroyed and created from this file!
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: fail
      BIN :: fl,fc,fe,fo
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs,labels
      MAT(REAL,:,:), allocatable :: pos,err

      ! Read the site labels FIRST because these seem to correlate with
      ! the anisotropic U's.
      call read_looped_item_(cif,"_atom_site_Cartn_label",labels,fl)
      fail = NOT fl
      if (fail) return

      ! Destroy the old atom list (if there)
      call destroy_(self)

      ! Create the new atom list
      call create_(self,size(labels))
      call set_atom_data_from_labels_(self,labels)
      call destroy_(labels)

      ! Read the coordinates
      call create_(IDs,3)
      IDs = ["_atom_site_Cartn_x","_atom_site_Cartn_y","_atom_site_Cartn_z"]
      call read_looped_items_(cif,IDs,pos,found=fc)
      call destroy_(IDs)
      fail = NOT fc
      if (fail) return

      ! Set the positions & space for errors
      call set_pos_axis_system_to_(self,"cartesian")
      call reset_pADPs_and_errors_(self,9)
      call convert_from_(pos,"angstrom")
! for process cif and c23 xml these need to be transformed into
! fractional otherwise tonto cannot see that they are the same atom as
! in the XML. Lorraine
      call set_positions_to_(self,pos)
      call destroy_(pos)

      ! Set the pADP_errors. This even does APD errors,
      ! not just positions.
      ! ERROR: change and merge with ADP setting
   ! .set_pADP_errors_to

      ! Read errors
      call create_(IDs,3)
      IDs = ["_atom_site_Cartn_x_esu","_atom_site_Cartn_y_esu","_atom_site_Cartn_z_esu"]
      call read_looped_items_(cif,IDs,err,found=fe)
      call destroy_(IDs)
      if (fe) then

         ! Set the positions
         call convert_from_(err,"angstrom")
! for process cif and c23 xml these need to be transformed into
! fractional otherwise tonto cannot see that they are the same atom as
! in the XML. Lorraine
         call set_position_errors_to_(self,err)
         call destroy_(pos)
      end if

      ! Read occupancies and disorder group
      call create_(IDs,2)
      IDs = ["_atom_site_Cartn_occupancy     ","_atom_site_Cartn_disorder_group"]
      call read_looped_items_(cif,IDs,err,transpose=TRUE,found=fo)
      call destroy_(IDs)
      if (fo) then

         ! Set the occupancies
         call set_site_occupancies_(self,err(:,1))
         call set_site_disorder_groups_(self,nint(err(:,2)))
         call destroy_(err)
      end if

      ! Clean
      call destroy_(IDs)

      ! Update?
      call update_(self)
   end subroutine

   subroutine read_smCIF_atoms_xtal(self,cif,fail)
   ! Read information from a small-molecul Crystallographic
   ! Information File, "cif".  It is an error if there are no atom
   ! coordinates in the file.  It is NOT an error if the ADP (thermal)
   ! tensors are missing.
   ! NOTE: self is destroyed and created from this file!
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: fail
      BIN :: fl,fc,fs,fd,fo
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs,labels,symbols
      MAT(STR(len=STR_SIZE),:,:), allocatable :: disorder_group
      MAT(REAL,:,:), allocatable :: pos,err
      STR(len=STR_SIZE) :: dg
      INT :: i

      ! Find atom labels somehow: each kind is tried only if the one before
      ! was not found
      fc = FALSE
      fs = FALSE
      call read_looped_item_(cif,"_atom_site_label",labels,fl)  ! Std
      if (NOT fl) call read_looped_item_(cif,"_atom_site_Cryst_label",labels,fc)  ! Tonto
      if (NOT fl AND NOT fc) call read_looped_item_(cif,"_atom_site_type_symbol",symbols,fs)  ! Desperate
      fail = (NOT fl) AND (NOT fc) AND (NOT fs)

      ! Destroy the old atom list (if there)
      call destroy_(self)

      ! Create new atom list & read labels
      if (allocated(labels)) then
         call create_(self,size(labels))
         call set_atom_data_from_labels_(self,labels)  ! This may not work
      else if (allocated(symbols)) then
         call create_(self,size(symbols))
         call set_atom_data_from_labels_(self,symbols)  ! For Z's
         if (allocated(labels)) call set_labels_(self,labels)  ! For U's
      end if
      call destroy_(labels)
      call destroy_(symbols)

      ! Read the site coordinates, and possibly occupancies
      call find_looped_item_(cif,"_atom_site_occupancy",fo)
      if (fo) then
         call create_(IDs,4)
         IDs = ["_atom_site_fract_x  ","_atom_site_fract_y  ","_atom_site_fract_z  ","_atom_site_occupancy"]
         call read_looped_items_(cif,IDs,pos,err,found=fc)
      else
         call create_(IDs,3)
         IDs = ["_atom_site_fract_x  ","_atom_site_fract_y  ","_atom_site_fract_z  "]
         call read_looped_items_(cif,IDs,pos,err,found=fc)
      end if
      call destroy_(IDs)
      fail = NOT fc
      if (fail) return

      ! Assign the positions and pADPs
      call set_pos_axis_system_to_(self,"crystal")
      call reset_pADPs_and_errors_(self,9)
      call set_positions_to_(self,pos(1:3,:))
      call set_position_errors_to_(self,err(1:3,:))
      if (fo) call set_site_occupancies_(self,pos(4,:))
      call destroy_(err)
      call destroy_(pos)

      ! Read the disorder, group if there
      call find_looped_item_(cif,"_atom_site_disorder_group",fd)
      if (fd) then
         call create_(IDs,1)
         IDs = ["_atom_site_disorder_group"]
         call read_looped_items_(cif,IDs,disorder_group)
         ENSURE(size(disorder_group,2)==size(self),"VEC{ATOM}:read_smCIF_atoms_xtal ... inconsistent # of _atom_site_disorder_groups")
         do i = 1,size(disorder_group,2)
            dg = disorder_group(1,i)
            if (dg==".") then; call set_site_disorder_group_(self(i),0)
            else if (is_int_(dg)) then; call set_site_disorder_group_(self(i),to_int_(dg))
            end if
         end do
         call destroy_(disorder_group)
         call destroy_(IDs)
      end if

      ! Clean
      call destroy_(IDs)

      ! Update?
      call update_(self)
   end subroutine

   subroutine read_smCIF_ADPs(self,cif)
   ! Read ADP information from the small-molecule "cif" file.
   ! NOTE: must read atoms first!
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
   DIE_IF(no_of_unique_elements_(self(:)%label)/=size(self),"VEC{ATOM}:read_smCIF_ADPs ... not all ADP site symbols are unique!")

      ! Read 4th order anharmonic ADPs
      call read_smCIF_ADP4_(self,cif)

      ! Read 3rd order anharmonic ADPs
      call read_smCIF_ADP3_(self,cif)

      ! Read 2nd order ADPs
      call read_smCIF_ADP2_(self,cif)
   end subroutine

   subroutine read_smCIF_ADP2(self,cif)
   ! Read ADP2 information from the small-molecule "cif" file.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: fail
   DIE_IF(no_of_unique_elements_(self(:)%label)/=size(self),"VEC{ATOM}:read_smCIF_ADP2 ... not all ADP site symbols are unique!")
      fail = FALSE

      ! Read cartesian 2nd order ADPs
      call read_smCIF_ADP2_cart_(self,cif,fail)
      if (NOT fail) return

      ! Read standard/crystal U's as last resort
      call read_smCIF_ADP2_xtal_(self,cif,fail)
   end subroutine

   subroutine read_smCIF_ADP2_cart(self,cif,fail)
   ! Read the cartesian ADP information from the small-molecule "cif".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: fail
      BIN :: fi,fd,fu,fl
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs,esd,labels
      INT :: i,ind
      MAT(REAL,:,:), allocatable :: U,dU
      VEC(REAL,:), allocatable :: U_iso,d_U_iso
      STR(len=STR_SIZE) :: lab
   DIE_IF(no_of_unique_elements_(self(:)%label)/=size(self),"VEC{ATOM}:read_smCIF_ADP2_cart ... not all ADP site symbols are unique!")

      ! Read Cartn U_iso values
      call read_looped_item_(cif,"_atom_site_Cartn_U_iso_or_equiv",U_iso,found=fi)
      call read_looped_item_(cif,"_atom_site_Cartn_U_iso_or_equiv_esu",d_U_iso,found=fd)

      ! Are they there?
      if (fi AND fd) then

         ! Make space
         call reset_pADPs_and_errors_(self,9)

         ! Convert
         call convert_from_(U_iso,"angstrom^2")
         call convert_from_(d_U_iso,"angstrom^2")

         ! Set
         call set_U_iso_(self,U_iso)
         call set_U_iso_error_(self,d_U_iso)

         ! Clean
         call destroy_(d_U_iso)
         call destroy_(U_iso)
      end if

      ! Read Cartn ADP2 tensor
      call create_(IDs,6)
      call create_(esd,6)
      do i = 1,6
         lab = ADP_label_(i,form="underscore-123-repeated")
         lab = "_atom_site_Cartn_ADP2_"//trim(lab)
         IDs(i) = trim(lab)
         esd(i) = trim(lab)//"_esu"
      end do
      call read_looped_item_(cif,"_atom_site_Cartn_ADP2_U_label",labels,fl)
      call read_looped_items_(cif,IDs,U,found=fu)
      call read_looped_items_(cif,esd,dU,found=fd)
      call destroy_(esd)
      call destroy_(IDs)

      ! Are they there?
      fail = NOT (fl AND fu AND fd)
      if (NOT fail) then

         ! Change U units to au^2
         call convert_from_(U,"angstrom^2")
         call convert_from_(dU,"angstrom^2")

         ! Match the anisotropic U labels and assign the U tensors
         do i = 1,size(labels)

            ! Find matching index "ind"
            ind = index_of_(self(:)%label,labels(i))
            DIE_IF(ind==0,"VEC{ATOM}:read_smCIF_ADP2_cart ... unmatching atom_site_Cartn_ADP_U_label, "//trim(labels(i)))

            ! Assign ADP
            call reset_pADPs_and_errors_(self(ind),9)
            call set_ADP_tensor_(self(ind),U(:,i))
            if (allocated(self(ind)%covariance_mx)) then
               call set_pADP_errors_to_(self(ind),self(ind)%covariance_mx,FALSE)
            else
               call set_ADP2_errors_to_(self(ind),dU)
            end if
         end do

         ! Set the crystal axis system. For anharmonic ADPs (if any)
         ! the form "g" or "u" was already detected and set.
         call set_ADP_axis_system_to_(self,"cartesian")
      end if

      ! Clean
      if (fd) call destroy_(dU)
      if (fu) call destroy_(U)
      if (fl) call destroy_(labels)
   end subroutine

   subroutine read_smCIF_ADP2_xtal(self,cif,fail)
   ! Read fractional ADP information from the small-molecule "cif".
   ! It is an error if there are no atom coordinates in the file.
   ! It is NOT an error if the ADP (thermal) tensors are missing.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: fail
      BIN :: fi,fu,fl
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs,labels
      INT :: i,ind
      MAT(REAL,:,:), allocatable :: U,dU
      VEC(REAL,:), allocatable :: U_iso,d_U_iso
      REAL :: fac
      STR(len=STR_SIZE) :: lab
   DIE_IF(no_of_unique_elements_(self(:)%label)/=size(self),"VEC{ATOM}:read_smCIF_ADP2_xtal ... not all ADP site symbols are unique!")

      ! Read U_iso values
      call read_looped_item_(cif,"_atom_site_U_iso_or_equiv",U_iso,d_U_iso,found=fi)

      ! Are they there?
      if (fi) then

         ! Convert B factor
         if (cif%U_iso_is_B_iso) then
            fac = ONE/(EIGHT*PI*PI)
            U_iso = fac*U_iso
            d_U_iso = fac*d_U_iso
         end if

         ! Make space
         call reset_pADPs_and_errors_(self,9)

         ! We are in the crystal axis system
         call set_ADP_axis_system_to_(self,"crystal")

         ! Set
         call convert_from_(U_iso,"angstrom^2")
         call set_U_iso_(self,U_iso)
         call destroy_(U_iso)

         ! Set errors
         if (allocated(d_U_iso)) then
            call convert_from_(d_U_iso,"angstrom^2")
            call set_U_iso_error_(self,d_U_iso)
            call destroy_(d_U_iso)
         end if
      end if

      ! Read U tensor
      call create_(IDs,6)
      do i = 1,6
         lab = ADP_label_(i,form="underscore-123-repeated")
         IDs(i) = "_atom_site_aniso_"//trim(lab)
      end do
      call read_looped_item_(cif,"_atom_site_aniso_label",labels,fl)
      call read_looped_items_(cif,IDs,U,dU,found=fu)
      call destroy_(IDs)

      ! Are they there?
      fail = NOT (fl AND fu)

      ! Read the ADP2s
      if (NOT fail) then

         ! Convert B factors
         if (cif%U_is_B) then
            fac = ONE/(EIGHT*PI*PI)
            U = fac*U
         end if

         ! Change U units to au^2
         call convert_from_(U,"angstrom^2")
         call convert_from_(dU,"angstrom^2")

         ! Match the anisotropic U labels and assign the U tensors
         do i = 1,size(labels)

            ! Find matching index "ind"
            ind = index_of_(self(:)%label,labels(i))
            DIE_IF(ind==0,"VEC{ATOM}:read_smCIF_ADP2_xtal ... unmatching atom_site_aniso_label, "//trim(labels(i)))

            ! Assign ADP
            call reset_pADPs_and_errors_(self(ind),9)
            call set_ADP_tensor_(self(ind),U(:,i))
            call set_ADP2_errors_to_(self(ind),dU(:,i))
         end do

         ! Set the crystal axis system. For anharmonic ADPs (if any)
         ! the form "g" or "u" was already detected and set.
         call set_ADP_axis_system_to_(self,"crystal")
      end if

      ! Clean
      if (fu) then
         call destroy_(dU)
         call destroy_(U)
      end if
      if (fl) call destroy_(labels)
   end subroutine

   subroutine read_smCIF_ADP3(self,cif)
   ! Detect if the small-molecule "cif" file has anharmonic ADP3s,
   ! either dimensionless "g" (crystal) or dimensioned "u" (cartesian)
   ! Gram-Charlier coefficients. The cif.GC_format will be set to this
   ! value, accordingly. It looks for "u" format first.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(STR(len=STR_SIZE),2) :: GC_format = ["u","g"]
      INT :: fmt
      BIN :: f3
      do fmt = 1,size(GC_format)

         ! Detect fmt
         call set_GC_format_(cif,GC_format(fmt))
         call read_smCIF_ADP3_helper_(self,cif,f3)

         ! If fmt found exit
         if (f3) exit
      end do
   end subroutine

   subroutine read_smCIF_ADP3_helper(self,cif,f3)
   ! Detect if stdout has ADP3s in the specified "cif.GC_format", either
   ! dimensionless "g" cartesian "u" form; and if so set "f3" TRUE,
   ! and set the ADP3's of "self".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: f3
      VEC(STR(len=STR_SIZE),:), allocatable :: labels,IDs,esd
      MAT(REAL,:,:), allocatable :: U,dU
      BIN :: fl,fu,fd
      REAL :: fac
      STR(len=STR_SIZE) :: lab
      INT :: i,ind
   ENSURE(is_one_of_(cif%GC_format,["u","g"]),"VEC{ATOM}:read_smCIF_ADP3_helper ... incorrect fmt")
      call read_looped_item_(cif,trim(cif%GC3_label)//"label",labels,fl)

      ! Any ADP3's to read?
      f3 = fl
      if (NOT fl) return
      select case (cif%GC_format)

      case ("u")

         ! U format ... should by C format?
         call create_(IDs,10)
         call create_(esd,10)
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            lab = trim(cif%GC3_label)//trim(lab)
            IDs(i) = trim(lab)
            esd(i) = trim(lab)//"_esu"
         end do

         ! Read U & dU
         call read_looped_items_(cif,IDs,U,found=fu)
         call read_looped_items_(cif,esd,dU,found=fd)

         ! Found?
         f3 = fl AND fu AND fd

         ! Clean
         call destroy_(esd)
         call destroy_(IDs)

         ! Unit conversion?
         if (f3) then
            call convert_from_(U,"angstrom^3")
            call convert_from_(dU,"angstrom^3")
         end if

      case ("g")

         ! G format
         call create_(IDs,10)
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            lab = trim(cif%GC3_label)//trim(lab)
            IDs(i) = trim(lab)
         end do

         ! Read U & dU
         call read_looped_items_(cif,IDs,U,dU,found=fu)

         ! Found?
         f3 = fu AND fl

         ! Clean
         call destroy_(IDs)
      end select

      !!!!!!!!!!!!!!!!!!
      if (NOT f3) return
      !!!!!!!!!!!!!!!!!!

      ! Read scale factor & scale
      call read_item_(cif,trim(cif%GC3_label)//"scale_factor",fac)
      call set_GC3_scale_factor_(cif,fac)
      U = U/cif%GC3_scale_factor
      dU = dU/cif%GC3_scale_factor

      ! Match the labels and assign the U3 tensors
      do i = 1,size(labels)

         ! Find matching index "ind"
         ind = index_of_(self(:)%label,labels(i))
         DIE_IF(ind==0,"VEC{ATOM}:read_smCIF_ADP3_helper ... unmatching label, "//trim(labels(i)))

         ! Assign ADP
         call reset_pADPs_and_errors_(self(ind),19)
         call set_ADP3_vector_to_(self(ind),U(:,i))
         call set_ADP3_errors_to_(self(ind),dU(:,i))

         ! Assign axes for later conversion
         if (cif%GC_format=="u") then
            call set_ADPn_axis_system_to_(self(ind),"cartesian")
         else
            call set_ADPn_axis_system_to_(self(ind),"crystal")
         end if
      end do

      ! Clean up
      call destroy_(U)
      call destroy_(dU)
      call destroy_(labels)
   end subroutine

   subroutine read_smCIF_ADP4(self,cif)
   ! Detect if the "cif" file has anharmonic ADP4s, either
   ! dimensionless "g" (crystal) or dimensioned "u" (cartesian)
   ! Gram-Charlier coefficients. The cif.GC_format will be set to this
   ! value, accordingly. It looks for "u" format first.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      VEC(STR(len=STR_SIZE),2) :: GC_format = ["u","g"]
      INT :: fmt
      BIN :: f4
      do fmt = 1,size(GC_format)

         ! Detect fmt
         call set_GC_format_(cif,GC_format(fmt))
         call read_smCIF_ADP4_helper_(self,cif,f4)

         ! If fmt found exit
         if (f4) exit
      end do
   end subroutine

   subroutine read_smCIF_ADP4_helper(self,cif,f4)
   ! Detect if stdout has ADP4s in the specified "cif.GC_format", either
   ! dimensionless "g" cartesian "u" form; and if so set "f4" TRUE,
   ! and set the ADP4's of "self".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: f4
      VEC(STR(len=STR_SIZE),:), allocatable :: labels,IDs,esd
      MAT(REAL,:,:), allocatable :: U,dU
      BIN :: fl,fu,fd
      STR(len=STR_SIZE) :: lab
      REAL :: fac
      INT :: i,ind
   ENSURE(is_one_of_(cif%GC_format,["u","g"]),"VEC{ATOM}:read_smCIF_ADP4_helper ... incorrect fmt")
      call read_looped_item_(cif,trim(cif%GC4_label)//"label",labels,fl)

      ! Any ADP4's to read?
      f4 = fl
      if (NOT fl) return
      select case (cif%GC_format)

      case ("u")

         ! U format ... should by C format?
         call create_(IDs,15)
         call create_(esd,15)
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            lab = trim(cif%GC4_label)//trim(lab)
            IDs(i) = trim(lab)
            esd(i) = trim(lab)//"_esu"
         end do

         ! Read U & dU
         call read_looped_items_(cif,IDs,U,found=fu)
         call read_looped_items_(cif,esd,dU,found=fd)

         ! Found?
         f4 = fl AND fu AND fd

         ! Clean
         call destroy_(esd)
         call destroy_(IDs)

         ! Unit conversion?
         if (f4) then
            call convert_from_(U,"angstrom^4")
            call convert_from_(dU,"angstrom^4")
         end if

      case ("g")

         ! G format
         call create_(IDs,15)
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            lab = trim(cif%GC4_label)//trim(lab)
            IDs(i) = trim(lab)
         end do

         ! Read U & dU
         call read_looped_items_(cif,IDs,U,dU,found=fu)

         ! Found?
         f4 = fu AND fl

         ! Clean
         call destroy_(IDs)
      end select

      !!!!!!!!!!!!!!!!!!
      if (NOT f4) return
      !!!!!!!!!!!!!!!!!!

      ! Read scale factor & scale
      call read_item_(cif,trim(cif%GC4_label)//"scale_factor",fac)
      call set_GC4_scale_factor_(cif,fac)
      U = U/cif%GC4_scale_factor
      dU = dU/cif%GC4_scale_factor

      ! Match the labels and assign the U4 tensors
      do i = 1,size(labels)

         ! Find matching index "ind"
         ind = index_of_(self(:)%label,labels(i))
         DIE_IF(ind==0,"VEC{ATOM}:read_smCIF_ADP4_helper ... unmatching label, "//trim(labels(i)))

         ! Assign ADP
         call reset_pADPs_and_errors_(self(ind),34)
         call set_ADP4_vector_to_(self(ind),U(:,i))
         call set_ADP4_errors_to_(self(ind),dU(:,i))

         ! Assign axes for later conversion
         if (cif%GC_format=="u") then
            call set_ADPn_axis_system_to_(self(ind),"cartesian")
         else
            call set_ADPn_axis_system_to_(self(ind),"crystal")
         end if
      end do

      ! Clean up
      call destroy_(U)
      call destroy_(dU)
      call destroy_(labels)
   end subroutine

   subroutine read_smCIF_dispersion(cif)
   ! Read the anomalous dispersion coefficients (if present)
   ! from the small-molecule "cif" file.
      type(CIF_TYPE), INOUT :: cif
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs
      VEC(REAL,:), allocatable :: f_real,f_imag
      BIN :: fs,fr,fi
      INT :: a,Z

      ! Read CIF
      call read_looped_item_(cif,"_atom_type_symbol",IDs,found=fs)
      call read_looped_item_(cif,"_atom_type_scat_dispersion_real",f_real,found=fr)
      call read_looped_item_(cif,"_atom_type_scat_dispersion_imag",f_imag,found=fi)

      ! Are they there?
      if (fs AND fr AND fi) then
         do a = 1,size(IDs)

            ! Get the atomic number Z
            Z = atomic_number_from_label_(IDs(a))

            ! Skip unrecognised element label
            if (Z==0) then
               WARN("VEC{ATOM}:read_smCIF_dispersion ... atom_type_symbol "//trim(IDs(a))//" unrecognised for anamalous dispersion")
               cycle
            end if

            ! Set them in the table
            call set_dispersion_coefficients_(Z,f_real(a),f_imag(a))
         end do
      end if

      ! Clean
      if (fi) call destroy_(f_imag)
      if (fr) call destroy_(f_real)
      if (fs) call destroy_(IDs)
   end subroutine


! mmCIF

   subroutine read_mmCIF(self,cif)
   ! Read information from a macromolwcular Crystallographic
   ! Information File "name"
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN :: fail

      ! Read atom positions
      call read_mmCIF_atoms_(self,cif,fail)

      ! Read ADPs including U_iso/B_iso
   ! .read_mmCIF_ADP2(cif,fail)

      ! Read dispersion
      call read_mmCIF_dispersion(cif)
   end subroutine

   subroutine read_mmCIF_atoms(self,cif,fail)
   ! Read Tonto information from a macromolecular "cif".  Cartesian
   ! NOTE: self is destroyed and created from this file!
      VEC(type(ATOM_TYPE),:), allocatable, OUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: fail
      INT :: n_atoms,i,val
      BIN :: fl,fc
      VEC(INT,:), allocatable :: ind
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      MAT(REAL,:,:), allocatable :: data
      STR(len=STR_SIZE) :: lab

      ! Read the site id's FIRST.
      call read_looped_item_(cif,"_atom_site.id",ind,fl)
      fail = NOT fl
      if (fail) then
         call destroy_(ind)
         return
      end if

      ! Success: id's are there
      n_atoms = size(ind)

      ! Destroy the old atom list (if there)
      call destroy_(self)

      ! Create the new atom list
      call create_(self,n_atoms)

      ! Read the labels
      call create_(IDs,6)
      IDs = ["_atom_site.type_symbol    ","_atom_site.label_atom_id  ","_atom_site.label_comp_id  ","_atom_site.label_seq_id   ","_atom_site.label_asym_id  ","_atom_site.label_entity_id"]
      call read_looped_items_(cif,IDs,labels,found=fc)
      call destroy_(IDs)
      fail = NOT fc
      if (fail) then
         call destroy_(labels)
         return
      end if

      ! Success: Replace .'s with #
      where (labels==".") labels = "#"

      ! Set the atom labels 
      do i = 1,n_atoms

         ! Set protein-specific data
         call set_atom_sequence_id_(self(i),ind(i))
         call set_compound_id_(self(i),labels(3,i))
         val = to_int_(labels(4,i))
         call set_compound_sequence_id_(self(i),val)
         call set_group_(self(i),val)
         call set_asym_id_(self(i),labels(5,i))
         call set_entity_id_(self(i),labels(6,i))

         ! Set the label, Tonto style
         lab = trim(labels(2,i))  ! CA
         lab = trim(lab)//"-"//trim(to_str_(ind(i)))  ! CA-2
         lab = trim(lab)//"-"//trim(labels(3,i))  ! CA-2-MET
         lab = trim(lab)//"-"//trim(labels(4,i))  ! CA-2-MET-1
      ! Use below to change labels ...
      ! lab = trim(labels(1,i))                   ! C
      ! lab = trim(lab)//"-"//trim(labels(5,i))   ! C-2-CA-MET-1-A
      ! lab = trim(lab)//"-"//trim(labels(6,i))   ! C-2-CA-MET-1-A-1
         call set_label_(self(i),lab,skip_mass=TRUE)

         ! Set atomic number from chemical symbol
         ! WARNING: AN has to come after setting unique label
         lab = labels(1,i)(1:2)
         call to_upper_case_(lab(1:1))
         call to_lower_case_(lab(2:2))
         call set_atomic_number_and_mass_(self(i),lab)
      end do

      ! Clean
      call destroy_(labels)
      call destroy_(ind)

      ! Read the coordinates, occupancy, B value
      call create_(IDs,4)
      IDs = ["_atom_site.Cartn_x  ","_atom_site.Cartn_y  ","_atom_site.Cartn_z  ","_atom_site.occupancy"]
      call read_looped_items_(cif,IDs,data,found=fc)
      call destroy_(IDs)
      fail = NOT fc
      if (fail) return

      ! Set the positions
      call set_pos_axis_system_to_(self,"cartesian")
      call reset_pADPs_and_errors_(self,9)
      call convert_from_(data(1:3,:),"angstrom")
      call set_positions_to_(self,data(1:3,:))

      ! Set occupancy
      call set_site_occupancies_(self,data(4,:))
      call destroy_(data)

      ! Read errors (if there)
   ! IDs.create(3)

    !        "_atom_site.Cartn_z_esd"  ]
   ! cif.read_looped_items(IDs,data,found=fe)
   ! IDs.destroy

   ! if (fe) then
   !    ! Set the positions
   !    data.convert_from("angstrom")
   !    .set_position_errors_to(data)
   !    data.destroy
   ! end

      ! Update?
      call update_(self)
   end subroutine

   subroutine read_mmCIF_ADP2(self,cif,fail)
   ! Read fractional ADP information from the macromolecular "cif".
   ! It is an error if there are no atom coordinates in the file.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(CIF_TYPE), INOUT :: cif
      BIN, OUT :: fail
      BIN :: fi,fu
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs
      INT :: a,i
      MAT(REAL,:,:), allocatable :: U
      VEC(REAL,:), allocatable :: U_iso
      VEC(INT,:), allocatable :: ind
      REAL :: fac
      STR(len=STR_SIZE) :: lab

      ! Read the site id's
      call read_looped_item_(cif,"_atom_site_anisotrop.id",ind,fi)
      fail = NOT fi
      if (fail) return

      ! Read U_iso values
      call read_looped_item_(cif,"_atom_site.U_iso_or_equiv",U_iso,found=fi)

      ! Read B_iso values if U_iso's are not there ...
      if (NOT fi) then
         call read_looped_item_(cif,"_atom_site.B_iso_or_equiv",U_iso,found=fi)
         fac = ONE/(EIGHT*PI*PI)
         U_iso = fac*U_iso
      end if

      ! If there set the values
      if (fi) then

         ! We are in the crystal axis system
         call set_ADP_axis_system_to_(self,"crystal")

         ! Set
         call convert_from_(U_iso,"angstrom^2")
         call set_U_iso_(self,U_iso)
         call destroy_(U_iso)
      end if

      ! Read U tensor
      call create_(IDs,6)
      do i = 1,6
         lab = ADP_label_(i,form="123-repeated-square-braces")
         IDs(i) = "_atom_site_anisotrop.U_"//trim(lab)
      end do
      call read_looped_items_(cif,IDs,U,found=fu)
      call destroy_(IDs)

      ! Read B tensor in U is not there ...
      if (NOT fu) then
         call create_(IDs,6)
         do i = 1,6
            lab = ADP_label_(i,form="123-repeated-square-braces")
            IDs(i) = "_atom_site_anisotrop.B_"//trim(lab)
         end do
         call read_looped_items_(cif,IDs,U,found=fu)
         call destroy_(IDs)

         ! Rescale B's
         fac = ONE/(EIGHT*PI*PI)
         U_iso = fac*U_iso
      end if

      ! Are they there?
      fail = NOT fu

      ! Read the ADP2s
      if (NOT fail) then

         ! Change U units to au^2
         call convert_from_(U,"angstrom^2")

         ! Assign the U tensors
         do i = 1,size(self)

            ! Get atom index
            a = ind(i)

            ! Assign ADP
            call reset_pADPs_and_errors_(self(a),9)
            call set_ADP_tensor_(self(a),U(:,i))
         end do

         ! Set the crystal axis system. For anharmonic ADPs (if any)
         ! the form "g" or "u" was already detected and set.
         call set_ADP_axis_system_to_(self,"crystal")
      end if

      ! Clean
      call destroy_(U)
      call destroy_(U_iso)
      call destroy_(ind)
   end subroutine

   subroutine read_mmCIF_dispersion(cif)
   ! Read the anomalous dispersion coefficients (if present)
   ! from the small-molecule "cif" file.
      type(CIF_TYPE), INOUT :: cif
      VEC(STR(len=STR_SIZE),:), allocatable :: IDs
      VEC(REAL,:), allocatable :: f_real,f_imag
      BIN :: fs,fr,fi
      INT :: a,Z

      ! Read CIF
      call read_looped_item_(cif,"_atom_type.symbol",IDs,found=fs)
      call read_looped_item_(cif,"_atom_type.scat_dispersion_real",f_real,found=fr)
      call read_looped_item_(cif,"_atom_type.scat_dispersion_imag",f_imag,found=fi)

      ! Are they there?
      if (fs AND fr AND fi) then
         do a = 1,size(IDs)

            ! Get the atomic number Z
            Z = atomic_number_from_label_(IDs(a))

            ! Skip unrecognised element label
            if (Z==0) then
               WARN("VEC{ATOM}:read_mmCIF_dispersion ... atom_type_symbol "//trim(IDs(a))//" unrecognised for anamalous dispersion")
               cycle
            end if

            ! Set them in the table
            call set_dispersion_coefficients_(Z,f_real(a),f_imag(a))
         end do
      end if

      ! Clean
      if (fi) call destroy_(f_imag)
      if (fr) call destroy_(f_real)
      if (fs) call destroy_(IDs)
   end subroutine

   subroutine read_use_interpolators(self)
   ! Read whether to use interpolators for spherical atom densities
      VEC(type(ATOM_TYPE),:), allocatable, INOUT :: self
      BIN :: use_interp
      call read_(stdin,use_interp)
      if (NOT allocated(self)) return
      call set_use_interpolators_(self,use_interp)
   end subroutine

!  =============
!  Resolve bases
!  =============

   PURE subroutine resolve_bases_0(self,basis,suffix)
   ! Resolve the basis sets for each atom -- by pointer assigning its basis to
   ! the element in "basis" which matches either the atoms .basis_label, or else
   ! matches a label constructed in a standard way, by joining the atom chemical
   ! symbol with the ":suffix" string.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(type(BASIS_TYPE),:), allocatable, IN :: basis
      STR(len=*), optional, IN :: suffix
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      do a = 1,size(self)
         call resolve_basis_(self(a),basis,suffix)
      end do
   end subroutine

   PURE subroutine resolve_bases_1(self,basis,suffix)
   ! Resolve the basis sets for each atom -- by pointer assigning its basis to
   ! the element in "basis" which matches either the atoms .basis_label, or else
   ! matches a label constructed in a standard way, by joining the atom chemical
   ! symbol with the ":suffix" string.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(type(SLATERBASIS_TYPE),:), allocatable, IN :: basis
      STR(len=*), optional, IN :: suffix
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      do a = 1,size(self)
         call resolve_basis_(self(a),basis,suffix)
      end do
   end subroutine

   PURE subroutine resolve_bases_2(self,basis,suffix)
   ! Resolve the basis sets for each atom -- by pointer assigning its basis to
   ! the element in "basis" which matches either the atoms .basis_label, or else
   ! matches a label constructed in a standard way, by joining the atom chemical
   ! symbol with the ":suffix" string.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(type(COPPENSBASIS_TYPE),:), allocatable, IN :: basis
      STR(len=*), optional, IN :: suffix
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      do a = 1,size(self)
         call resolve_basis_(self(a),basis,suffix)
      end do
   end subroutine


!  ===================
!  Axis system changes
!  ===================

   subroutine make_pADP_covariances_from_esds(self,cell)
   ! Make each atom's cartesian covariance matrix from its own standard
   ! uncertainties, assuming those are uncorrelated. Call this while the
   ! atoms are still in the crystal frame, i.e. after reading a CIF and
   ! before the change to cartesians. See ATOM:make_pADP_covariance_from_esds.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      type(UNIT_CELL_TYPE), IN :: cell
      INT :: a

      ! All or nothing. Converting only some atoms would leave
      ! "pADP_errors" holding cartesian numbers for those and crystal-frame
      ! numbers for the rest, in one array, with nothing to tell them
      ! apart -- and no writer could then interpret it. So one atom that
      ! cannot be converted stops the whole list.
      do a = 1,size(self)
         if (NOT pADP_covariance_can_be_made_(self(a))) return
      end do
      do a = 1,size(self)
         call make_pADP_covariance_from_esds_(self(a),cell)
      end do
   end subroutine

   pure function any_pADP_covariances_derived(self) result (res)
   ! Return TRUE if any atom's covariance matrix was derived from CIF
   ! standard uncertainties rather than measured by a refinement.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(self(:)%pADP_covariances_derived)
   end function

   PURE subroutine change_axis_system_to(self,axis_system,cell)
   ! Change the atom axis systems to the desired "axis_system".
   ! NOTE: the ESDs are NOT changed. They belong to the frame they were
   ! made in; transforming them needs the covariance, not the sigmas.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: axis_system
      type(UNIT_CELL_TYPE), IN :: cell
   ! H_U_iso :: BIN, IN
      INT :: a
      do a = 1,size(self)
         call change_axis_system_to_(self(a),axis_system,cell)
      end do
   end subroutine

   PURE subroutine change_axis_for_xtal23_system_to(self,axis_system,cell)
   ! Change the atom axis systems to the desired "axis_system".
   ! NOTE: the ESDs are NOT changed. They belong to the frame they were
   ! made in; transforming them needs the covariance, not the sigmas.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: axis_system
      type(UNIT_CELL_TYPE), IN :: cell
   ! H_U_iso :: BIN, IN
      INT :: a
      do a = 1,size(self)
         call change_axis_for_xtal23_system_to_(self(a),axis_system,cell)
      end do
   end subroutine

   PURE subroutine change_ADP2_axis_system_to(self,axis_system,cell)
   ! Change the ADP2 axis systems to the desired "axis_system".
   ! NOTE: the ESDs are NOT changed. They belong to the frame they were
   ! made in; transforming them needs the covariance, not the sigmas.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: axis_system
      type(UNIT_CELL_TYPE), IN :: cell
   ! H_U_iso :: BIN, IN
      INT :: a
      do a = 1,size(self)
         call change_ADP2_axis_system_to_(self(a),axis_system,cell)
      end do
   end subroutine


!  ===============
!  Change geometry
!  ===============

   PURE subroutine extend_bond_lengths(self,z1,z2,factor)
   ! Uniformly extend the bond lengths between all BONDED atoms with
   ! atomic number "z1" and "z2" by a "factor". Only the coordinates
   ! of the atoms with atomic number "z2" are altered.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, IN :: z1,z2
      REAL, IN :: factor
      INT :: z1_ind,z2_ind,a1,a2
      VEC(REAL,3) :: r21,new
      if (all(self(:)%atomic_number/=z1) OR all(self(:)%atomic_number/=z2)) return

      ! First occurence of the atoms
      z1_ind = index_of_value_(self(:)%atomic_number,z1)
      z2_ind = index_of_value_(self(:)%atomic_number,z2)
      do a1 = z1_ind,size(self)
         if (self(a1)%atomic_number/=z1) cycle
         do a2 = z2_ind,size(self)
            if (self(a2)%atomic_number/=z2) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            r21 = self(a2)%position-self(a1)%position
            new = self(a1)%position+factor*r21
            call set_position_(self(a2),new)
         end do
      end do
   end subroutine

   PURE subroutine set_bond_lengths(self,z1,z2,length)
   ! Uniformly set the bond lengths between all BONDED atoms with atomic number
   ! "z1" and "z2" to be "length". Only the coordinates of the atoms with atomic
   ! number "z2" are altered.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, IN :: z1,z2
      REAL, IN :: length
      INT :: z1_ind,z2_ind,a1,a2
      VEC(REAL,3) :: r21,new
      if (all(self(:)%atomic_number/=z1) OR all(self(:)%atomic_number/=z2)) return

      ! First occurence of the atoms
      z1_ind = index_of_value_(self(:)%atomic_number,z1)
      z2_ind = index_of_value_(self(:)%atomic_number,z2)
      do a1 = z1_ind,size(self)
         if (self(a1)%atomic_number/=z1) cycle
         do a2 = z2_ind,size(self)
            if (self(a2)%atomic_number/=z2) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            r21 = self(a2)%position-self(a1)%position
            call normalize_(r21)
            new = self(a1)%position+length*r21
            call set_position_(self(a2),new)
         end do
      end do
   end subroutine

   PURE subroutine set_bond_length(self,a,b,length)
   ! Set the bond length between atoms "a" and and "b" to be "length"
   ! by modifying the position of atom "b".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, IN :: a,b
      REAL, IN :: length
      VEC(REAL,3) :: rab,new

      ! Normalised vector A->B
      rab = self(b)%position-self(a)%position
      call normalize_(rab)

      ! New vector A->B
      rab = length*rab

      ! Get new position
      new = self(a)%position+rab

      ! Set new position
      call set_position_(self(b),new)
   end subroutine


!  ===================
!  Information methods
!  ===================

   pure function bond_length(self,a,b) result (res)
   ! Return the bond length between atoms "a" and and "b".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      REAL :: res
      VEC(REAL,3) :: rab

      ! Normalised vector A->B
      rab = self(b)%position-self(a)%position
      res = sqrt(dot_product(rab,rab))
   end function

   pure function no_of_dummy_atoms(self) result (res)
   ! Return the number of dummy atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      if (size(self)==0) then
         res = 0
      else
         res = count(is_a_dummy_atom_(self))
      end if
   end function

   pure function dummy_atoms(self) result (ind)
   ! Return the list of dummy atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_dummy_atoms_(self)) :: ind
      INT :: i
      if (no_of_dummy_atoms_(self)==0) return
      i = 0
      ind = pack([(i,i = 1,size(self))],mask=is_a_dummy_atom_(self))
   end function

   pure function no_of_nondummy_atoms(self) result (res)
   ! Return the number of non-dummy atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      if (size(self)==0) then
         res = 0
      else
         res = count(is_a_nondummy_atom_(self))
      end if
   end function

   pure function nondummy_atoms(self) result (ind)
   ! Return TRUE if all atoms have unique labels
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_nondummy_atoms_(self)) :: ind
      INT :: i
      if (no_of_nondummy_atoms_(self)==0) return
      i = 0
      ind = pack([(i,i = 1,size(self))],mask=is_a_nondummy_atom_(self))
   end function

   pure function no_of_atoms_with_bases(self) result (res)
   ! Return the number of atoms with bases
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = count(has_basis_(self))
   end function

   pure function atoms_with_bases(self) result (ind)
   ! Return TRUE if all atoms have unique labels
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_atoms_with_bases_(self)) :: ind
      INT :: i
      i = 0
      ind = pack([(i,i = 1,size(self))],mask=has_basis_(self))
   end function

   PURE function chemical_formula(self,with_spaces) result (res)
   ! Return the chemical formula for the molecule, as a string, in
   ! alphabetical order of elements
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: with_spaces
      STR(len=STR_SIZE) :: res
      INT :: a,na
      BIN :: spaces
      VEC(STR(len=STR_SIZE),:), allocatable :: symbol
   ENSURE(size(self)>0,"VEC{ATOM}:chemical_formula ... Must have at least one atom for a chemical formula")

      ! Do we want spaces between the elements?
      spaces = FALSE
      if (present(with_spaces)) spaces = with_spaces

      ! Get alphabetical list of chemical symbols
      call create_(symbol,size(self))
      do a = 1,size(self)
         symbol(a) = chemical_symbol_(self(a))
      end do
      call quick_sort_(symbol)

      ! Make the chemical symbol
      res = " "
      a = 1
      do
         if (a>size(self)) exit
         na = count(symbol==symbol(a))
         if (spaces AND res/=" ") then; res = trim(res)//" "//trim(symbol(a))
         else; res = trim(res)//trim(symbol(a))
         end if
         if (na>1) res = trim(res)//trim(to_str_(na))
         a = a+na
      end do

      ! Clean up
      call destroy_(symbol)
   end function

   pure function molecular_weight(self) result (res)
   ! Return the molceular weight for this atomvec
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: res
      INT :: a
      res = ZERO
      do a = 1,size(self)
         res = res+self(a)%mass
      end do
   end function

   subroutine get_mean_neutron_numbers(self,NN)
   ! Get the atom coordinates in a matrix object "coord"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: NN
      INT :: a
      do a = 1,size(self)
         NN(a) = mean_neutron_number_(self(a))
      end do
   end subroutine

   pure function no_of_H_atoms(self) result (res)
   ! Return the number of H atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = count(is_a_H_atom_(self))
   end function

   pure function no_of_non_H_atoms(self) result (res)
   ! Return the number of non H atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = count(is_not_a_H_atom_(self))
   end function

   PURE function list_of_non_H_atoms(self) result (res)
   ! Return the list of indices of non-H atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable :: res
      INT :: n,a,i
      n = no_of_non_H_atoms_(self)
      call create_(res,n)
      i = 0
      do a = 1,size(self)
         if (is_not_a_H_atom_(self(a))) then
            i = i+1
            res(i) = a
         end if
      end do
   end function

   PURE function indices_of_non_H_atom_for_atom(self) result (res)
   ! Return the list of indices of non-H atoms as a function of the
   ! atom index in the total list of atoms; the inverse of previous.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable :: res
      INT :: a,i
      call create_(res,size(self))
      res = 0
      i = 0
      do a = 1,size(self)
         if (is_not_a_H_atom_(self(a))) then
            i = i+1
            res(a) = i
         end if
      end do
   end function

   pure function no_of_nonmetal_atoms(self) result (res)
   ! Return the number of non-dummy atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = count(is_a_nonmetal_(self))
   end function

   pure function no_of_metalloid_atoms(self) result (res)
   ! Return the number of non-dummy atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = count(is_a_metalloid_(self))
   end function

   pure function no_of_metal_atoms(self) result (res)
   ! Return the number of non-dummy atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = count(is_a_metal_(self))
   end function

   PURE function list_of_metal_atoms(self) result (res)
   ! Return the indices of the metal atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable :: res
      INT :: n,a,i
      n = no_of_metal_atoms_(self)
      call create_(res,n)
      i = 0
      do a = 1,n
         if (NOT is_a_metal_(self(a))) cycle
         i = i+1
         res(i) = a
      end do
   end function


!  =====================
!  Symbols, labels, tags
!  =====================

   PURE function unique_tags(self,list) result (res)
   ! Return the unique tags
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: i
      call create_(res,size(list))
      do i = 1,size(list)
         res(i) = self(i)%tag
      end do
   end function

   PURE function non_H_atom_tags(self) result (tags)
   ! Return the list of non-H atom tags
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: tags
      VEC(INT,:), allocatable :: list
      INT :: n
      n = no_of_non_H_atoms_(self)
      call create_(tags,n)

      ! Get the list
      list = list_of_non_H_atoms_(self)

      ! Get the tags
      tags = self(list)%tag
   end function

   PURE function chemical_symbols(self) result (res)
   ! Return an array of the chemical symbols for each atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),size(self)) :: res
      res = chemical_symbol_(self)
   end function

   PURE function has_dummy_labels(self) result (res)
   ! Return TRUE if any atom has a dummy charge labels
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(has_a_dummy_label_(self))
   end function

   PURE function has_core_electrons(self) result (res)
   ! Return TRUE if any atom has non-zero core electrons, implying
   ! ECPs are being used.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(self(:)%n_core_electrons>1)
   end function


! pADP's

   PURE function no_of_NPD_ADPs(self) result (res)
   ! Return the number of non-positive definite ADPs
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         if (has_NPD_ADP_(self(a))) res = res+1
      end do
   end function

   PURE function no_of_flat_ADPs(self) result (res)
   ! Return the number of flat ADPs
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         if (has_flat_ADP_(self(a))) res = res+1
      end do
   end function

   pure function no_of_pADPs(self) result (res)
   ! Return the number of positional and ADP parameters.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+no_of_pADPs_(self(a))
      end do
   end function

   PURE subroutine put_no_of_pADPs_per_atom_to(self,res)
   ! Return the number of positional and ADP parameters for each atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), OUT :: res
      INT :: a
   ENSURE(size(res)==size(self),"VEC{ATOM}:put_no_of_pADPs_per_atom_to ... wrong size, res")
      do a = 1,size(self)
         res(a) = no_of_pADPs_(self(a))
      end do
   end subroutine

   PURE function no_of_pADPs_up_to_atom_0(self,a) result (res)
   ! Return the no. of the pADPs up to but *not* including atom "a".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      INT :: res
      INT :: b,n
   ENSURE(a>0,"VEC{ATOM}:no_of_pADPs_up_to_atom_0 ... a must be +ve")
   ENSURE(a<=size(self),"VEC{ATOM}:no_of_pADPs_up_to_atom_0 ... a too large")
      res = 0
      do b = 1,a-1
         n = no_of_pADPs_(self(b))
         res = res+n
      end do
   end function

   PURE function no_of_pADPs_up_to_atom_1(self,a,list) result (res)
   ! Return the no. of the pADPs up to but *not* including atom "a".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      VEC(INT,:), IN :: list
      INT :: res
      INT :: b,n
   ENSURE(a>0,"VEC{ATOM}:no_of_pADPs_up_to_atom_1 ... a must be +ve")
   ENSURE(a<=size(list),"VEC{ATOM}:no_of_pADPs_up_to_atom_1 ... a too large")
      res = 0
      do b = 1,a-1
         n = no_of_pADPs_(self(list(b)))
         res = res+n
      end do
   end function

   pure function max_no_of_pADPs_per_atom(self) result (res)
   ! Return the *maximum* number of pADP parameters per atom
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = max(res,no_of_pADPs_(self(a)))
      end do
   end function

   PURE subroutine get_atom_for_pADP_index(self,par_index,res,last_index)
   ! Return the atom for the cumulative position-ADP index "par_index".
   ! Also return the cumulative index up to the previous atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: par_index
      INT, OUT :: res
      INT, OUT :: last_index
      INT :: a,n,l
   ENSURE(par_index>0,"VEC{ATOM}:get_atom_for_pADP_index ... par_index must be +ve")
   ENSURE(par_index<=no_of_pADPs_(self),"VEC{ATOM}:get_atom_for_pADP_index ... par_index too large")
      n = 0
      l = 0
      do a = 1,size(self)
         n = no_of_pADPs_(self(a))
         l = l+n
         if (par_index>l) cycle
         exit
      end do
      res = a
      last_index = l-n
   end subroutine

   PURE function tag_pADP_label(self,par_index) result (res)
   ! Return the unique tag with the parameter (positional-ADP) label
   ! for the *cumulative* parameter index "par_index"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: par_index
      STR(len=STR_SIZE) :: res
      INT :: a,ind
      call get_atom_for_pADP_index_(self,par_index,a,ind)
      ind = par_index-ind
      res = tag_pADP_label_(self(a),ind)
   end function

   PURE function tag_pADP_labels_0(self) result (res)
   ! Return *all* the tags and positional-ADP labels.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: a,n,i,l,dim

      ! No. of parameters
      dim = no_of_pADPs_(self)

      ! Leaky
      call create_(res,dim)

      ! Get the labels
      l = 0
      do a = 1,size(self)
         n = no_of_pADPs_(self(a))
         do i = 1,n
            res(l+i) = tag_pADP_label_(self(a),i)
         end do
         l = l+n
      end do
   end function

   PURE function tag_pADP_labels_1(self,par_indices) result (res)
   ! Return the tag and positional-ADP parameter with  the indices
   ! "par_indices"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: par_indices
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: i

      ! Create the label arrays (leaky)
      call create_(res,size(par_indices))

      ! Get the labels
      do i = 1,size(par_indices)
         res(i) = tag_pADP_label_(self,par_indices(i))
      end do
   end function


! ADP's

   pure function no_of_ADP3s(self) result (res)
   ! Return the number of ADP3 parameters.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle
         res = res+10
      end do
   end function

   pure function no_of_ADP3_atoms(self) result (res)
   ! Return the number of atoms with ADP3 parameters.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle
         res = res+1
      end do
   end function

   pure function no_of_ADP4s(self) result (res)
   ! Return the number of ADP4 parameters.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle
         res = res+15
      end do
   end function

   pure function no_of_ADP4_atoms(self) result (res)
   ! Return the number of atoms with ADP4 parameters.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle
         res = res+1
      end do
   end function


!  ================
!  Nuclear energies
!  ================

   pure function sum_of_nuclear_charges(self) result (res)
   ! Return the sum of the nuclear_charges
   ! Note: dummy atoms may have non-zero nuclear charge
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: res
      res = sum(self(:)%nuclear_charge)
   end function

   pure function sum_of_core_electrons(self) result (res)
   ! Return the sum of the nuclear_charges
   ! Note: dummy atoms may have non-zero nuclear charge
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: res
      res = sum(self(:)%n_core_electrons)
   end function

   pure function nuclear_repulsion_energy_0(self) result (res)
   ! Return the nuclear repulsion energy.
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: res
      INT :: i,j
      REAL :: qi,qj
      VEC(REAL,3) :: r
      res = ZERO
      do i = 1,size(self)
         qi = self(i)%nuclear_charge
         do j = 1,i-1
            qj = self(j)%nuclear_charge
            r = self(j)%position-self(i)%position
            res = res+qi*qj/sqrt(dot_product(r,r))
         end do
      end do
   end function

   pure function nuclear_repulsion_energy_1(self,atoms) result (res)
   ! Return the nuclear repulsion energy felt by the group of atoms
   ! "a" in the field of all the nuclei in "self"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: atoms
      REAL :: res
      INT :: a,i,j
      REAL :: qi,qj
      VEC(REAL,3) :: r
      res = ZERO
      do i = 1,size(atoms)
         a = atoms(i)
         qi = self(a)%nuclear_charge
         do j = 1,size(self)
            if (any(atoms==j)) cycle
            qj = self(j)%nuclear_charge
            r = self(j)%position-self(a)%position
            res = res+qi*qj/sqrt(dot_product(r,r))
         end do
      end do
      res = HALF*res  ! only count half the interaction energy
      res = res+nuclear_repulsion_energy_(self(atoms))
   end function

   pure function nuclear_repulsion_energy_2(self,atoms,nuclei) result (res)
   ! Return the nuclear repulsion energy felt by the group of atoms
   ! "a" in the field of all the nuclei in "nuclei"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: atoms,nuclei
      REAL :: res
      INT :: a,i,j,k,n_atoms,n_field
      REAL :: qi,qj
      VEC(REAL,3) :: r
      res = ZERO
      n_atoms = size(atoms)
      n_field = size(nuclei)
      do i = 1,n_atoms
         a = atoms(i)
         qi = self(a)%nuclear_charge
         do k = 1,n_field
            j = nuclei(k)
            if (any(atoms==j)) cycle
            qj = self(j)%nuclear_charge
            r = self(j)%position-self(a)%position
            res = res+qi*qj/sqrt(dot_product(r,r))
         end do
      end do
      res = res+nuclear_repulsion_energy_(self(atoms))
   end function

   pure function nuclear_repulsion_for_atom(self,a) result (res)
   ! Return the nuclear repulsion energy felt by atom "a" in the field
   ! of all the nuclei in "self"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      REAL :: res
      INT :: j
      REAL :: qa,qj,r2
      VEC(REAL,3) :: r
      res = ZERO
      qa = self(a)%nuclear_charge
      do j = 1,size(self)
         if (j==a) cycle
         qj = self(j)%nuclear_charge
         r = self(j)%position-self(a)%position
         r2 = dot_product(r,r)
         res = res+qj*qa/sqrt(r2)
      end do
   end function

   pure function nuclear_repulsion_for_atoms(self,list) result (res)
   ! Return the nuclear repulsion energy for the atoms whose indices
   ! are in "list".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: list
      REAL :: res
      INT :: i,j,a,b
      REAL :: qa,qb,r2
      VEC(REAL,3) :: ra,rb,r
      res = ZERO
      if (size(list)==1) return
      do i = 1,size(list)
         a = list(i)
         qa = self(a)%nuclear_charge
         ra = self(a)%position
         do j = 1,i-1
            b = list(j)
            qb = self(b)%nuclear_charge
            rb = self(b)%position
            r = rb-ra
            r2 = dot_product(r,r)
            res = res+qa*qb/sqrt(r2)
         end do
      end do
   end function


!  ======================
!  Center of mass & shape
!  ======================

   pure function center_of_mass(self) result (res)
   ! Return the center of mass
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3) :: res
      INT :: a
      REAL :: mw
      res = ZERO
      do a = 1,size(self)
         res = res+self(a)%position*self(a)%mass
      end do
      mw = ONE/molecular_weight_(self)
      res = res*mw
   end function

   PURE subroutine move_origin_to_center_of_mass(self)
   ! Move the origin to the center of mass
      VEC(type(ATOM_TYPE),:), INOUT :: self
      call translate_by_(self,-center_of_mass_(self))
   end subroutine

   PURE subroutine change_to_principal_axes(self)
   ! Move the origin to the center of mass and change the coordinates
   ! to be with respect to the principal axes
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,3) :: pm
      MAT(REAL,3,3) :: pa
      call translate_by_(self,-center_of_mass_(self))
      call make_principal_moments_(self,pm,pa)
      call change_coordinate_axes_(self,pa)
   end subroutine

   PURE subroutine change_to_principal_top_axes(self)
   ! Move the origin to the center of mass and change the coordinates to be with
   ! respect to the principal top axes i.e. in the case of symmetric tops the
   ! last axis is the unique axes.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,3) :: pm
      MAT(REAL,3,3) :: pa
      BIN :: reorder
      call translate_by_(self,-center_of_mass_(self))
      call make_principal_moments_(self,pm,pa,reorder)
      call change_coordinate_axes_(self,pa)
   end subroutine

   PURE subroutine change_coordinate_axes(self,axes)
   ! Change the coordinates of the atoms to be with respect to the columns of
   ! "axes", which must be an orthogonal matrix.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,3,3), IN :: axes
      INT :: a
      do a = 1,size(self)
         call change_coordinate_axes_(self(a),axes)
      end do
   end subroutine

   pure function displacement_mass_vector(self) result (res)
   ! Return the length 3*.dim mass vector used for weighting the
   ! cartesian force contant matrix
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3*size(self)) :: res
      INT :: a,i,ia
      ia = 0
      do a = 1,size(self)
         do i = 1,3
            ia = ia+1
            res(ia) = self(a)%mass
         end do
      end do
   end function

   PURE subroutine make_rigid_body_modes(self,B,n_rb)
   ! Make "B", the rigid-body displacements of the atoms in the mass-
   ! weighted coordinates sqrt(m_a) x_a, ordered as in
   ! displacement_mass_vector: three translations sqrt(m_a) e_i and three
   ! rotations sqrt(m_a) e_i x (r_a - r_com) about the centre of mass,
   ! orthonormalised. A linear molecule has five; "n_rb" is how many,
   ! and B(:,n_rb+1:6) is zero.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), OUT :: B
      INT, OUT :: n_rb
      VEC(REAL,3) :: rc,d,e,v
      REAL :: sm,nrm
      INT :: a,i,j,c
   ENSURE(size(B,1)==3*size(self),"VEC{ATOM}:make_rigid_body_modes ... B must have 3 x no. of atoms rows")
   ENSURE(size(B,2)==6,"VEC{ATOM}:make_rigid_body_modes ... B must have 6 columns")
      rc = center_of_mass_(self)
      B = ZERO
      do a = 1,size(self)
         sm = sqrt(self(a)%mass)
         d = self(a)%position-rc
         do i = 1,3
            B(3*(a-1)+i,i) = sm
            e = ZERO; e(i) = ONE
            v(1) = e(2)*d(3)-e(3)*d(2)
            v(2) = e(3)*d(1)-e(1)*d(3)
            v(3) = e(1)*d(2)-e(2)*d(1)
            B(3*(a-1)+1:3*a,3+i) = sm*v
         end do
      end do

      ! Gram-Schmidt, dropping a column with nothing left after projection
      ! (the rotation about the axis of a linear molecule)
      n_rb = 0
      do c = 1,6
         do j = 1,n_rb
            B(:,c) = B(:,c)-dot_product(B(:,j),B(:,c))*B(:,j)
         end do
         nrm = norm2(B(:,c))
         if (nrm<TOL(6)*sqrt(sum(self(:)%mass))) then
            B(:,c) = ZERO
            cycle
         end if
         n_rb = n_rb+1
         B(:,n_rb) = B(:,c)/nrm
         if (n_rb<c) B(:,c) = ZERO
      end do
   end subroutine

   pure function mass(self) result (res)
   ! Return the mass of the molecule
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: res
      INT :: a
      res = ZERO
      do a = 1,size(self)
         res = res+self(a)%mass
      end do
   end function

   PURE function reduced_mass(self) result (mu)
   ! Return the center of mass
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: mu
      INT :: a
      mu = ZERO
      do a = 1,size(self)
         mu = mu+ONE/self(a)%mass
      end do
      mu = ONE/mu
   end function

   PURE subroutine make_inertia_tensor(self,it)
   ! Make the moment of inertia tensor wrt the center of mass
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,3,3), OUT :: it
      MAT(REAL,3,3) :: m
      VEC(REAL,3) :: com,r
      REAL :: trace
      INT :: a
      com = center_of_mass_(self)
      it = ZERO
      do a = 1,size(self)
         r = self(a)%position-com
         m = spread(r,dim=1,ncopies=3)*spread(r,dim=2,ncopies=3)
         m = self(a)%mass*m
         trace = trace_(m)
         m = -m
         call increment_diagonal_by_(m,trace)
         it = it+m
      end do
   end subroutine

   PURE subroutine make_inertial_axes(self,axes)
   ! Make the inertial "axes".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,3,3), OUT :: axes
      VEC(REAL,3) :: pm
      call make_principal_moments_(self,pm,axes)
   end subroutine

   PURE subroutine make_principal_moments(self,pm,pa,reorder)
   ! Make the principal moments "pm" and principal axes "pm" wrt the center of
   ! mass.  If "reorder" is present then the principal axes are made to be right
   ! handed. Also:
   ! - For sperical tops, the local x,y,z axes are the pricipal axes
   ! - For symmetric tops, the C axis is unique
   ! - For asymmetric tops, the pricipal axes are aligned close to the local
   !    x,y,z axes
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), OUT :: pm
      MAT(REAL,3,3), OUT :: pa
      BIN, optional, IN :: reorder
      VEC(REAL,3) :: pn
      MAT(REAL,3,3) :: it

      ! Make the inertia tensor and axes
      call make_inertia_tensor_(self,it)
      call diagonalize_Jacobi_(it,pm,pa)

      ! Set small moments to zero
      if (NOT present(reorder)) return
      where (pm<TOL(6))
         pm = ZERO
      end where

      ! Make principal axes close to x,y,z
      if (abs(pa(1,2))>abs(pa(1,1))) then
         call swap_columns_(pa,1,2)
         call swap_elements_(pm,1,2)
      end if
      if (abs(pa(1,3))>abs(pa(1,1))) then
         call swap_columns_(pa,1,3)
         call swap_elements_(pm,1,3)
      end if
      if (abs(pa(2,3))>abs(pa(2,2))) then
         call swap_columns_(pa,2,3)
         call swap_elements_(pm,2,3)
      end if

      ! Use normalized moments for now
      pn = pm/maxval(abs(pm))
      if (is_spherical_top_(self,pn)) then

            ! Spherical top? ... axes are x,y,z
         call to_unit_mx_(pa)
      else if (is_symmetric_top_(self,pn)) then

         ! Symmetric top? ... C axis is unique
         if (abs(pn(1)-pn(3))<TOL(2)) then
            call swap_columns_(pa,2,3)
            call swap_elements_(pm,2,3)
         else if (abs(pn(2)-pn(3))<TOL(2)) then
            call swap_columns_(pa,1,3)
            call swap_elements_(pm,1,3)
         end if
      end if

      ! Make positive diagonals
      if (pa(1,1)<ZERO AND pa(2,2)<ZERO) then
         pa(:,1) = -pa(:,1)
         pa(:,2) = -pa(:,2)
      end if

      ! Lehthanded system?
      if (determinant_(pa)>ZERO) return

      ! Make axes are right handed
      if (abs(pm(1)-pm(2))<TOL(6)) then

         ! Swap columns 1,2 if equal moments
         call swap_elements_(pm,1,2)
         call swap_columns_(pa,1,2)
      else if (abs(pm(2)-pm(3))<TOL(6)) then

         ! Swap columns 2,3 if equal moments
         call swap_elements_(pm,2,3)
         call swap_columns_(pa,2,3)
      else

         ! Reverse all C axis
         pa(:,3) = -pa(:,3)
      end if
   end subroutine

   PURE subroutine make_shape_tensor(self,st)
   ! Make the shape tensor "st" wrt the center of atoms. This is the
   ! same as the moment of inertia tensor except that each atom is
   ! assumed to have unit mass.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,3,3), OUT :: st
      MAT(REAL,3,3) :: m
      VEC(REAL,3) :: c,r
      INT :: a
      c = center_of_atoms_(self)
      st = ZERO
      do a = 1,size(self)
         r = self(a)%position-c
         m = spread(r,dim=1,ncopies=3)*spread(r,dim=2,ncopies=3)
         call increment_diagonal_by_(m,-trace_(m))
         st = st-m
      end do
   end subroutine

   PURE subroutine make_shape_moments(self,sm,sa)
   ! Make the shape moments "sm" and principal shape axes "sa" wrt the
   ! center of atoms. The shape axes are the same as the principal
   ! moment of inertia axes except that each atom is assumed to have
   ! unit mass. The principal shape axes are made right handed.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), OUT :: sm
      MAT(REAL,3,3), OUT :: sa
      MAT(REAL,3,3) :: st
      call make_shape_tensor_(self,st)
      call diagonalize_Jacobi_(st,sm,sa)

      ! Make axes are close to x,y,z
      if (abs(sa(1,2))>abs(sa(1,1))) then
         call swap_columns_(sa,1,2)
         call swap_elements_(sm,1,2)
      end if
      if (abs(sa(1,3))>abs(sa(1,1))) then
         call swap_columns_(sa,1,3)
         call swap_elements_(sm,1,3)
      end if
      if (abs(sa(2,3))>abs(sa(2,2))) then
         call swap_columns_(sa,2,3)
         call swap_elements_(sm,2,3)
      end if

      ! Make axes +ve
      if (sa(1,1)<ZERO) sa(:,1) = -sa(:,1)
      if (sa(2,2)<ZERO) sa(:,2) = -sa(:,2)
      if (determinant_(sa)<ZERO) sa(:,3) = -sa(:,3)
   end subroutine


!  ===========
!  Shape tests
!  ===========

   PURE function is_linear(self,pm) result (res)
   ! Return TRUE if the geometry is linear.
   ! Needs principal moments of inertia "pm"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      BIN :: res
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      if (present(pm)) then; mi = pm
      else; call make_principal_moments_(self,mi,ma,reorder=TRUE)
      end if
      res = any(mi==ZERO)
   end function

   PURE function is_spherical_top(self,pm) result (res)
   ! Return TRUE if the geometry is a spherical top
   ! Needs principal moments of inertia "pm"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      BIN :: res
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      if (present(pm)) then; mi = pm
      else; call make_principal_moments_(self,mi,ma,reorder=TRUE)
      end if
      res = no_of_same_principal_moments_(self,mi)==3
   end function

   PURE function is_symmetric_top(self,pm) result (res)
   ! Return TRUE if the geometry is a symmetric top
   ! Needs principal moments of inertia "pm"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      BIN :: res
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      if (present(pm)) then
         mi = pm
      else
         call make_principal_moments_(self,mi,ma)
      end if
      res = no_of_same_principal_moments_(self,mi)==1
   end function

   PURE function is_prolate_top(self,pm) result (res)
   ! Return TRUE if the geometry is a prolate top
   ! Needs principal moments of inertia "pm" after alignment
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      BIN :: res
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      if (present(pm)) then; mi = pm
      else; call make_principal_moments_(self,mi,ma,reorder=TRUE)
      end if
      res = is_symmetric_top_(self,pm) AND (mi(3)<mi(1))
   end function

   PURE function is_oblate_top(self,pm) result (res)
   ! Return TRUE if the geometry is a oblate top
   ! Needs principal moments of inertia "pm" after alignment
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      BIN :: res
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      if (present(pm)) then; mi = pm
      else; call make_principal_moments_(self,mi,ma,reorder=TRUE)
      end if
      res = is_symmetric_top_(self,pm) AND (mi(3)>mi(1))
   end function

   PURE function is_asymmetric_top(self,pm) result (res)
   ! Return TRUE if the geometry is a asymmetric top
   ! Needs principal moments of inertia "pm"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      BIN :: res
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      if (present(pm)) then; mi = pm
      else; call make_principal_moments_(self,mi,ma,reorder=TRUE)
      end if
      res = no_of_same_principal_moments_(self,mi)==0
   end function

   PURE function no_of_same_principal_moments(self,pm) result (same)
   ! Return the number of "same" pairs of principal moments of inertia.
   ! Needs principal moments of inertia "pm"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), optional, IN :: pm
      INT :: same
      VEC(REAL,3) :: mi
      MAT(REAL,3,3) :: ma
      INT :: i,j
      if (present(pm)) then; mi = pm
      else; call make_principal_moments_(self,mi,ma,reorder=TRUE)
      end if
      same = 0
      do i = 1,3
         do j = 1,i-1
            if (abs(mi(i)-mi(j))<=TOL(2)) then
               same = same+1
            end if
         end do
      end do
   end function


!  =============
!  Box centering
!  =============

   PURE function center_of_atoms(self,axes) result (center)
   ! Return the centroid of the atom positions in "center". If "axes" is present
   ! then the "center" is expressed with respect to the new "axes", where the
   ! columns of "axes" are the coordinates of the new axes in terms of the old.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,3,3), optional, IN :: axes
      VEC(REAL,3) :: center
      INT :: a
      VEC(REAL,3) :: c
      center = ZERO
      do a = 1,size(self)
         center = center+self(a)%position
      end do
      center = center/size(self)
      if (present(axes)) then
         c = center
         call to_product_of_(center,axes,c,transpose_a=TRUE)
      end if
   end function

   PURE subroutine move_origin_to_center_of_atoms(self)
   ! Move the origin to the center of atoms
      VEC(type(ATOM_TYPE),:), INOUT :: self
      call translate_by_(self,-center_of_atoms_(self))
   end subroutine

   PURE function bounding_cube_width(self) result (width)
   ! Return "width" which is a width of a side of a cube in which the
   ! molecule nicely sits. Suitable for generating plot widths.
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: width
      VEC(REAL,3) :: center,dist
      REAL :: length
      INT :: a
      width = ZERO
      center = center_of_atoms_(self)
      do a = 1,size(self)
         dist = self(a)%position-center
         length = norm_(dist)+bragg_slater_radius_(self(a))*BOHR_PER_ANGSTROM*TWO
         length = max(length,norm_(dist)*TWO)
         width = max(width,length)
      end do
      width = TWO*width
   end function

   PURE function bounding_box_widths(self,axes) result (box)
   ! Return "box" which are three widths of a side of a box in which
   ! the molecule nicely sits. If "axes" is present, the "box"
   ! coordinates are expressed in terms of the new "axes", where the
   ! columns of "axes" are the coordinates of the new axes in terms of
   ! the old. These "axes" may be (typically) the principal moment
   ! axes. This routine is suitable for generating plot widths.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,3,3), optional, IN :: axes
      VEC(REAL,3) :: box
      VEC(REAL,3) :: center,v,dist
      INT :: a
      box = ZERO
      center = center_of_atoms_(self)
      do a = 1,size(self)

         ! Position relative to center
         dist = self(a)%position-center

         ! Position in new axis system
         if (present(axes)) then
            v = dist
            call to_product_of_(dist,axes,v,transpose_a=TRUE)
         end if

         ! Flip to +ve quadrant and add boundary
         dist = abs(dist)
         dist = dist+bragg_slater_radius_(self(a))*BOHR_PER_ANGSTROM*TWO

         ! Get box width
         box = max(box,dist)
      end do
      box = FOUR*box
   end function


!  ===========================================
!  Presence/absence of features and properties
!  ===========================================

   PURE function has_sequence_numbers(self) result (res)
   ! Return TRUE if any atom in the list has a non zero sequence number
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(self%atom_sequence_id>1)
   end function

   pure function has_covariance_mxs(self) result (res)
   ! Return TRUE if every atom has a covariance_mx defined.
   ! NOTE: "self" was declared allocatable purely to allow a .deallocated
   ! test on it, which meant this could not be called with an ordinary
   ! VEC{ATOM} dummy argument -- and so it never was. A plain IN self is
   ! enough: the array exists by the time anyone asks.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: i
      res = FALSE
      do i = 1,size(self)
         if (NOT allocated(self(i)%covariance_mx)) return
      end do
      res = TRUE
   end function

   PURE function has_nonzero_position_errors(self) result (res)
   ! Return TRUE if all atom have non-zero position errors
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: i
      res = TRUE
      do i = 1,size(self)
         if (is_zero_(norm_(self(i)%position_error))) then
            res = FALSE
            exit
         end if
      end do
   end function

   PURE function has_nonzero_ADP_tensors(self) result (res)
   ! Return TRUE if any atom has a non-zero ADP tensor
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: i
      res = FALSE
      do i = 1,size(self)
         if (all(self(i)%ADP_tensor==ZERO)) cycle
         res = TRUE
         exit
      end do
   end function

   PURE function has_nonzero_U_iso(self) result (res)
   ! Return TRUE if any atom in the list has a non zero U_iso
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(self%U_iso>ZERO)
   end function

   PURE function has_nonzero_ADPs(self) result (res)
   ! Return TRUE if any atom in the list has a non zero ADP tensor
   ! OR non zero isotropic thermal factor.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = has_nonzero_ADP_tensors_(self) OR has_nonzero_U_iso_(self)
   end function

   PURE function has_nonzero_pos_errors(self) result (res)
   ! Return TRUE if *any* atom has a non-zero position errors
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = FALSE
      do a = 1,size(self)
         if (NOT has_pADPs_and_errors_(self(a))) cycle
         if (is_zero_(norm_(self(a)%pADP_errors(1:3)))) cycle
         res = TRUE
         exit
      end do
   end function

   PURE function has_nonzero_pADP2_errors(self) result (res)
   ! Return TRUE if *any* atom has a non-zero pADP2 errors
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = FALSE
      do a = 1,size(self)
         if (NOT has_pADPs_and_errors_(self(a))) cycle
         if (is_zero_(norm_(self(a)%pADP_errors(1:9)))) cycle
         res = TRUE
         exit
      end do
   end function

   PURE function has_nonzero_ADP3_errors(self) result (res)
   ! Return TRUE if *any* atom has a non-zero pADP3 error
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = FALSE
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle
         if (is_zero_(norm_(self(a)%pADP_errors(10:19)))) cycle
         res = TRUE
         exit
      end do
   end function

   function has_nonzero_ADP4_errors(self) result (res)
   ! Return TRUE if *any* atom has a non-zero ADP4 error
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = FALSE
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle
         if (is_zero_(norm_(self(a)%pADP_errors(20:34)))) cycle
         res = TRUE
         exit
      end do
   end function

   pure function has_any_ADP3s_and_errors(self) result (res)
   ! Return TRUE if there are *any* ADP3s
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = FALSE
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle
         res = TRUE
         exit
      end do
   end function

   pure function has_any_ADP4s_and_errors(self) result (res)
   ! Return TRUE if there are *any* ADP4s
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = FALSE
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle
         res = TRUE
         exit
      end do
   end function

   PURE function has_residue_codes(self) result (res)
   ! Return TRUE if any atom in the list has a residue names
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(self(:)%compound_id/=" ")
   end function

   PURE function has_restraints(self) result (res)
   ! Return TRUE if any atom in the list has a restrained position
   ! or restrained force constants
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(self(:)%restraining_force_constant/=ZERO)
   end function

   PURE function has_dipoles(self) result (has)
   ! Return TRUE if any of the atoms have dipoles
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = FALSE
      do a = 1,size(self)
         has = has_dipole_(self(a))
         if (has) exit
      end do
   end function

   PURE function has_polarisabilities(self) result (has)
   ! Return TRUE if any of the atoms have polarisabilities
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = FALSE
      do a = 1,size(self)
         has = has_polarisability_(self(a))
         if (has) exit
      end do
   end function

   PURE function has_any_dispersion(self) result (res)
   ! Return TRUE if any atom has an anomalous scattering factor
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = any(has_dispersion_(self(:)))
   end function


!  ====================
!  ADP inquiry methods
!  ====================

   PURE function has_NPD_ADP(self) result (res)
   ! Return TRUE if the ADP is non-zero and non-positive definite.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(BIN,size(self)) :: res
      INT :: a
      do a = 1,size(self)
         res(a) = has_NPD_ADP_(self(a))
      end do
   end function

   PURE function has_flat_ADP(self,ratio) result (res)
   ! Return TRUE if the ADP is flat i.e. the the max to
   ! the min eigenvalue is greater than "ratio" (or FOUR
   ! if not supplied).
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL, optional, IN :: ratio
      VEC(BIN,size(self)) :: res
      INT :: a
      do a = 1,size(self)
         res(a) = has_flat_ADP_(self(a),ratio)
      end do
   end function

   PURE function ADP_principal_axis_ratio(self) result (res)
   ! Return the actual max/min ADP principal axis ratio
   ! (or 1000 if the min eigenvalue is zero).
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      INT :: a
      do a = 1,size(self)
         res(a) = ADP_principal_axis_ratio_(self(a))
      end do
   end function

   PURE function ADP_type(self,refine_H_U_iso) result (res)
   ! Return whether the atom is refined "Uani" or "Uiso"
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(STR(len=STR_SIZE),size(self)) :: res
      INT :: a
      if (NOT refine_H_U_iso OR all(self(:)%atomic_number>1)) then
         res = "Uani"
      else
         do a = 1,size(self)
            if (self(a)%atomic_number>1) then; res(a) = "Uani"
            else; res(a) = "Uiso"
            end if
         end do
      end if
   end function


!  ================
!  Bases and labels
!  ================

   PURE function has_unique_labels(self) result (has)
   ! Return TRUE if all non-dummy atoms have unique labels.
   ! A dummy atom has zero charge, no basis and a dummy-type label.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      BIN :: unique_labels,unique_x_kind
      VEC(INT,:), allocatable :: dum
      INT :: n
      n = no_of_nondummy_atoms_(self)
      if (n==0) then
         has = TRUE
      else
         call create_(dum,n)
         dum = nondummy_atoms_(self)
         unique_labels = NOT has_repetitions_(self(dum)%label)
         unique_x_kind = has_unique_xtal_kinds_(self,dum)
         has = unique_labels AND unique_x_kind
         call destroy_(dum)
      end if
   end function

   PURE subroutine get_1st_repeated_labels(self,li,lj)
   ! Return the indices of the first repeated labels
   ! in "li" and "lj".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, OUT :: li,lj
      VEC(INT,:), allocatable :: dum
      INT :: n
      n = no_of_nondummy_atoms_(self)
      if (n==0) then
         li = 0
         lj = 0
      else
         call create_(dum,n)
         dum = nondummy_atoms_(self)
         call get_1st_repetition_(self(dum)%label,li,lj)
         call destroy_(dum)
      end if
   end subroutine

   PURE function has_unique_tags(self) result (has)
   ! Return TRUE if all non-dummy atoms have unique tags.
   ! A dummy atom has zero charge, no basis and a dummy-type label.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      VEC(INT,:), allocatable :: dum
      INT :: n
      n = no_of_nondummy_atoms_(self)
      if (no_of_nondummy_atoms_(self)==0) then
         has = TRUE
      else
         call create_(dum,n)
         dum = nondummy_atoms_(self)
         has = NOT has_repetitions_(self(dum)%tag) AND NOT any(self(dum)%tag==" ")
         call destroy_(dum)
      end if
   end function

   pure function has_unique_kinds(self) result (has)
   ! Return TRUE if all atoms have unique kinds
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      has = NOT has_repetitions_(self(:)%kind)
   end function

   pure function has_unique_xtal_kinds_0(self) result (unq)
   ! Return TRUE if all atoms have unique *xtal* kinds
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: unq
      INT :: a,b
      unq = TRUE
      do a = 2,size(self)
         do b = 1,a-1
            if (NOT is_xtal_identical_to_(self(a),self(b))) cycle
            unq = FALSE
            return
         end do
      end do
   end function

   pure function has_unique_xtal_kinds_1(self,list) result (unq)
   ! Return TRUE if all atoms have unique *xtal* kinds
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: list
      BIN :: unq
      INT :: a,b
      unq = TRUE
      do a = 2,size(list)
         do b = 1,a-1
            if (NOT is_xtal_identical_to_(self(list(a)),self(list(b)))) cycle
            unq = FALSE
            return
         end do
      end do
   end function

   pure subroutine get_1st_repeated_xtal_kinds(self,ka,kb)
   ! Return the indices of the first repeated xtal kinds
   ! in "ka" and "kb".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, OUT :: ka,kb
      INT :: a,b
      ka = 0
      kb = 0
      do a = 2,size(self)
         do b = 1,a-1
            if (NOT is_xtal_identical_to_(self(a),self(b))) cycle
            ka = a
            kb = b
            return
         end do
      end do
   end subroutine

   pure function has_all_bases(self) result (has)
   ! Return TRUE if all atoms have a basis on one kind or another.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      has = has_all_gaussian_bases_(self) OR has_all_slater_bases_(self) OR has_all_coppens_bases_(self)
   end function

   pure function has_all_gaussian_bases(self) result (has)
   ! Return TRUE if all atom gaussian basis data exists
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = allocated(self(a)%basis)
         if (NOT has) exit
      end do
   end function

   pure function has_all_slater_bases(self) result (has)
   ! Return TRUE if all atom slater basis data exists
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = allocated(self(a)%slaterbasis)
         if (NOT has) exit
      end do
   end function

   pure function has_all_coppens_bases(self) result (has)
   ! Return TRUE if all atom slater basis data exists
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = allocated(self(a)%coppensbasis)
         if (NOT has) exit
      end do
   end function

   pure function has_all_density_data(self) result (has)
   ! Return TRUE if all atoms have some kind of atom density data, either
   ! slaterbasis, coppensbasis or gausian ANO data. This routine does NOT check
   ! that they are all the same kind of data!
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      has = has_all_slater_bases_(self) OR has_all_coppens_bases_(self) OR has_all_ANO_matrices_(self)
   end function

   PURE function has_all_shell_info(self) result (res)
   ! Return TRUE if all shell info is there for every atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = all(has_shell_info_(self))
   end function

   PURE function minimum_basis_exponents(self) result (res)
   ! Return the minimum exponent in the basis.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      INT :: i
      do i = 1,size(self)
         res(i) = minimum_basis_exponent_(self(i))
      end do
   end function

   PURE function tag_xyz_basis_fn_part(self) result (res)
   ! Return a character representation for the cartesian product part
   ! of the gaussians in this shell.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),no_of_basis_functions_(self)) :: res
      INT :: a,i,f,l,width,spaces
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      l = 0
      do a = 1,size(self)
         f = l+1
         l = l+no_of_basis_functions_(self(a)%basis)
         labels = xyz_basis_fn_part_(self(a)%basis)
         width = maxval(len_trim(labels))+1
         do i = f,l
            spaces = width-len_trim(labels(i-f+1))
            res(i) = trim(self(a)%tag)//repeat(" ",spaces)//trim(labels(i-f+1))
         end do
      end do
   end function

   PURE function basis_labels(self) result (labels)
   ! Return a list of basis set "labels". Missing labels are returned blank.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      INT :: i
      call create_(labels,size(self))
      do i = 1,size(self)
         if (self(i)%basis_label/=" ") then; labels(i) = self(i)%basis_label
         else; labels(i) = " "
         end if
      end do
   end function

   PURE function library_basis_labels_0(self,suffix) result (labels)
   ! Return a list of library basis set labels, the atoms element name with
   ! ":suffix" appended to it. Only a unique list of basis labels is returned.
      VEC(type(ATOM_TYPE),:), IN :: self
      STR(len=*), IN :: suffix
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      INT :: i
      call create_(labels,size(self))
      do i = 1,size(self)
         labels(i) = library_basis_label_(self(i),suffix)
      end do
      call remove_repetitions_(labels)
   end function

   PURE function library_basis_labels_1(Z_max,suffix) result (labels)
   ! Return a list of library basis set labels, for the atoms with
   ! atomic numbers up to "Z_max", with ":suffix" appended to it. Only
   ! a unique list of basis labels is returned.
      INT, IN :: Z_max
      STR(len=*), IN :: suffix
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      INT :: Z
      call create_(labels,Z_max)
      do Z = 1,Z_max
         labels(Z) = library_basis_label_(Z,suffix)
      end do
   end function


!  ======================
!  ANOs and interpolators
!  ======================

   pure function has_all_NAO_matrices(self) result (res)
   ! Return TRUE if the NAOs are made for every atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = TRUE
      do a = 1,size(self)
         res = has_NAO_matrices_(self(a))
         if (NOT res) exit
      end do
   end function

   pure function has_all_ANO_matrices(self) result (res)
   ! Return TRUE if the ANOs are made for every atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = TRUE
      do a = 1,size(self)
         res = has_ANO_matrices_(self(a))
         if (NOT res) exit
      end do
   end function

   pure function has_all_atom_data(self) result (res)
   ! Return TRUE if the ANOs are made for every atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = all(self(:)%atom_data_made)
   end function


!  =============
!  Interpolators
!  =============

   pure function has_all_slater_interpolators(self) result (has)
   ! Return TRUE if all atoms have slaterbases which have interpolators.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = allocated(self(a)%slaterbasis)
         if (NOT has) exit
         has = allocated(self(a)%slaterbasis%interpolator)
         if (NOT has) exit
      end do
   end function

   pure function has_all_coppens_interpolators(self) result (has)
   ! Return TRUE if all atoms have coppensbases which have interpolators.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = allocated(self(a)%coppensbasis)
         if (NOT has) exit
         has = allocated(self(a)%coppensbasis%interpolator)
         if (NOT has) exit
      end do
   end function

   pure function has_interpolators_any_kind(self) result (has)
   ! Return TRUE if all atoms have slaterbases which have interpolators, or all
   ! atoms have coppensbases with interpolators.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      BIN :: has_i,has_s,has_c
      has_i = has_all_interpolators_(self)
      has_s = has_all_slater_interpolators_(self)
      has_c = has_all_coppens_interpolators_(self)
      has = has_i OR has_s OR has_c
   end function

   pure function has_all_interpolators(self) result (has)
   ! Return TRUE if all atoms have interpolators.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = allocated(self(a)%interpolator)
         if (NOT has) exit
      end do
   end function

   function max_interpolator_table_length(self,tol) result (res)
   ! Returns the maximum interpolator table length for a given table cutoff
   ! tolerance "tol".  This table length is used for setting the cluster radius,
   ! for calculations where the cluster size must be determined so that the
   ! Hirshfeld surface is accurate.
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL, IN :: tol
      REAL :: res
      REAL :: last
      INT :: i
   ENSURE(is_one_of_(self(1)%basis_kind,["slater  ","coppens ","gaussian"]),"VEC{ATOM}:max_interpolator_table_length ... unknown basis_kind")
      res = ZERO
      select case (self(1)%basis_kind)

      case ("slater")
         ENSURE(has_all_slater_bases_(self),"VEC{ATOM}:max_interpolator_table_length ... missing slater bases")
         do i = 1,size(self)
            last = interpolator_table_length_(self(i)%slaterbasis,tol)
            res = max(res,last)
         end do

      case ("coppens")
         ENSURE(has_all_coppens_bases_(self),"VEC{ATOM}:max_interpolator_table_length ... missing coppens bases")
         do i = 1,size(self)
            last = interpolator_table_length_(self(i)%coppensbasis,tol)
            res = max(res,last)
         end do

      case ("gaussian")
         ENSURE(has_all_ANO_matrices_(self),"VEC{ATOM}:max_interpolator_table_length ... missing atom ANOs")
         do i = 1,size(self)
            last = interpolator_table_length_(self(i),tol)
            res = max(res,last)
         end do
      end select
   end function

   PURE subroutine make_Hirshfeld_weight_grid(self,grid,pt,c)
   ! Make the Hirshfeld weights in "grid" for the supplied points
   ! "pt", for atom "c" in a sufficientl;y large "atom" cluster.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pt
      INT, IN :: c
      VEC(REAL,:), allocatable :: rho_c
      INT :: n_pt,a
      n_pt = size(grid)
      call create_(rho_c,n_pt)
      rho_c = ZERO
      if (NOT has_disorder_(self)) then
         call add_ED_grid_(self(c)%slaterbasis,rho_c,pt,self(c)%position)
         grid = rho_c
         do a = 1,size(self)
            if (a==c) cycle
            call add_ED_grid_(self(a)%slaterbasis,grid,pt,self(a)%position)
         end do
      else
         call add_ED_grid_(self(c)%slaterbasis,rho_c,pt,self(c)%position,self(c)%site_occupancy)
         grid = rho_c
         do a = 1,size(self)
            if (a==c) cycle
            call add_ED_grid_(self(a)%slaterbasis,grid,pt,self(a)%position,self(a)%site_occupancy)
         end do
      end if
      where (grid>ZERO)
         grid = rho_c/grid
      elsewhere
         grid = ZERO
      end where
      call destroy_(rho_c)
   end subroutine


!  =================
!  Axes and disorder
!  =================

   pure function has_cartesian_axes(self) result (res)
   ! Return TRUE if all atom pisitions *and* ADP (thermal) tensors are
   ! in the cartesian axis system.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      BIN :: pos,ADP
      INT :: a
      res = TRUE
      do a = 1,size(self)
         pos = self(a)%pos_axis_system=="cartesian"
         ADP = self(a)%ADP_axis_system=="cartesian"
         res = pos AND ADP
         if (has_ADP3s_(self(a))) res = res AND self(a)%ADPn_axis_system=="cartesian"
         if (has_ADP4s_(self(a))) res = res AND self(a)%ADPn_axis_system=="cartesian"
         if (NOT res) exit
      end do
   end function

   pure function has_cartesian_ADP_axes(self) result (res)
   ! Return TRUE if all atom ADP (thermal) tensors are in the
   ! cartesian axis system.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a
      res = TRUE
      do a = 1,size(self)
         res = self(a)%ADP_axis_system=="cartesian"
         if (NOT res) exit
      end do
   end function

   pure function has_crystal_ADP_axes(self) result (has)
   ! Return TRUE if all atom ADP (thermal) tensors are in the crystal axis system.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      INT :: a
      has = TRUE
      do a = 1,size(self)
         has = self(a)%ADP_axis_system=="crystal"
         if (NOT has) exit
      end do
   end function

   pure function has_disorder(self) result (has)
   ! Return TRUE if any atom has disorder i.e. an occupancy which is not one.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: has
      has = NOT has_all_equal_to_(self(:)%site_occupancy,ONE)
   end function

   PURE function atom_index_from_position(self,pos) result (res)
   ! Return the index of atom from its position "pos".
   ! If no position is found return zero.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), IN :: pos
      INT :: res
      INT :: a
      BIN :: found
      res = 0
      found = FALSE
      do a = 1,size(self)
         found = same_as_(pos,self(a)%position)
         if (found) then
            res = a
            exit
         end if
      end do
   end function


!  =====
!  Radii
!  =====

   PURE function covalent_radii_ccdc(self) result (res)
   ! Returns the CCDC covalent radius for this atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      INT :: a
      do a = 1,size(self)
         res(a) = covalent_radius_ccdc_(self(a))
      end do
   end function

   PURE function vdw_radii_ccdc(self) result (res)
   ! Returns the CCDC vdw radius for this atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,size(self)) :: res
      INT :: a
      do a = 1,size(self)
         res(a) = vdw_radius_ccdc_(self(a))
      end do
   end function

   PURE subroutine get_covalent_radii_ccdc(self,radii)
   ! Returns the CCDC covalent radius for this atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,size(self)), OUT :: radii
      INT :: a
      do a = 1,size(self)
         radii(a) = covalent_radius_ccdc_(self(a))
      end do
   end subroutine

   pure subroutine get_vdw_radii_ccdc(self,radii)
   ! Returns the CCDC vdw radius for this atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,size(self)), OUT :: radii
      INT :: a
      do a = 1,size(self)
         radii(a) = vdw_radius_ccdc_(self(a))
      end do
   end subroutine


!  =======================
!  Spackman's coefficients
!  =======================

   pure subroutine get_Spackman86_a_coeffs_in(self,coeff)
   ! Returns the GKR a-coefficients
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: coeff
      INT :: i
      do i = 1,size(self)
         coeff(i) = Spackman86_a_6_dispersion_coeff(self(i))
      end do
   end subroutine

   pure subroutine get_Spackman86_b_coeffs_in(self,coeff)
   ! Returns the GKR a-coefficients
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: coeff
      INT :: i
      do i = 1,size(self)
         coeff(i) = Spackman86_b_repulsion_coeff(self(i))
      end do
   end subroutine

   pure subroutine get_Spackman86_c_coeffs_in(self,coeff)
   ! Returns the GKR a-coefficients
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: coeff
      INT :: i
      do i = 1,size(self)
         coeff(i) = Spackman86_c_repulsion_coeff(self(i))
      end do
   end subroutine


!  =====================
!  Grimme's coefficients
!  =====================

   pure subroutine get_Grimme06_a_coeffs_in(self,coeff)
   ! Returns the Grimme a-coefficients
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: coeff
      INT :: i
      do i = 1,size(self)
         coeff(i) = Grimme06_a_6_dispersion_coeff(self(i))
      end do
   end subroutine

   pure subroutine get_Grimme06_r_coeffs_in(self,coeff)
   ! Returns the Grimme a-coefficients
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), OUT :: coeff
      INT :: i
      do i = 1,size(self)
         coeff(i) = Grimme06_r_0_dispersion_coeff(self(i))
      end do
   end subroutine


!  ================================
!  Moments, fields, field gradients
!  ================================

   PURE function nuclear_dipole_moment(self) result (res)
   ! Return the dipole moment obtained from the atomic numbers.
   ! Dummy nuclear charges are not included
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3) :: res
      INT :: a
      res = ZERO
      do a = 1,size(self)
         res = res+self(a)%atomic_number*self(a)%position
      end do
   end function

   PURE function nuclear_quadrupole_moment_vec(self) result (res)
   ! Return the quadrupole moment from the nuclear charges
   ! as a vector, in the order: xx, yy, zz, xy, xz, yz
   ! Dummy nuclear charges are not included
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,6) :: res
      REAL :: Z
      INT :: a
      VEC(REAL,3) :: pos
      res = ZERO
      do a = 1,size(self)
         Z = self(a)%atomic_number
         pos = self(a)%position
         res(1) = res(1)+Z*pos(1)*pos(1)
         res(2) = res(2)+Z*pos(2)*pos(2)
         res(3) = res(3)+Z*pos(3)*pos(3)
         res(4) = res(4)+Z*pos(1)*pos(2)
         res(5) = res(5)+Z*pos(1)*pos(3)
         res(6) = res(6)+Z*pos(2)*pos(3)
      end do
   end function

   PURE function nuclear_octupole_moment_vec(self) result (res)
   ! Return the octupole moment from the nuclear charges 
   ! as a vector, in the order: xxx, yyy, zzz, 
   ! xxy, xxz, yyx, yyz, zzx, zzy, xyz
   ! Dummy nuclear charges are not included
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,10) :: res
      REAL :: Z
      INT :: a
      VEC(REAL,3) :: pos
      res = ZERO
      do a = 1,size(self)
         Z = self(a)%atomic_number
         pos = self(a)%position
         res(1) = res(1)+Z*pos(1)*pos(1)*pos(1)
         res(2) = res(2)+Z*pos(2)*pos(2)*pos(2)
         res(3) = res(3)+Z*pos(3)*pos(3)*pos(3)
         res(4) = res(4)+Z*pos(1)*pos(1)*pos(2)
         res(5) = res(5)+Z*pos(1)*pos(1)*pos(3)
         res(6) = res(6)+Z*pos(2)*pos(2)*pos(1)
         res(7) = res(7)+Z*pos(2)*pos(2)*pos(3)
         res(8) = res(8)+Z*pos(3)*pos(3)*pos(1)
         res(9) = res(9)+Z*pos(3)*pos(3)*pos(2)
         res(10) = res(10)+Z*pos(1)*pos(2)*pos(3)
      end do
   end function

   PURE function nuclear_E_field_at_positions(self,pos) result (res)
   ! Return the nuclear contribution to the electric field from "self"
   ! at the positions "pos" as a (3 x .dim) array
   ! NOTE: "pos" should not be a nuclear position!
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: pos
      MAT(REAL,3,size(pos,2)) :: res
      REAL :: Z,r
      INT :: a,b
      VEC(REAL,3) :: ab
      res = ZERO
      do a = 1,size(pos,2)
         do b = 1,size(self)
            Z = self(b)%nuclear_charge
            ab = pos(:,a)-self(b)%position
            r = norm_(ab)
            res(:,a) = res(:,a)+Z*ab/(r*r*r)
         end do
      end do
   end function

   PURE subroutine add_nuclear_E_field_at_points_to(self,res,pt)
   ! Add the nuclear contribution to the electric field from "self" at
   ! the points "pt" (n_pt x 3) to the (n_pt x 3) array "res"
   ! NOTE: a point should not be a nuclear position!
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: pt
      MAT(REAL,size(pt,1),3), INOUT :: res
      REAL :: Z,r
      INT :: a,b
      VEC(REAL,3) :: ab
      do b = 1,size(self)
         do a = 1,size(pt,1)
            Z = self(b)%nuclear_charge
            ab = pt(a,:)-self(b)%position
            r = norm_(ab)
            res(a,:) = res(a,:)+Z*ab/(r*r*r)
         end do
      end do
   end subroutine

   PURE function nuclear_E_field_at_nuclei(self) result (res)
   ! Return the nuclear contribution to the electric fields
   ! at the positions of the nuclei as a (3 x .dim) array
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,3,size(self)) :: res
      REAL :: Z,r
      INT :: a,b
      VEC(REAL,3) :: ab
      res = ZERO
      do a = 1,size(self)
         do b = 1,size(self)
            if (b==a) cycle  ! skip
            Z = self(b)%nuclear_charge
            ab = self(a)%position-self(b)%position
            r = norm_(ab)
            res(:,a) = res(:,a)+Z*ab/(r*r*r)
         end do
      end do
   end function

   PURE function nuclear_E_field_at_nuclei_of(self,atoms) result (res)
   ! Return the nuclear contribution to the electric fields
   ! from "self" at the positions of the nuclei of "atoms"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atoms
      MAT(REAL,3,size(atoms)) :: res
      MAT(REAL,:,:), allocatable :: pos
      pos = coordinates_(atoms)
      res = nuclear_E_field_at_positions_(self,pos)
      call destroy_(pos)
   end function

   PURE function nuclear_EFG_at_nuclei(self) result (res)
   ! Return the nuclear contribution to the electric fields gradient
   ! (EFG) at the nuclei as a (6 x .dim) array
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,6,size(self)) :: res
      REAL :: Z,r,r3,r5
      INT :: a,b
      VEC(REAL,3) :: ab
      res = ZERO
      do a = 1,size(self)
         do b = 1,size(self)
            if (b==a) cycle
            Z = self(b)%nuclear_charge
            ab = self(a)%position-self(b)%position
            r = norm_(ab)
            r3 = r*r*r
            r5 = r3*r*r
            res(1,a) = res(1,a)-Z*(THREE*ab(1)*ab(1)/r5-ONE/r3)
            res(2,a) = res(2,a)-Z*(THREE*ab(2)*ab(2)/r5-ONE/r3)
            res(3,a) = res(3,a)-Z*(THREE*ab(3)*ab(3)/r5-ONE/r3)
            res(4,a) = res(4,a)-Z*(THREE*ab(1)*ab(2)/r5)
            res(5,a) = res(5,a)-Z*(THREE*ab(1)*ab(3)/r5)
            res(6,a) = res(6,a)-Z*(THREE*ab(2)*ab(3)/r5)
         end do
      end do
   end function


!  ========================
!  Atom kinds, unique atoms
!  ========================

   pure function same_kind_of_atoms(self,a,b) result (res)
   ! Return true if atoms "a" and "b" are the same kind
   ! . They are the same kind if they have the same kind of basis
   !   and same atomic number.
   ! . This is used for ANO or other QM type calculations.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      BIN :: res
      res = is_same_kind_as_(self(a),self(b))
   end function

   PURE subroutine make_atom_Z_list(self,Z_list)
   ! Make the atom Z list ... Z_list(k).element(c) is the c-th atom of
   ! the same atomic number (atomic numbers in decreasing order).
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: Z_list
      INT :: n_kind,n,k,Z_n
      VEC(INT,:), allocatable :: Z,unique_Z

      ! Atomic numbers
      call create_(Z,size(self))
      Z = self(:)%atomic_number

      ! Sorted unique atomic numvers
      call to_unique_elements_of_(unique_Z,Z)
      call quick_sort_(unique_Z,decreasing_order=TRUE)

      ! Assign each atom to its kind in
      ! decreasing order
      n_kind = size(unique_Z)
      call create_(Z_list,n_kind,0)
      do n = 1,size(self)
         Z_n = self(n)%atomic_number
         do k = 1,n_kind
            if (Z_n==unique_Z(k)) then
               call append_(Z_list(k)%element,n)
               exit
            end if
         end do
      end do

      ! Clean up
      call destroy_(unique_Z)
      call destroy_(Z)
   end subroutine

   PURE subroutine make_atom_kind_list(self,atom_kind,n_kind)
   ! Make the atom=kind list ... atom_kind(k) is the kind index of
   ! the k-th atom, as determined by .same_kind_of_atoms(k,l)
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), OUT :: atom_kind
      INT, OUT :: n_kind
      INT :: n_atom,n,k,l
   ENSURE(size(atom_kind)==size(self),"VEC{ATOM}:make_atom_kind_list ... atom_kind is incorrectly dimensioned")
      n_atom = size(self)
      atom_kind = [(n,n = 1,n_atom)]
      k = 0
      do n = 1,n_atom

         ! Found kind for atom n?
         if (atom_kind(n)<=k) cycle

         ! Set kind
         k = k+1
         atom_kind(n) = k

         ! Set non-unique kinds
         do l = n+1,n_atom
            if (atom_kind(l)<=k) cycle
            if (NOT same_kind_of_atoms_(self,l,n)) cycle
            atom_kind(l) = k
         end do
      end do
      n_kind = k
   end subroutine

   PURE subroutine make_unique_atom_list(self,unique_atom)
   ! "unique_atom(k)" is the index of the first atom which represents
   ! all the the atoms which are of the same kind, k. This requires
   ! "is_unique_kind" to have been made via "update_kinds".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), OUT :: unique_atom
      INT :: n_kind,k,n
   ENSURE(size(unique_atom)==maxval(self(:)%kind),"VEC{ATOM}:make_unique_atom_list ... unique atom incorrectly dimensioned")
      n_kind = maxval(self(:)%kind)

      ! Make the list
      unique_atom = 0
      k = 0
      do n = 1,size(self)
         if (NOT self(n)%is_unique_kind) cycle
         k = k+1
         unique_atom(k) = n
      end do
   end subroutine

   pure function are_distinct(self,a,b) result (res)
   ! Return true if atoms "a" and "b" are crystallographically
   ! distinct based on occupancy and atomic number
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      BIN :: res
      res = self(a)%atomic_number/=self(b)%atomic_number OR self(a)%site_disorder_group/=self(b)%site_disorder_group OR NOT equals_(self(a)%site_occupancy,self(b)%site_occupancy)
   end function


!  ===========
!  Atom groups
!  ===========

   pure function has_site_groups(self) result (res)
   ! Return TRUE if any crstal site dirder groups have been defined
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      if (any(self(:)%site_disorder_group>0)) then; res = TRUE
      else; res = FALSE
      end if
   end function

   pure function has_groups(self) result (res)
   ! Return TRUE if more than one atom group has been defined.
   ! This only counts if there is more than one group!
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      if (any(self(:)%group>1)) then; res = TRUE
      else; res = FALSE
      end if
   end function

   pure function no_of_groups(self) result (res)
   ! Return the number of groups
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = maxval(self(:)%group)
   end function

   PURE subroutine set_connected_groups(self)
   ! Search through the list of atoms and assign each atom the same
   ! group number if the atom is a part of the same connected
   ! molecule.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: atoms_bonded_to_atom
      VEC(INT,:), allocatable :: group_for
      INT :: n_atoms,a,b,c,n,ga,gb
      INT :: n_group,g

      ! Can only proceed in cartesian axes
      if (NOT all(is_in_cartesian_system_(self))) return

      ! Make the bond list
      call make_connection_table_(self,atoms_bonded_to_atom)

      ! Assume every atom it's own molecule
      n_atoms = size(self)
      call to_sequence_(group_for,1,n_atoms)

      ! Loop on all atoms "a"
      do a = 1,n_atoms

         ! Group "ga" foratom "a" 
         ga = group_for(a)

         ! No of atoms bonded to atom "a"
         n = size(atoms_bonded_to_atom(a)%element)

         ! Loop over bonded atoms
         do c = 1,n
            b = atoms_bonded_to_atom(a)%element(c)
            gb = group_for(b)
            if (gb>ga) then
               where (group_for==gb) group_for = ga
            else if (gb<ga) then
               where (group_for==ga) group_for = gb
            end if
         end do
      end do

      ! Make groups consecutive
      n = maxval(group_for)
      group_for = -group_for
      n_group = 0
      do g = -1,-n,-1
         if (any(group_for==g)) then
            n_group = n_group+1
            where (group_for==g) group_for = n_group
         end if
      end do

      ! Assign
      call set_groups_to_(self,group_for)

      ! Clean
      call destroy_(group_for)
      call destroy_(atoms_bonded_to_atom)
   end subroutine

   subroutine make_atom_group_list(self,atom_group)
   ! This routine finds all the atoms in the same group and makes
   ! their indices as a atom_group of integers. The result is a
   ! "atom_group" of atom_group of integers. NOTE that "atom_group" is
   ! created.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: atom_group
      INT :: n_group,g,n,i
      VEC(INT,:), allocatable :: group_indices

      ! Extract the group indices
      call create_(group_indices,size(self))
      group_indices = self%group
      n_group = no_of_unique_elements_(group_indices)

      ! Create the atom_group list of indices
      call create_(atom_group,n_group)

      ! All atoms in one group, size .dim
      if (n_group==1) then
         call create_(atom_group(1)%element,size(self))
         atom_group(1)%element = [(i,i = 1,size(self))]
      ! Atoms in different groups, size n_group
      else
         do g = 1,n_group
            n = count(group_indices==g)
            DIE_IF(n==0,"VEC{ATOM}:make_atom_group_list ... group indices are not sequential!")
            call create_(atom_group(g)%element,n)
            atom_group(g)%element = pack([(i,i = 1,size(self))],group_indices==g)
         end do
      end if

      ! Clean
      call destroy_(group_indices)
   end subroutine

   pure subroutine set_group_to(self,g)
   ! Set the atom "group" index to "g".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, IN :: g
      INT :: a
      do a = 1,size(self)
         call set_group_(self(a),g)
      end do
   end subroutine

   pure subroutine set_groups_to(self,groups)
   ! Set the atom "group" index to "groups".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(INT,:), IN :: groups
      INT :: a
      do a = 1,size(self)
         call set_group_(self(a),groups(a))
      end do
   end subroutine

   pure function no_of_compounds(self) result (res)
   ! Return the number of different compound_id's
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      res = maxval(self(:)%compound_sequence_id)
   end function


!  ========================
!  Sorted list of basis fns
!  ========================

   pure function maximum_basis_set_l_value(self) result (res)
   ! Returns the maximum basis set l value over all atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a,s,ls
      res = 0
      do a = 1,size(self)
         do s = 1,size(self(a)%basis%shell)
            ls = self(a)%basis%shell(s)%l
            res = max(ls,res)
         end do
      end do
   end function

   PURE subroutine make_L_sorted_basis(self,basis)
   ! Make a concatenated basis sorted by descending L 
   ! angular momentum for efficient integral evaluation.
   ! So that the first and third basis functions have
   ! high angular momentum, which means a smaller horizontal
   ! recurrence relation.
      VEC(type(ATOM_TYPE),:), IN :: self
      type(BASIS_TYPE), OUT :: basis
      INT :: n,max_L
      INT :: l,a,s,ls
      call create_(basis%shell,no_of_shells_(self))
      max_L = maximum_basis_set_l_value_(self)
      n = 0
      do l = max_L,0
         do a = 1,size(self)
            do s = 1,size(self(a)%basis%shell)
               ls = self(a)%basis%shell(s)%l
               if (ls==l) then
                  n = n+1
                  basis%shell(n) = self(a)%basis%shell(s)
                  basis%atom = a
               end if
            end do
         end do
      end do

      ! Update
      call update_(basis)
   end subroutine


!  =========================
!  Shell information methods
!  =========================

   PURE subroutine make_shell_limits_0(self,first,last)
   ! Get the shell function limit vectors "first" and "last" for
   ! corresponding to the concatenated vector of atomvec shells.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: first,last
      INT :: n_shell,a,as,n,ss,f,l
      n_shell = no_of_shells_(self)
      call create_(first,n_shell)
      call create_(last,n_shell)
      ss = 0
      l = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         do as = 1,n
            ss = ss+1
            f = l+1
            l = f+self(a)%basis%shell(as)%n_bf-1
            first(ss) = f
            last(ss) = l
         end do
      end do
   end subroutine

   PURE subroutine make_shell_limits_1(self,first,last,momentum)
   ! Get the shell function limit vectors "first" and "last" for
   ! corresponding to the concatenated vector of atomvec shells. Also
   ! get the angular "momentum" for the shell.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: first,last,momentum
      INT :: n_shell,a,as,n,ss,f,l
      n_shell = no_of_shells_(self)
      call create_(first,n_shell)
      call create_(last,n_shell)
      call create_(momentum,n_shell)
      ss = 0
      l = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         do as = 1,n
            ss = ss+1
            f = l+1
            l = f+self(a)%basis%shell(as)%n_bf-1
            first(ss) = f
            last(ss) = l
            momentum(ss) = self(a)%basis%shell(as)%l
         end do
      end do
   end subroutine

   PURE subroutine make_cartesian_shell_limits(self,first,last)
   ! Get the shell function limit vectors "first" and "last" as above,
   ! but always counting cartesian components, whatever the basis is
   ! set to. A spherical basis uses these to drive the cartesian J/K
   ! engine; see MOLECULE.FOCK:make_r_JK_engine.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: first,last
      INT :: n_shell,a,as,n,ss,f,l
      n_shell = no_of_shells_(self)
      call create_(first,n_shell)
      call create_(last,n_shell)
      ss = 0
      l = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         do as = 1,n
            ss = ss+1
            f = l+1
            l = f+self(a)%basis%shell(as)%n_comp-1
            first(ss) = f
            last(ss) = l
         end do
      end do
   end subroutine

   PURE subroutine make_atom_basis_fn_limits(self,first,last)
   ! Get the first and last basis functions for each atom in "self"
   ! (treating the basis set as the flattened list of basis functions
   ! on each atom)
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: first,last
      INT :: a,as,n,l
      call create_(first,size(self))
      call create_(last,size(self))
      l = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         first(a) = l+1
         do as = 1,n
            l = l+self(a)%basis%shell(as)%n_bf
         end do
         last(a) = l
      end do
   end subroutine

   PURE subroutine make_primitive_limits(self,frst,last,atom,lmom,expo)
   ! Get the primitive function limit vectors "frst" and "last" and
   ! other data pertaining to the primitive function corresponding to
   ! the concatenated bases of all the atoms.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: frst
      VEC(INT,:), allocatable, OUT :: last
      VEC(INT,:), allocatable, OUT :: atom
      VEC(INT,:), allocatable, OUT :: lmom
      VEC(REAL,:), allocatable, OUT :: expo
      INT :: nps,a,as,ap,p,f,l
      nps = no_of_primitive_shells_(self)
      call create_(frst,nps)
      call create_(last,nps)
      call create_(atom,nps)
      call create_(lmom,nps)
      call create_(expo,nps)
      p = 0
      l = 0
      do a = 1,size(self)
         do as = 1,size(self(a)%basis%shell)
            do ap = 1,self(a)%basis%shell(as)%n_cc
               p = p+1
               f = l+1
               l = l+self(a)%basis%shell(as)%n_bf
               frst(p) = f
               last(p) = l
               atom(p) = a
               lmom(p) = self(a)%basis%shell(as)%l
               expo(p) = self(a)%basis%shell(as)%exponent(ap)
            end do
         end do
      end do
   end subroutine

   PURE subroutine make_atom_for_basis_fn_in(self,at4bf)
   ! Make "at4bf", the atom for the basis function index, treating the
   ! basis set as the flattened list.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: at4bf
      INT :: n_bf,a,as,f,l
      n_bf = no_of_basis_functions_(self)
      call create_(at4bf,n_bf)
      l = 0
      do a = 1,size(self)
         f = l+1
         do as = 1,size(self(a)%basis%shell)
            l = l+self(a)%basis%shell(as)%n_bf
         end do
         at4bf(f:l) = a
      end do
   end subroutine


!  ========================
!  Basis shell info methods
!  ========================

   PURE subroutine make_basis_shell_for_shell(self,res,basis)
   ! Return "res" which has the index of the first shell in .basis
   ! (treating .basis as a flattened list of shells) for a given shell
   ! index in the molecular basis set (treated as the flattened list
   ! of shells in the .atom list). 
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(INT,:), allocatable, OUT :: res
      VEC(type(BASIS_TYPE),:), IN :: basis
      INT :: sh,a,as,f

      ! Index array
      call create_(res,no_of_shells_(self))

      ! Loop over atoms
      sh = 0
      do a = 1,size(self)

         ! Index of first shell in "basis" which matches atoms basis
         f = first_shell_for_basis_(basis,label=trim(self(a)%basis%label))

         ! Loop over this atoms shells
         do as = 1,size(self(a)%basis%shell)
            sh = sh+1
            res(sh) = f+as-1
         end do
      end do
   end subroutine


!  ==============================
!  Atom-shell information methods
!  ==============================

   pure function atom_for_shell(self,s) result (a)
   ! Return the *atom* number "a" corresponding to the
   ! flattened shell number "s"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: s
      INT :: a
      INT :: ss,n
      ss = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         ss = ss+n
         if (s<=ss) exit
      end do
   end function

   PURE subroutine make_atom_for_shell(self,res)
   ! Return a vector of *atom* numbers corresponding to the
   ! flattened shell numbers
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: res
      INT :: a,s,n
      call create_(res,no_of_shells_(self))
      s = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         res(s+1:s+n) = a
         s = s+n
      end do
   end subroutine

   pure function atom_shell_for_shell(self,s) result (as)
   ! Return the *atom* shell number "as" corresponding to the
   ! flattened shell number "s"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: s
      INT :: as
      INT :: a,ss,n
      n = 0
      ss = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         ss = ss+n
         if (s<=ss) exit
      end do
      as = s-ss+n
   end function

   PURE subroutine make_atom_shell_for_shell(self,res)
   ! Return a vector of *atom* shell numbers corresponding to the
   ! flattened shell vector
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: res
      INT :: a,ss,n,as
      call create_(res,no_of_shells_(self))
      ss = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         do as = 1,n
            res(ss+as) = as
         end do
         ss = ss+n
      end do
   end subroutine

   pure function first_shell_for_atom(self,a) result (res)
   ! Return the index of the first shell for atom "a"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      INT :: res
      INT :: at,n
      res = 1
      do at = 1,(a-1)
         n = size(self(a)%basis%shell)
         res = res+n
      end do
   end function

   pure subroutine make_shell_for_atom_limits(self,first,last)
   ! Return the indices of the first and last shell for each atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), OUT :: first,last
      INT :: ss,a,n
      ss = 0
      do a = 1,size(self)
         first(a) = ss+1
         n = size(self(a)%basis%shell)
         ss = ss+n
         last(a) = ss
      end do
   end subroutine

   PURE subroutine make_first_shell_for_atom(self,res)
   ! Return the indices of the first shell for each atom
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: res
      INT :: ss,a,n
      call create_(res,size(self))
      ss = 0
      do a = 1,size(self)
         n = size(self(a)%basis%shell)
         res(a) = ss+1
         ss = ss+n
      end do
   end subroutine


!  ===============
!  Invariom labels
!  ===============

   pure function neighbours_of(self,a,range_factor) result (res)
   ! Return the list of atoms bonded to atom "a".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      REAL, optional, IN :: range_factor
      VEC(INT,:), allocatable :: res
      INT :: b
      do b = 1,size(self)
         if (b==a) cycle
         if (NOT bonded_(self,a,b,range_factor)) cycle
         call append_(res,b)
      end do
   end function

   pure function is_next_neighbour_chiral(self,a) result (res)
   ! Return TRUE if there are four next nearest neighbours for the
   ! atom "a", and if they are all different.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      BIN :: res
      VEC(INT,:), allocatable :: neighbour
      VEC(INT,4) :: Z
      neighbour = neighbours_of_(self,a)
      if (NOT allocated(neighbour)) then
         res = FALSE
      else if (size(neighbour)==4) then
         Z = self(neighbour)%atomic_number
         res = NOT has_repetitions_(Z)
      else
         res = FALSE
      end if
   end function

   PURE function next_neighbour_handedness_of(self,a) result (res)
   ! Return the handedness of the atom "a" based only on the four
   ! connecting atoms around it.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      STR(len=STR_SIZE) :: res
      INT :: n
      VEC(REAL,3) :: p2,p3,p4,pc
      VEC(INT,4) :: Z,order
      VEC(INT,:), allocatable :: neighbour
      neighbour = neighbours_of_(self,a)
      ENSURE(allocated(neighbour),"VEC{ATOM}:next_neighbour_handedness_of ... there are no neighbours")
      n = size(neighbour)
      ENSURE(size(neighbour)==4,"VEC{ATOM}:next_neighbour_handedness_of ... must have four neighbours, n = "//trim(to_str_(n)))
      Z = self(neighbour)%atomic_number
      call quick_sort_(Z,order)
      neighbour = neighbour(order)
      p2 = self(neighbour(2))%position-self(a)%position
      p3 = self(neighbour(3))%position-self(a)%position
      p4 = self(neighbour(4))%position-self(a)%position
      pc = cross_(p4,p3)
      if (dot_(p2,pc)>ZERO) then; res = "R"
      else; res = "S"
      end if
   end function

   PURE function invariom_labels(self) result (res)
   ! Return the invariom label for the atom "a"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: a
      call create_(res,size(self))
      do a = 1,size(self)
         res(a) = invariom_label_of_(self,a)
      end do
   end function

   PURE function invariom_label_of(self,a) result (res)
   ! Return the invariom label for the atom "a"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      STR(len=STR_SIZE) :: res
      INT :: i,j,n,m,f2,l2,p
      VEC(INT,:), allocatable :: Z,bond,order,neighbour,Z2,bond2,order2,neighbour2
      VEC(STR(len=STR_SIZE),:), allocatable :: bonds,bonds2,mesomer,ZZ
      BIN :: special,do_2nd_neighbour
      VEC(INT,3) :: special_Z
      STR(len=STR_SIZE) :: symbol
      special_Z = [1,15,16]

      ! Start with the R or S symbol, if the atom is chiral
      if (is_next_neighbour_chiral_(self,a)) then; res = trim(next_neighbour_handedness_of_(self,a))//"-"
      else; res = " "
      end if

      ! Start with "a" atoms chemical symbol
      res = trim(res)//chemical_symbol_(self(a))

      ! Get indices of the nearest neighbours; return if no neighbours
      neighbour = neighbours_of_(self,a)
      if (NOT allocated(neighbour)) return
      call prune_(neighbour,a)
      if (size(neighbour)==0) return

      ! Create arrays to store the bond order (times 10) and string
      ! form of bond order
      n = size(neighbour)
      call create_(bond,n)
      call create_(bonds,n)

      ! Get the atomic numbers Z of neighbors and order them in
      ! decreasing bond-order/atomic number
      call create_(Z,n)
      call create_(order,n)
      Z = self(neighbour)%atomic_number
      call allred_rochow_bond_orders_(self,a,neighbour,bond,bonds)
      Z = 1000*bond+Z
      call quick_sort_(Z,order,decreasing_order=TRUE)
      neighbour = neighbour(order)
      bond = bond(order)
      bonds = bonds(order)
      call destroy_(order)
      call destroy_(Z)
      f2 = 0
      special = FALSE
      if (any(special_Z==self(a)%atomic_number)) then

         ! If special Z atoms switch on next nearest neighbour
         f2 = 1
         l2 = size(bond)
         special = TRUE
      else

         ! Find out how many mesomeric bonds there are
         f2 = index_of_value_(bond,15)
         l2 = 0
         if (f2>0) then
            if (f2==size(bond)) then; l2 = f2
            else if (bond(size(bond))==15) then; l2 = size(bond)
            else; l2 = index_of_value_ne_to_(bond(f2+1:),15)
               l2 = f2+l2-1
            end if
         end if
      end if
      do_2nd_neighbour = f2>0 AND l2>=f2

      ! If we should do second nearest neighbours ...
      if (do_2nd_neighbour) then
         call create_(mesomer,f2,l2); mesomer = " "
         call create_(ZZ,f2,l2); ZZ = "0"

         ! Loop over second nearest neighbour atoms or mesomeric atoms
         do i = f2,l2

            ! Get indices of the 2nd nearest neighbours; return if no neighbours
            neighbour2 = neighbours_of_(self,neighbour(i))
            ENSURE(allocated(neighbour2),"VEC{ATOM}:invariom_label_of ... mesomeric atom has no neighbours")

            ! We don't want the original first neighbour
            call prune_(neighbour2,a)
            if (size(neighbour2)==0) then
               call destroy_(neighbour2)
               cycle
            end if

            ! Order the 2nd nearest neighbours in decreasing bond-order/atomic number
            m = size(neighbour2)
            call create_(bond2,m)
            call create_(bonds2,m)
            call create_(Z2,m)
            call create_(order2,m)
            Z2 = self(neighbour2)%atomic_number
            call allred_rochow_bond_orders_(self,neighbour(i),neighbour2,bond2,bonds2)
            Z2 = 1000*bond2+Z2
            call quick_sort_(Z2,order2,decreasing_order=TRUE)
            neighbour2 = neighbour2(order2)
            Z2 = Z2(order2)
            bond2 = bond2(order2)
            bonds2 = bonds2(order2)

            ! Make and store the unsorted mesomer 2nd nearest neighbour invariom names
            mesomer(i) = achar(91)
            do j = 1,m
               symbol = chemical_symbol_(self(neighbour2(j)))
               mesomer(i) = trim(mesomer(i))//trim(bonds2(j))//trim(lower_cased_(symbol))
            end do
            mesomer(i) = trim(mesomer(i))//achar(93)

            ! Make the 2nd nearest neighbour ordering string ZZ
            p = 1
            do j = 1,m
               ZZ(i)(p:p+3) = trim(to_str_(Z2(j)))
               p = p+4
            end do
            call destroy_(order2)
            call destroy_(Z2)
            call destroy_(bonds2)
            call destroy_(bond2)
            call destroy_(neighbour2)
         end do

         ! Now reorder the mesomer strings according to ZZ
         if (l2>f2) then
            call create_(order,f2,l2)
            call quick_sort_(ZZ,order,decreasing_order=TRUE)
            order = order+f2-1
            neighbour(f2:l2) = neighbour(order)
            mesomer(f2:l2) = mesomer(order)
            call destroy_(order)
         end if
         call destroy_(ZZ)
      end if

      ! Now everything is sorted; just make the invariom label
      res = trim_blanks_from_start_(res)
      do i = 1,n
         symbol = chemical_symbol_(self(neighbour(i)))
         res = trim(res)//trim(bonds(i))//trim(lower_cased_(symbol))
         if (bond(i)==15 OR special) then
            res = trim(res)//trim(mesomer(i))
         end if
      end do
   end function

   subroutine put_invariom_labels(self)
   ! Print out the invariom labels
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: a
      do a = 1,size(self)
         call show_(stdout,"label for atom ="//trim(to_str_(a))//" = ",invariom_label_of_(self,a))
      end do
   end subroutine

   PURE subroutine allred_rochow_bond_orders(self,a,neighbours,bond,bonds)
   ! Given at atom "a" and its "neighbours" return the allred-rochow
   ! bond distinguishing parameters "bond" (times 10) and the
   ! cooresponding string form.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      VEC(INT,:), IN :: neighbours
      VEC(INT,:), OUT :: bond
      VEC(STR(len=*),:), OUT :: bonds
      INT :: i,b
      REAL :: br
   ENSURE(0<a AND a<=size(self),"VEC{ATOM}:allred_rochow_bond_orders ... index a out of range")
   ENSURE(size(bond)==size(neighbours),"VEC{ATOM}:allred_rochow_bond_orders ... bond array, wrong size")
   ENSURE(size(bonds)==size(neighbours),"VEC{ATOM}:allred_rochow_bond_orders ... bond array, wrong size")
      do i = 1,size(neighbours)
         b = bond_distinguisher_(self,a,neighbours(i))
         bond(i) = b
         if (b==15) then
            br = b/TEN
            bonds(i) = to_str_(br,"f3.1")
         else
            b = b/10
            bonds(i) = to_str_(b)
         end if
      end do
   end subroutine

   PURE function bond_distinguisher(self,a,b) result (res)
   ! Return true if "self" is bonded to "b".  If present, "range_factor" is used
   ! to determine the distance range in which the atoms are regarded as bonded.
   ! This uses the CCDC method, as documented on their web page.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      INT :: res
      REAL :: d,chi,cra,crb,ena,enb
      d = bond_distance_(self,a,b)*ANGSTROM_PER_BOHR
      cra = invariom_covalent_radius_(self(a))
      crb = invariom_covalent_radius_(self(b))
      ena = allred_rochow_en_(self(a))
      enb = allred_rochow_en_(self(b))
      chi = cra+crb-0.08*abs(ena-enb)-d
      res = 0
      if (chi<=0.0847) then; res = 10
      else if (chi<=0.184) then; res = 15
      else if (chi<=0.27) then; res = 20
      else if (chi>0.27) then; res = 30
      end if
   end function

   PURE function smiles_invariom_labels(self) result (res)
   ! Return the invariom label for the atom "a"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: res
      INT :: a
      call create_(res,size(self))
      do a = 1,size(self)
         res(a) = smiles_invariom_label_of_(self,a)
      end do
   end function

   PURE function smiles_invariom_label_of(self,a) result (res)
   ! Return the invariom label for the atom "a"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      STR(len=STR_SIZE) :: res
      INT :: i,j,n,m,f2,l2,p
      VEC(INT,:), allocatable :: Z,bond,order,neighbour,Z2,bond2,order2,neighbour2
      VEC(STR(len=STR_SIZE),:), allocatable :: bonds,bonds2,mesomer,ZZ
      BIN :: special,do_2nd_neighbour
      VEC(INT,3) :: special_Z
      STR(len=STR_SIZE) :: symbol
      special_Z = [1,15,16]

      ! Start with the R or S symbol, if the atom is chiral
      if (is_next_neighbour_chiral_(self,a)) then; res = trim(next_neighbour_handedness_of_(self,a))//"-"
      else; res = " "
      end if

      ! Start with "a" atoms chemical symbol
      res = trim(res)//chemical_symbol_(self(a))

      ! Get indices of the nearest neighbours; return if no neighbours
      neighbour = neighbours_of_(self,a)
      if (NOT allocated(neighbour)) return
      call prune_(neighbour,a)
      if (size(neighbour)==0) return

      ! Create arrays to store the bond order (times 10) and string
      ! form of bond order
      n = size(neighbour)
      call create_(bond,n)
      call create_(bonds,n)

      ! Get the atomic numbers Z of neighbors and order them in
      ! decreasing bond-order/atomic number
      call create_(Z,n)
      call create_(order,n)
      Z = self(neighbour)%atomic_number
      call smiles_bond_orders_(self,a,neighbour,bond,bonds)
      Z = 1000*bond+Z
      call quick_sort_(Z,order,decreasing_order=TRUE)
      neighbour = neighbour(order)
      bond = bond(order)
      bonds = bonds(order)
      call destroy_(order); call destroy_(Z)
      f2 = 0
      special = FALSE
      if (any(special_Z==self(a)%atomic_number)) then

         ! If special Z atoms switch on next nearest neighbour
         f2 = 1
         l2 = size(bond)
         special = TRUE
      else

         ! Find out how many mesomeric bonds there are
         f2 = index_of_value_(bond,15)
         l2 = 0
         if (f2>0) then
            if (f2==size(bond)) then; l2 = f2
            else if (bond(size(bond))==15) then; l2 = size(bond)
            else; l2 = index_of_value_ne_to_(bond(f2+1:),15)
               l2 = f2+l2-1
            end if
         end if
      end if
      do_2nd_neighbour = f2>0 AND l2>=f2

      ! If we should do second nearest neighbours ...
      if (do_2nd_neighbour) then
         call create_(mesomer,f2,l2); mesomer = " "
         call create_(ZZ,f2,l2); ZZ = "0"

         ! Loop over second nearest neighbour atoms or mesomeric atoms
         do i = f2,l2

            ! Get indices of the 2nd nearest neighbours; return if no neighbours
            neighbour2 = neighbours_of_(self,neighbour(i))
            ENSURE(allocated(neighbour2),"VEC{ATOM}:smiles_invariom_label_of ... mesomeric atom has no neighbours")

            ! We don't want the original first neighbour
            call prune_(neighbour2,a)
            if (size(neighbour2)==0) then
               call destroy_(neighbour2)
               cycle
            end if

            ! Order the 2nd nearest neighbours in decreasing bond-order/atomic number
            m = size(neighbour2)
            call create_(bond2,m)
            call create_(bonds2,m)
            call create_(Z2,m)
            call create_(order2,m)
            Z2 = self(neighbour2)%atomic_number
            call smiles_bond_orders_(self,neighbour(i),neighbour2,bond2,bonds2)
            Z2 = 1000*bond2+Z2
            call quick_sort_(Z2,order2,decreasing_order=TRUE)
            neighbour2 = neighbour2(order2)
            Z2 = Z2(order2)
            bond2 = bond2(order2)
            bonds2 = bonds2(order2)

            ! Make and store the unsorted mesomer 2nd nearest neighbour invariom names
            mesomer(i) = "("
            do j = 1,m
               symbol = chemical_symbol_(self(neighbour2(j)))
               mesomer(i) = trim(mesomer(i))//trim(bonds2(j))//trim(lower_cased_(symbol))
            end do
            mesomer(i) = trim(mesomer(i))//")"

            ! Make the 2nd nearest neighbour ordering string ZZ
            p = 1
            do j = 1,m
               ZZ(i)(p:p+3) = trim(to_str_(Z2(j)))
               p = p+4
            end do
            call destroy_(order2)
            call destroy_(Z2)
            call destroy_(bonds2)
            call destroy_(bond2)
            call destroy_(neighbour2)
         end do

         ! Now reorder the mesomer strings according to ZZ
         if (l2>f2) then
            call create_(order,f2,l2)
            call quick_sort_(ZZ,order,decreasing_order=TRUE)
            order = order+f2-1
            neighbour(f2:l2) = neighbour(order)
            mesomer(f2:l2) = mesomer(order)
            call destroy_(order)
         end if
         call destroy_(ZZ)
      end if

      ! Now everything is sorted; just make the invariom label
      res = trim_blanks_from_start_(res)
      res = trim(res)//"("
      do i = 1,n
         symbol = chemical_symbol_(self(neighbour(i)))
         res = trim(res)//trim(bonds(i))//trim(lower_cased_(symbol))
         if (bond(i)==15 OR special) then
            res = trim(res)//trim(mesomer(i))
         end if
      end do
      res = trim(res)//")"
   end function

   subroutine put_SMILES_invariom_labels(self)
   ! Print out the invariom labels
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: a
      do a = 1,size(self)
         call show_(stdout,"label for atom ="//trim(to_str_(a))//" = ",smiles_invariom_label_of_(self,a))
      end do
   end subroutine

   PURE subroutine smiles_bond_orders(self,a,neighbours,bond,bonds)
   ! Given at atom "a" and its "neighbours" return the allred-rochow
   ! bond distinguishing parameters "bond" (times 10) and the
   ! cooresponding string form.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      VEC(INT,:), IN :: neighbours
      VEC(INT,:), OUT :: bond
      VEC(STR(len=*),:), OUT :: bonds
      INT :: i,b
   ENSURE(0<a AND a<=size(self),"VEC{ATOM}:smiles_bond_orders ... index a out of range")
   ENSURE(size(bond)==size(neighbours),"VEC{ATOM}:smiles_bond_orders ... bond array, wrong size")
   ENSURE(size(bonds)==size(neighbours),"VEC{ATOM}:smiles_bond_orders ... bond array, wrong size")
      do i = 1,size(neighbours)
         b = bond_distinguisher_(self,a,neighbours(i))
         bond(i) = b
         select case (b)
         case (10); bonds(i) = "-"
         case (15); bonds(i) = ":"
         case (20); bonds(i) = "="
         case (25); bonds(i) = "%"
         case (30); bonds(i) = "#"
         case default; bonds(i) = "?"
         end select
      end do
   end subroutine


!  =================
!  Protein detection
!  =================

   subroutine find_and_put_N_terminal_atoms_0(self)
   ! Print the indices of the N terminal atoms.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable :: list
      call make_connection_table_(self,skip_hydrogen=TRUE)
      call find_and_put_N_terminal_atoms_(self,list)
      call text_(stdout,"List of N terminal atoms:")
      call put_(stdout,list)
      call destroy_(list)
      call destroy_(connections_for)
   end subroutine

   subroutine find_and_put_N_terminal_atoms_1(self,list)
   ! Print the indices of the protein backbone atoms.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable, OUT :: list
      INT :: N,Cp0,Ca,Cb,Cp,N1,Ot,Ct
      BIN :: N_terminal
   ENSURE(allocated(connections_for),"VEC{ATOM}:find_and_put_N_terminal_atoms_1 ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:find_and_put_N_terminal_atoms_1 ... wrong size connection table")
      call create_(list,0)
      do N = 1,size(self)
         if (self(N)%atomic_number/=7) cycle
         Cp0 = 0
         if (NOT is_protein_backbone_N_(self,N,Cp0,Ca,Cb,Cp,N1,Ot,Ct,N_terminal,try=TRUE)) cycle
         if (NOT N_terminal) cycle
         call append_(list,N)
      end do
   end subroutine

   subroutine find_and_put_protein_backbone_0(self)
   ! Return the of indices of the N terminal atoms.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: backbone
      call make_connection_table_(self,skip_hydrogen=TRUE)
      call find_and_put_protein_backbone_(self,backbone)
      call destroy_(backbone)
      call destroy_(connections_for)
   end subroutine

   subroutine find_and_put_protein_backbone_1(self,backbone)
   ! Return the "backbone" of atom indices in each protein backbone.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: backbone
      VEC(INT,:), allocatable :: terminal
      BIN :: N_terminal
      INT :: i,N,Cp0,Ca,Cb,Cp,N1,Ot,Ct
   ENSURE(allocated(connections_for),"VEC{ATOM}:find_and_put_protein_backbone_1 ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:find_and_put_protein_backbone_1 ... wrong size connection table")
      call find_and_put_N_terminal_atoms_(self,terminal)
      call create_(backbone,size(terminal),1)
      do i = 1,size(terminal)
         N = terminal(i)
         backbone(i)%element(1) = N
         Cp0 = 0
         do
            if (NOT is_protein_backbone_N_(self,N,Cp0,Ca,Cb,Cp,N1,Ot,Ct,N_terminal,try=TRUE)) exit
            if (Ot/=0) then
               call append_(backbone(i)%element,[Ca,Cp,Ot])
               exit
            else if (Ct/=0) then
               call append_(backbone(i)%element,[Ca,Cp,Ct])
               exit
            else
               call append_(backbone(i)%element,[Ca,Cp,N1])
               N = N1
               Cp0 = Cp
            end if
         end do
         call show_(stdout,"Backbone for protein strand =",i)
         call put_(stdout,backbone(i)%element)
      end do
      call destroy_(terminal)
   end subroutine


!   find_protein_backbone_from_N(N,C,found,Ca,Cc,N1,Ot)
!   ! Given the index "N" of a nitrogen atom and possibly the index "C"
!   ! of a connected carbon atom, return "found" as TRUE if that
!   ! nitrogen atom is part of a protein backbone. Also return the
!   ! index "Ca" of the C-alpha carbon, the index "Cc" of the C-carboxy
!   ! carbon, and either "N1" the next nitrogen in the backbone, or
!   ! "Ot" the terminal oxygen in the backbone. This assumes a
!   ! connection table has been made.
!     N,C :: INT, IN
!     found :: BIN, OUT
!     Ca,Cc,N1,Ot :: INT, OUT
!
!   ENSURE(connections_for.allocated,"no connection table")
!   ENSURE(connections_for.dim==.dim,"wrong size connection table")
!   ENSURE(N>0 AND N<=.dim,"N index out of range")
!   ENSURE(self(N).atomic_number==7,"N is not the index of a nitrogen atom")
!   ENSURE(C>=0 AND C<=.dim,"C index out of range")
!
!     nn,na,ia,nc,ic,O2,X :: INT
!
!     ! Set defaults
!     found = FALSE
!     Ca = 0; Cc = 0; N1 = 0; Ot = 0
!
!     ! Check if C is bonded to N
!     if (C>0) then
!        DIE_IF(NOT self(N).is_bonded_to(self(C)),"N is not bonded to C")
!     end
!
!     ! # of connections for N
!     nn = connections_for(N).element.dim
!
!     ! A backbone N must have 1 connection and can't have
!     ! more than 2 connections
!     if (nn<=0) return
!     if (nn >2) return
!
!     ! Get the index of C-alpha atom
!     Ca = connections_for(N)[1]
!     if (Ca==C) then
!        DIE_IF(nn<2,"not enough connections to N = "//trim(N.to_str))
!        Ca = connections_for(N)[2]
!     end
!
!     ! C-alpha must be a carbon
!     if (self(Ca).atomic_number/=6) return
!
!     ! # of  connections for C-alpha
!     na = connections_for(Ca).element.dim
!
!     ! C-alpha must have 2 connections (GLY) or 3 connections
!     if (na/=3 AND na/=2) return
!
!     ! Now find C-carboxy carbon
!     do ia = 1,na
!        Cc = connections_for(Ca)[ia]          ! Index of C-carboxy
!        if (Cc==N) cycle                      ! Cc can't be the first N
!        if (self(Cc).atomic_number/=6) cycle  ! Cc must be a carbon
!        nc = connections_for(Cc).element.dim
!        if (nc/=3) cycle                      ! C-carboxy must have 3 connections
!        O2 = 0                                ! It must have a =O
!        N1 = 0                                ! It must have a -N, or
!        Ot = 0                                ! It must have a terminal -OH
!        do ic = 1,nc
!           X = connections_for(Cc)[ic]
!           if (X==Ca) cycle
!           if (self(X).atomic_number==7) N1 = X
!           if (self(X).atomic_number==8) then
!              if (Ot==0) then; O2 = X
!              else;            Ot = X
!              end
!           end
!        end
!        found = O2/=0 AND (N1/=0 OR Ot/=0)
!        if (found) exit
!     end
!   end

   function is_protein_backbone_N(self,N,Cp0,Ca,Cb,Cp,N1,Ot,Ct,N_terminal,try) result (res)
   ! Return TRUE if "N" is the index of a nitrogen atom in the
   ! backbone of a protein, Cp0-N-Ca-Cp-(N1,Ot,Ct), given also
   ! (possibly) the index "Cp0" of a connected C prime carbon atom.
   ! Return the index "Ca" of the C-alpha carbon, the index of the
   ! beta carbon "Cb" (if it exists), the index "Cp" of the next C
   ! prime carboxy carbon, and either "N1" the next nitrogen in the
   ! backbone, or "Ot" the terminal oxygen in the backbone. If "N" is
   ! an N-terminal atom, return "N_terminal" as TRUE. This assumes a
   ! connection table has been made.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: N,Cp0
      INT, OUT :: Ca,Cb,Cp,N1,Ot,Ct
      BIN, OUT :: N_terminal
      BIN, optional, IN :: try
      BIN :: res
      INT :: C1,C2,C3,O1,O11,Ca1,N11
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_protein_backbone_N ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_protein_backbone_N ... wrong size connection table")
   ENSURE(N>0 AND N<=size(self),"VEC{ATOM}:is_protein_backbone_N ... N index out of range")
   ENSURE(self(N)%atomic_number==7,"VEC{ATOM}:is_protein_backbone_N ... N is not the index of a nitrogen atom")
   ENSURE(Cp0>=0 AND Cp0<=size(self),"VEC{ATOM}:is_protein_backbone_N ... Cp0 index out of range")
      res = FALSE
      N_terminal = FALSE
      if (has_1_connection_(self,N,6,Ca)) then

         ! One connection to N: Terminal N-Ca connection
         res = is_protein_C_alpha_(self,Ca,N,Cb,Cp,O1,N1,Ot,Ct)
         N_terminal = TRUE
      else if (has_2_connections_(self,N,6,6,C1,C2)) then

         ! Two connections to N: Backbone or terminal PRO C1-N-C2 connection
         Ca = 0
         if (C1==Cp0) then; Ca = C2
         else if (C2==Cp0) then; Ca = C1
         end if

         ! We found Cp0, so we know Ca
         if (Ca/=0) then
            res = is_protein_C_alpha_(self,Ca,N,Cb,Cp,O1,N1,Ot,Ct)

         ! Find which of C1 and C2 is Ca. If requested, determine which
         ! of C1 and C2 is NOT a Cp, which implies N_terminal. Also
         ! check for a terminal proline.
         else
            if (is_protein_C_alpha_(self,C1,N,Cb,Cp,O1,N1,Ot,Ct)) then
               res = TRUE
               Ca = C1
               if (has_ring_(self,N,C2,[6,6,6])) N_terminal = TRUE
               if (NOT N_terminal AND present(try)) N_terminal = NOT is_protein_C_prime_carbon_(self,C2,N,O11,Ca1,N11)
            else if (is_protein_C_alpha_(self,C2,N,Cb,Cp,O1,N1,Ot,Ct)) then
               res = TRUE
               Ca = C2
               if (has_ring_(self,N,C1,[6,6,6])) N_terminal = TRUE
               if (NOT N_terminal AND present(try)) N_terminal = NOT is_protein_C_prime_carbon_(self,C1,N,O11,Ca1,N11)
            end if
         end if
      else if (has_3_connections_(self,N,6,6,6,C1,C2,C3)) then

         ! Three connections to N: Backbone PRO N-C1C2C3 connection
         ! NOTE: this only works if Cp0 is given
         if (C1==Cp0) then; C1 = C2; C2 = C3; C3 = 0
         else if (C2==Cp0) then; C2 = C3; C3 = 0
         else if (C3==Cp0) then; C3 = 0
         end if
         if (C3==0) then
            if (is_protein_C_alpha_(self,C1,N,Cb,Cp,O1,N1,Ot,Ct)) then
               if (has_ring_(self,N,C2,[6,6,6])) then
                  Ca = C1
                  res = TRUE
               end if
            else if (is_protein_C_alpha_(self,C2,N,Cb,Cp,O1,N1,Ot,Ct)) then
               if (has_ring_(self,N,C1,[6,6,6])) then
                  Ca = C2
                  res = TRUE
               end if
            end if
         end if
      end if
   end function

   function is_protein_C_alpha(self,Ca,N,Cb,Cp,O1,N1,Ot,Ct) result (res)
   ! Return TRUE if "Ca" is the index of a C-alpha atom in the
   ! backbone of a protein. You must supply the connected nitrogen
   ! atom "N". Returned are the index of the beta carbon "Cb", the
   ! index of the next C prime carboxy carbon "Cp", the connected
   ! oxygen atom "O1", and either the next nitrogen in the
   ! backbone "N1", or "Ot" the terminal oxygen in the backbone.
   ! NOTE: this is useful for travelling from N terminal to C terminal
   ! Diagram: N-CaCb-Cp=O1-(N1,Ot,Ct)
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,N
      INT, OUT :: Cb,Cp,O1,N1,Ot,Ct
      BIN :: res
      INT :: C1,C2,C3
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_protein_C_alpha ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_protein_C_alpha ... wrong size connection table")
   ENSURE(N>0 AND N<=size(self),"VEC{ATOM}:is_protein_C_alpha ... N index out of range")
   ENSURE(Ca>=0 AND Ca<=size(self),"VEC{ATOM}:is_protein_C_alpha ... Ca index out of range")
      res = FALSE

      ! GLY N-Ca-Cp connection
      if (has_2_connections_(self,Ca,N,6,Cp)) then
         if (is_CCOO_carbon_(self,Cp,Ca,Ot,O1)) then  ! two terminal oxygens
            N1 = 0
            Ct = 0
            Cb = 0
            res = TRUE
         else if (is_CCON_carbon_(self,Cp,Ca,O1,N1)) then  ! Cp has =O and next nitrogen
            Ot = 0
            Ct = 0
            Cb = 0
            res = TRUE
         else if (is_CCOC_carbon_(self,Cp,Ca,O1,Ct)) then  ! terminal methyl
            N1 = 0
            Ot = 0
            Cb = 0
            res = TRUE
         else if (has_2_connections_(self,Cp,Ca,7,N1)) then  ! Cp has no oxygen
            Ot = 0
            Ct = 0
            Cb = 0
            res = TRUE
         end if

      ! Any other residue, which of C1 and C2 is Cp?
      else if (has_3_connections_(self,Ca,N,6,6,C1,C2)) then
         if (is_CCOO_carbon_(self,C1,Ca,Ot,O1)) then  ! two terminal oxygens
            Cp = C1
            Cb = C2
            N1 = 0
            Ct = 0
            res = TRUE
         else if (is_CCON_carbon_(self,C1,Ca,O1,N1)) then  ! Cp=C1 has =O and next nitrogen
            Cp = C1
            Cb = C2
            Ot = 0
            Ct = 0
            res = TRUE
         else if (is_CCOC_carbon_(self,C1,Ca,O1,Ct)) then  ! terminal methyl
            Cp = C1
            Cb = C2
            N1 = 0
            Ot = 0
            res = TRUE
         else if (is_CCOO_carbon_(self,C2,Ca,Ot,O1)) then
            Cp = C2
            Cb = C1
            N1 = 0
            Ct = 0
            res = TRUE
         else if (is_CCON_carbon_(self,C2,Ca,O1,N1)) then
            Cp = C2
            Cb = C1
            Ot = 0
            Ct = 0
            res = TRUE
         else if (is_CCOC_carbon_(self,C2,Ca,O1,Ct)) then  ! terminal methyl
            Cp = C2
            Cb = C1
            N1 = 0
            Ot = 0
            res = TRUE
         else if (has_2_connections_(self,C1,Ca,7,N1)) then  ! Cp has no oxygen
            Cp = C1
            Cb = C2
            Ot = 0
            Ct = 0
            res = TRUE
         else if (has_2_connections_(self,C2,Ca,7,N1)) then  ! Cp has no oxygen
            Cp = C2
            Cb = C1
            Ot = 0
            Ct = 0
            res = TRUE
         end if

      ! For non-standard branched residues, which of C1, C2, C3 is Cp?
      else if (has_4_connections_(self,Ca,N,6,C1,C2,C3)) then
         if (is_CCOO_carbon_(self,C1,Ca,Ot,O1)) then
            Cp = C1
            Cb = 0
            N1 = 0
            Ct = 0
            res = TRUE
         else if (is_CCON_carbon_(self,C1,Ca,O1,N1)) then
            Cp = C1
            Cb = 0
            Ot = 0
            Ct = 0
            res = TRUE
         else if (is_CCOC_carbon_(self,C1,Ca,O1,Ct)) then  ! terminal methyl
            Cp = C1
            Cb = 0
            N1 = 0
            Ot = 0
            res = TRUE
         else if (is_CCOO_carbon_(self,C2,Ca,Ot,O1)) then
            Cp = C2
            Cb = 0
            N1 = 0
            Ct = 0
            res = TRUE
         else if (is_CCON_carbon_(self,C2,Ca,O1,N1)) then
            Cp = C2
            Cb = 0
            Ot = 0
            Ct = 0
            res = TRUE
         else if (is_CCOC_carbon_(self,C2,Ca,O1,Ct)) then  ! terminal methyl
            Cp = C2
            Cb = 0
            N1 = 0
            Ot = 0
            res = TRUE
         else if (is_CCOO_carbon_(self,C3,Ca,Ot,O1)) then
            Cp = C3
            Cb = 0
            N1 = 0
            Ct = 0
            res = TRUE
         else if (is_CCON_carbon_(self,C3,Ca,O1,N1)) then
            Cp = C3
            Cb = 0
            Ot = 0
            Ct = 0
            res = TRUE
         else if (is_CCOC_carbon_(self,C3,Ca,O1,Ct)) then  ! terminal methyl
            Cp = C3
            Cb = 0
            N1 = 0
            Ot = 0
            res = TRUE
         else if (has_2_connections_(self,C1,Ca,7,N1)) then  ! Cp has no oxygen
            Cp = C1
            Cb = 0
            Ot = 0
            Ct = 0
            res = TRUE
         else if (has_2_connections_(self,C2,Ca,7,N1)) then  ! Cp has no oxygen
            Cp = C2
            Cb = 0
            Ot = 0
            Ct = 0
            res = TRUE
         else if (has_2_connections_(self,C3,Ca,7,N1)) then  ! Cp has no oxygen
            Cp = C3
            Cb = 0
            Ot = 0
            Ct = 0
            res = TRUE
         end if
      end if
   end function

   function is_protein_C_prime_carbon(self,Cp,N,O,Ca,N1) result (res)
   ! Return TRUE if "Cp" is the index of a N-Cp-Ca-N1 carbon, where
   ! "N" is the index of a nitrogen bonded to "Cp". If TRUE, "O",
   ! "Ca" and "N1" are returned as the index of the bonded oxygen,
   ! alpha Carbon and previous nitrogen respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Cp,N
      INT, OUT :: O,Ca,N1
      BIN :: res
      INT :: Cb,Cb1,s,x,i
      VEC(INT,:), allocatable :: list
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_protein_C_prime_carbon ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_protein_C_prime_carbon ... wrong size connection table")
   ENSURE(Cp>=0 AND Cp<=size(self),"VEC{ATOM}:is_protein_C_prime_carbon ... Cp index out of range")
   ENSURE(N>=0 AND N<=size(self),"VEC{ATOM}:is_protein_C_prime_carbon ...  N index out of range")
      x = 6
      s = 7
      O = 0
      Ca = 0
      N1 = 0
      res = FALSE
      if (has_3_connections_(self,Cp,N,8,6,O,Ca)) then  ! Cp=ON connection
         res = has_1_connection_(O,Cp) AND (has_2_connections_(self,Ca,Cp,7,N1) OR has_3_connections_(self,Ca,Cp,7,6,N1,Cb) OR has_4_connections_(self,Ca,Cp,s,x,x,N1,Cb,Cb1))
      else if (has_a_connection(Cp,N)) then  ! Cp has no O connection
         if (has_Z_connections_(self,Cp,6,list)) then
            do i = 1,size(list)
               Ca = list(i)
               res = has_2_connections_(self,Ca,Cp,7,N1) OR has_3_connections_(self,Ca,Cp,7,6,N1,Cb) OR has_4_connections_(self,Ca,Cp,s,x,x,N1,Cb,Cb1)
               if (res) exit
            end do
            call destroy_(list)
         end if
      end if
   end function

   function is_NCOC_carbon(self,Cp,N,O,Ca,N1) result (res)
   ! Return TRUE if "Cp" is the index of a N-Cp=O-Ca-N1 carbon, where
   ! "N" is the index of a nitrogen bonded to "Cp". If TRUE, "O",
   ! "Ca" and "N1" are returned as the index of the bonded oxygen,
   ! alpha Carbon and next nitrogen respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Cp,N
      INT, OUT :: O,Ca,N1
      BIN :: res
      INT :: Cb,Cb1,s,x
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_NCOC_carbon ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_NCOC_carbon ... wrong size connection table")
   ENSURE(Cp>=0 AND Cp<=size(self),"VEC{ATOM}:is_NCOC_carbon ... Cp index out of range")
   ENSURE(N>=0 AND N<=size(self),"VEC{ATOM}:is_NCOC_carbon ...  N index out of range")
      x = 6
      s = 7
      N1 = 0
      if (has_3_connections_(self,Cp,N,8,6,O,Ca)) then  ! Cp=ON connection
         res = has_1_connection_(O,Cp) AND (has_2_connections_(self,Ca,Cp,7,N1) OR has_3_connections_(self,Ca,Cp,7,6,N1,Cb) OR has_4_connections_(self,Ca,Cp,s,x,x,N1,Cb,Cb1))
      else
         res = FALSE
      end if
   end function

   function is_CCOO_carbon(self,Cp,Ca,O1,O2) result (res)
   ! Return TRUE if "Cp" is the index of a Ca-Cp=OO carbon, where "Ca"
   ! is the index of an (alpha) carbon bonded to "Cp". If TRUE, "O1" and
   ! "O2" are returned as the indices of the bonded oxygens.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Cp,Ca
      INT, OUT :: O1,O2
      BIN :: res
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_CCOO_carbon ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_CCOO_carbon ... wrong size connection table")
   ENSURE(Cp>=0 AND Cp<=size(self),"VEC{ATOM}:is_CCOO_carbon ... Cp index out of range")
   ENSURE(Ca>=0 AND Ca<=size(self),"VEC{ATOM}:is_CCOO_carbon ... Ca index out of range")
      if (has_3_connections_(self,Cp,Ca,8,8,O1,O2)) then  ! Cp=O1O2 connection
         res = has_1_connection_(O1,Cp)
         res = has_1_connection_(O2,Cp) AND res
      else
         res = FALSE
      end if
   end function

   function is_CCOC_carbon(self,Cp,Ca,O1,C) result (res)
   ! Return TRUE if "Cp" is the index of a Ca-Cp=OC carbon, where "Ca"
   ! is the index of an (alpha) carbon bonded to "Cp". If TRUE, "O1" and
   ! "C" are returned as the indices of the bonded oxygen and terminal
   ! methyl.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Cp,Ca
      INT, OUT :: O1,C
      BIN :: res
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_CCOC_carbon ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_CCOC_carbon ... wrong size connection table")
   ENSURE(Cp>=0 AND Cp<=size(self),"VEC{ATOM}:is_CCOC_carbon ... Cp index out of range")
   ENSURE(Ca>=0 AND Ca<=size(self),"VEC{ATOM}:is_CCOC_carbon ... Ca index out of range")
      if (has_3_connections_(self,Cp,Ca,8,6,O1,C)) then  ! Cp=O1C connection
         res = has_1_connection_(O1,Cp)
         res = has_1_connection_(C,Cp) AND res
      else
         res = FALSE
      end if
   end function

   function is_CCON_carbon(self,Cp,Ca,O1,N1) result (res)
   ! Return TRUE if "Cp" is the index of a Ca-Cp=ON carbon, where "Ca"
   ! is the index of an (alpha) carbon bonded to "Cp". If TRUE, "O1" and
   ! "N1" are returned as the indices of the bonded oxygen and
   ! nitrogen atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Cp,Ca
      INT, OUT :: O1,N1
      BIN :: res
   ENSURE(allocated(connections_for),"VEC{ATOM}:is_CCON_carbon ... no connection table")
   ENSURE(size(connections_for)==size(self),"VEC{ATOM}:is_CCON_carbon ... wrong size connection table")
   ENSURE(Cp>=0 AND Cp<=size(self),"VEC{ATOM}:is_CCON_carbon ... C index out of range")
      res = has_3_connections_(self,Cp,Ca,8,7,O1,N1)
   end function


!  =================
!  Connection tables
!  =================

   pure function has_connection_table() result (res)
   ! Returns TRUE if the connection table connections_for is made.
      BIN :: res
      res = allocated(connections_for)
   end function

   subroutine make_connection_table_0(self,skip_hydrogen,skip_self)
   ! Make the atom connection table "connections_for".  If
   ! "skip_hydrogen" is present and TRUE, connections are not made to
   ! any hydrogen atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: skip_hydrogen
      BIN, optional, IN :: skip_self
      REAL :: r
      if (allocated(connections_for)) return
      r = 0.5d0
      call convert_from_(r,"angstrom")
      call set_atom_bonded_range_factor_(r)
      call make_connection_table_(self,connections_for,skip_hydrogen,skip_self)  ! NOT PURE
   end subroutine

   PURE subroutine make_connection_table_1(self,connections_for,skip_hydrogen,skip_self)
   ! Make the atom connection table "connections_for".  If
   ! "skip_hydrogen" is present and TRUE, connections are not made to
   ! any hydrogen atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: connections_for
      BIN, optional, IN :: skip_hydrogen
      BIN, optional, IN :: skip_self
      INT :: i,j,k
      BIN :: skip_H,skip_s

      ! Skip H's?
      skip_H = FALSE
      if (present(skip_hydrogen)) skip_H = skip_hydrogen

      ! Skip atom itself in the connections?
      skip_s = TRUE
      if (present(skip_self)) skip_s = skip_self

      ! Create connection table
      call create_(connections_for,size(self),0)

      ! Loop all atoms "i"
      do i = 1,size(self)

         ! Skip H's?
         if (skip_H) then
            if (self(i)%atomic_number==1) cycle
         end if

         ! If skipping atom "i" look at next atom
         if (skip_s) then; k = i+1
         else; k = i
         end if

         ! Loop possible connected atoms "k"
         do j = k,size(self)

            ! Skip H's?
            if (skip_H) then
               if (self(j)%atomic_number==1) cycle
            end if

            ! Add connected atom?
            if (is_bonded_to_(self(i),self(j)) OR j==i) then
               call append_(connections_for(i)%element,j)
               call append_(connections_for(j)%element,i)
            end if
         end do
      end do
   end subroutine

   subroutine assign_connection_table(connections)
   ! Assign the atom connection connection_table to "connections".
      VEC(type(EVEC_INT_TYPE),:), allocatable, IN :: connections
   ENSURE(allocated(connections),"VEC{ATOM}:assign_connection_table ... no connection table")
      connections_for = connections  ! NOT PURE
   end subroutine

   subroutine destroy_connection_table()
   ! Clean the "connection_for" atom connection table
      call destroy_(connections_for)  ! NOT PURE
   end subroutine


!   has_group_connection_table result (res) :: selfless, pure
!   ! Returns TRUE if the connection table connections_for is made.
!      self :: IN
!      res :: BIN
!
!      res = group_connections_for.allocated
!
!   end
!
!   make_group_connection_table :: leaky
!   ! Make the atom group connection table "connections_for". 
!      self :: IN
!
!      r :: REAL
!
!      if (group_connections_for.allocated) return
!
!      r = 0.5d0
!      r.convert_from("angstrom")
!      ATOM:set_atom_bonded_range_factor(r)
!      .make_group_connection_table(group_connections_for)
!
!   end
!
!   make_group_connection_table(connections_for,skip_self) :: leaky, PURE
!   ! Make the atom group connection table "connections_for".
!      self :: IN
!      connections_for :: VEC{EVEC{INT}}@
!      skip_self     :: BIN, optional, IN
!
!      n_group, i,j,k :: INT
!      skip_s :: BIN
!
!      ! Skip atom itself in the connections?
!      skip_s = TRUE
!      if (present(skip_self)) skip_s = skip_self
!
!      ! No. of groups
!      n_group =  .no_of_groups
!
!      ! Create connection table
!      connections_for.create(n_group,0)
!
!      ! Loop all groups "i"
!      do i = 1,n_group
!
!        ! If skipping atom "i" look at next atom
!        if (skip_s) then; k = i + 1
!        else;             k = i
!        end
!
!        ! Loop possible connected atoms "k"
!        do j = k,n_group
!
!           ! Add connected atom?
!           if (.bonded_groups(i,j) OR j==i) then
!              connections_for(i).element.append(j)
!              connections_for(j).element.append(i)
!           end
!
!        end
!
!      end
!
!   end
!
!   assign_group_connection_table(connections) :: selfless
!   ! Assign the group connection table "group_connections_for" to
!   ! "connections".
!      connections :: VEC{EVEC{INT}}@
!
!   ENSURE(connections.allocated,"no connection table")
!
!      group_connections_for => connections
!
!   end
!
!   destroy_group_connection_table :: leaky, selfless
!   ! Clean the "group_connections_for" connection table
!
!   ENSURE(group_connections_for.allocated,"no connection table")
!
!      group_connections_for.destroy
!
!   end

!  =====================
!  Atom connection tests
!  =====================

! Connections to any atom?

   pure function has_a_connection(X,W) result (res)
   ! Returns TRUE if the atom with index "X" is connected to the atom
   ! with index "W".
      INT, IN :: X,W
      BIN :: res
      res = any(connections_for(X)%element(:)==W)
   end function


! Connections to an atom with atomic number "Z"

   pure function has_Z_connection(self,X,Z) result (res)
   ! Returns TRUE if the atom with index "X" is connected to an atom
   ! with atomic number "Z".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,Z
      BIN :: res
      res = any(self(connections_for(X)%element(:))%atomic_number==Z)
   end function

   function has_Z_connections(self,X,Z,list) result (res)
   ! Return "TRUE" if atom "X" has connections to atoms with atomic
   ! number "Z", and if so, return a "list" of the atom indices.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,Z
      VEC(INT,:), allocatable :: list
      BIN :: res
      INT :: nx,i,Y
      res = FALSE
      nx = size(connections_for(X)%element)
      if (nx==0) return
      call create_(list,0)
      res = FALSE
      do i = 1,nx
         Y = connections_for(X)%element(i)  ! Index of atom connected to X
         if (self(Y)%atomic_number/=Z) cycle  ! Y must have atomic number Z
         call append_(list,Y)
         res = TRUE
      end do
   end function


! Zero connections

   pure function has_0_connections(X) result (res)
   ! Returns TRUE if the atom with index "X" no connection
      INT, IN :: X
      BIN :: res
      res = size(connections_for(X)%element)==0
   end function


! One connections

   pure function has_1_connection_0(X) result (res)
   ! Returns TRUE if the atom with index "X" only has one connection
      INT, IN :: X
      BIN :: res
      res = size(connections_for(X)%element)==1
   end function

   pure function has_1_connection_1(X,Y) result (res)
   ! Returns TRUE if the atom with index "X" only has one connection
   ! to an atom with index "Y".
      INT, IN :: X,Y
      BIN :: res
      res = size(connections_for(X)%element)==1
      if (NOT res) return
      res = connections_for(X)%element(1)==Y
   end function

   function has_1_connection_2(self,X,Z,Y) result (res)
   ! Returns TRUE if the atom with index "X" only has one connection
   ! to an atom with atomic number "Z". If there is a connection,
   ! return the index of the connected atom in "Y".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,Z
      INT, OUT :: Y
      BIN :: res
      INT :: nx
      res = FALSE
      nx = size(connections_for(X)%element)
      if (nx/=1) return
      Y = connections_for(X)%element(1)
      if (self(Y)%atomic_number/=Z) then
         Y = 0
      else
         res = TRUE
      end if
   end function


! Two connections

   pure function has_2_connections_0(X) result (res)
   ! Returns TRUE if the atom with index "X" has two connections
      INT, IN :: X
      BIN :: res
      res = size(connections_for(X)%element)==2
   end function

   pure function has_2_connections_1(X,Y1,Y2) result (res)
   ! Returns TRUE if the atom with index "X" has two connections:
   ! one to the atom with index "Y1", the other to the atom with
   ! index "Y2".
      INT, IN :: X,Y1,Y2
      BIN :: res
      res = size(connections_for(X)%element)==2
      if (NOT res) return
      res = (connections_for(X)%element(1)==Y1 AND connections_for(X)%element(2)==Y2) OR (connections_for(X)%element(2)==Y1 AND connections_for(X)%element(1)==Y2)
   end function

   function has_2_connections_2(self,X,W,Z,Y) result (res)
   ! Returns TRUE if the atom with index "X" has two connections:
   ! one to the atom with index "W", the other to an atom with atomic
   ! number "Z".If there is a latter connection, return the index of
   ! atom with atomic number "Z" in "Y".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,W,Z
      INT, OUT :: Y
      BIN :: res
      INT :: nx,Y1,Y2
      Y = 0
      res = FALSE
      nx = size(connections_for(X)%element)
      if (nx/=2) return
      Y1 = connections_for(X)%element(1)
      Y2 = connections_for(X)%element(2)
      if (Y1==W AND self(Y2)%atomic_number==Z) then
         Y = Y2
         res = TRUE
      else if (Y2==W AND self(Y1)%atomic_number==Z) then
         Y = Y1
         res = TRUE
      end if
   end function

   function has_2_connections_3(self,X,Z1,Z2,Y1,Y2) result (res)
   ! Returns TRUE if the atom with index "X" has two connections: one
   ! to an atom with atomic number "Z1", the other to an atom with
   ! atomic number "Z2". If there are connectons, return the indices
   ! of these atoms as "Y1" and "Y2" respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,Z1,Z2
      INT, OUT :: Y1,Y2
      BIN :: res
      INT :: A1,A2
      Y1 = 0
      Y2 = 0
      res = FALSE
      if (size(connections_for(X)%element)/=2) return
      A1 = connections_for(X)%element(1)
      A2 = connections_for(X)%element(2)
      if (self(A1)%atomic_number==Z1 AND self(A2)%atomic_number==Z2) then
         Y1 = A1
         Y2 = A2
         res = TRUE
      else if (self(A2)%atomic_number==Z1 AND self(A1)%atomic_number==Z2) then
         Y1 = A2
         Y2 = A1
         res = TRUE
      end if
   end function


! Three connections

   pure function has_3_connections_0(X) result (res)
   ! Returns TRUE if the atom with index "X" has three connections
      INT, IN :: X
      BIN :: res
      res = size(connections_for(X)%element)==3
   end function

   function has_3_connections_1(self,X,V,W,Z,Y) result (res)
   ! Returns TRUE if the atom with index "X" has three connections:
   ! two to atoms "V" and "W", and one to an atom with atomic
   ! numbers "Z".If there is this latter connection, return the
   ! index of this atom in "Y".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,V,W,Z
      INT, OUT :: Y
      BIN :: res
      INT :: A1,A2,A3,N1,N2,N3
      Y = 0
      res = FALSE
      if (size(connections_for(X)%element)/=3) return
      A1 = connections_for(X)%element(1)
      A2 = connections_for(X)%element(2)
      A3 = connections_for(X)%element(3)
      N1 = self(A1)%atomic_number
      N2 = self(A2)%atomic_number
      N3 = self(A3)%atomic_number
      if (A1==V AND A2==W AND Z==N1) then; Y = A1; res = TRUE
      else if (A1==W AND A2==V AND Z==N1) then; Y = A1; res = TRUE
      else if (A1==V AND A2==W AND Z==N2) then; Y = A2; res = TRUE
      else if (A1==W AND A2==V AND Z==N2) then; Y = A2; res = TRUE
      else if (A1==V AND A2==W AND Z==N3) then; Y = A3; res = TRUE
      else if (A1==W AND A2==V AND Z==N3) then; Y = A3; res = TRUE
      end if
   end function

   function has_3_connections_2(self,X,W,Z1,Z2,Y1,Y2) result (res)
   ! Returns TRUE if the atom with index "X" has three connections:
   ! one to the atom with index "W", the other two to atoms with atomic
   ! numbers "Z1" and "Z2".If there are these latter connections,
   ! return the indices of atoms with atomic numbers "Z1" and "Z2"
   ! in "Y1" and "Y2".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,W,Z1,Z2
      INT, OUT :: Y1,Y2
      BIN :: res
      INT :: A1,A2,A3,N1,N2,N3
      Y1 = 0
      Y2 = 0
      res = FALSE
      if (size(connections_for(X)%element)/=3) return
      A1 = connections_for(X)%element(1)
      A2 = connections_for(X)%element(2)
      A3 = connections_for(X)%element(3)
      N1 = self(A1)%atomic_number
      N2 = self(A2)%atomic_number
      N3 = self(A3)%atomic_number
      if (A1==W AND Z1==N2 AND Z2==N3) then; Y1 = A2; Y2 = A3; res = TRUE
      else if (A1==W AND Z1==N3 AND Z2==N2) then; Y1 = A3; Y2 = A2; res = TRUE
      else if (A2==W AND Z1==N1 AND Z2==N3) then; Y1 = A1; Y2 = A3; res = TRUE
      else if (A2==W AND Z1==N3 AND Z2==N1) then; Y1 = A3; Y2 = A1; res = TRUE
      else if (A3==W AND Z1==N1 AND Z2==N2) then; Y1 = A1; Y2 = A2; res = TRUE
      else if (A3==W AND Z1==N2 AND Z2==N1) then; Y1 = A2; Y2 = A1; res = TRUE
      end if
   end function

   function has_3_connections_3(self,X,Z1,Z2,Z3,Y1,Y2,Y3) result (res)
   ! Returns TRUE if the atom with index "X" has three connections:
   ! to atoms with atomic numbers "Z1", "Z2" and "Z3" with the atom
   ! indices returned in "Y1", "Y2" and "Y3".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,Z1,Z2,Z3
      INT, OUT :: Y1,Y2,Y3
      BIN :: res
      INT :: A1,A2,A3,N1,N2,N3
      Y1 = 0
      Y2 = 0
      Y3 = 0
      res = FALSE
      if (size(connections_for(X)%element)/=3) return
      A1 = connections_for(X)%element(1)
      A2 = connections_for(X)%element(2)
      A3 = connections_for(X)%element(3)
      N1 = self(A1)%atomic_number
      N2 = self(A2)%atomic_number
      N3 = self(A3)%atomic_number
      if (Z1==N1 AND Z2==N2 AND Z3==N3) then; Y1 = A1; Y2 = A2; Y3 = A3; res = TRUE
      else if (Z1==N1 AND Z2==N3 AND Z3==N2) then; Y1 = A1; Y2 = A3; Y3 = A2; res = TRUE
      else if (Z1==N2 AND Z2==N1 AND Z3==N3) then; Y1 = A2; Y2 = A1; Y3 = A3; res = TRUE
      else if (Z1==N2 AND Z2==N3 AND Z3==N1) then; Y1 = A2; Y2 = A3; Y3 = A1; res = TRUE
      else if (Z1==N3 AND Z2==N1 AND Z3==N2) then; Y1 = A3; Y2 = A1; Y3 = A2; res = TRUE
      else if (Z1==N3 AND Z2==N2 AND Z3==N1) then; Y1 = A3; Y2 = A2; Y3 = A1; res = TRUE
      end if
   end function


! Four connections

   pure function has_4_connections_0(X) result (res)
   ! Returns TRUE if the atom with index "X" has four connections
      INT, IN :: X
      BIN :: res
      res = size(connections_for(X)%element)==4
   end function

   function has_4_connections_1(self,X,W,Z1,Z2,Z3,Y1,Y2,Y3) result (res)
   ! Returns TRUE if the atom with index "X" has four connections: one
   ! to the atom with index "W", the other three to atoms with atomic
   ! numbers "Z1", "Z2" and "Z3".If there are these latter
   ! connections, return the indices of atoms with thse atomic numbers
   ! in "Y1", "Y2" and "Y3" respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,W,Z1,Z2,Z3
      INT, OUT :: Y1,Y2,Y3
      BIN :: res
      INT :: A1,A2,A3,A4,N1,N2,N3,N4
      Y1 = 0
      Y2 = 0
      Y3 = 0
      res = FALSE
      if (size(connections_for(X)%element)/=4) return
      A1 = connections_for(X)%element(1)
      A2 = connections_for(X)%element(2)
      A3 = connections_for(X)%element(3)
      A4 = connections_for(X)%element(4)
      N1 = self(A1)%atomic_number
      N2 = self(A2)%atomic_number
      N3 = self(A3)%atomic_number
      N4 = self(A4)%atomic_number
      if (A1==W AND Z1==N2 AND Z2==N3 AND Z3==N4) then; Y1 = A2; Y2 = A3; Y3 = A4; res = TRUE
      else if (A1==W AND Z1==N2 AND Z2==N4 AND Z3==N3) then; Y1 = A2; Y2 = A4; Y3 = A3; res = TRUE
      else if (A1==W AND Z1==N3 AND Z2==N2 AND Z3==N4) then; Y1 = A3; Y2 = A2; Y3 = A4; res = TRUE
      else if (A1==W AND Z1==N3 AND Z2==N4 AND Z3==N2) then; Y1 = A3; Y2 = A4; Y3 = A2; res = TRUE
      else if (A1==W AND Z1==N4 AND Z2==N2 AND Z3==N3) then; Y1 = A4; Y2 = A2; Y3 = A3; res = TRUE
      else if (A1==W AND Z1==N4 AND Z2==N3 AND Z3==N2) then; Y1 = A4; Y2 = A3; Y3 = A2; res = TRUE
      else if (A2==W AND Z1==N1 AND Z2==N3 AND Z3==N4) then; Y1 = A1; Y2 = A3; Y3 = A4; res = TRUE
      else if (A2==W AND Z1==N1 AND Z2==N4 AND Z3==N3) then; Y1 = A1; Y2 = A4; Y3 = A3; res = TRUE
      else if (A2==W AND Z1==N3 AND Z2==N1 AND Z3==N4) then; Y1 = A3; Y2 = A1; Y3 = A4; res = TRUE
      else if (A2==W AND Z1==N3 AND Z2==N4 AND Z3==N1) then; Y1 = A3; Y2 = A4; Y3 = A1; res = TRUE
      else if (A2==W AND Z1==N4 AND Z2==N1 AND Z3==N3) then; Y1 = A4; Y2 = A1; Y3 = A3; res = TRUE
      else if (A2==W AND Z1==N4 AND Z2==N3 AND Z3==N1) then; Y1 = A4; Y2 = A3; Y3 = A1; res = TRUE
      else if (A3==W AND Z1==N2 AND Z2==N1 AND Z3==N4) then; Y1 = A2; Y2 = A1; Y3 = A4; res = TRUE
      else if (A3==W AND Z1==N2 AND Z2==N4 AND Z3==N1) then; Y1 = A2; Y2 = A4; Y3 = A1; res = TRUE
      else if (A3==W AND Z1==N1 AND Z2==N2 AND Z3==N4) then; Y1 = A1; Y2 = A2; Y3 = A4; res = TRUE
      else if (A3==W AND Z1==N1 AND Z2==N4 AND Z3==N2) then; Y1 = A1; Y2 = A4; Y3 = A2; res = TRUE
      else if (A3==W AND Z1==N4 AND Z2==N2 AND Z3==N1) then; Y1 = A4; Y2 = A2; Y3 = A1; res = TRUE
      else if (A3==W AND Z1==N4 AND Z2==N1 AND Z3==N2) then; Y1 = A4; Y2 = A1; Y3 = A2; res = TRUE
      else if (A4==W AND Z1==N2 AND Z2==N3 AND Z3==N1) then; Y1 = A2; Y2 = A3; Y3 = A1; res = TRUE
      else if (A4==W AND Z1==N2 AND Z2==N1 AND Z3==N3) then; Y1 = A2; Y2 = A1; Y3 = A3; res = TRUE
      else if (A4==W AND Z1==N3 AND Z2==N2 AND Z3==N1) then; Y1 = A3; Y2 = A2; Y3 = A1; res = TRUE
      else if (A4==W AND Z1==N3 AND Z2==N1 AND Z3==N2) then; Y1 = A3; Y2 = A1; Y3 = A2; res = TRUE
      else if (A4==W AND Z1==N1 AND Z2==N2 AND Z3==N3) then; Y1 = A1; Y2 = A2; Y3 = A3; res = TRUE
      else if (A4==W AND Z1==N1 AND Z2==N3 AND Z3==N2) then; Y1 = A1; Y2 = A3; Y3 = A2; res = TRUE
      end if
   end function

   function has_4_connections_2(self,X,W,Z,Y1,Y2,Y3) result (res)
   ! Returns TRUE if the atom with index "X" has four connections: one
   ! to the atom with index "W", the other three to atoms with atomic
   ! number "Z".If there are these latter connections, return the
   ! indices of atoms with this atomic number in "Y1", "Y2" and "Y3"
   ! respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: X,W,Z
      INT, OUT :: Y1,Y2,Y3
      BIN :: res
      INT :: A1,A2,A3,A4,N1,N2,N3,N4
      Y1 = 0
      Y2 = 0
      Y3 = 0
      res = FALSE
      if (size(connections_for(X)%element)/=4) return
      A1 = connections_for(X)%element(1)
      A2 = connections_for(X)%element(2)
      A3 = connections_for(X)%element(3)
      A4 = connections_for(X)%element(4)
      N1 = self(A1)%atomic_number
      N2 = self(A2)%atomic_number
      N3 = self(A3)%atomic_number
      N4 = self(A4)%atomic_number
      if (A1==W AND Z==N2 AND Z==N3 AND Z==N4) then; Y1 = A2; Y2 = A3; Y3 = A4; res = TRUE
      else if (A2==W AND Z==N1 AND Z==N3 AND Z==N4) then; Y1 = A1; Y2 = A3; Y3 = A4; res = TRUE
      else if (A3==W AND Z==N2 AND Z==N1 AND Z==N4) then; Y1 = A2; Y2 = A1; Y3 = A4; res = TRUE
      else if (A4==W AND Z==N2 AND Z==N3 AND Z==N1) then; Y1 = A2; Y2 = A3; Y3 = A1; res = TRUE
      end if
   end function


! Rings & chains

   function has_ring(self,A,B,Z,R) result (res)
   ! Return TRUE if the atoms with indices "A" and "B" form part of a
   ! single connection ring of atoms whose atomic numbers after atom
   ! "B" are given in "Z". NOTE: "A" need not be single connected.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: A,B
      VEC(INT,:), IN :: Z
      VEC(INT,:), optional, OUT :: R
      BIN :: res
      INT :: i,W,X,Y
      W = A
      X = B
      do i = 1,size(Z)
         res = has_2_connections_(self,X,W,Z(i),Y)
         if (NOT res) return
         if (present(R)) R(i) = Y
         W = X
         X = Y
      end do
      res = has_a_connection_(X,A)
   end function

   function has_chain(self,A,B,Z,C) result (res)
   ! Return TRUE if the atoms with indices "A" and "B" form part of a
   ! single connection chain of atoms whose atomic numbers after atom
   ! "B" are given in "Z".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: A,B
      VEC(INT,:), IN :: Z
      VEC(INT,:), optional, OUT :: C
      BIN :: res
      INT :: i,W,X,Y
      res = FALSE
      W = A
      X = B
      do i = 1,size(Z)
         res = has_2_connections_(self,X,W,Z(i),Y)
         if (NOT res) return
         if (present(C)) C(i) = Y
         W = X
         X = Y
      end do
   end function


!  ====================
!  Amino acid detectors
!  ====================

   subroutine put_protein_sequence(self)
   ! Return the "backbone" of atom indices in each protein backbone.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: backbone
      VEC(INT,:), allocatable :: terminal
      BIN :: N_terminal
      INT :: i,N,Cp0,Ca,Cb,Cp,N1,Ot,Ct
      call find_and_put_N_terminal_atoms_(self,terminal)
      call create_(backbone,size(terminal),1)
      do i = 1,size(terminal)
         N = terminal(i)
         backbone(i)%element(1) = N
         Cp0 = 0
         call show_(stdout,"Sequence for protein strand =",i)
         do
            if (NOT is_protein_backbone_N_(self,N,Cp0,Ca,Cb,Cp,N1,Ot,Ct,N_terminal,try=TRUE)) exit
            if (Ot/=0) then
               call append_(backbone(i)%element,[Ca,Cp,Ot])
               exit
            else if (Ct/=0) then
               call append_(backbone(i)%element,[Ca,Cp,Ct])
               exit
            else
               call append_(backbone(i)%element,[Ca,Cp,N1])
               N = N1
               Cp0 = Cp
            end if
            if (Cb==0) then; call text_(stdout,"GLY")
            else; call text_(stdout,protein_side_chain_3_code_(self,Ca,Cb))
            end if
         end do
      end do
      call destroy_(terminal)
      call destroy_(backbone)
      call destroy_(connections_for)
   end subroutine


!   find_protein_backbone_from_N(N,C,found,Ca,Cc,N1,Ot)
!   ! Given the index "N" of a nitrogen atom and possibly the index "C"
!   ! of a connected carbon atom, return "found" as TRUE if that
!   ! nitrogen atom is part of a protein backbone. Also return the
!   ! index "Ca" of the C-alpha carbon, the index "Cc" of the C-carboxy
!   ! carbon, and either "N1" the next nitrogen in the backbone, or
!   ! "Ot" the terminal oxygen in the backbone. This assumes a
!   ! connection table has been made.
!     N,C :: INT, IN
!     found :: BIN, OUT
!     Ca,Cc,N1,Ot :: INT, OUT
!
!   ENSURE(connections_for.allocated,"no connection table")
!   ENSURE(connections_for.dim==.dim,"wrong size connection table")
!   ENSURE(N>0 AND N<=.dim,"N index out of range")
!   ENSURE(self(N).atomic_number==7,"N is not the index of a nitrogen atom")
!   ENSURE(C>=0 AND C<=.dim,"C index out of range")
!
!     nn,na,ia,nc,ic,O2,X :: INT
!
!     ! Set defaults
!     found = FALSE
!     Ca = 0; Cc = 0; N1 = 0; Ot = 0
!
!     ! Check if C is bonded to N
!     if (C>0) then
!        DIE_IF(NOT self(N).is_bonded_to(self(C)),"N is not bonded to C")
!     end
!
!     ! # of connections for N
!     nn = connections_for(N).element.dim
!
!     ! A backbone N must have 1 connection and can't have
!     ! more than 2 connections
!     if (nn<=0) return
!     if (nn >2) return
!
!     ! Get the index of C-alpha atom
!     Ca = connections_for(N)[1]
!     if (Ca==C) then
!        DIE_IF(nn<2,"not enough connections to N = "//trim(N.to_str))
!        Ca = connections_for(N)[2]
!     end
!
!     ! C-alpha must be a carbon
!     if (self(Ca).atomic_number/=6) return
!
!     ! # of  connections for C-alpha
!     na = connections_for(Ca).element.dim
!
!     ! C-alpha must have 2 connections (GLY) or 3 connections
!     if (na/=3 AND na/=2) return
!
!     ! Now find C-carboxy carbon
!     do ia = 1,na
!        Cc = connections_for(Ca)[ia]          ! Index of C-carboxy
!        if (Cc==N) cycle                      ! Cc can't be the first N
!        if (self(Cc).atomic_number/=6) cycle  ! Cc must be a carbon
!        nc = connections_for(Cc).element.dim
!        if (nc/=3) cycle                      ! C-carboxy must have 3 connections
!        O2 = 0                                ! It must have a =O
!        N1 = 0                                ! It must have a -N, or
!        Ot = 0                                ! It must have a terminal -OH
!        do ic = 1,nc
!           X = connections_for(Cc)[ic]
!           if (X==Ca) cycle
!           if (self(X).atomic_number==7) N1 = X
!           if (self(X).atomic_number==8) then
!              if (Ot==0) then; O2 = X
!              else;            Ot = X
!              end
!           end
!        end
!        found = O2/=0 AND (N1/=0 OR Ot/=0)
!        if (found) exit
!     end
!   end

   function protein_side_chain_3_code(self,Ca,Cb) result (res)
   ! Returns the 3 letter code for a side chain connected at carbon
   ! atoms "Ca" and "Cb".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      STR(len=STR_SIZE) :: res
   ENSURE(Ca>=0 AND Ca<=size(self),"VEC{ATOM}:protein_side_chain_3_code ... Ca index out of range")
   ENSURE(Cb>=0 AND Cb<=size(self),"VEC{ATOM}:protein_side_chain_3_code ... Cb index out of range")
   ENSURE(self(Ca)%atomic_number==6,"VEC{ATOM}:protein_side_chain_3_code ... must suppy index of alpha carbon atom")
   ENSURE(self(Cb)%atomic_number==6,"VEC{ATOM}:protein_side_chain_3_code ... must suppy index of beta carbon atom")
      if (has_ALA_(Ca,Cb)) then; res = "ALA"
      else if (has_ARG_(self,Ca,Cb)) then; res = "ARG"
      else if (has_ASN_(self,Ca,Cb)) then; res = "ASN"
      else if (has_ASP_(self,Ca,Cb)) then; res = "ASP"
      else if (has_CYS_(self,Ca,Cb)) then; res = "CYS"
      else if (has_GLN_(self,Ca,Cb)) then; res = "GLN"
      else if (has_GLU_(self,Ca,Cb)) then; res = "GLU"
      else if (has_HIS_(self,Ca,Cb)) then; res = "HIS"
      else if (has_ILE_(self,Ca,Cb)) then; res = "ILE"
      else if (has_LEU_(self,Ca,Cb)) then; res = "LEU"
      else if (has_LYS_(self,Ca,Cb)) then; res = "LYS"
      else if (has_MET_(self,Ca,Cb)) then; res = "MET"
      else if (has_PHE_(self,Ca,Cb)) then; res = "PHE"
      else if (has_SER_(self,Ca,Cb)) then; res = "SER"
      else if (has_THR_(self,Ca,Cb)) then; res = "THR"
      else if (has_TRP_(self,Ca,Cb)) then; res = "TRP"
      else if (has_TYR_(self,Ca,Cb)) then; res = "TYR"
      else if (has_VAL_(self,Ca,Cb)) then; res = "VAL"
      else; res = "UNK"
      end if
   end function

   pure function has_ALA(Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes ALA
      INT, IN :: Ca,Cb
      BIN :: res
      res = has_1_connection_(Cb,Ca)
   end function

   function has_ARG(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes ARG
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      VEC(INT,4) :: C
      INT :: Nz1,Nz2
      res = has_chain_(self,Ca,Cb,[6,6,7,6],C)
      if (NOT res) return
      res = has_3_connections_(self,C(4),C(3),7,7,Nz1,Nz2)
      if (NOT res) return
      res = has_1_connection_(Nz1,C(4))
      if (NOT res) return
      res = has_1_connection_(Nz2,C(4))
   end function

   function has_ASN(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes ASN
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Og,Ng
      res = has_2_connections_(self,Cb,Ca,6,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,6,7,Og,Ng)
      if (NOT res) return
      res = has_1_connection_(Og,Cg)
      if (NOT res) return
      res = has_1_connection_(Ng,Cg)
   end function

   function has_ASP(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes ASP
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Og1,Og2
      res = has_2_connections_(self,Cb,Ca,6,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,6,7,Og1,Og2)
      if (NOT res) return
      res = has_1_connection_(Og1,Cg)
      if (NOT res) return
      res = has_1_connection_(Og2,Cg)
   end function

   function has_CYS(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes CYS
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Sg
      res = has_2_connections_(self,Cb,Ca,16,Sg)
      if (NOT res) return
      res = has_1_connection_(Sg,Cb)
   end function

   function has_GLN(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes GLN
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      VEC(INT,2) :: C
      INT :: Ne,Oe
      res = has_chain_(self,Ca,Cb,[6,6],C)
      if (NOT res) return
      res = has_3_connections_(self,C(2),C(1),8,7,Oe,Ne)
      if (NOT res) return
      res = has_1_connection_(Ne,C(2))
      if (NOT res) return
      res = has_1_connection_(Oe,C(2))
   end function

   function has_GLU(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes GLU
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      VEC(INT,2) :: C
      INT :: Oe1,Oe2
      res = has_chain_(self,Ca,Cb,[6,6],C)
      if (NOT res) return
      res = has_3_connections_(self,C(2),C(1),8,8,Oe1,Oe2)
      if (NOT res) return
      res = has_1_connection_(Oe1,C(2))
      if (NOT res) return
      res = has_1_connection_(Oe2,C(2))
   end function

   function has_HIS(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes HIS
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Nd,Cd
      res = has_2_connections_(self,Cb,Ca,16,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,7,6,Nd,Cd)
      if (NOT res) return
      res = has_ring_(self,Cg,Nd,[6,7,6])
   end function

   function has_ILE(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes ILE
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg1,Cg2,Cd
      res = has_3_connections_(self,Cb,Ca,6,6,Cg1,Cg2)
      if (NOT res) return
      if (has_1_connection_(Cg1,Cb)) then
         res = has_2_connections_(self,Cg2,Cb,6,Cd)
         if (NOT res) return
         res = has_1_connection_(Cd,Cg2)
      else if (has_1_connection_(Cg2,Cb)) then
         res = has_2_connections_(self,Cg1,Cb,6,Cd)
         if (NOT res) return
         res = has_1_connection_(Cd,Cg1)
      else
         res = FALSE
      end if
   end function

   function has_LEU(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes LEU
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Cd1,Cd2
      res = has_2_connections_(self,Cb,Ca,6,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,6,7,Cd1,Cd2)
      if (NOT res) return
      res = has_1_connection_(Cd1,Cg)
      if (NOT res) return
      res = has_1_connection_(Cd2,Cg)
   end function

   function has_LYS(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes LYS
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      VEC(INT,4) :: C
      res = has_chain_(self,Ca,Cb,[6,6,6,7],C)
      if (NOT res) return
      res = has_1_connection_(C(4),C(3))
   end function

   function has_MET(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes MET
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      VEC(INT,3) :: C
      res = has_chain_(self,Ca,Cb,[6,16,6],C)
      if (NOT res) return
      res = has_1_connection_(C(3),C(2))
   end function

   function has_PHE(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes PHE
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Cd1,Cd2
      res = has_2_connections_(self,Cb,Ca,6,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,6,6,Cd1,Cd2)
      if (NOT res) return
      res = has_ring_(self,Cg,Cd1,[6,6,6,6])
   end function

   function has_SER(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes SER
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Og
      res = has_2_connections_(self,Cb,Ca,8,Og)
      if (NOT res) return
      res = has_1_connection_(Og,Cb)
   end function

   function has_THR(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes THR
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Og
      res = has_3_connections_(self,Cb,Ca,6,8,Cg,Og)
      if (NOT res) return
      res = has_1_connection_(Cg,Cb)
      if (NOT res) return
      res = has_1_connection_(Og,Cb)
   end function

   function has_TRP(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes TRP
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      VEC(INT,4) :: C
      INT :: Cg,Cd1,Cd2,Ne,Ce1,Cf1
      res = has_2_connections_(self,Cb,Ca,6,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,6,6,Cd1,Cd2)
      if (NOT res) return
      if (has_2_connections_(self,Cd1,Cg,7,Ne)) then
         res = has_2_connections_(self,Ne,Cd1,6,Ce1)
         if (NOT res) return
         res = has_3_connections_(self,Ce1,Ne,Cd2,6,Cf1)
         if (NOT res) return
         res = has_chain_(self,Ce1,Cf1,[6,6,6,6],C)
         if (NOT res) return
         res = C(4)==Cd2
      else if (has_2_connections_(self,Cd2,Cg,7,Ne)) then
         res = has_2_connections_(self,Ne,Cd2,6,Ce1)
         if (NOT res) return
         res = has_3_connections_(self,Ce1,Ne,Cd1,6,Cf1)
         if (NOT res) return
         res = has_chain_(self,Ce1,Cf1,[6,6,6,6],C)
         if (NOT res) return
         res = C(4)==Cd1
      else
         res = FALSE
      end if
   end function

   function has_TYR(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes TYR
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg,Cd1,Cd2,Ce1,Ce2,Cz,Oz
      res = has_2_connections_(self,Cb,Ca,6,Cg)
      if (NOT res) return
      res = has_3_connections_(self,Cg,Cb,6,6,Cd1,Cd2)
      if (NOT res) return
      res = has_2_connections_(self,Cd1,Cg,6,Ce1)
      if (NOT res) return
      res = has_2_connections_(self,Cd2,Cg,6,Ce2)
      if (NOT res) return
      res = has_2_connections_(self,Ce1,Cg,6,Cz)
      if (NOT res) return
      res = has_2_connections_(self,Ce2,Cg,6,Cz)
      if (NOT res) return
      res = has_3_connections_(self,Cz,Ce1,Ce2,8,Oz)
      if (NOT res) return
      res = has_1_connection_(Oz,Cz)
   end function

   function has_VAL(self,Ca,Cb) result (res)
   ! Returns TRUE if the side chain at the alpha and beta carbon
   ! "Ca" and "Cb" describes THR
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: Ca,Cb
      BIN :: res
      INT :: Cg1,Cg2
      res = has_3_connections_(self,Cb,Ca,6,6,Cg1,Cg2)
      if (NOT res) return
      res = has_1_connection_(Cg1,Cb)
      if (NOT res) return
      res = has_1_connection_(Cg2,Cb)
   end function


!  ==============
!  Salvador atoms
!  ==============

   pure function minimum_distances(self) result (res)
   ! Make the minimum distances between atoms.
   ! Required for the Stratmann-Scuseria partitioning.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), allocatable :: res
      INT :: p,q,qmin
      REAL :: px,py,pz
      REAL :: qx,qy,qz
      REAL :: dx,dy,dz
      REAL :: d2min,d2
      allocate(res(size(self)))
      if (size(self)==1) then
         res(1) = ZERO
         return
      else
         res(:) = -ONE
      end if
      do p = 1,size(self)

         ! Done already?
         if (res(p)>ZERO) cycle
         px = self(p)%position(1)
         py = self(p)%position(2)
         pz = self(p)%position(3)

         ! Start large
         d2min = huge(ONE)
         do q = 1,p-1
            if (p==q) cycle

            ! Done already?
            if (res(q)>ZERO) cycle
            qx = self(q)%position(1)
            qy = self(q)%position(2)
            qz = self(q)%position(3)

            ! x2 distance
            dx = px-qx
            dx = dx*dx
            if (dx>=d2min) cycle

            ! y2 distance
            dy = py-qy
            dy = dy*dy
            if (dy>=d2min) cycle

            ! z2 distance
            dz = pz-qz
            dz = dz*dz
            if (dz>=d2min) cycle
            d2 = dx+dy+dz
            if (d2>=d2min) cycle
            d2min = d2
            qmin = q
         end do

         ! Store minimum distance
         d2min = sqrt(d2min)
         q = qmin
         res(p) = d2min
         res(q) = d2min
      end do
   end function

   pure function Salvador_neighbours_of(self,a) result (res)
   ! Return list of Salvador & Ramos-Cordoba neighbours for atom "a".
   ! See (2013) JCP 139 p071103. Especially p. 3, end of 3rd paragraph. 
   ! NOTE: no disorder groups
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      VEC(INT,:), allocatable :: res
      VEC(REAL,3) :: ra,rb,rm,rc
      VEC(BIN,:), allocatable :: is_close
      REAL :: dm,dc
      INT :: b,c

      ! Position of "a"
      ra = self(a)%position

      ! Atoms close to "a"
      allocate(is_close(size(self)))
      is_close = TRUE
      is_close(a) = FALSE

      ! Is "b" close?
      do b = 1,size(self)
         if (b==a) cycle
         rb = self(b)%position
         rm = HALF*(ra+rb)  ! rm = midpt of "ab"
         dm = norm2(ra-rm)  ! dist of "a" to midpt
         do c = 1,size(self)  ! third nucleus "c"
            if (c==a) cycle
            if (c==b) cycle
            rc = self(c)%position
            dc = norm2(rc-rm)  ! dist of "c" to midpt

            ! Is nucleus "c" closer to the midpt
            ! of "ab" than "a" is, to the same?
            if (dc<dm) then
               is_close(b) = FALSE
               exit
            end if
         end do
      end do
      res = pack([(b,b = 1,size(self))],is_close)
   end function

   pure subroutine make_Salvador_neighbours(self,bonds_for)
   ! Make the Salvador & Ramos-Cordoba atom connections "bonds_for".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: bonds_for
      INT :: a

      ! Create connection table
      allocate(bonds_for(size(self)))
      do a = 1,size(self)
         bonds_for(a)%element = Salvador_neighbours_of_(self,a)
      end do
   end subroutine


!  ========================
!  Bonds, distances, angles
!  ========================

   pure function distance_between(self,a,b) result (res)
   ! Return the distance between "a" and "b".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      REAL :: res
      res = distance_to_(self(a),self(b))
   end function

   pure function nearest_atom_to(self,pos) result (res)
   ! Return the index of the nearest atom to point "pos".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), IN :: pos
      INT :: res
      INT :: a
      VEC(REAL,3) :: dpos
      REAL :: dmin,dist
      res = 1
      dmin = huge(ONE)
      do a = 1,size(self)
         dpos = pos-self(a)%position
         dist = norm_(dpos)
         if (dist<dmin) then
            dmin = dist
            res = a
         end if
      end do
   end function

   PURE function nearest_atoms_to(self,pos,tol) result (res)
   ! Return the indices of the nearest atoms to point "pos"
   ! within a distance tolerance of "tol".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), IN :: pos
      REAL, IN :: tol
      VEC(INT,:), allocatable :: res
      INT :: a,n
      VEC(REAL,3) :: dpos
      VEC(REAL,:), allocatable :: dist
      call create_(dist,size(self))

      ! Distances
      do a = 1,size(self)
         dpos = pos-self(a)%position
         dist(a) = norm_(dpos)
      end do

      ! Distances from minimum value
      dist = dist-minval(dist)

      ! How many within "tol"?
      n = count(dist<=tol)
      call create_(res,n)

      ! Get indices
      n = 0
      do a = 1,size(self)
         if (dist(a)>tol) cycle
         n = n+1
         res(n) = a
      end do
      call destroy_(dist)
   end function

   pure function bonded(self,a,b,range_factor,H_bonded) result (res)
   ! Return true if "self" is bonded to "b". 
   ! If present, "range_factor" is used to determine the 
   ! distance range in which the atoms are regarded as bonded.
   ! If present, "H_bonded" checks for vdw H bonds.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      REAL, optional, IN :: range_factor
      BIN, optional, IN :: H_bonded
      BIN :: res
      res = is_bonded_to_(self(a),self(b),range_factor)

      ! Check if H-bonds are to be considered
      if (present(H_bonded)) then
         if (H_bonded) then
            if (NOT res) then
               if (self(a)%atomic_number==1 OR self(b)%atomic_number==1) then
                  res = is_vdw_bonded_to_(self(a),self(b),range_factor)
               end if
            end if
         end if
      end if
   end function

   pure function are_nearby(self,a,b,dist) result (res)
   ! Return whether atoms "a" and "b" are nearby, i.e. within length "dist".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      REAL, IN :: dist
      BIN :: res
      res = is_nearby_to_(self(a),self(b),dist)
   end function

   pure function connected(self,a,b,c,d) result (res)
   ! Return true if atoms "a" "b" "c" and "d" are connected together
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b,c,d
      BIN :: res
      res = bonded_(self,a,b) OR bonded_(self,a,c) OR bonded_(self,a,d)
      res = res AND (bonded_(self,b,a) OR bonded_(self,b,c) OR bonded_(self,b,d))
      res = res AND (bonded_(self,c,a) OR bonded_(self,c,b) OR bonded_(self,c,d))
      res = res AND (bonded_(self,d,a) OR bonded_(self,d,b) OR bonded_(self,d,c))
   end function

   pure function vdw_bonded(self,a,b,range_factor,vdw_range_pc) result (res)
   ! Return true if "a" is vdw bonded to "b". Atoms which are
   ! covalently bonded are not van-der-waals bonded.  If present,
   ! "range_factor" is used to determine the distance range in which
   ! the atoms are regarded as covalent bonded. If present
   ! "vdw_range_pc" is the percentage increase in the vdw radius to be
   ! tolerated while still regarding the atoms vdw bonded (default
   ! 10%). This uses the CCDC method, as documented on their web page.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      REAL, optional, IN :: range_factor
      REAL, optional, IN :: vdw_range_pc
      BIN :: res
      res = is_vdw_bonded_to_(self(a),self(b),range_factor,vdw_range_pc)
   end function

   pure function is_vdw_bonded_to(self,list,range_factor,vdw_range_pc) result (res)
   ! Return true if "self" has any atom vdw bonded to "list".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: list
      REAL, optional, IN :: range_factor
      REAL, optional, IN :: vdw_range_pc
      BIN :: res
      INT :: a,b
      res = FALSE
      do a = 1,size(self)
         do b = 1,size(list)
            res = is_vdw_bonded_to_(self(a),list(b),range_factor,vdw_range_pc)
            if (res) exit
         end do
      end do
   end function

   PURE function min_distance_between(self,group_a,group_b) result (res)
   ! Return the minimum distance between the atoms whose 
   ! indices are listed in "group_a" and "group_b".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: group_a
      VEC(INT,:), IN :: group_b
      REAL :: res
      INT :: a,b
      REAL :: val
   ENSURE(size(group_a)*size(group_b)>0,"VEC{ATOM}:min_distance_between ... must specify non-empty atom group arrays")
      res = huge(ONE)
      do a = 1,size(group_a)
         do b = 1,size(group_b)
            val = bond_distance_to_(self(group_a(a)),self(group_b(b)))
            res = min(res,val)
         end do
      end do
   end function

   pure function bonded_groups(self,g1,g2) result (res)
   ! Return true if any of the atoms in group "g1" are bonded 
   ! to any in group "g2", as determined by the group indices.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: g1,g2
      BIN :: res
      res = no_of_bonds_between_groups_(self,g1,g2)>0
   end function

   pure function no_of_bonds_between_groups(self,g1,g2) result (res)
   ! Return true if group "g1" is bonded to group "g2", 
   ! as determined by the group indices.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: g1,g2
      INT :: res
      INT :: n_group,a,b
      n_group = no_of_groups_(self)
      res = 0
      group_A: do a = 1,n_group
         if (self(a)%group/=g1) cycle
         group_B: do b = 1,n_group
            if (self(b)%group/=g2) cycle
            if (NOT is_bonded_to_(self(a),self(b))) cycle
            res = res+1
         end do group_B
      end do group_A
   end function

   pure function list_of_bonds_between_groups(self,g1,g2) result (res)
   ! Return the list of pairs of atom indices between group "g1" 
   ! and group "g2" which are bonded, according to the group indices.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: g1,g2
      MAT(INT,no_of_bonds_between_groups_(self,g1,g2),2) :: res
      INT :: n_group,n,a,b
      n_group = no_of_groups_(self)
      n = 0
      group_A: do a = 1,n_group
         if (self(a)%group/=g1) cycle
         group_B: do b = 1,n_group
            if (self(b)%group/=g1) cycle
            if (NOT is_bonded_to_(self(a),self(b))) cycle
            n = n+1
            res(n,1) = a
            res(n,2) = b
         end do group_B
      end do group_A
   end function

   pure function bonded_to(self,group) result (res)
   ! Return true if any of the atoms in "self" are bonded to any of
   ! the atoms in "group". 
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: group
      BIN :: res
      res = no_of_bonds_to_(self,group)>0
   end function

   pure function no_of_bonds_to(self,group) result (res)
   ! Return the number of bonds from the atoms in "self" to those
   ! atoms in "group". 
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: group
      INT :: res
      INT :: a,b
      res = 0
      group_A: do a = 1,size(self)
         group_B: do b = 1,size(group)
            if (NOT is_bonded_to_(self(a),self(b))) cycle
            res = res+1
         end do group_B
      end do group_A
   end function

   pure function list_of_bonds_to(self,group) result (res)
   ! Return the list of pairs of atom indices between group "g1" and
   ! group "g2" which are bonded.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: group
      MAT(INT,no_of_bonds_to_(self,group),2) :: res
      INT :: n,a,b
      n = 0
      group_A: do a = 1,size(self)
         group_B: do b = 1,size(group)
            if (NOT is_bonded_to_(self(a),self(b))) cycle
            n = n+1
            res(n,1) = a
            res(n,2) = b
         end do group_B
      end do group_A
   end function

   pure function has_vdw_bond(self) result (res)
   ! Return whether the atom list has two atoms involved in a
   ! (shortest) intermolecular bond. 
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      VEC(INT,2) :: vdw
      vdw = shortest_vdw_bond_(self)
      res = vdw(1)/=0 AND vdw(2)/=0
      ! More than one molecule?
   end function

   pure function shortest_vdw_bond(self) result (res)
   ! Return indices of the two atoms involved in the shortest
   ! IM van der waals bond. If there are none result is (0,0).
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,2) :: res
      INT :: a,b
      VEC(REAL,3) :: rab
      REAL :: dab,test
      BIN :: has_mols
      res = 0

      ! More than one molecule?
      has_mols = any(self(:)%site_disorder_group>1) OR any(self(:)%group>1)
      if (NOT has_mols) return
      test = HUGE(ONE)
      do a = 2,size(self)
         do b = 1,a-1

      ! UNCOMMENT
      ! if (NOT .vdw_bonded(a,b)) cycle
            rab = self(a)%position-self(b)%position
            dab = norm_(rab)
            if (dab<test) then
               res(1) = a
               res(2) = b
               test = dab
            end if
         end do
      end do
   end function

   PURE subroutine shorten_shortest_vdw_bond(self)
   ! Shorten the shortest intermolecular vdw bond to be the sum of the
   ! covalent bond lengths + 0.6 bohr, and apply the shift to all
   ! atoms in the group (there must be only two groups).
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(INT,2) :: vdw
      INT :: a,b,g
      REAL :: ra,rb
      VEC(REAL,3) :: r,d,v
   ENSURE(maxval(self(:)%group)==2,"VEC{ATOM}:shorten_shortest_vdw_bond ... more than two groups")
   ENSURE(minval(self(:)%group)==1,"VEC{ATOM}:shorten_shortest_vdw_bond ... there are not two groups")

      ! Shortest vdw bond
      vdw = shortest_vdw_bond_(self)
      a = vdw(1)
      b = vdw(2)

      ! Distance vector between atoms
      r = self(a)%position-self(b)%position

      ! Desired distance vector
      ra = covalent_radius_ccdc_(self(a))
      rb = covalent_radius_ccdc_(self(b))
      d = (r/norm_(r))*(ra+rb+0.6)

      ! Shift
      v = d-r

      ! Molecule index of atom "a"
      g = self(a)%group

      ! Shift group "a"
      call translate_group_(self,g,v)
   end subroutine

   pure function no_of_bonds_0(self,range_factor,H_bonded) result (res)
   ! Return the number of bonded atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL, optional, IN :: range_factor
      BIN, optional, IN :: H_bonded
      INT :: res
      INT :: a,b
      res = 0
      do a = 1,size(self)
         do b = 1,a-1
            if (bonded_(self,a,b,range_factor,H_bonded)) res = res+1
         end do
      end do
   end function

   pure function bond_distance_0(self,a,b,angstrom) result (res)
   ! Return the bond distance between atoms "a" and "b"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      BIN, optional, IN :: angstrom
      REAL :: res
      VEC(REAL,3) :: rab,rab2
      BIN :: change
      rab = self(b)%position-self(a)%position
      rab2 = rab*rab
      res = sqrt(rab2(1)+rab2(2)+rab2(3))
      change = FALSE
      if (present(angstrom)) change = angstrom
      if (change) res = res*ANGSTROM_PER_BOHR
   end function

   PURE function bond_distance_1(self,a,b,covariance,angstrom) result (res)
   ! Return the Hirshfeld test (and error) between atoms "a" and "b".
   ! The test is the first value, the error the second in res(:).
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: angstrom
      VEC(REAL,2) :: res
      VEC(REAL,3) :: rab,rab2
      VEC(REAL,6) :: der
      MAT(REAL,6,6) :: cov
      BIN :: change

      ! Bond length
      rab = self(b)%position-self(a)%position
      rab2 = rab*rab
      res(1) = sqrt(rab2(1)+rab2(2)+rab2(3))

      ! Covariance
      call put_blocks_to_(covariance,cov,block_dim=3,block_list=[a,b])

      ! Bond length error
      call bond_distance_deriv_(self,a,b,der)
      res(2) = sqrt(dot_(cov,der,der))
      change = FALSE
      if (present(angstrom)) change = angstrom
      if (change) res = res*ANGSTROM_PER_BOHR
   end function

   pure subroutine bond_distance_deriv(self,a,b,deriv)
   ! Return the bond distance derivatives "deriv" wrt cartesian
   ! positions between atoms "a" and "b". The bond vector is rb - ra
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      VEC(REAL,6), OUT :: deriv
      VEC(REAL,3) :: rab
      REAL :: dab

      ! Vector rab and (inverse) length
      rab = self(b)%position-self(a)%position
      dab = sqrt(rab(1)*rab(1)+rab(2)*rab(2)+rab(3)*rab(3))
      dab = ONE/dab

      ! Derivative
      deriv(1:3) = -rab*dab  ! wrt a
      deriv(4:6) = rab*dab  ! wrt b
   end subroutine

   pure function no_of_angles(self) result (res)
   ! Return the number of angles within bond contact distance
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: n,a,b,c
      n = size(self)
      res = 0
      do a = 1,n
         do b = 1,n
            if (b==a) cycle
            if (NOT bonded_(self,a,b)) cycle
            do c = 1,n
               if (c==a OR c==b) cycle
               if (NOT bonded_(self,b,c)) cycle
               if (c<a) cycle
               res = res+1
            end do
         end do
      end do
   end function

   pure function bond_angle(self,a,b,c,degrees) result (res)
   ! Return the bond angle between atoms "a" , "b" and "c".
   ! The central atom is "b". The angle is calculated as
   ! arccos(nba.nbc) . If "degrees" is present and TRUE,
   ! result is in degrees.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b,c
      BIN, optional, IN :: degrees
      REAL :: res
      VEC(REAL,3) :: rba,rbc
      BIN :: change
      rba = self(a)%position-self(b)%position
      rbc = self(c)%position-self(b)%position
      call normalize_(rba)
      call normalize_(rbc)
      res = dot_(rba,rbc)
      res = arccos_(res)
      change = FALSE
      if (present(degrees)) change = degrees
      if (change) res = res*DEGREE_PER_RADIAN
   end function

   pure function no_of_angle_center_atoms(self) result (res)
   ! Return the number of list of atom indices B which belong to
   ! angles centers angle(ABC)
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: n,a,b,c
      VEC(BIN,:), allocatable :: cnt
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE
      do a = 1,n
         do b = 1,n
            if (b==a) cycle
            if (NOT bonded_(self,a,b)) cycle
            do c = 1,n
               if (c==a OR c==b) cycle
               if (NOT bonded_(self,b,c)) cycle
               if (c<a) cycle
               cnt(b) = TRUE
            end do
         end do
      end do
      res = count(cnt)
      deallocate(cnt)
   end function

   pure function angle_center_atoms(self) result (res)
   ! Return the list of atom indices B which belong to angles centers
   ! angle(ABC)
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_angle_center_atoms_(self)) :: res
      INT :: n,a,b,c
      VEC(BIN,:), allocatable :: cnt
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE
      do a = 1,n
         do b = 1,n
            if (b==a) cycle
            if (NOT bonded_(self,a,b)) cycle
            do c = 1,n
               if (c==a OR c==b) cycle
               if (NOT bonded_(self,b,c)) cycle
               if (c<a) cycle
               cnt(b) = TRUE
            end do
         end do
      end do
      res = pack([(a,a = 1,n)],mask=cnt)
      deallocate(cnt)
   end function

   pure function no_of_angle_outer_atoms(self) result (res)
   ! Return the number of list of atoms which are outer angle atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: n,a,b,c
      VEC(BIN,:), allocatable :: cnt
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE
      do a = 1,n
         do b = 1,n
            if (b==a) cycle
            if (NOT bonded_(self,a,b)) cycle
            do c = 1,n
               if (c==a OR c==b) cycle
               if (NOT bonded_(self,b,c)) cycle
               if (c<a) cycle
               cnt(a) = TRUE
               cnt(c) = TRUE
            end do
         end do
      end do
      res = count(cnt)
      deallocate(cnt)
   end function

   pure function angle_outer_atoms(self) result (res)
   ! Return the list of atoms which are outer angle atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_angle_outer_atoms_(self)) :: res
      INT :: n,a,b,c
      VEC(BIN,:), allocatable :: cnt
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE
      do a = 1,n
         do b = 1,n
            if (b==a) cycle
            if (NOT bonded_(self,a,b)) cycle
            do c = 1,n
               if (c==a OR c==b) cycle
               if (NOT bonded_(self,b,c)) cycle
               if (c<a) cycle
               cnt(a) = TRUE
               cnt(c) = TRUE
            end do
         end do
      end do
      res = pack([(a,a = 1,n)],mask=cnt)
      deallocate(cnt)
   end function

   pure function no_of_torsion_angles(self) result (res)
   ! Return the number of torsion angles within bond contact distance
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: n,a,b,c,d
      n = size(self)
      res = 0
      do a = 1,n
         do b = 1,n
            if (a==b) cycle
            if (NOT bonded_(self,a,b)) cycle
            do c = 1,n
               if (a==c OR b==c) cycle
               if (NOT bonded_(self,b,c)) cycle
               do d = 1,n
                  if (a==d OR b==d OR c==d) cycle
                  if (NOT bonded_(self,c,d)) cycle
                  res = res+1
               end do
            end do
         end do
      end do
      res = res/2
   end function

   pure function torsion_angle_0(self,a,b,c,d) result (res)
   ! Return the torsion angle between atoms "a", "b", "c" and "d". The
   ! atoms are assumed connected like a--b--c--d and the angle
   ! returned is that between vectors (a-b) and (d-c) i.e. the torsion
   ! angle looking down the b--c bond.  torsion is positive if (a-b)
   ! is counterclockwise of (d-c). If "degrees" is present and TRUE,
   ! result is in degrees.  NOTE: If the result is -ONE, either
   ! a--b--c or b--c--d are colinear, and the variables "abc_colinear"
   ! and "bcd_colinear" are set.  See Tuzun et al (2000) J. Comp. Chem
   ! 21 p. 553-561
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b,c,d
      REAL :: res
      VEC(REAL,3) :: rba,rcb,rdc,tba,tdc
      REAL :: dot

      ! Vectors point from d -> c -> b -> a
      rba = self(a)%position-self(b)%position
      rcb = self(b)%position-self(c)%position
      rdc = self(c)%position-self(d)%position
      call normalize_(rba)
      call normalize_(rcb)
      call normalize_(rdc)
      call to_cross_product_of_(tba,rba,rcb)
      call to_cross_product_of_(tdc,rcb,rdc)

      ! Collinear test
      res = ZERO
      if (abs(norm_(tba))<TOL(5)) then
         res = -ONE
      end if
      if (abs(norm_(tdc))<TOL(5)) then
         res = -ONE
      end if
      if (res<ZERO) then
         res = ZERO  ! Reset angle to zero
         return
      end if
      call normalize_(tba)
      call normalize_(tdc)
      res = dot_product(tba,tdc)
      res = arccos_(res)

      ! Sign of angle: see Tuzun et al eq. (11)
      ! And also Figure 1
      dot = dot_product(rdc,tba)
      if (dot>ZERO) res = -res
      if (abs(res+PI)<TOL(5)) res = abs(res)  ! -180 -> 180
   end function

   function torsion_angle_1(self,a,b,c,d,abc_colinear,bcd_colinear,degrees) result (res)
   ! Return the torsion angle between atoms "a", "b", "c" and "d". The
   ! atoms are assumed connected like a--b--c--d and the angle
   ! returned is that between vectors (a-b) and (d-c) i.e. the torsion
   ! angle looking down the b--c bond.  torsion is positive if (a-b)
   ! is counterclockwise of (d-c). If "degrees" is present and TRUE,
   ! result is in degrees.  NOTE: If the result is -ONE, either
   ! a--b--c or b--c--d are colinear, and the variables "abc_colinear"
   ! and "bcd_colinear" are set.  See Tuzun et al (2000) J. Comp. Chem
   ! 21 p. 553-561
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b,c,d
      BIN, OUT :: abc_colinear,bcd_colinear
      BIN, optional, IN :: degrees
      REAL :: res
      BIN :: change
      VEC(REAL,3) :: rba,rcb,rdc,tba,tdc
      REAL :: dot
      abc_colinear = FALSE
      bcd_colinear = FALSE

      ! Vectors point from d -> c -> b -> a
      rba = self(a)%position-self(b)%position
      rcb = self(b)%position-self(c)%position
      rdc = self(c)%position-self(d)%position
      call normalize_(rba)
      call normalize_(rcb)
      call normalize_(rdc)
      call to_cross_product_of_(tba,rba,rcb)
      call to_cross_product_of_(tdc,rcb,rdc)

      ! Collinear test
      res = ZERO
      if (abs(norm_(tba))<TOL(5)) then
         res = -ONE
      end if
      if (abs(norm_(tdc))<TOL(5)) then
         res = -ONE
      end if
      if (res<ZERO) then
         res = ZERO  ! Reset angle to zero
         return
      end if
      call normalize_(tba)
      call normalize_(tdc)
      res = dot_product(tba,tdc)
      res = arccos_(res)

      ! Sign of angle: see Tuzun et al eq. (11)
      ! And also Figure 1
      dot = dot_product(rdc,tba)
      if (dot>ZERO) res = -res
      if (abs(res+PI)<TOL(5)) res = abs(res)  ! -180 -> 180
      change = FALSE
      if (present(degrees)) change = degrees
      if (change) res = res*DEGREE_PER_RADIAN
   end function

   pure function no_of_torsion_angle_1st_atoms(self) result (res)
   ! Return the number of list of indices of 1st atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a1) = TRUE
               end do
            end do
         end do
      end do
      res = count(cnt)
      deallocate(cnt)
   end function

   pure function torsion_angle_1st_atoms(self) result (res)
   ! Return the list of indices of 1st atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_torsion_angle_1st_atoms_(self)) :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a1) = TRUE
               end do
            end do
         end do
      end do
      res = pack([(a1,a1 = 1,n)],mask=cnt)
      deallocate(cnt)
   end function

   pure function no_of_torsion_angle_2nd_atoms(self) result (res)
   ! Return the number of list of indices of 2nd atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a2) = TRUE
               end do
            end do
         end do
      end do
      res = count(cnt)
      deallocate(cnt)
   end function

   pure function torsion_angle_2nd_atoms(self) result (res)
   ! Return the list of indices of 2nd atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_torsion_angle_2nd_atoms_(self)) :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a2) = TRUE
               end do
            end do
         end do
      end do
      res = pack([(a1,a1 = 1,n)],mask=cnt)
      deallocate(cnt)
   end function

   pure function no_of_torsion_angle_3rd_atoms(self) result (res)
   ! Return the number of list of indices of 2nd atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a3) = TRUE
               end do
            end do
         end do
      end do
      res = count(cnt)
      deallocate(cnt)
   end function

   pure function torsion_angle_3rd_atoms(self) result (res)
   ! Return the list of indices of 2nd atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_torsion_angle_3rd_atoms_(self)) :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a3) = TRUE
               end do
            end do
         end do
      end do
      res = pack([(a1,a1 = 1,n)],mask=cnt)
      deallocate(cnt)
   end function

   pure function no_of_torsion_angle_4th_atoms(self) result (res)
   ! Return the umber list of indices of 2nd atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
      VEC(BIN,:), allocatable :: cnt
      INT :: a1,a2,a3,a4,n

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a4) = TRUE
               end do
            end do
         end do
      end do
      res = count(cnt)
      deallocate(cnt)
   end function

   pure function torsion_angle_4th_atoms(self) result (res)
   ! Return the list of indices of 2nd atoms in a torsion angle
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,no_of_torsion_angle_4th_atoms_(self)) :: res
      INT :: a1,a2,a3,a4,n
      VEC(BIN,:), allocatable :: cnt

      ! Length
      n = size(self)
      allocate(cnt(n))
      cnt = FALSE

      ! Loop and store first atoms
      do a1 = 1,n
         do a2 = 1,n
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do a3 = 1,n
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do a4 = 1,n
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle
                  cnt(a4) = TRUE
               end do
            end do
         end do
      end do
      res = pack([(a1,a1 = 1,n)],mask=cnt)
      deallocate(cnt)
   end function

   function has_all_Hs_single_bonded(self,has_Hs) result (res)
   ! Return TRUE if "self" has all H atom single bonded. If there are
   ! no H's then the result is TRUE but "has_Hs" is set FALSE..
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, OUT :: has_Hs
      BIN :: res
      INT :: a,b,n
      res = TRUE
      has_Hs = FALSE
      do a = 1,size(self)
         if (self(a)%atomic_number/=1) cycle
         has_Hs = TRUE
         n = 0
         do b = 1,size(self)
            if (self(b)%atomic_number<=1) cycle
            if (NOT is_bonded_to_(self(a),self(b))) cycle
            n = n+1
            if (n>1) exit
         end do
         res = n==1
         if (NOT res) exit
      end do
   end function

   PURE function Spackman86_dispersion_energy(self,atom) result (res)
   ! Return Spackman's dispersion energy between "self" and "atom"s.
   ! See Spackman M. A. (1986) J. Chem. Phys. 85(11) p. 6579
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      REAL :: res
      INT :: i,j
      REAL :: r,a_i,a_j
   ENSURE(has_all_Spackman86_a_coeffs_(self),"VEC{ATOM}:Spackman86_dispersion_energy ... self: missing Spackman a coefficients")
   ENSURE(has_all_Spackman86_a_coeffs_(atom),"VEC{ATOM}:Spackman86_dispersion_energy ... atom: missing Spackman a coefficients")

      ! Loop over atom pairs
      res = ZERO
      do i = 1,size(self)
         a_i = Spackman86_a_6_dispersion_coeff(self(i))
         do j = 1,size(atom)
            a_j = Spackman86_a_6_dispersion_coeff(atom(j))
            r = bond_distance_to_(self(i),atom(j))
            res = res-a_i*a_j/r**6
         end do
      end do
   end function

   PURE function Spackman86_repulsion_energy(self,atom) result (res)
   ! Return Spackman's repulsion energy between "self" and "atom"s.
   ! See Spackman M. A. (1986) J. Chem. Phys. 85(11) p. 6579
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      REAL :: res
      INT :: i,j,Z_i,Z_j
      REAL :: r,r_offset
      REAL :: b_i,b_j
      REAL :: c_i,c_j
   ENSURE(has_all_Spackman86_b_c_coeffs_(self),"VEC{ATOM}:Spackman86_repulsion_energy ... self: missing Spackman b/c coefficients")
   ENSURE(has_all_Spackman86_b_c_coeffs_(atom),"VEC{ATOM}:Spackman86_repulsion_energy ... atom: missing Spackman b/c coefficients")
      res = ZERO

      ! Loop over pairs
      do i = 1,size(self)
         Z_i = self(i)%atomic_number
         b_i = Spackman86_b_repulsion_coeff(self(i))
         c_i = Spackman86_c_repulsion_coeff(self(i))
         do j = 1,size(atom)
            Z_j = atom(j)%atomic_number
            b_j = Spackman86_b_repulsion_coeff(atom(j))
            c_j = Spackman86_c_repulsion_coeff(atom(j))
            r = bond_distance_to_(self(i),atom(j))

            ! Modify distance r only for repulsion term
            ! for H atoms ... just an approximate hack
            r_offset = ZERO
            if (Z_i==1 OR Z_j==1) r_offset = HALF*BOHR_PER_ANGSTROM  ! X-H interactions
            if (Z_i==1 AND Z_j==1) r_offset = TENTH*BOHR_PER_ANGSTROM  ! H-H interactions
            r = r+r_offset
            res = res+b_i*b_j*exp(-r*(c_i+c_j))
         end do
      end do
   end function

   PURE function Grimme06_dispersion_energy(self,atom) result (res)
   ! Return Grimmes's dispersion energy between "self" and "atom"s.
   ! Based on C6 terms from Grimme (2006) J. Comp. Chem.  27(15) p. 1787
   ! E_disp = sum over atoms (C6 / r^6).
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      REAL :: res
      INT :: i,j
      REAL :: rij,a_i,a_j,r_i,r_j
      REAL :: d,rr,damping_factor
   ENSURE(has_all_Grimme06_a_coeffs_(self),"VEC{ATOM}:Grimme06_dispersion_energy ... self: missing Grimme a coefficients")
   ENSURE(has_all_Grimme06_a_coeffs_(atom),"VEC{ATOM}:Grimme06_dispersion_energy ... atom: missing Grimme a coefficients")
      res = ZERO
      d = TWO*TEN  ! Magic number

      ! Loop over pairs
      do i = 1,size(self)
         a_i = Grimme06_a_6_dispersion_coeff(self(i))
         r_i = Grimme06_r_0_dispersion_coeff(self(i))
         do j = 1,size(atom)
            a_j = Grimme06_a_6_dispersion_coeff(atom(j))
            r_j = Grimme06_r_0_dispersion_coeff(atom(j))
            rr = (r_i+r_j)
            rij = bond_distance_to_(self(i),atom(j))
            damping_factor = (ONE/(ONE+exp(-d*(rij/(rr)-ONE))))
            res = res-(a_i*a_j/rij**6*damping_factor)
         end do
      end do
   end function


!   dispersion_energy(atom) result (res)
!   ! Return the dispersion energy from various formula.
!   ! Refs?
!      self :: IN
!      atom :: VEC{ATOM}, IN
!      res  :: REAL
!
!      i, n1,n2 :: INT
!      e1,e2, edisp :: REAL
!      s6, rs6, s18, rs18, alp :: REAL
!
!      coords :: MAT{REAL}@
!      atomic_numbers :: VEC{INT}@
!      version :: INT
!      type(dftd3_calc) calc
!      type(dftd3_input) input
!
!      n1 = .dim
!      n2 = atom.dim
!      allocate(coords(3, n1 + n2))
!      allocate(atomic_numbers(n1 + n2))
!
!      ! Versions:
!      ! 6: DFT-D3 with Becke-Johnson damping parameters
!      ! 5: DFT-D3 with zero damping parameters
!      ! 4: DFT-D3 with Becke-Johnson finite-damping, variant 2 with their radii
!      ! 3: DFT-D3 with no damping
!      ! 2: DFT-D2
!      version = 4
!
!      ! include 3 body terms
!      input.threebody = FALSE
!
!      ! Numerical gradients
!      input.numgrad = FALSE
!
!      ! Cutoffs (should parameterize these)
!      !input%cutoff = sqrt(9000.0d0)
!      !input%cutoff_cn = sqrt(1600.0d0)
!
!      call dftd3_init(calc, input)
!      ! D2
!!     rs6 = 1.0d0
!!     s18 = 0.0d0
!!     alp = 20.0d0
!
!      ! hf
!   ! s6 = 1.0 
!   ! rs6 =0.3385
!   ! s18 =0.9171
!   ! rs18=2.8830
!
!      ! b3lyp
!      s6   = 1.0 
!      rs6  = 0.3981
!      s18  = 1.9889
!      rs18 = 4.4211
!      alp  = ZERO ! Remove warning
!
!      call dftd3_set_params(calc, [s6, rs6, s18, rs18, alp], version)
!
!      atomic_numbers = [self(:).atomic_number, atom(:).atomic_number]
!
!      do i = 1,n1
!         coords(:, i) = self(i).position
!      end
!
!      do i = 1,n2
!         coords(:, i+n1) = atom(i).position
!      end
!
!      edisp = ZERO
!      e1    = ZERO
!      e2    = ZERO
!      call dftd3_dispersion(calc, coords, atomic_numbers, edisp)
!      call dftd3_dispersion(calc, coords(:,1:n1), atomic_numbers(1:n1), e1)
!      call dftd3_dispersion(calc, coords(:,n1+1:n1+n2), atomic_numbers(n1+1:n1+n2), e2)
!
!      ! Do it
!      res = edisp - e2 - e1
!
!   end

   PURE function polarization_energy(self,F_sq,use_charged_values) result (res)
   ! Return simplified polarization energy
   ! Uses atomic polarizabilties from Thakkar and Lupinetti (2006)
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), IN :: F_sq
      BIN, optional, IN :: use_charged_values
      REAL :: res
      REAL :: polarizability
      INT :: i
   ENSURE(has_all_Thakkar_atomic_polarizabilities_(self),"VEC{ATOM}:polarization_energy ... self: missing Grimme a coefficients")
      res = ZERO
      do i = 1,size(self)
         polarizability = Thakkar_atomic_dipole_polarizability(self(i),use_charged_values)
         res = res+polarizability*F_sq(i)
      end do
      res = -HALF*res
   end function

   pure function has_all_Spackman86_a_coeffs(self) result (res)
   ! Return TRUE if "self" has all dispersion coefficients.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = all(Spackman86_a_6_dispersion_coeff(self)/=ZERO)
   end function

   pure function has_all_Spackman86_b_c_coeffs(self) result (res)
   ! Return TRUE if "self" has all repulsion coefficients.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      BIN :: has_b,has_c
      has_b = all(Spackman86_b_repulsion_coeff(self)/=ZERO)
      has_c = all(Spackman86_c_repulsion_coeff(self)/=ZERO)
      res = has_b AND has_c
   end function

   pure function has_all_Grimme06_a_coeffs(self) result (res)
   ! Return TRUE if "self" has all dispersion coefficients.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = all(Grimme06_a_6_dispersion_coeff(self)/=ZERO)
   end function

   pure function has_all_Thakkar_atomic_polarizabilities(self) result (res)
   ! Return TRUE if "self" has all atomic polarizabilities
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      res = all(Thakkar_atomic_dipole_polarizability(self)/=ZERO)
   end function


!  =====================================
!  Set anamalous dispersion coefficients
!  =====================================

!   set_dispersion_coefficients(wavelength)
!   ! set the anamalous dispersion coefficients at a certain
!   ! "wavelength" in bohr.
!      self :: inout
!      wavelength :: real, in
!
!      a :: int 
!
!      do a = 1,.dim
!         self(a).set_dispersion_coefficient(wavelength)
!      end
!
!   end

   PURE subroutine set_tabular_dispersion(self)
   ! set the anamalous dispersion coefficients enetered by the user in
   ! the table.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT :: a
      do a = 1,size(self)
         call set_tabular_dispersion_(self(a))
      end do
   end subroutine


!   has_all_dispersion result (res) :: PURE
!   ! Return TRUE if all the elements in the unique atom list
!   ! have disperesion coefficient entries in the table.
!      self :: IN
!      res :: BIN
!
!      a :: INT
!
!      ! Any without?
!      res = TRUE
!      do a = 1,.dim
!         res = self(a).has_dispersion
!         if (NOT res) exit
!      end
!
!   end

!   has_part_tabular_dispersion result (res) :: PURE
!   ! Return TRUE if nether all nor none of the elements in the unique
!   ! atom list have dispresion coefficient entries in the table.
!      self :: IN
!      res :: BIN
!
!      all,none :: BIN
!
!      all  = .has_all_tabular_dispersion
!      none = .has_no_tabular_dispersion
!      res  = (NOT all) AND (NOT none)
!
!   end

!   has_no_tabular_dispersion result (res) :: PURE
!   ! Return TRUE if all of the elements in the unique atom list have
!   ! *no* disperesion coefficient entries in the table. No means
!   ! "none" here.
!      self :: IN
!      res :: BIN
!
!      n_kind,u :: INT
!      ulist :: VEC{INT}@
!
!      ! Unique atom list
!      n_kind = maxval(self(:).kind)
!      ulist.create(n_kind)
!      .make_unique_atom_list(ulist)
!
!      ! Any with
!      res = TRUE
!      do u = 1,n_kind
!         res = NOT self(ulist(u)).has_tabular_dispersion
!         if (res) exit
!      end
!
!      ! Clean
!      ulist.destroy
!
!   end

!   has_all_tabular_dispersion result (res) :: PURE
!   ! Return TRUE if all the elements in the unique atom list
!   ! have disperesion coefficient entries in the table.
!      self :: IN
!      res :: BIN
!
!      n_kind,u :: INT
!      ulist :: VEC{INT}@
!
!      ! Unique atom list
!      n_kind = maxval(self(:).kind)
!      ulist.create(n_kind)
!      .make_unique_atom_list(ulist)
!
!      ! Any without?
!      res = TRUE
!      do u = 1,n_kind
!         res = self(ulist(u)).has_tabular_dispersion
!         if (NOT res) exit
!      end
!
!      ! Clean
!      ulist.destroy
!
!   end

   PURE subroutine get_dispersion_correction(self,Fa,k_pts,rf)
   ! Adds the anomalous dispersion correction for every atom in "Fa"
   ! for a set of supplied "k_pts" which are the list of user-supplied
   ! (hkl)'s multiplied by all the symmetry operations. "rf" are the
   ! repetition factors  (site symmetry factor). 
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(CPX,:), INOUT :: Fa
      MAT(REAL,:,:), IN :: k_pts
      VEC(REAL,:), IN :: rf
      INT :: a

      ! Loop over all atoms.
      ! Note that H U_iso refinement will not affect this section of code.
      do a = 1,size(self)
         call get_dispersion_(self(a),Fa,k_pts,rf(a))
      end do
   end subroutine

   PURE subroutine add_dispersion_correction(self,Fc,k_pts,rf)
   ! Adds the dispersion correction for every atom into "Fc" for a set
   ! of supplied "k_pts" which are the list of user-supplied (hkl)'s
   ! multiplied by all the symmetry operations. "rf" are the
   ! repetition factors  (site symmetry factor). 
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(CPX,:), INOUT :: Fc
      MAT(REAL,:,:), IN :: k_pts
      VEC(REAL,:), IN :: rf
      INT :: a

      ! Loop over all atoms.
      ! Note that H U_iso refinement will not affect this section of code.
      do a = 1,size(self)
         call add_dispersion_(self(a),Fc,k_pts,rf(a))
      end do
   end subroutine


!  ================================================
!  Position derivatives of bonds, distances, angles
!  ================================================

   subroutine bond_angle_deriv(self,a,b,c,deriv,abc_colinear)
   ! Return the bond angle derivatives "deriv" between atoms "a" , "b"
   ! and "c".  The central atom is "b". The angle is calculated as
   ! arccos(nba.nbc). See Tuzun et al (2000) J. Comp. Chem 21 p.
   ! 553-561 equation (6).
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b,c
      VEC(REAL,9), OUT :: deriv
      BIN, optional, OUT :: abc_colinear
      VEC(REAL,3) :: rba,rbc,rx,ry
      REAL :: dba,dbc,cs,sn
      if (present(abc_colinear)) abc_colinear = FALSE

      ! Vectors
      rba = self(a)%position-self(b)%position
      rbc = self(c)%position-self(b)%position

      ! Get lengths and normalize
      dba = ONE/norm_(rba); rba = rba*dba
      dbc = ONE/norm_(rbc); rbc = rbc*dbc

      ! sin part
      cs = dot_product(rba,rbc)
      sn = sqrt((ONE-cs)*(ONE+cs))
      if (abs(sn)<TOL(5)) then
         deriv = ZERO
         if (present(abc_colinear)) abc_colinear = TRUE
         return
      else
         sn = ONE/sn
      end if

      ! I checked the formula below myself
      ! Derivtive wrt c: see (6b)
      ! NOTE: this equation is actually -Nabla1
      call to_cross_product_of_(rx,rbc,rba)
      call to_cross_product_of_(ry,rba,rx)
      deriv(1:3) = ry*dba

      ! Derivative wrt a: see (6a)
      ! NOTE: this equation is actually Nabla3
      call to_cross_product_of_(rx,rba,rbc)
      call to_cross_product_of_(ry,rbc,rx)
      deriv(7:9) = ry*dbc

      ! Derivative wrt b: use translational invariance
      deriv(4:6) = -deriv(1:3)-deriv(7:9)

      ! Divide by sin: see (6c)
      deriv = -sn*deriv
   end subroutine

   subroutine torsion_angle_deriv(self,a,b,c,d,deriv,abc_colinear,bcd_colinear)
   ! Return the torsion angle derivative "deriv" between atoms "a",
   ! "b", "c" and "d". NOTE: If the result is -ONE, either a--b--c or
   ! b--c--d are colinear, and the variables "abc_colinear" and
   ! "bcd_colinear" are set.  See Tuzun et al (2000) J. Comp. Chem 21
   ! p. 553-561 equation (9).
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b,c,d
      VEC(REAL,12), OUT :: deriv
      BIN, optional, OUT :: abc_colinear,bcd_colinear
      VEC(REAL,3) :: rba,rcb,rdc,tba,tdc
      REAL :: dba,dcb,ddc,sin_abc,sin_bcd,cos_abc,cos_bcd,cc,bb
      if (present(abc_colinear)) abc_colinear = FALSE
      if (present(bcd_colinear)) bcd_colinear = FALSE

      ! Vectors point from d -> c -> b -> a
      rba = self(a)%position-self(b)%position
      rcb = self(b)%position-self(c)%position
      rdc = self(c)%position-self(d)%position

      ! Lengths and normalize
      dba = norm_(rba); rba = rba/dba
      dcb = norm_(rcb); rcb = rcb/dcb
      ddc = norm_(rdc); rdc = rdc/ddc

      ! Normals to planes
      call to_cross_product_of_(tba,rba,rcb)
      call to_cross_product_of_(tdc,rdc,rcb)

      ! Collinear test
      if (abs(norm_(tba))<TOL(5)) then
         deriv = ZERO
         if (present(abc_colinear)) abc_colinear = TRUE
         return
      end if
      if (abs(norm_(tdc))<TOL(5)) then
         deriv = ZERO
         if (present(bcd_colinear)) bcd_colinear = TRUE
         return
      end if

      ! sin's and cos's
      sin_abc = ONE/norm_(tba)
      sin_bcd = ONE/norm_(tdc)
      cos_abc = -dot_product(rba,rcb)
      cos_bcd = -dot_product(rcb,rdc)

      ! Gradient's
      deriv(1:3) = -tba*sin_abc*sin_abc/dba  ! wrt a: see (9a)
      deriv(10:12) = -tdc*sin_bcd*sin_bcd/ddc  ! wrt d: see (9b)
      cc = dba*cos_abc/dcb-ONE  ! wrt b: see (9d)
      bb = ddc*cos_bcd/dcb
      deriv(4:6) = cc*deriv(1:3)-bb*deriv(10:12)
      deriv(7:9) = -deriv(1:3)-deriv(4:6)-deriv(10:12)
   end subroutine


!  ========================
!  Size information methods
!  ========================

   PURE function no_of_electrons_0(self) result (res)
   ! If the atom list is *not* a single explicit "El" electron:
   ! . Return the number of electrons in the atom list assuming
   !   that it is *neutrally charged*. Any explicit "El" electrons
   !   are ignored i.e. they have no electrons.
   ! . Else return 1 ... the explicit "El" electron is assumed to be
   !   surrounded by one (beta) electron. This ensures a good initial
   !   guess is obtained for promolecule guess SCF calculations.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: res
   ENSURE(size(self)>0,"VEC{ATOM}:no_of_electrons_0 ... zero size atom list")
      res = sum(self(:)%atomic_number)
   end function

   PURE function no_of_electrons_1(self,list) result (res)
   ! If the atom list is *not* a single explicit "El" electron:
   ! . Return the number of electrons in the atom list assuming
   !   that it is *neutrally charged*. Any explicit "El" electrons
   !   are ignored i.e. they have no electrons.
   ! . Else return 1 ... the explicit "El" electron is assumed to be
   !   surrounded by one (beta) electron. This ensures a good initial
   !   guess is obtained for promolecule guess SCF calculations.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: list
      INT :: res
      INT :: i
   ENSURE(size(self)>0,"VEC{ATOM}:no_of_electrons_1 ... zero size atom list")
      res = 0
      do i = 1,size(list)
         res = res+self(list(i))%atomic_number
      end do
   end function

   PURE function no_of_occupied_NAOs(self,tol) result (res)
   ! Returns the number of non-zero occupied NAOs. For this purpose,
   ! zero is defined to be "tol" if present, or 1/14. See ROBY.
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL, optional, IN :: tol
      INT :: res
      INT :: a
   ENSURE(allocated(self(1)%NAO_occupations),"VEC{ATOM}:no_of_occupied_NAOs ... no ANO occupations")
      res = 0
      do a = 1,size(self)
         res = res+no_of_occupied_NAOs_(self(a),tol)
      end do
   end function

   PURE function no_of_occupied_ANOs(self,ANOkind,tol) result (res)
   ! Returns the number of non-zero occupied atomic natural orbitals. For this
   ! purpose, zero is defined to be "tol" if present, or TOL(7) otherwise
      VEC(type(ATOM_TYPE),:), IN :: self
      STR(len=*), optional, IN :: ANOkind
      REAL, optional, IN :: tol
      INT :: res
      INT :: a
   ENSURE(allocated(self(1)%NO_occ_nos),"VEC{ATOM}:no_of_occupied_ANOs ... no occupation numbers")
      res = 0
      do a = 1,size(self)
         res = res+no_of_occupied_NOs_(self(a),ANOkind,tol)
      end do
   end function

!  ===============================
!  Basis set existence information
!  ===============================

   pure function bases_are_resolved(self) result (res)
   ! Return TRUE if all basis sets are associated AND their shell list
   ! parts are also associated, dummy-atoms excepted
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      res = TRUE
      do a = 1,size(self)
         if (is_created_and_resolved_(self(a)%basis)) cycle
         if (has_a_dummy_label_(self(a))) cycle
         res = FALSE
         exit
      end do
   end function

   pure function slaterbases_are_resolved(self) result (res)
   ! Return TRUE if all basis sets are associated AND their shell list
   ! parts are also associated, dummy-atoms excepted
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      res = TRUE
      do a = 1,size(self)
         if (is_created_and_resolved_(self(a)%slaterbasis)) cycle
         if (has_a_dummy_label_(self(a))) cycle
         res = FALSE
         exit
      end do
   end function

   pure function coppensbases_are_resolved(self) result (res)
   ! Return TRUE if all basis sets are associated AND their shell list
   ! parts are also associated, dummy-atoms excepted
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      res = TRUE
      do a = 1,size(self)
         if (is_created_and_resolved_(self(a)%coppensbasis)) cycle
         if (has_a_dummy_label_(self(a))) cycle
         res = FALSE
         exit
      end do
   end function

   pure function bases_are_all_labeled(self) result (res)
   ! Return TRUE if all basis set labels exist (i.e. are not blank)
   ! Note: If a dummy atom has a basis it should have a label
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      res = TRUE
      do a = 1,size(self)
         if (is_created_and_has_label_(self(a)%basis)) cycle
         if (has_a_dummy_label_(self(a))) cycle
         res = FALSE
         exit
      end do
   end function

   pure function slaterbases_are_all_labeled(self) result (res)
   ! Return TRUE if all basis set labels exist (i.e. are not blank)
   ! Note: If a dummy atom has a basis it should have a label
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      res = TRUE
      do a = 1,size(self)
         if (is_created_and_has_label_(self(a)%slaterbasis)) cycle
         if (has_a_dummy_label_(self(a))) cycle
         res = FALSE
         exit
      end do
   end function

   pure function coppensbases_are_all_labeled(self) result (res)
   ! Return TRUE if all basis set labels exist (i.e. are not blank)
   ! Note: If a dummy atom has a basis it should have a label
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN :: res
      INT :: a

      ! The following code is inherited from VEC{ATOM}
      res = TRUE
      do a = 1,size(self)
         if (is_created_and_has_label_(self(a)%coppensbasis)) cycle
         if (has_a_dummy_label_(self(a))) cycle
         res = FALSE
         exit
      end do
   end function

   PURE subroutine get_distance_to_0(self,atomvec,distance,t1,t2)
   ! Calculates the shortest distance between an atom in self and one in
   ! atomvec.  Will set the distance to zero if calculated to less than 10^-6.
   ! If present, t1 and t2 are the indices of the two closest atoms.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atomvec
      REAL, OUT :: distance
      INT, optional, OUT :: t1,t2
      REAL :: dist
      INT :: i,j,dim1,dim2
      VEC(REAL,3) :: difference
   ENSURE(present(t1) EQV present(t2),"VEC{ATOM}:get_distance_to_0 ... need 0 or 2 optional arguments")
      dim1 = size(self)
      dim2 = size(atomvec)

      ! Do the first pair explicitly to set a starting distance.
      ! We also work with distance^2 until the end - saves computation.
      difference = self(1)%position(:)-atomvec(1)%position(:)
      distance = dot_product(difference,difference)
      do i = 1,dim1
         do j = 1,dim2
            difference = self(i)%position(:)-atomvec(j)%position(:)
            dist = dot_product(difference,difference)
            if (dist<TOL(6)) dist = ZERO
            if (dist<distance) then
               distance = dist
               if (present(t1)) then
                  t1 = i
                  t2 = j
               end if
            end if
         end do
      end do
      distance = sqrt(distance)
   end subroutine

   PURE subroutine get_distance_to_1(self,pos,distance,t1)
   ! Calculates the shortest distance of "pos" to an atom in self.
   ! If present, t1 is the index of the closest atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3), IN :: pos
      REAL, OUT :: distance
      INT, optional, OUT :: t1
      REAL :: dist
      VEC(REAL,3) :: difference
      INT :: i

      ! Do the first pair explicitly to set a starting distance.
      ! We also work with distance^2 until the end - saves computation.
      difference = self(1)%position(:)-pos(:)
      distance = dot_product(difference,difference)
      do i = 1,size(self)
         difference = self(i)%position(:)-pos(:)
         dist = dot_product(difference,difference)
         if (dist<TOL(6)) dist = ZERO
         if (dist<distance) then
            distance = dist
            if (present(t1)) t1 = i
         end if
      end do
      distance = sqrt(distance)
   end subroutine

   PURE function same_as(self,atomvec) result (res)
   ! Returns true if the two atomvecs contain the same atoms, though maybe in a
   ! different order.  Checks atomic number and position of each atom, but not
   ! the basis sets.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atomvec
      BIN :: res
      VEC(BIN,size(self)) :: matched
      BIN :: match_pos,match_kind,match
      INT :: n,q,dim
      res = FALSE
      dim = size(self)
      if (dim/=size(atomvec)) return  ! different number of atoms in each.
      matched = FALSE
      do n = 1,dim
         match = FALSE
         do q = 1,dim
            match_kind = self(n)%atomic_number==atomvec(q)%atomic_number
            match_pos = same_as_(self(n)%position,atomvec(q)%position,TOL(3))
            if (match_pos AND match_kind AND (NOT matched(q))) then
               matched(q) = TRUE
               match = TRUE
               exit
            end if
         end do
         if (NOT match) return  ! atom n doesn't have a match.
      end do
      do q = 1,dim  ! If not all of q are matched then atomvecs not same.
         if (NOT matched(q)) return
      end do
      res = TRUE
   end function


!  =============
!  Crystal stuff
!  =============

   PURE subroutine translate_by(self,v)
   ! Translate self by vector.
      VEC(type(ATOM_TYPE),:), INOUT :: self
      VEC(REAL,3), IN :: v
      VEC(REAL,3) :: pos
      INT :: a
      if (is_zero_(v)) return
      do a = 1,size(self)
         pos = self(a)%position+v
         call set_position_(self(a),pos)
      end do
   end subroutine

   PURE subroutine rotate_by(self,matrix)
   ! Rotate the atom positions by the rotation matrix
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,3,3), IN :: matrix
      call rotate_positions_by_(self,matrix)
      call rotate_ADP_tensors_by_(self,matrix)
   end subroutine

   PURE subroutine rotate_positions_by(self,matrix)
   ! Rotate the atom positions by the rotation matrix
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,3,3), IN :: matrix
      INT :: a
      VEC(REAL,3) :: pos
      do a = 1,size(self)
         call to_product_of_(pos,matrix,self(a)%position)
         call set_position_(self(a),pos)
      end do
   end subroutine

   PURE subroutine rotate_ADP_tensors_by(self,matrix)
   ! Rotate the atom ADP (thermal) tensors by the rotation matrix:
   ! this is the rotation matrix which transforms the cartesian
   ! position to a new position.
   ! WARNING: does not do ADP3 and ADP4 yet
      VEC(type(ATOM_TYPE),:), INOUT :: self
      MAT(REAL,3,3), IN :: matrix
      INT :: a
      MAT(REAL,3,3) :: ADP
      do a = 1,size(self)
         call back_transform_to_(self(a)%ADP_tensor,ADP,matrix)
         call set_ADP_tensor_(self(a),ADP)
      end do
   end subroutine

   subroutine change_ADP_axis_system_to(self,axis_system,cell)
   ! Convert all ADP tensors into the desired "axis_system".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      STR(len=*), IN :: axis_system
      type(UNIT_CELL_TYPE), IN :: cell
      INT :: a
      do a = 1,size(self)
         call change_ADP_axis_system_to_(self(a),axis_system,cell)
      end do
   end subroutine

   function default_multiplicity(self) result (res)
   ! Return the default multiplicity
      VEC(type(ATOM_TYPE),:), IN :: self
      REAL :: res
      if (size(self)==1) then
         res = ground_state_multiplicity_(self(1))
      else
         res = mod(no_of_electrons_(self),2)+1
      end if
   end function

   PURE subroutine translate_group(self,g,v)
   ! Translate group "g" by vector "v".
      VEC(type(ATOM_TYPE),:), INOUT :: self
      INT, IN :: g
      VEC(REAL,3), IN :: v
      VEC(REAL,3) :: pos
      INT :: a
      if (is_zero_(v)) return
      if (all(self(:)%group/=g)) return
      do a = 1,size(self)
         if (self(a)%group/=g) cycle
         pos = self(a)%position+v
         call set_position_(self(a),pos)
      end do
   end subroutine


!  ====
!  Dump
!  ====

   subroutine dump(self,object_name)
   ! Dump a vector object
      VEC(type(ATOM_TYPE),:), IN :: self
      STR(len=*), IN :: object_name
      STR(len=STR_SIZE) :: val
      INT :: i

      ! The following code is inherited from VEC{OBJECT}
      val = trim(object_name)//":VEC{ATOM} = {"
      call text_(stdout,trim(val))
      call increment_margin_width_(stdout,1)
      call put_(stdout,"dimension =",width=11)
      call put_(stdout,size(self),width=0)
      call flush_(stdout)
      do i = 1,size(self)
         val = trim(object_name)//"("//trim(to_str_(i))//")"
         call dump_(self(i),trim(val))
      end do
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump an allocatable vector object
      VEC(type(ATOM_TYPE),:), allocatable, IN :: self
      STR(len=*), IN :: object_name
      STR(len=STR_SIZE) :: val
      INT :: i

      ! The following code is inherited from VEC{OBJECT}
      call flush_(stdout)
      if (NOT allocated(self)) then
         val = trim(object_name)//":VEC{ATOM}* = null"
         call text_(stdout,trim(val))
      else
         val = trim(object_name)//":VEC{ATOM}* = {"
         call text_(stdout,trim(val))
         call increment_margin_width_(stdout,1)
         call put_(stdout,"dimension =",width=11)
         call put_(stdout,size(self),width=0)
         call flush_(stdout)
         do i = 1,size(self)
            val = trim(object_name)//"("//trim(to_str_(i))//")"
            call dump_(self(i),trim(val))
         end do
         call increment_margin_width_(stdout,-1)
         call text_(stdout,"}")
      end if
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Output atom information, without full basis set info
      VEC(type(ATOM_TYPE),:), IN :: self
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Atom list information")
      call text_(stdout,"=====================")
      call flush_(stdout)
      call show_(stdout,"Chemical Formula       =",trim(chemical_formula_(self,with_spaces=TRUE)))
      call show_(stdout,"No of atoms            =",size(self))
      call show_(stdout,"No of electrons        =",no_of_electrons_(self))
      call put_coordinate_info_(self)

   ! if (.has_restraints)          .put_restraint_atoms
   end subroutine

   subroutine put_list(self)
   ! Output atom information,
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: a
      STR(len=STR_SIZE) :: title
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Atom list information")
      call text_(stdout,"=====================")
      call flush_(stdout)
      call show_(stdout,"Chemical Formula       =",trim(chemical_formula_(self,with_spaces=TRUE)))
      call show_(stdout,"No of atoms            =",size(self))
      call show_(stdout,"No of electrons        =",no_of_electrons_(self))
      call flush_(stdout)
      do a = 1,size(self)
         title = "Atom "//trim(to_str_(a))
         call text_(stdout,trim(title))
         call text_(stdout,repeat("=",len_trim(title)))
         call put_(self(a))
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_coordinate_info(self)
   ! Output atom coordinate information, including bond lengths, angles
      VEC(type(ATOM_TYPE),:), IN :: self
      call put_coordinates_(self)
      if (has_core_electrons_(self)) call put_core_electrons_(self)
      if (has_nonzero_ADP_tensors_(self)) call put_ADPs_(self)
      if (xray_dispersion_entered_()) call put_dispersion_factors_(self)
      call put_bond_length_table_(self)
      call put_bond_angle_table_(self)
      call put_torsion_angle_table_(self)
   end subroutine


!  ======================
!  Coordinate information
!  ======================

   subroutine put_coordinates(self,no_header)
   ! Output the atom coordinates information.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: no_header
      if (has_disorder_(self)) then; call put_coords_disorder_y_(self,no_header)
      else if (has_groups_(self)) then; call put_coords_groups_y_(self,no_header)
      else; call put_coords_(self,no_header)
      end if
   end subroutine

   subroutine put_coords(self,no_header)
   ! Output the atom coordinates information without disorder
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: no_header
      REAL :: fac
      MAT(REAL,:,:), allocatable :: xyz,err
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: la,lb,ka,kb
      BIN :: cartesian,header,has_unique_labels
      header = TRUE
      if (present(no_header)) header = NOT no_header

      ! Axis system
      cartesian = self(1)%pos_axis_system=="cartesian"

      ! Use angstroms
      if (cartesian) then
         fac = conversion_factor_("angstrom")
      else
         fac = ONE
      end if

      ! Table column data
      call create_(xyz,size(self),3)
      call create_(err,size(self),3)

      ! Get table column data
      call put_pos_vector_to_(self,xyz,fac)
      call put_pos_errors_to_(self,err,fac)

      ! Title
      if (header) then
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"================")
         call text_(stdout,"Atom coordinates")
         call text_(stdout,"================")
      end if

      ! Has unique labels?
      call get_1st_repeated_labels_(self,la,lb)
      call get_1st_repeated_xtal_kinds_(self,ka,kb)
      has_unique_labels = (la==0 AND ka==0) OR size(self)==1

      ! Unique labels?
      if (has_unique_labels) then

         ! Unique labels
         if (header) then
            if (NOT cartesian) then
               call flush_(stdout)
               call text_(stdout,". The coordinates are referred to cell axes")
            end if
            call flush_(stdout)
            call show_(stdout,"No. of atoms =",size(self))
            call flush_(stdout)
         end if

         ! Table headings
         call create_(table,5)
         call set_heading_(table(1),"ID")
         call set_heading_(table(2),"Z")
         call set_subheading_(table(2),"/au")
         call set_real_precision_(table(2),1)
         call set_heading_(table(3),"- x -")
         call set_heading_(table(4),"- y -")
         call set_heading_(table(5),"- z -")
         if (cartesian) then
            call set_subheading_(table(4),"/A")
         end if

         ! Set table column data
         call set_values_(table(1),self(:)%label)
         call set_real_precision_(table(2),1)
         call set_values_(table(2),self(:)%nuclear_charge)
         if (NOT has_nonzero_pADP2_errors_(self)) then
            call set_values_(table(3),xyz(:,1))
            call set_values_(table(4),xyz(:,2))
            call set_values_(table(5),xyz(:,3))
         else
            call set_values_and_errors_(table(3),xyz(:,1),err(:,1))
            call set_values_and_errors_(table(4),xyz(:,2),err(:,2))
            call set_values_and_errors_(table(5),xyz(:,3),err(:,3))
         end if
      else

         ! Not unique labels
         if (header) then
            if (NOT cartesian) then
               call flush_(stdout)
               call text_(stdout,". The coordinates are referred to cell axes")
            end if
            call flush_(stdout)
            call text_(stdout,". Unique atom labels were not set for each atom.")
            call text_(stdout,"  Therefore, unique ID tags are provided.")
            if (la/=0) then
               call flush_(stdout)
               call text_(stdout,". First repeated labels are for atoms "//trim(to_str_(la))//" and "//trim(to_str_(lb))//".")
            end if
            if (ka/=0) then
               call flush_(stdout)
               call text_(stdout,". First repeated xtal kinds are for atoms "//trim(to_str_(ka))//" and "//trim(to_str_(kb))//".")
            end if
            call flush_(stdout)
            call show_(stdout,"No. of atoms =",size(self))
            call flush_(stdout)
         end if

         ! Table headings
         call create_(table,6)
         call set_heading_(table(1),"Label")
         call set_heading_(table(2),"ID")
         call set_subhead_(table(2),"tag")
         call set_real_precision_(table(3),1)
         call set_heading_(table(3),"Z")
         call set_subheading_(table(3),"/au")
         call set_real_precision_(table(3),1)
         call set_heading_(table(4),"- x -")
         call set_heading_(table(5),"- y -")
         call set_heading_(table(6),"- z -")
         if (cartesian) then
            call set_subheading_(table(5),"/A")
         end if

         ! Set table column data
         call set_values_(table(1),self(:)%label)
         call set_values_(table(2),self(:)%tag)
         call set_values_(table(3),self(:)%nuclear_charge)
         if (NOT has_nonzero_pADP2_errors_(self)) then
            call set_values_(table(4),xyz(:,1))
            call set_values_(table(5),xyz(:,2))
            call set_values_(table(6),xyz(:,3))
         else
            call set_values_and_errors_(table(4),xyz(:,1),err(:,1))
            call set_values_and_errors_(table(5),xyz(:,2),err(:,2))
            call set_values_and_errors_(table(6),xyz(:,3),err(:,3))
         end if
      end if

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(err)
      call destroy_(xyz)
   end subroutine

   subroutine put_coords_disorder_y(self,no_header)
   ! Output the atom coordinates information with disorder
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: no_header
      REAL :: fac
      MAT(REAL,:,:), allocatable :: xyz
      VEC(REAL,:), allocatable :: Z,occ
      VEC(STR(len=STR_SIZE),:), allocatable :: lab,ID
      VEC(INT,:), allocatable :: grp
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: la,lb,ka,kb
      BIN :: cartesian,header,has_unique_labels
      header = TRUE
      if (present(no_header)) header = NOT no_header

      ! Axis system
      cartesian = self(1)%pos_axis_system=="cartesian"

      ! Use angstroms
      if (cartesian) then
         fac = conversion_factor_("angstrom")
      else
         fac = ONE
      end if

      ! Title
      if (header) then
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"================")
         call text_(stdout,"Atom coordinates")
         call text_(stdout,"================")
      end if

      ! Table column data
      call create_(lab,size(self))
      call create_(ID,size(self))
      call create_(Z,size(self))
      call create_(grp,size(self))
      call create_(occ,size(self))
      call create_(xyz,size(self),3)

      ! Get table column data
      call put_coordinates_to_(self,xyz,positions_as_rows=TRUE)
      xyz = xyz*fac

      ! Has unique labels?
      call get_1st_repeated_labels_(self,la,lb)
      call get_1st_repeated_xtal_kinds_(self,ka,kb)
      has_unique_labels = (la==0 AND ka==0) OR size(self)==1

      ! Unique labels?
      if (has_unique_labels) then
         if (header) then
            if (NOT cartesian) then
               call flush_(stdout)
               call text_(stdout,". The coordinates are referred to cell axes")
            end if
            call flush_(stdout)
            call show_(stdout,"No. of atoms =",size(self))
            call flush_(stdout)
         end if

         ! Table headings
         call create_(table,7)
         call set_heading_(table(1),"ID")
         call set_heading_(table(2),"Z")
         call set_subhead_(table(2),"/au")
         call set_real_precision_(table(2),1)
         call set_heading_(table(3),"Group")
         call set_subhead_(table(3),"#")
         call set_heading_(table(4),"Site")
         call set_subhead_(table(4),"occupancy")
         call set_heading_(table(5),"- x -")
         call set_heading_(table(6),"- y -")
         call set_heading_(table(7),"- z -")
         if (cartesian) then
            call set_subhead_(table(6),"/A")
         end if

         ! Set table column data
         call set_values_(table(1),self(:)%label)
         call set_real_precision_(table(2),1)
         call set_values_(table(2),self(:)%nuclear_charge)
         call set_values_(table(3),self(:)%site_disorder_group)
         call set_values_(table(4),self(:)%site_occupancy)
         call set_values_(table(5),xyz(:,1))
         call set_values_(table(6),xyz(:,2))
         call set_values_(table(7),xyz(:,3))
      else  ! Not unique labels
         if (header) then
            if (NOT cartesian) then
               call flush_(stdout)
               call text_(stdout,". The coordinates are referred to cell axes")
            end if
            call flush_(stdout)
            call text_(stdout,". Unique atom labels were not set for each atom.")
            call text_(stdout,"  Therefore, unique ID tags are provided.")
            if (la/=0) then
               call flush_(stdout)
               call text_(stdout,". First repeated labels are for atoms "//trim(to_str_(la))//" and "//trim(to_str_(lb))//".")
            end if
            if (ka/=0) then
               call flush_(stdout)
               call text_(stdout,". First repeated xtal kinds are for atoms "//trim(to_str_(ka))//" and "//trim(to_str_(kb))//".")
            end if
            call flush_(stdout)
            call show_(stdout,"No. of atoms =",size(self))
            call flush_(stdout)
         end if

         ! Table headings
         call create_(table,8)
         call set_heading_(table(1),"Label")
         call set_heading_(table(2),"ID")
         call set_subhead_(table(2),"tag")
         call set_heading_(table(3),"Z")
         call set_subhead_(table(3),"/au")
         call set_real_precision_(table(4),1)
         call set_heading_(table(4),"Group")
         call set_subhead_(table(4),"#")
         call set_heading_(table(5),"Site")
         call set_subhead_(table(5),"occupancy")
         call set_heading_(table(6),"- x -")
         call set_heading_(table(7),"- y -")
         call set_heading_(table(8),"- z -")
         if (cartesian) then
            call set_subhead_(table(7),"/A")
         end if

         ! Set table column data
         call set_values_(table(1),self(:)%label)
         call set_values_(table(2),self(:)%tag)
         call set_real_precision_(table(3),1)
         call set_values_(table(3),self(:)%nuclear_charge)
         call set_values_(table(4),self(:)%site_disorder_group)
         call set_values_(table(5),self(:)%site_occupancy)
         call set_values_(table(6),xyz(:,1))
         call set_values_(table(7),xyz(:,2))
         call set_values_(table(8),xyz(:,3))
      end if

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(xyz)
   end subroutine

   subroutine put_coords_groups_y(self,no_header)
   ! Output the atom coordinates with groups (no disorder)
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: no_header
      REAL :: fac
      MAT(REAL,:,:), allocatable :: xyz
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: la,lb,ka,kb
      BIN :: cartesian,header,has_unique_labels
      header = TRUE
      if (present(no_header)) header = NOT no_header

      ! Axis system
      cartesian = self(1)%pos_axis_system=="cartesian"

      ! Use angstroms
      if (cartesian) then
         fac = conversion_factor_("angstrom")
      else
         fac = ONE
      end if

      ! Title
      if (header) then
         call flush_(stdout)
         call flush_(stdout)
         call text_(stdout,"================")
         call text_(stdout,"Atom coordinates")
         call text_(stdout,"================")
      end if

      ! Table column data
      call create_(xyz,size(self),3)

      ! Get table column data
      call put_coordinates_to_(self,xyz,positions_as_rows=TRUE)
      xyz = xyz*fac

      ! Has unique labels?
      call get_1st_repeated_labels_(self,la,lb)
      call get_1st_repeated_xtal_kinds_(self,ka,kb)
      has_unique_labels = (la==0 AND ka==0) OR size(self)==1

      ! Unique labels?
      if (has_unique_labels) then
         if (header) then
            if (NOT cartesian) then
               call flush_(stdout)
               call text_(stdout,". The coordinates are referred to cell axes")
            end if
            call flush_(stdout)
            call text_(stdout,". This molecule has non trivial group indices, assigned")
            call text_(stdout,"  explicitly or by CCDC connectivity criteria")
            call flush_(stdout)
            call show_(stdout,"No. of atoms =",size(self))
            call flush_(stdout)
         end if

         ! Table headings
         call create_(table,6)
         call set_heading_(table(1),"ID"); call set_values_(table(1),self(:)%label)
         call set_real_precision_(table(2),1)
         call set_heading_(table(2),"Z"); call set_values_(table(2),self(:)%nuclear_charge)
         call set_subhead_(table(2),"/au")
         call set_heading_(table(3),"Grp"); call set_values_(table(3),self(:)%group)
         call set_subhead_(table(3),"#")
         call set_heading_(table(4),"- x -"); call set_values_(table(4),xyz(:,1))
         call set_heading_(table(5),"- y -"); call set_values_(table(5),xyz(:,2))
         call set_heading_(table(6),"- z -"); call set_values_(table(6),xyz(:,3))
         if (cartesian) then
            call set_subhead_(table(5),"/A")
         end if
      else  ! Not unique labels
         if (header) then
            if (NOT cartesian) then
               call flush_(stdout)
               call text_(stdout,". The coordinates are referred to cell axes")
            end if
            call flush_(stdout)
            call text_(stdout,". This molecule has non trivial group indices, assigned")
            call text_(stdout,"  explicity, or by CCDC connectivity criteria")
            call flush_(stdout)
            call text_(stdout,". Unique atom labels were not set for each atom.")
            call text_(stdout,"  Therefore, unique ID tags are provided.")
            if (la/=0) then
               call flush_(stdout)
               call text_(stdout,". First repeated labels are for atoms "//trim(to_str_(la))//" and "//trim(to_str_(lb))//".")
            end if
            if (ka/=0) then
               call flush_(stdout)
               call text_(stdout,". First repeated xtal kinds are for atoms "//trim(to_str_(ka))//" and "//trim(to_str_(kb))//".")
            end if
            call flush_(stdout)
            call show_(stdout,"No. of atoms =",size(self))
            call flush_(stdout)
         end if

         ! Table headings
         call create_(table,7)
         call set_heading_(table(1),"Label"); call set_values_(table(1),self(:)%label)
         call set_heading_(table(2),"ID"); call set_values_(table(2),self(:)%tag)
         call set_subhead_(table(2),"tag")
         call set_real_precision_(table(3),1)
         call set_heading_(table(3),"Z"); call set_values_(table(3),self(:)%nuclear_charge)
         call set_subhead_(table(3),"/au")
         call set_heading_(table(4),"Group"); call set_values_(table(4),self(:)%group)
         call set_subhead_(table(4),"#")
         call set_heading_(table(5),"- x -"); call set_values_(table(5),xyz(:,1))
         call set_heading_(table(6),"- y -"); call set_values_(table(6),xyz(:,2))
         call set_heading_(table(7),"- z -"); call set_values_(table(7),xyz(:,3))
         if (cartesian) then
            call set_subhead_(table(6),"/A")
         end if

         ! Set table column data
      end if

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(xyz)
   end subroutine

   subroutine put_xyz_file(self,name,units)
   ! Output the coordinates in XYZ file format with "name" header
      VEC(type(ATOM_TYPE),:), IN :: self
      STR(len=*), optional, IN :: name
      STR(len=*), optional, IN :: units
      INT :: n,a
      STR(len=STR_SIZE) :: filename,u
      VEC(REAL,3) :: pos

      ! Filename
      filename = "geometry"
      if (present(name)) filename = name

      ! Units
      u = "angstroms"
      if (present(units)) u = units

      ! Redirect
      call redirect_(stdout,trim(filename)//".xyz")

      ! No. pf atoms
      n = size(self)
      call put_(stdout,trim(to_str_(n)),width=3)
      call flush_(stdout)

      ! Header
      call put_(stdout,trim(name),width=len_trim(name))
      call flush_(stdout)

      ! Labels & coordinates
      do a = 1,n
         call put_(stdout,trim(chemical_symbol_(self(a))),width=3)
         pos = self(a)%position
         call put_(stdout,to_units_(pos(1),u),width=10,precision=5)
         call put_(stdout,to_units_(pos(2),u),width=10,precision=5)
         call put_(stdout,to_units_(pos(3),u),width=10,precision=5)
         call flush_(stdout)
      end do

      ! Revert
      call revert_(stdout)
   end subroutine

   subroutine put_core_electrons(self)
   ! Output the no. of core electrons (if any)
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(INT,:), allocatable :: valence
      if (NOT has_core_electrons_(self)) return

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=====================================")
      call text_(stdout,"Atom effective core potentials (ECPs)")
      call text_(stdout,"=====================================")
      call flush_(stdout)
      call text_(stdout,". Some atoms have effective core potentials (ECPs)")
      call text_(stdout,". The number of core electrons replaced is shown below")
      call flush_(stdout)

      ! Table headings
      call create_(table,4)
      call set_heading_(table(1),"tag")
      call set_heading_(table(2),"Z")
      call set_subheading_(table(2),"/au")
      call set_heading_(table(3),"# of core")
      call set_subhead_(table(3),"electrons")
      call set_heading_(table(4),"# valence")
      call set_subhead_(table(4),"electrons")

      ! Data
      valence = nint(self(:)%nuclear_charge)-self(:)%n_core_electrons

      ! Set table column data
      call set_values_(table(1),self(:)%tag)
      call set_real_precision_(table(2),1)
      call set_values_(table(2),self(:)%nuclear_charge)
      call set_values_(table(3),self(:)%n_core_electrons)
      call set_values_(table(4),valence)

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_bases(self)
   ! Output the atom bases
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: lab
      INT :: i
      if (NOT bases_are_resolved_(self)) return

      ! Get table column data
      call create_(lab,size(self))
      do i = 1,size(self)
         lab(i) = self(i)%basis%label
      end do

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"==========")
      call text_(stdout,"Atom bases")
      call text_(stdout,"==========")
      call flush_(stdout)
      call show_(stdout,"No. of atoms =",size(self))
      call flush_(stdout)

      ! Table headings
      call create_(table,3)
      call set_heading_(table(1),"ID")
      call set_heading_(table(2),"Z")
      call set_subheading_(table(2),"/au")
      call set_real_precision_(table(2),1)
      call set_heading_(table(3),"Basis")
      call set_subhead_(table(3),"label")

      ! Set table column data
      call set_values_(table(1),self(:)%tag)
      call set_values_(table(2),self(:)%nuclear_charge)
      call set_values_(table(3),lab)

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(lab)
   end subroutine

   subroutine put_g09_input(self,name,units)
   ! Output the coordinates in XYZ file format with "name" header
      VEC(type(ATOM_TYPE),:), IN :: self
      STR(len=*), optional, IN :: name
      STR(len=*), optional, IN :: units
      INT :: n,a
      STR(len=STR_SIZE) :: filename,u
      VEC(REAL,3) :: pos

      ! Filename
      filename = "input"
      if (present(name)) filename = name
      u = "angstroms"
      if (present(units)) u = units

      ! Redirect
      call redirect_(stdout,trim(filename)//".gjf")

      ! No. pf atoms
      n = size(self)
      call text_(stdout,"#P B3LYP/6-31G(d,p) 6d 10f NoSymm FChk")
      call flush_(stdout)
      ! Header
      call text_(stdout,trim(name))
      call flush_(stdout)

      ! Labels & coordinates
      call text_(stdout,"0 1")
      do a = 1,n
         call put_(stdout,trim(chemical_symbol_(self(a))),width=3)
         pos = self(a)%position
         call put_(stdout,to_units_(pos(1),u),width=10,precision=5)
         call put_(stdout,to_units_(pos(2),u),width=10,precision=5)
         call put_(stdout,to_units_(pos(3),u),width=10,precision=5)
         call flush_(stdout)
      end do
      call flush_(stdout)
      ! Revert
      call revert_(stdout)
   end subroutine

   subroutine put_pdb(self,pdbfile)
   ! Output the atoms in pdb file format to "pdbfile" (blame Birger)
      VEC(type(ATOM_TYPE),:), IN :: self
      type(TEXTFILE_TYPE), allocatable, INOUT :: pdbfile
      INT :: i,in,ic,ip
      INT :: len_code
      INT :: len_pnum
      INT :: element_len
      STR(len=STR_SIZE) :: label
      STR(len=STR_SIZE) :: element_name
      STR(len=STR_SIZE) :: residue_number
      STR(len=STR_SIZE) :: code
      STR(len=STR_SIZE) :: three_letter_code
      STR(len=STR_SIZE) :: position_symbol
      STR(len=STR_SIZE) :: position_number

      ! Save PDB settings for later
      call save_style_(pdbfile)

      ! Loop over atoms
      do i = 1,size(self)

         ! The ATOM number
         call put_(pdbfile,"ATOM",width=4)
         call put_(pdbfile,i,width=7)

         ! Start to analyze the atom label
         label = self(i)%label
      ! print *,"label =",trim(label)

         ! Get start of atom index, hence the element name
         call replace_all_(label,"(",'')
         call replace_all_(label,")",'')
         in = index_of_digit_(label)
         DIE_IF(in==0,"VEC{ATOM}:put_pdb ... no atom index in atom label: "//trim(label))
         ! Get rid of left bracket
         element_name = label(1:in-1)
      ! print *,"in =",in
      ! print *,"element_name =",trim(element_name)

         ! Get start of residue code, hence the residue number
         ic = index_of_nondigit_(label(in:))
         DIE_IF(ic==0,"VEC{ATOM}:put_pdb ... no residue code in atom label: "//trim(label))
         ic = ic+in-1
         residue_number = label(in:ic-1)
      ! print *,"ic =",ic
      ! print *,"residue_number =",trim(residue_number)

         ! Get start of position symbol, hence residue code, position
         ! symbol and position number (could be blank)
         ! position symbols are alpha (a)
         ! position symbols are beta (b)
         ! position symbols are gamma (c,g)
         ! position symbols are delta (d)
         ! position symbols are epsilon (e)
         ! position symbols are nu (n)
         ! position symbols are terminal (t)
         ! position symbols are zeta (z)
         ! position symbols are peptide bond (')
         ip = index_of_character_in_(label(ic:),"'abcgdentz")
         if (ip/=0) then
            ip = ip+ic-1
            code = label(ic:ip-1)
            position_symbol = label(ip:ip)
            position_number = label(ip+1:)
         else
            code = label(ic:)
            position_symbol = " "
            position_number = " "
         end if
      ! print *,"ip =",ip
      ! print *,"code =",trim(code)
      ! print *,"position_symbol =",trim(position_symbol)
      ! print *,"position_number =",trim(position_number)

         ! Do some checks ...
         len_code = len_trim(code)
         DIE_IF(NOT (len_code==1 OR len_code==3),"VEC{ATOM}:put_pdb ... residue code must be 1 or 3 characters: "//trim(label))
         len_pnum = len_trim(position_number)
         DIE_IF(len_pnum>1,"VEC{ATOM}:put_pdb ... position number code cannot exceed 1 character: "//trim(label))
         if (position_number/=" ") then
            DIE_IF(NOT is_included_in_(position_number,"123456789"),"VEC{ATOM}:put_pdb ... unknown position number: "//trim(label))
         end if

         ! If the code is a one letter code, convert it
         if (len_code==3) then
            select case (code)
            case ("UNK","AIB","IVA","WAT","EOH","TER","DLE","DVA","FOR","ETA")
            case ("ALA","ARG","GLN","GLY","VAL","LEU","ILE","SER","THR","CYS")
            case ("MET","PRO","HIS","ASN","GLU","ASP","PHE","TRP","TYR","LYS")
            case ("ACD","HSE","ACE","HYP","HYL","ALB","ALI","ABU","ARO","ORN")
            case ("ASX","PCA","BAS","SAR","BET","TAU","THY","GLX","HET")
            case default; DIE("VEC{ATOM}:put_pdb ... unknown three letter residue code: "//trim(code))
            end select
            three_letter_code = code
         else if (len_code==1) then
            select case (code)
            case ("X"); three_letter_code = "UNK"
            case ("~"); three_letter_code = "WAT"
            case ("_"); three_letter_code = "EOH"
            case ("B"); three_letter_code = "DLE"
            case ("O"); three_letter_code = "DVA"
            case ("U"); three_letter_code = "AIB"
            case ("A"); three_letter_code = "ALA"
            case ("R"); three_letter_code = "ARG"
            case ("Q"); three_letter_code = "GLN"
            case ("G"); three_letter_code = "GLY"
            case ("V"); three_letter_code = "VAL"
            case ("L"); three_letter_code = "LEU"
            case ("I"); three_letter_code = "ILE"
            case ("J"); three_letter_code = "IVA"
            case ("S"); three_letter_code = "SER"
            case ("T"); three_letter_code = "THR"
            case ("C"); three_letter_code = "CYS"
            case ("M"); three_letter_code = "MET"
            case ("P"); three_letter_code = "PRO"
            case ("H"); three_letter_code = "HIS"
            case ("N"); three_letter_code = "ASN"
            case ("E"); three_letter_code = "GLU"
            case ("D"); three_letter_code = "ASP"
            case ("F"); three_letter_code = "PHE"
            case ("W"); three_letter_code = "TRP"
            case ("Y"); three_letter_code = "TYR"
            case ("K"); three_letter_code = "LYS"
            case ("Z"); three_letter_code = "TER"
            case default; DIE("VEC{ATOM}:put_pdb ... unknown one letter residue code: "//trim(code))
            end select
         else
            DIE("VEC{ATOM}:put_pdb ... code must be one or three characters")
         end if

         ! Print the new atom label from one read in
         call tab_(pdbfile,width=1)
         if (position_number==" ") then
            call tab_(pdbfile,width=1)
         else
            call put_(pdbfile,position_number,width=1,left=TRUE)
         end if
         call put_(pdbfile,element_name,width=len_trim(element_name),left=TRUE)
         if (is_included_in_(position_symbol,"'abcgdentz")) then
            call to_upper_case_(position_symbol)
            call put_(pdbfile,position_symbol,width=1,left=TRUE)
            call tab_(pdbfile,width=1)
         else
            call tab_(pdbfile,width=2)
         end if
         element_len = len_trim(element_name)
         if (element_len==1) then
            call tab_(pdbfile,width=1)
         end if
         call put_(pdbfile,three_letter_code,width=3,left=TRUE)
         call tab_(pdbfile,width=2)
         call put_(pdbfile,residue_number,width=4)
         call tab_(pdbfile,width=4)
         call set_real_precision_(pdbfile,3)
         call set_real_width_(pdbfile,8)
         call put_(pdbfile,to_units_(self(i)%position(1),"angstrom"))
         call put_(pdbfile,to_units_(self(i)%position(2),"angstrom"))
         call put_(pdbfile,to_units_(self(i)%position(3),"angstrom"))
         call set_real_precision_(pdbfile,2)
         call set_real_width_(pdbfile,6)
         call put_(pdbfile,self(i)%site_occupancy)
         call put_(pdbfile,8*PI*PI*self(i)%U_iso)
         call put_(pdbfile,chemical_symbol_(self(i)),width=12)
         call flush_(pdbfile)

         ! Print Anisotropic U's if non-hydrogen
         if (self(i)%atomic_number==1) cycle
         call put_(pdbfile,"ANISOU",width=6)
         call put_(pdbfile,i,width=5)
         call tab_(pdbfile,width=1)
         if (position_number==" ") then
            call tab_(pdbfile,width=1)
         else
            call put_(pdbfile,position_number,width=1,left=TRUE)
         end if
         call put_(pdbfile,element_name,width=len_trim(element_name),left=TRUE)
         if (is_included_in_(position_symbol,"'ABCGDENTZ")) then
            call to_upper_case_(position_symbol)
            call put_(pdbfile,position_symbol,width=1,left=TRUE)
            call tab_(pdbfile,width=1)
         else
            call tab_(pdbfile,width=2)
         end if
         element_len = len_trim(element_name)
         if (element_len==1) then
            call tab_(pdbfile,width=1)
         end if
         call put_(pdbfile,three_letter_code,width=3,left=TRUE)
         call tab_(pdbfile,width=2)
         call put_(pdbfile,residue_number,width=4)
         call tab_(pdbfile,width=2)
         call set_int_width_(pdbfile,7)

         ! Order for the ADP's .......
         ! u11 u22 u33 u12 u13 u23 XD-format
         ! u11 u22 u33 u12 u23 u13 SHELX
         ! u11 u22 u33 u13 u23 u12 PDB-format
         call put_(pdbfile,nint(to_units_(self(i)%ADP_tensor(1,1),"angstrom^2")*10000))
         call put_(pdbfile,nint(to_units_(self(i)%ADP_tensor(2,2),"angstrom^2")*10000))
         call put_(pdbfile,nint(to_units_(self(i)%ADP_tensor(3,3),"angstrom^2")*10000))
         call put_(pdbfile,nint(to_units_(self(i)%ADP_tensor(1,3),"angstrom^2")*10000))
         call put_(pdbfile,nint(to_units_(self(i)%ADP_tensor(2,3),"angstrom^2")*10000))
         call put_(pdbfile,nint(to_units_(self(i)%ADP_tensor(1,2),"angstrom^2")*10000))

         ! Last thing on line, chemical symbol
         call put_(pdbfile,chemical_symbol_(self(i)),width=8)

         ! Flush the line
         call flush_(pdbfile)
      end do
      call unsave_style_(pdbfile)
   end subroutine

   subroutine put_coordinates_and_charges(self)
   ! Output the atom coordinate information
      VEC(type(ATOM_TYPE),:), IN :: self
      call put_coords_and_charges_(self)
      call put_coords_and_charges_(self,angstrom=TRUE)
   end subroutine

   subroutine put_coords_and_charges(self,angstrom)
   ! Output the atom coordinates and charges
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: angstrom
      INT :: i
      BIN :: in_angstrom
      in_angstrom = FALSE
      if (present(angstrom)) in_angstrom = angstrom
      call flush_(stdout)
      if (NOT in_angstrom) then
         call text_(stdout,"Atom list information:")
      else
         call text_(stdout,"Atom list information (in angstrom):")
      end if
      call dash_(stdout,int_fields=3,real_fields=4)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"ID",int_width=TRUE)
      call put_(stdout,"Z",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"charge")
      call flush_(stdout)
      call dash_(stdout,int_fields=3,real_fields=4)
      do i = 1,size(self)
         call put_(stdout,i)
         call put_(stdout,self(i)%label,int_width=TRUE)
         call put_(stdout,self(i)%nuclear_charge,int_width=TRUE)
         if (NOT in_angstrom) then
            call put_(stdout,self(i)%position(1))
            call put_(stdout,self(i)%position(2))
            call put_(stdout,self(i)%position(3))
         else
            call put_(stdout,to_units_(self(i)%position(1),"angstrom"))
            call put_(stdout,to_units_(self(i)%position(2),"angstrom"))
            call put_(stdout,to_units_(self(i)%position(3),"angstrom"))
         end if
         call put_(stdout,self(i)%charge)
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=3,real_fields=4)
   end subroutine


!  ====================
!  Groups & connections
!  ====================

   subroutine put_kinds_and_groups(self)
   ! Output the atom kinds and groups
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: la,lb,ka,kb
      BIN :: has_unique_labels

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Atom kinds and groups")
      call text_(stdout,"=====================")

      ! Has unique labels?
      call get_1st_repeated_labels_(self,la,lb)
      call get_1st_repeated_xtal_kinds_(self,ka,kb)
      has_unique_labels = (la==0 AND ka==0) OR size(self)==1

      ! Unique labels?
      if (has_unique_labels) then
         call flush_(stdout)
         call show_(stdout,"No. of atoms =",size(self))
         call flush_(stdout)

         ! Table headings
         call create_(table,6)
         call set_heading_(table(1),"ID")
         call set_heading_(table(2),"Z")
         call set_heading_(table(3),"Atom")
         call set_subhead_(table(3),"kind")
         call set_heading_(table(4),"Unique")
         call set_subhead_(table(4),"kind?")
         call set_heading_(table(5),"Molecule")
         call set_subhead_(table(5),"group")
         call set_heading_(table(6),"Disorder")
         call set_subhead_(table(6),"group")

         ! Set table column data
         call set_values_(table(1),self(:)%label)
         call set_values_(table(2),self(:)%atomic_number)
         call set_values_(table(3),self(:)%kind)
         call set_values_(table(4),self(:)%is_unique_kind)
         call set_values_(table(5),self(:)%group)
         call set_values_(table(6),self(:)%site_disorder_group)
      else  ! Not unique labels
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number ID tags are used.")
         if (la/=0) then
            call flush_(stdout)
            call text_(stdout,". First repeated labels are for atoms "//trim(to_str_(la))//" and "//trim(to_str_(lb))//".")
         end if
         if (ka/=0) then
            call flush_(stdout)
            call text_(stdout,". First repeated xtal kinds are for atoms "//trim(to_str_(ka))//" and "//trim(to_str_(kb))//".")
         end if
         call flush_(stdout)
         call show_(stdout,"No. of atoms =",size(self))
         call flush_(stdout)

         ! Table headings
         call create_(table,7)
         call set_heading_(table(1),"Label")
         call set_heading_(table(2),"ID")
         call set_subhead_(table(2),"tag")
         call set_heading_(table(3),"Z")
         call set_heading_(table(4),"Atom")
         call set_subhead_(table(4),"kind")
         call set_heading_(table(5),"Unique")
         call set_subhead_(table(5),"kind?")
         call set_heading_(table(6),"Molecule")
         call set_subhead_(table(6),"group")
         call set_heading_(table(7),"Disorder")
         call set_subhead_(table(7),"group")

         ! Set table column data
         call set_values_(table(1),self(:)%label)
         call set_values_(table(2),self(:)%tag)
         call set_values_(table(3),self(:)%atomic_number)
         call set_values_(table(4),self(:)%kind)
         call set_values_(table(5),self(:)%is_unique_kind)
         call set_values_(table(6),self(:)%group)
         call set_values_(table(7),self(:)%site_disorder_group)
      end if

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_atom_groups(self)
   ! Put out the atom group information
      VEC(type(ATOM_TYPE),:), IN :: self
      INT :: n,i
      VEC(type(EVEC_INT_TYPE),:), allocatable :: list
   ENSURE(has_groups_(self),"VEC{ATOM}:put_atom_groups ... no atom group information")
      call make_atom_group_list_(self,list)
      call save_style_(stdout)
      call set_int_width_(stdout,3)
      call set_n_fields_per_line_(stdout,25)
      call set_using_array_labels_(stdout,FALSE)
      call flush_(stdout)
      call text_(stdout,"Atom group information")
      call flush_(stdout)
      call show_(stdout,"n_groups =",size(list))
      do n = 1,size(list)
         call put_(stdout,"group "//trim(to_str_(n))//"  =")
         do i = 1,size(list(n)%element)
            call put_(stdout,list(n)%element(i))
         end do
         call flush_(stdout)
      end do
      call unsave_style_(stdout)
      call destroy_(list)
   end subroutine

   subroutine put_CCDC_bonded_neighbours(self)
   ! Put the Salvador-Ramos-Cordoba neighbouts for reach atom.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable :: connections_for
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      call save_style_(stdout)
      call set_int_width_(stdout,3)
      call set_n_fields_per_line_(stdout,25)
      call set_using_array_labels_(stdout,FALSE)
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"Bonded neighbours")
      call text_(stdout,"=================")
      call flush_(stdout)
      call text_(stdout,". These atoms are bonded according to the CCDC criteria.")
      call flush_(stdout)
      call text_(stdout,". Atoms A and B are bonded together if the distance d between")
      call text_(stdout,"  them obeys d < r_A + r_B + 0.4 A, where r_A and r_B are the")
      call text_(stdout,"  covalent radii from Brun (2011) Struct. Sci 67 p. 333-49.")
      call flush_(stdout)

      ! Table
      call make_connection_table_(self,connections_for)

      ! Table
      call create_(table,1)
      call set_heading_(table(1),"Neigbours")
      call set_values_(table(1),connections_for)
      call put_(table,label_rows=TRUE)

      ! Clean
      call destroy_(table)
      call destroy_(connections_for)
   end subroutine


!  =====
!  ADP's
!  =====

   subroutine put_ADPs(self,no_header,covariance)
   ! Output the ADP information
   ! Output position and ADP errors if there.
   ! Always use Angstrom units for display.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), optional, IN :: covariance
      BIN, optional, IN :: no_header

      ! If there are errors, tghese should have been
      ! made beforehand and stored in pADP_errors.
      call put_ADP2s_(self,no_header)
      if (present(covariance)) then

      ! .put_ADP2s_in_inertial_axes(covariance)
         call put_ADP2s_in_ADP2_principal_axes_(self,covariance)
      end if

      ! Show ADP3's if present
      if (has_any_ADP3s_and_errors_(self)) then
         call put_ADP3s_(self)
         if (present(covariance)) then

      ! .put_ADP3s_in_inertial_axes(covariance)
            call put_ADP3s_in_ADP2_principal_axes_(self,covariance)
         end if
      end if

      ! Show ADP4's if present
      if (has_any_ADP4s_and_errors_(self)) then
         call put_ADP4s_(self)
         if (present(covariance)) then

      ! .put_ADP4s_in_inertial_axes(covariance)
            call put_ADP4s_in_ADP2_principal_axes_(self,covariance)
         end if
      end if
   end subroutine

   subroutine put_ADP2s(self,no_header)
   ! Output the ADP information
   ! Output position and ADP errors if there.
   ! Always use Angstrom units for display.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: no_header
      VEC(STR(len=STR_SIZE),:), allocatable :: Uc
      MAT(REAL,:,:), allocatable :: U,dU
      VEC(BIN,:), allocatable :: is_flat,is_npd
      VEC(REAL,:), allocatable :: ratio
      VEC(REAL,:), allocatable :: U_iso,U_iso_e
      VEC(REAL,:), allocatable :: M_ani,M_ani_e
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: fac,fac2
      INT :: i
      BIN :: cartesian,header
      header = TRUE
      if (present(no_header)) header = NOT no_header

      ! Axis system
      cartesian = self(1)%pos_axis_system=="cartesian"

      ! Use angstroms/angstrom^2
      if (cartesian) then
         fac = conversion_factor_("angstrom")
      else
         fac = ONE
      end if
      fac2 = fac*fac

      ! Create columns
      call create_(Uc,6)
      call create_(U,size(self),6)
      call create_(dU,size(self),6)

      ! Get columns
      call put_ADP2_labels_to_(Uc)
      call put_ADP2_vector_to_(self,U,fac)
      is_flat = has_flat_ADP_(self(:))
      is_npd = has_NPD_ADP_(self(:))
      ratio = ADP_principal_axis_ratio_(self(:))
      U_iso = self(:)%U_iso*fac2
      U_iso_e = self(:)%U_iso_error*fac2
      M_ani = self(:)%M_ani*fac2
      M_ani_e = self(:)%M_ani_error*fac2

      ! Title
      if (header) then
         call flush_(stdout)
         call text_(stdout,"=====================================")
         call text_(stdout,"Atomic displacement parameters (ADPs)")
         call text_(stdout,"=====================================")
         call flush_(stdout)
         if (cartesian) then
            call text_(stdout,". The ADPs are referred to cartesian axes.")
         else
            call text_(stdout,". The ADPs are referred to unit cell axes.")
         end if
         call text_(stdout,". The length units are in Angstrom.")
         call text_(stdout,". An ADP is deemed bad if the max-on-min axis ratio")
         call text_(stdout,"  - is greater than 4 (i.e. flat)")
         call text_(stdout,"  - or is non-positive-definite (i.e. NPD)")
         call text_(stdout,". M_ani is the square root of the usual anistropy")
         call text_(stdout,"  in a 2nd order tensor. It has units Angstrom^2.")
         call text_(stdout,". NOTE: If the su in U_iso or M_ani is zero, it may")
         call text_(stdout,"  mean that there is no covariance matrix was available")
         call text_(stdout,"  there to calculate it e.g. if you used a non-CIF2 file.")
         call flush_(stdout)
         call show_(stdout,"No. of NPD  ADPs =",no_of_NPD_ADPs_(self))
         call show_(stdout,"No. of Flat ADPs =",no_of_flat_ADPs_(self))
         call flush_(stdout)
      end if

      ! Set table headings & data
      call create_(table,12)
      call set_column_spacing_(table,2)

      ! Set table
      call set_heading_(table(1),"ID")
      call set_values_(table(1),self(:)%label)  ! put_coordinates is ".label" so should be same here
      if (NOT has_nonzero_pADP2_errors_(self)) then

         ! Set table no errors
         do i = 1,6
            call set_heading_(table(i+1),Uc(i))
            call set_values_(table(i+1),U(:,i))
         end do
         call set_heading_(table(8),"U_iso")
         call set_values_(table(8),U_iso)
         call set_heading_(table(9),"M_ani")
         call set_values_(table(9),M_ani)
      else  ! have position/ADP errors

         ! Get error column data
         call put_ADP2_errors_to_(self,dU,fac)

         ! Set table with (errors)
         do i = 1,6
            call set_heading_(table(i+1),Uc(i))
            call set_values_and_errors_(table(i+1),U(:,i),dU(:,i))
         end do
         call set_heading_(table(8),"U_iso")
         call set_values_and_errors_(table(8),U_iso,U_iso_e)
         call set_heading_(table(9),"M_ani")
         call set_values_and_errors_(table(9),M_ani,M_ani_e)
      end if

      ! Units headings
      call set_subhead_(table(4),trim("/A^2"))
      call set_subhead_(table(8),trim("/A^2"))
      call set_subhead_(table(9),trim("/A^2"))

      ! ADP diagonistics
      call set_heading_(table(10),"NPD")
      call set_subhead_(table(10),"  ?")
      call set_values_(table(10),is_npd)
      call set_heading_(table(11),"Flat")
      call set_subhead_(table(11),"   ?")
      call set_values_(table(11),is_flat)
      call set_heading_(table(12)," Axis")
      call set_subhead_(table(12),"ratio")
      call set_values_(table(12),ratio)

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine get_ADP2s_in_new_axes_in(self,U,dU,axes,cov)
   ! Rotate the ADP2s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: U
      MAT(REAL,:,:), allocatable, OUT :: dU
      MAT(REAL,3,3), IN :: axes
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,3,3) :: adp2,new2
      VEC(REAL,6) :: vec2,lex2
      MAT(REAL,6,6) :: rcm
      REAL :: tol
      INT :: a,n
   ENSURE(is_square_(cov),"VEC{ATOM}:get_ADP2s_in_new_axes_in ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:get_ADP2s_in_new_axes_in ... wrong size, covariance matrix!")

      ! Create columns
      n = size(self)
      call create_(U,n,6)
      call create_(dU,n,6)

      ! Loop over atoms
      do a = 1,size(self)

         ! Change ADP2s to new coord system
         adp2 = self(a)%ADP_tensor
         call change_basis_to_(adp2,new2,axes)
         call zip_lower_triangle_to_(new2,vec2)
         call lower_pyramid_to_lexical_2_(vec2,lex2)
         U(a,1:6) = lex2

         ! Rotate whole covariance matrix by "axes"
         rcm = rotated_U2_covariance_mx_for_atom_(self,a,axes,cov)

         ! Extract diagonal elements as errors
         call put_diagonal_to_(rcm,dU(a,1:6))
      end do

      ! Square root sigma^2 -- see the note in
      ! get_ADP2s_in_ADP2_principal_axes_in for why the clamp is needed.
      tol = 1.0d-9*maxval(abs(dU))
      DIE_IF(any(dU<-tol),"VEC{ATOM}:get_ADP2s_in_new_axes_in ... negative variance in the rotated ADP2 covariance matrix")
      where (dU<tol) dU = ZERO
      dU = sqrt(dU)
   end subroutine

   subroutine put_ADP2s_in_inertial_axes(self,cov)
   ! Rotate the ADP2s for each atom into the inertial axis frame,
   ! where "cov" is the covariance matrix for all the parameters in
   ! the concatenated pADP vectors. 
   ! WARNING: PROBLEM WITH THIS ROUTINE? Dylan 25/11/2024
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,:,:), allocatable :: U,dU
      MAT(REAL,3,3) :: axes
   ENSURE(is_square_(cov),"VEC{ATOM}:put_ADP2s_in_inertial_axes ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP2s_in_inertial_axes ... wrong size, covariance matrix!")
      call make_inertial_axes_(self,axes)
      call get_ADP2s_in_new_axes_in_(self,U,dU,axes,cov)

      ! Title
      call flush_(stdout)
      call text_(stdout,"=============================")
      call text_(stdout,"ADP2s in inertial axis system")
      call text_(stdout,"=============================")
      call flush_(stdout)
      call text_(stdout,". ADPs are referred to cartesian axes.")
      call flush_(stdout)
      call text_(stdout,"Inertial axes in the cartesian system:")
      call put_(stdout,axes)

      ! Finally ... print table
      ! This routine scales the U & dU
      call put_ADP2s_helper_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine get_ADP2s_in_ADP2_principal_axes_in(self,U,dU,cov)
   ! Rotate the ADP2s for each atom into the principal axis frame of
   ! each atoms ADP2, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: U
      MAT(REAL,:,:), allocatable, OUT :: dU
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,3,3) :: adp2,new2
      VEC(REAL,6) :: vec2,lex2
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: R
      MAT(REAL,6,6) :: rcm
      REAL :: tol
      INT :: a,n
   ENSURE(is_square_(cov),"VEC{ATOM}:get_ADP2s_in_ADP2_principal_axes_in ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:get_ADP2s_in_ADP2_principal_axes_in ... wrong size, covariance matrix!")

      ! Create columns
      n = size(self)
      call create_(U,n,6)
      call create_(dU,n,6)

      ! ADP2 atom index
      n = 0

      ! Loop over atoms
      do a = 1,size(self)

         ! ADP2 atom index
         n = n+1

         ! Get ADP2 principal axes into "R"
      ! self(a).ADP_tensor.solve_symmetric_eigenproblem(eval,R)
         call diagonalize_Jacobi_(self(a)%ADP_tensor,eval,R)

         ! Change ADP2s to new coord system
         adp2 = self(a)%ADP_tensor
         call change_basis_to_(adp2,new2,R)
         call zip_lower_triangle_to_(new2,vec2)
         call lower_pyramid_to_lexical_2_(vec2,lex2)
         U(n,1:6) = lex2

         ! Rotate whole covariance matrix by "R"
         rcm = rotated_U2_covariance_mx_for_atom_(self,a,R,cov)

         ! Extract diagonal elements as errors
         call put_diagonal_to_(rcm,dU(n,1:6))
      end do

      ! Square root sigma^2.
      ! In this frame the three off-diagonal ADP2 *values* vanish by
      ! construction (each atom's tensor has been diagonalized). Their
      ! *variances* need not: they measure how well determined the
      ! principal axis directions are, so they are genuinely non-zero for
      ! an atom whose axes are free to rotate, and zero for one whose axes
      ! are fixed by site symmetry. In the latter case the computed value
      ! is round-off about zero -- of order 1e-23 against 1e-6 for the
      ! genuine variances -- with an arbitrary sign; sqrt then turns the
      ! ones that land negative into NaN, which printed as a confident
      ! "(0)" esd. Clamp those, symmetrically, so the result does not
      ! depend on which side of zero the noise fell (i.e. on the platform).
      ! A negative variance that is *not* round-off means the covariance
      ! matrix itself is wrong, and must not be silently square-rooted away.
      tol = 1.0d-9*maxval(abs(dU))
      DIE_IF(any(dU<-tol),"VEC{ATOM}:get_ADP2s_in_ADP2_principal_axes_in ... negative variance in the rotated ADP2 covariance matrix")
      where (dU<tol) dU = ZERO
      dU = sqrt(dU)
   end subroutine

   subroutine put_ADP2s_in_ADP2_principal_axes(self,cov)
   ! Rotate the ADP2s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,:,:), allocatable :: U,dU
   ENSURE(is_square_(cov),"VEC{ATOM}:put_ADP2s_in_ADP2_principal_axes ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP2s_in_ADP2_principal_axes ... wrong size, covariance matrix!")
      call get_ADP2s_in_ADP2_principal_axes_in_(self,U,dU,cov)

      ! Title
      call flush_(stdout)
      call text_(stdout,"==================================")
      call text_(stdout,"ADP2s in ADP2 principal axis frame")
      call text_(stdout,"==================================")
      call flush_(stdout)
      call text_(stdout,". ADPs are referred to cartesian axes.")
      call text_(stdout,". ESUs have been obtained from the covariances.")

      ! Finally ... print table
      ! This routine scales the U & dU
      call put_ADP2s_helper_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE function rotated_U2_covariance_mx_for_atom(self,a,R,fcm) result (rcm)
   ! Given index of atom "a", and given a 3x3 rotation matrix "R" and
   ! given the *full* covariance matrix "fcm", return in "rcm" the U2
   ! covariance matrix for atom "a" rotated by matrix "R".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      MAT(REAL,3,3), IN :: R
      MAT(REAL,:,:), IN :: fcm
      MAT(REAL,6,6) :: rcm
      MAT(REAL,6,6) :: old,RR
      INT :: off,f,l
   ENSURE(size(R,1)==3,"VEC{ATOM}:rotated_U2_covariance_mx_for_atom ... wrong dim1, R")
   ENSURE(size(R,2)==3,"VEC{ATOM}:rotated_U2_covariance_mx_for_atom ... wrong dim2, R")
   ENSURE(is_square_(fcm),"VEC{ATOM}:rotated_U2_covariance_mx_for_atom ... full covariance matrix is not square")
   ENSURE(size(fcm,1)==no_of_pADPs_(self),"VEC{ATOM}:rotated_U2_covariance_mx_for_atom ... wrong dimension, fcm")

      ! Offset into diagonal block
      off = no_of_pADPs_up_to_atom_(self,a)+3
      f = off+1
      l = off+6

      ! Get the cov. matrix block
      old = fcm(f:l,f:l)

      ! Get transformation matrix "RR"
      call symmetric_tensor_2_product_mx_(RR,R)

      ! Get the rotated U3 covariance matrix
      call back_transform_to_(old,rcm,RR)
   end function

   subroutine put_ADP2s_helper(self,U,dU)
   ! Output the ADP2s U(N,6) and errors dU(N,6) (if present).
   ! N is tge number of atoms. U and dU in Bohr as usual.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), INOUT :: U
      MAT(REAL,:,:), optional, INOUT :: dU
      VEC(STR(len=STR_SIZE),:), allocatable :: ID,Uc
      VEC(INT,:), allocatable :: list
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: fac
      INT :: i,n
   ENSURE(size(U,1)==size(self),"VEC{ATOM}:put_ADP2s_helper ... U dim1 not equal to no. of atoms with ADP2s")
   ENSURE(size(U,2)==6,"VEC{ATOM}:put_ADP2s_helper ... U dim2 is not equal to 6")

      ! No. of atoms with ADP2s
      n = size(self)

      ! Use angstroms/angstrom^2
      fac = conversion_factor_("angstrom^2")

      ! Create columns
      call create_(list,n)
      call create_(ID,n)
      call create_(Uc,6)

      ! Get columns
      ID = self(:)%label
      call put_ADP2_labels_to_(Uc)

      ! Scale & change to Angstrom
      U = fac*U

      ! Set table headings & data
      call create_(table,7)
      call set_column_spacing_(table,2)

      ! Set table
      call set_heading_(table(1),"ID")
      call set_values_(table(1),ID)
      if (NOT present(dU)) then

         ! Set table
         do i = 1,6
            call set_heading_(table(i+1),Uc(i))
            call set_values_(table(i+1),U(:,i))
         end do
      else  ! have position/ADP errors

         ! Scale errors
         dU = fac*dU

         ! Set table
         do i = 1,6
            call set_heading_(table(i+1),Uc(i))
            call set_values_and_errors_(table(i+1),U(:,i),dU(:,i))
         end do
      end if

      ! Units headings
      call set_subhead_(table(6),trim("/A^2"))

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(Uc)
      call destroy_(ID)
   end subroutine

   subroutine put_ADP3s_0(self)
   ! Output the ADP3 information. Always comes after the ADP
   ! information. Use Angstroms.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable :: U,dU
      INT :: n
      if (NOT has_any_ADP3s_and_errors_(self)) return

      ! No. of atoms with ADP3s
      n = no_of_ADP3_atoms_(self)

      ! Create columns
      call create_(U,n,10)
      call create_(dU,n,10)

      ! Get U3's and scale to Angstrom
      call put_ADP3_vector_to_(self,U)

      ! Get error column data
      call put_ADP3_errors_to_(self,dU)

      ! Print table
      call put_ADP3s_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE subroutine get_ADP3s_in_new_axes_in(self,U,dU,axes,cov)
   ! Rotate the ADP3s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: U
      MAT(REAL,:,:), allocatable, OUT :: dU
      MAT(REAL,3,3), IN :: axes
      MAT(REAL,:,:), IN :: cov
      MAT3(REAL,3,3,3) :: adp3,new3
      VEC(REAL,10) :: vec3,lex3
      MAT(REAL,10,10) :: rcm
      INT :: a,n
   ENSURE(is_square_(cov),"VEC{ATOM}:get_ADP3s_in_new_axes_in ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:get_ADP3s_in_new_axes_in ... wrong size, covariance matrix!")
      if (NOT has_any_ADP3s_and_errors_(self)) return

      ! Create columns
      n = no_of_ADP3_atoms_(self)
      call create_(U,n,10)
      call create_(dU,n,10)

      ! ADP3 atom index
      n = 0

      ! Loop over atoms
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! ADP3 atom index
         n = n+1

         ! Change ADP3s to new coord system
         call put_ADP3_vector_to_(self(a),adp3)
         call change_basis_to_(adp3,new3,axes)
         call compress_to_pyramid_(new3,vec3)
         call lower_pyramid_to_lexical_3_(vec3,lex3)
         U(n,1:10) = lex3

         ! Rotate whole covariance matrix by "axes"
         rcm = rotated_U3_covariance_mx_for_atom_(self,a,axes,cov)

         ! Extract diagonal elements as errors
         call put_diagonal_to_(rcm,dU(n,1:10))
      end do

      ! Square root sigma^2
      dU = sqrt(dU)
   end subroutine

   subroutine put_ADP3s_in_inertial_axes(self,cov)
   ! Rotate the ADP3s for each atom into the inertial axis frame,
   ! where "cov" is the covariance matrix for all the parameters in
   ! the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,:,:), allocatable :: U,dU
      MAT(REAL,3,3) :: axes
   ENSURE(is_square_(cov),"VEC{ATOM}:put_ADP3s_in_inertial_axes ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP3s_in_inertial_axes ... wrong size, covariance matrix!")
      if (NOT has_any_ADP3s_and_errors_(self)) return
      call make_inertial_axes_(self,axes)
      call get_ADP3s_in_new_axes_in_(self,U,dU,axes,cov)

      ! Title
      call flush_(stdout)
      call text_(stdout,"=============================")
      call text_(stdout,"ADP3s in inertial axis system")
      call text_(stdout,"=============================")
      call flush_(stdout)
      call text_(stdout,". ADPs are referred to cartesian axes.")
      call flush_(stdout)
      call text_(stdout,"Inertial axes in the cartesian system:")
      call put_(stdout,axes)

      ! Finally ... print table
      ! This routine scales the U & dU
      call put_ADP3s_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE subroutine get_ADP3s_in_ADP2_principal_axes_in(self,U,dU,cov)
   ! Rotate the ADP3s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: U
      MAT(REAL,:,:), allocatable, OUT :: dU
      MAT(REAL,:,:), IN :: cov
      MAT3(REAL,3,3,3) :: adp3,new3
      VEC(REAL,10) :: vec3,lex3
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: R
      MAT(REAL,10,10) :: rcm
      INT :: a,n
   ENSURE(is_square_(cov),"VEC{ATOM}:get_ADP3s_in_ADP2_principal_axes_in ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:get_ADP3s_in_ADP2_principal_axes_in ... wrong size, covariance matrix!")
      if (NOT has_any_ADP3s_and_errors_(self)) return

      ! Create columns
      n = no_of_ADP3_atoms_(self)
      call create_(U,n,10)
      call create_(dU,n,10)

      ! ADP3 atom index
      n = 0

      ! Loop over atoms
      do a = 1,size(self)
         if (NOT has_ADP3s_and_errors_(self(a))) cycle

         ! ADP3 atom index
         n = n+1

         ! Get ADP2 principal axes into "R"
      ! self(a).ADP_tensor.solve_symmetric_eigenproblem(eval,R)
         call diagonalize_Jacobi_(self(a)%ADP_tensor,eval,R)

         ! Change ADP3s to new coord system
         call put_ADP3_vector_to_(self(a),adp3)
         call change_basis_to_(adp3,new3,R)
         call compress_to_pyramid_(new3,vec3)
         call lower_pyramid_to_lexical_3_(vec3,lex3)
         U(n,1:10) = lex3

         ! Rotate whole covariance matrix by "R"
         rcm = rotated_U3_covariance_mx_for_atom_(self,a,R,cov)

         ! Extract diagonal elements as errors
         call put_diagonal_to_(rcm,dU(n,1:10))
      end do

      ! Square root sigma^2
      dU = sqrt(dU)
   end subroutine

   subroutine put_ADP3s_in_ADP2_principal_axes(self,cov)
   ! Rotate the ADP3s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,:,:), allocatable :: U,dU
   ENSURE(is_square_(cov),"VEC{ATOM}:put_ADP3s_in_ADP2_principal_axes ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP3s_in_ADP2_principal_axes ... wrong size, covariance matrix!")

      ! NOT WORKING - CHEcK - dylan
      return
      if (NOT has_any_ADP3s_and_errors_(self)) return
      call get_ADP3s_in_ADP2_principal_axes_in_(self,U,dU,cov)

      ! Title
      call flush_(stdout)
      call text_(stdout,"==================================")
      call text_(stdout,"ADP3s in ADP2 principal axis frame")
      call text_(stdout,"==================================")
      call flush_(stdout)
      call text_(stdout,". ADPs are referred to cartesian axes.")

      ! Finally ... print table
      ! This routine scales the U & dU
      call put_ADP3s_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE function rotated_U3_covariance_mx_for_atom(self,a,R,fcm) result (rcm)
   ! Given index of atom "a", and given a 3x3 rotation matrix "R" and
   ! given the *full* covariance matrix "fcm", return in "rcm" the U3
   ! covariance matrix for atom "a" rotated by matrix "R".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      MAT(REAL,3,3), IN :: R
      MAT(REAL,:,:), IN :: fcm
      MAT(REAL,10,10) :: rcm
      MAT(REAL,10,10) :: old,RR
      INT :: off,f,l
   ENSURE(size(R,1)==3,"VEC{ATOM}:rotated_U3_covariance_mx_for_atom ... wrong dim1, R")
   ENSURE(size(R,2)==3,"VEC{ATOM}:rotated_U3_covariance_mx_for_atom ... wrong dim2, R")
   ENSURE(is_square_(fcm),"VEC{ATOM}:rotated_U3_covariance_mx_for_atom ... full covariance matrix is not square")
   ENSURE(size(fcm,1)==no_of_pADPs_(self),"VEC{ATOM}:rotated_U3_covariance_mx_for_atom ... wrong dimension, fcm")

      ! Offset into diagonal block
      off = no_of_pADPs_up_to_atom_(self,a)+9
      f = off+1
      l = off+10

      ! Get the cov. matrix block
      old = fcm(f:l,f:l)

      ! Get transformation matrix "RR"
      call symmetric_tensor_3_product_mx_(RR,R)

      ! Get the rotated U3 covariance matrix
      call back_transform_to_(old,rcm,RR)
   end function

   subroutine put_ADP3s_1(self,U,dU)
   ! Output the ADP3s U(N,10) and errors dU(N,10) (if present).
   ! N is thge number of atoms. U and dU in Bohr as usual.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), INOUT :: U
      MAT(REAL,:,:), optional, INOUT :: dU
      VEC(STR(len=STR_SIZE),:), allocatable :: ID,Uc
      VEC(INT,:), allocatable :: list
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: fac,sfac
      INT :: i,n
   ENSURE(size(U,1)==no_of_ADP3_atoms_(self),"VEC{ATOM}:put_ADP3s_1 ... U dim1 not equal to no. of atoms with ADP3s")
   ENSURE(size(U,2)==10,"VEC{ATOM}:put_ADP3s_1 ... U dim2 is not equal to 10")

      ! Scale factor
      sfac = CIF_GC3_SCALE_FACTOR
      call flush_(stdout)
      call text_(stdout,". The cartesian ADP3s below have been scaled")
      call show_(stdout,"  scale factor =",sfac)

      ! No. of atoms with ADP3s
      n = no_of_ADP3_atoms_(self)

      ! Use angstroms/angstrom^2
      fac = conversion_factor_("angstrom^3")

      ! Create columns
      call create_(list,n)
      call create_(ID,n)
      call create_(Uc,10)

      ! Get columns
      call put_ADP3_atom_list_to_(self,list)
      ID = self(list)%tag
      call put_ADP3_labels_to_(Uc)

      ! Scale & change to Angstrom
      U = sfac*fac*U

      ! Set table headings & data
      call create_(table,11)
      call set_column_spacing_(table,2)

      ! Set table
      call set_heading_(table(1),"ID")
      call set_values_(table(1),ID)
      if (NOT present(dU)) then

         ! Set table
         do i = 1,10
            call set_heading_(table(i+1),Uc(i))
            call set_values_(table(i+1),U(:,i))
         end do
      else  ! have position/ADP errors

         ! Scale errors
         dU = sfac*fac*dU

         ! Set table
         do i = 1,10
            call set_heading_(table(i+1),Uc(i))
            call set_values_and_errors_(table(i+1),U(:,i),dU(:,i))
         end do
      end if

      ! Units headings
      call set_subhead_(table(6),trim("/A^3"))

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(Uc)
      call destroy_(ID)
      call destroy_(list)
   end subroutine


!   put_ADP3s_transposed :: private
!   ! Output the ADP3 information. Always comes after the ADP
!   ! information. Use Angstroms.
!      self :: IN
!
!      U,dU :: MAT{REAL}@
!      n :: INT
!
!      if (NOT .has_any_ADP3s_and_errors) return
!
!      ! No. of atoms with ADP3s
!      n = .no_of_ADP3_atoms
!
!      ! Create columns
!      U.create(n,10)
!      dU.create(n,10)
!
!      ! Get U3's and scale to Angstrom
!      .put_ADP3_vector_to(U)
!
!      ! Get error column data
!      .put_ADP3_errors_to(dU)
!
!      ! Print table
!      .put_ADP3s_transposed(U,dU)
!
!      ! Clean
!      dU.destroy
!      U.destroy
!
!   end

!   put_ADP3s_transposed(U,dU) :: private
!   ! Output the ADP3s U(N,10) and errors dU(N,10) (if present).
!   ! N is thge number of atoms. U and dU in Bohr as usual.
!      self :: IN 
!      U  :: MAT{REAL}, INOUT
!      dU :: MAT{REAL}, optional, INOUT
!
!   ENSURE(U.dim1==.no_of_ADP3_atoms,"U dim1 not equal to no. of atoms with ADP3s")
!   ENSURE(U.dim2==10  ,"U dim2 is not equal to 10")
!
!      ID,Uc :: VEC{STR}@
!      list :: VEC{INT}@
!      table :: VEC{TABLE_COLUMN}@
!      fac,sfac :: REAL
!      i,n :: INT
!
!      ! Scale factor
!      sfac = CIF_GC3_SCALE_FACTOR
!      stdout.flush
!      stdout.text(". The cartesian ADP3s below have been scaled")
!      stdout.show("  scale factor =",sfac)
!
!      ! No. of atoms with ADP3s
!      n = .no_of_ADP3_atoms
!
!      ! Use angstroms/angstrom^2
!      fac  = STR:conversion_factor("angstrom^3")
!
!      ! Create columns
!      list.create(n)
!      ID.create(n)
!      Uc.create(10)
!
!      ! Get columns
!      .put_ADP3_atom_list_to(list)
!      ID = self(list).tag
!      ATOM:put_ADP3_labels_to(Uc)
!
!      ! Scale & change to Angstrom
!      U = sfac*fac*U
!
!      ! Set table headings & data
!      table.create(n+1)
!      table.set_column_spacing(2)
!
!      ! Set table first column
!      table(1).set_heading("U3 ADP")
!      table(1).set_subhead("/A^3")
!      table(1).set_values(Uc)
!
!      if (NOT present(dU)) then
!
!         ! Set table
!         do i = 1,n
!         table(i+1).set_heading(self(list(i)).tag)
!         table(i+1).set_values(U(i,:))
!         end
!
!      else ! have position/ADP errors
!
!         ! Scale errors
!         dU = sfac*fac*dU
!
!         ! Set table
!         do i = 1,10
!         table(i+1).set_heading(self(list(i)).tag)
!         table(i+1).set_values_and_errors(U(i,:),dU(i,:))
!         end
!
!      end
!
!      ! Make the table
!      table.put
!
!      ! Clean
!      table.destroy
!      Uc.destroy
!      ID.destroy
!      list.destroy
!
!   end

   subroutine put_ADP4s_0(self)
   ! Output the ADP4 information. Always comes after the ADP
   ! information. Use Angstroms.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable :: U,dU
      INT :: n
      if (NOT has_any_ADP4s_and_errors_(self)) return

      ! No. of atoms with ADP4s
      n = no_of_ADP4_atoms_(self)

      ! Create columns
      call create_(U,n,15)
      call create_(dU,n,15)

      ! Get U4's and scale to Angstrom
      call put_ADP4_vector_to_(self,U)

      ! Get error column data
      call put_ADP4_errors_to_(self,dU)

      ! Print table
      call put_ADP4s_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE subroutine get_ADP4s_in_new_axes_in(self,U,dU,axes,cov)
   ! Rotate the ADP4s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: U
      MAT(REAL,:,:), allocatable, OUT :: dU
      MAT(REAL,3,3), IN :: axes
      MAT(REAL,:,:), IN :: cov
      MAT4(REAL,3,3,3,3) :: adp4,new4
      VEC(REAL,15) :: vec4,lex4
      MAT(REAL,15,15) :: rcm
      INT :: a,n
   ENSURE(is_square_(cov),"VEC{ATOM}:get_ADP4s_in_new_axes_in ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:get_ADP4s_in_new_axes_in ... wrong size, covariance matrix!")
      if (NOT has_any_ADP4s_and_errors_(self)) return

      ! Create columns
      n = no_of_ADP4_atoms_(self)
      call create_(U,n,15)
      call create_(dU,n,15)

      ! ADP4 atom index
      n = 0

      ! Loop over atoms
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! ADP4 atom index
         n = n+1

         ! Change ADP4s to new coord system
         call put_ADP4_vector_to_(self(a),adp4)
         call change_basis_to_(adp4,new4,axes)
         call compress_to_pyramid_(new4,vec4)
         call lower_pyramid_to_lexical_4_(vec4,lex4)
         U(n,1:15) = lex4

         ! Rotate whole covariance matrix by "axes"
         rcm = rotated_U4_covariance_mx_for_atom_(self,a,axes,cov)

         ! Extract diagonal elements as errors
         call put_diagonal_to_(rcm,dU(n,1:15))
      end do

      ! Square root sigma^2
      dU = sqrt(dU)
   end subroutine

   subroutine put_ADP4s_in_inertial_axes(self,cov)
   ! Rotate the ADP4s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,:,:), allocatable :: U,dU
      MAT(REAL,3,3) :: axes
   ENSURE(is_square_(cov),"VEC{ATOM}:put_ADP4s_in_inertial_axes ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP4s_in_inertial_axes ... wrong size, covariance matrix!")
      if (NOT has_any_ADP4s_and_errors_(self)) return
      call make_inertial_axes_(self,axes)
      call get_ADP4s_in_new_axes_in_(self,U,dU,axes,cov)

      ! Title
      call flush_(stdout)
      call text_(stdout,"=============================")
      call text_(stdout,"ADP4s in inertial axis system")
      call text_(stdout,"=============================")
      call flush_(stdout)
      call text_(stdout,". ADPs are referred to cartesian axes.")
      call flush_(stdout)
      call text_(stdout,"Inertial axes in the cartesian system:")
      call put_(stdout,axes)

      ! Finally ... print table
      ! This routine scales the U & dU
      call put_ADP4s_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE subroutine get_ADP4s_in_ADP2_principal_axes_in(self,U,dU,cov)
   ! Rotate the ADP4s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable, OUT :: U
      MAT(REAL,:,:), allocatable, OUT :: dU
      MAT(REAL,:,:), IN :: cov
      MAT4(REAL,3,3,3,3) :: adp4,new4
      VEC(REAL,15) :: vec4,lex4
      VEC(REAL,3) :: eval
      MAT(REAL,3,3) :: R
      MAT(REAL,15,15) :: rcm
      INT :: a,n
   ENSURE(is_square_(cov),"VEC{ATOM}:get_ADP4s_in_ADP2_principal_axes_in ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:get_ADP4s_in_ADP2_principal_axes_in ... wrong size, covariance matrix!")
      if (NOT has_any_ADP4s_and_errors_(self)) return

      ! Create columns
      n = no_of_ADP4_atoms_(self)
      call create_(U,n,15)
      call create_(dU,n,15)

      ! ADP4 atom index
      n = 0

      ! Loop over atoms
      do a = 1,size(self)
         if (NOT has_ADP4s_and_errors_(self(a))) cycle

         ! ADP4 atom index
         n = n+1

         ! Get ADP2 principal axes into "R"
      ! self(a).ADP_tensor.solve_symmetric_eigenproblem(eval,R)
         call diagonalize_Jacobi_(self(a)%ADP_tensor,eval,R)

         ! Change ADP4s to new coord system
         call put_ADP4_vector_to_(self(a),adp4)
         call back_transform_to_(adp4,new4,R)
         call compress_to_pyramid_(new4,vec4)
         call lower_pyramid_to_lexical_4_(vec4,lex4)
         U(n,1:15) = lex4

         ! Rotate whole covariance matrix by "R"
         rcm = rotated_U4_covariance_mx_for_atom_(self,a,R,cov)

         ! Extract diagonal elements as errors
         call put_diagonal_to_(rcm,dU(n,1:15))
      end do

      ! Square root sigma^2
      dU = sqrt(dU)
   end subroutine

   subroutine put_ADP4s_in_ADP2_principal_axes(self,cov)
   ! Rotate the ADP4s for each atom into the principal axis frame of
   ! each atom, where "cov" is the covariance matrix for all the
   ! parameters in the concatenated pADP vectors. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      MAT(REAL,:,:), allocatable :: U,dU
   ENSURE(is_square_(cov),"VEC{ATOM}:put_ADP4s_in_ADP2_principal_axes ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_ADP4s_in_ADP2_principal_axes ... wrong size, covariance matrix!")

      ! NOT WORKING - CHEcK - dylan
      return
      if (NOT has_any_ADP4s_and_errors_(self)) return
      call get_ADP4s_in_ADP2_principal_axes_in_(self,U,dU,cov)

      ! Title
      call flush_(stdout)
      call text_(stdout,"==================================")
      call text_(stdout,"ADP4s in ADP2 principal axis frame")
      call text_(stdout,"==================================")
      call flush_(stdout)
      call text_(stdout,". ADPs are referred to cartesian axes.")

      ! Finally ... print table
      ! This routine scales the U & dU
      call put_ADP4s_(self,U,dU)

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   PURE function rotated_U4_covariance_mx_for_atom(self,a,R,fcm) result (rcm)
   ! Given index of atom "a", and given a 3x3 rotation matrix "R" and
   ! given the *full* covariance matrix "fcm", return in "rcm" the U3
   ! covariance matrix for atom "a" rotated by matrix "R".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a
      MAT(REAL,3,3), IN :: R
      MAT(REAL,:,:), IN :: fcm
      MAT(REAL,15,15) :: rcm
      MAT(REAL,15,15) :: old,RR
      INT :: off,f,l
   ENSURE(size(R,1)==3,"VEC{ATOM}:rotated_U4_covariance_mx_for_atom ... wrong dim1, R")
   ENSURE(size(R,2)==3,"VEC{ATOM}:rotated_U4_covariance_mx_for_atom ... wrong dim2, R")
   ENSURE(is_square_(fcm),"VEC{ATOM}:rotated_U4_covariance_mx_for_atom ... full covariance matrix is not square")
   ENSURE(size(fcm,1)==no_of_pADPs_(self),"VEC{ATOM}:rotated_U4_covariance_mx_for_atom ... wrong dimension, fcm")

      ! Offset into diagonal block
      off = no_of_pADPs_up_to_atom_(self,a)+19
      f = off+1
      l = off+15

      ! Get the cov. matrix block
      old = fcm(f:l,f:l)

      ! Get transformation matrix "RR"
      call symmetric_tensor_4_product_mx_(RR,R)

      ! Get the rotated U3 covariance matrix
      call back_transform_to_(old,rcm,RR)
   end function

   subroutine put_ADP4s_1(self,U,dU)
   ! Output the ADP4s U(N,15) and errors dU(N,15) (if present).
   ! N is thge number of atoms. U and dU in Bohr as usual.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), INOUT :: U
      MAT(REAL,:,:), optional, INOUT :: dU
      VEC(STR(len=STR_SIZE),:), allocatable :: ID,Uc
      VEC(INT,:), allocatable :: list
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: fac,sfac
      INT :: i,n
   ENSURE(size(U,1)==no_of_ADP4_atoms_(self),"VEC{ATOM}:put_ADP4s_1 ... U dim1 not equal to no. of atoms with ADP4s")
   ENSURE(size(U,2)==15,"VEC{ATOM}:put_ADP4s_1 ... U dim2 is not equal to 15")

      ! Scale factor
      sfac = CIF_GC4_SCALE_FACTOR
      call flush_(stdout)
      call text_(stdout,". The cartesian ADP4s below have been scaled")
      call show_(stdout,"  scale factor =",sfac)

      ! No. of atoms with ADP4s
      n = no_of_ADP4_atoms_(self)

      ! Use angstroms/angstrom^2
      fac = conversion_factor_("angstrom^4")

      ! Create columns
      call create_(list,n)
      call create_(ID,n)
      call create_(Uc,15)

      ! Get columns
      call put_ADP4_atom_list_to_(self,list)
      ID = self(list)%tag
      call put_ADP4_labels_to_(Uc)

      ! Scale & change to Angstrom
      U = sfac*fac*U

      ! Set table headings & data
      call create_(table,16)
      call set_column_spacing_(table,2)

      ! Set table
      call set_heading_(table(1),"ID")
      call set_values_(table(1),ID)
      if (NOT present(dU)) then

         ! Set table
         do i = 1,15
            call set_heading_(table(i+1),Uc(i))
            call set_values_(table(i+1),U(:,i))
         end do
      else  ! have position/ADP errors

         ! Scale errors
         dU = sfac*fac*dU

         ! Set table
         do i = 1,15
            call set_heading_(table(i+1),Uc(i))
            call set_values_and_errors_(table(i+1),U(:,i),dU(:,i))
         end do
      end if

      ! Units headings
      call set_subhead_(table(8),trim("/A^4"))

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(Uc)
      call destroy_(ID)
      call destroy_(list)
   end subroutine

   subroutine put_pADPs(self)
   ! Output the pADP vector information
   ! Genrally used for debugging only.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), allocatable :: mx
      INT :: n
      REAL :: fac

      ! Use angstroms/angstrom^2
      fac = conversion_factor_("angstrom")

      ! No. of ADP's.
      n = no_of_pADPs_(self)

      ! Get then
      call create_(mx,n,2)
      call put_pADP_vector_to_(self,mx(:,1),fac)
      call put_pADP_errors_to_(self,mx(:,2),fac)

      ! Title
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Position/ADP vector")
      call text_(stdout,"===================")
      call flush_(stdout)
      call text_(stdout,". Column 1 is the pADP vector")
      call text_(stdout,". Column 2 are the corresponding errors")
      call text_(stdout,". The length unit is Angstrom")
      call flush_(stdout)
      call show_(stdout,"No. of pADPs =",n)
      call flush_(stdout)

      ! Put
      call put_(stdout,mx)
   end subroutine

   subroutine put_dispersion_factors(self)
   ! Output the anamalous dispersion coefficients for every unique
   ! element in the molecule. 
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), allocatable :: ulist
      VEC(STR(len=STR_SIZE),:), allocatable :: ID
      VEC(REAL,:), allocatable :: f_r,f_i
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      INT :: n_kind

      ! Unique atom list
      n_kind = maxval(self(:)%kind)
      call create_(ulist,n_kind)
      call make_unique_atom_list_(self,ulist)
      ID = chemical_symbol_(self(ulist))
      f_r = self(ulist)%f_r
      f_i = self(ulist)%f_i

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"======================================")
      call text_(stdout,"Anomalous dispersion f' and f'' values")
      call text_(stdout,"======================================")
      call flush_(stdout)
      call show_(stdout,"No. of elements =",n_kind)
      call flush_(stdout)
      call text_(stdout,"WARNING: it is up to you to ensure these dispersion")
      call text_(stdout,"coefficients below correspond to the wavelength used. ")
      call flush_(stdout)

      ! Table headings
      call create_(table,3)
      call set_heading_(table(1),"ID")
      call set_heading_(table(2),"f'")
      call set_heading_(table(3),"f''")

      ! Set table column data
      call set_values_(table(1),ID)
      call set_values_(table(2),f_r)
      call set_values_(table(3),f_i)

      ! Print the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine


!  ============
!  Bond lengths
!  ============

   subroutine put_bond_length_table_0(self)
   ! Output the bond length table.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist
      MAT(INT,:,:), allocatable :: pair
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds
      BIN :: has_unique_labels
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_bond_length_table_0 ... axis system must be cartesian")

      ! Anything to do?
      if (size(self)<2) return
      no_of_bonds = no_of_bonds_(self)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"============")
      call text_(stdout,"Bond lengths")
      call text_(stdout,"============")
      call flush_(stdout)
      call text_(stdout,". The bond-detection criteria is the same as used")
      call text_(stdout,"  by the Cambridge Structural Database (CSD).")
      if (NOT has_unique_labels) then
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of bonds =",no_of_bonds)
      if (no_of_bonds>VEC_ATOM_MAX_BONDS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many bonds to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,7)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self(:)))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_left_justify_(table(3),TRUE)
      call set_heading_(table(4),"A")
      call set_width_from_(table(4),self(:)%tag)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"--")
      call set_width_(table(5),2)
      call set_heading_(table(6),"B")
      call set_width_(table(6),table(4)%width)
      call set_left_justify_(table(6),TRUE)
      call set_heading_(table(7),"R(A--B)")
      call set_subheading_(table(7),"/A")
      call set_width_from_(table(7),ONE)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            call get_bonded_atoms_(self,pair,dist,Z_list,k1,k2)
            dist = dist*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Element symbols
            call create_(symbols,n_bonds,2)
            symbols = " "
            symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
            symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self(pair(:,1))%tag
            labels(:,2) = self(pair(:,2))%tag

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "--"
            dsh1 = "  "
            dsh1(1) = "--"

         ! Set table data
            call set_values_(table(1),symbols(:,1))
            call set_values_(table(2),dsh1)
            call set_values_(table(3),symbols(:,2))
            call set_values_(table(4),labels(:,1))
            call set_values_(table(5),dash)
            call set_values_(table(6),labels(:,2))
            call set_values_(table(7),dist)

         ! Put body
            call flush_(stdout)
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(labels)
            call destroy_(symbols)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_bond_length_table_1(self,covariance,parentheses)
   ! Output the bond length table with errors, given the "covariance"
   ! matrix between all atomic positions. Set "paretheses" FALSE if
   ! you want errors without them.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: parentheses
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist,esds
      MAT(INT,:,:), allocatable :: pair
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds
      BIN :: use_brackets,has_unique_labels
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_bond_length_table_1 ... axis system must be cartesian")

      ! Anything to do?
      if (size(self)<2) return
      no_of_bonds = no_of_bonds_(self)
      if (no_of_bonds<1) return

      ! Parentheses
      use_brackets = TRUE
      if (present(parentheses)) use_brackets = parentheses

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"============")
      call text_(stdout,"Bond lengths")
      call text_(stdout,"============")
      call flush_(stdout)
      call text_(stdout,". The bond-detection criteria is the same as used")
      call text_(stdout,"  by the Cambridge Structural Database (CSD).")
      if (NOT has_unique_labels) then
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number ID tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of bonds =",no_of_bonds)
      call flush_(stdout)

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      if (use_brackets) then; call create_(table,7)
      else; call create_(table,8)
      end if

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self(:)))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_left_justify_(table(3),TRUE)
      call set_heading_(table(4),"A")
      call set_width_from_(table(4),self(:)%tag)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"--")
      call set_width_(table(5),2)
      call set_heading_(table(6),"B")
      call set_width_(table(6),table(4)%width)
      call set_left_justify_(table(6),TRUE)
      call set_heading_(table(7),"R(A--B)")
      call set_subheading_(table(7),"/A")
      call set_width_from_(table(7),ONE)
      if (NOT use_brackets) then
         call set_heading_(table(8),"Error")
         call set_width_from_(table(8),ONE)
      end if

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom Z classes, Z(k1) >= Z(k2)
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            call get_bonded_atoms_(self,pair,dist,esds,Z_list,k1,k2,covariance)  ! leaky
            dist = dist*ANGSTROM_PER_BOHR
            esds = esds*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(esds)
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Element symbols
            call create_(symbols,n_bonds,2)
            symbols = " "
            symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
            symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self(pair(:,1))%tag
            labels(:,2) = self(pair(:,2))%tag

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "--"
            dsh1 = "  "
            dsh1(1) = "--"

         ! Set table data
            call set_values_(table(1),symbols(:,1))
            call set_values_(table(2),dsh1)
            call set_values_(table(3),symbols(:,2))
            call set_values_(table(4),labels(:,1))
            call set_values_(table(5),dash)
            call set_values_(table(6),labels(:,2))
            if (use_brackets) then
               call set_real_precision_(table(7),stdout%style%real_precision)  ! Must do, for every block
               call set_values_and_errors_(table(7),dist,esds)
            else
               call set_values_(table(7),dist)
               call set_values_(table(8),esds)
            end if

         ! Put body
            call flush_(stdout)
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(labels)
            call destroy_(symbols)
            call destroy_(esds)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine get_bonded_atoms_0(self,pair,dist,Z_list,k1,k2,range_factor)
   ! Get the Nx2 bonded-atom "pair"s, and the distances "dist" between
   ! them. The pair have "Z_list" groups "k1" and "k2", respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: pair
      VEC(REAL,:), allocatable, OUT :: dist
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2
      REAL, optional, IN :: range_factor
      INT :: i1,i2,a1,a2,n
   ENSURE(k1<=k2,"VEC{ATOM}:get_bonded_atoms_0 ... k2 is smaller than k1")

      ! How many pairs, n, are there?

      ! The following code is inherited from VEC{ATOM}
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,range_factor)) cycle

            ! Count
            n = n+1
         end do
      end do

      ! Allocate
      call create_(pair,n,2)
      call create_(dist,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,range_factor)) cycle

            ! Add the data
            n = n+1
            pair(n,1) = a1
            pair(n,2) = a2
            dist(n) = bond_distance_(self,a1,a2)
         end do
      end do
   end subroutine

   subroutine get_vdw_bonded_atoms(self,pair,dist,Z_list,k1,k2,range_factor)
   ! Get the Nx2 bonded-atom "pair"s, and the distances "dist" between
   ! them. The pair have "Z_list" groups "k1" and "k2", respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: pair
      VEC(REAL,:), allocatable, OUT :: dist
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2
      REAL, optional, IN :: range_factor
      INT :: i1,i2,a1,a2,n
   ENSURE(k1<=k2,"VEC{ATOM}:get_vdw_bonded_atoms ... k2 is smaller than k1")

      ! How many pairs, n, are there?

      ! The following code is inherited from VEC{ATOM}
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT vdw_bonded_(self,a1,a2,range_factor)) cycle

            ! Count
            n = n+1
         end do
      end do

      ! Allocate
      call create_(pair,n,2)
      call create_(dist,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT vdw_bonded_(self,a1,a2,range_factor)) cycle

            ! Add the data
            n = n+1
            pair(n,1) = a1
            pair(n,2) = a2
            dist(n) = bond_distance_(self,a1,a2)
         end do
      end do
   end subroutine

   subroutine get_bonded_atoms_1(self,pair,dist,esds,Z_list,k1,k2,covariance,H_bonded)
   ! Get the Nx2 bonded-atom "pair"s, and the distances "dist" between
   ! them, and the associated "esds". The pair have "Z_list" groups of
   ! "k1" and "k2", respectively. The "covariance" matrix for *all*
   ! atom coordinates is also given.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: pair
      VEC(REAL,:), allocatable, OUT :: dist,esds
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: H_bonded
      INT :: i1,i2,a1,a2,n
      VEC(REAL,2) :: d
   ENSURE(k1<=k2,"VEC{ATOM}:get_bonded_atoms_1 ... k2 is smaller than k1")
   ENSURE(is_square_(covariance),"VEC{ATOM}:get_bonded_atoms_1 ... covariance: not square")
   ENSURE(size(covariance,1)==3*size(self),"VEC{ATOM}:get_bonded_atoms_1 ... covariance: wrong size")

      ! How many pairs, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,H_bonded=H_bonded)) cycle

            ! Count
            n = n+1
         end do
      end do

      ! Allocate
      call create_(pair,n,2)
      call create_(dist,n)
      call create_(esds,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,H_bonded=H_bonded)) cycle

            ! Get bond distance & error
            d = bond_distance_(self,a1,a2,covariance)

            ! Add the data
            n = n+1
            pair(n,1) = a1
            pair(n,2) = a2
            dist(n) = d(1)
            esds(n) = d(2)
         end do
      end do
   end subroutine

   subroutine put_bond_length_with_error(self,a1,a2,covariance)
   ! Put the bond length with error between atoms "a1" and "a2".
   ! The "covariance" matrix is for *all* atomn.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a1,a2
      MAT(REAL,:,:), IN :: covariance
      VEC(REAL,2) :: d
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_bond_length_with_error ... axis system must be cartesian")
   ENSURE(is_square_(covariance),"VEC{ATOM}:put_bond_length_with_error ... covariance: not square")
   ENSURE(size(covariance,1)==3*size(self),"VEC{ATOM}:put_bond_length_with_error ... covariance: wrong size")

      ! Add the data
      d = bond_distance_(self,a1,a2,covariance,angstrom=TRUE)

      ! Output
      call flush_(stdout)
      call text_(stdout,"======================")
      call text_(stdout,"Bond length with error")
      call text_(stdout,"======================")
      call show_(stdout,"Atom 1        =",self(a1)%tag)
      call show_(stdout,"Atom 2        =",self(a2)%tag)
      call show_(stdout,"Bond length   =",d(1))
      call show_(stdout,"Error         =",d(2))
   end subroutine

   subroutine put_Salvador_radii_table_0(self,bonds_for,radii_for)
   ! Output the Salvador bond length table.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds_for
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii_for
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: strA,labA
      VEC(STR(len=STR_SIZE),:), allocatable :: strB,labB
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: dist,radi
      INT :: n_bonds,a,n,b
      BIN :: has_unique_labels

      ! Anything to do?
      if (size(self)<2) return
      n_bonds = no_of_bonds_(self,bonds_for)
      if (n_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Mayer-Salvador radii")
      call text_(stdout,"====================")
      call flush_(stdout)
      call text_(stdout,". R(A.B) = distance from A to a minimum in the ED between")
      call text_(stdout,"           a pair of 'neighbour' atoms, A and B.")
      call flush_(stdout)
      call text_(stdout,"  The . in R(A.B) isn't a bond critical point, but nearly.")
      call text_(stdout,"  Since it is a minimum in only 1 dimension, not all three.")
      call text_(stdout,"  A -ve R(A.B) means no minimum in the ED was found -- bad.")
      call flush_(stdout)
      call text_(stdout,". Two atoms are neighbours if their midpoint is *not* closer")
      call text_(stdout,"  to any third atom. See Salvador & Ramos-Cordoba (2013)")
      call text_(stdout,"  JCP 139 p. 071103 p3 paragraph 3.")
      call flush_(stdout)
      call text_(stdout,". The radii are used to define Topological Fuzzy Voronoi Atoms")
      call text_(stdout,"  (TFVAs) i.e. 'Salvador atoms'.")
      call flush_(stdout)
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"Total no. of neighbours =",n_bonds)
      if (n_bonds>VEC_ATOM_MAX_BONDS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many bonds to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Create table
      call create_(table,7)
      call set_heading_(table(1),"#A")
      call set_width_(table(1),3)
      call set_heading_(table(2),"#B")
      call set_width_(table(2),3)
      call set_heading_(table(3),"A")
      call set_width_from_(table(3),self(:)%tag)
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_heading_(table(5),"B")
      call set_width_(table(5),table(3)%width)
      call set_left_justify_(table(5),TRUE)
      call set_heading_(table(6),"R(A-B)")
      call set_subheading_(table(6),"/A")
      call set_real_precision_(table(6),4)
      call set_width_from_(table(6),ONE)
      call set_heading_(table(7),"R(A.B)")
      call set_subheading_(table(7),"/A")
      call set_real_precision_(table(7),4)
      call set_width_from_(table(7),ONE)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)
      do a = 1,size(self)

         ! Get the info
         n_bonds = size(bonds_for(a)%element)
         call create_(strA,n_bonds)
         call create_(strB,n_bonds)
         call create_(labA,n_bonds)
         call create_(labB,n_bonds)
         call create_(dash,n_bonds)
         call create_(dist,n_bonds)
         call create_(radi,n_bonds)
         strA = " "
         dash = "--"
         do n = 1,n_bonds
            b = bonds_for(a)%element(n)
            if (n==1) strA(n) = to_str_(a)
            strB(n) = to_str_(b)
            labA(n) = self(a)%tag
            labB(n) = self(b)%tag
            dist(n) = norm2(self(b)%position-self(a)%position)
            radi(n) = radii_for(a)%element(n)
         end do
         dist = dist*ANGSTROM_PER_BOHR
         radi = radi*ANGSTROM_PER_BOHR

         ! Set table data
         call set_values_(table(1),strA)
         call set_values_(table(2),strB)
         call set_values_(table(3),labA)
         call set_values_(table(4),dash)
         call set_values_(table(5),labB)
         call set_values_(table(6),dist)
         call set_values_(table(7),radi)

         ! Put body
         call flush_(stdout)
         call put_body_(table,label_rows=FALSE)

         ! Clean
         call clear_columns_(table)
         call destroy_(radi)
         call destroy_(dist)
         call destroy_(dash)
         call destroy_(labB)
         call destroy_(labA)
         call destroy_(strB)
         call destroy_(strA)
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_Salvador_radii_table_1(self,bonds_for,radii_for,radii_pro)
   ! Output the Salvador bond length table.
   ! This version compares with promolecule Salvador radii.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds_for
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii_for
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii_pro
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: strA,labA
      VEC(STR(len=STR_SIZE),:), allocatable :: strB,labB
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: dist,radi
      VEC(REAL,:), allocatable :: rpro,rtio
      VEC(REAL,:), allocatable :: rchi
      INT :: n_bonds,a,n,b
      BIN :: has_unique_labels

      ! Anything to do?
      if (size(self)<2) return
      n_bonds = no_of_bonds_(self,bonds_for)
      if (n_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Mayer-Salvador radii")
      call text_(stdout,"====================")
      call flush_(stdout)
      call text_(stdout,". R(A.B) = distance from A to a minimum in the ED between")
      call text_(stdout,"           a pair of 'neighbour' atoms, A and B.")
      call flush_(stdout)
      call text_(stdout,"  The . in R(A.B) isn't a bond critical point, but nearly.")
      call text_(stdout,"  Since it is a minimum in only 1 dimension, not all three.")
      call text_(stdout,"  A -ve R(A.B) means no minimum in the ED was found -- bad.")
      call flush_(stdout)
      call text_(stdout,". Also reported are R(A.B) for the promolecular density.")
      call flush_(stdout)
      call text_(stdout,". Two atoms are neighbours if their midpoint is *not* closer")
      call text_(stdout,"  to any third atom. See Salvador & Ramos-Cordoba (2013)")
      call text_(stdout,"  JCP 139 p. 071103 p3 paragraph 3.")
      call flush_(stdout)
      call text_(stdout,". The radii are used to define Topological Fuzzy Voronoi Atoms")
      call text_(stdout,"  (TFVAs) i.e. 'Salvador atoms'.")
      call flush_(stdout)
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"Total no. of neighbours =",n_bonds)
      if (n_bonds>VEC_ATOM_MAX_BONDS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many bonds to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Create table
      call create_(table,10)
      call set_heading_(table(1),"#A")
      call set_width_(table(1),3)
      call set_heading_(table(2),"#B")
      call set_width_(table(2),3)
      call set_heading_(table(3),"A")
      call set_width_from_(table(3),self(:)%tag)
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_heading_(table(5),"B")
      call set_width_(table(5),table(3)%width)
      call set_left_justify_(table(5),TRUE)
      call set_heading_(table(6),"R(A-B)")
      call set_subhead_(table(6),"/A")
      call set_real_precision_(table(6),4)
      call set_width_from_(table(6),ONE)
      call set_heading_(table(7),"R(A.B)")
      call set_subhead_(table(7),"/A")
      call set_real_precision_(table(7),4)
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"Becke")
      call set_subhead_(table(8),"chi")
      call set_real_precision_(table(8),4)
      call set_width_from_(table(8),ONE)
      call set_heading_(table(9),"R(A.B)")
      call set_subhead_(table(9),"promol")
      call set_real_precision_(table(9),4)
      call set_width_from_(table(9),ONE)
      call set_heading_(table(10),"R(A.B)/")
      call set_subhead_(table(10),"promol")
      call set_real_precision_(table(10),4)
      call set_width_from_(table(10),ONE)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)
      do a = 1,size(self)

         ! Get the info
         n_bonds = size(bonds_for(a)%element)
         call create_(strA,n_bonds)
         call create_(strB,n_bonds)
         call create_(labA,n_bonds)
         call create_(labB,n_bonds)
         call create_(dash,n_bonds)
         call create_(dist,n_bonds)
         call create_(radi,n_bonds)
         call create_(rchi,n_bonds)
         call create_(rpro,n_bonds)
         call create_(rtio,n_bonds)
         strA = " "
         dash = "--"
         do n = 1,n_bonds
            b = bonds_for(a)%element(n)
            if (n==1) strA(n) = to_str_(a)
            strB(n) = to_str_(b)
            labA(n) = self(a)%tag
            labB(n) = self(b)%tag
            dist(n) = norm2(self(b)%position-self(a)%position)
            radi(n) = radii_for(a)%element(n)
            rchi(n) = radi(n)/(dist(n)-radi(n))
            rpro(n) = radii_pro(a)%element(n)
            rtio(n) = radi(n)/rpro(n)
         end do
         dist = dist*ANGSTROM_PER_BOHR
         radi = radi*ANGSTROM_PER_BOHR
         rpro = rpro*ANGSTROM_PER_BOHR

         ! Set table data
         call set_values_(table(1),strA)
         call set_values_(table(2),strB)
         call set_values_(table(3),labA)
         call set_values_(table(4),dash)
         call set_values_(table(5),labB)
         call set_values_(table(6),dist)
         call set_values_(table(7),radi)
         call set_values_(table(8),rchi)  ! chi
         call set_values_(table(9),rpro)
         call set_values_(table(10),rtio)

         ! Put body
         call flush_(stdout)
         call put_body_(table,label_rows=FALSE)

         ! Clean
         call clear_columns_(table)
         call destroy_(rtio)
         call destroy_(rpro)
         call destroy_(rchi)
         call destroy_(radi)
         call destroy_(dist)
         call destroy_(dash)
         call destroy_(labB)
         call destroy_(labA)
         call destroy_(strB)
         call destroy_(strA)
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
   end subroutine

   subroutine put_Salvador_radii_table_2(self,bonds_for,radii_for,radii_pro,radii_hir)
   ! Output the Salvador bond length table.
   ! This version compares with the promolecule radii and with the radii
   ! where the two atoms' spherical densities are equal (tfvh).
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds_for
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii_for
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii_pro
      VEC(type(EVEC_REAL_TYPE),:), IN :: radii_hir
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: strA,labA
      VEC(STR(len=STR_SIZE),:), allocatable :: strB,labB
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: dist,radi
      VEC(REAL,:), allocatable :: rpro,rtio
      VEC(REAL,:), allocatable :: rchi,rhir
      INT :: n_bonds,a,n,b
      BIN :: has_unique_labels

      ! Anything to do?
      if (size(self)<2) return
      n_bonds = no_of_bonds_(self,bonds_for)
      if (n_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call text_(stdout,"====================")
      call text_(stdout,"Mayer-Salvador radii")
      call text_(stdout,"====================")
      call flush_(stdout)
      call text_(stdout,". R(A.B) = distance from A to a minimum in the ED between")
      call text_(stdout,"           a pair of 'neighbour' atoms, A and B.")
      call flush_(stdout)
      call text_(stdout,"  The . in R(A.B) isn't a bond critical point, but nearly.")
      call text_(stdout,"  Since it is a minimum in only 1 dimension, not all three.")
      call text_(stdout,"  A -ve R(A.B) means no minimum in the ED was found -- bad.")
      call flush_(stdout)
      call text_(stdout,". Also reported are R(A.B) for the promolecular density, and")
      call text_(stdout,"  R(A.B) where the two atoms' spherical densities are equal (tfvh).")
      call flush_(stdout)
      call text_(stdout,". Two atoms are neighbours if their midpoint is *not* closer")
      call text_(stdout,"  to any third atom. See Salvador & Ramos-Cordoba (2013)")
      call text_(stdout,"  JCP 139 p. 071103 p3 paragraph 3.")
      call flush_(stdout)
      call text_(stdout,". The radii are used to define Topological Fuzzy Voronoi Atoms")
      call text_(stdout,"  (TFVAs) i.e. 'Salvador atoms'.")
      call flush_(stdout)
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"Total no. of neighbours =",n_bonds)
      if (n_bonds>VEC_ATOM_MAX_BONDS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many bonds to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Create table
      call create_(table,11)
      call set_heading_(table(1),"#A")
      call set_width_(table(1),3)
      call set_heading_(table(2),"#B")
      call set_width_(table(2),3)
      call set_heading_(table(3),"A")
      call set_width_from_(table(3),self(:)%tag)
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_heading_(table(5),"B")
      call set_width_(table(5),table(3)%width)
      call set_left_justify_(table(5),TRUE)
      call set_heading_(table(6),"R(A-B)")
      call set_subhead_(table(6),"/A")
      call set_real_precision_(table(6),4)
      call set_width_from_(table(6),ONE)
      call set_heading_(table(7),"R(A.B)")
      call set_subhead_(table(7),"/A")
      call set_real_precision_(table(7),4)
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"Becke")
      call set_subhead_(table(8),"chi")
      call set_real_precision_(table(8),4)
      call set_width_from_(table(8),ONE)
      call set_heading_(table(9),"R(A.B)")
      call set_subhead_(table(9),"promol")
      call set_real_precision_(table(9),4)
      call set_width_from_(table(9),ONE)
      call set_heading_(table(10),"R(A.B)/")
      call set_subhead_(table(10),"promol")
      call set_real_precision_(table(10),4)
      call set_width_from_(table(10),ONE)
      call set_heading_(table(11),"R(A.B)")
      call set_subhead_(table(11),"eq-dens")
      call set_real_precision_(table(11),4)
      call set_width_from_(table(11),ONE)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)
      do a = 1,size(self)

         ! Get the info
         n_bonds = size(bonds_for(a)%element)
         call create_(strA,n_bonds)
         call create_(strB,n_bonds)
         call create_(labA,n_bonds)
         call create_(labB,n_bonds)
         call create_(dash,n_bonds)
         call create_(dist,n_bonds)
         call create_(radi,n_bonds)
         call create_(rchi,n_bonds)
         call create_(rpro,n_bonds)
         call create_(rtio,n_bonds)
         call create_(rhir,n_bonds)
         strA = " "
         dash = "--"
         do n = 1,n_bonds
            b = bonds_for(a)%element(n)
            if (n==1) strA(n) = to_str_(a)
            strB(n) = to_str_(b)
            labA(n) = self(a)%tag
            labB(n) = self(b)%tag
            dist(n) = norm2(self(b)%position-self(a)%position)
            radi(n) = radii_for(a)%element(n)
            rchi(n) = radi(n)/(dist(n)-radi(n))
            rpro(n) = radii_pro(a)%element(n)
            rtio(n) = radi(n)/rpro(n)
            rhir(n) = radii_hir(a)%element(n)
         end do
         dist = dist*ANGSTROM_PER_BOHR
         radi = radi*ANGSTROM_PER_BOHR
         rpro = rpro*ANGSTROM_PER_BOHR
         rhir = rhir*ANGSTROM_PER_BOHR

         ! Set table data
         call set_values_(table(1),strA)
         call set_values_(table(2),strB)
         call set_values_(table(3),labA)
         call set_values_(table(4),dash)
         call set_values_(table(5),labB)
         call set_values_(table(6),dist)
         call set_values_(table(7),radi)
         call set_values_(table(8),rchi)  ! chi
         call set_values_(table(9),rpro)
         call set_values_(table(10),rtio)
         call set_values_(table(11),rhir)

         ! Put body
         call flush_(stdout)
         call put_body_(table,label_rows=FALSE)

         ! Clean
         call clear_columns_(table)
         call destroy_(rhir)
         call destroy_(rtio)
         call destroy_(rpro)
         call destroy_(rchi)
         call destroy_(radi)
         call destroy_(dist)
         call destroy_(dash)
         call destroy_(labB)
         call destroy_(labA)
         call destroy_(strB)
         call destroy_(strA)
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
   end subroutine

   pure function no_of_bonds_1(self,bonds_for) result (res)
   ! Return the number of bonded atoms
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds_for
      INT :: res
      INT :: a
      res = 0
      do a = 1,size(self)
         res = res+size(bonds_for(a)%element)
      end do
   end function


!  ===============
!  Hirshfelds test
!  ===============

   PURE function Hirshfeld_test_0(self,a,b,angstrom) result (res)
   ! Return the Hirshfeld test between atoms "a" and "b"
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      BIN, optional, IN :: angstrom
      REAL :: res
      VEC(REAL,3) :: n
      MAT(REAL,3,3) :: dU
      BIN :: change

      ! Unit vector
      n = self(a)%position-self(b)%position
      call normalize_(n)

      ! Difference in ADPs
      dU = self(a)%ADP_tensor-self(b)%ADP_tensor

      ! H test value
      res = dot_product(n,matmul(dU,n))
      change = FALSE
      if (present(angstrom)) change = angstrom
      if (change) res = res*ANGSTROM_PER_BOHR*ANGSTROM_PER_BOHR
   end function

   PURE function Hirshfeld_test_1(self,a,b,covariance,angstrom) result (res)
   ! Return the Hirshfeld test (and error) between atoms "a" and "b".
   ! The test is the first value, the error the second in res(:).
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: angstrom
      VEC(REAL,2) :: res
      VEC(REAL,3) :: n
      MAT(REAL,3,3) :: delU
      VEC(REAL,18) :: der
      MAT(REAL,18,18) :: cov
      BIN :: change
   ENSURE(a/=b,"VEC{ATOM}:Hirshfeld_test_1 ... Atom indices are equal!")

      ! Unit vector
      n = self(a)%position-self(b)%position
      call normalize_(n)

      ! Difference in ADPs
      delU = self(a)%ADP_tensor-self(b)%ADP_tensor

      ! H test value
      res(1) = dot_product(n,matmul(delU,n))

      ! Covariance
      call put_blocks_to_(covariance,cov,block_dim=9,block_list=[a,b])

      ! Make bond distance error
      call Hirshfeld_test_deriv_(self,a,b,der)
      res(2) = sqrt(dot_product(der,matmul(cov,der)))

      ! Change units?
      change = FALSE
      if (present(angstrom)) change = angstrom
      if (change) res = res*ANGSTROM_PER_BOHR*ANGSTROM_PER_BOHR
   end function

   PURE subroutine Hirshfeld_test_deriv(self,a,b,deriv)
   ! Return the Hirshfeld test derivatives "deriv" wrt all nine pADPs
   ! atoms "A" and "B" as a 2*9=18 vector in pADP order.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      VEC(REAL,18), OUT :: deriv
      VEC(REAL,3) :: n
      VEC(REAL,6) :: dr
      MAT(REAL,3,3) :: delU
      MAT(REAL,3,6) :: dn
      VEC(REAL,12) :: dU
      INT :: i,j,k

      ! Derivatives of n wrt pos & U wrt U^{A,B}
      call Hirshfeld_test_n_deriv_(self,a,b,n,dn)
      call Hirshfeld_test_U_deriv_(self,a,b,dU)

      ! Delta U matrix
      delU = self(a)%ADP_tensor-self(b)%ADP_tensor

      ! Derivative of Hirshfeld test wrt positions
      dr = ZERO
      do i = 1,3
         do j = 1,3
            do k = 1,6
               dr(k) = dr(k)+delU(i,j)*(dn(i,k)*n(j)+n(i)*dn(j,k))
            end do
         end do
      end do

      ! Full derivative
      deriv(1:3) = dr(1:3)
      deriv(4:9) = dU(1:6)
      deriv(10:12) = dr(4:6)
      deriv(13:18) = dU(7:12)
   end subroutine

   PURE subroutine Hirshfeld_test_n_deriv(self,a,b,n,dn)
   ! Return the normalized vector "n" from atoms B to A, and derivs of
   ! each component "dn" wrt position coordinates on atoms A and B as
   ! a 3+3=6 vector in the second index of "dn".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      VEC(REAL,3), OUT :: n
      MAT(REAL,3,6), OUT :: dn
      VEC(REAL,3) :: rab,rab2
      REAL :: dab,dab3
      INT :: i,j

      ! Vector rab 
      rab = self(a)%position-self(b)%position

      ! Normalised vector
      n = rab
      call normalize_(n)

      ! Intermediates
      rab2 = rab*rab
      dab = sqrt(rab2(1)+rab2(2)+rab2(3))
      dab = ONE/dab
      dab3 = dab*dab*dab

      ! Derivative wrt r_A
      dn = ZERO
      do i = 1,3
         dn(i,i) = dab
         do j = 1,3
            dn(i,j) = dn(i,j)-rab(i)*rab(j)*dab3
         end do
      end do

      ! Derivative wrt tor_B 
      dn(:,4:6) = -dn(:,1:3)
   end subroutine

   PURE subroutine Hirshfeld_test_U_deriv(self,a,b,dU)
   ! Return the Hirshfeld test derivatives "dU" wrt all nine ADPs
   ! U^A and U^B on atoms "A" and "B" as a 6+6=12 vector in the usual
   ! order xx, yy, zz, xy, xz,yz.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a,b
      VEC(REAL,12), OUT :: dU
      VEC(REAL,3) :: n

      ! Normalised vector 
      n = self(a)%position-self(b)%position
      call normalize_(n)

      ! Derivative wrt U^A
      dU(1) = n(1)*n(1)
      dU(2) = n(2)*n(2)
      dU(3) = n(3)*n(3)
      dU(4) = n(1)*n(2)
      dU(5) = n(1)*n(3)
      dU(6) = n(2)*n(3)

      ! Derivative wrt to U^B
      dU(7:12) = -dU(1:6)
   end subroutine

   subroutine put_Hirshfeld_test(self)
   ! Output the Hirshfeld test for all atom pairs.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=STR_SIZE),:), allocatable :: IDa,IDb
      MAT(REAL,:,:), allocatable :: tab
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,3) :: n
      VEC(BIN,:), allocatable :: con
      VEC(REAL,:), allocatable :: hab
      MAT(REAL,3,3) :: dU
      REAL :: fac,test
      INT :: dim1,dim2,a,b,i
   DIE_IF(any(self(:)%ADP_axis_system/="cartesian"),"VEC{ATOM}:put_Hirshfeld_test ... ADP axis system must be cartesian")
   DIE_IF(has_zero_ADP_tensor_(self(1)),"VEC{ATOM}:put_Hirshfeld_test ... An ADP tensor is zero!")

      ! Unit conversion for ADP's: A^2!
      fac = ANGSTROM_PER_BOHR
      fac = fac*fac

      ! CIF ADP header
      call flush_(stdout)
      call text_(stdout,"# ==============")
      call text_(stdout,"# Hirshfeld test")
      call text_(stdout,"# ==============")
      call flush_(stdout)
      call text_(stdout,". Values are printed out for all atom pairs")
      call flush_(stdout)
      call text_(stdout,". Units are in Angstrom^2")
      call flush_(stdout)
      call text_(stdout,". The test prints:")
      call flush_(stdout)
      call text_(stdout,"     h_AB = n^T_AB (U^A - U^B) n_AB")
      call text_(stdout,"     where n = r_A - r_B/|r_A - r_B|")
      call flush_(stdout)
      call text_(stdout,"  is the unit vector from B to A")
      call flush_(stdout)

      ! Make the Hirshfeld test matrix (lower triangle only)
      dim1 = size(self)
      dim2 = dim1*(dim1-1)/2
      call create_(tab,dim1,dim1)
      tab = ZERO
      call create_(IDa,dim2)
      call create_(IDb,dim2)
      call create_(hab,dim2)
      call create_(con,dim2)
      i = 0
      do a = 2,size(self)
         do b = 1,a-1

         ! Unit vector
            n = self(a)%position-self(b)%position
            call normalize_(n)

         ! Difference in ADPs
            dU = self(a)%ADP_tensor-self(b)%ADP_tensor

         ! H test value
            test = dot_product(n,matmul(dU,n))*fac

         ! Store labels & H test value
            i = i+1
            IDa(i) = self(a)%tag
            IDb(i) = self(b)%tag
            con(i) = bonded_(self,a,b)
            hab(i) = test
            tab(a,b) = test
         end do
      end do

      ! Table
      call create_(table,4)

      ! Headings
      call set_heading_(table(1),"Atom")
      call set_subhead_(table(1),"A")
      call set_heading_(table(2),"Atom")
      call set_subhead_(table(2),"B")
      call set_heading_(table(3),"Bonded?")
      call set_heading_(table(4),"H. Test")
      call set_subhead_(table(4),"  /A^2")

      ! Set data
      call set_values_(table(1),IDa)
      call set_values_(table(2),IDb)
      call set_values_(table(3),con)
      call set_values_(table(4),hab)

      ! Put tables
      call put_(table)

      ! Clean
      call destroy_(table)
      call destroy_(hab)
      call destroy_(con)
      call destroy_(IDb)
      call destroy_(IDa)

      ! Full triangle
      call flush_(stdout)
      call text_(stdout,"Full triangle of Hirshfeld test values:")
      call flush_(stdout)
      call put_(stdout,tab)

      ! Clean
      call destroy_(tab)
   end subroutine

   subroutine put_bonds_and_Hirshfeld_test(self,cov_R,cov_pADP)
   ! Output the Hirshfeld test table with errors, given the
   ! "cov_R" position covariance matrix and the  "cov_pADP" pADP
   ! covariance matrix. Set "paretheses" FALSE if you want errors
   ! without them.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov_R,cov_pADP
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: no_of_bonds,n_kind,k1,k2,n_bonds
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist,derr,test,terr
      MAT(INT,:,:), allocatable :: pair
      BIN :: has_unique_labels
   DIE_IF(self(1)%pos_axis_system/="cartesian","VEC{ATOM}:put_bonds_and_Hirshfeld_test ... axis system must be cartesian")
   DIE_IF(any(self(:)%ADP_axis_system/="cartesian"),"VEC{ATOM}:put_bonds_and_Hirshfeld_test ... ADP axis system must be cartesian")
   DIE_IF(has_zero_ADP_tensor_(self(1)),"VEC{ATOM}:put_bonds_and_Hirshfeld_test ... An ADP tensor is zero!")

      ! Anything to do?
      if (size(self)<2) return
      no_of_bonds = no_of_bonds_(self)
      if (no_of_bonds<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"=============================")
      call text_(stdout,"Bond lengths & Hirshfeld test")
      call text_(stdout,"=============================")
      call flush_(stdout)
      call text_(stdout,". The bond-detection criteria is the same as used")
      call text_(stdout,"  by the Cambridge Structural Database (CSD).")
      if (NOT has_unique_labels) then
         call flush_(stdout)
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number tags are used.")
      end if
      call flush_(stdout)
      call text_(stdout,". Hirshfeld test values are defined by:")
      call flush_(stdout)
      call text_(stdout,"     h_AB = n^T_AB (U^A - U^B) n_AB")
      call flush_(stdout)
      call text_(stdout,"  for bonded atom pairs A--B, where n = (r_A-r_B)/|r_A-r_B|")
      call text_(stdout,"  is the unit vector from B to A. The units are Angstrom^2.")
      call flush_(stdout)
      call show_(stdout,"No. of bonds =",no_of_bonds)
      if (no_of_bonds>VEC_ATOM_MAX_BONDS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many bonds to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,8)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_left_justify_(table(3),TRUE)
      call set_heading_(table(4),"A")
      call set_width_from_(table(4),self%tag)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"--")
      call set_width_(table(5),2)
      call set_heading_(table(6),"B")
      call set_width_(table(6),table(4)%width)
      call set_left_justify_(table(6),TRUE)
      call set_heading_(table(7),"Bond distance")
      call set_subhead_(table(7),"R(A--B) /A")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"Hirshfeld Test")
      call set_subhead_(table(8),"/A^2")

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            call get_bonded_Hirshfeld_test_(self,pair,dist,derr,test,terr,Z_list,k1,k2,cov_R,cov_pADP,angstrom=TRUE)

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(terr)
               call destroy_(test)
               call destroy_(derr)
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Element symbols
            call create_(symbols,n_bonds,2)
            symbols = " "
            symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
            symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self(pair(:,1))%tag
            labels(:,2) = self(pair(:,2))%tag

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "--"
            dsh1 = "  "
            dsh1(1) = "--"

         ! Set table data
            call set_values_(table(1),symbols(:,1))
            call set_values_(table(2),dsh1)
            call set_values_(table(3),symbols(:,2))
            call set_values_(table(4),labels(:,1))
            call set_values_(table(5),dash)
            call set_values_(table(6),labels(:,2))
            call set_real_precision_(table(7),stdout%style%real_precision)  ! Must do, for every block
            call set_values_and_errors_(table(7),dist,derr)
            call set_values_and_errors_(table(8),test,terr)

         ! Put body
            call flush_(stdout)
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(labels)
            call destroy_(symbols)
            call destroy_(test)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine get_bonded_Hirshfeld_test_0(self,pair,dist,test,Z_list,k1,k2,angstrom,range_factor)
   ! Get the Nx2 bonded-atom "pair"s, and the distances "dist" between
   ! them. The pair have "Z_list" groups "k1" and "k2", respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: pair
      VEC(REAL,:), allocatable, OUT :: dist,test
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2
      BIN, optional, IN :: angstrom
      REAL, optional, IN :: range_factor
      INT :: i1,i2,a1,a2,n
   ENSURE(k1<=k2,"VEC{ATOM}:get_bonded_Hirshfeld_test_0 ... k2 is smaller than k1")

      ! How many pairs, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,range_factor)) cycle

            ! Count
            n = n+1
         end do
      end do

      ! Allocate
      call create_(pair,n,2)
      call create_(dist,n)
      call create_(test,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,range_factor)) cycle

            ! Add the data
            n = n+1
            pair(n,1) = a1
            pair(n,2) = a2
            dist(n) = bond_distance_(self,a1,a2,angstrom)
            test(n) = Hirshfeld_test_(self,a1,a2,angstrom)
         end do
      end do
   end subroutine

   subroutine get_bonded_Hirshfeld_test_1(self,pair,dist,derr,test,terr,Z_list,k1,k2,cov_R,cov_pADP,angstrom,range_factor)
   ! Get the Nx2 bonded-atom "pair"s, and the distances "dist" between
   ! them. The pair have "Z_list" groups "k1" and "k2", respectively.
   ! The position and pADP covariances are "cov_R", "cov_pADP".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: pair
      VEC(REAL,:), allocatable, OUT :: dist,derr,test,terr
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2
      MAT(REAL,:,:), IN :: cov_R,cov_pADP
      BIN, optional, IN :: angstrom
      REAL, optional, IN :: range_factor
      INT :: i1,i2,a1,a2,n
      VEC(REAL,2) :: d,t
   ENSURE(k1<=k2,"VEC{ATOM}:get_bonded_Hirshfeld_test_1 ... k2 is smaller than k1")

      ! How many pairs, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,range_factor)) cycle

            ! Count
            n = n+1
         end do
      end do

      ! Allocate
      call create_(pair,n,2)
      call create_(dist,n)
      call create_(derr,n)
      call create_(test,n)
      call create_(terr,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (k1==k2 AND a2<=a1) cycle
            if (NOT bonded_(self,a1,a2,range_factor)) cycle

            ! Get values
            d = bond_distance_(self,a1,a2,cov_R,angstrom)
            t = Hirshfeld_test_(self,a1,a2,cov_pADP,angstrom)

            ! Add the data
            n = n+1
            pair(n,1) = a1
            pair(n,2) = a2
            dist(n) = d(1)
            derr(n) = d(2)
            test(n) = t(1)
            terr(n) = t(2)
         end do
      end do
   end subroutine


!  ===========
!  Bond angles
!  ===========

   subroutine put_bond_angle_table_0(self)
   ! Output the bond angle table.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      MAT(INT,:,:), allocatable :: triple
      VEC(REAL,:), allocatable :: angle
      STR(len=STR_SIZE) :: val
      INT :: no_of_angles,n_kind,k1,k2,k3,n_angles,w
      BIN :: has_unique_labels
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_bond_angle_table_0 ... axis system must be cartesian")
      if (size(self)<3) return
      no_of_angles = no_of_angles_(self)
      if (no_of_angles<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===========")
      call text_(stdout,"Bond Angles")
      call text_(stdout,"===========")
      call flush_(stdout)
      call text_(stdout,". Atom B of Angle(A--B--C) is the middle atom.")
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number ID tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of bond angles =",no_of_angles)
      if (no_of_angles>VEC_ATOM_MAX_ANGLES_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many angles to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,11)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_column_spacing_(table(3),0)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),chemical_symbols_(self))
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"C")
      call set_width_(table(5),table(2)%width)
      call set_left_justify_(table(5),TRUE)
      call set_heading_(table(6),"A")
      call set_width_from_(table(6),self(angle_outer_atoms_(self))%tag)
      call set_column_spacing_(table(7),0)
      call set_heading_(table(7),"--")
      call set_width_(table(7),2)
      call set_column_spacing_(table(8),0)
      call set_heading_(table(8),"B")
      call set_width_from_(table(8),self(angle_center_atoms_(self))%tag)
      w = table(8)%width
      val = "B"
      call center_justify_(val(1:w))
      call replace_all_(val(1:w)," ","-")
      call set_heading_(table(8),val(1:w))
      call set_column_spacing_(table(9),0)
      call set_heading_(table(9),"--")
      call set_width_(table(9),2)
      call set_column_spacing_(table(10),0)
      call set_heading_(table(10),"C")
      call set_width_from_(table(10),self(angle_outer_atoms_(self))%tag)
      call set_left_justify_(table(10),TRUE)
      call set_heading_(table(11),"Angle(A--B--C)")
      call set_subheading_(table(11),"/degree")
      call set_width_from_(table(11),-180d0)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = k1,n_kind

         ! Get the bonded atoms per kind
               call get_bonded_angles_(self,triple,angle,Z_list,k1,k2,k3)
               angle = angle*DEGREE_PER_RADIAN

         ! No of bonds in this class
               n_angles = size(angle)
               if (n_angles==0) then
                  call destroy_(angle)
                  call destroy_(triple)
                  cycle
               end if

         ! Element symbols
               call create_(symbols,n_angles,3)
               symbols = " "
               symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
               symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))
               symbols(1,3) = chemical_symbol_(self(Z_list(k3)%element(1)))

         ! Labels
               call create_(labels,n_angles,3)
               labels(:,1) = self(triple(:,1))%tag
               labels(:,2) = self(triple(:,2))%tag
               labels(:,3) = self(triple(:,3))%tag
               call pad_with_(labels(:,2),"-",width=w,prepad=TRUE)

         ! Dashes
               call create_(dash,n_angles)
               call create_(dsh1,n_angles)
               dash = "--"
               dsh1 = "  "
               dsh1(1) = "--"

         ! Set table data
               call set_values_(table(1),symbols(:,1))
               call set_values_(table(2),dsh1)
               call set_values_(table(3),symbols(:,2))
               call set_values_(table(4),dsh1)
               call set_values_(table(5),symbols(:,3))
               call set_values_(table(6),labels(:,1))
               call set_values_(table(7),dash)
               call set_values_(table(8),labels(:,2))
               call set_values_(table(9),dash)
               call set_values_(table(10),labels(:,3))
               call set_values_(table(11),angle)

         ! Put body
               call flush_(stdout)
               call put_body_(table,label_rows=FALSE)

         ! Clean
               call clear_columns_(table)
               call destroy_(dsh1)
               call destroy_(dash)
               call destroy_(labels)
               call destroy_(symbols)
               call destroy_(angle)
               call destroy_(triple)
            end do
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_bond_angle_table_1(self,covariance,parentheses)
   ! Output the bond angle table with errors given the "covariance"
   ! matrix between all atomic positions. Set "parentheses" FALSE if
   ! you don't want them for errors.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: parentheses
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: angle,esds
      MAT(INT,:,:), allocatable :: triple
      STR(len=STR_SIZE) :: val
      INT :: no_of_angles,n_kind,k1,k2,k3,n_angles,w
      BIN :: use_brackets,has_unique_labels
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_bond_angle_table_1 ... axis system must be cartesian")
      if (size(self)<3) return
      no_of_angles = no_of_angles_(self)
      if (no_of_angles<1) return

      ! Parentheses
      use_brackets = TRUE
      if (present(parentheses)) use_brackets = parentheses

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===========")
      call text_(stdout,"Bond Angles")
      call text_(stdout,"===========")
      call flush_(stdout)
      call text_(stdout,"Atom B of Angle(A--B--C) is the middle atom.")
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number ID tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of bond angles =",no_of_angles_(self))
      if (no_of_angles>VEC_ATOM_MAX_ANGLES_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many angles to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      if (use_brackets) then; call create_(table,11)
      else; call create_(table,12)
      end if

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self(:)))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_column_spacing_(table(3),0)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),chemical_symbols_(self(:)))
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"C")
      call set_width_(table(5),table(2)%width)
      call set_left_justify_(table(5),TRUE)
      call set_heading_(table(6),"A")
      call set_width_from_(table(6),self(angle_outer_atoms_(self(:)))%tag)
      call set_column_spacing_(table(7),0)
      call set_heading_(table(7),"--")
      call set_width_(table(7),2)
      call set_column_spacing_(table(8),0)
      call set_heading_(table(8),"B")
      call set_width_from_(table(8),self(angle_center_atoms_(self(:)))%tag)
      w = table(8)%width
      val = "B"
      call center_justify_(val(1:w))
      call replace_all_(val(1:w)," ","-")
      call set_heading_(table(8),val(1:w))
      call set_column_spacing_(table(9),0)
      call set_heading_(table(9),"--")
      call set_width_(table(9),2)
      call set_column_spacing_(table(10),0)
      call set_heading_(table(10),"C")
      call set_width_from_(table(10),self(angle_outer_atoms_(self(:)))%tag)
      call set_left_justify_(table(10),TRUE)
      call set_heading_(table(11),"Angle(A--B--C)")
      call set_subheading_(table(11),"/degree")
      call set_width_from_(table(11),ONE)
      if (NOT use_brackets) then
         call set_heading_(table(12),"Error")
         call set_width_from_(table(12),ONE)
      end if

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = k1,n_kind

         ! Get the bonded atoms per kind
               call get_bonded_angles_(self,triple,angle,esds,Z_list,k1,k2,k3,covariance)
               angle = angle*DEGREE_PER_RADIAN
               esds = esds*DEGREE_PER_RADIAN

         ! No of bonds in this class
               n_angles = size(angle)
               if (n_angles==0) then
                  call destroy_(esds)
                  call destroy_(angle)
                  call destroy_(triple)
                  cycle
               end if

         ! Element symbols
               call create_(symbols,n_angles,3)
               symbols = " "
               symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
               symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))
               symbols(1,3) = chemical_symbol_(self(Z_list(k3)%element(1)))

         ! Labels
               call create_(labels,n_angles,3)
               labels(:,1) = self(triple(:,1))%tag
               labels(:,2) = self(triple(:,2))%tag
               labels(:,3) = self(triple(:,3))%tag
               call pad_with_(labels(:,2),"-",width=w,prepad=TRUE)

         ! Dashes
               call create_(dash,n_angles)
               call create_(dsh1,n_angles)
               dash = "--"
               dsh1 = "  "
               dsh1(1) = "--"

         ! Set table data
               call set_values_(table(1),symbols(:,1))
               call set_values_(table(2),dsh1)
               call set_values_(table(3),symbols(:,2))
               call set_values_(table(4),dsh1)
               call set_values_(table(5),symbols(:,3))
               call set_values_(table(6),labels(:,1))
               call set_values_(table(7),dash)
               call set_values_(table(8),labels(:,2))
               call set_values_(table(9),dash)
               call set_values_(table(10),labels(:,3))
               if (use_brackets) then
                  call set_real_precision_(table(11),stdout%style%real_precision)  ! Must do, for every block
                  call set_values_and_errors_(table(11),angle,esds)
               else
                  call set_values_(table(11),angle)
                  call set_values_(table(12),esds)
               end if

         ! Put body
               call flush_(stdout)
               call put_body_(table,label_rows=FALSE)

         ! Clean
               call clear_columns_(table)
               call destroy_(dsh1)
               call destroy_(dash)
               call destroy_(labels)
               call destroy_(symbols)
               call destroy_(esds)
               call destroy_(angle)
               call destroy_(triple)
            end do
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine get_bonded_angles_0(self,triple,angle,Z_list,k1,k2,k3)
   ! Get the Nx3 bonded-atom "triple"s, and the "angles" between them.
   ! The triple has "Z_list" groups of "k1", "k2", and "k3", resp.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: triple
      VEC(REAL,:), allocatable, OUT :: angle
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2,k3
      INT :: i1,i2,i3,a1,a2,a3,n
   ENSURE(k1<=k3,"VEC{ATOM}:get_bonded_angles_0 ... k3 is smaller than k1")

      ! How many triples, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               if (k1==k3 AND a3<a1) cycle

               ! Count
               n = n+1
            end do
         end do
      end do

      ! Allocate
      call create_(triple,n,3)
      call create_(angle,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               if (k1==k3 AND a3<a1) cycle

               ! Add data
               n = n+1
               triple(n,1) = a1
               triple(n,2) = a2
               triple(n,3) = a3
               angle(n) = bond_angle_(self,a1,a2,a3)
            end do
         end do
      end do
   end subroutine

   subroutine get_bonded_angles_1(self,triple,angle,esds,Z_list,k1,k2,k3,covariance)
   ! Get the bonded-atom "triple"s, and the "angles" between them, and
   ! the associated "esds". The triple has "Z_list" groups of "k1",
   ! "k2", and "k3", respectively. The "covariance" matrix is between
   ! *all* atom coordinates.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: triple
      VEC(REAL,:), allocatable, OUT :: angle,esds
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2,k3
      MAT(REAL,:,:), IN :: covariance
      INT :: i1,i2,i3,a1,a2,a3,n
      REAL :: err
      VEC(REAL,9) :: der
      MAT(REAL,9,9) :: cov
   ENSURE(k1<=k3,"VEC{ATOM}:get_bonded_angles_1 ... k3 is smaller than k1")
   ENSURE(is_square_(covariance),"VEC{ATOM}:get_bonded_angles_1 ... covariance: not square")
   ENSURE(size(covariance,1)==3*size(self),"VEC{ATOM}:get_bonded_angles_1 ... covariance: wrong size")

      ! How many triples, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               if (k1==k3 AND a3<a1) cycle

               ! Count
               n = n+1
            end do
         end do
      end do

      ! Allocate
      call create_(triple,n,3)
      call create_(angle,n)
      call create_(esds,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               if (k1==k3 AND a3<a1) cycle

               ! Extract covariances for atoms a, b, c
               call put_blocks_to_(covariance,cov,block_dim=3,block_list=[a1,a2,a3])

               ! Make error
               call bond_angle_deriv_(self,a1,a2,a3,der)
               err = dot_(cov,der,der)
               if (err<ZERO) then
                  err = ZERO
               end if
               err = sqrt(err)

               ! Add data
               n = n+1
               triple(n,1) = a1
               triple(n,2) = a2
               triple(n,3) = a3
               angle(n) = bond_angle_(self,a1,a2,a3)
               esds(n) = err
            end do
         end do
      end do
   end subroutine

   subroutine put_bond_angle_with_error(self,a1,a2,a3,covariance)
   ! Put the bond angle between atoms "a1" ... "a3" with error.  The
   ! "covariance" matrix is for *all* atom coordinates.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a1,a2,a3
      MAT(REAL,:,:), IN :: covariance
      REAL :: err,ang
      VEC(REAL,9) :: der
      MAT(REAL,9,9) :: cov
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_bond_angle_with_error ... axis system must be cartesian")
   ENSURE(is_square_(covariance),"VEC{ATOM}:put_bond_angle_with_error ... covariance: not square")
   ENSURE(size(covariance,1)==3*size(self),"VEC{ATOM}:put_bond_angle_with_error ... covariance: wrong size")

      ! Extract covariances for atom a, b, c positions
      call put_blocks_to_(covariance,cov,block_dim=3,block_list=[a1,a2,a3])

      ! Make error
      call bond_angle_deriv_(self,a1,a2,a3,der)
      err = dot_(cov,der,der)
      err = sqrt(err)
      call convert_to_(err,"degree")

      ! Add the data
      ang = bond_angle_(self,a1,a2,a3)
      call convert_to_(ang,"degree")

      ! Output
      call flush_(stdout)
      call text_(stdout,"=====================")
      call text_(stdout,"Bond angle with error")
      call text_(stdout,"=====================")
      call show_(stdout,"Atom 1        =",self(a1)%tag)
      call show_(stdout,"Atom 2        =",self(a2)%tag)
      call show_(stdout,"Atom 3        =",self(a3)%tag)
      call show_(stdout,"Bond angle    =",ang)
      call show_(stdout,"Error         =",err)
   end subroutine


!  ================
!  Torsions angles
!  ================

   subroutine put_torsion_angle_table_0(self)
   ! Output the torsion angle table.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      MAT(INT,:,:), allocatable :: quad
      VEC(REAL,:), allocatable :: angle
      STR(len=STR_SIZE) :: val
      INT :: no_of_torsions,n_kind,k1,k2,k3,k4,n_angles,w
      BIN :: has_unique_labels
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_torsion_angle_table_0 ... axis system must be cartesian")
      if (size(self)<4) return
      no_of_torsions = no_of_torsion_angles_(self)
      if (no_of_torsions<1) return

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"==============")
      call text_(stdout,"Torsion angles")
      call text_(stdout,"==============")
      call flush_(stdout)
      call text_(stdout,". The angle shown is the one that looking down B->C")
      call text_(stdout,"  i.e. it's the one between r(A)-r(B) and r(D)-r(C)")
      call flush_(stdout)
      call text_(stdout,". The angle is +ve if r(D)-r(C) is clockwise of r(A)-r(B)")
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number ID tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of torsion angles =",no_of_torsions)
      if (no_of_torsions>VEC_ATOM_MAX_TORSIONS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many angles to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Get atom Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,15)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self(:)))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_column_spacing_(table(3),0)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),chemical_symbols_(self(:)))
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"C")
      call set_width_from_(table(5),chemical_symbols_(self(:)))
      call set_column_spacing_(table(6),0)
      call set_heading_(table(6),"--")
      call set_width_(table(6),2)
      call set_column_spacing_(table(7),0)
      call set_heading_(table(7),"D")
      call set_width_(table(7),table(2)%width)
      call set_left_justify_(table(7),TRUE)
      call set_heading_(table(8),"A")
      call set_width_from_(table(8),self(torsion_angle_1st_atoms_(self(:)))%tag)
      call set_column_spacing_(table(9),0)
      call set_heading_(table(9),"--")
      call set_width_(table(9),2)
      call set_column_spacing_(table(10),0)
      call set_heading_(table(10),"B")
      call set_width_from_(table(10),self(torsion_angle_2nd_atoms_(self(:)))%tag)
      w = table(10)%width
      val = "B"
      call center_justify_(val(1:w))
      call replace_all_(val(1:w)," ","-")
      call set_heading_(table(10),val(1:w))
      call set_column_spacing_(table(11),0)
      call set_heading_(table(11),"--")
      call set_width_(table(11),2)
      call set_column_spacing_(table(12),0)
      call set_heading_(table(12),"C")
      call set_width_from_(table(12),self(torsion_angle_3rd_atoms_(self(:)))%tag)
      w = table(12)%width
      val = "C"
      call center_justify_(val(1:w))
      call replace_all_(val(1:w)," ","-")
      call set_heading_(table(12),val(1:w))
      call set_column_spacing_(table(13),0)
      call set_heading_(table(13),"--")
      call set_width_(table(13),2)
      call set_column_spacing_(table(14),0)
      call set_heading_(table(14),"D")
      call set_width_from_(table(14),self(torsion_angle_4th_atoms_(self(:)))%tag)
      call set_left_justify_(table(14),TRUE)
      call set_heading_(table(15),"Angle(A--B--C--D)")
      call set_subheading_(table(15),"/degree")
      call set_width_from_(table(15),-180d0)

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = 1,n_kind
               do k4 = 1,n_kind

         ! Get the bonded atoms per kind
                  call get_bonded_torsions_(self,quad,angle,Z_list,k1,k2,k3,k4)
                  angle = angle*DEGREE_PER_RADIAN

         ! No of bonds in this class
                  n_angles = size(angle)
                  if (n_angles==0) then
                     call destroy_(angle)
                     call destroy_(quad)
                     cycle
                  end if

         ! Element symbols
                  call create_(symbols,n_angles,4)
                  symbols = " "
                  symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
                  symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))
                  symbols(1,3) = chemical_symbol_(self(Z_list(k3)%element(1)))
                  symbols(1,4) = chemical_symbol_(self(Z_list(k4)%element(1)))

         ! Labels
                  call create_(labels,n_angles,4)
                  labels(:,1) = self(quad(:,1))%tag
                  labels(:,2) = self(quad(:,2))%tag
                  labels(:,3) = self(quad(:,3))%tag
                  labels(:,4) = self(quad(:,4))%tag
                  call pad_with_(labels(:,2),"-",width=w,prepad=TRUE)
                  call pad_with_(labels(:,3),"-",width=w,prepad=TRUE)

         ! Dashes
                  call create_(dash,n_angles)
                  call create_(dsh1,n_angles)
                  dash = "--"
                  dsh1 = "  "
                  dsh1(1) = "--"

         ! Set table data
                  call set_values_(table(1),symbols(:,1))
                  call set_values_(table(2),dsh1)
                  call set_values_(table(3),symbols(:,2))
                  call set_values_(table(4),dsh1)
                  call set_values_(table(5),symbols(:,3))
                  call set_values_(table(6),dsh1)
                  call set_values_(table(7),symbols(:,4))
                  call set_values_(table(8),labels(:,1))
                  call set_values_(table(9),dash)
                  call set_values_(table(10),labels(:,2))
                  call set_values_(table(11),dash)
                  call set_values_(table(12),labels(:,3))
                  call set_values_(table(13),dash)
                  call set_values_(table(14),labels(:,4))
                  call set_values_(table(15),angle)

         ! Put body
                  call flush_(stdout)
                  call put_body_(table,label_rows=FALSE)

         ! Clean
                  call clear_columns_(table)
                  call destroy_(dsh1)
                  call destroy_(dash)
                  call destroy_(labels)
                  call destroy_(symbols)
                  call destroy_(angle)
                  call destroy_(quad)
               end do
            end do
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_torsion_angle_table_1(self,covariance,parentheses)
   ! Output the torsion angle table with errors given the "covariance"
   ! matrix between all atomic positions. Set "parentheses" FALSE if
   ! ypou don't want them for errors.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: parentheses
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: angle,esds
      MAT(INT,:,:), allocatable :: quad
      INT :: no_of_torsions,n_kind,k1,k2,k3,k4,n_angles,w
      BIN :: use_brackets,has_unique_labels
      STR(len=STR_SIZE) :: val
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_torsion_angle_table_1 ... axis system must be cartesian")
      if (size(self)<4) return
      no_of_torsions = no_of_torsion_angles_(self)
      if (no_of_torsions<1) return

      ! Parentheses
      use_brackets = TRUE
      if (present(parentheses)) use_brackets = parentheses

      ! Has unique labels?
      has_unique_labels = has_unique_labels_(self)

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"==============")
      call text_(stdout,"Torsion angles")
      call text_(stdout,"==============")
      call flush_(stdout)
      call text_(stdout,"The angle shown is the one that looking down B->C")
      call text_(stdout,"i.e. it's the one between r(A)-r(B) and r(D)-r(C).")
      call flush_(stdout)
      call text_(stdout,"The angle is +ve if r(D)-r(C) is clockwise of r(A)-r(B).")
      call flush_(stdout)
      if (NOT has_unique_labels) then
         call text_(stdout,". Unique atom labels were not set for each atom.")
         call text_(stdout,"  Therefore, unique symbol-number ID tags are used.")
      end if
      call flush_(stdout)
      call show_(stdout,"No. of torsion angles =",no_of_torsions)
      if (no_of_torsions>VEC_ATOM_MAX_TORSIONS_IN_TABLE) then
         call flush_(stdout)
         call text_(stdout,"There are too many angles to display in a table.")
         return
      end if
      call flush_(stdout)

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      if (use_brackets) then; call create_(table,15)
      else; call create_(table,16)
      end if

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),chemical_symbols_(self(:)))
      call set_column_spacing_(table(2),0)
      call set_heading_(table(2),"--")
      call set_width_(table(2),2)
      call set_column_spacing_(table(3),0)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),chemical_symbols_(self(:)))
      call set_column_spacing_(table(4),0)
      call set_heading_(table(4),"--")
      call set_width_(table(4),2)
      call set_column_spacing_(table(5),0)
      call set_heading_(table(5),"C")
      call set_width_from_(table(5),chemical_symbols_(self(:)))
      call set_column_spacing_(table(6),0)
      call set_heading_(table(6),"--")
      call set_width_(table(6),2)
      call set_column_spacing_(table(7),0)
      call set_heading_(table(7),"D")
      call set_width_(table(7),table(2)%width)
      call set_left_justify_(table(7),TRUE)
      call set_heading_(table(8),"A")
      call set_width_from_(table(8),self(torsion_angle_1st_atoms_(self(:)))%tag)
      call set_column_spacing_(table(9),0)
      call set_heading_(table(9),"--")
      call set_width_(table(9),2)
      call set_column_spacing_(table(10),0)
      call set_heading_(table(10),"B")
      call set_width_from_(table(10),self(torsion_angle_2nd_atoms_(self(:)))%tag)
      w = table(10)%width
      val = "B"
      call center_justify_(val(1:w))
      call replace_all_(val(1:w)," ","-")
      call set_heading_(table(10),val(1:w))
      call set_column_spacing_(table(11),0)
      call set_heading_(table(11),"--")
      call set_width_(table(11),2)
      call set_column_spacing_(table(12),0)
      call set_heading_(table(12),"C")
      call set_width_from_(table(12),self(torsion_angle_3rd_atoms_(self(:)))%tag)
      w = table(12)%width
      val = "C"
      call center_justify_(val(1:w))
      call replace_all_(val(1:w)," ","-")
      call set_heading_(table(12),val(1:w))
      call set_column_spacing_(table(13),0)
      call set_heading_(table(13),"--")
      call set_width_(table(13),2)
      call set_column_spacing_(table(14),0)
      call set_heading_(table(14),"D")
      call set_width_from_(table(14),self(torsion_angle_4th_atoms_(self(:)))%tag)
      call set_left_justify_(table(14),TRUE)
      call set_heading_(table(15),"Angle(A--B--C--D)")
      call set_subheading_(table(15),"/degree")
      call set_width_from_(table(15),ONE)
      if (NOT use_brackets) then
         call set_heading_(table(16),"Error")
         call set_width_from_(table(16),ONE)
      end if

      ! Put out the headings
      call put_headings_(table,label_rows=FALSE,no_flush=TRUE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = 1,n_kind
               do k4 = 1,n_kind

         ! Get the bonded atoms per kind
                  call get_bonded_torsions_(self,quad,angle,esds,Z_list,k1,k2,k3,k4,covariance)
                  angle = angle*DEGREE_PER_RADIAN
                  esds = esds*DEGREE_PER_RADIAN

         ! No of bonds in this class
                  n_angles = size(angle)
                  if (n_angles==0) then
                     call destroy_(esds)
                     call destroy_(angle)
                     call destroy_(quad)
                     cycle
                  end if

         ! Element symbols
                  call create_(symbols,n_angles,4)
                  symbols = " "
                  symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
                  symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))
                  symbols(1,3) = chemical_symbol_(self(Z_list(k3)%element(1)))
                  symbols(1,4) = chemical_symbol_(self(Z_list(k4)%element(1)))

         ! Labels
                  call create_(labels,n_angles,4)
                  labels(:,1) = self(quad(:,1))%tag
                  labels(:,2) = self(quad(:,2))%tag
                  labels(:,3) = self(quad(:,3))%tag
                  labels(:,4) = self(quad(:,4))%tag
                  call pad_with_(labels(:,2),"-",width=w,prepad=TRUE)
                  call pad_with_(labels(:,3),"-",width=w,prepad=TRUE)

         ! Dashes
                  call create_(dash,n_angles)
                  call create_(dsh1,n_angles)
                  dash = "--"
                  dsh1 = "  "
                  dsh1(1) = "--"

         ! Set table data
                  call set_values_(table(1),symbols(:,1))
                  call set_values_(table(2),dsh1)
                  call set_values_(table(3),symbols(:,2))
                  call set_values_(table(4),dsh1)
                  call set_values_(table(5),symbols(:,3))
                  call set_values_(table(6),dsh1)
                  call set_values_(table(7),symbols(:,4))
                  call set_values_(table(8),labels(:,1))
                  call set_values_(table(9),dash)
                  call set_values_(table(10),labels(:,2))
                  call set_values_(table(11),dash)
                  call set_values_(table(12),labels(:,3))
                  call set_values_(table(13),dash)
                  call set_values_(table(14),labels(:,4))
                  if (use_brackets) then
                     call set_real_precision_(table(15),stdout%style%real_precision)  ! Must do, for every block
                     call set_values_and_errors_(table(15),angle,esds)
                  else
                     call set_values_(table(15),angle)
                     call set_values_(table(16),esds)
                  end if

         ! Put body
                  call flush_(stdout)
                  call put_body_(table,label_rows=FALSE)

         ! Put spacer if not last block
                  if (NOT (k1==n_kind AND k2==n_kind AND k3==n_kind AND k4==n_kind)) call flush_(stdout)

         ! Clean
                  call clear_columns_(table)
                  call destroy_(dsh1)
                  call destroy_(dash)
                  call destroy_(labels)
                  call destroy_(symbols)
                  call destroy_(esds)
                  call destroy_(angle)
                  call destroy_(quad)
               end do
            end do
         end do
      end do

      ! Table footer
      call put_footer_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine get_bonded_torsions_0(self,quad,angle,Z_list,k1,k2,k3,k4)
   ! Get the bonded-atom "quad"ruples, and the "angles" between them.
   ! The quadruple has "Z_list" groups of "k1" ... "k4", respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: quad
      VEC(REAL,:), allocatable, OUT :: angle
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2,k3,k4
      INT :: i1,i2,i3,i4,a1,a2,a3,a4,n

      ! How many quadruples, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do i4 = 1,size(Z_list(k4)%element)
                  a4 = Z_list(k4)%element(i4)
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle

                  ! Count
                  n = n+1
               end do
            end do
         end do
      end do

      ! Allocate
      call create_(quad,n,4)
      call create_(angle,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do i4 = 1,size(Z_list(k4)%element)
                  a4 = Z_list(k4)%element(i4)
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle

                  ! Add data
                  n = n+1
                  quad(n,1) = a1
                  quad(n,2) = a2
                  quad(n,3) = a3
                  quad(n,4) = a4
                  angle(n) = torsion_angle_(self,a1,a2,a3,a4)
               end do
            end do
         end do
      end do
   end subroutine

   subroutine get_bonded_torsions_1(self,quad,angle,esds,Z_list,k1,k2,k3,k4,covariance)
   ! Get the bonded-atom "quad"ruples, and the "angles" between them.
   ! The quadruple has "Z_list" groups of "k1" ... "k4", respectively.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(INT,:,:), allocatable, OUT :: quad
      VEC(REAL,:), allocatable, OUT :: angle,esds
      VEC(type(EVEC_INT_TYPE),:), IN :: Z_list
      INT, IN :: k1,k2,k3,k4
      MAT(REAL,:,:), IN :: covariance
      INT :: i1,i2,i3,i4,a1,a2,a3,a4,n
      REAL :: err
      VEC(REAL,12) :: der
      MAT(REAL,12,12) :: cov
   ENSURE(is_square_(covariance),"VEC{ATOM}:get_bonded_torsions_1 ... covariance: not square")
   ENSURE(size(covariance,1)==3*size(self),"VEC{ATOM}:get_bonded_torsions_1 ... covariance: wrong size")

      ! How many quadruples, n, are there?
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do i4 = 1,size(Z_list(k4)%element)
                  a4 = Z_list(k4)%element(i4)
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle

                  ! Count
                  n = n+1
               end do
            end do
         end do
      end do

      ! Allocate
      call create_(quad,n,4)
      call create_(angle,n)
      call create_(esds,n)

      ! Make the list
      n = 0
      do i1 = 1,size(Z_list(k1)%element)
         a1 = Z_list(k1)%element(i1)
         do i2 = 1,size(Z_list(k2)%element)
            a2 = Z_list(k2)%element(i2)
            if (a2==a1) cycle
            if (NOT bonded_(self,a1,a2)) cycle
            do i3 = 1,size(Z_list(k3)%element)
               a3 = Z_list(k3)%element(i3)
               if (a3==a1 OR a3==a2) cycle
               if (NOT bonded_(self,a2,a3)) cycle
               do i4 = 1,size(Z_list(k4)%element)
                  a4 = Z_list(k4)%element(i4)
                  if (a4==a1 OR a4==a2 OR a4==a3) cycle
                  if (NOT bonded_(self,a3,a4)) cycle
                  if (a4<a1) cycle

                  ! Extract covariances for atoms a, b, c
                  call put_blocks_to_(covariance,cov,block_dim=3,block_list=[a1,a2,a3,a4])

                  ! Make error
                  call torsion_angle_deriv_(self,a1,a2,a3,a4,der)
                  err = dot_(cov,der,der)
                  if (err<ZERO) then
                     err = ZERO
                  end if
                  err = sqrt(err)

                  ! Add data
                  n = n+1
                  quad(n,1) = a1
                  quad(n,2) = a2
                  quad(n,3) = a3
                  quad(n,4) = a4
                  angle(n) = torsion_angle_(self,a1,a2,a3,a4)
                  esds(n) = err
               end do
            end do
         end do
      end do
   end subroutine

   subroutine put_torsion_angle_with_error(self,a1,a2,a3,a4,covariance)
   ! Get the bonded-atom "pair"s, and the distances "dist" between
   ! them, and the associated "esds". The pair has "atom_kind" of "k1"
   ! and "k2", respectively. The "covariance" matrix for *all* atom
   ! coordinates is also given.
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: a1,a2,a3,a4
      MAT(REAL,:,:), IN :: covariance
      REAL :: err,tor
      VEC(REAL,12) :: der
      MAT(REAL,12,12) :: cov
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_torsion_angle_with_error ... axis system must be cartesian")
   ENSURE(is_square_(covariance),"VEC{ATOM}:put_torsion_angle_with_error ... covariance: not square")
   ENSURE(size(covariance,1)==3*size(self),"VEC{ATOM}:put_torsion_angle_with_error ... covariance: wrong size")

      ! Extract covariances for atom a, b positions
      call put_blocks_to_(covariance,cov,block_dim=3,block_list=[a1,a2,a3,a4])

      ! Make error
      call torsion_angle_deriv_(self,a1,a2,a3,a4,der)
      err = dot_(cov,der,der)
      err = sqrt(err)
      call convert_to_(err,"degree")

      ! Add the data
      tor = torsion_angle_(self,a1,a2,a3,a4)
      call convert_to_(tor,"degree")

      ! Output
      call flush_(stdout)
      call text_(stdout,"========================")
      call text_(stdout,"Torsion angle with error")
      call text_(stdout,"========================")
      call show_(stdout,"Atom 1        =",self(a1)%tag)
      call show_(stdout,"Atom 2        =",self(a2)%tag)
      call show_(stdout,"Atom 3        =",self(a3)%tag)
      call show_(stdout,"Atom 4        =",self(a4)%tag)
      call show_(stdout,"Torsion angle =",tor)
      call show_(stdout,"Error         =",err)
   end subroutine


!  ==============
!  Principal axes
!  ==============

   subroutine put_principal_axes(self)
   ! Put out the principal axis information
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,3) :: pm,pn
      MAT(REAL,3,3) :: pa
      BIN :: reorder
      call make_principal_moments_(self,pm,pa,reorder)
      pn = pm/maxval(pm)  ! normalized moments
      call flush_(stdout)
      call text_(stdout,"Principal moments and axes:")
      call flush_(stdout)
      call show_(stdout,"Center of mass    =",center_of_mass_(self))
      call show_(stdout,"Principal moments =",pm)
      call show_(stdout,"Spherical top?    =",is_spherical_top_(self,pn))
      call show_(stdout,"Symmetric top?    =",is_symmetric_top_(self,pn))
      call show_(stdout,"Prolate top?      =",is_prolate_top_(self,pn))
      call show_(stdout,"Oblate top?       =",is_oblate_top_(self,pn))
      call flush_(stdout)
      call text_(stdout,"Principal axes:")
      call put_(stdout,pa)
   end subroutine


!  ==
!  MM
!  ==

!   put_mm_info :: private
!   ! Output a table of the residue names, sequence numbers, charges, but
!   ! no basis sets. This is in PDB input format.
!      i :: INT
!      stdout.flush
!      stdout.text("Residue information:")
!      stdout.flush
!      stdout.dash(width=42)
!      stdout.put(" ",width=4)
!      stdout.put("#",width=7)
!      stdout.put("Name",width=4)
!      stdout.put("Residue",width=5)
!      stdout.put("Sequence",width=6)
!      stdout.put("x",width=3)
!      stdout.put("y",width=3)
!      stdout.put("z",width=3)
!      stdout.put("Charge",width=3)
!      stdout.put("Element",width=4)
!      stdout.flush
!      stdout.dash(width=42)
!      stdout.dash(int_fields=6,real_fields=5)
!      do i = 1,size(self)
!         stdout.put("ATOM",width=4)
!         stdout.put(i,width=7)
!         stdout.put(self(i).residue_atom_code,width=4)
!         stdout.put(self(i).residue_code,width=5)
!         stdout.put(self(i).residue_sequence_number,width=6)
!         stdout.put(self(i).position(1),width=8,precision=3)
!         stdout.put(self(i).position(2),width=8,precision=3)
!         stdout.put(self(i).position(3),width=8,precision=3)
!         stdout.put(self(i).mm_charge,width=7,precision=3)
!         stdout.put(self(i).label,width=4)
!         stdout.flush
!      end
!      stdout.dash(width=42)
!   end

!   put_restraint_atoms :: private
!   ! Output a table of the atom names, residue names and restraint atom
!   ! information
!      i :: INT
!      stdout.text("Restraint atoms:")
!      stdout.save_style
!      stdout.set_int_width(9)
!      stdout.set_real_width(9)
!      stdout.set_real_precision(3)
!      stdout.dash(int_fields=5,real_fields=3)
!      stdout.put(" ",int_width=TRUE)
!      stdout.put("Atom",int_width=TRUE)
!      stdout.put("Residue",int_width=TRUE)
!      stdout.put("Sequence")
!      stdout.put("x")
!      stdout.put("y")
!      stdout.put("z")
!      stdout.put("Force",int_width=TRUE)
!      stdout.flush
!      stdout.dash(int_fields=5,real_fields=3)
!      do i = 1,size(self)
!         if (self(i).restraining_force_constant.is_zero) cycle
!         stdout.put(" ",int_width=TRUE)
!         stdout.put(self(i).residue_atom_code,int_width=TRUE)
!         stdout.put(self(i).residue_code,int_width=TRUE)
!         stdout.put(self(i).residue_sequence_number)
!         stdout.put(self(i).restraining_position(1))
!         stdout.put(self(i).restraining_position(2))
!         stdout.put(self(i).restraining_position(3))
!         stdout.put(self(i).restraining_force_constant)
!         stdout.flush
!      end
!      stdout.dash(int_fields=5,real_fields=3)
!      stdout.unsave_style
!   end

!  ====
!  VRML
!  ====

   subroutine put_vrml(self,out)
   ! Put vrml version of the atomvec to the file in object "out".
      VEC(type(ATOM_TYPE),:), IN :: self
      type(TEXTFILE_TYPE), INOUT :: out
      call set_real_precision_(out,5)
      call set_real_width_(out,12)
      call text_(stdout,"Generating VRML atoms...")
      call put_vrml_header(out)
      call put_vrml_atoms_(self,out)
      call put_vrml_bonds_(self,out)
      call text_(stdout,"done VRML atoms")
      call flush_(stdout)
   end subroutine

   subroutine put_vrml_header(out)
   ! Put vrml header, and prototype bond and spheres, to the file.
      type(TEXTFILE_TYPE), INOUT :: out
      call text_(out,"PROTO Atom "//achar(91))
      call text_(out,"   field SFColor col 1 0 0")
      call text_(out,"   field SFFloat rad 1")
      call text_(out,"   field SFVec3f pos 0 0 0")
      call text_(out,achar(93))
      call text_(out,"{")
      call text_(out,"   Transform {")
      call text_(out,"      translation IS pos")
      call text_(out,"      children "//achar(91))
      call text_(out,"         Shape {")
      call text_(out,"            appearance Appearance {")
      call text_(out,"               material Material {")
      call text_(out,"                  diffuseColor IS col")
      call text_(out,"               }")
      call text_(out,"            }")
      call text_(out,"            geometry Sphere {")
      call text_(out,"              radius IS rad")
      call text_(out,"            }")
      call text_(out,"         }")
      call text_(out,"      "//achar(93))
      call text_(out,"   }")
      call text_(out,"}")
      call flush_(out)
      call text_(out,"PROTO Bond "//achar(91))
      call text_(out,"   field SFColor col 1 0 0")
      call text_(out,"   field SFFloat hgt 1")
      call text_(out,"   field SFVec3f pos 0 0 0")
      call text_(out,"   field SFRotation rot 1 0 0 0")
      call text_(out,achar(93))
      call text_(out,"{")
      call text_(out,"   Transform {")
      call text_(out,"      translation IS pos")
      call text_(out,"      rotation IS rot")
      call text_(out,"      children "//achar(91))
      call text_(out,"         Shape {")
      call text_(out,"            appearance Appearance {")
      call text_(out,"               material Material {")
      call text_(out,"                 diffuseColor IS col")
      call text_(out,"               }")
      call text_(out,"            }")
      call text_(out,"            geometry Cylinder {")
      call text_(out,"               radius 0.304245979")
      call text_(out,"               height IS hgt")
      call text_(out,"               top    FALSE")
      call text_(out,"               bottom FALSE")
      call text_(out,"            }")
      call text_(out,"         }")
      call text_(out,"      "//achar(93))
      call text_(out,"   }")
      call text_(out,"}")
      call flush_(out)
   end subroutine

   subroutine put_vrml_atoms(self,out)
   ! Put spheres for the atoms to view as vrml.
      VEC(type(ATOM_TYPE),:), IN :: self
      type(TEXTFILE_TYPE), INOUT :: out
      VEC(REAL,3) :: colour
      STR(len=2) :: label
      REAL :: radius
      INT :: n,Z
      do n = 1,size(self)
         Z = self(n)%atomic_number
         label = chemical_symbol_(self(n))
         select case (label)
         case ("H ","D ","T "); colour = [191,196,192]
         case ("He","Rn"); colour = [88,196,160]
         case ("Li","Be","Na","Mg"); colour = [144,149,145]
         case ("B "); colour = [187,4,187]
         case ("C "); colour = [160,80,17]
         case ("N ","Al"); colour = [126,169,176]
         case ("Si"); colour = [192,172,137]
         case ("S "); colour = [192,165,0]
         case ("Cl"); colour = [0,165,0]
         case ("O ","Ca","Ge","As"); colour = [192,12,8]
         case ("Se","Br","Sr","I "); colour = [192,12,8]
         case default; colour = [192,148,25]
         end select
         select case (Z)
         case (1:54); radius = bragg_slater_radius_(self(n))
         case default; radius = 1.30d0
         end select
         colour = colour/256
         call text_(out,"Atom {")
         call text_(out," col "//trim(to_concatenated_str_(colour,"f15.8",separator=", ")))
         call text_(out," pos "//trim(to_concatenated_str_(self(n)%position,"f15.8",separator=", ")))
         call text_(out," rad "//trim(to_str_(radius,"f15.8")))
         call text_(out,"}")
      end do
   end subroutine

   subroutine put_vrml_bonds(self,out)
   ! Put bonds for the atoms to view as vrml.
      VEC(type(ATOM_TYPE),:), IN :: self
      type(TEXTFILE_TYPE), INOUT :: out
      VEC(REAL,3) :: col_a,col_b,posa,posb,pos1,pos2,AB,center
      VEC(REAL,4) :: rot
      REAL :: hgta,hgtb,rada,radb
      INT :: a,b
      do a = 1,size(self)
         do b = 1,a-1
            if (bonded_(self,a,b)) then
               posa = self(a)%position
               posb = self(b)%position
               AB = posb-posa
               call normalize_(AB)
               rada = bragg_slater_radius_(self(a))
               radb = bragg_slater_radius_(self(b))
               center = HALF*(posa+rada*AB+posb-radb*AB)
               hgta = norm_((center-posa))
               hgtb = norm_((posb-center))
               pos1 = HALF*(center+posa)
               pos2 = HALF*(center+posb)
               rot(4) = PI
               rot(1:3) = AB+[ZERO,ONE,ZERO]
               call normalize_(rot(1:3))
               select case (chemical_symbol_(self(a)))
               case ("H ","D ","T "); col_a = [191,196,192]
               case ("He","Rn"); col_a = [88,196,160]
               case ("Li","Be","Na","Mg"); col_a = [144,149,145]
               case ("B "); col_a = [187,4,187]
               case ("C "); col_a = [160,80,17]
               case ("N ","Al"); col_a = [126,169,176]
               case ("Si"); col_a = [192,172,137]
               case ("S "); col_a = [192,165,0]
               case ("Cl"); col_a = [0,165,0]
               case ("O ","Ca","Ge","As"); col_a = [192,12,8]
               case ("Se","Br","Sr","I "); col_a = [192,12,8]
               case default; col_a = [192,148,25]
               end select
               select case (chemical_symbol_(self(b)))
               case ("H "); col_b = [191,196,192]
               case ("He","Rn"); col_b = [88,196,160]
               case ("Li","Be","Na","Mg"); col_b = [144,149,145]
               case ("B "); col_b = [187,4,187]
               case ("C "); col_b = [160,80,17]
               case ("N ","Al"); col_b = [126,169,176]
               case ("Si"); col_b = [192,172,137]
               case ("S "); col_b = [192,165,0]
               case ("Cl"); col_b = [0,165,0]
               case ("O ","Ca","Ge","As"); col_b = [192,12,8]
               case ("Se","Br","Sr","I "); col_b = [192,12,8]
               case default; col_b = [192,148,25]
               end select
               col_a = col_a/256
               col_b = col_b/256
               call text_(out,"Bond {")
               call text_(out," col "//trim(to_concatenated_str_(col_a,"f15.8",separator=", ")))
               call text_(out," pos "//trim(to_concatenated_str_(pos1,"f15.8",separator=", ")))
               call text_(out," rot "//trim(to_concatenated_str_(rot,"f15.8",separator=", ")))
               call text_(out," hgt "//trim(to_str_(hgta,"f15.8")))
               call text_(out,"}")
               call text_(out,"Bond {")
               call text_(out," col "//trim(to_concatenated_str_(col_b,"f15.8",separator=", ")))
               call text_(out," pos "//trim(to_concatenated_str_(pos2,"f15.8",separator=", ")))
               call text_(out," rot "//trim(to_concatenated_str_(rot,"f15.8",separator=", ")))
               call text_(out," hgt "//trim(to_str_(hgtb,"f15.8")))
               call text_(out,"}")
            end if
         end do
      end do
   end subroutine

   subroutine put_CX(self,label,angstrom)
   ! Outputs some information for the Crystal Explorer program
      VEC(type(ATOM_TYPE),:), IN :: self
      STR(len=*) :: label
      BIN, optional :: angstrom
      INT :: a
      VEC(REAL,3) :: pos
      BIN :: angst
      angst = FALSE
      if (present(angstrom)) angst = angstrom

      ! Print atom_coords
      call flush_(stdout)
      call text_(stdout,"begin atom_coords "//trim(label))
      do a = 1,size(self)
         call put_(stdout,self(a)%label)
         call put_(stdout,chemical_symbol_(self(a)))
         pos = self(a)%position
         if (angst) pos = ANGSTROM_PER_BOHR*pos
         call put_(stdout,pos(1))
         call put_(stdout,pos(2))
         call put_(stdout,pos(3))
         call put_(stdout," IN")
         call put_(stdout,self(a)%site_disorder_group)
         call put_(stdout,self(a)%site_occupancy)
         call flush_(stdout)
      end do
      call text_(stdout,"end atom_coords")
   end subroutine


!  ================
!  Put CIF routines
!  ================

   subroutine put_CIF_coords_0(self,refine_H_U_iso)
   ! Output the atoms and ADP's in CIF format to "stdout" in an
   ! axis system, with (optionally) errors "esd" provided externally.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      BIN :: is_cartn
      is_cartn = self(1)%pos_axis_system=="cartesian"
      if (is_cartn) then; call put_CIF_coords_cartn_(self,refine_H_U_iso)
      else; call put_CIF_coords_cryst_(self,refine_H_U_iso)
      end if
   end subroutine

   subroutine put_CIF_coords_1(self,refine_H_U_iso,esd)
   ! Output the atoms and ADP's in CIF format to "stdout" in an
   ! axis system, with errors "esd" provided externally.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(REAL,:), IN :: esd
      BIN :: is_cartn
      is_cartn = self(1)%pos_axis_system=="cartesian"
      if (is_cartn) then; call put_CIF_coords_cartn_(self,refine_H_U_iso,esd)
      else; call put_CIF_coords_cryst_(self,refine_H_U_iso,esd)
      end if
   end subroutine

   subroutine put_CIF_no_esd_note()
   ! Say, in the CIF, why the loop that follows has no esd's. This is the
   ! fallback: it fires only when there is nothing at all to make them
   ! from -- neither a refinement's covariance matrix nor any standard
   ! uncertainty in the source.
      call flush_(stdout)
      call text_(stdout,"# . NO standard uncertainties are given below")
      call text_(stdout,"# . There is nothing to derive them from: no covariance")
      call text_(stdout,"#   matrix from a refinement, and no standard uncertainties")
      call text_(stdout,"#   in the data this structure was read from")
      call text_(stdout,"# . Omitted means 'not available'. It does not mean zero,")
      call text_(stdout,"#   which would claim the value is known exactly")
      call flush_(stdout)
   end subroutine

   subroutine put_CIF_derived_esd_note()
   ! Say, in the CIF, that the esd's below were derived rather than
   ! measured, and on what assumption.
      call flush_(stdout)
      call text_(stdout,"# . The standard uncertainties below were DERIVED, not")
      call text_(stdout,"#   measured. This structure was read from a CIF, which")
      call text_(stdout,"#   reports uncertainties but not the covariance matrix")
      call text_(stdout,"#   they must be propagated through. They were taken to be")
      call text_(stdout,"#   uncorrelated, and a covariance built on that assumption")
      call text_(stdout,"# . Under that assumption the conversion is exact and")
      call text_(stdout,"#   reversible: writing this structure back out in")
      call text_(stdout,"#   fractional coordinates returns the original values")
      call text_(stdout,"# . Correlations between parameters are therefore assumed,")
      call text_(stdout,"#   not known. Do not read them as measured")
      call flush_(stdout)
   end subroutine

   subroutine put_CIF_coords_cartn_0(self,refine_H_U_iso)
   ! Output the atoms and ADP's in CIF format to "stdout" in the
   ! cartesian axis system.
   ! NOTE: no esd's are written. This routine is the no-covariance
   ! branch, so there is nothing to propagate the stored esd's through.
   ! See "put_CIF_no_esd_note".
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(STR(len=STR_SIZE),:), allocatable :: ADP_type
      VEC(REAL,:), allocatable :: Uiso
      MAT(REAL,:,:), allocatable :: xyz
      REAL :: fac,fac2
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_coords_cartn_0 ... not cartesian axis system!")

      ! Unit conversion
      fac = ANGSTROM_PER_BOHR
      fac2 = fac*fac

      ! CIF coordinate header
      call flush_(stdout)
      call text_(stdout,"# =====================")
      call text_(stdout,"# Cartesian coordinates")
      call text_(stdout,"# =====================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom")
      call put_CIF_no_esd_note
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cartn_label")
      call text_(stdout,"_atom_site_Cartn_x")
      call text_(stdout,"_atom_site_Cartn_y")
      call text_(stdout,"_atom_site_Cartn_z")
      call text_(stdout,"_atom_site_Cartn_U_iso_or_equiv")
      call text_(stdout,"_atom_site_Cartn_adp_type")
      call text_(stdout,"_atom_site_Cartn_occupancy")
      call text_(stdout,"_atom_site_Cartn_disorder_group")

      ! Table column data
      call create_(xyz,size(self),3)
      call create_(Uiso,size(self))
      call create_(ADP_type,size(self))

      ! Get table column data
      call put_pos_vector_to_(self,xyz,fac)
      Uiso = self%U_iso*fac2
      ADP_type = ADP_type_(self,refine_H_U_iso)

      ! Table 1
      call create_(tabl1,8)
      call set_values_(tabl1(1),self(:)%tag)
      call set_values_(tabl1(2),xyz(:,1))
      call set_values_(tabl1(3),xyz(:,2))
      call set_values_(tabl1(4),xyz(:,3))
      call set_values_(tabl1(5),Uiso)
      call set_values_(tabl1(6),ADP_type)
      call set_values_(tabl1(7),self(:)%site_occupancy)
      call set_values_(tabl1(8),self(:)%site_disorder_group)

      ! Put table
      call put_body_(tabl1)

      ! Clean
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_coords_cartn_1(self,refine_H_U_iso,esd)
   ! Output the atoms and ADP's in CIF format to "stdout" in an
   ! axis system, with errors "esd" provided externally.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(REAL,:), IN :: esd
      VEC(STR(len=STR_SIZE),:), allocatable :: blnk,ADP_type
      VEC(REAL,:), allocatable :: Uiso,Uerr
      MAT(REAL,:,:), allocatable :: xyz,err
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1,tabl2
      REAL :: fac,fac2
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_coords_cartn_1 ... wrong size, esd")
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_coords_cartn_1 ... not cartesian axis system!")

      ! Unit conversion
      fac = ANGSTROM_PER_BOHR
      fac2 = fac*fac

      ! CIF coordinate header
      call flush_(stdout)
      call text_(stdout,"# =====================")
      call text_(stdout,"# Cartesian coordinates")
      call text_(stdout,"# =====================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom")
      if (any_pADP_covariances_derived_(self)) then
         call put_CIF_derived_esd_note
      else
         call flush_(stdout)
      end if
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cartn_label")
      call text_(stdout,"_atom_site_Cartn_x")
      call text_(stdout,"_atom_site_Cartn_y")
      call text_(stdout,"_atom_site_Cartn_z")
      call text_(stdout,"_atom_site_Cartn_U_iso_or_equiv")
      call text_(stdout,"_atom_site_Cartn_adp_type")
      call text_(stdout,"_atom_site_Cartn_occupancy")
      call text_(stdout,"_atom_site_Cartn_disorder_group")
      call text_(stdout,"_atom_site_Cartn_x_esu")
      call text_(stdout,"_atom_site_Cartn_y_esu")
      call text_(stdout,"_atom_site_Cartn_z_esu")
      call text_(stdout,"_atom_site_Cartn_U_iso_or_equiv_esu")

      ! Table column data
      call create_(blnk,size(self))
      call create_(xyz,size(self),3)
      call create_(err,size(self),3)
      call create_(Uiso,size(self))
      call create_(Uerr,size(self))
      call create_(ADP_type,size(self))

      ! Get table column data
      blnk = " "
      call put_pos_vector_to_(self,xyz,fac)
      call put_pos_errors_to_(self,err,esd,fac)  ! Note esd
      Uiso = self%U_iso*fac2
      Uerr = self%U_iso_error*fac2
      ADP_type = ADP_type_(self,refine_H_U_iso)

      ! Table 1
      call create_(tabl1,8)
      call set_values_(tabl1(1),self(:)%tag)
      call set_values_(tabl1(2),xyz(:,1))
      call set_values_(tabl1(3),xyz(:,2))
      call set_values_(tabl1(4),xyz(:,3))
      call set_values_(tabl1(5),Uiso)
      call set_values_(tabl1(6),ADP_type)
      call set_values_(tabl1(7),self(:)%site_occupancy)
      call set_values_(tabl1(8),self(:)%site_disorder_group)

      ! Table 2
      call create_(tabl2,5)
      call set_values_(tabl2(1),blnk)
      call set_values_(tabl2(2),err(:,1))
      call set_values_(tabl2(3),err(:,2))
      call set_values_(tabl2(4),err(:,3))
      call set_values_(tabl2(5),Uerr)
      call set_width_(tabl2(1),tabl1(1)%width)
      call set_width_(tabl2(2),tabl1(2)%width)
      call set_width_(tabl2(3),tabl1(3)%width)
      call set_width_(tabl2(4),tabl1(4)%width)
      call set_width_(tabl2(5),tabl1(5)%width)

      ! Put tables
      call put_body_(tabl1,tabl2)

      ! Clean
      call destroy_(tabl2)
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_coords_cryst_0(self,refine_H_U_iso)
   ! Output the atoms and ADP's in CIF format to "stdout" in the
   ! crystal/fractional axis system. This version does not override
   ! existing esd's.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(STR(len=STR_SIZE),:), allocatable :: ADP_type,symbols
      VEC(REAL,:), allocatable :: Uiso
      MAT(REAL,:,:), allocatable :: xyz
      MAT(REAL,:,:), allocatable :: err
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      REAL :: fac,fac2

      ! Table column data
      call create_(xyz,size(self),3)
      call create_(err,size(self),3)
      call create_(Uiso,size(self))
      call create_(ADP_type,size(self))
      call create_(symbols,size(self))

      ! Unit conversion
      fac = ANGSTROM_PER_BOHR
      fac2 = fac*fac

      ! Get table column data
      Uiso = self%U_iso*fac2
      ADP_type = ADP_type_(self,refine_H_U_iso)
      symbols = chemical_symbol_(self(:))

      ! CIF coordinate header
      call flush_(stdout)
      call text_(stdout,"# ======================")
      call text_(stdout,"# Fractional coordinates")
      call text_(stdout,"# ======================")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_label")
      call text_(stdout,"_atom_site_type_symbol")
      call text_(stdout,"_atom_site_fract_x")
      call text_(stdout,"_atom_site_fract_y")
      call text_(stdout,"_atom_site_fract_z")
      call text_(stdout,"_atom_site_U_iso_or_equiv")
      call text_(stdout,"_atom_site_adp_type")
      call text_(stdout,"_atom_site_occupancy")
      call text_(stdout,"_atom_site_disorder_group")

      ! Get table column data
      call put_pos_vector_to_(self,xyz)
      call put_pos_errors_to_(self,err)

      ! Table 1
      call create_(table,9)
      call set_values_(table(1),self(:)%tag)
      call set_values_(table(2),symbols)
      call set_values_and_errors_(table(3),xyz(:,1),err(:,1))
      call set_values_and_errors_(table(4),xyz(:,2),err(:,2))
      call set_values_and_errors_(table(5),xyz(:,3),err(:,3))
      call set_values_(table(6),Uiso)
      call set_values_(table(7),ADP_type)
      call set_values_(table(8),self(:)%site_occupancy)
      call set_values_(table(9),self(:)%site_disorder_group)

      ! Put tables
      call put_body_(table)

      ! Clean
      call destroy_(table)
      call destroy_(symbols)
      call destroy_(ADP_type)
      call destroy_(Uiso)
      call destroy_(err)
      call destroy_(xyz)
   end subroutine

   subroutine put_CIF_coords_cryst_1(self,refine_H_U_iso,esd)
   ! Output the atoms and ADP's in CIF format to "stdout" in an
   ! axis system, with errors "esd" provided externally.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(REAL,:), IN :: esd
      VEC(STR(len=STR_SIZE),:), allocatable :: blnk,ADP_type
      VEC(REAL,:), allocatable :: Uiso,Uerr
      MAT(REAL,:,:), allocatable :: xyz,err
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1,tabl2
      REAL :: fac,fac2
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_coords_cryst_1 ... wrong size, esd")
   ENSURE(self(1)%pos_axis_system=="crystal","VEC{ATOM}:put_CIF_coords_cryst_1 ... not crystal axis system!")

      ! Unit conversion
      fac = ANGSTROM_PER_BOHR
      fac2 = fac*fac

      ! CIF coordinate header
      call flush_(stdout)
      call text_(stdout,"# =====================================")
      call text_(stdout,"# Precise fractional system coordinates")
      call text_(stdout,"# =====================================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Non-standard tags shadow standard tags to avoid duplication")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Better yet, use the refined cartesian parameters")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cryst_label")
      call text_(stdout,"_atom_site_Cryst_x")
      call text_(stdout,"_atom_site_Cryst_y")
      call text_(stdout,"_atom_site_Cryst_z")
      call text_(stdout,"_atom_site_Cryst_U_iso_or_equiv")
      call text_(stdout,"_atom_site_Cryst_adp_type")
      call text_(stdout,"_atom_site_Cryst_occupancy")
      call text_(stdout,"_atom_site_Cryst_disorder_group")
      call text_(stdout,"_atom_site_Cryst_x_esu")
      call text_(stdout,"_atom_site_Cryst_y_esu")
      call text_(stdout,"_atom_site_Cryst_z_esu")
      call text_(stdout,"_atom_site_Cryst_U_iso_or_equiv_esu")

      ! Table column data
      call create_(blnk,size(self))
      call create_(xyz,size(self),3)
      call create_(err,size(self),3)
      call create_(Uiso,size(self))
      call create_(Uerr,size(self))
      call create_(ADP_type,size(self))

      ! Get table column data
      blnk = " "
      call put_pos_vector_to_(self,xyz)
      call put_pos_errors_to_(self,err,esd)  ! Note esd
      Uiso = self%U_iso*fac2
      Uerr = self%U_iso_error*fac2
      ADP_type = ADP_type_(self,refine_H_U_iso)

      ! Table 1
      call create_(tabl1,8)
      call set_values_(tabl1(1),self(:)%tag)
      call set_values_(tabl1(2),xyz(:,1))
      call set_values_(tabl1(3),xyz(:,2))
      call set_values_(tabl1(4),xyz(:,3))
      call set_values_(tabl1(5),Uiso)
      call set_values_(tabl1(6),ADP_type)
      call set_values_(tabl1(7),self(:)%site_occupancy)
      call set_values_(tabl1(8),self(:)%site_disorder_group)

      ! Table 2
      call create_(tabl2,5)
      call set_values_(tabl2(1),blnk)
      call set_values_(tabl2(2),err(:,1))
      call set_values_(tabl2(3),err(:,2))
      call set_values_(tabl2(4),err(:,3))
      call set_values_(tabl2(5),Uerr)
      call set_width_(tabl2(1),tabl1(1)%width)
      call set_width_(tabl2(2),tabl1(2)%width)
      call set_width_(tabl2(3),tabl1(3)%width)
      call set_width_(tabl2(4),tabl1(4)%width)
      call set_width_(tabl2(5),tabl1(5)%width)

      ! Put tables
      call put_body_(tabl1,tabl2)

      ! Clean
      call destroy_(tabl2)
      call destroy_(tabl1)

      ! CIF coordinate header
      call flush_(stdout)
      call text_(stdout,"# ======================")
      call text_(stdout,"# Fractional coordinates")
      call text_(stdout,"# ======================")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_label")
      call text_(stdout,"_atom_site_fract_x")
      call text_(stdout,"_atom_site_fract_y")
      call text_(stdout,"_atom_site_fract_z")
      call text_(stdout,"_atom_site_U_iso_or_equiv")
      call text_(stdout,"_atom_site_adp_type")
      call text_(stdout,"_atom_site_occupancy")
      call text_(stdout,"_atom_site_disorder_group")

      ! Set table column data
      call create_(tabl1,8)
      call set_values_(tabl1(1),self(:)%tag)
      call set_values_and_errors_(tabl1(2),xyz(:,1),err(:,1))
      call set_values_and_errors_(tabl1(3),xyz(:,2),err(:,2))
      call set_values_and_errors_(tabl1(4),xyz(:,3),err(:,3))
      call set_values_and_errors_(tabl1(5),Uiso,Uerr)
      call set_values_(tabl1(6),ADP_type)
      call set_values_(tabl1(7),self(:)%site_occupancy)
      call set_values_(tabl1(8),self(:)%site_disorder_group)

      ! Put table
      call put_body_(tabl1)

      ! Clean
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_ADP2(self,refine_H_U_iso,esd)
   ! Output the ADP2's in CIF format to "stdout" in an axis system.
   ! This version uses inputted refinement "esd" errors.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: refine_H_U_iso
      VEC(REAL,:), optional, IN :: esd
      BIN :: is_cartn
      is_cartn = self(1)%ADP_axis_system=="cartesian"
      if (is_cartn) then; call put_CIF_ADP2_cartn_(self,refine_H_U_iso,esd)
      else; call put_CIF_ADP2_cryst_(self,refine_H_U_iso,esd)
      end if
   end subroutine

   subroutine put_CIF_ADP2_cartn(self,refine_H_U_iso,esd)
   ! Output the ADP2's in CIF format to "stdout" in cartesian axis system.
   ! If present and TRUE "refine_H_U_iso" removed H ADP printing.
   ! If present, "esd" array for whole atom list overrides errors.
   ! NOTE: if "esd" is absent no esd's are written at all -- that is the
   ! no-covariance branch, and the stored errors cannot be transformed
   ! into cartesians. See "put_CIF_no_esd_note".
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: refine_H_U_iso
      VEC(REAL,:), optional, IN :: esd
      VEC(INT,:), allocatable :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: ID,blnk
      MAT(REAL,:,:), allocatable :: ADPv,ADPe
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1,tabl2
      STR(len=STR_SIZE) :: lab
      REAL :: fac
      INT :: i,dim
      BIN :: H_U_iso,has_esd
   ENSURE(all(self(:)%ADP_axis_system=="cartesian"),"VEC{ATOM}:put_CIF_ADP2_cartn ... ADP axis system must be cartesian")
      H_U_iso = FALSE
      if (present(refine_H_U_iso)) H_U_iso = refine_H_U_iso
      has_esd = present(esd)

      ! Unit conversion for ADP's
      fac = ANGSTROM_PER_BOHR

      ! CIF ADP header
      call flush_(stdout)
      call text_(stdout,"# ==============")
      call text_(stdout,"# Cartesian ADPs")
      call text_(stdout,"# ==============")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom^2")
      if (NOT has_esd) then
         call put_CIF_no_esd_note
      else if (any_pADP_covariances_derived_(self)) then
         call put_CIF_derived_esd_note
      else
         call flush_(stdout)
      end if
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cartn_ADP2_U_label")
      do i = 1,6
         lab = ADP2_index_label_(i,form="123-repeated")
         call text_(stdout,"_atom_site_Cartn_ADP2_U_"//trim(lab))
      end do
      if (has_esd) then
         do i = 1,6
            lab = ADP2_index_label_(i,form="123-repeated")
            call text_(stdout,"_atom_site_Cartn_ADP2_U_"//trim(lab)//"_esu")
         end do
      end if

      ! Size of table
      dim = size(self)
      if (H_U_iso) dim = no_of_non_H_atoms_(self)

      ! Allocate column data
      call create_(ID,dim)
      call create_(blnk,dim)
      call create_(ADPv,dim,6)
      call create_(ADPe,dim,6)
      ADPe = ZERO

      ! Get column data
      if (NOT H_U_iso) then
         ID = self%tag
         blnk = " "
         call put_ADP2_vector_to_(self,ADPv,fac)
         if (has_esd) call put_ADP2_errors_to_(self,ADPe,fac,esd)
      else
         list = list_of_non_H_atoms_(self)
         ID = self(list)%tag
         blnk = " "
         call put_ADP2_vector_to_(self,ADPv,list,fac)
         if (has_esd) call put_ADP2_errors_to_(self,ADPe,list,fac,esd)
      end if

      ! Table 1
      call create_(tabl1,7)
      call set_values_(tabl1(1),ID)
      call set_values_(tabl1(2),ADPv(:,1))
      call set_values_(tabl1(3),ADPv(:,2))
      call set_values_(tabl1(4),ADPv(:,3))
      call set_values_(tabl1(5),ADPv(:,4))
      call set_values_(tabl1(6),ADPv(:,5))
      call set_values_(tabl1(7),ADPv(:,6))

      ! Table 2
      if (has_esd) then
         call create_(tabl2,7)
         call set_values_(tabl2(1),blnk)
         call set_values_(tabl2(2),ADPe(:,1))
         call set_values_(tabl2(3),ADPe(:,2))
         call set_values_(tabl2(4),ADPe(:,3))
         call set_values_(tabl2(5),ADPe(:,4))
         call set_values_(tabl2(6),ADPe(:,5))
         call set_values_(tabl2(7),ADPe(:,6))
         call set_width_(tabl2(1),tabl1(1)%width)
         call set_width_(tabl2(2),tabl1(2)%width)
         call set_width_(tabl2(3),tabl1(3)%width)
         call set_width_(tabl2(4),tabl1(4)%width)
         call set_width_(tabl2(5),tabl1(5)%width)
         call set_width_(tabl2(6),tabl1(6)%width)
         call set_width_(tabl2(7),tabl1(7)%width)
      end if

      ! Put tables
      if (has_esd) then; call put_body_(tabl1,tabl2)
      else; call put_body_(tabl1)
      end if

      ! Clean
      if (has_esd) call destroy_(tabl2)
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_ADP2_cryst(self,refine_H_U_iso,esd)
   ! Output the ADP2's in CIF format to "stdout" in crystal system.
   ! If present and TRUE "refine_H_U_iso" removed H ADP printing.
   ! If present and TRUE "esd" array for whole atom list overrides ! errors.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: refine_H_U_iso
      VEC(REAL,:), optional, IN :: esd
      VEC(INT,:), allocatable :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: ID,blnk
      MAT(REAL,:,:), allocatable :: ADPv,ADPe
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1,tabl2
      STR(len=STR_SIZE) :: lab
      REAL :: fac
      INT :: i,dim
      BIN :: H_U_iso
   ENSURE(all(self(:)%ADP_axis_system=="crystal"),"VEC{ATOM}:put_CIF_ADP2_cryst ... ADP axis system must be crystal")
      H_U_iso = FALSE
      if (present(refine_H_U_iso)) H_U_iso = refine_H_U_iso

      ! Unit conversion for ADP's
      fac = ANGSTROM_PER_BOHR

      ! CIF ADP header
      call flush_(stdout)
      call text_(stdout,"# ==================================")
      call text_(stdout,"# Precise cartesian axis system ADPs")
      call text_(stdout,"# ==================================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Non-standard tags shadow standard tags to avoid duplication")
      call text_(stdout,"# . Keep these for a refinement restart")
      call text_(stdout,"# . Better yet, use the refined cartesian parameters")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cryst_ADP2_U_label")
      do i = 1,6
         lab = ADP2_index_label_(i,form="123-repeated")
         call text_(stdout,"_atom_site_Cryst_ADP2_U_"//trim(lab))
      end do
      do i = 1,6
         lab = ADP2_index_label_(i,form="123-repeated")
         call text_(stdout,"_atom_site_Cryst_ADP2_U_"//trim(lab)//"_esu")
      end do

      ! Size of table
      dim = size(self)
      if (H_U_iso) dim = no_of_non_H_atoms_(self)

      ! Allocate column data
      call create_(ID,dim)
      call create_(blnk,dim)
      call create_(ADPv,dim,6)
      call create_(ADPe,dim,6)

      ! Get column data
      if (NOT H_U_iso) then
         ID = self%tag
         blnk = " "
         call put_ADP2_vector_to_(self,ADPv,fac)
         if (NOT present(esd)) then; call put_ADP2_errors_to_(self,ADPe,fac)
         else; call put_ADP2_errors_to_(self,ADPe,fac,esd)
         end if
      else
         list = list_of_non_H_atoms_(self)
         ID = self(list)%tag
         blnk = " "
         call put_ADP2_vector_to_(self(list),ADPv,fac)
         if (NOT present(esd)) then; call put_ADP2_errors_to_(self(list),ADPe,fac)
         else; call put_ADP2_errors_to_(self,ADPe,list,fac,esd)
         end if
      end if

      ! Table 1
      call create_(tabl1,7)
      call set_values_(tabl1(1),ID)
      call set_values_(tabl1(2),ADPv(:,1))
      call set_values_(tabl1(3),ADPv(:,2))
      call set_values_(tabl1(4),ADPv(:,3))
      call set_values_(tabl1(5),ADPv(:,4))
      call set_values_(tabl1(6),ADPv(:,5))
      call set_values_(tabl1(7),ADPv(:,6))

      ! Table 2
      call create_(tabl2,7)
      call set_values_(tabl2(1),blnk)
      call set_values_(tabl2(2),ADPe(:,1))
      call set_values_(tabl2(3),ADPe(:,2))
      call set_values_(tabl2(4),ADPe(:,3))
      call set_values_(tabl2(5),ADPe(:,4))
      call set_values_(tabl2(6),ADPe(:,5))
      call set_values_(tabl2(7),ADPe(:,6))
      call set_width_(tabl2(1),tabl1(1)%width)
      call set_width_(tabl2(2),tabl1(2)%width)
      call set_width_(tabl2(3),tabl1(3)%width)
      call set_width_(tabl2(4),tabl1(4)%width)
      call set_width_(tabl2(5),tabl1(5)%width)
      call set_width_(tabl2(6),tabl1(6)%width)
      call set_width_(tabl2(7),tabl1(7)%width)

      ! Put tables
      call put_body_(tabl1,tabl2)

      ! Clean
      call destroy_(tabl2)
      call destroy_(tabl1)

      ! CIF ADP header
      call flush_(stdout)
      call text_(stdout,"# ====")
      call text_(stdout,"# ADPs")
      call text_(stdout,"# ====")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_aniso_label")
      do i = 1,6
         lab = ADP_label_(i,form="underscore-123-repeated")
         call text_(stdout,"_atom_site_aniso_"//trim(lab))
      end do

      ! Table
      call create_(tabl1,7)
      call set_values_(tabl1(1),ID)
      call set_values_and_errors_(tabl1(2),ADPv(:,1),ADPe(:,1))
      call set_values_and_errors_(tabl1(3),ADPv(:,2),ADPe(:,2))
      call set_values_and_errors_(tabl1(4),ADPv(:,3),ADPe(:,3))
      call set_values_and_errors_(tabl1(5),ADPv(:,4),ADPe(:,4))
      call set_values_and_errors_(tabl1(6),ADPv(:,5),ADPe(:,5))
      call set_values_and_errors_(tabl1(7),ADPv(:,6),ADPe(:,6))

      ! Put tables
      call put_body_(tabl1)

      ! Clean
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_ADPn(self,cif,esd)
   ! Output anharmonic ADPs in CIF format to "stdout".  optionally
   ! with errors, stored in "esd". Can handle any .ADP_axis_system.
      VEC(type(ATOM_TYPE),:), IN :: self
      type(CIF_TYPE), IN :: cif
      VEC(REAL,:), optional, IN :: esd
      BIN :: is_cartn
      is_cartn = self(1)%ADP_axis_system=="cartesian"
      if (NOT present(esd)) then
         if (is_cartn) then; call put_CIF_ADPn_cartn_(self,cif)
         else; call put_CIF_ADPn_cryst_(self,cif)
         end if
      else
         if (is_cartn) then; call put_CIF_ADPn_cartn_(self,cif,esd)
         else; call put_CIF_ADPn_cryst_(self,cif,esd)
         end if
      end if
   end subroutine

   subroutine put_CIF_ADPn_cartn_0(self,cif)
   ! Output cartesian anharmonic ADPs in CIF format to "stdout".
   ! This version does not produce errors. See below.
      VEC(type(ATOM_TYPE),:), IN :: self
      type(CIF_TYPE), IN :: cif
      INT :: i
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,10) :: ADP3v
      VEC(REAL,15) :: ADP4v
      REAL :: fac1,fac2
      REAL :: fac3,fac4

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1
      fac4 = fac2*fac2

      ! ==========
      ! Put ADP3's
      ! ==========
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_Cartn_ADP3_U_"
         call flush_(stdout)
         call text_(stdout,"# ===============")
         call text_(stdout,"# Cartesian ADP3s")
         call text_(stdout,"# ===============")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^3 but are scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values
            call put_ADP3_vector_to_(self(i),ADP3v)

            ! Units !!!!!!!!!!!!
            ADP3v = fac3*ADP3v

            ! Scale factor
            ADP3v = cif%GC3_scale_factor*ADP3v

            ! Print ADP3
            call put_(stdout,ADP3v(1))
            call put_(stdout,ADP3v(2))
            call put_(stdout,ADP3v(3))
            call flush_(stdout)
            call put_(stdout,ADP3v(4))
            call put_(stdout,ADP3v(5))
            call flush_(stdout)
            call put_(stdout,ADP3v(6))
            call put_(stdout,ADP3v(7))
            call flush_(stdout)
            call put_(stdout,ADP3v(8))
            call put_(stdout,ADP3v(9))
            call flush_(stdout)
            call put_(stdout,ADP3v(10))
            call flush_(stdout)
         end do
      end if

      ! ==========
      ! Put ADP4's
      ! ==========
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_Cartn_ADP4_U_"
         call flush_(stdout)
         call text_(stdout,"# ===============")
         call text_(stdout,"# Cartesian ADP4s")
         call text_(stdout,"# ===============")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^4 but may be scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values
            call put_ADP4_vector_to_(self(i),ADP4v)

            ! Units !!!!!!!!!!
            ADP4v = fac4*ADP4v

            ! Scale factor
            ADP4v = cif%GC4_scale_factor*ADP4v

            ! Print ADP4
            call put_(stdout,ADP4v(1))
            call put_(stdout,ADP4v(2))
            call put_(stdout,ADP4v(3))
            call flush_(stdout)
            call put_(stdout,ADP4v(4))
            call put_(stdout,ADP4v(5))
            call flush_(stdout)
            call put_(stdout,ADP4v(6))
            call put_(stdout,ADP4v(7))
            call flush_(stdout)
            call put_(stdout,ADP4v(8))
            call put_(stdout,ADP4v(9))
            call flush_(stdout)
            call put_(stdout,ADP4v(10))
            call put_(stdout,ADP4v(11))
            call put_(stdout,ADP4v(12))
            call flush_(stdout)
            call put_(stdout,ADP4v(13))
            call put_(stdout,ADP4v(14))
            call put_(stdout,ADP4v(15))
            call flush_(stdout)
         end do
      end if
   end subroutine

   subroutine put_CIF_ADPn_cartn_1(self,cif,esd)
   ! Output cartesian anharmonic ADPs in CIF format to "stdout".
   ! This version produces errors, stored in "esd".
      VEC(type(ATOM_TYPE),:), IN :: self
      type(CIF_TYPE), IN :: cif
      VEC(REAL,:), IN :: esd
      VEC(REAL,15) :: ADP
      VEC(REAL,15) :: err
      STR(len=STR_SIZE) :: lab,ADPn_label
      INT :: i,k
      REAL :: fac1,fac2
      REAL :: fac3,fac4
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADPn_cartn_1 ... wrong sizem esd")

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1
      fac4 = fac2*fac2

      ! ==========
      ! Put ADP3's 
      ! ==========
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_Cartn_ADP3_U_"
         call flush_(stdout)
         call text_(stdout,"# ===============")
         call text_(stdout,"# Cartesian ADP3s")
         call text_(stdout,"# ===============")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^3 but may be scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,3
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 1,3
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 4,5
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 4,5
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 6,7
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 6,7
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 8,9
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 8,9
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 10,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values and errors
            call put_ADP3_vector_to_(self(i),ADP(1:10))
            err(1:10) = esd(k+10:k+19)

            ! Units !!!!!!!!!!!!!!!!!!
            ADP(1:10) = ADP(1:10)*fac3
            err(1:10) = err(1:10)*fac3

            ! Scale by scale factor
            ADP(1:10) = ADP(1:10)*cif%GC3_scale_factor
            err(1:10) = err(1:10)*cif%GC3_scale_factor

            ! Put quantity q with full error.
            call put_(stdout,ADP(1))
            call put_(stdout,ADP(2))
            call put_(stdout,ADP(3))
            call flush_(stdout)
            call put_(stdout,err(1))
            call put_(stdout,err(2))
            call put_(stdout,err(3))
            call flush_(stdout)
            call put_(stdout,ADP(4))
            call put_(stdout,ADP(5))
            call flush_(stdout)
            call put_(stdout,err(4))
            call put_(stdout,err(5))
            call flush_(stdout)
            call put_(stdout,ADP(6))
            call put_(stdout,ADP(7))
            call flush_(stdout)
            call put_(stdout,err(6))
            call put_(stdout,err(7))
            call flush_(stdout)
            call put_(stdout,ADP(8))
            call put_(stdout,ADP(9))
            call flush_(stdout)
            call put_(stdout,err(8))
            call put_(stdout,err(9))
            call flush_(stdout)
            call put_(stdout,ADP(10))
            call flush_(stdout)
            call put_(stdout,err(10))
            call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do
      end if

      ! ==========
      ! Put ADP4's
      ! ==========
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_Cartn_ADP4_U_"
         call flush_(stdout)
         call text_(stdout,"# ===============")
         call text_(stdout,"# Cartesian ADP4s")
         call text_(stdout,"# ===============")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^4")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,3
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 1,3
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 4,5
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 4,5
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 6,7
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 6,7
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 8,9
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 8,9
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 10,12
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 10,12
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         do i = 13,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 13,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values and errors (dimensionless)
            call put_ADP4_vector_to_(self(i),ADP(1:15))
            err(1:15) = esd(k+20:k+34)

            ! Units !!!!!!!!!!!!!!!!!!
            ADP(1:15) = ADP(1:15)*fac4
            err(1:15) = err(1:15)*fac4

            ! Scale by scale factor
            ADP(1:15) = ADP(1:15)*cif%GC4_scale_factor
            err(1:15) = err(1:15)*cif%GC4_scale_factor

            ! Put quantity q with full error.
            call put_(stdout,ADP(1))
            call put_(stdout,ADP(2))
            call put_(stdout,ADP(3))
            call flush_(stdout)
            call put_(stdout,err(1))
            call put_(stdout,err(2))
            call put_(stdout,err(3))
            call flush_(stdout)
            call put_(stdout,ADP(4))
            call put_(stdout,ADP(5))
            call flush_(stdout)
            call put_(stdout,err(4))
            call put_(stdout,err(5))
            call flush_(stdout)
            call put_(stdout,ADP(6))
            call put_(stdout,ADP(7))
            call flush_(stdout)
            call put_(stdout,err(6))
            call put_(stdout,err(7))
            call flush_(stdout)
            call put_(stdout,ADP(8))
            call put_(stdout,ADP(9))
            call flush_(stdout)
            call put_(stdout,err(8))
            call put_(stdout,err(9))
            call flush_(stdout)
            call put_(stdout,ADP(10))
            call put_(stdout,ADP(11))
            call put_(stdout,ADP(12))
            call flush_(stdout)
            call put_(stdout,err(10))
            call put_(stdout,err(11))
            call put_(stdout,err(12))
            call flush_(stdout)
            call put_(stdout,ADP(13))
            call put_(stdout,ADP(14))
            call put_(stdout,ADP(15))
            call flush_(stdout)
            call put_(stdout,err(13))
            call put_(stdout,err(14))
            call put_(stdout,err(15))
            call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do
      end if  ! =========
   end subroutine

   subroutine put_CIF_ADPn_cryst_0(self,cif)
   ! Output anharmonic ADPs in CIF format to "stdout".
   ! This version does not produce errors. See below.
   ! Can handle any .ADP_axis_system.
      VEC(type(ATOM_TYPE),:), IN :: self
      type(CIF_TYPE), IN :: cif
      INT :: i
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,10) :: ADP3v
      VEC(REAL,15) :: ADP4v

      ! ==========
      ! Put ADP3's
      ! ==========
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_anharm_GC_C_"
         call flush_(stdout)
         call text_(stdout,"# ======================================")
         call text_(stdout,"# Third order Gram-Charlier coefficients")
         call text_(stdout,"# ======================================")
         call flush_(stdout)
         call text_(stdout,"# . These data are in standard presentation")
         call text_(stdout,"# . They are dimensionless but are scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values
            call put_ADP3_vector_to_(self(i),ADP3v)

            ! Scale factor
            ADP3v = cif%GC3_scale_factor*ADP3v

            ! Print ADP3
            call put_(stdout,ADP3v(1))
            call put_(stdout,ADP3v(2))
            call put_(stdout,ADP3v(3))
            call flush_(stdout)
            call put_(stdout,ADP3v(4))
            call put_(stdout,ADP3v(5))
            call flush_(stdout)
            call put_(stdout,ADP3v(6))
            call put_(stdout,ADP3v(7))
            call flush_(stdout)
            call put_(stdout,ADP3v(8))
            call put_(stdout,ADP3v(9))
            call flush_(stdout)
            call put_(stdout,ADP3v(10))
            call flush_(stdout)
         end do
      end if

      ! ==========
      ! Put ADP4's
      ! ==========
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_anharm_GC_D_"
         call flush_(stdout)
         call text_(stdout,"# =======================================")
         call text_(stdout,"# Fourth order Gram-Charlier coefficients")
         call text_(stdout,"# =======================================")
         call flush_(stdout)
         call text_(stdout,"# . These data are in standard presentation")
         call text_(stdout,"# . They are dimensionless but may be scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values
            call put_ADP4_vector_to_(self(i),ADP4v)

            ! Scale factor
            ADP4v = cif%GC4_scale_factor*ADP4v

            ! Print ADP4
            call put_(stdout,ADP4v(1))
            call put_(stdout,ADP4v(2))
            call put_(stdout,ADP4v(3))
            call flush_(stdout)
            call put_(stdout,ADP4v(4))
            call put_(stdout,ADP4v(5))
            call flush_(stdout)
            call put_(stdout,ADP4v(6))
            call put_(stdout,ADP4v(7))
            call flush_(stdout)
            call put_(stdout,ADP4v(8))
            call put_(stdout,ADP4v(9))
            call flush_(stdout)
            call put_(stdout,ADP4v(10))
            call put_(stdout,ADP4v(11))
            call put_(stdout,ADP4v(12))
            call flush_(stdout)
            call put_(stdout,ADP4v(13))
            call put_(stdout,ADP4v(14))
            call put_(stdout,ADP4v(15))
            call flush_(stdout)
         end do
      end if
   end subroutine

   subroutine put_CIF_ADPn_cryst_1(self,cif,esd)
   ! Output fractional/GC anharmonic ADPs in CIF format to "stdout".
   ! This version produces errors, stored in "esd".
      VEC(type(ATOM_TYPE),:), IN :: self
      type(CIF_TYPE), IN :: cif
      VEC(REAL,:), IN :: esd
      VEC(REAL,15) :: ADP
      VEC(REAL,15) :: err
      STR(len=STR_SIZE) :: lab,ADPn_label
      INT :: i,k
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADPn_cryst_1 ... wrong sizem esd")

      ! ==========
      ! Put ADP3's 
      ! ==========
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_anharm_GC_C_"
         call flush_(stdout)
         call text_(stdout,"# ============================================")
         call text_(stdout,"# Precise 3rd order Gram-Charlier coefficients")
         call text_(stdout,"# ============================================")
         call flush_(stdout)
         call text_(stdout,"# . These are precise parameters and ESDs")
         call text_(stdout,"# . Keep these for a refinement restart")
         call text_(stdout,"# . Better yet, use the refined cartesian parameters")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values and errors (dimensionless)
            call put_ADP3_vector_to_(self(i),ADP(1:10))
            err(1:10) = esd(k+10:k+19)

            ! Scale by scale_factor
            ADP(1:10) = ADP(1:10)*cif%GC3_scale_factor
            err(1:10) = err(1:10)*cif%GC3_scale_factor

            ! Put quantity q with full error.
            call put_(stdout,ADP(1)); call flush_(stdout)
            call put_(stdout,err(1)); call flush_(stdout)
            call put_(stdout,ADP(2)); call flush_(stdout)
            call put_(stdout,err(2)); call flush_(stdout)
            call put_(stdout,ADP(3)); call flush_(stdout)
            call put_(stdout,err(3)); call flush_(stdout)
            call put_(stdout,ADP(4)); call flush_(stdout)
            call put_(stdout,err(4)); call flush_(stdout)
            call put_(stdout,ADP(5)); call flush_(stdout)
            call put_(stdout,err(5)); call flush_(stdout)
            call put_(stdout,ADP(6)); call flush_(stdout)
            call put_(stdout,err(6)); call flush_(stdout)
            call put_(stdout,ADP(7)); call flush_(stdout)
            call put_(stdout,err(7)); call flush_(stdout)
            call put_(stdout,ADP(8)); call flush_(stdout)
            call put_(stdout,err(8)); call flush_(stdout)
            call put_(stdout,ADP(9)); call flush_(stdout)
            call put_(stdout,err(9)); call flush_(stdout)
            call put_(stdout,ADP(10)); call flush_(stdout)
            call put_(stdout,err(10)); call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do

         ! Reset precision for errors
         call save_style_(stdout)
         call set_real_width_(stdout,stdout%saved%style%real_width+3)
         call set_real_precision_(stdout,stdout%saved%style%real_precision+3)

         ! CIF ADP header
         call flush_(stdout)
         call text_(stdout,"# ======================================")
         call text_(stdout,"# Third order Gram-Charlier coefficients")
         call text_(stdout,"# ======================================")
         call flush_(stdout)
         call text_(stdout,"# . These data are in standard presentation")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do

         ! Put ADP's
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values and errors
            call put_ADP3_vector_to_(self(i),ADP(1:10))
            err(1:10) = esd(k+10:k+19)

            ! Scale by scale_factor
            ADP(1:10) = ADP(1:10)*cif%GC3_scale_factor
            err(1:10) = err(1:10)*cif%GC3_scale_factor

            ! Put quantity q with error e in format q(e)
            call put_(stdout,ADP(1),err(1))
            call put_(stdout,ADP(2),err(2))
            call put_(stdout,ADP(3),err(3))
            call flush_(stdout)
            call put_(stdout,ADP(4),err(4))
            call put_(stdout,ADP(5),err(5))
            call flush_(stdout)
            call put_(stdout,ADP(6),err(6))
            call put_(stdout,ADP(7),err(7))
            call flush_(stdout)
            call put_(stdout,ADP(8),err(8))
            call put_(stdout,ADP(9),err(9))
            call flush_(stdout)
            call put_(stdout,ADP(10),err(10))
            call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do

         ! Revert
         call unsave_style_(stdout)
      end if

      ! ==========
      ! Put ADP4's
      ! ==========
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_anharm_GC_D_"
         call flush_(stdout)
         call text_(stdout,"# ============================================")
         call text_(stdout,"# Precise 4th order Gram-Charlier coefficients")
         call text_(stdout,"# ============================================")
         call flush_(stdout)
         call text_(stdout,"# . These are precise parameters and ESDs")
         call text_(stdout,"# . Keep these for a refinement restart")
         call text_(stdout,"# . Better yet, use the refined cartesian parameters")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values and errors (dimensionless)
            call put_ADP4_vector_to_(self(i),ADP(1:15))
            err(1:15) = esd(k+20:k+34)

            ! Scale by scale factor
            ADP(1:15) = ADP(1:15)*cif%GC4_scale_factor
            err(1:15) = err(1:15)*cif%GC4_scale_factor

            ! Put quantity q with full error.
            call put_(stdout,ADP(1)); call flush_(stdout)
            call put_(stdout,err(1)); call flush_(stdout)
            call put_(stdout,ADP(2)); call flush_(stdout)
            call put_(stdout,err(2)); call flush_(stdout)
            call put_(stdout,ADP(3)); call flush_(stdout)
            call put_(stdout,err(3)); call flush_(stdout)
            call put_(stdout,ADP(4)); call flush_(stdout)
            call put_(stdout,err(4)); call flush_(stdout)
            call put_(stdout,ADP(5)); call flush_(stdout)
            call put_(stdout,err(5)); call flush_(stdout)
            call put_(stdout,ADP(6)); call flush_(stdout)
            call put_(stdout,err(6)); call flush_(stdout)
            call put_(stdout,ADP(7)); call flush_(stdout)
            call put_(stdout,err(7)); call flush_(stdout)
            call put_(stdout,ADP(8)); call flush_(stdout)
            call put_(stdout,err(8)); call flush_(stdout)
            call put_(stdout,ADP(9)); call flush_(stdout)
            call put_(stdout,err(9)); call flush_(stdout)
            call put_(stdout,ADP(10)); call flush_(stdout)
            call put_(stdout,err(10)); call flush_(stdout)
            call put_(stdout,ADP(11)); call flush_(stdout)
            call put_(stdout,err(11)); call flush_(stdout)
            call put_(stdout,ADP(12)); call flush_(stdout)
            call put_(stdout,err(12)); call flush_(stdout)
            call put_(stdout,ADP(13)); call flush_(stdout)
            call put_(stdout,err(13)); call flush_(stdout)
            call put_(stdout,ADP(14)); call flush_(stdout)
            call put_(stdout,err(14)); call flush_(stdout)
            call put_(stdout,ADP(15)); call flush_(stdout)
            call put_(stdout,err(15)); call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do

         ! Reset precision for errors
         call save_style_(stdout)
         call set_real_width_(stdout,stdout%saved%style%real_width+3)
         call set_real_precision_(stdout,stdout%saved%style%real_precision+3)

         ! CIF ADP header
         call flush_(stdout)
         call text_(stdout,"# =======================================")
         call text_(stdout,"# Fourth order Gram-Charlier coefficients")
         call text_(stdout,"# =======================================")
         call flush_(stdout)
         call text_(stdout,"# . These data are in standard presentation")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(cif%GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
         end do

         ! Put ADP's
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
            call flush_(stdout)

            ! Get values and errors
            call put_ADP4_vector_to_(self(i),ADP(1:15))
            err(1:15) = esd(k+20:k+34)

            ! Scale by scale_factor
            ADP(1:15) = ADP(1:15)*cif%GC4_scale_factor
            err(1:15) = err(1:15)*cif%GC4_scale_factor

            ! Put quantity q with error e in format q(e)
            call put_(stdout,ADP(1),err(1))
            call put_(stdout,ADP(2),err(2))
            call put_(stdout,ADP(3),err(3))
            call flush_(stdout)
            call put_(stdout,ADP(4),err(4))
            call put_(stdout,ADP(5),err(5))
            call flush_(stdout)
            call put_(stdout,ADP(6),err(6))
            call put_(stdout,ADP(7),err(7))
            call flush_(stdout)
            call put_(stdout,ADP(8),err(8))
            call put_(stdout,ADP(9),err(9))
            call flush_(stdout)
            call put_(stdout,ADP(10),err(10))
            call put_(stdout,ADP(11),err(11))
            call put_(stdout,ADP(12),err(12))
            call flush_(stdout)
            call put_(stdout,ADP(13),err(13))
            call put_(stdout,ADP(14),err(14))
            call put_(stdout,ADP(15),err(15))
            call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do

         ! Revert
         call unsave_style_(stdout)
      end if  ! =========
   end subroutine

   subroutine put_CIF_bond_length_table(self,covariance)
   ! Output the CIF bond length table with errors, given the "covariance"
   ! matrix between all atomic positions. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: n_kind,k1,k2,n_bonds
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: dist,esds
      MAT(INT,:,:), allocatable :: pair
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_bond_length_table ... axis system must be cartesian")

      ! Header
      call flush_(stdout)
      call text_(stdout,"# ============")
      call text_(stdout,"# Bond lengths")
      call text_(stdout,"# ============")

      ! Loop 
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout," _geom_bond_atom_site_label_1")
      call text_(stdout," _geom_bond_site_symmetry_1")
      call text_(stdout," _geom_bond_atom_site_label_2")
      call text_(stdout," _geom_bond_site_symmetry_2")
      call text_(stdout," _geom_bond_distance")
      call text_(stdout," _geom_bond_publ_flag")

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,6)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),self%tag)
      call set_heading_(table(2),".")
      call set_width_(table(2),1)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_heading_(table(4),".")
      call set_width_(table(4),1)
      call set_heading_(table(5),"R(A--B)")
      call set_width_from_(table(5),ONE)
      call set_heading_(table(6),"yes")
      call set_width_(table(6),3)

      ! Loop over atom Z classes, Z(k1) >= Z(k2)
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            call get_bonded_atoms_(self,pair,dist,esds,Z_list,k1,k2,covariance)  ! leaky
            dist = dist*ANGSTROM_PER_BOHR
            esds = esds*ANGSTROM_PER_BOHR

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(esds)
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self(pair(:,1))%tag
            labels(:,2) = self(pair(:,2))%tag

         ! Dashes
            call create_(dash,n_bonds)
            call create_(dsh1,n_bonds)
            dash = "."
            dsh1 = "yes"

         ! Set table data
            call set_values_(table(1),labels(:,1))
            call set_values_(table(2),dash)
            call set_values_(table(3),labels(:,2))
            call set_values_(table(4),dash)
            call set_values_and_errors_(table(5),dist,esds)
            call set_values_(table(6),dsh1)

         ! Put body
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(dsh1)
            call destroy_(dash)
            call destroy_(labels)
            call destroy_(esds)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_CIF_bond_Hirshfeld_table(self,cov_R,cov_pADP)
   ! Output the CIF bond length table with errors, given the "covariance"
   ! matrix between all atomic positions. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov_R,cov_pADP
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: n_kind,k1,k2,n_bonds
      MAT(STR(len=STR_SIZE),:,:), allocatable :: labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: dist,derr,test,terr
      MAT(INT,:,:), allocatable :: pair
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_bond_Hirshfeld_table ... axis system must be cartesian")

      ! Header
      call flush_(stdout)
      call text_(stdout,"# ==============================")
      call text_(stdout,"# Bond lengths & Hirshfeld tests")
      call text_(stdout,"# ==============================")

      ! Loop 
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout," _geom_bond_atom_site_label_1")
      call text_(stdout," _geom_bond_site_symmetry_1")
      call text_(stdout," _geom_bond_atom_site_label_2")
      call text_(stdout," _geom_bond_site_symmetry_2")
      call text_(stdout," _geom_bond_distance")
      call text_(stdout," _geom_bond_distance_esu")
      call text_(stdout," _geom_bond_Hirshfeld_test")
      call text_(stdout," _geom_bond_Hirshfeld_test_esu")

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,8)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),self%tag)
      call set_heading_(table(2),".")
      call set_width_(table(2),1)
      call set_heading_(table(3),"B")
      call set_width_(table(3),table(1)%width)
      call set_heading_(table(4),".")
      call set_width_(table(4),1)
      call set_heading_(table(5),"R(A--B)")
      call set_width_from_(table(5),ONE)
      call set_heading_(table(6),"R(A--B) esu")
      call set_width_from_(table(6),ONE)
      call set_heading_(table(7),"Hirshfeld test")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"Hirshfeld test esu")
      call set_width_from_(table(8),ONE)

      ! Loop over atom Z classes, Z(k1) >= Z(k2)
      do k1 = 1,n_kind
         do k2 = k1,n_kind

         ! Get the bonded atoms per kind
            call get_bonded_Hirshfeld_test_(self,pair,dist,derr,test,terr,Z_list,k1,k2,cov_R,cov_pADP,angstrom=TRUE)

         ! No of bonds in this class
            n_bonds = size(dist)
            if (n_bonds==0) then
               call destroy_(terr)
               call destroy_(test)
               call destroy_(derr)
               call destroy_(dist)
               call destroy_(pair)
               cycle
            end if

         ! Labels
            call create_(labels,n_bonds,2)
            labels(:,1) = self(pair(:,1))%tag
            labels(:,2) = self(pair(:,2))%tag

         ! Dashes
            call create_(dash,n_bonds)
            dash = "."

         ! Set table data
            call set_values_(table(1),labels(:,1))
            call set_values_(table(2),dash)
            call set_values_(table(3),labels(:,2))
            call set_values_(table(4),dash)
            call set_values_(table(5),dist)
            call set_values_(table(6),derr)
            call set_values_(table(7),test)
            call set_values_(table(8),terr)

         ! Put body
            call put_body_(table,label_rows=FALSE)

         ! Clean
            call clear_columns_(table)
            call destroy_(dash)
            call destroy_(labels)
            call destroy_(terr)
            call destroy_(test)
            call destroy_(derr)
            call destroy_(dist)
            call destroy_(pair)
         end do
      end do

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_CIF_bond_angle_table(self,covariance)
   ! Output the CIF bond angle table with errors given the "covariance"
   ! matrix between all atomic positions. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: n_kind,k1,k2,k3,n_angles
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols
      VEC(STR(len=STR_SIZE),:), allocatable :: dash,dsh1
      VEC(REAL,:), allocatable :: angle,esds
      MAT(INT,:,:), allocatable :: triple
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_bond_angle_table ... axis system must be cartesian")

      ! Title
      call flush_(stdout)
      call text_(stdout,"# ===========")
      call text_(stdout,"# Bond Angles")
      call text_(stdout,"# ===========")

      ! Loop 
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout," _geom_angle_atom_site_label_1")
      call text_(stdout," _geom_angle_site_symmetry_1")
      call text_(stdout," _geom_angle_atom_site_label_2")
      call text_(stdout," _geom_angle_site_symmetry_2")
      call text_(stdout," _geom_angle_atom_site_label_3")
      call text_(stdout," _geom_angle_site_symmetry_3")
      call text_(stdout," _geom_angle")
      call text_(stdout," _geom_angle_publ_flag")

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,8)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),self(angle_outer_atoms_(self))%tag)
      call set_heading_(table(2),".")
      call set_width_(table(2),1)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),self(angle_center_atoms_(self))%tag)
      call set_heading_(table(4),".")
      call set_width_(table(4),1)
      call set_heading_(table(5),"C")
      call set_width_from_(table(5),self(angle_outer_atoms_(self))%tag)
      call set_heading_(table(6),".")
      call set_width_(table(6),1)
      call set_heading_(table(7),"Angle(A--B--C)")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"yes")
      call set_width_(table(8),3)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = k1,n_kind

         ! Get the bonded atoms per kind
               call get_bonded_angles_(self,triple,angle,esds,Z_list,k1,k2,k3,covariance)
               angle = angle*DEGREE_PER_RADIAN
               esds = esds*DEGREE_PER_RADIAN

         ! No of bonds in this class
               n_angles = size(angle)
               if (n_angles==0) then
                  call destroy_(esds)
                  call destroy_(angle)
                  call destroy_(triple)
                  cycle
               end if

         ! Element symbols
               call create_(symbols,n_angles,3)
               symbols = " "
               symbols(:,1) = self(triple(:,1))%tag
               symbols(:,2) = self(triple(:,2))%tag
               symbols(:,3) = self(triple(:,3))%tag

         ! Dashes
               call create_(dash,n_angles)
               call create_(dsh1,n_angles)
               dash = "."
               dsh1 = "yes"

         ! Set table data
               call set_values_(table(1),symbols(:,1))
               call set_values_(table(2),dash)
               call set_values_(table(3),symbols(:,2))
               call set_values_(table(4),dash)
               call set_values_(table(5),symbols(:,3))
               call set_values_(table(6),dash)
               call set_values_and_errors_(table(7),angle,esds)
               call set_values_(table(8),dsh1)

         ! Put body
               call put_body_(table,label_rows=FALSE)

         ! Clean
               call clear_columns_(table)
               call destroy_(dsh1)
               call destroy_(dash)
               call destroy_(symbols)
               call destroy_(esds)
               call destroy_(angle)
               call destroy_(triple)
            end do
         end do
      end do

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_CIF_bond_torsion_angle_table(self,covariance,parentheses)
   ! Output the CIF bond angle table with errors given the "covariance"
   ! matrix between all atomic positions. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      BIN, optional, IN :: parentheses
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols,labels
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: angle,esds
      MAT(INT,:,:), allocatable :: quad
      INT :: no_of_torsions,n_kind,k1,k2,k3,k4,n_angles
      BIN :: use_brackets
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_bond_torsion_angle_table ... axis system must be cartesian")
      if (size(self)<4) return
      no_of_torsions = no_of_torsion_angles_(self)
      if (no_of_torsions<1) return

      ! Title
      call flush_(stdout)
      call text_(stdout,"# ==============")
      call text_(stdout,"# Torsion angles")
      call text_(stdout,"# ==============")

      ! Parentheses
      use_brackets = TRUE
      if (present(parentheses)) use_brackets = parentheses

      ! Loop 
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout," _geom_torsion_atom_site_label_1")
      call text_(stdout," _geom_torsion_site_symmetry_1")
      call text_(stdout," _geom_torsion_atom_site_label_2")
      call text_(stdout," _geom_torsion_site_symmetry_2")
      call text_(stdout," _geom_torsion_atom_site_label_3")
      call text_(stdout," _geom_torsion_site_symmetry_3")
      call text_(stdout," _geom_torsion_atom_site_label_4")
      call text_(stdout," _geom_torsion_site_symmetry_4")
      call text_(stdout," _geom_torsion")
      if (NOT use_brackets) then
         call text_(stdout," _geom_torsion_esu")
      end if

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      if (use_brackets) then; call create_(table,9)
      else; call create_(table,10)
      end if

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),self(torsion_angle_1st_atoms_(self))%tag)
      call set_heading_(table(2),".")
      call set_width_(table(2),1)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),self(torsion_angle_2nd_atoms_(self))%tag)
      call set_heading_(table(4),".")
      call set_width_(table(4),1)
      call set_heading_(table(5),"C")
      call set_width_from_(table(5),self(torsion_angle_3rd_atoms_(self))%tag)
      call set_heading_(table(6),".")
      call set_width_(table(6),1)
      call set_heading_(table(7),"D")
      call set_width_from_(table(7),self(torsion_angle_4th_atoms_(self))%tag)
      call set_heading_(table(8),".")
      call set_width_(table(8),1)
      call set_heading_(table(9),"Torsion(A--B--C--D)")
      call set_width_from_(table(9),ONE)
      if (NOT use_brackets) then
         call set_heading_(table(10),"Torsion(A--B--C--D) esu")
         call set_width_from_(table(10),ONE)
      end if

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = 1,n_kind
               do k4 = 1,n_kind

         ! Get the bonded atoms per kind
                  call get_bonded_torsions_(self,quad,angle,esds,Z_list,k1,k2,k3,k4,covariance)
                  angle = angle*DEGREE_PER_RADIAN
                  esds = esds*DEGREE_PER_RADIAN

         ! No of bonds in this class
                  n_angles = size(angle)
                  if (n_angles==0) then
                     call destroy_(esds)
                     call destroy_(angle)
                     call destroy_(quad)
                     cycle
                  end if

         ! Element symbols
                  call create_(symbols,n_angles,4)
                  symbols = " "
                  symbols(1,1) = chemical_symbol_(self(Z_list(k1)%element(1)))
                  symbols(1,2) = chemical_symbol_(self(Z_list(k2)%element(1)))
                  symbols(1,3) = chemical_symbol_(self(Z_list(k3)%element(1)))
                  symbols(1,4) = chemical_symbol_(self(Z_list(k4)%element(1)))

         ! Labels
                  call create_(labels,n_angles,4)
                  labels(:,1) = self(quad(:,1))%tag
                  labels(:,2) = self(quad(:,2))%tag
                  labels(:,3) = self(quad(:,3))%tag
                  labels(:,4) = self(quad(:,4))%tag

         ! Dashes
                  call create_(dash,n_angles)
                  dash = "."

         ! Set table data
                  call set_values_(table(1),labels(:,1))
                  call set_values_(table(2),dash)
                  call set_values_(table(3),labels(:,2))
                  call set_values_(table(4),dash)
                  call set_values_(table(5),labels(:,3))
                  call set_values_(table(6),dash)
                  call set_values_(table(7),labels(:,4))
                  call set_values_(table(8),dash)
                  if (use_brackets) then
                     call set_real_precision_(table(9),stdout%style%real_precision)  ! Must do, for every block
                     call set_values_and_errors_(table(9),angle,esds)
                  else
                     call set_values_(table(9),angle)
                     call set_values_(table(10),esds)
                  end if

         ! Put body
                  call put_body_(table,label_rows=FALSE)

         ! Clean
                  call clear_columns_(table)
                  call destroy_(dash)
                  call destroy_(symbols)
                  call destroy_(esds)
                  call destroy_(angle)
                  call destroy_(quad)
               end do
            end do
         end do
      end do

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_CIF_coords_cartn_HBB(self,refine_H_U_iso,esd)
   ! Output the atoms and ADP's in CIF format to "stdout" in an
   ! axis system, with errors "esd" provided externally.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, IN :: refine_H_U_iso
      VEC(REAL,:), IN :: esd
      VEC(STR(len=STR_SIZE),:), allocatable :: blnk,ADP_type
      VEC(REAL,:), allocatable :: Uiso,Uerr
      MAT(REAL,:,:), allocatable :: xyz,err
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1,tabl2,tabl3,tabl4
      REAL :: fac,fac2
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_coords_cartn_HBB ... wrong size, esd")
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_coords_cartn_HBB ... not cartesian axis system!")

      ! Unit conversion
      fac = ANGSTROM_PER_BOHR
      fac2 = fac*fac

      ! CIF coordinate header
      call flush_(stdout)
      call text_(stdout,"# =====================")
      call text_(stdout,"# Cartesian coordinates")
      call text_(stdout,"# =====================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cartn_label")
      call text_(stdout,"_atom_site_Cartn_x")
      call text_(stdout,"_atom_site_Cartn_x_esu")
      call text_(stdout,"_atom_site_Cartn_y")
      call text_(stdout,"_atom_site_Cartn_y_esu")
      call text_(stdout,"_atom_site_Cartn_z")
      call text_(stdout,"_atom_site_Cartn_z_esu")

      ! Table column data
      call create_(blnk,size(self))
      call create_(xyz,size(self),3)
      call create_(err,size(self),3)
      call create_(Uiso,size(self))
      call create_(Uerr,size(self))
      call create_(ADP_type,size(self))

      ! Get table column data
      blnk = " "
      call put_pos_vector_to_(self,xyz,fac)
      call put_pos_errors_to_(self,err,esd,fac)  ! Note esd
      Uiso = self%U_iso*fac2
      Uerr = self%U_iso_error*fac2
      ADP_type = ADP_type_(self,refine_H_U_iso)

      ! Table 1
      call create_(tabl1,3)
      call set_values_(tabl1(1),self(:)%tag)
      call set_values_(tabl1(2),xyz(:,1))
      call set_values_(tabl1(3),err(:,1))

      ! Table 2
      call create_(tabl2,3)
      call set_values_(tabl2(1),blnk)
      call set_values_(tabl2(2),xyz(:,2))
      call set_values_(tabl2(3),err(:,2))
      call set_width_(tabl2(1),tabl1(1)%width)
      call set_width_(tabl2(2),tabl1(2)%width)
      call set_width_(tabl2(3),tabl1(3)%width)

      ! Table 3
      call create_(tabl3,3)
      call set_values_(tabl3(1),blnk)
      call set_values_(tabl3(2),xyz(:,3))
      call set_values_(tabl3(3),err(:,3))
      call set_width_(tabl3(1),tabl1(1)%width)
      call set_width_(tabl3(2),tabl1(2)%width)
      call set_width_(tabl3(3),tabl1(3)%width)

      ! Put tables
      call put_body_(tabl1,tabl1,tabl2,tabl3)

      ! CIF Ueq or Uiso header
      call flush_(stdout)
      call text_(stdout,"# =====================")
      call text_(stdout,"# Ueq or Uiso          ")
      call text_(stdout,"# =====================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom^2")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cartn_U_iso_or_equiv")
      call text_(stdout,"_atom_site_Cartn_U_iso_or_equiv_esu")

      ! Table 4
      call create_(tabl4,3)
      call set_values_(tabl4(1),self(:)%tag)
      call set_values_(tabl4(2),Uiso)
      call set_values_(tabl4(3),Uerr)

      ! Put tables
      call put_body_(tabl4)

      ! Clean
      call destroy_(tabl4)
      call destroy_(tabl3)
      call destroy_(tabl2)
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_ADP2_cartn_HBB(self,refine_H_U_iso,esd)
   ! Output the ADP2's in CIF format to "stdout" in cartesian axis system.
   ! If present and TRUE "refine_H_U_iso" removed H ADP printing.
   ! If present and TRUE "esd" array for whole atom list overrides errors.
      VEC(type(ATOM_TYPE),:), IN :: self
      BIN, optional, IN :: refine_H_U_iso
      VEC(REAL,:), optional, IN :: esd
      VEC(INT,:), allocatable :: list
      VEC(STR(len=STR_SIZE),:), allocatable :: ID,blnk
      MAT(REAL,:,:), allocatable :: ADPv,ADPe
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: tabl1,tabl2,tabl3,tabl4,tabl5,tabl6
      STR(len=STR_SIZE) :: lab
      REAL :: fac
      INT :: i,dim
      BIN :: H_U_iso
   ENSURE(all(self(:)%ADP_axis_system=="cartesian"),"VEC{ATOM}:put_CIF_ADP2_cartn_HBB ... ADP axis system must be cartesian")
      H_U_iso = FALSE
      if (present(refine_H_U_iso)) H_U_iso = refine_H_U_iso

      ! Unit conversion for ADP's
      fac = ANGSTROM_PER_BOHR

      ! CIF ADP header
      call flush_(stdout)
      call text_(stdout,"# ==============")
      call text_(stdout,"# Cartesian ADPs")
      call text_(stdout,"# ==============")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom^2")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,"_atom_site_Cartn_ADP2_U_label")
      do i = 1,6
         lab = ADP2_index_label_(i,form="123-repeated")
         call text_(stdout,"_atom_site_Cartn_ADP2_U_"//trim(lab))
         lab = ADP2_index_label_(i,form="123-repeated")
         call text_(stdout,"_atom_site_Cartn_ADP2_U_"//trim(lab)//"_esu")
      end do

      ! Size of table
      dim = size(self)
      if (H_U_iso) dim = no_of_non_H_atoms_(self)

      ! Allocate column data
      call create_(ID,dim)
      call create_(blnk,dim)
      call create_(ADPv,dim,6)
      call create_(ADPe,dim,6)

      ! Get column data
      if (NOT H_U_iso) then
         ID = self%tag
         blnk = " "
         call put_ADP2_vector_to_(self,ADPv,fac)
         if (NOT present(esd)) then; call put_ADP2_errors_to_(self,ADPe,fac)
         else; call put_ADP2_errors_to_(self,ADPe,fac,esd)
         end if
      else
         list = list_of_non_H_atoms_(self)
         ID = self(list)%tag
         blnk = " "
         call put_ADP2_vector_to_(self(list),ADPv,fac)
         if (NOT present(esd)) then; call put_ADP2_errors_to_(self(list),ADPe,fac)
         else; call put_ADP2_errors_to_(self,ADPe,list,fac,esd)
         end if
      end if

      ! Table 1
      call create_(tabl1,3)
      call set_values_(tabl1(1),ID)
      call set_values_(tabl1(2),ADPv(:,1))
      call set_values_(tabl1(3),ADPe(:,1))

      ! Table 2
      call create_(tabl2,3)
      call set_values_(tabl2(1),blnk)
      call set_values_(tabl2(2),ADPv(:,2))
      call set_values_(tabl2(3),ADPe(:,2))
      call set_width_(tabl2(1),tabl1(1)%width)
      call set_width_(tabl2(2),tabl1(2)%width)
      call set_width_(tabl2(3),tabl1(3)%width)

      ! Table 3
      call create_(tabl3,3)
      call set_values_(tabl3(1),blnk)
      call set_values_(tabl3(2),ADPv(:,3))
      call set_values_(tabl3(3),ADPe(:,3))
      call set_width_(tabl3(1),tabl1(1)%width)
      call set_width_(tabl3(2),tabl1(2)%width)
      call set_width_(tabl3(3),tabl1(3)%width)

      ! Table 4
      call create_(tabl4,3)
      call set_values_(tabl4(1),blnk)
      call set_values_(tabl4(2),ADPv(:,4))
      call set_values_(tabl4(3),ADPe(:,4))
      call set_width_(tabl4(1),tabl1(1)%width)
      call set_width_(tabl4(2),tabl1(2)%width)
      call set_width_(tabl4(3),tabl1(3)%width)

      ! Table 5
      call create_(tabl5,3)
      call set_values_(tabl5(1),blnk)
      call set_values_(tabl5(2),ADPv(:,5))
      call set_values_(tabl5(3),ADPe(:,5))
      call set_width_(tabl5(1),tabl1(1)%width)
      call set_width_(tabl5(2),tabl1(2)%width)
      call set_width_(tabl5(3),tabl1(3)%width)

      ! Table 6
      call create_(tabl6,3)
      call set_values_(tabl6(1),blnk)
      call set_values_(tabl6(2),ADPv(:,6))
      call set_values_(tabl6(3),ADPe(:,6))
      call set_width_(tabl6(1),tabl1(1)%width)
      call set_width_(tabl6(2),tabl1(2)%width)
      call set_width_(tabl6(3),tabl1(3)%width)

      ! Put tables
      call put_body_(tabl1,tabl1,tabl2,tabl3,tabl4,tabl5,tabl6)

      ! Clean
      call destroy_(tabl6)
      call destroy_(tabl5)
      call destroy_(tabl4)
      call destroy_(tabl3)
      call destroy_(tabl2)
      call destroy_(tabl1)
   end subroutine

   subroutine put_CIF_ADP2_in_inertial_axes_cartn_HBB(self,cov,refine_H_U_iso)
   ! Output cartesian ADP2s in the inertial axis frame, 
   ! in CIF format to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      BIN, optional, IN :: refine_H_U_iso
      STR(len=STR_SIZE) :: lab,ADP2_label
      VEC(REAL,6) :: ADP2v,ADP2s
      REAL :: fac1,fac2
      MAT(REAL,:,:), allocatable :: U,dU
      MAT(REAL,3,3) :: axes
      INT :: i,dim
      BIN :: H_U_iso
   ENSURE(is_square_(cov),"VEC{ATOM}:put_CIF_ADP2_in_inertial_axes_cartn_HBB ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP2_in_inertial_axes_cartn_HBB ... wrong size, covariance matrix!")
      H_U_iso = FALSE
      if (present(refine_H_U_iso)) H_U_iso = refine_H_U_iso

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1

      ! Get the quntities
      call make_inertial_axes_(self,axes)
      call get_ADP2s_in_new_axes_in_(self,U,dU,axes,cov)

      ! Put the ADP2's
      ADP2_label = "_atom_site_ADP2_in_inertial_U_"
      call flush_(stdout)
      call text_(stdout,"# ===================================")
      call text_(stdout,"# Cartesian ADP2s inertial axis frame")
      call text_(stdout,"# ===================================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Units are in Angstrom^2")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,trim(ADP2_label)//"label")
      do i = 1,6
         lab = ADP2_index_label_(i,form="123-repeated")
         call put_(stdout,trim(trim(ADP2_label)//trim(lab)),left=TRUE)
         call put_(stdout,trim(trim(ADP2_label)//trim(lab)//trim("_esu")),left=TRUE)
         call flush_(stdout)
      end do

      ! Size of table
      dim = size(self)
      if (H_U_iso) dim = no_of_non_H_atoms_(self)

!     ! Get column data
!     if (NOT H_U_iso) then
      do i = 1,dim
         call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)

            ! Units !!!!!!!!!!!!
         ADP2v = fac2*U(i,1:6)
         ADP2s = fac2*dU(i,1:6)

            ! Print ADP2
         call put_(stdout,ADP2v(1))
         call put_(stdout,ADP2s(1))
         call flush_(stdout)
         call put_(stdout,ADP2v(2))
         call put_(stdout,ADP2s(2))
         call flush_(stdout)
         call put_(stdout,ADP2v(3))
         call put_(stdout,ADP2s(3))
         call flush_(stdout)
         call put_(stdout,ADP2v(4))
         call put_(stdout,ADP2s(4))
         call flush_(stdout)
         call put_(stdout,ADP2v(5))
         call put_(stdout,ADP2s(5))
         call flush_(stdout)
         call put_(stdout,ADP2v(6))
         call put_(stdout,ADP2s(6))
         call flush_(stdout)
      end do

!     end

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine put_CIF_ADP2_in_ADP2_principal_axes_cartn_HBB(self,cov,refine_H_U_iso)
   ! Output cartesian ADP2s in the ADP2 principal axis frame (for
   ! checking) in CIF format to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      BIN, optional, IN :: refine_H_U_iso
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,6) :: ADP2v,ADP2s
      REAL :: fac1,fac2
      MAT(REAL,:,:), allocatable :: U,dU
      INT :: i,n,dim
      BIN :: H_U_iso
   ENSURE(is_square_(cov),"VEC{ATOM}:put_CIF_ADP2_in_ADP2_principal_axes_cartn_HBB ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP2_in_ADP2_principal_axes_cartn_HBB ... wrong size, covariance matrix!")

      ! NOT WORKING - CHEcK - dylan
      return
      H_U_iso = FALSE
      if (present(refine_H_U_iso)) H_U_iso = refine_H_U_iso

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1

      ! Get the quntities
      call get_ADP2s_in_ADP2_principal_axes_in_(self,U,dU,cov)

      ! Put the ADP2's
!     if (.has_any_ADP2s_and_errors) then
      ADPn_label = "_atom_site_ADP2_in_principal_U_"
      call flush_(stdout)
      call text_(stdout,"# ============================================")
      call text_(stdout,"# Cartesian ADP2s in ADP2 principal axis frame")
      call text_(stdout,"# ============================================")
      call flush_(stdout)
      call text_(stdout,"# . These precise data use non-standard CIF tags")
      call text_(stdout,"# . Please keep for a better refinement restart")
      call text_(stdout,"# . Units are in Angstrom^2")
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout,trim(ADPn_label)//"label")
      do i = 1,6
         lab = ADP2_index_label_(i,form="123-repeated")
         call put_(stdout,trim(trim(ADPn_label)//trim(lab)),left=TRUE)
         call put_(stdout,trim(trim(ADPn_label)//trim(lab)//trim("_esu")),left=TRUE)
         call flush_(stdout)
      end do
      n = 0

         ! Size of table
      dim = size(self)
      if (H_U_iso) dim = no_of_non_H_atoms_(self)

!        ! Get column data
!        if (NOT H_U_iso) then
      do i = 1,dim

               ! ADP2 atom index
         n = n+1
         call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)

               ! Units !!!!!!!!!!!!
         ADP2v = fac2*U(n,1:6)
         ADP2s = fac2*dU(n,1:6)

               ! Print ADP2
         call put_(stdout,ADP2v(1))
         call put_(stdout,ADP2s(1))
         call flush_(stdout)
         call put_(stdout,ADP2v(2))
         call put_(stdout,ADP2s(2))
         call flush_(stdout)
         call put_(stdout,ADP2v(3))
         call put_(stdout,ADP2s(3))
         call flush_(stdout)
         call put_(stdout,ADP2v(4))
         call put_(stdout,ADP2s(4))
         call flush_(stdout)
         call put_(stdout,ADP2v(5))
         call put_(stdout,ADP2s(5))
         call flush_(stdout)
         call put_(stdout,ADP2v(6))
         call put_(stdout,ADP2s(6))
         call flush_(stdout)
      end do

!        end
!     end

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine put_CIF_ADP3_cartn_HBB(self,esd,GC3_scale_factor)
   ! Output cartesian anharmonic ADPs in CIF format to "stdout".
   ! This version produces errors, stored in "esd".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), IN :: esd
      REAL, IN :: GC3_scale_factor
      VEC(REAL,15) :: ADP
      VEC(REAL,15) :: err
      STR(len=STR_SIZE) :: lab,ADPn_label
      INT :: i,k
      REAL :: fac1,fac2,fac3
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP3_cartn_HBB ... wrong sizem esd")

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1

      ! ==========
      ! Put ADP3's 
      ! ==========
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_Cartn_ADP3_U_"
         call flush_(stdout)
         call text_(stdout,"# ===============")
         call text_(stdout,"# Cartesian ADP3s")
         call text_(stdout,"# ===============")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^3 but may be scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
!            stdout.flush

            ! Get values and errors
            call put_ADP3_vector_to_(self(i),ADP(1:10))
            err(1:10) = esd(k+10:k+19)

            ! Units !!!!!!!!!!!!!!!!!!
            ADP(1:10) = ADP(1:10)*fac3
            err(1:10) = err(1:10)*fac3

            ! Scale by scale factor
            ADP(1:10) = ADP(1:10)*GC3_scale_factor
            err(1:10) = err(1:10)*GC3_scale_factor

            ! Put quantity q with full error.
            call put_(stdout,ADP(1))
            call put_(stdout,err(1))
            call flush_(stdout)
            call put_(stdout,ADP(2))
            call put_(stdout,err(2))
            call flush_(stdout)
            call put_(stdout,ADP(3))
            call put_(stdout,err(3))
            call flush_(stdout)
            call put_(stdout,ADP(4))
            call put_(stdout,err(4))
            call flush_(stdout)
            call put_(stdout,ADP(5))
            call put_(stdout,err(5))
            call flush_(stdout)
            call put_(stdout,ADP(6))
            call put_(stdout,err(6))
            call flush_(stdout)
            call put_(stdout,ADP(7))
            call put_(stdout,err(7))
            call flush_(stdout)
            call put_(stdout,ADP(8))
            call put_(stdout,err(8))
            call flush_(stdout)
            call put_(stdout,ADP(9))
            call put_(stdout,err(9))
            call flush_(stdout)
            call put_(stdout,ADP(10))
            call put_(stdout,err(10))
            call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do
      end if
   end subroutine

   subroutine put_CIF_ADP3_in_inertial_axes_cartn_HBB(self,cov,GC3_scale_factor)
   ! Output cartesian anharmonic ADPs in the ADP2 principal axis
   ! frame, in CIF format to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      REAL, IN :: GC3_scale_factor
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,10) :: ADP3v,ADP3s
      REAL :: fac1,fac2,fac3
      MAT(REAL,:,:), allocatable :: U,dU
      MAT(REAL,3,3) :: axes
      INT :: i,n
   ENSURE(is_square_(cov),"VEC{ATOM}:put_CIF_ADP3_in_inertial_axes_cartn_HBB ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP3_in_inertial_axes_cartn_HBB ... wrong size, covariance matrix!")

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1

      ! Get the quntities
      call make_inertial_axes_(self,axes)
      call get_ADP3s_in_new_axes_in_(self,U,dU,axes,cov)

      ! Put the ADP3's
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_ADP3_in_inertial_U_"
         call flush_(stdout)
         call text_(stdout,"# ===========================================")
         call text_(stdout,"# Cartesian ADP3s in ADP2 inertial axis frame")
         call text_(stdout,"# ===========================================")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^3 but are scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)),left=TRUE)
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)//trim("_esu")),left=TRUE)
            call flush_(stdout)
         end do
         n = 0
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle

            ! ADP3 atom index
            n = n+1
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
!           stdout.flush

            ! Units !!!!!!!!!!!!
            ADP3v = fac3*U(n,1:10)
            ADP3s = fac3*dU(n,1:10)

            ! Scale factor
            ADP3v = GC3_scale_factor*ADP3v
            ADP3s = GC3_scale_factor*ADP3s

            ! Print ADP3
            call put_(stdout,ADP3v(1))
            call put_(stdout,ADP3s(1))
            call flush_(stdout)
            call put_(stdout,ADP3v(2))
            call put_(stdout,ADP3s(2))
            call flush_(stdout)
            call put_(stdout,ADP3v(3))
            call put_(stdout,ADP3s(3))
            call flush_(stdout)
            call put_(stdout,ADP3v(4))
            call put_(stdout,ADP3s(4))
            call flush_(stdout)
            call put_(stdout,ADP3v(5))
            call put_(stdout,ADP3s(5))
            call flush_(stdout)
            call put_(stdout,ADP3v(6))
            call put_(stdout,ADP3s(6))
            call flush_(stdout)
            call put_(stdout,ADP3v(7))
            call put_(stdout,ADP3s(7))
            call flush_(stdout)
            call put_(stdout,ADP3v(8))
            call put_(stdout,ADP3s(8))
            call flush_(stdout)
            call put_(stdout,ADP3v(9))
            call put_(stdout,ADP3s(9))
            call flush_(stdout)
            call put_(stdout,ADP3v(10))
            call put_(stdout,ADP3s(10))
            call flush_(stdout)
         end do
      end if

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine put_CIF_ADP3_in_ADP2_principal_axes_cartn_HBB(self,cov,GC3_scale_factor)
   ! Output cartesian anharmonic ADPs in the ADP2 principal axis
   ! frame, in CIF format to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      REAL, IN :: GC3_scale_factor
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,10) :: ADP3v,ADP3s
      REAL :: fac1,fac2,fac3
      MAT(REAL,:,:), allocatable :: U,dU
      INT :: i,n
   ENSURE(is_square_(cov),"VEC{ATOM}:put_CIF_ADP3_in_ADP2_principal_axes_cartn_HBB ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP3_in_ADP2_principal_axes_cartn_HBB ... wrong size, covariance matrix!")

      ! NOT WORKING - CHEcK - dylan
      return

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1

      ! Get the quntities
      call get_ADP3s_in_ADP2_principal_axes_in_(self,U,dU,cov)

      ! Put the ADP3's
      if (has_any_ADP3s_and_errors_(self)) then
         ADPn_label = "_atom_site_ADP3_in_principal_U_"
         call flush_(stdout)
         call text_(stdout,"# ============================================")
         call text_(stdout,"# Cartesian ADP3s in ADP2 principal axis frame")
         call text_(stdout,"# ============================================")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^3 but are scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(GC3_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,10
            lab = ADP3_index_label_(i,form="123-repeated")
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)),left=TRUE)
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)//trim("_esu")),left=TRUE)
            call flush_(stdout)
         end do
         n = 0
         do i = 1,size(self)
            if (NOT has_ADP3s_and_errors_(self(i))) cycle

            ! ADP3 atom index
            n = n+1
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
!           stdout.flush

            ! Units !!!!!!!!!!!!
            ADP3v = fac3*U(n,1:10)
            ADP3s = fac3*dU(n,1:10)

            ! Scale factor
            ADP3v = GC3_scale_factor*ADP3v
            ADP3s = GC3_scale_factor*ADP3s

            ! Print ADP3
            call put_(stdout,ADP3v(1))
            call put_(stdout,ADP3s(1))
            call flush_(stdout)
            call put_(stdout,ADP3v(2))
            call put_(stdout,ADP3s(2))
            call flush_(stdout)
            call put_(stdout,ADP3v(3))
            call put_(stdout,ADP3s(3))
            call flush_(stdout)
            call put_(stdout,ADP3v(4))
            call put_(stdout,ADP3s(4))
            call flush_(stdout)
            call put_(stdout,ADP3v(5))
            call put_(stdout,ADP3s(5))
            call flush_(stdout)
            call put_(stdout,ADP3v(6))
            call put_(stdout,ADP3s(6))
            call flush_(stdout)
            call put_(stdout,ADP3v(7))
            call put_(stdout,ADP3s(7))
            call flush_(stdout)
            call put_(stdout,ADP3v(8))
            call put_(stdout,ADP3s(8))
            call flush_(stdout)
            call put_(stdout,ADP3v(9))
            call put_(stdout,ADP3s(9))
            call flush_(stdout)
            call put_(stdout,ADP3v(10))
            call put_(stdout,ADP3s(10))
            call flush_(stdout)
         end do
      end if

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine put_CIF_ADP4_cartn_HBB(self,esd,GC4_scale_factor)
   ! Output cartesian anharmonic ADPs in CIF format to "stdout".
   ! This version produces errors, stored in "esd".
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), IN :: esd
      REAL, IN :: GC4_scale_factor
      VEC(REAL,15) :: ADP
      VEC(REAL,15) :: err
      STR(len=STR_SIZE) :: lab,ADPn_label
      INT :: i,k
      REAL :: fac1,fac2
      REAL :: fac3,fac4
   ENSURE(size(esd)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP4_cartn_HBB ... wrong sizem esd")

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1
      fac4 = fac2*fac2

      ! ==========
      ! Put ADP4's
      ! ==========
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_Cartn_ADP4_U_"
         call flush_(stdout)
         call text_(stdout,"# ===============")
         call text_(stdout,"# Cartesian ADP4s")
         call text_(stdout,"# ===============")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^4")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call text_(stdout,trim(ADPn_label)//trim(lab))
            call text_(stdout,trim(ADPn_label)//trim(lab)//"_esu")
         end do
         k = 0
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
!           stdout.flush

            ! Get values and errors (dimensionless)
            call put_ADP4_vector_to_(self(i),ADP(1:15))
            err(1:15) = esd(k+20:k+34)

            ! Units !!!!!!!!!!!!!!!!!!
            ADP(1:15) = ADP(1:15)*fac4
            err(1:15) = err(1:15)*fac4

            ! Scale by scale factor
            ADP(1:15) = ADP(1:15)*GC4_scale_factor
            err(1:15) = err(1:15)*GC4_scale_factor

            ! Put quantity q with full error.
            call put_(stdout,ADP(1))
            call put_(stdout,err(1))
            call flush_(stdout)
            call put_(stdout,ADP(2))
            call put_(stdout,err(2))
            call flush_(stdout)
            call put_(stdout,ADP(3))
            call put_(stdout,err(3))
            call flush_(stdout)
            call put_(stdout,ADP(4))
            call put_(stdout,err(4))
            call flush_(stdout)
            call put_(stdout,ADP(5))
            call put_(stdout,err(5))
            call flush_(stdout)
            call put_(stdout,ADP(6))
            call put_(stdout,err(6))
            call flush_(stdout)
            call put_(stdout,ADP(7))
            call put_(stdout,err(7))
            call flush_(stdout)
            call put_(stdout,ADP(8))
            call put_(stdout,err(8))
            call flush_(stdout)
            call put_(stdout,ADP(9))
            call put_(stdout,err(9))
            call flush_(stdout)
            call put_(stdout,ADP(10))
            call put_(stdout,err(10))
            call flush_(stdout)
            call put_(stdout,ADP(11))
            call put_(stdout,err(11))
            call flush_(stdout)
            call put_(stdout,ADP(12))
            call put_(stdout,err(12))
            call flush_(stdout)
            call put_(stdout,ADP(13))
            call put_(stdout,err(13))
            call flush_(stdout)
            call put_(stdout,ADP(14))
            call put_(stdout,err(14))
            call flush_(stdout)
            call put_(stdout,ADP(15))
            call put_(stdout,err(15))
            call flush_(stdout)
            k = k+no_of_pADPs_(self(i))
         end do
      end if  ! =========
   end subroutine

   subroutine put_CIF_ADP4_in_inertial_axes_cartn_HBB(self,cov,GC4_scale_factor)
   ! Output cartesian anharmonic ADPs in the ADP2 principal axis
   ! frame, in CIF format to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      REAL, IN :: GC4_scale_factor
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,15) :: ADP4v,ADP4s
      REAL :: fac1,fac2
      REAL :: fac3,fac4
      MAT(REAL,:,:), allocatable :: U,dU
      MAT(REAL,3,3) :: axes
      INT :: i,n
   ENSURE(is_square_(cov),"VEC{ATOM}:put_CIF_ADP4_in_inertial_axes_cartn_HBB ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP4_in_inertial_axes_cartn_HBB ... wrong size, covariance matrix!")

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1
      fac4 = fac2*fac2

      ! Get the quntities
      call make_inertial_axes_(self,axes)
      call get_ADP4s_in_new_axes_in_(self,U,dU,axes,cov)

      ! Put the ADP4's
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_ADP4_in_inertial_U_"
         call flush_(stdout)
         call text_(stdout,"# ===========================================")
         call text_(stdout,"# Cartesian ADP4s in ADP2 inertial axis frame")
         call text_(stdout,"# ===========================================")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^4 but may be scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)),left=TRUE)
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)//trim("_esu")),left=TRUE)
            call flush_(stdout)
         end do
         n = 0
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle

            ! ADP4 atom index
            n = n+1
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
!           stdout.flush

            ! Get values
            call put_ADP4_vector_to_(self(i),ADP4v)

            ! Units !!!!!!!!!!
            ADP4v = fac4*U(n,1:15)
            ADP4s = fac4*dU(n,1:15)

            ! Scale factor
            ADP4v = GC4_scale_factor*ADP4v
            ADP4s = GC4_scale_factor*ADP4s

            ! Print ADP4
            call put_(stdout,ADP4v(1))
            call put_(stdout,ADP4s(1))
            call flush_(stdout)
            call put_(stdout,ADP4v(2))
            call put_(stdout,ADP4s(2))
            call flush_(stdout)
            call put_(stdout,ADP4v(3))
            call put_(stdout,ADP4s(3))
            call flush_(stdout)
            call put_(stdout,ADP4v(4))
            call put_(stdout,ADP4s(4))
            call flush_(stdout)
            call put_(stdout,ADP4v(5))
            call put_(stdout,ADP4s(5))
            call flush_(stdout)
            call put_(stdout,ADP4v(6))
            call put_(stdout,ADP4s(6))
            call flush_(stdout)
            call put_(stdout,ADP4v(7))
            call put_(stdout,ADP4s(7))
            call flush_(stdout)
            call put_(stdout,ADP4v(8))
            call put_(stdout,ADP4s(8))
            call flush_(stdout)
            call put_(stdout,ADP4v(9))
            call put_(stdout,ADP4s(9))
            call flush_(stdout)
            call put_(stdout,ADP4v(10))
            call put_(stdout,ADP4s(10))
            call flush_(stdout)
            call put_(stdout,ADP4v(11))
            call put_(stdout,ADP4s(11))
            call flush_(stdout)
            call put_(stdout,ADP4v(12))
            call put_(stdout,ADP4s(12))
            call flush_(stdout)
            call put_(stdout,ADP4v(13))
            call put_(stdout,ADP4s(13))
            call flush_(stdout)
            call put_(stdout,ADP4v(14))
            call put_(stdout,ADP4s(14))
            call flush_(stdout)
            call put_(stdout,ADP4v(15))
            call put_(stdout,ADP4s(15))
            call flush_(stdout)
         end do
      end if

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine put_CIF_ADP4_in_ADP2_principal_axes_cartn_HBB(self,cov,GC4_scale_factor)
   ! Output cartesian anharmonic ADPs in the ADP2 principal axis
   ! frame, in CIF format to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: cov
      REAL, IN :: GC4_scale_factor
      STR(len=STR_SIZE) :: lab,ADPn_label
      VEC(REAL,15) :: ADP4v,ADP4s
      REAL :: fac1,fac2
      REAL :: fac3,fac4
      MAT(REAL,:,:), allocatable :: U,dU
      INT :: i,n
   ENSURE(is_square_(cov),"VEC{ATOM}:put_CIF_ADP4_in_ADP2_principal_axes_cartn_HBB ... covariance matrix is not square!")
   ENSURE(size(cov,1)==no_of_pADPs_(self),"VEC{ATOM}:put_CIF_ADP4_in_ADP2_principal_axes_cartn_HBB ... wrong size, covariance matrix!")

      ! NOT WORKING - CHEcK - dylan
      return

      ! Unit conversion for ADP's
      fac1 = ANGSTROM_PER_BOHR
      fac2 = fac1*fac1
      fac3 = fac2*fac1
      fac4 = fac2*fac2

      ! Get the quntities
      call get_ADP4s_in_ADP2_principal_axes_in_(self,U,dU,cov)

      ! Put the ADP4's
      if (has_any_ADP4s_and_errors_(self)) then
         ADPn_label = "_atom_site_ADP4_in_principal_U_"
         call flush_(stdout)
         call text_(stdout,"# ============================================")
         call text_(stdout,"# Cartesian ADP4s in ADP2 principal axis frame")
         call text_(stdout,"# ============================================")
         call flush_(stdout)
         call text_(stdout,"# . These precise data use non-standard CIF tags")
         call text_(stdout,"# . Please keep for a better refinement restart")
         call text_(stdout,"# . Units are in Angstrom^4 but may be scaled")
         call flush_(stdout)
         call text_(stdout,trim(ADPn_label)//"scale_factor "//trim(to_str_(GC4_scale_factor)))
         call flush_(stdout)
         call text_(stdout,"loop_")
         call text_(stdout,trim(ADPn_label)//"label")
         do i = 1,15
            lab = ADP4_index_label_(i,form="123-repeated")
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)),left=TRUE)
            call put_(stdout,trim(trim(ADPn_label)//trim(lab)//trim("_esu")),left=TRUE)
            call flush_(stdout)
         end do
         n = 0
         do i = 1,size(self)
            if (NOT has_ADP4s_and_errors_(self(i))) cycle

            ! ADP4 atom index
            n = n+1
            call put_(stdout,trim(self(i)%tag),width=len_trim(self(i)%tag),left=TRUE)
!           stdout.flush

            ! Get values
            call put_ADP4_vector_to_(self(i),ADP4v)

            ! Units !!!!!!!!!!
            ADP4v = fac4*U(n,1:15)
            ADP4s = fac4*dU(n,1:15)

            ! Scale factor
            ADP4v = GC4_scale_factor*ADP4v
            ADP4s = GC4_scale_factor*ADP4s

            ! Print ADP4
            call put_(stdout,ADP4v(1))
            call put_(stdout,ADP4s(1))
            call flush_(stdout)
            call put_(stdout,ADP4v(2))
            call put_(stdout,ADP4s(2))
            call flush_(stdout)
            call put_(stdout,ADP4v(3))
            call put_(stdout,ADP4s(3))
            call flush_(stdout)
            call put_(stdout,ADP4v(4))
            call put_(stdout,ADP4s(4))
            call flush_(stdout)
            call put_(stdout,ADP4v(5))
            call put_(stdout,ADP4s(5))
            call flush_(stdout)
            call put_(stdout,ADP4v(6))
            call put_(stdout,ADP4s(6))
            call flush_(stdout)
            call put_(stdout,ADP4v(7))
            call put_(stdout,ADP4s(7))
            call flush_(stdout)
            call put_(stdout,ADP4v(8))
            call put_(stdout,ADP4s(8))
            call flush_(stdout)
            call put_(stdout,ADP4v(9))
            call put_(stdout,ADP4s(9))
            call flush_(stdout)
            call put_(stdout,ADP4v(10))
            call put_(stdout,ADP4s(10))
            call flush_(stdout)
            call put_(stdout,ADP4v(11))
            call put_(stdout,ADP4s(11))
            call flush_(stdout)
            call put_(stdout,ADP4v(12))
            call put_(stdout,ADP4s(12))
            call flush_(stdout)
            call put_(stdout,ADP4v(13))
            call put_(stdout,ADP4s(13))
            call flush_(stdout)
            call put_(stdout,ADP4v(14))
            call put_(stdout,ADP4s(14))
            call flush_(stdout)
            call put_(stdout,ADP4v(15))
            call put_(stdout,ADP4s(15))
            call flush_(stdout)
         end do
      end if

      ! Clean
      call destroy_(dU)
      call destroy_(U)
   end subroutine

   subroutine put_CIF_bond_angle_table_HBB(self,covariance)
   ! Output the CIF bond angle table with errors given the "covariance"
   ! matrix between all atomic positions. 
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: covariance
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(type(EVEC_INT_TYPE),:), allocatable :: Z_list
      INT :: n_kind,k1,k2,k3,n_angles
      MAT(STR(len=STR_SIZE),:,:), allocatable :: symbols
      VEC(STR(len=STR_SIZE),:), allocatable :: dash
      VEC(REAL,:), allocatable :: angle,esds
      MAT(INT,:,:), allocatable :: triple
   ENSURE(self(1)%pos_axis_system=="cartesian","VEC{ATOM}:put_CIF_bond_angle_table_HBB ... axis system must be cartesian")

      ! Title
      call flush_(stdout)
      call text_(stdout,"# ===========")
      call text_(stdout,"# Bond Angles")
      call text_(stdout,"# ===========")

      ! Loop 
      call flush_(stdout)
      call text_(stdout,"loop_")
      call text_(stdout," _geom_angle_atom_site_label_1")
      call text_(stdout," _geom_angle_site_symmetry_1")
      call text_(stdout," _geom_angle_atom_site_label_2")
      call text_(stdout," _geom_angle_site_symmetry_2")
      call text_(stdout," _geom_angle_atom_site_label_3")
      call text_(stdout," _geom_angle_site_symmetry_3")
      call text_(stdout," _geom_angle")
      call text_(stdout," _geom_angle_esu")

      ! Get Z list
      call make_atom_Z_list_(self,Z_list)

      ! Skip dummy atoms
      n_kind = size(Z_list)
      if (self(Z_list(n_kind)%element(1))%atomic_number==0) n_kind = n_kind-1

      ! Create table
      call create_(table,8)

      ! Set the table headings
      call set_heading_(table(1),"A")
      call set_width_from_(table(1),self(angle_outer_atoms_(self))%tag)
      call set_heading_(table(2),".")
      call set_width_(table(2),1)
      call set_heading_(table(3),"B")
      call set_width_from_(table(3),self(angle_center_atoms_(self))%tag)
      call set_heading_(table(4),".")
      call set_width_(table(4),1)
      call set_heading_(table(5),"C")
      call set_width_from_(table(5),self(angle_outer_atoms_(self))%tag)
      call set_heading_(table(6),".")
      call set_width_(table(6),1)
      call set_heading_(table(7),"Angle(A--B--C)")
      call set_width_from_(table(7),ONE)
      call set_heading_(table(8),"Angle(A--B--C) esu")
      call set_width_from_(table(8),ONE)

      ! Loop over atom kind classes
      do k1 = 1,n_kind
         do k2 = 1,n_kind
            do k3 = k1,n_kind

         ! Get the bonded atoms per kind
               call get_bonded_angles_(self,triple,angle,esds,Z_list,k1,k2,k3,covariance)
               angle = angle*DEGREE_PER_RADIAN
               esds = esds*DEGREE_PER_RADIAN

         ! No of bonds in this class
               n_angles = size(angle)
               if (n_angles==0) then
                  call destroy_(esds)
                  call destroy_(angle)
                  call destroy_(triple)
                  cycle
               end if

         ! Element symbols
               call create_(symbols,n_angles,3)
               symbols = " "
               symbols(:,1) = self(triple(:,1))%tag
               symbols(:,2) = self(triple(:,2))%tag
               symbols(:,3) = self(triple(:,3))%tag

         ! Dashes
               call create_(dash,n_angles)
               dash = "."

         ! Set table data
               call set_values_(table(1),symbols(:,1))
               call set_values_(table(2),dash)
               call set_values_(table(3),symbols(:,2))
               call set_values_(table(4),dash)
               call set_values_(table(5),symbols(:,3))
               call set_values_(table(6),dash)
               call set_values_(table(7),angle)
               call set_values_(table(8),esds)

         ! Put body
               call put_body_(table,label_rows=FALSE)

         ! Clean
               call clear_columns_(table)
               call destroy_(dash)
               call destroy_(symbols)
               call destroy_(esds)
               call destroy_(angle)
               call destroy_(triple)
            end do
         end do
      end do

      ! Clean
      call destroy_(table)
      call destroy_(Z_list)
   end subroutine

   subroutine put_NKA_with_errors(self,T_sequence_no)
   ! Output the atom positions, ADP's, ADP sigmas in NKA file format
   ! to "stdout".
      VEC(type(ATOM_TYPE),:), IN :: self
      INT, IN :: T_sequence_no
      INT :: i,k
      REAL :: fac
      MAT(REAL,3,3) :: ADP
      VEC(REAL,3) :: pos
      VEC(REAL,6) :: err

      ! Unit conversion for ADP's
      fac = ANGSTROM_PER_BOHR*ANGSTROM_PER_BOHR

      ! Coordinates and ADP's in A^2
      call flush_(stdout)
      do i = 1,size(self)
         call put_(stdout,chemical_symbol_(self(i)),width=3)

         ! Coordinates in crystal or cartesian system
         pos = ANGSTROM_PER_BOHR*self(i)%position
         call put_(stdout,pos(1))
         call put_(stdout,pos(2))
         call put_(stdout,pos(3))
         call put_(stdout,1,width=2)
         call put_(stdout,T_sequence_no,width=3)
         call flush_(stdout)

         ! Get values and errors
         ADP = fac*self(i)%ADP_tensor

         ! ADP's always in cartesians and A^2
         call put_(stdout,ADP(1,1))
         call put_(stdout,ADP(2,2))
         call put_(stdout,ADP(3,3))
         call put_(stdout,ADP(1,2))
         call put_(stdout,ADP(1,3))
         call put_(stdout,ADP(2,3))
         call flush_(stdout)
      end do

      ! Now the ADP errors
      call flush_(stdout)
      k = -9
      do i = 1,size(self)
         k = k+9
         call put_(stdout,"SIG",width=3)
         call put_(stdout,chemical_symbol_(self(i)),width=3)
         call put_(stdout,T_sequence_no,width=3)

         ! Get values and errors
         err = fac*self(i)%pADP_errors(4:9)

         ! Print ADPs
         call put_(stdout,err(1))
         call put_(stdout,err(2))
         call put_(stdout,err(3))
         call put_(stdout,err(4))
         call put_(stdout,err(5))
         call put_(stdout,err(6))
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_excel_with_errors(self,esd,temperature)
   ! Output the atoms in Excel file format to "stdout"
   ! This should be in the crystal coordinate system.
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(REAL,:), IN :: esd
      REAL, IN :: temperature
      INT :: i,k
      REAL :: fac
      MAT(REAL,3,3) :: ADP
      VEC(REAL,6) :: err
      STR(len=STR_SIZE) :: lab
   ENSURE(self(1)%pos_axis_system=="crystal","VEC{ATOM}:put_excel_with_errors ... must be in crystal axis system")

      ! Unit conversion for ADP's
      fac = ANGSTROM_PER_BOHR*ANGSTROM_PER_BOHR

      ! Coordinates in crystal axis system
      call show_(stdout,"Coordinates refined at T =",temperature)
      call flush_(stdout)
      k = 0
      do i = 1,size(self)
         call put_(stdout,"x")
         call put_(stdout,self(i)%position(1))
         call put_(stdout,esd(k+1))
         call put_(stdout,self(i)%tag)
         call flush_(stdout)
         call put_(stdout,"y")
         call put_(stdout,self(i)%position(2))
         call put_(stdout,esd(k+2))
         call put_(stdout,self(i)%tag)
         call flush_(stdout)
         call put_(stdout,"z")
         call put_(stdout,self(i)%position(3))
         call put_(stdout,esd(k+3))
         call put_(stdout,self(i)%tag)
         call flush_(stdout)
         k = k+9
      end do

      ! ADP's in crystal (CIF) axis system, angstrom^2 units
      call show_(stdout,"ADP's refined at T =",temperature)
      call flush_(stdout)
      k = 0
      do i = 1,size(self)

         ! Get values and errors
         ADP = fac*self(i)%ADP_tensor
         err = fac*esd(k+4:k+9)
         lab = self(i)%tag

         ! Print
         call put_(stdout,"U11"); call put_(stdout,ADP(1,1)); call put_(stdout,err(1)); call put_(stdout,lab); call flush_(stdout)
         call put_(stdout,"U22"); call put_(stdout,ADP(2,2)); call put_(stdout,err(2)); call put_(stdout,lab); call flush_(stdout)
         call put_(stdout,"U33"); call put_(stdout,ADP(3,3)); call put_(stdout,err(3)); call put_(stdout,lab); call flush_(stdout)
         call put_(stdout,"U12"); call put_(stdout,ADP(1,2)); call put_(stdout,err(4)); call put_(stdout,lab); call flush_(stdout)
         call put_(stdout,"U13"); call put_(stdout,ADP(1,3)); call put_(stdout,err(5)); call put_(stdout,lab); call flush_(stdout)
         call put_(stdout,"U23"); call put_(stdout,ADP(2,3)); call put_(stdout,err(6)); call put_(stdout,lab); call flush_(stdout)
         k = k+9
      end do
   end subroutine


!  ============
!  Put orbitals
!  ============

   subroutine put_orbitals(self,orb,pair)
   ! Output the orbitals "orb" in a table with basis function labels.
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(REAL,:,:), IN :: orb
      BIN, optional, IN :: pair
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(STR(len=STR_SIZE),:), allocatable :: labels
      BIN :: is_pair
      INT :: c
   ENSURE(has_all_gaussian_bases_(self),"VEC{ATOM}:put_orbitals ... no gaussian bases")
   ENSURE(size(orb,2)==no_of_basis_functions_(self),"VEC{ATOM}:put_orbitals ... wrong dim2, orb: "//trim(to_str_(size(orb,2))))

      ! Bond/antibond pair?
      is_pair = FALSE
      if (present(pair)) then
         if (size(orb,2)==2) then
            is_pair = pair
         end if
      end if

      ! Basis-fn tags
      call create_(labels,no_of_basis_functions_(self))
      labels = tag_xyz_basis_fn_part_(self)

      ! Create table
      call create_(table,size(orb,2)+1)

      ! Set table headings
      call set_heading_(table(1),"Basis")
      call set_subhead_(table(1),"func.")
      if (is_pair) then
         call set_heading_(table(2),"Bond")
         call set_subhead_(table(2),"Orb.")
         call set_heading_(table(3),"Anti")
         call set_subhead_(table(3),"bond")
      else
         do c = 1,size(orb,2)
            call set_heading_(table(c+1),"Orb")
            call set_subhead_(table(c+1),trim(to_str_(c)))
         end do
      end if

      ! Set data
      call set_values_(table(1),labels)
      do c = 1,size(orb,2)
         call set_values_(table(c+1),orb(:,c))
      end do

      ! Put
      call put_(table,label_rows=FALSE)

      ! Clean
      call destroy_(table)
      call destroy_(labels)
   end subroutine


!  =================
!  Atomic breakdowns 
!  =================

   PURE subroutine get_nn_repulsion_energy_breakdown(self,atom,res,distances)
   ! Gets the breakdown of the nucleus-nucleus repulsion sub-term into
   ! atom-atom components
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: distances
      MAT(REAL,:,:), allocatable, OUT :: res
      INT :: i,j
      REAL :: qi,qj
      call create_(res,size(self),size(atom))
      do i = 1,size(self)
         qi = self(i)%nuclear_charge
         do j = 1,size(atom)
            qj = atom(j)%nuclear_charge
            res(i,j) = qi*qj/distances(i,j)
         end do
      end do
   end subroutine

   PURE subroutine get_nuclear_field_breakdown(self,atom,n_field)
   ! Gets the breakdown of the nuclear electric field generated by "self" 
   ! at each atom in "atom" into atomic components
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT3(REAL,:,:,:), allocatable, OUT :: n_field
      INT :: i,j
      REAL :: r
      VEC(REAL,3) :: rij
      type(ATOM_TYPE) :: atom_j
      MAT(REAL,:,:), allocatable :: pos
      pos = coordinates_(self)
      call create_(n_field,size(self),size(atom),3)
      do j = 1,size(atom)
         atom_j = atom(j)
         do i = 1,size(self)
            rij = atom_j%position-pos(:,i)
            r = norm_(rij)
            n_field(i,j,:) = (atom_j%nuclear_charge/(r*r*r))*rij
         end do
      end do
   end subroutine

   PURE subroutine get_dispersion_energy_breakdown(self,atom,res,distances)
   ! Gets the breakdown of the dispersion term between "self" and "atom" into
   ! atom-atom components 
   ! Based on C6 terms from Grimme (2006) J. Comp. Chem.  27(15) p. 1787
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), allocatable, OUT :: res
      MAT(REAL,:,:), IN :: distances
      INT :: i,j
      REAL :: rij,a_i,a_j,r_i,r_j,d,rr,damping_factor
   ENSURE(has_all_Grimme06_a_coeffs_(self),"VEC{ATOM}:get_dispersion_energy_breakdown ... self: missing Grimme a coefficients")
   ENSURE(has_all_Grimme06_a_coeffs_(atom),"VEC{ATOM}:get_dispersion_energy_breakdown ... atom: missing Grimme a coefficients")

      ! Atom pair energies
      call create_(res,size(self),size(atom))
      res = ZERO
      d = 20.0d0

      ! Loop over pairs
      do i = 1,size(self)
         a_i = Grimme06_a_6_dispersion_coeff(self(i))
         r_i = Grimme06_r_0_dispersion_coeff(self(i))
         do j = 1,size(atom)
            a_j = Grimme06_a_6_dispersion_coeff(atom(j))
            r_j = Grimme06_r_0_dispersion_coeff(atom(j))
            rr = (r_i+r_j)
            rij = distances(i,j)
            damping_factor = (ONE/(ONE+exp(-d*(rij/(rr)-ONE))))
            res(i,j) = -(a_i*a_j/rij**6*damping_factor)
         end do
      end do
   end subroutine


!  ===================================================================
!  Helper functions for outputting group-group interaction-energy data 
!  ===================================================================

   subroutine put_minimum_interatomic_distance(self,atom,distance,factor)
   ! Prints the shortest distance between an atom in "atom" and an atom in
   ! "self", and the symbols of the atoms for which this is achieved
   ! To caller, very specific --dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:) :: distance
      REAL :: factor
      STR(len=STR_SIZE) :: best_i,best_j
      REAL :: min_dist
      call get_minimum_interatomic_distance_(self,atom,distance,factor,best_i,best_j,min_dist)
      call show_(stdout,"Minimum distance (between ="//trim(best_i)//" and "//trim(best_j)//")",min_dist)
   end subroutine

   PURE subroutine get_minimum_interatomic_distance(self,atom,distance,factor,best_i,best_j,min_dist)
   ! Determines "min_dist", the shortest distance between an atom in "atom" and
   ! an atom in "self", and "best_i" and "best_j", the symbols of the atoms in 
   ! "atom" and "self" respectively for which this is achieves
   ! This should go in the bonded, connection section --dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: distance
      REAL, IN :: factor
      REAL, OUT :: min_dist
      STR(len=*), OUT :: best_i
      STR(len=*), OUT :: best_j
      INT :: i,j
      REAL :: dist
      min_dist = -ONE
      do i = 1,size(self)
         do j = 1,size(atom)
            dist = distance(i,j)*factor
            if (dist<min_dist OR min_dist==-ONE) then
               min_dist = dist
               best_i = element_symbols(self(i)%atomic_number)
               best_j = element_symbols(atom(j)%atomic_number)
            end if
         end do
      end do
   end subroutine

   subroutine put_bonds(self,bonded,number)
   ! Prints all the bonds within "self"
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(BIN,:,:), IN :: bonded
      STR(len=*), IN :: number
      STR(len=STR_SIZE) :: bond_str
      bond_str = get_bonds_(self,bonded)
      call text_(stdout,"Bonds in molecule "//number//":"//trim(bond_str))
   end subroutine

   function get_bonds(self,bonded) result (bond_str)
   ! Returns a list of bonds within "self", as a string, separated by spaces 
   ! Very specific, to caller --dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(BIN,:,:), IN :: bonded
      STR(len=STR_SIZE) :: bond_str
      INT :: i,j,k,num_bonds
      STR(len=STR_SIZE) :: symbol_i,symbol_j
      VEC(STR(len=STR_SIZE),:), allocatable :: bond_list
      num_bonds = get_num_bonds(bonded)
      call create_(bond_list,num_bonds)
      k = 1
      do i = 1,size(self)
         symbol_i = element_symbols(self(i)%atomic_number)
         do j = i+1,size(self)
            if (bonded(i,j)) then
               symbol_j = element_symbols(self(j)%atomic_number)
               bond_list(k) = trim(symbol_i)//"--"//trim(symbol_j)
               k = k+1
            end if
         end do
      end do
      bond_str = to_concatenated_str_(bond_list,separator=" ")
      call destroy_(bond_list)
   end function

   pure function get_num_bonds(bonded) result (res)
   ! Returns the number of distinct bonds represented in "bonded"
   ! This just counts the true elements in the upper triangle? --dylan
      MAT(BIN,:,:), IN :: bonded
      INT :: res
      INT :: i,j
      res = 0
      do i = 1,size(bonded,1)
         do j = i+1,size(bonded,2)
            if (bonded(i,j)) then
               res = res+1
            end if
         end do
      end do
   end function


!  =============
!  Group-finding 
!  =============

   PURE subroutine find_groups(self,num_groups,bonded,element,parent)
   ! Matches the groups represented in "element" and "parent" to 
   ! groups of atoms in "self"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_EVEC_INT_TYPE),:), allocatable, INOUT :: num_groups  ! INOUT?
      MAT(BIN,:,:), IN :: bonded
      VEC(type(EVEC_STR_TYPE),:), IN :: element
      VEC(type(EVEC_EVEC_INT_TYPE),:), IN :: parent
      INT :: i
      call create_(num_groups,size(element))
      do i = 1,size(element)
         call find_group_(self,num_groups(i)%element,bonded,element(i)%element,parent(i)%element)
      end do
   end subroutine

   PURE subroutine find_group(self,num_group,bonded,element,parent)
   ! Sets up helper variables for and runs the recursive function to 
   ! search for groups
   ! Seems very specific, put to caller? --dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, INOUT :: num_group
      MAT(BIN,:,:), IN :: bonded
      VEC(STR(len=*),:), IN :: element
      VEC(type(EVEC_INT_TYPE),:), IN :: parent
      VEC(INT,:), allocatable :: candidate
      MAT(BIN,:,:), allocatable :: is_element
      VEC(BIN,:), allocatable :: included
      call get_is_element_(self,element,is_element)
      call create_(candidate,size(element))
      call create_(included,size(self))
      included = FALSE
      call find_group_inner_(self,num_group,bonded,is_element,candidate,included,parent,1)
      call destroy_(is_element)
      call destroy_(candidate)
      call destroy_(included)
   end subroutine

   PURE recursive subroutine find_group_inner(self,num_group,bonded,is_element,candidate,included,parent,atom)
   ! Recursively looks for groups of atoms in "self" with the same structure as
   ! the group represented by "parent". "atom" is the atom in the group that is 
   ! currently being matched
   ! Seems very specific, put to caller? --dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, INOUT :: num_group
      VEC(INT,:), INOUT :: candidate
      MAT(BIN,:,:), IN :: bonded
      MAT(BIN,:,:), IN :: is_element
      VEC(BIN,:), INOUT :: included
      VEC(type(EVEC_INT_TYPE),:), IN :: parent
      INT, IN :: atom
      INT :: i
      do i = 1,size(self)
         if (NOT included(i)) then
            if (is_element(i,atom)) then
               if (correct_bonds(bonded(i,:),candidate,parent(atom)%element)) then
                  candidate(atom) = i
                  if (atom==size(parent)) then
                     call add_candidate(num_group,candidate)
                  else
                     included(i) = TRUE
                     call find_group_inner_(self,num_group,bonded,is_element,candidate,included,parent,atom+1)
                     included(i) = FALSE
                  end if
               end if
            end if
         end if
      end do
   end subroutine

   pure function correct_bonds(bonded,candidate,parent) result (res)
   ! Determines whether, in the above function, atom "i" in self is bonded to the
   ! same atoms as atom "atom" in candidate
      VEC(BIN,:), IN :: bonded
      VEC(INT,:), IN :: candidate
      VEC(INT,:), IN :: parent
      BIN :: res
      if (parent(1)==0) then
         res = TRUE
      else
         res = all(bonded(candidate(parent)))
      end if
   end function

   PURE subroutine get_is_element(self,letter_group,is_element)
   ! Fills in "is_element" such that "element(i, j)" is whether atom i in
   ! "letter_group" equals atom j in "self"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(STR(len=*),:), IN :: letter_group
      MAT(BIN,:,:), allocatable, OUT :: is_element
      INT :: i,j
      call create_(is_element,size(self),size(letter_group))
      do i = 1,size(self)
         do j = 1,size(letter_group)
            is_element(i,j) = element_symbols(self(i)%atomic_number)==letter_group(j)
         end do
      end do
   end subroutine

   PURE subroutine add_candidate(num_group,candidate)
   ! Adds "candidate" to "num_group" if it is not already in "num_group"
      VEC(type(EVEC_INT_TYPE),:), allocatable, INOUT :: num_group
      VEC(INT,:), IN :: candidate
      VEC(INT,:), allocatable :: candidate_copy
      candidate_copy = candidate
      call sort_(candidate_copy)
      if (notin_(num_group,candidate_copy)) then
         if (NOT allocated(num_group)) then
            call create_(num_group,1)
            num_group(1)%element = candidate_copy
         else
            call append_(num_group,candidate_copy)
         end if
      end if
      call destroy_(candidate_copy)
   end subroutine

   pure function notin(num_group,candidate) result (res)
   ! Returns whether or not "candidate" is contained in "num_group"
   ! Doesn't look right --dylan
      VEC(type(EVEC_INT_TYPE),:), allocatable, IN :: num_group
      VEC(INT,:), IN :: candidate
      BIN :: res
      INT :: i
      res = TRUE
      if (allocated(num_group)) then
         do i = 1,size(num_group)
            res = res AND NOT all(num_group(i)%element==candidate)
         end do
      end if
   end function


!  ==============================================
!  Miscellaneous functions for energies breakdown 
!  ==============================================

   PURE subroutine get_bulk(self,group,bulk)
   ! Gets the complement of a group of atoms within "self"
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(INT,:), IN :: group
      VEC(INT,:), allocatable, OUT :: bulk
      INT :: i,j
      call create_(bulk,size(self)-size(group))
      do i = 1,group(1)-1
         bulk(i) = i
      end do
      do i = 1,size(group)-1
         do j = group(i)+1,group(i+1)-1
            bulk(j-i) = j
         end do
      end do
      do i = group(size(group))+1,size(self)
         bulk(i-size(group)) = i
      end do
   end subroutine

   PURE subroutine get_bonded(self,bonded)
   ! Creates a matrix "bonded" such that bonded(i, j) gives whether 
   ! or not atoms i and j are bonded.
   ! This should go as a connection matrix -- dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      MAT(BIN,:,:), allocatable, OUT :: bonded
      INT :: i,j
      call create_(bonded,size(self),size(self))
      do i = 1,size(self)
         do j = 1,size(self)
            bonded(i,j) = bonded_(self,i,j)
         end do
      end do
   end subroutine

   PURE subroutine get_distances(self,atom,distances)
   ! Creates a matrix "distances" such that distances(i,j) is the 
   ! distance between atom i in "self" and atom j in "distances"
   ! This should go in the bonded section, connections --dylan
      VEC(type(ATOM_TYPE),:), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), allocatable, OUT :: distances
      INT :: i,j
      VEC(REAL,3) :: rij
      call create_(distances,size(self),size(atom))
      do j = 1,size(atom)
         do i = 1,size(self)
            rij = atom(j)%position-self(i)%position
            distances(i,j) = norm_(rij)
         end do
      end do
   end subroutine

end module
