!==================================================================
! SHELL1PAIR : 
! A pair of gaussian SHELL1's, incorporating stored intermediate 
! data valuable for integral evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
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
module SHELL1PAIR_MODULE

#  include "macros"

   include "shell1pair.use"

   implicit none

   include "shell1pair.int"


contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create(self)
   ! Allocate an object
      type(SHELL1PAIR_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"SHELL1PAIR:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(SHELL1PAIR_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"SHELL1PAIR:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(SHELL1PAIR_TYPE), INOUT :: self
      call destroy_ptr_part_(self%a)
      call destroy_ptr_part_(self%b)
      call destroy_(self%exponent_sum)
      call destroy_(self%exponent_inv)
      call destroy_(self%contraction_prefactor)
      call destroy_(self%pair_center)
   end subroutine


!  ===========
!  Partial set
!  ===========

   PURE subroutine set_a_0(self,shell_a)
   ! Set the "a" shell1 of the shell2 objects from "shell_a"
      type(SHELL1PAIR_TYPE), INOUT :: self
      type(SHELL1_TYPE), IN :: shell_a
      self%a = shell_a
   end subroutine

   subroutine set_b_0(self,shell_b)
   ! Set the "b" shell1 of the shell2 objects from "shell_b"
      type(SHELL1PAIR_TYPE), INOUT :: self
      type(SHELL1_TYPE), IN :: shell_b
      self%b = shell_b
   end subroutine

   PURE subroutine set_a_1(self,shell_a)
   ! Set the "a" shell of the shell2 objects from "shell_a"
      type(SHELL1PAIR_TYPE), INOUT :: self
      type(SHELL_TYPE), IN :: shell_a
      call set_(self%a,shell_a)
   end subroutine

   PURE subroutine set_b_1(self,shell_b)
   ! Set the "b" shell of the shell2 objects from "shell_b"
      type(SHELL1PAIR_TYPE), INOUT :: self
      type(SHELL_TYPE), IN :: shell_b
      call set_(self%b,shell_b)
   end subroutine


!  ============
!  Precalculate
!  ============

   PURE subroutine unnormalize(self)
   ! Unnormalize each shell
      type(SHELL1PAIR_TYPE), INOUT :: self
      call unnormalize_(self%a)
      call unnormalize_(self%b)
   end subroutine

   PURE subroutine make_precalculated_data(self)
   ! Precalculate some data for the 1/r_{12} electron repulsion integrals
      type(SHELL1PAIR_TYPE), INOUT :: self
      REAL :: a_cc,b_cc,p_cc,a,b
      REAL :: ab_sum,ab_inv,cc_prefac
      INT :: ag,bg,i
      self%l_max = max(self%a%l,self%b%l)
      self%l_min = min(self%a%l,self%b%l)
      self%l_sum = self%a%l+self%b%l
      self%first_gaussian = n_comp_up_to_(self%l_max-1)+1
      self%last_gaussian = n_comp_up_to_(self%l_sum)
      self%n_gaussian = self%last_gaussian-self%first_gaussian+1
      self%n_gaussian_pairs = self%a%n_cc*self%b%n_cc
      call create_(self%exponent_sum,self%n_gaussian_pairs)
      call create_(self%exponent_inv,self%n_gaussian_pairs)
      call create_(self%contraction_prefactor,self%n_gaussian_pairs)
      call create_(self%pair_center,3,self%n_gaussian_pairs)
      i = 0
      do bg = 1,self%b%n_cc
         b = self%b%exponent(bg)
         b_cc = self%b%contraction(bg)
         do ag = 1,self%a%n_cc
            a = self%a%exponent(ag)
            a_cc = self%a%contraction(ag)
            ab_sum = a+b
            ab_inv = ONE/ab_sum
            p_cc = b_cc*a_cc
            cc_prefac = p_cc*ab_inv*sqrt(ab_inv)  ! *exp(-a*b*ab_inv)

         !  if (cc_prefac<pair_cutoff) cycle
            i = i+1
            self%exponent_sum(i) = ab_sum
            self%exponent_inv(i) = ab_inv
            self%contraction_prefactor(i) = cc_prefac
            self%pair_center(:,i) = (a*self%a%position+b*self%b%position)*ab_inv
         end do
      end do
      self%n_comp_pairs = self%a%n_comp*self%b%n_comp
      self%n_sph_pairs = self%a%n_sph*self%b%n_sph
      self%n_bf_pairs = self%a%n_bf*self%b%n_bf
   end subroutine


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump object data as text
      type(SHELL1PAIR_TYPE), IN :: self
      call dump_(self%a,"a")
      call dump_(self%b,"b")
      call dump_(stdout,"n_gaussian_pairs",self%n_gaussian_pairs)
      call dump_(stdout,"n_comp_pairs",self%n_comp_pairs)
      call dump_(stdout,"n_sph_pairs",self%n_sph_pairs)
      call dump_(stdout,"n_bf_pairs",self%n_bf_pairs)
      call dump_(stdout,"l_max",self%l_max)
      call dump_(stdout,"l_min",self%l_min)
      call dump_(stdout,"l_sum",self%l_sum)
      call dump_(stdout,"first_gaussian",self%first_gaussian)
      call dump_(stdout,"last_gaussian",self%last_gaussian)
      call dump_(stdout,"n_gaussian",self%n_gaussian)
      call dmpa_(stdout,"exponent_sum",self%exponent_sum)
      call dmpa_(stdout,"exponent_inv",self%exponent_inv)
      call dmpa_(stdout,"contraction_prefactor",self%contraction_prefactor)
      call dmpa_(stdout,"pair_center",self%pair_center)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(SHELL1PAIR_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":SHELL1PAIR@ = { ")
      else; call text_(stdout,trim(object_name)//":SHELL1PAIR = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(SHELL1PAIR_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":SHELL1PAIR@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Put the shell information to "stdout"
      type(SHELL1PAIR_TYPE), IN :: self
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table

      ! Title
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"===================")
      call text_(stdout,"Gaussian SHELL1PAIR")
      call text_(stdout,"===================")
      call flush_(stdout)
      call show_(stdout,"No. of primitive pair-products    =",self%n_gaussian_pairs)
      call show_(stdout,"No. of contracted cartesian pairs =",self%n_comp_pairs)
      call show_(stdout,"No. of contracted spherical pairs =",self%n_comp_pairs)
      call show_(stdout,"No. of basis fn. product-pairs    =",self%n_bf_pairs)
      call show_(stdout,"l_max, max of angular momenta     =",self%l_max)
      call show_(stdout,"l_min, min of angular momenta     =",self%l_min)
      call show_(stdout,"l_sum, sum of angular momenta     =",self%l_max)
      call show_(stdout,"Start index for l_max-type shell  =",self%first_gaussian)
      call show_(stdout,"End   index for l_sum-type shell  =",self%last_gaussian)
      call show_(stdout,"No. of source gaussian integrals  =",self%n_gaussian)
      call flush_(stdout)
      if (NOT allocated(self%exponent_sum)) return
      if (NOT allocated(self%exponent_inv)) return
      if (NOT allocated(self%contraction_prefactor)) return
      if (NOT allocated(self%pair_center)) return

      ! Table headings
      call create_(table,6)
      call set_heading_(table(1),"Exponent")
      call set_subhead_(table(1),"sum")
      call set_heading_(table(2),"Exponent")
      call set_subhead_(table(2)," inverse")
      call set_heading_(table(3),"Contraction")
      call set_subhead_(table(3)," prefactor")
      call set_heading_(table(4),"Primitive")
      call set_subhead_(table(4),"x/au")
      call set_heading_(table(5),"pair")
      call set_subhead_(table(5),"y/au")
      call set_heading_(table(6),"center")
      call set_subhead_(table(6),"z/au")

      ! Set table column data
      call set_values_(table(1),self%exponent_sum)
      call set_values_(table(2),self%exponent_inv)
      call set_values_(table(3),self%contraction_prefactor)
      call set_values_(table(4),self%pair_center(1,:))
      call set_values_(table(5),self%pair_center(2,:))
      call set_values_(table(6),self%pair_center(3,:))

      ! Make the table
      call put_(table)

      ! Clean
      call destroy_(table)
   end subroutine

end module
