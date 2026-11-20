!=====================================================================
!
! RMS_INDICES
!
! This object is used to store precalculated information pertaining to
! the reduced multiplication scheme (RMS) used in electron repulsion
! integral evaluation.
!
! Copyright (C) Dylan Jayatilaka, 2000
! Copyright (C) Dylan Jayatilaka, 2021
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
module RMS_INDICES_MODULE

#  include "macros"

   include "rms_indices.use"

   implicit none

   include "rms_indices.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(RMS_INDICES_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"RMS_INDICES:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(RMS_INDICES_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"RMS_INDICES:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(RMS_INDICES_TYPE), INOUT :: self

!    .nx.destroy
!    .ny.destroy
!    .nz.destroy
      call destroy_(self%xyz0)
      call destroy_(self%xy0)
      call destroy_(self%xz0)
      call destroy_(self%yz0)
      call destroy_(self%z0)
      call destroy_(self%y0)
      call destroy_(self%x0)
      call destroy_(self%n0)
      call destroy_(self%a0)
   ! .xy_rms.destroy
      call destroy_(self%xy_rms_n)
   ! .xy_unique_n.destroy
      call destroy_(self%xy_rms_a)
   ! .xy_unique_a.destroy
      call destroy_(self%yz0px)
      call destroy_(self%xz0py)
      call destroy_(self%xy0pz)
      call destroy_(self%z0px)
      call destroy_(self%z0py)
      call destroy_(self%y0px)
      call destroy_(self%y0pz)
      call destroy_(self%x0py)
      call destroy_(self%x0pz)
      call destroy_(self%n1)
      call destroy_(self%n1px)
      call destroy_(self%n1py)
      call destroy_(self%n1pz)
      call destroy_(self%n2)
      call destroy_(self%n2px)
      call destroy_(self%n2py)
      call destroy_(self%n2pz)
   end subroutine


!  ===========
!  Set methods
!  ===========

   subroutine set_indices(self,nx,ny,nz,px,py,pz)
   ! Make the arrays which tell where the powers are zero, and where
   ! the product of the first two factors is repeated, and where they
   ! are not. These arrays are used to reduce work in the
   ! multiplication of the 2d integrals to form the full ERI's.
      type(RMS_INDICES_TYPE), INOUT :: self
      VEC(INT,:), IN :: nx,ny,nz
      VEC(INT,:), IN :: px,py,pz
      INT :: n,i,ixyz,ixy,ixz,iyz,ix,iy,iz,in,ia,ir,n1,n2,j,k,l,m
      VEC(STR(len=STR_SIZE),:), allocatable :: listn,lista
   ENSURE(size(ny)==size(nx),"RMS_INDICES:set_indices ... wrong size ny")
   ENSURE(size(nz)==size(nx),"RMS_INDICES:set_indices ... wrong size nz")
      n = size(nx)

      ! Set where the arrays are 0 (actually 1)
      call create_(self%xyz0,n)
      call create_(self%xy0,n)
      call create_(self%xz0,n)
      call create_(self%yz0,n)
      call create_(self%x0,n)
      call create_(self%y0,n)
      call create_(self%z0,n)
      call create_(self%n0,n)
      call create_(self%a0,n)
      call create_(listn,n)
      call create_(lista,n)
      listn = " "
      lista = " "
      ixyz = 0
      ixy = 0
      ixz = 0
      iyz = 0
      ix = 0
      iy = 0
      iz = 0
      in = 0
      ia = 0
      do i = 1,n
         if (nx(i)==1 AND ny(i)==1 AND nz(i)==1) then; ixyz = ixyz+1; self%xyz0(ixyz) = i
         else if (ny(i)==1 AND nz(i)==1) then; iyz = iyz+1; self%yz0(iyz) = i
         else if (nx(i)==1 AND nz(i)==1) then; ixz = ixz+1; self%xz0(ixz) = i
         else if (nx(i)==1 AND ny(i)==1) then; ixy = ixy+1; self%xy0(ixy) = i
         else if (nz(i)==1) then; iz = iz+1; self%z0(iz) = i
         else if (ny(i)==1) then; iy = iy+1; self%y0(iy) = i
         else if (nx(i)==1) then; ix = ix+1; self%x0(ix) = i
         else; in = in+1; self%n0(in) = i
            listn(in) = trim(to_str_(nx(i)))//" "//trim(to_str_(ny(i)))
         end if
         if (nx(i)==1 OR ny(i)==1 OR nz(i)==1) then; ia = ia+1; self%a0(ia) = i
            lista(ia) = trim(to_str_(nx(i)))//" "//trim(to_str_(ny(i)))
         end if
      end do
      call shrink_(self%xyz0,ixyz)
      call shrink_(self%xy0,ixy)
      call shrink_(self%xz0,ixz)
      call shrink_(self%yz0,iyz)
      call shrink_(self%x0,ix)
      call shrink_(self%y0,iy)
      call shrink_(self%z0,iz)
      call shrink_(self%n0,in)
      call shrink_(self%a0,ia)
      call shrink_(listn,in)
      call shrink_(lista,ia)

      ! Locate the reduced multiplies
      call repetitions_in_(self%xy_rms_n,listn)  ! RMS for terms with non-zero indices
      call repetitions_in_(self%xy_rms_a,lista)  ! RMS with at least one zero index
      call destroy_(lista)
      call destroy_(listn)

   ! stdout.show("n = ",n)
   ! stdout.show("l_min = ",l_min)
   ! stdout.show("l_max = ",l_max)
   ! stdout.text("nn:")
   ! do i = 1,n
   !    stdout.put(i)
   !    stdout.put(nx(i))
   !    stdout.put(ny(i))
   !    stdout.put(nz(i))
   !    stdout.flush
   ! end
   ! stdout.text("n0:")
   ! stdout.put(.n0,"column")
   ! stdout.text("xy_rms_n:")
   ! do i = 1,.xy_rms_n.dim
   !    stdout.put(.xy_rms_n(i).element,"row")
   !    stdout.flush
   ! end
   ! stdout.text("a0:")
   ! stdout.put(.a0,"column")
   ! stdout.text("xy_rms_a:")
   ! do i = 1,.xy_rms_a.dim
   !    stdout.put(.xy_rms_a(i).element,"row")
   !    stdout.flush
   ! end
      call create_(self%yz0px,iyz)
      do i = 1,iyz
         self%yz0px(i) = px(self%yz0(i))
      end do
      call create_(self%xz0py,ixz)
      do i = 1,ixz
         self%xz0py(i) = py(self%xz0(i))
      end do
      call create_(self%xy0pz,ixy)
      do i = 1,ixy
         self%xy0pz(i) = pz(self%xy0(i))
      end do
      call create_(self%z0px,iz)
      call create_(self%z0py,iz)
      do i = 1,iz
         self%z0px(i) = px(self%z0(i))
         self%z0py(i) = py(self%z0(i))
      end do
      call create_(self%y0px,iy)
      call create_(self%y0pz,iy)
      do i = 1,iy
         self%y0px(i) = px(self%y0(i))
         self%y0pz(i) = pz(self%y0(i))
      end do
      call create_(self%x0py,ix)
      call create_(self%x0pz,ix)
      do i = 1,ix
         self%x0py(i) = py(self%x0(i))
         self%x0pz(i) = pz(self%x0(i))
      end do

      ! Count number of rms's without any reduction
      ir = size(self%xy_rms_n)
      n1 = 0
      n2 = 0
      do i = 1,ir
         if (size(self%xy_rms_n(i)%element)==1) n1 = n1+1
         if (size(self%xy_rms_n(i)%element)>1) n2 = n2+1
      end do
      DIE_IF(ir/=n1+n2,"RMS_INDICES:set_indices ... inconsistent n1 and n2")
      call create_(self%n1,n1)
      call create_(self%n1px,n1)
      call create_(self%n1py,n1)
      call create_(self%n1pz,n1)
      j = 0
      do i = 1,ir
         if (size(self%xy_rms_n(i)%element)>1) cycle
         DIE_IF(size(self%xy_rms_n(i)%element)<1,"RMS_INDICES:set_indices ... xy_rms_n has zero element component")
         j = j+1
         m = self%xy_rms_n(i)%element(1)
         self%n1(j) = self%n0(m)
         self%n1px(j) = px(self%n1(j))
         self%n1py(j) = py(self%n1(j))
         self%n1pz(j) = pz(self%n1(j))
      end do
      call create_(self%n2,n2)
      call create_(self%n2px,n2)
      call create_(self%n2py,n2)
      call create_(self%n2pz,n2)
      j = 0
      do i = 1,ir
         if (size(self%xy_rms_n(i)%element)==1) cycle
         DIE_IF(size(self%xy_rms_n(i)%element)<1,"RMS_INDICES:set_indices ... xy_rms_n has zero element component")
         j = j+1
         l = size(self%xy_rms_n(i)%element)
         call create_(self%n2(j)%element,l)
         call create_(self%n2px(j)%element,l)
         call create_(self%n2py(j)%element,l)
         call create_(self%n2pz(j)%element,l)
         do k = 1,l
            m = self%xy_rms_n(i)%element(k)
            self%n2(j)%element(k) = self%n0(m)
            self%n2px(j)%element(k) = px(self%n2(j)%element(k))
            self%n2py(j)%element(k) = py(self%n2(j)%element(k))
            self%n2pz(j)%element(k) = pz(self%n2(j)%element(k))
         end do
      end do
   end subroutine

end module
