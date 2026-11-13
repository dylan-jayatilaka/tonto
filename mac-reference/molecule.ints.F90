!==================================================================
!
! MOLECULE.INTS: An object representation of a molecule.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996
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
module MOLECULE_INTS_MODULE

#  include "macros"

   include "molecule.ints.use"

   implicit none

   include "molecule.ints.int"


contains

!  ==============
!  Overlap matrix
!  ==============

   subroutine make_overlap_mx(self)
   ! Get the overlap matrix. Return if already made.
      type(MOLECULE_TYPE), INOUT :: self
      REAL :: smallest,tol
      INT :: n_s,n_n
      type(SCF_DATA_TYPE) :: SCF_data
      if (NOT self%basis_info_made) return
      if (allocated(self%overlap_mx)) return

      ! Make the overlap matrix
      call create_(self%overlap_mx,self%n_bf,self%n_bf)
      call make_r_overlap_mx_(self,self%overlap_mx)

      ! Make maximum overlap between basis fns in shell pairs ...
      call destroy_(self%max_S)
      call create_(self%max_S,self%n_shell_pairs)
      call make_max_S_for_shell_pairs_(self,self%max_S)

      ! Get overlap eigenvalues and eigenvectors
      call destroy_(self%overlap_mx_evals)
      call destroy_(self%overlap_mx_evecs)
      call create_(self%overlap_mx_evecs,self%n_bf,self%n_bf)
      call create_(self%overlap_mx_evals,self%n_bf)
      call solve_symmetric_eigenproblem_(self%overlap_mx,self%overlap_mx_evals,self%overlap_mx_evecs)

      ! Filter small eigenvalues out
      if (allocated(self%SCF_data)) then; tol = self%SCF_data%linear_dependence_tol
      else; tol = SCF_data%linear_dependence_tol
      end if
      if (allocated(self%SCF_data)) tol = self%SCF_data%linear_dependence_tol
      call eigenfilter_using_(self%overlap_mx,self%overlap_mx_evals,self%overlap_mx_evecs,tol)

      ! Set smallest overlap eigenvalue
      if (allocated(self%SCF_data)) then
         smallest = minval(self%overlap_mx_evals)
         call set_min_overlap_mx_eigenvalue_(self%SCF_data,smallest)
         n_s = count(abs(self%overlap_mx_evals)<=self%SCF_data%linear_dependence_tol)
         call set_n_small_S_eigenvalues_(self%SCF_data,n_s)
         n_n = count(self%overlap_mx_evals<=ZERO)
         call set_n_neg_S_eigenvalues_(self%SCF_data,n_n)
      end if

      ! Report
      if (allocated(self%SCF_data)) then
         if (self%SCF_data%output) then
            call report_linear_dependencies_(self)
         end if
      end if
   end subroutine

   subroutine report_linear_dependencies(self)
   ! Report linear dependency informatyion from .overlap matrix. 
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), allocatable :: C
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable :: table
      VEC(REAL,:), allocatable :: lg,bin,W
      REAL :: X_min,X_max,X_bin
      INT :: n_s,n_n,i,n_sections
      n_s = self%SCF_data%n_small_S_eigenvalues
      n_n = self%SCF_data%n_neg_S_eigenvalues

      ! Header
      call flush_(stdout)
      call text_(stdout,"====================================")
      call text_(stdout,"Basis set linear dependence analysis")
      call text_(stdout,"====================================")
      call flush_(stdout)
      call text_(stdout,". Linear dependence is detected by small overlap matrix eigenvalues")
      call flush_(stdout)
      call text_(stdout,". Overlap matrix eigenfunctions with eigenvalues less than the")
      call text_(stdout,"  linear_dependence_tolerance are projected out of the AO basis")
      call text_(stdout,"  leading to, in this case, some exact-zero MO eigenvalues.")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call show_(stdout,"Min. overlap matrix eigenvalue =",self%SCF_data%min_overlap_mx_eigenvalue)
      call show_(stdout,"Linear dependence tolerance    =",self%SCF_data%linear_dependence_tol)
      call show_(stdout,"Linear dependence shift        =",self%SCF_data%linear_dependence_shift)
      call set_real_fortran_fmt_(stdout,"f")
      call show_(stdout,"No. of small eigenvalues       =",n_s)
      call show_(stdout,"No. of negative eigenvalues    =",n_n)

      ! Warn
      if (n_s>0) then
         WARN("MOLECULE.INTS:report_linear_dependencies ... There were "//trim(to_str_(n_s))//" small eigenvalues")
      end if
      if (n_n>0) then
         WARN("MOLECULE.INTS:report_linear_dependencies ... There were "//trim(to_str_(n_n))//" negative eigenvalues")
      end if

      ! Log eigenvalues
      call create_(lg,self%n_bf)
      lg = log10(abs(self%overlap_mx_evals))

      ! Range ...
      X_min = floor(minval(lg))
      X_max = ceiling(maxval(lg))
      X_bin = ONE
      n_sections = nint(X_max-X_min)

      ! Bin values
      call create_(bin,n_sections)
      bin = [(X_min+i*X_bin,i = 1,n_sections)]

      ! GOF
      call create_(C,n_sections)
      call create_(W,n_sections)
      call to_histogram_from_data_(W,lg,lg,X_min,X_max,X_bin,C)
      call destroy_(W)

      ! Table
      call flush_(stdout)
      call create_(table,2)
      call set_heading_(table(1),"log10 |eig|")
      call set_subhead_(table(1),"max. value")
      call set_values_(table(1),bin)
      call set_heading_(table(2),"# < max")
      call set_values_(table(2),C)
      call put_(table)
      call flush_(stdout)

      ! Clean
      call destroy_(table)
      call destroy_(C)
      call destroy_(bin)
      call destroy_(lg)
   end subroutine

   NON_MPI_PURE subroutine make_r_overlap_mx(self,S)
   ! Make the restricted overlap matrix "S"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: S
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
      S = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.INTS:make_r_overlap_mx")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call make_overlap_(sh,S(fa:la,fb:lb))
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.INTS:make_r_overlap_mx")
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(S)
      else
         call symmetric_reflect_(S)
      end if
      ENSURE(NOT is_zero_(S),"MOLECULE.INTS:make_r_overlap_mx ... overlap_mx is zero!")
   end subroutine

   subroutine make_3_center_overlaps(self,S)
   ! Make "S(a,b,c)", the overlap integrals of the basis functions a
   ! and b with the auxiliary basis function c. The auxiliary functions
   ! are in atom order and always spherical; give any library basis as
   ! auxiliary_basis_name=. "S" is .n_bf x .n_bf x .n_auxiliary_bf, so
   ! this is for small molecules.
      type(MOLECULE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: S
      INT :: q,fa,la,na,fb,lb,nb,atom,b,k,fc,lc,m
      type(SHELL2_TYPE) :: sh
      type(SHELL1_TYPE) :: c
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_3_center_overlaps ... no basis info")
      call resolve_auxiliary_bases_(self)
      ENSURE(size(S,1)==self%n_bf AND size(S,2)==self%n_bf,"MOLECULE.INTS:make_3_center_overlaps ... wrong shape, S")
      ENSURE(size(S,3)==n_auxiliary_bf_(self),"MOLECULE.INTS:make_3_center_overlaps ... wrong dim3, S")
      S = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         lc = 0
         do atom = 1,self%n_atom
            b = self%auxiliary_basis_for_atom(atom)
            do k = 1,self%auxiliary_basis(b)%n_shell
               call set_(c,self%auxiliary_basis(b)%shell(k),self%atom(atom)%position)
               fc = lc+1
               lc = lc+c%n_bf
               call make_3_center_overlap_ints_(sh,S(fa:la,fb:lb,fc:lc),c)
               if (fa==fb) cycle
               do m = fc,lc
                  S(fb:lb,fa:la,m) = transpose(S(fa:la,fb:lb,m))
               end do
            end do
         end do
      end do
   end subroutine

   subroutine make_r_overlap_inverse(self,S_inv)
   ! Get "S_inv", the inverse overlap matrix
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: S_inv
      REAL :: tol
   ENSURE(allocated(self%overlap_mx_evecs),"MOLECULE.INTS:make_r_overlap_inverse ... no overlap_mx_evecs")
   ENSURE(allocated(self%overlap_mx_evals),"MOLECULE.INTS:make_r_overlap_inverse ... no overlap_values")
   ENSURE(is_square_(S_inv),"MOLECULE.INTS:make_r_overlap_inverse ... non-square S_inv")
   ENSURE(size(S_inv,1)==size(self%overlap_mx_evecs,1),"MOLECULE.INTS:make_r_overlap_inverse ... wrong shape for S_sqrt")
      if (allocated(self%SCF_data)) then
         tol = self%SCF_data%linear_dependence_tol
         call to_inverse_of_(S_inv,self%overlap_mx_evals,self%overlap_mx_evecs,tol)
      else
         call to_inverse_of_(S_inv,self%overlap_mx_evals,self%overlap_mx_evecs)
      end if
   end subroutine


!  =================
!  Kinetic integrals
!  =================

   NON_MPI_PURE subroutine make_kinetic_energy_mx_0(self)
   ! Get the kinetic energy matrix. Return if already made.
      type(MOLECULE_TYPE), INOUT :: self
      if (allocated(self%kinetic_energy_mx)) return

      ! Make it
      call create_(self%kinetic_energy_mx,self%n_bf,"r")
      call make_kinetic_energy_mx_(self,self%kinetic_energy_mx%r)

      ! Compress
      call compress_(self%kinetic_energy_mx)
   end subroutine

   NON_MPI_PURE subroutine make_kinetic_energy_mx_1(self,T)
   ! Calculate the kinetic energy matrix "T".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: T
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_kinetic_energy_mx_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_kinetic_energy_mx_1 ... no atom list")
      T = ZERO
      do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.INTS:make_kinetic_energy_mx_1")
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call get_kei_(sh,T(fa:la,fb:lb))
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.INTS:make_kinetic_energy_mx_1")
      if (WORK_IS_SHARED) then
         PARALLEL_SYMMETRIC_SUM(T)
      else
         call symmetric_reflect_(T)
      end if
   end subroutine


!  =============================
!  Kinetic and overlap integrals
!  =============================

!   make_kinetic_overlap(T,S)
!   ! Calculate the kinetic energy matrix "T" and overlap integral matrix "S"
!     T,S :: MAT{REAL}
!   ENSURE(.basis_info_made, "no basis info")
!   ENSURE(.atom.allocated,  "no atom list")
!     q,fa,la,fb,lb :: INT
!     sh :: SHELL2
!     do q=1,.n_shell_pairs
!       .set_shell2_from(q,sh,fa,la,fb,lb)
!       sh.get_kei(T(fa:la,fb:lb), S(fa:la,fb:lb))
!     end
!     S.symmetric_reflect
!     T.symmetric_reflect
!   end

!  ===========================
!  Nuclear attracton integrals
!  ===========================

   NON_MPI_PURE subroutine make_nuclear_attraction_mx_0(self)
   ! Get the nuclear attraction matrix. Return if already made.
      type(MOLECULE_TYPE), INOUT :: self
      if (allocated(self%nuclear_attraction_mx)) return

      ! Make it
      call create_(self%nuclear_attraction_mx,self%n_bf,"r")
      call make_nuclear_attraction_mx_(self,self%nuclear_attraction_mx%r)

      ! Compress
      call compress_(self%nuclear_attraction_mx)
   end subroutine

   NON_MPI_PURE subroutine make_nuclear_attraction_mx_1(self,Z)
   ! Calculate the nuclear attraction matrix "Z".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Z
      MAT(REAL,:,:), allocatable :: Z_c
   ! X,MX,M :: MAT{REAL}@
      INT :: q,c,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_nuclear_attraction_mx_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_nuclear_attraction_mx_1 ... no atom list")
      Z = ZERO
      select case (self%nucleus_model)

   ! case ("finite","gaussian")

   !   X.create(.n_prim,.n_prim)
   !   X=ZERO
   !   .REL:make_primitive_nuclear_mx(X)
   !   M.create(.n_prim,.n_bf)
   !   .make_normalized_contraction_mx(M)
   !   MX.create(.n_bf,.n_prim)
   !   MX.to_product_of(M,X,transpose_a=TRUE)
   !   X.destroy
   !   Z.to_product_of(MX,M)
   !   M.destroy
   !   MX.destroy

      case ("point")
         do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
         LOCK_PARALLEL_DO("MOLECULE.INTS:make_nuclear_attraction_mx_1")
            call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
            call create_(Z_c,na,nb)
            do c = 1,self%n_atom
               call get_nuc_(sh,Z_c,self%atom(c)%mass,self%atom(c)%position)
               Z(fa:la,fb:lb) = Z(fa:la,fb:lb)-self%atom(c)%nuclear_charge*Z_c
            end do
            call destroy_(Z_c)
         end do
         UNLOCK_PARALLEL_DO("MOLECULE.INTS:make_nuclear_attraction_mx_1")
         if (WORK_IS_SHARED) then
            PARALLEL_SYMMETRIC_SUM(Z)
         else
            call symmetric_reflect_(Z)
         end if

      case default
         STOP_IF(TRUE,"MOLECULE.INTS:make_nuclear_attraction_mx_1 ... the nuclear attraction matrix is available only for point nuclei")
      end select
   end subroutine

   PURE subroutine make_nuclear_attraction_mx_2(self,Z,list)
   ! Calculate the nuclear attraction matrix "Z" only for a specified
   ! "list" of nuclei.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Z
      VEC(INT,:), IN :: list
      MAT(REAL,:,:), allocatable :: Z_c
      type(ATOM_TYPE) :: atom
   ! M,MX,X :: MAT{REAL}@
      INT :: q,c,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_nuclear_attraction_mx_2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_nuclear_attraction_mx_2 ... no atom list")
      Z = ZERO
      select case (self%nucleus_model)

      ! case ("finite","gaussian")

      ! X.create(.n_prim,.n_prim)
      ! X=ZERO
      ! .REL:make_primitive_nuclear_mx(X)
      ! M.create(.n_prim,.n_bf)
      ! .make_normalized_contraction_mx(M)
      ! MX.create(.n_bf,.n_prim)
      ! MX.to_product_of(M,X,transpose_a=TRUE)
      ! X.destroy
      ! Z.to_product_of(MX,M)
      ! M.destroy
      ! MX.destroy

      case ("point")
         do q = 1,self%n_shell_pairs
            call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
            call create_(Z_c,na,nb)
            do c = 1,size(list)
               atom = self%atom(list(c))
               call get_nuc_(sh,Z_c,atom%mass,atom%position)
               Z(fa:la,fb:lb) = Z(fa:la,fb:lb)-atom%nuclear_charge*Z_c
            end do
            call destroy_(Z_c)
         end do
         call symmetric_reflect_(Z)

      case default
         STOP_IF(TRUE,"MOLECULE.INTS:make_nuclear_attraction_mx_2 ... the nuclear attraction matrix is available only for point nuclei")
      end select
   end subroutine

   NON_MPI_PURE subroutine make_nuclear_attraction_mx_3(self,Z,charges,points)
   ! Calculate the nuclear attraction matrix "Z" given a set of
   ! "charges" at a set of "points".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Z
      VEC(REAL,:), IN :: charges
      MAT(REAL,:,:), IN :: points
      MAT(REAL,:,:), allocatable :: Z_c
   ! MX,M,X :: MAT{REAL}@
      INT :: q,c,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_nuclear_attraction_mx_3 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_nuclear_attraction_mx_3 ... no atom list")
   ENSURE(size(charges)==size(points,1),"MOLECULE.INTS:make_nuclear_attraction_mx_3 ... inconsistent charges and points arrays")
   ENSURE(size(points,2)==3,"MOLECULE.INTS:make_nuclear_attraction_mx_3 ... wrong 2nd dimension, points array")
      Z = ZERO
      select case (self%nucleus_model)

   ! case ("finite","gaussian")
   !    X.create(.n_prim,.n_prim)
   !    X=ZERO
   !    .REL:make_primitive_nuclear_mx(X)
   !    M.create(.n_prim,.n_bf)
   !    .make_normalized_contraction_mx(M)
   !    MX.create(.n_bf,.n_prim)
   !    MX.to_product_of(M,X,transpose_a=TRUE)
   !    X.destroy
   !    Z.to_product_of(MX,M)
   !    M.destroy
   !    MX.destroy

      case ("point")
         do q = PARALLEL_DO_START(1,1),self%n_shell_pairs,PARALLEL_DO_STRIDE(1)
         LOCK_PARALLEL_DO("MOLECULE.INTS:make_nuclear_attraction_mx_3")
            call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
            call create_(Z_c,na,nb)
            do c = 1,size(charges)
               call get_nuc_(sh,Z_c,ZERO,points(c,:))
               Z(fa:la,fb:lb) = Z(fa:la,fb:lb)-charges(c)*Z_c
            end do
            call destroy_(Z_c)
         end do
         UNLOCK_PARALLEL_DO("MOLECULE.INTS:make_nuclear_attraction_mx_3")
         if (WORK_IS_SHARED) then
            PARALLEL_SYMMETRIC_SUM(Z)
         else
            call symmetric_reflect_(Z)
         end if

      case default
         STOP_IF(TRUE,"MOLECULE.INTS:make_nuclear_attraction_mx_3 ... the nuclear attraction matrix is available only for point nuclei")
      end select
   end subroutine


!  =======================
!  Dipole moment integrals
!  =======================

   PURE subroutine make_dipole_matrices(self,Dx,Dy,Dz)
   ! Make "Di", the dipole moment integral matrices.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Dx,Dy,Dz
      MAT(REAL,:,:), allocatable :: DDx,DDy,DDz
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_dipole_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_dipole_matrices ... no atom list")
      Dx = ZERO
      Dy = ZERO
      Dz = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(DDx,na,nb)
         call create_(DDy,na,nb)
         call create_(DDz,na,nb)
         call make_dipole_ints_(sh,DDx,DDy,DDz,self%gauge_origin)
         Dx(fa:la,fb:lb) = Dx(fa:la,fb:lb)+DDx
         Dy(fa:la,fb:lb) = Dy(fa:la,fb:lb)+DDy
         Dz(fa:la,fb:lb) = Dz(fa:la,fb:lb)+DDz
         call destroy_(DDz)
         call destroy_(DDy)
         call destroy_(DDx)
      end do
      call symmetric_reflect_(Dx)
      call symmetric_reflect_(Dy)
      call symmetric_reflect_(Dz)
   end subroutine

   PURE function electronic_dipole_moment(self) result (res)
   ! Return the electronic dipole moment
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,3) :: res
      MAT(REAL,:,:), allocatable :: Dx,Dy,Dz
      call create_(Dx,self%n_bf,self%n_bf)
      call create_(Dy,self%n_bf,self%n_bf)
      call create_(Dz,self%n_bf,self%n_bf)
      call make_dipole_matrices_(self,Dx,Dy,Dz)

      ! Minus sign for electron charge
      res(1) = -expectation_(self,Dx)
      res(2) = -expectation_(self,Dy)
      res(3) = -expectation_(self,Dz)
      call destroy_(Dz)
      call destroy_(Dy)
      call destroy_(Dx)
   end function


!  ===========================
!  Quadrupole moment integrals
!  ===========================

   PURE subroutine make_quadrupole_matrices(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)
   ! Make "Qij", the quadrupole moment integral matrices.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qyy,Qzz
      MAT(REAL,:,:), OUT :: Qxy,Qxz,Qyz
      MAT(REAL,:,:), allocatable :: QQxx,QQyy,QQzz,QQxy,QQxz,QQyz
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_quadrupole_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_quadrupole_matrices ... no atom list")
      Qxx = ZERO; Qyy = ZERO; Qzz = ZERO
      Qxy = ZERO; Qxz = ZERO; Qyz = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(QQxx,na,nb)
         call create_(QQyy,na,nb)
         call create_(QQzz,na,nb)
         call create_(QQxy,na,nb)
         call create_(QQxz,na,nb)
         call create_(QQyz,na,nb)
         call make_quadrupole_ints_(sh,QQxx,QQyy,QQzz,QQxy,QQxz,QQyz,self%gauge_origin)
         Qxx(fa:la,fb:lb) = Qxx(fa:la,fb:lb)+QQxx
         Qyy(fa:la,fb:lb) = Qyy(fa:la,fb:lb)+QQyy
         Qzz(fa:la,fb:lb) = Qzz(fa:la,fb:lb)+QQzz
         Qxy(fa:la,fb:lb) = Qxy(fa:la,fb:lb)+QQxy
         Qxz(fa:la,fb:lb) = Qxz(fa:la,fb:lb)+QQxz
         Qyz(fa:la,fb:lb) = Qyz(fa:la,fb:lb)+QQyz
         call destroy_(QQyz)
         call destroy_(QQxz)
         call destroy_(QQxy)
         call destroy_(QQzz)
         call destroy_(QQyy)
         call destroy_(QQxx)
      end do
      call symmetric_reflect_(Qxx)
      call symmetric_reflect_(Qyy)
      call symmetric_reflect_(Qzz)
      call symmetric_reflect_(Qxy)
      call symmetric_reflect_(Qxz)
      call symmetric_reflect_(Qyz)
   end subroutine

   PURE function electronic_quadrupole_moment_vec(self) result (res)
   ! Return the electronic quadrupole moments as a vector in the
   ! order: xx, yy, zz, xy, xz, yz
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,6) :: res
      MAT(REAL,:,:), allocatable :: Qxx,Qyy,Qzz
      MAT(REAL,:,:), allocatable :: Qxy,Qxz,Qyz
      call create_(Qxx,self%n_bf,self%n_bf)
      call create_(Qyy,self%n_bf,self%n_bf)
      call create_(Qzz,self%n_bf,self%n_bf)
      call create_(Qxy,self%n_bf,self%n_bf)
      call create_(Qxz,self%n_bf,self%n_bf)
      call create_(Qyz,self%n_bf,self%n_bf)
      call make_quadrupole_matrices_(self,Qxx,Qyy,Qzz,Qxy,Qxz,Qyz)

      ! Minus sign for electron charge
      res(1) = -expectation_(self,Qxx)
      res(2) = -expectation_(self,Qyy)
      res(3) = -expectation_(self,Qzz)
      res(4) = -expectation_(self,Qxy)
      res(5) = -expectation_(self,Qxz)
      res(6) = -expectation_(self,Qyz)
      call destroy_(Qyz)
      call destroy_(Qxz)
      call destroy_(Qxy)
      call destroy_(Qzz)
      call destroy_(Qyy)
      call destroy_(Qxx)
   end function

   PURE function electronic_quadrupole_moment_mx(self) result (res)
   ! Return the electronic quadrupole moments as a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,3,3) :: res
      VEC(REAL,6) :: vec
      vec = electronic_quadrupole_moment_vec_(self)
      call expand_quadrupole_from_(res,vec)
   end function


!  =========================
!  Octupole moment integrals
!  =========================

   PURE subroutine make_octupole_matrices(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)
   ! Make "Oijk", the octupole moment integral matrices.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Oxxx,Oyyy,Ozzz
      MAT(REAL,:,:), OUT :: Oxxy,Oxxz,Oyyx
      MAT(REAL,:,:), OUT :: Oyyz,Ozzx,Ozzy
      MAT(REAL,:,:), OUT :: Oxyz
      MAT(REAL,:,:), allocatable :: OOxxx,OOyyy,OOzzz
      MAT(REAL,:,:), allocatable :: OOxxy,OOxxz,OOyyx
      MAT(REAL,:,:), allocatable :: OOyyz,OOzzx,OOzzy
      MAT(REAL,:,:), allocatable :: OOxyz
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_octupole_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_octupole_matrices ... no atom list")
      Oxxx = ZERO; Oyyy = ZERO; Ozzz = ZERO
      Oxxy = ZERO; Oxxz = ZERO
      Oyyx = ZERO; Oyyz = ZERO
      Ozzx = ZERO; Ozzy = ZERO
      Oxyz = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(OOxxx,na,nb)
         call create_(OOyyy,na,nb)
         call create_(OOzzz,na,nb)
         call create_(OOxxy,na,nb)
         call create_(OOxxz,na,nb)
         call create_(OOyyx,na,nb)
         call create_(OOyyz,na,nb)
         call create_(OOzzx,na,nb)
         call create_(OOzzy,na,nb)
         call create_(OOxyz,na,nb)
         call make_octupole_ints_(sh,OOxxx,OOyyy,OOzzz,OOxxy,OOxxz,OOyyx,OOyyz,OOzzx,OOzzy,OOxyz,self%gauge_origin)
         Oxxx(fa:la,fb:lb) = Oxxx(fa:la,fb:lb)+OOxxx
         Oyyy(fa:la,fb:lb) = Oyyy(fa:la,fb:lb)+OOyyy
         Ozzz(fa:la,fb:lb) = Ozzz(fa:la,fb:lb)+OOzzz
         Oxxy(fa:la,fb:lb) = Oxxy(fa:la,fb:lb)+OOxxy
         Oxxz(fa:la,fb:lb) = Oxxz(fa:la,fb:lb)+OOxxz
         Oyyx(fa:la,fb:lb) = Oyyx(fa:la,fb:lb)+OOyyx
         Oyyz(fa:la,fb:lb) = Oyyz(fa:la,fb:lb)+OOyyz
         Ozzx(fa:la,fb:lb) = Ozzx(fa:la,fb:lb)+OOzzx
         Ozzy(fa:la,fb:lb) = Ozzy(fa:la,fb:lb)+OOzzy
         Oxyz(fa:la,fb:lb) = Oxyz(fa:la,fb:lb)+OOxyz
         call destroy_(OOxyz)
         call destroy_(OOzzy)
         call destroy_(OOzzx)
         call destroy_(OOyyz)
         call destroy_(OOyyx)
         call destroy_(OOxxz)
         call destroy_(OOxxy)
         call destroy_(OOzzz)
         call destroy_(OOyyy)
         call destroy_(OOxxx)
      end do
      call symmetric_reflect_(Oxxx)
      call symmetric_reflect_(Oyyy)
      call symmetric_reflect_(Ozzz)
      call symmetric_reflect_(Oxxy)
      call symmetric_reflect_(Oxxz)
      call symmetric_reflect_(Oyyx)
      call symmetric_reflect_(Oyyz)
      call symmetric_reflect_(Ozzx)
      call symmetric_reflect_(Ozzy)
      call symmetric_reflect_(Oxyz)
   end subroutine

   PURE function electronic_octupole_moment_vec(self) result (res)
   ! Return the electronic octupole moments as a vectors in the
   ! order: xxx, yyy, zzz, xxy, xxz, yyx, yyz, zzx, zzy, xyz
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,10) :: res
      MAT(REAL,:,:), allocatable :: Oxxx,Oyyy,Ozzz
      MAT(REAL,:,:), allocatable :: Oxxy,Oxxz,Oyyx
      MAT(REAL,:,:), allocatable :: Oyyz,Ozzx,Ozzy
      MAT(REAL,:,:), allocatable :: Oxyz
      call create_(Oxxx,self%n_bf,self%n_bf)
      call create_(Oyyy,self%n_bf,self%n_bf)
      call create_(Ozzz,self%n_bf,self%n_bf)
      call create_(Oxxy,self%n_bf,self%n_bf)
      call create_(Oxxz,self%n_bf,self%n_bf)
      call create_(Oyyx,self%n_bf,self%n_bf)
      call create_(Oyyz,self%n_bf,self%n_bf)
      call create_(Ozzx,self%n_bf,self%n_bf)
      call create_(Ozzy,self%n_bf,self%n_bf)
      call create_(Oxyz,self%n_bf,self%n_bf)
      call make_octupole_matrices_(self,Oxxx,Oyyy,Ozzz,Oxxy,Oxxz,Oyyx,Oyyz,Ozzx,Ozzy,Oxyz)

      ! Minus sign for electron charge
      res(1) = -expectation_(self,Oxxx)
      res(2) = -expectation_(self,Oyyy)
      res(3) = -expectation_(self,Ozzz)
      res(4) = -expectation_(self,Oxxy)
      res(5) = -expectation_(self,Oxxz)
      res(6) = -expectation_(self,Oyyx)
      res(7) = -expectation_(self,Oyyz)
      res(8) = -expectation_(self,Ozzx)
      res(9) = -expectation_(self,Ozzy)
      res(10) = -expectation_(self,Oxyz)
      call destroy_(Oxyz)
      call destroy_(Ozzy)
      call destroy_(Ozzx)
      call destroy_(Oyyz)
      call destroy_(Oyyx)
      call destroy_(Oxxz)
      call destroy_(Oxxy)
      call destroy_(Ozzz)
      call destroy_(Oyyy)
      call destroy_(Oxxx)
   end function

   PURE function electronic_octupole_moment_mx(self) result (res)
   ! Return the electronic octupole moments as a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,3,3,3) :: res
      VEC(REAL,10) :: vec
      vec = electronic_octupole_moment_vec_(self)
      call expand_octupole_from_(res,vec)
   end function


!  =================
!  Charge multipoles
!  =================

   PURE function dipole_moment(self) result (res)
   ! The total dipole moment
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,3) :: res
      VEC(REAL,3) :: elec
      VEC(REAL,3) :: nucl
      elec = electronic_dipole_moment_(self)
      nucl = nuclear_dipole_moment_(self%atom)
      res = elec+nucl
   end function

   PURE function quadrupole_moment_vec(self) result (res)
   ! The total quadrupole moment as a vector
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,6) :: res
      VEC(REAL,6) :: elec
      VEC(REAL,6) :: nucl
   ENSURE(allocated(self%atom),"MOLECULE.INTS:quadrupole_moment_vec ... no atoms")
      elec = electronic_quadrupole_moment_vec_(self)
      nucl = nuclear_quadrupole_moment_vec_(self%atom)
      res = elec+nucl
   end function

   PURE function quadrupole_moment_mx(self) result (res)
   ! The total quadrupole moment as a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,3,3) :: res
      VEC(REAL,6) :: vec
      vec = quadrupole_moment_vec_(self)
      call expand_quadrupole_from_(res,vec)
   end function

   PURE function octupole_moment_vec(self) result (res)
   ! The total octupole moment as a vector
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,10) :: res
      VEC(REAL,10) :: elec
      VEC(REAL,10) :: nucl
   ENSURE(allocated(self%atom),"MOLECULE.INTS:octupole_moment_vec ... no atoms")
      elec = electronic_octupole_moment_vec_(self)
      nucl = nuclear_octupole_moment_vec_(self%atom)
      res = elec+nucl
   end function

   PURE function octupole_moment_mx(self) result (res)
   ! The total octupole moment as a matrix
      type(MOLECULE_TYPE), IN :: self
      MAT3(REAL,3,3,3) :: res
      VEC(REAL,10) :: vec
      vec = octupole_moment_vec_(self)
      call expand_octupole_from_(res,vec)
   end function


!  ========================
!  Electric field integrals
!  ========================

   PURE subroutine make_E_field_matrices(self,Ex,Ey,Ez,c)
   ! Make "Ei", the electric field integral matrices evaluated at
   ! position "c".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ex,Ey,Ez
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: EEx,EEy,EEz
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_E_field_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_E_field_matrices ... no atom list")
      Ex = ZERO
      Ey = ZERO
      Ez = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(EEx,na,nb)
         call create_(EEy,na,nb)
         call create_(EEz,na,nb)
         call make_E_field_ints_(sh,EEx,EEy,EEz,c)
         Ex(fa:la,fb:lb) = Ex(fa:la,fb:lb)+EEx
         Ey(fa:la,fb:lb) = Ey(fa:la,fb:lb)+EEy
         Ez(fa:la,fb:lb) = Ez(fa:la,fb:lb)+EEz
         call destroy_(EEz)
         call destroy_(EEy)
         call destroy_(EEx)
      end do
      call symmetric_reflect_(Ex)
      call symmetric_reflect_(Ey)
      call symmetric_reflect_(Ez)
   end subroutine

   PURE function electronic_E_field_at_positions(self,pos) result (res)
   ! Return the electronic contribution to the electric fields
   ! at the positions "pos"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: pos
      MAT(REAL,3,size(pos,2)) :: res
      MAT(REAL,:,:), allocatable :: Ex,Ey,Ez
      INT :: a
      call create_(Ex,self%n_bf,self%n_bf)
      call create_(Ey,self%n_bf,self%n_bf)
      call create_(Ez,self%n_bf,self%n_bf)
      do a = 1,size(pos,2)
         call make_E_field_matrices_(self,Ex,Ey,Ez,pos(:,a))
         ! Minus sign for electron charge
         res(1,a) = -expectation_(self,Ex)
         res(2,a) = -expectation_(self,Ey)
         res(3,a) = -expectation_(self,Ez)
      end do
      call destroy_(Ez)
      call destroy_(Ey)
      call destroy_(Ex)
   end function

   PURE function electronic_E_field_at_nuclei_of(self,atoms) result (res)
   ! Return the electronic contribution to the electric fields
   ! from "self" at the the positions of the nuclei in "atoms"
      type(MOLECULE_TYPE), IN :: self
      VEC(type(ATOM_TYPE),:), IN :: atoms
      MAT(REAL,3,size(atoms)) :: res
      MAT(REAL,:,:), allocatable :: pos
      pos = coordinates_(atoms)
      res = electronic_E_field_at_positions_(self,pos)
      call destroy_(pos)
   end function

   PURE function electronic_E_field_at_nuclei(self) result (res)
   ! Return the electronic contribution to the electric fields
   ! at the nuclei as a (3 x .n_atom) array
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,3,size(self%atom)) :: res
      res = electronic_E_field_at_nuclei_of_(self,self%atom)
   end function

   PURE function nuclear_E_field_at_nuclei_of(atoms) result (res)
   ! Return the nuclear contribution to the electric fields
   ! at the "atoms"
      VEC(type(ATOM_TYPE),:), IN :: atoms
      MAT(REAL,3,size(atoms)) :: res
      MAT(REAL,:,:), allocatable :: pos
      pos = coordinates_(atoms)
      res = nuclear_E_field_at_positions_(atoms,pos)
      call destroy_(pos)
   end function

   PURE function nuclear_E_field_at_nuclei(self) result (res)
   ! Return the nuclear contribution to the electric fields
   ! at the nuclei as a (3 x .n_atom) array
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,3,self%n_atom) :: res
      res = nuclear_E_field_at_nuclei_(self%atom)
   end function


!  =================================
!  Electric field gradient integrals
!  =================================

   PURE subroutine make_EFG_matrices(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,c)
   ! Make "Eij", the electric field gradient (EFG) integral matrices
   ! evaluated at point "c".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Exx,Eyy,Ezz
      MAT(REAL,:,:), OUT :: Exy,Exz,Eyz
      VEC(REAL,3), IN :: c
      MAT(REAL,:,:), allocatable :: EExx,EEyy,EEzz
      MAT(REAL,:,:), allocatable :: EExy,EExz,EEyz
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_EFG_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_EFG_matrices ... no atom list")
      Exx = ZERO
      Eyy = ZERO
      Ezz = ZERO
      Exy = ZERO
      Exz = ZERO
      Eyz = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(EExx,na,nb)
         call create_(EEyy,na,nb)
         call create_(EEzz,na,nb)
         call create_(EExy,na,nb)
         call create_(EExz,na,nb)
         call create_(EEyz,na,nb)
         call make_E_gradient_ints_(sh,EExx,EEyy,EEzz,EExy,EExz,EEyz,c)
         Exx(fa:la,fb:lb) = Exx(fa:la,fb:lb)+EExx
         Eyy(fa:la,fb:lb) = Eyy(fa:la,fb:lb)+EEyy
         Ezz(fa:la,fb:lb) = Ezz(fa:la,fb:lb)+EEzz
         Exy(fa:la,fb:lb) = Exy(fa:la,fb:lb)+EExy
         Exz(fa:la,fb:lb) = Exz(fa:la,fb:lb)+EExz
         Eyz(fa:la,fb:lb) = Eyz(fa:la,fb:lb)+EEyz
         call destroy_(EEyz)
         call destroy_(EExz)
         call destroy_(EExy)
         call destroy_(EEzz)
         call destroy_(EEyy)
         call destroy_(EExx)
      end do
      call symmetric_reflect_(Exx)
      call symmetric_reflect_(Eyy)
      call symmetric_reflect_(Ezz)
      call symmetric_reflect_(Exy)
      call symmetric_reflect_(Exz)
      call symmetric_reflect_(Eyz)
   end subroutine

   PURE function electronic_EFG_at_nuclei(self) result (res)
   ! Return the electronic contribution to the electric field gradient
   ! (EFG) at the nuclei as a (6 x .n_atom) array. The order of the
   ! electric field gradient elements is: xx, yy, zz, xy, xz, yz
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,6,self%n_atom) :: res
      MAT(REAL,:,:), allocatable :: Exx,Eyy,Ezz
      MAT(REAL,:,:), allocatable :: Exy,Exz,Eyz
      INT :: a
      call create_(Exx,self%n_bf,self%n_bf)
      call create_(Eyy,self%n_bf,self%n_bf)
      call create_(Ezz,self%n_bf,self%n_bf)
      call create_(Exy,self%n_bf,self%n_bf)
      call create_(Exz,self%n_bf,self%n_bf)
      call create_(Eyz,self%n_bf,self%n_bf)
      do a = 1,self%n_atom
         call make_EFG_matrices_(self,Exx,Eyy,Ezz,Exy,Exz,Eyz,self%atom(a)%position)

         ! Minus sign for electron charge
         res(1,a) = -expectation_(self,Exx)
         res(2,a) = -expectation_(self,Eyy)
         res(3,a) = -expectation_(self,Ezz)
         res(4,a) = -expectation_(self,Exy)
         res(5,a) = -expectation_(self,Exz)
         res(6,a) = -expectation_(self,Eyz)
      end do
      call destroy_(Eyz)
      call destroy_(Exz)
      call destroy_(Exy)
      call destroy_(Ezz)
      call destroy_(Eyy)
      call destroy_(Exx)
   end function


!   nuclear_EFG_at_nuclei result (res) :: PURE
!   ! Return the nuclear contribution to the electric fields gradient (EFG)
!   ! at the nuclei as a (6 x .n_atom) array
!      self :: IN
!      res :: MAT{REAL}(6,.n_atom)
!      res = .atom.nuclear_EFG_at_nuclei
!   end

!  ==========================
!  Angular momentum integrals
!  ==========================

   PURE subroutine make_L_matrices(self,Lx,Ly,Lz)
   ! Make the angular momentum matrices  "Lx", "Ly", and "Lz".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Lx,Ly,Lz
      INT :: q,fa,la,na,fb,lb,nb
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_L_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_L_matrices ... no atom list")
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call make_L_ints_(sh,Lx(fa:la,fb:lb),Ly(fa:la,fb:lb),Lz(fa:la,fb:lb),self%gauge_origin)
      end do
      call antisymmetric_reflect_(Lx)
      call antisymmetric_reflect_(Ly)
      call antisymmetric_reflect_(Lz)
   end subroutine


!  ====================
!  Spin-orbit integrals
!  ====================

   PURE subroutine make_spin_orbit_matrices(self,SOx,SOy,SOz)
   ! Calculate the spin orbit matrices "SOx" "SOy" and "SOz"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: SOx,SOy,SOz
      MAT(REAL,:,:), allocatable :: SOx_c,SOy_c,SOz_c
      INT :: q,c,fa,la,na,fb,lb,nb
      REAL :: Z
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_spin_orbit_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_spin_orbit_matrices ... no atom list")
      SOx = ZERO
      SOy = ZERO
      SOz = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(SOx_c,na,nb)
         call create_(SOy_c,na,nb)
         call create_(SOz_c,na,nb)
         do c = 1,self%n_atom
            Z = self%atom(c)%nuclear_charge
            call make_spin_orbit_ints_(sh,SOx_c,SOy_c,SOz_c,self%atom(c)%position)
            SOx(fa:la,fb:lb) = SOx(fa:la,fb:lb)+Z*SOx_c
            SOy(fa:la,fb:lb) = SOy(fa:la,fb:lb)+Z*SOy_c
            SOz(fa:la,fb:lb) = SOz(fa:la,fb:lb)+Z*SOz_c
         end do
         call destroy_(SOz_c)
         call destroy_(SOy_c)
         call destroy_(SOx_c)
      end do
      call antisymmetric_reflect_(SOx)
      call antisymmetric_reflect_(SOy)
      call antisymmetric_reflect_(SOz)
   end subroutine

   PURE subroutine make_spin_orbit_B_matrices(self,SOBx,SOBy,SOBz)
   ! Calculate the gauge modified (B field) spin orbit matrices "SOBx"
   ! "SOBy" and "SOBz"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: SOBx,SOBy,SOBz
      MAT(REAL,:,:), allocatable :: Qxx,Qxy,Qxz
      MAT(REAL,:,:), allocatable :: Qyx,Qyy,Qyz
      MAT(REAL,:,:), allocatable :: Qzx,Qzy,Qzz
      INT :: n_bf
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_spin_orbit_B_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_spin_orbit_B_matrices ... no atom list")
      n_bf = self%n_bf
      call create_(Qxx,n_bf,n_bf)
      call create_(Qxy,n_bf,n_bf)
      call create_(Qxz,n_bf,n_bf)
      call create_(Qyx,n_bf,n_bf)
      call create_(Qyy,n_bf,n_bf)
      call create_(Qyz,n_bf,n_bf)
      call create_(Qzx,n_bf,n_bf)
      call create_(Qzy,n_bf,n_bf)
      call create_(Qzz,n_bf,n_bf)
      call make_spin_orbit_Q_matrices_(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
      SOBx = (Qyy+Qzz)*self%B_field(1)-Qxy*self%B_field(2)-Qxz*self%B_field(3)
      SOBy = (Qxx+Qzz)*self%B_field(2)-Qyx*self%B_field(1)-Qyz*self%B_field(3)
      SOBz = (Qxx+Qyy)*self%B_field(3)-Qzx*self%B_field(1)-Qzy*self%B_field(2)
      call destroy_(Qzz)
      call destroy_(Qzy)
      call destroy_(Qzx)
      call destroy_(Qyz)
      call destroy_(Qyy)
      call destroy_(Qyx)
      call destroy_(Qxz)
      call destroy_(Qxy)
      call destroy_(Qxx)
      call symmetric_reflect_(SOBx)
      call symmetric_reflect_(SOBy)
      call symmetric_reflect_(SOBz)
   end subroutine

   PURE subroutine make_spin_orbit_Q_matrices(self,Qxx,Qxy,Qxz,Qyx,Qyy,Qyz,Qzx,Qzy,Qzz)
   ! Calculate the gauge modified (B field) spin orbit quadrupole
   ! matrices "Qij".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Qxx,Qxy,Qxz
      MAT(REAL,:,:), OUT :: Qyx,Qyy,Qyz
      MAT(REAL,:,:), OUT :: Qzx,Qzy,Qzz
      MAT(REAL,:,:), allocatable :: xx,xy,xz
      MAT(REAL,:,:), allocatable :: yx,yy,yz
      MAT(REAL,:,:), allocatable :: zx,zy,zz
      INT :: q,c,fa,la,na,fb,lb,nb
      REAL :: Z
      type(SHELL2_TYPE) :: sh
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_spin_orbit_Q_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_spin_orbit_Q_matrices ... no atom list")
      Qxx = ZERO; Qxy = ZERO; Qxz = ZERO
      Qyx = ZERO; Qyy = ZERO; Qyz = ZERO
      Qzx = ZERO; Qzy = ZERO; Qzz = ZERO
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb)
         call create_(xx,na,nb)
         call create_(xy,na,nb)
         call create_(xz,na,nb)
         call create_(yx,na,nb)
         call create_(yy,na,nb)
         call create_(yz,na,nb)
         call create_(zx,na,nb)
         call create_(zy,na,nb)
         call create_(zz,na,nb)
         do c = 1,self%n_atom
            Z = self%atom(c)%nuclear_charge
            call make_spin_orbit_B_ints_(sh,xx,xy,xz,yx,yy,yz,zx,zy,zz,self%atom(c)%position,self%gauge_origin)

            ! minus due to r-c, not c-r in the electric field part
            ! this comment should go in gaussian2?
            Qxx(fa:la,fb:lb) = Qxx(fa:la,fb:lb)-Z*xx
            Qxy(fa:la,fb:lb) = Qxy(fa:la,fb:lb)-Z*xy
            Qxz(fa:la,fb:lb) = Qxz(fa:la,fb:lb)-Z*xz
            Qyx(fa:la,fb:lb) = Qyx(fa:la,fb:lb)-Z*yx
            Qyy(fa:la,fb:lb) = Qyy(fa:la,fb:lb)-Z*yy
            Qyz(fa:la,fb:lb) = Qyz(fa:la,fb:lb)-Z*yz
            Qzx(fa:la,fb:lb) = Qzx(fa:la,fb:lb)-Z*zx
            Qzy(fa:la,fb:lb) = Qzy(fa:la,fb:lb)-Z*zy
            Qzz(fa:la,fb:lb) = Qzz(fa:la,fb:lb)-Z*zz
         end do
         call destroy_(zz)
         call destroy_(zy)
         call destroy_(zx)
         call destroy_(yz)
         call destroy_(yy)
         call destroy_(yx)
         call destroy_(xz)
         call destroy_(xy)
         call destroy_(xx)
      end do
      call symmetric_reflect_(Qxx)
      call symmetric_reflect_(Qxy)
      call symmetric_reflect_(Qxz)
      call symmetric_reflect_(Qyx)
      call symmetric_reflect_(Qyy)
      call symmetric_reflect_(Qyz)
      call symmetric_reflect_(Qzx)
      call symmetric_reflect_(Qzy)
      call symmetric_reflect_(Qzz)
   end subroutine


!  ==============
!  ZORA integrals
!  ==============

   pure subroutine ZORA_potential(self,values,pts)
   ! Calculate the one electron ZORA potential "values" of a given set
   ! of "pts".  This is useful for numerical integration.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: values
      MAT(REAL,:,:), IN :: pts
      REAL :: fac
      call nuclear_potential_(self,values,pts)
      fac = ONE/(SPEED_OF_LIGHT_AU*SPEED_OF_LIGHT_AU)
      values = ONE/(TWO+fac*values)
   end subroutine

   PURE subroutine make_1e_ZORA_matrices(self,T,Zx,Zy,Zz)
   ! Calculate the one-electron ZORA spin orbit matrices numerically.
   ! This includes the relativitically modified kinetic energy integrals.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: T,Zx,Zy,Zz
      MAT4(REAL,:,:,:,:), allocatable :: ZORA
      MAT3(REAL,:,:,:), allocatable :: a1,b1
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: wt,V0
      VEC(INT,1) :: atom1
      VEC(INT,2) :: atom2
      VEC(INT,:), allocatable :: atoms
      INT :: fa,la,na,a,aa,ma,atom_a
      INT :: fb,lb,nb,b,bb,mb,atom_b
      INT :: n_pt,q,k,l
      BIN :: same_atoms
   ENSURE(self%basis_info_made,"MOLECULE.INTS:make_1e_ZORA_matrices ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.INTS:make_1e_ZORA_matrices ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.INTS:make_1e_ZORA_matrices ... no becke_grid")
      call create_(ZORA,self%n_bf,self%n_bf,3,3)

      ! Make the lower half of the ZORA spin orbit integrals
      do q = 1,no_of_atom_pairs_(self)
         if (NOT are_overlapping_atoms_(self,q)) cycle
         call set_atom_pair_indices_from_(self,q,atom_a,atom_b,fa,la,na,fb,lb,nb)
         same_atoms = atom_a==atom_b
         if (same_atoms) then; atom1 = [atom_a]; atoms = atom1
         else; atom2 = [atom_a,atom_b]; atoms = atom2
         end if

         ! Make the grid points and weights
         call make_grid_(self%becke_grid,pt,wt,atoms,compress=TRUE,wt_is_0=FALSE)
         n_pt = size(pt,1)  ! Keep the wt_is_0 array
         call create_(V0,n_pt)  ! local potential grids
         call ZORA_potential_(self,V0,pt)
         call make_bf_grids_(self%becke_grid,a1,b1,ma,mb,pt,atoms)
         V0 = wt*V0
         call destroy_(wt); call destroy_(pt)
         do k = 1,3
            do l = 1,3
               do a = 1,na
                  do b = 1,nb
                     aa = fa+a
                     bb = fb+b
                     ZORA(aa,bb,k,l) = sum(V0*a1(:,a,k)*b1(:,b,l))
                  end do
               end do
            end do
         end do
         call destroy_(V0)
         call destroy_(b1)
         call destroy_(a1)
      end do

      ! Make the upper half of the ZORA spin orbit integrals
      do k = 1,3
         do l = 1,3
            do a = 1,self%n_bf
               do b = 1,a-1
                  ZORA(b,a,l,k) = ZORA(a,b,k,l)
               end do
            end do
         end do
      end do

      ! Assemble the ZORA contribution to the 1 electron hamiltonian
      ! Scalar kinetic energy contribution
      T = ZORA(:,:,1,1)+ZORA(:,:,2,2)+ZORA(:,:,3,3)

      ! Spin-dependent spin-orbit contribution
      Zx = ZORA(:,:,2,3)-ZORA(:,:,3,2)
      Zy = ZORA(:,:,3,1)-ZORA(:,:,1,3)
      Zz = ZORA(:,:,1,2)-ZORA(:,:,2,1)
      call destroy_(ZORA)
   end subroutine

end module
