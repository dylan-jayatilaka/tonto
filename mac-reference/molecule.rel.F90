!==================================================================
!
! MOLECULE.REL: An object representation of a molecule.
! The scalar-relativistic (X2C) one-electron hamiltonian.
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
module MOLECULE_REL_MODULE

#  include "macros"

   include "molecule.rel.use"

   implicit none

   include "molecule.rel.int"


contains

!  ===================
!  Primitive integrals
!  ===================

   subroutine make_primitive_shells(self,frst,last,at,lmom,ex,shell_for)
   ! Make the list of the different primitive shells in the molecular
   ! basis set: for each, its first and last function "frst" and "last",
   ! its atom "at", its angular momentum "lmom" and its exponent "ex". A
   ! primitive which appears in several shells of an atom, with the same
   ! angular momentum and exponent, is listed once, so the functions are
   ! linearly independent. "shell_for" gives, for every primitive of every
   ! shell in the order of VEC{ATOM}:make_primitive_limits (by atom, by
   ! shell, then by primitive), its place in the list.
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: frst,last,at,lmom,shell_for
      VEC(REAL,:), allocatable, OUT :: ex
      VEC(INT,:), allocatable :: f_all,l_all,at_all,lmom_all
      VEC(REAL,:), allocatable :: ex_all
      INT :: nps,n_u,a,b,n
   ENSURE(allocated(self%atom),"MOLECULE.REL:make_primitive_shells ... no atom list")
   ENSURE(self%basis_info_made,"MOLECULE.REL:make_primitive_shells ... no basis info")

      ! Every primitive of every shell
      call make_primitive_limits_(self%atom,f_all,l_all,at_all,lmom_all,ex_all)
      nps = size(f_all)

      ! Give each one the place of the first one like it
      call create_(shell_for,nps)
      n_u = 0
      do a = 1,nps
         shell_for(a) = 0
         do b = 1,a-1
            if (at_all(b)/=at_all(a)) cycle
            if (lmom_all(b)/=lmom_all(a)) cycle
            if (abs(ex_all(b)-ex_all(a))>TOL(12)*ex_all(a)) cycle
            shell_for(a) = shell_for(b)
            exit
         end do
         if (shell_for(a)>0) cycle
         n_u = n_u+1
         shell_for(a) = n_u
      end do

      ! The list of different ones
      call create_(frst,n_u)
      call create_(last,n_u)
      call create_(at,n_u)
      call create_(lmom,n_u)
      call create_(ex,n_u)
      n = 0
      n_u = 0
      do a = 1,nps
         if (shell_for(a)<=n_u) cycle
         n_u = n_u+1
         frst(n_u) = n+1
         n = n+l_all(a)-f_all(a)+1
         last(n_u) = n
         at(n_u) = at_all(a)
         lmom(n_u) = lmom_all(a)
         ex(n_u) = ex_all(a)
      end do
      call destroy_(ex_all)
      call destroy_(lmom_all)
      call destroy_(at_all)
      call destroy_(l_all)
      call destroy_(f_all)
   end subroutine

   function no_of_primitive_functions(self) result (res)
   ! Return the number of different primitive functions in the molecular
   ! basis set, cartesian or spherical as the basis is. See
   ! "make_primitive_shells".
      type(MOLECULE_TYPE), IN :: self
      INT :: res
      VEC(INT,:), allocatable :: frst,last,at,lmom,shell_for
      VEC(REAL,:), allocatable :: ex
      call make_primitive_shells_(self,frst,last,at,lmom,ex,shell_for)
      res = last(size(last))
      call destroy_(shell_for)
      call destroy_(ex)
      call destroy_(lmom)
      call destroy_(at)
      call destroy_(last)
      call destroy_(frst)
   end function

   subroutine make_primitive_X2C_ints(self,S,T,V,W)
   ! Make the four one-electron matrices of the X2C hamiltonian between
   ! the normalised primitive functions of "make_primitive_shells": the
   ! overlap "S", the kinetic energy "T", the nuclear attraction "V" from
   ! all the nuclei, and "W", the matrix of p.Vp where p is the momentum.
   ! The nuclei are points or gaussian charges, as ".nucleus_model" says.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: S,T,V,W
      VEC(INT,:), allocatable :: frst,last,at,lmom,shell_for
      VEC(REAL,:), allocatable :: ex
      MAT(REAL,:,:), allocatable :: Vc,Wc
      INT :: nps,a,b,c,fa,la,na,fb,lb,nb,z_c
      REAL :: charge,eta
      BIN :: finite
      type(GAUSSIAN2_TYPE) :: G
   ENSURE(allocated(self%atom),"MOLECULE.REL:make_primitive_X2C_ints ... no atom list")
   ENSURE(self%basis_info_made,"MOLECULE.REL:make_primitive_X2C_ints ... no basis info")
   ENSURE(is_square_(S),"MOLECULE.REL:make_primitive_X2C_ints ... S must be square")
   ENSURE(is_same_shape_as_(T,S),"MOLECULE.REL:make_primitive_X2C_ints ... wrong shape, T")
   ENSURE(is_same_shape_as_(V,S),"MOLECULE.REL:make_primitive_X2C_ints ... wrong shape, V")
   ENSURE(is_same_shape_as_(W,S),"MOLECULE.REL:make_primitive_X2C_ints ... wrong shape, W")
   DIE_IF(self%nucleus_model/="point" AND self%nucleus_model/="gaussian","MOLECULE.REL:make_primitive_X2C_ints ... unknown nucleus model")
      finite = self%nucleus_model=="gaussian"

      ! Where each primitive shell is, and its functions
      call make_primitive_shells_(self,frst,last,at,lmom,ex,shell_for)
      nps = size(frst)
      ENSURE(size(S,1)==last(nps),"MOLECULE.REL:make_primitive_X2C_ints ... wrong size, S")
      S = ZERO
      T = ZERO
      V = ZERO
      W = ZERO

      ! Lower triangle of primitive shell pairs
      do a = 1,nps
         call set_l_(G%a,lmom(a))
         call set_position_(G%a,self%atom(at(a))%position)
         call set_exponent_(G%a,ex(a))
         fa = frst(a)
         la = last(a)
         na = la-fa+1
         do b = 1,a
            call set_l_(G%b,lmom(b))
            call set_position_(G%b,self%atom(at(b))%position)
            call set_exponent_(G%b,ex(b))
            fb = frst(b)
            lb = last(b)
            nb = lb-fb+1
            call make_normalized_overlap_ints_(G,S(fa:la,fb:lb),self%use_spherical_basis)
            call make_normalized_kinetic_ints_(G,T(fa:la,fb:lb),self%use_spherical_basis)

            ! Sum over the nuclei
            call create_(Vc,na,nb)
            call create_(Wc,na,nb)
            do c = 1,self%n_atom
               charge = self%atom(c)%nuclear_charge
               z_c = self%atom(c)%atomic_number
               if (finite AND z_c>0) then
                  eta = nucleus_eta_(z_c)
                  call make_normalized_NA_ints_(G,Vc,self%atom(c)%position,self%use_spherical_basis,eta)
                  call make_normalized_pVp_ints_(G,Wc,self%atom(c)%position,self%use_spherical_basis,eta)
               else
                  call make_normalized_NA_ints_(G,Vc,self%atom(c)%position,self%use_spherical_basis)
                  call make_normalized_pVp_ints_(G,Wc,self%atom(c)%position,self%use_spherical_basis)
               end if
               V(fa:la,fb:lb) = V(fa:la,fb:lb)-charge*Vc
               W(fa:la,fb:lb) = W(fa:la,fb:lb)-charge*Wc
            end do
            call destroy_(Wc)
            call destroy_(Vc)
         end do
      end do

      ! Fill in the upper triangle
      call symmetric_reflect_(S)
      call symmetric_reflect_(T)
      call symmetric_reflect_(V)
      call symmetric_reflect_(W)
      call destroy_(shell_for)
      call destroy_(ex)
      call destroy_(lmom)
      call destroy_(at)
      call destroy_(last)
      call destroy_(frst)
   end subroutine

   subroutine make_primitive_contraction_mx(self,cc,S)
   ! Make "cc", the matrix which turns a matrix between the normalised
   ! primitive functions into the matrix between the basis functions:
   ! column "i" holds the contraction coefficients of basis function "i".
   ! "S" is the primitive overlap matrix from "make_primitive_X2C_ints".
   ! Each column is scaled so that the basis function has the norm the
   ! rest of the program gives it, whatever the convention for the
   ! cartesian components; it is an error if the overlap matrix that "cc"
   ! gives is then not the overlap matrix of the molecule.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: cc
      MAT(REAL,:,:), IN :: S
      VEC(INT,:), allocatable :: frst,last,at,lmom,shell_for
      VEC(REAL,:), allocatable :: ex
      MAT(REAL,:,:), allocatable :: Sb,Sc
      INT :: a,q,i,k,n,p,row,col,l_q
      REAL :: coeff,err
   ENSURE(allocated(self%atom),"MOLECULE.REL:make_primitive_contraction_mx ... no atom list")
   ENSURE(self%basis_info_made,"MOLECULE.REL:make_primitive_contraction_mx ... no basis info")
   ENSURE(is_square_(S),"MOLECULE.REL:make_primitive_contraction_mx ... S must be square")
   ENSURE(size(cc,1)==size(S,1),"MOLECULE.REL:make_primitive_contraction_mx ... wrong 1st dimension, cc")
   ENSURE(size(cc,2)==self%n_bf,"MOLECULE.REL:make_primitive_contraction_mx ... wrong 2nd dimension, cc")
      call make_primitive_shells_(self,frst,last,at,lmom,ex,shell_for)

      ! Coefficients with respect to normalised primitives. The stored
      ! ones are with respect to unnormalised primitives.
      cc = ZERO
      p = 0
      col = 0
      do a = 1,self%n_atom
         do q = 1,size(self%atom(a)%basis%shell)
            n = self%atom(a)%basis%shell(q)%n_bf
            l_q = self%atom(a)%basis%shell(q)%l
            do i = 1,self%atom(a)%basis%shell(q)%n_cc
               p = p+1
               row = frst(shell_for(p))-1
               coeff = self%atom(a)%basis%shell(q)%contraction(i)/(FOUR*self%atom(a)%basis%shell(q)%exponent(i))**(HALF*l_q+HALF+QUARTER)
               do k = 1,n
                  cc(row+k,col+k) = cc(row+k,col+k)+coeff
               end do
            end do
            col = col+n
         end do
      end do

      ! Scale the columns to the norms of the basis functions
      call create_(Sb,self%n_bf,self%n_bf)
      call create_(Sc,self%n_bf,self%n_bf)
      call make_r_overlap_mx_(self,Sb)
      call change_basis_to_(S,Sc,cc)
      do i = 1,self%n_bf
         cc(:,i) = cc(:,i)*sqrt(Sb(i,i)/Sc(i,i))
      end do

      ! The contracted primitives must be the basis functions
      call change_basis_to_(S,Sc,cc)
      err = maxval(abs(Sc-Sb))
      DIE_IF(err>TOL(8),"MOLECULE.REL:make_primitive_contraction_mx ... the primitive functions do not contract to the basis functions")
      call destroy_(Sc)
      call destroy_(Sb)
      call destroy_(shell_for)
      call destroy_(ex)
      call destroy_(lmom)
      call destroy_(at)
      call destroy_(last)
      call destroy_(frst)
   end subroutine


!  ===============
!  X2C core matrix
!  ===============

   subroutine make_r_X2C_core_mx(self,H)
   ! Make "H", the scalar-relativistic one-electron (core) matrix by
   ! exact two-component decoupling, X2C. It replaces the kinetic energy
   ! plus nuclear attraction matrix; nothing else in an SCF changes.
   !
   ! In the primitive basis, with S, T and V the overlap, kinetic and
   ! nuclear attraction matrices, W the matrix of p.Vp and c the speed of
   ! light, the one-electron Dirac equation without spin-orbit coupling is
   !
   !    ( V        T       ) (C_L)   ( S    0     ) (C_L)
   !    ( T   W/4c^2 - T   ) (C_S) = ( 0  T/2c^2  ) (C_S) E .
   !
   ! From its positive-energy solutions,
   !
   !    X = C_S C_L^-1 ,   S' = S + X^t T X/2c^2 ,
   !    R = S^-1/2 (S^-1/2 S' S^-1/2)^-1/2 S^1/2 ,
   !    H = R^t [ V + T X + X^t T + X^t (W/4c^2 - T) X ] R ,
   !
   ! and H is then contracted. The eigenvalues of H are exactly the
   ! positive energies E. The work is done in the orthonormal bases given
   ! by the Cholesky factors of S, for the large component, and of
   ! T/2c^2, for the small; there the right side above is the unit
   ! matrix, S' = 1 + X^t X and R = S'^-1/2.
   !
   ! Reference: Ilias and Saue, J. Chem. Phys. 126, 064102 (2007).
   ! No correction is made to other operators (no "picture change").
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: H
      MAT(REAL,:,:), allocatable :: S,T,V,W,cc,evec,dirac,CL,XX,RR,Ho,HR,proj
      VEC(REAL,:), allocatable :: eval,eig
      INT :: n,i
      REAL :: c_light,c2,err
      BIN :: fail
   ENSURE(allocated(self%atom),"MOLECULE.REL:make_r_X2C_core_mx ... no atom list")
   ENSURE(self%basis_info_made,"MOLECULE.REL:make_r_X2C_core_mx ... no basis info")
   ENSURE(is_square_(H),"MOLECULE.REL:make_r_X2C_core_mx ... H must be square")
   ENSURE(size(H,1)==self%n_bf,"MOLECULE.REL:make_r_X2C_core_mx ... wrong size, H")
   DIE_IF(spinorbital_genre_(self%SCF_data)=="gc","MOLECULE.REL:make_r_X2C_core_mx ... X2C is not available for general complex orbitals")
      c_light = SPEED_OF_LIGHT_AU
      c2 = c_light*c_light

      ! Integrals between primitives, and the contraction matrix
      n = no_of_primitive_functions_(self)
      call create_(S,n,n)
      call create_(T,n,n)
      call create_(V,n,n)
      call create_(W,n,n)
      call create_(cc,n,self%n_bf)
      call make_primitive_X2C_ints_(self,S,T,V,W)
      call make_primitive_contraction_mx_(self,cc,S)

      ! Cholesky factors: S = L_S L_S^t and T = L_T L_T^t
      call to_cholesky_factor_(S,fail)
      DIE_IF(fail,"MOLECULE.REL:make_r_X2C_core_mx ... the primitive functions are linearly dependent")
      call to_cholesky_factor_(T,fail)
      DIE_IF(fail,"MOLECULE.REL:make_r_X2C_core_mx ... the primitive kinetic energy matrix is not positive definite")

      ! The contraction from the orthonormal large-component basis
      call create_(proj,n,self%n_bf)
      call to_product_of_(proj,S,cc,transpose_a=TRUE)
      call destroy_(cc)

      ! V = L_S^-1 V L_S^-t and W = L_T^-1 W L_T^-t
      call solve_lower_triangular_equations_(S,V)
      V = transpose(V)
      call solve_lower_triangular_equations_(S,V)
      call solve_lower_triangular_equations_(T,W)
      W = transpose(W)
      call solve_lower_triangular_equations_(T,W)

      ! T = L_S^-1 L_T: the kinetic energy between the two bases, over c sqrt(2)
      call solve_lower_triangular_equations_(S,T)

      ! The Dirac matrix
      call create_(dirac,2*n,2*n)
      dirac(1:n,1:n) = HALF*(V+transpose(V))
      dirac(1:n,n+1:2*n) = sqrt(TWO)*c_light*T
      dirac(n+1:2*n,1:n) = sqrt(TWO)*c_light*transpose(T)
      dirac(n+1:2*n,n+1:2*n) = QUARTER*(W+transpose(W))
      do i = n+1,2*n
         dirac(i,i) = dirac(i,i)-TWO*c2
      end do
      call destroy_(W)
      call destroy_(V)
      call destroy_(T)
      call destroy_(S)

      ! Its solutions; the upper half are the positive-energy ones
      call create_(eval,2*n)
      call create_(evec,2*n,2*n)
      call solve_symmetric_eigenproblem_(dirac,eval,evec)
      DIE_IF(eval(n)>-c2 OR eval(n+1)<-c2,"MOLECULE.REL:make_r_X2C_core_mx ... cannot tell positive from negative energy solutions")

      ! XX = C_S C_L^-1
      call create_(CL,n,n)
      call create_(XX,n,n)
      call to_inverse_of_(CL,evec(1:n,n+1:2*n))
      call to_product_of_(XX,evec(n+1:2*n,n+1:2*n),CL)
      call destroy_(CL)
      call destroy_(evec)

      ! Ho = h_LL + h_LS X + X^t h_SL + X^t h_SS X, not yet renormalised
      call create_(Ho,n,n)
      call create_(HR,n,n)
      call create_(RR,n,n)
      Ho = dirac(1:n,1:n)
      call to_product_of_(HR,dirac(1:n,n+1:2*n),XX)
      Ho = Ho+HR+transpose(HR)
      RR = dirac(n+1:2*n,n+1:2*n)
      call change_basis_to_(RR,HR,XX)
      Ho = Ho+HR
      call destroy_(dirac)

      ! RR = (1 + X^t X)^-1/2
      call create_(evec,n,n)
      call create_(eig,n)
      call to_product_of_(HR,XX,XX,transpose_a=TRUE)
      do i = 1,n
         HR(i,i) = HR(i,i)+ONE
      end do
      call solve_symmetric_eigenproblem_(HR,eig,evec)
      do i = 1,n
         HR(:,i) = evec(:,i)/sqrt(sqrt(eig(i)))
      end do
      call to_product_of_(RR,HR,HR,transpose_b=TRUE)
      call destroy_(XX)

      ! Renormalise: Ho = R Ho R
      call change_basis_to_(Ho,HR,RR)
      Ho = HALF*(HR+transpose(HR))
      call destroy_(RR)

      ! The decoupling is exact: Ho must have the positive energies
      call solve_symmetric_eigenproblem_(Ho,eig,evec)
      err = maxval(abs(eig-eval(n+1:2*n))/(ONE+abs(eig)))
      DIE_IF(err>TOL(8),"MOLECULE.REL:make_r_X2C_core_mx ... the X2C hamiltonian does not reproduce the Dirac energies")
      call destroy_(eig)
      call destroy_(evec)
      call destroy_(eval)

      ! Contract
      call change_basis_to_(Ho,H,proj)
      H = HALF*(H+transpose(H))
      call destroy_(proj)
      call destroy_(HR)
      call destroy_(Ho)
   end subroutine

end module
