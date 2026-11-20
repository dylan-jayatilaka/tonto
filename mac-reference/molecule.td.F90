!==================================================================
! MOLECULE.TD: Time-dependent procedures
!
! Copyright (C) Dylan Jayatilaka (2024-2025)
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
module MOLECULE_TD_MODULE

#  include "macros"

   include "molecule.td.use"

   implicit none

   include "molecule.td.int"


contains

! ================
! Testing Davidson
! ================

   subroutine diagonalize_Davidson(A,eval,evec,n_evals,n_keep,maxit,tolcon)
   ! Davidson diagonaliser for matrix "A".
   ! See Crouzeix et al (1990) Inria RR 1353
   ! Step numbers & symbols below mostly from p. 5 Crouzeix.
      MAT(REAL,:,:), IN :: A
      VEC(REAL,:), allocatable, OUT :: eval  ! The "n" eigenvalues
      MAT(REAL,:,:), allocatable, OUT :: evec  ! The "n" eigenvector columns
      INT, IN :: n_evals  ! No. of eigenvalues wanted, "n"
      INT, OUT :: n_keep  ! No. of trial vector sets kept
      INT, IN :: maxit  ! Max. no. of iterations allowed
      REAL, IN :: tolcon  ! Max. residual vec norm, to end
      MAT(REAL,:,:), allocatable :: R  ! Ritz vectors <- becomes "evec"
      MAT(REAL,:,:), allocatable :: V  ! V(:,n)       <- the Davidson trial vectors
      MAT(REAL,:,:), allocatable :: W  ! W(:,n)       <- A.V
      MAT(REAL,:,:), allocatable :: H  ! A            <- in basis "V"
      VEC(REAL,:), allocatable :: T  ! New direction, T
      VEC(REAL,:), allocatable :: rvl  ! Length of the residual vectors
      INT :: iter,n_tr
      INT :: d,n,f,l,i
      BIN :: converged
      BIN :: too_many
      BIN :: collapse
      REAL :: bot
   ENSURE(n_keep>=3,"MOLECULE.TD:diagonalize_Davidson ... Must store minimum 2+1=3 trial vector sets")

      ! Sizes
      d = size(A,1)  ! "d" could be big!
      n = n_evals  ! Usually n << d

      ! Best vectors
      call create_(R,d,n)  ! Will be "evec"
      call create_(eval,n*n_keep)  ! Will shrink to "n"

      ! Residue vector length
      call create_(rvl,n)

      ! Davidson vectors "V"
      call create_(V,d,n*n_keep)  ! Need all in memory???
      call create_(W,d,n*n_keep)  ! Space for product A*V, worst case
      call create_(T,d)  ! Space for new directions

   ! .TD_data.put_D_table_header

      ! Initialize
      call to_unit_mx_(V)  ! Initial guess
      iter = 0  ! Overall iteration count
      n_tr = 0  ! No. of trial vector sets
      converged = FALSE
      too_many = FALSE
      do
         iter = iter+1  ! Iteration 
         n_tr = n_tr+1  ! No. of vector blocks (-1)

         ! First & last trial vector columns
         l = n*(n_tr-1)
         f = l+1
         l = l+n

         ! 1. Expensive step, extends W
         ! Replace with low-mem matrix product
         W(:,f:l) = matmul(A,V(:,f:l))

         ! 2. Expand Rayleigh matrix
         call expand_(H,l,l)
         call create_(evec,l,l)
         H(f:l,:) = matmul(transpose(V(:,f:l)),W(:,1:l))
         call symmetric_reflect_(H,set_upper=TRUE)

         ! 3. Diagonalise (increasing eigenvalues)
         call diagonalize_Jacobi_(H,eval(1:l),evec)

         ! 4. Lowest Ritz vectors
         R = matmul(V(:,1:l),evec(:,1:n))

         ! Too many iters? Collapse space?
         too_many = iter>maxit; if (too_many) exit
         collapse = n_tr==n_keep
         call flush_(stdout)
         call text_(stdout,"=========")
         call show_(stdout,"iter      =",iter)
         call show_(stdout,"n_tr      =",n_tr)
         call show_(stdout,"collapse? =",collapse)
         call show_(stdout,"too many? =",too_many)
         call show_(stdout,"f         =",f)
         call show_(stdout,"l         =",l)
         if (too_many) then

            ! Save to disk?
            call destroy_(evec)
            call destroy_(H)
         else if (collapse) then

            ! Restart with Ritz vector
            n_tr = 0
            V(:,1:n) = R
         ! V(:,1:n).Householder_QR ! Needed?

            ! Note: clean both
            call destroy_(evec)
            call destroy_(H)
         else

            ! 5-7. Get residual & new direction T, per-column
            do i = 1,n

               ! 5. Residual T & its length
               T = eval(i)*R(:,i)-matmul(W(:,1:l),evec(:,i))
               rvl(i) = norm2(T)

               ! 6. Precondition
               bot = eval(i)-H(i,i)
               if (abs(bot)<0.001d0) bot = bot+TOL(2)
               bot = ONE/bot

               ! 7a. Set new direction t
               V(:,l+i) = bot*T
            end do
            call flush_(stdout)
            call show_(stdout,"conv =",maxval(abs(rvl)))
            call flush_(stdout)
            call text_(stdout,"eval:")
            call put_(stdout,eval(1:n))

            ! Finished?
            converged = all(rvl<tolcon); if (converged) exit

            ! 7. Orthonormalize vectors V
            call modified_Gram_Schmidt_QR_(V(:,1:l+n))
         ! V(:,1:l+n).Householder_QR

            ! Clean up
            call destroy_(evec)
         end if
      end do
      ENSURE(converged,"MOLECULE.TD:diagonalize_Davidson ... No convergence")
      ENSURE(NOT too_many,"MOLECULE.TD:diagonalize_Davidson ... Too many iterations")

      ! Clean up
      call destroy_(evec)
      call destroy_(H)
      call destroy_(T)
      call destroy_(W)
      call destroy_(V)
      call destroy_(rvl)

      ! Recover solution
      call move_alloc(R,evec)
      call shrink_(eval,n)
   end subroutine

   subroutine test_MGS_QR()
   ! Test orthogonalisation routines for Davidson diagonaliser.
      MAT(REAL,:,:), allocatable :: V,Q,R
      INT :: dim
      INT :: n_keep

      ! Sizes
      dim = 10
      n_keep = 5

      ! Number Davidson vectors to keep.
      call create_(V,dim,n_keep)

      ! Initial
      call random_number(V)
      call text_(stdout,"V:")
      call put_(stdout,V)
      call create_(Q,dim,n_keep)
      call create_(R,n_keep,n_keep)
      call modified_Gram_Schmidt_QR_(V,Q,R)
      call flush_(stdout)
      call text_(stdout,"========================")
      call text_(stdout,"modified Gram-Schmidt QR")
      call text_(stdout,"========================")
      call flush_(stdout)
      call text_(stdout,"Q:")
      call put_(stdout,Q)
      call text_(stdout,"Q^T Q:")
      call put_(stdout,matmul(transpose(Q),Q))
      call text_(stdout,"R:")
      call put_(stdout,R)
      call text_(stdout,"QR - V:")
      call put_(stdout,matmul(Q,R)-V)
      call flush_(stdout)
      call text_(stdout,"==============")
      call text_(stdout,"Householder QR")
      call text_(stdout,"==============")
      call flush_(stdout)
      call Householder_QR_(V,Q,R)
      call text_(stdout,"Q:")
      call put_(stdout,Q)
      call text_(stdout,"Q^T Q:")
      call put_(stdout,matmul(transpose(Q),Q))
      call text_(stdout,"R:")
      call put_(stdout,R)
      call text_(stdout,"QR - V:")
      call put_(stdout,matmul(Q,R)-V)
   end subroutine

   subroutine test_Davidson_CPS_6()
   ! Test the Davidson algorithm with the example in section 6 of
   ! Crouzeix et al (1990) Inria RR 1353
      MAT(REAL,:,:), allocatable :: A
      MAT(REAL,:,:), allocatable :: evec
      VEC(REAL,:), allocatable :: eval
      INT :: d,n_evals,i
      INT :: n_keep,maxit

      ! Sizes
      d = 30
      n_evals = 2
      n_keep = 8
      maxit = 50

      ! Set up
      call create_(A,d,d)
      call create_(eval,d)
      call create_(evec,d,d)
      A = ZERO
      do i = 1,d
         A(i,i) = i
         if (i>1) then
            A(i-1,i) = HALF
            A(i,i-1) = HALF
         end if
         if (i<d) then
            A(i+1,i) = HALF
            A(i,i+1) = HALF
         end if
      end do
      A(1,d) = HALF
      A(d,1) = HALF
      call flush_(stdout)
      call text_(stdout,"===========")
      call text_(stdout,"Jacobi eigs")
      call text_(stdout,"===========")
      call flush_(stdout)
      call diagonalize_Jacobi_(A,eval,evec)
      call flush_(stdout)
      call text_(stdout,"eval:")
      call put_(stdout,eval(1:n_evals))
      call text_(stdout,"evec:")
      call put_(stdout,evec(:,1:n_evals))
      call destroy_(eval)
      call destroy_(evec)

   ! A.diagonalize_Davidson(eval,evec,n_evals,n_keep,maxit,tolconv=TOL(7))
      call diagonalize_Davidson(A,eval,evec,n_evals,n_keep,maxit,tolcon=TOL(7))
      call flush_(stdout)
      call text_(stdout,"===============")
      call text_(stdout,"Davidson test 6")
      call text_(stdout,"===============")
      call flush_(stdout)
      call flush_(stdout)
      call text_(stdout,"eval:")
      call put_(stdout,eval)
      call flush_(stdout)
      call text_(stdout,"evec:")
      call put_(stdout,evec)
      call flush_(stdout)
      call text_(stdout,"evec^T evec:")
      call put_(stdout,matmul(transpose(evec),evec))
   end subroutine


!  ==============
!  Excited states
!  ==============

   subroutine TD_excited_states(self)
   ! Do a TD excited state calculation.
   ! Most of the info is kept in .TD_data.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%atom_info_made,"MOLECULE.TD:TD_excited_states ... No atoms!")
   ENSURE(self%basis_info_made,"MOLECULE.TD:TD_excited_states ... No basis set!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:TD_excited_states ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:TD_excited_states ... MOs.r does not exist")

      ! Minimal setup
      call set_defaults_(self%TD_data,self%n_bf,self%n_a,self%n_b)
      if (allocated(self%SCF_data)) then
         call set_defaults_(self%TD_data,self%MOs%r,self%MO_energies%r,self%SCF_data%VMO_energies)
      else
         call set_defaults_(self%TD_data,self%MOs%r,self%MO_energies%r)
      end if

      ! State selection
      call update_(self%TD_data)

      ! Banner
      call put_banner_(self%TD_data)

      ! Which TD calc?
      select case (self%TD_data%implementation)

      case ("in-core ")
         select case (self%TD_data%method)
         case ("r-cis-s=0        "); call do_r_CIS_S0_IC_(self)
         case ("r-cis-s=0&1      "); call do_r_CIS_S01_IC_(self)
         case ("r-cis-s=1        "); call do_r_CIS_S1_IC_(self)
         case ("r-cis-ss-s=0     "); call do_r_CIS_SS_S0_IC_(self)
         case ("r-tdhf-s=0       "); call do_r_TDHF_S0_IC_(self)
         case ("r-tdhf-s=0&1     "); call do_r_TDHF_S01_IC_(self)
         case ("r-tdhf-s=1       "); call do_r_TDHF_S1_IC_(self)
         case ("u-cis            "); call do_u_CIS_IC_(self)
         case ("u-cis-v1         "); call do_u_CIS_IC_v1_(self)  ! Uses SS
         case default; allocate(tonto%known_keywords(9))
         tonto%known_keywords(1) = "r-cis-s=0        "
         tonto%known_keywords(2) = "r-cis-s=0&1      "
         tonto%known_keywords(3) = "r-cis-s=1        "
         tonto%known_keywords(4) = "r-cis-ss-s=0     "
         tonto%known_keywords(5) = "r-tdhf-s=0       "
         tonto%known_keywords(6) = "r-tdhf-s=0&1     "
         tonto%known_keywords(7) = "r-tdhf-s=1       "
         tonto%known_keywords(8) = "u-cis            "
         tonto%known_keywords(9) = "u-cis-v1         "
         call unknown_(tonto,self%TD_data%method,"MOLECULE.TD:TD_excited_states")
         deallocate(tonto%known_keywords)
         end select

      case ("davidson")
         select case (self%TD_data%method)
         case ("r-cis-ss-s=0     "); call do_r_CIS_SS_S0_(self)
         case ("r-cis-ss-s=0&1   "); call do_r_CIS_SS_S01_(self)
         case ("r-cis-ss-s=1     "); call do_r_CIS_SS_S1_(self)
         case ("u-cis-ss         "); call do_u_CIS_SS_(self)
      ! case("r-cis-ss-s=0-d1  "); .do_r_CIS_SS_S0_D1 ! In core
      ! case("r-cis-ss-s=0-d2  "); .do_r_CIS_SS_S0_D2 ! In core
      ! case("r-cis-ss-s=0-v0  "); .do_r_CIS_SS_S0_v0
         case default; allocate(tonto%known_keywords(4))
         tonto%known_keywords(1) = "r-cis-ss-s=0     "
         tonto%known_keywords(2) = "r-cis-ss-s=0&1   "
         tonto%known_keywords(3) = "r-cis-ss-s=1     "
         tonto%known_keywords(4) = "u-cis-ss         "
         call unknown_(tonto,self%TD_data%method,"MOLECULE.TD:TD_excited_states")
         deallocate(tonto%known_keywords)
         end select

      case default
         allocate(tonto%known_keywords(2))
         tonto%known_keywords(1) = "in-core "
         tonto%known_keywords(2) = "davidson"
         call unknown_(tonto,self%TD_data%implementation,"MOLECULE.TD:TD_excited_states")
         deallocate(tonto%known_keywords)
      end select

      ! Put results
      call put_results_(self%TD_data)
      call put_evecs_(self%TD_data)
   end subroutine


! Efficient Davidson methods

   subroutine eigensolve_Davidson(self,TD,HV_prod)
   ! Davidson diagonalizer. See Crouzeix (1990) Inria RR 1353.
   ! See also Crouzeix (1994) SIAM J Sci Comput 15 p. 62-76.
   !
   ! Steps 1-7 & symbols in code below are mostly from p. 5
   ! of Crouzeix (1990). But "R" is the residual, not "T"! 
   !
   ! "TD" ......... Holds key calculation parameters.
   !                It is a kind of Davidson iterator object.
   !
   ! "HV_prod" .... Does the expensive method-specific 2e part
   !                of the "A matrix product" in the AO basis.
   !
   ! "TD" holds iteration count information, methods to test 
   ! for convergence, for subspace collapse, and CIS/TD 
   ! specific storage. Key variables are:
   !
   ! n_evals  ..... desired no. of lowest excitations   IN
   ! n_keep   ..... No. of trial vectors to keep        IN
   ! max_iter ..... Max. no. of iterations allowed      IN
   ! tol_Davidson.. Convergence tol on residual vector  IN
   ! eval     ..... the lowest eigenvalues              OUT
   ! evec     ..... corresponding eigenvectors          OUT
   !                Note: n_keep > evec.dim2 >= 3
   ! max_res  ..... Maximum residual vector norms       OUT
   !
   ! NOTE: the trial vectors "V" are initially set to unit
   ! vectors in the first few components. So the states that
   ! these components in "V" refer to must be chosen wisely
   ! e.g. by selecting appropriate virtual orbitals and by
   ! sorting single excitations into energy order.
   ! =====
      type(MOLECULE_TYPE), INOUT :: self
      type(TD_DATA_TYPE), target, INOUT :: TD
      interface
         NON_MPI_PURE subroutine HV_prod(mol,HV,V,TD)
         use TYPES_MODULE
            type(MOLECULE_TYPE), INOUT :: mol
            MAT(REAL,:,:), OUT :: HV
            MAT(REAL,:,:), IN :: V
            type(TD_DATA_TYPE), INOUT :: TD
         end subroutine
      end interface
      MAT(REAL,:,:), allocatable :: H  ! H in basis of "V" columns
      MAT(REAL,:,:), allocatable :: U  ! H eigenvectors
      VEC(REAL,:), allocatable :: E  ! H eigenvalues
      MAT(REAL,:,:), PTR :: W  ! Aliases
      MAT(REAL,:,:), PTR :: V
      VEC(REAL,:), PTR :: R
      MAT(REAL,:,:), PTR :: evec
      VEC(REAL,:), PTR :: eval
      REAL :: bot,maxres
      INT :: d,n,n_keep
      INT :: f,l,i
   ENSURE(allocated(TD%eval),"MOLECULE.TD:eigensolve_Davidson ... no TD.eval")
   ENSURE(allocated(TD%evec),"MOLECULE.TD:eigensolve_Davidson ... no TD.eval")
   ENSURE(size(TD%eval)==size(TD%evec,2),"MOLECULE.TD:eigensolve_Davidson ... in TD: eval & evec incompatible")

      ! Set up aliases.
      ! Put in TD to store & also to
      ! avoid extra memory allocation
      W => TD%W
      V => TD%V
      R => TD%R
      evec => TD%evec
      eval => TD%eval

      ! Sizes
      d = size(evec,1)  ! No. of excitations
      n = size(evec,2)  ! Block of evals, n << d
      n_keep = TD%n_keep  ! No. of trial vector sets

      ! Output options
      call put_D_table_header_(TD)
      do  ! Davidson loop
         call update_iteration_(TD)
         if (collapse_(TD)) then
            call reset_subspace_(TD)  ! Keep one trial set
            call destroy_(H)  ! Remake H from scratch
         end if

         ! 1. EXPENSIVE: W(:,f:l) = H x V(:,f:l) 
         f = fst_trial_vector_(TD)
         l = lst_trial_vector_(TD)
         call HV_prod(self,W(:,f:l),V(:,f:l),TD)

         ! 2. Expand Rayleigh matrix
         call expand_(H,l,l); call create_(U,l,l); call create_(E,l)
         H(f:l,:) = matmul(transpose(V(:,f:l)),W(:,1:l))
         call symmetric_reflect_(H,set_upper=TRUE)

         ! 3. Diagonalise 
         call solve_symmetric_eigenproblem_(H,E,U)

         ! 4. Keep Ritz vectors
         TD%eval = E(1:n)  ! <== ANSWER
         TD%evec = matmul(V(:,1:l),U(:,1:n))

         ! Get residual, add new directions
         maxres = ZERO
         do i = 1,n

            ! 5. Residual
            R = E(i)*TD%evec(:,i)-matmul(W(:,1:l),U(:,i))
            maxres = max(norm2(R),maxres)

            ! 6. New directions
            bot = E(i)-H(i,i)
            if (abs(bot)<TOL(3)) bot = bot+TOL(2)
            bot = ONE/bot
            V(:,l+i) = bot*R
         end do
         call set_max_res_(TD,maxres)  ! <== For convergence test

         ! Clean up
         call destroy_(E); call destroy_(U)

         ! Print out table
         call put_D_table_body_and_footer_(TD)

         ! Converged?
         if (converged_(TD)) then; exit
         else

            ! 7. Orthonormalize vectors V 
            if (TD%use_MGS) then; call modified_Gram_Schmidt_QR_(V(:,1:l+n))
            else; call Householder_QR_(V(:,1:l+n))
            end if
         end if
      end do

      ! Store results
      call store_results_(TD,self%name)
   end subroutine


! Unrestricted

   subroutine do_u_CIS_SS(self)
   ! Do an unrestricted CIS calculation.
   ! Magic is hidden in the .TD_data object, which keeps
   ! Davidson iteration, and other info. Also, the passed
   ! method does the efficient Davidson A.V product.
      type(MOLECULE_TYPE), INOUT :: self

      ! Start up; set archive name
      call initialize_Davidson_(self%TD_data,self%name)

      ! Do the diagonalisation & print results
      call eigensolve_Davidson_(self,self%TD_data,do_u_CIS_SS_AV_prod)

      ! Doubles?
   ! .do_doubles(.TD_data)

      ! Clean up
      call finalize_iteration_(self%TD_data)
   end subroutine

   NON_MPI_PURE subroutine do_u_CIS_SS_AV_prod(self,AV,V,TD)
   ! With *unrestricted* trial vectors "V" do the (A x V)
   ! product for the Davidson procedure (efficiently in
   ! the AO basis) and place the result into "AV".
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: AV
      MAT(REAL,:,:), IN :: V
      type(TD_DATA_TYPE), INOUT :: TD
      INT :: d,d1,d2,d3,d4
      INT :: n,n1,n2,n3,n4
   ENSURE(allocated(TD%Vao),"MOLECULE.TD:do_u_CIS_SS_AV_prod ... TD.Vao not preallocated")
   ENSURE(allocated(TD%Jao),"MOLECULE.TD:do_u_CIS_SS_AV_prod ... TD.Jao not preallocated")
   ENSURE(allocated(TD%Kao),"MOLECULE.TD:do_u_CIS_SS_AV_prod ... TD.Kao not preallocated")

      ! 1e part: Set AV = diag(A).V
      call make_u_diag_AV_(TD,AV,V)

      ! Sizes
      d = size(TD%single,1)
      d1 = d
      d2 = d+d1
      d3 = d+d2
      d4 = d+d3
      n = TD%n_evals
      n1 = n
      n2 = 2*n
      n3 = 3*n
      n4 = 4*n
      call change_r_V_to_AO_(TD,TD%Vao(1:n1,:,:),V(1:d1,:))  ! aa
      call change_r_V_to_AO_(TD,TD%Vao(n1+1:n2,:,:),V(d1+1:d2,:))  ! ba
      call change_r_V_to_AO_(TD,TD%Vao(n2+1:n3,:,:),V(d2+1:d3,:))  ! ab
      call change_r_V_to_AO_(TD,TD%Vao(n3+1:n4,:,:),V(d3+1:d4,:))  ! bb

      ! 2e part: AV =+ (AV)ai, in AO basis
      call u_CIS_AV_(self,TD%Jao,TD%Kao,TD%Vao)  ! Expensive

      ! Change to MO basis
      call change_u_V_to_MO_(TD,AV,TD%Jao,TD%Kao)
   end subroutine

   subroutine do_doubles(self,TD)
                  ! :: PURE
   ! Do the doubles contribution at the end if wanted.
      type(MOLECULE_TYPE), INOUT :: self
      type(TD_DATA_TYPE), INOUT :: TD
      select case (TD%dressing)
      case ("mazur "); call do_doubles_Mazur_(self,self%TD_data)
   ! case ("window");   .do_doubles_window(.TD_data)
      case default; allocate(tonto%known_keywords(1))
      tonto%known_keywords(1) = "mazur "
      call unknown_(tonto,TD%dressing,"MOLECULE.TD:do_doubles")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine do_doubles_Mazur(self,TD)
                        ! :: PURE
   ! Do a one-shot (non-iterative) doubles contribution
   ! using the perturbative Mazur theory.
      type(MOLECULE_TYPE), INOUT :: self
      type(TD_DATA_TYPE), INOUT :: TD
      MAT(REAL,:,:), allocatable :: Jd,Kd,Pd
      MAT(REAL,:,:), allocatable :: J0,K0,P0
      MAT(REAL,:,:), allocatable :: Fg,Kg,Fe,Ke
      INT :: d,n,no,nv
      INT :: d1,d2,d3,d4
      INT :: ai,a,i,bj,b,j
      REAL :: Aai,Bai,Eai,Ebj
      REAL :: del,val,rt2,F,X
      MAT3(REAL,:,:,:), allocatable :: V
      call find_doubles_Mazur_(TD)
      if (NOT allocated(TD%double)) return

      ! See doubles
      call put_doubles_(TD)

      ! Constants
      n = TD%n_evals
      d = size(TD%single,1)
      rt2 = ONE/sqrt(TWO)

      ! Create correction array
      call create_(TD%evalc,size(TD%double,1))

      ! Copy Vaa and Vbb
      d1 = d
      d2 = d+d1
      d3 = d+d2
      d4 = d+d3
      call create_(V,d,n,2)
      V(1:d1,:,1) = TD%V(1:d1,:)  ! aa
      V(d+1:d2,:,2) = TD%V(d3+1:d4,:)  ! bb

      ! Density & J,K matrices
      n = TD%n_bf
      no = TD%n_occ
      nv = TD%n_vir
      call create_(J0,n,n); call create_(K0,n,n); call create_(P0,n,n)  ! G state
      call create_(Jd,n,n); call create_(Kd,n,n); call create_(Pd,n,n)  ! Delta S-G
      call create_(Fg,nv,no); call create_(Kg,nv,no)  ! G state
      call create_(Fe,nv,no); call create_(Ke,nv,no)  ! S state

      ! Ground state density
      P0 = matmul(TD%MOo,transpose(TD%MOo))

      ! Ground state Fock mx, in AO basis
      call make_r_JK_engine_(self,J0,K0,P0)

      ! VO block. Not all needed, wasteful.
      Fg = matmul(transpose(TD%MOv),matmul(J0,TD%MOo))  ! J
      Kg = matmul(transpose(TD%MOv),matmul(K0,TD%MOo))
      Fg = TWO*Fg-Kg

      ! Loop over classes
      do n = 1,size(TD%double,1)
         ai = TD%double(n,1)
         a = TD%single(ai,1)
         i = TD%single(ai,2)
         bj = TD%double(n,2)
         b = TD%single(bj,1)
         j = TD%single(bj,2)
         call show_(stdout,"ai = ",ai)
         call show_(stdout,"i  = ",i-TD%n_occ)
         call show_(stdout,"a  = ",a)
         call show_(stdout,"bj = ",bj)
         call show_(stdout,"j  = ",j-TD%n_occ)
         call show_(stdout,"b  = ",b)

         ! Difference DM due to Phi(J->B)
         Pd = matmul(TD%MOv(:,b:b),transpose(TD%MOv(:,b:b)))-matmul(TD%MOo(:,j:j),transpose(TD%MOo(:,j:j)))

         ! Delta Fock matrix, in AO basis
         call make_r_JK_engine_(self,Jd,Kd,Pd)

         ! Delta Fock, VO block.
         Fe = matmul(transpose(TD%MOv),matmul(Jd,TD%MOo))  ! J
         Ke = matmul(transpose(TD%MOv),matmul(Kd,TD%MOo))
         Fe = Fe-Ke

         ! Coefficients & Denominator
         Aai = TD%V(ai,n)  ! aa
         Bai = TD%V(d3+ai,n)  ! bb
         Eai = TD%delEab(ai)
         Ebj = TD%delEab(bj)
         del = TD%eval(n)-Eai-Ebj
         del = ONE/del

         ! Add the correction
         if (ai/=bj) then
            F = Fg(a,i)+Fe(a,i)
            X = Kg(a,i)
            val = del*F*F*Aai
         end if
      end do
   end subroutine


!   do_doubles_window(TD) ! :: PURE
!   ! Do the doubles contribution at the end if wanted.
!      self :: INOUT
!      TD :: TD_DATA, INOUT
!
!      F, J, K ,P  :: MAT{REAL}@
!      F0,J0,K0,P0 :: MAT{REAL}@
!      ai, a,i :: INT
!      bj, b,j :: INT
!      n,  f,l :: INT
!      val :: REAL
!
!      if (TD.double.deallocated) return
!
!      TD.put_doubles
!
!      ! Density matrices
!      n = .n_bf
!       F.create(n,n);  J.create(n,n);  K.create(n,n);  P.create(n,n)
!      F0.create(n,n); J0.create(n,n); K0.create(n,n); P0.create(n,n)
!
!      ! Ground state density
!      P0 = matmul(TD.MO,transpose(TD.MO))
!
!      ! Ground state Fock mx
!      .make_r_JK_direct(J0,K0,P0)
!
!      ! Loop over classes
!      do n = 1,TD.S_list.dim1
!         f  = TD.S_list(n,1)
!         l  = TD.S_list(n,2)
!         bj = TD.double(f,2)
!         b  = TD.single(bj,1)
!         j  = TD.single(bj,2)
!         stdout.show("f  = ",f)
!         stdout.show("l  = ",l)
!         stdout.show("bj = ",bj)
!         stdout.show("j  = ",j-TD.n_occ)
!         stdout.show("b  = ",b)
!         
!         ! Make Phi(J->B) determinant density

!               + matmul(TD.MO(:,b),transpose(TD.MO(:,j)))
!
!         ! AO Fock matrix
!         .make_r_JK_direct(J,K,P)
!
!         do ai = f,l
!            a  = TD.single(ai,1)
!            i  = TD.single(ai,2)
!            stdout.show("ai = ",ai)
!            stdout.show("i  = ",i-TD.n_occ)
!            stdout.show("a  = ",a)
!
!            ! Add the correction
!            val = F(a,i)
!            eval(i) = eval(i) + val*val/(E-TD.delEab(ai))
!
!         end
!      end
!
!      stop
!
!   end

   subroutine make_u_CIS_SS_A_mx(self,AA,one,two)
   ! FOR TESTING AND UNDERSTANDING.
   ! Make urestricted CIS "A" matrix. It is composed of 16
   ! spin sectors, labelled by single excitations, and the
   ! two-electron part of it is given by:
   !
   !                    BJ  aa  ba  ab  bb 
   !                 AI    ________________
   !                 aa  | J-K   0   0   J
   !    AA(AI,BJ) =  ba  |  0   -K   0   0
   !                 ab  |  0    0  -K   0
   !                 bb  |  J    0   0  J-K
   !
   ! where e.g. ab are spins for excitations ai or bj, and
   ! where J = (AI|BJ) and K = (AB|IJ). The above matrix can
   ! be derived from the 2e part of the spinorbital equation
   ! below, by performing integration over spin labels.
   !
   !  AA(AI,BJ) = (E_A-E_I) d_IJ d_AB + (AI|BJ) - (AB|IJ)
   !
   ! or better with complex orbitals
   !
   !  AA(AI,BJ) = (E_A-E_I) d_IJ d_AB + (AI|JB) - (AB|JI)
   !            = (E_A-E_I) d_IJ d_AB + <AJ|IB> - <AJ|BI>
   !
   ! NOTE: The 1e part in this eqn is non-zero for spin labels
   ! aa;aa aa;bb bb;aa and bb;bb, the corners of the mx above.
   ! See Dreuw & Head-Gordon (2005) eqn (17).
   ! See Oddershede (1984) Comp Phys Rep 2 p33-94 eqn (3.6)
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: AA
      BIN, optional, IN :: one
      BIN, optional, IN :: two
      MAT4(REAL,:,:,:,:), allocatable :: v
      INT :: ai,a,i
      INT :: bj,b,j
      INT :: d,d2,d3
      INT :: no,lo,hv
      BIN :: do1,do2
      REAL :: val,aibj,abji
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:make_u_CIS_SS_A_mx ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:make_u_CIS_SS_A_mx ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:make_u_CIS_SS_A_mx ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:make_u_CIS_SS_A_mx ... MOs.r does not exist")

      ! Do one electron part?
      do1 = TRUE
      if (present(one)) do1 = one

      ! Do two electron part?
      do2 = TRUE
      if (present(two)) do2 = two

      ! Dimensions
      d = size(self%TD_data%single,1)-1  ! Remove G
      d2 = 2*d
      d3 = 3*d
      no = self%TD_data%n_occ
      lo = self%TD_data%min_OMO
      hv = self%TD_data%max_VMO+no  ! NOTE

      ! A matrix (leaky)
      AA = ZERO
      if (do1) then

         ! Diagonal Fock mx
         if (NOT allocated(self%TD_data%Fv)) then

            ! Eigenvalue difference
            do ai = 2,d
               val = self%TD_data%deltaE(ai)
               AA(ai,ai) = val  ! aa;aa
               AA(d+ai,d+ai) = val  ! ba;ba
               AA(d2+ai,d2+ai) = val  ! ab;ab
               AA(d3+ai,d3+ai) = val  ! bb;bb
            end do
         else

            ! Non-diagonal VMO part
            do ai = 2,d
               a = self%TD_data%single(ai,1)
               i = self%TD_data%single(ai,2)
               val = -self%TD_data%Eo(i)
               AA(ai,ai) = val
               AA(d+ai,d+ai) = val
               AA(d2+ai,d2+ai) = val
               AA(d3+ai,d3+ai) = val
               do bj = 2,d
                  b = self%TD_data%single(bj,1)
                  j = self%TD_data%single(bj,2)
                  if (i==j) then
                     val = self%TD_data%Fv(a,b)
                     AA(ai,bj) = AA(ai,bj)+val
                     AA(d+ai,d+bj) = AA(d+ai,d+bj)+val
                     AA(d2+ai,d2+bj) = AA(d2+ai,d2+bj)+val
                     AA(d3+ai,d3+bj) = AA(d3+ai,d3+bj)+val
                  end if
               end do
            end do
         end if
      end if
      if (do2) then

         ! Get ERIs in MO basis
         call create_(v,[lo,hv],[lo,hv],[lo,hv],[lo,hv])
         call make_MO_ERIs_v1_(self,v,lo,hv)

         ! Two-electron part
         do ai = 2,d
            a = self%TD_data%single(ai,1)+no
            i = self%TD_data%single(ai,2)
            do bj = 2,d
               b = self%TD_data%single(bj,1)+no
               j = self%TD_data%single(bj,2)
               ! J, Coulomb
               aibj = v(a,i,b,j)
               AA(ai,bj) = AA(ai,bj)+aibj  ! aa;aa
               AA(ai,d3+bj) = AA(ai,d3+bj)+aibj  ! aa;bb
               AA(d3+ai,bj) = AA(d3+ai,bj)+aibj  ! bb;aa
               AA(d3+ai,d3+bj) = AA(d3+ai,d3+bj)+aibj  ! bb;bb
               ! K, exchange
               abji = v(a,b,j,i)
               AA(ai,bj) = AA(ai,bj)-abji  ! aa;aa
               AA(d+ai,d+bj) = AA(d+ai,d+bj)-abji  ! ba;ba
               AA(d2+ai,d2+bj) = AA(d2+ai,d2+bj)-abji  ! ab;ab
               AA(d3+ai,d3+bj) = AA(d3+ai,d3+bj)-abji  ! bb;bb
            end do
         end do
         call destroy_(v)
      end if

      ! Symmetrize
      call symmetric_reflect_(AA)
   end subroutine


! Restricted

   subroutine do_r_CIS_SS_S01(self)
   ! Do a restricted CIS S=0 & S=1 calculation.
   ! This does both separately and then merges.
      type(MOLECULE_TYPE), INOUT :: self

      ! S=0 calculation
      call set_method_(self%TD_data,"r-cis-ss-s=0")
      call do_r_CIS_SS_S0_(self)

      ! S=1 calculation
      call set_method_(self%TD_data,"r-cis-ss-s=1")
      call do_r_CIS_SS_S1_(self)

      ! Reset calc type & append results
      call set_method_(self%TD_data,"r-cis-ss-s=0&1")
      call merge_S01_results_(self%TD_data)
   end subroutine

   subroutine do_r_CIS_SS_S0(self)
   ! Do a restricted CIS S=0 *singlet* calculation.
   ! Magic is hidden in the .TD_data object, which keeps
   ! Davidson iteration, and other info; also the passed
   ! AV_prod method does the efficient Davidson A.V product
   ! where "A" are the 2e integral terms.
      type(MOLECULE_TYPE), INOUT :: self

      ! Start up; set archive name
      call initialize_Davidson_(self%TD_data,self%name)

      ! Do the diagonalisation & print results
      call eigensolve_Davidson_(self,self%TD_data,do_r_CIS_SS_S0_AV_prod)

      ! Clean up
      call finalize_iteration_(self%TD_data)
   end subroutine

   NON_MPI_PURE subroutine do_r_CIS_SS_S0_AV_prod(self,AV,V,TD)
   ! Given Davidson trial vectors "V", do the (A x V)
   ! product (efficiently in the AO basis) and place the
   ! result into "AV". *Singlet* S=0 case.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: AV
      MAT(REAL,:,:), IN :: V
      type(TD_DATA_TYPE), INOUT :: TD
   ENSURE(allocated(TD%Vao),"MOLECULE.TD:do_r_CIS_SS_S0_AV_prod ... TD_data.Vao not preallocated")
   ENSURE(allocated(TD%Aao),"MOLECULE.TD:do_r_CIS_SS_S0_AV_prod ... TD_data.Aao not preallocated")

      ! 1e part: Set AV = diag(A).V
      call make_r_diag_AV_(TD,AV,V)

      ! 2e part: accumulate AV =+ (AV)ai
      call change_r_V_to_AO_(TD,TD%Vao,V)  ! Get the DMs
      call r_CIS_S0_AV_(self,TD%Aao,TD%Vao)  ! AV in AO basis
      call change_r_V_to_MO_(TD,AV,TD%Aao)  ! Accumulate
   end subroutine

   subroutine do_r_CIS_SS_S1(self)
   ! Do a restricted CIS S=1 *singlet* calculation.
   ! Magic is hidden in the .TD_data object, which keeps
   ! Davidson iteration, and other info; also and the passed
   ! AV_prod method does the efficient Davidson A.V product.
      type(MOLECULE_TYPE), INOUT :: self

      ! Start up; set archive name
      call initialize_Davidson_(self%TD_data,self%name)

      ! Do the diagonalisation & print results
      call eigensolve_Davidson_(self,self%TD_data,do_r_CIS_SS_S1_AV_prod)

      ! Clean up
      call finalize_iteration_(self%TD_data)
   end subroutine

   NON_MPI_PURE subroutine do_r_CIS_SS_S1_AV_prod(self,AV,V,TD)
   ! Given Davidson trial vectors "V", do the (A x V)
   ! product (efficiently in the AO basis) and place the
   ! result into "AV". *Triplet* S=1 case.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: AV
      MAT(REAL,:,:), IN :: V
      type(TD_DATA_TYPE), INOUT :: TD
   ENSURE(allocated(TD%Vao),"MOLECULE.TD:do_r_CIS_SS_S1_AV_prod ... TD_data.Vao not preallocated")
   ENSURE(allocated(TD%Aao),"MOLECULE.TD:do_r_CIS_SS_S1_AV_prod ... TD_data.Aao not preallocated")

      ! 1e part: Set AV = diag(A).V
      call make_r_diag_AV_(TD,AV,V)

      ! 2e part: accumulate AV =+ (AV)ai
      call change_r_V_to_AO_(TD,TD%Vao,V)  ! Get the DMs
      call r_CIS_S1_AV_(self,TD%Aao,TD%Vao)  ! AV in AO basis
      call change_r_V_to_MO_(TD,AV,TD%Aao)  ! Accumulate
   end subroutine


! Inefficient Davidson methods

!   do_r_CIS_SS_S0_v0
!   ! Do a restricted CIS/Tamm-Dancoff triplet calculation.
!   ! via Davidson. See Crouzeix et al (1990) Inria RR 1353.
!   ! Step numbers & symbols below mostly from p. 5 Crouzeix.
!   ! NOTE: there is a complex interaction with .TD_data
!   ! which keeps iteration and convergence information.
!   ! THIS VERSION DOES NOT MAKE THE A MATRIX
!   ! BUT DOES EXPLICITLY CALL THE AV_prod LIKE METHODS.
!      self :: INOUT
!
!      ! Main variables
!      H :: MAT{REAL}@     ! A, in trial basis "V"
!      eval :: VEC{REAL}@  ! The "n" eigenvalues
!      evec :: MAT{REAL}@  ! The "n" eigenvector columns
!
!      ! Matrix temporaries
!      V :: MAT{REAL}@     ! V(:,n) <- Davidson trial vectors
!      W :: MAT{REAL}@     ! W(:,n) <- A.V vectors, MO basis
!      T :: VEC{REAL}@     ! A new trial vector direction
!      Vao :: MAT3{REAL}@  !   V in the AO basis
!      Aao :: MAT3{REAL}@  ! A.V in the AO basis
!
!      d, n   :: INT
!      n_keep :: INT
!      f,l, i :: INT
!      maxres :: REAL
!      bot :: REAL
!
!      ! Start up
!      .TD_data.initialize_iteration  ! Leaky
!      .TD_data.set_D_table_headings
!      .TD_data.put_D_table_header
!
!      ! Sizes
!      d = .TD_data.deltaE.dim        ! "d" could nvo
!      n = .TD_data.n_evals           ! Usually n << d
!      n_keep = .TD_data.n_keep       ! No. of trial vectors to keep
!
!      ! Temporaries
!      V.create(d,n*n_keep)           ! Davidson trial vector colums "V"
!      W.create(d,n*n_keep)           ! A.V in MO basis
!      T.create(d)                    ! New directions, scaled residual
!      Vao.create(n,.n_bf,.n_bf)      ! Density matrices
!      Aao.create(n,.n_bf,.n_bf)      ! A.V in AO basis
!
!      V(:,1:n).to_unit_mx            ! Initialise "V", choose wisely
!
!      do ! Davidson loop
!
!         ! Next iteration
!         .TD_data.update_iteration
!
!         ! Collapse subspace down?
!         if (.TD_data.collapse) then
!            .TD_data.reset_subspace  ! Keep one trial set
!            V(:,1:n) = .TD_data.evec ! Restart with last one
!            H.destroy                ! Remake H
!         end
!
!         ! 1. Expensive step: extend W = A.V
!         f = .TD_data.fst_trial_vector
!         l = .TD_data.lst_trial_vector
!         ! Below does: W(:,f:l) = matmul(A,V(:,f:l))
!         .TD_data.make_r_diag_AV(W(:,f:l),V(:,f:l))   ! 1e: Set W = diag(A).V
!         .TD_data.change_r_V_to_AO(Vao     ,V(:,f:l)) ! Back transform to Vao
!               .r_CIS_S0_AV(Aao,Vao)             ! AV in AO basis
!         .TD_data.change_r_V_to_MO(W(:,f:l),Aao)      ! W = W + (AV)ai
!       ! stdout.text("W:")
!       ! stdout.put(W(:,f:l))
!       ! stop
!
!         ! 2. Expand Rayleigh matrix
!         H.expand(l,l)    ! A in V basis
!         evec.create(l,l)
!         eval.create(l)
!         H(f:l,:) = matmul( transpose(V(:,f:l)) , W(:,1:l) )
!         H.symmetric_reflect(set_upper=TRUE)
!
!         ! 3. Diagonalise (increasing eigenvalues)
!         H.solve_symmetric_eigenproblem(eval(1:l),evec)
!
!         ! 4. Keep Ritz vectors "R" (lowest only)
!         .TD_data.eval = eval(1:n)
!         .TD_data.evec = matmul(V(:,1:l),evec(:,1:n))
!
!          ! 5-7. Get residual & add new directions
!          maxres = ZERO
!          do i = 1,n
!
!             ! 5. Residual T & its length
!             T = eval(i)*.TD_data.evec(:,i) & ! "R"
!               - matmul(W(:,1:l), evec(:,i) )
!             maxres = max(norm2(T),maxres)
!
!             ! 6. Precondition
!             bot = eval(i) - H(i,i)
!             if (abs(bot)<TOL(3)) bot = bot + TOL(2)
!             bot= ONE/bot
!
!             ! 7a. Set new direction T
!             V(:,l+i) = bot*T
!
!          end
!          .TD_data.set_max_res(maxres)
!
!         ! Clean up
!         eval.destroy
!         evec.destroy
!
!         ! 7. Orthonormalize vectors V ?
!         if (NOT .TD_data.converged) then
!            V(:,1:l+n).modified_Gram_Schmidt_QR
!          ! V(:,1:l+n).Householder_QR
!         end
!
!         ! Print out table & finish? !!!!!!
!         .TD_data.put_D_table_body_and_footer
!         if (.TD_data.Davidson_done)   exit
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      end
!
!      ! Clean up
!      H.destroy
!      Aao.destroy
!      Vao.destroy
!      T.destroy
!      W.destroy
!      V.destroy
!
!   end

!   do_r_CIS_SS_S0_D2
!   ! Do a restricted CIS/Tamm-Dancoff singlet calculation.
!   ! via Davidson. See Crouzeix et al (1990) Inria RR 1353.
!   ! Step numbers & symbols below mostly from p. 5 Crouzeix.
!   ! THIS VERSION MAKES THE A MATRIX
!      self :: INOUT
!
!      ! Key results
!      A :: MAT{REAL}@     ! A matrix
!      H :: MAT{REAL}@     ! A, in trial basis "V"
!      eval :: VEC{REAL}@  ! The "n" eigenvalues
!      evec :: MAT{REAL}@  ! The "n" eigenvector columns
!
!      ! Matrix temporaries
!      V :: MAT{REAL}@     ! V(:,n) <- the Davidson trial vectors
!      W :: MAT{REAL}@     ! W(:,n) <- A.V
!      T :: VEC{REAL}@     ! T      <- new ditrection
!
!    ! MOv :: MAT{REAL}@
!    ! MOo :: MAT{REAL}@
!
!      d, n   :: INT
!      n_keep :: INT
!      f,l, i :: INT
!      maxres :: REAL
!      bot :: REAL
!
!      ! Start up
!      .TD_data.initialize_iteration ! Leaky
!      .TD_data.set_D_table_headings
!      .TD_data.put_D_table_header
!
!      ! Sizes
!      d = .TD_data.deltaE.dim  ! "d" could nvo
!      n = .TD_data.n_evals     ! Usually n << d
!      n_keep = .TD_data.n_keep ! No. of trial vectors to keep
!
!      ! Temporaries
!      V.create(d,n*n_keep)  ! Davidson trial vectors "V"
!      V.to_unit_mx          ! Initialise trial evecs
!      W.create(d,n*n_keep)  ! Space for product A*V, worst case
!      T.create(d)           ! Space for new directions
!
!      ! MOs to transform
!    ! no = .TD_data.n_occ
!    ! MOv = .MOs.r(:,no+1:  )
!    ! MOo = .MOs.r(:,   1:no)
!
!      ! The A matrix
!      A.create(d,d)
!      .make_r_CIS_SS_S0_A_mx(A)
!
!      do ! Davidson loop
!
!         ! Next iteration.
!         .TD_data.update_iteration
!
!         ! Collapse subspace?
!         if (.TD_data.collapse) then
!
!            ! Keep one trial set
!            .TD_data.reset_subspace
!
!            ! Restart last Ritz vector
!            V(:,1:n) = .TD_data.evec
!          ! V(:,1:n).Householder_QR ! Needed?
!
!            ! Remake
!            H.destroy
!
!         end
!
!         ! Trial vector columns
!         f = .TD_data.fst_trial_vector
!         l = .TD_data.lst_trial_vector
!
!         ! 2. Expand Rayleigh matrix
!         H.expand(l,l)                 ! A in reduced basis
!         evec.create(l,l)
!         eval.create(l)
!         W(:,f:l) = matmul(A,V(:,f:l)) ! 1. Expensive step, extends W
!         H(f:l,:) = matmul( transpose(V(:,f:l)) , W(:,1:l) )
!         H.symmetric_reflect(set_upper=TRUE)
!
!         ! 3. Diagonalise (increasing eigenvalues)
!       ! H.diagonalize_Jacobi(eval(1:l),evec)
!         H.solve_symmetric_eigenproblem(eval(1:l),evec)
!
!         ! 4. Keep lowest Ritz vectors "R"
!         .TD_data.eval = eval(1:n)
!         .TD_data.evec = matmul(V(:,1:l),evec(:,1:n))
!         !!!!!!!!!!!!!!!!!!!!!!!!
!
!       ! stdout.flush
!       ! stdout.text("=========")
!       ! stdout.show("iter      ",.TD_data.iteration)
!       ! stdout.show("n_tr      ",.TD_data.n_trial)
!       ! stdout.show("collapse? ",.TD_data.collapse)
!       ! stdout.show("too many? ",.TD_data.reached_max_it)
!       ! stdout.show("f         ",f)
!       ! stdout.show("l         ",l)
!
!          ! 5-7. Get residual
!          ! Append new directions
!          maxres = ZERO
!          do i = 1,n
!
!             ! 5. Residual T & its length
!             T = eval(i)*.TD_data.evec(:,i) & ! "R"
!               - matmul(W(:,1:l),evec(:,i) )
!             maxres = max(norm2(T),maxres)
!
!             ! 6. Precondition
!             bot = eval(i) - H(i,i)
!             if (abs(bot)<TOL(3)) bot = bot + TOL(2)
!             bot= ONE/bot
!
!             ! 7a. Set new direction t
!             V(:,l+i) = bot*T
!
!          end
!          .TD_data.set_max_res(maxres)
!
!       ! stdout.flush
!       ! stdout.show("conv      ",maxres)
!       ! stdout.flush
!       ! stdout.text("eval:")
!       ! stdout.put(eval(1:n))
!
!         ! Clean up
!         eval.destroy
!         evec.destroy
!
!         if (NOT .TD_data.converged) then
!            ! Continue ...
!            ! 7. Orthonormalize vectors V
!            V(:,1:l+n).modified_Gram_Schmidt_QR
!          ! V(:,1:l+n).Householder_QR
!         end
!
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         .TD_data.put_D_table_body_and_footer
!         if (.TD_data.Davidson_done)   exit
!         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      end
!
!      ! Clean up
!      H.destroy
!      T.destroy
!      W.destroy
!      V.destroy
!
!   end

!   do_r_CIS_SS_S0_D1
!   ! Do a restricted CIS/Tamm-Dancoff singlet calculation.
!   ! See Weiss et al (1993) JCP 99 p1262-70.
!   ! THIS OLDER VERSION MAKES THE A MATRIX IN CORE
!   ! BUT DOES NOT USE TD_DATA iteration test methods.
!      self :: INOUT
!
!      ! Key results
!      eval :: VEC{REAL}@  ! The "n" eigenvalues
!      evec :: MAT{REAL}@  ! The "n" eigenvector columns
!      maxit  :: INT       ! Max. no. of iterations allowed
!      tolcon :: REAL      ! Max. residual vec norm, to end
!      n_keep :: INT       ! No. of trial vectors to keep
!
!      ! Matrix temporaries
!      A :: MAT{REAL}@     ! A matrix
!      R :: MAT{REAL}@     ! Ritz vectors <- becomes "evec"
!      V :: MAT{REAL}@     ! V(:,n)       <- the Davidson trial vectors
!      W :: MAT{REAL}@     ! W(:,n)       <- A.V
!      H :: MAT{REAL}@     ! A            <- in basis "V"
!      T :: VEC{REAL}@     ! New direction, T
!      rvl :: VEC{REAL}@   ! Length of the residual vectors
!
!      d,n,no :: INT
!      f,l, i :: INT
!      iter :: INT
!      n_tr :: INT
!      converged :: BIN
!      too_many  :: BIN
!      collapse  :: BIN
!      bot :: REAL
!
!      ! Sizes
!      d  = .TD_data.deltaE.dim  ! "d" could nvo
!      n  = .TD_data.n_evals     ! Usually n << d
!      no = .TD_data.n_occ
!
!      maxit  = .TD_data.max_iterations ! Max iterations
!      tolcon = .TD_data.tol_Davidson   ! Convergence
!      n_keep = .TD_data.n_keep         ! No. of trial vectors to keep
!
!      ! Best vectors
!      R.create(d,n)         ! Will be "evec"
!      eval.create(n*n_keep) ! Will shrink to "n"
!
!      ! The A matrix
!      A.create(d,d)
!      .make_r_CIS_SS_S0_A_mx(A)
!
!      ! Residue vector length
!      rvl.create(n)
!
!      ! Davidson vectors "V"
!      V.create(d,n*n_keep)  ! Need all in memory???
!      W.create(d,n*n_keep)  ! Space for product A*V, worst case
!      T.create(d)           ! Space for new directions
!
!      ! Initialize
!      V.to_unit_mx          ! Initial guess
!      iter = 0              ! Overall iteration count
!      n_tr = 0              ! No. of trial vector sets
!      converged = FALSE
!      too_many  = FALSE
!
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      .TD_data.set_D_table_headings
!      .TD_data.put_D_table_header
!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!      do
!
!         ! Iteration & no. of trial vector sets (-1)
!         iter = iter + 1
!         n_tr = n_tr + 1
!
!         ! First & last trial vector columns
!         l = n*(n_tr-1)
!         f = l + 1
!         l = l + n
!
!         ! 1. Expensive step, extends W
!         ! Replace with low-mem matrix product
!         W(:,f:l) = matmul(A,V(:,f:l))
!
!         ! 2. Expand Rayleigh matrix
!         H.expand(l,l)
!         evec.create(l,l)
!         H(f:l,:) = matmul( transpose(V(:,f:l)) , W(:,1:l) )
!         H.symmetric_reflect(set_upper=TRUE)
!
!         ! 3. Diagonalise (increasing eigenvalues)
!       ! H.diagonalize_Jacobi(eval(1:l),evec)
!         H.solve_symmetric_eigenproblem(eval(1:l),evec)
!
!         ! 4. Lowest Ritz vectors
!         R = matmul(V(:,1:l),evec(:,1:n))
!
!         ! Too many iters? Collapse space?
!         too_many = iter>maxit; if (too_many) exit
!         collapse = n_tr==n_keep
!
!         stdout.flush
!         stdout.text("=========")
!         stdout.show("iter      ",iter)
!         stdout.show("n_tr      ",n_tr)
!         stdout.show("collapse? ",collapse)
!         stdout.show("too many? ",too_many)
!         stdout.show("f         ",f)
!         stdout.show("l         ",l)
!
!         if (too_many) then
!
!            ! Save to disk?
!            evec.destroy
!            H.destroy
!
!         else if (collapse) then
!
!            ! Restart with Ritz vector
!            n_tr = 0
!            V(:,1:n) = R
!            V(:,1:n).Householder_QR ! Needed?
!
!            ! Note: clean both
!            evec.destroy
!            H.destroy
!
!         else
!
!            ! 5-7. Get residual & new direction T, per-column
!            do i = 1,n
!
!               ! 5. Residual T & its length
!               T = eval(i)*R(:,i) - matmul(W(:,1:l),evec(:,i))
!               rvl(i) = norm2(T)
!
!               ! 6. Precondition
!               bot = eval(i) - H(i,i)
!               if (abs(bot) < 0.001d0) bot = bot + TOL(2)
!               bot= ONE/bot
!
!               ! 7a. Set new direction t
!               V(:,l+i) = bot*T
!
!            end
!
!         stdout.flush
!         stdout.show("conv      ",maxval(abs(rvl)))
!         stdout.flush
!         stdout.text("eval:")
!         stdout.put(eval(1:n))
!
!            ! Finished?
!            converged = all(rvl<tolcon); if (converged) exit
!
!            ! 7. Orthonormalize vectors V
!            V(:,1:l+n).modified_Gram_Schmidt_QR
!          ! V(:,1:l+n).Householder_QR
!
!            ! Clean up
!            evec.destroy
!
!         end
!
!      end
!
!      ENSURE(converged,"No convergence")
!      ENSURE(NOT too_many,"Too many iterations")
!
!      ! Clean up
!      evec.destroy
!      H.destroy
!      T.destroy
!      W.destroy
!      V.destroy
!      rvl.destroy
!      A.destroy
!
!      ! Recover & store solution
!      eval.shrink(n)
!      call move_alloc(eval,.TD_data.eval)
!      call move_alloc(R   ,.TD_data.evec)
!
!   end

   subroutine make_r_CIS_SS_S0_A_mx(self,AA,one,two)
   ! Make the restricted CIS singlet A matrix in "AA".
   ! If "one" is FALSE then skip 1-electron part.
   ! If "two" is FALSE then skip 2-electron part.
   ! See Weiss et al (1993) JCP 99 p1262-70.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: AA
      BIN, optional, IN :: one
      BIN, optional, IN :: two
      MAT4(REAL,:,:,:,:), allocatable :: v
      INT :: a,i,ai
      INT :: b,j,bj
      INT :: lo,hv
      INT :: d,n,no
      REAL :: aibj,abji
      BIN :: do1,do2
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:make_r_CIS_SS_S0_A_mx ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:make_r_CIS_SS_S0_A_mx ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:make_r_CIS_SS_S0_A_mx ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:make_r_CIS_SS_S0_A_mx ... MOs.r does not exist")

      ! Do one electron part?
      do1 = TRUE
      if (present(one)) do1 = one

      ! Do two electron part?
      do2 = TRUE
      if (present(two)) do2 = two

      ! Dimensions
      d = size(self%TD_data%deltaE)
      n = self%TD_data%n_bf
      no = self%TD_data%n_occ
      lo = self%TD_data%min_OMO
      hv = self%TD_data%max_VMO+no  ! NOTE

      ! A matrix (leaky)
      AA = ZERO
      if (do1) then

         ! Do diagonal part
         if (NOT allocated(self%TD_data%Fv)) then

            ! Eigenvalue difference
            do ai = 1,d
               AA(ai,ai) = self%TD_data%deltaE(ai)
            end do
         else

            ! Non-diagonal VMO part
            do ai = 1,d
               a = self%TD_data%single(ai,1)
               i = self%TD_data%single(ai,2)
               AA(ai,ai) = -self%TD_data%Eo(i)
               do bj = 1,d
                  b = self%TD_data%single(bj,1)
                  j = self%TD_data%single(bj,2)
                  if (i==j) then
                     AA(ai,bj) = AA(ai,bj)+self%TD_data%Fv(a,b)
                  end if
               end do
            end do
         end if
      end if
      if (do2) then

         ! Get ERIs in MO basis
         call create_(v,[lo,hv],[lo,hv],[lo,hv],[lo,hv])
      ! .make_MO_ERI_chem_integrals(v)
      ! .make_MO_ERIs_v0(v,lo,hv)
         call make_MO_ERIs_v1_(self,v,lo,hv)
      ! .make_MO_ERIs_v2(v,lo,hv)

         ! Two-electron part
         do ai = 1,d
            a = self%TD_data%single(ai,1)+no
            i = self%TD_data%single(ai,2)
            do bj = 1,d
               b = self%TD_data%single(bj,1)+no
               j = self%TD_data%single(bj,2)
               ! Integrals
               aibj = v(a,i,b,j)
               abji = v(a,b,j,i)
               ! Weiss et al (1993) JCP 99 p1262-70: ! Singlet
               AA(ai,bj) = AA(ai,bj)+TWO*aibj-abji  ! eqn (3)+(5)/2
            end do
         end do
         call destroy_(v)
      end if

      ! Symmetrize
      call symmetric_reflect_(AA)
   end subroutine


! ===============
! In-core methods
! ===============

   subroutine TD_excited_state_solver_IC(self,AA)
   ! Diagonalise the CIS/TD matrix "A", or similar, in-core.
   ! Triplet (S=1) results are appended if requested.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: AA
      INT :: dim,n

      ! Dimensions
      dim = size(AA,1)

      ! Solve
      call create_(self%TD_data%eval,dim)
      call create_(self%TD_data%evec,dim,dim)
      call solve_symmetric_eigenproblem_(AA,self%TD_data%eval,self%TD_data%evec)

      ! Shrink to desired
      n = self%TD_data%n_evals
      call shrink_(self%TD_data%eval,n)
      call shrink_columns_(self%TD_data%evec,n)

      ! Sqrt?
      if (do_sqrt_evals_(self%TD_data)) then
         self%TD_data%eval = sqrt(self%TD_data%eval)
      end if
   end subroutine


! Singlet & triplet

   subroutine do_r_CIS_S01_IC(self)
   ! Do a restricted CIS/Tamm-Dancoff singlet (S=0)
   ! and triplet (S=1) calculation and merge and sort results
   ! by increasing excitation energy.
      type(MOLECULE_TYPE), INOUT :: self

      ! S=0 calculation
      call set_method_(self%TD_data,"r-cis-s=0")
      call do_r_CIS_S0_IC(self)

      ! S=1 calculation
      call set_method_(self%TD_data,"r-cis-s=1")
      call do_r_CIS_S1_IC(self)

      ! Reset calc type & append results
      call set_method_(self%TD_data,"r-cis-s=0&1")
      call merge_S01_results_(self%TD_data)
   end subroutine

   subroutine do_r_TDHF_S01_IC(self)
   ! Do a restricted CIS/Tamm-Dancoff singlet (S=0)
   ! and triplet (S=1) calculation and merge and sort results
   ! by increasing excitation energy.
      type(MOLECULE_TYPE), INOUT :: self

      ! S=0 calculation
      call set_method_(self%TD_data,"r-tdhf-s=0")
      call do_r_TDHF_S0_IC(self)

      ! S=1 calculation
      call set_method_(self%TD_data,"r-tdhf-s=1")
      call do_r_TDHF_S1_IC(self)

      ! Reset calc type & append results
      call set_method_(self%TD_data,"r-tdhf-s=0&1")
      call merge_S01_results_(self%TD_data)
   end subroutine


! Restricted Singlet

   subroutine do_r_CIS_S0_IC(self)
   ! Do a restricted CIS/Tamm-Dancoff singlet calculation.
   ! See Weiss et al (1993) JCP 99 p1262-70.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: AA
      INT :: nvo
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_r_CIS_S0_IC ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_r_CIS_S0_IC ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_r_CIS_S0_IC ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_r_CIS_S0_IC ... MOs.r does not exist")

      ! Dimensions
      nvo = no_of_singles_(self%TD_data)

      ! A matrix (leaky)
      call create_(AA,nvo,nvo)
      AA = ZERO

      ! Get A matrix
      call make_r_CIS_SS_S0_A_mx_(self,AA)

      ! Symmetrize
      call symmetric_reflect_(AA)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,AA)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(AA)
   end subroutine

   subroutine do_r_CIS_SS_S0_IC(self)
   ! Do a restricted CIS/Tamm-Dancoff singlet calculation.
   ! See Weiss et al (1993) JCP 99 p1262-70.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: AA
      INT :: nvo
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_r_CIS_SS_S0_IC ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_r_CIS_SS_S0_IC ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_r_CIS_SS_S0_IC ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_r_CIS_SS_S0_IC ... MOs.r does not exist")

      ! Dimensions
      nvo = size(self%TD_data%deltaE)

      ! A matrix (leaky)
      call create_(AA,nvo,nvo)
      AA = ZERO

      ! Get A matrix
      call make_r_CIS_SS_S0_A_mx_(self,AA)

      ! Symmetrize
      call symmetric_reflect_(AA)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,AA)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(AA)
   end subroutine

   subroutine do_r_TDHF_S0_IC(self)
   ! Do a restricted TDHF singlet calculation.
   ! NOTE: for singlet or triplet instabilities this may fail
   ! See Weiss et al (1993) JCP 99 p1262-70.
   ! See Hirata & Head-Gordon (1999) CPL 314, 291-299.
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: v
      MAT(REAL,:,:), allocatable :: ApB
      MAT(REAL,:,:), allocatable :: AmB
      INT :: n,nvo
      INT :: no,nv
      INT :: a,i,ai
      INT :: b,j,bj
      REAL :: Ea,Ei
      REAL :: val,aibj,ajbi,abji
   ENSURE(NOT allocated(self%TD_data%Fv),"MOLECULE.TD:do_r_TDHF_S0_IC ... VMO canonisation disallowed here")
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_r_TDHF_S0_IC ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_r_TDHF_S0_IC ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_r_TDHF_S0_IC ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_r_TDHF_S0_IC ... MOs.r does not exist")

      ! Dimensions
      n = self%TD_data%n_bf
      no = self%TD_data%n_occ
      nv = self%TD_data%n_vir
      nvo = nv*no

      ! A+B & A-B matrices (leaky)
      call create_(ApB,nvo,nvo)
      call create_(AmB,nvo,nvo)

      ! Get ERIs in MO basis
      call create_(v,n,n,n,n)
      call make_MO_ERI_chem_integrals_(self,v)
      ApB = ZERO
      AmB = ZERO
      ai = 0
      do i = 1,no
         do a = no+1,n
            ai = ai+1

         ! Eigenvalues
            Ea = self%TD_data%Ev(a-no)
            Ei = self%TD_data%Eo(i)

         ! Diagonals
            val = Ea-Ei
            AmB(ai,ai) = val
            ApB(ai,ai) = val
            bj = 0
            do j = 1,no
               do b = no+1,n
                  bj = bj+1

            ! Integrals
                  aibj = v(a,i,b,j)
                  ajbi = v(a,j,b,i)
                  abji = v(a,b,j,i)

            ! Weiss et al (1993) JCP 99 p1262-70: ! Singlet
                  ApB(ai,bj) = ApB(ai,bj)+FOUR*aibj-ajbi-abji  ! eqn (3)
                  AmB(ai,bj) = AmB(ai,bj)+ajbi-abji  ! eqn (5)
               end do
            end do
         end do
      end do
      call destroy_(v)

      ! Symmetrize
      call symmetric_reflect_(AmB)
      call symmetric_reflect_(ApB)

      ! eqn (28) in Dreuw (2005)
      call to_sqrt_(AmB)
      call change_basis_using_(ApB,AmB)
      call destroy_(AmB)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,ApB)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(ApB)
      call destroy_(AmB)
   end subroutine


! Restricted Triplet

   subroutine do_r_CIS_S1_IC(self)
   ! Do a restricted CIS/Tamm-Dancoff triplet calculation.
   ! See Weiss et al (1993) JCP 99 p1262-70.
   ! See Hirata & Head-Gordon (1999) CPL 314, 291-299.
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: v
      MAT(REAL,:,:), allocatable :: AA
      INT :: n,nvo
      INT :: no,nv
      INT :: a,i,ai
      INT :: b,j,bj
      REAL :: Ea,Ei
      REAL :: abji
   ENSURE(NOT allocated(self%TD_data%Fv),"MOLECULE.TD:do_r_CIS_S1_IC ... VMO canonisation disallowed here")
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_r_CIS_S1_IC ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_r_CIS_S1_IC ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_r_CIS_S1_IC ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_r_CIS_S1_IC ... MOs.r does not exist")

      ! Dimensions
      n = self%TD_data%n_bf
      no = self%TD_data%n_occ
      nv = self%TD_data%n_vir
      nvo = nv*no

      ! Allocate (leaky)
      call create_(AA,nvo,nvo)

      ! Get ERIs in MO basis
      call create_(v,n,n,n,n)
      call make_MO_ERI_chem_integrals_(self,v)
      AA = ZERO
      ai = 0
      do i = 1,no
         do a = no+1,n
            ai = ai+1

         ! Eigenvalues
            Ea = self%TD_data%Ev(a-no)
            Ei = self%TD_data%Eo(i)
            AA(ai,ai) = Ea-Ei
            bj = 0
            do j = 1,no
               do b = no+1,n
                  bj = bj+1

            ! Integrals
                  abji = v(a,b,j,i)

            ! Weiss et al (1993) JCP 99 p1262-70: ! Triplet
                  AA(ai,bj) = AA(ai,bj)-abji  ! eqn (4)+(5)/2
               end do
            end do
         end do
      end do
      call destroy_(v)

      ! Symmetrize
      call symmetric_reflect_(AA)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,AA)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(AA)
   end subroutine

   subroutine do_r_TDHF_S1_IC(self)
   ! Do a restricted TDHF triplet calculation.
   ! NOTE: for singlet or triplet instabilities this may fail
   ! See Weiss et al (1993) JCP 99 p1262-70.
   ! See Hirata & Head-Gordon (1999) CPL 314, 291-299.
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: v
      MAT(REAL,:,:), allocatable :: ApB
      MAT(REAL,:,:), allocatable :: AmB
      INT :: n,nvo
      INT :: no,nv
      INT :: a,i,ai
      INT :: b,j,bj
      REAL :: Ea,Ei
      REAL :: val,ajbi,abji
   ENSURE(NOT allocated(self%TD_data%Fv),"MOLECULE.TD:do_r_TDHF_S1_IC ... VMO canonisation disallowed here")
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_r_TDHF_S1_IC ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_r_TDHF_S1_IC ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_r_TDHF_S1_IC ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_r_TDHF_S1_IC ... MOs.r does not exist")

      ! Dimensions
      n = self%TD_data%n_bf
      no = self%TD_data%n_occ
      nv = self%TD_data%n_vir
      nvo = nv*no

      ! Allocate (leaky)
      call create_(ApB,nvo,nvo)
      call create_(AmB,nvo,nvo)

      ! Get ERIs in MO basis
      call create_(v,n,n,n,n)
      call make_MO_ERI_chem_integrals_(self,v)
      ApB = ZERO
      AmB = ZERO
      ai = 0
      do i = 1,no
         do a = no+1,n
            ai = ai+1
            Ea = self%TD_data%Ev(a-no)
            Ei = self%TD_data%Eo(i)
            val = Ea-Ei
            AmB(ai,ai) = val
            ApB(ai,ai) = val
            bj = 0
            do j = 1,no
               do b = no+1,n
                  bj = bj+1

            ! Integrals
                  ajbi = v(a,j,b,i)
                  abji = v(a,b,j,i)

            ! Weiss et al (1993) JCP 99 p1262-70
            ! Triplet
                  ApB(ai,bj) = ApB(ai,bj)-ajbi-abji  ! eqn (4)
                  AmB(ai,bj) = AmB(ai,bj)+ajbi-abji  ! eqn (5)
               end do
            end do
         end do
      end do
      call destroy_(v)

      ! Symmetrize
      call symmetric_reflect_(AmB)
      call symmetric_reflect_(ApB)

      ! eqn (28) in Dreuw (2005)
      call to_sqrt_(AmB)
      call change_basis_using_(ApB,AmB)
      call destroy_(AmB)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,ApB)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(ApB)
   end subroutine


! Unrestricted CIS

   subroutine do_u_CIS_IC(self)
   ! Do an urestricted CIS/Tamm-Dancoff calculation.
   ! See Weiss et al (1993) JCP 99 p1262-70.
   ! See Hirata & Head-Gordon (1999) CPL 314, 291-299.
      type(MOLECULE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), allocatable :: v  ! Eeek
      MAT(REAL,:,:), allocatable :: AA
      INT :: nvo
      INT :: n,no,nv
      INT :: a,i,ai  ! spinorbital
      INT :: b,j,bj  ! spinorbital
      INT :: ra,ri,sa,si  ! restricted
      INT :: rb,rj,sb,sj  ! restricted
      REAL :: Ea,Ei,aibj,abji
   ENSURE(NOT allocated(self%TD_data%Fv),"MOLECULE.TD:do_u_CIS_IC ... VMO canonisation disallowed here")
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_u_CIS_IC ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_u_CIS_IC ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_u_CIS_IC ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_u_CIS_IC ... MOs.r does not exist")

      ! Dimensions
      n = self%TD_data%n_bf  ! restricted
      no = self%TD_data%n_occ
      nv = self%TD_data%n_vir
      nvo = 2*nv*2*no

      ! Get ERIs in MO basis
      call create_(v,n,n,n,n)
      call make_MO_ERI_chem_integrals_(self,v)

      ! Allocate (leaky)
      call create_(AA,nvo,nvo)
      AA = ZERO

      ! Loops over spinorbitals
      ! order: alpha then beta
      ai = 0
      do i = 1,2*no
         do a = 1,2*nv
            ai = ai+1

         ! Restricted indices
            ri = i
            si = 1
            if (i>no) then
               ri = ri-no
               si = -1
            end if
            ra = a+no
            sa = 1
            if (a>nv) then
               ra = ra-nv
               sa = -1
            end if

         ! Eigenvalues & diagonals
            Ea = self%TD_data%Ev(ra-no)
            Ei = self%TD_data%Eo(ri)
            AA(ai,ai) = Ea-Ei
            bj = 0
            do j = 1,2*no
               do b = 1,2*nv
                  bj = bj+1

            ! Restricted indices
                  rj = j
                  sj = 1
                  if (j>no) then
                     rj = rj-no
                     sj = -1
                  end if
                  rb = b+no
                  sb = 1
                  if (b>nv) then
                     rb = rb-nv
                     sb = -1
                  end if

            ! Dreuw & Head-Gordon (2005) eqn (17).
            ! See also eqn (95) for the HF part.
                  if (sa==si AND sb==sj) then
                     aibj = v(ra,ri,rb,rj)
                     AA(ai,bj) = AA(ai,bj)+aibj
                  end if
                  if (sa==sb AND si==sj) then
                     abji = v(ra,rb,rj,ri)
                     AA(ai,bj) = AA(ai,bj)-abji
                  end if
               end do
            end do
         end do
      end do
      call destroy_(v)

      ! Symmetrize
      call symmetric_reflect_(AA)

   ! .put_u_CIS_A_mx(AA)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,AA)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(AA)
   end subroutine

   subroutine put_u_CIS_A_mx(self,AA)
   ! Put out the different sectors of the unrestricted A matrix.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: AA
      INT :: nvo
      INT :: no,nv
      STR(len=STR_SIZE) :: ai,bj
      INT :: a,i,b,j
      VEC(INT,:), allocatable :: sa,sb
      VEC(STR(len=STR_SIZE),2) :: s

      ! Dimensions
      no = self%TD_data%n_occ
      nv = self%TD_data%n_vir
      nvo = nv*no

      ! Indices
      call create_(sa,nvo)
      call create_(sb,nvo)

      ! Spin chars
      s(1) = "a"
      s(2) = "b"
      do i = 1,2
         do a = 1,2
            call set_u_VO_spin_ind_(self,sa,a,i)
            ai = trim(s(a))//trim(s(i))
            do j = 1,2
               do b = 1,2
                  call set_u_VO_spin_ind_(self,sb,b,j)
                  bj = trim(s(b))//trim(s(j))
                  call text_(stdout,"A "//trim(bj)//","//trim(ai))
                  call put_(stdout,AA(sb,sa))
               end do
            end do
         end do
      end do
   end subroutine

   subroutine set_u_VO_spin_ind(self,ind,as,is)
   ! Put out the different sectors of the unrestricted A matrix.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), OUT :: ind
      INT, IN :: as
      INT, IN :: is
      INT :: no,nv
      INT :: ai,a,i,k

      ! Dimensions
      no = self%TD_data%n_occ
      nv = self%TD_data%n_vir
      if (as==1 AND is==1) then
         ai = 0
         k = 0
         do i = 1,2*no
            do a = 1,2*nv
               ai = ai+1
               if (a<=nv AND i<=no) then
                  k = k+1
                  ind(k) = ai
               end if
            end do
         end do
      end if
      if (as==2 AND is==1) then
         ai = 0
         k = 0
         do i = 1,2*no
            do a = 1,2*nv
               ai = ai+1
               if (a>nv AND i<=no) then
                  k = k+1
                  ind(k) = ai
               end if
            end do
         end do
      end if
      if (as==1 AND is==2) then
         ai = 0
         k = 0
         do i = 1,2*no
            do a = 1,2*nv
               ai = ai+1
               if (a<=nv AND i>no) then
                  k = k+1
                  ind(k) = ai
               end if
            end do
         end do
      end if
      if (as==2 AND is==2) then
         ai = 0
         k = 0
         do i = 1,2*no
            do a = 1,2*nv
               ai = ai+1
               if (a>nv AND i>no) then
                  k = k+1
                  ind(k) = ai
               end if
            end do
         end do
      end if
   end subroutine

   subroutine do_u_CIS_IC_v1(self)
   ! Do an urestricted CIS calculation.
   ! This version does different order of states.
   ! See Weiss et al (1993) JCP 99 p1262-70.
   ! See Hirata & Head-Gordon (1999) CPL 314, 291-299.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: AA
      INT :: d,d4
   ENSURE(allocated(self%TD_data%Eo),"MOLECULE.TD:do_u_CIS_IC_v1 ... NO TD_data.Eo!")
   ENSURE(allocated(self%TD_data%Ev),"MOLECULE.TD:do_u_CIS_IC_v1 ... NO TD_data.Ev!")
   ENSURE(allocated(self%MOs),"MOLECULE.TD:do_u_CIS_IC_v1 ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:do_u_CIS_IC_v1 ... MOs.r does not exist")

      ! Dimensions
      d = size(self%TD_data%deltaE)-1  ! Remove G state
      d4 = 4*d+1

      ! Allocate & make
      call create_(AA,d4,d4)
      call make_u_CIS_SS_A_mx_(self,AA)

      ! Symmetrize
      call symmetric_reflect_(AA)

   ! .put_u_CIS_A_mx(AA)
   ! .put_u_CIS_SS_A_mx(AA)

      ! Diagonalize for excitations
      call TD_excited_state_solver_IC_(self,AA)

      ! Store
      call store_results_(self%TD_data,self%name)

      ! Clean up
      call destroy_(AA)
   end subroutine


!   put_u_CIS_SS_A_mx(AA) :: leaky
!   ! Put out the different sectors of the unrestricted A matrix.
!      self :: INOUT
!      AA :: MAT{REAL}, IN
!
!      a,i, fa,la :: INT
!      b,j, fb,lb :: INT
!      d,d2,d3,d4 :: INT
!      s :: VEC{STR}(2)
!
!      ! Dimensions
!      d   = .TD_data.deltaE.dim
!      d2  = 2*d
!      d3  = 3*d
!      d4  = 4*d
!
!      ! Spin chars
!      s(1) = "a"
!      s(2) = "b"
!
!      do j = 1,2
!      do b = 1,2
!
!         if (b==1 AND j==1) then
!            fb = 1
!            lb = d
!         end
!         if (b==2 AND j==1) then
!            fb = d+1
!            lb = d2
!         end
!         if (b==1 AND j==2) then
!            fb = d2+1
!            lb = d3
!         end
!         if (b==2 AND j==2) then
!            fb = d3+1
!            lb = d4
!         end
!
!         do i = 1,2
!         do a = 1,2
!
!            if (a==1 AND i==1) then
!               fa = 1
!               la = d
!            end
!            if (a==2 AND i==1) then
!               fa = d+1
!               la = d2
!            end
!            if (a==1 AND i==2) then
!               fa = d2+1
!               la = d3
!            end
!            if (a==2 AND i==2) then
!               fa = d3+1
!               la = d4
!            end
!
!            stdout.text("A "//trim(s(a))//trim(s(i))//","//trim(s(b))//trim(s(j)))
!            stdout.put(AA(fa:la,fb:lb))
!
!         end
!         end
!
!      end
!      end
!
!      stop
!
!   end

!  =========
!  Integrals
!  =========

   subroutine test_VOVO(self)
   ! Test orthogonalisation routines for Davidson diagonaliser.
      type(MOLECULE_TYPE), IN :: self
      INT :: n,no,nv,nvo
      MAT(REAL,:,:), allocatable :: vovo
      MAT(REAL,:,:), allocatable :: vvoo

      ! Dimensions
      n = self%n_bf
      no = self%n_a
      nv = n-no
      nvo = nv*no

      ! The VOVO integrals
      call create_(vovo,nvo,nvo)
      call create_(vvoo,nvo,nvo)
      call make_VOVO_VVOO_ERIs(self,vovo,vvoo)
      call destroy_(vvoo)
      call destroy_(vovo)
   end subroutine

   subroutine make_VOVO_VVOO_ERIs(self,vovo,vvoo)
   ! Return the "vovo" and "vvoo ERIs stored as a 2D matrix,
   ! with indices 12 and 34 linearized, respectively.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: vovo
      MAT(REAL,:,:), OUT :: vvoo
      MAT4(REAL,:,:,:,:), allocatable :: v
      MAT(REAL,:,:), allocatable :: W
      INT :: n,no,nv,nvo
      INT :: p,q
      INT :: a,i,ai
      INT :: b,j,bj
   ENSURE(allocated(self%MOs),"MOLECULE.TD:make_VOVO_VVOO_ERIs ... MOs do not exist")
   ENSURE(allocated(self%MOs%r),"MOLECULE.TD:make_VOVO_VVOO_ERIs ... MOs.r does not exist")

      ! Dimensions
      n = self%n_bf
      no = self%n_a
      nv = n-no
      nvo = nv*no

      ! The VOVO integrals
   ! vovo.create(nvo,nvo)

      ! Get 2e integrals
      call create_(v,n,n,n,n)
      v = ZERO
      call make_ERI_integrals_(self,v)

      ! Change first two indices
      do p = 1,n
         call change_basis_using_(v(:,:,p,p),self%MOs%r)
         do q = 1,p-1
            call change_basis_using_(v(:,:,p,q),self%MOs%r)
            v(:,:,q,p) = v(:,:,p,q)
         end do
      end do

      ! VOVO
      call create_(W,nv,no)
      ai = 0
      do i = 1,no
         do a = no+1,n
            ai = ai+1
            call change_basis_to_(v(a,i,:,:),W,self%MOs%r(:,no+1:),self%MOs%r(:,1:no))
            vovo(ai,:) = reshape(W,[nvo])
         end do
      end do
      call destroy_(W)

      ! VVOO
      ! Can join with above?
      call create_(W,no,no)
      do b = no+1,n
         do a = no+1,n
            call change_basis_to_(v(a,b,:,:),W,self%MOs%r(:,1:no),self%MOs%r(:,1:no))
            do j = 1,no
               do i = 1,no
                  ai = a-no+(i-1)*nv
                  bj = b-no+(j-1)*nv
                  vvoo(ai,bj) = W(i,j)
               end do
            end do
         end do
      end do
      call destroy_(W)

      ! Check
   ! val = huge(ONE)
   ! ai = 0
   ! do i = 1,no
   ! do a = no+1,n
   !    ai = ai + 1
   !    bj = 0
   !    do j = 1,no
   !    do b = no+1,n
   !       bj = bj + 1
   !       val = min(val,abs(vovo(ai,bj)-v(a,i,b,j)))
   !    end
   !    end
   ! end
   ! end
   ! stdout.flush
   ! stdout.show("difference = ",val)
      call destroy_(v)
   end subroutine

end module
