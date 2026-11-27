!==================================================================
!
! MOLECULE.RHO: 
! 
! Molecule, basic electron density grids, and gradients of rho.
! It also includes fuzzy atomic partitioning for the density.
! And their FTs, the atomic form factor calcvulations.
!
! Copyright (C) Dylan Jayatilaka, Daniel Grimwood, 1996-2012
! Copyright (C) Dylan Jayatilaka 2012-
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
module MOLECULE_RHO_MODULE

#  include "macros"

   include "molecule.rho.use"

   implicit none

   include "molecule.rho.int"


contains

!  ============
!  Orbital grid
!  ============

   PURE subroutine make_orbital_grid_r(self,Ogrid,Orb,pts,square)
   ! Evaluate the orbital "Ogrid" for *one* AO-basis coefficient
   ! orbital column "Orb", on a set of grid "pts". 
   ! If present and TRUE, then "square" the result.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: Ogrid
      VEC(REAL,:), IN :: Orb
      MAT(REAL,:,:), IN :: pts
      BIN, optional, IN :: square
      BIN :: sq
      type(SHELL1_TYPE) :: sh
      MAT(REAL,:,:), allocatable :: BFgrid
      INT :: n_pts,s,fs,ls,ns
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_orbital_grid_r ... no basis info")
      sq = FALSE
      if (present(square)) sq = square
      n_pts = size(pts,1)
      Ogrid = ZERO
      do s = 1,self%n_shell
         call set_shell1_from_(self,s,sh,fs,ls,ns)

         ! Basis Fn grid for shell s
         call create_(BFgrid,n_pts,ns)
         call make_grid_(sh,BFgrid,pts)
         Ogrid = Ogrid+matmul(BFgrid,Orb(fs:ls))
         call destroy_(BFgrid)
      end do
      if (sq) Ogrid = Ogrid*Ogrid
   end subroutine

!  ==================
!  Nabla orbital grid
!  ==================

   PURE subroutine make_nabla_orbital_grid_r(self,Ngrid,Ogrid,Orb,pts)
   ! Return the nabla orbital density "Ngrid", and orbital density
   ! grid "Ogrid" for *one* orbital coefficient column "Orb", on a set
   ! of points "pts".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ngrid
      VEC(REAL,:), OUT :: Ogrid
      VEC(REAL,:), IN :: Orb
      MAT(REAL,:,:), IN :: pts
      type(SHELL1_TYPE) :: sh
      MAT3(REAL,:,:,:), allocatable :: Ngrids
      MAT(REAL,:,:), allocatable :: Bgrids
      INT :: n_pts,fs,ls,ns,s
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_nabla_orbital_grid_r ... no basis info")
   ENSURE(size(Ngrid,2)==3,"MOLECULE.RHO:make_nabla_orbital_grid_r ... wrong dim2, Ngrid")
   ENSURE(size(Ngrid,1)==size(pts,1),"MOLECULE.RHO:make_nabla_orbital_grid_r ... inconsistent Ngrid and pts")
   ENSURE(size(Ogrid)==size(pts,1),"MOLECULE.RHO:make_nabla_orbital_grid_r ... inconsistent Ogrid and pts")

      ! The following code is inherited from MOLECULE.RHO
      n_pts = size(pts,1)
      Ngrid = ZERO
      Ogrid = ZERO
      do s = 1,self%n_shell
         call set_shell1_from_(self,s,sh,fs,ls,ns)

         ! Nabla and Basis fn grid for shell s
         ! Second index are shell basis fns
         call create_(Ngrids,n_pts,ns,3)
         call create_(Bgrids,n_pts,ns)
         call make_nabla_grid_(sh,Ngrids,Bgrids,pts)
         Ngrid(:,1) = Ngrid(:,1)+matmul(Ngrids(:,:,1),Orb(fs:ls))
         Ngrid(:,2) = Ngrid(:,2)+matmul(Ngrids(:,:,2),Orb(fs:ls))
         Ngrid(:,3) = Ngrid(:,3)+matmul(Ngrids(:,:,3),Orb(fs:ls))
         Ogrid(:) = Ogrid(:)+matmul(Bgrids(:,:),Orb(fs:ls))
         call destroy_(Ngrids)
         call destroy_(Bgrids)
      end do
   end subroutine


!  ======================
!  Laplacian orbital grid
!  ======================

   PURE subroutine make_laplacian_orbital_grid_r(self,L,N,O,Orb,pts)
   ! Return the Laplacian orbital density grid "L", the Nabla orbital
   ! grid "N", and the Orbital grid "O" for *one* AO-basis orbital
   ! vector "Orb" on a set of grid points "pts"
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: L
      MAT(REAL,:,:), OUT :: N
      VEC(REAL,:), OUT :: O
      VEC(REAL,:), IN :: Orb
      MAT(REAL,:,:), IN :: pts
      type(SHELL1_TYPE) :: sh
      MAT3(REAL,:,:,:), allocatable :: Lgrid
      MAT3(REAL,:,:,:), allocatable :: Ngrid
      MAT(REAL,:,:), allocatable :: Bgrid
      INT :: n_pts,fs,ls,ns,s
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_laplacian_orbital_grid_r ... no basis info")
   ENSURE(size(L,2)==3,"MOLECULE.RHO:make_laplacian_orbital_grid_r ... wrong dim2, L")
   ENSURE(size(N,2)==3,"MOLECULE.RHO:make_laplacian_orbital_grid_r ... wrong dim2, N")
   ENSURE(size(L,1)==size(pts,1),"MOLECULE.RHO:make_laplacian_orbital_grid_r ... inconsistent dim1, L and pts")
   ENSURE(size(N,1)==size(pts,1),"MOLECULE.RHO:make_laplacian_orbital_grid_r ... inconsistent dim1, N and pts")
   ENSURE(size(O,1)==size(pts,1),"MOLECULE.RHO:make_laplacian_orbital_grid_r ... inconsistent dim1, O and pts")
      n_pts = size(pts,1)
      L = ZERO
      N = ZERO
      O = ZERO
      do s = 1,self%n_shell
         call set_shell1_from_(self,s,sh,fs,ls,ns)

         ! Laplacian, Nabla, and regular Basis fn grids
         call create_(Lgrid,n_pts,ns,3)
         call create_(Ngrid,n_pts,ns,3)
         call create_(Bgrid,n_pts,ns)
         call make_laplacian_grid_(sh,Lgrid,Ngrid,Bgrid,pts)
         L(:,1) = L(:,1)+matmul(Lgrid(:,:,1),Orb(fs:ls))
         L(:,2) = L(:,2)+matmul(Lgrid(:,:,2),Orb(fs:ls))
         L(:,3) = L(:,3)+matmul(Lgrid(:,:,3),Orb(fs:ls))
         N(:,1) = N(:,1)+matmul(Ngrid(:,:,1),Orb(fs:ls))
         N(:,2) = N(:,2)+matmul(Ngrid(:,:,2),Orb(fs:ls))
         N(:,3) = N(:,3)+matmul(Ngrid(:,:,3),Orb(fs:ls))
         O(:) = O(:)+matmul(Bgrid(:,:),Orb(fs:ls))
         call destroy_(Lgrid)
         call destroy_(Ngrid)
         call destroy_(Bgrid)
      end do
   end subroutine


!  =========================
!  Electron density routines
!  =========================

   PURE subroutine make_ED_grid_at_pts(self,grid,pts)
   ! Work out the electron "grid" on "pt" using ".natural orbitals" and
   ! the ".NO_occ_nos" vector. 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      STR(len=STR_SIZE) :: relativity
      STR(len=STR_SIZE) :: NOgenre
   ENSURE(allocated(self%NOs),"MOLECULE.RHO:make_ED_grid_at_pts ... no natural orbitals")
   ENSURE(has_real_genre_(self%NOs),"MOLECULE.RHO:make_ED_grid_at_pts ... no real NOs")
      relativity = "none"
      if (allocated(self%SCF_data)) then
         relativity = self%SCF_data%relativity_kind
         if (NOT self%SCF_data%PCE_CORR) relativity = "none"
      end if
      ENSURE(relativity=="none","MOLECULE.RHO:make_ED_grid_at_pts ... relativistic densities unmaintained")
      NOgenre = genre_(self%NOs)
      select case (NOgenre)
      case ("r "); call make_ED_grid_r_(self,grid,pts)
      case ("u "); call make_ED_grid_u_(self,grid,pts)
      end select
   end subroutine

   PURE subroutine make_ED_grid_r_0(self,grid,pts)
   ! Make the "grid" for the supplied points "pts" from restricted 
   ! real density matrix (not NOs as in v1, which is deprecated).
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts

   ! .make_ED_grid_r_v1(grid,pts)
      call make_ED_grid_r_v2_(self,grid,pts)
   end subroutine

   PURE subroutine make_ED_grid_r_v2_0(self,grid,pts)
   ! Make the electron density "grid" for the supplied points "pts"
   ! from restricted real .density_mx. Elements smaller than TOL(9)
   ! are ignored.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_ED_grid_r_v2_0 ... wrong dim2, pts")
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.RHO:make_ED_grid_r_v2_0 ... no restricted density_mx")
      call make_ED_grid_r_v2_(self,grid,pts,self%density_mx%r)
   end subroutine

   PURE subroutine make_ED_grid_r_1(self,grid,pts,DM)
   ! Make the "grid" for the supplied points "pts" from restricted 
   ! real density matrix "DM".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      MAT(REAL,:,:), IN :: DM
      call make_ED_grid_r_v2_(self,grid,pts,DM)
   end subroutine

   PURE subroutine make_ED_grid_r_v2_1(self,grid,pts,DM,tol)
   ! Set "grid" to the electron density at the points "pts" for density
   ! matrix "DM", batch by batch, as the batched XC quadrature does. The
   ! points are cut into small boxes (make_box_batches); for each box, G
   ! holds the basis functions of every shell whose "tol" sphere reaches
   ! it, X = G D_s with D_s the matching block of "DM", and the density is
   ! sum_j G(:,j)*X(:,j).
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      MAT(REAL,:,:), IN :: DM
      REAL, optional, IN :: tol
      MAT(REAL,:,:), allocatable :: G,X,Ds
      VEC(REAL,:), allocatable :: xs,ys,zs,rho
      VEC(INT,:), allocatable :: perm,first,last,shell,first_shell,last_shell,bf
      type(SHELL1_TYPE) :: sh
      REAL :: eps,h
      INT :: max_pts,b,f,np,ns,j,q,s,fs,ls,n,off
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_ED_grid_r_v2_1 ... inconsistent dim1, grid & pts")
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_ED_grid_r_v2_1 ... wrong dim2, pt")
   ENSURE(is_square_(DM),"MOLECULE.RHO:make_ED_grid_r_v2_1 ... wrong shape, DM")
   ENSURE(size(DM,1)==self%n_bf,"MOLECULE.RHO:make_ED_grid_r_v2_1 ... wrong size, DM")

      ! Same as .becke_grid.basis_fn_cutoff
      eps = SHELL_BASIS_FN_CUTOFF
      if (present(tol)) eps = tol

      ! Batch sizes as for the XC quadrature
      h = THREE
      max_pts = 512
      if (allocated(self%becke_grid)) then
         h = self%becke_grid%XC_batch_box_edge
         max_pts = self%becke_grid%XC_batch_max_pts
      end if
      grid = ZERO
      if (size(pts,1)==0) return
      call make_box_batches_(self,pts,perm,first,last,h,max_pts)
      call make_batch_shells_(self,pts,eps,shell,first_shell,last_shell,perm,first,last)
      do b = 1,size(first)
         if (last_shell(b)<first_shell(b)) cycle
         f = first(b)
         np = last(b)-f+1

         ! Number of basis functions in the batch's shells
         ns = 0
         do j = first_shell(b),last_shell(b)
            s = shell(j)
            ns = ns+self%last_basis_fn_for_shell(s)-self%first_basis_fn_for_shell(s)+1
         end do

         ! The batch's points
         call create_(xs,np)
         call create_(ys,np)
         call create_(zs,np)
         do q = 1,np
            xs(q) = pts(perm(f+q-1),1)
            ys(q) = pts(perm(f+q-1),2)
            zs(q) = pts(perm(f+q-1),3)
         end do

         ! Basis functions on the points, shell by shell
         call create_(bf,ns)
         call create_(G,np,ns)
         off = 0
         do j = first_shell(b),last_shell(b)
            call set_shell1_from_(self,shell(j),sh,fs,ls,n)
            call make_grid_(sh,G(:,off+1:off+n),xs,ys,zs)
            do q = 1,n
               bf(off+q) = fs+q-1
            end do
            off = off+n
         end do
         call destroy_(zs)
         call destroy_(ys)
         call destroy_(xs)

         ! Density: sum_j G(:,j) (G D_s)(:,j)
         call create_(Ds,ns,ns)
         Ds = DM(bf,bf)
         call create_(X,np,ns)
         call to_product_of_BLAS_(X,G,Ds)
         call create_(rho,np)
         rho = ZERO
         do j = 1,ns
            rho = rho+G(:,j)*X(:,j)
         end do
         do q = 1,np
            grid(perm(f+q-1)) = rho(q)
         end do
         call destroy_(rho)
         call destroy_(X)
         call destroy_(Ds)
         call destroy_(G)
         call destroy_(bf)
      end do
      call destroy_(last_shell)
      call destroy_(first_shell)
      call destroy_(shell)
      call destroy_(last)
      call destroy_(first)
      call destroy_(perm)
   end subroutine


! Should be deprecated? Version without NOs is better?

   NON_MPI_PURE subroutine make_ED_grid_r_v1_0(self,grid,pts)
   ! Make the "grid" for the supplied points "pts" from
   ! restricted real natural orbitals
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: NO
      REAL :: occ
      INT :: n_occ,n
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_ED_grid_r_v1_0 ... wrong dim2, pts")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.RHO:make_ED_grid_r_v1_0 ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.RHO:make_ED_grid_r_v1_0 ... no occupation numbers")
      n_occ = no_of_occupied_NOs_(self)
      call create_(NO,size(pts,1))
      grid = ZERO
      do n = PARALLEL_DO_START(1,1),n_occ,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_ED_grid_r_v1_0")
         call make_orbital_grid_r_(self,NO,self%NOs%r(:,n),pts)
         occ = self%NO_occ_nos%r(n)
         grid = grid+occ*NO*NO
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_ED_grid_r_v1_0")
      PARALLEL_SUM(grid)
      call destroy_(NO)
   end subroutine

   PURE subroutine make_ED_grid_r_v1_1(self,grid,pts,NO,occ)
   ! Make the electron density "grid" for the supplied points "pts"
   ! from the natural orbitals "NO" and occupuation numbers "occ".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      MAT(REAL,:,:), IN :: NO
      VEC(REAL,:), IN :: occ
      VEC(REAL,:), allocatable :: NOn
      INT :: n
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_ED_grid_r_v1_1 ... wrong dimension for points array")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_ED_grid_r_v1_1 ... inconsistent number of points")
      call create_(NOn,size(pts,1))
      grid = ZERO
      do n = 1,size(NO,2)
         if (occ(n)==ZERO) cycle
         call make_orbital_grid_r_(self,NOn,NO(:,n),pts)
         grid = grid+occ(n)*NOn*NOn
      end do
      call destroy_(NOn)
   end subroutine

   PURE subroutine make_ED_grid_u(self,grid,pts,sgn)
   ! Make the electron density  "grid" for the supplied points "pt"
   ! from unrestricted real natural orbitals. If present "sgn" is used
   ! to determine whether the total density or spin density is to be
   ! calculated: sgn=1 means sum the alpha and beta densities to form
   ! the total density, sgn=-1 means to subtract, forming the Sz spin
   ! density.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      INT, optional, IN :: sgn
      VEC(REAL,:), allocatable :: NO
      REAL :: occ,fac
      INT :: na,nb,n
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_ED_grid_u ... wrong dimension for points array")
   ENSURE(is_allocated_with_genre_(self%NOs,"u "),"MOLECULE.RHO:make_ED_grid_u ... no unrestricted NO's")
   ENSURE(allocated(self%NO_occ_nos%a),"MOLECULE.RHO:make_ED_grid_u ... no alpha occupation numbers")
   ENSURE(allocated(self%NO_occ_nos%b),"MOLECULE.RHO:make_ED_grid_u ... no beta occupation numbers")
      fac = ONE
      if (present(sgn)) fac = REALIFY(sgn)
      na = no_of_occupied_NOs_(self,"alpha")
      nb = no_of_occupied_NOs_(self,"beta")
      call create_(NO,size(pts,1))
      grid = ZERO
      do n = 1,na
         call make_orbital_grid_r_(self,NO,self%NOs%a(:,n),pts)
         occ = self%NO_occ_nos%a(n)
         grid = grid+occ*NO*NO
      end do
      do n = 1,nb
         call make_orbital_grid_r_(self,NO,self%NOs%b(:,n),pts)
         occ = self%NO_occ_nos%b(n)
         grid = grid+fac*occ*NO*NO
      end do

      ! Clean
      call destroy_(NO)
   end subroutine

   subroutine integrate_ED_numerically(self)
   ! Integrate the density numerically
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: wt
      VEC(REAL,:), allocatable :: rho
      INT :: n_pt
      REAL :: n_e
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:integrate_ED_numerically ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:integrate_ED_numerically ... no finalized becke_grid")

   ! Z    :: MAT{REAL}@
   ! i,j  :: INT
   ! V_ee :: REAL
      call head_(stdout,"Numerically integrate the electron density")

      ! Make becke grid (leaky)
      call put_(self%becke_grid)
      call put_atom_info_(self%becke_grid)
      call make_grid_(self%becke_grid,pt,wt)
      n_pt = size(pt,1)
      call create_(rho,n_pt)
      call make_ED_grid_at_pts_(self,rho,pt)
      n_e = sum(rho*wt)
      call show_(stdout,"n_pt =",n_pt)
      call show_(stdout,"n_e  =",n_e)

   ! stdout.text("wt:")
   ! do i = 1,wt.dim
   !     write(*,"(e20.12)") wt(i)
   ! end

   ! stdout.text("pt:")
   ! stdout.put(pt)
   ! do i = 1,pt.dim1
   !     write(*,"(3e20.12)") (pt(i,j), j=1,3)
   ! end
      call destroy_(rho)
      call destroy_(wt)
      call destroy_(pt)

      ! Now do the int rho_2() r_12^{-1} dr2
   ! Z.create(.n_bf,.n_bf)
   ! pt.create(1,3)
   ! i = 1
   ! pt(1,:) = .atom(i).position
   ! .make_nuclear_attraction_mx(Z,[-ONE],pt)
   ! V_ee = .expectation(Z)
   ! stdout.text("V_ee:")
   ! write(*,"(e20.12)") V_ee
   ! pt.destroy
   ! Z.destroy
   end subroutine


!  ==================
!  Derivative ED grid
!  ==================

   PURE subroutine make_derivative_rho_grid(self,N0,pts,D)
   ! Make density grids "N0" on points "pt" using density matrices "D"
   ! skipping as points as possible. This routine will probably have
   ! to be batched over *spatially localised* points to reduce memory
   ! requirements.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: N0
      MAT(REAL,:,:), IN :: pts
      MAT3(REAL,:,:,:), IN :: D
      VEC(type(EVEC_INT_TYPE),:), allocatable :: bf_lgval
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: bf_grid0
      VEC(REAL,3) :: ra
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(INT,:), allocatable :: overlapping_atom,lga,lgb,sp
      MAT3(REAL,:,:,:), allocatable :: DD
      type(SHELL1_TYPE) :: sha
      REAL :: Dab,fac,val,ga,gb
      INT :: atom_o,cutoff,n_pts,n_keep,bs,p,n,q,n_dens
      INT :: atom_a,fsa,lsa,sa,fa,la,na,a
      INT :: atom_b,fsb,lsb,sb,fb,lb,nb,b
   ENSURE(size(N0,1)==size(pts,1),"MOLECULE.RHO:make_derivative_rho_grid ... N0 and pt: incompatible sizes")
   ENSURE(size(N0,2)==size(D,3),"MOLECULE.RHO:make_derivative_rho_grid ... N0 and D: incompatible sizes")
   ENSURE(3==size(pts,2),"MOLECULE.RHO:make_derivative_rho_grid ... pt: wrong dim2")
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_derivative_rho_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_derivative_rho_grid ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_derivative_rho_grid ... no becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_derivative_rho_grid ... no overlapping_atoms_for_atom")

      ! No of densities
      n_dens = size(D,3)

      ! Basis function cutoff
      cutoff = floor(log10(self%becke_grid%basis_fn_cutoff))

      ! Saved basis function grid and skip list
      call create_(bf_lgval,self%n_shell)
      call create_(bf_grid0,self%n_shell)

      ! Atom "a" displaced points and skip switches
      n_pts = size(pts,1)
      call create_(xa,n_pts)
      call create_(ya,n_pts)
      call create_(za,n_pts)
      call create_(a2,n_pts)
      call create_(sp,n_pts)

      ! Create N0
      N0 = ZERO

      ! Loop atom_a
      do atom_a = 1,self%n_atom
         fsa = self%first_shell_for_atom(atom_a)
         lsa = self%last_shell_for_atom(atom_a)

         ! Make shell atom displaced points
         call make_displaced_pts_(self%atom(atom_a),xa,ya,za,a2,pts,n_pts)

         ! Atom a position
         ra = self%atom(atom_a)%position

         ! Loop overlapping atom_b
         overlapping_atom = self%overlapping_atoms_for_atom(atom_a)%element
         do atom_o = 1,size(overlapping_atom)
            atom_b = overlapping_atom(atom_o)
            if (atom_b>atom_a) cycle
            fsb = self%first_shell_for_atom(atom_b)
            lsb = min(self%last_shell_for_atom(atom_b),lsa)

            ! Loop shells on atom a
            do sa = fsa,lsa
               fa = self%first_basis_fn_for_shell(sa)
               la = self%last_basis_fn_for_shell(sa)
               na = la-fa+1

               ! Set shell for index "sa"
               bs = self%basis_shell_for_shell(sa)
               sha = self%basis_shell1pair(bs,1)%a
               sha%position = ra

               ! Make shell "sa" basis function grid and lga values
               call create_(bf_grid0(sa)%element,n_pts,na)
               call create_(lga,n_pts)
               call make_grid_(sha,bf_grid0(sa)%element,n_pts,xa,ya,za,a2,lga)

               ! Save the values
               bf_lgval(sa)%element = lga

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Get the "sb" basis function grid
                  lgb = bf_lgval(sb)%element

                  ! How many points in common?
                  n_keep = count((lga+lgb)>=cutoff)
                  if (n_keep==0) cycle

                  ! Make significant points
                  sp(1:n_keep) = pack([(n,n = 1,n_pts)],mask=(lga+lgb)>=cutoff)

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  DD = D(fa:la,fb:lb,:)
                  if (na*nb==1) then
                     ga0 = bf_grid0(sa)%element(:,1)
                     gb0 = bf_grid0(sa)%element(:,1)
                     do q = 1,n_dens
                        Dab = fac*DD(1,1,q)
                        do p = 1,n_keep
                           n = sp(p)
                           ga = ga0(n)
                           gb = gb0(n)
                           val = ga*gb*Dab
                           N0(n,q) = N0(n,q)+val
                        end do
                     end do
                  else

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_grid0(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_grid0(sb)%element(:,b)
                           do q = 1,n_dens
                              Dab = fac*DD(a,b,q)
                              do p = 1,n_keep
                                 n = sp(p)
                                 ga = ga0(n)
                                 gb = gb0(n)
                                 val = ga*gb*Dab
                                 N0(n,q) = N0(n,q)+val
                              end do
                           end do
                        end do
                     end do

                     ! Clean up
                  end if  ! if ss or not ss
               end do  ! -- loop sb
            end do  ! -- loop sa
         end do  ! -- atom_b
      end do  ! -- atom_a

      ! Clean up
      call destroy_(sp)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(bf_grid0)
      call destroy_(bf_lgval)
   end subroutine


!  ========================
!  CRYSTAL232 density, fast
!  ========================

   subroutine make_CRYSTAL23_ED_grid_v1(self,grid,pts)
   ! Make the "grid" for the supplied points "pts" using the CRYSTAL23
   ! density marices which have been read in.
   ! This version from Kanghyun Chu, tested in  Matlab for NH3
   ! 7/12/23
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      MAT(type(EVEC_REAL_TYPE),:,:), allocatable :: psi
      MAT(type(EVEC_INT_TYPE),:,:), allocatable :: ind
      MAT(REAL,:,:), allocatable :: bf
      MAT(INT,:,:), allocatable :: l_ind
      VEC(REAL,:), allocatable :: PSIPSI
      VEC(INT,:), allocatable :: common,posA,posB
      VEC(BIN,:), allocatable :: mask
      MAT(REAL,3,3) :: cm
      VEC(REAL,3) :: mt
      VEC(INT,3) :: mv,lv,rv,dv
      INT :: n_pts,n_g,s,fs,ls,ns
      INT :: n,l,r,m,i,j,l1,l2
      REAL :: tol
      type(SHELL1_TYPE) :: sh
   ENSURE(allocated(self%crystal23),"MOLECULE.RHO:make_CRYSTAL23_ED_grid_v1 ... Idiot! No crystal23 info read in!")
   ENSURE(allocated(self%crystal23%density_mx),"MOLECULE.RHO:make_CRYSTAL23_ED_grid_v1 ... no crystal23 density_mx info")

      ! Strange p basis function ordering
      call set_CRYSTAL23_ordering_(TRUE)

      ! Array sizes
      n_g = self%crystal23%n_g
      n_pts = size(pts,1)
      tol = exp(-20.00)

      ! Unit cell matrix
      cm = self%crystal23%Primitive_unit_cell%direct_mx  ! This is the unit cell written in the XML file

      ! <<Kang's memo>>
      ! The "psi" and "ind" objects should have 2+1 indices.
      ! Former "2" dimensions have fixed sized: 
      !            (# of basis) X (# of cells)
      ! Last   "1" dimension has varying size:
      !        (of # of grid points where Basis_Func(r) > tolerance) 
      ! Sparse storage of significant b.f. values on grid 
      ! in "psi" and corresponding indices in "ind".
      call create_(psi,n_g,self%n_bf)
      call create_(ind,n_g,self%n_bf)
      call create_(mask,n_pts)
      do m = 1,n_g

         ! Left UC translational vector
         mv = self%crystal23%g_latt(:,m)  ! m-th IVDL 
         mt = matmul(cm,mv)  ! This is m-th CVDL
         do s = 1,self%n_shell  ! s: for every shell
            call set_shell1_from_(self,s,sh,fs,ls,ns)
            sh%position = sh%position+mt  ! either +mt or -mt
            call create_(bf,n_pts,ns)  ! bf:2D array: n_pt X ns
            call make_grid_s_(sh,bf,pts(:,1),pts(:,2),pts(:,3))
            do i = fs,ls
               mask = abs(bf(:,i-fs+1))>tol
               psi(m,i)%element = pack(bf(:,i-fs+1),mask)
               ind(m,i)%element = pack([(j,j = 1,n_pts)],mask)
            end do
            call destroy_(bf)
         end do
      end do
      call destroy_(mask)

   ! stdout.text("pt:")
   ! stdout.put(pts)

      ! Values on grid & psi*psi product
      grid = ZERO
      call create_(PSIPSI,n_pts)

      ! m-cell loop
      do m = 1,n_g
         mv = self%crystal23%g_latt(:,m)
         mt = matmul(cm,mv)  ! This is m-th CVDL

         ! List of allowed pairs
         call create_(l_ind,n_g,2)
         n = 0
         do l = 1,n_g
            lv = self%crystal23%g_latt(:,l)
            dv = lv+mv
            do r = 1,n_g
               rv = self%crystal23%g_latt(:,r)
               if (same_as_(dv,rv)) then
                  n = n+1
                  l_ind(n,1) = l
                  l_ind(n,2) = r
               end if
            end do
         end do
         call shrink_(l_ind,n,2)
         do i = 1,self%n_bf  ! psi(i, cell1)
            do j = 1,self%n_bf  ! psi(j, cell2)
               if (abs(self%crystal23%density_mx(i,j,m))<tol) cycle
               PSIPSI = ZERO
            ! dim: (# Grid pointd)
            ! psi(i,m) , i:1..n_ba; m:1..n_g
            ! ind(i,m)
               do l = 1,size(l_ind,1)
                  l1 = l_ind(l,1)
                  l2 = l_ind(l,2)
                  call common_indices_in_(ind(l1,i)%element(:),ind(l2,j)%element(:),posA,posB,common)
                  if (size(common)>0) then

                  ! Product for both basis functions being large
                     PSIPSI(common) = PSIPSI(common)+psi(l1,i)%element(posA)*psi(l2,j)%element(posB)
                  end if
                  call destroy_(posA)
                  call destroy_(posB)
                  call destroy_(common)
               end do

            ! Add up signinifcant parts
               grid = grid+self%crystal23%density_mx(i,j,m)*PSIPSI
            end do
         end do
      end do

   ! stdout.text("grid:")
   ! stdout.put(grid,by_column=TRUE)

      ! Clean
      call destroy_(PSIPSI)
   end subroutine


!   make_CRYSTAL23_ED_grid(grid,pt)
!   ! Make the "grid" for the supplied points "pt" using the CRYSTAL23
!   ! density marices which have been read in.
!      self :: IN
!      grid :: VEC{REAL}, OUT
!      pt   :: MAT{REAL}, IN
!
!   ENSURE(.crystal23.allocated , "no crystal23 info")
!   ENSURE(.crystal23.density_mx.allocated , "no crystal23 density_mx info")
!
!      cm :: MAT{REAL}(3,3)
!      lt,rt    :: VEC{REAL}(3)
!      lv,rv,dv :: VEC{INT}(3)
!      l, r, d :: INT
!
!      GAUSSIAN_DATA:set_CRYSTAL23_ordering(TRUE)
!
!      ! Unit cell matrix
!      cm = .crystal23.Primitive_unit_cell.direct_mx ! Kang update 2024.Jun.11
!           ! Kang's note
!           !   crystal23's unit cell is always primitive cell
!           !   crystal  's unit cell is crystallogrphic conventional cell   
!
!      ! Values on grid
!      grid = ZERO
!
!      ! Left b.f. unit cell "l"
!      ! This is translated back to the first unit cell
!      do l = .crystal23.n_g,1,-1
!
!         ! Left UC translational vector
!         lv = .crystal23.g_latt(:,l) ! l-th IVDL
!         lt = matmul(cm,lv)          ! This is l-th CVDL
!
!         ! Diagonal contribution from DM^{l l}
!         .add_ED_grid(grid, pt, .crystal23.density_mx(:,:,1), lshft=lt , rshft=lt )
!
!         ! Right b.f. unit cell
!         do r = .crystal23.n_g,1,-1 ! Check from here! off-diagonal
!                                    ! density matrix contributions xxxxxxxxxx 
!
!            ! Done diagonal case, above?
!            if (r==l) cycle 
!
!            ! Right UC translational vector
!            rv = .crystal23.g_latt(:,r)
!            rt = matmul(cm,rv)         ! This is g-th CVDL
!
!            ! Difference between left & right cell vectors
!            ! "d" depends on how DM^{l r}_{a b} is defined when it
!            ! mutiplies the basis functions.
!            dv = rv - lv 
!            d  = .crystal23.g_latt.index_for_column(dv)
!
!            ! Do we have DM^d ?
!            if (d==0) cycle
!
!            ! Add contribution from shifted central unit cell
!            ! DYLAN: I think the P's are not read in correct?
!            .add_ED_grid(grid,pt,.crystal23.density_mx(:,:,d),lshft=lt,rshft=rt)
!
!         end
!
!      end
!
!   end 

!  ==================
!  Nabla density grid
!  ==================

   PURE subroutine make_nabla_ED_grid_0(self,Ngrid,pts)
   ! Return nabla of the electron density in "Ngrid" on points "pts".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ngrid
      MAT(REAL,:,:), IN :: pts
      if (trim(number_kind_(self%NOs))=="real") then
         call make_nabla_ED_grid_r_(self,Ngrid,pts)
   ! else
   !    DIE("not implemented")
      end if
   end subroutine

   PURE subroutine make_nabla_ED_grid_r_0(self,Ngrid,pts)
   ! Return nabla of electron density in "Ngrid" for points "pts".
   ! Using ".NOs" and their occupation numbers.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ngrid
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: orb
      MAT(REAL,:,:), allocatable :: Norb
      REAL :: occ
      INT :: n_pts,n_occ,n
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_nabla_ED_grid_r_0 ... wrong dim2, pts")
   ENSURE(size(Ngrid,2)==3,"MOLECULE.RHO:make_nabla_ED_grid_r_0 ... wrong dim2, Ngrid")
   ENSURE(allocated(self%NOs),"MOLECULE.RHO:make_nabla_ED_grid_r_0 ... no NOss")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.RHO:make_nabla_ED_grid_r_0 ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.RHO:make_nabla_ED_grid_r_0 ... no NO occupation numbers")
   ENSURE(is_allocated_with_genre_(self%NO_occ_nos,"r "),"MOLECULE.RHO:make_nabla_ED_grid_r_0 ... no restricted NO occupation numbers")
      n_pts = size(pts,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(orb,n_pts)
      call create_(Norb,n_pts,3)
      Ngrid = ZERO
      do n = 1,n_occ

         ! Nabla grid for n-th NO 
         call make_nabla_orbital_grid_r_(self,Norb,orb,self%NOs%r(:,n),pts)
         occ = self%NO_occ_nos%r(n)
         Ngrid(:,1) = Ngrid(:,1)+occ*orb(:)*Norb(:,1)
         Ngrid(:,2) = Ngrid(:,2)+occ*orb(:)*Norb(:,2)
         Ngrid(:,3) = Ngrid(:,3)+occ*orb(:)*Norb(:,3)
      end do
      Ngrid = TWO*Ngrid

      ! Clean
      call destroy_(Norb)
      call destroy_(orb)
   end subroutine

   PURE subroutine make_nabla_ED_grid_1(self,Ngrid,Egrid,pts)
   ! Return nabla of electron density in "Ngrid" on points "pts".
   ! And also the electron density in "Egrid".
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ngrid
      VEC(REAL,:), OUT :: Egrid
      MAT(REAL,:,:), IN :: pts
      if (trim(number_kind_(self%NOs))=="real") then
         call make_nabla_ED_grid_r_(self,Ngrid,Egrid,pts)
   ! else
   !    DIE("not implemented")
      end if
   end subroutine

   PURE subroutine make_nabla_ED_grid_r_1(self,Ngrid,Egrid,pts)
   ! Return nabla of electron density in "Ngrid" on points "pts".
   ! And also the electron density in "Egrid".  Using the grid "pts"
   ! and the ".NOs" and their occupation numbers.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: Ngrid
      VEC(REAL,:), OUT :: Egrid
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: orb
      MAT(REAL,:,:), allocatable :: Norb
      REAL :: occ
      INT :: n_pts,n_occ,n
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... wrong dim2, pts")
   ENSURE(size(Ngrid,2)==3,"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... wrong dim2, Ngrid")
   ENSURE(size(Ngrid,1)==size(pts,1),"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... incosistent Ngrid and pts")
   ENSURE(size(Egrid)==size(pts,1),"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... incosistent Egrid and pts")
   ENSURE(allocated(self%NOs),"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... no natural orbitals")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... no restricted NO's")
   ENSURE(allocated(self%NO_occ_nos),"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... no occupation numbers")
   ENSURE(is_allocated_with_genre_(self%NO_occ_nos,"r "),"MOLECULE.RHO:make_nabla_ED_grid_r_1 ... no occupation numbers")
      n_pts = size(pts,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(orb,n_pts)
      call create_(Norb,n_pts,3)
      Ngrid = ZERO
      Egrid = ZERO
      do n = 1,n_occ

         ! Nabla grid for n-th NO 
         call make_nabla_orbital_grid_r_(self,Norb,orb,self%NOs%r(:,n),pts)
         occ = self%NO_occ_nos%r(n)
         Ngrid(:,1) = Ngrid(:,1)+occ*orb(:)*Norb(:,1)
         Ngrid(:,2) = Ngrid(:,2)+occ*orb(:)*Norb(:,2)
         Ngrid(:,3) = Ngrid(:,3)+occ*orb(:)*Norb(:,3)
         Egrid(:) = Egrid(:)+occ*orb(:)*orb(:)
      end do
      Ngrid = TWO*Ngrid
      call destroy_(Norb)
      call destroy_(orb)
   end subroutine


!   make_nabla_density_grid_fdm_r(nabla_grid,pts)
!   ! Work out the nabla density on ".plot_grid" using ".natural orbitals" and the
!   ! ".occupation" number vector.
!   ! This method uses the finite difference method to calculate the gradient
!   ! from the density.  It is much slower, but useful for checking.
!     nabla_grid, pts :: MAT{REAL}
!   ENSURE(pts.dim2==3,  "wrong dimension for points array")
!   ENSURE(nabla_grid.dim2==3,  "wrong dimension for nabla_grid array")
!   ENSURE(.NOs.is_allocated_with_genre("r "), "no restricted NO's")
!   ENSURE(.NO_occ_nos.r.allocated, "no occupation #'s")
!     orb :: VEC{REAL}*
!     dens_1,dens_2,nabla_orb,comp :: VEC{REAL}*
!     pt1 :: MAT{REAL}*
!     alpha :: REAL
!     n_occ,n,n_pts,i :: INT
!     alpha = TOL(6)
!     n_pts = size(pts,1)
!     pt1.create(n_pts,3)
!     nabla_grid = ZERO
!     orb.create(n_pts)
!     dens_1.create(n_pts)
!     dens_2.create(n_pts)
!     nabla_orb.create(n_pts)
!     n_occ = .no_of_occupied_NOs
!     do n = 1,n_occ
!        do i = 1,3
!          comp => pt1(:,i)
!          pt1 = pts;    comp = comp + alpha
!          .make_orbital_grid_r(dens_1,.NOs.r(:,n),pt1,square=TRUE)
!          pt1 = pts;    comp = comp - alpha
!          .make_orbital_grid_r(dens_2,.NOs.r(:,n),pt1,square=TRUE)
!          nabla_orb = HALF/alpha * (dens_1 - dens_2)
!          .make_orbital_grid_r(orb,.NOs.r(:,n), pts)
!          nabla_grid(:,i) = nabla_grid(:,i) + .NO_occ_nos.r(n)*orb(:)*nabla_orb
!        end
!     end
!     nabla_grid = TWO*nabla_grid
!     nabla_orb.destroy
!     dens_2.destroy
!     dens_1.destroy
!     orb.destroy
!     pt1.destroy
!   end

! ==========================
! Laplacian density routines
! ==========================

   NON_MPI_PURE subroutine make_laplacian_ED_grid_r(self,Lgrid,pts)
   ! Work out the electron density laplacian "Lgrid" on points "pts"
   ! using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: Lgrid
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: O
      MAT(REAL,:,:), allocatable :: N
      MAT(REAL,:,:), allocatable :: L
      INT :: n_pts,n_occ,i
      REAL :: occ
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_laplacian_ED_grid_r ... wrong dim2, pts")
   ENSURE(is_allocated_with_genre_(self%NOs,"r "),"MOLECULE.RHO:make_laplacian_ED_grid_r ... no restricted real NO's")
   ENSURE(allocated(self%NO_occ_nos%r),"MOLECULE.RHO:make_laplacian_ED_grid_r ... no rean NO occupation #'s")
      n_pts = size(pts,1)
      n_occ = no_of_occupied_NOs_(self)
      call create_(O,n_pts)
      call create_(N,n_pts,3)
      call create_(L,n_pts,3)
      Lgrid = ZERO
      do i = PARALLEL_DO_START(1,1),n_occ,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_laplacian_ED_grid_r")
         call make_laplacian_orbital_grid_r_(self,L,N,O,self%NOs%r(:,i),pts)
         occ = self%NO_occ_nos%r(i)
         Lgrid = Lgrid+occ*(O(:)*(L(:,1)+L(:,2)+L(:,3))+N(:,1)*N(:,1)+N(:,2)*N(:,2)+N(:,3)*N(:,3))
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_laplacian_ED_grid_r")
      PARALLEL_SUM(Lgrid)
      Lgrid = TWO*Lgrid

      ! Clean
      call destroy_(L)
      call destroy_(N)
      call destroy_(O)
   end subroutine

   NON_MPI_PURE subroutine make_laplacian_ED_grid_u(self,Lgrid,pts)
   ! Work out the electron density laplacian "Lgrid" on points "pts"
   ! using ".natural orbitals" and  the ".NO_occ_nos" vector.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: Lgrid
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: O
      MAT(REAL,:,:), allocatable :: N
      MAT(REAL,:,:), allocatable :: L
      INT :: na,nb,n_occ,n_pts,i
      REAL :: occ
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_laplacian_ED_grid_u ... wrong dim2, pts")
   ENSURE(is_allocated_with_genre_(self%NOs,"u"),"MOLECULE.RHO:make_laplacian_ED_grid_u ... no unrestricted real NO's")
   ENSURE(is_allocated_with_genre_(self%NO_occ_nos,"u"),"MOLECULE.RHO:make_laplacian_ED_grid_u ... no unrestricted occupation numbers")
      na = no_of_occupied_NOs_(self,"alpha")
      nb = no_of_occupied_NOs_(self,"alpha")
      n_occ = max(na,nb)
      n_pts = size(pts,1)
      call create_(O,n_pts)
      call create_(N,n_pts,3)
      call create_(L,n_pts,3)
      Lgrid = ZERO
      do i = PARALLEL_DO_START(1,1),n_occ,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_laplacian_ED_grid_u")

         ! Alpha
         call make_laplacian_orbital_grid_r_(self,L,N,O,self%NOs%a(:,i),pts)
         occ = self%NO_occ_nos%a(i)
         Lgrid = Lgrid+occ*(O(:)*(L(:,1)+L(:,2)+L(:,3))+N(:,1)*N(:,1)+N(:,2)*N(:,2)+N(:,3)*N(:,3))

         ! Beta
         call make_laplacian_orbital_grid_r_(self,L,N,O,self%NOs%b(:,i),pts)
         occ = self%NO_occ_nos%b(i)
         Lgrid = Lgrid+occ*(O(:)*(L(:,1)+L(:,2)+L(:,3))+N(:,1)*N(:,1)+N(:,2)*N(:,2)+N(:,3)*N(:,3))
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_laplacian_ED_grid_u")
      PARALLEL_SUM(Lgrid)
      Lgrid = TWO*Lgrid

      ! Clean
      call destroy_(L)
      call destroy_(N)
      call destroy_(O)
   end subroutine


!  ===================
!  Becke density grids
!  ===================

! For DFT

   PURE subroutine set_up_becke_grid(self)
   ! Set up the Becke grids and overlapping atoms
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(self%basis_info_made,"MOLECULE.RHO:set_up_becke_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:set_up_becke_grid ... no atom list")
      if (NOT allocated(self%becke_grid)) call create_(self%becke_grid)

      ! ALWAYS Set up integration grid
      ! (in case of new positions) (leaky)
      call set_atom_info_(self%becke_grid,self%atom)
      call set_grid_data_(self%becke_grid)
      call destroy_XC_grid_(self)

      ! ALWAYS make the overlapping atom lists
      call set_overlapping_atoms_for_atom_(self)
   end subroutine

   subroutine put_grid_shell_errors(self)
   ! For every atom and every radial shell of its DFT grid: the shell's
   ! contribution to the electron count and to the Dirac exchange energy
   ! integrated with the angular order the pruning scheme assigns, and the
   ! error of that and of L = 5, 11, 17, 23, 29 against L = 59 on the same
   ! shell. Becke-partitioned like the real grid. Run on a converged
   ! restricted density. A diagnostic for the adaptive-pruning work
   ! (TASKS_AND_HISTORY.md); it says which shells are under- or over-resolved.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,6) :: Ls
      type(LEBEDEV_TYPE), allocatable :: leb
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: wt,rho
      VEC(INT,:), allocatable :: overlapping_atom
      VEC(REAL,6) :: S,X
      INT :: a,i,k,n,nr,L,L_used,n_used,n_atom_pts
      REAL :: r,wr,S_used,X_used,atom_err,atom_X
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:put_grid_shell_errors ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:put_grid_shell_errors ... no finalized becke_grid")
   ENSURE(is_allocated_with_genre_(self%density_mx,"r "),"MOLECULE.RHO:put_grid_shell_errors ... needs a restricted density")
      Ls = [5,11,17,23,29,59]
      if (NOT allocated(self%overlapping_atoms_for_atom)) call set_overlapping_atoms_for_atom_(self)
      call flush_(stdout)
      call text_(stdout,"=================================================")
      call text_(stdout,"DFT grid: angular error per radial shell, by atom")
      call text_(stdout,"=================================================")
      call flush_(stdout)
      call text_(stdout,"N     = shell contribution to the electron count")
      call text_(stdout,"X     = shell contribution to the Dirac exchange energy, -(3/4)(3/pi)^(1/3) rho^(4/3)")
      call text_(stdout,"dX(L) = X at Lebedev order L minus X at L = 59; 'used' is the order the pruning scheme assigns")
      call flush_(stdout)
      do a = 1,self%n_atom
         overlapping_atom = self%overlapping_atoms_for_atom(a)%element
         nr = n_radial_pts_for_atom_(self%becke_grid,a)
         atom_err = ZERO
         atom_X = ZERO
         n_atom_pts = 0
         call flush_(stdout)
         call text_(stdout,"Atom "//trim(to_str_(a))//"  "//trim(self%atom(a)%label))
         call flush_(stdout)
         call text_(stdout,"  shell   r/bohr   L used  n_pts       N used         X used        dX used"//"     dX(5)     dX(11)     dX(17)     dX(23)     dX(29)")
         do i = 1,nr
            call radial_shell_for_atom_(self%becke_grid,a,i,r,wr)
            L_used = angular_order_for_shell_(self%becke_grid,a,i)

            ! The shell at each order, partitioned like the real grid
            do k = 1,6
               L = Ls(k)
               call destroy_(leb)
               call create_(leb)
               call set_l_(leb,L)
               n = leb%n_pts
               call create_(pt,n,3)
               call create_(wt,n)
               pt(:,1) = self%atom(a)%position(1)+r*leb%pts(:,1)
               pt(:,2) = self%atom(a)%position(2)+r*leb%pts(:,2)
               pt(:,3) = self%atom(a)%position(3)+r*leb%pts(:,3)
               wt = wr*leb%wts
               call apply_partition_(self%becke_grid,pt,wt,a,overlapping_atom)
               call create_(rho,n)
               call make_ED_grid_r_v2_(self,rho,pt)
               S(k) = sum(wt*rho)
               X(k) = -0.75d0*(THREE/PI)**THIRD*sum(wt*rho**(FOUR/THREE))
               if (L==L_used) then
                  S_used = S(k); X_used = X(k); n_used = n
               end if
               call destroy_(rho)
               call destroy_(wt)
               call destroy_(pt)
            end do
            call destroy_(leb)

            ! The used order is one of the six unless the scheme names another
            if (all(Ls/=L_used)) then
               call create_(leb)
               call set_l_(leb,L_used)
               n = leb%n_pts
               call create_(pt,n,3)
               call create_(wt,n)
               pt(:,1) = self%atom(a)%position(1)+r*leb%pts(:,1)
               pt(:,2) = self%atom(a)%position(2)+r*leb%pts(:,2)
               pt(:,3) = self%atom(a)%position(3)+r*leb%pts(:,3)
               wt = wr*leb%wts
               call apply_partition_(self%becke_grid,pt,wt,a,overlapping_atom)
               call create_(rho,n)
               call make_ED_grid_r_v2_(self,rho,pt)
               S_used = sum(wt*rho)
               X_used = -0.75d0*(THREE/PI)**THIRD*sum(wt*rho**(FOUR/THREE))
               n_used = n
               call destroy_(rho)
               call destroy_(wt)
               call destroy_(pt)
               call destroy_(leb)
            end if
            atom_err = atom_err+abs(X_used-X(6))
            atom_X = atom_X+X_used
            n_atom_pts = n_atom_pts+n_used
            call put_(stdout,i,width=7)
            call put_(stdout,r,width=9,precision=4,fmt="f")
            call put_(stdout,L_used,width=7)
            call put_(stdout,n_used,width=7)
            call put_(stdout,S_used,width=13,precision=6,fmt="f")
            call put_(stdout,X_used,width=15,precision=8,fmt="f")
            call put_(stdout,X_used-X(6),width=11,precision=2,fmt="e")
            do k = 1,5
               call put_(stdout,X(k)-X(6),width=11,precision=2,fmt="e")
            end do
            call flush_(stdout)
         end do
         call flush_(stdout)
         call show_(stdout,"  points on this atom's grid          =",n_atom_pts)
         call show_(stdout,"  X of this atom's grid               =",atom_X)
         call show_(stdout,"  sum over shells of |dX used|        =",atom_err)
      end do
   end subroutine

   subroutine put_XC_batch_statistics(self)
   ! For batches of the molecular DFT grid: the number of batches, points
   ! per batch, significant basis functions per batch, and the costs
   ! sum(n_pts*n_sig) and sum(n_pts*n_sig^2), for per-atom batches, box
   ! batches (several cube edges and split limits) and radial-by-octant
   ! batches. A diagnostic for choosing the stage E batches (TASKS_AND_HISTORY.md).
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,4) :: hs
      VEC(INT,4) :: ms
      VEC(INT,:), allocatable :: perm,first,last
      INT :: i,k,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:put_XC_batch_statistics ... no basis info")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:put_XC_batch_statistics ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:put_XC_batch_statistics ... no finalized becke_grid")
      hs = [ONE,1.5d0,TWO,THREE]
      ms = [64,128,256,512]
      if (NOT allocated(self%overlapping_atoms_for_atom)) call set_overlapping_atoms_for_atom_(self)
      if (NOT allocated(self%XC_pts)) call make_XC_grid_(self)
      n = size(self%XC_pts,1)
      call flush_(stdout)
      call text_(stdout,"===============================================")
      call text_(stdout,"DFT grid: significant basis functions per batch")
      call text_(stdout,"===============================================")
      call flush_(stdout)
      call show_(stdout,"Points in the grid       =",n)
      call show_(stdout,"Basis function cutoff    =",self%becke_grid%basis_fn_cutoff)
      call flush_(stdout)
      call text_(stdout,"  scheme   edge/bohr max_pts batches  mean_pts max_pts  mean_sig max_sig"//"   sum(n*s)   sum(n*s^2)")

      ! Per-atom batches, for reference
      call create_(perm,n)
      do i = 1,n
         perm(i) = i
      end do
      call create_(first,self%n_atom); first = self%XC_first_pt
      call create_(last,self%n_atom); last = self%XC_last_pt
      call put_XC_batch_row_(self,"atom",ZERO,0,perm,first,last)
      call destroy_(perm); call destroy_(first); call destroy_(last)
      do k = 1,4
         do i = 1,4
            call make_XC_box_batches_(self,perm,first,last,hs(i),ms(k))
            call put_XC_batch_row_(self,"box",hs(i),ms(k),perm,first,last)
            call destroy_(perm); call destroy_(first); call destroy_(last)
         end do
         call make_XC_octant_batches_(self,perm,first,last,ms(k))
         call put_XC_batch_row_(self,"octant",ZERO,ms(k),perm,first,last)
         call destroy_(perm); call destroy_(first); call destroy_(last)
      end do
   end subroutine

   subroutine put_XC_batch_row(self,scheme,h,max_pts,perm,first,last)
   ! One row of put_XC_batch_statistics.
      type(MOLECULE_TYPE), IN :: self
      STR(len=*), IN :: scheme
      REAL, IN :: h
      INT, IN :: max_pts
      VEC(INT,:), IN :: perm,first,last
      VEC(INT,:), allocatable :: np
      REAL :: mean_p,mean_s,cost1,cost2
      INT :: n_b,max_p,max_s
      n_b = size(first)
      call create_(np,n_b)
      np = last-first+1
      max_p = maxval(np)
      mean_p = sum(np)
      mean_p = mean_p/n_b
      call destroy_(np)
      call get_XC_batch_costs_(self,mean_s,max_s,cost1,cost2,perm,first,last)
      call put_(stdout,scheme,width=10)
      call put_(stdout,h,width=9,precision=2,fmt="f")
      call put_(stdout,max_pts,width=8)
      call put_(stdout,n_b,width=8)
      call put_(stdout,mean_p,width=10,precision=1,fmt="f")
      call put_(stdout,max_p,width=8)
      call put_(stdout,mean_s,width=10,precision=1,fmt="f")
      call put_(stdout,max_s,width=8)
      call put_(stdout,cost1,width=11,precision=3,fmt="e")
      call put_(stdout,cost2,width=13,precision=3,fmt="e")
      call flush_(stdout)
   end subroutine

   PURE subroutine get_XC_batch_costs(self,mean_s,max_s,cost1,cost2,perm,first,last)
   ! For the batches perm(first(b):last(b)) of .XC_pts: the mean and largest
   ! number of significant basis functions "mean_s", "max_s", and the costs
   ! "cost1" = sum(n_pts*n_sig), "cost2" = sum(n_pts*n_sig^2). A shell is
   ! significant for a batch if its cutoff sphere reaches the bounding box
   ! of the batch's points, so it is kept whenever any point would keep it.
      type(MOLECULE_TYPE), IN :: self
      REAL, OUT :: mean_s,cost1,cost2
      INT, OUT :: max_s
      VEC(INT,:), IN :: perm,first,last
      VEC(INT,:), allocatable :: shell,first_shell,last_shell
      REAL :: rn,rs
      INT :: s,b,k,ns
   ENSURE(self%basis_info_made,"MOLECULE.RHO:get_XC_batch_costs ... no basis info")
   ENSURE(allocated(self%XC_pts),"MOLECULE.RHO:get_XC_batch_costs ... no XC grid")
      call make_XC_batch_shells_(self,shell,first_shell,last_shell,perm,first,last)
      mean_s = ZERO
      max_s = 0
      cost1 = ZERO
      cost2 = ZERO
      do b = 1,size(first)
         ns = 0
         do k = first_shell(b),last_shell(b)
            s = shell(k)
            ns = ns+self%last_basis_fn_for_shell(s)-self%first_basis_fn_for_shell(s)+1
         end do
         rn = last(b)-first(b)+1
         rs = ns
         mean_s = mean_s+rs
         max_s = max(max_s,ns)
         cost1 = cost1+rn*rs
         cost2 = cost2+rn*rs*rs
      end do
      mean_s = mean_s/size(first)
      call destroy_(last_shell)
      call destroy_(first_shell)
      call destroy_(shell)
   end subroutine

   PURE subroutine make_XC_batches(self)
   ! Batch the molecular DFT grid for the XC quadrature: box batches with
   ! cubes of edge .becke_grid.XC_batch_box_edge and at most
   ! .becke_grid.XC_batch_max_pts points, each with its significant shells.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: perm,first,last,shell,first_shell,last_shell
   ENSURE(allocated(self%XC_pts),"MOLECULE.RHO:make_XC_batches ... no XC grid")
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_XC_batches ... no basis info")
      call destroy_XC_batches_(self)
      call make_XC_box_batches_(self,perm,first,last,self%becke_grid%XC_batch_box_edge,self%becke_grid%XC_batch_max_pts)
      call make_XC_batch_shells_(self,shell,first_shell,last_shell,perm,first,last)
      self%XC_batch_pt = perm
      self%XC_batch_first = first
      self%XC_batch_last = last
      self%XC_batch_shell = shell
      self%XC_batch_first_shell = first_shell
      self%XC_batch_last_shell = last_shell
      call destroy_(last_shell)
      call destroy_(first_shell)
      call destroy_(shell)
      call destroy_(last)
      call destroy_(first)
      call destroy_(perm)
   end subroutine

   PURE subroutine make_XC_batch_shells(self,shell,first_shell,last_shell,perm,first,last)
   ! The significant shells of the batches of .XC_pts; see make_batch_shells.
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: shell,first_shell,last_shell
      VEC(INT,:), IN :: perm,first,last
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_XC_batch_shells ... no basis info")
   ENSURE(allocated(self%XC_pts),"MOLECULE.RHO:make_XC_batch_shells ... no XC grid")
      call make_batch_shells_(self,self%XC_pts,self%becke_grid%basis_fn_cutoff,shell,first_shell,last_shell,perm,first,last)
   end subroutine

   PURE subroutine make_batch_shells(self,pts,cutoff,shell,first_shell,last_shell,perm,first,last)
   ! For the batches perm(first(b):last(b)) of "pts", the significant
   ! shells of batch "b", shell(first_shell(b):last_shell(b)): those whose
   ! "cutoff" sphere reaches the bounding box of the batch's points,
   ! so a shell is kept whenever any point of the batch would keep it.
   ! The shells are counted first and then listed, so "shell" holds only
   ! the kept ones: a list sized batches x shells grows as N^2 in memory.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: pts
      REAL, IN :: cutoff
      VEC(INT,:), allocatable, OUT :: shell,first_shell,last_shell
      VEC(INT,:), IN :: perm,first,last
      MAT(REAL,:,:), allocatable :: cen,lo,hi
      VEC(REAL,:), allocatable :: r2m
      REAL :: dd,d2
      INT :: s,b,q,p,j,n,n_b,pass
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_batch_shells ... no basis info")
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_batch_shells ... pts: need 3 columns")
      n_b = size(first)
      call create_(cen,3,self%n_shell)
      call create_(r2m,self%n_shell)
      do s = 1,self%n_shell
         r2m(s) = r2_max_(self%basis_shell1pair(self%basis_shell_for_shell(s),1)%a,cutoff)
         cen(:,s) = self%atom(self%atom_for_shell(s))%position
      end do

      ! The bounding box of each batch
      call create_(lo,3,n_b)
      call create_(hi,3,n_b)
      lo = huge(ONE)
      hi = -huge(ONE)
      do b = 1,n_b
         do q = first(b),last(b)
            p = perm(q)
            do j = 1,3
               lo(j,b) = min(lo(j,b),pts(p,j))
               hi(j,b) = max(hi(j,b),pts(p,j))
            end do
         end do
      end do
      call create_(first_shell,n_b)
      call create_(last_shell,n_b)

      ! Pass 1 counts the kept shells; pass 2 lists them
      do pass = 1,2
         if (pass==2) call create_(shell,n)
         n = 0
         do b = 1,n_b
            first_shell(b) = n+1
            do s = 1,self%n_shell
               d2 = ZERO
               do j = 1,3
                  dd = max(ZERO,lo(j,b)-cen(j,s),cen(j,s)-hi(j,b))
                  d2 = d2+dd*dd
               end do
               if (d2>r2m(s)) cycle
               n = n+1
               if (pass==2) shell(n) = s
            end do
            last_shell(b) = n
         end do
      end do
      call destroy_(hi)
      call destroy_(lo)
      call destroy_(r2m)
      call destroy_(cen)
   end subroutine

   PURE subroutine make_XC_box_batches(self,perm,first,last,h,max_pts)
   ! Batch the molecular DFT grid .XC_pts spatially; see make_box_batches.
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: perm,first,last
      REAL, IN :: h
      INT, IN :: max_pts
   ENSURE(allocated(self%XC_pts),"MOLECULE.RHO:make_XC_box_batches ... no XC grid")
      call make_box_batches_(self,self%XC_pts,perm,first,last,h,max_pts)
   end subroutine

   PURE subroutine make_box_batches(self,pts,perm,first,last,h,max_pts)
   ! Batch the points "pts" spatially: cut their bounding box
   ! into cubes of edge "h" (bohr), then split any cube holding more than
   ! "max_pts" points into octants until none does. Batch "b" is the points
   ! perm(first(b):last(b)); "pts" itself is not reordered.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: pts
      VEC(INT,:), allocatable, OUT :: perm,first,last
      REAL, IN :: h
      INT, IN :: max_pts
      VEC(REAL,3) :: lo,hi,c0
      VEC(INT,3) :: nc
      VEC(INT,8) :: oct_cnt,oct_pos
      VEC(INT,:), allocatable :: cell,cnt,fill,codes,buf
      VEC(INT,:), allocatable :: st_f,st_l
      VEC(REAL,:), allocatable :: st_h
      MAT(REAL,:,:), allocatable :: st_c
      REAL :: hw
      INT :: n,n_cell,n_leaf,n_st,cap,p,q,j,k,f,l,ix,iy,iz,code
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_box_batches ... pts: need 3 columns")
   ENSURE(h>ZERO,"MOLECULE.RHO:make_box_batches ... cube edge must be positive")
   ENSURE(max_pts>0,"MOLECULE.RHO:make_box_batches ... max_pts must be positive")
      n = size(pts,1)
      do j = 1,3
         lo(j) = minval(pts(:,j))
         hi(j) = maxval(pts(:,j))
         nc(j) = max(1,ceiling((hi(j)-lo(j))/h))
      end do
      n_cell = nc(1)*nc(2)*nc(3)

      ! Bucket the points by cube: cube k is perm(cnt(k)+1:cnt(k+1))
      call create_(cell,n)
      call create_(cnt,n_cell+1)
      cnt = 0
      do p = 1,n
         ix = min(nc(1),1+int((pts(p,1)-lo(1))/h))
         iy = min(nc(2),1+int((pts(p,2)-lo(2))/h))
         iz = min(nc(3),1+int((pts(p,3)-lo(3))/h))
         k = ix+nc(1)*((iy-1)+nc(2)*(iz-1))
         cell(p) = k
         cnt(k+1) = cnt(k+1)+1
      end do
      do k = 2,n_cell+1
         cnt(k) = cnt(k)+cnt(k-1)
      end do
      call create_(perm,n)
      call create_(fill,n_cell)
      fill = cnt(1:n_cell)
      do p = 1,n
         k = cell(p)
         fill(k) = fill(k)+1
         perm(fill(k)) = p
      end do
      call destroy_(fill)
      call destroy_(cell)

      ! Split over-full cubes by octants, depth first
      call create_(first,n)
      call create_(last,n)
      call create_(codes,n)
      call create_(buf,n)
      cap = 512
      call create_(st_f,cap)
      call create_(st_l,cap)
      call create_(st_h,cap)
      call create_(st_c,3,cap)
      n_leaf = 0
      do k = 1,n_cell
         f = cnt(k)+1
         l = cnt(k+1)
         if (l<f) cycle
         iz = (k-1)/(nc(1)*nc(2))
         iy = (k-1-iz*nc(1)*nc(2))/nc(1)
         ix = k-1-iz*nc(1)*nc(2)-iy*nc(1)
         n_st = 1
         st_f(1) = f
         st_l(1) = l
         st_h(1) = HALF*h
         st_c(1,1) = lo(1)+(ix+HALF)*h
         st_c(2,1) = lo(2)+(iy+HALF)*h
         st_c(3,1) = lo(3)+(iz+HALF)*h
         do while (n_st>0)
            f = st_f(n_st)
            l = st_l(n_st)
            hw = st_h(n_st)
            c0 = st_c(:,n_st)
            n_st = n_st-1

            ! A batch; the size floor stops coincident points splitting forever
            if (l-f+1<=max_pts OR hw<1.0d-3) then
               n_leaf = n_leaf+1
               first(n_leaf) = f
               last(n_leaf) = l
               cycle
            end if

            ! Reorder perm(f:l) by octant about the box centre
            oct_cnt = 0
            do q = f,l
               p = perm(q)
               code = 1
               if (pts(p,1)>c0(1)) code = code+1
               if (pts(p,2)>c0(2)) code = code+2
               if (pts(p,3)>c0(3)) code = code+4
               codes(q) = code
               oct_cnt(code) = oct_cnt(code)+1
            end do
            oct_pos(1) = f-1
            do j = 2,8
               oct_pos(j) = oct_pos(j-1)+oct_cnt(j-1)
            end do
            do q = f,l
               code = codes(q)
               oct_pos(code) = oct_pos(code)+1
               buf(oct_pos(code)) = perm(q)
            end do
            perm(f:l) = buf(f:l)

            ! Push the non-empty octants
            q = f-1
            do j = 1,8
               if (oct_cnt(j)>0) then
                  ENSURE(n_st<cap,"MOLECULE.RHO:make_box_batches ... octree stack full")
                  n_st = n_st+1
                  st_f(n_st) = q+1
                  st_l(n_st) = q+oct_cnt(j)
                  st_h(n_st) = HALF*hw
                  st_c(1,n_st) = c0(1)+(2*mod(j-1,2)-1)*HALF*hw
                  st_c(2,n_st) = c0(2)+(2*mod((j-1)/2,2)-1)*HALF*hw
                  st_c(3,n_st) = c0(3)+(2*((j-1)/4)-1)*HALF*hw
               end if
               q = q+oct_cnt(j)
            end do
         end do
      end do
      call shrink_(first,n_leaf)
      call shrink_(last,n_leaf)
      call destroy_(st_c)
      call destroy_(st_h)
      call destroy_(st_l)
      call destroy_(st_f)
      call destroy_(buf)
      call destroy_(codes)
      call destroy_(cnt)
   end subroutine

   PURE subroutine make_XC_octant_batches(self,perm,first,last,max_pts)
   ! Batch the molecular DFT grid per atom, by octant about the atom and then
   ! by radius, "max_pts" points at a time. The comparison for
   ! make_XC_box_batches.
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: perm,first,last
      INT, IN :: max_pts
      VEC(REAL,:), allocatable :: key
      VEC(INT,:), allocatable :: idx,codes
      VEC(REAL,3) :: r0,d
      INT :: n,n_leaf,a,f,l,m,q,p,code,f0
   ENSURE(allocated(self%XC_pts),"MOLECULE.RHO:make_XC_octant_batches ... no XC grid")
   ENSURE(max_pts>0,"MOLECULE.RHO:make_XC_octant_batches ... max_pts must be positive")
      n = size(self%XC_pts,1)
      call create_(perm,n)
      call create_(first,n)
      call create_(last,n)
      n_leaf = 0
      do a = 1,self%n_atom
         f = self%XC_first_pt(a)
         l = self%XC_last_pt(a)
         m = l-f+1
         if (m<1) cycle
         r0 = self%atom(a)%position
         call create_(key,m)
         call create_(idx,m)
         call create_(codes,m)
         do q = 1,m
            p = f+q-1
            d = self%XC_pts(p,:)-r0
            code = 0
            if (d(1)>ZERO) code = code+1
            if (d(2)>ZERO) code = code+2
            if (d(3)>ZERO) code = code+4
            codes(q) = code
            key(q) = code
            key(q) = 1000*key(q)+min(sqrt(dot_product(d,d)),999.0d0)
         end do
         call quick_sort_(key,idx)

         ! Runs of one octant, cut every max_pts
         q = 1
         do while (q<=m)
            code = codes(idx(q))
            f0 = q
            do while (q<=m)
               if (codes(idx(q))/=code OR q-f0==max_pts) exit
               q = q+1
            end do
            n_leaf = n_leaf+1
            first(n_leaf) = f-1+f0
            last(n_leaf) = f-1+q-1
            perm(f-1+f0:f-1+q-1) = f-1+idx(f0:q-1)
         end do
         call destroy_(codes)
         call destroy_(idx)
         call destroy_(key)
      end do
      call shrink_(first,n_leaf)
      call shrink_(last,n_leaf)
   end subroutine

   PURE subroutine make_XC_grid(self)
   ! Make the DFT integration grid for the whole molecule: every atom's
   ! partitioned grid, exactly as make_partitioned_grid returns it, laid
   ! end to end in .XC_pts/.XC_wts, with atom "a" occupying rows
   ! .XC_first_pt(a) to .XC_last_pt(a), and .XC_index holding each
   ! point's row in its unique-atom grid so the cached basis functions
   ! still apply. Built once per SCF, where the atom grids and their
   ! partition weights used to be rebuilt on every iteration. Points where
   ! the promolecule density is below .becke_grid.prune_rho_cutoff are
   ! dropped, if the atomic interpolators exist. The test is on the size
   ! of the density, so that the core is never touched.
   ! Never threshold on the weight: that was the 2026-09-10 grid defect
   ! (docs/TASK_DFT_STANDARDISATION.md 6b).
      type(MOLECULE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), allocatable :: pts
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: wts
      VEC(type(EVEC_INT_TYPE),:), allocatable :: ixs
      MAT(REAL,:,:), allocatable :: pt
      VEC(REAL,:), allocatable :: wt
      MAT(REAL,:,:), allocatable :: ix
      VEC(REAL,:), allocatable :: rho
      VEC(INT,:), allocatable :: overlapping_atom
      INT :: n_tot,c,f,l,k,n,b
      REAL :: cut
      BIN :: prune
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_XC_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_XC_grid ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_XC_grid ... no overlapping_atoms")
      call destroy_XC_grid_(self)

      ! Prune by the promolecule density?
      cut = self%becke_grid%prune_rho_cutoff
      prune = cut>ZERO
      if (prune) prune = has_all_interpolators_(self%atom)
      self%XC_n_unpruned = 0
      call create_(self%XC_first_pt,self%n_atom)
      call create_(self%XC_last_pt,self%n_atom)

      ! The atom grids, one at a time
      call create_(pts,self%n_atom)
      call create_(wts,self%n_atom)
      call create_(ixs,self%n_atom)
      n_tot = 0
      do c = 1,self%n_atom
         overlapping_atom = self%overlapping_atoms_for_atom(c)%element
         call make_partitioned_grid_(self%becke_grid,pt,wt,ix,c,overlapping_atom,index=TRUE)
         self%XC_n_unpruned = self%XC_n_unpruned+size(wt)
         if (prune) then
            call create_(rho,size(wt))
            rho = ZERO
            do b = 1,size(overlapping_atom)
               call add_interpolated_density_(self%atom(overlapping_atom(b)),rho,pt)
            end do
            ! Drop a point only if the density is small in size. Near a heavy
            ! nucleus the interpolated density can be large and negative,
            ! see TASKS_AND_HISTORY.md, and those points must stay.
            k = 0
            do n = 1,size(wt)
               if (abs(rho(n))<cut) cycle
               k = k+1
               pt(k,1) = pt(n,1)
               pt(k,2) = pt(n,2)
               pt(k,3) = pt(n,3)
               wt(k) = wt(n)
               ix(k,1) = ix(n,1)
            end do
            call shrink_(pt,k,3)
            call shrink_(wt,k)
            call shrink_(ix,k,1)
            call destroy_(rho)
         end if
         self%XC_first_pt(c) = n_tot+1
         n_tot = n_tot+size(wt)
         self%XC_last_pt(c) = n_tot
         call create_(pts(c)%element,size(pt,1),3)
         call create_(wts(c)%element,size(wt))
         call create_(ixs(c)%element,size(wt))
         pts(c)%element = pt
         wts(c)%element = wt
         do k = 1,size(wt)
            ixs(c)%element(k) = nint(ix(k,1))
         end do
         call destroy_(ix)
         call destroy_(pt)
         call destroy_(wt)
      end do

      ! Lay them end to end
      call create_(self%XC_pts,n_tot,3)
      call create_(self%XC_wts,n_tot)
      call create_(self%XC_index,n_tot)
      do c = 1,self%n_atom
         f = self%XC_first_pt(c)
         l = self%XC_last_pt(c)
         self%XC_pts(f:l,:) = pts(c)%element
         self%XC_wts(f:l) = wts(c)%element
         self%XC_index(f:l) = ixs(c)%element
      end do
      call destroy_(ixs)
      call destroy_(wts)
      call destroy_(pts)
   end subroutine

   PURE subroutine get_XC_grid_for_atom_0(self,pt,wt,f0,c)
   ! Return atom "c"'s slice of the molecular DFT grid and its own basis
   ! functions "f0" on it, building the grid first if it does not exist
   ! (a property job outside an SCF).
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      MAT(REAL,:,:), allocatable, OUT :: f0
      INT, IN :: c
      INT :: f,l,n
      if (NOT allocated(self%XC_pts)) call make_XC_grid_(self)
      f = self%XC_first_pt(c)
      l = self%XC_last_pt(c)
      n = l-f+1
      call create_(pt,n,3)
      call create_(wt,n)
      pt = self%XC_pts(f:l,:)
      wt = self%XC_wts(f:l)
      call get_atom_bf_grid_(self%becke_grid,f0,c,self%XC_index(f:l))
   end subroutine

   PURE subroutine get_XC_grid_for_atom_1(self,pt,wt,f0,gx,gy,gz,c)
   ! As above, with the gradients of the basis functions too.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      MAT(REAL,:,:), allocatable, OUT :: f0
      MAT(REAL,:,:), allocatable, OUT :: gx,gy,gz
      INT, IN :: c
      INT :: f,l,n
      if (NOT allocated(self%XC_pts)) call make_XC_grid_(self)
      f = self%XC_first_pt(c)
      l = self%XC_last_pt(c)
      n = l-f+1
      call create_(pt,n,3)
      call create_(wt,n)
      pt = self%XC_pts(f:l,:)
      wt = self%XC_wts(f:l)
      call get_atom_bf_grid_(self%becke_grid,f0,gx,gy,gz,c,self%XC_index(f:l))
   end subroutine

   PURE subroutine make_rho_becke_atom_grid_0(self,den,pt,wt,bf_skip,bf_grid0,c,D)
   ! Make the density, returned in "den", on the Becke grid ("pt","wt") for
   ! atom "c" given AO density matrix "D". Also return the "bf_skip" arrays
   ! and the "bf_grid0" array used for evaluating the rho (or other
   ! density functionals) on the grid.
      type(MOLECULE_TYPE), INOUT :: self
      type(XC_DENSITY_TYPE), INOUT :: den
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      VEC(type(EVEC_BIN_TYPE),:), allocatable, OUT :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: bf_grid0
      INT, IN :: c
      MAT(REAL,:,:), IN :: D
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(REAL,3) :: ra
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipa0,skipab,skipa,skipb
      MAT(REAL,:,:), allocatable :: gridc0
      MAT(REAL,:,:), allocatable :: DD
      type(SHELL1_TYPE) :: sha
      REAL :: cutoff,Dab,fac,val,ga,gb
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,fba,ob,fsb,lsb
      INT :: sa,fa,la,na,ba,sb,fb,lb,nb,a,b,i,j,p,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_rho_becke_atom_grid_0 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_rho_becke_atom_grid_0 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_rho_becke_atom_grid_0 ... no density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_rho_becke_atom_grid_0 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_rho_becke_atom_grid_0 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_rho_becke_atom_grid_0 ... no overlapping_atoms")

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Get the Becke grid for atom "c"
      call get_XC_grid_for_atom_(self,pt,wt,gridc0,c)
      n_pt = size(pt,1)

      ! Create the density
      call create_grids_(den,n_pt,1,FALSE)

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Basis function grid and skip list
      call create_(bf_skip,self%n_shell)
      call create_(bf_grid0,self%n_shell)

      ! Atom "a" displaced points (from "c") and skip switches
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)
         fba = self%first_basis_fn_for_atom(oa)
         ra = self%atom(oa)%position

         ! Make the displaced points for atom "oa"
         call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

         ! Loop shells on atom a
         do sa = fsa,lsa
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set shell "sa"
            ba = self%basis_shell_for_shell(sa)
            sha = self%basis_shell1pair(ba,1)%a
            sha%position = ra

            ! Make shell "sa" skipa list
            call create_(skipa,n_pt)
            call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

            ! Next shell "sa" if no sig. points
            if (n_keep==0) then
               call destroy_(skipa)
               cycle
            end if

            ! Assign the "sa" skipa list
            call create_(bf_skip(sa)%element,size(skipa))
            bf_skip(sa)%element = skipa

            ! Make/copy the "sa" basis function grid
            ! This must be copied since skipa could be TRUE
            call create_(bf_grid0(sa)%element,n_keep,na)
            if (oa==c) then
               do a = 1,na
                  i = 0
                  do n = 1,n_pt
                     if (skipa(n)) cycle
                     i = i+1
                     bf_grid0(sa)%element(i,a) = gridc0(n,a+fa-fba)
                  end do
               end do
            else
               call make_skip_grid_(sha,bf_grid0(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)
            end if

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  DD = D(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = bf_grid0(sa)%element(:,1)
                     gb0 = bf_grid0(sb)%element(:,1)
                     Dab = fac*DD(1,1)
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        val = ga*gb*Dab
                        den%rho(n,1) = den%rho(n,1)+val
                     end do
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_grid0(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_grid0(sb)%element(:,b)
                           Dab = fac*DD(a,b)
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              val = ga*gb*Dab
                              den%rho(n,1) = den%rho(n,1)+val
                           end do
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(skipab); call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(gridc0)
   end subroutine

   PURE subroutine make_rho_becke_atom_grid_1(self,den,pt,wt,bf_skip,bf_gr0,bf_grx,bf_gry,bf_grz,c,D)
   ! Make the density and its gradient, returned in "den", on
   ! the Becke grid ("pt","wt") for atom "c" given AO density matrix
   ! "D". Also return the "bf_skip" array, and the basis function and
   ! basis function derivative arrays "bf_gr0", "bf_grx", "bf_gry", 
   ! "bf_grz" used for evaluating the density functionals on the grid.
      type(MOLECULE_TYPE), INOUT :: self
      type(XC_DENSITY_TYPE), INOUT :: den
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      VEC(type(EVEC_BIN_TYPE),:), allocatable, OUT :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: bf_gr0
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: bf_grx,bf_gry,bf_grz
      INT, IN :: c
      MAT(REAL,:,:), IN :: D
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,3) :: ra
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipa0,skipab,skipa
      MAT(REAL,:,:), allocatable :: gridc0,gridcx,gridcy,gridcz
      type(SHELL1_TYPE) :: sha
      REAL :: cutoff,Dab,fac,ga,gb
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,fba,offset,ob,fsb,lsb
      INT :: sa,fa,la,na,ba,sb,fb,lb,nb,a,b,i,j,p,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_rho_becke_atom_grid_1 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_rho_becke_atom_grid_1 ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_rho_becke_atom_grid_1 ... no density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_rho_becke_atom_grid_1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_rho_becke_atom_grid_1 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_rho_becke_atom_grid_1 ... no overlapping_atoms")

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Get the Becke grid for atom "c"
      call get_XC_grid_for_atom_(self,pt,wt,gridc0,gridcx,gridcy,gridcz,c)
      n_pt = size(pt,1)

      ! Create the density and its gradient
      call create_grids_(den,n_pt,1,TRUE)

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Basis function grid and skip list
      call create_(bf_skip,self%n_shell)
      call create_(bf_gr0,self%n_shell)
      call create_(bf_grx,self%n_shell)
      call create_(bf_gry,self%n_shell)
      call create_(bf_grz,self%n_shell)

      ! Atom "a" displaced points (from "c") and skip switches
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)
      call create_(pi,n_pt)
      call create_(pj,n_pt)
      call create_(pn,n_pt)

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)
         fba = self%first_basis_fn_for_atom(oa)
         ra = self%atom(oa)%position

         ! Make the displaced points for atom "oa"
         call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

         ! Loop shells on atom a
         do sa = fsa,lsa
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set shell "sa"
            ba = self%basis_shell_for_shell(sa)
            sha = self%basis_shell1pair(ba,1)%a
            sha%position = ra

            ! Make shell "sa" skipa list
            call create_(skipa,n_pt)
            call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

            ! Next shell "sa" if no sig. points
            if (n_keep==0) then
               call destroy_(skipa)
               cycle
            end if

            ! Assign the "sa" skipa list
            call create_(bf_skip(sa)%element,size(skipa))
            bf_skip(sa)%element = skipa

            ! Make/copy the "sa" basis function grid
            ! This must be copied since skipa could be TRUE
            call create_(bf_gr0(sa)%element,n_keep,na)
            call create_(bf_grx(sa)%element,n_keep,na)
            call create_(bf_gry(sa)%element,n_keep,na)
            call create_(bf_grz(sa)%element,n_keep,na)
            if (oa==c) then
               offset = fa-fba
               do a = 1,na
                  i = 0
                  do n = 1,n_pt
                     if (skipa(n)) cycle
                     i = i+1
                     bf_gr0(sa)%element(i,a) = gridc0(n,a+offset)
                     bf_grx(sa)%element(i,a) = gridcx(n,a+offset)
                     bf_gry(sa)%element(i,a) = gridcy(n,a+offset)
                     bf_grz(sa)%element(i,a) = gridcz(n,a+offset)
                  end do
               end do
            else
               call make_skip_nabla_grid_(sha,bf_grx(sa)%element,bf_gry(sa)%element,bf_grz(sa)%element,bf_gr0(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)
            end if

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR bf_skip(sb)%element
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Map common points to the "sa" and "sb" grids
                  p = 0
                  i = 0
                  j = 0
                  do n = 1,n_pt
                     if (NOT skipa(n)) i = i+1
                     if (NOT bf_skip(sb)%element(n)) j = j+1
                     if (skipab(n)) cycle
                     p = p+1
                     pi(p) = i
                     pj(p) = j
                     pn(p) = n
                  end do

                  ! Add density contribution to rho; grids read in place
                  do a = 1,na
                     do b = 1,nb
                        Dab = fac*D(fa+a-1,fb+b-1)
                        do p = 1,n_keep
                           i = pi(p)
                           j = pj(p)
                           n = pn(p)
                           ga = bf_gr0(sa)%element(i,a)
                           gb = bf_gr0(sb)%element(j,b)
                           den%rho(n,1) = den%rho(n,1)+Dab*ga*gb
                           den%grad(n,1,1) = den%grad(n,1,1)+Dab*(bf_grx(sa)%element(i,a)*gb+ga*bf_grx(sb)%element(j,b))
                           den%grad(n,2,1) = den%grad(n,2,1)+Dab*(bf_gry(sa)%element(i,a)*gb+ga*bf_gry(sb)%element(j,b))
                           den%grad(n,3,1) = den%grad(n,3,1)+Dab*(bf_grz(sa)%element(i,a)*gb+ga*bf_grz(sb)%element(j,b))
                        end do
                     end do
                  end do
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(pn)
      call destroy_(pj)
      call destroy_(pi)
      call destroy_(skipab)
      call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(gridcz)
      call destroy_(gridcy)
      call destroy_(gridcx)
      call destroy_(gridc0)
   end subroutine

   PURE subroutine make_rho_becke_atom_grid_2(self,den,pt,wt,bf_skip,bf_grid0,c,Da,Db)
   ! Make the alpha and beta densities, returned in "den", on the Becke grid
   ! ("pt","wt") for atom "c" given AO density matrices "Da", "Db".
   ! Also return the "bf_skip" arrays and the "bf_grid0" array used
   ! for evaluating the density (or density functionals) on the grid.
      type(MOLECULE_TYPE), INOUT :: self
      type(XC_DENSITY_TYPE), INOUT :: den
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      VEC(type(EVEC_BIN_TYPE),:), allocatable, OUT :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: bf_grid0
      INT, IN :: c
      MAT(REAL,:,:), IN :: Da,Db
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: ga0,gb0
      VEC(REAL,3) :: ra
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipa0,skipab,skipa,skipb
      MAT(REAL,:,:), allocatable :: gridc0
      MAT(REAL,:,:), allocatable :: DDa,DDb
      type(SHELL1_TYPE) :: sha
      REAL :: cutoff,Da_ab,Db_ab,fac,ga,gb,gab
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,fba,ob,fsb,lsb
      INT :: sa,fa,la,na,ba,sb,fb,lb,nb,a,b,i,j,p,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_rho_becke_atom_grid_2 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_rho_becke_atom_grid_2 ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_rho_becke_atom_grid_2 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_rho_becke_atom_grid_2 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_rho_becke_atom_grid_2 ... no overlapping_atoms")

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Get the Becke grid for atom "c"
      call get_XC_grid_for_atom_(self,pt,wt,gridc0,c)
      n_pt = size(pt,1)

      ! Create the alpha and beta densities
      call create_grids_(den,n_pt,2,FALSE)

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Basis function grid and skip list
      call create_(bf_skip,self%n_shell)
      call create_(bf_grid0,self%n_shell)

      ! Atom "a" displaced points (from "c") and skip switches
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)
         fba = self%first_basis_fn_for_atom(oa)
         ra = self%atom(oa)%position

         ! Make the displaced points for atom "oa"
         call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

         ! Loop shells on atom a
         do sa = fsa,lsa
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set shell "sa"
            ba = self%basis_shell_for_shell(sa)
            sha = self%basis_shell1pair(ba,1)%a
            sha%position = ra

            ! Make shell "sa" skipa list
            call create_(skipa,n_pt)
            call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

            ! Next shell "sa" if no sig. points
            if (n_keep==0) then
               call destroy_(skipa)
               cycle
            end if

            ! Assign the "sa" skipa list
            call create_(bf_skip(sa)%element,size(skipa))
            bf_skip(sa)%element = skipa

            ! Make/copy the "sa" basis function grid
            ! This must be copied since skipa could be TRUE
            call create_(bf_grid0(sa)%element,n_keep,na)
            if (oa==c) then
               do a = 1,na
                  i = 0
                  do n = 1,n_pt
                     if (skipa(n)) cycle
                     i = i+1
                     bf_grid0(sa)%element(i,a) = gridc0(n,a+fa-fba)
                  end do
               end do
            else
               call make_skip_grid_(sha,bf_grid0(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)
            end if

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  DDa = Da(fa:la,fb:lb)
                  DDb = Db(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = bf_grid0(sa)%element(:,1)
                     gb0 = bf_grid0(sb)%element(:,1)
                     Da_ab = fac*DDa(1,1)
                     Db_ab = fac*DDb(1,1)
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        gab = ga*gb
                        den%rho(n,1) = den%rho(n,1)+Da_ab*gab
                        den%rho(n,2) = den%rho(n,2)+Db_ab*gab
                     end do
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_grid0(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_grid0(sb)%element(:,b)
                           Da_ab = fac*DDa(a,b)
                           Db_ab = fac*DDb(a,b)
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              gab = ga*gb
                              den%rho(n,1) = den%rho(n,1)+Da_ab*gab
                              den%rho(n,2) = den%rho(n,2)+Db_ab*gab
                           end do
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(skipab); call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(gridc0)
   end subroutine

   PURE subroutine make_rho_becke_atom_grid_3(self,den,pt,wt,bf_skip,bf_gr0,bf_grx,bf_gry,bf_grz,c,Da,Db)
   ! Make the alpha and beta densities and their gradients, returned in
   ! "den", on the Becke grid
   ! ("pt","wt") for atom "c" given AO density matrices "Da", "Db".
   ! Also return the "bf_skip" array, and the basis function and basis
   ! function derivative arrays "bf_gr0", "bf_grx", "bf_gry",
   ! "bf_grz" for evaluating the density functionals on the grid.
      type(MOLECULE_TYPE), INOUT :: self
      type(XC_DENSITY_TYPE), INOUT :: den
      MAT(REAL,:,:), allocatable, OUT :: pt
      VEC(REAL,:), allocatable, OUT :: wt
      VEC(type(EVEC_BIN_TYPE),:), allocatable, OUT :: bf_skip
      VEC(type(EMAT_REAL_TYPE),:), allocatable, OUT :: bf_gr0,bf_grx,bf_gry,bf_grz
      INT, IN :: c
      MAT(REAL,:,:), IN :: Da,Db
      VEC(REAL,:), allocatable :: xa,ya,za,a2
      VEC(REAL,:), allocatable :: ga0,gax,gay,gaz
      VEC(REAL,:), allocatable :: gb0,gbx,gby,gbz
      VEC(REAL,3) :: ra
      VEC(INT,:), allocatable :: overlapping_atom,pi,pj,pn
      VEC(BIN,:), allocatable :: skipa0,skipab,skipa,skipb
      MAT(REAL,:,:), allocatable :: DDa,DDb
      MAT(REAL,:,:), allocatable :: gridc0,gridcx,gridcy,gridcz
      type(SHELL1_TYPE) :: sha
      REAL :: cutoff,Da_ab,Db_ab,fac,ga,gb,gab,gx,gy,gz
      INT :: n_pt,n_keep,ca,cb,oa,fsa,lsa,fba,offset,ob,fsb,lsb
      INT :: sa,fa,la,na,ba,sb,fb,lb,nb,a,b,i,j,p,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_rho_becke_atom_grid_3 ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_rho_becke_atom_grid_3 ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_rho_becke_atom_grid_3 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_rho_becke_atom_grid_3 ... no finalized becke_grid")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_rho_becke_atom_grid_3 ... no overlapping_atoms")

      ! Overlapping atoms
      overlapping_atom = self%overlapping_atoms_for_atom(c)%element

      ! Get the Becke grid for atom "c"
      call get_XC_grid_for_atom_(self,pt,wt,gridc0,gridcx,gridcy,gridcz,c)
      n_pt = size(pt,1)

      ! Create the alpha and beta densities and their gradients
      call create_grids_(den,n_pt,2,TRUE)

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Basis function grid and skip list
      call create_(bf_skip,self%n_shell)
      call create_(bf_gr0,self%n_shell)
      call create_(bf_grx,self%n_shell)
      call create_(bf_gry,self%n_shell)
      call create_(bf_grz,self%n_shell)

      ! Atom "a" displaced points (from "c") and skip switches
      call create_(xa,n_pt)
      call create_(ya,n_pt)
      call create_(za,n_pt)
      call create_(a2,n_pt)
      call create_(skipa0,n_pt)
      call create_(skipab,n_pt)

      ! Loop c-overlapping atoms "oa"
      do ca = 1,size(overlapping_atom)
         oa = overlapping_atom(ca)
         fsa = self%first_shell_for_atom(oa)
         lsa = self%last_shell_for_atom(oa)
         fba = self%first_basis_fn_for_atom(oa)
         ra = self%atom(oa)%position

         ! Make the displaced points for atom "oa"
         call make_displaced_skip_pts_(self%atom(oa),xa,ya,za,a2,skipa0,pt,n_pt,cutoff)

         ! Loop shells on atom a
         do sa = fsa,lsa
            fa = self%first_basis_fn_for_shell(sa)
            la = self%last_basis_fn_for_shell(sa)
            na = la-fa+1

            ! Set shell "sa"
            ba = self%basis_shell_for_shell(sa)
            sha = self%basis_shell1pair(ba,1)%a
            sha%position = ra

            ! Make shell "sa" skipa list
            call create_(skipa,n_pt)
            call make_skip_list_(sha,skipa,n_keep,a2,skipa0,n_pt,cutoff)

            ! Next shell "sa" if no sig. points
            if (n_keep==0) then
               call destroy_(skipa)
               cycle
            end if

            ! Assign the "sa" skipa list
            call create_(bf_skip(sa)%element,size(skipa))
            bf_skip(sa)%element = skipa

            ! Make/copy the "sa" basis function grid
            ! This must be copied since skipa could be TRUE
            call create_(bf_gr0(sa)%element,n_keep,na)
            call create_(bf_grx(sa)%element,n_keep,na)
            call create_(bf_gry(sa)%element,n_keep,na)
            call create_(bf_grz(sa)%element,n_keep,na)
            if (oa==c) then
               offset = fa-fba
               do a = 1,na
                  i = 0
                  do n = 1,n_pt
                     if (skipa(n)) cycle
                     i = i+1
                     bf_gr0(sa)%element(i,a) = gridc0(n,a+offset)
                     bf_grx(sa)%element(i,a) = gridcx(n,a+offset)
                     bf_gry(sa)%element(i,a) = gridcy(n,a+offset)
                     bf_grz(sa)%element(i,a) = gridcz(n,a+offset)
                  end do
               end do
            else
               call make_skip_nabla_grid_(sha,bf_grx(sa)%element,bf_gry(sa)%element,bf_grz(sa)%element,bf_gr0(sa)%element,n_keep,xa,ya,za,a2,skipa,n_pt)
            end if

            ! Loop c-overlapping atoms b
            do cb = 1,ca
               ob = overlapping_atom(cb)
               fsb = self%first_shell_for_atom(ob)
               lsb = self%last_shell_for_atom(ob)

               ! Loop shells on atom b
               do sb = fsb,min(lsb,sa)
                  if (NOT allocated(bf_skip(sb)%element)) cycle
                  fb = self%first_basis_fn_for_shell(sb)
                  lb = self%last_basis_fn_for_shell(sb)
                  nb = lb-fb+1

                  ! Set the "sb" basis function grid
                  skipb = bf_skip(sb)%element

                  ! How many points in common?
                  skipab(1:n_pt) = skipa OR skipb
                  n_keep = count(NOT skipab(1:n_pt))
                  if (n_keep==0) cycle

                  ! Factor
                  fac = TWO
                  if (sa==sb) fac = ONE

                  ! Density block
                  DDa = Da(fa:la,fb:lb)
                  DDb = Db(fa:la,fb:lb)
                  if (na*nb==1) then
                     ga0 = bf_gr0(sa)%element(:,1)
                     gax = bf_grx(sa)%element(:,1)
                     gay = bf_gry(sa)%element(:,1)
                     gaz = bf_grz(sa)%element(:,1)
                     gb0 = bf_gr0(sb)%element(:,1)
                     gbx = bf_grx(sb)%element(:,1)
                     gby = bf_gry(sb)%element(:,1)
                     gbz = bf_grz(sb)%element(:,1)
                     Da_ab = fac*DDa(1,1)
                     Db_ab = fac*DDb(1,1)
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        ga = ga0(i)
                        gb = gb0(j)
                        gab = ga*gb
                        gx = gax(i)*gb+ga*gbx(j)
                        gy = gay(i)*gb+ga*gby(j)
                        gz = gaz(i)*gb+ga*gbz(j)
                        den%rho(n,1) = den%rho(n,1)+Da_ab*gab
                        den%grad(n,1,1) = den%grad(n,1,1)+Da_ab*gx
                        den%grad(n,2,1) = den%grad(n,2,1)+Da_ab*gy
                        den%grad(n,3,1) = den%grad(n,3,1)+Da_ab*gz
                        den%rho(n,2) = den%rho(n,2)+Db_ab*gab
                        den%grad(n,1,2) = den%grad(n,1,2)+Db_ab*gx
                        den%grad(n,2,2) = den%grad(n,2,2)+Db_ab*gy
                        den%grad(n,3,2) = den%grad(n,3,2)+Db_ab*gz
                     end do
                  else

                     ! Create space for gathered index info
                     call create_(pi,n_keep)
                     call create_(pj,n_keep)
                     call create_(pn,n_keep)

                     ! Make mapper arrays
                     p = 0
                     i = 0
                     j = 0
                     do n = 1,n_pt
                        if (NOT skipa(n)) i = i+1
                        if (NOT skipb(n)) j = j+1
                        if (skipab(n)) cycle
                        p = p+1
                        pi(p) = i
                        pj(p) = j
                        pn(p) = n
                     end do

                     ! Add density contribution to rho
                     do a = 1,na
                        ga0 = bf_gr0(sa)%element(:,a)
                        gax = bf_grx(sa)%element(:,a)
                        gay = bf_gry(sa)%element(:,a)
                        gaz = bf_grz(sa)%element(:,a)
                        do b = 1,nb
                           gb0 = bf_gr0(sb)%element(:,b)
                           gbx = bf_grx(sb)%element(:,b)
                           gby = bf_gry(sb)%element(:,b)
                           gbz = bf_grz(sb)%element(:,b)
                           Da_ab = fac*DDa(a,b)
                           Db_ab = fac*DDb(a,b)
                           do p = 1,n_keep
                              i = pi(p)
                              j = pj(p)
                              n = pn(p)
                              ga = ga0(i)
                              gb = gb0(j)
                              gab = ga*gb
                              gx = gax(i)*gb+ga*gbx(j)
                              gy = gay(i)*gb+ga*gby(j)
                              gz = gaz(i)*gb+ga*gbz(j)
                              den%rho(n,1) = den%rho(n,1)+Da_ab*gab
                              den%grad(n,1,1) = den%grad(n,1,1)+Da_ab*gx
                              den%grad(n,2,1) = den%grad(n,2,1)+Da_ab*gy
                              den%grad(n,3,1) = den%grad(n,3,1)+Da_ab*gz
                              den%rho(n,2) = den%rho(n,2)+Db_ab*gab
                              den%grad(n,1,2) = den%grad(n,1,2)+Db_ab*gx
                              den%grad(n,2,2) = den%grad(n,2,2)+Db_ab*gy
                              den%grad(n,3,2) = den%grad(n,3,2)+Db_ab*gz
                           end do
                        end do
                     end do

                     ! Clean up
                     call destroy_(pn)
                     call destroy_(pj)
                     call destroy_(pi)
                  end if
               end do  ! -- loops sa,sb
            end do
         end do  ! -- lops ca,cb
      end do

      ! Clean up
      call destroy_(skipab)
      call destroy_(skipa0)
      call destroy_(a2)
      call destroy_(za)
      call destroy_(ya)
      call destroy_(xa)
      call destroy_(gridcz)
      call destroy_(gridcy)
      call destroy_(gridcx)
      call destroy_(gridc0)
   end subroutine


!  ======================
!  Set interpolator grids
!  ======================

! For faster Hirshfeld weight function evaluation

   subroutine make_ANO_interpolators(self)
   ! Get the gaussian ANO interpolators. Note that for gaussian ANO
   ! interpolators the atom.basis_kind's must have been "gaussian" at
   ! the time the interpolator was made.
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: template
      INT :: a,b
      VEC(BIN,:), allocatable :: done
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_ANO_interpolators ... no atoms")
   ENSURE(self%atom(1)%basis_kind=="gaussian","MOLECULE.RHO:make_ANO_interpolators ... the basis_kind is not gaussian!")
   ENSURE(self%n_atom_kind>0,"MOLECULE.RHO:make_ANO_interpolators ... no atom kinds set")

      ! Already there?
      if (has_all_interpolators_(self)) return
      call create_(done,self%n_atom)
      done = FALSE
      do a = 1,self%n_atom

         ! Skip?
         if (done(a)) cycle

         ! Make the interpolator
         if (NOT allocated(self%atom(a)%interpolator)) then
            if (allocated(self%interpolator)) then
               call make_interpolator_(self%atom(a),self%interpolator)
            else
               call make_interpolator_sqrt_x1x_(self,template)
               call make_interpolator_(self%atom(a),template)
               call destroy_(template)
            end if
         end if
         done(a) = TRUE

         ! Copy interpolartor to other atoms
         do b = a+1,self%n_atom

            ! Skip?
            if (done(b)) cycle
            if (NOT is_same_kind_as_(self%atom(b),self%atom(a))) cycle
            ! Copy 
            self%atom(b)%interpolator = self%atom(a)%interpolator
            done(b) = TRUE
         end do
      end do
      call destroy_(done)
   end subroutine

   subroutine make_slater_interpolators(self)
   ! Make the slaterbasis orbital density interpolators.
   ! Make them using the defaults set in a give "interpolator".
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: template
      INT :: i
   ENSURE(allocated(self%slaterbasis),"MOLECULE.RHO:make_slater_interpolators ... no slaterbasis")

   ! stdout.flush
   ! stdout.text("Making slater interpolators ...")

      ! Make the interpolators
      if (allocated(self%interpolator)) then

         ! User defined interpolator
         do i = 1,size(self%slaterbasis)
            call make_interpolator_(self%slaterbasis(i),self%interpolator)
         end do
      else

         ! Default interpolator
         call make_interpolator_sqrt_(self,template)
         do i = 1,size(self%slaterbasis)
            call make_interpolator_(self%slaterbasis(i),template)
         end do
         call destroy_(template)
      end if

      ! Set .atom to use this interpolator
      if (allocated(self%atom)) call set_basis_kind_(self%atom,"slater")
   end subroutine

   subroutine make_coppens_interpolators(self)
   ! Make the coppensbasis orbital density interpolators.
   ! Make them using the defaults set in a give "interpolator".
   ! Otherwise use an atom density cutoff "tol".
   ! NOTE: any existing interpolators are destroyed first
      type(MOLECULE_TYPE), INOUT :: self
      type(INTERPOLATOR_TYPE), allocatable :: template
      INT :: i
   ENSURE(allocated(self%coppensbasis),"MOLECULE.RHO:make_coppens_interpolators ... no slaterbasis")
      call flush_(stdout)
      call text_(stdout,"Making coppens interpolators ...")

      ! Make the interpolators
      if (allocated(self%interpolator)) then
         do i = 1,size(self%coppensbasis)  ! Make the interpolators
            call make_interpolator_(self%coppensbasis(i),self%interpolator)
         end do
      else
         call make_interpolator_sqrt_(self,template)
         do i = 1,size(self%coppensbasis)  ! Make the interpolators
            call make_interpolator_(self%coppensbasis(i),template)
         end do
         call destroy_(template)
      end if

      ! Set .atom to use this interpolator
      if (allocated(self%atom)) call set_basis_kind_(self%atom,"coppens")
   end subroutine

   subroutine make_interpolator_sqrt(self,interpolator)
   ! Make a non-gaussian interpolator. These typically used for
   ! stockholder weight functions.
      type(MOLECULE_TYPE), IN :: self
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
      type(CLUSTER_TYPE) :: cluster

      ! Create default interpolator
      call create_(interpolator)

      ! Reset default table eps
      call set_table_eps_(interpolator,cluster%atom_density_cutoff)

      ! If a saved cluster exists, use a table eps from that
      if (allocated(self%saved)) then
         if (allocated(self%saved%cluster)) then
            if (self%saved%cluster%info_made) then
               call set_table_eps_(interpolator,self%saved%cluster%atom_density_cutoff)
            end if
         end if
      end if

      ! Set a linear-sqrt interpolator
      call set_interpolation_method_(interpolator,"linear")
      call set_range_mapping_(interpolator,"none")
      call set_domain_mapping_(interpolator,"sqrt")
      call set_table_length_(interpolator,900d0)
      call set_table_spacing_(interpolator,0.1d0)
   end subroutine

   subroutine make_interpolator_sqrt_x1x(self,interpolator)
   ! Make a gaussian interpolator.
      type(MOLECULE_TYPE), IN :: self
      type(INTERPOLATOR_TYPE), allocatable :: interpolator
      type(CLUSTER_TYPE) :: cluster

      ! Create default interpolator
      call create_(interpolator)

      ! Reset default table eps
      call set_table_eps_(interpolator,cluster%atom_density_cutoff)

      ! If a saved cluster exists, use a table eps from that
      if (allocated(self%saved)) then
         if (allocated(self%saved%cluster)) then
            if (self%saved%cluster%info_made) then
               call set_table_eps_(interpolator,self%saved%cluster%atom_density_cutoff)
            end if
         end if
      end if

      ! Set a linear-sqrt interpolator
      call set_interpolation_method_(interpolator,"cubic_spline")
      call set_range_mapping_(interpolator,"none")
      call set_domain_mapping_(interpolator,"sqrt(x/(1-x))*scale")
      call set_table_length_(interpolator,ONE)
      call set_spacing_(interpolator,0.0001d0)
   end subroutine


!  =========================
!  ED at a point & on a line
!  =========================

   PURE function ED_at_point(self,pt) result (res)
   ! Return the electron density at a single point "pt"
   ! NOTE: This is Kanghyun's version. Now working.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,3), IN :: pt
      REAL :: res
      VEC(REAL,1) :: grid
      call make_ED_grid_r_(self,grid,reshape(pt,[1,3]))
      res = grid(1)
   end function

   PURE function ED_between_points_0(self,ra,rb,n_pts) result (res)
   ! Return ED on equally spaced grid points between "ra" and "rb"
   ! as the first and last points, inclusive.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,3), IN :: ra
      VEC(REAL,3), IN :: rb
      INT, IN :: n_pts
      VEC(REAL,n_pts) :: res
      MAT(REAL,:,:), allocatable :: pts
      VEC(REAL,3) :: del
      VEC(REAL,3) :: p
      INT :: i
   ENSURE(n_pts>3,"MOLECULE.RHO:ED_between_points_0 ... too few points")
      call create_(pts,n_pts,3)

      ! Step vector
      del = rb-ra
      del = del/(n_pts-1)

      ! Line of points
      p = ra
      do i = 1,n_pts
         pts(i,1) = p(1)
         pts(i,2) = p(2)
         pts(i,3) = p(3)
         p = p+del
      end do
      call make_ED_grid_r_(self,res,pts)

      ! Clean
      call destroy_(pts)
   end function

   PURE function ED_between_points_1(self,ra,rb,n_pts,DM) result (res)
   ! Return ED on equally spaced grid points between "ra" and "rb"
   ! as the first and last points, inclusive. "DM" is the density
   ! matrix used to calculated the electron density.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,3), IN :: ra
      VEC(REAL,3), IN :: rb
      INT, IN :: n_pts
      MAT(REAL,:,:), IN :: DM
      VEC(REAL,n_pts) :: res
      MAT(REAL,:,:), allocatable :: pts
      VEC(REAL,3) :: del
      VEC(REAL,3) :: p
      INT :: i
   ENSURE(n_pts>3,"MOLECULE.RHO:ED_between_points_1 ... too few points")
      call create_(pts,n_pts,3)

      ! Step vector
      del = rb-ra
      del = del/(n_pts-1)

      ! Line of points
      p = ra
      do i = 1,n_pts
         pts(i,1) = p(1)
         pts(i,2) = p(2)
         pts(i,3) = p(3)
         p = p+del
      end do
      call make_ED_grid_r_(self,res,pts,DM)

      ! Clean
      call destroy_(pts)
   end function

   subroutine make_ED_interpolator_between_atoms(self,a,b)
   ! Make the .interpolator for the ED between two atoms "a" and "b"
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a,b
      INT :: n_pts
      VEC(REAL,:), allocatable :: pts
      VEC(REAL,:), allocatable :: line
      VEC(REAL,3) :: ra,rb
      VEC(REAL,3) :: del
      REAL :: stp,val
      INT :: i
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_ED_interpolator_between_atoms ... no atoms")
   ENSURE(allocated(self%interpolator),"MOLECULE.RHO:make_ED_interpolator_between_atoms ... no interpolator info, specify n_data=")
   ENSURE(self%interpolator%n_data>0,"MOLECULE.RHO:make_ED_interpolator_between_atoms ... wrong no. of data, interpolator")

      ! How many points?
      n_pts = self%interpolator%n_data

      ! Positions
      ra = self%atom(a)%position
      rb = self%atom(b)%position

      ! Step vector
      del = rb-ra
      del = del/(n_pts-1)
      stp = norm2(del)

      ! Points as steps
      call create_(pts,n_pts)
      val = ZERO
      do i = 1,n_pts
         pts(i) = val
         val = val+stp
      end do

      ! Make ED line values
      line = ED_between_points_(self,ra,rb,n_pts)

      ! Set domain scale factor. May not be required.
      call set_data_points_(self%interpolator,pts)
      call set_data_values_(self%interpolator,line)

      ! Clean
      call destroy_(line)
      call destroy_(pts)
   end subroutine

   NON_MPI_PURE function promolecule_ED_at_point(self,pt) result (res)
   ! Return promolecule ED at a point "pt".
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,3), IN :: pt
      REAL :: res
      VEC(REAL,1) :: grid
      INT :: a
   ENSURE(allocated(self%atom),"MOLECULE.RHO:promolecule_ED_at_point ... no atoms")

      ! Sum up the atom grid values
      res = ZERO
      do a = PARALLEL_DO_START(1,1),self%n_atom,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:promolecule_ED_at_point")
         call make_ED_grid_(self%atom(a),grid,reshape(pt,[1,3]))
         res = res+grid(1)
      end do
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:promolecule_ED_at_point")
      PARALLEL_SUM(res)
   end function


!  ======================
!  Promolecule density mx
!  ======================

   subroutine add_ANO_densities(self,DM)
   ! Add the ANO densities to make the total density matrix
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), allocatable, OUT :: DM
      INT :: a,b,f,l
   ENSURE(allocated(self%atom),"MOLECULE.RHO:add_ANO_densities ... no atoms")
   ENSURE(has_all_ANO_matrices_(self),"MOLECULE.RHO:add_ANO_densities ... no ANOs")

      ! Create
      call create_(DM,self%n_bf,"r ")

      ! Set to zero
      DM%r = ZERO

      ! Add 'em up
      do a = 1,self%n_atom

         ! Transfer non-unique atoms
         do b = 1,self%n_atom
            if (NOT is_same_kind_as_(self%atom(b),self%atom(a))) cycle
            f = self%first_basis_fn_for_atom(b)
            l = self%last_basis_fn_for_atom(b)
            DM%r(f:l,f:l) = self%atom(a)%density_mx%r
         end do
      end do

      ! Symmetrize
      call symmetric_reflect_(DM%r)
   end subroutine


!  ============================
!  Stockholder atom weight grid
!  ============================

   PURE subroutine apply_stockholder_atom_weight(self,grid,a,pts)
   ! Apply (i.e. multiply) "grid" by the stockholder weight function
   ! for the atom with index "a" using supplied points "pts".
   ! * If .use_interpolators is TRUE then interpolators are used.
   ! * If .use_interpolators is FALSE then interpolators could still
   !   be used if explicitly set for each atom (however the
   !   overlapping_atoms array will not be used to eliminate work).
   !   More likely the density will be explicitly evaluated.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), INOUT :: grid
      INT, IN :: a
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: Wa
   ENSURE(allocated(self%atom),"MOLECULE.RHO:apply_stockholder_atom_weight ... no atoms")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:apply_stockholder_atom_weight ... incompatible dim1, grid & pts")

      ! Only needed for more than one atom
      if (self%n_atom>1) then

         ! Get weights
         call create_(Wa,size(pts,1))
         call make_stockholder_atom_weight_(self,Wa,a,pts)

         ! Apply them
         grid = grid*Wa

         ! Clean
         call destroy_(Wa)
      end if
   end subroutine

   PURE subroutine make_stockholder_atom_weight_0(self,grid,a,pts)
   ! Make the "grid" of stockholder weight function values for the
   ! atom with index "a" on supplied points "pts".
   ! * If .use_interpolators is TRUE then interpolators are used.
   ! * If .use_interpolators is FALSE then interpolators could still
   !   be used if explicitly set for each atom (however the
   !   overlapping_atoms array will not be used to eliminate work).
   !   More likely the density will be explicitly evaluated.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      INT, IN :: a
      MAT(REAL,:,:), IN :: pts
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_0 ... no atoms")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_stockholder_atom_weight_0 ... incompatible dim1, grid & pts")
      if (self%n_atom==1) then

         ! Avoid this case if possible
         grid = ONE
      else

         ! Get weights
         if (self%use_interpolators) then
            ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_0 ... missing atomic interpolators")
            ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_stockholder_atom_weight_0 ... no overlapping_atoms")
            call make_stockholder_atom_weight_int_(self,grid,a,pts,self%overlapping_atoms_for_atom(a)%element)
         else
            call make_stockholder_atom_weight_acc_(self,grid,a,pts)
         end if
      end if
   end subroutine

   PURE subroutine make_stockholder_atom_weight_acc(self,grid,a,pts)
   ! Make the Hirshfeld stockholder weight density "grid" for the atom
   ! with index "a" using supplied points "pts".
   ! NOTE: This DOES NOT USE an interpolated density for each atom
   ! UNLESS use_interpolators is explicitly set. In this case you
   ! should probably use the next routine which uses the
   ! overlapping_atoms.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      INT, IN :: a
      MAT(REAL,:,:), IN :: pts
      VEC(REAL,:), allocatable :: rho_a
      VEC(REAL,:), allocatable :: rho_i
      REAL :: n
      INT :: n_pts,i
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_acc ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_acc ... missing atomic interpolators")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_stockholder_atom_weight_acc ... incompatible dim1, grid & pts")

      ! Allocate temporary densities
      n_pts = size(grid)
      call create_(rho_a,n_pts)

      ! Make density for atom "a" in "rho_a"
      call make_ED_grid_(self%atom(a),rho_a,pts)

      ! Make total density in "grid"
      n = stockholder_exponent_(self)
      if (n==ONE) then
         grid = rho_a
         do i = 1,self%n_atom
            if (i==a) cycle
            call add_ED_grid_(self%atom(i),grid,pts)
         end do
      else

         ! Exponential Hirshfeld: each atom's density to the power n
         call create_(rho_i,n_pts)
         rho_a = rho_a**n
         grid = rho_a
         do i = 1,self%n_atom
            if (i==a) cycle
            call make_ED_grid_(self%atom(i),rho_i,pts)
            grid = grid+rho_i**n
         end do
         call destroy_(rho_i)
      end if

      ! Make the stockholder weight
      do i = 1,n_pts
         if (grid(i)>epsilon(ONE)) then; grid(i) = rho_a(i)/grid(i)
         else; grid(i) = ZERO
         end if
      end do
      call destroy_(rho_a)
   end subroutine

   PURE subroutine make_stockholder_atom_weight_int(self,grid,a,pts,overlapping_atom)
   ! Make the Hirshfeld stockholder atom weight desity "grid" for the
   ! atom with index "a" using supplied points "pts" and given a list
   ! of "overlapping_atom". This makes use of a previously calculated
   ! interpolated density for each atom.
   ! NOTE: the CLUSTER.atom_density_cutoff must be the same as the
   !       the BECKE_GRID.basis_fn_cutoff
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      INT, IN :: a
      MAT(REAL,:,:), IN :: pts
      VEC(INT,:), IN :: overlapping_atom
      VEC(REAL,:), allocatable :: rho_a
      VEC(REAL,:), allocatable :: rho_i
      REAL :: n
      INT :: n_pts,i,j
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_int ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_int ... missing atomic interpolators")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_stockholder_atom_weight_int ... incompatible dim1, grid & pts")

      ! Allocate temporary densities
      n_pts = size(grid)
      call create_(rho_a,n_pts)

      ! Make density for atom "a"
      call make_interpolated_density_(self%atom(a),rho_a,pts)

      ! Make total density in "grid"
      n = stockholder_exponent_(self)
      if (n==ONE) then
         grid = rho_a
         do j = 1,size(overlapping_atom)
            i = overlapping_atom(j)
            if (i==a) cycle
            call add_interpolated_density_(self%atom(i),grid,pts)
         end do
      else

         ! Exponential Hirshfeld: each atom's density to the power n
         call create_(rho_i,n_pts)
         rho_a = rho_a**n
         grid = rho_a
         do j = 1,size(overlapping_atom)
            i = overlapping_atom(j)
            if (i==a) cycle
            call make_interpolated_density_(self%atom(i),rho_i,pts)
            grid = grid+rho_i**n
         end do
         call destroy_(rho_i)
      end if

      ! Make the stockholder weight
      do i = 1,n_pts
         if (grid(i)>epsilon(ONE)) then; grid(i) = rho_a(i)/grid(i)
         else; grid(i) = ZERO
         end if
      end do
      call destroy_(rho_a)
   end subroutine

   pure function stockholder_exponent(self) result (res)
   ! The power of the free-atom densities in the stockholder weight:
   ! expHAR_power for the "oc-exphar" and "oc-sph-exphar" partition
   ! models, otherwise 1, the Hirshfeld weight.
      type(MOLECULE_TYPE), IN :: self
      REAL :: res
      res = ONE
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%crystal%xray_data)) return
      if (self%crystal%xray_data%partition_model=="oc-exphar" OR self%crystal%xray_data%partition_model=="oc-sph-exphar") then
         res = self%crystal%xray_data%expHAR_power
      end if
   end function

   PURE subroutine make_stockholder_atom_weight_1(self,grid,a,pts,overlapping_atom)
   ! Make the Hirshfeld stockholder weight density "grid" for the atom
   ! with index "a" using supplied points "pts". This version uses
   ! interpolated atom densities to make the weight function only for
   ! the "overlapping_atoms" which are considered the near neighbours.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      INT, IN :: a
      MAT(REAL,:,:), IN :: pts
      VEC(INT,:), IN :: overlapping_atom
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_1 ... no atoms")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.RHO:make_stockholder_atom_weight_1 ... missing atomic interpolators")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_stockholder_atom_weight_1 ... incompatible dim1, grid & pts")
      if (self%n_atom>1) then
         call make_stockholder_atom_weight_int_(self,grid,a,pts,overlapping_atom)
      else
         grid = ONE
      end if
   end subroutine


!  ==============================
!  Salvador-RC neighbours & radii
!  ==============================

   subroutine make_Salvador_radii(self)
   ! Put the Salvador & Ramos-Cordoba radii for each atom. For the
   ! "oc-tfvp" partition model they are found on the promolecule density
   ! rather than the molecular one; for "oc-tfvh" where the two atoms'
   ! spherical densities are equal.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii ... no atoms")
      if (uses_Hirshfeld_Salvador_radii_(self)) then
         call make_Salvador_radii_hirshfeld_(self)
      else if (uses_promolecule_Salvador_radii_(self)) then
         call make_Salvador_radii_promolecule_(self)
      else
         call make_Salvador_radii_orig_(self,self%Salvador_bonds_for,self%Salvador_radii_for,self%Salvador_chi)
      end if
   ! .make_Salvador_radii_orig(.Salvador_radii_for,50,400)
   end subroutine

   pure function uses_promolecule_Salvador_radii(self) result (res)
   ! Return TRUE if the Salvador radii are to be found on the promolecule
   ! density: the "oc-tfvp" and "oc-sph-tfvp" partition models.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%crystal%xray_data)) return
      res = self%crystal%xray_data%partition_model=="oc-tfvp" OR self%crystal%xray_data%partition_model=="oc-sph-tfvp"
   end function

   pure function uses_Hirshfeld_Salvador_radii(self) result (res)
   ! Return TRUE if each Salvador radius is to be put where the two atoms'
   ! spherical densities are equal: the "oc-tfvh" and "oc-sph-tfvh" models.
      type(MOLECULE_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (NOT allocated(self%crystal)) return
      if (NOT allocated(self%crystal%xray_data)) return
      res = self%crystal%xray_data%partition_model=="oc-tfvh" OR self%crystal%xray_data%partition_model=="oc-sph-tfvh"
   end function

   subroutine make_Salvador_radii_promolecule(self)
   ! Make the Salvador neighbours, and the Mayer-Salvador radii and size
   ! adjustments "chi" found on the promolecule density: the sum of the
   ! atoms' ANO densities, built as put_Salvador_radii does.
      type(MOLECULE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), allocatable :: DM
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_promolecule ... no atoms")

      ! Neighbours
      call make_Salvador_neighbours_(self%atom,self%Salvador_bonds_for)

      ! Promolecule density matrix
      call add_ANO_densities_(self,DM)
      call convert_to_(DM,"r",factor=HALF)

      ! Radii
      call make_Salvador_radii_pro_(self,self%Salvador_radii_for,self%Salvador_bonds_for,DM%r)
      call destroy_(DM)

      ! Size adjustments
      call make_Salvador_chi_(self)
   end subroutine

   subroutine make_Salvador_radii_hirshfeld(self)
   ! Make the Salvador neighbours, and the radii and size adjustments
   ! "chi" with each pair's boundary where the two atoms' spherical ANO
   ! densities are equal: the point on the bond where the pair's Hirshfeld
   ! weight is one half. Like the promolecule radii, these depend only on
   ! the geometry and the atom kinds.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_hirshfeld ... no atoms")

      ! Neighbours
      call make_Salvador_neighbours_(self%atom,self%Salvador_bonds_for)

      ! Radii
      call make_Salvador_radii_hir_(self,self%Salvador_radii_for,self%Salvador_bonds_for)

      ! Size adjustments
      call make_Salvador_chi_(self)
   end subroutine

   PURE subroutine make_Salvador_chi(self)
   ! Make .Salvador_chi, the size adjustments of make_Salvador_radii_orig,
   ! from .Salvador_radii_for and .Salvador_bonds_for.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,3) :: ra,rb
      REAL :: fac
      INT :: a,b,n
   ENSURE(allocated(self%Salvador_bonds_for),"MOLECULE.RHO:make_Salvador_chi ... no Salvador_bonds")
   ENSURE(allocated(self%Salvador_radii_for),"MOLECULE.RHO:make_Salvador_chi ... no Salvador_radii")
      call destroy_(self%Salvador_chi)
      call create_(self%Salvador_chi,size(self%atom),size(self%atom))
      self%Salvador_chi = ONE
      do a = 1,size(self%atom)
         ra = self%atom(a)%position
         do n = 1,size(self%Salvador_bonds_for(a)%element)
            b = self%Salvador_bonds_for(a)%element(n)
            rb = self%atom(b)%position
            fac = self%Salvador_radii_for(a)%element(n)/norm2(rb-ra)
            self%Salvador_chi(a,b) = fac/(ONE-fac)
         end do
      end do
   end subroutine

   PURE subroutine make_Salvador_radii_orig(self,bonds_for,radii_for,chi,n_div,n_sub)
   ! Make the Mayer-Salvador atom radii corresponding to the
   ! bonded neighbours (which are also calculated) for the Topological
   ! Fuzzy Voronoi Atoms.
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: bonds_for
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: radii_for
      MAT(REAL,:,:), allocatable, OUT :: chi
      INT, optional, IN :: n_div
      INT, optional, IN :: n_sub
      INT :: n_atoms
      INT :: n_bonds
      INT :: a,b,n
      VEC(REAL,3) :: ra,rb
      REAL :: fac
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_orig ... no atoms")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Salvador_radii_orig ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_Salvador_radii_orig ... no restricted density_mx")

      ! Neighbours first
      call make_Salvador_neighbours_(self%atom,bonds_for)

      ! Create radii table
      n_atoms = size(self%atom)
      call create_(radii_for,n_atoms)
      call create_(chi,n_atoms,n_atoms)
      chi = ONE

      ! Atom a
      do a = 1,n_atoms
         n_bonds = size(bonds_for(a)%element)
         call create_(radii_for(a)%element,n_bonds)
         ra = self%atom(a)%position
         ! Neighbours b
         do n = 1,n_bonds
            b = bonds_for(a)%element(n)
            rb = self%atom(b)%position
            fac = Mayer_Salvador_radius_for_(self,a,b,n_div,n_sub)
            radii_for(a)%element(n) = fac*norm2(rb-ra)
            chi(a,b) = fac/(ONE-fac)
         end do
      end do
   end subroutine

   PURE function Mayer_Salvador_radius_for_0(self,a,b,n_div,n_sub) result (res)
   ! Return the Mayer-Salvador radius R(a.b) for atoms "a" and "b" in
   ! the direction a->b, used to make Topological Fuzzy Voronoi atoms.
   ! The code is accurate to 1/200-th of the bond length R(a-b).
   ! The code also ensures that R(a.b) + R(b.a) = R(a-b).
   ! Options:
   ! . "n_div" [50] is the number of divisions used to detect minima.
   ! . "n_sub" [40] is the no. of subdivisions to refine the result.
   ! NOTE: Code was extracted from APOST-3D and contains the original

   ! Ramos-Cordoba radii. The method accounts for numerical errors and
   ! instabilities in the internuclear density and atoms too far away.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      INT, optional, IN :: n_div
      INT, optional, IN :: n_sub
      REAL :: res
      VEC(REAL,3) :: ra,rb,del
      VEC(REAL,3) :: rc,rd
      VEC(REAL,:), allocatable :: rho
      VEC(INT,:), allocatable :: kmin,kmax
      INT :: nmin,nmax
      INT :: ndiv,nsub
      REAL :: step,subs
      INT :: k,km,kf
      INT :: midd,maxd
      INT :: best,diff
      BIN :: is_min
      BIN :: swap
      INT :: Za,Zb
   ENSURE(a/=b,"MOLECULE.RHO:Mayer_Salvador_radius_for_0 ... atoms should not be the same")

      ! Scan resolution: must be even
      ndiv = 50  ! No. of divisions 
      if (present(n_div)) ndiv = n_div

      ! Polishing resolution: also even
      nsub = 40  ! No. of subdivisions
      if (present(n_sub)) nsub = n_sub

      ! Are atoms too far?
      ra = self%atom(a)%position
      rb = self%atom(b)%position
      if (norm2(rb-ra)>TEN) then
         res = HALF  ! Divide half-way
         return
      end if

      ! Ensure a has *smaller* atomic number.
      ! Or if equal, index a is less than b.
      ! This ensures R(a.b) + R(b.a) = R(a-b)
      Za = self%atom(a)%atomic_number
      Zb = self%atom(b)%atomic_number
      swap = FALSE
      if (Za>Zb) then; swap = TRUE
      else if (Za==Zb AND a>b) then; swap = TRUE
      end if

      ! Swap positions
      if (swap) then
         rc = ra
         ra = rb
         rb = rc
      end if

      ! ED, min & max locations
      call create_(rho,ndiv+1)  ! Note +1
      call create_(kmin,ndiv)  ! idx of k-th min
      call create_(kmax,ndiv)  ! idx of k-th max

      ! Get ED from a->b at ndiv+1 points 
      ! First and last point are on ra & rb
      rho = ED_between_points_(self,ra,rb,ndiv+1)

      ! Find indices of minima & maxima
      nmin = 0
      nmax = 0
      do k = 2,ndiv
         if (rho(k-1)>rho(k) AND rho(k)<rho(k+1)) then

            ! Minima found
            nmin = nmin+1
            kmin(nmin) = k
         end if
         if (rho(k-1)<rho(k) AND rho(k)>rho(k+1)) then

            ! Maxima found
            nmax = nmax+1
            kmax(nmax) = k
         end if
      end do

      ! Sanity check: any critical points?
      ENSURE(nmin>0,"MOLECULE.RHO:Mayer_Salvador_radius_for_0 ... no minima found between the atoms!")
   ! WARN_IF(nmax>0,"maxima found between the atoms!")
      if (nmin==1 AND nmax==0) then

         ! Easy case: 1 minima, 0 maxima
         km = kmin(1)  ! Index of minimum
         is_min = TRUE  ! Set 1 if maxima
      else if (nmax>0 AND abs(nmin-nmax)==1) then

         ! Troubles.
         ! One more min than max, or vice versa
         if (is_odd_(nmax)) then

            ! Central max selected 
            km = kmax((nmax+1)/2)
            is_min = FALSE
         else

            ! Central min selected. If any.
            ! But there must, if nmax is even!
            km = kmin((nmin+1)/2)
            is_min = TRUE
         end if
      else

         ! Real touble. Inconsistent no. of extrema.
         ! Select best min/max i.e closest to midpoint
         midd = ndiv/2  ! Middle division
         maxd = ndiv  ! Max. diff to midd
         best = midd  ! Start at middle 
         is_min = TRUE

         ! First minima to middle division.
         ! If there is one, it is found
         do k = 1,nmin
            diff = abs(kmin(k)-midd)
            if (diff<maxd) then
               best = kmin(k)  ! Store
               maxd = diff  ! Look no further
               is_min = TRUE  ! Label it
            end if
         end do

         ! Any maxima closer than maxd?
         ! Maxima trump minima.
         do k = 1,nmax
            diff = abs(kmax(k)-midd)
            if (diff<maxd) then
               best = kmax(k)
               maxd = diff
               is_min = FALSE
            end if
         end do

         ! Finally
         km = best
      end if

      ! Extrema index km found.
      ! Go back one step, look ahead two steps
      step = ONE/ndiv  ! Orig. step size
      del = (rb-ra)*step  ! Step vector
      rc = ra+(km-2)*del  ! km-1 is the extrema
      rd = ra+(km)*del  ! *Two* steps ahead
      subs = TWO*step/nsub  ! Substep size, over 2 steps

      ! ED from rc->rd at nsub+1 points 
      ! First & last points on rc & rd
      rho = ED_between_points_(self,rc,rd,nsub+1)

      ! Find first extrema, finer level
      if (is_min) then
         do k = 2,nsub
            kf = k
            if (rho(k-1)>rho(k) AND rho(k)<rho(k+1)) exit
         end do
      else
         do k = 2,nsub
            kf = k
            if (rho(k-1)<rho(k) AND rho(k)>rho(k+1)) exit
         end do
      end if

      ! Steps from ra
      res = (km-2)*step+(kf-1)*subs

      ! Swap back?
      if (swap) res = ONE-res
      ENSURE(res>0.1 AND res<0.9,"MOLECULE.RHO:Mayer_Salvador_radius_for_0 ... Too close to nuclei!")
   end function

   PURE function Mayer_Salvador_radius_for_1(self,a,b,n_div,n_sub,DM) result (res)
   ! Return the Mayer-Salvador radius R(a.b) for atoms "a" and "b" in
   ! the direction a->b, used to make Topological Fuzzy Voronoi atoms.
   ! "DM" is the density matrix used to calculated the electron
   ! density. The code is accurate to 1/2000-th of the separation
   ! R(a-b). The code ensures that R(a.b) + R(b.a) = R(a-b). 
   ! Options:
   ! . "n_div"  [50] is the number of divisions used to detect minima.
   ! . "n_sub" [400] is the no. of subdivisions to refine the result.
   ! NOTE: Code was extracted from APOST3D and contains the original

   ! Ramos-Cordoba radii. The method accounts for numerical errors and
   ! instabilities in the internuclear density and atoms too far away.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      INT, optional, IN :: n_div
      INT, optional, IN :: n_sub
      MAT(REAL,:,:), IN :: DM
      REAL :: res
      VEC(REAL,3) :: ra,rb,del
      VEC(REAL,3) :: rc,rd
      VEC(REAL,:), allocatable :: rho
      VEC(INT,:), allocatable :: kmin,kmax
      INT :: nmin,nmax
      INT :: ndiv,nsub
      REAL :: step,subs
      INT :: k,km,kf
      INT :: midd,maxd
      INT :: best,diff
      BIN :: is_min
      BIN :: swap
      INT :: Za,Zb
   ENSURE(a/=b,"MOLECULE.RHO:Mayer_Salvador_radius_for_1 ... atoms should not be the same")

      ! Scan resolution: must be even
      ndiv = 50  ! No. of divisions 
      if (present(n_div)) ndiv = n_div

      ! Polishing resolution: also even
      nsub = 400  ! No. of subdivisions
      if (present(n_sub)) nsub = n_sub

      ! Are atoms too far?
      ra = self%atom(a)%position
      rb = self%atom(b)%position
      if (norm2(rb-ra)>TEN) then
         res = HALF  ! Divide half-way
         return
      end if

      ! Ensure a has *smaller* atomic number.
      ! Or if equal, index a is less than b.
      ! This ensures R(a.b) + R(b.a) = R(a-b)
      Za = self%atom(a)%atomic_number
      Zb = self%atom(b)%atomic_number
      swap = FALSE
      if (Za>Zb) then; swap = TRUE
      else if (Za==Zb AND a>b) then; swap = TRUE
      end if

      ! Swap positions
      if (swap) then
         rc = ra
         ra = rb
         rb = rc
      end if

      ! ED, min & max locations
      call create_(rho,ndiv+1)  ! Note +1
      call create_(kmin,ndiv)  ! idx of k-th min
      call create_(kmax,ndiv)  ! idx of k-th max

      ! Get ED from a->b at ndiv+1 points 
      ! First and last point are on ra & rb
      rho = ED_between_points_(self,ra,rb,ndiv+1,DM)

      ! Find indices of minima & maxima
      nmin = 0
      nmax = 0
      do k = 2,ndiv
         if (rho(k-1)>rho(k) AND rho(k)<rho(k+1)) then

            ! Minima found
            nmin = nmin+1
            kmin(nmin) = k
         end if
         if (rho(k-1)<rho(k) AND rho(k)>rho(k+1)) then

            ! Maxima found
            nmax = nmax+1
            kmax(nmax) = k
         end if
      end do

      ! Sanity check: any critical points?
      ENSURE(nmin>0,"MOLECULE.RHO:Mayer_Salvador_radius_for_1 ... no minima found between the atoms!")
      WARN_IF(nmax>0,"MOLECULE.RHO:Mayer_Salvador_radius_for_1 ... maxima found between the atoms!")
      if (nmin==1 AND nmax==0) then

         ! Easy case: 1 minima, 0 maxima
         km = kmin(1)  ! Index of minimum
         is_min = TRUE  ! Set 1 if maxima
      else if (nmax>0 AND abs(nmin-nmax)==1) then

         ! Troubles.
         ! One more min than max, or vice versa
         if (is_odd_(nmax)) then

            ! Central max selected 
            km = kmax((nmax+1)/2)
            is_min = FALSE
         else

            ! Central min selected. If any.
            ! But there must, if nmax is even!
            km = kmin((nmin+1)/2)
            is_min = TRUE
         end if
      else

         ! Real touble. Inconsistent no. of extrema.
         ! Select best min/max i.e closest to midpoint
         midd = ndiv/2  ! Middle division
         maxd = ndiv  ! Max. diff to midd
         best = midd  ! Start at middle 
         is_min = TRUE

         ! First minima to middle division.
         ! If there is one, it is found
         do k = 1,nmin
            diff = abs(kmin(k)-midd)
            if (diff<maxd) then
               best = kmin(k)  ! Store
               maxd = diff  ! Look no further
               is_min = TRUE  ! Label it
            end if
         end do

         ! Any maxima closer than maxd?
         ! Maxima trump minima.
         do k = 1,nmax
            diff = abs(kmax(k)-midd)
            if (diff<maxd) then
               best = kmax(k)
               maxd = diff
               is_min = FALSE
            end if
         end do

         ! Finally
         km = best
      end if

      ! Extrema index km found.
      ! Go back one step, look ahead two steps
      step = ONE/ndiv  ! Orig. step size
      del = (rb-ra)*step  ! Step vector
      rc = ra+(km-2)*del  ! km-1 is the extrema
      rd = ra+(km)*del  ! *Two* steps ahead
      subs = TWO*step/nsub  ! Substep size, over 2 steps

      ! ED from rc->rd at nsub+1 points 
      ! First & last points on rc & rd
      rho = ED_between_points_(self,rc,rd,nsub+1,DM)

      ! Find first extrema, finer level
      if (is_min) then
         do k = 2,nsub
            kf = k
            if (rho(k-1)>rho(k) AND rho(k)<rho(k+1)) exit
         end do
      else
         do k = 2,nsub
            kf = k
            if (rho(k-1)<rho(k) AND rho(k)>rho(k+1)) exit
         end do
      end if

      ! Steps from ra
      res = (km-2)*step+(kf-1)*subs

      ! Swap back?
      if (swap) res = ONE-res
      ENSURE(res>0.1 AND res<0.9,"MOLECULE.RHO:Mayer_Salvador_radius_for_1 ... Too close to nuclei!")
   end function

   PURE subroutine make_Salvador_radii_lmin_0(self)
   ! Put the Salvador & Ramos-Cordoba radii for each atom.
      type(MOLECULE_TYPE), INOUT :: self
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_lmin_0 ... no atoms")
      call make_Salvador_neighbours_(self%atom,self%Salvador_bonds_for)
      call make_Salvador_radii_lmin_(self,self%Salvador_radii_for)
   end subroutine

   PURE subroutine make_Salvador_radii_lmin_1(self,radii_for)
   ! Make the Mayer-Salvador atom radii corresponding to the bonded
   ! neighbours. If the point is outside the range a -ve radii is
   ! returned then it is not between those atoms, or too close.
   ! NOTE: This version uses canned line minimisation routines.
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: radii_for
      VEC(type(EVEC_INT_TYPE),:), allocatable :: bonds_for
      INT :: n_bonds
      INT :: a,b,c
      VEC(REAL,3) :: ra,rc
      VEC(REAL,3) :: r1,r2
      VEC(REAL,3) :: rac,dir
      REAL :: E1,L,EDmin,r
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_lmin_1 ... no atoms")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Salvador_radii_lmin_1 ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_Salvador_radii_lmin_1 ... no restricted density_mx")
      call make_Salvador_neighbours_(self%atom,bonds_for)

      ! Create radii table
      call create_(radii_for,size(self%atom))

      ! Atom a
      do a = 1,size(self%atom)
         n_bonds = size(bonds_for(a)%element)
         call create_(radii_for(a)%element,n_bonds)
         ra = self%atom(a)%position
         ! Bonded atom c
         do b = 1,n_bonds
            c = bonds_for(a)%element(b)
            rc = self%atom(c)%position
            rac = rc-ra
            r1 = ra
            r2 = ra+HALF*rac
            dir = r2-r1  ! search direction
            E1 = ED_at_point_(self,r1)
            call min_line_(self,ED_at_point,r1,dir,EDmin,TOL(4),E1,L)
            r = norm2(dir*L)
            if (L<0.05 OR L>1.95) r = -BOHR_PER_ANGSTROM
            radii_for(a)%element(b) = r
         end do
      end do
   end subroutine

   PURE subroutine make_Salvador_radii_pro(self,radii_for,bonds_for,DM,n_div,n_sub)
   ! Make the Mayer-Salvador atom radii corresponding to the
   ! bonded neighbours (previously calculated!) for the Topological
   ! Fuzzy Voronoi Atoms, for the electron density obtained from
   ! density matrix "DM".
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: radii_for
      VEC(type(EVEC_INT_TYPE),:), allocatable, IN :: bonds_for
      MAT(REAL,:,:), IN :: DM
      INT, optional, IN :: n_div
      INT, optional, IN :: n_sub
      INT :: n_atoms
      INT :: n_bonds
      INT :: a,b,n
      VEC(REAL,3) :: ra,rb
      REAL :: fac
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_pro ... no atoms")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Salvador_radii_pro ... no density_mx")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_Salvador_radii_pro ... no restricted density_mx")

      ! Create radii table
      n_atoms = size(self%atom)
      call create_(radii_for,n_atoms)

      ! Atom a
      do a = 1,n_atoms
         n_bonds = size(bonds_for(a)%element)
         call create_(radii_for(a)%element,n_bonds)
         ra = self%atom(a)%position
         ! Neighbours b
         do n = 1,n_bonds
            b = bonds_for(a)%element(n)
            rb = self%atom(b)%position
            fac = Mayer_Salvador_radius_for_(self,a,b,n_div,n_sub,DM)
            radii_for(a)%element(n) = fac*norm2(rb-ra)
         end do
      end do
   end subroutine

   PURE subroutine make_Salvador_radii_hir(self,radii_for,bonds_for)
   ! Make the atom radii for the bonded neighbours (previously
   ! calculated!) with each boundary where the two atoms' spherical ANO
   ! densities are equal.
      type(MOLECULE_TYPE), IN :: self
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: radii_for
      VEC(type(EVEC_INT_TYPE),:), allocatable, IN :: bonds_for
      INT :: n_atoms,n_bonds,a,b,n
      VEC(REAL,3) :: ra,rb
      REAL :: fac
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_radii_hir ... no atoms")
      n_atoms = size(self%atom)
      call create_(radii_for,n_atoms)
      do a = 1,n_atoms
         n_bonds = size(bonds_for(a)%element)
         call create_(radii_for(a)%element,n_bonds)
         ra = self%atom(a)%position
         do n = 1,n_bonds
            b = bonds_for(a)%element(n)
            rb = self%atom(b)%position
            fac = Hirshfeld_half_radius_for_(self,a,b)
            radii_for(a)%element(n) = fac*norm2(rb-ra)
         end do
      end do
   end subroutine

   PURE function Hirshfeld_half_radius_for(self,a,b) result (res)
   ! Return the fraction of the a-b distance, from "a", at which the
   ! spherical ANO densities of "a" and "b" are equal: where the pair's
   ! Hirshfeld weight for "a" is one half. Found by bisection to 1e-12 of
   ! the distance, so R(a.b) + R(b.a) = R(a-b) by construction. Atoms more
   ! than 10 bohr apart are divided half way, as Mayer_Salvador_radius_for
   ! does.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      REAL :: res
      REAL :: d,lo,hi,s,f
      INT :: i
   ENSURE(a/=b,"MOLECULE.RHO:Hirshfeld_half_radius_for ... atoms should not be the same")
      d = norm2(self%atom(b)%position-self%atom(a)%position)
      if (d>TEN) then
         res = HALF
         return
      end if

      ! rho_a - rho_b falls monotonically along the bond: bisect its root
      lo = ZERO
      hi = ONE
      do i = 1,40
         s = HALF*(lo+hi)
         f = density_at_radius_(self%atom(a),s*d)-density_at_radius_(self%atom(b),(ONE-s)*d)
         if (f>ZERO) then; lo = s
         else; hi = s
         end if
      end do
      res = HALF*(lo+hi)
   end function

   subroutine put_Salvador_radii(self)
   ! Put the Salvador & Ramos-Cordoba radii for each atom.
   ! The neighbours and radii should have veen made already.
   ! Promolecule and equal-density radii are made here, only for comparison.
      type(MOLECULE_TYPE), IN :: self
      type(OPMATRIX_TYPE), allocatable :: DM
      VEC(type(EVEC_REAL_TYPE),:), allocatable :: radii_pro,radii_hir
   ENSURE(allocated(self%atom),"MOLECULE.RHO:put_Salvador_radii ... no atoms")
   ENSURE(allocated(self%Salvador_bonds_for),"MOLECULE.RHO:put_Salvador_radii ... no Salvador_bonds: make them!")
   ENSURE(allocated(self%Salvador_radii_for),"MOLECULE.RHO:put_Salvador_radii ... no Salvador_radii: make them!")

      ! Make promolecule density
      call add_ANO_densities_(self,DM)
      call convert_to_(DM,"r",factor=HALF)

      ! Make promolecule radii, and the equal-density radii
      call make_Salvador_radii_pro_(self,radii_pro,self%Salvador_bonds_for,DM%r)
      call make_Salvador_radii_hir_(self,radii_hir,self%Salvador_bonds_for)

      ! Put table
      call put_Salvador_radii_table_(self%atom,self%Salvador_bonds_for,self%Salvador_radii_for,radii_pro,radii_hir)

      ! Clean
      call destroy_(radii_hir)
      call destroy_(radii_pro)
      call destroy_(DM)
   end subroutine


!  ===================================
!  Spherically averaged atomic density
!  ===================================

   PURE function SA_cell_fn_between_atoms(self,a,b,n_pts) result (res)
   ! Return ED on equally spaced grid points between "ra" and "rb"
   ! as the first and last points, inclusive. "DM" is the density
   ! matrix used to calculated the electron density.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      INT, IN :: n_pts
      VEC(REAL,n_pts) :: res
      MAT(REAL,:,:), allocatable :: pts
      VEC(REAL,3) :: ra
      VEC(REAL,3) :: rb
      VEC(REAL,3) :: del
      VEC(REAL,3) :: p
      INT :: i
   ENSURE(n_pts>3,"MOLECULE.RHO:SA_cell_fn_between_atoms ... too few points")
      call create_(pts,n_pts,3)

      ! Step vector
      ra = self%atom(a)%position
      rb = self%atom(b)%position
      del = rb-ra
      del = del/(n_pts-1)

      ! Line of points
      p = ra
      do i = 1,n_pts
         pts(i,1) = p(1)
         pts(i,2) = p(2)
         pts(i,3) = p(3)
         p = p+del
      end do

      ! Cell fn. "a"
      call make_Salvador_cell_fn_(self%becke_grid,res,pts,a)

      ! Clean
      call destroy_(pts)
   end function

   subroutine make_SA_cell_fn_interp_between_atoms(self,a,b)
   ! Make the .interpolator for the ED between two atoms "a" and "b"
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a,b
      INT :: n_pts
      VEC(REAL,:), allocatable :: pts
      VEC(REAL,:), allocatable :: line
      VEC(REAL,3) :: ra,rb
      VEC(REAL,3) :: del
      REAL :: stp,val
      INT :: i
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_SA_cell_fn_interp_between_atoms ... no atoms")
   ENSURE(allocated(self%interpolator),"MOLECULE.RHO:make_SA_cell_fn_interp_between_atoms ... no interpolator info, specify n_data=")
   ENSURE(self%interpolator%n_data>0,"MOLECULE.RHO:make_SA_cell_fn_interp_between_atoms ... wrong no. of data, interpolator")

      ! How many points?
      n_pts = self%interpolator%n_data

      ! Positions
      ra = self%atom(a)%position
      rb = self%atom(b)%position

      ! Step vector
      del = rb-ra
      del = del/(n_pts-1)
      stp = norm2(del)

      ! Points as steps
      call create_(pts,n_pts)
      val = ZERO
      do i = 1,n_pts
         pts(i) = val
         val = val+stp
      end do

      ! Make ED line values
      line = SA_cell_fn_between_atoms_(self,a,b,n_pts)

      ! Set domain scale factor. May not be required.
      call set_data_points_(self%interpolator,pts)
      call set_data_values_(self%interpolator,line)

      ! Clean
      call destroy_(line)
      call destroy_(pts)
   end subroutine

   PURE function SA_ED_between_atoms(self,a,b,n_pts) result (res)
   ! Return ED on equally spaced grid points between "ra" and "rb"
   ! as the first and last points, inclusive. "DM" is the density
   ! matrix used to calculated the electron density.
      type(MOLECULE_TYPE), IN :: self
      INT, IN :: a,b
      INT, IN :: n_pts
      VEC(REAL,n_pts) :: res
      VEC(REAL,:), allocatable :: ED
      MAT(REAL,:,:), allocatable :: pts
      VEC(REAL,3) :: ra
      VEC(REAL,3) :: rb
      VEC(REAL,3) :: del
      VEC(REAL,3) :: p
      INT :: i
   ENSURE(n_pts>3,"MOLECULE.RHO:SA_ED_between_atoms ... too few points")
      call create_(pts,n_pts,3)

      ! Step vector
      ra = self%atom(a)%position
      rb = self%atom(b)%position
      del = rb-ra
      del = del/(n_pts-1)

      ! Line of points
      p = ra
      do i = 1,n_pts
         pts(i,1) = p(1)
         pts(i,2) = p(2)
         pts(i,3) = p(3)
         p = p+del
      end do

      ! Cell fn. "a"
      call make_Salvador_cell_fn_(self%becke_grid,res,pts,a)

      ! ED
      call create_(ED,n_pts)
      ED = ED_between_points_(self,ra,rb,n_pts)

      ! Multiply
      res = ED*res

      ! Clean
      call destroy_(ED)
      call destroy_(pts)
   end function

   subroutine make_SA_ED_interp_between_atoms(self,a,b)
   ! Make the .interpolator for the ED between two atoms "a" and "b"
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a,b
      INT :: n_pts
      VEC(REAL,:), allocatable :: pts
      VEC(REAL,:), allocatable :: line
      VEC(REAL,3) :: ra,rb
      VEC(REAL,3) :: del
      REAL :: stp,val
      INT :: i
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_SA_ED_interp_between_atoms ... no atoms")
   ENSURE(allocated(self%interpolator),"MOLECULE.RHO:make_SA_ED_interp_between_atoms ... no interpolator info, specify n_data=")
   ENSURE(self%interpolator%n_data>0,"MOLECULE.RHO:make_SA_ED_interp_between_atoms ... wrong no. of data, interpolator")

      ! How many points?
      n_pts = self%interpolator%n_data

      ! Positions
      ra = self%atom(a)%position
      rb = self%atom(b)%position

      ! Step vector
      del = rb-ra
      del = del/(n_pts-1)
      stp = norm2(del)

      ! Points as steps
      call create_(pts,n_pts)
      val = ZERO
      do i = 1,n_pts
         pts(i) = val
         val = val+stp
      end do

      ! Make ED line values
      line = SA_ED_between_atoms_(self,a,b,n_pts)

      ! Set domain scale factor. May not be required.
      call set_data_points_(self%interpolator,pts)
      call set_data_values_(self%interpolator,line)

      ! Clean
      call destroy_(line)
      call destroy_(pts)
   end subroutine

   PURE subroutine make_sph_avgd_SA_ED_v0(self,avg,pts,c)
   ! Make "avg", the spherically averaged Salvador atom electron
   ! densities for atom "c" evaluated at a set of radial "pts" away
   ! from the nuclear position.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: avg
      VEC(REAL,:), IN :: pts
      INT, IN :: c
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: wts,Wc,N0
      MAT(REAL,:,:), allocatable :: shell
      type(QUADRATURE_TYPE), allocatable :: theta,phi
      INT :: nq,n_pts
      INT :: t,p,n,i
      REAL :: th,ct,st
      REAL :: ph,cp,sp
      REAL :: r,rst
   ENSURE(size(avg)==size(pts),"MOLECULE.RHO:make_sph_avgd_SA_ED_v0 ... incompatible size, avg & pts")

      ! Quadrature pt. density
      nq = 25
      n_pts = 2*nq*nq

      ! Polar angle quadrature
      call create_(theta)
      call set_kind_(theta,"gauss_legendre")
      call set_n_pts_(theta,nq)
      call set_quadrature_points_(theta)
      call set_limits_(theta,[ZERO,PI])  ! <---

      ! Azimuth - twice as many pts
      call create_(phi)
      call set_kind_(phi,"gauss_legendre")
      call set_n_pts_(phi,2*nq)
      call set_quadrature_points_(phi)
      call set_limits_(phi,[ZERO,2*PI])  ! <---

      ! Atom "c" position
      rc = self%atom(c)%position

      ! Radial pts
      do i = 1,size(pts)

         ! Radius
         r = pts(i)

         ! Shell radius "r"
         call create_(shell,n_pts,3)
         call create_(wts,n_pts)  ! Quad. weights
         call create_(Wc,n_pts)  ! Salvador atom
         n = 0
         do t = 1,theta%n_pts
            do p = 1,phi%n_pts
               n = n+1
               th = theta%pts(t)
               ph = phi%pts(p)
               ct = cos(th)
               st = sin(th)
               cp = cos(ph)
               sp = sin(ph)
            ! Spherical shell around "rc"
               rst = r*st  ! <---
               shell(n,1) = rst*cp+rc(1)
               shell(n,2) = rst*sp+rc(2)
               shell(n,3) = r*ct+rc(3)
            ! The area x weights
               wts(n) = st*theta%wts(t)*phi%wts(p)
            end do
         end do

         ! Salvador atom "Wc" on "shell"
         call make_Salvador_cell_fn_(self%becke_grid,Wc,shell,c)

         ! Prune "shell"
         wts = Wc*wts  ! <---
      ! .becke_grid.prune_grid(shell,wts)

         ! ED for atom "c" on shell
         call create_(N0,size(shell,1))
         call make_ED_grid_r_(self,N0,shell)

         ! Population of shell x 4 PI
         avg(i) = sum(N0*wts)  ! Integrate

         ! Clean
         call destroy_(N0)
         call destroy_(Wc)
         call destroy_(wts)
         call destroy_(shell)
      end do

      ! Clean
      call destroy_(phi)
      call destroy_(theta)

      ! Scale
      avg = avg/(FOUR*PI)
   end subroutine

   PURE subroutine make_sph_avgd_SA_ED_v1(self,avg,pts,c)
   ! Make "avg", the spherically averaged Salvador atom electron
   ! densities for atom "c" at the radii "pts" of its Becke radial
   ! shells, averaging over each shell's Lebedev grid. The Becke
   ! partition weights are not used: the average is over the whole
   ! sphere, and the Salvador weight takes it to zero at other nuclei.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: avg
      VEC(REAL,:), IN :: pts
      INT, IN :: c
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: Wc,N0
      MAT(REAL,:,:), allocatable :: shell
      type(LEBEDEV_TYPE), allocatable :: lebedev
      INT :: i,j
   ENSURE(size(avg)==size(pts),"MOLECULE.RHO:make_sph_avgd_SA_ED_v1 ... incompatible size, avg & pts")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_sph_avgd_SA_ED_v1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_sph_avgd_SA_ED_v1 ... becke_grid not finalized")
      rc = self%atom(c)%position
      do i = 1,size(pts)

         ! Shell "i" of radius pts(i) around atom "c"
         lebedev = angular_grid_for_atom_(self%becke_grid,c,i)
         shell = pts(i)*lebedev%pts
         do j = 1,3
            shell(:,j) = shell(:,j)+rc(j)
         end do

         ! Salvador atom "c" density on the shell
         call create_(Wc,size(shell,1))
         call make_Salvador_cell_fn_(self%becke_grid,Wc,shell,c)
         call create_(N0,size(shell,1))
         call make_ED_grid_r_(self,N0,shell)

         ! Average: the Lebedev weights sum to one
         avg(i) = sum(lebedev%wts*Wc*N0)
         call destroy_(N0)
         call destroy_(Wc)
         call destroy_(shell)
         call destroy_(lebedev)
      end do
   end subroutine

   PURE subroutine make_sph_avgd_HA_ED_v1(self,avg,pts,c)
   ! Make "avg", the spherically averaged (exponential) Hirshfeld atom
   ! electron densities for atom "c" at the radii "pts" of its Becke
   ! radial shells, averaging over each shell's Lebedev grid. The
   ! stockholder weight is that of make_stockholder_atom_weight, so it
   ! carries the oc-exphar power.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: avg
      VEC(REAL,:), IN :: pts
      INT, IN :: c
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: Wc,N0
      MAT(REAL,:,:), allocatable :: shell
      type(LEBEDEV_TYPE), allocatable :: lebedev
      INT :: i,j
   ENSURE(size(avg)==size(pts),"MOLECULE.RHO:make_sph_avgd_HA_ED_v1 ... incompatible size, avg & pts")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_sph_avgd_HA_ED_v1 ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_sph_avgd_HA_ED_v1 ... becke_grid not finalized")
      rc = self%atom(c)%position
      do i = 1,size(pts)

         ! Shell "i" of radius pts(i) around atom "c"
         lebedev = angular_grid_for_atom_(self%becke_grid,c,i)
         shell = pts(i)*lebedev%pts
         do j = 1,3
            shell(:,j) = shell(:,j)+rc(j)
         end do

         ! Stockholder atom "c" density on the shell
         call create_(Wc,size(shell,1))
         call make_stockholder_atom_weight_(self,Wc,c,shell)
         call create_(N0,size(shell,1))
         call make_ED_grid_r_(self,N0,shell)

         ! Average: the Lebedev weights sum to one
         avg(i) = sum(lebedev%wts*Wc*N0)
         call destroy_(N0)
         call destroy_(Wc)
         call destroy_(shell)
         call destroy_(lebedev)
      end do
   end subroutine

   PURE subroutine make_sph_SA_interp_from_atom(self,a,b,r_max)
   ! Make the Salvador atom ED interpolator from atom "a". 
   ! The furthest neighbour atom index "b" is also returned.
   ! The no. of points "n_pts" must be given in an .interpolator.
   ! The distance away to plot is "r_max", if specified.
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: a
      INT, OUT :: b
      REAL, optional, IN :: r_max
      VEC(REAL,:), allocatable :: line
      VEC(REAL,:), allocatable :: pts
      VEC(INT,:), allocatable :: neigh
      REAL :: del,p
      REAL :: rmax
      REAL :: dist
      INT :: i,x
      INT :: n_pts
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_sph_SA_interp_from_atom ... no atoms")
   ENSURE(size(self%atom)>1,"MOLECULE.RHO:make_sph_SA_interp_from_atom ... must have more than one atom.")
   ENSURE(allocated(self%Salvador_bonds_for),"MOLECULE.RHO:make_sph_SA_interp_from_atom ... no Salvador_bonds: make them!")
   ENSURE(allocated(self%interpolator),"MOLECULE.RHO:make_sph_SA_interp_from_atom ... no interpolator info, specify n_data=")
   ENSURE(self%interpolator%n_data>0,"MOLECULE.RHO:make_sph_SA_interp_from_atom ... wrong no. of data, interpolator")

      ! How many points?
      n_pts = self%interpolator%n_data

      ! Find furthest neighbour
      b = 0
      rmax = ZERO
      neigh = self%Salvador_bonds_for(a)%element
      do i = 1,size(neigh)
         x = neigh(i)
         dist = distance_between_(self%atom,a,x)
         if (dist>rmax) then
            b = x
            rmax = dist
         end if
      end do
   ! rmax = .atom.distance_between(a,x)

      ! Override r_max?
      if (present(r_max)) then
         rmax = r_max
      end if

      ! line of ED values
      call create_(pts,n_pts)
      call create_(line,n_pts)

      ! Step size
      del = rmax/(n_pts-1)

      ! Line of points from "ra"
      p = ZERO
      do i = 1,n_pts
         pts(i) = p
         p = p+del
      end do

      ! Make the density
      call make_sph_avgd_SA_ED_v0_(self,line,pts,a)

      ! Set domain scale factor. May not be required.
      call set_data_points_(self%interpolator,pts)
      call set_data_values_(self%interpolator,line)

      ! Clean
      call destroy_(line)
      call destroy_(pts)
   end subroutine


!  ====================
!  Atom partition grids
!  ====================

! These routines include Becke grid weights

   PURE subroutine make_Hirshfeld_atom_ED_grid(self,N0,pts,c,DM)
   ! Return "N0", the Hirshfeld atom electron density (ED) grid for
   ! atom "c" weighted by the Becke grid wts; and the pruned Becke
   ! grid "pts" on which N0 is evaluated. "DM" is the density mx.
   ! NOTE: sum(N0) is the electron population for atom "c". 
   ! NOTE: Rewritten to use Kang's code.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: N0
      MAT(REAL,:,:), allocatable, OUT :: pts
      INT, IN :: c
      MAT(REAL,:,:), IN :: DM
      VEC(REAL,:), allocatable :: wts
      REAL :: cutoff
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_Hirshfeld_atom_ED_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Hirshfeld_atom_ED_grid ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Hirshfeld_atom_ED_grid ... no density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_Hirshfeld_atom_ED_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_Hirshfeld_atom_ED_grid ... no finalized becke_grid")

      ! Get the Becke grid for atom "c"
      call make_atom_grid_(self%becke_grid,pts,wts,c)

      ! Make stockholder weight function "Wc" for atom "c"
      ! If using interpolatators, then overlapping_atoms
      ! are not needed ... otherwise nothing is skipped
      call apply_stockholder_atom_weight_(self,wts,c,pts)

      ! Prune the grid with new "wts"
      call prune_grid_(self%becke_grid,pts,wts)

      ! Density grid for atom "c"
      call create_(N0,size(pts,1))
      cutoff = self%becke_grid%basis_fn_cutoff
      call make_ED_grid_r_v2_(self,N0,pts,DM,cutoff)

      ! N0 x weight for atom "c"
      N0 = N0*wts

      ! Clean
      call destroy_(wts)
   end subroutine

   PURE subroutine make_Becke_atom_ED_grid(self,N0,pts,c,DM)
   ! Return "N0", the Becke atom electron density (ED) grid for
   ! atom "c" weighted by the Becke grid wts; and the pruned Becke
   ! grid "pts" on which N0 is evaluated. "DM" is the density mx.
   ! NOTE: sum(N0) is the electron population for atom "c". 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: N0
      MAT(REAL,:,:), allocatable, OUT :: pts
      INT, IN :: c
      MAT(REAL,:,:), IN :: DM
      VEC(REAL,:), allocatable :: wts
      REAL :: cutoff
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_Becke_atom_ED_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Becke_atom_ED_grid ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Becke_atom_ED_grid ... no density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_Becke_atom_ED_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_Becke_atom_ED_grid ... no finalized becke_grid")

      ! Get the Stratmann-Scuseria grid for atom "c"
      ! Actually this is with a=ONE.
      call make_Becke_atom_grid_(self%becke_grid,pts,wts,c)

      ! Prune the grid with new "wts"
      call prune_grid_(self%becke_grid,pts,wts)

      ! Density grid for atom "c"
      call create_(N0,size(pts,1))
      cutoff = self%becke_grid%basis_fn_cutoff
      call make_ED_grid_r_v2_(self,N0,pts,DM,cutoff)  ! Kang

      ! N0*Wc, weights for atom "c"
      N0 = N0*wts

      ! Clean
      call destroy_(wts)
   end subroutine

   PURE subroutine make_Stratmann_atom_ED_grid(self,N0,pts,c,DM)
   ! Return "N0", the Stratmann-Scuseria atom electron density (ED)
   ! grid for atom "c" weighted by the Becke grid wts; and the pruned
   ! Becke grid "pts" on which N0 is evaluated. "DM", the density mx.
   ! NOTE: sum(N0) is the electron population for atom "c". 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: N0
      MAT(REAL,:,:), allocatable, OUT :: pts
      INT, IN :: c
      MAT(REAL,:,:), IN :: DM
      VEC(REAL,:), allocatable :: wts
      REAL :: cutoff
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_Stratmann_atom_ED_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Stratmann_atom_ED_grid ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Stratmann_atom_ED_grid ... no density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_Stratmann_atom_ED_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_Stratmann_atom_ED_grid ... no finalized becke_grid")

      ! Get the Stratmann-Scuseria grid for atom "c"
      ! Actually this is with a=ONE.
      ! Becke grid "wts" are multiplied by normalized Voronoi cell fn "W"
      call make_SS1_atom_grid_(self%becke_grid,pts,wts,c)

      ! Prune the grid with new "wts"
      call prune_grid_(self%becke_grid,pts,wts)

      ! Density grid for atom "c"
      call create_(N0,size(pts,1))
      cutoff = self%becke_grid%basis_fn_cutoff
      call make_ED_grid_r_v2_(self,N0,pts,DM,cutoff)  ! Kang

      ! N0*Wc, weights for atom "c"
      N0 = N0*wts

      ! Clean
      call destroy_(wts)
   end subroutine

   PURE subroutine make_Salvador_atom_ED_grid(self,N0,pts,c,DM)
   ! Return "N0", the Salvador atom electron density (ED) grid for
   ! atom "c" weighted by the Becke grid wts; and the pruned Becke
   ! grid "pts" on which N0 is evaluated. "DM" is the density mx.
   ! NOTE: sum(N0) is the electron population for atom "c". 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: N0
      MAT(REAL,:,:), allocatable, OUT :: pts
      INT, IN :: c
      MAT(REAL,:,:), IN :: DM
      VEC(REAL,:), allocatable :: wts
      REAL :: cutoff
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_Salvador_atom_ED_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_atom_ED_grid ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Salvador_atom_ED_grid ... no density matrix")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_Salvador_atom_ED_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_Salvador_atom_ED_grid ... no finalized becke_grid")

      ! Get the Salvador & Ramos-Cordoba grid for atom "c"
      ! Becke grid "wts" are multiplied by normalized Voronoi cell fn "W"
      call make_Salvador_atom_grid_v0_(self%becke_grid,pts,wts,c)
   ! .becke_grid.make_Salvador_atom_grid_v1(pts,wts,c) ! NOT WORKING

      ! Prune the grid with new "wts"
      call prune_grid_(self%becke_grid,pts,wts)

      ! Density grid for atom "c"
      call create_(N0,size(pts,1))
      cutoff = self%becke_grid%basis_fn_cutoff
      call make_ED_grid_r_v2_(self,N0,pts,DM,cutoff)  ! Kang

      ! N0*Wc, weights for atom "c"
      N0 = N0*wts

      ! Clean
      call destroy_(wts)
   end subroutine

   PURE subroutine make_sph_avgd_SA_ED_grid(self,N0,pts,c)
   ! Return "N0", the spherically averaged Salvador atom electron
   ! density (ED) grid for atom "c" *weighted* by the Becke radial
   ! grid wts; and return the radial grid points as well on which N0
   ! is evaluated. 
   ! NOTE: the radial grid points are from the origin.
   ! NOTE: sum(N0) is the electron population for atom "c". 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: N0
      VEC(REAL,:), allocatable, OUT :: pts
      INT, IN :: c
      type(QUADRATURE_TYPE), allocatable :: rgrid
      VEC(REAL,:), allocatable :: wts
      INT :: i,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_sph_avgd_SA_ED_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_sph_avgd_SA_ED_grid ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_sph_avgd_SA_ED_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_sph_avgd_SA_ED_grid ... no finalized becke_grid")

      ! Number of radial shells
      rgrid = radial_grid_for_atom_(self%becke_grid,c)
      n = rgrid%n_pts
      call destroy_(rgrid)

      ! Radii, and weights 4 pi r^2 dr with the atomic scale factor
      call create_(pts,n)
      call create_(wts,n)
      do i = 1,n
         call radial_shell_for_atom_(self%becke_grid,c,i,pts(i),wts(i))
      end do

      ! Averaged densities
      call create_(N0,n)
      call make_sph_avgd_SA_ED_v1_(self,N0,pts,c)

      ! N0 x radial weights
      N0 = N0*wts

      ! Clean
      call destroy_(wts)
   end subroutine

   PURE subroutine make_sph_avgd_HA_ED_grid(self,N0,pts,c)
   ! Return "N0", the spherically averaged (exponential) Hirshfeld atom
   ! density (ED) grid for atom "c" *weighted* by the Becke radial
   ! grid wts; and return the radial grid points as well on which N0
   ! is evaluated. 
   ! NOTE: the radial grid points are from the origin.
   ! NOTE: sum(N0) is the electron population for atom "c". 
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: N0
      VEC(REAL,:), allocatable, OUT :: pts
      INT, IN :: c
      type(QUADRATURE_TYPE), allocatable :: rgrid
      VEC(REAL,:), allocatable :: wts
      INT :: i,n
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_sph_avgd_HA_ED_grid ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_sph_avgd_HA_ED_grid ... no atom list")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_sph_avgd_HA_ED_grid ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_sph_avgd_HA_ED_grid ... no finalized becke_grid")

      ! Number of radial shells
      rgrid = radial_grid_for_atom_(self%becke_grid,c)
      n = rgrid%n_pts
      call destroy_(rgrid)

      ! Radii, and weights 4 pi r^2 dr with the atomic scale factor
      call create_(pts,n)
      call create_(wts,n)
      do i = 1,n
         call radial_shell_for_atom_(self%becke_grid,c,i,pts(i),wts(i))
      end do

      ! Averaged densities
      call create_(N0,n)
      call make_sph_avgd_HA_ED_v1_(self,N0,pts,c)

      ! N0 x radial weights
      N0 = N0*wts

      ! Clean
      call destroy_(wts)
   end subroutine

!  =================
!  Charges & moments
!  =================

   subroutine make_r_HA_moments(self,spin,output)
   ! Make the restricted atomic moments for some partitioning.
   ! NOTE: the charges & dipoles are set for every .atom 
   !       and then propagated to .asymmetric_unit_atoms.
   !       Needed for core-matrix-cluster-of-charges method
   ! NOTE: Moments aren't printed, but total no. of electrons
   !       is printed as a check unless supressed via "output".
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: spin
      BIN, optional, IN :: output
      MAT(REAL,:,:), allocatable :: pts,DM
      VEC(REAL,:), allocatable :: N0
      VEC(REAL,:), allocatable :: r1,r2,r3
      REAL :: c1,c2,c3
      REAL :: q,n_e
      VEC(REAL,3) :: dipole
      MAT(REAL,3,3) :: quadru
      BIN :: spn,out
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_r_HA_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_r_HA_moments ... no atom list")
   ENSURE(DFT_grid_info_made_(self),"MOLECULE.RHO:make_r_HA_moments ... no becke_grid")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_r_HA_moments ... no density matrix")

      ! The following code is inherited from MOLECULE.RHO
      spn = FALSE
      if (present(spin)) spn = spin
      out = FALSE
      if (present(output)) out = output

      ! Make the AO density matrix
      call create_(DM,self%n_bf,self%n_bf)
      if (spn) then; call make_r_Sz_density_mx_(self,DM)
      else; call make_r_density_mx_(self,DM)
      end if

      ! Total no of electrons (check)
      n_e = ZERO

      ! Loop over atoms "c"
      do c = 1,self%n_atom
         if (self%atom(c)%atomic_number<1) cycle

         ! Get density N0 for atom "c"
         call make_Hirshfeld_atom_ED_grid_(self,N0,pts,c,DM)

         ! Integration points
         r1 = pts(:,1)
         r2 = pts(:,2)
         r3 = pts(:,3)

         ! Atom c position
         c1 = self%atom(c)%position(1)
         c2 = self%atom(c)%position(2)
         c3 = self%atom(c)%position(3)

         ! Make moments
         q = sum(N0)
         n_e = n_e+q
         dipole(1) = sum(N0*(r1-c1))
         dipole(2) = sum(N0*(r2-c2))
         dipole(3) = sum(N0*(r3-c3))
         quadru(1,1) = sum(N0*(r1-c1)*(r1-c1))
         quadru(2,2) = sum(N0*(r2-c2)*(r2-c2))
         quadru(3,3) = sum(N0*(r3-c3)*(r3-c3))
         quadru(1,2) = sum(N0*(r1-c1)*(r2-c2))
         quadru(1,3) = sum(N0*(r1-c1)*(r3-c3))
         quadru(2,3) = sum(N0*(r2-c2)*(r3-c3))
         quadru(2,1) = quadru(1,2)
         quadru(3,1) = quadru(1,3)
         quadru(3,2) = quadru(2,3)

         ! charges and moments
         q = -q+self%atom(c)%atomic_number
         dipole = -dipole
         quadru = -quadru

         ! Assign the charges and moments
         call set_charge_(self%atom(c),q)
         call set_dipole_(self%atom(c),dipole)
         call set_quadrupole_(self%atom(c),quadru)

         ! Clean up
         call destroy_(pts)
         call destroy_(N0)
      end do  ! -- integration atom c

      ! Clean up
      call destroy_(DM)

      ! Check integration accuracy
      if (out) then
         call flush_(stdout)
         call text_(stdout,"Making Hirshfeld atom moments ...")
         call flush_(stdout)
         call show_(stdout,"Check: no. of electrons =",n_e)
      end if

      ! Put charges to asymmetric unit
      if (allocated(self%crystal)) then
         call destroy_asymmetric_unit_(self%crystal)
         call update_fragment_info_(self%crystal,self%atom)
      end if
   end subroutine

   subroutine make_r_BA_moments(self,spin,output)
   ! Make the restricted atomic moments for some partitioning.
   ! NOTE: the charges & dipoles are set for every .atom 
   !       and then propagated to .asymmetric_unit_atoms.
   !       Needed for core-matrix-cluster-of-charges method
   ! NOTE: Moments aren't printed, but total no. of electrons
   !       is printed as a check unless supressed via "output".
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: spin
      BIN, optional, IN :: output
      MAT(REAL,:,:), allocatable :: pts,DM
      VEC(REAL,:), allocatable :: N0
      VEC(REAL,:), allocatable :: r1,r2,r3
      REAL :: c1,c2,c3
      REAL :: q,n_e
      VEC(REAL,3) :: dipole
      MAT(REAL,3,3) :: quadru
      BIN :: spn,out
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_r_BA_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_r_BA_moments ... no atom list")
   ENSURE(DFT_grid_info_made_(self),"MOLECULE.RHO:make_r_BA_moments ... no becke_grid")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_r_BA_moments ... no density matrix")

      ! The following code is inherited from MOLECULE.RHO
      spn = FALSE
      if (present(spin)) spn = spin
      out = FALSE
      if (present(output)) out = output

      ! Make the AO density matrix
      call create_(DM,self%n_bf,self%n_bf)
      if (spn) then; call make_r_Sz_density_mx_(self,DM)
      else; call make_r_density_mx_(self,DM)
      end if

      ! Total no of electrons (check)
      n_e = ZERO

      ! Loop over atoms "c"
      do c = 1,self%n_atom
         if (self%atom(c)%atomic_number<1) cycle

         ! Get density N0 for atom "c"
         call make_Becke_atom_ED_grid_(self,N0,pts,c,DM)

         ! Integration points
         r1 = pts(:,1)
         r2 = pts(:,2)
         r3 = pts(:,3)

         ! Atom c position
         c1 = self%atom(c)%position(1)
         c2 = self%atom(c)%position(2)
         c3 = self%atom(c)%position(3)

         ! Make moments
         q = sum(N0)
         n_e = n_e+q
         dipole(1) = sum(N0*(r1-c1))
         dipole(2) = sum(N0*(r2-c2))
         dipole(3) = sum(N0*(r3-c3))
         quadru(1,1) = sum(N0*(r1-c1)*(r1-c1))
         quadru(2,2) = sum(N0*(r2-c2)*(r2-c2))
         quadru(3,3) = sum(N0*(r3-c3)*(r3-c3))
         quadru(1,2) = sum(N0*(r1-c1)*(r2-c2))
         quadru(1,3) = sum(N0*(r1-c1)*(r3-c3))
         quadru(2,3) = sum(N0*(r2-c2)*(r3-c3))
         quadru(2,1) = quadru(1,2)
         quadru(3,1) = quadru(1,3)
         quadru(3,2) = quadru(2,3)

         ! charges and moments
         q = -q+self%atom(c)%atomic_number
         dipole = -dipole
         quadru = -quadru

         ! Assign the charges and moments
         call set_charge_(self%atom(c),q)
         call set_dipole_(self%atom(c),dipole)
         call set_quadrupole_(self%atom(c),quadru)

         ! Clean up
         call destroy_(pts)
         call destroy_(N0)
      end do  ! -- integration atom c

      ! Clean up
      call destroy_(DM)

      ! Check integration accuracy
      if (out) then
         call flush_(stdout)
         call text_(stdout,"Making Becke atom moments ...")
         call flush_(stdout)
         call show_(stdout,"Check: no. of electrons =",n_e)
      end if

      ! Put charges to asymmetric unit
      if (allocated(self%crystal)) then
         call destroy_asymmetric_unit_(self%crystal)
         call update_fragment_info_(self%crystal,self%atom)
      end if
   end subroutine

   subroutine make_r_SS_moments(self,spin,output)
   ! Make the restricted atomic moments for some partitioning.
   ! NOTE: the charges & dipoles are set for every .atom 
   !       and then propagated to .asymmetric_unit_atoms.
   !       Needed for core-matrix-cluster-of-charges method
   ! NOTE: Moments aren't printed, but total no. of electrons
   !       is printed as a check unless supressed via "output".
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: spin
      BIN, optional, IN :: output
      MAT(REAL,:,:), allocatable :: pts,DM
      VEC(REAL,:), allocatable :: N0
      VEC(REAL,:), allocatable :: r1,r2,r3
      REAL :: c1,c2,c3
      REAL :: q,n_e
      VEC(REAL,3) :: dipole
      MAT(REAL,3,3) :: quadru
      BIN :: spn,out
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_r_SS_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_r_SS_moments ... no atom list")
   ENSURE(DFT_grid_info_made_(self),"MOLECULE.RHO:make_r_SS_moments ... no becke_grid")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_r_SS_moments ... no density matrix")

      ! The following code is inherited from MOLECULE.RHO
      spn = FALSE
      if (present(spin)) spn = spin
      out = FALSE
      if (present(output)) out = output

      ! Make the AO density matrix
      call create_(DM,self%n_bf,self%n_bf)
      if (spn) then; call make_r_Sz_density_mx_(self,DM)
      else; call make_r_density_mx_(self,DM)
      end if

      ! Total no of electrons (check)
      n_e = ZERO

      ! Loop over atoms "c"
      do c = 1,self%n_atom
         if (self%atom(c)%atomic_number<1) cycle

         ! Get density N0 for atom "c"
         call make_Stratmann_atom_ED_grid_(self,N0,pts,c,DM)

         ! Integration points
         r1 = pts(:,1)
         r2 = pts(:,2)
         r3 = pts(:,3)

         ! Atom c position
         c1 = self%atom(c)%position(1)
         c2 = self%atom(c)%position(2)
         c3 = self%atom(c)%position(3)

         ! Make moments
         q = sum(N0)
         n_e = n_e+q
         dipole(1) = sum(N0*(r1-c1))
         dipole(2) = sum(N0*(r2-c2))
         dipole(3) = sum(N0*(r3-c3))
         quadru(1,1) = sum(N0*(r1-c1)*(r1-c1))
         quadru(2,2) = sum(N0*(r2-c2)*(r2-c2))
         quadru(3,3) = sum(N0*(r3-c3)*(r3-c3))
         quadru(1,2) = sum(N0*(r1-c1)*(r2-c2))
         quadru(1,3) = sum(N0*(r1-c1)*(r3-c3))
         quadru(2,3) = sum(N0*(r2-c2)*(r3-c3))
         quadru(2,1) = quadru(1,2)
         quadru(3,1) = quadru(1,3)
         quadru(3,2) = quadru(2,3)

         ! charges and moments
         q = -q+self%atom(c)%atomic_number
         dipole = -dipole
         quadru = -quadru

         ! Assign the charges and moments
         call set_charge_(self%atom(c),q)
         call set_dipole_(self%atom(c),dipole)
         call set_quadrupole_(self%atom(c),quadru)

         ! Clean up
         call destroy_(pts)
         call destroy_(N0)
      end do  ! -- integration atom c

      ! Clean up
      call destroy_(DM)

      ! Check integration accuracy
      if (out) then
         call flush_(stdout)
         call text_(stdout,"Making Stratmann atom moments ...")
         call flush_(stdout)
         call show_(stdout,"Check: no. of electrons =",n_e)
      end if

      ! Put charges to asymmetric unit
      if (allocated(self%crystal)) then
         call destroy_asymmetric_unit_(self%crystal)
         call update_fragment_info_(self%crystal,self%atom)
      end if
   end subroutine

   subroutine make_r_SA_moments(self,spin,output)
   ! Make the restricted atomic moments for some partitioning.
   ! NOTE: the charges & dipoles are set for every .atom 
   !       and then propagated to .asymmetric_unit_atoms.
   !       Needed for core-matrix-cluster-of-charges method
   ! NOTE: Moments aren't printed, but total no. of electrons
   !       is printed as a check unless supressed via "output".
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: spin
      BIN, optional, IN :: output
      MAT(REAL,:,:), allocatable :: pts,DM
      VEC(REAL,:), allocatable :: N0
      VEC(REAL,:), allocatable :: r1,r2,r3
      REAL :: c1,c2,c3
      REAL :: q,n_e
      VEC(REAL,3) :: dipole
      MAT(REAL,3,3) :: quadru
      BIN :: spn,out
      INT :: c
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_r_SA_moments ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_r_SA_moments ... no atom list")
   ENSURE(DFT_grid_info_made_(self),"MOLECULE.RHO:make_r_SA_moments ... no becke_grid")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_r_SA_moments ... no density matrix")

      ! The following code is inherited from MOLECULE.RHO
      spn = FALSE
      if (present(spin)) spn = spin
      out = FALSE
      if (present(output)) out = output

      ! Make the AO density matrix
      call create_(DM,self%n_bf,self%n_bf)
      if (spn) then; call make_r_Sz_density_mx_(self,DM)
      else; call make_r_density_mx_(self,DM)
      end if

      ! Total no of electrons (check)
      n_e = ZERO

      ! Loop over atoms "c"
      do c = 1,self%n_atom
         if (self%atom(c)%atomic_number<1) cycle

         ! Get density N0 for atom "c"
         call make_Salvador_atom_ED_grid_(self,N0,pts,c,DM)

         ! Integration points
         r1 = pts(:,1)
         r2 = pts(:,2)
         r3 = pts(:,3)

         ! Atom c position
         c1 = self%atom(c)%position(1)
         c2 = self%atom(c)%position(2)
         c3 = self%atom(c)%position(3)

         ! Make moments
         q = sum(N0)
         n_e = n_e+q
         dipole(1) = sum(N0*(r1-c1))
         dipole(2) = sum(N0*(r2-c2))
         dipole(3) = sum(N0*(r3-c3))
         quadru(1,1) = sum(N0*(r1-c1)*(r1-c1))
         quadru(2,2) = sum(N0*(r2-c2)*(r2-c2))
         quadru(3,3) = sum(N0*(r3-c3)*(r3-c3))
         quadru(1,2) = sum(N0*(r1-c1)*(r2-c2))
         quadru(1,3) = sum(N0*(r1-c1)*(r3-c3))
         quadru(2,3) = sum(N0*(r2-c2)*(r3-c3))
         quadru(2,1) = quadru(1,2)
         quadru(3,1) = quadru(1,3)
         quadru(3,2) = quadru(2,3)

         ! charges and moments
         q = -q+self%atom(c)%atomic_number
         dipole = -dipole
         quadru = -quadru

         ! Assign the charges and moments
         call set_charge_(self%atom(c),q)
         call set_dipole_(self%atom(c),dipole)
         call set_quadrupole_(self%atom(c),quadru)

         ! Clean up
         call destroy_(pts)
         call destroy_(N0)
      end do  ! -- integration atom c

      ! Clean up
      call destroy_(DM)

      ! Check integration accuracy
      if (out) then
         call flush_(stdout)
         call text_(stdout,"Making Salvador atom moments ...")
         call flush_(stdout)
         call show_(stdout,"Check: no. of electrons =",n_e)
      end if

      ! Put charges to asymmetric unit
      if (allocated(self%crystal)) then
         call destroy_asymmetric_unit_(self%crystal)
         call update_fragment_info_(self%crystal,self%atom)
      end if
   end subroutine

   subroutine make_BA_info(self,output)
   ! Make the Becke atomic charges & moments.
   ! Set up Becke grid before (if not there) ready for integration.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_BA_info ... no atoms")
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_BA_info ... no basis info")

      ! Set up integration grid
      call set_up_becke_grid_(self)

      ! Make Becke moments
      call make_r_BA_moments_(self,output=output)  ! NOT PURE
   end subroutine

   subroutine make_SS_info(self,output)
   ! Make the Stratmann-Scuseria atomic charges & moments.
   ! Set up Becke grid before (if not there) ready for integration.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_SS_info ... no atoms")
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_SS_info ... no basis info")

      ! Set up integration grid
      call set_up_becke_grid_(self)

      ! Make Becke moments
      call make_r_SS_moments_(self,output=output)  ! NOT PURE
   end subroutine

   subroutine make_SA_info(self,output)
   ! Make the TFVA atomic charges & moments.
   ! Set up Becke grid before (if not there) ready for integration.
   ! The latter need Salvador neighbors & radii which are also made.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: output
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_SA_info ... no atoms")
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_SA_info ... no basis info")

      ! Set up integration grid
      call set_up_becke_grid_(self)

      ! Make Salvador radii
      call make_Salvador_radii_(self)
      call set_Salvador_bonds_(self%becke_grid,self%Salvador_bonds_for)
      call set_Salvador_radii_(self%becke_grid,self%Salvador_radii_for)
      call set_Salvador_chi_(self%becke_grid,self%Salvador_chi)

      ! Make Salvador moments
      call make_r_SA_moments_(self,output=output)  ! NOT PURE
   end subroutine


!  ============
!  Form factors
!  ============

   PURE subroutine make_unique_IAM_atom_FFs(self,ff)
   ! Get "ff", the IAM structure factors for the symmetry generated
   ! K points with *no* thermal smearing, for every unique atom.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: n_atom,n_k,u,f
      REAL :: rf
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_unique_IAM_atom_FFs ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_unique_IAM_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_unique_IAM_atom_FFs ... no x-ray data")
   ENSURE(allocated(self%crystal%asymmetric_unit_atom),"MOLECULE.RHO:make_unique_IAM_atom_FFs ... no asymmetric unit atoms")
   ENSURE(allocated(self%crystal%unique_atom_for_frag_atom),"MOLECULE.RHO:make_unique_IAM_atom_FFs ... no unique-frag_atom")
   ENSURE(allocated(self%crystal%asym_atom_for_frag_atom),"MOLECULE.RHO:make_unique_IAM_atom_FFs ... no asym-frag_atom")
   ! old :: BIN
   ! rc :: VEC{REAL}(3)
   ! k  :: INT
   ! k1,k2,k3,kr :: REAL
   ! sf :: CPX

      ! Asymmetric unit atoms;
      ! Symmetry generated K points and SF's
      n_atom = size(self%crystal%asymmetric_unit_atom)
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Make the SF's
      ff = ZERO

      ! Loop over integration atoms "c"
      do u = 1,self%crystal%n_unique_frag_atoms

         ! Atoms
         f = self%crystal%unique_frag_atom(u)

         ! Structure factors
         call make_FT_(self%atom(f),ff(:,u),k_pts)

         ! Repetition (site-symmetry) factor
         rf = self%crystal%frag_atom_sym_factor(f)
         if (is_zero_(rf)) cycle
         rf = ONE/rf

         ! Scale
         ff(:,u) = ff(:,u)*rf
      end do

      ! Clean up
      call destroy_(k_pts)
   end subroutine


!   get_Tanaka_atom_FFs(ff_n,spin_density) :: leaky, PURE
!   ! Get "ff_n", the Tanaka *static* atomic form factors for the
!   ! symmetry related K points for every unique atom.
!   ! NOTE: "ff_n" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
!   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
!   ! without the thermal smearing factor.
!      self :: INOUT
!      ff_n :: MAT{CPX}@, OUT
!      spin_density :: BIN, optional, IN
!
!   ENSURE(.basis_info_made,"no basis info")
!   ENSURE(.atom.allocated,"no atom list")
!   ENSURE(.density_mx.allocated, "no density matrix")
!   ENSURE(.crystal.allocated, "no crystal")
!   ENSURE(.crystal.xray_data.allocated, "no reflection data")
!   ENSURE(.crystal.xray_data.allocated, "no x-ray reflection data")
!   ENSURE(.crystal.fragment_info_made, "no crystal fragment info")
!
!      FT :: MAT3{CPX}@
!      k_pts, DM, pf :: MAT{REAL}@
!      sh :: SHELL2
!      n_atom, n_k,k, q :: INT
!      ua,ra,fa,la,na,atom_a :: INT
!      ub,   fb,lb,nb,atom_b :: INT
!      pa,pk  :: VEC{REAL}(3)
!    ! pb     :: VEC{REAL}(3)
!      sa     :: REAL
!      cutoff :: REAL
!      do_it,spin :: BIN
!      II, phase  :: CPX
!
!      ! Asymmetric unit atoms
!      n_atom = .crystal.asymmetric_unit_atom.dim
!
!      ! Symmetry generated K points and SF's
!      n_k = .crystal.n_unique_SF_k_pts
!
!      ! Form factor array (no temp factor) (leaky)
!      ff_n.create(n_k,n_atom)
!      ff_n = IMAGIFY(ZERO)
!      II   = IMAGIFY(ONE)
!
!      ! Make list of symmetry non-equivalent symops
!      ! and the necessary k points
!      k_pts.create(n_k,3)
!      .crystal.make_unique_X_SF_k_pts(k_pts)
!
!      ! X-ray or PND?
!      spin = FALSE
!      if (present(spin_density)) spin = spin_density
!
!      ! Density matrix (leaky)
!      if (NOT spin) then; .make_r_density_mx
!      else;               .make_r_Sz_density_mx
!      end
!      ENSURE(.density_mx.r.allocated, "no restricted DM")
!
!      ! Store density matrix
!      DM.create(.n_bf,.n_bf)
!      DM = .density_mx.r
!
!      ! Too small threshhold
!      cutoff = TOL(10)/.n_shell_pairs
!
!      ! Loop over all shell pairs
!      do q = 1,.n_shell_pairs
!
!         ! Get shell pairs
!         .set_shell2_from(q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
!
!         ! Skip?
!         ua = .crystal.unique_frag_atom.index_of_value(atom_a)
!         ub = .crystal.unique_frag_atom.index_of_value(atom_b)
!         do_it = (ua>0 OR ub>0) AND NOT sh.skip_ft(cutoff)
!
!         ! =============================
!         ! Do (non-zero) asym-atom pairs
!         ! =============================
!         if (do_it) then
!
!            ! Tanaka gaussian-partition factors
!            pf.create(sh.a.n_cc,sh.b.n_cc)
!
!            ! Block of FT basis-function integrals
!            FT.create(n_k,na,nb)
!
!            if (atom_a==atom_b) then ! SAME ATOMS A==B
!
!               ! Site symmetry factor can't be zero
!               pa = .atom(atom_a).position
!               ra = .crystal.frag_atom_sym_factor(atom_a)
!               sa = ONE/ra
!
!               ! Only one atom -- no need to partition
!               pf = ONE
!              
!               ! The static FT integrals 
!               sh.make_FT_parted(FT,k_pts,pf)
!
!               if (fa==fb) then
!                  ! Diagonal shell
!                  do k = 1,n_k
!                     pk = k_pts(k,:)
!                     phase = exp(-II*dot_product(pk,pa))
!                     ff_n(k,ua) = ff_n(k,ua) +     sum(FT(k,:,:)*DM(fa:la,fb:lb))*phase
!                  end
!               else
!                  ! Off-diagonal shell -- count twice for same atom
!                  do k = 1,n_k
!                     pk = k_pts(k,:)
!                     phase = exp(-II*dot_product(pk,pa))
!                     ff_n(k,ua) = ff_n(k,ua) + TWO*sum(FT(k,:,:)*DM(fa:la,fb:lb))*phase
!                  end
!               end
!
!          ! else ! DIFFERENT ATOMS A /= B
!
!          !    ! Zero site symmetry factors if not asymmetric-unit atom
!          !    pa = .atom(atom_a).position
!          !    ra = .crystal.frag_atom_sym_factor(atom_a)
!          !    if      (ua<1) then; sa = ZERO
!          !    else;                sa = ONE/ra
!          !    end
!          !    pb = .atom(atom_b).position
!          !    rb = .crystal.frag_atom_sym_factor(atom_b)
!          !    if      (ub<1) then; sb = ZERO
!          !    else;                sb = ONE/rb
!          !    end
!
!          !    ! Make the ATOM A static FT integrals, lower-diagonal
!          !    if (ua>1) then
!          !       sh.make_tanaka_partition(pf,sa,sb,center="A")
!          !       sh.make_FT_parted(FT,k_pts,pf)
!          !       do k = 1,n_k
!          !          phase = exp(-II*dot_product(pk,pa))
!          !          ff_n(k,ua) = ff_n(k,ua) + sum(FT(k,:,:)*DM(fa:la,fb:lb))*phase
!          !       end
!          !    end
!
!          !    ! Make the ATOM B static FT integrals, lower-diagonal
!          !    ! No need to actually transpose ...
!          !    if (ub>1) then
!          !       sh.make_tanaka_partition(pf,sa,sb,center="B")
!          !       sh.make_FT_parted(FT,k_pts,pf)
!          !       do k = 1,n_k
!          !          phase = exp(-II*dot_product(pk,pb))
!          !          ff_n(k,ub) = ff_n(k,ub) + sum(FT(k,:,:)*DM(fa:la,fb:lb))*phase
!          !       end
!          !    end
!          !   
!            end
!
!            ! Clean
!            pf.destroy
!            FT.destroy
!
!         end
!
!         ! Clean
!         sh.destroy_ptr_part
!
!      end
!
!      ! Clean up
!      k_pts.destroy
!      DM.destroy
!
!   end

   subroutine make_aspherical_atomic_FFs(self,ff)
   ! Get "ff", the *static* Hirshfeld atom form factors for the
   ! symmetry related K points, for every unique fragment atom.
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka (2008)
   !       Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn 
   !       (11) except without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff

      ! Hirshfeld-atom structure factors
      ! NOTE: in iteration 0, these were already calculated
      !       as part of make_X_SFs ... double work!
      select case (self%crystal%xray_data%partition_model)
      case ("oc-crystal23  "); call make_C23_Hirshfeld_atom_FFs_(self,ff)  ! NOT PURE
      case ("oc-g-hirshfeld"); call make_gHirshfeld_atom_FFs_(self,ff)
      case ("oc-g-salvador "); call make_gSalvador_atom_FFs_(self,ff)
      case ("oc-hirshfeld  "); call make_Hirshfeld_atom_FFs_(self,ff)
      case ("oc-exphar     "); call make_Hirshfeld_atom_FFs_(self,ff)
      case ("oc-sph-exphar "); call make_sph_HA_atom_FFs_(self,ff)
      case ("oc-ri         "); call make_Hirshfeld_atom_FFs_(self,ff)
      case ("oc-salvador   "); call make_Salvador_atom_FFs_(self,ff)
      case ("oc-tfvp       "); call make_Salvador_atom_FFs_(self,ff)
      case ("oc-tfvh       "); call make_Salvador_atom_FFs_(self,ff)
      case ("oc-sph-tfvp   "); call make_sph_TFVA_atom_FFs_(self,ff)
      case ("oc-sph-tfvh   "); call make_sph_TFVA_atom_FFs_(self,ff)
      case ("oc-sph-tfva   "); call make_sph_TFVA_atom_FFs_(self,ff)
      end select
   end subroutine

   NON_MPI_PURE subroutine make_Hirshfeld_atom_FFs(self,ff,spin_density)
   ! Get "ff", the static Hirshfeld atom form factors for the
   ! symmetry related K points with *no* thermal smearing, for every
   ! unique fragment atom.
   ! NOTE: this version uses Kang's density code!
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
   ! without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      BIN, optional, IN :: spin_density
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: rho
      MAT(REAL,:,:), allocatable :: pt,k_pts
      REAL :: cutoff
      REAL :: ss,sc,s2
      INT :: n_atom,u,c
      INT :: n_k
      BIN :: spin
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no crystal fragment info")

      ! Asymmetric unit atoms
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! X-ray or PND?
      spin = FALSE
      if (present(spin_density)) spin = spin_density

      ! Density matrix (leaky)
      if (NOT spin) then; call make_r_density_mx_(self)
      else; call make_r_Sz_density_mx_(self)
      end if
      ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_Hirshfeld_atom_FFs ... no restricted DM")

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Loop over asymmetric/unique atoms "c"
      do u = PARALLEL_DO_START(1,1),self%crystal%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_Hirshfeld_atom_FFs")
         c = self%crystal%unique_frag_atom(u)
         rc = self%atom(c)%position

         ! Site-symmetry factor for this atom
         ss = self%crystal%frag_atom_sym_factor(c)
         sc = ss
         if (is_zero_(sc)) cycle
         s2 = ONE/sc

         ! Get the Becke grid for atom "c" (leaky)
         call make_Hirshfeld_atom_ED_grid_(self,rho,pt,c,self%density_mx%r)

         ! The (unique) atomic form factors
         ! Fourier transform, yay!
         call make_atom_FFs_on_grid_(self,ff(:,u),k_pts,pt,rho,c)
         ff(:,u) = ff(:,u)*s2*self%atom(c)%site_occupancy

         ! Clean up
         call destroy_(rho)
         call destroy_(pt)
      end do  ! -- loop over atom "c"
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_Hirshfeld_atom_FFs")

      ! Parallel
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(ff)
      end if

      ! Clean up
      call destroy_(k_pts)
   end subroutine

   PURE subroutine make_atom_FFs_on_grid(self,ff,k_pts,pt,rho,c)
   ! Set "ff" to the transform at "k_pts" of the density of atom "c", given
   ! as "rho", its values times the grid weights, at points "pt": by the
   ! sum over the points, or, for partition_model= oc-ri, from a fit of it
   ! in functions on atom "c".
      type(MOLECULE_TYPE), IN :: self
      VEC(CPX,:), OUT :: ff
      MAT(REAL,:,:), IN :: k_pts
      MAT(REAL,:,:), IN :: pt
      VEC(REAL,:), IN :: rho
      INT, IN :: c
      VEC(REAL,:), allocatable :: alpha
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_atom_FFs_on_grid ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_atom_FFs_on_grid ... no reflection data")
      if (self%crystal%xray_data%partition_model=="oc-ri") then
         call make_RI_exponents_(self,alpha,c)
         call fitted_exp_ikr_sums_(ff,k_pts,pt,rho,self%atom(c)%position,alpha,self%crystal%xray_data%RI_l_max)
         call destroy_(alpha)
      else
         call exp_ikr_sums_(ff,k_pts,pt,rho)
      end if
   end subroutine

   PURE subroutine make_RI_exponents(self,alpha,c)
   ! Make "alpha", the exponents of the oc-ri fitting functions on atom
   ! "c": twice its largest basis exponent, then each divided by
   ! RI_exponent_ratio, down to its smallest basis exponent.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), allocatable, OUT :: alpha
      INT, IN :: c
      REAL :: a_hi,a_lo,ratio
      INT :: n,i
   ENSURE(allocated(self%atom(c)%basis),"MOLECULE.RHO:make_RI_exponents ... no basis for atom "//trim(to_str_(c)))
      a_hi = TWO*max_exponent_(self%atom(c)%basis%shell)
      a_lo = min_exponent_(self%atom(c)%basis%shell)
      ratio = self%crystal%xray_data%RI_exponent_ratio
      n = 1+floor(log(a_hi/a_lo)/log(ratio))
      call create_(alpha,n)
      do i = 1,n
         alpha(i) = a_hi/ratio**(i-1)
      end do
   end subroutine

   NON_MPI_PURE subroutine make_Salvador_atom_FFs(self,ff)
   ! Make "ff", the static Salvador (TFVA) atom form factors for the
   ! symmetry related K points with *no* thermal smearing, for every
   ! unique fragment atom.
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
   ! without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: rho
      MAT(REAL,:,:), allocatable :: pt,k_pts
      REAL :: cutoff
      REAL :: ss,sc,s2
      INT :: n_atom,u,c
      INT :: n_k
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_Salvador_atom_FFs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_Salvador_atom_FFs ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_Salvador_atom_FFs ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_Salvador_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_Salvador_atom_FFs ... no reflection data")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_Salvador_atom_FFs ... no x-ray reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_Salvador_atom_FFs ... no crystal fragment info")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_Salvador_atom_FFs ... no crystal fragment info")

      ! Asymmetric unit atoms
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Density matrix (leaky)
      call make_r_density_mx_(self)
      ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_Salvador_atom_FFs ... no restricted DM")

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Loop over asymmetric/unique atoms "c"
      do u = PARALLEL_DO_START(1,1),self%crystal%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_Salvador_atom_FFs")
         c = self%crystal%unique_frag_atom(u)
         rc = self%atom(c)%position

         ! Site-symmetry factor for this atom
         ss = self%crystal%frag_atom_sym_factor(c)
         sc = ss
         if (is_zero_(sc)) cycle
         s2 = ONE/sc

         ! Get the Becke grid for atom "c" (leaky)
         ! Grid points "pt" are around position "rc"
         call make_Salvador_atom_ED_grid_(self,rho,pt,c,self%density_mx%r)

         ! The (unique) atomic form factors
         ! Fourier transform, yay!
         call exp_ikr_sums_(ff(:,u),k_pts,pt,rho)
         ff(:,u) = ff(:,u)*s2*self%atom(c)%site_occupancy

         ! Clean up
         call destroy_(rho)
         call destroy_(pt)
      end do  ! -- loop over atom "c"
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_Salvador_atom_FFs")

      ! Parallel
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(ff)
      end if

      ! Clean up
      call destroy_(k_pts)
   end subroutine

   NON_MPI_PURE subroutine make_sph_TFVA_atom_FFs(self,ff)
   ! Make "ff", the static Hirshfeld atom form factors for the
   ! symmetry related K points with *no* thermal smearing, for every
   ! unique fragment atom.
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
   ! without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: rho,pt
      MAT(REAL,:,:), allocatable :: k_pts
      REAL :: cutoff
      REAL :: ss,sc,s2
      VEC(REAL,:), allocatable :: kk,sf
      REAL :: kc
      INT :: n_atom,u,c
      INT :: n_k,k
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no reflection data")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no x-ray reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no crystal fragment info")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no crystal fragment info")

      ! Asymmetric unit atoms
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Length of each k
      call create_(kk,n_k)
      do k = 1,n_k
         kk(k) = norm2(k_pts(k,:))
      end do

      ! Density matrix (leaky)
      call make_r_density_mx_(self)
      ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_sph_TFVA_atom_FFs ... no restricted DM")

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Loop over asymmetric/unique atoms "c"
      do u = PARALLEL_DO_START(1,1),self%crystal%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_sph_TFVA_atom_FFs")
         c = self%crystal%unique_frag_atom(u)
         rc = self%atom(c)%position

         ! Site-symmetry factor for this atom
         ss = self%crystal%frag_atom_sym_factor(c)
         sc = ss
         if (is_zero_(sc)) cycle
         s2 = ONE/sc

         ! Get the Becke grid for atom "c" (leaky)
         ! Note: "pt" is around the origin, not "rc"
         call make_sph_avgd_SA_ED_grid_(self,rho,pt,c)

         ! The (unique) atomic form factors, times the position phase
         call create_(sf,n_k)
         call sinc_kr_sums_(sf,kk,pt,rho)
         do k = 1,n_k
            kc = dot_product(k_pts(k,:),rc)
            ff(k,u) = sf(k)*exp(IMAGIFY(kc))*s2*self%atom(c)%site_occupancy
         end do

         ! Clean up
         call destroy_(sf)
         call destroy_(rho)
         call destroy_(pt)
      end do  ! -- loop over atom "c"
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_sph_TFVA_atom_FFs")

      ! Parallel
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(ff)
      end if

      ! Clean up
      call destroy_(kk)
      call destroy_(k_pts)
   end subroutine

   subroutine make_sph_atom_FF_curve(self,f,s,c)
   ! Make "f", the spherical atom form factor of unique atom "c" at
   ! "s" = sin(theta)/lambda in 1/Angstrom: FF_fit_n_points evenly
   ! spaced from 0 to FF_fit_s_max. The atom is the one of the sph-
   ! partition model in force: the spherically averaged Salvador atom
   ! (oc-sph-tfva, oc-sph-tfvp, oc-sph-tfvh) or the spherically
   ! averaged (exponential) Hirshfeld atom (oc-sph-exphar). "f" and "s"
   ! are created here.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable, OUT :: f,s
      INT, IN :: c
      VEC(REAL,:), allocatable :: rho,pt,k
      REAL :: s_max
      INT :: n,i
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_sph_atom_FF_curve ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_sph_atom_FF_curve ... no x-ray data")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_sph_atom_FF_curve ... no density matrix")
      n = self%crystal%xray_data%FF_fit_n_points
      s_max = self%crystal%xray_data%FF_fit_s_max

      ! The s grid, and k = 4 pi s in 1/bohr
      call create_(s,n)
      call create_(k,n)
      do i = 1,n
         s(i) = s_max*(i-1)/(n-1)
      end do
      k = FOUR*PI*s*ANGSTROM_PER_BOHR

      ! The averaged density on the radial grid, times its weights
      call make_r_density_mx_(self)
      select case (self%crystal%xray_data%partition_model)
      case ("oc-sph-tfva   ","oc-sph-tfvp   ","oc-sph-tfvh   ")
         call make_sph_avgd_SA_ED_grid_(self,rho,pt,c)
      case ("oc-sph-exphar ")
         call make_sph_avgd_HA_ED_grid_(self,rho,pt,c)
      case default
         DIE("MOLECULE.RHO:make_sph_atom_FF_curve ... fit_sph_atom_FFs needs a sph- partition model, not "//trim(self%crystal%xray_data%partition_model))
      end select

      ! Its transform
      call create_(f,n)
      call sinc_kr_sums_(f,k,pt,rho)
      call destroy_(pt)
      call destroy_(rho)
      call destroy_(k)
   end subroutine

   NON_MPI_PURE subroutine make_sph_HA_atom_FFs(self,ff)
   ! Make "ff", the static spherically averaged Hirshfeld (or
   ! exponential Hirshfeld) atom form factors for the symmetry related
   ! K points with *no* thermal smearing, for every unique fragment atom.
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
   ! without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: rho,pt
      MAT(REAL,:,:), allocatable :: k_pts
      REAL :: cutoff
      REAL :: ss,sc,s2
      VEC(REAL,:), allocatable :: kk,sf
      REAL :: kc
      INT :: n_atom,u,c
      INT :: n_k,k
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_sph_HA_atom_FFs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_sph_HA_atom_FFs ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_sph_HA_atom_FFs ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_sph_HA_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_sph_HA_atom_FFs ... no reflection data")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_sph_HA_atom_FFs ... no x-ray reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_sph_HA_atom_FFs ... no crystal fragment info")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_sph_HA_atom_FFs ... no crystal fragment info")

      ! Asymmetric unit atoms
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Length of each k
      call create_(kk,n_k)
      do k = 1,n_k
         kk(k) = norm2(k_pts(k,:))
      end do

      ! Density matrix (leaky)
      call make_r_density_mx_(self)
      ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_sph_HA_atom_FFs ... no restricted DM")

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Loop over asymmetric/unique atoms "c"
      do u = PARALLEL_DO_START(1,1),self%crystal%n_unique_frag_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_sph_HA_atom_FFs")
         c = self%crystal%unique_frag_atom(u)
         rc = self%atom(c)%position

         ! Site-symmetry factor for this atom
         ss = self%crystal%frag_atom_sym_factor(c)
         sc = ss
         if (is_zero_(sc)) cycle
         s2 = ONE/sc

         ! Get the Becke grid for atom "c" (leaky)
         ! Note: "pt" is around the origin, not "rc"
         call make_sph_avgd_HA_ED_grid_(self,rho,pt,c)

         ! The (unique) atomic form factors, times the position phase
         call create_(sf,n_k)
         call sinc_kr_sums_(sf,kk,pt,rho)
         do k = 1,n_k
            kc = dot_product(k_pts(k,:),rc)
            ff(k,u) = sf(k)*exp(IMAGIFY(kc))*s2*self%atom(c)%site_occupancy
         end do

         ! Clean up
         call destroy_(sf)
         call destroy_(rho)
         call destroy_(pt)
      end do  ! -- loop over atom "c"
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_sph_HA_atom_FFs")

      ! Parallel
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(ff)
      end if

      ! Clean up
      call destroy_(kk)
      call destroy_(k_pts)
   end subroutine

   subroutine delete_Hirshfeld_atom_FFs(self)
   ! Delete all the asymmetric unit form factor (FF) files
   ! before they are (re)generated.
      type(MOLECULE_TYPE), IN :: self
      type(ARCHIVE_TYPE) :: arch
      INT :: a
      do a = 1,self%crystal%n_asymmetric_unit_atoms
         call set_(arch,trim(self%crystal%asymmetric_unit_atom(a)%label)//"-SFs")
         call delete_(arch)
      end do
   end subroutine

   PURE subroutine make_gHirshfeld_atom_FFs(self,ff)
   ! Get "ff", the static gaussian-partitioned Hirshfeld atom form
   ! factors for the symmetry related K points "k_pts", for every
   ! unique fragment atom.
   ! NOTE: This version uses interpolators for efficiency.
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      type(SHELL2_TYPE) :: sh
      VEC(REAL,:), allocatable :: Wc
      MAT(REAL,:,:), allocatable :: pts
      MAT(REAL,:,:), allocatable :: k_pts
      MAT(REAL,:,:), allocatable :: DM
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      INT :: n_k,k,q
      INT :: n_atom,u,c
      INT :: fa,la,na,atom_a
      INT :: fb,lb,nb,atom_b
      REAL :: cutoff,ss
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no basis info")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no restricted DM")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no atom list")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... missing atomic interpolators")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no overlapping_atoms")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_gHirshfeld_atom_FFs ... no crystal fragment info")

   ! TF    :: MAT3{REAL}@
   ! Wcm   :: MAT{REAL}@
   ! Ua,Ub :: MAT{REAL}(3,3)

      ! Parameters
      cutoff = TOL(10)/self%n_shell_pairs
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Shell pairs
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)

         ! Get thermal factors "TF"
      ! TF.create(n_k,sh.a.n_cc,sh.b.n_cc)
      ! TF = ONE
      ! Ua = .atom(atom_a).ADP_tensor
      ! Ub = .atom(atom_b).ADP_tensor
      ! sh.make_temperature_factors(TF,"tc-tanaka",k_pts,Ua,Ub)
         DM = self%density_mx%r(fa:la,fb:lb)
         if (fa/=fb) DM = TWO*DM
         pts = transpose(sh%pair_center)
         call create_(Wc,size(pts,1))
         call create_(FT_ab,n_k,na,nb)

         ! Loop on asymmetric/unique atoms "c"
         do u = 1,self%crystal%n_unique_frag_atoms
            c = self%crystal%unique_frag_atom(u)

            ! Site-symmetry & occupancy for this atom
            ss = self%crystal%frag_atom_sym_factor(c)
            if (is_zero_(ss)) cycle
            ss = self%atom(c)%site_occupancy/ss

            ! Get weights
            call make_stockholder_atom_weight_int_(self,Wc,c,pts,self%overlapping_atoms_for_atom(c)%element)
            Wc = ss*Wc

            ! Do FT with partition factor
            call make_FT_no_TF_(sh,FT_ab,k_pts,Wc)
         ! Wcm = reshape(Wc,[sh.a.n_cc,sh.b.n_cc])
         ! sh.make_ft(FT_ab,k_pts,TF,Wcm)

            ! Trace with density matrix
            do k = 1,n_k
               ff(k,u) = ff(k,u)+sum(FT_ab(k,:,:)*DM)
            end do
         end do

         ! Clean
         call destroy_(FT_ab)
         call destroy_(Wc)
         call destroy_(pts)
         call destroy_(DM)
      ! TF.destroy

         ! Clean
         call destroy_ptr_part_(sh)
      end do
   end subroutine

   PURE subroutine make_gSalvador_atom_FFs(self,ff)
   ! Get "ff", the static gaussian-partitioned Hirshfeld atom form
   ! factors for the symmetry related K points "k_pts", for every
   ! unique fragment atom.
   ! NOTE: This version uses interpolators for efficiency.
      type(MOLECULE_TYPE), IN :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      type(SHELL2_TYPE) :: sh
      VEC(REAL,:), allocatable :: Wc
      MAT(REAL,:,:), allocatable :: pts
      MAT(REAL,:,:), allocatable :: k_pts
      MAT(REAL,:,:), allocatable :: DM
      MAT3(CPX,:,:,:), allocatable :: FT_ab
      INT :: n_k,k,q
      INT :: n_atom,u,c
      INT :: fa,la,na,atom_a
      INT :: fb,lb,nb,atom_b
      REAL :: cutoff,ss
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_gSalvador_atom_FFs ... no basis info")
   ENSURE(allocated(self%becke_grid),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no becke_grid")
   ENSURE(self%becke_grid%finalized,"MOLECULE.RHO:make_gSalvador_atom_FFs ... no finalized becke_grid")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no density matrix")
   ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no restricted DM")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no atom list")
   ENSURE(has_interpolators_any_kind_(self%atom),"MOLECULE.RHO:make_gSalvador_atom_FFs ... missing atomic interpolators")
   ENSURE(allocated(self%overlapping_atoms_for_atom),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no overlapping_atoms")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_gSalvador_atom_FFs ... no reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_gSalvador_atom_FFs ... no crystal fragment info")

      ! Parameters
      cutoff = TOL(10)/self%n_shell_pairs
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Shell pairs
      do q = 1,self%n_shell_pairs
         call set_shell2_from_(self,q,sh,fa,la,na,fb,lb,nb,atom_a,atom_b)
         DM = self%density_mx%r(fa:la,fb:lb)
         if (fa/=fb) DM = TWO*DM
         pts = transpose(sh%pair_center)
         call create_(Wc,size(pts,1))
         call create_(FT_ab,n_k,na,nb)

         ! Loop on asymmetric/unique atoms "c"
         do u = 1,self%crystal%n_unique_frag_atoms
            c = self%crystal%unique_frag_atom(u)

            ! Site-symmetry & occupancy for this atom
            ss = self%crystal%frag_atom_sym_factor(c)
            if (is_zero_(ss)) cycle
            ss = self%atom(c)%site_occupancy/ss

            ! Get weights
            call make_Salvador_cell_fn_(self%becke_grid,Wc,pts,c)
            Wc = ss*Wc

            ! Do FT with partition factor
            call make_FT_no_TF_(sh,FT_ab,k_pts,Wc)

            ! Trace with density matrix
            do k = 1,n_k
               ff(k,u) = ff(k,u)+sum(FT_ab(k,:,:)*DM)
            end do
         end do

         ! Clean
         call destroy_(FT_ab)
         call destroy_(Wc)
         call destroy_(pts)
         call destroy_(DM)

         ! Clean
         call destroy_ptr_part_(sh)
      end do
   end subroutine

   subroutine make_C23_Hirshfeld_atom_FFs(self,ff)
   ! MAke "ff", the static Hirshfeld atom form factors (from a
   ! periodic CRYSTAL23 calculation) for the symmetry related K points
   ! with *no* thermal smearing, for every unique atom.
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
   ! without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: ff
      VEC(REAL,:), allocatable :: rho,wt,Wa
      MAT(REAL,:,:), allocatable :: pt,k_pts
      VEC(STR(len=STR_SIZE),:), allocatable :: library_basis_labels
      STR(len=PATH_SIZE) :: library
      type(CRYSTAL_TYPE), allocatable :: cluster
      VEC(type(ATOM_TYPE),:), allocatable :: HS_atoms
      REAL :: sa
      INT :: n_atom,n_k,n_pt
      INT :: a,i
      VEC(type(ATOM_TYPE),:), allocatable :: tmp_atom
      BIN :: found
      VEC(REAL,3) :: pos1,pos2
   ENSURE(self%basis_info_made,"MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs ... no atom list")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs ... no reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs ... no crystal fragment info")

      ! Asymmetric unit atoms,
      ! Symmetry-generated K points and/or SF's
      n_atom = size(self%crystal%asymmetric_unit_atom)
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Form factor array (leaky)
      call create_(ff,n_k,n_atom)
      ff = ZERO

      ! Make the Thakkar slaterbases
      call destroy_(self%slaterbasis)
      self%slaterbasis_name = "Thakkar"  ! ugly
      call set_basis_kind_(self%atom,"slater")
      library = library_file_(self%slaterbasis,self%slaterbasis_name)
      library_basis_labels = library_basis_labels_(self%atom,self%slaterbasis_name)
      call read_library_data_(self%slaterbasis,library,library_basis_labels)  !  NOT PURE
      call destroy_(library_basis_labels)
      call unnormalize_(self%slaterbasis)

      ! Ensure .atom's have their .slaterbases
      call make_slater_interpolators_(self)
      call resolve_slaterbases_(self,self%crystal%asymmetric_unit_atom)

      ! Make a new crystal only for generating clusters.
      ! Since we need to define the fragment_geometry to be
      ! the asymmetric unit for this purpose, but for
      ! structure refinement we need to stay with the C23
      ! XML file geometry which is the [-half,+half]^3 cell.
      ! NOTE: wasteful copy of xray_data is done here
      ! In case of periodic defragment, .asymmetric_unit_atom(:) are compared with .atom(:)
      !   If an asym_atom is not found in .atom, then it is manually updated by (CRY23 primitive_cell) lattice translastion.
      !   Without defragment, all the asym_atoms are always found among .atom(:). 2025.Mar.19 Kang
      cluster = self%crystal

!!! // phar_defragment compatibility //
!
!! Kang Note. 1-to-1 match between asymmetric_unit_atom(=tmp_atom) and (.molecule).atom (=atoms from Crystal23 xml file)
!            Without pHAR_defragment, it does basically nothing.
!            If .asymmetric_unit_atom(a) is not found in .atom, then it is replaced by one of atom(i)
!            Take a lattice-translation-equivalent atom in atom(:). Use the CRY23 primitive unit cell, not crystallographic .crystal.unit_cell
!            tmp_atom will be the source of HS_atoms 
      call create_(tmp_atom,self%crystal%n_asymmetric_unit_atoms)
      tmp_atom = self%crystal%asymmetric_unit_atom
      do a = 1,size(tmp_atom)
         pos1 = tmp_atom(a)%position  ! pos1 from asymmetric_unit_atom
         found = FALSE
         do i = 1,size(self%atom)  ! Kang comment. It is not efficient search. any found item is not excluded from the search
            pos2 = self%atom(i)%position  ! pos2 from Crystal23 xml_atom (=.atom); .atom is for the ED calculation
            if (equals_(pos1,pos2)) then

!>DEBUG         stdout.show("cry.asym_atom, .atom" ,[a,i] ) ! Without phar_defragment, it is same to .crystal.asym_atom_for_frag_atom
               found = TRUE
               exit
            end if
         end do
         if (found) cycle

         ! asymmetric_unit_atom is not found in .atom(:). Then find translation-equivalent one from .atom(:)
         do i = 1,size(self%atom)
            pos2 = self%atom(i)%position-pos1  ! Abusing variable "pos2". Now pos2 is the coordinate difference.
            pos2 = matmul(self%crystal23%Primitive_unit_cell%inverse_mx,pos2)  ! To fractional coord (in C23 primitive cell)
            pos2 = mod(pos2+HALF+ONE,ONE)-HALF  ! modulo operation. Map to [-0.5, 0.5)
            if (is_zero_(pos2)) then

!               stdout.show("cry.asym_atom translated, .atom" ,[a,i] ) ! Without periodic defragmen
               call set_position_(tmp_atom(a),self%atom(i)%position)  ! Overwrite tmp asym atom
               ! Kang's note. You may double check by comparing ADPs. 
               exit
            end if
         end do
      end do
      self%crystal%asymmetric_unit_source = "Crystal23-xml-selected-by-tonto"

!! //phar_defragment compatibility// by Kang 2025.Mar.20
      call destroy_(cluster%asymmetric_unit_geometry)
      call destroy_(cluster%asymmetric_unit_atom)
      call update_fragment_info_(cluster,tmp_atom); call destroy_(tmp_atom)  ! tmp_atom ~ .asymmetric_unit_atom
      call set_basis_kind_(cluster%asymmetric_unit_atom,"slater")

      ! Make the HS_atoms & resolve the interpolators
      call make_asym_unit_HS_cluster_(cluster,HS_atoms,TOL(10))  ! <=== Kang note: HS_atoms are the source of becke_grid
      call set_basis_kind_(HS_atoms,"slater")
      call resolve_bases_(HS_atoms,self%slaterbasis,self%slaterbasis_name)
      call update_kinds_(HS_atoms)

!>DEBUG: show super cluster for Hirshfeld weight function calculation
!      stdout.text(" ----------------------------------------------------")
!      stdout.text(" CLUSTER GEOMETRY for Hirshfeld partitioning function")
!      stdout.text(" ----------------------------------------------------")
!      HS_atoms.put_coordinates(no_header=TRUE)

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! Set up integration grid for the HS atom cluster
      call create_(self%becke_grid)
      call set_atom_info_(self%becke_grid,HS_atoms)  ! NOT PURE. Kang note: HS_atoms = .becke_grid.atom
      call set_one_atom_grid_data_(self%becke_grid)

      ! Loop over asymmetric/unique atoms "c"
      do a = PARALLEL_DO_START(1,1),self%crystal%n_asymmetric_unit_atoms,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs")

         ! Site-symmetry factor for this atom
         sa = self%crystal%asym_atom_sym_factor(a)
         if (is_zero_(sa)) cycle
         sa = ONE/sa

         ! Get the Becke grid for one-atom grid
         ! For 'a' from 1 to .n_asym_atoms:
         ! HS_atoms(a) == .asymmetric_unit_atom(a)
         call make_atom_grid_(self%becke_grid,pt,wt,a)
         n_pt = size(pt,1)

         ! Stockholder weight function "Wa" for atom "a"
         call create_(Wa,n_pt)
         call make_Hirshfeld_weight_grid_(HS_atoms,Wa,pt,a)

         ! Incorporate integration weight
         wt = Wa*wt
         call destroy_(Wa)

         ! Prune the grid with new "wt"
         ! and new no of points, n_pt 
         call prune_grid_(self%becke_grid,pt,wt)
         n_pt = size(pt,1)  ! *New* points
         !!!!!!!!!!!!!!

         ! Density for atom "a"
         ! NOTE: only uses actual "pt" array for the C23 .atom
         ! geometry & density matrix read from the XML file.
         call create_(rho,n_pt)
         call make_CRYSTAL23_ED_grid_v1_(self,rho,pt)

         ! Weight the atom "a" density
         ! Now only the FT e^(ik.r) part missing
         rho = rho*wt

         ! Make the (symmetry-generated) atomic form factors "ff"
         ! Fourier transform, yay!
         call exp_ikr_sums_(ff(:,a),k_pts,pt,rho)
         ff(:,a) = ff(:,a)*sa
         call destroy_(rho)
         call destroy_(wt)
         call destroy_(pt)
      end do  ! -- loop over atom "a"
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:make_C23_Hirshfeld_atom_FFs")

      ! Parallel
      if (WORK_IS_SHARED) then
         PARALLEL_SUM(ff)
      end if

      ! Clean up
      call destroy_(k_pts)
      call destroy_(HS_atoms)
   end subroutine

   subroutine get_unique_fragHAR_SFs(self,spin_density)
   ! Write out the fragHAR static structure factors for each
   ! .atom_group.mol molecule.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, optional, IN :: spin_density
      INT :: g,n_group
   ENSURE(allocated(self%atom_group),"MOLECULE.RHO:get_unique_fragHAR_SFs ... no .atom_group")
   ENSURE(allocated(self%mol),"MOLECULE.RHO:get_unique_fragHAR_SFs ... no .mol")
   ENSURE(size(self%mol)==size(self%atom_group),"MOLECULE.RHO:get_unique_fragHAR_SFs ... wrong size, .mol")
   ENSURE(allocated(self%cif),"MOLECULE.RHO:get_unique_fragHAR_SFs ... no cif specified")

      ! Make ANO's & interpolators for parent
      ! WARNING: dies if no ANOs or interpolators
   ! .make_ANO_data

      ! The fragments integrate on the parent's grid settings, and this must
      ! precede .initialize_DFT_grids below
      call set_group_becke_grids_(self)

      ! Make sutructure factors for each residue
      do g = 1,size(self%atom_group)

         ! NOTE: change DFT -> becke
         call initialize_DFT_grids_(self%mol(g))

         ! Assign previously made ANOs
         call resolve_ANOs_from_(self%mol(g),self)

         ! Write the SFs for each fragment to disk
         n_group = size(self%atom_group(g)%atom_index)
         call get_Hirshfeld_atom_FFs_disk_(self%mol(g),n_group,spin_density)
      end do
   end subroutine

   subroutine get_Hirshfeld_atom_FFs_disk(self,n_last,spin_density)
   ! Write out the static Hirshfeld atom form factors for the symmetry
   ! related K points with *no* thermal smearing, for every unique
   ! atom. If "n_last" is present only do unique atoms up to this
   ! atom, which is useful to eliminate capping atoms in a fragment.
   ! If "spin_densityy" is present the spinb density SFs are made.
   ! NOTE: The integrals "sf_u" written  are the FT rho_a(r), eqn (3)
   ! in Jayatilaka 2008, Acta Cryst A 64 p. 383-393. It is also f_a(q)
   ! in eqn (11) except without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      INT, optional, IN :: n_last
      BIN, optional, IN :: spin_density
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: wt,rho
      MAT(REAL,:,:), allocatable :: pt
      MAT(REAL,:,:), allocatable :: k_pts
      INT :: n_unique,n_atom
      INT :: u,c,n_k,n_pt
      REAL :: cutoff,sc,s2
      VEC(CPX,:), allocatable :: sf_u
      BIN :: spin
      type(ARCHIVE_TYPE), allocatable :: arch
   ENSURE(self%basis_info_made,"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no crystal fragment info")
      n_unique = self%crystal%n_unique_frag_atoms
      if (present(n_last)) n_unique = n_last

      ! Asymmetric unit atoms
      n_atom = size(self%crystal%asymmetric_unit_atom)

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(sf_u,n_k)

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! X-ray or PND?
      spin = FALSE
      if (present(spin_density)) spin = spin_density

      ! Density matrix (leaky)
      if (NOT spin) then; call make_r_density_mx_(self)
      else; call make_r_Sz_density_mx_(self)
      end if
      ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk ... no restricted DM")

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Loop over integration atoms "c"
      do u = PARALLEL_DO_START(1,1),n_unique,PARALLEL_DO_STRIDE(1)
      LOCK_PARALLEL_DO("MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk")
         sf_u = ZERO
         c = self%crystal%unique_frag_atom(u)
         rc = self%atom(c)%position

         ! Skip cap atoms
         if (self%atom(c)%is_cap_atom) cycle

         ! Repetition factor for this atom
         ! Also include the site occupancy factor
         ! (Dont forget this for the non-fragHAR routine)
         sc = self%crystal%frag_atom_sym_factor(c)
         if (is_zero_(sc)) cycle
         s2 = self%crystal%fragment_atom(c)%site_occupancy/sc
      ! s2 = s2*s2

         ! Get the Becke grid for atom "c" (leaky)
         call make_atom_grid_(self%becke_grid,pt,wt,c)

         ! Apply stockholder weights "Wc" for atom "c" to "wt"
         call apply_stockholder_atom_weight_(self,wt,c,pt)

         ! Prune the grid with new "wt"
         call prune_grid_(self%becke_grid,pt,wt)

         ! Density for atom "c"
         n_pt = size(pt,1)  ! <-- New n_pt !!!
         call create_(rho,n_pt)
         call make_ED_grid_r_v2_(self,rho,pt,self%density_mx%r,cutoff)

         ! Weight the atom "c" density
         rho = rho*wt

         ! Get aspherical form factor sf_u
         call make_atom_FFs_on_grid_(self,sf_u,k_pts,pt,rho,c)
         sf_u = sf_u*s2

         ! Write out sf_u
         call create_(arch,trim(self%atom(c)%tag)//"-SFs")
         if (self%crystal%xray_data%use_text_SFs) call set_format_(arch,"ascii")
         call per_rank_write_(arch,sf_u)
         call destroy_(arch)

         ! Clean up atom "c" stuff
         call destroy_(rho)
         call destroy_(wt)
         call destroy_(pt)
      end do  ! -- loop over atom "c"
      UNLOCK_PARALLEL_DO("MOLECULE.RHO:get_Hirshfeld_atom_FFs_disk")

      ! NO barrier here -- deliberately. See MOLECULE.SCF:make_X_SFs_HAR_disk and
      ! MOLECULE.SCF:fragment_SCF, which have it instead.
      !
      ! One was added here on 2026-08-02 and removed the same day. It is wrong
      ! at THIS level because this routine has two kinds of caller:
      !
      !   collective  MOLECULE.SCF:make_X_SFs_HAR_disk, and the `do g` loop in
      !               MOLECULE.RHO:get_Hirshfeld_atom_FFs -- every rank calls,
      !               the same number of times.
      !   RANK-LOCAL  the three per-fragment calls in MOLECULE.SCF:fragment_SCF_-
      !               norm / _para -- rank r calls it only for the fragments it
      !               owns, and in the >2-rank master/worker branch the master
      !               `cycle`s and never calls it at all.
      !
      ! A barrier is a COLLECTIVE, so on the rank-local paths it would execute a
      ! different number of times on different ranks and deadlock. The rule from
      ! milestone 6 applies to the CALL SITE as much as to the macro: whether a
      ! collective executes must not depend on rank-local state.

      ! Clean up
      call destroy_(sf_u)
      call destroy_(k_pts)
   end subroutine

   subroutine get_Hirshfeld_atom_FFs_for_atom(self,u,ff,spin_density)
   ! Get "ff", the static Hirshfeld atom form factors for the
   ! symmetry related K points with *no* thermal smearing, for every
   ! unique atom.
   ! NOTE: "ff" is the FT rho_a(r), eqn (3) in Jayatilaka 2008,
   ! Acta Cryst A 64 p. 383-393. It is also f_a(q) in eqn (11) except
   ! without the thermal smearing factor.
      type(MOLECULE_TYPE), INOUT :: self
      INT, IN :: u
      VEC(CPX,:), allocatable, OUT :: ff
      BIN, optional, IN :: spin_density
      VEC(REAL,3) :: rc
      VEC(REAL,:), allocatable :: wt,rho
      MAT(REAL,:,:), allocatable :: pt
      MAT(REAL,:,:), allocatable :: k_pts
      REAL :: cutoff,sc,s2
      INT :: n_k
      INT :: n_pt
      INT :: c
      BIN :: spin
   ENSURE(self%basis_info_made,"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no basis info")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no atom list")
   ENSURE(allocated(self%density_mx),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no density matrix")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no reflection data")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no x-ray reflection data")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no crystal fragment info")

      ! Symmetry generated K points and SF's
      n_k = n_unique_SF_k_pts_(self%crystal)

      ! Allocate structure factor array (leaky)
      call create_(ff,n_k)
      ff = ZERO

      ! Make list of symmetry non-equivalent symops
      ! and the necessary k points
      call create_(k_pts,n_k,3)
      call make_unique_X_SF_k_pts_(self%crystal,k_pts)

      ! X-ray or PND?
      spin = FALSE
      if (present(spin_density)) spin = spin_density

      ! Density matrix (leaky)
      if (NOT spin) then; call make_r_density_mx_(self)
      else; call make_r_Sz_density_mx_(self)
      end if
      ENSURE(allocated(self%density_mx%r),"MOLECULE.RHO:get_Hirshfeld_atom_FFs_for_atom ... no restricted DM")

      ! Basis function cutoff
      cutoff = self%becke_grid%basis_fn_cutoff

      ! Max no of becke grid points per atom
      n_pt = max_no_of_pts_per_atom_(self%becke_grid)

      ! Loop over integration atoms "c"
      c = self%crystal%unique_frag_atom(u)
      rc = self%atom(c)%position

         ! Repetition factor for this atom
      sc = self%crystal%frag_atom_sym_factor(c)
      ! if (sc.is_zero) cycle
      s2 = ONE/sc
      ! s2 = s2*s2

         ! Get the Becke grid for atom "c" (leaky)
      call make_atom_grid_(self%becke_grid,pt,wt,c)
      n_pt = size(pt,1)

         ! Apply stockholder weights "Wc" for atom "c" to "wt"
      call apply_stockholder_atom_weight_(self,wt,c,pt)

         ! Prune the grid with new "wt"
      call prune_grid_(self%becke_grid,pt,wt)

         ! Density for atom "c"
      n_pt = size(pt,1)  ! <-- New n_pt !!!
      call create_(rho,n_pt)
      call make_ED_grid_r_v2_(self,rho,pt,self%density_mx%r,cutoff)

         ! Weight the atom "c" density
      rho = rho*wt

         ! Make the (unique) structure factors
      call make_atom_FFs_on_grid_(self,ff,k_pts,pt,rho,c)
      ff = ff*s2*self%atom(c)%site_occupancy

         ! Clean up atom "c" stuff
      call destroy_(rho)
      call destroy_(wt)
      call destroy_(pt)

      ! Clean up
      call destroy_(k_pts)
   end subroutine


!  ============================================
!  Angular decomposition of the Hirshfeld atoms
!  ============================================

   PURE subroutine make_atom_lm_radial_functions(self,f,radii,a,l_max,l_lebedev,amplitude,weighted,use_cell_map)
   ! Set "f(i,lm)" to the radial functions of Hirshfeld atom "a", l = 0 to
   ! "l_max", column lm = l^2 + l + m + 1 with m = -l to l: on the sphere
   ! of radius "radii(i)" about its nucleus, the integral over directions
   ! of Y_lm times the atom's density w_a rho, by a Lebedev grid of order
   ! "l_lebedev". The sum over l, m of f(i,lm) Y_lm(s^) rebuilds w_a rho on
   ! that sphere. With "amplitude" TRUE the square root sqrt(w_a rho) is
   ! expanded instead, whose power integrals over s are the populations
   ! n^A_l. With "weighted" FALSE the weight is left out, and with
   ! "use_cell_map" TRUE the density is the Fourier density of ".cell_map"
   ! in place of the wavefunction's: together those give the radial
   ! functions of the cell density about the nucleus, which
   ! CELL_MAP:make_radial_functions gives exactly. Y_lm are real harmonics
   ! of unit integral over the sphere; m = -1, 0, 1 are y, z, x.
      type(MOLECULE_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: f
      VEC(REAL,:), IN :: radii
      INT, IN :: a,l_max,l_lebedev
      BIN, IN :: amplitude,weighted,use_cell_map
      type(LEBEDEV_TYPE), allocatable :: lebedev
      MAT(REAL,:,:), allocatable :: Y,shell
      VEC(REAL,:), allocatable :: rho,wa
      VEC(REAL,3) :: ra
      INT :: i,j,n_leb,lv,m,lm,col
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_atom_lm_radial_functions ... no atoms")
   ENSURE(a>=1 AND a<=self%n_atom,"MOLECULE.RHO:make_atom_lm_radial_functions ... atom index out of range")
   ENSURE(size(f,1)==size(radii),"MOLECULE.RHO:make_atom_lm_radial_functions ... f: wrong dim1")
   ENSURE(size(f,2)==(l_max+1)*(l_max+1),"MOLECULE.RHO:make_atom_lm_radial_functions ... f: wrong dim2")
   ENSURE(NOT use_cell_map OR allocated(self%cell_map),"MOLECULE.RHO:make_atom_lm_radial_functions ... no cell_map")

      ! The angular grid, and the harmonics at its points (unit vectors, so
      ! the solid harmonics are the surface harmonics)
      call create_(lebedev)
      call set_l_(lebedev,l_lebedev)
      n_leb = lebedev%n_pts
      call create_(Y,n_leb,(l_max+1)*(l_max+1))
      call make_solid_harmonics_(Y,lebedev%pts,l_max)
      call create_(shell,n_leb,3)
      call create_(rho,n_leb)
      call create_(wa,n_leb)
      ra = self%atom(a)%position
      do i = 1,size(radii)

         ! The sphere of radius radii(i) about the nucleus
         do j = 1,3
            shell(:,j) = radii(i)*lebedev%pts(:,j)+ra(j)
         end do

         ! The atom's density on it
         if (use_cell_map) then
            call make_values_at_(self%cell_map,rho,shell)
         else
            call make_ED_grid_r_(self,rho,shell)
         end if
         if (weighted) then
            call make_stockholder_atom_weight_(self,wa,a,shell)
            rho = wa*rho
         end if
         if (amplitude) rho = sqrt(max(rho,ZERO))

         ! Its harmonic components: the Lebedev weights sum to one
         do lv = 0,l_max
            do m = -lv,lv
               lm = lv*lv+lv+m+1
               col = harmonic_column_(lv,m)
               f(i,lm) = 4*PI*sum(lebedev%wts*Y(:,col)*rho)
            end do
         end do
      end do
      call destroy_(wa)
      call destroy_(rho)
      call destroy_(shell)
      call destroy_(Y)
      call destroy_(lebedev)
   end subroutine

   PURE subroutine make_angular_Hirshfeld_grid(self,grid,pts)
   ! The plot kind angular_hirshfeld: the density rebuilt from the
   ! Hirshfeld atoms' radial functions, see make_rebuilt_atoms_grid.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      call make_rebuilt_atoms_grid_(self,grid,pts,amplitude=FALSE)
   end subroutine

   PURE subroutine make_Hirshfeld_amplitude_grid(self,grid,pts)
   ! The plot kind hirshfeld_amplitude: the square roots of the Hirshfeld
   ! atoms rebuilt from their radial functions, see make_rebuilt_atoms_grid.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      call make_rebuilt_atoms_grid_(self,grid,pts,amplitude=TRUE)
   end subroutine

   PURE subroutine make_rebuilt_atoms_grid(self,grid,pts,amplitude)
   ! Set "grid" at the cartesian points "pts" to the sum over the chosen
   ! atoms A of their Hirshfeld atoms rebuilt from the radial functions:
   !
   !    sum_A sum_lm f^A_lm(|r - c_A|) Y_lm(r - c_A),
   !
   ! l from 0 to the cell_map block's l_max, or l_value alone when that is
   ! set, so the map is the density filtered to those l; with every atom
   ! and l_max large enough it tends to the electron density. "amplitude"
   ! rebuilds sqrt(w_A rho) instead. The atoms are those of the block's
   ! atoms= (indices or tags), or all of them. Each atom's radial functions
   ! are made on a table of radii 0.05 bohr apart to 10 bohr and
   ! interpolated cubically between them; beyond 10 bohr an atom adds
   ! nothing. Values are in atomic units.
      type(MOLECULE_TYPE), IN :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      BIN, IN :: amplitude
      MAT(REAL,:,:), allocatable :: f,Y,d
      VEC(REAL,:), allocatable :: radii
      VEC(INT,:), allocatable :: atoms
      VEC(REAL,3) :: ca
      REAL :: step,r_max,s,t,val,sl
      REAL :: fm1,f0,f1,f2
      INT :: n_rad,n_lm,n_pts,i,j,k,a,lv,m,lm,col,l_lo,l_hi,l_max,l_leb
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_rebuilt_atoms_grid ... no atoms")
   ENSURE(allocated(self%cell_map),"MOLECULE.RHO:make_rebuilt_atoms_grid ... no cell_map block")
   ENSURE(size(grid)==size(pts,1),"MOLECULE.RHO:make_rebuilt_atoms_grid ... grid: wrong dim")
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_rebuilt_atoms_grid ... pts: wrong dim2")

      ! Which l, which atoms
      l_max = self%cell_map%l_max
      if (self%cell_map%l_value>=0) then
         l_lo = self%cell_map%l_value; l_hi = self%cell_map%l_value
         l_max = max(l_max,l_hi)
      else
         l_lo = 0; l_hi = l_max
      end if
      n_lm = (l_max+1)*(l_max+1)
      l_leb = max(2*l_max+2,29)
      call atoms_for_cell_map_(self,atoms)

      ! The table of radii
      step = 0.05d0
      r_max = 10.0d0
      n_rad = nint(r_max/step)+1
      call create_(radii,n_rad)
      do i = 1,n_rad
         radii(i) = (i-1)*step
      end do
      n_pts = size(pts,1)
      grid = ZERO
      call create_(f,n_rad,n_lm)
      call create_(d,n_pts,3)
      call create_(Y,n_pts,n_lm)
      do k = 1,size(atoms)
         a = atoms(k)
         ca = self%atom(a)%position
         call make_atom_lm_radial_functions_(self,f,radii,a,l_max,l_leb,amplitude,TRUE,FALSE)

         ! The solid harmonics of the displacements from the nucleus
         do i = 1,3
            d(:,i) = pts(:,i)-ca(i)
         end do
         call make_solid_harmonics_(Y,d,l_max)

         ! Each point: interpolate the radial functions at its distance from
         ! the four table entries around it, and add the harmonics' sum
         do i = 1,n_pts
            s = sqrt(d(i,1)*d(i,1)+d(i,2)*d(i,2)+d(i,3)*d(i,3))
            if (s>=r_max-2*step) cycle
            j = int(s/step)  ! the table interval
            j = max(1,min(j,n_rad-3))
            t = s/step-j  ! 0 to 1 within it
            val = ZERO
            do lv = l_lo,l_hi
               sl = s**lv
               if (sl<1.0d-30) then
                  if (lv>0) cycle
                  sl = ONE
               end if
               do m = -lv,lv
                  lm = lv*lv+lv+m+1
                  col = harmonic_column_(lv,m)
                  fm1 = f(j,lm); f0 = f(j+1,lm)
                  f1 = f(j+2,lm); f2 = f(j+3,lm)
                  val = val+Y(i,col)/sl*(-t*(t-1)*(t-2)/6*fm1+(t+1)*(t-1)*(t-2)/2*f0-(t+1)*t*(t-2)/2*f1+(t+1)*t*(t-1)/6*f2)
               end do
            end do
            grid(i) = grid(i)+val
         end do
      end do
      call destroy_(Y)
      call destroy_(d)
      call destroy_(f)
      call destroy_(radii)
      call destroy_(atoms)
   end subroutine

   PURE subroutine atoms_for_cell_map(self,atoms)
   ! Set "atoms" to the indices of the atoms named in the cell_map block's
   ! atoms=, each an index or a tag; all atoms when none were named.
      type(MOLECULE_TYPE), IN :: self
      VEC(INT,:), allocatable, OUT :: atoms
      STR(len=STR_SIZE) :: tag,word
      INT :: i,a,n
      BIN :: found,all
   ENSURE(allocated(self%atom),"MOLECULE.RHO:atoms_for_cell_map ... no atoms")
      all = TRUE
      if (allocated(self%cell_map)) then
         if (allocated(self%cell_map%atom_tags)) all = FALSE
      end if
      if (all) then
         call create_(atoms,self%n_atom)
         do a = 1,self%n_atom
            atoms(a) = a
         end do
         return
      end if
      n = size(self%cell_map%atom_tags)
      call create_(atoms,n)
      do i = 1,n
         word = self%cell_map%atom_tags(i)
         if (is_int_(word)) then
            atoms(i) = to_int_(word)
            ENSURE(atoms(i)>=1 AND atoms(i)<=self%n_atom,"MOLECULE.RHO:atoms_for_cell_map ... atoms=: index out of range: "//trim(word))
         else
            call to_lower_case_(word)
            found = FALSE
            do a = 1,self%n_atom
               tag = self%atom(a)%tag
               call to_lower_case_(tag)
               if (tag/=word) cycle
               atoms(i) = a
               found = TRUE
               exit
            end do
            ENSURE(found,"MOLECULE.RHO:atoms_for_cell_map ... atoms=: no atom with tag "//trim(word))
         end if
      end do
   end subroutine


!  ======================
!  Residual density grids
!  ======================

   PURE subroutine make_residual_density_grid(self,grid,pts)
   ! Make the residual density grid from experimental and calculated
   ! structure factors. Needs crystal & those to exist already!
   ! NOTE: unusually, this always returns values in Anstrom^-3
   ! NOTE: USE residual_density_cell FOR RECTILIEAR PTS GRID.
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
      REAL :: fac,fac3
   ENSURE(size(pts,1)==size(grid),"MOLECULE.RHO:make_residual_density_grid ... wrong dim1, pts")
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_residual_density_grid ... wrong dim2, pts")
   ENSURE(allocated(self%atom),"MOLECULE.RHO:make_residual_density_grid ... no atoms to fit")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_residual_density_grid ... no crystal")
   ENSURE(self%crystal%fragment_info_made,"MOLECULE.RHO:make_residual_density_grid ... no crystal fragment info")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_residual_density_grid ... no crystal xray data")

      ! Make the residual map
      call make_residual_density_grid_(self%crystal,grid,pts)

      ! Change to /A3
      fac = ANGSTROM_PER_BOHR
      fac3 = BOHR_PER_ANGSTROM**3
      grid = fac3*grid
   end subroutine

   subroutine make_cell_map_grid(self,grid,pts)
   ! Make the map described by ".cell_map" at the cartesian points "pts",
   ! one row per point: the residual, observed or model density of the
   ! unit cell, or with l_value set its local moment of that angular
   ! momentum (see CELL_MAP:make_moment_at).
   ! NOTE: like the residual map, the values are in Angstrom^-3
      type(MOLECULE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: grid
      MAT(REAL,:,:), IN :: pts
   ENSURE(size(pts,1)==size(grid),"MOLECULE.RHO:make_cell_map_grid ... wrong dim1, pts")
   ENSURE(size(pts,2)==3,"MOLECULE.RHO:make_cell_map_grid ... wrong dim2, pts")
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:make_cell_map_grid ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:make_cell_map_grid ... no crystal xray data")
      if (NOT allocated(self%cell_map)) call create_(self%cell_map)
      DIE_IF(self%cell_map%sharpen_U>ZERO AND self%cell_map%l_value<0,"MOLECULE.RHO:make_cell_map_grid ... sharpen_u needs a window: set l_value")
      DIE_IF(is_deformation_kind_(self%cell_map) AND NOT allocated(self%cell_map%F_pro),"MOLECULE.RHO:make_cell_map_grid ... a deformation kind needs the promolecule: give cell_map= after the structure factors")
      call make_cell_map_(self%crystal,self%cell_map)
      call make_map_at_(self%cell_map,grid,pts)

      ! Change to /A3
      grid = BOHR_PER_ANGSTROM**3*grid
   end subroutine


!  ========================
!  Min-max residual density
!  ========================

   subroutine get_minmax_residual_density(self,output)
   ! Evaluate the min and max residual density from the
   ! fitted structure factors, evaluated on a grid 0.1 A
   ! per side. Also evaluate the residuals at the atom
   ! centers and report the worst one. Store results in the
   ! diffraction data object.
      type(MOLECULE_TYPE), INOUT :: self
      BIN, IN :: output
   ENSURE(allocated(self%crystal),"MOLECULE.RHO:get_minmax_residual_density ... no crystal")
   ENSURE(allocated(self%crystal%xray_data),"MOLECULE.RHO:get_minmax_residual_density ... no crystal data")
      call get_minmax_residual_density_p_(self,self%crystal%xray_data%min_residual,self%crystal%xray_data%max_residual,self%crystal%xray_data%rms_residual,output)
   end subroutine

   subroutine get_minmax_residual_density_p(self,minp,maxp,rmsp,output)
   ! Evaluate the min and max residual density from the
   ! fitted structure factors, evaluated on a grid 0.1 A
   ! per side. Also evaluate the residuals at the atom
   ! centers and report the worst one.
      type(MOLECULE_TYPE), INOUT :: self
      REAL, OUT :: minp,maxp,rmsp
!     minpa,maxpa,rmspa :: REAL, OUT
!     minat,maxat       :: STR, OUT
      BIN, IN :: output
      VEC(REAL,:), allocatable :: map,map_peeled
      MAT(REAL,:,:), allocatable :: pt
      INT :: n_pt
      INT :: nx,ny,nz
      INT :: n_peeled
      MAT3(REAL,:,:,:), allocatable :: map_mat3
      VEC(REAL,3) :: maxpos,minpos
      INT :: maxa,mina
      VEC(INT,:), allocatable :: maxats,minats
      VEC(STR(len=STR_SIZE),:), allocatable :: maxtag,mintag
      REAL :: fac,fac3,maxd,mind
   DIE_IF(NOT allocated(self%atom),"MOLECULE.RHO:get_minmax_residual_density_p ... no atoms to fit")
   DIE_IF(NOT allocated(self%crystal),"MOLECULE.RHO:get_minmax_residual_density_p ... no crystal")
   DIE_IF(NOT self%crystal%fragment_info_made,"MOLECULE.RHO:get_minmax_residual_density_p ... no crystal fragment info")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.RHO:get_minmax_residual_density_p ... no crystal data")
   DIE_IF(NOT allocated(self%crystal%xray_data),"MOLECULE.RHO:get_minmax_residual_density_p ... no crystal xray data")
      if (NOT allocated(self%plot_grid)) call create_(self%plot_grid)

      ! Set up
      call set_plot_kind_(self%plot_grid,"residual_density")
      call set_box_scale_factor_(self%plot_grid,ONE)
      if (is_zero_(self%plot_grid%desired_separation)) then
         if (self%plot_grid%n_x==1) call set_desired_separation_(self%plot_grid,0.1*BOHR_PER_ANGSTROM)
         if (self%plot_grid%n_y==1) call set_desired_separation_(self%plot_grid,0.1*BOHR_PER_ANGSTROM)
         if (self%plot_grid%n_z==1) call set_desired_separation_(self%plot_grid,0.1*BOHR_PER_ANGSTROM)
      end if

      ! Use unit cell as bbox 
      call set_bbox_from_unit_cell_(self%plot_grid,self%crystal%unit_cell)
      if (output) then
         call flush_(stdout)
         call text_(stdout,"=====================")
         call text_(stdout,"Residual density data")
         call text_(stdout,"=====================")
         call flush_(stdout)
         call text_(stdout,". Grid has voxels less than 0.1 A per side")
         call text_(stdout,". Length units are in Angstrom")
         call flush_(stdout)
         call put_minimal_(self%plot_grid)
      end if

      ! Make the residual density map
      n_pt = self%plot_grid%n_pt
      call create_(pt,n_pt,3)  ! Needed to see cartesian (xyz) coord
      call create_(map,n_pt)  ! Done in fractionals using plot.nx's
      call set_plot_kind_(self%plot_grid,"residual_density")
      call make_points_(self%plot_grid,pt)
      call make_residual_density_cell_(self%crystal,map,self%plot_grid%n_x,self%plot_grid%n_y,self%plot_grid%n_z)

      ! Change to /A3
      fac = ANGSTROM_PER_BOHR
      fac3 = BOHR_PER_ANGSTROM**3
      map = fac3*map
      maxa = maxloc(map,dim=1)
      mina = minloc(map,dim=1)
      maxp = map(maxa)
      minp = map(mina)

      ! RMS value calculation without double counting of boundaries / Kang
      nx = self%plot_grid%n_x
      ny = self%plot_grid%n_y
      nz = self%plot_grid%n_z
      call create_(map_mat3,nx,ny,nz)  ! Note: map_mat3(1,:,:) = map_mat3(nx,:,:)
      map_mat3 = reshape(map,[nx,ny,nz])  !       map_mat3(:,1,:) = map_mat3(:,ny,:)
                                             !       map_mat3(:,:,1) = map_mat3(:,:,nz)
      n_peeled = (nx-1)*(ny-1)*(nz-1)
      call create_(map_peeled,n_peeled)
      map_peeled = reshape(map_mat3(1:nx-1,1:ny-1,1:nz-1),[n_peeled])
      rmsp = standard_deviation_(map_peeled)  ! Kang 2024 Nov. 1
      maxpos = pt(maxa,:)
      minpos = pt(mina,:)
      maxats = nearest_atoms_to_(self%crystal%unit_cell_atom,maxpos,TOL(1))  ! There could be many
      minats = nearest_atoms_to_(self%crystal%unit_cell_atom,minpos,TOL(1))
      maxtag = self%crystal%unit_cell_atom(maxats)%tag
      mintag = self%crystal%unit_cell_atom(minats)%tag
      maxd = norm2(maxpos-self%crystal%unit_cell_atom(maxats(1))%position)
      mind = norm2(minpos-self%crystal%unit_cell_atom(minats(1))%position)
      if (output) then
         call flush_(stdout)
         call text_(stdout,"Unit cell residual density:")
         call flush_(stdout)
         call show_(stdout,"Maximum                 = ",maxp)
         if (self%crystal%xray_data%do_minmax_atoms) then
            call show_(stdout,"- nearest atom          = ",maxtag)
            call show_(stdout,"- distance to it        = ",maxd*fac)
         end if
         call show_(stdout,"Minimum                 = ",minp)
         if (self%crystal%xray_data%do_minmax_atoms) then
            call show_(stdout,"- nearest atom          = ",mintag)
            call show_(stdout,"- distance to it        = ",mind*fac)
         end if
         call show_(stdout,"RMS                     = ",rmsp)
      end if

      ! Write cube file
      call dump_plot_grid_(self,map,"residual_density",form="cell.cube")

      ! Clean
      call destroy_(pt)
      call destroy_(map)
      call destroy_(map_mat3)
      call destroy_(map_peeled)
!     .plot_grid.destroy

!     ! Create atom coords
!     pt.create(.n_atom,3)
!     map.create(.n_atom)
!     .atom.put_coordinates_to(pt,positions_as_rows=FALSE)
!     .crystal.make_residual_density_map(map,pt)

!     ! Change to /A3
!     map    = fac3*map
!     maxa   = maxloc(map,dim=1)
!     mina   = minloc(map,dim=1)
!     maxpa  = map(maxa)
!     minpa  = map(mina)
!     rmspa  = map.standard_deviation
!     maxat  = .atom(maxa).tag
!     minat  = .atom(mina).tag

   ! if (output) then
   ! stdout.flush
   ! stdout.text("Residuals on atoms:")
   ! stdout.flush
   ! stdout.show("Maximum                 = ",maxpa)
   ! stdout.show("- for atom              = ",maxat)
   ! stdout.show("Minimum                 = ",minpa)
   ! stdout.show("- for atom              = ",minat)
   ! stdout.show("RMS                     = ",rmspa)
   ! end

      ! Clean
!     pt.destroy
!     map.destroy
   end subroutine

end module
