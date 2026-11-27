!==================================================================
!
! MO_LOCALISER: Data structure for localising molecular orbitals
!
! This code was adapted from original work by Janos Angyan.
! A good reference is Kleier et al (1974), JCP 61 p. 3905
!
! $Id: mo_localiser.foo 4009 2013-05-25 07:36:15Z dylan_ $
!
! Copyright (C) Dylan Jayatilaka
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
module MO_LOCALISER_MODULE

#  include "macros"

   include "mo_localiser.use"

   implicit none

   include "mo_localiser.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(MO_LOCALISER_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"MO_LOCALISER:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(MO_LOCALISER_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MO_LOCALISER:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(MO_LOCALISER_TYPE), INOUT :: self
      call destroy_(self%MO)
      call destroy_(self%TM)
      call destroy_(self%dipole)
      call destroy_(self%quadrupole)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self,MO,dipole,quadrupole)
   ! Set up the defaults. "MO" are the original molecular orbital coefficients.
   ! "dipole" are the dipole moment integrals in the AO basis. "quadrupole" are
   ! the quadrupole integrals in the AO basis.
      type(MO_LOCALISER_TYPE), INOUT :: self
      MAT(REAL,:,:), optional, IN :: MO
      MAT3(REAL,:,:,:), optional, IN :: dipole,quadrupole
      INT :: i
      self%algorithm = MO_LOCALISER_ALGORITHM  ! boys
      self%accuracy = MO_LOCALISER_ACCURACY  ! TOL(3)
      self%max_iterations = MO_LOCALISER_MAXIT  ! 20
      self%n_orb = 0
      self%iteration = 0
      if (present(MO)) then
         self%n_orb = size(MO,2)
         self%MO = MO
         call create_(self%TM,self%n_orb,self%n_orb)
      end if
      if (present(dipole)) then
         ENSURE(allocated(self%MO),"MO_LOCALISER:set_defaults ... must have MO's")
         ENSURE(self%n_orb==size(self%MO,2),"MO_LOCALISER:set_defaults ... inconsistent n_orb")
         ENSURE(size(dipole,1)==size(self%MO,1),"MO_LOCALISER:set_defaults ... wrong size, dipole")
         ENSURE(size(dipole,2)==size(self%MO,1),"MO_LOCALISER:set_defaults ... wrong size, dipole")
         ENSURE(size(dipole,3)==3,"MO_LOCALISER:set_defaults ... wrong size, dipole")
         call create_(self%dipole,self%n_orb,self%n_orb,3)
         do i = 1,3
            call change_basis_to_(dipole(:,:,i),self%dipole(:,:,i),self%MO)
         end do
      end if
      if (present(quadrupole)) then
         ENSURE(allocated(self%MO),"MO_LOCALISER:set_defaults ... must have MO's")
         ENSURE(self%n_orb==size(self%MO,2),"MO_LOCALISER:set_defaults ... inconsistent n_orb")
         ENSURE(size(quadrupole,1)==size(self%MO,1),"MO_LOCALISER:set_defaults ... wrong size, dipole")
         ENSURE(size(quadrupole,2)==size(self%MO,1),"MO_LOCALISER:set_defaults ... wrong size, dipole")
         ENSURE(size(quadrupole,3)==6,"MO_LOCALISER:set_defaults ... wrong size, dipole")
         call create_(self%quadrupole,self%n_orb,self%n_orb,6)
         do i = 1,6
            call change_basis_to_(quadrupole(:,:,i),self%quadrupole(:,:,i),self%MO)
         end do
      end if
   end subroutine

   PURE subroutine set_accuracy(self,acc)
   ! Set the accuracy
      type(MO_LOCALISER_TYPE), INOUT :: self
      REAL, IN :: acc
   ENSURE(acc>ZERO,"MO_LOCALISER:set_accuracy ... accuracy must be positive")
      self%accuracy = acc
   end subroutine

   PURE subroutine set_algorithm(self,alg)
   ! Set the kind of algorithm
      type(MO_LOCALISER_TYPE), INOUT :: self
      STR(len=*), IN :: alg
   ENSURE(is_one_of_(alg,["boys"]),"MO_LOCALISER:set_algorithm ... unknown algorithm")
      self%algorithm = alg
      call to_lower_case_(self%algorithm)
   end subroutine

   PURE subroutine set_max_iterations(self,maxit)
   ! Set the maximum number of iterations
      type(MO_LOCALISER_TYPE), INOUT :: self
      INT, IN :: maxit
   ENSURE(maxit>0,"MO_LOCALISER:set_max_iterations ... must have positive iterations")
      self%max_iterations = maxit
   end subroutine

   PURE subroutine set_MO(self,MO)
   ! Set "MO" to be the current localised MO's
      type(MO_LOCALISER_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: MO
   ENSURE(allocated(self%MO),"MO_LOCALISER:set_MO ... no MO's")
      self%MO = MO
   end subroutine

   PURE subroutine get_MO(self,MO)
   ! Get "MO" to be the current localised MO's
      type(MO_LOCALISER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: MO
   ENSURE(allocated(self%MO),"MO_LOCALISER:get_MO ... no MO's")
      MO = self%MO
   end subroutine


!  ============
!  Read methods
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(MO_LOCALISER_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","MO_LOCALISER:read_keywords ... expecting open bracket symbol, {")

      ! Read the open braces {

      ! The following code is inherited from OBJECT
      call read_(stdin,word)

      ! Echo keyword if requested
      if (tonto%keyword_echo) then
         call flush_(stdout)
         call text_(stdout,"keyword found --> "//trim(word))
      end if
      do  ! Loop over input keywords

         ! Read the keyword
         call read_(stdin,word)
         DIE_IF(reverted_(stdin),"MO_LOCALISER:read_keywords ... unexpected end of file while reading list keywords")
         call to_lower_case_(word)

         ! Append any lonely = sign
         if (NOT exhausted_(stdin%buffer)) then
            if (next_item_(stdin)=="=") then
               call skip_next_item_(stdin)
               word = trim(word)//"="
            end if
         end if

         ! Echo keyword if requested
         if (tonto%keyword_echo) then
            call flush_(stdout)
            call text_(stdout,"keyword found --> "//trim(word))
         end if

         ! Exit if keyword is closing brace }
         if (word=="}") exit

         ! Process the keyword 
         call process_keyword_(self,word)
      end do
   end subroutine

   subroutine process_keyword(self,keyword)
   ! Process command "keyword". Any required data needed by the "keyword" is
   ! inputted from "stdin".
      type(MO_LOCALISER_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                          ")  ! exit read_loop
      case ("accuracy=                  "); call read_accuracy_(self)
      case ("algorithm=                 "); call read_algorithm_(self)
      case ("boys_localise              "); call boys_localise_(self)
      case ("max_iterations=            "); call read_max_iterations_(self)
      case ("put_analysis               "); call put_analysis_(self)
      case ("put_centroids              "); call put_centroids_(self)
      case ("put_dipoles                "); call put_dipoles_(self)
      case ("put_gradient               "); call put_gradient_(self)
      case ("put_lmos                   "); call put_LMOs_(self)
      case ("put_moments                "); call put_moments_(self)
      case ("put_mos                    "); call put_MOs_(self)
      case ("put_quadrupoles            "); call put_quadrupoles_(self)
      case ("put_tranformation_mx   "); call put_TM_(self)
      case ("update_mos                 "); call update_MOs_(self)
      case default; allocate(tonto%known_keywords(15))
      tonto%known_keywords(1) = "}                          "
      tonto%known_keywords(2) = "accuracy=                  "
      tonto%known_keywords(3) = "algorithm=                 "
      tonto%known_keywords(4) = "boys_localise              "
      tonto%known_keywords(5) = "max_iterations=            "
      tonto%known_keywords(6) = "put_analysis               "
      tonto%known_keywords(7) = "put_centroids              "
      tonto%known_keywords(8) = "put_dipoles                "
      tonto%known_keywords(9) = "put_gradient               "
      tonto%known_keywords(10) = "put_lmos                   "
      tonto%known_keywords(11) = "put_moments                "
      tonto%known_keywords(12) = "put_mos                    "
      tonto%known_keywords(13) = "put_quadrupoles            "
      tonto%known_keywords(14) = "put_tranformation_mx   "
      tonto%known_keywords(15) = "update_mos                 "
      call unknown_(tonto,word,"MO_LOCALISER:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_accuracy(self)
   ! Read the accuracy to converge the orbitals to
      type(MO_LOCALISER_TYPE), INOUT :: self
      REAL :: acc
      call read_(stdin,acc)
      call set_accuracy_(self,acc)
   end subroutine

   subroutine read_algorithm(self)
   ! Read the algorithm to use
      type(MO_LOCALISER_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: alg
      call read_(stdin,alg)
      call set_algorithm_(self,alg)
   end subroutine

   subroutine read_max_iterations(self)
   ! Read the algorithm to use
      type(MO_LOCALISER_TYPE), INOUT :: self
      INT :: maxit
      call read_(stdin,maxit)
      call set_max_iterations_(self,maxit)
   end subroutine


!  ==============
!  The algorithms
!  ==============

   subroutine boys_localise(self)
   ! Localization transformation of a subset of molecular orbitals by
   ! successive 2x2 rotations. Ref: Kleier et al (1974), JCP 61 p. 3905
   ! The function maximised in the sum-of-states function:
   !    sum_i^n_orb  sum_k^3 <i|r_k|i><i|r_k|i>
      type(MO_LOCALISER_TYPE), INOUT :: self
      VEC(REAL,3) :: dii,djj,dij
      REAL :: qii,qjj,qij,qik,qjk,dik,djk
      REAL :: c4a,s4a,fac,sq,xm,xp,ym,yp,z
      REAL :: a,b,sa,ca,sa2,ca2,csa
      REAL :: sos,old_sos,diff,del
      BIN :: done,converged,exceeded_maxit,sos_decreases
      MAT(REAL,:,:), allocatable :: g
      VEC(INT,2) :: pos
      INT :: i,j,k,c
      sos = boys_SOS_(self)
      call create_(g,self%n_orb,self%n_orb)
      self%iteration = 0
      diff = ZERO
      call put_boys_results_(self,sos,diff,FALSE,FALSE,FALSE)

      !  Main loop for 2x2 rotations
      call to_unit_mx_(self%TM)
      do
         self%iteration = self%iteration+1
         call kleier_gradient_(self,g)
         pos = maxloc(g)
         i = maxval(pos)
         j = minval(pos)
         dii = self%dipole(i,i,:)
         djj = self%dipole(j,j,:)
         dij = self%dipole(i,j,:)
         a = ZERO; b = ZERO
         do c = 1,3  ! Evaluate A and B functions
            del = djj(c)-dii(c)
            qij = dij(c)
            a = a-QUARTER*del*del+qij*qij
            b = b+del*qij
         end do
         if (a==ZERO AND b==ZERO) then
            ca = ONE
            sa = ZERO
         else

            ! Find optimal rotation angle
            fac = ONE/dsqrt(a*a+b*b)
            c4a = -a*fac
            s4a = b*fac
            sq = dsqrt(ONE-HALF*(ONE-c4a))
            xm = HALF*(ONE-sq); ym = ONE-xm
            xp = HALF*(ONE+sq); yp = ONE-xp
            z = FOUR*dsqrt(xm*ym)*(xm-ym)
            if (equals_(z,s4a)) then
               ca = dsqrt(xm)
               sa = dsqrt(ym)
            else
               ca = dsqrt(xp)
               sa = dsqrt(yp)
            end if
         end if

         ! Update transformation matrix
         do k = 1,self%n_orb
            dik = ca*self%TM(k,i)-sa*self%TM(k,j)
            djk = sa*self%TM(k,i)+ca*self%TM(k,j)
            self%TM(k,i) = dik
            self%TM(k,j) = djk
         end do

         ! Rotate integrals corresponding to the orbital transformation
         !    i =  cos(alpha) * i - sin (alpha) * j 
         !    j =  sin(alpha) * i + cos (alpha) * j 
         do c = 1,3
            ca2 = ca*ca
            sa2 = sa*sa
            csa = ca*sa
            qii = ca2*dii(c)+sa2*djj(c)-TWO*csa*dij(c)
            qjj = sa2*dii(c)+ca2*djj(c)+TWO*csa*dij(c)
            qij = (ca2-sa2)*dij(c)+csa*(dii(c)-djj(c))
            do k = 1,self%n_orb
               if (k==i) then
                  qik = qii; qjk = qij
               else if (k==j) then
                  qik = qij; qjk = qjj
               else
                  dik = self%dipole(i,k,c)
                  djk = self%dipole(j,k,c)
                  qik = ca*dik-sa*djk
                  qjk = sa*dik+ca*djk
               end if
               self%dipole(i,k,c) = qik
               self%dipole(k,i,c) = qik
               self%dipole(j,k,c) = qjk
               self%dipole(k,j,c) = qjk
            end do
         end do  ! c=x,y,z
         old_sos = sos
         sos = boys_SOS_(self)
         diff = sos-old_sos

         ! Check convergence          
         converged = abs((diff/self%n_orb))<self%accuracy
         exceeded_maxit = self%iteration>self%max_iterations
         sos_decreases = diff<ZERO
         call put_boys_results_(self,sos,diff,converged,exceeded_maxit,sos_decreases)
         done = converged OR exceeded_maxit OR sos_decreases
         if (done) exit
      end do  ! -- main do loop
      call destroy_(g)
   end subroutine

   PURE function boys_SOS(self) result (res)
   ! Boys sum-of-squares of orbital centroids localization function
      type(MO_LOCALISER_TYPE), IN :: self
      REAL :: res
      INT :: i,c
      REAL :: val
      res = ZERO
      do c = 1,3
         do i = 1,self%n_orb
            val = self%dipole(i,i,c)
            res = res+val*val
         end do
      end do
   end function

   PURE function boys_centroid_SOS(self) result (res)
   ! Boys sum of squares of the distances betwen orbital centroids. Maximising
   ! this is equivalent to maximising the SOS localization function.
      type(MO_LOCALISER_TYPE), IN :: self
      REAL :: res
      INT :: i,j,c
      REAL :: ci,cj
      res = ZERO
      do i = 2,self%n_orb
         do j = 1,i-1
            do c = 1,3
               ci = self%dipole(i,i,c)
               cj = self%dipole(j,j,c)
               res = res+(ci-cj)*(ci-cj)
            end do
         end do
      end do
   end function

   PURE function kleier_gradient_norm(self) result (grad)
   ! Localization function gradient (Kleier et al. JCP, 61 (1974) 3905)
      type(MO_LOCALISER_TYPE), IN :: self
      REAL :: grad
      INT :: i,j,c
      REAL :: tmp
      grad = ZERO
      do i = 2,self%n_orb
         do j = 1,i-1
            tmp = ZERO
            do c = 1,3
               tmp = tmp+(self%dipole(i,i,c)-self%dipole(j,j,c))*self%dipole(i,j,c)
            end do
            grad = grad+tmp*tmp
         end do
      end do
      grad = FOUR*sqrt(grad)
   end function

   PURE subroutine kleier_gradient(self,grad)
   ! Localization function gradient (Kleier et al. JCP, 61 (1974) 3905)
      type(MO_LOCALISER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: grad
      INT :: i,j,c
      REAL :: tmp
      grad = ZERO
      do i = 2,self%n_orb
         do j = 1,i-1
            tmp = ZERO
            do c = 1,3
               tmp = tmp+(self%dipole(i,i,c)-self%dipole(j,j,c))*self%dipole(i,j,c)
            end do
            grad(i,j) = tmp
            grad(j,i) = -tmp
         end do
      end do
   end subroutine

   subroutine put_boys_results(self,sos,diff,converged,exceeded_maxit,sos_decreases)
   ! Print LMO centroids
      type(MO_LOCALISER_TYPE), INOUT :: self
      REAL, IN :: sos,diff
      BIN, IN :: converged,exceeded_maxit,sos_decreases
      BIN :: done
      INT :: i
      if (self%iteration==0) then
         call flush_(stdout)
         call text_(stdout,"Boys localisation procedure:")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=5)
         call flush_(stdout)
         call tab_(stdout,int_fields=1)
         call put_(stdout,"Centroid")
         call put_(stdout,"Centroid")
         call tab_(stdout,real_fields=1)
         call put_(stdout,"Normalised")
         call flush_(stdout)
         call put_(stdout,"Iter",int_width=TRUE)
         call put_(stdout,"diff SOS")
         call put_(stdout,"SOS")
         call put_(stdout,"Delta")
         call put_(stdout,"delta")
         call put_(stdout,"Gradient")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=5)
         call flush_(stdout)
      end if
      if (self%iteration>0) then
         call put_(stdout,self%iteration)
         call put_(stdout,boys_centroid_SOS_(self))
         call put_(stdout,sos)
         call put_(stdout,diff)
         call put_(stdout,diff/self%n_orb)
         call put_(stdout,kleier_gradient_norm_(self))
         call flush_(stdout)
      end if
      done = converged OR exceeded_maxit OR sos_decreases
      if (done) then
         call dash_(stdout,int_fields=1,real_fields=5)
         call flush_(stdout)
         if (converged) then
            call text_(stdout,"Boys procedure converged")
         else if (exceeded_maxit) then
            call text_(stdout,"Boys procedure unconverged: exceeded maximum iterations")
         else if (sos_decreases) then
            call text_(stdout,"Boys procedure converged? SOS decreases")
         end if
         call flush_(stdout)
         do i = 1,6
            call change_basis_to_(self%quadrupole(:,:,i),self%quadrupole(:,:,i),self%TM)
         end do
         call put_analysis_(self)  ! This is important, it also does some sorting
         call put_moments_(self)
      end if
   end subroutine

   PURE subroutine update_MOs(self)
   ! Update the original MO's by multiplying with the .TM  matrix to produce the
   ! localised MO's. Reset .TM matrix to unity.
      type(MO_LOCALISER_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable :: LMO
   ENSURE(allocated(self%MO),"MO_LOCALISER:update_MOs ... no MO's")
   ENSURE(allocated(self%TM),"MO_LOCALISER:update_MOs ... no transformation matrix")
      call create_(LMO,size(self%MO,1),size(self%MO,2))
      call to_product_of_(LMO,self%MO,self%TM)
      self%MO = LMO
      call to_unit_mx_(self%TM)
      call destroy_(LMO)
   end subroutine


!  =======================
!  analysis of the results
!  =======================

   PURE subroutine get_orbital_centroids(self,c)
   ! Get the orbital centroids in "c"
      type(MO_LOCALISER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: c
      INT :: i
   ENSURE(size(c,1)==3,"MO_LOCALISER:get_orbital_centroids ... wrong size, c")
   ENSURE(size(c,2)==self%n_orb,"MO_LOCALISER:get_orbital_centroids ... wrong size, c")
      do i = 1,self%n_orb
         c(:,i) = self%dipole(i,i,:)
      end do
   end subroutine

   PURE subroutine get_spread_mx(self,rsq,rtr,ran)
   ! Calculate spread matrix "rsq" of a set of orbitals:
   ! rsq(i,a,b) = <i| r_a r_b |i> - <i|r_a|i> <i|r_b|i>
   ! The last two indices have been collapsed to one, and the order is:
   ! ab = xx,yy,zz,xy,xz,yz.
   ! Also get the trace and anisotropy, "rtr" and "ran" of the spread matrix.
      type(MO_LOCALISER_TYPE), IN :: self
      MAT(REAL,:,:), OUT :: rsq
      VEC(REAL,:), OUT :: rtr,ran
      REAL :: x,y,z,xx,yy,zz,xy,xz,yz,val
      INT :: i
   ENSURE(size(rsq,1)==self%n_orb,"MO_LOCALISER:get_spread_mx ... wrong size, rsq")
   ENSURE(size(rsq,2)==6,"MO_LOCALISER:get_spread_mx ... wrong size, rsq")
   ENSURE(size(rtr)==self%n_orb,"MO_LOCALISER:get_spread_mx ... wrong size, rtr")
   ENSURE(size(ran)==self%n_orb,"MO_LOCALISER:get_spread_mx ... wrong size, ran")
   ENSURE(allocated(self%quadrupole),"MO_LOCALISER:get_spread_mx ... no quadrupole integrals")
   ENSURE(size(self%quadrupole,1)>=self%n_orb,"MO_LOCALISER:get_spread_mx ... wrong size, quadrupole_integrals")
      do i = 1,self%n_orb

         ! Orbital centroids
         x = self%dipole(i,i,1)
         y = self%dipole(i,i,2)
         z = self%dipole(i,i,3)

         ! Make the spread matrix moments
         xx = self%quadrupole(i,i,1)-x*x
         yy = self%quadrupole(i,i,2)-y*y
         zz = self%quadrupole(i,i,3)-z*z
         xy = self%quadrupole(i,i,4)-x*y
         xz = self%quadrupole(i,i,5)-x*z
         yz = self%quadrupole(i,i,6)-y*z

         ! Assign the spread matrix
         rsq(i,1) = xx
         rsq(i,2) = yy
         rsq(i,3) = zz
         rsq(i,4) = xy
         rsq(i,5) = xz
         rsq(i,6) = yz

         ! Trace of orbital spread matrices
         rtr(i) = xx+yy+zz

         ! Anisotropy of spread matrices
         val = (xx*xx+yy*yy+zz*zz)+THREE*(xy*xy+xz*xz+yz*yz)-xy*xz-xy*yz-xz*yz
         ran(i) = sqrt(val)
      end do
   end subroutine

   subroutine put_centroids(self)
   ! Print LMO centroids
      type(MO_LOCALISER_TYPE), IN :: self
      INT :: i
      VEC(REAL,3) :: centroid
      call flush_(stdout)
      call text_(stdout,"LMO centroids:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=3)
      call flush_(stdout)
      call put_(stdout,"Local",int_width=TRUE)
      call tab_(stdout,real_fields=1)
      call put_(stdout,"Centroid")
      call flush_(stdout)
      call put_(stdout,"Orb",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=3)
      call flush_(stdout)
      do i = 1,self%n_orb
         centroid = self%dipole(i,i,:)
         call put_(stdout,i)
         call put_(stdout,centroid(1))
         call put_(stdout,centroid(2))
         call put_(stdout,centroid(3))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=3)
   end subroutine

   subroutine put_analysis(self)
   ! Sort the LMO centroids and print the centroids and moment analysis
      type(MO_LOCALISER_TYPE), INOUT :: self
      INT :: i
      REAL :: x,y,z
      MAT(REAL,:,:), allocatable :: rsq
      VEC(REAL,:), allocatable :: rtr,ran
      VEC(INT,:), allocatable :: ind
   ENSURE(self%n_orb>0,"MO_LOCALISER:put_analysis ... n_orb isnot positive")
   ENSURE(allocated(self%dipole),"MO_LOCALISER:put_analysis ... no dipole integrals")
   ENSURE(size(self%dipole,1)>=self%n_orb,"MO_LOCALISER:put_analysis ... wrong size, dipole integrals")
      call create_(rtr,self%n_orb)
      call create_(ran,self%n_orb)
      call create_(rsq,self%n_orb,6)
      call get_spread_mx_(self,rsq,rtr,ran)
      call destroy_(rsq)

      ! Sort according to rtr
      call create_(ind,self%n_orb)
      call quick_sort_(rtr,ind)

      ! Reorder ...
      rtr = rtr(ind)
      ran = ran(ind)
      self%dipole = self%dipole(ind,ind,:)
      self%quadrupole = self%quadrupole(ind,ind,:)
      self%TM = self%TM(:,ind)
      call destroy_(ind)
      call flush_(stdout)
      call text_(stdout,"Localised molecular orbital centroids and moment analysis:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      call put_(stdout,"Local",int_width=TRUE)
      call tab_(stdout,real_fields=1)
      call put_(stdout,"Centroid")
      call tab_(stdout,real_fields=1)
      call put_(stdout,"Trace")
      call put_(stdout,"Anisotropy")
      call flush_(stdout)
      call put_(stdout,"Orb",int_width=TRUE)
      call put_(stdout,"x")
      call put_(stdout,"y")
      call put_(stdout,"z")
      call put_(stdout,"r2")
      call put_(stdout,"r2")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=5)
      do i = 1,self%n_orb
         call put_(stdout,i)
         x = self%dipole(i,i,1)
         y = self%dipole(i,i,2)
         z = self%dipole(i,i,3)
         call put_(stdout,x)
         call put_(stdout,y)
         call put_(stdout,z)
         call put_(stdout,rtr(i))
         call put_(stdout,ran(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=5)
      call destroy_(ran)
      call destroy_(rtr)
   end subroutine

   subroutine put_moments(self)
   ! Print LMO moments
      type(MO_LOCALISER_TYPE), IN :: self
      INT :: i
      MAT(REAL,:,:), allocatable :: rsq
      VEC(REAL,:), allocatable :: rtr,ran
   ENSURE(self%n_orb>0,"MO_LOCALISER:put_moments ... n_orb isnot positive")
   ENSURE(allocated(self%dipole),"MO_LOCALISER:put_moments ... no dipole integrals")
   ENSURE(size(self%dipole,1)>=self%n_orb,"MO_LOCALISER:put_moments ... wrong size, dipole integrals")
      call create_(rtr,self%n_orb)
      call create_(ran,self%n_orb)
      call create_(rsq,self%n_orb,6)
      call get_spread_mx_(self,rsq,rtr,ran)
      call flush_(stdout)
      call text_(stdout,"Localised molecular orbital moments:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      call put_(stdout,"Orb",int_width=TRUE)
      call put_(stdout,"xx")
      call put_(stdout,"yy")
      call put_(stdout,"zz")
      call put_(stdout,"xy")
      call put_(stdout,"xz")
      call put_(stdout,"yz")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=6)
      do i = 1,self%n_orb
         call put_(stdout,i)
         call put_(stdout,rsq(i,1))
         call put_(stdout,rsq(i,2))
         call put_(stdout,rsq(i,3))
         call put_(stdout,rsq(i,4))
         call put_(stdout,rsq(i,5))
         call put_(stdout,rsq(i,6))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=6)
      call destroy_(rsq)
      call destroy_(ran)
      call destroy_(rtr)
   end subroutine

   subroutine put_MOs(self)
   ! Print the MO's
      type(MO_LOCALISER_TYPE), IN :: self
   ENSURE(allocated(self%MO),"MO_LOCALISER:put_MOs ... no MO's")
      call flush_(stdout)
      call text_(stdout,"Original MO's:")
      call flush_(stdout)
      call put_(stdout,self%MO)
   end subroutine

   subroutine put_TM(self)
   ! Print the transformation matrix
      type(MO_LOCALISER_TYPE), IN :: self
   ENSURE(allocated(self%TM),"MO_LOCALISER:put_TM ... no transformanhtion matrix")
      call flush_(stdout)
      call text_(stdout,"LMO Transformation matrix:")
      call flush_(stdout)
      call put_(stdout,self%TM)
      call flush_(stdout)
      call text_(stdout,"Check transformation matrix orthogonality:")
      call flush_(stdout)
      call put_(stdout,matmul(self%TM,transpose(self%TM)))
   end subroutine

   subroutine put_LMOs(self)
   ! Print the MO's
      type(MO_LOCALISER_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: LMO
   ENSURE(allocated(self%MO),"MO_LOCALISER:put_LMOs ... no MO's")
      call create_(LMO,size(self%MO,1),size(self%MO,2))
      call to_product_of_(LMO,self%MO,self%TM)
      call flush_(stdout)
      call text_(stdout,"Localised MO's:")
      call flush_(stdout)
      call put_(stdout,LMO)
      call destroy_(LMO)
   end subroutine

   subroutine put_gradient(self)
   ! Print the MO's
      type(MO_LOCALISER_TYPE), IN :: self
      MAT(REAL,:,:), allocatable :: G
   ENSURE(self%n_orb>0,"MO_LOCALISER:put_gradient ... no occupied MO's")
      call create_(G,self%n_orb,self%n_orb)
      call kleier_gradient_(self,G)
      call flush_(stdout)
      call text_(stdout,"Kleier gradient vector:")
      call flush_(stdout)
      call put_(stdout,G)
      call destroy_(G)
   end subroutine

   subroutine put_dipoles(self)
   ! Print the dipoles out
      type(MO_LOCALISER_TYPE), IN :: self
   ENSURE(self%n_orb>0,"MO_LOCALISER:put_dipoles ... n_orb isnot positive")
   ENSURE(allocated(self%dipole),"MO_LOCALISER:put_dipoles ... no dipole integrals")
   ENSURE(size(self%dipole,1)>=self%n_orb,"MO_LOCALISER:put_dipoles ... wrong size, dipole integrals")
      call flush_(stdout)
      call text_(stdout,"Dipole moment integrals:")
      call flush_(stdout)
      call text_(stdout,"x:")
      call flush_(stdout)
      call put_(stdout,self%dipole(:,:,1))
      call flush_(stdout)
      call text_(stdout,"y:")
      call flush_(stdout)
      call put_(stdout,self%dipole(:,:,2))
      call flush_(stdout)
      call text_(stdout,"z:")
      call flush_(stdout)
      call put_(stdout,self%dipole(:,:,3))
   end subroutine

   subroutine put_quadrupoles(self)
   ! Print the quadrupoles out
      type(MO_LOCALISER_TYPE), IN :: self
   ENSURE(self%n_orb>0,"MO_LOCALISER:put_quadrupoles ... n_orb isnot positive")
   ENSURE(allocated(self%quadrupole),"MO_LOCALISER:put_quadrupoles ... no quadrupole integrals")
   ENSURE(size(self%quadrupole,1)>=self%n_orb,"MO_LOCALISER:put_quadrupoles ... wrong size, quadrupole integrals")
      call flush_(stdout)
      call text_(stdout,"Quadrupole moment integrals:")
      call flush_(stdout)
      call text_(stdout,"xx:")
      call flush_(stdout)
      call put_(stdout,self%quadrupole(:,:,1))
      call flush_(stdout)
      call text_(stdout,"yy:")
      call flush_(stdout)
      call put_(stdout,self%quadrupole(:,:,2))
      call flush_(stdout)
      call text_(stdout,"zz:")
      call flush_(stdout)
      call put_(stdout,self%quadrupole(:,:,3))
      call flush_(stdout)
      call text_(stdout,"xy:")
      call flush_(stdout)
      call put_(stdout,self%quadrupole(:,:,4))
      call flush_(stdout)
      call text_(stdout,"xz:")
      call flush_(stdout)
      call put_(stdout,self%quadrupole(:,:,5))
      call flush_(stdout)
      call text_(stdout,"yz:")
      call flush_(stdout)
      call put_(stdout,self%quadrupole(:,:,6))
   end subroutine

end module
