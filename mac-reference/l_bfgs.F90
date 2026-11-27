!==================================================================
!
! L_BFGS: Low memory BFGS minimiser routine.
!
! Copyright (C) Max Davidson, 2017
! Copyright (C) Dylan Jayatilaka, 2021
!
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
module L_BFGS_MODULE

#  include "macros"

   include "l_bfgs.use"

   implicit none

   include "l_bfgs.int"


contains

! ==========
! Allocation
! ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(L_BFGS_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"L_BFGS:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(L_BFGS_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"L_BFGS:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(L_BFGS_TYPE), INOUT :: self
      call destroy_(self%par)
      call destroy_(self%grd)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set up the default settings
      type(L_BFGS_TYPE), INOUT :: self
      self%iteration = 0
      self%start_iteration = DIIS_START_ITERATION
      self%save_iteration = DIIS_SAVE_ITERATION
      self%keep = DIIS_KEEP
      self%grd_norm = 1000d0
      self%convergence_tolerance = DIIS_CONVERGENCE_TOLERANCE
      self%start_tolerance = DIIS_START_TOLERANCE

      ! Clean up (leaky)
      call destroy_ptr_part_(self)
   end subroutine

   PURE subroutine reset_iteration_defaults(self)
   ! Reset defaults without changing user settings
      type(L_BFGS_TYPE), INOUT :: self

      ! The first time called it is iteration 0
      self%iteration = -1

      ! Clean up old stuff (leaky)
      call destroy_ptr_part_(self)
   end subroutine

   PURE subroutine set_convergence_tolerance(self,tolerance)
   ! Read the convergence tolerance
      type(L_BFGS_TYPE), INOUT :: self
      REAL, IN :: tolerance
   ENSURE(tolerance>ZERO,"L_BFGS:set_convergence_tolerance ... must have positive convergence tolerance")
! WARN_IF(tolerance<TOL(6),"convergence tolerance may be too small")
      self%convergence_tolerance = tolerance
   end subroutine

   PURE subroutine set_start_tolerance(self,tolerance)
   ! Read the start tolerance when extrapolation is supposed to start
      type(L_BFGS_TYPE), INOUT :: self
      REAL, IN :: tolerance
   ENSURE(tolerance>ZERO,"L_BFGS:set_start_tolerance ... must have positive start tolerance")
   ENSURE(tolerance>self%convergence_tolerance,"L_BFGS:set_start_tolerance ... start tolerance is <= convergence tolerance")
! WARN_IF(tolerance<TOL(6),"start tolerance may be too small")
      self%start_tolerance = tolerance
   end subroutine

   PURE subroutine set_keep(self,keep)
   ! Set the number of parameter/error vectors to keep 
      type(L_BFGS_TYPE), INOUT :: self
      INT, IN :: keep
   ENSURE(keep>=4,"L_BFGS:set_keep ... no. to keep must be more than 4")
      self%keep = keep
   end subroutine

   pure subroutine set_start_iteration(self,start)
   ! Set at what iteration the procedure should start
      type(L_BFGS_TYPE), INOUT :: self
      INT, IN :: start
      self%start_iteration = start
   end subroutine

   pure subroutine set_save_iteration(self,save_it)
   ! Set at what iteration the procedure should start saving vectors
      type(L_BFGS_TYPE), INOUT :: self
      INT, IN :: save_it
      self%save_iteration = save_it
   end subroutine


!  =================
!  Read the keywords
!  =================

   subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(L_BFGS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","L_BFGS:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"L_BFGS:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process a command "keyword". Data is inputted from "stdin", unless
   ! "word" is a sequence of blank separated strings. In this case,
   ! the sequence is processed as if it were a separate file.
      type(L_BFGS_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                       ")  ! exit case
      case ("convergence_tolerance=  "); call read_convergence_tolerance_(self)
      case ("keep=                   "); call read_keep_(self)
      case ("save_iteration=         "); call read_save_iteration_(self)
      case ("start_iteration=        "); call read_start_iteration_(self)
      case ("start_tolerance=        "); call read_start_tolerance_(self)
      case default; allocate(tonto%known_keywords(6))
      tonto%known_keywords(1) = "}                       "
      tonto%known_keywords(2) = "convergence_tolerance=  "
      tonto%known_keywords(3) = "keep=                   "
      tonto%known_keywords(4) = "save_iteration=         "
      tonto%known_keywords(5) = "start_iteration=        "
      tonto%known_keywords(6) = "start_tolerance=        "
      call unknown_(tonto,word,"L_BFGS:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_convergence_tolerance(self)
   ! Generic read and set a quantity with units
      type(L_BFGS_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_convergence_tolerance_(self,val)
   end subroutine

   subroutine read_start_tolerance(self)
   ! Generic read and set a quantity with units
      type(L_BFGS_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_start_tolerance_(self,val)
   end subroutine

   subroutine read_keep(self)
   ! Generic read and set a quantity with units
      type(L_BFGS_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_keep_(self,val)
   end subroutine

   subroutine read_start_iteration(self)
   ! Generic read and set a quantity with units
      type(L_BFGS_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_start_iteration_(self,val)
   end subroutine

   subroutine read_save_iteration(self)
   ! Generic read and set a quantity with units
      type(L_BFGS_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_save_iteration_(self,val)
   end subroutine


!  ======
!  L-BFGS
!  ======

   subroutine minimize_L_BFGS(self,func,par)
   ! Use the limited memory Broyden-Fletcher-Goldfarb-Shanno method to
   ! minimise the molecular orbitals with gradient function from
   ! Stoll(1980). This code is all per Nocedal & Wright
   ! NOTE: set saved_self before calling self & dself
      type(L_BFGS_TYPE), INOUT :: self
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      VEC(REAL,:), INOUT :: par
      INT :: j,n,m

      ! alpha, c1 and c2 are used in the line search algorithm and
      ! beta, gamma and rho are used in the Hessian approximation
      REAL :: alpha,beta,gamma,rho,c1,c2

      ! a is used to store the various alpha values in the two loop
      ! recursion, p,q and r are used as generic vectors, and as part
      ! of the two loop recursion process
      VEC(REAL,:), allocatable :: a,q,s,y
      self%iteration = self%iteration+1
      n = size(par,1)

      ! Gradient 
      call create_(q,n)
      call func(par,c1,q)

      ! Store the parameter & gradient
      ! NOTE: you should reset if you change
      ! any of the par vectors outside of here ...
      call append_fifo_(self%par,par,self%keep)
      call append_fifo_(self%grd,q,self%keep)

      ! Size of the kept vectors
      m = size(self%par,1)  ! Note self

      ! Converged? Cleanup
      self%grd_norm = norm_(q)
      if (is_converged_(self)) then
         call destroy_(q)
         return
      end if

      ! Workspace
      call create_(s,n)
      call create_(y,n)
      call create_(a,m)
      a = ZERO

      ! The values used for line search algorithms. These values are
      ! the ones which usually work.
      c1 = TOL(3)
      c2 = 9*TOL(1)

      ! Initialises the values used in the two-loop recursion.
      rho = 1
      if (m>1) then
         s = self%par(m)%element-self%par(m-1)%element
         y = self%grd(m)%element-self%grd(m-1)%element
         gamma = dot_product(s,y)/dot_product(y,y)
      else
         gamma = 1
      end if

      ! Will cause the program to exit if a suitable step can not be
      ! determined 
      !alpha = ZERO

      ! The two loop recursion process approximates the Hessian by
      ! using stored values of delta x and delta grad, as well as a
      ! pre-scaling, gamma, to allow for a stepping at alpha = 1, to
      ! be suitable 

      ! H^0 is approximated by I   
      if (m>1) then
         do j = m,2,-1
            s = self%par(j)%element-self%par(j-1)%element
            y = self%grd(j)%element-self%grd(j-1)%element
            rho = 1/(dot_product(y,s))
            a(j) = rho*dot_product(s,q)
            q = q-a(j)*y
         end do
      end if
      q = gamma*q
      do j = 2,m
         s = self%par(j)%element-self%par(j-1)%element
         y = self%grd(j)%element-self%grd(j-1)%element
         rho = 1/dot_product(y,s)
         beta = rho*dot_product(y,q)
         q = q+(a(j)-beta)*s
      end do
      q = -q
      call line_search(func,1d0,par,q,c1,c2,alpha)

      ! Performs the step (and stores the previous x value)
      par = par+alpha*q

      ! Cleanup
      call destroy_(q)
   end subroutine

   subroutine minimize_parabolic_L_BFGS(self,func,par)
   ! Use the limited memory Broyden-Fletcher-Goldfarb-Shanno method to
   ! minimise the molecular orbitals with gradient function from
   ! Stoll(1980). This code is all per Nocedal & Wright. This function
   ! utilises a parabolic stepping protocol rather than a line-search
   ! to reduce memory usage.
   ! NOTE: set saved_self before calling self & dself
      type(L_BFGS_TYPE), INOUT :: self
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      VEC(REAL,:), INOUT :: par
      INT :: j,n,m

      ! alpha, c1 and c2 are used in the line search algorithm and
      ! beta, gamma and rho are used in the Hessian approximation
      REAL :: beta,gamma,rho,c1,c2,E

      ! a is used to store the various alpha values in the two loop
      ! recursion, p,q and r are used as generic vectors, and as part
      ! of the two loop recursion process
      VEC(REAL,:), allocatable :: a,q,g,s,y
      self%iteration = self%iteration+1
      n = size(par,1)

      ! Gradient 
      call create_(q,n)
      call func(par,c1,q)

      ! Store the parameter & gradient
      ! NOTE: you should reset if you change
      ! any of the par vectors outside of here ...
      call append_fifo_(self%par,par,self%keep)
      call append_fifo_(self%grd,q,self%keep)

      ! Size of the kept vectors
      m = size(self%par,1)  ! Note self

      ! Converged?
      self%grd_norm = norm_(q)
      if (is_converged_(self)) then

         ! Cleanup
         call destroy_(q)
         return
      end if

      ! Workspace
      call create_(s,n)
      call create_(y,n)
      call create_(a,m)
      a = ZERO

      ! The values used for line search algorithms. These values are
      ! the ones which usually work.
      c1 = TOL(3)
      c2 = 9*TOL(1)

      ! Initialises the values used in the two-loop recursion.
      rho = 1
      if (m>1) then
         s = self%par(m)%element-self%par(m-1)%element
         y = self%grd(m)%element-self%grd(m-1)%element
         gamma = dot_product(s,y)/dot_product(y,y)
      else
         gamma = 1
      end if

      ! The two loop recursion process approximates the Hessian by
      ! using stored values of delta x and delta grad, as well as a
      ! pre-scaling, gamma, to allow for a stepping at alpha = 1, to
      ! be suitable 

      ! H^0 is approximated by I   
      if (m>1) then
         do j = m,2,-1
            s = self%par(j)%element-self%par(j-1)%element
            y = self%grd(j)%element-self%grd(j-1)%element
            rho = 1/(dot_product(y,s))
            a(j) = rho*dot_product(s,q)
            q = q-a(j)*y
         end do
      end if
      q = gamma*q
      do j = 2,m
         s = self%par(j)%element-self%par(j-1)%element
         y = self%grd(j)%element-self%grd(j-1)%element
         rho = 1/dot_product(y,s)
         beta = rho*dot_product(y,q)
         q = q+(a(j)-beta)*s
      end do

      ! This is the step toward the minimum
      q = -q
      if (self%iteration>7) then
      else
         q = 0.1*q
      end if
      call create_(g,n)
      call func(par+q,E,g)
      par = par+dot_product(self%grd(min(self%iteration,self%keep))%element,q/norm_(q))/(dot_product(g,q/norm_(q))-dot_product(self%grd(min(self%iteration,self%keep))%element,q/norm_(q)))*q
      call destroy_(g)
      call destroy_(q)
   end subroutine

   subroutine minimize_nols_L_BFGS(self,func,par)
   ! Use the limited memory Broyden-Fletcher-Goldfarb-Shanno method to
   ! minimise the molecular orbitals with gradient function from
   ! Stoll(1980). This code is all per Nocedal & Wright. This function
   ! utilises an in-built stepping protocol rather than a line-search
   ! to reduce memory usage.
   ! NOTE: set saved_self before calling self & dself
      type(L_BFGS_TYPE), INOUT :: self
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      VEC(REAL,:), INOUT :: par
      INT :: j,n,m

      ! alpha, c1 and c2 are used in the line search algorithm and
      ! beta, gamma and rho are used in the Hessian approximation
      REAL :: alpha,beta,gamma,rho,c1,c2

      ! a is used to store the various alpha values in the two loop
      ! recursion, p,q and r are used as generic vectors, and as part
      ! of the two loop recursion process
      VEC(REAL,:), allocatable :: a,q,s,y
      self%iteration = self%iteration+1
      n = size(par,1)

      ! Gradient 
      call create_(q,n)
      call func(par,c1,q)

      ! Store the parameter & gradient
      ! NOTE: you should reset if you change
      ! any of the par vectors outside of here ...
      call append_fifo_(self%par,par,self%keep)
      call append_fifo_(self%grd,q,self%keep)

      ! Size of the kept vectors
      m = size(self%par,1)  ! Note self

      ! Converged?
      self%grd_norm = norm_(q)
      if (is_converged_(self)) then

         ! Cleanup
         call destroy_(q)
         return
      end if

      ! Workspace
      call create_(s,n)
      call create_(y,n)
      call create_(a,m)
      a = ZERO

      ! The values used for line search algorithms. These values are
      ! the ones which usually work.
      c1 = 10d-4
      c2 = 9d-1

      ! Initialises the values used in the two-loop recursion.
      rho = 1
      if (m>1) then
         s = self%par(m)%element-self%par(m-1)%element
         y = self%grd(m)%element-self%grd(m-1)%element
         gamma = dot_product(s,y)/dot_product(y,y)
      else
         gamma = 1
      end if

      ! Will cause the program to exit if a suitable step can not be
      ! determined 
      !alpha = ZERO

      ! The two loop recursion process approximates the Hessian by
      ! using stored values of delta x and delta grad, as well as a
      ! pre-scaling, gamma, to allow for a stepping at alpha = 1, to
      ! be suitable 

      ! H^0 is approximated by I   
      if (m>1) then
         do j = m,2,-1
            s = self%par(j)%element-self%par(j-1)%element
            y = self%grd(j)%element-self%grd(j-1)%element
            rho = 1/(dot_product(y,s))
            a(j) = rho*dot_product(s,q)
            q = q-a(j)*y
         end do
      end if
      q = gamma*q
      do j = 2,m
         s = self%par(j)%element-self%par(j-1)%element
         y = self%grd(j)%element-self%grd(j-1)%element
         rho = 1/dot_product(y,s)
         beta = rho*dot_product(y,q)
         q = q+(a(j)-beta)*s
      end do

      ! This is the step toward the minimum
      q = -q
      if (self%iteration>7) then
      else
         q = 0.1*q
      end if
      alpha = dot_product(self%grd(min(self%iteration,self%keep))%element,self%grd(min(self%iteration,self%keep))%element)/(dot_product(q,q)+dot_product(self%grd(min(self%iteration,self%keep))%element,self%grd(min(self%iteration,self%keep))%element))
      par = par+alpha*q
      call destroy_(q)

      !stdout.show("alpha =",alpha)
      !stdout.text("par:")
      !stdout.put(par)
   end subroutine

   subroutine minimize_L_BFGS_a(self,func,par)
   ! Use the limited memory Broyden-Fletcher-Goldfarb-Shanno method to
   ! minimise the molecular orbitals with gradient function from
   ! Stoll(1980). This code is all per Nocedal & Wright
   ! NOTE: set saved_self before calling self & dself
      type(L_BFGS_TYPE), INOUT :: self
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      VEC(REAL,:), INOUT :: par
      INT :: j,n,m

      ! alpha, c1 and c2 are used in the line search algorithm and
      ! beta, gamma and rho are used in the Hessian approximation
      REAL :: alpha,beta,gamma,rho,c1,c2

      ! a is used to store the various alpha values in the two loop
      ! recursion, p,q and r are used as generic vectors, and as part
      ! of the two loop recursion process
      VEC(REAL,:), allocatable :: a,q,s,y
      self%iteration = self%iteration+1
      n = size(par,1)

      ! Gradient 
      call create_(q,n)
      call func(par,c1,q)

      ! Store the parameter & gradient
      ! NOTE: you should reset if you change
      ! any of the par vectors outside of here ...
      call append_fifo_(self%par_a,par,self%keep)
      call append_fifo_(self%grd_a,q,self%keep)

      ! Size of the kept vectors
      m = size(self%par_a,1)  ! Note self

      ! Converged?
      self%grd_norm_a = norm_(q)
      if (is_converged_(self)) then

         ! Cleanup
         call destroy_(q)
         return
      end if

      ! Workspace
      call create_(s,n)
      call create_(y,n)
      call create_(a,m)
      a = ZERO

      ! The values used for line search algorithms. These values are
      ! the ones which usually work.
      c1 = 10d-4
      c2 = 9d-1

      ! Initialises the values used in the two-loop recursion.
      rho = 1
      if (m>1) then
         s = self%par_a(m)%element-self%par_a(m-1)%element
         y = self%grd_a(m)%element-self%grd_a(m-1)%element
         gamma = dot_product(s,y)/dot_product(y,y)
      else
         gamma = 1
      end if

      ! Will cause the program to exit if a suitable step can not be
      ! determined 
      alpha = ZERO

      ! The two loop recursion process approximates the Hessian by
      ! using stored values of delta x and delta grad, as well as a
      ! pre-scaling, gamma, to allow for a stepping at alpha = 1, to
      ! be suitable 

      ! H^0 is approximated by I   
      if (m>1) then
         do j = m,2,-1
            s = self%par_a(j)%element-self%par_a(j-1)%element
            y = self%grd_a(j)%element-self%grd_a(j-1)%element
            rho = 1/(dot_product(y,s))
            a(j) = rho*dot_product(s,q)
            q = q-a(j)*y
         end do
      end if
      q = gamma*q
      do j = 2,m
         s = self%par_a(j)%element-self%par_a(j-1)%element
         y = self%grd_a(j)%element-self%grd_a(j-1)%element
         rho = 1/dot_product(y,s)
         beta = rho*dot_product(y,q)
         q = q+(a(j)-beta)*s
      end do

      ! This is the step toward the minimum
      q = -q

      ! Fines the ideal multiple of the step to take (usually 1)
      ! and stores that in alpha
      call line_search(func,10d1,par,q,c1,c2,alpha)

      ! Performs the step (and stores the previous x value)
      par = par+alpha*q

      ! Cleanup
      call destroy_(a)
      call destroy_(y)
      call destroy_(s)
      call destroy_(q)
   end subroutine

   subroutine minimize_L_BFGS_b(self,func,par)
   ! Use the limited memory Broyden-Fletcher-Goldfarb-Shanno method to
   ! minimise the molecular orbitals with gradient function from
   ! Stoll(1980). This code is all per Nocedal & Wright
   ! NOTE: set saved_self before calling self & dself
      type(L_BFGS_TYPE), INOUT :: self
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      VEC(REAL,:), INOUT :: par
      INT :: j,n,m

      ! alpha, c1 and c2 are used in the line search algorithm and
      ! beta, gamma and rho are used in the Hessian approximation
      REAL :: alpha,beta,gamma,rho,c1,c2

      ! a is used to store the various alpha values in the two loop
      ! recursion, p,q and r are used as generic vectors, and as part
      ! of the two loop recursion process
      VEC(REAL,:), allocatable :: a,q,s,y
      self%iteration = self%iteration+1
      n = size(par,1)

      ! Gradient 
      call create_(q,n)
      call func(par,c1,q)

      ! Store the parameter & gradient
      ! NOTE: you should reset if you change
      ! any of the par vectors outside of here ...
      call append_fifo_(self%par_b,par,self%keep)
      call append_fifo_(self%grd_b,q,self%keep)

      ! Size of the kept vectors
      m = size(self%par_b,1)  ! Note self

      ! Converged?
      self%grd_norm_b = norm_(q)
      if (is_converged_(self)) then

         ! Cleanup
         call destroy_(q)
         return
      end if

      ! Workspace
      call create_(s,n)
      call create_(y,n)
      call create_(a,m)
      a = ZERO

      ! The values used for line search algorithms. These values are
      ! the ones which usually work.
      c1 = 10d-4
      c2 = 9d-1

      ! Initialises the values used in the two-loop recursion.
      rho = 1
      if (m>1) then
         s = self%par_b(m)%element-self%par_b(m-1)%element
         y = self%grd_b(m)%element-self%grd_b(m-1)%element
         gamma = dot_product(s,y)/dot_product(y,y)
      else
         gamma = 1
      end if

      ! Will cause the program to exit if a suitable step can not be
      ! determined 
      alpha = ZERO

      ! The two loop recursion process approximates the Hessian by
      ! using stored values of delta x and delta grad, as well as a
      ! pre-scaling, gamma, to allow for a stepping at alpha = 1, to
      ! be suitable 

      ! H^0 is approximated by I   
      if (m>1) then
         do j = m,2,-1
            s = self%par_b(j)%element-self%par_b(j-1)%element
            y = self%grd_b(j)%element-self%grd_b(j-1)%element
            rho = 1/(dot_product(y,s))
            a(j) = rho*dot_product(s,q)
            q = q-a(j)*y
         end do
      end if
      q = gamma*q
      do j = 2,m
         s = self%par_b(j)%element-self%par_b(j-1)%element
         y = self%grd_b(j)%element-self%grd_b(j-1)%element
         rho = 1/dot_product(y,s)
         beta = rho*dot_product(y,q)
         q = q+(a(j)-beta)*s
      end do

      ! This is the step toward the minimum
      q = -q

      ! Fines the ideal multiple of the step to take (usually 1)
      ! and stores that in alpha
      call line_search(func,10d1,par,q,c1,c2,alpha)

      ! Performs the step (and stores the previous x value)
      par = par+alpha*q

      ! Cleanup
      call destroy_(a)
      call destroy_(y)
      call destroy_(s)
      call destroy_(q)
   end subroutine


!  ===========
!  Line Search
!  ===========

   subroutine line_search(func,alphamax,x,p,c1,c2,b)
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2.
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      VEC(REAL,:), IN :: x
      VEC(REAL,:), IN :: p

      ! Wolfe constants c1 and c2 and a highest value of alpha
      REAL, IN :: c1,c2,alphamax

      ! The output alpha value
      REAL, OUT :: b

      ! alpha is the current value of alpha. alpha0 is the previous
      ! value. phi0 is the initial phi value, phi1 is the previous phi
      ! value and phi is thge current phi value. The same is true for
      ! dphi0, dphi1 and dphi.
      REAL :: alpha,alpha0,phi0,phi1,phi,dphi,dphi0,dphi1

      ! temp is used in vector dot product calculations. Such as in
      ! phi' (dphi)
      VEC(REAL,:), allocatable :: temp

      ! The maximum (iter) and current (j) number of iterations
      INT :: iter,j

      ! Logical for if the loop can conclude
      BIN :: converged

      ! The maximum number of iterations that may be performed
      iter = 5

      ! Allows temp to be used for calculations
      call create_(temp,size(x))

      ! Gives the first value of alpha
      call RANDOM_NUMBER(alpha0)
      alpha = alpha0*alphamax

      ! Initialise alpha0, phi0, dphi0, phi1, j and converged
      alpha0 = 0
      call func(x,phi0,temp)
      dphi0 = dot_product(temp,p)
      phi1 = 0
      j = 1
      converged = FALSE

      ! Performs the loop until it has converged or the loop has
      ! failed
      do while ((converged EQV FALSE) AND (j<iter))

         ! calculates phi for this iteration
         call func(x+alpha*p,phi,temp)

         ! The interval alpha0, alpha will contain ideal step lengths if
         ! any of the if conditionals below are violated
         if (phi>phi0+c1*alpha*dphi0) then

            ! Sends interval off to find an ideal step and stores it
            ! in b
            call zoom(func,alpha0,alpha,x,p,c1,c2,b,converged)
         else if (phi>=phi1 AND j>1) then

            ! Sends interval off to find an ideal step and stores it
            ! in b
            call zoom(func,alpha0,alpha,x,p,c1,c2,b,converged)
         end if

         ! Ensures that if alpha has converged, the value does not
         ! change   
         if (converged EQV FALSE) then

            ! calculates dphi for this iteration      
            dphi = dot_product(temp,p)

            ! If the second wolfe condition holds, this has converged      
            if (abs(dphi)<=-c2*dphi0) then

               ! Stores the ideal stepping in b         
               converged = TRUE
               b = alpha
            else if (dphi>=0) then

               ! Sends the interval off to find an ideal step and stores it in
               ! b
               call zoom(func,alpha,alpha0,x,p,c1,c2,b,converged)
            end if

            ! Ensures that if alpha has converged, the value does not
            ! change   
            if (converged EQV FALSE) then

               ! Stores the values from this iteration, and proceeds to the
               ! next iteration         
               phi1 = phi
               dphi1 = dphi
               j = j+1
               alpha0 = alpha
               call RANDOM_NUMBER(phi)
               alpha = alpha+phi*alphamax
            end if
         end if
      end do

      ! Cleanup
      call destroy_(temp)
   end subroutine

   subroutine zoom(func,low,high,x,p,c1,c2,alpha,converged)
   ! Given a real vector, x, function f, gradient function
   ! df, calculaes the ideal stepping scale alpha, given
   ! stepping p and constants c1, c2. An ideal stepping must also
   ! reside between low and high.
      interface
         subroutine func(x,res,g)
            VEC(REAL,:), IN :: x
            REAL, OUT :: res
            VEC(REAL,:), OUT :: g
         end subroutine
      end interface
      ! Interval in which ideal step resides, low and high and Wolfe
      ! constants c1 and c2
      REAL, IN :: low,high,c1,c2
      VEC(REAL,:), IN :: x,p

      ! Ideal stepping scale
      REAL, OUT :: alpha

      ! Logical which describes whether or not it has converged to an
      ! ideal step
      BIN, OUT :: converged

      ! Vector used in various multiplications
      VEC(REAL,:), allocatable :: temp

      ! Flexible interval alphalo and alphahi. Initial value of phi
      ! and dphi, phi0 and dphi0. Current values at phi and dphi. phi
      ! value at alpha lo is at philo.
      REAL :: alphalo,alphahi,phi0,dphi,dphi0,phi,philo

      ! The current (j) and maximum (iter) number of iterations
      INT :: j,iter
      iter = 10

      ! creates temp
      call create_(temp,size(x))

      ! Initialise alphalo, alphahi, phi0, dphi0, converged and j
      alphalo = low
      alphahi = high
      call func(x,phi0,temp)
      dphi0 = dot_product(temp,p)
      converged = FALSE
      j = 0

      ! Does the loops until interations maximum is reached or the
      ! ideal stepping is found.
      do while ((converged EQV FALSE) AND (j<iter))

      ! New value of alpha is determined by bisection
         alpha = alphalo+alphahi/2

      ! Calculates current value of philo and phi   
         call func(x+alphalo*p,philo,temp)
         call func(x+alpha*p,phi,temp)

      ! If either of these condition are satisfied alphahi becomes
      ! alpha amd the cycle repeats   
         if (phi>phi0+c1*alpha*dphi0) then
            alphahi = alpha
         else if (phi>=philo) then
            alphahi = alpha
         else

      ! Calculates the current value of dphi
            dphi = dot_product(temp,p)

      ! Cycle is complete if strong Wolfe condition holds      
            if (abs(dphi)<=-c2*dphi0) then
               converged = TRUE
            end if

      ! Stops pointless calculations if ideal stepping has been found
            if (converged EQV FALSE) then

      ! Ensures alphahi and alphalo are the right way around         
               if (dphi*(alphahi-alphalo)>=ZERO) then
                  alphahi = alphalo
               end if
      ! alphalo becomes alpha         
               alphalo = alpha
            end if
         end if
         j = j+1
      end do
      call destroy_(temp)
      converged = TRUE
   end subroutine


!  =======
!  Queries
!  =======

   pure function subspace_saturated(self) result (res)
   ! Return TRUE if the subspace is saturated; no more new
   ! vectors can be stored.
      type(L_BFGS_TYPE), IN :: self
      BIN :: res
      res = size(self%par)==self%keep
   end function

   pure function apply_l_bfgs(self) result (res)
   ! Return TRUE if extrapolation must be applied this iteration
      type(L_BFGS_TYPE), IN :: self
      BIN :: res
      res = self%iteration-self%start_iteration>=0
   end function

   pure function saved_iteration(self) result (res)
   ! Return the actual iteration number since starting to save vectors
      type(L_BFGS_TYPE), IN :: self
      INT :: res
      res = self%iteration-self%save_iteration
   end function


!  =============
!  Extrapolation
!  =============

   PURE function is_converged(self) result (res)
   ! Return TRUE if the procedure is converged
      type(L_BFGS_TYPE), IN :: self
      BIN :: res
      res = self%grd_norm<self%convergence_tolerance
   end function


!  ======
!  Output
!  ======

   subroutine put(self)
   ! Print out info
      type(L_BFGS_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"L_BFGS options: ")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call show_(stdout,"Convergence on max(abs(error)) =",self%convergence_tolerance)
      call set_real_fortran_fmt_(stdout,"f")
      call show_(stdout,"Extrapolate  from iteration    =",self%start_iteration)
      call show_(stdout,"Save vectors from iteration    =",self%save_iteration)
      call show_(stdout,"No. of vectors to keep         =",self%keep)
   end subroutine

end module
