!==================================================================
!
! DIIS: Simple in-core DIIS extrapolation technique for vectors
!
! Copyright (C) Dylan Jayatilaka, 2011
! Copyright (C) Dylan Jayatilaka, 2021
!
! This is a memory-hungry in-core DIIS routine. Only the simple 
! DIIS procedure is used. Since all parameter and error vectors 
! are stored in-core, you should avoid storing more than 10 
! vectors (which corresponds to 20 Fock matrix triangles).
!
! There are two modes of operation:
!
! . If a corresponding error vector is supplied, the first
!   nontrivial DIIS extrapolation occurs at actual iteration 2. 
!
! . If no error vector is supplied, the error at iteration (i) is
!   defined as error(i)[:] = param(i+1)[:] - param(i)[:] and the
!   dimension of the DIIS problem is one less than the number of
!   parameter vectors stored, so extrapolation occurs at actual
!   iteration 3.
!
!   Enjoy.
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
module DIIS_MODULE

#  include "macros"

   include "diis.use"

   implicit none

   include "diis.int"


contains

! ==========
! Allocation
! ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(DIIS_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"DIIS:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(DIIS_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"DIIS:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(DIIS_TYPE), INOUT :: self
      call destroy_(self%oldest)
      call destroy_(self%iter_4)
      call destroy_(self%itor_4)
      call destroy_(self%param)
      call destroy_(self%error)
      call destroy_(self%lhs)
      call destroy_(self%rhs)
      call destroy_(self%coeff)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set up the default settings
      type(DIIS_TYPE), INOUT :: self
      self%iteration = 0
      self%use_errors = TRUE
      self%start_iteration = DIIS_START_ITERATION
      self%save_iteration = DIIS_SAVE_ITERATION
      self%keep = DIIS_KEEP
      self%kept = 0
      self%new = 0
      self%error_length = 1000d0
      self%convergence_tolerance = DIIS_CONVERGENCE_TOLERANCE
      self%start_tolerance = DIIS_START_TOLERANCE
      self%replacement_method = DIIS_REPLACEMENT_METHOD

      ! Clean up (leaky)
      call destroy_ptr_part_(self)
   end subroutine

   PURE subroutine set_to(self,val)
   ! Set "self" to "val"
      type(DIIS_TYPE), INOUT :: self
      type(DIIS_TYPE), IN :: val
      self = val
   end subroutine

   PURE subroutine reset_iteration_defaults(self)
   ! Reset defaults without changing user settings
      type(DIIS_TYPE), INOUT :: self
      self%iteration = -1  ! The first time called it is iteration 0
      self%use_errors = TRUE
      self%kept = 0
      self%new = 0

      ! Clean up old stuff (leaky)
      call destroy_ptr_part_(self)
   end subroutine

   PURE subroutine set_convergence_tolerance(self,tolerance)
   ! Read the convergence tolerance
      type(DIIS_TYPE), INOUT :: self
      REAL, IN :: tolerance
   ENSURE(tolerance>ZERO,"DIIS:set_convergence_tolerance ... must have positive convergence tolerance")
! WARN_IF(tolerance<TOL(6),"convergence tolerance may be too small")
      self%convergence_tolerance = tolerance
   end subroutine

   PURE subroutine set_start_tolerance(self,tolerance)
   ! Read the start tolerance when extrapolation is supposed to start
      type(DIIS_TYPE), INOUT :: self
      REAL, IN :: tolerance
   ENSURE(tolerance>ZERO,"DIIS:set_start_tolerance ... must have positive start tolerance")
   ENSURE(tolerance>self%convergence_tolerance,"DIIS:set_start_tolerance ... start tolerance is <= convergence tolerance")
! WARN_IF(tolerance<TOL(6),"start tolerance may be too small")
      self%start_tolerance = tolerance
   end subroutine

   PURE subroutine set_keep(self,keep)
   ! Set the number of parameter/error vectors to keep 
      type(DIIS_TYPE), INOUT :: self
      INT, IN :: keep
   ENSURE(keep>=4,"DIIS:set_keep ... no. to keep must be more than 4")
      self%keep = keep
   end subroutine

   pure subroutine set_start_iteration(self,start)
   ! Set at what iteration the procedure should start
      type(DIIS_TYPE), INOUT :: self
      INT, IN :: start
      self%start_iteration = start
   end subroutine

   pure subroutine set_save_iteration(self,save_it)
   ! Set at what iteration the procedure should start saving vectors
      type(DIIS_TYPE), INOUT :: self
      INT, IN :: save_it
      self%save_iteration = save_it
   end subroutine

   PURE subroutine set_replacement_method(self,method)
   ! Set the method to use for replaceing vectors, when the subspace
   ! become saturated.
      type(DIIS_TYPE), INOUT :: self
      STR(len=*), IN :: method
   ENSURE(is_one_of_(method,["oldest","worst "]),"DIIS:set_replacement_method ... unknown method")
      self%replacement_method = method
      call to_lower_case_(self%replacement_method)
   end subroutine

   PURE subroutine set_error_length(self,error)
   ! Set the error length
      type(DIIS_TYPE), INOUT :: self
      REAL, IN :: error
   ENSURE(error>=ZERO,"DIIS:set_error_length ... error must be +ve")
      self%error_length = error
   end subroutine

   pure subroutine set_use_errors(self,val)
   ! Set whether tpo use error vectors
      type(DIIS_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_errors = val
   end subroutine


!  =================
!  Read the keywords
!  =================

   subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(DIIS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","DIIS:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"DIIS:read_keywords ... unexpected end of file while reading list keywords")
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
      type(DIIS_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                       ")  ! exit case
      case ("convergence_tolerance=  "); call read_convergence_tolerance_(self)
      case ("keep=                   "); call read_keep_(self)
      case ("replacement_method=     "); call read_replacement_method_(self)
      case ("save_iteration=         "); call read_save_iteration_(self)
      case ("start_iteration=        "); call read_start_iteration_(self)
      case ("start_tolerance=        "); call read_start_tolerance_(self)
      case default; allocate(tonto%known_keywords(7))
      tonto%known_keywords(1) = "}                       "
      tonto%known_keywords(2) = "convergence_tolerance=  "
      tonto%known_keywords(3) = "keep=                   "
      tonto%known_keywords(4) = "replacement_method=     "
      tonto%known_keywords(5) = "save_iteration=         "
      tonto%known_keywords(6) = "start_iteration=        "
      tonto%known_keywords(7) = "start_tolerance=        "
      call unknown_(tonto,word,"DIIS:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_convergence_tolerance(self)
   ! Generic read and set a quantity with units
      type(DIIS_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_convergence_tolerance_(self,val)
   end subroutine

   subroutine read_start_tolerance(self)
   ! Generic read and set a quantity with units
      type(DIIS_TYPE), INOUT :: self
      REAL :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_start_tolerance_(self,val)
   end subroutine

   subroutine read_keep(self)
   ! Generic read and set a quantity with units
      type(DIIS_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_keep_(self,val)
   end subroutine

   subroutine read_replacement_method(self)
   ! Generic read and set
      type(DIIS_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: val

      ! The following code is inherited from OBJECT
      call read_(stdin,val)
      call set_replacement_method_(self,val)
   end subroutine

   subroutine read_start_iteration(self)
   ! Generic read and set a quantity with units
      type(DIIS_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_start_iteration_(self,val)
   end subroutine

   subroutine read_save_iteration(self)
   ! Generic read and set a quantity with units
      type(DIIS_TYPE), INOUT :: self
      INT :: val

      ! The following code is inherited from OBJECT
      call read_quantity_(stdin,val)
      call set_save_iteration_(self,val)
   end subroutine


!  =======
!  Queries
!  =======

   pure function subspace_saturated(self) result (res)
   ! Return TRUE if the subspace is saturated; no more new
   ! vectors can be stored.
      type(DIIS_TYPE), IN :: self
      BIN :: res
      res = self%kept==self%keep
   end function

   pure function apply_DIIS(self) result (res)
   ! Return TRUE if extrapolation must be applied this iteration
   ! (or has already been applied in this iteration). It works by
   ! telling whether parameter vectors were saved.
   ! NOTE: To know if the DIIS equations are to be solved for a
   ! meaningful extrapolation, ".dimension" must be greater than 1.
      type(DIIS_TYPE), IN :: self
      BIN :: res
      res = self%iteration-self%save_iteration>=0
   end function

   pure function saved_iteration(self) result (res)
   ! Return the actual iteration number since starting to save vectors
   ! and construct left hand side matrices -- not just the total
   ! number of iterations in the procedure. This is usually equal to
   ! .kept but not always -- especially in constrained SCF
   ! calculations where we reuse past vectors.
      type(DIIS_TYPE), IN :: self
      INT :: res
      res = self%iteration-self%save_iteration
   end function

   pure function dimension(self) result (res)
   ! Return the dimension of the DIIS matrix. 
   ! . If .use_errors is TRUE  it never exceeds .kept+1
   ! . If .use_errors is FALSE it never exceeds .kept because the
   !   last param vector has no associated error vector.
      type(DIIS_TYPE), IN :: self
      INT :: res
      res = min(self%kept,self%keep)
      if (self%use_errors) res = res+1
   end function

   pure function is_stuck_or_oscillating(self) result (res)
   ! Return TRUE if extrapolation is stuck or oscillating
      type(DIIS_TYPE), IN :: self
      BIN :: res
      res = is_stuck_(self) OR is_oscillating_(self)
   end function

   pure function is_stuck(self) result (res)
   ! Return TRUE if extrapolation stuck
      type(DIIS_TYPE), IN :: self
      BIN :: res

      ! Stuck?
      res = self%kept==self%keep AND self%oldest(self%keep)==self%oldest(self%keep-1) AND self%oldest(self%keep-1)==self%oldest(self%keep-2)
   end function

   pure function is_oscillating(self) result (res)
   ! Return TRUE if extrapolation is oscillating
      type(DIIS_TYPE), IN :: self
      BIN :: res

      ! Saturated and Oscillating?
      res = self%kept==self%keep AND self%oldest(self%keep)==self%oldest(self%keep-2) AND self%oldest(self%keep-1)==self%oldest(self%keep-3)
   end function


!  =====================
!  Iteration information
!  =====================

   pure function actual_iteration(self) result (res)
   ! Return the actual iteration number since extrapolation is really
   ! done.  Even though the first actual iteration may do nothing, it
   ! is still counted as an actual iteration.
      type(DIIS_TYPE), IN :: self
      INT :: res
      res = self%iteration-self%start_iteration+1
   end function

   function next_replacement(self) result (new)
   ! This routine:
   ! . Returns the index of the next item in the expansion which
   !   is to be replaced. 
   ! . Keeps a record of this item in .oldest so we know when the
   !   procedure is stuck or oscillating.  
   ! . If the procedure is stuck, it replaces parameter/error vectors
   !   with the best pair, and restarts.
   ! * NOTE: .kept is *not* to be updated until *after* this routine
   !   is called (see .update_kept)
   ! * NOTE: do not call this more than once per cycle.
      type(DIIS_TYPE), INOUT :: self
      INT :: new
      INT :: best
      new = 0

      ! Find the new replacement index
      if (self%kept<self%keep) then

         ! Just return the next index
         new = self%kept+1

         ! Append youngest as last
         self%oldest(new) = new
      else

         ! Return the item to be replaced:
         ! either the worst or oldest one.
         select case (self%replacement_method)
         case ("oldest"); new = oldest_item_(self)
         case ("worst"); new = worst_item_(self)
         end select

         ! Move history up, store youngest at end
         self%oldest(1:self%keep-1) = self%oldest(2:self%keep)
         self%oldest(self%keep) = new
      end if

      ! Save actual iteration no's
      self%itor_4 = self%iter_4
      self%iter_4(new) = self%iteration

      ! Is it stuck?
      if (is_stuck_or_oscillating_(self)) then

         ! Index of best item 
         best = best_item_(self)

         ! If best is oldest, keep nothing
         if (best==self%oldest(1)) then
            self%kept = 0
            best = 0
            new = 1  ! New one to be stored
            self%oldest(1) = 1
            self%iter_4(1) = self%iteration

         ! Otherwise keep best parameters
         else
            self%kept = 1  ! Best one stored below
            new = 2  ! New one to be stored
            self%oldest(1) = 1
            self%iter_4(1) = self%iter_4(best)
            self%oldest(2) = 2
            self%iter_4(2) = self%iteration
            self%param(1)%element(:) = self%param(best)%element(:)
            self%error(1)%element(:) = self%error(best)%element(:)
         end if
      end if
   end function

   PURE function oldest_item(self) result (res)
   ! Return the index of the current item (if it is less than or equal
   ! to the number of vectors to be kept), otherwise return the index
   ! of the oldest item. 
      type(DIIS_TYPE), IN :: self
      INT :: res
   ENSURE(self%kept>0,"DIIS:oldest_item ... must have some kept vectors")
   ENSURE(allocated(self%oldest),"DIIS:oldest_item ... oldest list not created")
      res = self%oldest(1)
      ENSURE(res>0,"DIIS:oldest_item ... oldest item must be positive")
      ENSURE(res<=self%kept,"DIIS:oldest_item ... oldest item must be <= no. kept")
   end function

   PURE function worst_item(self) result (res)
   ! Return the index of the worst item which should be replaced
   ! (when there is no more space i.e. when ".keep" limit reached)
      type(DIIS_TYPE), IN :: self
      INT :: res
      VEC(REAL,:), allocatable :: diag
      INT :: d
   ENSURE(self%kept>0,"DIIS:worst_item ... no stored vectors")
   ENSURE(allocated(self%lhs),"DIIS:worst_item ... no LHS")

      ! Get the diagonal
      d = size(self%lhs,1)
      call create_(diag,d)

      ! The error vector lengths^2
      call put_diagonal_to_(self%lhs,diag)

      ! Worst item has the largest error
      res = index_of_largest_value_(diag(1:d-1))

      ! stdout.text("errors:")
      ! stdout.put(diag(1:d-1),by_column=TRUE)

      ! Clean
      call destroy_(diag)
   end function

   PURE function best_item(self) result (res)
   ! Return the index of the best item which should be kept,
   ! when the procedure is reset.
      type(DIIS_TYPE), IN :: self
      INT :: res
      VEC(REAL,:), allocatable :: diag
      INT :: d
   ENSURE(self%kept>0,"DIIS:best_item ... no stored vectors")
   ENSURE(allocated(self%lhs),"DIIS:best_item ... no LHS")

      ! Get the diagonal
      d = size(self%lhs,1)
      call create_(diag,d)

      ! The error vector lengths^2
      call put_diagonal_to_(self%lhs,diag)

      ! Best item has the smallest error
      res = index_of_smallest_value_(diag(1:d-1))

      ! Clean
      call destroy_(diag)
   end function

   pure function replaced_iteration(self) result (res)
   ! Return the actual parent iteration number which was replaced
   ! by the DIIS procedure.
      type(DIIS_TYPE), IN :: self
      INT :: res
      res = self%itor_4(self%new)
   end function


!  =============
!  Extrapolation
!  =============

   subroutine extrapolate_0(self,par,dim)
   ! Generic extrapolation of the parameter vector "par".
   ! No error vector is required, but in this case the number
   ! of actual stored vectors used is .keep-1.
      type(DIIS_TYPE), INOUT :: self
      INT, IN :: dim
      VEC(REAL,dim), INOUT :: par
      INT :: ind

      ! Increment iteration
      self%iteration = self%iteration+1

      ! Return if nothing to do
      if (self%iteration<self%save_iteration) return

      ! Initialize (*very* leaky)
      if (self%iteration==self%save_iteration) then
         call destroy_ptr_part_(self)
         call create_(self%oldest,self%keep); self%oldest = 0
         call create_(self%iter_4,self%keep); self%iter_4 = 0
         call create_(self%itor_4,self%keep); self%itor_4 = 0
         call create_(self%param,self%keep,dim)
         call create_(self%error,self%keep,dim)
      end if

      ! Next index in param list to be replaced
      self%new = next_replacement_(self)

      ! Always save .new parameter vector 
      ! and update the # of kept vectors
      self%param(self%new)%element(:) = par
      self%kept = min(self%kept+1,self%keep)

      ! Only one parameter vector? No error to define 
      if (self%kept==1) return

      ! Define error for *previous* param vector
      ind = self%oldest(self%kept-1)
      self%error(ind)%element(:) = par-self%param(ind)%element(:)

      ! Set the error length
      self%error_length = norm_(self%error(ind)%element)

      ! Make sure .dimension is one smaller
      self%use_errors = FALSE  ! <<<

      ! Extrapolate as usual
      call do_extrapolate_(self,par,dim)
   end subroutine

   subroutine extrapolate_1(self,par,err,dim)
   ! Generic extrapolation of the parameter vector "par", using "err"
   ! as the error vector. User must define "err" properly and
   ! consistent with "par".
      type(DIIS_TYPE), INOUT :: self
      INT, IN :: dim
      VEC(REAL,dim), INOUT :: par
      VEC(REAL,dim), IN :: err
   ENSURE(self%use_errors,"DIIS:extrapolate_1 ... always use extrapolate with error vectors")

      ! Increment iteration
      self%iteration = self%iteration+1

      ! Return if nothing to do
      if (self%iteration<self%save_iteration) return

      ! Initialize (*very* leaky)
      if (self%iteration==self%save_iteration) then
         call destroy_ptr_part_(self)
         call create_(self%oldest,self%keep); self%oldest = 0
         call create_(self%iter_4,self%keep); self%iter_4 = 0
         call create_(self%itor_4,self%keep); self%itor_4 = 0
         call create_(self%param,self%keep,dim)
         call create_(self%error,self%keep,dim)
      end if

      ! Next index in param list to be replaced
      self%new = next_replacement_(self)

      ! Save .new parameter/error vector 
      ! and update the # of kept vectors
      self%param(self%new)%element(:) = par
      self%error(self%new)%element(:) = err
      self%kept = min(self%kept+1,self%keep)

      ! Set the error length
      self%error_length = norm_(err)

      ! Now set up and solve the DIIS equations
      call do_extrapolate_(self,par,dim)
   end subroutine

   subroutine do_extrapolate(self,par,dim)
   ! Do a generic extrapolation on vector "par".
      type(DIIS_TYPE), INOUT :: self
      INT, IN :: dim
      VEC(REAL,dim), INOUT :: par
      INT :: d,i,j
      REAL :: fac

      ! Size of the equations
      d = dimension_(self)

      ! Nontrivial extrapolation possible?
      if (d<2) return

      ! Must be bigger than start iteration
      if (actual_iteration_(self)<1) return

      ! Create the DIIS matrices (leaky)
      call destroy_(self%lhs); call create_(self%lhs,d,d)
      call destroy_(self%rhs); call create_(self%rhs,d)
      call destroy_(self%coeff); call create_(self%coeff,d)

      ! Make the lower triangle of the the LHS 
      ! Loop from oldest to youngest vector.
      do i = 1,d-1
         do j = 1,i

         ! Hamilton/Pulay factor for youngest
            fac = ONE
            if (i==self%new) fac = fac+0.02d0

         ! Define lower half of LHS
            self%lhs(i,j) = fac*dot_product(self%error(i)%element,self%error(j)%element)
         end do
      end do

      ! Set auxiliary row on the bottom
      self%lhs(d,1:d-1) = ONE
      self%lhs(d,d) = ZERO

      ! Symmetrize
      call symmetric_reflect_(self%lhs)

      ! The DIIS RHS 
      self%rhs = ZERO
      self%rhs(d) = ONE

      ! Solve DIIS equations
      call solve_linear_equation_(self%lhs,self%rhs,self%coeff)

      ! Now do interpolation
      par = ZERO
      do i = 1,d-1
         par = par+self%coeff(i)*self%param(i)%element(:)
      end do
   end subroutine

   pure function is_converged(self) result (res)
   ! Return TRUE if the procedure is converged
      type(DIIS_TYPE), IN :: self
      BIN :: res
      res = self%error_length<self%convergence_tolerance
   end function


!  ======
!  Output
!  ======

   subroutine put(self,matrices)
   ! Print out the data to stdout
      type(DIIS_TYPE), IN :: self
      BIN, IN, optional :: matrices
      call flush_(stdout)
      call text_(stdout,"DIIS options: ")
      call flush_(stdout)
      call set_real_fortran_fmt_(stdout,"e")
      call show_(stdout,"Convergence on max(abs(error)) =",self%convergence_tolerance)
      call set_real_fortran_fmt_(stdout,"f")
      call show_(stdout,"Extrapolate  from iteration    =",self%start_iteration)
      call show_(stdout,"Save vectors from iteration    =",self%save_iteration)
      call show_(stdout,"No. of vectors to keep         =",self%keep)
      call show_(stdout,"Replace the vector which is    =",self%replacement_method)
      if (NOT present(matrices)) return
      if (NOT matrices) return
      if (allocated(self%lhs)) then
         call text_(stdout,"LHS:")
         call put_(stdout,self%lhs,auto_width=TRUE)
      end if
      if (allocated(self%rhs)) then
         call text_(stdout,"RHS:")
         call put_(stdout,self%rhs,by_column=TRUE,auto_width=TRUE)
      end if
      if (allocated(self%coeff)) then
         call text_(stdout,"coeff:")
         call put_(stdout,self%coeff,by_column=TRUE,auto_width=TRUE)
      end if
   end subroutine

end module
