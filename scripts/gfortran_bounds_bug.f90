! Reproducer for a gfortran 16 code-generation bug in -fcheck=bounds.
!
! For a bounds-checked subscript reached through an allocatable component
! chain, gfortran 16 copies the array descriptor of SELF%IRREP into a stack
! temporary and then emits the check reading a DIFFERENT temporary, which is
! only written later in the same statement. The check therefore consults
! uninitialised stack memory: it either faults on a nonsense address or
! reports a false "outside of expected range".
!
!   gfortran-14 -O0                    -> OK
!   gfortran-14 -O0 -fcheck=bounds     -> OK
!   gfortran-16 -O0                    -> OK
!   gfortran-16 -O0 -fcheck=bounds     -> SIGSEGV on x86_64
!
! The fault depends on what happens to be on the stack, so a clean run is not
! proof the compiler is sound -- check the generated code as well. On x86_64
! the descriptor is written to -0x100(%rbp) and read from -0xc0(%rbp).
!
! Run scripts/check_gfortran_bounds_bug.py to test a compiler.
! Background: docs/GFORTRAN16_DEBUG_CRASH.md.
!
! Reduced from POINTGROUP:make_character_table.

module types_m
   implicit none
   type IRREP_TYPE
      character(len=4) :: label = "?"
      integer :: dim = 0
      real(8), dimension(:),     allocatable :: chi
      real(8), dimension(:,:,:), allocatable :: mx
   end type
   type POINTGROUP_TYPE
      character(len=4) :: symbol = " "
      integer :: order = 0, n_irrep = 0
      type(IRREP_TYPE), dimension(:), allocatable :: irrep
   end type
end module

module work_m
   implicit none
contains
   pure function trace_(a) result(res)
      real(8), dimension(:,:), intent(in) :: a
      real(8) :: res
      integer :: i
      res = 0.0d0
      do i = 1, size(a,1); res = res + a(i,i); end do
   end function
   pure subroutine create_(self,dim)
      real(8), dimension(:), allocatable, intent(out) :: self
      integer, intent(in) :: dim
      allocate(self(dim))
   end subroutine
end module

module pg_m
   use types_m
   use work_m, only: trace_, create_
   implicit none
contains
   subroutine make_character_table(self)
      type(POINTGROUP_TYPE), intent(inout) :: self
      integer :: n, i
      do i = 1, self%n_irrep
         call create_(self%irrep(i)%chi,self%order)
         do n = 1, self%order
            self%irrep(i)%chi(n) = trace_(self%irrep(i)%mx(:,:,n))   ! <== miscompiled
         end do
      end do
   end subroutine
end module

program gfortran_bounds_bug
   use types_m
   use pg_m
   implicit none
   type(POINTGROUP_TYPE) :: pg
   integer :: i, d
   integer, parameter :: dims(10) = [1,1,2,3,3,1,1,2,3,3]   ! the Oh irreps

   pg%symbol = "oh  "; pg%order = 48; pg%n_irrep = 10
   allocate(pg%irrep(pg%n_irrep))
   do i = 1, pg%n_irrep
      d = dims(i)
      pg%irrep(i)%dim = d
      allocate(pg%irrep(i)%mx(d,d,pg%order))
      pg%irrep(i)%mx = 1.0d0
   end do

   call make_character_table(pg)

   if (abs(pg%irrep(1)%chi(1) - 1.0d0) > 1.0d-12) then
      print *, "WRONG: chi(1,1) =", pg%irrep(1)%chi(1)
      stop 1
   end if
   print *, "OK: chi(1,1) =", pg%irrep(1)%chi(1)
end program
