!===================================================================
!
! INTERPOLATOR:
!
! For interpolating values from a 1-D table.
!
! The module takes as input some "data_point"'s with their assigned
! "values".  Any data value is then assigned a value by interpolating
! between the two nearest data points.
! 
! The module allows for even-spaced or uneven-spaced data. In the
! former case all the "data_point"'s are not required: only the first
! data point and the "spacing".
!
! Copyright (C) Dylan Jayatilaka, 2002
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
!===================================================================
!
module INTERPOLATOR_MODULE

#  include "macros"

   include "interpolator.use"

   implicit none

   include "interpolator.int"


contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create_0(self)
   ! Allocate an object and set defaults
      type(INTERPOLATOR_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"INTERPOLATOR:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
      call set_defaults_(self)
   end subroutine

   PURE subroutine create_1(self,spacing,first)
   ! Create the object
      type(INTERPOLATOR_TYPE), allocatable, OUT :: self
      REAL, IN :: spacing
      REAL, optional, IN :: first
      call create_(self)
      call set_spacing_(self,spacing)
      if (present(first)) call set_table_origin_(self,first)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(INTERPOLATOR_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"INTERPOLATOR:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(INTERPOLATOR_TYPE), INOUT :: self
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      call destroy_(self%y2)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self,spacing,first)
   ! Set up a defaults
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, optional, IN :: spacing,first
      call set_interpolation_method_(self,INTERPOLATOR_INTERPOLATION_METHOD)
      call set_range_mapping_(self,INTERPOLATOR_RANGE_MAPPING)
      self%table_length = INTERPOLATOR_TABLE_LENGTH
      self%table_origin = INTERPOLATOR_TABLE_ORIGIN
      self%table_spacing = INTERPOLATOR_TABLE_SPACING
      self%table_eps = INTERPOLATOR_TABLE_EPS
      self%domain_scale_factor = ZERO
      call set_domain_mapping_(self,INTERPOLATOR_DOMAIN_MAPPING)
      self%n_data = 0
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      self%finalized = FALSE
      self%y1_initial = INTERPOLATOR_Y1_INITIAL
      self%y1_final = INTERPOLATOR_Y1_FINAL
      if (present(spacing)) call set_spacing_(self,spacing)
      if (present(first)) call set_table_origin_(self,first)
   end subroutine

   PURE subroutine finalize(self)
   ! Check everything is OK after reading data. 
      type(INTERPOLATOR_TYPE), INOUT :: self
   ENSURE(allocated(self%data_point),"INTERPOLATOR:finalize ...  no data_point's")
   ENSURE(allocated(self%data_value),"INTERPOLATOR:finalize ...  no data_value's")
   ENSURE(size(self%data_point)==size(self%data_value),"INTERPOLATOR:finalize ... inconsistent data/value lengths")
   ENSURE(size(self%data_point)>2,"INTERPOLATOR:finalize ... not enough data_point's")

      ! Generate spline data
      if (self%interpolation_is_spline) call set_spline_2nd_derivatives_(self)
      self%finalized = TRUE
   end subroutine

   PURE subroutine set_interpolation_method(self,method)
   ! Set the interpolator "method" to use. 
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=*), IN :: method
   ENSURE(is_one_of_(method,["linear      ","cubic_spline"]),"INTERPOLATOR:set_interpolation_method ... unknown interpolation_method")
      self%interpolation_method = method
      call to_lower_case_(self%interpolation_method)
      select case (self%interpolation_method)
      case ("linear"); self%interpolation_is_spline = FALSE
      case ("cubic_spline"); self%interpolation_is_spline = TRUE
      end select
   end subroutine

   PURE subroutine set_range_mapping(self,method)
   ! Set the range mapping "method" i.e how the function values are
   ! treated before being stored as table data_values 
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=*), IN :: method
   ENSURE(is_one_of_(method,["none       ","logarithmic"]),"INTERPOLATOR:set_range_mapping ... unknown range_mapping")
      self%range_mapping = method
      call to_lower_case_(self%range_mapping)
      select case (self%range_mapping)
      case ("none       "); self%range_is_logarithmic = FALSE
      case ("logarithmic"); self%range_is_logarithmic = TRUE
      end select
   end subroutine

   PURE subroutine set_domain_mapping(self,method)
   ! Set the domain mapping "method" i.e how the data_points are
   ! treated before evaluation by the table function.
   ! NOTE: when obtaining the tabulated values it is the users
   ! responsibility to use the inverse domain mapping function to
   ! obtain the actual table value x. For the sqrt-type domain
   ! remappings the .values_for_squared method can be used; then
   ! no remapping is necessary.
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=*), IN :: method
   ENSURE(is_one_of_(method,["none               ","sqrt               ","sqrt(x/(1-x))*scale"]),"INTERPOLATOR:set_domain_mapping ... unknown domain_mapping")
      self%domain_mapping = method
      call to_lower_case_(self%domain_mapping)
      select case (self%domain_mapping)

      case ("none               ")
         self%domain_is_sqrt_x1x = FALSE
         self%domain_is_sqrt = FALSE

      case ("sqrt               ")
         self%domain_is_sqrt_x1x = FALSE
         self%domain_is_sqrt = TRUE
         if (equals_(self%table_length,INTERPOLATOR_TABLE_LENGTH)) self%table_length = self%table_length*self%table_length

      case ("sqrt(x/(1-x))*scale")
         self%domain_is_sqrt_x1x = TRUE
         self%domain_is_sqrt = FALSE
         call set_interpolation_method_(self,"cubic_spline")
         call set_table_length_(self,ONE)
         call set_spacing_(self,0.0001d0)
         if (self%domain_scale_factor<=ZERO) self%domain_scale_factor = ONE
      end select
   end subroutine

   pure subroutine set_domain_scale_factor(self,fac)
   ! Set the domain mapping scale factor
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: fac
      self%domain_scale_factor = fac
   end subroutine

   PURE subroutine set_spacing(self,spacing)
   ! Set the "spacing" of an even-spaced interpolator
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: spacing
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_spacing ... spacing must be +ve")
      self%table_spacing = spacing
   end subroutine

   PURE subroutine set_table_spacing(self,spacing)
   ! Set the "spacing" of an even-spaced interpolator table
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: spacing
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_table_spacing ... spacing must be +ve")
      self%table_spacing = spacing
   end subroutine

   PURE subroutine set_table_length(self,length)
   ! Set the length of the interpolation table to be "length". This is for
   ! constant step size interpolation tables.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: length
   ENSURE(length>ZERO,"INTERPOLATOR:set_table_length ... the length must be positive")
      self%table_length = length
   end subroutine

   PURE subroutine set_table_origin(self,origin)
   ! Set the "origin" of the interpolation table to be "length" i.e. the first
   ! point. This is for constant step size interpolation tables.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: origin
      self%table_origin = origin
   end subroutine

   PURE subroutine set_table_eps(self,eps)
   ! Set the tolernace on the table such that values smaller than "eps" are
   ! regarded as zero. This may reduce the length of the interpolation table.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: eps
   ENSURE(eps>ZERO,"INTERPOLATOR:set_table_eps ... the eps must be positive")
      self%table_eps = eps
   end subroutine


! Data & values

   pure subroutine set_n_data(self,val)
   ! Set the no. of data points.
   ! Normally this is rest when adding data.
      type(INTERPOLATOR_TYPE), INOUT :: self
      INT, IN :: val

   ! ENSURE(val>0,"n_data should be +ve")
      self%n_data = val
   end subroutine

   PURE subroutine set_data_points(self,points)
   ! Set the data "points". NOTE: Make sure that .finalize is called
   ! after all set routines.
      type(INTERPOLATOR_TYPE), INOUT :: self
      VEC(REAL,:), IN :: points
      call destroy_(self%data_point)
      call create_(self%data_point,size(points))
      self%data_point = points
      self%n_data = size(self%data_point)
   end subroutine

   PURE subroutine set_data_values_0(self,values)
   ! Set the data "values". NOTE: Make sure that .finalize is called
   ! after all set routines.
      type(INTERPOLATOR_TYPE), INOUT :: self
      VEC(REAL,:), IN :: values
      call destroy_(self%data_value)
      call create_(self%data_value,size(values))
      self%data_value = values
   end subroutine

   PURE subroutine set_spline_2nd_derivatives(self)
   ! Set .y2, the second derivatives of the interpolating function at the
   ! tabulated .data_points's. The values of the second derivatives at
   ! the endpoints are normally zero. The values of the first derivative are
   ! normally taken to be the finite difference of the first two or last two
   ! points.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL :: dx,dxp,dxm,dy,dyp,dym,sig,p
      INT :: n,i,ip1,im1
      VEC(REAL,:), allocatable :: x,y,u
   ENSURE(allocated(self%data_point),"INTERPOLATOR:set_spline_2nd_derivatives ... no data points")
   ENSURE(allocated(self%data_value),"INTERPOLATOR:set_spline_2nd_derivatives ... no data values")
   ENSURE(self%n_data>=3,"INTERPOLATOR:set_spline_2nd_derivatives ... must have at least 3 data points")

      ! No of data
      n = self%n_data

      ! Create the y2
      call destroy_(self%y2)
      call create_(self%y2,n)

      ! Shorthand for data and values
      x = self%data_point
      y = self%data_value
      call create_(u,n)
      if (y1_initial_set_(self)) then
         self%y2(1) = -HALF
         dx = x(2)-x(1)
         dy = y(2)-y(1)
         u(1) = (THREE/dx)*(dy/dx-self%y1_initial)
      else
         self%y2(1) = ZERO
         u(1) = ZERO
      end if
      do i = 2,n-1
         ip1 = i+1
         im1 = i-1
         dxp = x(ip1)-x(i)
         dxm = x(i)-x(im1)
         dyp = y(ip1)-y(i)
         dym = y(i)-y(im1)
         dx = x(ip1)-x(im1)
         sig = dxm/dx
         p = ONE/(sig*self%y2(im1)+TWO)
         self%y2(i) = (sig-ONE)*p
         u(i) = (SIX*(dyp/dxp-dym/dxm)/dx-sig*u(im1))*p
      end do
      if (y1_final_set_(self)) then
         p = HALF
         dy = y(n)-y(n-1)
         dx = x(n)-x(n-1)
         u(n) = (THREE/dx)*(self%y1_final-dy/dx)
      else
         p = ZERO
         u(n) = ZERO
      end if
      self%y2(n) = (u(n)-p*u(n-1))/(p*self%y2(n-1)+ONE)

      ! Back substitution of the triadiagonal algorithm
      do i = n-1,1,-1
         self%y2(i) = self%y2(i)*self%y2(i+1)+u(i)
      end do

      ! Set smoother second derivatives
      call destroy_(u)
   end subroutine

   pure function y1_initial_set(self) result (res)
   ! Return TRUE if the first derivative at the start, .y1_initial, has been set
   ! by the user
      type(INTERPOLATOR_TYPE), IN :: self
      BIN :: res
      res = self%y1_initial/=INTERPOLATOR_Y1_INITIAL
   end function

   pure function y1_final_set(self) result (res)
   ! Return TRUE if the first derivative at the endpoint, .y1_final, has been
   ! set by the user
      type(INTERPOLATOR_TYPE), IN :: self
      BIN :: res
      res = self%y1_final/=INTERPOLATOR_Y1_FINAL
   end function


!  =======
!  Inquiry
!  =======

   PURE function first_data_point(self) result (res)
   ! Returns the first data point
      type(INTERPOLATOR_TYPE), IN :: self
      REAL :: res
   ENSURE(self%finalized,"INTERPOLATOR:first_data_point ... Not finalized")
   ENSURE(self%n_data>0,"INTERPOLATOR:first_data_point ... No data")
   ENSURE(allocated(self%data_point),"INTERPOLATOR:first_data_point ... No data")
      res = self%data_point(1)
   end function

   PURE function last_data_point(self) result (res)
   ! Returns the last data point
      type(INTERPOLATOR_TYPE), IN :: self
      REAL :: res
   ENSURE(self%finalized,"INTERPOLATOR:last_data_point ... Not finalized")
   ENSURE(self%n_data>0,"INTERPOLATOR:last_data_point ... No data")
   ENSURE(allocated(self%data_point),"INTERPOLATOR:last_data_point ... No data")
      res = self%data_point(self%n_data)
   end function

   PURE function last_data_value(self) result (res)
   ! Returns the last data value
      type(INTERPOLATOR_TYPE), IN :: self
      REAL :: res
   ENSURE(self%finalized,"INTERPOLATOR:last_data_value ... Not finalized")
   ENSURE(self%n_data>0,"INTERPOLATOR:last_data_value ... No data")
   ENSURE(allocated(self%data_value),"INTERPOLATOR:last_data_value ... No data")
      res = self%data_value(self%n_data)
   end function


!  ============
!  Read methods
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","INTERPOLATOR:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"INTERPOLATOR:read_keywords ... unexpected end of file while reading list keywords")
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
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=*) :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                    ")  ! exit case
      case ("data_points=         "); call read_data_points_(self)
      case ("data_values=         "); call read_data_values_(self)
      case ("domain_mapping=      "); call read_domain_mapping_(self)
      case ("domain_scale_factor= "); call read_domain_scale_factor_(self)
      case ("interpolation_method="); call read_interpolation_method_(self)
      case ("n_data=              "); call read_n_data_(self)
      case ("put                  "); call put_(self)
      case ("range_mapping=       "); call read_range_mapping_(self)
      case ("table_spacing=       "); call read_spacing_(self)
      case ("table_eps=           "); call read_table_eps_(self)
      case ("table_length=        "); call read_table_length_(self)
      case ("table_origin=        "); call read_table_origin_(self)
      case ("units=               "); call read_units_(self)
      case ("y1_initial=          "); call read_y1_initial_(self)
      case ("y1_final=            "); call read_y1_final_(self)
      case default; allocate(tonto%known_keywords(16))
      tonto%known_keywords(1) = "}                    "
      tonto%known_keywords(2) = "data_points=         "
      tonto%known_keywords(3) = "data_values=         "
      tonto%known_keywords(4) = "domain_mapping=      "
      tonto%known_keywords(5) = "domain_scale_factor= "
      tonto%known_keywords(6) = "interpolation_method="
      tonto%known_keywords(7) = "n_data=              "
      tonto%known_keywords(8) = "put                  "
      tonto%known_keywords(9) = "range_mapping=       "
      tonto%known_keywords(10) = "table_spacing=       "
      tonto%known_keywords(11) = "table_eps=           "
      tonto%known_keywords(12) = "table_length=        "
      tonto%known_keywords(13) = "table_origin=        "
      tonto%known_keywords(14) = "units=               "
      tonto%known_keywords(15) = "y1_initial=          "
      tonto%known_keywords(16) = "y1_final=            "
      call unknown_(tonto,word,"INTERPOLATOR:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(INTERPOLATOR_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_interpolation_method(self)
   ! Read the kind of interpolation method to use.
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: method
      call read_(stdin,method)
      call set_interpolation_method_(self,method)
   end subroutine

   subroutine read_range_mapping(self)
   ! Read the range mapping method to use
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: method
      call read_(stdin,method)
      call set_range_mapping_(self,method)
   end subroutine

   subroutine read_domain_mapping(self)
   ! Read the domain mapping method to use
      type(INTERPOLATOR_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: method
      call read_(stdin,method)
      call set_domain_mapping_(self,method)
   end subroutine

   subroutine read_domain_scale_factor(self)
   ! Read the domain mapping scale factor to use
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL :: fac
      call read_(stdin,fac)
      call set_domain_scale_factor_(self,fac)
   end subroutine

   subroutine read_n_data(self)
   ! Read the no. of data points to use.
      type(INTERPOLATOR_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_n_data_(self,val)
   end subroutine

   subroutine read_table_length(self)
   ! Read the length of the interpolation table to use. This is for constant
   ! step size interpolation tables.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL :: length
      call read_quantity_(stdin,length)
      call set_table_length_(self,length)
   end subroutine

   subroutine read_table_origin(self)
   ! Read the table origin i.e. where the data points begin. This is used for
   ! constant step width interpolation.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL :: origin
      call read_quantity_(stdin,origin)
      call set_table_origin_(self,origin)
   end subroutine

   subroutine read_table_eps(self)
   ! Read the table eps value. Values smaller than this in the interpolation
   ! table are set to zero. This may reduce the size of the table.
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL :: eps
      call read_quantity_(stdin,eps)
      call set_table_eps_(self,eps)
   end subroutine

   subroutine read_data_points(self)
   ! Read in the data points.
      type(INTERPOLATOR_TYPE), INOUT :: self

   ! stdin.read_ptr_quantity(.data_point)
      call read_(stdin,self%data_point)
   end subroutine

   subroutine read_data_values(self)
   ! Read in the data values.
      type(INTERPOLATOR_TYPE), INOUT :: self

   ! stdin.read_ptr_quantity(.data_point)
      call read_(stdin,self%data_point)
   end subroutine

   subroutine read_spacing(self)
   ! Read in the spacing
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL :: spacing
      call read_quantity_(stdin,spacing)
      call set_spacing_(self,spacing)
   end subroutine

   subroutine read_y1_initial(self)
   ! Read the initial first derivative for cubic spline interpolation
      type(INTERPOLATOR_TYPE), INOUT :: self
      call read_quantity_(stdin,self%y1_initial)
   end subroutine

   subroutine read_y1_final(self)
   ! Read the final first derivative for cubic spline interpolation
      type(INTERPOLATOR_TYPE), INOUT :: self
      call read_quantity_(stdin,self%y1_final)
   end subroutine


!  ===================
!  Set from a function
!  ===================

! VEC{SLATERSHELL}

   subroutine set_even_spaced_data_vs(self,sh,func_vs,tol,length)
   ! Set ".data_point" to be even-spaced, starting from
   ! ".table_origin", with a given ".table_spacing", and with
   ! ".data_value" to be the corresponding values evaluated using
   ! monotonically decreasing function "func". The number of data
   ! values and the ".table_length" is either worked out from where
   ! function "func" is just smaller than "tol", with maximum length
   ! ".table_length"; or from the supplied "length".
      type(INTERPOLATOR_TYPE), INOUT :: self
      VEC(type(SLATERSHELL_TYPE),:), IN :: sh
      REAL, optional, IN :: tol
      REAL, optional, IN :: length
      interface
         function func_vs(sh,point) result (value)
         use TYPES_MODULE
            VEC(type(SLATERSHELL_TYPE),:), IN :: sh
            REAL, IN :: point
            REAL :: value
         end function
      end interface
   ENSURE(NOT (present(tol) AND present(length)),"INTERPOLATOR:set_even_spaced_data_vs ... can't supply both tol and length")
      if (present(tol)) then
         call set_sequence_data_vs_(self,sh,self%table_origin,self%table_spacing,self%table_length,func_vs,tol)
      else if (present(length)) then
         call set_sequence_data_vs_(self,sh,self%table_origin,self%table_spacing,length,func_vs)
      else
         call set_sequence_data_vs_(self,sh,self%table_origin,self%table_spacing,self%table_length,func_vs,self%table_eps)
      end if
   end subroutine

   subroutine set_sequence_data_vs(self,sh,first,spacing,length,func_vs,tol)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending until "length", and with ".data_value"
   ! to be the corresponding values evaluated using monotonically decreasing
   ! function "func". The number of data values is worked out from where
   ! function "func" is only just smaller than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      VEC(type(SLATERSHELL_TYPE),:), IN :: sh
      REAL, IN :: spacing
      REAL, IN :: first
      REAL, IN :: length
      REAL, optional, IN :: tol
      interface
         function func_vs(sh,point) result (value)
         use TYPES_MODULE
            VEC(type(SLATERSHELL_TYPE),:), IN :: sh
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      INT :: max_data
      INT :: i,n
      REAL :: p,val
   ENSURE(self%n_data==0,"INTERPOLATOR:set_sequence_data_vs ... there is already data")
   ENSURE(NOT allocated(self%data_value),"INTERPOLATOR:set_sequence_data_vs ... there are already data_values")
   ENSURE(NOT allocated(self%data_point),"INTERPOLATOR:set_sequence_data_vs ... there are already data_points")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_sequence_data_vs ... spacing must be +ve")

      ! Set table spacing
      self%table_spacing = spacing

      ! Get the maximum amount of data
      max_data = ceiling((length-first)/spacing)+1

      ! Create the table
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)

      ! Assign the table; exit if tolerance is reached
      val = ONE
      n = 0
      do i = 1,max_data
         n = n+1

         ! for numerical accuracy
         p = first+(i-1)*spacing

         ! Assign table point
         self%data_point(n) = p

         ! Domain remapping. Warning: user may need to do remapping
         ! before using the table. In all cases below, p^2 must be given
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if

         ! Call the function
         val = func_vs(sh,p)

         ! Range mapping
         if (self%range_is_logarithmic) then
            ENSURE(val>ZERO,"INTERPOLATOR:set_sequence_data_vs ... -ve func cant be used with log interpolation")
            val = log(val)
         end if

         ! Assign table value
         self%data_value(n) = val

         ! If too small?
         if (present(tol)) then
            if (val<tol) exit
         end if
      end do

      ! Warn if the tolerance was not reached
      if (present(tol)) then
         ENSURE(val<tol,"INTERPOLATOR:set_sequence_data_vs ... data table not large enough")
      end if

      ! Reset the table size
      self%n_data = n
      if (present(tol)) then
         call shrink_(self%data_value,self%n_data)
         call shrink_(self%data_point,self%n_data)
      end if

      ! Reset the table length
      self%table_length = self%data_point(self%n_data)

      ! Generate spline data
      if (self%interpolation_is_spline) then

         ! Get approximate LHS derivative
         p = first+spacing*HALF
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if
         val = func_vs(sh,p)
         if (self%range_is_logarithmic) val = log(val)
         self%y1_initial = TWO*(val-self%data_value(1))/spacing

         ! Get approximate RHS derivative
         p = self%data_point(self%n_data)-spacing*HALF
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if
         val = func_vs(sh,p)
         if (self%range_is_logarithmic) val = log(val)
         self%y1_final = TWO*(self%data_value(self%n_data)-val)/spacing

         ! Set the spline second derivatives
         call set_spline_2nd_derivatives_(self)
      end if
      self%finalized = TRUE
   end subroutine


! NONE OF THESE ROUTINES CAN BE PURE
! since they set a saved_self ......

   subroutine set_even_spaced_data(self,func,tol,length)
   ! Set ".data_point" to be even-spaced, starting from
   ! ".table_origin", with a given ".table_spacing", and with
   ! ".data_value" to be the corresponding values evaluated using
   ! monotonically decreasing function "func". The number of data
   ! values and the ".table_length" is either worked out from where
   ! function "func" is just smaller than "tol", with maximum length
   ! ".table_length"; or from the supplied "length".
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, optional, IN :: tol
      REAL, optional, IN :: length
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
   ENSURE(NOT (present(tol) AND present(length)),"INTERPOLATOR:set_even_spaced_data ... can't supply both tol and length")
      if (present(tol)) then
         call set_sequence_data_(self,self%table_origin,self%table_spacing,self%table_length,func,tol)
      else if (present(length)) then
         call set_sequence_data_(self,self%table_origin,self%table_spacing,length,func)
      else
         call set_sequence_data_(self,self%table_origin,self%table_spacing,self%table_length,func,self%table_eps)
      end if
   end subroutine

   subroutine set_sequence_data(self,first,spacing,length,func,tol)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending until "length", and with ".data_value"
   ! to be the corresponding values evaluated using monotonically decreasing
   ! function "func". The number of data values is worked out from where
   ! function "func" is only just smaller than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: spacing
      REAL, IN :: first
      REAL, IN :: length
      REAL, optional, IN :: tol
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      INT :: max_data
      INT :: i,n
      REAL :: p,val
   ENSURE(self%n_data==0,"INTERPOLATOR:set_sequence_data ... there is already data")
   ENSURE(NOT allocated(self%data_value),"INTERPOLATOR:set_sequence_data ... there are already data_values")
   ENSURE(NOT allocated(self%data_point),"INTERPOLATOR:set_sequence_data ... there are already data_points")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_sequence_data ... spacing must be +ve")

      ! Set table spacing
      self%table_spacing = spacing

      ! Get the maximum amount of data
      max_data = ceiling((length-first)/spacing)+1

      ! Create the table
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)

      ! Assign the table; exit if tolerance is reached
      val = ONE
      n = 0
      do i = 1,max_data
         n = n+1

         ! for numerical accuracy
         p = first+(i-1)*spacing

         ! Assign table point
         self%data_point(n) = p

         ! Domain remapping. Warning: user may need to do remapping
         ! before using the table. In all cases below, p^2 must be given
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if

         ! Call the function
         val = func(p)

         ! Range mapping
         if (self%range_is_logarithmic) then
            ENSURE(val>ZERO,"INTERPOLATOR:set_sequence_data ... -ve func cant be used with log interpolation")
            val = log(val)
         end if

         ! Assign table value
         self%data_value(n) = val

         ! If too small?
         if (present(tol)) then
            if (val<tol) exit
         end if
      end do

      ! Warn if the tolerance was not reached
      if (present(tol)) then
         ENSURE(val<tol,"INTERPOLATOR:set_sequence_data ... data table not large enough")
      end if

      ! Reset the table size
      self%n_data = n
      if (present(tol)) then
         call shrink_(self%data_value,self%n_data)
         call shrink_(self%data_point,self%n_data)
      end if

      ! Reset the table length
      self%table_length = self%data_point(self%n_data)

      ! Generate spline data
      if (self%interpolation_is_spline) then

         ! Get approximate LHS derivative
         p = first+spacing*HALF
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if
         val = func(p)
         if (self%range_is_logarithmic) val = log(val)
         self%y1_initial = TWO*(val-self%data_value(1))/spacing

         ! Get approximate RHS derivative
         p = self%data_point(self%n_data)-spacing*HALF
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if
         val = func(p)
         if (self%range_is_logarithmic) val = log(val)
         self%y1_final = TWO*(self%data_value(self%n_data)-val)/spacing

         ! Set the spline second derivatives
         call set_spline_2nd_derivatives_(self)
      end if
      self%finalized = TRUE
   end subroutine

   subroutine set_even_spaced_general_data_0(self,func,tol,length)
   ! Set ".data_point" to be even-spaced, starting from
   ! ".table_origin", with a given ".table_spacing", and with
   ! ".data_value" to be the corresponding values evaluated using
   ! monotonically decreasing function "func". The number of data
   ! values and the ".table_length" is either worked out from where
   ! function "func" is just smaller than "tol", with maximum length
   ! ".table_length"; or from the supplied "length".
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, optional, IN :: tol
      REAL, optional, IN :: length
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
   ENSURE(NOT (present(tol) AND present(length)),"INTERPOLATOR:set_even_spaced_general_data_0 ... can't supply both tol and length")
      if (present(tol)) then
         call set_even_spaced_general_data_(self,self%table_origin,self%table_spacing,self%table_length,func,tol)
      else if (present(length)) then
         call set_even_spaced_general_data_(self,self%table_origin,self%table_spacing,length,func)
      else
         call set_even_spaced_general_data_(self,self%table_origin,self%table_spacing,self%table_length,func,self%table_eps)
      end if
   end subroutine

   subroutine set_even_spaced_general_data_1(self,first,spacing,length,func,tol)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending until "length", and with ".data_value"
   ! to be the corresponding values evaluated using monotonically decreasing
   ! function "func". The number of data values is worked out from where
   ! function "func" is only just smaller than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: first
      REAL, IN :: spacing
      REAL, IN :: length
      REAL, optional, IN :: tol
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      INT :: max_data
      INT :: i,n
      REAL :: p,val
   ENSURE(self%n_data==0,"INTERPOLATOR:set_even_spaced_general_data_1 ... there is already data")
   ENSURE(NOT (NOT allocated(self%data_value)),"INTERPOLATOR:set_even_spaced_general_data_1 ... there are already data_values")
   ENSURE(NOT (NOT allocated(self%data_point)),"INTERPOLATOR:set_even_spaced_general_data_1 ... there are already data_points")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_even_spaced_general_data_1 ... spacing must be +ve")

      ! Set table spacing
      self%table_spacing = spacing

      ! Get the maximum amount of data
      max_data = ceiling((length-first)/spacing)+1

      ! Create the table
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)

      ! Assign the table; exit if tolerance is reached
      val = ONE
      n = 0
      do i = 1,max_data
         n = n+1

         ! for numerical accuracy
         p = first+(i-1)*spacing

         ! Assign table point
         self%data_point(n) = p

         ! Domain remapping. Warning: user may need to do remapping
         ! before using the table. In all cases below, p^2 must be given
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if

         ! Call the function
         val = func(p)

         ! Range mapping
         if (self%range_is_logarithmic) then
            ENSURE(val>ZERO,"INTERPOLATOR:set_even_spaced_general_data_1 ... -ve func cant be used with log interpolation")
            val = log(val)
         end if

         ! Assign table value
         self%data_value(n) = val

         ! If too small?
         if (present(tol)) then
            if (val<tol) exit
         end if
      end do

      ! Warn if the tolerance was not reached
      if (present(tol)) then
         ENSURE(val<tol,"INTERPOLATOR:set_even_spaced_general_data_1 ... data table not large enough")
      end if

      ! Reset the table size
      self%n_data = n
      if (present(tol)) then
         call shrink_(self%data_value,self%n_data)
         call shrink_(self%data_point,self%n_data)
      end if

      ! Reset the table length
      self%table_length = self%data_point(self%n_data)

      ! Generate spline data
      if (self%interpolation_is_spline) then

         ! Get approximate LHS derivative
         p = first+spacing*HALF
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if
         val = func(p)
         if (self%range_is_logarithmic) val = log(val)
         self%y1_initial = TWO*(val-self%data_value(1))/spacing

         ! Get approximate RHS derivative
         p = self%data_point(self%n_data)-spacing*HALF
         if (self%domain_is_sqrt) then; p = sqrt(p)
         else if (self%domain_is_sqrt_x1x) then; p = self%domain_scale_factor*sqrt(p/(ONE-p))
         end if
         val = func(p)
         if (self%range_is_logarithmic) val = log(val)
         self%y1_final = TWO*(self%data_value(self%n_data)-val)/spacing

         ! Set the spline second derivatives
         call set_spline_2nd_derivatives_(self)
      end if
      self%finalized = TRUE
   end subroutine

   function the_table_length(self,func,tol) result (p)
   ! Return the length of the interpolation table starting from ".table_origin"
   ! with a given ".table_spacing", extending for a total ".table_length", and
   ! with the data values to be the corresponding values evaluated using
   ! monotonically decreasing function "func". The length of the table "p" is
   ! worked out from where function "func" is only just smaller than "tol". 
   ! It cannot exceed ".table_length".
      type(INTERPOLATOR_TYPE), IN :: self
      REAL, IN :: tol
      REAL :: p
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      REAL :: first,spacing,length
      INT :: max_data
      INT :: i,s
      REAL :: val
   ENSURE(self%n_data==0,"INTERPOLATOR:the_table_length ... there is already data")
   ENSURE(self%table_spacing>ZERO,"INTERPOLATOR:the_table_length ... spacing must be +ve")
      first = self%table_origin
      length = self%table_length
      spacing = self%table_spacing
      max_data = ceiling((length-first)/spacing)+1
      p = first
      s = 0
      do i = s+1,max_data
         val = func(p)
         if (val<tol) exit
         p = p+spacing
      end do
   end function

   function the_table_length_vs(self,sh,func_vs,tol) result (p)
   ! Return the length of the interpolation table starting from ".table_origin"
   ! with a given ".table_spacing", extending for a total ".table_length", and
   ! with the data values to be the corresponding values evaluated using
   ! monotonically decreasing function "func". The length of the table "p" is
   ! worked out from where function "func" is only just smaller than "tol". 
   ! It cannot exceed ".table_length".
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(type(SLATERSHELL_TYPE),:), IN :: sh
      REAL, IN :: tol
      REAL :: p
      interface
         function func_vs(sh,point) result (value)
         use TYPES_MODULE
            VEC(type(SLATERSHELL_TYPE),:), IN :: sh
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      REAL :: first,spacing,length
      INT :: max_data
      INT :: i,s
      REAL :: val
   ENSURE(self%n_data==0,"INTERPOLATOR:the_table_length_vs ... there is already data")
   ENSURE(self%table_spacing>ZERO,"INTERPOLATOR:the_table_length_vs ... spacing must be +ve")
      first = self%table_origin
      length = self%table_length
      spacing = self%table_spacing
      max_data = ceiling((length-first)/spacing)+1
      p = first
      s = 0
      do i = s+1,max_data
         val = func_vs(sh,p)
         if (val<tol) exit
         p = p+spacing
      end do
   end function

   subroutine set_data(self,length,spacing,make_values)
   ! Set ".data_point" to be even-spaced: starting from 0, extending
   ! until "length", and with a given "spacing"; and set ".data_value"
   ! to be from "make_values(.data_value,.data_point)".
   ! NOTE: If .data_points is already defined, use those points
   ! NOTE: interpolation is not yet allowed
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: length
      REAL, IN :: spacing
      interface
         subroutine make_values(values,points)
            VEC(REAL,:), OUT :: values
            VEC(REAL,:), IN :: points
         end subroutine
      end interface
      INT :: i
   ENSURE(self%n_data==0,"INTERPOLATOR:set_data ... there is already data")
   ENSURE(NOT allocated(self%data_value),"INTERPOLATOR:set_data ... there are already data_values")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_data ... spacing must be +ve")

      ! Set table spacing
      self%table_origin = ZERO
      self%table_length = length
      self%table_spacing = spacing
      if (NOT allocated(self%data_point)) then

         ! Assign linear data points
         self%n_data = ceiling((length)/spacing)+1  ! The number of data
         call create_(self%data_value,self%n_data)
         call create_(self%data_point,self%n_data)
         self%data_point = [((i-1)*spacing,i = 1,self%n_data)]
      else

         ! Custom data points
         self%n_data = size(self%data_point)
         call create_(self%data_value,self%n_data)
      end if

      ! Assign data values
      call make_values(self%data_value,self%data_point)
      self%finalized = TRUE
   end subroutine

   subroutine set_even_spaced_d_data_0(self,func)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending for a total "length", and with ".data_value"
   ! to be the corresponding DERIVATIVE values evaluated using monotonically
   ! decreasing function "func". The number of data values is worked out from
   ! where function "func" is greater than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      call set_even_spaced_d_data_(self,self%table_origin,self%table_spacing,self%table_length,func,self%table_eps)
   end subroutine

   subroutine set_even_spaced_d_data_1(self,first,spacing,length,func,tol)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending for a total "length", and with ".data_value"
   ! to be the corresponding DERIVATIVE values evaluated using monotonically
   ! decreasing function "func". The number of data values is worked out from
   ! where function "func" is only just smaller than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: first
      REAL, IN :: spacing
      REAL, IN :: length
      REAL, IN :: tol
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      INT :: max_data
      INT :: i,s,n
      REAL :: p,del,val0,valp,valm
   ENSURE(self%n_data==0,"INTERPOLATOR:set_even_spaced_d_data_1 ... there is already data")
   ENSURE(NOT allocated(self%data_value),"INTERPOLATOR:set_even_spaced_d_data_1 ... there are already data_values")
   ENSURE(NOT allocated(self%data_point),"INTERPOLATOR:set_even_spaced_d_data_1 ... there are already data_points")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_even_spaced_d_data_1 ... spacing must be +ve")

      ! Set table spacing
      self%table_spacing = spacing

      ! Get the maximum amount of data
      max_data = ceiling((length-first)/spacing)+1

      ! Create the table
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)

      ! Assign the table; exit if tolerance is reached
      del = TOL(6)
      p = first
      s = 0
      n = 1
      do i = s+1,max_data
         valp = func(p+del)
         val0 = func(p)
         valm = func(p-del)
         self%data_point(n) = p
         self%data_value(n) = (valp-valm)/(TWO*del)
         p = p+spacing
         n = n+1
         if (val0<tol) exit
      end do

      ! Reset the table size
      self%n_data = n-1
      call shrink_(self%data_value,self%n_data)
      call shrink_(self%data_point,self%n_data)

      ! Reset the table length
      self%table_length = self%data_point(self%n_data)

      ! Finalise: assume monotonic decreasing function
      if (self%range_is_logarithmic) then
         ENSURE(all(self%data_value>ZERO),"INTERPOLATOR:set_even_spaced_d_data_1 ... -ve data_vales in log interpolation!")
         self%data_value = log(-self%data_value)
      end if
      if (self%interpolation_is_spline) call set_spline_2nd_derivatives_(self)
      self%finalized = TRUE
   end subroutine

   subroutine set_even_spaced_d2_data_0(self,func)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending for a total "length", and with ".data_value"
   ! to be the corresponding 2nd DERIVATIVE values evaluated using monotonically
   ! decreasing function "func". The number of data values is worked out from
   ! where function "func" is greater than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      call set_even_spaced_d2_data_(self,self%table_origin,self%table_spacing,self%table_length,func,self%table_eps)
   end subroutine

   subroutine set_even_spaced_d2_data_1(self,first,spacing,length,func,tol)
   ! Set ".data_point" to be even-spaced, starting from the "first" value, with
   ! a given "spacing", extending for a total "length", and with ".data_value"
   ! to be the corresponding 2nd DERIVATIVE values evaluated using monotonically
   ! decreasing function "func". The number of data values is worked out from
   ! where function "func" is only just smaller than "tol". 
      type(INTERPOLATOR_TYPE), INOUT :: self
      REAL, IN :: first
      REAL, IN :: spacing
      REAL, IN :: length
      REAL, IN :: tol
      interface
         function func(point) result (value)
            REAL, IN :: point
            REAL :: value
         end function
      end interface
      INT :: max_data
      INT :: i,s,n
      REAL :: p,del,del2,val0,valp,valm
   ENSURE(self%n_data==0,"INTERPOLATOR:set_even_spaced_d2_data_1 ... there is already data")
   ENSURE(NOT allocated(self%data_value),"INTERPOLATOR:set_even_spaced_d2_data_1 ... there are already data_values")
   ENSURE(NOT allocated(self%data_point),"INTERPOLATOR:set_even_spaced_d2_data_1 ... there are already data_points")
   ENSURE(spacing>ZERO,"INTERPOLATOR:set_even_spaced_d2_data_1 ... spacing must be +ve")

      ! Set table spacing
      self%table_spacing = spacing

      ! Get the maximum amount of data
      max_data = ceiling((length-first)/spacing)+1

      ! Create the table
      call destroy_(self%data_point)
      call destroy_(self%data_value)
      call create_(self%data_point,max_data)
      call create_(self%data_value,max_data)

      ! Assign the table; exit if tolerance is reached
      del = TOL(6)
      del2 = del*del
      p = first
      s = 0
      n = 1
      do i = s+1,max_data
         valp = func(p+del)
         val0 = func(p)
         valm = func(p-del)
         self%data_point(n) = p
         self%data_value(n) = (valp+valm-TWO*val0)/del2
         p = p+spacing
         n = n+1
         if (val0<tol) exit
      end do

      ! Reset the table size
      self%n_data = n-1
      call shrink_(self%data_value,self%n_data)
      call shrink_(self%data_point,self%n_data)

      ! Reset the table size
      self%table_length = self%data_point(self%n_data)

      ! Finalize
      call finalize_(self)
   end subroutine

   subroutine set_data_values_1(self,func)
   ! Set the ".data_values" from the function "func", which returns "values"
   ! from a set of given "points".
      type(INTERPOLATOR_TYPE), INOUT :: self
      interface
         subroutine func(points,values)
            VEC(REAL,:), IN :: points
            VEC(REAL,:), OUT :: values
         end subroutine
      end interface
   ENSURE(self%finalized,"INTERPOLATOR:set_data_values_1 ... not finalized")
   ENSURE(allocated(self%data_point),"INTERPOLATOR:set_data_values_1 ... no data_points")
   ENSURE(size(self%data_point)==self%n_data,"INTERPOLATOR:set_data_values_1 ... wrong number of data_points")
      call destroy_(self%data_value)
      call create_(self%data_value,self%n_data)
      call func(self%data_point,self%data_value)
   end subroutine

   subroutine set_data_values_2(self,func_at,pos)
   ! Set the ".data_values" from the function "func_at", which returns "values"
   ! from a set of given "points", and an additional single "pos" as parameter.
      type(INTERPOLATOR_TYPE), INOUT :: self
      interface
         PURE subroutine func_at(points,pos,values)
            VEC(REAL,:), IN :: points
            VEC(REAL,3), IN :: pos
            VEC(REAL,:), OUT :: values
         end subroutine
      end interface
      VEC(REAL,3), IN :: pos
   ENSURE(self%finalized,"INTERPOLATOR:set_data_values_2 ... not finalized")
   ENSURE(allocated(self%data_point),"INTERPOLATOR:set_data_values_2 ... no data_points")
   ENSURE(size(self%data_point)==self%n_data,"INTERPOLATOR:set_data_values_2 ... wrong number of data_points")
      call destroy_(self%data_value)
      call create_(self%data_value,self%n_data)
      call func_at(self%data_point,pos,self%data_value)
   end subroutine


!  =================
!  Values for points
!  =================

   PURE function value_for(self,point) result (res)
   ! Return the interpolated value for "point".
   ! WARNING: not to be used with domain mapping unless the user
   !          pre-treats the point
   ! WARNING: "point" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      REAL, IN :: point
      REAL :: res

! ENSURE(.finalized,"not finalized")
      if (self%interpolation_is_spline) then; res = value_for_spline_(self,point)
      else; res = value_for_linear_(self,point)
      end if
   end function

   PURE function value_for_linear(self,point) result (res)
   ! WARNING: "point" may be overwritten
   ! Return the interpolated value for "point".
   ! WARNING: not to be used with domain mapping unless the user
   !          pre-treats the point
   ! WARNING: "point" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      REAL, IN :: point
      REAL :: res
      REAL :: pt,first,last,frac,s
      INT :: f,f1
   ENSURE(self%finalized,"INTERPOLATOR:value_for_linear ... not finalized")

      ! Rescale data if required
      pt = point
      if (self%domain_is_sqrt_x1x) then
         s = self%domain_scale_factor
         s = s*s
         pt = point/(s+point)
      end if
      first = self%data_point(1)
      last = self%data_point(self%n_data)
      if (pt<first OR last<pt) then
         res = ZERO
      else
         frac = (pt-self%data_point(1))/self%table_spacing
         f = floor(frac)
         f1 = f+1
         res = self%data_value(f1)
         if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
         if (self%range_is_logarithmic) res = exp(res)
      end if
   end function

   PURE function value_for_spline(self,point) result (res)
   ! Return the spline interpolated value for "point".
   ! WARNING: not to be used with domain mapping unless the user
   !          pre-treats the point
   ! WARNING: "point" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      REAL, IN :: point
      REAL :: res
      REAL :: pt,first,last,frac,dl6,v2,a,b,s
      INT :: f,f1,f2
   ENSURE(self%finalized,"INTERPOLATOR:value_for_spline ... not finalized")

      ! Rescale point if required
      pt = point
      if (self%domain_is_sqrt_x1x) then
         s = self%domain_scale_factor
         s = s*s
         pt = point/(s+point)
      end if
      first = self%data_point(1)
      last = self%data_point(self%n_data)
      if (pt<first OR last<pt) then
         res = ZERO
      else
         frac = (pt-self%data_point(1))/self%table_spacing
         f = floor(frac)
         f1 = f+1
         res = self%data_value(f1)
         if (frac>f) then  ! point lies between spacings
            f2 = f1+1
            v2 = self%data_value(f2)
            dl6 = self%table_spacing*self%table_spacing*SIXTH
            b = frac-f
            a = ONE-b
            res = a*res+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*dl6
         end if
         if (self%range_is_logarithmic) res = exp(res)
      end if
   end function

   PURE subroutine values_for(self,points,values,fac)
   ! Return the interpolated "values" for a series of squared "points".
   ! Optional "fac" may be app[lied to the "values"
   ! WARNING: not to be used with domain mapping unless the user
   !          pre-treats the points
   ! WARNING: "points" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), IN :: points
      VEC(REAL,:), OUT :: values
      REAL, optional, IN :: fac

! ENSURE(.finalized,"not finalized")
      if (self%interpolation_is_spline) then; call values_for_spline_(self,points,values,fac)
      else; call values_for_linear_(self,points,values,fac)
      end if
   end subroutine

   PURE subroutine values_for_linear(self,points,values,fac)
   ! Get interpolated "values" for a series of squared "points" 
   ! Optional "fac" may be app[lied to the "values"
   ! WARNING: To used with sqrt type domain mapping.
   ! WARNING: "points" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), IN :: points
      VEC(REAL,:), OUT :: values
      REAL, optional, IN :: fac

! ENSURE(.finalized,"not finalized")
      VEC(REAL,:), allocatable :: pt
      REAL :: first,last,del1,p,frac,res,s
      INT :: i,f,f1
      BIN :: do_fac

      ! fac present?
      do_fac = FALSE
      if (present(fac)) then
         if (NOT equals_(fac,ONE)) then
            do_fac = TRUE
         end if
      end if

      ! Rescale points if required
      pt = points
      if (self%domain_is_sqrt_x1x) then
         s = self%domain_scale_factor
         s = s*s
         pt = pt/(s+pt)
      end if
      first = self%data_point(1)
      last = self%data_point(self%n_data)
      del1 = ONE/self%table_spacing
      if (NOT do_fac) then
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  res = self%data_value(f1)
                  if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
                  values(i) = exp(res)
               end if
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  res = self%data_value(f1)
                  if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
                  values(i) = res
               end if
            end do
         end if
      else  ! use fac
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  res = self%data_value(f1)
                  if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
                  values(i) = fac*exp(res)
               end if
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  res = self%data_value(f1)
                  if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
                  values(i) = fac*res
               end if
            end do
         end if
      end if
   end subroutine

   PURE subroutine values_for_spline(self,points,values,fac)
   ! Return the spline interpolated "values" for squared "points".
   ! Optional "fac" may be app[lied to the "values"
   ! WARNING: To used with sqrt type domain mapping.
   ! WARNING: "points" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), IN :: points
      VEC(REAL,:), OUT :: values
      REAL, optional, IN :: fac

! ENSURE(.finalized,"not finalized")
      VEC(REAL,:), allocatable :: pt
      REAL :: first,last,p,frac,del1,del6,v1,v2,a,b,s
      INT :: i,f,f1,f2
      BIN :: do_fac

      ! fac present?
      do_fac = FALSE
      if (present(fac)) then
         if (NOT equals_(fac,ONE)) then
            do_fac = TRUE
         end if
      end if

      ! Rescale data if required
      pt = points
      if (self%domain_is_sqrt_x1x) then
         s = self%domain_scale_factor
         s = s*s
         pt = pt/(s+pt)
      end if
      first = self%data_point(1)
      last = self%data_point(self%n_data)
      del1 = self%table_spacing
      del6 = del1*del1*SIXTH
      del1 = ONE/del1
      if (NOT do_fac) then
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  v1 = self%data_value(f1)
                  if (frac>f) then  ! point lies between spacings
                     f2 = f1+1
                     v2 = self%data_value(f2)
                     b = frac-f
                     a = ONE-b
                     v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
                  end if
                  values(i) = exp(v1)
               end if
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  v1 = self%data_value(f1)
                  if (frac>f) then  ! point lies between spacings
                     f2 = f1+1
                     v2 = self%data_value(f2)
                     b = frac-f
                     a = ONE-b
                     v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
                  end if
                  values(i) = v1
               end if
            end do
         end if
      else  ! do fac
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  v1 = self%data_value(f1)
                  if (frac>f) then  ! point lies between spacings
                     f2 = f1+1
                     v2 = self%data_value(f2)
                     b = frac-f
                     a = ONE-b
                     v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
                  end if
                  values(i) = fac*exp(v1)
               end if
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) then
                  values(i) = ZERO
               else
                  frac = (p-first)*del1
                  f = floor(frac)
                  f1 = f+1
                  v1 = self%data_value(f1)
                  if (frac>f) then  ! point lies between spacings
                     f2 = f1+1
                     v2 = self%data_value(f2)
                     b = frac-f
                     a = ONE-b
                     v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
                  end if
                  values(i) = fac*v1
               end if
            end do
         end if
      end if
   end subroutine

   PURE subroutine add_values_for(self,points,values,fac)
   ! Add in the interpolated "values" for a series of "points". 
   ! Optional "fac" may be applied to the values.
   ! WARNING: not to be used with domain mapping unless the user
   !          pre-treats the point
   ! WARNING: "points" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), IN :: points
      VEC(REAL,:), INOUT :: values
      REAL, optional, IN :: fac

! ENSURE(.finalized,"not finalized")
      if (self%interpolation_is_spline) then; call add_values_for_spline_(self,points,values,fac)
      else; call add_values_for_linear_(self,points,values,fac)
      end if
   end subroutine

   PURE subroutine add_values_for_linear(self,points,values,fac)
   ! Add interpolated "values" for a series of "points" 
   ! Optional "fac" may be applied to the values.
   ! WARNING: To used with sqrt type domain mapping.
   ! WARNING: "points" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), IN :: points
      VEC(REAL,:), INOUT :: values
      REAL, optional, IN :: fac

! ENSURE(.finalized,"not finalized")
      VEC(REAL,:), allocatable :: pt
      REAL :: first,last,del1,p,frac,res,s
      INT :: i,f,f1
      BIN :: do_fac

      ! fac present?
      do_fac = FALSE
      if (present(fac)) then
         if (NOT equals_(fac,ONE)) then
            do_fac = TRUE
         end if
      end if

      ! Rescale data if required
      pt = points
      if (self%domain_is_sqrt_x1x) then
         s = self%domain_scale_factor
         s = s*s
         pt = pt/(s+pt)
      end if
      first = self%data_point(1)
      last = self%data_point(self%n_data)
      del1 = ONE/self%table_spacing
      if (NOT do_fac) then
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               res = self%data_value(f1)
               if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
               values(i) = values(i)+exp(res)
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               res = self%data_value(f1)
               if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
               values(i) = values(i)+res
            end do
         end if
      else  ! use fac
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               res = self%data_value(f1)
               if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
               values(i) = values(i)+fac*exp(res)
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               res = self%data_value(f1)
               if (frac>f) res = res+(frac-f)*(self%data_value(f1+1)-res)
               values(i) = values(i)+fac*res
            end do
         end if
      end if
   end subroutine

   PURE subroutine add_values_for_spline(self,points,values,fac)
   ! Add in the spline interpolated "values for the "points".
   ! Optional "fac" may be applied to the values.
   ! WARNING: To used with sqrt type domain mapping.
   ! WARNING: "points" may be overwritten
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), target, IN :: points
      VEC(REAL,:), INOUT :: values
      REAL, optional, IN :: fac

! ENSURE(.finalized,"not finalized")
      VEC(REAL,:), allocatable :: pt
      REAL :: first,last,p,frac,del1,del6,v1,v2,a,b,s
      INT :: i,f,f1,f2
      BIN :: do_fac

      ! fac present?
      do_fac = FALSE
      if (present(fac)) then
         if (NOT equals_(fac,ONE)) then
            do_fac = TRUE
         end if
      end if

      ! Rescale data if required
      pt = points
      if (self%domain_is_sqrt_x1x) then
         s = self%domain_scale_factor
         s = s*s
         pt = pt/(s+pt)
      end if
      first = self%data_point(1)
      last = self%data_point(self%n_data)
      del1 = self%table_spacing
      del6 = del1*del1*SIXTH
      del1 = ONE/del1
      if (NOT do_fac) then
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               v1 = self%data_value(f1)
               if (frac>f) then  ! point lies between spacings
                  f2 = f1+1
                  v2 = self%data_value(f2)
                  b = frac-f
                  a = ONE-b
                  v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
               end if
               values(i) = values(i)+exp(v1)
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               v1 = self%data_value(f1)
               if (frac>f) then  ! point lies between spacings
                  f2 = f1+1
                  v2 = self%data_value(f2)
                  b = frac-f
                  a = ONE-b
                  v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
               end if
               values(i) = values(i)+v1
            end do
         end if
      else  ! do fac
         if (self%range_is_logarithmic) then
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               v1 = self%data_value(f1)
               if (frac>f) then  ! point lies between spacings
                  f2 = f1+1
                  v2 = self%data_value(f2)
                  b = frac-f
                  a = ONE-b
                  v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
               end if
               values(i) = values(i)+fac*exp(v1)
            end do
         else
            do i = 1,size(pt)
               p = pt(i)
               if (p<first OR last<p) cycle
               frac = (p-first)*del1
               f = floor(frac)
               f1 = f+1
               v1 = self%data_value(f1)
               if (frac>f) then  ! point lies between spacings
                  f2 = f1+1
                  v2 = self%data_value(f2)
                  b = frac-f
                  a = ONE-b
                  v1 = a*v1+b*v2+(a*(a*a-1)*self%y2(f1)+b*(b*b-1)*self%y2(f2))*del6
               end if
               values(i) = values(i)+fac*v1
            end do
         end if
      end if
   end subroutine


!   fast_d_values_for(points) result (values)
!   ! Return the interpolated "values" for a series of "points" in the special
!   ! case where the table_origin is ZERO and where the data is known to be even
!   ! spaced. The finalisation tests and other preconditions are skipped in the
!   ! interests of speed. This version is for radial DERIVATIVES, and has a minus
!   ! sign when the log interpolation is used.
!      self :: IN
!      points :: VEC{REAL}, IN
!      values :: VEC{REAL}(points.dim)
!   ! ENSURE(.finalized,"not finalized")
!   ! ENSURE(.table_origin.is_zero,"table_origin is not zero")
!   ! ENSURE(all(points)>ZERO,"the points are not all +ve")
!      del,frac,res,last :: REAL
!      i,n,f,i1,i2 :: INT
!      is_log :: BIN
!      n = 0
!      last =.data_point(.n_data)
!      do i = 1,points.dim       ! set points off the end of the table
!        if (points(i)<last) cycle
!        values(i) = ZERO
!        n = n + 1
!      end
!      if (n==points.dim) return ! all points too far away
!      is_log = .is_logarithmic
!      if (is_log) then
!        do i = 1,points.dim
!          if (points(i)>=last) cycle
!          frac = points(i)/.table_spacing
!          f  = floor(frac)
!          i1 = f + 1
!          i2 = i1 + 1
!          if (frac>f) then ! point lies between spacing
!            del = .data_value(i2) - .data_value(i1)
!            res = .data_value(i1) + (frac-f)*del
!          else             ! point is exactly equal to a data_point
!            res  = .data_value(i1)
!          end
!          values(i) = -exp(res)
!        end
!      else
!        do i = 1,points.dim
!          if (points(i)>=last) cycle
!          frac = points(i)/.table_spacing
!          f  = floor(frac)
!          i1 = f + 1
!          i2 = i1 + 1
!          if (frac>f) then ! point lies between spacing
!            del = .data_value(i2) - .data_value(i1)
!            res = .data_value(i1) + (frac-f)*del
!          else             ! point is exactly equal to a data_point
!            res  = .data_value(i1)
!          end
!          values(i) = res
!       end
!     end
!   end

!  ===============
!  Inquiry methods
!  ===============

   pure function skip_values_for(self,points) result (res)
   ! Return TRUE if the interpolated "values" for a series of "points" are all
   ! zero, so that we can skip their evaluation.
      type(INTERPOLATOR_TYPE), IN :: self
      VEC(REAL,:), IN :: points
      BIN :: res

   ! ENSURE(.finalized,"not finalized")
   ! ENSURE(.table_origin.is_zero,"table_origin is not zero")
   ! ENSURE(all(points)>ZERO,"the points are not all +ve")
      INT :: i,n
      REAL :: last
      n = 0
      last = self%data_point(self%n_data)
      ! Set points off the end of the table
      do i = 1,size(points)
         if (points(i)<last) cycle
         n = n+1
      end do
      res = n==size(points)
   end function


!  ====
!  Dump
!  ====

   subroutine do_dump(self)
   ! Dump for debugging or reading back in
      type(INTERPOLATOR_TYPE), IN :: self
      call dump_(stdout,"interpolation_method",self%interpolation_method)
      call dump_(stdout,"interpolation_is_spline",self%interpolation_is_spline)
      call dump_(stdout,"range_mapping",self%range_mapping)
      call dump_(stdout,"range_is_logarithmic",self%range_is_logarithmic)
      call dump_(stdout,"domain_mapping",self%domain_mapping)
      call dump_(stdout,"domain_scale_factor",self%domain_scale_factor)
      call dump_(stdout,"domain_is_sqrt",self%domain_is_sqrt)
      call dump_(stdout,"domain_is_sqrt_x1x",self%domain_is_sqrt_x1x)
      call dump_(stdout,"table_origin",self%table_origin)
      call dump_(stdout,"table_eps",self%table_eps)
      call dump_(stdout,"n_data",self%n_data)
      call dump_(stdout,"data_point",self%data_point)
      call dump_(stdout,"data_value",self%data_value)
      call dump_(stdout,"finalized",self%finalized)
      call dump_(stdout,"y2",self%y2)
      call dump_(stdout,"y1_initial",self%y1_initial)
      call dump_(stdout,"y1_final",self%y1_final)
   end subroutine

   subroutine dump(self,object_name,all)
   ! Dump object data as text
      type(INTERPOLATOR_TYPE), IN :: self
      STR(len=*), IN :: object_name
      BIN, optional, IN :: all
      BIN :: is_all

      ! The following code is inherited from OBJECT
      is_all = FALSE
      if (present(all)) is_all = all
      if (is_all) then; call text_(stdout,trim(object_name)//":INTERPOLATOR@ = { ")
      else; call text_(stdout,trim(object_name)//":INTERPOLATOR = { ")
      end if
      call increment_margin_width_(stdout,1)
      call do_dump_(self)
      call increment_margin_width_(stdout,-1)
      call text_(stdout,"}")
   end subroutine

   subroutine dmpa(self,object_name)
   ! Dump allocatable object data as text
      type(INTERPOLATOR_TYPE), allocatable, IN :: self
      STR(len=*), IN :: object_name

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) then
         call text_(stdout,trim(object_name)//":INTERPOLATOR@ = null")
      else
         call dump_(self,trim(object_name),all=TRUE)
      end if
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self,fac)
   ! Put the list of interpolating data
      type(INTERPOLATOR_TYPE), IN :: self
      REAL, optional, IN :: fac
      INT :: i
      REAL :: factor
   ENSURE(self%finalized,"INTERPOLATOR:put ... not finalized")
      factor = ONE
      if (present(fac)) factor = fac
      call flush_(stdout)
      call text_(stdout,"INTERPOLATOR info")
      call flush_(stdout)
      call show_(stdout,"Interpolation method             =",self%interpolation_method)
      call show_(stdout,"Spline interpolation?            =",self%interpolation_is_spline)
      call show_(stdout,"Range mapping?                   =",self%range_mapping)
      call show_(stdout,"Range is logarithmic?            =",self%range_is_logarithmic)
      call show_(stdout,"Domain mapping?                  =",self%domain_mapping)
      call show_(stdout,"Domain scale factor?             =",self%domain_scale_factor)
      call show_(stdout,"Domain is sqrt'd?                =",self%domain_is_sqrt)
      call show_(stdout,"Domain is sqrt(x/1-x)            =",self%domain_is_sqrt_x1x)
      call flush_(stdout)
      call show_(stdout,"No. of interpolating data points =",self%n_data)
      call flush_(stdout)
      call show_(stdout,"Initial data value (tab. origin) =",self%table_origin)
      call show_(stdout,"Table length                     =",self%table_length)
      call show_(stdout,"Data point spacing               =",self%table_spacing)
      call show_(stdout,"Table eps (smallest data value)  =",self%table_eps)
      if (allocated(self%data_value)) then
         call flush_(stdout)
         if (present(fac)) then
            call show_(stdout,"Using x point conversion factor  =",fac)
         end if
         call show_(stdout,"First data value                 =",self%data_point(1)*fac)
         call show_(stdout,"Last data value                  =",self%data_point(self%n_data)*fac)
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=2)
         call put_(stdout,"#",int_width=TRUE)
         call put_(stdout,"point")
         call put_(stdout,"value")
         call flush_(stdout)
         call dash_(stdout,int_fields=1,real_fields=2)
         do i = 1,self%n_data
            call put_(stdout,i)
            call put_(stdout,self%data_point(i)*fac)
            call put_(stdout,self%data_value(i))
            call flush_(stdout)
         end do
         call dash_(stdout,int_fields=1,real_fields=2)
      end if
      if (NOT self%interpolation_is_spline) return
      call flush_(stdout)
      call text_(stdout,"Spline y2:")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"Value")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=1)
      do i = 1,self%n_data
         call put_(stdout,i)
         call put_(stdout,self%y2(i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=1)
   end subroutine

   subroutine put_gnuplot_0(self,do_log10,to_angstrom)
   ! Put the list of interpolating data in a gnuplot file assuming it
   ! is even spaced
      type(INTERPOLATOR_TYPE), IN :: self
      BIN, optional, IN :: do_log10
      BIN, optional, IN :: to_angstrom
      BIN :: lg10,angstrom
      INT :: i
   ENSURE(allocated(self%data_point),"INTERPOLATOR:put_gnuplot_0 ... no data_point")
   ENSURE(allocated(self%data_value),"INTERPOLATOR:put_gnuplot_0 ... no data_value")
      lg10 = FALSE
      if (present(do_log10)) lg10 = do_log10
      angstrom = FALSE
      if (present(to_angstrom)) angstrom = to_angstrom
      if (NOT lg10) then
         if (angstrom) then
            call text_(stdout,"# points/Angstrom")
            do i = 1,self%n_data
               call put_(stdout,to_units_(self%data_point(i),"angstrom"))
               call put_(stdout,self%data_value(i))
               call flush_(stdout)
            end do
         else
            call text_(stdout,"#")
            do i = 1,self%n_data
               call put_(stdout,self%data_point(i))
               call put_(stdout,self%data_value(i))
               call flush_(stdout)
            end do
         end if
      else
         if (angstrom) then
            call text_(stdout,"# points/Angstrom, log10(values)")
            do i = 1,self%n_data
               call put_(stdout,to_units_(self%data_point(i),"angstrom"))
               call put_(stdout,log10(self%data_value(i)))
               call flush_(stdout)
            end do
         else
            call text_(stdout,"# log10(values)")
            do i = 1,self%n_data
               call put_(stdout,self%data_point(i))
               call put_(stdout,log10(self%data_value(i)))
               call flush_(stdout)
            end do
         end if
      end if
   end subroutine

   subroutine put_gnuplot_1(self,ref,do_log10,to_angstrom)
   ! Put the list of interpolating data in a gnuplot file assuming it
   ! is even spaced, self - ref.
      type(INTERPOLATOR_TYPE), IN :: self
      type(INTERPOLATOR_TYPE), IN :: ref
      BIN, optional, IN :: do_log10
      BIN, optional, IN :: to_angstrom
      BIN :: lg10,angstrom
      INT :: i
   ENSURE(allocated(self%data_point),"INTERPOLATOR:put_gnuplot_1 ... no data_point")
   ENSURE(allocated(self%data_value),"INTERPOLATOR:put_gnuplot_1 ... no data_value")
   ENSURE(allocated(ref%data_point),"INTERPOLATOR:put_gnuplot_1 ... no data_point")
   ENSURE(allocated(ref%data_value),"INTERPOLATOR:put_gnuplot_1 ... no data_value")
      lg10 = FALSE
      if (present(do_log10)) lg10 = do_log10
      angstrom = FALSE
      if (present(to_angstrom)) angstrom = to_angstrom
      if (NOT lg10) then
         if (angstrom) then
            call text_(stdout,"#")
            do i = 1,min(self%n_data,ref%n_data)
               call put_(stdout,to_units_(self%data_point(i),"angstrom"))
               call put_(stdout,(self%data_value(i)-ref%data_value(i)))
               call flush_(stdout)
            end do
         else
            call text_(stdout,"#")
            do i = 1,min(self%n_data,ref%n_data)
               call put_(stdout,self%data_point(i))
               call put_(stdout,(self%data_value(i)-ref%data_value(i)))
               call flush_(stdout)
            end do
         end if
      else
         if (angstrom) then
            call text_(stdout,"#")
            do i = 1,min(self%n_data,ref%n_data)
               call put_(stdout,to_units_(self%data_point(i),"angstrom"))
               call put_(stdout,log10(self%data_value(i)-ref%data_value(i)))
               call flush_(stdout)
            end do
         else
            call text_(stdout,"#")
            do i = 1,min(self%n_data,ref%n_data)
               call put_(stdout,self%data_point(i))
               call put_(stdout,log10(self%data_value(i)-ref%data_value(i)))
               call flush_(stdout)
            end do
         end if
      end if
   end subroutine

end module
