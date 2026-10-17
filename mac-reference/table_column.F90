!==================================================================
! TABLE_COLUMN:
!
! For setting up table columns ready to print
!
! Copyright (C) Dylan Jayatilaka, June 2011
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
module TABLE_COLUMN_MODULE

#  include "macros"

   include "table_column.use"

   implicit none

   include "table_column.int"

   type(TABLE_COLUMN_TYPE), allocatable, public :: std_table_column

contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self)
   ! Allocate an object and set defaults
      type(TABLE_COLUMN_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"TABLE_COLUMN:create ... error in allocating memory")
#else
      allocate(self)
#endif
      call set_defaults_(self)
   end subroutine

   subroutine create_std_table_column()
   ! Create an object
      call destroy_(std_table_column)
      call create_(std_table_column)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(TABLE_COLUMN_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"TABLE_COLUMN:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine clear_column(self)
   ! Destroy the pointer parts, only if created
      type(TABLE_COLUMN_TYPE), INOUT :: self
      call destroy_(self%b_values)  ! leaky
      call destroy_(self%i_values)
      call destroy_(self%r_values)
      call destroy_(self%e_values)
      call destroy_(self%s_values)
      call destroy_(self%iv_values)
      call destroy_(self%rv_values)
      call destroy_(self%sv_values)
   end subroutine


!  ============
!  Set routines
!  ============

   PURE subroutine set_defaults(self)
   ! Set up defaults
      type(TABLE_COLUMN_TYPE), INOUT :: self

      ! Set some defaults from stdout
      if (allocated(stdout)) then
         self%width = stdout%style%real_width
         self%real_precision = stdout%style%real_precision
      end if
   end subroutine

   PURE subroutine set_heading(self,val)
   ! Set the heading
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%heading = val
      call set_width_from_(self,self%heading)
   end subroutine

   PURE subroutine set_subheading(self,val)
   ! Set the subheading
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%subheading = val
      call set_width_from_(self,self%subheading)
   end subroutine

   PURE subroutine set_subhead(self,val)
   ! Set the subheading
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      call set_subheading_(self,val)
   end subroutine

   PURE subroutine set_sb3head(self,val)
   ! Set the sub3 heading
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%sb3heading = val
      call set_width_from_(self,self%sb3heading)
   end subroutine

   PURE subroutine set_real_precision(self,val)
   ! Set the real precision
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val

! ENSURE(val>0,"real_precision must be positive")
   ENSURE(val<self%width,"TABLE_COLUMN:set_real_precision ... real_precision must be less than column width")
      self%real_precision = max(val,0)
   end subroutine

   PURE subroutine set_real_fortran_fmt(self,val)
   ! Set the fortran format style string
   ! WARNING: Auto-width won't work, always set width explicity
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%real_fortran_fmt = val
   end subroutine

   PURE subroutine set_max_cp(self,val)
   ! Set the maximum closing parenthesis position, for imprecise real
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val<self%width,"TABLE_COLUMN:set_max_cp ... max_cp must be less than column width")
      self%max_cp = max(val,0)
   end subroutine

   PURE subroutine set_e_under_r(self,val)
   ! Set whether to print the error value under the real.
   ! In the case errors are not shown as parentheses.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%e_under_r = val
   end subroutine

   PURE subroutine set_two_digit_error(self,val)
   ! Set whether toalways use two-digit errors for more precision.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%two_digit_error = val
   end subroutine

   PURE subroutine set_column_spacing(self,val)
   ! Set the free space within each column width
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"TABLE_COLUMN:set_column_spacing ... column_spacing must be non-negative")
   ENSURE(NOT self%width_set,"TABLE_COLUMN:set_column_spacing ... set column_spacing before width or heading")
      self%column_spacing = val
   end subroutine

   PURE subroutine set_list_item_spacing(self,val)
   ! Set the free space between items in a list-item column
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"TABLE_COLUMN:set_list_item_spacing ... spacing must be non-negative")
   ENSURE(NOT self%width_set,"TABLE_COLUMN:set_list_item_spacing ... set spacing before width or heading")
      self%list_item_spacing = val
   end subroutine

   PURE subroutine set_spacer(self,val)
   ! Set the space character between each column
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%spacer = val
   end subroutine

   PURE subroutine set_center(self,val)
   ! Set whether to center a STR column
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%center = val
   end subroutine

   PURE subroutine set_left_justify(self,val)
   ! Set whether to left-justify a column
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%left_justify = val
   end subroutine

   PURE subroutine set_left(self,val)
   ! Set whether to left-justify a column
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%left_justify = val
   end subroutine

   PURE subroutine set_list_as_column(self,val)
   ! Set whether to put a list-of-lists as a column, one-per-line.
   ! NOTE: this can only be set when the list is the last column.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%list_as_column = val
   end subroutine

   PURE subroutine set_width_set(self,val)
   ! Set whether width was set.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%width_set = val
   end subroutine

   PURE subroutine set_width(self,val)
   ! Set the column width to be the maximum of what it was and "val"
   ! If headings are set, take into account those widths too.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
      if (self%width_set) then; self%width = max(self%width,val)
      else; self%width = val
      end if
      self%width_set = TRUE
   end subroutine

   PURE subroutine force_width(self,val)
   ! Force set the column width to be "val"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
      self%width = val
      self%width_set = TRUE
   end subroutine

   PURE subroutine set_auto_width(self,val)
   ! Set whether to use automatic widths
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      self%auto_width = val
   end subroutine

   PURE subroutine set_item_width(self,val)
   ! Set the list item width
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
      if (self%item_width_set) then; self%item_width = max(self%item_width,val)
      else; self%item_width = val
      end if
      self%item_width_set = TRUE
   end subroutine

! =========================
! Set column width (scalar)
! =========================

! Non-real ...

   PURE subroutine set_width_from_0(self,val)
   ! Set width from the width of scalar value "val"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN, IN :: val
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_str_length_(val,wid,self%column_spacing)
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_1(self,val)
   ! Set width from the width of scalar value "val"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT, IN :: val
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_str_length_(val,wid,self%column_spacing)
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_2(self,val)
   ! Set width from the width of scalar value "val"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: val
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_str_length_(val,wid,self%column_spacing)
      call set_width_(self,wid)
   end subroutine


! Real ...

   PURE subroutine set_width_from_3(self,val)
   ! Set width from the width of the REAL value "val".
      type(TABLE_COLUMN_TYPE), INOUT :: self
      REAL, IN :: val
      INT :: wid
      call get_str_length_maxdp_(val,wid,self%real_precision,self%column_spacing)
      call set_width_(self,wid)
   end subroutine


! =========================
! Set column width (arrays)
! =========================

   PURE subroutine set_width_automatically(self)
   ! Set width automatically from the column values
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: valtyp
      valtyp = value_type_to_output_(self)

      ! Cases
      select case (valtyp)
      case ("bin        "); call set_width_from_(self,self%b_values)
      case ("int        "); call set_width_from_(self,self%i_values)
      ! A value-with-error column must derive its layout from the (value,error)
      ! PAIRS, not from the errors alone: only the pair version works out the
      ! decimal places "mdp" and the closing-brace position "mcb", and sets
      ! .real_precision and .max_cp from them. The errors-only version sets the
      ! width and nothing else, leaving .real_precision at whatever it happened
      ! to hold -- and since .real_precision is handed to every cell as its
      ! "max_dp" (see put_value), the whole column's precision was effectively
      ! undefined. That is the longstanding "zero(error) problem": cells with an
      ! exactly-zero error take dp = max_dp, so they printed a stale precision
      ! that differed from the rest of the column and even between platforms
      ! (0.000000(0) on Linux vs 0.00(0) on macOS, with the requested
      ! real_precision being 4 on both).
      case ("real(error)"); call set_width_from_(self,self%r_values,self%e_values)
      case ("real       "); call set_width_from_(self,self%r_values)
      case ("real-over  "); call set_width_from_(self,self%r_values)  ! Handle outside
      case ("error-under"); call set_width_from_(self,self%e_values)  ! with coltype
      case ("str        "); call set_width_from_(self,self%s_values)
      end select  ! case
   end subroutine

! Arrays of non-real ...

   PURE subroutine set_width_from_4(self,v)
   ! Set width from the maximum width of values in array "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(BIN,:), IN :: v
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_max_str_length_(v,wid,self%column_spacing)
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_5(self,v)
   ! Set width from the maximum width of values in array "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(INT,:), IN :: v
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_max_str_length_(v,wid,self%column_spacing)
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_6(self,v)
   ! Set width from the maximum width of values in array "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: v
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_max_str_length_(v,wid,self%column_spacing)
      call set_width_(self,wid)
   end subroutine

! Arrays of real ...

   PURE subroutine set_width_from_7(self,v)
   ! Set width from the maximum width of REALs in array "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_max_str_length_(v,wid,self%real_precision,self%column_spacing)
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_8(self,v)
   ! Set width from the maximum width of REALs in array "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: v
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      call get_max_str_length_(v,wid,self%real_precision,self%column_spacing)
      call set_width_(self,wid)
   end subroutine


! Real with errors ...

   PURE subroutine set_width_from_9(self,r,e)
   ! Set width from the width of value "r" and its error "e".
   ! Also set the precision.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      REAL, IN :: r,e
      INT :: wid,dp,de,le,pad
      STR(len=STR_SIZE) :: valtyp
      valtyp = value_type_to_output_(self)
      if (valtyp=="real-over") then
         call set_defaults_(self)
      else

         ! Get width & precision
         call get_str_length_align_(r,e,wid,dp,de,le,pad,self%real_precision)

         ! Set width & precision
         call set_width_(self,wid+self%column_spacing)
         call set_real_precision_(self,dp)
      end if
   end subroutine


! Arrays of real with errors ...

   PURE subroutine set_width_from_10(self,r,e)
   ! Set width from the maximum  width of value "r" and its error "e".
   ! Also set the precision.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(REAL,:), IN :: r,e
      INT :: msl,mdp,mcb,mcc
      STR(len=STR_SIZE) :: valtyp
      valtyp = value_type_to_output_(self)
      if (valtyp=="real-over") then
         call set_defaults_(self)
      else

         ! Get max width & precision & closing bracket
         call get_max_str_length_(r,msl,mdp,mcc,e,self%real_precision)
         call get_max_str_length_(r,msl,mdp,mcb,e,self%real_precision,max_cb=mcc,two_digit_error=self%two_digit_error)

         ! Set 
         call set_width_(self,msl+self%column_spacing)
         call set_real_precision_(self,mdp)
         call set_max_cp_(self,mcb)
      end if
   end subroutine

! List of lists VEC{EVEC{TYPE}} of non-real ...  

   PURE subroutine set_width_from_11(self,v)
   ! Set width from the width of list-of-lists value(s) in "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: v
      INT :: vid
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      vid = item_str_length_(v)
      wid = vid
      wid = wid+self%list_item_spacing
      call set_item_width_(self,wid)
      wid = wid*min(self%n_list_fields,max_element_dim_(v))
      wid = wid+self%column_spacing
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_12(self,v)
   ! Set width from the width of list-of-lists value(s) in "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(type(EVEC_REAL_TYPE),:), IN :: v
      INT :: vid
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      vid = item_str_length_(v)
      wid = vid
      wid = wid+self%list_item_spacing
      call set_item_width_(self,wid)
      wid = wid*min(self%n_list_fields,max_element_dim_(v))
      wid = wid+self%column_spacing
      call set_width_(self,wid)
   end subroutine

   PURE subroutine set_width_from_13(self,v)
   ! Set width from the width of list-of-lists value(s) in "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(type(EVEC_STR_TYPE),:), IN :: v
      INT :: vid
      INT :: wid

      ! The following code is inherited from TABLE_COLUMN
      vid = item_str_length_(v)
      wid = vid
      wid = wid+self%list_item_spacing
      call set_item_width_(self,wid)
      wid = wid*min(self%n_list_fields,max_element_dim_(v))
      wid = wid+self%column_spacing
      call set_width_(self,wid)
   end subroutine

! =================
! Set column values
! =================

   subroutine set_values_0(self,v,auto)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(BIN,:), IN :: v
      BIN, optional, IN :: auto
      BIN :: flex
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_0 ... column has already been set!")

      ! The following code is inherited from TABLE_COLUMN
      self%b_values = v

      ! Auto adjust width?
      flex = TRUE
      if (present(auto)) flex = auto
      if (flex) call set_width_from_(self,v)
   end subroutine

   subroutine set_values_1(self,v,auto)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(INT,:), IN :: v
      BIN, optional, IN :: auto
      BIN :: flex
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_1 ... column has already been set!")

      ! The following code is inherited from TABLE_COLUMN
      self%i_values = v

      ! Auto adjust width?
      flex = TRUE
      if (present(auto)) flex = auto
      if (flex) call set_width_from_(self,v)
   end subroutine

   subroutine set_values_2(self,v,auto)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: auto
      BIN :: flex
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_2 ... column has already been set!")

      ! The following code is inherited from TABLE_COLUMN
      self%r_values = v

      ! Auto adjust width?
      flex = TRUE
      if (present(auto)) flex = auto
      if (flex) call set_width_from_(self,v)
   end subroutine

   subroutine set_values_3(self,v,auto)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), IN :: v
      BIN, optional, IN :: auto
      BIN :: flex
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_3 ... column has already been set!")

      ! The following code is inherited from TABLE_COLUMN
      self%iv_values = v

      ! Auto adjust width?
      flex = TRUE
      if (present(auto)) flex = auto
      if (flex) call set_width_from_(self,v)
   end subroutine

   subroutine set_values_4(self,v,auto)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(type(EVEC_REAL_TYPE),:), IN :: v
      BIN, optional, IN :: auto
      BIN :: flex
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_4 ... column has already been set!")

      ! The following code is inherited from TABLE_COLUMN
      self%rv_values = v

      ! Auto adjust width?
      flex = TRUE
      if (present(auto)) flex = auto
      if (flex) call set_width_from_(self,v)
   end subroutine

   subroutine set_values_5(self,v,auto)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(type(EVEC_STR_TYPE),:), IN :: v
      BIN, optional, IN :: auto
      BIN :: flex
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_5 ... column has already been set!")

      ! The following code is inherited from TABLE_COLUMN
      self%sv_values = v

      ! Auto adjust width?
      flex = TRUE
      if (present(auto)) flex = auto
      if (flex) call set_width_from_(self,v)
   end subroutine

   subroutine set_values_6(self,v)
   ! Set the column values "v"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(STR(len=STR_SIZE),:), IN :: v
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_6 ... column has already been set!")
      self%s_values = v
      call set_width_from_(self,v)
   end subroutine

   subroutine set_values_and_errors(self,r,e)
   ! Set the column values "r" and errors "e"
      type(TABLE_COLUMN_TYPE), INOUT :: self
      VEC(REAL,:), IN :: r,e
   DIE_IF(values_set_(self),"TABLE_COLUMN:set_values_and_errors ... column has already been set!")
      self%r_values = r
      self%e_values = e

      ! An esd is a square root: it can be neither NaN nor negative. Both occur
      ! in practice, out of the least-squares variance-covariance matrix (see
      ! TASKS_AND_HISTORY.md). REAL:get_dp_de_le now renders them in a well-defined
      ! way, but silently printing a NaN esd as "(0)" would hide a real defect in
      ! the numbers, so say so. Warned here rather than in get_dp_de_le because
      ! that routine is PURE and cannot do I/O.
      WARN_IF(count(e/=e)>0,"TABLE_COLUMN:set_values_and_errors ... NaN esd in this table column -- the variance-covariance matrix is suspect; the printed error is NOT meaningful")
      WARN_IF(count(e<ZERO)>0,"TABLE_COLUMN:set_values_and_errors ... negative esd in this table column -- the variance-covariance matrix is suspect")
      call set_width_from_(self,r,e)
   end subroutine


!  ================
!  Inquiry routines
!  ================

   pure function values_set(self) result (res)
   ! Return true if any column has been set
      type(TABLE_COLUMN_TYPE), IN :: self
      BIN :: res
      res = allocated(self%b_values) OR allocated(self%i_values) OR allocated(self%r_values) OR allocated(self%s_values) OR allocated(self%iv_values) OR allocated(self%rv_values) OR allocated(self%sv_values)
   end function


! This is very important

   pure function value_type_to_output(self,coltype) result (res)
   ! Return a string "res" describing kind of value to be printed.
   ! Normally the value printed is related to "X" if .X_values is set.
   ! If "coltype" is present (and equal to "error") then "res" is:
   !  . "error-under" ... for a error value like 0.00693 if there
   ! If "coltype" is NOT present then "res" is:
   !  . "real(error)" ... for a value printed like 1.234(7)
   !  . "real-over"   ... for a real value like  1.23456 with error 
   !                      to be printed below as 0.00693
      type(TABLE_COLUMN_TYPE), IN :: self
      STR(len=*), optional, IN :: coltype
      STR(len=STR_SIZE) :: res
      res = "none"
      if (present(coltype)) then

         ! ENSURE(coltype=="error","coltype is not 'error'")
         if (may_be_e_under_column_(self)) then; res = "error-under"
         else; res = "blank"
         end if
      else if (allocated(self%b_values)) then; res = "bin"
      else if (allocated(self%i_values)) then; res = "int"
      else if (may_be_an_re_column_(self)) then; res = "real(error)"
      else if (may_be_r_over_column_(self)) then; res = "real-over"
      else if (allocated(self%r_values)) then; res = "real"
      else if (allocated(self%s_values)) then; res = "str"
      else if (allocated(self%iv_values)) then; res = "int-list"
      else if (allocated(self%rv_values)) then; res = "real-list"
      else if (allocated(self%sv_values)) then; res = "str-list"
      end if
   end function

   elemental function may_be_e_under_column(self) result (res)
   ! Return TRUE if self has e_values allocated, and 
   !    e_under_r is TRUE.
   ! It really is an "error-under" column if .value_type_to_output 
   ! is called *with* the "coltype" option.
      type(TABLE_COLUMN_TYPE), IN :: self
      BIN :: res
      res = allocated(self%e_values) AND self%e_under_r
   end function

   pure function may_be_r_over_column(self) result (res)
   ! Return TRUE if self has r_values, and 
   !    e_under_r is TRUE. 
   ! It really is a "real-over" column if .value_type_to_output 
   ! is called *without* the "coltype" option.
      type(TABLE_COLUMN_TYPE), IN :: self
      BIN :: res
      res = allocated(self%r_values) AND self%e_under_r
   end function

   pure function may_be_an_re_column(self) result (res)
   ! Return TRUE if self has r_values, e_values, and 
   !    e_under_r is FALSE. 
   ! It definitely is an "real(error)" column if .value_type_to_output 
   ! is called *without* the "coltype".
      type(TABLE_COLUMN_TYPE), IN :: self
      BIN :: res
      res = allocated(self%r_values) AND allocated(self%e_values) AND NOT self%e_under_r
   end function

   pure function n_rows(self) result (res)
   ! Returns the number of rows.
   ! NOTE: if the error column is printed, this may not correspond
   ! to the actual number of outputted rows, because the error may be
   ! wriiten underneath the real value.
      type(TABLE_COLUMN_TYPE), IN :: self
      INT :: res

   ! ENSURE(.values_set,"column not set")
      res = 0
      if (allocated(self%b_values)) then; res = size(self%b_values)
      else if (allocated(self%i_values)) then; res = size(self%i_values)
      else if (allocated(self%r_values)) then; res = size(self%r_values)
      else if (allocated(self%s_values)) then; res = size(self%s_values)
      else if (allocated(self%iv_values)) then; res = size(self%iv_values)
      else if (allocated(self%rv_values)) then; res = size(self%rv_values)
      else if (allocated(self%sv_values)) then; res = size(self%sv_values)
      end if
   end function


!  ============
!  I/O Routines
!  ============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","TABLE_COLUMN:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"TABLE_COLUMN:read_keywords ... unexpected end of file while reading list keywords")
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
      type(TABLE_COLUMN_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}                 ")  ! exit case
      case ("column_spacing=   "); call read_column_spacing_(self)
      case ("error_under_value="); call read_su_under_value_(self)
      case ("list_item_spacing="); call read_list_item_spacing_(self)
      case ("real_precision=   "); call read_real_precision_(self)
      case ("su_under_value=   "); call read_su_under_value_(self)
      case ("two_digit_error=  "); call read_two_digit_error_(self)
      case default; allocate(tonto%known_keywords(7))
      tonto%known_keywords(1) = "}                 "
      tonto%known_keywords(2) = "column_spacing=   "
      tonto%known_keywords(3) = "error_under_value="
      tonto%known_keywords(4) = "list_item_spacing="
      tonto%known_keywords(5) = "real_precision=   "
      tonto%known_keywords(6) = "su_under_value=   "
      tonto%known_keywords(7) = "two_digit_error=  "
      call unknown_(tonto,word,"TABLE_COLUMN:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_column_spacing(self)
   ! Read space to apply between columns
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_column_spacing_(self,val)
   end subroutine

   subroutine read_list_item_spacing(self)
   ! Read spacing to apply between list-of-items column
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_list_item_spacing_(self,val)
   end subroutine

   subroutine read_real_precision(self)
   ! Read the number of decimal places for rea number output
      type(TABLE_COLUMN_TYPE), INOUT :: self
      INT :: val
      call read_(stdin,val)
      call set_real_precision_(self,val)
   end subroutine

   subroutine read_su_under_value(self)
   ! Read whether to priunt su's under tha colum value.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_e_under_r_(self,val)
   end subroutine

   subroutine read_two_digit_error(self)
   ! Read whether to always use two-digit errors for imprecise real
   ! output.
      type(TABLE_COLUMN_TYPE), INOUT :: self
      BIN :: val
      call read_(stdin,val)
      call set_two_digit_error_(self,val)
   end subroutine


!  ============
!  Put routines
!  ============

   subroutine put_heading(self)
   ! Put the column heading out
      type(TABLE_COLUMN_TYPE), IN :: self
      if (self%spacer/=" ") call put_(stdout,trim(self%spacer))
      call put_(stdout,self%heading,center=self%center,left=self%left_justify,width=self%width)
   end subroutine

   subroutine put_subheading(self)
   ! Put the column subheading out
      type(TABLE_COLUMN_TYPE), IN :: self
      if (self%spacer/=" ") call put_(stdout,trim(self%spacer))
      call put_(stdout,self%subheading,center=self%center,left=self%left_justify,width=self%width)
   end subroutine

   subroutine put_sb3heading(self)
   ! Put the column subheading out
      type(TABLE_COLUMN_TYPE), IN :: self
      if (self%spacer/=" ") call put_(stdout,trim(self%spacer))
      call put_(stdout,self%sb3heading,center=self%center,left=self%left_justify,width=self%width)
   end subroutine


! This is the main procedure

   subroutine put_value(self,i,coltype)
   ! Put out the value "i" in the column; it is normally of type 
   ! "X" if .X_values is allocated, except i "coltype" is present 
   ! and equal to "error"
      type(TABLE_COLUMN_TYPE), IN :: self
      INT, IN :: i
      STR(len=*), optional, IN :: coltype
      INT :: margin
      INT :: fields
      INT :: n_block
      INT :: dim,s,f
      INT :: w,p,b,j
      BIN :: l,c
      STR(len=STR_SIZE) :: valtyp
      STR(len=STR_SIZE) :: m
   ENSURE(values_set_(self),"TABLE_COLUMN:put_value ... column not set")
   ENSURE(is_in_range_(i,[1,n_rows_(self)]),"TABLE_COLUMN:put_value ... column index i out of range")
   ENSURE(present(coltype) EQV coltype=="error","TABLE_COLUMN:put_value ... if used, coltype must be 'error'")

      ! Widths & centering
      w = self%width
      p = self%real_precision
      m = self%real_fortran_fmt
      l = self%left_justify
      c = self%center

      ! Put the spacer
      if (self%spacer/=" ") call put_(stdout,trim(self%spacer))

      ! Value type to be printed
      valtyp = value_type_to_output_(self,coltype)

      ! Cases
      select case (valtyp)
      case ("blank      "); call put_(stdout," ",width=w,blank=TRUE)  ! Output a blank
      case ("bin        "); call put_(stdout,self%b_values(i),left=l,center=c,width=w)
      case ("int        "); call put_(stdout,self%i_values(i),left=l,center=c,width=w)
      case ("real(error)")  ! Default
         call put_(stdout,self%r_values(i),self%e_values(i),width=w,precision=p,max_cb=self%max_cp,d2_err=self%two_digit_error)
      case ("real       ")
         call put_(stdout,self%r_values(i),left=l,center=c,width=w,precision=p,fmt=m)
      case ("real-over  "); call put_(stdout,self%r_values(i),left=l,center=c)  ! Handle outside
      case ("error-under"); call put_(stdout,self%e_values(i),left=l,center=c)  !   with coltype
      case ("str        "); call put_(stdout,self%s_values(i),left=l,center=c,width=w)
      case ("int-list   ")  ! VEC{INT}
         if (self%list_as_column) then
            call put_(stdout," ",width=self%column_spacing)
            margin = stdout%buffer%item_end
            do j = 1,size(self%iv_values(i)%element)
               if (j>1) then
                  call flush_(stdout)
                  call tab_(stdout,width=margin)
               end if
               call put_(stdout,self%iv_values(i)%element(j),left=l,center=c,width=self%item_width)
            end do
         else
            dim = size(self%iv_values(i)%element)
            fields = self%n_list_fields
            n_block = int((dim-0.1)/fields)+1
            call put_(stdout," ",width=self%column_spacing)
            margin = stdout%buffer%item_end  ! save margin
            do b = 1,n_block
               s = 1+fields*(b-1)
               f = min(s+fields-1,dim)
               if (b>1) then
                  call flush_(stdout)
                  call tab_(stdout,width=margin)
               end if
               do j = s,f
                  call put_(stdout,self%iv_values(i)%element(j),left=l,center=c,width=self%item_width)
               end do
            end do
         end if

      case ("real-list  ")  ! VEC{REAL}
         if (self%list_as_column) then
            call put_(stdout," ",width=self%column_spacing)
            margin = stdout%buffer%item_end
            do j = 1,size(self%rv_values(i)%element)
               if (j>1) then
                  call flush_(stdout)
                  call tab_(stdout,width=margin)
               end if
               call put_(stdout,self%rv_values(i)%element(j),left=l,center=c,width=self%item_width)
            end do
         else
            dim = size(self%rv_values(i)%element)
            fields = self%n_list_fields
            n_block = int((dim-0.1)/fields)+1
            call put_(stdout," ",width=self%column_spacing)
            margin = stdout%buffer%item_end  ! save margin
            do b = 1,n_block
               s = 1+fields*(b-1)
               f = min(s+fields-1,dim)
               if (b>1) then
                  call flush_(stdout)
                  call tab_(stdout,width=margin)
               end if
               do j = s,f
                  call put_(stdout,self%rv_values(i)%element(j),left=l,center=c,width=self%item_width)
               end do
            end do
         end if

      case ("str-list")  ! VEC{STR}
         if (self%list_as_column) then
            call put_(stdout," ",width=self%column_spacing)
            margin = stdout%buffer%item_end
            do j = 1,size(self%sv_values(i)%element)
               if (self%sv_values(i)%element(j)==" ") cycle
               if (j>1) then
                  call flush_(stdout)
                  call tab_(stdout,width=margin)
               end if
               call put_(stdout,self%sv_values(i)%element(j),left=l,center=c,width=self%item_width)
            end do
         else
            dim = size(self%sv_values(i)%element)
            fields = self%n_list_fields
            n_block = int((dim-0.1)/fields)+1
            call put_(stdout," ",width=self%column_spacing)
            margin = stdout%buffer%item_end  ! save margin
            do b = 1,n_block
               s = 1+fields*(b-1)
               f = min(s+fields-1,dim)
               if (b>1) then
                  call flush_(stdout)
                  call tab_(stdout,width=margin)
               end if
               do j = s,f
                  call put_(stdout,self%sv_values(i)%element(j),left=l,center=c,width=self%item_width)
               end do
            end do
         end if
      end select  ! case
   end subroutine

end module
