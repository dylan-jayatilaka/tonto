!===================================================================
!
! VEC{TABLE_COLUMN}: A list of TABLE_COLUMN
!
! Copyright (C) Dylan Jayatilaka, 2011
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
module VEC_TABLE_COLUMN_MODULE

#  include "macros"

   include "vec{table_column}.use"

   implicit none

   include "vec{table_column}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create(self,dim)
   ! Allocate vector and set defaults
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable, OUT :: self
      INT, IN :: dim

#ifdef USE_PRECONDITIONS
      INT :: status
   ENSURE(dim>=0,"VEC{TABLE_COLUMN}:create ... array dimension is -ve, dim = "//trim(to_str_(dim)))
#endif

      ! The following code is inherited from VEC{OBJECT}
      call destroy_(self)

#ifdef USE_PRECONDITIONS
      allocate(self(dim),stat=status)
      ENSURE(status==0,"VEC{TABLE_COLUMN}:create ... error in allocating memory")
      if (status/=0) stop
#else
      allocate(self(dim))
#endif
      call set_defaults_(self)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate the vector
      VEC(type(TABLE_COLUMN_TYPE),:), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from VEC{OBJECT}
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"VEC{TABLE_COLUMN}:destroy ... error in deallocating memory")
      if (status/=0) stop
#else
      deallocate(self)
#endif
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self)
   ! Set default values
      VEC(type(TABLE_COLUMN_TYPE),:), OUT :: self
      INT :: n

      ! The following code is inherited from VEC{OBJECT}
      do n = 1,size(self)
         call set_defaults_(self(n))
      end do
   end subroutine

   PURE subroutine clear_columns(self)
   ! Clear all the columns
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      INT :: i
      do i = 1,size(self)
         call clear_column_(self(i))
      end do
   end subroutine

   PURE subroutine set_column_spacing(self,val)
   ! Set the free space within each column width
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      INT, IN :: val
      INT :: i
      do i = 1,size(self)
         call set_column_spacing_(self(i),val)
      end do
   end subroutine

   PURE subroutine set_auto_width(self,val)
   ! Set whether to use automatic widths
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      BIN, IN :: val
      INT :: i
      do i = 1,size(self)
         call set_auto_width_(self(i),val)
      end do
   end subroutine

   subroutine update(self)
   ! Sanity checks, and ensure object is consistent.
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      BIN :: any_e_cols
      any_e_cols = any(may_be_e_under_column_(self))

      ! If there are no "error-under" columns at all
      ! then there is no need to have e_under_r set
      ! even if the user requested it ...
      if (NOT any_e_cols) then
         self(:)%e_under_r = FALSE
      end if
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   function no_of_rows(self) result (res)
   ! The number of rows in the list of columns. They must all be the
   ! same to avoid an error.
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      INT :: res

      ! Check no. of rows
      call check_no_of_rows_(self)

      ! OK
      res = n_rows_(self(1))
   end function

   subroutine check_no_of_rows(self)
   ! Check that the number of rows are all the same
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      INT :: i,n1

      ! No. of rows in col 1
      n1 = n_rows_(self(1))

      ! Check no. of rows
      do i = 2,size(self)
         if (n_rows_(self(i))==n1) cycle
         DIE("VEC{TABLE_COLUMN}:check_no_of_rows ... mismatching rows, column "//trim(to_str_(i)))
      end do
   end subroutine

   function dash_width(self,label_rows) result (res)
   ! The length of the dashed line
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN, optional, IN :: label_rows
      INT :: res
      BIN :: row_labels
      INT :: lr,ls
      type(TABLE_COLUMN_TYPE) :: stdcol
      row_labels = FALSE
      if (present(label_rows)) row_labels = label_rows

      ! Width of first column
      lr = 0
      if (row_labels) then
         lr = no_of_rows_(self)
         lr = str_length_(lr)+stdcol%column_spacing
      end if

      ! Width of spacers
      ls = sum(len_trim(self%spacer))

      ! Sum column widths & extras
      res = sum(self%width)+lr+ls
   end function

   PURE function use_e_under_r(self) result (res)
   ! Whether to have a line because of an error (su) under a real.
   ! This must e done even if only *one* column has an error.
   ! If there is no error a blank is output.
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN :: res
      res = any(self(:)%e_under_r)
   end function

   pure function has_list_column(self) result (res)
   ! Return if there is a list column.
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN :: res
      INT :: c
      STR(len=STR_SIZE) :: typ
      res = FALSE
      do c = 1,size(self)
         typ = value_type_to_output_(self(c))
         res = includes_(typ,"list")
         if (res) exit
      end do
   end function

   pure function has_list_column_last(self) result (res)
   ! Return if TRUE there is no list column; or if there is one,
   ! then there is only one of them, and it is the last column.
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN :: res
      INT :: c
      STR(len=STR_SIZE) :: typ
      BIN :: has
      res = TRUE
      do c = 1,size(self)
         typ = value_type_to_output_(self(c))
         has = includes_(typ,"list")
         if (has) then
            if (c<size(self)) then
               res = FALSE
               return
            end if
         end if
      end do
   end function


!  =======
!  Methods
!  =======

   subroutine put(self,label_rows,heading_line)
   ! Put out the table of the columns. "label_rows" tells whether
   ! you want the first column to be the row index (default TRUE)
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      BIN, optional, IN :: label_rows
      BIN, optional, IN :: heading_line
      BIN :: row_labels
      row_labels = TRUE
      if (present(label_rows)) row_labels = label_rows
      call put_headings_(self,row_labels,heading_line)
      call put_body_(self,row_labels)
      call put_footer_(self,row_labels)
   end subroutine

   subroutine put_headings(self,label_rows,heading_line,no_flush)
   ! Put out the column headings/subheadings
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN, optional, IN :: label_rows
      BIN, optional, IN :: heading_line
      BIN, optional, IN :: no_flush
      BIN :: row_labels,line,flush
      INT :: c,lr
      type(TABLE_COLUMN_TYPE) :: stdcol
      row_labels = FALSE
      if (present(label_rows)) row_labels = label_rows
      flush = TRUE
      if (present(no_flush)) flush = NOT no_flush
      line = TRUE
      if (present(heading_line)) line = heading_line

      ! Horizontal line
      if (line) call put_dash_(self,label_rows,"_")

      ! Flush
      call flush_(stdout)

      ! Row label heading as first column?
      lr = 0
      if (row_labels) then
         lr = no_of_rows_(self)
         lr = str_length_(lr)+stdcol%column_spacing
         call put_(stdout,"#",width=lr)
      end if

      ! Top heading
      do c = 1,size(self)
         call put_heading_(self(c))
      end do
      call flush_(stdout)

      ! Any subheadings?
      if (any(self%subheading/=" ")) then

         ! Blank first row subheading
         if (row_labels) call put_(stdout," ",width=lr)

         ! Do subheadings
         do c = 1,size(self)
            call put_subheading_(self(c))
         end do
         call flush_(stdout)
      end if

      ! Any sb3headings?
      if (any(self%sb3heading/=" ")) then

         ! Blank first row subheading
         if (row_labels) call put_(stdout," ",width=lr)

         ! Do subheadings
         do c = 1,size(self)
            call put_sb3heading_(self(c))
         end do
         call flush_(stdout)
      end if

      ! Horizontal line
      if (line) call put_dash_(self,label_rows,"_")

      ! Flush
      if (flush) call flush_(stdout)
   end subroutine

   subroutine put_body_0(self,label_rows)
   ! Put out the body of the table (minus header & footer)
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      BIN, optional, IN :: label_rows
      INT :: n_rows,lr,r,c
      BIN :: row_labels
      type(TABLE_COLUMN_TYPE) :: stdcol
   ENSURE(has_list_column_last_(self),"VEC{TABLE_COLUMN}:put_body_0 ... the list column must occur last")

      ! Finalize
      call update_(self)

      ! Row labels?
      row_labels = FALSE
      if (present(label_rows)) row_labels = label_rows

      ! No. of rows
      n_rows = no_of_rows_(self)

      ! Row label width (column 1)
      lr = 0
      if (row_labels) then
         lr = str_length_(n_rows)+stdcol%column_spacing
      end if

      ! The table body
      do r = 1,n_rows
         if (row_labels) call put_(stdout,r,width=lr)
         do c = 1,size(self)
            call put_value_(self(c),r)
         end do
         call flush_(stdout)

         ! Put error under the real?
         ! Must have if even one of them is e_under_r
         if (NOT use_e_under_r_(self)) cycle
         ENSURE(NOT has_list_column_(self),"VEC{TABLE_COLUMN}:put_body_0 ... can't use list-column and e-under-r option")

         ! Blank for label
         if (row_labels) call put_(stdout,r,width=lr,blank=TRUE)

         ! The errors and/or blanks
         do c = 1,size(self)
            call put_value_(self(c),r,coltype="error")
         end do
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_body_1(self,tabl,label_rows)
   ! Put out the body of the table (minus header & footer)
   ! and a dove-tailed second "table" ...
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: tabl
      BIN, optional, IN :: label_rows
      INT :: n_rows,lr,r,c
      BIN :: row_labels
      type(TABLE_COLUMN_TYPE) :: stdcol
   ENSURE(size(tabl)<=size(self),"VEC{TABLE_COLUMN}:put_body_1 ... 2nd table has more cols")

      ! Finalize
      call update_(self)

      ! Row labels?
      row_labels = FALSE
      if (present(label_rows)) row_labels = label_rows

      ! No. of rows
      n_rows = no_of_rows_(self)

      ! Row label width (column 1)
      lr = 0
      if (row_labels) then
         lr = str_length_(n_rows)+stdcol%column_spacing
      end if

      ! The table body
      do r = 1,n_rows
         if (row_labels) call put_(stdout,r,width=lr)
         do c = 1,size(self)
            call put_value_(self(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl)
            call put_value_(tabl(c),r)
         end do
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_body_2(self,tabl1,tabl2,tabl3)
   ! Put out the body of the table (minus header & footer)
   ! and a dove-tailed second "table" ...
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: tabl1,tabl2,tabl3
      INT :: n_rows,lr,r,c
      type(TABLE_COLUMN_TYPE) :: stdcol
   ENSURE(size(tabl1)<=size(self),"VEC{TABLE_COLUMN}:put_body_2 ... 1st table has more cols")
   ENSURE(size(tabl2)<=size(self),"VEC{TABLE_COLUMN}:put_body_2 ... 2nd table has more cols")
   ENSURE(size(tabl3)<=size(self),"VEC{TABLE_COLUMN}:put_body_2 ... 3rd table has more cols")

      ! Finalize
      call update_(self)

      ! No. of rows
      n_rows = no_of_rows_(self)

      ! Row label width (column 1)
      lr = str_length_(n_rows)+stdcol%column_spacing
      do r = 1,n_rows
         do c = 1,size(self)
            call put_value_(self(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl2)
            call put_value_(tabl2(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl3)
            call put_value_(tabl3(c),r)
         end do
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_body_3(self,tabl1,tabl2,tabl3,tabl4,tabl5,tabl6)
   ! Put out the body of the table (minus header & footer)
   ! and a dove-tailed second "table" ...
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: self
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: tabl1,tabl2,tabl3,tabl4
      VEC(type(TABLE_COLUMN_TYPE),:), INOUT :: tabl5,tabl6
      INT :: n_rows,lr,r,c
      type(TABLE_COLUMN_TYPE) :: stdcol
   ENSURE(size(tabl1)<=size(self),"VEC{TABLE_COLUMN}:put_body_3 ... 1st table has more cols")
   ENSURE(size(tabl2)<=size(self),"VEC{TABLE_COLUMN}:put_body_3 ... 2nd table has more cols")
   ENSURE(size(tabl3)<=size(self),"VEC{TABLE_COLUMN}:put_body_3 ... 3rd table has more cols")
   ENSURE(size(tabl4)<=size(self),"VEC{TABLE_COLUMN}:put_body_3 ... 4th table has more cols")
   ENSURE(size(tabl5)<=size(self),"VEC{TABLE_COLUMN}:put_body_3 ... 5th table has more cols")
   ENSURE(size(tabl6)<=size(self),"VEC{TABLE_COLUMN}:put_body_3 ... 6th table has more cols")

      ! Finalize
      call update_(self)

      ! No. of rows
      n_rows = no_of_rows_(self)

      ! Row label width (column 1)
      lr = str_length_(n_rows)+stdcol%column_spacing
      do r = 1,n_rows
         do c = 1,size(self)
            call put_value_(self(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl2)
            call put_value_(tabl2(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl3)
            call put_value_(tabl3(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl4)
            call put_value_(tabl4(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl5)
            call put_value_(tabl5(c),r)
         end do
         call flush_(stdout)
         do c = 1,size(tabl6)
            call put_value_(tabl6(c),r)
         end do
         call flush_(stdout)
      end do
   end subroutine

   subroutine put_last_row(self)
   ! Put out the last row of the table (minus header & footer)
   ! Useful for printing tables one row at a time.
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      INT :: n_rows,c

      ! Last row
      n_rows = no_of_rows_(self)

      ! The table body
      do c = 1,size(self)
         call put_value_(self(c),n_rows)
      end do
      call flush_(stdout)
   end subroutine

   subroutine put_footer(self,label_rows,flush)
   ! Put out the table footer
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN, optional, IN :: label_rows
      BIN, optional, IN :: flush
      BIN :: do_flush
      call put_dash_(self,label_rows,"_")
      do_flush = FALSE
      if (present(flush)) do_flush = flush

      ! Flush
      if (do_flush) call flush_(stdout)
   end subroutine

   subroutine put_dash(self,label_rows,symbol)
   ! Put out a dashed line of the correct length
      VEC(type(TABLE_COLUMN_TYPE),:), IN :: self
      BIN, optional, IN :: label_rows
      STR(len=1), optional, IN :: symbol
      INT :: w
      w = dash_width_(self,label_rows)
      call dash_(stdout,symbol,width=w)
   end subroutine

end module
