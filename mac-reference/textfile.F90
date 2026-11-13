!=====================================================================
!
! TEXTFILE
!
! This is for text-based files. It uses a line-oriented, sequential
! advancing approach for input and output. It can handle input from
! standard input "stdin", and output to standard output "stdout",
! which are one-off objects stored in this module.
!
! You cannot open a TEXTFILE simultaneously for input and output.
!
! Line analysis and output is done using a BUFFER object. Note that
! the input buffer is limited to string of size BSTR_SIZE, set in the
! "macros" file.
!
! A line count is maintained to allow back-tracking to previous lines,
! and particular items on each line. This is useful for input.
!
! For output, items can be put in columns of a specified width, with
! double precision numbers having a specified precision, and a
! specified "style" specified by fortran conventions. This is useful
! for dynamic tables, which the user can change at run time. Rewind
! and backtracking are not allowed for output files.
!
! It is possible to redirect the input or output to a new file
! using the "redirect" command. The previous file can be recovered
! using the "revert" command; or it will revert back automatically to
! the previous file if the redirected file ends. It is also possible
! to redirect input to an internal file. This is useful for processing
! a quoted list of text as if it were an embedded file. Output to an
! internal file is not allowed.
!
! The system information in "tonto" is updated whenever a I/O operation
! or a buffer operation is performed.
!
! Copyright (C) Dylan Jayatilaka, 1999
! Copyright (C) Dylan Jayatilaka, 2024
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
!======================================================================
module TEXTFILE_MODULE

#  include "macros"

   include "textfile.use"

   implicit none

   include "textfile.int"


   ! Std in, out, error files
   type(TEXTFILE_TYPE), allocatable, public :: stdin
   type(TEXTFILE_TYPE), allocatable, public :: stdout
   type(TEXTFILE_TYPE), allocatable, public :: std_err

contains

!  ====================
!  Allocation std files
!  ====================

   subroutine create_stdin()
   ! Create a the standard input file object, if needed.

#ifdef USE_PRECONDITIONS
      INT :: status
#endif
      if (NOT allocated(stdin)) then

         ! Allocate
#ifdef USE_PRECONDITIONS
         allocate(stdin,stat=status)
         ENSURE(status==0,"TEXTFILE:create_stdin ... error in allocating stdin")
#else
         allocate(stdin)
#endif

         ! Reset some defaults
         stdin%name = "stdin"
         stdin%action = "read"
         stdin%unit = tonto%stdin_unit
      end if
   end subroutine

   subroutine create_stdout()
   ! Create a standard output file object.

#ifdef USE_PRECONDITIONS
      INT :: status
#endif
      if (NOT allocated(stdout)) then

         ! Allocate
#ifdef USE_PRECONDITIONS
         allocate(stdout,stat=status)
         ENSURE(status==0,"TEXTFILE:create_stdout ... error in allocating stdout")
#else
         allocate(stdout)
#endif

         ! Reset some defaults
         stdout%name = "stdout"
         stdout%action = "write"
         stdout%unit = tonto%stdout_unit
      end if
      call clear_and_put_margin_(stdout)
   end subroutine

   subroutine create_std_err()
   ! Create a standard output file object.

#ifdef USE_PRECONDITIONS
      INT :: status
#endif
      if (NOT allocated(std_err)) then

         ! Allocate
#ifdef USE_PRECONDITIONS
         allocate(std_err,stat=status)
         ENSURE(status==0,"TEXTFILE:create_std_err ... error in allocating std_err")
#else
         allocate(std_err)
#endif
         ! Reset some defaults
         std_err%name = "stderr"
         std_err%action = "write"
         std_err%unit = tonto%std_err_unit
      end if
      call clear_and_put_margin_(std_err)
   end subroutine

   subroutine redirect_std_err_to(file)
   ! Close the "stderr" file and send everything written to std_err from now
   ! on into "file", which must already be open. Used once the real output
   ! file is known. Repointing the std_err object as well as tonto's error
   ! unit is what keeps the two channels together: a std_err left merely
   ! closed used to write into whichever file next took its unit number,
   ! which in "tonto" was the input file. See TASKS_AND_HISTORY.md.
      type(TEXTFILE_TYPE), IN :: file
      if (exists_(std_err)) call close_(std_err)
      std_err%name = file%name
      std_err%unit = file%unit
      std_err%action = file%action
      call set_std_err_file_(tonto,file)
   end subroutine


!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(TEXTFILE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"TEXTFILE:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(TEXTFILE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"TEXTFILE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   subroutine create_1(self,name)
   ! Create a textfile, and optionally set the name. 
   ! Does not open the file.
      type(TEXTFILE_TYPE), allocatable, OUT :: self
      STR(len=*), IN :: name
      call create_(self)

      ! Set the name
      call set_name_(self,name)
   end subroutine

   subroutine create_2(self,internal)
   ! Create an internal textfile
      type(TEXTFILE_TYPE), allocatable, OUT :: self
      VEC(STR(len=*),:), IN :: internal

      ! The dummy is assumed-length, so len() is the CALLER's line length.
      ! Lines wider than the buffer would be cut by the assignment below --
      ! the same truncation read_line_external now refuses, and refused here
      ! for the same reason, so the two paths cannot disagree.
   DIE_IF(len(internal)>BSTR_SIZE,"TEXTFILE:create_2 ... internal file lines are longer than the line buffer")
      call create_(self)

      ! Reset some defaults
      self%action = "read"
      self%no_of_lines = size(internal)
      self%unit = 0

      ! Set internal file
      self%name = "internal"
      self%internal = internal

      ! Get ready to read
      call read_line_(self)  ! <<< IMPURE
   end subroutine

   PURE subroutine destroy_replacement_list(self)
   ! Destroy the .replacement_list; needed if set_replacement_list is used
      type(TEXTFILE_TYPE), INOUT :: self
      call destroy_(self%style%replacement_list)
      call destroy_(self%buffer%replacement_list)
   end subroutine


!  ===================
!  Saving and unsaving
!  ===================

   PURE subroutine save_style(self)
   ! Save style settings. NOTE: can only be used once.
      type(TEXTFILE_TYPE), INOUT :: self
      self%saved_style = self%style
   end subroutine

   PURE subroutine unsave_style(self)
   ! Restore saved style settings. NOTE: can only be used once.
      type(TEXTFILE_TYPE), INOUT :: self
      self%style = self%saved_style
   end subroutine


!  ========================
!  File redirect and revert
!  ========================

   subroutine redirect_0(self,name)
   ! Save or push "self" in ".saved" and open new textfile "name".
   ! This is used for input, or output redirection.  The new file
   ! retains the output style settings of the cuurent file.
      type(TEXTFILE_TYPE), allocatable, INOUT :: self
      STR(len=*), IN :: name
      type(TEXTFILE_TYPE), allocatable :: saved

      ! Keep self in saved
      call move_alloc(self,saved)

      ! New self "name"
      call create_(self,name)

      ! .saved component of new self is the old self
      self%saved = saved

      ! New style is same as original
      self%style = saved%style

      ! Open the new file
      call open_for_(self,saved%action)
   end subroutine

   subroutine redirect_1(self,internal)
   ! Save or push "self" in ".saved" and open a new internal textfile.
   ! This is used *only* for input redirection.
      type(TEXTFILE_TYPE), allocatable, INOUT :: self
      VEC(STR(len=*),:), IN :: internal
      type(TEXTFILE_TYPE), allocatable :: saved

      ! Keep self in saved
      call move_alloc(self,saved)

      ! New self "name"
      call create_(self,internal)

      ! .saved component of new self is the old self
      self%saved = saved

      ! New style is same as original
      self%style = saved%style
   end subroutine

   subroutine revert(self)
   ! Revert to the previously redirected textfile, but keep *current*
   ! style settings, including e.g. any default_units settings.
      type(TEXTFILE_TYPE), allocatable, INOUT :: self
      type(TEXTFILE_TYPE), allocatable :: saved
   ENSURE(allocated(self%saved),"TEXTFILE:revert ... no previous settings")

      ! Keep .saved file
      call move_alloc(self%saved,saved)

      ! Set .saved style to current settings
      saved%style = self%style

      ! Soft-ending
      saved%IO_status = -1

      ! Close external files or destroy internal file
      if (allocated(self%internal)) then; call destroy_(self%internal)
      else; call close_(self)
      end if
      call destroy_(self)

      ! Restore self
      call move_alloc(saved,self)
   end subroutine


!  =======
!  Opening
!  =======

   subroutine open_for(self,action)
   ! Open the textfile for "action" either "read" or "write".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: action
      select case (action)
      case ("read      ","reading   ","read-only "); call open_for_read_(self)
      case ("write     ","writing   ","write-only"); call open_for_write_(self)
      case default; allocate(tonto%known_keywords(6))
      tonto%known_keywords(1) = "read      "
      tonto%known_keywords(2) = "reading   "
      tonto%known_keywords(3) = "read-only "
      tonto%known_keywords(4) = "write     "
      tonto%known_keywords(5) = "writing   "
      tonto%known_keywords(6) = "write-only"
      call unknown_(tonto,action,"TEXTFILE:open_for")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine open_for_read(self)
   ! Open the input file. The input file object must already be
   ! created. The file is positioned at the first line.
      type(TEXTFILE_TYPE), INOUT :: self
   DIE_IF(NOT exists_(self),"TEXTFILE:open_for_read ... not an existing file!")
   ENSURE(NOT allocated(self%internal),"TEXTFILE:open_for_read ... no need to open an internal file")
      self%action = "read"

      ! Open the file & set unit number
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         open(file=trim(self%name),newunit=self%unit,status="old",access="sequential",form="formatted",iostat=self%IO_status)
      end if

      ! Broadcast & check for fail
      PARALLEL_BROADCAST_IO(self%unit,master_processor_(tonto))
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:open_for_read ... error opening old file: "//trim(self%name))

      ! Set no of lines in the file & rewind
      call set_no_of_lines_(self)

      ! Slurp whole file once-and-for-all
   end subroutine

   subroutine open_for_write(self)
   ! Open the output file for writing
      type(TEXTFILE_TYPE), INOUT :: self
   ENSURE(NOT allocated(self%internal),"TEXTFILE:open_for_write ... why is internal file allocated?")
      if (exists_(self)) then; call open_old_file_for_write_(self)
      else; call open_new_file_for_write_(self)
      end if

      ! Get ready to write
      call clear_and_put_margin_(self)
   end subroutine

   subroutine open_old_file_for_write(self)
   ! Open an old output file for writing
      type(TEXTFILE_TYPE), INOUT :: self
   ENSURE(exists_(self),"TEXTFILE:open_old_file_for_write ... not an existing file!")
   ENSURE(NOT allocated(self%internal),"TEXTFILE:open_old_file_for_write ... why is internal file allocated?")
      self%action = "write"

      ! Open the file & set unit number
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         open(file=trim(self%name),newunit=self%unit,status="old",access="sequential",form="formatted",iostat=self%IO_status)
      end if

      ! Broadcast & check for fail
      PARALLEL_BROADCAST_IO(self%unit,master_processor_(tonto))
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:open_old_file_for_write ... error opening old file: "//trim(self%name))
   end subroutine

   subroutine open_new_file_for_write(self)
   ! Open an new output file for writing
      type(TEXTFILE_TYPE), INOUT :: self
   ENSURE(NOT exists_(self),"TEXTFILE:open_new_file_for_write ... output file exists!")
   ENSURE(NOT allocated(self%internal),"TEXTFILE:open_new_file_for_write ... why is internal file allocated?")
      self%action = "write"

      ! Open the file & set unit number
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         open(file=trim(self%name),newunit=self%unit,status="new",access="sequential",form="formatted",iostat=self%IO_status)
      end if

      ! Broadcast & check for fail
      PARALLEL_BROADCAST_IO(self%unit,master_processor_(tonto))
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:open_new_file_for_write ... error opening new file "//trim(self%name))
   end subroutine


!  ==================
!  Closing & deleting
!  ==================

   subroutine close(self)
   ! Close the input file, if opened, and give up its unit number.
   ! Clearing ".unit" is not tidiness: Fortran hands a closed unit number
   ! straight back to the next "newunit=" open, so an object left holding
   ! one writes into whichever file inherited it. See TASKS_AND_HISTORY.md.
      type(TEXTFILE_TYPE), INOUT :: self
      BIN :: opened
   DIE_IF(NOT exists_(self),"TEXTFILE:close ... not an existing file!")

      ! Check if unit is opened. Only the master inquires, and only when the
      ! file still holds a unit -- a closed one holds TEXTFILE_NO_UNIT, which
      ! "inquire" rejects. The broadcast below stays unconditional: no branch
      ! here may skip a collective.
      opened = FALSE
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         if (self%unit/=TEXTFILE_NO_UNIT) then
            inquire(unit=self%unit,iostat=self%IO_status,opened=opened)
         end if
      end if

      ! Failure?
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:close ... error inquiring about file "//trim(self%name))

      ! Close the file
      if (IO_IS_ALLOWED) then
         if (opened) then
            close(unit=self%unit)
         end if
      end if

      ! Give up the unit
      self%unit = TEXTFILE_NO_UNIT

      ! And stop claiming to be the file an error message should name
      call forget_IO_info_(self)
   end subroutine

   subroutine close_and_delete(self)
   ! Close the input file and delete it from the file system
      type(TEXTFILE_TYPE), INOUT :: self
   DIE_IF(NOT exists_(self),"TEXTFILE:close_and_delete ... not an existing file!")

      ! Close & delete the file
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         close(unit=self%unit,iostat=self%IO_status,status="delete")
      end if

      ! Failure?
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:close_and_delete ... error closing and deleting file "//trim(self%name))

      ! Give up the unit; see the note in ":close"
      self%unit = TEXTFILE_NO_UNIT

      ! And stop claiming to be the file an error message should name
      call forget_IO_info_(self)
   end subroutine

   subroutine delete(self)
   ! Delete the input file from the file system
      type(TEXTFILE_TYPE), INOUT :: self

      ! Open file: is this needed?
      if (NOT is_open_(self)) call open_for_(self,"read")
      call close_and_delete_(self)
   end subroutine


!  =========================================
!  Set comment, quote and specials character
!  =========================================

   subroutine set_name(self,name)
   ! Set the file name
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: name
   DIE_IF(name==" ","TEXTFILE:set_name ... name is blank!")
      self%name = name
   end subroutine

   subroutine set_no_of_lines(self)
   ! Set the number of lines in the file
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: l
      STR(len=8) :: line
   DIE_IF(NOT exists_(self),"TEXTFILE:set_no_of_lines ... file "//trim(self%name)//" does not exist!")
   DIE_IF(NOT is_open_(self),"TEXTFILE:set_no_of_lines ... file "//trim(self%name)//" is not opened!")
   DIE_IF(self%action/="read","TEXTFILE:set_no_of_lines ... has not been opened for reading")

      ! Read the lines until done i.e. failed
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         l = 0
         do
            read(self%unit,fmt="(a)",iostat=self%IO_status) line
            l = l+1
            if (self%IO_status/=0) exit
         end do
      end if

      ! Broadcast - but don't check .IO_status!
      PARALLEL_BROADCAST_IO(l,master_processor_(tonto))

      ! Set the number of lines.
      ! WARNING: this assumes the fail happens after
      ! the stop record, so we have to move before that.
      self%no_of_lines = l-1

      ! Rewind the file
      call rewind_(self)
   end subroutine

   pure subroutine reset_IO_status(self)
   ! Reset manually the IO_status if it is safe to do so.
      type(TEXTFILE_TYPE), INOUT :: self
      self%IO_status = 0
   end subroutine


!  =========================================
!  Set comment, quote and specials character
!  =========================================

   PURE subroutine set_comment_chars(self,val)
   ! Set the characters which represent comment characters.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: val
      call set_comment_chars_(self%style,val)
      call set_comment_chars_(self%buffer,val)
   end subroutine

   PURE subroutine set_quote_chars(self,val)
   ! Set the characters which represent the quotes.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: val
      call set_quote_chars_(self%style,val)
      call set_quote_chars_(self%buffer,val)
   end subroutine

   pure subroutine set_list_delimiters(self,val)
   ! Set the two open- and close- characters which delimit a list.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=2), IN :: val
      call set_list_delimiters_(self%style,val)
   end subroutine

   pure subroutine set_ignore_unmatched_quotes(self,val)
   ! Set whether to ignore unmatched quotes when processing input.
      type(TEXTFILE_TYPE), INOUT :: self
      BIN, IN :: val
      call set_ignore_unmatched_quotes_(self%style,val)
      call set_ignore_unmatched_quotes_(self%buffer,val)
   end subroutine

   PURE subroutine set_eliminate_specials(self,val)
   ! Set the list of characters which should be treated as whitespace.
      type(TEXTFILE_TYPE), INOUT :: self
      BIN, IN :: val
      call set_eliminate_specials_(self%style,val)
      call set_eliminate_specials_(self%buffer,val)
   end subroutine

   PURE subroutine set_replacement_list(self,list)
   ! The first column in "list" is the character to be replaced, and
   ! the second column is the character it should be replaced with.
   ! Set the replacement_list.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), allocatable, IN :: list
      call set_replacement_list_(self%style,list)
      call set_replacement_list_(self%buffer,list)
   end subroutine


!  ==========
!  Set format
!  ==========

   PURE subroutine set_using_fields(self,val)
   ! Set whether to use fields of fixed width or not.
      type(TEXTFILE_TYPE), INOUT :: self
      BIN, IN :: val
      call set_using_fields_(self%style,val)
   end subroutine

   PURE subroutine set_spacing(self,val)
   ! Set the spacing between fields.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      call set_spacing_(self%style,val)
   end subroutine

   PURE subroutine set_n_fields_per_line(self,val)
   ! Set the number of fields per file.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      call set_n_fields_per_line_(self%style,val)
   end subroutine

   PURE subroutine set_int_width(self,val)
   ! Set the width of an integer field.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      call set_int_width_(self%style,val)
   end subroutine

   PURE subroutine set_real_width(self,val)
   ! Set the width of an integer field.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      call set_real_width_(self%style,val)
   end subroutine

   PURE subroutine set_real_precision(self,val)
   ! Set the real precision i.e. no of decimal places.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      call set_real_precision_(self%style,val)
   end subroutine

   PURE subroutine set_real_fortran_fmt(self,val)
   ! Set the fortran format style string
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: val
      call set_real_fortran_fmt_(self%style,val)
   end subroutine

   PURE subroutine set_line_fortran_fmt(self,fmt)
   ! Set the line format from fortran format string "fmt". 
   ! It should include the number of fields e.g. "5f10.3". 
   ! The style must be an allowed fortran real style like "f".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: fmt
      call set_line_fortran_fmt_(self%style,fmt)
   end subroutine

   PURE subroutine set_margin_width(self,val)
   ! Set the width of the margin in the buffer.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      call set_margin_width_(self%style,val)
   end subroutine

   PURE subroutine set_using_array_labels(self,val)
   ! Set whether to use numbered columns, or row labels, on matrix or
   ! vector output.
      type(TEXTFILE_TYPE), INOUT :: self
      BIN, IN :: val
      call set_using_array_labels_(self%style,val)
   end subroutine

   PURE subroutine set_default_units(self,units)
   ! Set the .default_units to "units". This is reset back to 1
   ! after a particular number has been read and converted.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: units
      call set_default_units_(self%style,units)
   end subroutine

   PURE function real_format(self,n_fields,fmt,width,precision,with_parenthesis) result (res)
   ! Return real "<n_fields><fmt><width>.<precision>" fortran format
   ! string, like "5f10.3", for use in writing real quantities.
      type(TEXTFILE_TYPE), IN :: self
      INT, optional, IN :: n_fields
      STR(len=*), optional, IN :: fmt
      INT, optional, IN :: width
      INT, optional, IN :: precision
      BIN, optional, IN :: with_parenthesis
      STR(len=STR_SIZE) :: res
      INT :: n,w,p
      STR(len=STR_SIZE) :: s
      n = self%style%n_fields_per_line
      if (present(n_fields)) n = n_fields
      s = self%style%real_fortran_fmt
      if (present(fmt)) s = fmt
      w = self%style%real_width
      if (present(width)) w = width
      p = self%style%real_precision
      if (present(precision)) p = precision
      res = trim(to_str_(n))
      res = trim(res)//trim(s)
      res = trim(res)//trim(to_str_(w))
      res = trim(res)//"."
      res = trim(res)//trim(to_str_(p))
      if (present(with_parenthesis)) then
         if (with_parenthesis) then
            res = "("//trim(res)//")"
         end if
      end if
   end function

   PURE function int_format(self,n_fields,width,with_parenthesis) result (fmt)
   ! Return integer "<n_fields>i<width>" fortran format string, like
   ! "5i10", for use in writing integer quantities.
      type(TEXTFILE_TYPE), IN :: self
      INT, optional, IN :: n_fields
      INT, optional, IN :: width
      BIN, optional, IN :: with_parenthesis
      STR(len=STR_SIZE) :: fmt
      INT :: n,w
      n = self%style%n_fields_per_line
      if (present(n_fields)) n = n_fields
      w = self%style%int_width
      if (present(width)) w = width
      fmt = trim(to_str_(n))
      fmt = trim(fmt)//"i"
      fmt = trim(fmt)//trim(to_str_(w))
      if (present(with_parenthesis)) then
         if (with_parenthesis) then
            fmt = "("//trim(fmt)//")"
         end if
      end if
   end function


!  ===================
!  Read format options
!  ===================

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","TEXTFILE:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"TEXTFILE:read_keywords ... unexpected end of file while reading list keywords")
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
   ! Process a command "keyword" from "stdin".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      STR(len=STR_SIZE) :: sty
      BIN :: bin
      INT :: val
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("int_width=            "); call read_(stdin,val); call set_int_width_(self,val)
      case ("fortran_line_fmt=     "); call read_(stdin,sty); call set_line_fortran_fmt_(self,sty)
      case ("fortran_real_fmt=     "); call read_(stdin,sty); call set_real_fortran_fmt_(self,sty)
      case ("line_fmt=             "); call read_(stdin,sty); call set_line_fortran_fmt_(self,sty)
      case ("line_format=          "); call read_(stdin,sty); call set_line_fortran_fmt_(self,sty)
      case ("line_fortran_fmt=     "); call read_(stdin,sty); call set_line_fortran_fmt_(self,sty)
      case ("margin_width=         "); call read_(stdin,val); call set_margin_width_(self,val)
      case ("n_fields_per_line=    "); call read_(stdin,val); call set_n_fields_per_line_(self,val)
      case ("no_of_fields_per_line="); call read_(stdin,val); call set_n_fields_per_line_(self,val)
      case ("precision=            "); call read_(stdin,val); call set_real_precision_(self,val)
      case ("real_fortran_fmt=     "); call read_(stdin,sty); call set_real_fortran_fmt_(self,sty)
      case ("real_precision=       "); call read_(stdin,val); call set_real_precision_(self,val)
      case ("real_style=           "); call read_(stdin,sty); call set_real_fortran_fmt_(self,sty)
      case ("real_width=           "); call read_(stdin,val); call set_real_width_(self,val)
      case ("use_array_labels=     "); call read_(stdin,bin); call set_using_array_labels_(self,bin)
      case ("using_fields=         "); call read_(stdin,bin); call set_using_fields_(self,bin)
      case ("using_array_labels=   "); call read_(stdin,bin); call set_using_array_labels_(self,bin)
      case default; allocate(tonto%known_keywords(17))
      tonto%known_keywords(1) = "int_width=            "
      tonto%known_keywords(2) = "fortran_line_fmt=     "
      tonto%known_keywords(3) = "fortran_real_fmt=     "
      tonto%known_keywords(4) = "line_fmt=             "
      tonto%known_keywords(5) = "line_format=          "
      tonto%known_keywords(6) = "line_fortran_fmt=     "
      tonto%known_keywords(7) = "margin_width=         "
      tonto%known_keywords(8) = "n_fields_per_line=    "
      tonto%known_keywords(9) = "no_of_fields_per_line="
      tonto%known_keywords(10) = "precision=            "
      tonto%known_keywords(11) = "real_fortran_fmt=     "
      tonto%known_keywords(12) = "real_precision=       "
      tonto%known_keywords(13) = "real_style=           "
      tonto%known_keywords(14) = "real_width=           "
      tonto%known_keywords(15) = "use_array_labels=     "
      tonto%known_keywords(16) = "using_fields=         "
      tonto%known_keywords(17) = "using_array_labels=   "
      call unknown_(tonto,word,"TEXTFILE:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


!  ====================
!  File inquiry methods
!  ====================

   function exists(self,name) result (res)
   ! Returns true if the file exists on the file system.
   ! Uses "name" if present, otherwise ".name".
      type(TEXTFILE_TYPE), IN :: self
      STR(len=*), optional, IN :: name
      BIN :: res
      if (present(name)) then
         if (IO_IS_ALLOWED) then
            inquire(file=trim(name),exist=res)
         end if
         PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
      else
         if (IO_IS_ALLOWED) then
            inquire(file=trim(self%name),exist=res)
         end if
         PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
      end if
   end function

   function is_open(self) result (res)
   ! Returns true if the file has been opened
      type(TEXTFILE_TYPE), IN :: self
      BIN :: res
      if (self%unit==TEXTFILE_STD_IN_UNIT OR self%unit==TEXTFILE_STD_OUT_UNIT) then
         res = TRUE  ! not needed for stdin, stdout
      else
         if (IO_IS_ALLOWED) then
            inquire(file=self%name,opened=res)
         ! inquire(unit=.unit,opened=res)
         end if
      end if
      PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
   end function

   function is_open_io(self) result (res)
   ! Returns true if the file has been opened
      type(TEXTFILE_TYPE), IN :: self
      BIN :: res
      if (self%unit==TEXTFILE_STD_IN_UNIT OR self%unit==TEXTFILE_STD_OUT_UNIT) then
         res = TRUE  ! not needed for stdin, stdout
      else
         if (IO_IS_ALLOWED) then
            inquire(file=self%name,opened=res)
         ! inquire(unit=.unit,opened=res)
         end if
      end if
   end function

   function unit_used(self) result (res)
   ! Returns true if the file unit is in use.
   ! A closed file holds no unit, and inquiring on one aborts.
      type(TEXTFILE_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (IO_IS_ALLOWED) then
         if (self%unit/=TEXTFILE_NO_UNIT) then
            inquire(unit=self%unit,opened=res)
         end if
      end if
      PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
   end function

   function at_EOF(self) result (res)
   ! Read a *non empty* line into the buffer from 
   ! the file and see if it is at the end of file. 
   ! This is an explicit test for EOF, not just a 
   ! viewing of ".IO_status".  
      type(TEXTFILE_TYPE), INOUT :: self
      BIN :: res
      STR(len=STR_SIZE) :: word
      BIN :: ignore
   ENSURE(self%action=="read","TEXTFILE:at_EOF ... file does not have read status")

      ! Save ignore EOF status
      ignore = self%ignore_EOF
      self%ignore_EOF = TRUE

      ! Read a word ...
      call read_(self,word)

      ! Is it the end of file?
      res = self%IO_status/=0

      ! Reset .IO_status and move back
      self%IO_status = 0
      call move_to_previous_item_(self)

      ! Put back ignore EOF status
      self%ignore_EOF = ignore
   end function

   pure function reverted(self) result (res)
   ! See if .IO_status/=0, indicating an internal file 
   ! has ended i.e. a soft ending.
      type(TEXTFILE_TYPE), IN :: self
      BIN :: res
      res = self%IO_status/=0
   end function


!  ===========
!  Read a line
!  ===========

   subroutine read_line(self)
   ! Read a line into the buffer.
      type(TEXTFILE_TYPE), INOUT :: self
      if (allocated(self%internal)) then; call read_line_internal_(self)
      else; call read_line_external_(self)
      end if
   end subroutine

   subroutine read_line_internal(self)
   ! Read line into buffer from the *internal* file. 
   ! If the file ends, this routine dies, UNLESS 
   ! .ignore_EOF is set, in which case another succesive 
   ! failure to read a line will generate an error (this 
   ! prevents infinite read loops, and is also useful 
   ! for testing whether at the end of a file).
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=BSTR_SIZE) :: string
   ENSURE(allocated(self%internal),"TEXTFILE:read_line_internal ... no internal file")

      ! Update
      call update_IO_info_(self)
      if (self%record<size(self%internal)) then

         ! No errors.
         self%IO_status = 0
         self%record = self%record+1
         string = self%internal(self%record)
         call set_and_analyze_(self%buffer,string)

         ! Now the line is in, snapshot it: the call above only had the
         ! previous one. See .update_IO_info.
         call update_IO_info_(self)
      else

         ! At end of file.
         self%IO_status = 1
         if (self%ignore_EOF) then

            ! Next time don't ignore EOF
            self%ignore_EOF = FALSE
         else

            ! WTF?
            call read_line_bad_EOF_(self)
         end if
      end if
   end subroutine

   subroutine read_line_external(self)
   ! Read line into buffer from an *external* disk file.  
   ! If the file ends, this routine dies, UNLESS 
   ! .ignore_EOF is set, in which case another succesive 
   ! failure to read a line will generate an error (this 
   ! prevents infinite read loops, and is also useful 
   ! for testing whether at the end of a file).
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=BSTR_SIZE) :: string,tail
      INT :: length,n
      VEC(INT,3) :: info
      BIN :: too_long
   ENSURE(is_open_(self),"TEXTFILE:read_line_external ... file is not open")

      ! Update
      call update_IO_info_(self)

      ! Read a line.
      !
      ! The read is NON-ADVANCING, which is the only way in standard Fortran to
      ! tell a line that just fitted from one that was cut off. An ordinary
      ! advancing read blank-pads a short record, silently DISCARDS whatever
      ! runs past the end of "string", and reports iostat=0 in both cases -- so
      ! the truncation is invisible. With advance="no" the outcomes separate:
      !
      !   iostat < 0, not EOF : end of record -- the normal case; "size" then
      !                         gives the record's true length
      !   iostat = 0          : "string" filled before the record ended, i.e.
      !                         THE LINE IS TOO LONG
      !   iostat = EOF        : end of file
      !   iostat > 0          : a real read error
      !
      ! End-of-record and end-of-file are both negative and processor-dependent
      ! (ISO_FORTRAN_ENV names them IOSTAT_EOR and IOSTAT_END). Only EOF is
      ! compared against below, so no end-of-record constant is needed.
      !
      ! Master only, as every read here is; the results are broadcast after.
      self%IO_status = 0
      string = " "
      length = 0
      too_long = FALSE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%IO_status,fmt="(a)",advance="no",size=length) string

         ! Too long. Read the rest of the record, both to say how long it
         ! really is and to leave the file positioned on the next line.
         too_long = self%IO_status==0
         do while (self%IO_status==0)
            read(unit=self%unit,iostat=self%IO_status,fmt="(a)",advance="no",size=n) tail
            length = length+n
         end do

         ! Back to the advancing convention used below, where 0 is a good line.
         ! A positive status is a real error and must survive untouched.
         if (self%IO_status<0 AND self%IO_status/=TEXTFILE_IOSTAT_EOF) self%IO_status = 0
      end if

      ! Broadcast. Unconditional, and outside the guard above, so that every
      ! rank enters the same collectives.
      ! The status, the length and the too-long flag go as one message.
      info = [self%IO_status,length,0]
      if (too_long) info(3) = 1
      PARALLEL_BROADCAST_IO(info,master_processor_(tonto))
      PARALLEL_BROADCAST_IO(string,master_processor_(tonto))
      self%IO_status = info(1)
      length = info(2)
      too_long = info(3)==1

      ! An over-long line is an error, not something to trim quietly
      if (too_long) call read_line_too_long_(self,length)

      ! Is the read OK?
      select case (self%IO_status)

      case (0)

            ! No errors. Analyze the line
         call set_and_analyze_(self%buffer,string)
         self%record = self%record+1

            ! Now the line is in, snapshot it: the call above only had the
            ! previous one. See .update_IO_info.
         call update_IO_info_(self)

      case (TEXTFILE_IOSTAT_EOF)
         if (self%ignore_EOF) then

            ! Status still indicates EOF
            ! Do an explicit backspace
            ! No need to (and don't) check .IO_status
            if (IO_IS_ALLOWED) then
               backspace(unit=self%unit)
            end if
         else

            ! Unexpected EOF
            call read_line_bad_EOF_(self)
         end if

      case default

         ! Some error, but not end of file.
         DIE("TEXTFILE:read_line_external ... read error")
      end select
   end subroutine

   subroutine read_line_too_long(self,length)
   ! An input line was longer than the line buffer. Say so, naming the line and
   ! its true length: quietly keeping the first BSTR_SIZE characters is how a
   ! long path in a job file used to fail, with an error naming the NEXT line.
      type(TEXTFILE_TYPE), IN :: self
      INT, IN :: length
      STR(len=STR_SIZE) :: msg
      write(msg,'(a,i0,a,i0,a,i0,a)') "line ",self%record+1," is ",length," characters, over the ",BSTR_SIZE," character line limit, in file "
      DIE("TEXTFILE:read_line_too_long ... input "//trim(msg)//" "//trim(self%name))
   end subroutine

   subroutine read_line_bad_EOF(self)
   ! Reading a line encountered an unexpected EOF.
      type(TEXTFILE_TYPE), IN :: self
      if (allocated(self%saved)) then; DIE("TEXTFILE:read_line_bad_EOF ... did you forget to set ignore_EOF?")
      else; DIE("TEXTFILE:read_line_bad_EOF ... unexpected end of file")
      end if
   end subroutine

   subroutine update_line(self)
   ! Get the next non-exhausted line if the current 
   ! one is exhausted of tokens. If there is no EOF
   ! the buffer pointer is positioned before the 
   ! first item on the new line. 
      type(TEXTFILE_TYPE), INOUT :: self
      do

         ! Any items to read?
         if (NOT exhausted_(self%buffer)) exit

         ! No? Then read a new line
         call read_line_(self)

         ! Error: no more lines to read
         if (self%IO_status/=0) exit
      end do
   end subroutine

   subroutine revert_line(self)
   ! Revert back to the previous non-empty line 
   ! in the input file (if the current buffer 
   ! pointer is before the first item). And in
   ! this case, the buffer pointer is positioned 
   ! *after* the last line item.

      ! Any previous line items?
      type(TEXTFILE_TYPE), INOUT :: self
      if (next_item_index_(self%buffer)>1) return

      ! No previous items: back to non-blank line
      do
         call backspace_line_(self)
         if (NOT exhausted_(self%buffer)) exit
      end do

      ! Move *after* last item on non-blank line
      call move_to_line_item_(self,last_line_item_(self)+1)
   end subroutine


!  ========================
!  File rewind, move to end
!  ========================

   subroutine rewind(self)
   ! Rewind the input file, reading the first line for read-only files.
      type(TEXTFILE_TYPE), INOUT :: self
      if (allocated(self%internal)) then; call rewind_internal_(self)
      else; call rewind_external_(self)
      end if
      select case (self%action)
      case ("read      ","reading   ","read-only ")
      case ("write     ","writing   ","write-only")
      case default; allocate(tonto%known_keywords(6))
      tonto%known_keywords(1) = "read      "
      tonto%known_keywords(2) = "reading   "
      tonto%known_keywords(3) = "read-only "
      tonto%known_keywords(4) = "write     "
      tonto%known_keywords(5) = "writing   "
      tonto%known_keywords(6) = "write-only"
      call unknown_(tonto,self%action,"TEXTFILE:rewind")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   PURE subroutine rewind_internal(self)
   ! Rewind the input file, but do not read a line.
      type(TEXTFILE_TYPE), INOUT :: self
      self%record = 0
      self%IO_status = 0
   end subroutine

   subroutine rewind_external(self)
   ! Rewind the input file, but do not read a line.
      type(TEXTFILE_TYPE), INOUT :: self

      ! Update
      call update_IO_info_(self)

      ! Rewind
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         rewind(unit=self%unit,iostat=self%IO_status)
      end if

      ! Failure?
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:rewind_external ... error rewinding file "//trim(self%name))

      ! All good: set line no.
      self%record = 0
   end subroutine

   subroutine move_to_end(self)
   ! Move to the end of the input file
      type(TEXTFILE_TYPE), INOUT :: self
      if (allocated(self%internal)) then; call move_to_end_internal_(self)
      else; call move_to_end_external_(self)
      end if
   end subroutine

   subroutine move_to_end_internal(self)
   ! Move to the end of the input file
      type(TEXTFILE_TYPE), INOUT :: self
      self%record = size(self%internal)-1
      call read_line_(self)
   end subroutine

   subroutine move_to_end_external(self)
   ! Move to the end of the input file
      type(TEXTFILE_TYPE), INOUT :: self

      ! Update
      call update_IO_info_(self)

      ! Read till failure
      do

         ! Read a line
         self%IO_status = 0
         if (IO_IS_ALLOWED) then
            read(unit=self%unit,iostat=self%IO_status,fmt="()")
         end if
         PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))

         ! Failed?
         if (self%IO_status/=0) exit

         ! Increment line no.
         self%record = self%record+1
      end do

      ! At the EOF: backspace twice
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         backspace(unit=self%unit,iostat=self%IO_status)
         backspace(unit=self%unit,iostat=self%IO_status)
      end if
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))

      ! Set the record
      self%record = max(0,self%record-1)

      ! Get the last line
      call read_line_(self)
   end subroutine


!  =================
!  Skip file records
!  =================

   subroutine skip_line(self)
   ! Skip the next line in the input file
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: record
      record = self%record+1
      call move_to_record_(self,record)
   end subroutine

   subroutine backspace_line(self)
   ! Move to the previous line
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: record
      record = self%record-1
      call move_to_record_(self,record)
   end subroutine

   subroutine move_to_line(self,line)
   ! Make sure that "line" was the last one processed
   ! in the input file
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: line
      call move_to_record_(self,line)
   end subroutine

   subroutine move_to_record(self,rec)
   ! Move to the requested record "rec" in the input file.
   ! and read it into the buffer. Set .record to the
   ! last processed record.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: rec
   ENSURE(rec>=0,"TEXTFILE:move_to_record ... line number must be non-negative")
      if (allocated(self%internal)) then; call move_to_record_internal_(self,rec)
      else; call move_to_record_external_(self,rec)
      end if
   end subroutine

   subroutine move_to_record_internal(self,rec)
   ! Move to the requested record "rec" in the internal file.
   ! and read it into the buffer. Set .record to the
   ! last processed record.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: rec
   ENSURE(rec>=0,"TEXTFILE:move_to_record_internal ... record number must be non-negative")
   ENSURE(rec<=size(self%internal),"TEXTFILE:move_to_record_internal ... record outside file range")
      self%record = rec-1
      call read_line_(self)
   end subroutine

   subroutine move_to_record_external(self,rec)
   ! Move to the requested record "rec" in the input file.
   ! and read it into the buffer. Set .record to the
   ! last processed record.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: rec
   ENSURE(rec>=0,"TEXTFILE:move_to_record_external ... record number must be non-negative")

      ! Update
      call update_IO_info_(self)

      ! Position the file on the I/O processor ALONE, then broadcast the result.
      ! The number of iterations depends on .record, and on "rec" -- which CIF
      ! derives from .record (cif.foo: .end_of_data = .file.record). Both are
      ! rank-local. When every rank ran this loop they ran it a different number
      ! of times, and because each iteration issued a collective the ranks shifted
      ! permanently out of step: a later broadcast then delivered a record counter
      ! into .IO_status and the job died claiming it could not open a file.
      ! See docs/TASK_MPI.md Finding 7.
      if (IO_IS_ALLOWED) then
         if (rec<(self%record+1)) then

            ! Go backwards
            do
               call move_to_back_record_(self)
               if (self%IO_status/=0) exit
               if (rec==(self%record+1)) exit
            end do
         else if (rec>(self%record+1)) then

            ! Go forwards
            do
               call move_to_next_record_(self)
               if (self%IO_status/=0) exit
               if (rec==(self%record+1)) exit
            end do
         end if
      end if

      ! Failure? Exactly one of each, on every rank, whatever the file position
      ! was -- that is the point: the collective count must not depend on state
      ! that can differ between ranks.
      PARALLEL_BROADCAST_IO(self%IO_status,master_processor_(tonto))
      PARALLEL_BROADCAST_IO(self%record,master_processor_(tonto))
      DIE_IF(self%IO_status/=0,"TEXTFILE:move_to_record_external ... error moving to a record in "//trim(self%name))

      ! Read it
      call read_line_(self)
   end subroutine

   subroutine move_to_back_record(self)
   ! Backspace a record. Skip system info update.
   ! NOT COLLECTIVE, and it must stay that way: the only caller,
   ! move_to_record_external, runs this inside an IO_IS_ALLOWED guard a
   ! rank-dependent number of times, and broadcasts .IO_status and .record once
   ! afterwards. Putting a collective back in here reinstates the desync of
   ! docs/TASK_MPI.md Finding 7. The caller must check .IO_status.
      type(TEXTFILE_TYPE), INOUT :: self
      self%IO_status = 0
      backspace(unit=self%unit,iostat=self%IO_status)

      ! Previous record
      self%record = self%record-1
   end subroutine

   subroutine move_to_next_record(self)
   ! Forward space a record. Skip system info update.
      type(TEXTFILE_TYPE), INOUT :: self

      ! Read a line. NOT COLLECTIVE -- see move_to_back_record for why, and do
      ! not add a broadcast here. The caller must check .IO_status.
      self%IO_status = 0
      read(unit=self%unit,fmt="()",iostat=self%IO_status)

      ! Next record
      self%record = self%record+1
   end subroutine

   subroutine increment_record(self)
   ! Increment the .record counter for the file
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: record
      record = self%record+1
   end subroutine


!  ===========
!  Line search
!  ===========

!  Consider slurping whole file into memory?

   subroutine look_for_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
   ! Scan through the file for an "item", optionally:
   ! . Starting at line "from", inclusive
   ! . Ending   at line "until" inclusive
   ! . Ending   at any token staring with "end_tokens"
   ! . With "item" matching exactly (if "exact_match" is TRUE)
   ! . With "item" head-matching    (if "head_match" is TRUE)
   ! . Return cursor to start line  (if "return_cursor" is TRUE)
   ! . Return without error with "found" TRUE (if "item" found)
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: item
      INT, optional, IN :: from
      INT, optional, IN :: until
      VEC(STR(len=*),:), optional, IN :: end_tokens
      BIN, optional, IN :: exact_match
      BIN, optional, IN :: head_match
      BIN, optional, IN :: return_cursor
      BIN, optional, OUT :: found
      INT :: start_record
      STR(len=STR_SIZE) :: word
      BIN :: fnd,exact,head,put_back,has_item,has_end,match
      INT :: i

      ! Look from .record or "from"
      start_record = self%record
      if (present(from)) start_record = from

      ! Look until EOF or "until"
      if (present(until)) then
         ENSURE(until>=start_record,"TEXTFILE:look_for_item ... ending line must be greater than starting line")
      end if

      ! Item to match exactly?
      exact = FALSE
      if (present(exact_match)) exact = exact_match

      ! Item to match token head only?
      head = FALSE
      if (present(head_match)) head = head_match
      if (head) exact = FALSE

      ! Rewind afterwards?
      put_back = FALSE
      if (present(return_cursor)) put_back = return_cursor

      ! Move to start
      call move_to_record_(self,start_record)

      ! Initialize
      self%ignore_EOF = TRUE
      fnd = FALSE

      ! Loop over file lines (if all OK)
      if (self%IO_status==0) then

         ! Now do the search here ...
         line_search: do

            ! Item found?
            has_item = includes_(self%buffer%string,item)

            ! End token found?
            has_end = FALSE
            if (present(end_tokens)) then
               if (self%record>start_record) then
                  has_end = includes_any_in_(self%buffer%string,end_tokens)
               end if
            end if
            if (has_item OR has_end) then

               ! Find the matching token
               do i = 1,self%buffer%n_items
                  call get_next_item_(self,word)
                  if (has_item) then

                     ! What kind of match was it?
                     if (head) then; match = includes_(word,item,at_start=TRUE)
                     else if (exact) then; match = word==item
                     else; match = includes_(word,item)
                     end if
                     if (match) then
                        fnd = TRUE
                        exit line_search
                     end if
                  end if
                  if (has_end) then
                     if (has_any_included_in_(end_tokens,word,at_start=TRUE)) then
                        fnd = TRUE
                        exit line_search
                     end if
                  end if
               end do
            end if
            if (present(until)) then
               if (self%record==until) exit line_search
            end if
            call read_line_(self)
            if (self%IO_status/=0) exit line_search
         end do line_search
      end if
      if (present(found)) found = fnd
      if (NOT fnd AND put_back) call move_to_record_(self,start_record)
      self%ignore_EOF = FALSE
   end subroutine

   subroutine look_backwards_for_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
   ! Scan *backwards* through the file for an "item". If present:
   ! . Starting at line "from"  inclusive
   ! . Ending at line "until" inclusive
   ! . Ending at any token which *begins* with "end_tokens"
   ! . With "item" matching exactly, if "exact_match" is TRUE
   ! . With "item" head-matching,    if "head_match" is TRUE
   ! . Return cursor to orginal line, if "return_cursor" is TRUE
   ! . Return without error, with "found" TRUE if "item" was found
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: item
      INT, optional, IN :: from
      INT, optional, IN :: until
      VEC(STR(len=*),:), optional, IN :: end_tokens
      BIN, optional, IN :: exact_match
      BIN, optional, IN :: head_match
      BIN, optional, IN :: return_cursor
      BIN, optional, OUT :: found
      INT :: start_record,end_record
      STR(len=STR_SIZE) :: word
      BIN :: fnd,exact,head,put_back,has_item,has_end,match
      INT :: i

      ! Look from .record or "from"
      start_record = self%record
      if (present(from)) start_record = from

      ! Look until SOF or "until"
      end_record = 1
      if (present(until)) end_record = until
      ENSURE(end_record<=start_record,"TEXTFILE:look_backwards_for_item ... ending line must be smaller than starting line")

      ! Item to match exactly?
      exact = FALSE
      if (present(exact_match)) exact = exact_match

      ! Item to match token head only?
      head = FALSE
      if (present(head_match)) head = head_match
      if (head) exact = FALSE

      ! Rewind afterwards?
      put_back = FALSE
      if (present(return_cursor)) put_back = return_cursor

      ! Move to start
      call move_to_record_(self,start_record)

      ! Initialise
      self%ignore_EOF = TRUE
      fnd = FALSE
      if (self%IO_status==0) then
         call move_to_last_item_on_line_(self)

         ! Search for tokens ...
         line_search: do

            ! Item found?
            has_item = includes_(self%buffer%string,item)

            ! End token found?
            has_end = FALSE
            if (present(end_tokens)) then
               if (self%record>start_record) then
                  has_end = includes_any_in_(self%buffer%string,end_tokens)
               end if
            end if
            if (has_item OR has_end) then

               ! Look backwards ...
               do i = self%buffer%n_items,1,-1
                  call move_to_line_item_(self,i)
                  call get_next_item_(self,word)
                  if (has_item) then

                     ! What kind of match was it?
                     if (head) then; match = includes_(word,item,at_start=TRUE)
                     else if (exact) then; match = word==item
                     else; match = includes_(word,item)
                     end if

                     ! Found a match
                     if (match) then
                        fnd = TRUE
                        exit line_search
                     end if
                  end if
                  if (has_end) then
                     if (has_any_included_in_(end_tokens,word,at_start=TRUE)) then
                        fnd = TRUE
                        exit line_search
                     end if
                  end if
               end do
            end if
            if (self%record==end_record) exit line_search

            ! Move to previous line
            call backspace_line_(self)
         end do line_search
      end if
      if (present(found)) found = fnd
      if (NOT fnd AND put_back) call move_to_record_(self,start_record)
      self%ignore_EOF = FALSE
   end subroutine

   subroutine look_for_any_item(self,item,from,until,end_tokens,exact_match,head_match,return_cursor,found)
   ! Scan through the file for *any* in "item". If present:
   ! . Starting at line "from"  inclusive
   ! . Ending at line "until" inclusive
   ! . Ending at any token which *begins* with "end_tokens"
   ! . With "item" matching exactly, if "exact_match" is TRUE
   ! . With "item" head-matching,    if "head_match" is TRUE
   ! . Return cursor to orginal line, if "return_cursor" is TRUE
   ! . Return without error, with "found" TRUE if "item" was found
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: item
      INT, optional, IN :: from
      INT, optional, IN :: until
      VEC(STR(len=*),:), optional, IN :: end_tokens
      BIN, optional, IN :: exact_match
      BIN, optional, IN :: head_match
      BIN, optional, IN :: return_cursor
      BIN, optional, OUT :: found
      INT :: start_record
      STR(len=STR_SIZE) :: word
      BIN :: fnd,exact,head,put_back,has_item,has_end,match
      INT :: i,j

      ! Look from .record or "from"
      start_record = self%record
      if (present(from)) start_record = from

      ! Look until EOF or "until"
      if (present(until)) then
         ENSURE(until>=start_record,"TEXTFILE:look_for_any_item ... ending line must be greater than starting line")
      end if

      ! Item to match exactly?
      exact = FALSE
      if (present(exact_match)) exact = exact_match

      ! Item to match token head only?
      head = FALSE
      if (present(head_match)) head = head_match
      if (head) exact = FALSE

      ! Rewind afterwards?
      put_back = FALSE
      if (present(return_cursor)) put_back = return_cursor

      ! Move to start
      call move_to_record_(self,start_record)

      ! Initialize
      self%ignore_EOF = TRUE
      fnd = FALSE
      if (self%IO_status==0) then
         line_search: do

            ! Item found?
            has_item = includes_any_in_(self%buffer%string,item)

            ! End token found?
            has_end = FALSE
            if (present(end_tokens)) then
               if (self%record>start_record) then
                  has_end = includes_any_in_(self%buffer%string,end_tokens)
               end if
            end if
            if (has_item OR has_end) then

               ! Find the matching token
               do i = 1,self%buffer%n_items
                  call get_next_item_(self,word)
                  if (has_item) then

                     ! What kind of match was it?
                     match = FALSE
                     if (head) then
                        do j = 1,size(item)
                           match = includes_(word,item(j),at_start=TRUE)
                           if (match) exit
                        end do
                     else if (exact) then
                        do j = 1,size(item)
                           match = word==item(j)
                           if (match) exit
                        end do
                     else  ! partial
                        do j = 1,size(item)
                           match = includes_(word,item(j))
                           if (match) exit
                        end do
                     end if

                     ! Found a match
                     if (match) then
                        fnd = TRUE
                        exit line_search
                     end if
                  end if
                  if (has_end) then
                     if (has_any_included_in_(end_tokens,word,at_start=TRUE)) then
                        exit line_search
                     end if
                  end if
               end do
            end if
            if (present(until)) then
               if (self%record==until) exit line_search
            end if
            call read_line_(self)
            if (self%IO_status/=0) exit line_search
         end do line_search
      end if
      if (present(found)) found = fnd
      if (NOT fnd AND put_back) call move_to_record_(self,start_record)
      self%ignore_EOF = FALSE
   end subroutine

   subroutine look_for(self,item,from,until,first,found)
   ! Scans backward through the file for a line which 
   ! includes string "item". 
   ! - If there, the file buffer is left at the first 
   !   line at which the match occured. 
   ! - If not there, the file is rewound to the 
   !   original line before the search.
   ! - If "from" is present then it is used as the 
   !   start line for the search. 
   ! - If "until" is present then matches to these
   !   strings indicate the end of search condition 
   !   for "item". 
   ! - If "first" is present and TRUE, then the item 
   !   is matched only if it is the first non-blank 
   !   token in the input; and likewise the search 
   !   is terminated only when the "until" tokens are 
   !   the first characters in the input. 
   ! - If "found" is present, it is set TRUE when the 
   !   item is found, else FALSE.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: item
      INT, optional, IN :: from
      VEC(STR(len=*),:), optional, IN :: until
      BIN, optional, IN :: first
      BIN, optional, OUT :: found
      INT :: start_record
      BIN :: fnd
      start_record = self%record
      if (present(from)) start_record = from

      ! Move to start
      call move_to_record_(self,start_record)

      ! Initialize
      self%ignore_EOF = TRUE
      fnd = FALSE
      line_search: do

         ! NOTE: below should be a method of buffer
         if (is_included_in_(item,self%buffer%string,first)) then
            fnd = TRUE
            exit line_search
         end if
         if (present(until)) then
            if (self%record>start_record) then
               if (has_any_included_in_(until,self%buffer%string,first)) then
                  call move_to_record_(self,start_record)
                  fnd = FALSE
                  exit line_search
               end if
            end if
         end if
         call read_line_(self)
         if (self%IO_status/=0) then
            call move_to_record_(self,start_record)
            fnd = FALSE
            exit line_search
         end if
      end do line_search
      if (present(found)) found = fnd
      self%ignore_EOF = FALSE
   end subroutine

   subroutine look_backwards_for(self,item,from,until,first,found)
   ! Scans backward through the file for a line which 
   ! includes string "item". 
   ! - If there, the file buffer is left at the first 
   !   line at which the match occured. 
   ! - If not there, the file is rewound to the 
   !   original line before the search.
   ! - If "from" is present then it is used as the 
   !   start line for the search. 
   ! - If "until" is present then matches to these
   !   strings indicate the end of search condition 
   !   for "item". 
   ! - If "first" is present and TRUE, then the item 
   !   is matched only if it is the first non-blank 
   !   token in the input; and likewise the search 
   !   is terminated only when the "until" tokens are 
   !   the first characters in the input. 
   ! - If "found" is present, it is set TRUE when the 
   !   item is found, else FALSE.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: item
      INT, optional, IN :: from
      VEC(STR(len=*),:), optional, IN :: until
      BIN, optional, IN :: first
      BIN, optional, OUT :: found
      INT :: start_record
      BIN :: fnd

      ! Look from .record or "from"
      start_record = self%record
      if (present(from)) start_record = from

      ! Move to start
      call move_to_record_(self,start_record)

      ! Initialize
      self%ignore_EOF = TRUE
      fnd = FALSE
      do

         ! NOTE: below should be a method of buffer
         if (is_included_in_(item,self%buffer%string,first)) then
            fnd = TRUE
            exit
         end if

         ! Start of the file?
         if (self%record==1) then
            call move_to_record_(self,start_record)
            fnd = FALSE
            exit
         end if
         call backspace_line_(self)
         if (present(until)) then
            if (self%record<start_record) then
               if (has_any_included_in_(until,self%buffer%string,first)) then
                  call move_to_record_(self,start_record)
                  fnd = FALSE
                  exit
               end if
            end if
         end if
      end do
      if (present(found)) found = fnd
      self%ignore_EOF = FALSE
   end subroutine

   function has_string(self,search) result (res)
   ! Returns TRUE if the file contains string "search". 
   ! The file is returned to its original line number 
   ! after this routine.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: search
      BIN :: res
      INT :: update_record
      BIN :: found
      update_record = self%record
      call rewind_(self)
      self%ignore_EOF = TRUE
      found = FALSE
      do
         if (index(self%buffer%string,search)/=0) then
            found = TRUE
            exit
         end if
         call read_line_(self)
         if (self%IO_status/=0) exit
      end do
      call move_to_record_(self,update_record)
      self%ignore_EOF = FALSE
      res = found
   end function


!  ====================
!  Item inquiry methods
!  ====================

   pure function next_line_item(self) result (res)
   ! Return index of the next line item to be processed 
      type(TEXTFILE_TYPE), IN :: self
      INT :: res
      res = next_item_index_(self%buffer)
   end function

   pure function previous_line_item(self) result (res)
   ! Return the index of the previous item to be processed on the line
      type(TEXTFILE_TYPE), IN :: self
      INT :: res
      res = self%buffer%item_index
   end function

   pure function last_line_item(self) result (res)
   ! Return the index of the final item on the line
      type(TEXTFILE_TYPE), IN :: self
      INT :: res
      res = self%buffer%n_items
   end function

   pure function n_line_items(self) result (res)
   ! Return the number of items on the line
      type(TEXTFILE_TYPE), IN :: self
      INT :: res
      res = self%buffer%n_items
   end function

   pure function at_end_of_line(self) result (res)
   ! Return TRUE if at the end of the line
      type(TEXTFILE_TYPE), IN :: self
      BIN :: res
      res = self%buffer%item_index==self%buffer%n_items
   end function

   pure function line_number(self) result (res)
   ! Return the input file line number which is being processed
      type(TEXTFILE_TYPE), IN :: self
      INT :: res
      res = self%record
   end function

   pure function buffer_string(self) result (res)
   ! Put a string into the buffer
      type(TEXTFILE_TYPE), IN :: self
      STR(len=BSTR_SIZE) :: res
      res = buffer_string_(self%buffer)
   end function

   function rest_of_line(self) result (res)
   ! If there is anything left on the input line, then it is returned, else
      type(TEXTFILE_TYPE) :: self
   ! nothing is returned.  Record moves to the next line.
      STR(len=STR_SIZE) :: res
   ENSURE(self%action=="read","TEXTFILE:rest_of_line ... file does not have read action!")
      if (NOT empty_(self%buffer)) then
         res = self%buffer%string(self%buffer%item_end+1:)
         call read_line_(self)
      else
         res = " "
         call read_line_(self)
      end if
   end function


!   buffer_exhausted result (res) :: pure
!   ! Return whether the buffer is exhausted
!     self :: IN
!     res :: BIN
!
!     res = .buffer.exhausted
!
!   end

!  ============
!  Read an item
!  ============

   subroutine get_next_item(self,word)
   ! Get the next item in the file
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*) :: word
      call update_line_(self)
      call get_(self%buffer,word)
      call update_IO_cursor_(self)
   end subroutine

   function next_item(self) result (word)
   ! Read a str from the input file and return it but
   ! *do not advance the cursor*. Use next_str for that.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
      call read_(self,word)
      call move_to_previous_item_(self)
   end function

   function next_str(self) result (word)
   ! Read a str from the input file and return it and advance the cursor
      type(TEXTFILE_TYPE) :: self
      STR(len=STR_SIZE) :: word
      call read_(self,word)
   end function


!  ==================
!  Skip/move to items
!  ==================

   subroutine skip_next_item(self)
   ! Move to (i.e. just after) the next item in the input file.
      type(TEXTFILE_TYPE), INOUT :: self
      call update_line_(self)
      call skip_item_(self%buffer)
      call update_IO_cursor_(self)
   end subroutine

   subroutine skip_next_items(self,n)
   ! Skip several items.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: n
      INT :: i
      do i = 1,n
         call skip_next_item_(self)
      end do
   end subroutine

   subroutine move_to_previous_item(self)
   ! Move to (i.e. before) the previous item in the input file.
   ! Backspace a line if required.
      type(TEXTFILE_TYPE), INOUT :: self
      call revert_line_(self)

      ! Move to the last read item
      call move_to_previous_item_on_line_(self)
   end subroutine

   subroutine move_to_last_item_on_line(self)
   ! Move the cursor over to the beginning of the last item on the line
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: item
      item = last_line_item_(self)
      call move_to_line_item_(self,item)
   end subroutine

   subroutine move_to_previous_item_on_line(self)
   ! Move the cursor over to the beginning of the previous item on the
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: item
      item = previous_line_item_(self)
      call move_to_line_item_(self,item)
   end subroutine

   subroutine move_to_line_item(self,number)
   ! Move the cursor over to the beginning of the item with index "number"
   ! on the current line. (More accurately: after the end of the previous item
   ! on the current line. So if there are "nitems" on the line you can move to
   ! "nitem+1" if you want to, and that will be the end of the line).
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: number
      call move_to_item_(self%buffer,number)

      ! The full snapshot, not just the cursor: this is one of the places that
      ! claims the file, and it may be reached without an intervening read.
      call update_IO_info_(self)
   end subroutine

!  ====================
!  Read a built-in TYPE
!  ====================

   subroutine read_0(self,value)
   ! Read a built-in type into "value".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), OUT :: value
   ENSURE(self%action=="read","TEXTFILE:read_0 ... file does not have read action!")

      ! The following code is inherited from TEXTFILE
      call update_line_(self)
      call get_(self%buffer,value)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_1(self,value)
   ! Read a built-in type into "value".
      type(TEXTFILE_TYPE), INOUT :: self
      BIN, OUT :: value
   ENSURE(self%action=="read","TEXTFILE:read_1 ... file does not have read action!")

      ! The following code is inherited from TEXTFILE
      call update_line_(self)
      call get_(self%buffer,value)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_2(self,value)
   ! Read a built-in type into "value".
      type(TEXTFILE_TYPE), INOUT :: self
      INT, OUT :: value
   ENSURE(self%action=="read","TEXTFILE:read_2 ... file does not have read action!")

      ! The following code is inherited from TEXTFILE
      call update_line_(self)
      call get_(self%buffer,value)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_3(self,value)
   ! Read a built-in type into "value".
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, OUT :: value
   ENSURE(self%action=="read","TEXTFILE:read_3 ... file does not have read action!")

      ! The following code is inherited from TEXTFILE
      call update_line_(self)
      call get_(self%buffer,value)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_4(self,value,format)
   ! Read a formatted INT into "value". Does not check the end of line!
      type(TEXTFILE_TYPE), INOUT :: self
      INT, OUT :: value
      STR(len=*), IN :: format
   ENSURE(self%action=="read","TEXTFILE:read_4 ... file does not have read action!")
      call update_line_(self)
      call get_(self%buffer,value,format)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_5(self,value,format)
   ! Read a formatted REAL into "value". Does not check the end of line!
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, OUT :: value
      STR(len=*), IN :: format
   ENSURE(self%action=="read","TEXTFILE:read_5 ... file does not have read action!")
      call update_line_(self)
      call get_(self%buffer,value,format)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_6(self,value,error)
   ! Read a REAL into "value" and also associated "error", 
   ! which appears in parentheses after it.
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, OUT :: value
      REAL, OUT :: error
   ENSURE(self%action=="read","TEXTFILE:read_6 ... file does not have read action!")
      call update_line_(self)
      call get_imprecise_real_(self%buffer,value,error)
      call update_IO_cursor_(self)
   end subroutine

   subroutine read_7(self,value)
   ! Read a built-in type into "value".
      type(TEXTFILE_TYPE), INOUT :: self
      CPX, OUT :: value
   ENSURE(self%action=="read","TEXTFILE:read_7 ... file does not have read action!")

      ! The following code is inherited from TEXTFILE
      call update_line_(self)
      call get_(self%buffer,value)
      call update_IO_cursor_(self)
   end subroutine

!  =============================
!  Read a built-in TYPE quantity
!  =============================

   subroutine read_quantity_0(self,value)
   ! Read a number quantity (with optional units) into "value".
   ! WARNING: You must *not* be at the end of file to use this
   ! routine, since it will try to read the next token as a units specifier.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, OUT :: value
      STR(len=STR_SIZE) :: units
      BIN :: known_unit
   ENSURE(self%action=="read","TEXTFILE:read_quantity_0 ... file does not have read action!")

      ! Read the value

      ! The following code is inherited from TEXTFILE
      call read_(self,value)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") call convert_from_(value,self%style%default_units)

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_quantity_0 ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then; call convert_from_(value,units)
         else; call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine

   subroutine read_quantity_1(self,value)
   ! Read a number quantity (with optional units) into "value".
   ! WARNING: You must *not* be at the end of file to use this
   ! routine, since it will try to read the next token as a units specifier.
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, OUT :: value
      STR(len=STR_SIZE) :: units
      BIN :: known_unit
   ENSURE(self%action=="read","TEXTFILE:read_quantity_1 ... file does not have read action!")

      ! Read the value

      ! The following code is inherited from TEXTFILE
      call read_(self,value)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") call convert_from_(value,self%style%default_units)

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_quantity_1 ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then; call convert_from_(value,units)
         else; call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine

   subroutine read_quantity_2(self,value,error)
   ! Read a number quantity (with optional units) into "value".
   ! WARNING: You must *not* be at the end of file to use this
   ! routine, since it will try to read the next token as a units specifier.
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, OUT :: value
      REAL, OUT :: error
      STR(len=STR_SIZE) :: units
      BIN :: known_unit
   ENSURE(self%action=="read","TEXTFILE:read_quantity_2 ... file does not have read action!")

      ! Read the value & error
      call read_(self,value,error)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") then
         call convert_from_(value,self%style%default_units)
         call convert_from_(error,self%style%default_units)
      end if

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_quantity_2 ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then
            call convert_from_(value,units)
            call convert_from_(error,units)
         else
            call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine

!  ==============
!  Read VEC{TYPE}
!  ==============

   subroutine read_8(self,v)
   ! Read in a vector sequentially. Line breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_(self,v(i))
      end do
   end subroutine

   subroutine read_9(self,v)
   ! Read in a vector sequentially. Line breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(BIN,:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_(self,v(i))
      end do
   end subroutine

   subroutine read_10(self,v)
   ! Read in a vector sequentially. Line breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_(self,v(i))
      end do
   end subroutine

   subroutine read_11(self,v)
   ! Read in a vector sequentially. Line breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_(self,v(i))
      end do
   end subroutine

   subroutine read_12(self,v)
   ! Read in a vector sequentially. Line breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(CPX,:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_(self,v(i))
      end do
   end subroutine

   subroutine read_13(self,v,e)
   ! Read in a vector sequentially. Line breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: v
      VEC(REAL,:), OUT :: e
      INT :: i
      do i = 1,size(v)
         call read_(self,v(i),e(i))
      end do
   end subroutine

!  =========================
!  Read VEC{TYPE} quantities
!  =========================

   subroutine read_quantity_3(self,v)
   ! Read in a vector quantity (with units).  Line breaks are not
   ! significant.  WARNING: You must *not* be at the end of file to
   ! use this routine, since it will try to read the next token after
   ! the vector as a units specifier.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), OUT :: v
      STR(len=STR_SIZE) :: units
      BIN :: known_unit

      ! Read the vector

      ! The following code is inherited from TEXTFILE
      call read_(self,v)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") call convert_from_(v,self%style%default_units)

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_quantity_3 ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then; call convert_from_(v,units)
         else; call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine

   subroutine read_quantity_4(self,v)
   ! Read in a vector quantity (with units).  Line breaks are not
   ! significant.  WARNING: You must *not* be at the end of file to
   ! use this routine, since it will try to read the next token after
   ! the vector as a units specifier.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: v
      STR(len=STR_SIZE) :: units
      BIN :: known_unit

      ! Read the vector

      ! The following code is inherited from TEXTFILE
      call read_(self,v)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") call convert_from_(v,self%style%default_units)

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_quantity_4 ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then; call convert_from_(v,units)
         else; call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine

   subroutine read_quantity_5(self,v,e)
   ! Read in a vector quantity (with units).  Line breaks are not
   ! significant.  WARNING: You must *not* be at the end of file to
   ! use this routine, since it will try to read the next token after
   ! the vector as a units specifier.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: v
      VEC(REAL,:), OUT :: e
      STR(len=STR_SIZE) :: units
      BIN :: known_unit

      ! Read the vector
      call read_(self,v,e)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") then
         call convert_from_(v,self%style%default_units)
         call convert_from_(e,self%style%default_units)
      end if

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_quantity_5 ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then
            call convert_from_(v,units)
            call convert_from_(e,units)
         else
            call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine


!  ===============
!  Read VEC{TYPE}*
!  ===============

   function list_length(self,op,cl) result (res)
   ! Return the size of the list, by reading the input after an
   ! initial opening bracket "{" until a *matching* end bracket "}"
   ! token is found. Line breaks are not significant. "op" and "cl"
   ! are optional opening/closing delimniters, default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: op,cl
      INT :: res
      INT :: line,item,n
      STR(len=STR_SIZE) :: op_del,cl_del
      STR(len=STR_SIZE) :: word

      ! Set optional delimiters
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Store original pos
      line = line_number_(self)
      item = next_item_index_(self%buffer)

      ! Read open brace
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:list_length ... list does not begin with "//trim(op_del))

      ! Count the items
      n = 0
      res = 0
      do
         call read_(self,word)
         if (word==op_del) n = n+1  ! Allow open delim in list
         if (word==cl_del AND n==0) exit  ! Found cl_del but empty list
         if (word==cl_del AND n>0) n = n-1  ! Found cl_del non-empty
         res = res+1
      end do

      ! Reinstate original position
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)
   end function

!   read_ptr(v) :: template
!   ! Read in a vector pointer "v" sequentially. Line breaks are not
!   ! significant. The "v" vector pointer is created.
!      self :: INOUT
!      v :: V_TYPE?*
!
!      dim :: INT
!      word :: STR
!
!      ! Find out the list length
!      dim = .list_length
!      v.create(dim)
!
!      ! Read opening brace
!      .read(word)
!      ENSURE(word=="{","list does not begin with a {")
!
!      ! Read vector
!      .read(v)
!
!      ! Read closing brace
!      .read(word)
!      ENSURE(word=="}","list does not end with a }")
!
!   end
!
!   read_ptr(v) :: get_from(TEXTFILE, V_TYPE?=>VEC{STR}), leaky
!   ! Read in a vector pointer "v" sequentially. Line breaks are not
!   ! significant. The "v" vector pointer is created.
!   end
!
!   read_ptr(v) :: get_from(TEXTFILE, V_TYPE?=>VEC{BIN}), leaky
!   ! Read in a vector pointer "v" sequentially. Line breaks are not
!   ! significant. The "v" vector pointer is created.
!   end
!
!   read_ptr(v) :: leaky
!   ! Read in a fancy vector pointer "v" sequentially. Fancy because a
!   ! looping construct in understood. Line breaks are not significant.
!   ! The "v" vector pointer is created.
!      self :: INOUT
!      v :: VEC{INT}*
!
!      dim,f,l,i :: INT
!      word,first,second :: STR
!
!      ! Read opening brace and first two items
!      .read(word)
!      ENSURE(word=="{","list does not begin with a {")
!      .read(first)
!      .read(second)
!
!      ! If second item is looping construct ..
!      if (second=="...") then
!
!         ! Read last index & create "v"
!         .read(l)
!         f = first.to_int
!         dim = l - f + 1
!         v.create(abs(dim))
!
!         ! Define sequental "v"
!         if (dim>0) then; v = [ (i, i=f,l)    ]
!         else           ; v = [ (i, i=f,l,-1) ]
!         end
!
!         ! Read closing brace
!         .read(word)
!         ENSURE(word=="}","list does not end with a {")
!
!      ! Plain old VEC{INT}*
!      else
!
!         ! Backtrack before "{"
!         .move_to_previous_item
!         .move_to_previous_item
!         .move_to_previous_item
!
!         ! Read "v" in
!         .read_ptr_helper(v)
!
!      end
!
!   end
!
!   read_ptr_helper(v) :: get_from(TEXTFILE:read_ptr, V_TYPE?=>VEC{INT}), leaky
!   ! Read in a vector pointer "v" sequentially. Line breaks are not
!   ! significant. The "v" vector pointer is created.
!   end
!
!   read_ptr(v) :: get_from(TEXTFILE, V_TYPE?=>VEC{REAL}), leaky
!   ! Read in a vector pointer "v" sequentially. Line breaks are not
!   ! significant. The "v" vector pointer is created.
!   end
!
!   read_ptr(v) :: get_from(TEXTFILE, V_TYPE?=>VEC{CPX}), leaky
!   ! Read in a vector pointer "v" sequentially. Line breaks are not
!   ! significant. The "v" vector pointer is created.
!   end

!  ===============
!  Read VEC{TYPE}@
!  ===============

   subroutine read_all_0(self,v,op,cl)
   ! Read in an allocatable vector "v" sequentially. Line breaks are
   ! not significant. "v" is created. "op" and "cl" are optional
   ! opening and closing delimiters, by default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), allocatable, OUT :: v
      STR(len=*), optional, IN :: op
      STR(len=*), optional, IN :: cl
      INT :: dim
      STR(len=STR_SIZE) :: op_del,cl_del
      STR(len=STR_SIZE) :: word

      ! Set optional delimiters

      ! The following code is inherited from TEXTFILE
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Find out the list length
      dim = list_length_(self,op,cl)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:read_all_0 ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word==cl_del,"TEXTFILE:read_all_0 ... list does not end with a }")
   end subroutine

   subroutine read_all_1(self,v,op,cl)
   ! Read in an allocatable vector "v" sequentially. Line breaks are
   ! not significant. "v" is created. "op" and "cl" are optional
   ! opening and closing delimiters, by default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(BIN,:), allocatable, OUT :: v
      STR(len=*), optional, IN :: op
      STR(len=*), optional, IN :: cl
      INT :: dim
      STR(len=STR_SIZE) :: op_del,cl_del
      STR(len=STR_SIZE) :: word

      ! Set optional delimiters

      ! The following code is inherited from TEXTFILE
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Find out the list length
      dim = list_length_(self,op,cl)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:read_all_1 ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word==cl_del,"TEXTFILE:read_all_1 ... list does not end with a }")
   end subroutine

   subroutine read_all_2(self,v,op,cl)
   ! Read in an allocatable vector "v" sequentially. Fancy because a
   ! looping construct is recognised. Line breaks are not significant.
   ! "v" is created. "op" and "cl" are optional opening and closing
   ! delimiters, by default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), allocatable, OUT :: v
      STR(len=*), optional, IN :: op
      STR(len=*), optional, IN :: cl
      INT :: dim,f,l,i
      STR(len=STR_SIZE) :: word,first,second
      STR(len=STR_SIZE) :: op_del,cl_del

      ! Set optional delimiters
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Read opening brace and first two items
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:read_all_2 ... list does not begin with a "//trim(op_del))
      call read_(self,first)
      call read_(self,second)

      ! If second item is looping construct ..
      if (second=="...") then

         ! Read last index & create "v"
         call read_(self,l)
         f = to_int_(first)
         dim = l-f+1
         call create_(v,abs(dim))

         ! Define sequental "v"
         if (dim>0) then; v = [(i,i = f,l)]
         else; v = [(i,i = f,l,-1)]
         end if

         ! Read closing brace
         call read_(self,word)
         ENSURE(word==cl_del,"TEXTFILE:read_all_2 ... list does not end with a "//trim(cl_del))

      ! Plain old VEC{INT}*
      else

         ! Backtrack before openeing delimiter
         call move_to_previous_item_(self)
         call move_to_previous_item_(self)
         call move_to_previous_item_(self)

         ! Read "v" in
         call read_all_helper_(self,v,op,cl)
      end if
   end subroutine

   subroutine read_all_helper(self,v,op,cl)
   ! Read in an allocatable vector "v" sequentially. Line breaks are
   ! not significant. "v" is created. "op" and "cl" are optional
   ! opening and closing delimiters, by default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), allocatable, OUT :: v
      STR(len=*), optional, IN :: op
      STR(len=*), optional, IN :: cl
      INT :: dim
      STR(len=STR_SIZE) :: op_del,cl_del
      STR(len=STR_SIZE) :: word

      ! Set optional delimiters

      ! The following code is inherited from TEXTFILE
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Find out the list length
      dim = list_length_(self,op,cl)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:read_all_helper ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word==cl_del,"TEXTFILE:read_all_helper ... list does not end with a }")
   end subroutine

   subroutine read_all_3(self,v,op,cl)
   ! Read in an allocatable vector "v" sequentially. Line breaks are
   ! not significant. "v" is created. "op" and "cl" are optional
   ! opening and closing delimiters, by default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable, OUT :: v
      STR(len=*), optional, IN :: op
      STR(len=*), optional, IN :: cl
      INT :: dim
      STR(len=STR_SIZE) :: op_del,cl_del
      STR(len=STR_SIZE) :: word

      ! Set optional delimiters

      ! The following code is inherited from TEXTFILE
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Find out the list length
      dim = list_length_(self,op,cl)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:read_all_3 ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word==cl_del,"TEXTFILE:read_all_3 ... list does not end with a }")
   end subroutine

   subroutine read_all_4(self,v,op,cl)
   ! Read in an allocatable vector "v" sequentially. Line breaks are
   ! not significant. "v" is created. "op" and "cl" are optional
   ! opening and closing delimiters, by default "{" and "}".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(CPX,:), allocatable, OUT :: v
      STR(len=*), optional, IN :: op
      STR(len=*), optional, IN :: cl
      INT :: dim
      STR(len=STR_SIZE) :: op_del,cl_del
      STR(len=STR_SIZE) :: word

      ! Set optional delimiters

      ! The following code is inherited from TEXTFILE
      op_del = "{"
      cl_del = "}"
      if (present(op)) op_del = op
      if (present(cl)) cl_del = cl

      ! Find out the list length
      dim = list_length_(self,op,cl)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word==op_del,"TEXTFILE:read_all_4 ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word==cl_del,"TEXTFILE:read_all_4 ... list does not end with a }")
   end subroutine

!  ========================
!  Read VEC{TYPE}* quantity
!  ========================

!   read_ptr_quantity(v) :: template
!   ! Read in a vector pointer quantity (with units) sequentially.
!   ! Line breaks are not significant.  WARNING: You must *not* be at
!   ! the end of file to use this routine, since it will try to read
!   ! the next token after the vector as a units specifier.
!      self :: INOUT
!      v :: V_TYPE?*
!
!      units :: STR
!      known_unit :: BIN
!
!      ! Read the vector pointer
!      .read_ptr(v)
!
!      ! Convert from assumed default units

!         v.convert_from(.style.default_units)
!
!      ! Check for explicit units
!      if (NOT .at_EOF) then
!
!        ! Is there a known unit next?
!        .read(units)
!        known_unit = units.is_known_unit
!
!        ! Has conversion already been done?
!        DIE_IF(known_unit AND .style.default_units/=" ","two units specified!")
!
!        ! Convert from known units if we can ...
!        ! Or move back to where we were
!        if (known_unit) then; v.convert_from(units)
!        else;                 .move_to_previous_item
!        end
!
!      end
!
!      ! Reset default units
!      .style.default_units = " "
!
!   end
!
!   read_ptr_quantity(v) :: get_from(TEXTFILE, V_TYPE?=>VEC{REAL}), leaky
!   ! Read in a vector pointer quantity (with units) sequentially.
!   ! Line breaks are not significant.  WARNING: You must *not* be at
!   ! the end of file to use this routine, since it will try to read
!   end

!  ========================
!  Read VEC{TYPE}@ quantity
!  ========================

   subroutine read_all_quantity(self,v)
   ! Read in an allocatable vector quantity (with units) sequentially.
   ! Line breaks are not significant.  WARNING: You must *not* be at
   ! the end of file to use this routine, since it will try to read
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), allocatable, OUT :: v
      STR(len=STR_SIZE) :: units
      BIN :: known_unit

      ! Read the vector pointer

      ! The following code is inherited from TEXTFILE
      call read_all_(self,v)

      ! Convert from assumed default units
      if (self%style%default_units/=" ") then
         call convert_from_(v,self%style%default_units)
      end if

      ! Check for explicit units
      if (NOT at_EOF_(self)) then

         ! Is there a known unit next?
         call read_(self,units)
         known_unit = is_known_unit_(units)

         ! Has conversion already been done?
         DIE_IF(known_unit AND self%style%default_units/=" ","TEXTFILE:read_all_quantity ... two units specified!")

         ! Convert from known units if we can ...
         ! Or move back to where we were
         if (known_unit) then; call convert_from_(v,units)
         else; call move_to_previous_item_(self)
         end if
      end if

      ! Reset default units
      self%style%default_units = " "
   end subroutine

!  ===========================
!  Read VEC{T1}, VEC{T2} pairs
!  ===========================

   subroutine read_pair_0(self,v1,v2)
   ! Read in a pair of vectors sequentially, alternating. Line breaks
   ! are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), OUT :: v1
      VEC(INT,:), OUT :: v2
      INT :: i
   ENSURE(size(v1)==size(v2),"TEXTFILE:read_pair_0 ... incompatible vectors")

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v1)
         call read_(self,v1(i))
         call read_(self,v2(i))
      end do
   end subroutine

   subroutine read_pair_1(self,v1,v2)
   ! Read in a pair of vectors sequentially, alternating. Line breaks
   ! are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: v1
      VEC(REAL,:), OUT :: v2
      INT :: i
   ENSURE(size(v1)==size(v2),"TEXTFILE:read_pair_1 ... incompatible vectors")

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v1)
         call read_(self,v1(i))
         call read_(self,v2(i))
      end do
   end subroutine

!  Read VEC{T1}, VEC{T2}, VEC{T3} triples

   subroutine read_triple(self,v1,v2,v3)
   ! Read in a triple of vectors sequentially, alternating. Line
   ! breaks are not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: v1
      VEC(REAL,:), OUT :: v2
      VEC(REAL,:), OUT :: v3
      INT :: i
   ENSURE(size(v1)==size(v2),"TEXTFILE:read_triple ... incompatible vectors")
   ENSURE(size(v1)==size(v3),"TEXTFILE:read_triple ... incompatible vectors")

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v1)
         call read_(self,v1(i))
         call read_(self,v2(i))
         call read_(self,v3(i))
      end do
   end subroutine

!  ==============
!  Read MAT{TYPE}
!  ==============

   subroutine read_14(self,mx,by_column)
   ! Read in matrix "mx" by row. If "by_column" is present and TRUE it
   ! is read in sequentially by column. Line breaks not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), OUT :: mx
      BIN, optional :: by_column
      INT :: i,j
      BIN :: read_by_column

      ! Get input order

      ! The following code is inherited from TEXTFILE
      read_by_column = FALSE
      if (present(by_column)) read_by_column = by_column

      ! Read by column or (default) by row
      if (read_by_column) then
         do j = 1,size(mx,2)
            do i = 1,size(mx,1)
               call read_(self,mx(i,j))
            end do
         end do
      else
         do i = 1,size(mx,1)
            do j = 1,size(mx,2)
               call read_(self,mx(i,j))
            end do
         end do
      end if
   end subroutine

   subroutine read_15(self,mx,by_column)
   ! Read in matrix "mx" by row. If "by_column" is present and TRUE it
   ! is read in sequentially by column. Line breaks not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(BIN,:,:), OUT :: mx
      BIN, optional :: by_column
      INT :: i,j
      BIN :: read_by_column

      ! Get input order

      ! The following code is inherited from TEXTFILE
      read_by_column = FALSE
      if (present(by_column)) read_by_column = by_column

      ! Read by column or (default) by row
      if (read_by_column) then
         do j = 1,size(mx,2)
            do i = 1,size(mx,1)
               call read_(self,mx(i,j))
            end do
         end do
      else
         do i = 1,size(mx,1)
            do j = 1,size(mx,2)
               call read_(self,mx(i,j))
            end do
         end do
      end if
   end subroutine

   subroutine read_16(self,mx,by_column)
   ! Read in matrix "mx" by row. If "by_column" is present and TRUE it
   ! is read in sequentially by column. Line breaks not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(INT,:,:), OUT :: mx
      BIN, optional :: by_column
      INT :: i,j
      BIN :: read_by_column

      ! Get input order

      ! The following code is inherited from TEXTFILE
      read_by_column = FALSE
      if (present(by_column)) read_by_column = by_column

      ! Read by column or (default) by row
      if (read_by_column) then
         do j = 1,size(mx,2)
            do i = 1,size(mx,1)
               call read_(self,mx(i,j))
            end do
         end do
      else
         do i = 1,size(mx,1)
            do j = 1,size(mx,2)
               call read_(self,mx(i,j))
            end do
         end do
      end if
   end subroutine

   subroutine read_17(self,mx,by_column)
   ! Read in matrix "mx" by row. If "by_column" is present and TRUE it
   ! is read in sequentially by column. Line breaks not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: mx
      BIN, optional :: by_column
      INT :: i,j
      BIN :: read_by_column

      ! Get input order

      ! The following code is inherited from TEXTFILE
      read_by_column = FALSE
      if (present(by_column)) read_by_column = by_column

      ! Read by column or (default) by row
      if (read_by_column) then
         do j = 1,size(mx,2)
            do i = 1,size(mx,1)
               call read_(self,mx(i,j))
            end do
         end do
      else
         do i = 1,size(mx,1)
            do j = 1,size(mx,2)
               call read_(self,mx(i,j))
            end do
         end do
      end if
   end subroutine

   subroutine read_18(self,mx,format,items_per_line)
   ! Read a formatted matrix "mx" with fortran "format" and a given
   ! number of "items_per_line". Extra line items are skipped!
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: mx
      STR(len=*), IN :: format
      INT, IN :: items_per_line
      INT :: i,j,n
      call move_to_item_(self%buffer,1)
      call update_IO_cursor_(self)
      do i = 1,size(mx,1)

         ! Read a row i
         n = 0
         do j = 1,size(mx,2)
            call read_(self,mx(i,j),format)
            n = n+1
            if (n==items_per_line) exit
         end do

         ! Next non-empty line
         do
            call read_line_(self)
            if (NOT empty_(self%buffer)) exit
         end do
      end do
   end subroutine

   subroutine read_19(self,mx,by_column)
   ! Read in matrix "mx" by row. If "by_column" is present and TRUE it
   ! is read in sequentially by column. Line breaks not significant.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(CPX,:,:), OUT :: mx
      BIN, optional :: by_column
      INT :: i,j
      BIN :: read_by_column

      ! Get input order

      ! The following code is inherited from TEXTFILE
      read_by_column = FALSE
      if (present(by_column)) read_by_column = by_column

      ! Read by column or (default) by row
      if (read_by_column) then
         do j = 1,size(mx,2)
            do i = 1,size(mx,1)
               call read_(self,mx(i,j))
            end do
         end do
      else
         do i = 1,size(mx,1)
            do j = 1,size(mx,2)
               call read_(self,mx(i,j))
            end do
         end do
      end if
   end subroutine


!  ===============
!  Read MAT{TYPE}*
!  ===============

   subroutine get_mx_shape(self,dim)
   ! Returns dim(1) as the number of lines which are not blank, and
   ! dim(2) as the number of items per line between an opening and
   ! closing brace. It is an error if there are a different number of
   ! items per line on every non-blank line.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,2), OUT :: dim
      INT :: line,item,first_line
      STR(len=STR_SIZE) :: word
      INT :: dim1,dim2

      ! Store initial position
      line = line_number_(self)
      item = next_item_index_(self%buffer)

      ! Opening brace?
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:get_mx_shape ... list does not begin with a {")

      ! Initialize
      dim1 = 0
      dim2 = 0
      dim = [dim1,dim2]

      ! First token. Return if closing brace
      call read_(self,word)
      if (word=="}") return

      ! Line on which first item lies; at least one row
      first_line = line_number_(self)
      dim1 = 1

      ! The number of columns; it shouldn't change
      dim2 = n_line_items_(self)
      dim(2) = dim2

      ! Opening { is on the same line as first item
      if (first_line==line) dim2 = dim2-1

      ! Process all lines
      do

         ! Next line
         call skip_line_(self)

         ! Skip empty lines
         if (n_line_items_(self)==0) cycle

         ! Exit if first item of next non-blank line
         ! is closing brace
         call read_(self,word)
         if (word=="}") exit

         ! If not empty and not clsing brace, add another row
         dim1 = dim1+1
         dim2 = n_line_items_(self)

         ! If non matching dim2, it must be a closing brace
         ! (if so, exit) it is an error if not the same
         if (dim2>dim(2)) then
            dim2 = dim(2)
            call move_to_line_item_(self,dim2+1)
            DIE_IF(next_item_(self)/="}","TEXTFILE:get_mx_shape ... different number of line items on succesive lines")
            exit
         else
            DIE_IF(dim2/=dim(2),"TEXTFILE:get_mx_shape ... different number of line items on succesive lines")
         end if
      end do

      ! Return to initial position
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)

      ! Set the shape
      dim = [dim1,dim2]
   end subroutine

!
!   read_ptr(mx) :: template
!   ! Read in a matrix pointer "mx" sequentially. The shape is
!   ! induced from the formatting. Line breaks are significant!
!      self :: INOUT
!      mx :: MX_TYPE?*
!
!      word :: STR
!      dim :: VEC{INT}(2)
!
!      ! Get the shape
!      .get_mx_shape(dim)
!
!      ! Opening brace
!      .read(word)
!      ENSURE(word=="{","matrix does not begin with a {")
!
!      ! Read the matrix
!      mx.create(dim(1),dim(2))
!      .read(mx)
!
!      ! Closing brace
!      .read(word)
!      ENSURE(word=="}","expecting close bracket symbol: }")
!
!   end
!
!   read_ptr(mx) :: get_from(TEXTFILE, MX_TYPE?=>MAT{STR}), leaky
!   ! Read in a matrix pointer "mx" sequentially. The shape is
!   ! induced from the formatting. Line breaks are significant!
!   end
!
!   read_ptr(mx) :: get_from(TEXTFILE, MX_TYPE?=>MAT{BIN}), leaky
!   ! Read in a matrix pointer "mx" sequentially. The shape is
!   ! induced from the formatting. Line breaks are significant!
!   end
!
!   read_ptr(mx) :: get_from(TEXTFILE, MX_TYPE?=>MAT{INT}), leaky
!   ! Read in a matrix pointer "mx" sequentially. The shape is
!   ! induced from the formatting. Line breaks are significant!
!   end
!
!   read_ptr(mx) :: get_from(TEXTFILE, MX_TYPE?=>MAT{REAL}), leaky
!   ! Read in a matrix pointer "mx" sequentially. The shape is
!   ! induced from the formatting. Line breaks are significant!
!   end
!
!   read_ptr(mx) :: get_from(TEXTFILE, MX_TYPE?=>MAT{CPX}), leaky
!   ! Read in a matrix pointer "mx" sequentially. The shape is
!   ! induced from the formatting. Line breaks are significant!
!   end

!  ===============
!  Read MAT{TYPE}@
!  ===============

   subroutine read_all_5(self,mx)
   ! Read in a allocatable matrix "mx" sequentially. The shape is
   ! induced from the formatting. Line breaks are significant!
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), allocatable, OUT :: mx
      STR(len=STR_SIZE) :: word
      VEC(INT,2) :: dim

      ! Get the shape

      ! The following code is inherited from TEXTFILE
      call get_mx_shape_(self,dim)

      ! Opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_5 ... matrix does not begin with a {")

      ! Read the matrix
      call create_(mx,dim(1),dim(2))
      call read_(self,mx)

      ! Closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_5 ... expecting close bracket symbol: }")
   end subroutine

   subroutine read_all_6(self,mx)
   ! Read in a allocatable matrix "mx" sequentially. The shape is
   ! induced from the formatting. Line breaks are significant!
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(BIN,:,:), allocatable, OUT :: mx
      STR(len=STR_SIZE) :: word
      VEC(INT,2) :: dim

      ! Get the shape

      ! The following code is inherited from TEXTFILE
      call get_mx_shape_(self,dim)

      ! Opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_6 ... matrix does not begin with a {")

      ! Read the matrix
      call create_(mx,dim(1),dim(2))
      call read_(self,mx)

      ! Closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_6 ... expecting close bracket symbol: }")
   end subroutine

   subroutine read_all_7(self,mx)
   ! Read in a allocatable matrix "mx" sequentially. The shape is
   ! induced from the formatting. Line breaks are significant!
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(INT,:,:), allocatable, OUT :: mx
      STR(len=STR_SIZE) :: word
      VEC(INT,2) :: dim

      ! Get the shape

      ! The following code is inherited from TEXTFILE
      call get_mx_shape_(self,dim)

      ! Opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_7 ... matrix does not begin with a {")

      ! Read the matrix
      call create_(mx,dim(1),dim(2))
      call read_(self,mx)

      ! Closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_7 ... expecting close bracket symbol: }")
   end subroutine

   subroutine read_all_8(self,mx)
   ! Read in a allocatable matrix "mx" sequentially. The shape is
   ! induced from the formatting. Line breaks are significant!
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(REAL,:,:), allocatable, OUT :: mx
      STR(len=STR_SIZE) :: word
      VEC(INT,2) :: dim

      ! Get the shape

      ! The following code is inherited from TEXTFILE
      call get_mx_shape_(self,dim)

      ! Opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_8 ... matrix does not begin with a {")

      ! Read the matrix
      call create_(mx,dim(1),dim(2))
      call read_(self,mx)

      ! Closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_8 ... expecting close bracket symbol: }")
   end subroutine

   subroutine read_all_9(self,mx)
   ! Read in a allocatable matrix "mx" sequentially. The shape is
   ! induced from the formatting. Line breaks are significant!
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(CPX,:,:), allocatable, OUT :: mx
      STR(len=STR_SIZE) :: word
      VEC(INT,2) :: dim

      ! Get the shape

      ! The following code is inherited from TEXTFILE
      call get_mx_shape_(self,dim)

      ! Opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_9 ... matrix does not begin with a {")

      ! Read the matrix
      call create_(mx,dim(1),dim(2))
      call read_(self,mx)

      ! Closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_9 ... expecting close bracket symbol: }")
   end subroutine

!  =====================
!  Read VEC{EVEC{TYPE}}@
!  =====================

   subroutine read_20(self,v)
   ! Read in an VEC{EVEC{TYPE}} "v" sequentially. Line breaks are not
   ! significant. The "v" is created.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_all_(self,v(i)%element)
      end do
   end subroutine

   subroutine read_21(self,v)
   ! Read in an VEC{EVEC{TYPE}} "v" sequentially. Line breaks are not
   ! significant. The "v" is created.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EVEC_REAL_TYPE),:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_all_(self,v(i)%element)
      end do
   end subroutine

   function list_list_length_all(self) result (res)
   ! Return the size of an VEC{EVEC{INT}} list, by reading the input
   ! until an end bracket "}" token is found. Line breaks are not
   ! significant.
      type(TEXTFILE_TYPE), INOUT :: self
      INT :: res
      INT :: line,item
      STR(len=STR_SIZE) :: word
      VEC(STR(len=STR_SIZE),:), allocatable :: v

      ! Store original pos
      line = line_number_(self)
      item = next_item_index_(self%buffer)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:list_list_length_all ... list does not begin with {")

      ! Count enclosed lists
      res = 0
      do
         call read_(self,word)
         if (word=="}") exit
         call move_to_previous_item_(self)
         call read_all_(self,v)
         call destroy_(v)
         res = res+1
      end do

      ! Reinstate original position
      call move_to_line_(self,line)
      call move_to_line_item_(self,item)
   end function

   subroutine read_all_10(self,v)
   ! Read in a VEC{EVEC{TYPE}}@ "v" sequentially. Line breaks are not
   ! significant. The "v" is created.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EVEC_INT_TYPE),:), allocatable, OUT :: v
      STR(len=STR_SIZE) :: word
      INT :: dim

      ! Find out the list length

      ! The following code is inherited from TEXTFILE
      dim = list_list_length_all_(self)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_10 ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_10 ... list does not end with a {")
   end subroutine

   subroutine read_all_11(self,v)
   ! Read in a VEC{EVEC{TYPE}}@ "v" sequentially. Line breaks are not
   ! significant. The "v" is created.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EVEC_REAL_TYPE),:), allocatable, OUT :: v
      STR(len=STR_SIZE) :: word
      INT :: dim

      ! Find out the list length

      ! The following code is inherited from TEXTFILE
      dim = list_list_length_all_(self)
      call create_(v,dim)

      ! Read opening brace
      call read_(self,word)
      ENSURE(word=="{","TEXTFILE:read_all_11 ... list does not begin with a {")

      ! Read vector
      call read_(self,v)

      ! Read closing brace
      call read_(self,word)
      ENSURE(word=="}","TEXTFILE:read_all_11 ... list does not end with a {")
   end subroutine

!  ====================
!  Read VEC{EMAT{TYPE}}
!  ====================

   subroutine read_22(self,v)
   ! Read in an VEC{EMAT{TYPE}} "v" sequentially. Line breaks are not
   ! significant. The "v" is created.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EMAT_INT_TYPE),:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_all_(self,v(i)%element)
      end do
   end subroutine

   subroutine read_23(self,v)
   ! Read in an VEC{EMAT{TYPE}} "v" sequentially. Line breaks are not
   ! significant. The "v" is created.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), OUT :: v
      INT :: i

      ! The following code is inherited from TEXTFILE
      do i = 1,size(v)
         call read_all_(self,v(i)%element)
      end do
   end subroutine

!  ===============
!  Read MAT3{TYPE}
!  ===============

   subroutine read_24(self,mx)
   ! Read a MAT3{TYPE} matrix "mx". Indices for the element precede
   ! the element value.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: mx
      INT :: i1,i2,i3,j1,j2,j3

      ! The following code is inherited from TEXTFILE
      do i1 = 1,size(mx,1)
         do i2 = 1,size(mx,2)
            do i3 = 1,size(mx,3)
               call read_(self,j1)
               call read_(self,j2)
               call read_(self,j3)
               call read_(self,mx(j1,j2,j3))
            end do
         end do
      end do
   end subroutine

   subroutine read_25(self,mx)
   ! Read a MAT3{TYPE} matrix "mx". Indices for the element precede
   ! the element value.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), OUT :: mx
      INT :: i1,i2,i3,j1,j2,j3

      ! The following code is inherited from TEXTFILE
      do i1 = 1,size(mx,1)
         do i2 = 1,size(mx,2)
            do i3 = 1,size(mx,3)
               call read_(self,j1)
               call read_(self,j2)
               call read_(self,j3)
               call read_(self,mx(j1,j2,j3))
            end do
         end do
      end do
   end subroutine

!  ===============
!  Read MAT4{TYPE}
!  ===============

   subroutine read_26(self,mx)
   ! Read a MAT4{TYPE} matrix "mx". Indices for the element precede
   ! the element value.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), OUT :: mx
      INT :: i1,i2,i3,i4,j1,j2,j3,j4

      ! The following code is inherited from TEXTFILE
      do i1 = 1,size(mx,1)
         do i2 = 1,size(mx,2)
            do i3 = 1,size(mx,3)
               do i4 = 1,size(mx,4)
                  call read_(self,j1)
                  call read_(self,j2)
                  call read_(self,j3)
                  call read_(self,j4)
                  call read_(self,mx(j1,j2,j3,j4))
               end do
            end do
         end do
      end do
   end subroutine

   subroutine read_27(self,mx)
   ! Read a MAT4{TYPE} matrix "mx". Indices for the element precede
   ! the element value.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), OUT :: mx
      INT :: i1,i2,i3,i4,j1,j2,j3,j4

      ! The following code is inherited from TEXTFILE
      do i1 = 1,size(mx,1)
         do i2 = 1,size(mx,2)
            do i3 = 1,size(mx,3)
               do i4 = 1,size(mx,4)
                  call read_(self,j1)
                  call read_(self,j2)
                  call read_(self,j3)
                  call read_(self,j4)
                  call read_(self,mx(j1,j2,j3,j4))
               end do
            end do
         end do
      end do
   end subroutine

!  ===============
!  Read MAT5{TYPE}
!  ===============

   subroutine read_28(self,mx)
   ! Read a MAT5{TYPE} matrix "mx". Indices for the element precede
   ! the element value.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT5(REAL,:,:,:,:,:), OUT :: mx
      INT :: i1,i2,i3,i4,i5,j1,j2,j3,j4,j5

      ! The following code is inherited from TEXTFILE
      do i1 = 1,size(mx,1)
         do i2 = 1,size(mx,2)
            do i3 = 1,size(mx,3)
               do i4 = 1,size(mx,4)
                  do i5 = 1,size(mx,5)
                     call read_(self,j1)
                     call read_(self,j2)
                     call read_(self,j3)
                     call read_(self,j4)
                     call read_(self,j5)
                     call read_(self,mx(j1,j2,j3,j4,j5))
                  end do
               end do
            end do
         end do
      end do
   end subroutine

   subroutine read_29(self,mx)
   ! Read a MAT5{TYPE} matrix "mx". Indices for the element precede
   ! the element value.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), OUT :: mx
      INT :: i1,i2,i3,i4,i5,j1,j2,j3,j4,j5

      ! The following code is inherited from TEXTFILE
      do i1 = 1,size(mx,1)
         do i2 = 1,size(mx,2)
            do i3 = 1,size(mx,3)
               do i4 = 1,size(mx,4)
                  do i5 = 1,size(mx,5)
                     call read_(self,j1)
                     call read_(self,j2)
                     call read_(self,j3)
                     call read_(self,j4)
                     call read_(self,j5)
                     call read_(self,mx(j1,j2,j3,j4,j5))
                  end do
               end do
            end do
         end do
      end do
   end subroutine


!  ==========================
!  Read OPVECTOR and OPMATRIX
!  ==========================

   subroutine read_30(self,v,genre)
   ! Read OPVECTOR "v"
      type(TEXTFILE_TYPE), INOUT :: self
      type(OPVECTOR_TYPE), INOUT :: v
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: vgenre
      if (present(genre)) then; vgenre = genre
      else; vgenre = genre_(v)
      end if
      select case (vgenre)

      case ("r ")
         call destroy_(v,"r ")
         call create_(v,"r ")
         call look_for_(self,"Restricted part:",from=1)
         call read_line_(self)
         call read_(self,v%r)

      case ("u ")
         call destroy_(v,"u ")
         call create_(v,"u ")
         call look_for_(self,"Alpha part:",from=1)
         call read_line_(self)
         call read_(self,v%a)
         call look_for_(self,"Beta part:",from=1)
         call read_line_(self)
         call read_(self,v%b)

      case default
         DIE("TEXTFILE:read_30 ... unknown genre, "//trim(vgenre))
      end select
   end subroutine

   subroutine read_31(self,mx,genre,by_column)
   ! Read OPMATRIX "mx".
      type(TEXTFILE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: mx
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_column
      STR(len=STR_SIZE) :: mxgenre
      if (present(genre)) then; mxgenre = genre
      else; mxgenre = genre_(mx)
      end if
      select case (mxgenre)

      case ("r ")
         call destroy_(mx,"r ")
         call create_(mx,"r ")
         call look_for_(self,"Restricted part:")
         call read_line_(self)
         call read_(self,mx%r,by_column)

      case ("u ")
         call destroy_(mx,"u ")
         call create_(mx,"u ")
         call look_for_(self,"Alpha part:")
         call read_line_(self)
         call read_(self,mx%a,by_column)
         call look_for_(self,"Beta part:")
         call read_line_(self)
         call read_(self,mx%b,by_column)

      case ("gc")
         call destroy_(mx,"gc")
         call create_(mx,"gc")
         call look_for_(self,"General complex part:")
         call read_line_(self)
         call read_(self,mx%gc,by_column)

      case default
         DIE("TEXTFILE:read_31 ... unknown genre, "//trim(mxgenre))
      end select
   end subroutine


!  ===============
!  Output routines
!  ===============

!  Clearing, flushing, tabbing, dashing & plain text line

   subroutine clear_and_put_margin(self)
   ! Clear the buffer and put a margin in.
   ! Not PURE, because BUFFER:put_str is not -- it carries a live DIE.
      type(TEXTFILE_TYPE), INOUT :: self
      call clear_(self%buffer)
      if (self%style%margin_width==0) return
      if (self%buffer%item_start>0) return
      call put_(self%buffer,repeat(" ",self%style%margin_width))
   end subroutine


!  This important routine actually writes to the file

   subroutine flush_0(self)
   ! Flush the buffer to the output file.
   !
   ! In a parallel job only the I/O process writes, and nothing is broadcast:
   ! every process clears its own buffer and counts the record itself. So
   ! putting text and flushing make no collective calls (opening, closing and
   ! redirecting a file still do). See docs/TONTO_DEVELOPER_INFO.md section 1a.
      type(TEXTFILE_TYPE), INOUT :: self
   ENSURE(self%action=="write","TEXTFILE:flush_0 ... file does not have write action!")

      ! Write the buffer.string
      self%IO_status = 0
      if (IO_IS_ALLOWED) then
         ENSURE(is_open_io_(self),"TEXTFILE:flush_0 ... file "//trim(self%name)//" is not open!")
         write(unit=self%unit,iostat=self%IO_status,fmt="(a)") trim(self%buffer%string)
         DIE_IF(self%IO_status>0,"TEXTFILE:flush_0 ... flush error")
         TONTO_FLUSH_BUFFER(self%unit)
      end if
      self%record = self%record+1

      ! Write margin
      call clear_and_put_margin_(self)
   end subroutine

   subroutine flush_1(self,times)
   ! Flush the buffer multiple times to get extra carriage returns
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: times
      INT :: i
      do i = 1,times
         call flush_(self)
      end do
   end subroutine

   subroutine tab(self,real_fields,int_fields,bin_fields,width)
   ! Tab across the specified number of fields in the output buffer
   ! "real_fields", "int_fields" and "bin_fields" refer to the number of
   ! real, integer and logical fields to tab; "width" is the width of spaces
   ! to tab.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, optional, IN :: real_fields
      INT, optional, IN :: int_fields
      INT, optional, IN :: bin_fields
      INT, optional, IN :: width
      if (present(real_fields)) call put_(self%buffer,repeat(repeat(" ",self%style%real_width),real_fields))
      if (present(int_fields)) call put_(self%buffer,repeat(repeat(" ",self%style%int_width),int_fields))
      if (present(bin_fields)) call put_(self%buffer,repeat(repeat(" ",self%style%int_width),bin_fields))
      if (present(width)) call put_(self%buffer,repeat(" ",width))
   end subroutine

   subroutine dash(self,symbol,real_fields,int_fields,bin_fields,width)
   ! Put a dashed line into the buffer. "real_fields", "int_fields" and
   ! "bin_fields" refer to the number of real, integer and logical fields
   ! to draw the line over; "width" is the width of characters to draw the
   ! dashed line.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=1), optional, IN :: symbol
      INT, optional, IN :: real_fields
      INT, optional, IN :: int_fields
      INT, optional, IN :: bin_fields
      INT, optional, IN :: width
      STR(len=1) :: dsh
      dsh = "-"
      if (present(symbol)) dsh = symbol
      if (present(real_fields)) call put_(self%buffer,repeat(dsh,self%style%real_width*real_fields))
      if (present(int_fields)) call put_(self%buffer,repeat(dsh,self%style%int_width*int_fields))
      if (present(bin_fields)) call put_(self%buffer,repeat(dsh,self%style%int_width*bin_fields))
      if (present(width)) call put_(self%buffer,repeat(dsh,width))
      call flush_(self)
   end subroutine

   subroutine text(self,string,real_width,int_width,flush)
   ! Put text into the output buffer as is and flush, unless
   ! flush is present and FALSE. You can specify widths too.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: string
      BIN, optional, IN :: real_width
      BIN, optional, IN :: int_width
      BIN, optional, IN :: flush
      STR(len=self%style%int_width) :: int_string
      STR(len=self%style%real_width) :: real_string
      BIN :: use_real_width,use_int_width,do_flush
      use_real_width = FALSE
      if (present(real_width)) use_real_width = real_width
      use_int_width = FALSE
      if (present(int_width)) use_int_width = int_width
      do_flush = TRUE
      if (present(flush)) do_flush = flush
      if (use_real_width AND len(string)<=self%style%real_width) then
         real_string = string
         call put_(self%buffer,real_string)
      else if (use_int_width AND len(string)<=self%style%int_width) then
         int_string = string
         call put_(self%buffer,int_string)
      else
         call put_(self%buffer,string)
      end if
      if (do_flush) call flush_(self)
   end subroutine

   subroutine head(self,heading)
   ! Put "heading" out, underlined and overlined, and with 
   ! surrounding newlines. This is a Tonto style thing ...
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: heading
      call flush_(self)
      call text_(self,repeat("=",len(heading)))
      call text_(self,trim(heading))
      call text_(self,repeat("=",len(heading)))
      call flush_(self)
   end subroutine


!  ===========================
!  Put TYPE methods (no flush)
!  ===========================

!  Most everything is converted to a string and put into the buffer,
!  so comes through the important routine below, which is quite slow.

   subroutine put_0(self,string,left,center,int_width,width,blank)
   ! Put a "string" into the output buffer; if it is too big, put as is.
   ! NOTE: Head space is never altered, but trailing spaces are trimmed.
   ! . "left" ........ left justifies output         (default: right justified)
   ! . "center" ...... centers the output            (overrides "left")
   ! . "int_width" ... field width set to .int_width (default: .real_width)
   ! . "width" ....... sets field width in chars     (overrides "int_width")
   ! . "blank" ....... sets the field to be blank    (overrides everything)
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: string
      BIN, optional, IN :: left
      BIN, optional, IN :: center
      BIN, optional, IN :: int_width
      INT, optional, IN :: width
      BIN, optional, IN :: blank
      STR(len=STR_SIZE) :: tmpcpy
      INT :: wid,len
      BIN :: leftify,centrify,blnk
      if (NOT self%style%using_fields) then

         ! Put string as is, separated by a space
         ! Head space preserved, trailing space trimmed
         if (self%buffer%item_end==0) then; call put_(self%buffer,trim(string))
         else; call put_(self%buffer," "//trim(string))
         end if
      else

         ! Get the field width
         wid = self%style%real_width
         if (present(int_width)) then
            if (int_width) then
               wid = self%style%int_width
            end if
         end if
         if (present(width)) wid = width

         ! Whether to left-justify
         leftify = FALSE
         if (present(left)) leftify = left

         ! Whether to center
         centrify = FALSE
         if (present(center)) centrify = center

         ! Blank?
         blnk = FALSE
         if (present(blank)) then
            if (blank) then
               blnk = blank
            end if
         end if

         ! Sanity check
         DIE_IF(leftify AND centrify,"TEXTFILE:put_0 ... too many justifications")

         ! The actual string lenth
         len = len_trim(string)
         if (blnk) then

            ! Fill the field with a blank of withd "width"
            call put_(self%buffer,repeat(" ",wid))
         else if (len>wid) then

            ! If string too long, just put it in, with a space in
            ! front (unless it is the first item).
            if (self%buffer%item_end==0) then; call put_(self%buffer,trim(string))
            else; call put_(self%buffer," "//trim(string))
            end if
         else if (len==wid) then
            if (present(width)) then

               ! If it just fits, and the user specified the character
               ! width, put it in just as the user wanted
               call put_(self%buffer,trim(string))
            else

               ! If it just fits, and the user didn't specify width,
               ! put it in with a spacer in front
               call put_(self%buffer," "//trim(string))
            end if
         else if (centrify) then

            ! Centered
            tmpcpy = string
            call center_justify_(tmpcpy(1:wid))
            call put_(self%buffer,tmpcpy(1:wid))
         else

            ! Finally: put it in with the specified width
            ! This works for blanks too ...
            call put_(self%buffer,to_str_(string,wid,leftify))
         end if
      end if
   end subroutine

   subroutine put_1(self,value,left,center,real_width,width,blank)
   ! Put a "value" into the output buffer. If present and TRUE:
   ! . "left" will left justify the string (default is right justified)
   ! . "center" will center the string (overrides left-justification)
   ! . "real_width" sets width to .real_width (default width is .int_width)
   ! If present:
   ! . "width" sets the field_width (default width is .int_width)
   ! . "blank" ....... sets the field to be blank  (overrides everything)
      type(TEXTFILE_TYPE), INOUT :: self
      BIN, IN :: value
      BIN, optional, IN :: left
      BIN, optional, IN :: center
      BIN, optional, IN :: real_width
      INT, optional, IN :: width
      BIN, optional, IN :: blank
      BIN :: int_width

      ! The following code is inherited from TEXTFILE
      int_width = TRUE
      if (present(real_width)) int_width = NOT real_width

      ! Change to string, and put it out
      call put_(self,to_str_(value),left,center,int_width,width,blank)
   end subroutine

   subroutine put_2(self,value,left,center,real_width,width,blank)
   ! Put a "value" into the output buffer. If present and TRUE:
   ! . "left" will left justify the string (default is right justified)
   ! . "center" will center the string (overrides left-justification)
   ! . "real_width" sets width to .real_width (default width is .int_width)
   ! If present:
   ! . "width" sets the field_width (default width is .int_width)
   ! . "blank" ....... sets the field to be blank  (overrides everything)
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: value
      BIN, optional, IN :: left
      BIN, optional, IN :: center
      BIN, optional, IN :: real_width
      INT, optional, IN :: width
      BIN, optional, IN :: blank
      BIN :: int_width

      ! The following code is inherited from TEXTFILE
      int_width = TRUE
      if (present(real_width)) int_width = NOT real_width

      ! Change to string, and put it out
      call put_(self,to_str_(value),left,center,int_width,width,blank)
   end subroutine

   subroutine put_bit_string(self,value,left,center,int_width,width,blank)
   ! Put a "value" into the output buffer. If present and TRUE:
   ! . "int_width" sets width to .int_width (default width is .real_width)
   ! . "left" will left justify the string (default is right justified)
   ! . "center" will center the string (overrides left-justification)
   ! If present:
   ! . "width" sets the field_width (default width is .int_width)
   ! . "blank" ....... sets the field to be blank  (overrides everything)
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: value
      BIN, IN, optional :: left
      BIN, IN, optional :: center
      BIN, IN, optional :: int_width
      INT, IN, optional :: width
      BIN, optional, IN :: blank

      ! Change to string, and put it out
      call put_(self,to_bit_string_(value),left,center,int_width,width,blank)
   end subroutine

!  ==============
!  Put REAL & CPX
!  ==============

   subroutine put_3(self,value,left,center,int_width,fmt,width,precision)
   ! Put a real or complex "value" into the output buffer.
   ! If present and TRUE:
   ! . "left"   will left justify the string  (default: right-justified)
   ! . "center" will center the string        (default: left-justified)
   ! . "int_width" sets width to .int_width   (default: .real_width)
   ! If present:
   ! . "fmt"   sets the fortran fmt           (default: .real_fortran_fmt)
   ! . "width" sets the field_width           (default: .real_width)
   ! . "precision" sets the precision         (default: .real_precision)
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, IN :: value
      BIN, IN, optional :: left
      BIN, IN, optional :: center
      BIN, IN, optional :: int_width
      STR(len=*), IN, optional :: fmt
      INT, IN, optional :: width
      INT, IN, optional :: precision
      STR(len=STR_SIZE) :: sty
      INT :: wid0,wid,pre,nf

      ! Defaults

      ! The following code is inherited from TEXTFILE
      sty = self%style%real_fortran_fmt
      wid = self%style%real_width
      pre = self%style%real_precision

      ! Set int_width option
      if (present(int_width)) then
         if (int_width) then
            wid = self%style%int_width
            pre = 3
         end if
      end if

      ! Sensible width
      wid0 = wid

      ! Set options
      if (present(fmt)) sty = fmt
      if (present(width)) wid = width
      if (present(precision)) pre = precision

      ! Sensible width
      if (wid>0) wid0 = wid

      ! Change to string, and put it out
      nf = no_of_fields_per_value_(value)
      call put_(self,to_str_(value,sty,wid0,pre,left),left,center,FALSE,width=nf*wid)
   end subroutine

   subroutine put_4(self,value,left,center,int_width,fmt,width,precision)
   ! Put a real or complex "value" into the output buffer.
   ! If present and TRUE:
   ! . "left"   will left justify the string  (default: right-justified)
   ! . "center" will center the string        (default: left-justified)
   ! . "int_width" sets width to .int_width   (default: .real_width)
   ! If present:
   ! . "fmt"   sets the fortran fmt           (default: .real_fortran_fmt)
   ! . "width" sets the field_width           (default: .real_width)
   ! . "precision" sets the precision         (default: .real_precision)
      type(TEXTFILE_TYPE), INOUT :: self
      CPX, IN :: value
      BIN, IN, optional :: left
      BIN, IN, optional :: center
      BIN, IN, optional :: int_width
      STR(len=*), IN, optional :: fmt
      INT, IN, optional :: width
      INT, IN, optional :: precision
      STR(len=STR_SIZE) :: sty
      INT :: wid0,wid,pre,nf

      ! Defaults

      ! The following code is inherited from TEXTFILE
      sty = self%style%real_fortran_fmt
      wid = self%style%real_width
      pre = self%style%real_precision

      ! Set int_width option
      if (present(int_width)) then
         if (int_width) then
            wid = self%style%int_width
            pre = 3
         end if
      end if

      ! Sensible width
      wid0 = wid

      ! Set options
      if (present(fmt)) sty = fmt
      if (present(width)) wid = width
      if (present(precision)) pre = precision

      ! Sensible width
      if (wid>0) wid0 = wid

      ! Change to string, and put it out
      nf = no_of_fields_per_value_(value)
      call put_(self,to_str_(value,sty,wid0,pre,left),left,center,FALSE,width=nf*wid)
   end subroutine


!  ==============================
!  Put imprecise REAL(WITH_ERROR)
!  ==============================

   subroutine put_5(self,value,error,int_width,width,precision,max_cb,d2_err)
   ! Put a real "value" into the output buffer which has "error".
   ! You can specify the "width" (defualt .real_width) or the
   ! maximum "precision". The decimal points are lined up.
   ! WARNING: Make sure the error is larger than the precision!
      type(TEXTFILE_TYPE), INOUT :: self
      REAL, IN :: value
      REAL, IN :: error
      BIN, optional, IN :: int_width
      INT, optional, IN :: width
      INT, optional, IN :: precision
      INT, optional, IN :: max_cb
      BIN, optional, IN :: d2_err
      INT :: wid,pre
      STR(len=STR_SIZE) :: valstr

      ! Defaults
      wid = self%style%real_width
      pre = self%style%real_precision

      ! Set int_width option
      if (present(int_width)) then
         if (int_width) then
            wid = self%style%int_width
            pre = 3
         end if
      end if

      ! Set options
      if (present(width)) wid = width
      if (present(precision)) pre = precision

      ! Change to string, put out like 1.002(19)
      call get_str_(value,valstr(1:wid),error,wid,pre,max_cb,d2_err)  ! aligned
      call put_(self%buffer,valstr(1:wid))
   end subroutine

!  =============
!  Put VEC{TYPE}
!  =============

   ! ================================================================
   ! Auto field-width helpers used by the "put" routines below.
   ! The auto-width line in each "put" was previously a single shared
   ! call, which forced a decimal-places notion onto integer/string
   ! types where it is meaningless (and made real widths depend on the
   ! compiler's list-directed formatting). Instead we dispatch on the
   ! element type:
   !  . non-real/complex : width = natural string length + spacing.
   !  . real/complex     : width follows .style.real_precision, so
   !                       auto-width tracks the printed precision at
   !                       runtime and is compiler-independent.
   ! The real/complex versions are dedicated routines (no get_from).
   ! ================================================================

   subroutine get_auto_width_0(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      VEC(STR(len=*),:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_1(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      VEC(BIN,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_2(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      VEC(INT,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_3(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      MAT(STR(len=*),:,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_4(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      MAT(BIN,:,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_5(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      MAT(INT,:,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_6(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      MAT3(INT,:,:,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_7(self,x,rwid)
      type(TEXTFILE_TYPE), IN :: self
      MAT4(INT,:,:,:,:), IN :: x
      INT, OUT :: rwid

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(x,rwid,self%style%spacing)
   end subroutine

   subroutine get_auto_width_8(self,x,rwid)
   ! REAL vector: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      VEC(REAL,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_9(self,x,rwid)
   ! CPX vector: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      VEC(CPX,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_10(self,x,rwid)
   ! REAL matrix: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      MAT(REAL,:,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_11(self,x,rwid)
   ! CPX matrix: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      MAT(CPX,:,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_12(self,x,rwid)
   ! REAL 3-matrix: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      MAT3(REAL,:,:,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_13(self,x,rwid)
   ! CPX 3-matrix: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      MAT3(CPX,:,:,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_14(self,x,rwid)
   ! REAL 4-matrix: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      MAT4(REAL,:,:,:,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine get_auto_width_15(self,x,rwid)
   ! CPX 4-matrix: field width follows .style.real_precision.
      type(TEXTFILE_TYPE), IN :: self
      MAT4(CPX,:,:,:,:), IN :: x
      INT, OUT :: rwid
      call get_max_str_length_(x,rwid,self%style%real_precision,self%style%spacing)
   end subroutine

   subroutine put_6(self,v,by_row,by_column,left,width,auto_width)
   ! Put VEC{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: v
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: left
      INT, optional, IN :: width
      BIN, optional, IN :: auto_width
      INT :: i,f,l,dim
      INT :: iwid,rwid,nfpv
      INT :: block,n_block,fields
      BIN :: column,auto
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_6 ... row and column options present!")

      ! Vector length

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      WARN_IF(dim==0,"TEXTFILE:put_6 ... zero sized dimension!")

      ! Extract options
      column = TRUE
      if (present(by_row)) column = NOT by_row
      if (present(by_column)) column = by_column
      iwid = str_length_(dim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(v(1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width (dispatched by element type)
      if (auto) call get_auto_width_(self,v,rwid)
      if (column) then

         ! =================================
         ! Put vector "v" as vertical column
         ! =================================
         f = lbound(v,1)
         l = ubound(v,1)
         if (self%style%using_array_labels) then

            ! With labels
            do i = f,l
               call put_(self,i,left,width=nfpv*iwid)
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         else

            ! No labels
            do i = f,l
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         end if
      else

         ! ================================
         ! Put vector "v" as horizontal row
         ! ================================
         fields = self%style%n_fields_per_line
         if (fields==0) fields = dim
         n_block = int((dim-0.1)/fields)+1

         ! Put vector "v" as horizontal row
         if (self%style%using_array_labels) then

            ! Use labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,i,left,width=nfpv*rwid)
               end do
               call flush_(self,2)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
               call flush_(self)
            end do
         else

            ! NO labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
            end do
            call flush_(self)
         end if
      end if
   end subroutine

   subroutine put_7(self,v,by_row,by_column,left,width,auto_width)
   ! Put VEC{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(BIN,:), IN :: v
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: left
      INT, optional, IN :: width
      BIN, optional, IN :: auto_width
      INT :: i,f,l,dim
      INT :: iwid,rwid,nfpv
      INT :: block,n_block,fields
      BIN :: column,auto
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_7 ... row and column options present!")

      ! Vector length

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      WARN_IF(dim==0,"TEXTFILE:put_7 ... zero sized dimension!")

      ! Extract options
      column = TRUE
      if (present(by_row)) column = NOT by_row
      if (present(by_column)) column = by_column
      iwid = str_length_(dim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(v(1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width (dispatched by element type)
      if (auto) call get_auto_width_(self,v,rwid)
      if (column) then

         ! =================================
         ! Put vector "v" as vertical column
         ! =================================
         f = lbound(v,1)
         l = ubound(v,1)
         if (self%style%using_array_labels) then

            ! With labels
            do i = f,l
               call put_(self,i,left,width=nfpv*iwid)
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         else

            ! No labels
            do i = f,l
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         end if
      else

         ! ================================
         ! Put vector "v" as horizontal row
         ! ================================
         fields = self%style%n_fields_per_line
         if (fields==0) fields = dim
         n_block = int((dim-0.1)/fields)+1

         ! Put vector "v" as horizontal row
         if (self%style%using_array_labels) then

            ! Use labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,i,left,width=nfpv*rwid)
               end do
               call flush_(self,2)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
               call flush_(self)
            end do
         else

            ! NO labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
            end do
            call flush_(self)
         end if
      end if
   end subroutine

   subroutine put_8(self,v,by_row,by_column,left,width,auto_width)
   ! Put VEC{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), IN :: v
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: left
      INT, optional, IN :: width
      BIN, optional, IN :: auto_width
      INT :: i,f,l,dim
      INT :: iwid,rwid,nfpv
      INT :: block,n_block,fields
      BIN :: column,auto
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_8 ... row and column options present!")

      ! Vector length

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      WARN_IF(dim==0,"TEXTFILE:put_8 ... zero sized dimension!")

      ! Extract options
      column = TRUE
      if (present(by_row)) column = NOT by_row
      if (present(by_column)) column = by_column
      iwid = str_length_(dim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(v(1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width (dispatched by element type)
      if (auto) call get_auto_width_(self,v,rwid)
      if (column) then

         ! =================================
         ! Put vector "v" as vertical column
         ! =================================
         f = lbound(v,1)
         l = ubound(v,1)
         if (self%style%using_array_labels) then

            ! With labels
            do i = f,l
               call put_(self,i,left,width=nfpv*iwid)
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         else

            ! No labels
            do i = f,l
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         end if
      else

         ! ================================
         ! Put vector "v" as horizontal row
         ! ================================
         fields = self%style%n_fields_per_line
         if (fields==0) fields = dim
         n_block = int((dim-0.1)/fields)+1

         ! Put vector "v" as horizontal row
         if (self%style%using_array_labels) then

            ! Use labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,i,left,width=nfpv*rwid)
               end do
               call flush_(self,2)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
               call flush_(self)
            end do
         else

            ! NO labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
            end do
            call flush_(self)
         end if
      end if
   end subroutine

   subroutine put_9(self,v,by_row,by_column,left,width,auto_width)
   ! Put VEC{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: left
      INT, optional, IN :: width
      BIN, optional, IN :: auto_width
      INT :: i,f,l,dim
      INT :: iwid,rwid,nfpv
      INT :: block,n_block,fields
      BIN :: column,auto
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_9 ... row and column options present!")

      ! Vector length

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      WARN_IF(dim==0,"TEXTFILE:put_9 ... zero sized dimension!")

      ! Extract options
      column = TRUE
      if (present(by_row)) column = NOT by_row
      if (present(by_column)) column = by_column
      iwid = str_length_(dim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(v(1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width (dispatched by element type)
      if (auto) call get_auto_width_(self,v,rwid)
      if (column) then

         ! =================================
         ! Put vector "v" as vertical column
         ! =================================
         f = lbound(v,1)
         l = ubound(v,1)
         if (self%style%using_array_labels) then

            ! With labels
            do i = f,l
               call put_(self,i,left,width=nfpv*iwid)
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         else

            ! No labels
            do i = f,l
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         end if
      else

         ! ================================
         ! Put vector "v" as horizontal row
         ! ================================
         fields = self%style%n_fields_per_line
         if (fields==0) fields = dim
         n_block = int((dim-0.1)/fields)+1

         ! Put vector "v" as horizontal row
         if (self%style%using_array_labels) then

            ! Use labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,i,left,width=nfpv*rwid)
               end do
               call flush_(self,2)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
               call flush_(self)
            end do
         else

            ! NO labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
            end do
            call flush_(self)
         end if
      end if
   end subroutine

   subroutine put_10(self,v,by_row,by_column,left,width,auto_width)
   ! Put VEC{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: left
      INT, optional, IN :: width
      BIN, optional, IN :: auto_width
      INT :: i,f,l,dim
      INT :: iwid,rwid,nfpv
      INT :: block,n_block,fields
      BIN :: column,auto
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_10 ... row and column options present!")

      ! Vector length

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      WARN_IF(dim==0,"TEXTFILE:put_10 ... zero sized dimension!")

      ! Extract options
      column = TRUE
      if (present(by_row)) column = NOT by_row
      if (present(by_column)) column = by_column
      iwid = str_length_(dim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(v(1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width (dispatched by element type)
      if (auto) call get_auto_width_(self,v,rwid)
      if (column) then

         ! =================================
         ! Put vector "v" as vertical column
         ! =================================
         f = lbound(v,1)
         l = ubound(v,1)
         if (self%style%using_array_labels) then

            ! With labels
            do i = f,l
               call put_(self,i,left,width=nfpv*iwid)
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         else

            ! No labels
            do i = f,l
               call put_(self,v(i),left,width=rwid)
               call flush_(self)
            end do
         end if
      else

         ! ================================
         ! Put vector "v" as horizontal row
         ! ================================
         fields = self%style%n_fields_per_line
         if (fields==0) fields = dim
         n_block = int((dim-0.1)/fields)+1

         ! Put vector "v" as horizontal row
         if (self%style%using_array_labels) then

            ! Use labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,i,left,width=nfpv*rwid)
               end do
               call flush_(self,2)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
               call flush_(self)
            end do
         else

            ! NO labels
            do block = 1,n_block
               f = 1+fields*(block-1)
               l = min(f+fields-1,dim)
               if (block>1) call flush_(self)
               do i = f,l
                  call put_(self,v(i),left,width=rwid)
               end do
            end do
            call flush_(self)
         end if
      end if
   end subroutine

   subroutine put_11(self,v,row_label,col_label,left)
   ! Put VEC{TYPE} into output as column, with "row_labels" and a
   ! single "col_label".
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      VEC(STR(len=*),:), IN :: row_label
      STR(len=*), IN :: col_label
      BIN, IN, optional :: left
      INT :: wid,widc,widr,i

      ! Warning ...
   WARN_IF(size(v)==0,"TEXTFILE:put_11 ... zero sized dimensiont!")

      ! Get the width

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(row_label,widr,self%style%spacing)
      call get_str_length_(col_label,widc,spaces=0)
      wid = max(widr,widc,self%style%real_width)

      ! Tab column width & put column label
      call tab_(self,width=wid)
      call put_(self,col_label,left,width=wid)
      call flush_(self,2)

      ! Put vector "v" as vertical column
      do i = 1,size(v)
         call put_(self,row_label(i),left,width=wid)
         call put_(self,v(i),left,width=wid)
         call flush_(self)
      end do
   end subroutine


!  These routines avoid the buffer.string and output directly to the file.

   subroutine parallel_put_0(self,v)
   ! Parallel put VEC{INT} "v" directly into the output file.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), IN :: v
      INT :: i
      STR(len=STR_SIZE) :: fmt

      ! One element per line
      fmt = int_format_(self,n_fields=1,width=2*self%style%int_width,with_parenthesis=TRUE)

      ! Write the array
      do i = 1,size(v)
         write(unit=self%unit,fmt=fmt) v(i)
         call increment_record_(self)
      end do
   end subroutine

   subroutine parallel_put_1(self,v)
   ! Parallel put VEC{INT} "v" directly into the output file.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      INT :: i
      STR(len=STR_SIZE) :: fmt

      ! One element per line
      fmt = real_format_(self,n_fields=1,width=2*self%style%real_width,precision=2*self%style%real_precision,with_parenthesis=TRUE)

      ! Write the array
      do i = 1,size(v)
         write(unit=self%unit,fmt=fmt) v(i)
         call increment_record_(self)
      end do
   end subroutine

   subroutine parallel_put_2(self,v)
   ! Parallel put VEC{INT} "v" directly into the output file.
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: v
      INT :: i
      STR(len=STR_SIZE) :: fmt

      ! One element per line
      fmt = real_format_(self,n_fields=2,width=2*self%style%real_width,precision=2*self%style%real_precision,with_parenthesis=TRUE)

      ! Write the array
      do i = 1,size(v)
         write(unit=self%unit,fmt=fmt) v(i)
         call increment_record_(self)
      end do
   end subroutine

!  =============
!  Put MAT{TYPE}
!  =============

   subroutine put_12(self,mx,by_row,by_column,transpose,left,width,auto_width)
   ! Put MAT{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), IN :: mx
      BIN, IN, optional :: by_row
      BIN, IN, optional :: by_column
      BIN, IN, optional :: transpose
      BIN, IN, optional :: left
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      BIN :: row,column,trans,auto
      INT :: i,j,dim1,dim2,maxdim,iwid,rwid,nfpv,block,n_block,fields,f,l

      ! Dimensions
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_12 ... row and column options present!")
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_12 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)

      ! Extract options
      row = FALSE
      if (present(by_row)) row = by_row
      column = FALSE
      if (present(by_column)) column = by_column
      trans = FALSE
      if (present(transpose)) trans = transpose

      ! Width
      maxdim = max(dim1,dim2)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(mx(1,1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides real_width
      if (auto) call get_auto_width_(self,mx,rwid)

      ! No of fields per line
      fields = self%style%n_fields_per_line
      if (column) then

         ! Put columns as rows, new line for new column
         ! ... if needed
         if (fields==0) fields = dim1
         do j = 1,dim2
            do i = 1,dim1
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(i,fields)==0) call flush_(self)
            end do
            if (mod(i,fields)/=0) call flush_(self)
         end do
      else if (row) then

         ! Put as rows, new line for new row
         if (fields==0) fields = dim2
         do i = 1,dim1
            do j = 1,dim2
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(j,fields)==0) call flush_(self)
            end do
            if (mod(j,fields)/=0) call flush_(self)
         end do
      else
         if (NOT trans) then  ! NO TRANSPOSE
            if (NOT self%style%using_fields) then
               do i = 1,dim1
                  do j = 1,dim2
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim2
               n_block = int((dim2-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do j = f,l
                        call put_(self,j,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do i = 1,dim1
                        call put_(self,i,left,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     do i = 1,dim1
                        call tab_(self,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         else  ! TRANSPOSE
            if (NOT self%style%using_fields) then
               do j = 1,dim2
                  do i = 1,dim1
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim1
               n_block = int((dim1-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do i = f,l
                        call put_(self,i,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do j = 1,dim2
                        call put_(self,j,left,width=iwid)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     do j = 1,dim2
                        call tab_(self,int_fields=1)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         end if
      end if
   end subroutine

   subroutine put_13(self,mx,by_row,by_column,transpose,left,width,auto_width)
   ! Put MAT{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(BIN,:,:), IN :: mx
      BIN, IN, optional :: by_row
      BIN, IN, optional :: by_column
      BIN, IN, optional :: transpose
      BIN, IN, optional :: left
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      BIN :: row,column,trans,auto
      INT :: i,j,dim1,dim2,maxdim,iwid,rwid,nfpv,block,n_block,fields,f,l

      ! Dimensions
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_13 ... row and column options present!")
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_13 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)

      ! Extract options
      row = FALSE
      if (present(by_row)) row = by_row
      column = FALSE
      if (present(by_column)) column = by_column
      trans = FALSE
      if (present(transpose)) trans = transpose

      ! Width
      maxdim = max(dim1,dim2)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(mx(1,1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides real_width
      if (auto) call get_auto_width_(self,mx,rwid)

      ! No of fields per line
      fields = self%style%n_fields_per_line
      if (column) then

         ! Put columns as rows, new line for new column
         ! ... if needed
         if (fields==0) fields = dim1
         do j = 1,dim2
            do i = 1,dim1
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(i,fields)==0) call flush_(self)
            end do
            if (mod(i,fields)/=0) call flush_(self)
         end do
      else if (row) then

         ! Put as rows, new line for new row
         if (fields==0) fields = dim2
         do i = 1,dim1
            do j = 1,dim2
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(j,fields)==0) call flush_(self)
            end do
            if (mod(j,fields)/=0) call flush_(self)
         end do
      else
         if (NOT trans) then  ! NO TRANSPOSE
            if (NOT self%style%using_fields) then
               do i = 1,dim1
                  do j = 1,dim2
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim2
               n_block = int((dim2-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do j = f,l
                        call put_(self,j,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do i = 1,dim1
                        call put_(self,i,left,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     do i = 1,dim1
                        call tab_(self,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         else  ! TRANSPOSE
            if (NOT self%style%using_fields) then
               do j = 1,dim2
                  do i = 1,dim1
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim1
               n_block = int((dim1-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do i = f,l
                        call put_(self,i,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do j = 1,dim2
                        call put_(self,j,left,width=iwid)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     do j = 1,dim2
                        call tab_(self,int_fields=1)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         end if
      end if
   end subroutine

   subroutine put_14(self,mx,by_row,by_column,transpose,left,width,auto_width)
   ! Put MAT{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: mx
      BIN, IN, optional :: by_row
      BIN, IN, optional :: by_column
      BIN, IN, optional :: transpose
      BIN, IN, optional :: left
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      BIN :: row,column,trans,auto
      INT :: i,j,dim1,dim2,maxdim,iwid,rwid,nfpv,block,n_block,fields,f,l

      ! Dimensions
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_14 ... row and column options present!")
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_14 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)

      ! Extract options
      row = FALSE
      if (present(by_row)) row = by_row
      column = FALSE
      if (present(by_column)) column = by_column
      trans = FALSE
      if (present(transpose)) trans = transpose

      ! Width
      maxdim = max(dim1,dim2)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(mx(1,1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides real_width
      if (auto) call get_auto_width_(self,mx,rwid)

      ! No of fields per line
      fields = self%style%n_fields_per_line
      if (column) then

         ! Put columns as rows, new line for new column
         ! ... if needed
         if (fields==0) fields = dim1
         do j = 1,dim2
            do i = 1,dim1
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(i,fields)==0) call flush_(self)
            end do
            if (mod(i,fields)/=0) call flush_(self)
         end do
      else if (row) then

         ! Put as rows, new line for new row
         if (fields==0) fields = dim2
         do i = 1,dim1
            do j = 1,dim2
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(j,fields)==0) call flush_(self)
            end do
            if (mod(j,fields)/=0) call flush_(self)
         end do
      else
         if (NOT trans) then  ! NO TRANSPOSE
            if (NOT self%style%using_fields) then
               do i = 1,dim1
                  do j = 1,dim2
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim2
               n_block = int((dim2-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do j = f,l
                        call put_(self,j,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do i = 1,dim1
                        call put_(self,i,left,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     do i = 1,dim1
                        call tab_(self,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         else  ! TRANSPOSE
            if (NOT self%style%using_fields) then
               do j = 1,dim2
                  do i = 1,dim1
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim1
               n_block = int((dim1-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do i = f,l
                        call put_(self,i,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do j = 1,dim2
                        call put_(self,j,left,width=iwid)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     do j = 1,dim2
                        call tab_(self,int_fields=1)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         end if
      end if
   end subroutine

   subroutine put_15(self,mx,by_row,by_column,transpose,left,width,auto_width)
   ! Put MAT{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: mx
      BIN, IN, optional :: by_row
      BIN, IN, optional :: by_column
      BIN, IN, optional :: transpose
      BIN, IN, optional :: left
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      BIN :: row,column,trans,auto
      INT :: i,j,dim1,dim2,maxdim,iwid,rwid,nfpv,block,n_block,fields,f,l

      ! Dimensions
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_15 ... row and column options present!")
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_15 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)

      ! Extract options
      row = FALSE
      if (present(by_row)) row = by_row
      column = FALSE
      if (present(by_column)) column = by_column
      trans = FALSE
      if (present(transpose)) trans = transpose

      ! Width
      maxdim = max(dim1,dim2)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(mx(1,1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides real_width
      if (auto) call get_auto_width_(self,mx,rwid)

      ! No of fields per line
      fields = self%style%n_fields_per_line
      if (column) then

         ! Put columns as rows, new line for new column
         ! ... if needed
         if (fields==0) fields = dim1
         do j = 1,dim2
            do i = 1,dim1
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(i,fields)==0) call flush_(self)
            end do
            if (mod(i,fields)/=0) call flush_(self)
         end do
      else if (row) then

         ! Put as rows, new line for new row
         if (fields==0) fields = dim2
         do i = 1,dim1
            do j = 1,dim2
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(j,fields)==0) call flush_(self)
            end do
            if (mod(j,fields)/=0) call flush_(self)
         end do
      else
         if (NOT trans) then  ! NO TRANSPOSE
            if (NOT self%style%using_fields) then
               do i = 1,dim1
                  do j = 1,dim2
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim2
               n_block = int((dim2-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do j = f,l
                        call put_(self,j,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do i = 1,dim1
                        call put_(self,i,left,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     do i = 1,dim1
                        call tab_(self,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         else  ! TRANSPOSE
            if (NOT self%style%using_fields) then
               do j = 1,dim2
                  do i = 1,dim1
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim1
               n_block = int((dim1-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do i = f,l
                        call put_(self,i,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do j = 1,dim2
                        call put_(self,j,left,width=iwid)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     do j = 1,dim2
                        call tab_(self,int_fields=1)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         end if
      end if
   end subroutine

   subroutine put_16(self,mx,by_row,by_column,transpose,left,width,auto_width)
   ! Put MAT{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: mx
      BIN, IN, optional :: by_row
      BIN, IN, optional :: by_column
      BIN, IN, optional :: transpose
      BIN, IN, optional :: left
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      BIN :: row,column,trans,auto
      INT :: i,j,dim1,dim2,maxdim,iwid,rwid,nfpv,block,n_block,fields,f,l

      ! Dimensions
   ENSURE(NOT (present(by_row) AND present(by_column)),"TEXTFILE:put_16 ... row and column options present!")
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_16 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)

      ! Extract options
      row = FALSE
      if (present(by_row)) row = by_row
      column = FALSE
      if (present(by_column)) column = by_column
      trans = FALSE
      if (present(transpose)) trans = transpose

      ! Width
      maxdim = max(dim1,dim2)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      nfpv = no_of_fields_per_value_(mx(1,1))
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides real_width
      if (auto) call get_auto_width_(self,mx,rwid)

      ! No of fields per line
      fields = self%style%n_fields_per_line
      if (column) then

         ! Put columns as rows, new line for new column
         ! ... if needed
         if (fields==0) fields = dim1
         do j = 1,dim2
            do i = 1,dim1
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(i,fields)==0) call flush_(self)
            end do
            if (mod(i,fields)/=0) call flush_(self)
         end do
      else if (row) then

         ! Put as rows, new line for new row
         if (fields==0) fields = dim2
         do i = 1,dim1
            do j = 1,dim2
               call put_(self,mx(i,j),left,width=rwid)
               if (mod(j,fields)==0) call flush_(self)
            end do
            if (mod(j,fields)/=0) call flush_(self)
         end do
      else
         if (NOT trans) then  ! NO TRANSPOSE
            if (NOT self%style%using_fields) then
               do i = 1,dim1
                  do j = 1,dim2
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim2
               n_block = int((dim2-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do j = f,l
                        call put_(self,j,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do i = 1,dim1
                        call put_(self,i,left,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim2)
                     if (block>1) call flush_(self)
                     do i = 1,dim1
                        call tab_(self,width=iwid)
                        do j = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         else  ! TRANSPOSE
            if (NOT self%style%using_fields) then
               do j = 1,dim2
                  do i = 1,dim1
                     call put_(self,mx(i,j),left,width=rwid)
                  end do
                  call flush_(self)
               end do
            else
               if (fields==0) fields = dim1
               n_block = int((dim1-0.1)/fields)+1
               if (self%style%using_array_labels) then

                  ! Put in row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     call tab_(self,width=iwid)
                     do i = f,l
                        call put_(self,i,left,width=nfpv*rwid)
                     end do
                     call flush_(self,2)
                     do j = 1,dim2
                        call put_(self,j,left,width=iwid)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               else

                  ! NO row & column labels
                  do block = 1,n_block
                     f = 1+fields*(block-1)
                     l = min(f+fields-1,dim1)
                     if (block>1) call flush_(self)
                     do j = 1,dim2
                        call tab_(self,int_fields=1)
                        do i = f,l
                           call put_(self,mx(i,j),left,width=rwid)
                        end do
                        call flush_(self)
                     end do
                  end do
               end if
            end if
         end if
      end if
   end subroutine

   subroutine put_17(self,mx,row_label,col_label,left)
   ! Put MAT{TYPE} into output with "row_labels" and "column_labels"
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: mx
      VEC(STR(len=*),:), IN :: row_label
      VEC(STR(len=*),:), IN :: col_label
      BIN, IN, optional :: left
      INT :: wid,widc,widr,i,j,block,n_block,fields,f,l

      ! Warning ...
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_17 ... zero sized dimensions present!")

      ! Get the width

      ! The following code is inherited from TEXTFILE
      call get_max_str_length_(row_label,widr,self%style%spacing)
      call get_max_str_length_(col_label,widc,self%style%spacing)
      wid = max(widr,widc,self%style%real_width)

      ! No of fields per line
      fields = self%style%n_fields_per_line
      if (fields==0) fields = size(mx,2)

      ! No of matrix column-blocks
      n_block = int((fields-0.1)/fields)+1

      ! Put in (row and column-labeled) column-blocks
      l = 0
      do block = 1,n_block

         ! First and last columns
         f = l+1
         l = min(l+fields,size(mx,2))

         ! New block requires new line
         if (block>1) call flush_(self)

         ! Tab column width
         call tab_(self,width=wid)

         ! Column labels
         do j = f,l
            call put_(self,col_label(j),left,width=wid)
         end do
         call flush_(self,2)

         ! The rows of the matrix
         do i = 1,size(mx,1)

            ! The row label
            call put_(self,row_label(i),left,width=wid)

            ! The row of the matrix
            do j = f,l
               call put_(self,mx(i,j),left,width=wid)
            end do
            call flush_(self)
         end do
      end do
   end subroutine


!  These routines avoid the buffer.string and output directly to the file.

   subroutine parallel_put_3(self,mx)
   ! Parallel put MAT{INT} "mx" directly into the output file.
   ! Column order assumed.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: mx
      INT :: i,j
      STR(len=STR_SIZE) :: fmt

      ! One element per line
      fmt = int_format_(self,n_fields=1,width=2*self%style%int_width,with_parenthesis=TRUE)

      ! Write the array
      do j = 1,size(mx,2)
         do i = 1,size(mx,1)
            write(unit=self%unit,fmt=fmt) mx(i,j)
            call increment_record_(self)
         end do
      end do
   end subroutine

   subroutine parallel_put_4(self,mx)
   ! Parallel put MAT{REAL} "mx" directly into the output file.
   ! Column order assumed.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: mx
      INT :: i,j
      STR(len=STR_SIZE) :: fmt

      ! One element per line
      fmt = real_format_(self,n_fields=1,width=2*self%style%real_width,precision=2*self%style%real_precision,with_parenthesis=TRUE)

      ! Write the array
      do j = 1,size(mx,2)
         do i = 1,size(mx,1)
            write(unit=self%unit,fmt=fmt) mx(i,j)
            call increment_record_(self)
         end do
      end do
   end subroutine

   subroutine parallel_put_5(self,mx)
   ! Parallel put MAT{CPX} "mx" directly into the output file.
   ! Column order assumed.
      type(TEXTFILE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: mx
      INT :: i,j
      STR(len=STR_SIZE) :: fmt

      ! One element per line
      fmt = real_format_(self,n_fields=2,width=2*self%style%real_width,precision=2*self%style%real_precision,with_parenthesis=TRUE)

      ! Write the array
      do j = 1,size(mx,2)
         do i = 1,size(mx,1)
            write(unit=self%unit,fmt=fmt) mx(i,j)
            call increment_record_(self)
         end do
      end do
   end subroutine

!  ==============
!  Put MAT3{TYPE}
!  ==============

   subroutine put_18(self,mx,by_dim3_matrices,width,auto_width)
   ! Put a MAT3{TYPE} into the output buffer flat style
      type(TEXTFILE_TYPE) :: self
      MAT3(INT,:,:,:), IN :: mx
      BIN, optional :: by_dim3_matrices
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      INT :: dim1,dim2,dim3,maxdim,rwid,iwid,i1,i2,i3
      BIN :: by_d3_matrices,auto

      ! Dimensions
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_18 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)

      ! Extract options
      by_d3_matrices = FALSE
      if (present(by_dim3_matrices)) by_d3_matrices = by_dim3_matrices

      ! Width
      maxdim = max(dim1,dim2,dim3)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width
      if (auto) call get_auto_width_(self,mx,rwid)
      if (by_d3_matrices) then
         do i3 = 1,dim3
            do i2 = 1,dim2
               do i1 = 1,dim1
                  call put_(self,i1,width=iwid)
                  call put_(self,i2,width=iwid)
                  call put_(self,i3,width=iwid)
                  call put_(self,mx(i1,i2,i3),width=rwid)
                  call flush_(self)
               end do
            end do
         end do
      else
         do i3 = 1,dim3
            call flush_(stdout)
            call show_(stdout,"matrix =",i3)
            call flush_(stdout)
            call put_(self,mx(:,:,i3),width=rwid)
         end do
      end if
   end subroutine

   subroutine put_19(self,mx,by_dim3_matrices,width,auto_width)
   ! Put a MAT3{TYPE} into the output buffer flat style
      type(TEXTFILE_TYPE) :: self
      MAT3(REAL,:,:,:), IN :: mx
      BIN, optional :: by_dim3_matrices
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      INT :: dim1,dim2,dim3,maxdim,rwid,iwid,i1,i2,i3
      BIN :: by_d3_matrices,auto

      ! Dimensions
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_19 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)

      ! Extract options
      by_d3_matrices = FALSE
      if (present(by_dim3_matrices)) by_d3_matrices = by_dim3_matrices

      ! Width
      maxdim = max(dim1,dim2,dim3)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width
      if (auto) call get_auto_width_(self,mx,rwid)
      if (by_d3_matrices) then
         do i3 = 1,dim3
            do i2 = 1,dim2
               do i1 = 1,dim1
                  call put_(self,i1,width=iwid)
                  call put_(self,i2,width=iwid)
                  call put_(self,i3,width=iwid)
                  call put_(self,mx(i1,i2,i3),width=rwid)
                  call flush_(self)
               end do
            end do
         end do
      else
         do i3 = 1,dim3
            call flush_(stdout)
            call show_(stdout,"matrix =",i3)
            call flush_(stdout)
            call put_(self,mx(:,:,i3),width=rwid)
         end do
      end if
   end subroutine

   subroutine put_20(self,mx,by_dim3_matrices,width,auto_width)
   ! Put a MAT3{TYPE} into the output buffer flat style
      type(TEXTFILE_TYPE) :: self
      MAT3(CPX,:,:,:), IN :: mx
      BIN, optional :: by_dim3_matrices
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      INT :: dim1,dim2,dim3,maxdim,rwid,iwid,i1,i2,i3
      BIN :: by_d3_matrices,auto

      ! Dimensions
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_20 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)

      ! Extract options
      by_d3_matrices = FALSE
      if (present(by_dim3_matrices)) by_d3_matrices = by_dim3_matrices

      ! Width
      maxdim = max(dim1,dim2,dim3)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width
      if (auto) call get_auto_width_(self,mx,rwid)
      if (by_d3_matrices) then
         do i3 = 1,dim3
            do i2 = 1,dim2
               do i1 = 1,dim1
                  call put_(self,i1,width=iwid)
                  call put_(self,i2,width=iwid)
                  call put_(self,i3,width=iwid)
                  call put_(self,mx(i1,i2,i3),width=rwid)
                  call flush_(self)
               end do
            end do
         end do
      else
         do i3 = 1,dim3
            call flush_(stdout)
            call show_(stdout,"matrix =",i3)
            call flush_(stdout)
            call put_(self,mx(:,:,i3),width=rwid)
         end do
      end if
   end subroutine

!  ==============
!  Put MAT4{TYPE}
!  ==============

   subroutine put_21(self,mx,by_matrices,width,auto_width)
   ! Put a MAT4{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT4(INT,:,:,:,:), IN :: mx
      BIN, optional :: by_matrices
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      INT :: dim1,dim2,dim3,dim4,maxdim,rwid,iwid,i1,i2,i3,i4
      BIN :: do_matrices,auto

      ! Dimensions
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_21 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      dim4 = size(mx,4)

      ! Extract options
      do_matrices = TRUE
      if (present(by_matrices)) do_matrices = by_matrices

      ! Width
      maxdim = max(dim1,dim2,dim3,dim4)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width
      if (auto) call get_auto_width_(self,mx,rwid)
      if (do_matrices) then
         do i4 = 1,dim4
            do i3 = 1,dim3
               call flush_(stdout)
               call text_(stdout,"matrix = (:,:,"//trim(to_str_(i3))//","//trim(to_str_(i4))//")")
               call flush_(stdout)
               call put_(self,mx(:,:,i3,i4),width=rwid)
            end do
         end do
      else
         do i4 = 1,dim4
            do i3 = 1,dim3
               do i2 = 1,dim2
                  do i1 = 1,dim1
                     call put_(self,i1,width=iwid)
                     call put_(self,i2,width=iwid)
                     call put_(self,i3,width=iwid)
                     call put_(self,i4,width=iwid)
                     call put_(self,mx(i1,i2,i3,i4),width=rwid)
                     call flush_(self)
                  end do
               end do
            end do
         end do
      end if
   end subroutine

   subroutine put_22(self,mx,by_matrices,width,auto_width)
   ! Put a MAT4{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), IN :: mx
      BIN, optional :: by_matrices
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      INT :: dim1,dim2,dim3,dim4,maxdim,rwid,iwid,i1,i2,i3,i4
      BIN :: do_matrices,auto

      ! Dimensions
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_22 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      dim4 = size(mx,4)

      ! Extract options
      do_matrices = TRUE
      if (present(by_matrices)) do_matrices = by_matrices

      ! Width
      maxdim = max(dim1,dim2,dim3,dim4)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width
      if (auto) call get_auto_width_(self,mx,rwid)
      if (do_matrices) then
         do i4 = 1,dim4
            do i3 = 1,dim3
               call flush_(stdout)
               call text_(stdout,"matrix = (:,:,"//trim(to_str_(i3))//","//trim(to_str_(i4))//")")
               call flush_(stdout)
               call put_(self,mx(:,:,i3,i4),width=rwid)
            end do
         end do
      else
         do i4 = 1,dim4
            do i3 = 1,dim3
               do i2 = 1,dim2
                  do i1 = 1,dim1
                     call put_(self,i1,width=iwid)
                     call put_(self,i2,width=iwid)
                     call put_(self,i3,width=iwid)
                     call put_(self,i4,width=iwid)
                     call put_(self,mx(i1,i2,i3,i4),width=rwid)
                     call flush_(self)
                  end do
               end do
            end do
         end do
      end if
   end subroutine

   subroutine put_23(self,mx,by_matrices,width,auto_width)
   ! Put a MAT4{TYPE} into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), IN :: mx
      BIN, optional :: by_matrices
      INT, IN, optional :: width
      BIN, IN, optional :: auto_width
      INT :: dim1,dim2,dim3,dim4,maxdim,rwid,iwid,i1,i2,i3,i4
      BIN :: do_matrices,auto

      ! Dimensions
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_23 ... zero sized dimensions present!")

      ! The following code is inherited from TEXTFILE
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      dim4 = size(mx,4)

      ! Extract options
      do_matrices = TRUE
      if (present(by_matrices)) do_matrices = by_matrices

      ! Width
      maxdim = max(dim1,dim2,dim3,dim4)
      iwid = str_length_(maxdim)+self%style%spacing
      rwid = self%style%real_width
      if (present(width)) rwid = width
      auto = FALSE
      if (present(auto_width)) auto = auto_width

      ! Auto field width overrides width
      if (auto) call get_auto_width_(self,mx,rwid)
      if (do_matrices) then
         do i4 = 1,dim4
            do i3 = 1,dim3
               call flush_(stdout)
               call text_(stdout,"matrix = (:,:,"//trim(to_str_(i3))//","//trim(to_str_(i4))//")")
               call flush_(stdout)
               call put_(self,mx(:,:,i3,i4),width=rwid)
            end do
         end do
      else
         do i4 = 1,dim4
            do i3 = 1,dim3
               do i2 = 1,dim2
                  do i1 = 1,dim1
                     call put_(self,i1,width=iwid)
                     call put_(self,i2,width=iwid)
                     call put_(self,i3,width=iwid)
                     call put_(self,i4,width=iwid)
                     call put_(self,mx(i1,i2,i3,i4),width=rwid)
                     call flush_(self)
                  end do
               end do
            end do
         end do
      end if
   end subroutine

!  ==============
!  Put MAT5{TYPE}
!  ==============

   subroutine put_24(self,mx)
   ! Put a MAT5{TYPE} into the output buffer flat style
      type(TEXTFILE_TYPE), INOUT :: self
      MAT5(INT,:,:,:,:,:), IN :: mx
      INT :: i1,i2,i3,i4,i5
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_24 ... there are zero sized dimensions")

      ! The following code is inherited from TEXTFILE
      do i5 = 1,size(mx,5)
         do i4 = 1,size(mx,4)
            do i3 = 1,size(mx,3)
               do i2 = 1,size(mx,2)
                  do i1 = 1,size(mx,1)
                     call put_(self,i1)
                     call put_(self,i2)
                     call put_(self,i3)
                     call put_(self,i4)
                     call put_(self,i5)
                     call put_(self,mx(i1,i2,i3,i4,i5))
                     call flush_(self)
                  end do
               end do
            end do
         end do
      end do
   end subroutine

   subroutine put_25(self,mx)
   ! Put a MAT5{TYPE} into the output buffer flat style
      type(TEXTFILE_TYPE), INOUT :: self
      MAT5(REAL,:,:,:,:,:), IN :: mx
      INT :: i1,i2,i3,i4,i5
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_25 ... there are zero sized dimensions")

      ! The following code is inherited from TEXTFILE
      do i5 = 1,size(mx,5)
         do i4 = 1,size(mx,4)
            do i3 = 1,size(mx,3)
               do i2 = 1,size(mx,2)
                  do i1 = 1,size(mx,1)
                     call put_(self,i1)
                     call put_(self,i2)
                     call put_(self,i3)
                     call put_(self,i4)
                     call put_(self,i5)
                     call put_(self,mx(i1,i2,i3,i4,i5))
                     call flush_(self)
                  end do
               end do
            end do
         end do
      end do
   end subroutine

   subroutine put_26(self,mx)
   ! Put a MAT5{TYPE} into the output buffer flat style
      type(TEXTFILE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), IN :: mx
      INT :: i1,i2,i3,i4,i5
   WARN_IF(any(shape(mx)==0),"TEXTFILE:put_26 ... there are zero sized dimensions")

      ! The following code is inherited from TEXTFILE
      do i5 = 1,size(mx,5)
         do i4 = 1,size(mx,4)
            do i3 = 1,size(mx,3)
               do i2 = 1,size(mx,2)
                  do i1 = 1,size(mx,1)
                     call put_(self,i1)
                     call put_(self,i2)
                     call put_(self,i3)
                     call put_(self,i4)
                     call put_(self,i5)
                     call put_(self,mx(i1,i2,i3,i4,i5))
                     call flush_(self)
                  end do
               end do
            end do
         end do
      end do
   end subroutine

!  ===================
!  Put VEC{EMAT{TYPE}}
!  ===================

   subroutine put_27(self,v)
   ! Put a VEC{EMAT{TYPE}} list into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EMAT_INT_TYPE),:), IN :: v
      INT :: i
      BIN :: using_array_labels

      ! Save label status

      ! The following code is inherited from TEXTFILE
      using_array_labels = self%style%using_array_labels
      call set_using_array_labels_(self,FALSE)
      do i = 1,size(v)
         call text_(self,"Matrix "//trim(to_str_(i))//":")
         call put_(self,v(i)%element,by_row=TRUE)
      end do

      ! Restore labels
      call set_using_array_labels_(self,using_array_labels)
   end subroutine

   subroutine put_28(self,v)
   ! Put a VEC{EMAT{TYPE}} list into the output buffer
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), IN :: v
      INT :: i
      BIN :: using_array_labels

      ! Save label status

      ! The following code is inherited from TEXTFILE
      using_array_labels = self%style%using_array_labels
      call set_using_array_labels_(self,FALSE)
      do i = 1,size(v)
         call text_(self,"Matrix "//trim(to_str_(i))//":")
         call put_(self,v(i)%element,by_row=TRUE)
      end do

      ! Restore labels
      call set_using_array_labels_(self,using_array_labels)
   end subroutine


!  =========================
!  Put OPVECTOR and OPMATRIX
!  =========================

   subroutine put_29(self,v)
   ! Outputs the OPVECTOR "v"
      type(TEXTFILE_TYPE), INOUT :: self
      type(OPVECTOR_TYPE), IN :: v
      if (allocated(v%r)) then
         call flush_(self)
         call text_(self,"Restricted part:")
         call flush_(self,2)
         call put_(self,v%r)
      end if
      if (allocated(v%a)) then
         call flush_(self)
         call text_(self,"Alpha part:")
         call flush_(self,2)
         call put_(self,v%a)
      end if
      if (allocated(v%b)) then
         call flush_(self)
         call text_(self,"Beta part:")
         call put_(self,v%b)
      end if
   end subroutine

   subroutine put_30(self,mx,by_row,by_column)
   ! Outputs the OPMATRIX "mx"
      type(TEXTFILE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: mx
      BIN, IN, optional :: by_row
      BIN, IN, optional :: by_column
      if (allocated(mx%r)) then
         call flush_(self)
         call text_(self,"Restricted part:")
         call flush_(self,2)
         call put_(self,mx%r,by_row,by_column)
      end if
      if (allocated(mx%a)) then
         call flush_(self)
         call text_(self,"Alpha part:")
         call flush_(self,2)
         call put_(self,mx%a,by_row,by_column)
      end if
      if (allocated(mx%b)) then
         call flush_(self)
         call text_(self,"Beta part:")
         call flush_(self,2)
         call put_(self,mx%b,by_row,by_column)
      end if
      if (allocated(mx%gc)) then
         call flush_(self)
         call text_(self,"General complex part:")
         call flush_(self,2)
         call put_(self,mx%gc,by_row,by_column)
      end if
   end subroutine


! Miscellaneous

   subroutine put_ppm(self,mx)
   ! Creates a ppm file from a 3*w*h array of RGB
   ! values and write to disk
      type(TEXTFILE_TYPE), INOUT :: self
      MAT3(INT,:,:,:) :: mx
      INT :: i,w,h,k,m
   ENSURE(minval(mx)>=0,"TEXTFILE:put_ppm ... rgb values should be positive")
   ENSURE(size(mx,1)==3,"TEXTFILE:put_ppm ... array should have dimensions 3*w*h")
      i = maxval(mx)
      w = size(mx,2)
      h = size(mx,3)
      call text_(self,"P3")
      call put_(self,w); call flush_(self)
      call put_(self,h); call flush_(self)
      call put_(self,i); call flush_(self)
      do k = 1,h
         do m = 1,w
            call put_(self,mx(1,m,k))
            call put_(self,mx(2,m,k))
            call put_(self,mx(3,m,k))
            call flush_(self)
         end do
      end do
   end subroutine

!  =================
!  Show TYPE methods
!  =================

   subroutine show_0(self,pretext,value,width,dots)
   ! Put a left-justfied formatted "value" into the output buffer with
   ! some descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      STR(len=*), IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the value
      if (present(width)) then; call put_(self,value,width=width)
      else; call put_(self,value,left=TRUE,width=0)
      end if
      call flush_(self)
   end subroutine

   subroutine show_1(self,pretext,value,width,dots)
   ! Put a left-justfied formatted "value" into the output buffer with
   ! some descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      BIN, IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the value
      if (present(width)) then; call put_(self,value,width=width)
      else; call put_(self,value,left=TRUE,width=0)
      end if
      call flush_(self)
   end subroutine

   subroutine show_2(self,pretext,value,width,dots)
   ! Put a left-justfied formatted "value" into the output buffer with
   ! some descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      INT, IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the value
      if (present(width)) then; call put_(self,value,width=width)
      else; call put_(self,value,left=TRUE,width=0)
      end if
      call flush_(self)
   end subroutine

   subroutine show_3(self,pretext,value,width,precision,right,dots)
   ! Put a left-justfied formatted "value" into the output buffer with
   ! descriptive "pretext" and nondefault "width" and "precision".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      REAL, IN :: value
      INT, IN, optional :: width
      INT, IN, optional :: precision
      BIN, IN, optional :: right
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      BIN :: do_dots,do_right

      ! The following code is inherited from TEXTFILE
      do_right = FALSE
      if (present(right)) do_right = right
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the value
      if (present(width)) then; call put_(self,value,width=width,precision=precision)
      else if (do_right) then; call put_(self,value,precision=precision)
      else; call put_(self,value,left=TRUE,width=0,precision=precision)
      end if
      call flush_(self)
   end subroutine

   subroutine show_4(self,pretext,value,width,precision,right,dots)
   ! Put a left-justfied formatted "value" into the output buffer with
   ! descriptive "pretext" and nondefault "width" and "precision".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      CPX, IN :: value
      INT, IN, optional :: width
      INT, IN, optional :: precision
      BIN, IN, optional :: right
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      BIN :: do_dots,do_right

      ! The following code is inherited from TEXTFILE
      do_right = FALSE
      if (present(right)) do_right = right
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the value
      if (present(width)) then; call put_(self,value,width=width,precision=precision)
      else if (do_right) then; call put_(self,value,precision=precision)
      else; call put_(self,value,left=TRUE,width=0,precision=precision)
      end if
      call flush_(self)
   end subroutine

   subroutine show_5(self,pretext,value,error,width,precision,dots)
   ! Put a left-justfied formatted "value" into the output buffer with
   ! some descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      REAL, IN :: value
      REAL, IN :: error
      INT, optional, IN :: width
      INT, optional, IN :: precision
      BIN, optional, IN :: dots
      STR(len=(len(pretext)+3)) :: description
      BIN :: do_dots
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the value
      call put_(self,value,error,width=width,precision=precision)

      ! Put
      call flush_(self)
   end subroutine

   subroutine show_bit_string(self,pretext,value,width)
   ! Put a formatted INT as a binary number into the output buffer,
   ! with descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*) :: pretext
      INT :: value
      INT, optional :: width
      call text_(self,pretext,flush=FALSE)
      if (present(width)) then; call put_bit_string_(self,value,width=width)
      else; call put_bit_string_(self,value,width=self%style%real_width)
      end if
      call flush_(self)
   end subroutine

!  ===============
!  Show two TYPE's
!  ===============

!   show(pretext,val1,val2) :: get_from(TEXTFILE, VAL?=>INT)
!   ! Put two scalars "val1", "val2" into the output buffer, with
!   ! descriptive "pretext" before.
!   end

!   show(pretext,val1,val2) :: get_from(TEXTFILE, VAL?=>REAL)
!   ! Put two scalars "val1", "val2" into the output buffer, with
!   ! descriptive "pretext" before.
!   end

!  =================
!  Show three TYPE's
!  =================

   subroutine show_6(self,pretext,val1,val2,val3,tol)
   ! Put three scalar "val1", "val2" and "val3" into the output buffer, with
   ! descriptive "pretext" before.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      BIN, IN :: val1,val2,val3
      INT, IN, optional :: tol
      INT :: tol_save

      ! The following code is inherited from TEXTFILE
      call text_(self,pretext,flush=FALSE)
      if (present(tol)) then
         tol_save = self%style%real_precision
         self%style%real_precision = tol
         call put_(self,val1,width=self%style%real_width)
         call put_(self,val2,width=self%style%real_width)
         call put_(self,val3,width=self%style%real_width)
         self%style%real_precision = tol_save
      else
         call put_(self,val1,width=self%style%real_width)
         call put_(self,val2,width=self%style%real_width)
         call put_(self,val3,width=self%style%real_width)
      end if
      call flush_(self)
   end subroutine

   subroutine show_7(self,pretext,val1,val2,val3,tol)
   ! Put three scalar "val1", "val2" and "val3" into the output buffer, with
   ! descriptive "pretext" before.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      INT, IN :: val1,val2,val3
      INT, IN, optional :: tol
      INT :: tol_save

      ! The following code is inherited from TEXTFILE
      call text_(self,pretext,flush=FALSE)
      if (present(tol)) then
         tol_save = self%style%real_precision
         self%style%real_precision = tol
         call put_(self,val1,width=self%style%real_width)
         call put_(self,val2,width=self%style%real_width)
         call put_(self,val3,width=self%style%real_width)
         self%style%real_precision = tol_save
      else
         call put_(self,val1,width=self%style%real_width)
         call put_(self,val2,width=self%style%real_width)
         call put_(self,val3,width=self%style%real_width)
      end if
      call flush_(self)
   end subroutine

   subroutine show_8(self,pretext,val1,val2,val3,tol)
   ! Put three scalar "val1", "val2" and "val3" into the output buffer, with
   ! descriptive "pretext" before.
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      REAL, IN :: val1,val2,val3
      INT, IN, optional :: tol
      INT :: tol_save

      ! The following code is inherited from TEXTFILE
      call text_(self,pretext,flush=FALSE)
      if (present(tol)) then
         tol_save = self%style%real_precision
         self%style%real_precision = tol
         call put_(self,val1,width=self%style%real_width)
         call put_(self,val2,width=self%style%real_width)
         call put_(self,val3,width=self%style%real_width)
         self%style%real_precision = tol_save
      else
         call put_(self,val1,width=self%style%real_width)
         call put_(self,val2,width=self%style%real_width)
         call put_(self,val3,width=self%style%real_width)
      end if
      call flush_(self)
   end subroutine

!  ==============
!  Show VEC{TYPE}
!  ==============

   subroutine show_9(self,pretext,value,width,dots)
   ! Put a formatted VEC{TYPE} "value" into the output buffer with
   ! descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      VEC(STR(len=*),:), IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      INT :: i
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the values
      if (present(width)) then
         do i = 1,size(value)
            call put_(self,value(i),width=width)
         end do
      else
         call put_(self,to_concatenated_str_(value,separator=" "),left=TRUE)
      end if
      call flush_(self)
   end subroutine

   subroutine show_10(self,pretext,value,width,dots)
   ! Put a formatted VEC{TYPE} "value" into the output buffer with
   ! descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      VEC(BIN,:), IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      INT :: i
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the values
      if (present(width)) then
         do i = 1,size(value)
            call put_(self,value(i),width=width)
         end do
      else
         call put_(self,to_concatenated_str_(value,separator=" "),left=TRUE)
      end if
      call flush_(self)
   end subroutine

   subroutine show_11(self,pretext,value,width,dots)
   ! Put a formatted VEC{TYPE} "value" into the output buffer with
   ! descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      VEC(INT,:), IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      INT :: i
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the values
      if (present(width)) then
         do i = 1,size(value)
            call put_(self,value(i),width=width)
         end do
      else
         call put_(self,to_concatenated_str_(value,separator=" "),left=TRUE)
      end if
      call flush_(self)
   end subroutine

   subroutine show_12(self,pretext,value,width,dots)
   ! Put a formatted VEC{TYPE} "value" into the output buffer with
   ! descriptive "pretext".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: pretext
      VEC(REAL,:), IN :: value
      INT, IN, optional :: width
      BIN, IN, optional :: dots
      STR(len=(len(pretext)+3)) :: description
      INT :: i
      BIN :: do_dots

      ! The following code is inherited from TEXTFILE
      do_dots = TRUE
      if (present(dots)) do_dots = dots
      if (do_dots) then

         ! Modify the description pretext
         description = " "
         description = pretext
         call replace_end_from_(description," =",".",plus=1)

         ! Show it
         call text_(self,description,flush=FALSE)
      else

         ! Show it
         call text_(self,pretext,flush=FALSE)
      end if

      ! Put the values
      if (present(width)) then
         do i = 1,size(value)
            call put_(self,value(i),width=width)
         end do
      else
         call put_(self,to_concatenated_str_(value,separator=" "),left=TRUE)
      end if
      call flush_(self)
   end subroutine

!  =================
!  Dump TYPE methods
!  =================

   subroutine dump_0(self,varname,value)
   ! Dump a "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      STR(len=*), IN :: value

      ! The following code is inherited from TEXTFILE
      call text_(self,trim(varname)//":STR =",flush=FALSE)
      call put_(self,value,left=TRUE,width=0)
      call flush_(self)
   end subroutine

   subroutine dump_1(self,varname,value)
   ! Dump a "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      BIN, IN :: value

      ! The following code is inherited from TEXTFILE
      call text_(self,trim(varname)//":BIN =",flush=FALSE)
      call put_(self,value,left=TRUE,width=0)
      call flush_(self)
   end subroutine

   subroutine dump_2(self,varname,value)
   ! Dump a "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      INT, IN :: value

      ! The following code is inherited from TEXTFILE
      call text_(self,trim(varname)//":INT =",flush=FALSE)
      call put_(self,value,left=TRUE,width=0)
      call flush_(self)
   end subroutine

   subroutine dump_3(self,varname,value)
   ! Dump a REAL/CPX "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      REAL, IN :: value

      ! The following code is inherited from TEXTFILE
      call text_(self,trim(varname)//":REAL = ",flush=FALSE)
      call put_(self,value,left=TRUE,fmt="e",width=TEXTFILE_REAL_WIDTH,precision=TEXTFILE_REAL_PRECISION)
      call flush_(self)
   end subroutine

   subroutine dump_4(self,varname,value)
   ! Dump a REAL/CPX "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      CPX, IN :: value

      ! The following code is inherited from TEXTFILE
      call text_(self,trim(varname)//":CPX = ",flush=FALSE)
      call put_(self,value,left=TRUE,fmt="e",width=TEXTFILE_REAL_WIDTH,precision=TEXTFILE_REAL_PRECISION)
      call flush_(self)
   end subroutine

!  ==============================
!  Dump VEC{TYPE} without pretext
!  ==============================

   subroutine dump_5(self,v)
   ! Dump a non-numeric vector "v" without any description
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: v
      INT :: dim,n_block,block,fields,i,f,l

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      fields = TEXTFILE_N_FIELDS_PER_LINE
      n_block = int((dim-0.1)/fields)+1
      do block = 1,n_block
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         do i = f,l
            call put_(self,v(i),left=TRUE,width=0)
         end do
      end do
      call flush_(self)
   end subroutine

   subroutine dump_6(self,v)
   ! Dump a non-numeric vector "v" without any description
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(BIN,:), IN :: v
      INT :: dim,n_block,block,fields,i,f,l

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      fields = TEXTFILE_N_FIELDS_PER_LINE
      n_block = int((dim-0.1)/fields)+1
      do block = 1,n_block
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         do i = f,l
            call put_(self,v(i),left=TRUE,width=0)
         end do
      end do
      call flush_(self)
   end subroutine

   subroutine dump_7(self,v)
   ! Dump a non-numeric vector "v" without any description
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(INT,:), IN :: v
      INT :: dim,n_block,block,fields,i,f,l

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      fields = TEXTFILE_N_FIELDS_PER_LINE
      n_block = int((dim-0.1)/fields)+1
      do block = 1,n_block
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         do i = f,l
            call put_(self,v(i),left=TRUE,width=0)
         end do
      end do
      call flush_(self)
   end subroutine

   subroutine dump_8(self,v)
   ! Dump a numeric vector "v" without any description
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: v
      INT :: dim,n_block,block,fields,i,f,l

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      fields = TEXTFILE_N_FIELDS_PER_LINE
      n_block = int((dim-0.1)/fields)+1
      do block = 1,n_block
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         do i = f,l
            call put_(self,v(i),fmt="e",width=TEXTFILE_REAL_WIDTH,precision=TEXTFILE_REAL_PRECISION)
         end do
      end do
      call flush_(self)
   end subroutine

   subroutine dump_9(self,v)
   ! Dump a numeric vector "v" without any description
      type(TEXTFILE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: v
      INT :: dim,n_block,block,fields,i,f,l

      ! The following code is inherited from TEXTFILE
      dim = size(v)
      fields = TEXTFILE_N_FIELDS_PER_LINE
      n_block = int((dim-0.1)/fields)+1
      do block = 1,n_block
         f = 1+fields*(block-1)
         l = min(f+fields-1,dim)
         if (block>1) call flush_(self)
         do i = f,l
            call put_(self,v(i),fmt="e",width=TEXTFILE_REAL_WIDTH,precision=TEXTFILE_REAL_PRECISION)
         end do
      end do
      call flush_(self)
   end subroutine

!  ==============
!  Dump VEC{TYPE}
!  ==============

   subroutine dump_10(self,varname,v,all)
   ! Dump a vector "v" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(STR(len=*),:), IN :: v
      BIN, optional, IN :: all
      INT :: dim
      BIN :: is_all

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":VEC{VEC{STR}}@ = {")
      else; call text_(stdout,trim(varname)//":VEC{VEC{STR}} = {")
      end if

      ! Dimension
      dim = size(v)
      call text_(self," dimension = "//trim(to_str_(dim)))

      ! Dump vector
      call dump_(self,v)
      call text_(self,"}")
   end subroutine

   subroutine dump_11(self,varname,v,all)
   ! Dump a vector "v" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(BIN,:), IN :: v
      BIN, optional, IN :: all
      INT :: dim
      BIN :: is_all

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":VEC{VEC{BIN}}@ = {")
      else; call text_(stdout,trim(varname)//":VEC{VEC{BIN}} = {")
      end if

      ! Dimension
      dim = size(v)
      call text_(self," dimension = "//trim(to_str_(dim)))

      ! Dump vector
      call dump_(self,v)
      call text_(self,"}")
   end subroutine

   subroutine dump_12(self,varname,v,all)
   ! Dump a vector "v" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(INT,:), IN :: v
      BIN, optional, IN :: all
      INT :: dim
      BIN :: is_all

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":VEC{VEC{INT}}@ = {")
      else; call text_(stdout,trim(varname)//":VEC{VEC{INT}} = {")
      end if

      ! Dimension
      dim = size(v)
      call text_(self," dimension = "//trim(to_str_(dim)))

      ! Dump vector
      call dump_(self,v)
      call text_(self,"}")
   end subroutine

   subroutine dump_13(self,varname,v,all)
   ! Dump a vector "v" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(REAL,:), IN :: v
      BIN, optional, IN :: all
      INT :: dim
      BIN :: is_all

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":VEC{VEC{REAL}}@ = {")
      else; call text_(stdout,trim(varname)//":VEC{VEC{REAL}} = {")
      end if

      ! Dimension
      dim = size(v)
      call text_(self," dimension = "//trim(to_str_(dim)))

      ! Dump vector
      call dump_(self,v)
      call text_(self,"}")
   end subroutine

   subroutine dump_14(self,varname,v,all)
   ! Dump a vector "v" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(CPX,:), IN :: v
      BIN, optional, IN :: all
      INT :: dim
      BIN :: is_all

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":VEC{VEC{CPX}}@ = {")
      else; call text_(stdout,trim(varname)//":VEC{VEC{CPX}} = {")
      end if

      ! Dimension
      dim = size(v)
      call text_(self," dimension = "//trim(to_str_(dim)))

      ! Dump vector
      call dump_(self,v)
      call text_(self,"}")
   end subroutine

!  ==============
!  Dump MAT{TYPE}
!  ==============

   subroutine dump_15(self,varname,mx,all)
   ! Dump a matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(STR(len=*),:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2
      BIN :: is_all
      VEC(STR(len=STR_SIZE),:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call text_(self,"dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_16(self,varname,mx,all)
   ! Dump a matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(BIN,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2
      BIN :: is_all
      VEC(BIN,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call text_(self,"dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_17(self,varname,mx,all)
   ! Dump a matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(INT,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2
      BIN :: is_all
      VEC(INT,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call text_(self,"dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_18(self,varname,mx,all)
   ! Dump a matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(REAL,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2
      BIN :: is_all
      VEC(REAL,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call text_(self,"dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_19(self,varname,mx,all)
   ! Dump a matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(CPX,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2
      BIN :: is_all
      VEC(CPX,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      call text_(self,"dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

!  ===============
!  Dump MAT3{TYPE}
!  ===============

   subroutine dump_20(self,varname,mx,all)
   ! Dump a 3d matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(BIN,:,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2,dim3
      BIN :: is_all
      VEC(BIN,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT3{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT3{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      call text_(self," dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)//" "//trim(to_str_(dim3))))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_21(self,varname,mx,all)
   ! Dump a 3d matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(INT,:,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2,dim3
      BIN :: is_all
      VEC(INT,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT3{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT3{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      call text_(self," dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)//" "//trim(to_str_(dim3))))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_22(self,varname,mx,all)
   ! Dump a 3d matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(REAL,:,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2,dim3
      BIN :: is_all
      VEC(REAL,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT3{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT3{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      call text_(self," dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)//" "//trim(to_str_(dim3))))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

   subroutine dump_23(self,varname,mx,all)
   ! Dump a 3d matrix "mx" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(CPX,:,:,:), IN :: mx
      BIN, optional, IN :: all
      INT :: dim,dim1,dim2,dim3
      BIN :: is_all
      VEC(CPX,:), allocatable :: v

      ! Is this a pointer?

      ! The following code is inherited from TEXTFILE
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":MAT3{TYPE}@ = {")
      else; call text_(stdout,trim(varname)//":MAT3{TYPE} = {")
      end if

      ! Dimensions
      dim = size(mx)
      dim1 = size(mx,1)
      dim2 = size(mx,2)
      dim3 = size(mx,3)
      call text_(self," dimensions = "//trim(to_str_(dim1))//" "//trim(to_str_(dim2)//" "//trim(to_str_(dim3))))

      ! Dump array as vector
      call create_(v,dim)
      v = reshape(mx,[dim])
      call dump_(self,v)
      call destroy_(v)
      call text_(self,"}")
   end subroutine

!  =================
!  Dump allocatables
!  =================


!  ===============
!  Dump VEC{TYPE}*
!  ===============
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>VEC{STR})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>VEC{BIN})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>VEC{INT})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>VEC{REAL})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>VEC{CPX})
!   ! Dump a pointer "value" with variable name "varname".
!   end

!  ===============
!  Dump VEC{TYPE}@
!  ===============

   subroutine dmpa_0(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(STR(len=*),:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":VEC{STR}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_1(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(BIN,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":VEC{BIN}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_2(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(INT,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":VEC{INT}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_3(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(REAL,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":VEC{REAL}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_4(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      VEC(CPX,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":VEC{CPX}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine


!  ===============
!  Dump MAT{TYPE}*
!  ===============
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT{STR})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT{BIN})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT{INT})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT{REAL})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT{CPX})
!   ! Dump a pointer "value" with variable name "varname".
!   end

!  ===============
!  Dump MAT{TYPE}@
!  ===============

   subroutine dmpa_5(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(STR(len=*),:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT{STR}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_6(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(BIN,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT{BIN}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_7(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(INT,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT{INT}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_8(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(REAL,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT{REAL}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_9(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT(CPX,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT{CPX}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine


!  ================
!  Dump MAT3{TYPE}*
!  ================

!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT3{BIN})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT3{INT})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT3{REAL})
!   ! Dump a pointer "value" with variable name "varname".
!   end
!
!   dmpp(varname,value) :: get_from(TEXTFILE, VALUE_TYPE?=>MAT3{CPX})
!   ! Dump a pointer "value" with variable name "varname".
!   end

!  ================
!  Dump MAT3{TYPE}@
!  ================

   subroutine dmpa_10(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(BIN,:,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT3{BIN}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_11(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(INT,:,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT3{INT}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_12(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(REAL,:,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT3{REAL}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine

   subroutine dmpa_13(self,varname,value)
   ! Dump an allocatable "value" with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      MAT3(CPX,:,:,:), allocatable, IN :: value

      ! The following code is inherited from TEXTFILE
      if (NOT allocated(value)) then
         call text_(self,trim(varname)//":MAT3{CPX}@ = null")
      else
         call dump_(self,varname,value,all=TRUE)
      end if
   end subroutine


!  ==========================
!  Dump OPVECTOR and OPMATRIX
!  ==========================

   subroutine increment_margin_width(self,val)
   ! Increment the width of the margin in the buffer to "val".
   ! Not PURE, because .clear_and_put_margin below is not.
      type(TEXTFILE_TYPE), INOUT :: self
      INT, IN :: val
      self%style%margin_width = self%style%margin_width+val

      ! Put the margin if needed
      call clear_and_put_margin_(self)
   end subroutine

   subroutine dump_24(self,varname,v,all)
   ! Dump an OPVECTOR with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      type(OPVECTOR_TYPE), IN :: v
      BIN, optional, IN :: all
      BIN :: is_all

      ! Is this a pointer?
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":OPVECTOR@ = {")
      else; call text_(stdout,trim(varname)//":OPVECTOR  = {")
      end if
      call increment_margin_width_(self,1)

      ! Dump
      call dump_(self,"n_bf",v%n_bf)
      call dump_(self,"r   ",v%r)
      call dump_(self,"a   ",v%a)
      call dump_(self,"b   ",v%b)
      call dump_(self,"g   ",v%g)
      call increment_margin_width_(self,-1)
      call flush_(self)
      call text_(self,"}")
   end subroutine

   subroutine dump_25(self,varname,mx,all)
   ! Dump an OPMATRIX with variable name "varname".
      type(TEXTFILE_TYPE), INOUT :: self
      STR(len=*), IN :: varname
      type(OPMATRIX_TYPE), IN :: mx
      BIN, optional, IN :: all
      BIN :: is_all

      ! Is this a pointer?
      is_all = FALSE
      if (present(all)) is_all = all

      ! Variable declaration
      if (is_all) then; call text_(stdout,trim(varname)//":OPMATRIX@ = {")
      else; call text_(stdout,trim(varname)//":OPMATRIX  = {")
      end if
      call increment_margin_width_(self,1)

      ! Dump
      call dump_(self,"n_bf",mx%n_bf)
      call dump_(self,"r   ",mx%r)
      call dump_(self,"a   ",mx%a)
      call dump_(self,"b   ",mx%b)
      call dump_(self,"gc  ",mx%gc)
      call dump_(self,"tri ",mx%tri)
      call dump_(self,"sqr ",mx%sqr)
      call increment_margin_width_(self,-1)
      call flush_(self)
      call text_(self,"}")
   end subroutine


!  ================
!  Output self info
!  ================

   subroutine put_31(self)
   ! Put some information on file "stdout"
      type(TEXTFILE_TYPE), IN :: self
      call show_(stdout,"Name       =",self%name)
      call show_(stdout,"I/O action =",self%action)
      call show_(stdout,"Unit       =",self%unit)
      call show_(stdout,"Line       =",self%record)
      call show_(stdout,"Buffer     =",trim(self%buffer%string))
      call show_(stdout,"Cursor pos =",trim(cursor_pointer_(self%buffer)))
   end subroutine

   subroutine put_output_format(self)
   ! Put more information on file "stdout"
      type(TEXTFILE_TYPE), IN :: self
      call show_(stdout,"Name               =",self%name)
      call show_(stdout,"I/O action         =",self%action)
      call show_(stdout,"Unit               =",self%unit)
      call show_(stdout,"int_width          =",self%style%int_width)
      call show_(stdout,"margin_width       =",self%style%margin_width)
      call show_(stdout,"n_fields_per_line  =",self%style%n_fields_per_line)
      call show_(stdout,"real_fortran_fmt   =",self%style%real_fortran_fmt)
      call show_(stdout,"real_width         =",self%style%real_width)
      call show_(stdout,"real_precision     =",self%style%real_precision)
      call show_(stdout,"using_array_labels =",self%style%using_array_labels)
      call show_(stdout,"using_fields       =",self%style%using_fields)
   end subroutine


!  ===============
!  System routines
!  ===============

   subroutine update_IO_info(self)
   ! Let the tonto system know where this file has got to, in case of error.
   ! Copied by value: a pointer here would outlive the file. See TASKS_AND_HISTORY.md.
      type(TEXTFILE_TYPE), IN :: self

#ifndef NO_TONTO_SYSTEM_CALLS
      call set_io_file_info_(tonto,self%name,self%record,self%buffer%string,self%buffer%item_end)
#endif
   end subroutine

   subroutine update_IO_cursor(self)
   ! Let the tonto system know how far along the line the parser has got. The
   ! cursor moves item by item, so it cannot be snapshotted once per line with
   ! the rest of .update_IO_info; every buffer call that advances it must
   ! report the new position, or an error message marks the wrong column.
      type(TEXTFILE_TYPE), IN :: self

#ifndef NO_TONTO_SYSTEM_CALLS
      call set_io_file_cursor_(tonto,self%buffer%item_end)
#endif
   end subroutine

   subroutine forget_IO_info(self)
   ! Tell the tonto system this file is no longer being read, so an error
   ! message does not name a file that has been closed. Matched on the name:
   ! closing an output file must not discard the input file's position.
      type(TEXTFILE_TYPE), IN :: self

#ifndef NO_TONTO_SYSTEM_CALLS
      if (tonto%io_file_name==self%name) call clear_io_file_info_(tonto)
#endif
   end subroutine

end module
