!---------------------------------------------------------------------------
!
! COMMAND_LINE: Object for processing the command line.
!
! The command line is separated into separate items comprising GNU-style long
! options, their values, and non-optional arguments.
!
! An option is spelled --option. A single-dash spelling is rejected, with a
! message naming the long form to use instead.
!
! By default it is *assumed* that every --option is followed by an option
! value, like: --option <value> ...
!   *unless* immediately followed by another option,
!   like: --option1 --option2 --option3, or
!   like: --option1 --option2 --option3 <value3> arg1 arg2
!
! A value beginning with "-" is still read as a value, so a negative number
! e.g. --fzcut -1.0 is not mistaken for an option.
!
! Copyright (C) Dylan Jayatilaka, 2014
! Copyright (C) Dylan Jayatilaka, 2021
!
! ===
!
! This library is free software; you can redistribute it and/or modify it under
! the terms of the GNU Library General Public License as published by the Free
! Software Foundation; either version 2 of the License, or (at your option) any
! later version.
!
! This library is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more
! details.
!
! You should have received a copy of the GNU Library General Public License
! along with this library; if not, write to the Free Software Foundation, Inc.,
! 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
! $Id: command_line.foo $
!---------------------------------------------------------------------------
module COMMAND_LINE_MODULE

#  include "macros"

   include "command_line.use"

   implicit none

   include "command_line.int"


contains

! ==========
! Allocation
! ==========

   PURE subroutine create(self)
   ! Allocate an object
      type(COMMAND_LINE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"COMMAND_LINE:create ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(COMMAND_LINE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"COMMAND_LINE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts of self
      type(COMMAND_LINE_TYPE), INOUT :: self
      call destroy_(self%command_optarg)
      call destroy_(self%item)
      call destroy_(self%option)
      call destroy_(self%option_value)
      call destroy_(self%arg)
   end subroutine


! ===========
! Set methods
! ===========

   PURE subroutine set_defaults(self)
   ! Set up the default settings
      type(COMMAND_LINE_TYPE), INOUT :: self
      self%command = "?"
      self%n_items = 0
      self%n_options = 0
      self%n_args = 0
   end subroutine

   subroutine process_options(self)
   ! Process GNU-style long options on the command line.
   ! Options are spelled "--name"; a single-dash spelling is an error.
      type(COMMAND_LINE_TYPE), INOUT :: self

      ! PATH_SIZE, not STR: an argument is very often a file path, and
      ! STR is only 256 characters. See the truncation check below.
      STR(len=PATH_SIZE) :: token,opt,no_val
      INT :: i,j,length,n_option_values,maxlen
      BIN :: expecting_option_value

      ! Defaults

      ! Get the command line arguments
      if (IO_IS_ALLOWED) then
         call get_command_argument(0,token)
      end if
      PARALLEL_BROADCAST(token,master_processor_(tonto))
      if (token==" ") token = "?"
      self%command = token

      ! Loop over command line & process
      i = 0
      self%n_items = 0
      expecting_option_value = FALSE
      n_option_values = 0
      do
         i = i+1
         if (IO_IS_ALLOWED) then
            call get_command_argument(i,token,length)
         end if
         PARALLEL_BROADCAST(token,master_processor_(tonto))
         PARALLEL_BROADCAST(length,master_processor_(tonto))
         if (length==0) exit

         ! FAIL LOUDLY on truncation. "length" is the argument's TRUE length,
         ! which the loop above already uses to detect the end, so this check
         ! is free. Without it an over-long argument is silently cut to fit and
         ! the failure surfaces much later as something unrelated -- a long
         ! --basis path used to end up as "no library basis set", naming the
         ! basis rather than the path. See TASKS_AND_HISTORY.md, "long paths to the
         ! basis sets fail".
         maxlen = len(token)
         DIE_IF(length>maxlen,"COMMAND_LINE:process_options ... command line argument "//trim(to_str_(i))//" is "//trim(to_str_(length))//" characters, which exceeds the "//trim(to_str_(maxlen))//" this build allows")

         ! New item
         self%n_items = self%n_items+1

         ! Add to item list
         call append_(self%item,token)

         ! Is it an option? Only the GNU long form "--name" is one.
         if (token(1:2)=="--" AND len_trim(token)>2) then

            ! Add it to list.
            !
            ! Both values go through a local of the SAME width as .option
            ! and .option_value first. Appending a trimmed string, a literal
            ! or a narrower STR makes an array constructor whose elements
            ! have different CHARACTER lengths, which is illegal: release
            ! does not check, but a debug build rejects it at run time on
            ! the second option of every program. Keep the widths equal.
            opt = token(3:)
            no_val = "none"
            call append_(self%option,opt)
            call append_(self%option_value,no_val)
            self%n_options = self%n_options+1
            expecting_option_value = TRUE

         ! Is it an option value? This test must precede the single-dash
         ! rejection below, so a negative number e.g. "--fzcut -1.0" is
         ! read as the value it is, not mistaken for an option.
         else if (expecting_option_value) then
            self%option_value(self%n_options) = token
            n_option_values = n_option_values+1
            expecting_option_value = FALSE

         ! A single-dash option, or a bare "--". No longer accepted.
         else if (token(1:1)=="-") then
            j = verify(trim(token),"-")
            DIE_IF(j==0,"COMMAND_LINE:process_options ... '"//trim(token)//"' is not an option")
            DIE("COMMAND_LINE:process_options ... options must be given in GNU long form, use '--"//trim(token(j:))//"' instead of '"//trim(token)//"'")
         else

            ! Add command line argument
            call append_(self%arg,token)
            self%n_args = self%n_args+1
            expecting_option_value = FALSE
         end if
      end do

      ! Every item is an argument, an option, or an option value. Note that
      ! not every option has a value -- a valueless flag e.g. "--help" is
      ! counted once, not twice.
      ENSURE(self%n_items==(self%n_args+self%n_options+n_option_values),"COMMAND_LINE:process_options ... inconsistent command line processing")

      ! Make command_optarg
      call create_(self%command_optarg,self%n_options+self%n_args)

      ! Options
      do i = 1,self%n_options
         if (self%option_value(i)=="none") then
            self%command_optarg(i) = "--"//trim(self%option(i))
         else
            self%command_optarg(i) = "--"//trim(self%option(i))//" "//trim(to_quoted_str_(self%option_value(i)))
         end if
      end do

      ! Arguments
      do i = 1,self%n_args
         self%command_optarg(self%n_options+i) = trim(self%arg(i))
      end do
   end subroutine


! ===============
! Inquiry methods
! ===============

   pure function has_items(self) result (res)
   ! Return TRUE if there are items on the command line
   ! WARNING: do not confuse this with arguments.
      type(COMMAND_LINE_TYPE), IN :: self
      BIN :: res
      res = self%n_items>0
   end function

   pure function has_arguments(self) result (res)
   ! Return TRUE if there are *non-optional* arguments ...
      type(COMMAND_LINE_TYPE), IN :: self
      BIN :: res
      res = self%n_args>0
   end function

   pure function has_options(self) result (res)
   ! Return TRUE if there are any options ...
      type(COMMAND_LINE_TYPE), IN :: self
      BIN :: res
      res = self%n_options>0
   end function

   pure function has_option(self,opt) result (res)
   ! Return TRUE if the command line has *specific* option "opt"
      type(COMMAND_LINE_TYPE), IN :: self
      STR(len=*), IN :: opt
      BIN :: res
      if (NOT allocated(self%option)) then; res = FALSE
      else; res = any(self%option==opt)
      end if
   end function

   pure function value_for_option(self,opt) result (res)
   ! Return the (string) value for option "opt"
      type(COMMAND_LINE_TYPE), IN :: self
      STR(len=*), IN :: opt
      STR(len=STR_SIZE) :: res

   ! ENSURE(.option.allocated,"no options given")
      INT :: i
      do i = 1,size(self%option)
         if (self%option(i)/=opt) cycle
         res = self%option_value(i)
         exit
      end do
   end function

   subroutine int_vec_for_option(self,opt,v)
   ! Read the value of option "opt" as a braced list of integers into the
   ! allocatable vector "v", e.g. --opt '{ 1 -1 3 }'.
   !
   ! The braced form is deliberate: it is the same list syntax tonto uses for
   ! the equivalent job-file keywords (`atom_group_charges= { 1 -1 }`), and the
   ! braces make a truncated or shell-mangled list an error rather than a
   ! shorter list that is quietly accepted.
   !
   ! The WHOLE list is one command-line token, so it must be quoted for the
   ! shell -- process_options binds exactly one token as an option's value.
   ! A bare `--opt { 1 -1 }` takes "{" as the value and then sends "-1" to the
   ! single-dash rejection, which unhelpfully advises "--1".
      type(COMMAND_LINE_TYPE), IN :: self
      STR(len=*), IN :: opt
      VEC(INT,:), allocatable, OUT :: v
      STR(len=STR_SIZE) :: val
      type(TEXTFILE_TYPE), allocatable :: f
      val = value_for_option_(self,opt)

   ! Checked here rather than left to TEXTFILE:read_all, whose own brace checks
   ! are ENSUREs and so compile away in an optimised build -- which is every
   ! build a user has.
      DIE_IF(n_items_(val)<3,"COMMAND_LINE:int_vec_for_option ... --"//trim(opt)//": expected a braced list of integers, e.g. "//"'{ 1 -1 }', not '"//trim(val)//"'")
      DIE_IF(item_(val,1)/="{" OR item_(val,n_items_(val))/="}","COMMAND_LINE:int_vec_for_option ... --"//trim(opt)//": list must be in braces, e.g. '{ 1 -1 }', not '"//trim(val)//"'")

      ! Read it with the same reader the job-file keywords use, via a
      ! one-line internal file. No external file and no stdin redirection:
      ! an argv-driven program may have no stdin at all.
      call create_(f,[val])
      call read_all_(f,v)
      call destroy_(f)
   end subroutine


! ===
! Put
! ===

   subroutine put(self)
   ! Put out the command line info
      type(COMMAND_LINE_TYPE), IN :: self
      call flush_(stdout)
      call text_(stdout,"=================")
      call text_(stdout,"Command line info")
      call text_(stdout,"=================")
      call flush_(stdout)
      call show_(stdout,"Command (program) name        =",self%command)
      call show_(stdout,"No. of items on command line  =",self%n_items)
      call show_(stdout,"No. of unix-style options     =",self%n_options)
      call show_(stdout,"No. of non-optional arguments =",self%n_args)
      if (self%n_items>0) then
         call flush_(stdout)
         call text_(stdout,"Items: ")
         call put_(stdout,self%item,by_column=TRUE,left=TRUE)
         if (self%n_options>0) then
            call flush_(stdout)
            call text_(stdout,"Options: ")
            call put_(stdout,self%option,by_column=TRUE,left=TRUE)
         end if
         if (self%n_options>0) then
            call flush_(stdout)
            call text_(stdout,"Option values: ")
            call put_(stdout,self%option_value,by_column=TRUE,left=TRUE)
         end if
         if (self%n_args>0) then
            call flush_(stdout)
            call text_(stdout,"Arguments: ")
            call put_(stdout,self%arg,by_column=TRUE,left=TRUE)
         end if
      end if
   end subroutine

   subroutine put_command_optarg(self)
   ! Put out the command_optarg's
      type(COMMAND_LINE_TYPE), IN :: self
      if (self%n_options>0) then
         call save_style_(stdout)
         call set_margin_width_(stdout,2)
         call set_using_array_labels_(stdout,FALSE)
         call clear_and_put_margin_(stdout)
      ! Width must leave room for the margin: the buffer is BSTR_SIZE and
      ! STR_SIZE is the same, so asking for a full STR_SIZE column overflowed
      ! it by exactly the margin width. Release does not check the buffer
      ! cursor, a debug build does -- so this aborted every program that
      ! echoed its command line. Still far wider than a page, so the entries
      ! stay one per line.
         call put_(stdout,self%command_optarg,by_column=TRUE,left=TRUE,width=STR_SIZE-8)
         call unsave_style_(stdout)
      end if
   end subroutine

end module
