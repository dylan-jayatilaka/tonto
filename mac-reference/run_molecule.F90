!==================================================================
! Copyright (C) Dylan Jayatilaka, 1999
! Copyright (C) Dylan Jayatilaka, 2023
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

! The version, commit and build date (GIT_VERSION ...), remade on every build
#  include "build_stamp"
program run_MOLECULE

#  include "macros"

   include "run_molecule.use"

   implicit none

   type(COMMAND_LINE_TYPE) :: command_line
   STR(len=STR_SIZE) :: option
   STR(len=PATH_SIZE) :: value  ! an option value is often a path
   STR(len=PATH_SIZE) :: infile,outfile,basis_library_dir  ! paths
   INT :: i
   INT :: maxlen  ! len() into a declared INT: see CLAUDE.md sec 8
   type(MOLECULE_TYPE), allocatable :: m

   ! Macro to create Tonto system object
   ! Initialise MPI parallel stuff too.
   TONTO_CREATE

   ! Assign std_err to screen
   call create_std_err
   call open_for_(std_err,"write")

   ! Assign stdout to screen
   call create_stdout

   ! Get command line
   call process_options_(command_line)
   DIE_IF(has_arguments_(command_line),"run_MOLECULE:run_MOLECULE ... illegal arguments; use --options only e.g. --help")

   ! Default option values
   infile = " "
   outfile = " "
   basis_library_dir = " "

   ! Analyze command line options
   do i = 1,command_line%n_options

      ! Get options
      option = command_line%option(i)
      value = command_line%option_value(i)

      ! Analyze options
      select case (option)

      case ("input")
         infile = value

      case ("output")
         outfile = value

      case ("basis-library")
         basis_library_dir = value

      case ("help")
         call text_(stdout,"Usage:")
         call flush_(stdout)
         call text_(stdout,"  tonto [--help] [--input INPUT] [--output OUTPUT]")
         call flush_(stdout)
         call text_(stdout,"        [--basis-library BASIS_LIBRARY_DIR] [--version]")
         call flush_(stdout)
         call text_(stdout,"Optional arguments:")
         call flush_(stdout)
         call text_(stdout,"--help")
         call flush_(stdout)
         call text_(stdout,"  Show this help message and exit")
         call flush_(stdout)
         call text_(stdout,"--input INPUT")
         call flush_(stdout)
         call text_(stdout,"  Set input file to INPUT [default='stdin']")
         call flush_(stdout)
         call text_(stdout,"--output OUTPUT")
         call flush_(stdout)
         call text_(stdout,"  Set output file to OUTPUT [default='stdout']")
         call flush_(stdout)
         call text_(stdout,"--basis-library BASIS_LIBRARY_DIR")
         call flush_(stdout)
         call text_(stdout,"  Set the basis set library directory to BASIS_LIBRARY_DIR [default='"//VEC_BASIS_LIBRARY_DIRECTORY//"']")
         call flush_(stdout)
         call text_(stdout,"--version")
         call flush_(stdout)
         call text_(stdout,"  Return the version of tonto and stop")
         ! Stop, like --version below. Without this, --help printed the usage
         ! and then fell through to stdin.open_for("read"), dying on a missing
         ! input file straight after telling the user how to supply one.
         stop

      case ("version")
         call text_(stdout,"This is Tonto version"//TONTO_VERSION//" "//GIT_VERSION)
         stop

      case default
         DIE("run_MOLECULE:run_MOLECULE ... unknown option: "//trim(option))
      end select
   end do

   ! Renitialise stdout?
   if (outfile/=" ") then
      call destroy_(stdout)
      call create_(stdout,outfile)
   end if
   call open_for_(stdout,"write")
   call set_stdout_file_(tonto,stdout)

   ! Reinitialise std_err: errors go into the output file from here on
   call redirect_std_err_to(stdout)

   ! Assign stdin stdin
   if (infile/=" ") then; call create_(stdin,infile)
   else; call create_stdin
   end if
   call open_for_(stdin,"read")
   if (basis_library_dir==" ") then

      ! status is -1 when the value was TRUNCATED to fit, and >0 when the
      ! variable is unset. The status used to be ignored entirely, so an
      ! over-long path was silently cut and the failure surfaced later as
      ! "no library basis set" -- naming the basis rather than the path.
      ! See TASKS_AND_HISTORY.md, "long paths to the basis sets fail".
      call get_environment_variable(BASIS_LIBRARY_ENV_NAME,basis_library_dir,status=i)
      maxlen = len(basis_library_dir)
      DIE_IF(i<0,"run_MOLECULE:run_MOLECULE ... the "//BASIS_LIBRARY_ENV_NAME//" path is too long: it does "//"not fit in "//trim(to_str_(maxlen))//" characters")
      if (i/=0) basis_library_dir = " "
   end if

   ! Run the molecule
   call run_(m,basis_library_dir)

   ! Clean-up molecule
   call cleanup_

   ! Clean-up files
   call destroy_(stdout)
   call destroy_(stdin)
   call destroy_(std_err)

   ! Clean-up tonto system
   TONTO_DESTROY

end program
