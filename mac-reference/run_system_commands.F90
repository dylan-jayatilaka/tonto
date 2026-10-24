! Exerciser for the SYSTEM_COMMAND virtual module, which is inherited into
! SYSTEM -- so every method below is called on the global "tonto" object.
!
! It is self-checking: every test either passes or increments a counter, and the
! program DIEs (exit 1) if any failed. That makes it usable as a ctest, which is
! the point -- shell execution is exactly the kind of thing that silently does
! nothing, and this codebase has been bitten by that repeatedly.
!
! Run it by hand with:
!    make run_system_commands && ./run_system_commands
!
! Copyright (C) Dylan Jayatilaka 2014
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
program run_SYSTEM_COMMANDS

#  include "macros"

   include "run_system_commands.use"

   implicit none

   type(TEXTFILE_TYPE), allocatable :: f
   STR(len=STR_SIZE) :: tmp,word
   INT :: n_failed
   BIN :: ok,failed,there
   INT :: exit_status,status

   ! Create Tonto system object
   ! Initialise MPI parallel stuff too.
   TONTO_CREATE

   ! Initialise standard I/O files.
   call start_timing_(std_time)
   call create_std_err
   call open_for_(std_err,"write")
   call create_stdout
   n_failed = 0
   tmp = "run_system_commands.tmp"
   call flush_(stdout)
   call text_(stdout,"========================")
   call text_(stdout,"SYSTEM_COMMAND exerciser")
   call text_(stdout,"========================")
   call flush_(stdout)

   ! ---------------------------------------------------------------------
   ! 1. Defaults. A freshly created tonto must have an empty command slot.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 1: defaults are set")
   ok = tonto%command==" " AND tonto%command_wait
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)

   ! ---------------------------------------------------------------------
   ! 2. A command that succeeds. Nothing may be reported as having failed.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 2: a command that succeeds")
   call set_command_(tonto,"true")
   call execute_command_(tonto)
   exit_status = tonto%command_exit_status
   status = tonto%command_status
   failed = command_failed_(tonto)
   call show_(stdout,"... exit status               =",exit_status)
   call show_(stdout,"... command status            =",status)
   ok = exit_status==0 AND status==0 AND (NOT failed)
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)

   ! ---------------------------------------------------------------------
   ! 3. The shell really runs: write a file, and read back what was written.
   !    This is the test that matters. A do-nothing implementation passes
   !    tests 1 and 2 by accident; it cannot pass this one.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 3: the command really executes")
   call execute_(tonto,"echo tonto_was_here > "//trim(tmp))
   there = exists_(stdout,tmp)
   call show_(stdout,"... file was created?         =",there)
   word = " "
   if (there) then
      call create_(f,tmp)
      call open_for_(f,"read")
      call read_(f,word)
      call close_(f)
      call destroy_(f)
   end if
   call show_(stdout,"... file contains             =",word)
   ok = there AND word=="tonto_was_here"
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)

   ! ---------------------------------------------------------------------
   ! 4. A command that fails must be reported as failing. `sh` returns the
   !    exit code of `exit 3` verbatim, so this is deterministic.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 4: a command that fails is detected")
   call execute_(tonto,"exit 3")
   exit_status = tonto%command_exit_status
   failed = command_failed_(tonto)
   call show_(stdout,"... exit status               =",exit_status)
   call show_(stdout,"... reported as failed?       =",failed)
   ok = exit_status==3 AND failed
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)

   ! ---------------------------------------------------------------------
   ! 5. A command that does not exist. The shell reports 127; what matters
   !    is only that .command_failed says so rather than staying silent.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 5: a non-existent command is detected")
   call execute_(tonto,"no_such_program_exists_here_1234")
   exit_status = tonto%command_exit_status
   status = tonto%command_status
   failed = command_failed_(tonto)
   call show_(stdout,"... exit status               =",exit_status)
   call show_(stdout,"... command status            =",status)
   call show_(stdout,"... reported as failed?       =",failed)
   ok = failed
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)

   ! ---------------------------------------------------------------------
   ! 6. set_command clears the previous result, so a stale failure cannot
   !    be reported against a command which has not been run.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 6: set_command clears the previous result")
   call set_command_(tonto,"true")
   exit_status = tonto%command_exit_status
   failed = command_failed_(tonto)
   ok = exit_status==0 AND (NOT failed)
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)

   ! ---------------------------------------------------------------------
   ! 7. Asynchronous execution. Nothing is reported back, so the only
   !    contract is that it runs and claims no failure.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 7: asynchronous execution")
   call set_command_(tonto,"true")
   call set_command_asynchronous_(tonto)
   call execute_command_(tonto)
   failed = command_failed_(tonto)
   ok = NOT failed
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... passed?                   =",ok)
   call set_command_synchronous_(tonto)

   ! ---------------------------------------------------------------------
   ! 8. put_command_info prints something sane. Also cleans up test 3.
   ! ---------------------------------------------------------------------
   call text_(stdout,"Test 8: put_command_info")
   call execute_(tonto,"rm -f "//trim(tmp))
   call put_command_info_(tonto)
   there = exists_(stdout,tmp)
   ok = NOT there
   if (NOT ok) n_failed = n_failed+1
   call show_(stdout,"... temporary file removed?   =",ok)

   ! ---------------------------------------------------------------------
   ! Verdict
   ! ---------------------------------------------------------------------
   call flush_(stdout)
   call show_(stdout,"Number of failed tests        =",n_failed)
   call flush_(stdout)
   if (n_failed==0) then
      call text_(stdout,"ALL SYSTEM_COMMAND TESTS PASSED")
   else
      call text_(stdout,"SOME SYSTEM_COMMAND TESTS FAILED")
   end if
   call flush_(stdout)
   DIE_IF(n_failed>0,"run_SYSTEM_COMMANDS:run_SYSTEM_COMMANDS ... SYSTEM_COMMAND exerciser failed")

   ! Clean-up files
   call destroy_(std_err)
   call destroy_(stdout)

   ! Clean-up tonto system
   TONTO_DESTROY

end program
