!---------------------------------------------------------------------------
!
!  FILE : Unformatted sequential advancing I/O, for fast mass data storage
!
!  NOTE: If ascii files are required consider using TEXTFILE.
!
!  A file is referred to only by a STR name. Unit numbers are not required.
!  Creation of a file object does not lead to creation of the actual file
!  on the computer, it merely creates the label for the file.
!  The actual file may already exist. If it does not exist, then an
!  FILE_open command will bring it into existence. Otherwise, FILE_open
!  will open the existing file. Closing a file does not delete the actual
!  file unless specifically requested.
!
!  When reading or writing an object to the file, it is assumed that
!  each object occupied one abstract "record". After a recor5d is written,
!  it may not be overwritten without destroying all data objects in the
!  following records. It is recommended that multiple data objects which
!  are logically related be stored in different files with appropriate names
!  for each file which indicate the relationship of the data objects
!  within them.
!
!  Strings are regarded as type STR for purposes of output to the file.
!
!  If the read or write statements give a segmentation fault for large
!  arrays or matrices, try increasing your stack size.
!
!  Copyright (C) Dylan Jayatilaka, 1997, 2019
! 
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Library General Public
!  License as published by the Free Software Foundation; either
!  version 2 of the License, or (at your option) any later version.
! 
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Library General Public License for more details.
! 
!  You should have received a copy of the GNU Library General Public
!  License along with this library; if not, write to the
!  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
!  Boston, MA  02111-1307, USA.
!
!---------------------------------------------------------------------------
module FILE_MODULE

#  include "macros"

   include "file.use"

   implicit none

   include "file.int"


contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create_0(self)
   ! Allocate an object
      type(FILE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"FILE:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,name)
   ! Allocate an object. Set the filename to "name".
   ! DOes not open the file!
      type(FILE_TYPE), allocatable, INOUT :: self
      STR(len=*), IN :: name
      call create_(self)
      call set_name_(self,name)
   end subroutine

   subroutine destroy(self)
   ! Destroy a file-label object and close corresponding file, if open
      type(FILE_TYPE), allocatable, INOUT :: self
      if (NOT allocated(self)) return

      ! Close the unit. The tests are NESTED, not "AND"ed: Fortran does not
      ! promise to short-circuit .and., and ".unit_used" inquires on a unit
      ! number that a closed file no longer holds.
      if (is_open_(self)) then
         if (unit_used_(self)) then
            call close_(self)
         end if
      end if
      deallocate(self)
   end subroutine


!  ===========
!  Set methods
!  ===========

   pure subroutine set_name(self,name)
   ! Sets the filename to "name"
      type(FILE_TYPE), INOUT :: self
      STR(len=*), IN :: name
      self%name = name
   end subroutine

   subroutine set_action(self,action)
   ! Set the to "action" indicated whether the file is "read_write",
   ! "read-only" or "write-only".
      type(FILE_TYPE), INOUT :: self
      STR(len=*), IN :: action
      select case (action)
      case ("rw        ","readwrite ","read-write"); self%action = "readwrite"
      case ("r         ","read      ","reading   ","read-only "); self%action = "read"
      case ("w         ","write     ","writing   ","write-only"); self%action = "write"
      case default; allocate(tonto%known_keywords(11))
      tonto%known_keywords(1) = "rw        "
      tonto%known_keywords(2) = "readwrite "
      tonto%known_keywords(3) = "read-write"
      tonto%known_keywords(4) = "r         "
      tonto%known_keywords(5) = "read      "
      tonto%known_keywords(6) = "reading   "
      tonto%known_keywords(7) = "read-only "
      tonto%known_keywords(8) = "w         "
      tonto%known_keywords(9) = "write     "
      tonto%known_keywords(10) = "writing   "
      tonto%known_keywords(11) = "write-only"
      call unknown_(tonto,action,"FILE:set_action")
      deallocate(tonto%known_keywords)
      end select
   end subroutine


!  ==============================
!  Opening, closing, and deletion
!  ==============================

   subroutine open_for(self,action)
   ! Open the file. The file object must already be created.
   ! If present, "action" indicated whether the file is "read_write",
   ! "read-only" or "write-only".
      type(FILE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: action
      if (present(action)) then
         call set_action_(self,action)
      end if
      self%file_status = "new"
      if (exists_(self)) self%file_status = "old"

      ! Open file and set unit number
      if (IO_IS_ALLOWED) then
         open(file=self%name,newunit=self%unit,status=self%file_status,access="sequential",form="unformatted",action=self%action,iostat=self%io_status)
      end if

      ! Check for failure
      PARALLEL_BROADCAST_IO(self%unit,master_processor_(tonto))
      PARALLEL_BROADCAST_IO(self%io_status,master_processor_(tonto))
      DIE_IF(self%io_status/=0,"FILE:open_for ... error opening "//trim(self%file_status)//" file "//self%name)
   end subroutine

   subroutine close(self)
   ! Close the file, if opened, and give up its unit number.
   ! Clearing ".unit" is not tidiness: Fortran hands a closed unit number
   ! straight back to the next "newunit=" open, so an object left holding
   ! one reads or writes whichever file inherited it. See TASKS_AND_HISTORY.md.
      type(FILE_TYPE), INOUT :: self
      BIN :: opened
   DIE_IF(NOT exists_(self),"FILE:close ... not an existing file!")

      ! Check if unit is opened. Only the master inquires, and only when the
      ! file still holds a unit -- a closed one holds FILE_NO_UNIT, which
      ! "inquire" rejects. The broadcast below stays unconditional: no branch
      ! here may skip a collective.
      opened = FALSE
      self%io_status = 0
      if (IO_IS_ALLOWED) then
         if (self%unit/=FILE_NO_UNIT) then
            inquire(unit=self%unit,iostat=self%io_status,opened=opened)
         end if
      end if

      ! Failure?
      PARALLEL_BROADCAST_IO(self%io_status,master_processor_(tonto))
      DIE_IF(self%io_status/=0,"FILE:close ... error inquiring about file "//trim(self%name))

      ! Close the file
      if (IO_IS_ALLOWED) then
         if (opened) then
            close(unit=self%unit)
         end if
      end if

      ! Give up the unit
      self%unit = FILE_NO_UNIT
   end subroutine

   subroutine close_and_delete(self)
   ! Close the file and delete it from the file system
      type(FILE_TYPE), INOUT :: self
      if (IO_IS_ALLOWED) then
         close(unit=self%unit,status="delete",iostat=self%io_status)
      end if

      ! Broadcast before testing, exactly as FILE:close does after its own
      ! guarded inquire. Without this .io_status is set only on the master and
      ! every other rank tests whatever it happened to hold -- so the DIE_IF
      ! below is decided on RANK-LOCAL state. That is the milestone-6 rule one
      ! level down: not a collective gated on rank-local state, but a BRANCH
      ! taken on it, in a routine whose branches contain collectives. Ranks that
      ! branch differently then arrive at different collectives, which is
      ! MPI_ERR_TRUNCATE on the next broadcast.
      PARALLEL_BROADCAST_IO(self%io_status,master_processor_(tonto))
      DIE_IF(self%io_status/=0,"FILE:close_and_delete ... error closing and deleting"//trim(self%file_status)//" file "//self%name)

      ! Give up the unit; see the note in ":close"
      self%unit = FILE_NO_UNIT
   end subroutine

   subroutine delete(self)
   ! Delete the file from the file system
      type(FILE_TYPE), INOUT :: self
      if (NOT is_open_(self)) call open_for_(self,"read")
      call close_and_delete_(self)
   end subroutine

!  ==========================
!  Data input type operations
!  ==========================

!  Scalars

   subroutine read_0(self,value)
   ! Read a "value" from the file.
      type(FILE_TYPE), INOUT :: self
      INT, OUT :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:read_0 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(value,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_1(self,value)
   ! Read a "value" from the file.
      type(FILE_TYPE), INOUT :: self
      REAL, OUT :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:read_1 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(value,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_2(self,value)
   ! Read a "value" from the file.
      type(FILE_TYPE), INOUT :: self
      CPX, OUT :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:read_2 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(value,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_3(self,value)
   ! Read a "value" from the file.
      type(FILE_TYPE), INOUT :: self
      STR(len=*), OUT :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:read_3 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(value,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

!  Arrays

   subroutine read_4(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      VEC(INT,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_4 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_5(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_5 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_6(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      VEC(CPX,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_6 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_7(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_7 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_8(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT(INT,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_8 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_9(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_9 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_10(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT(CPX,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_10 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_11(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT3(INT,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_11 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_12(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_12 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_13(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_13 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_14(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT4(INT,:,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_14 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_15(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_15 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_16(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_16 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_17(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT5(REAL,:,:,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_17 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

   subroutine read_18(self,values)
   ! Read a series of "values" from the file.
      type(FILE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), OUT :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         read(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:read_18 ... read error, #"//trim(to_str_(self%io_status)))
      end if
      PARALLEL_BROADCAST_IO(values,master_processor_(tonto))
      self%record = self%record+1
   end subroutine

!  Vectors of embedded arrays

   subroutine read_19(self,v)
   ! Read an vector of embedded array types 
      type(FILE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), OUT :: v
      INT :: i

      ! The following code is inherited from FILE
      do i = 1,size(v)
         if (IO_IS_ALLOWED) then
            read(unit=self%unit,iostat=self%io_status) v(i)%element
            ENSURE(self%io_status==0,"FILE:read_19 ... read error, #"//trim(to_str_(self%io_status)))
         end if
         PARALLEL_BROADCAST_IO(v(i)%element,master_processor_(tonto))
         self%record = self%record+1
      end do
   end subroutine

!  ============================
!  Parallel data input routines
!  ============================

!  Allow each processor to separately do a read

!  Arrays

!   parallel_read(values) :: template
!   ! Parallel read the "values" array from the file.
!      self :: INOUT
!      values :: VALUES_TYPE?, OUT
!
!      read(unit=.unit,iostat=.io_status) values
!      ENSURE(.io_status==0,"write error, #"//trim(.io_status.to_str))
!
!      .record = .record + 1
!
!   end
!
!   parallel_read(values) :: get_from(FILE, VALUES_TYPE?=>VEC{INT})
!   ! Parallel read the "values" array from the file.
!   end
!
!   parallel_read(values) :: get_from(FILE, VALUES_TYPE?=>VEC{REAL})
!   ! Parallel read the "values" array from the file.
!   end
!
!   parallel_read(values) :: get_from(FILE, VALUES_TYPE?=>VEC{CPX})
!   ! Parallel read the "values" array from the file.
!   end
!
!   parallel_read(values) :: get_from(FILE, VALUES_TYPE?=>MAT{INT})
!   ! Parallel read the "values" array from the file.
!   end
!
!   parallel_read(values) :: get_from(FILE, VALUES_TYPE?=>MAT{REAL})
!   ! Parallel read the "values" array from the file.
!   end
!
!   parallel_read(values) :: get_from(FILE, VALUES_TYPE?=>MAT{CPX})
!   ! Parallel read the "values" array from the file.
!   end

!  ====================
!  Data output routines
!  ====================

!  Scalars

   subroutine write_0(self,value)
   ! Write the "value" to the file.
      type(FILE_TYPE), INOUT :: self
      INT, IN :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:write_0 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_1(self,value)
   ! Write the "value" to the file.
      type(FILE_TYPE), INOUT :: self
      REAL, IN :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:write_1 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_2(self,value)
   ! Write the "value" to the file.
      type(FILE_TYPE), INOUT :: self
      CPX, IN :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:write_2 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_3(self,value)
   ! Write the "value" to the file.
      type(FILE_TYPE), INOUT :: self
      STR(len=*), IN :: value

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) value
         ENSURE(self%io_status==0,"FILE:write_3 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

!  Arrays

   subroutine write_4(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(INT,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_4 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_5(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_5 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_6(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_6 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_7(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(STR(len=*),:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_7 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_8(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_8 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_9(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_9 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_10(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_10 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_11(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT3(INT,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_11 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_12(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_12 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_13(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_13 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_14(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT4(INT,:,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_14 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_15(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_15 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_16(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_16 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_17(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT5(REAL,:,:,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_17 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

   subroutine write_18(self,values)
   ! Write the "values" to the file.
      type(FILE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), IN :: values

      ! The following code is inherited from FILE
      if (IO_IS_ALLOWED) then
         write(unit=self%unit,iostat=self%io_status) values
         ENSURE(self%io_status==0,"FILE:write_18 ... write error, #"//trim(to_str_(self%io_status)))
      end if
      self%record = self%record+1
   end subroutine

!  Vectors of embedded arrays

   subroutine write_19(self,v)
   ! Write a vector of embedded array types 
      type(FILE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), IN :: v
      INT :: i

      ! The following code is inherited from FILE
      do i = 1,size(v)
         if (IO_IS_ALLOWED) then
            write(unit=self%unit,iostat=self%io_status) v(i)%element
            ENSURE(self%io_status==0,"FILE:write_19 ... read error, #"//trim(to_str_(self%io_status)))
         end if
         self%record = self%record+1
      end do
   end subroutine

!  =============================
!  Parallel data output routines
!  =============================

!  Allow each processor to separately do a write

!  Arrays

   subroutine per_rank_write_0(self,values)
   ! Parallel write the "value" array to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(INT,:), IN :: values

      ! The following code is inherited from FILE
      write(unit=self%unit,iostat=self%io_status) values
      ENSURE(self%io_status==0,"FILE:per_rank_write_0 ... write error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine

   subroutine per_rank_write_1(self,values)
   ! Parallel write the "value" array to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: values

      ! The following code is inherited from FILE
      write(unit=self%unit,iostat=self%io_status) values
      ENSURE(self%io_status==0,"FILE:per_rank_write_1 ... write error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine

   subroutine per_rank_write_2(self,values)
   ! Parallel write the "value" array to the file.
      type(FILE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: values

      ! The following code is inherited from FILE
      write(unit=self%unit,iostat=self%io_status) values
      ENSURE(self%io_status==0,"FILE:per_rank_write_2 ... write error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine

   subroutine per_rank_write_3(self,values)
   ! Parallel write the "value" array to the file.
      type(FILE_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: values

      ! The following code is inherited from FILE
      write(unit=self%unit,iostat=self%io_status) values
      ENSURE(self%io_status==0,"FILE:per_rank_write_3 ... write error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine

   subroutine per_rank_write_4(self,values)
   ! Parallel write the "value" array to the file.
      type(FILE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: values

      ! The following code is inherited from FILE
      write(unit=self%unit,iostat=self%io_status) values
      ENSURE(self%io_status==0,"FILE:per_rank_write_4 ... write error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine

   subroutine per_rank_write_5(self,values)
   ! Parallel write the "value" array to the file.
      type(FILE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: values

      ! The following code is inherited from FILE
      write(unit=self%unit,iostat=self%io_status) values
      ENSURE(self%io_status==0,"FILE:per_rank_write_5 ... write error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine


!  ==================================
!  File repositioning type operations
!  ==================================

   subroutine rewind(self)
   ! Rewind the file
      type(FILE_TYPE), INOUT :: self
      if (IO_IS_ALLOWED) rewind(unit=self%unit,iostat=self%io_status)
      ENSURE(self%io_status==0,"FILE:rewind ... rewind error, #"//trim(to_str_(self%io_status)))
      self%record = 1
   end subroutine

   subroutine backspace(self)
   ! Backspace the file one record of the file. Backspacing before
   ! record 1 has no effect.
      type(FILE_TYPE), INOUT :: self
      if (IO_IS_ALLOWED) backspace(unit=self%unit,iostat=self%io_status)
      ENSURE(self%io_status==0,"FILE:backspace ... backspace error, #"//trim(to_str_(self%io_status)))
      self%record = max(1,self%record-1)
   end subroutine

   subroutine skip(self)
   ! Skip over the next record of the file. An error is generated if
   ! at the end of the file
      type(FILE_TYPE), INOUT :: self
      if (IO_IS_ALLOWED) read(unit=self%unit,iostat=self%io_status)
      ENSURE(self%io_status==0,"FILE:skip ... read error, #"//trim(to_str_(self%io_status)))
      self%record = self%record+1
   end subroutine

   subroutine move_to_end(self)
   ! Move to the end of the file, say for adding more data
      type(FILE_TYPE), INOUT :: self
      do
         if (IO_IS_ALLOWED) read(unit=self%unit,iostat=self%io_status)
         PARALLEL_BROADCAST_IO(self%io_status,master_processor_(tonto))
         if (self%io_status/=0) exit
         self%record = self%record+1
      end do
   end subroutine

   subroutine move_to_position(self,pos)
   ! Move to a particular record position in the file
      type(FILE_TYPE), INOUT :: self
      INT, IN :: pos
      call move_to_record_(self,pos)
   end subroutine

   subroutine move_to_record(self,rec)
   ! Move to a particular record position in the file
      type(FILE_TYPE), INOUT :: self
      INT :: rec
   ENSURE(rec>=1,"FILE:move_to_record ... can't move to non-positive record")
      if (rec<position_(self)) then
         do
            call backspace_(self)
            if (position_(self)==rec) exit
         end do
      else if (rec>position_(self)) then
         do
            call skip_(self)
            if (position_(self)==rec) exit
         end do
      end if
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   function exists(self,name) result (res)
   ! Returns true if the file exists on the file system.  If present,
   ! "name" is used, otherwise .name is used.
      type(FILE_TYPE), IN :: self
      STR(len=*), optional, IN :: name
      BIN :: res
      if (present(name)) then
         if (IO_IS_ALLOWED) inquire(file=name,exist=res)
      else
         if (IO_IS_ALLOWED) inquire(file=self%name,exist=res)
      end if
      PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
   end function

   function is_open(self) result (res)
   ! Returns true if the file has been opened
      type(FILE_TYPE), IN :: self
      BIN :: res
      if (IO_IS_ALLOWED) then

      ! inquire(unit=.unit,opened=res)
         inquire(file=self%name,opened=res)
      end if
      PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
   end function

   function unit_used(self) result (res)
   ! Returns true if the file unit is in use.
   ! A closed file holds no unit, and inquiring on one aborts.
      type(FILE_TYPE), IN :: self
      BIN :: res
      res = FALSE
      if (IO_IS_ALLOWED) then
         if (self%unit/=FILE_NO_UNIT) then
            inquire(unit=self%unit,opened=res)
         end if
      end if
      PARALLEL_BROADCAST_IO(res,master_processor_(tonto))
   end function

   pure function position(self) result (res)
   ! Return record position of the file
      type(FILE_TYPE), IN :: self
      INT :: res
      res = self%record
   end function

   function is_for_reading(self) result (res)
   ! Return whether the file is opened for reading.
   ! A closed file holds no unit, and inquiring on one aborts.
      type(FILE_TYPE), IN :: self
      BIN :: res
      STR(len=5) :: reading
      reading = "NO"
      if (IO_IS_ALLOWED) then
         if (self%unit/=FILE_NO_UNIT) then
            inquire(unit=self%unit,read=reading)
         end if
      end if
      PARALLEL_BROADCAST_IO(reading,master_processor_(tonto))
      if (reading=="YES") then; res = TRUE
      else; res = FALSE
      end if
   end function

end module
