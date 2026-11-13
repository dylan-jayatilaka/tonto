!=======================================================================
!
! ARCHIVE: For archiving tonto objects to disk
!
! A polymorphic file object which can store ascii or binary format.
! Archives with the string "ascii" somewhere in their name are assumed
! to be ascii files, otherwise they are binary files.
!
! An archive can't be used for input and output at the same time.
! Also, read and write routines are used to deal with entire single
! objects.  The archive is closed after the requested IO operation.
!
! If the above behaviour is not what is needed, the archive can be opened
! for multiple object I/O in one archive (say, a list of integral blocks)
! and you can deal "by hand" with the appropriate object of the archive.
!
! Copyright (C) Dylan Jayatilaka, 1998
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
!=======================================================================
module ARCHIVE_MODULE

#  include "macros"

   include "archive.use"

   implicit none

   include "archive.int"


contains

!  ==================
!  Allocation methods
!  ==================

   PURE subroutine create_0(self)
   ! Allocate an object
      type(ARCHIVE_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"ARCHIVE:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(ARCHIVE_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"ARCHIVE:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   subroutine create_1(self,root_name,name,genre,format)
   ! Reset an archive to have main name "root_name", sub name "name".
   ! "genre" (if present) is used to identify OPVECTOR components.
   ! "format" (if present) is used to identify file format (e.g. ascii). 
   ! The default is binary.
      type(ARCHIVE_TYPE), allocatable, OUT :: self
      STR(len=*), IN :: root_name
      STR(len=*), optional, IN :: name
      STR(len=*), optional, IN :: genre
      STR(len=*), optional, IN :: format
      call create_(self)
      call set_(self,root_name,name,genre,format)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set(self,root_name,name,genre,format)
   ! Reset an archive to have main name "root_name", sub name "name"
   ! "genre" (if present) is used to identify components of OPMATRIX and
   ! OPVECTOR objects.
   ! "format" (if present) is used to identify file format (e.g. ascii). The
   ! default is binary.
   ! Otherwise use defaults.
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: root_name
      STR(len=*), optional, IN :: name
      STR(len=*), optional, IN :: genre
      STR(len=*), optional, IN :: format
      if (present(root_name)) self%root_name = root_name
      if (present(name)) self%name = name
      if (present(genre)) self%genre = genre
      if (present(format)) self%format = format
   end subroutine

   PURE subroutine set_root_name(self,root_name)
   ! Set an archive to have sub name "name".
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), IN :: root_name
      self%root_name = root_name
   end subroutine

   PURE subroutine set_root(self,root_name)
   ! Set an archive to have sub name "name".
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), IN :: root_name
      self%root_name = root_name
   end subroutine

   PURE subroutine set_name(self,name)
   ! Set an archive to have sub name "name".
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), IN :: name
      self%name = name
   end subroutine

   PURE subroutine set_genre(self,genre)
   ! Set an archive to have genre "genre".
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), IN :: genre
      self%genre = genre
   end subroutine

   PURE subroutine set_format(self,format)
   ! Set an archive to have format "format".
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), IN :: format
      self%format = format
   end subroutine

   PURE subroutine set_use_all_commas(self,val)
   ! Set whether to use all commas in the archive name
      type(ARCHIVE_TYPE), INOUT :: self
      BIN, IN :: val
      self%use_all_commas = val
   end subroutine


!  ===============
!  Inquiry methods
!  ===============

   PURE function file_name(self,genre) result (res)
   ! Return the archive file name from string ".root_name" by prepending the
   ! archive header ".name". If present "genre" is also appended. If present,
   ! ".format" is also appended.
      type(ARCHIVE_TYPE), IN :: self
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: res
      STR(len=STR_SIZE) :: k
   ENSURE(self%root_name/=" ","ARCHIVE:file_name ... must have an archive root_name!")
      res = trim(self%root_name)

      ! Add the file name
      if (self%name/=" ") then
         if (self%use_all_commas) then
            res = trim(res)//","//trim(self%name)
         else
            res = trim(res)//"."//trim(self%name)
         end if
      end if

      ! Add the genrw
      k = self%genre
      if (present(genre)) k = genre
      if (k/=" ") res = trim(res)//","//trim(k)

      ! Add the format
      if (self%format/=" ") res = trim(res)//","//self%format
   end function

   PURE function is_ascii(self) result (res)
   ! Return true if the file is a text (ASCII) file
      type(ARCHIVE_TYPE), IN :: self
      BIN :: res
      res = includes_(self%format,"ascii")
   end function

   function exists(self,genre) result (res)
   ! Return TRUE if the archive exists in some form on disk.
      type(ARCHIVE_TYPE), IN :: self
      STR(len=*), optional, IN :: genre
      BIN :: res
      STR(len=STR_SIZE) :: file_name
      type(TEXTFILE_TYPE), allocatable :: Afile
      type(FILE_TYPE), allocatable :: Bfile
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(Afile,trim(file_name))
         res = exists_(Afile)
         call destroy_(Afile)
      else
         call create_(Bfile,trim(file_name))
         res = exists_(Bfile)
         call destroy_(Bfile)
      end if
   end function


!  ==============================
!  Opening, closing, and deletion
!  ==============================

   subroutine open_for(self,action,genre,ascii)
   ! Open archive file for a certain "action" either readin or
   ! writing. Specify also thge "genre" and whether the file should be
   ! "ascii" or not (default not).
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), IN :: action
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,action)
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,action)
      end if
   end subroutine

   subroutine close(self)
   ! Close *and* destroy the archive file part.
      type(ARCHIVE_TYPE), INOUT :: self
      if (allocated(self%Afile)) then
         call close_(self%Afile)
         call destroy_(self%Afile)
      end if
      if (allocated(self%Bfile)) then
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine delete(self,genre)
   ! Delete archive file if it exists, *and* destroy the archive file part
      type(ARCHIVE_TYPE), INOUT :: self
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: file_name
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then

         ! ASCII file
         call create_(self%Afile,trim(file_name))
         if (exists_(self%Afile)) call delete_(self%Afile)
         call destroy_(self%Afile)
      else

         ! Binary file
         call create_(self%Bfile,trim(file_name))
         if (exists_(self%Bfile)) call delete_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine delete_all_genres(self)
   ! Delete all genres of archive file, if they exists.
      type(ARCHIVE_TYPE), INOUT :: self
      call delete_(self)
      call delete_(self,"r ")
      call delete_(self,"a ")
      call delete_(self,"b ")
      call delete_(self,"gc")
   end subroutine

!  =========================================
!  Whole-object intrinsic-type read routines
!  =========================================

   subroutine read_0(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      INT, OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_0 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_0 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_1(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      REAL, OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_1 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_1 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_2(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      CPX, OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_2 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_2 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_3(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(INT,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_3 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_3 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_4(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_4 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_4 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_5(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(CPX,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_5 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_5 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_6(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_6 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_6 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_7(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_7 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_7 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_8(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_8 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_8 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_9(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_9 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_9 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_10(self,item,genre)
   ! Read from the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), OUT :: item
      STR(len=*), IN, optional :: genre
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_10 ... no text archive "//trim(self%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_10 ... no binary archive "//trim(self%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_11(self,item,genre,by_column)
   ! Read a matrix "item" from the archive. If present, "by_column" is
   ! the input order for the matrix.
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), OUT :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_column
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_11 ... no text archive "//trim(self%Afile%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_11 ... no binary archive "//trim(self%Bfile%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_12(self,item,genre,by_column)
   ! Read a matrix "item" from the archive. If present, "by_column" is
   ! the input order for the matrix.
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(CPX,:,:), OUT :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_column
      STR(len=STR_SIZE) :: file_name

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      if (is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         ENSURE(exists_(self%Afile),"ARCHIVE:read_12 ... no text archive "//trim(self%Afile%name))
         call open_for_(self%Afile,"read")
         call read_(self%Afile,item,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         ENSURE(exists_(self%Bfile),"ARCHIVE:read_12 ... no binary archive "//trim(self%Bfile%name))
         call open_for_(self%Bfile,"read-only")
         call read_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine read_13(self,item,genre)
   ! Read a binary archive.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), OUT :: item
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: file_name
   DIE_IF(is_ascii_(self),"ARCHIVE:read_13 ... text files not implemented")

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self,genre)
      call create_(self%Bfile,trim(file_name))
      ENSURE(exists_(self%Bfile),"ARCHIVE:read_13 ... no binary archive "//trim(file_name))
      call open_for_(self%Bfile,"read-only")
      call read_(self%Bfile,item)
      call close_(self%Bfile)
      call destroy_(self%Bfile)
   end subroutine

   subroutine read_14(self,item,genre)
   ! Read from the archive operator vector "item". If present, "genre"
   ! is the genre of "item" to be read in.
      type(ARCHIVE_TYPE), INOUT :: self
      type(OPVECTOR_TYPE), INOUT :: item
      STR(len=*), optional, IN :: genre
      STR(len=STR_SIZE) :: item_genre
      if (present(genre)) then; item_genre = genre
      else if (self%genre/=" ") then; item_genre = self%genre
      else; item_genre = genre_(item)
      end if
      select case (item_genre)

      case ("r")
         call destroy_(item,"r")
         call create_(item,"r")
         call read_(self,item%r,genre="r")

      case ("u")
         call destroy_(item,"a")
         call create_(item,"a")
         call destroy_(item,"b")
         call create_(item,"b")
         call read_(self,item%a,genre="a")
         call read_(self,item%b,genre="b")

      case ("g")
         call destroy_(item,"g")
         call create_(item,"g")
         call read_(self,item%g,genre="g")

      case default
         DIE("ARCHIVE:read_14 ... unknown genre, "//trim(item_genre))
      end select
   end subroutine

   subroutine read_15(self,item,genre,by_column)
   ! Read from the archive operator matrix "item". If present, "genre" is the
   ! genre of "item" to be read in. "order" indicated the input order for ascii
   ! files.
      type(ARCHIVE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), INOUT :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_column
      STR(len=STR_SIZE) :: itemgenre
      if (present(genre)) then; itemgenre = genre
      else if (self%genre/=" ") then; itemgenre = self%genre
      else; itemgenre = genre_(item)
      end if
      select case (itemgenre)

      case ("r              ","restricted     ")
         call destroy_(item,"r ")
         call create_(item,"r ")
         call read_(self,item%r,"r ",by_column)

      case ("u              ","unrestricted   ")
         call destroy_(item,"a")
         call create_(item,"a")
         call destroy_(item,"b")
         call create_(item,"b")
         call read_(self,item%a,"a",by_column)
         call read_(self,item%b,"b",by_column)

      case ("gc             ","general_complex")
         call destroy_(item,"gc")
         call create_(item,"gc")
         call read_(self,item%gc,"gc",by_column)

      case default
         DIE("ARCHIVE:read_15 ... unknown genre, "//trim(itemgenre))
      end select
   end subroutine

!  ===================================
!  Parallel whole-object read routines
!  ===================================

!   parallel_read(item,genre) :: template
!   ! Allow parallel read from the archive, a single "item".
!      self :: INOUT
!      item :: ITEM_TYPE?, OUT
!      genre :: STR, IN, optional
!
!      file_name :: STR
!
!      file_name = .file_name(genre)
!
!      if (.is_ascii) then
!
!         .Afile.create(trim(file_name))
!         ENSURE(.Afile.exists,"no text archive "// trim(.name))
!         .Afile.open_for("read")
!         .Afile.parallel_read(item)
!         .Afile.close
!         .Afile.destroy
!
!      else
!
!         .Bfile.create(trim(file_name))
!         ENSURE(.Bfile.exists,"no binary archive "// trim(.name))
!         .Bfile.open_for("read-only")
!         .Bfile.parallel_read(item)
!         .Bfile.close
!         .Bfile.destroy
!
!      end
!
!   end
!
!   parallel_read(item,genre) :: get_from(ARCHIVE, ITEM_TYPE?=>VEC{INT})
!   ! Allow parallel read from the archive, a single "item".
!   end
!
!   parallel_read(item,genre) :: get_from(ARCHIVE, ITEM_TYPE?=>VEC{REAL})
!   ! Allow parallel read from the archive, a single "item".
!   end
!
!   parallel_read(item,genre) :: get_from(ARCHIVE, ITEM_TYPE?=>VEC{CPX})
!   ! Allow parallel read from the archive, a single "item".
!   end
!
!   parallel_read(item,genre) :: get_from(ARCHIVE, ITEM_TYPE?=>MAT{INT})
!   ! Allow parallel read from the archive, a single "item".
!   end
!
!   parallel_read(item,genre) :: get_from(ARCHIVE, ITEM_TYPE?=>MAT{REAL})
!   ! Allow parallel read from the archive, a single "item".
!   end
!
!   parallel_read(item,genre) :: get_from(ARCHIVE, ITEM_TYPE?=>MAT{CPX})
!   ! Allow parallel read from the archive, a single "item".
!   end

!  ==========================================
!  Whole-object intrinsic-type write routines
!  ==========================================

   subroutine write_0(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      INT, IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_1(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      REAL, IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_2(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      CPX, IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_3(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT3(REAL,:,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_4(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT3(CPX,:,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_5(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT4(REAL,:,:,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_6(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT4(CPX,:,:,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_7(self,item,genre,ascii)
   ! Write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT5(CPX,:,:,:,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_8(self,item,genre,by_row,by_column,ascii)
   ! Write to the archive, a single vector or matrix "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(INT,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item,by_row,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_9(self,item,genre,by_row,by_column,ascii)
   ! Write to the archive, a single vector or matrix "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item,by_row,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_10(self,item,genre,by_row,by_column,ascii)
   ! Write to the archive, a single vector or matrix "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item,by_row,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_11(self,item,genre,by_row,by_column,ascii)
   ! Write to the archive, a single vector or matrix "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item,by_row,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_12(self,item,genre,by_row,by_column,ascii)
   ! Write to the archive, a single vector or matrix "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_row
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call put_(self%Afile,item,by_row,by_column)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_13(self,item1,item2,ascii)
   ! Write to the archive, "item1" followed by "item2".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: item1
      VEC(REAL,:), IN :: item2
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call put_(self%Afile,item1)
         call put_(self%Afile,item2)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item1)
         call write_(self%Bfile,item2)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_14(self,item1,item2,ascii)
   ! Write to the archive, "item1" followed by "item2".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: item1
      MAT(REAL,:,:), IN :: item2
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call put_(self%Afile,item1)
         call put_(self%Afile,item2)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item1)
         call write_(self%Bfile,item2)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_15(self,item1,item2,ascii)
   ! Write to the archive, "item1" followed by "item2".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: item1
      VEC(REAL,:), IN :: item2
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call put_(self%Afile,item1)
         call put_(self%Afile,item2)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item1)
         call write_(self%Bfile,item2)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_16(self,item1,item2,ascii)
   ! Write to the archive, "item1" followed by "item2".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: item1
      MAT(REAL,:,:), IN :: item2
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call open_for_(self%Afile,"write")
         call set_using_array_labels_(self%Afile,FALSE)
         call put_(self%Afile,item1)
         call put_(self%Afile,item2)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call write_(self%Bfile,item1)
         call write_(self%Bfile,item2)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine write_17(self,item)
   ! Write to a binary archive
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(type(EMAT_REAL_TYPE),:), IN :: item
      STR(len=STR_SIZE) :: file_name
   DIE_IF(is_ascii_(self),"ARCHIVE:write_17 ... text files not implemented")

      ! The following code is inherited from ARCHIVE
      file_name = file_name_(self)
      call create_(self%Bfile,trim(file_name))
      call open_for_(self%Bfile,"write-only")
      call write_(self%Bfile,item)
      call close_(self%Bfile)
      call destroy_(self%Bfile)
   end subroutine


! OPVECTOR and OPMATRIX

   subroutine write_18(self,item,genre,by_column,ascii)
   ! Write to the archive operator vector "item". "by_column"
   ! indicated the output order for ascii files.
      type(ARCHIVE_TYPE), INOUT :: self
      type(OPVECTOR_TYPE), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: itemgenre
      if (present(genre)) then; itemgenre = genre
      else; itemgenre = genre_(item)
      end if
      select case (itemgenre)
      case ("r"); call write_(self,item%r,"r",by_column,ascii)
      case ("u"); call write_(self,item%a,"a",by_column,ascii)
         call write_(self,item%b,"b",by_column,ascii)
      case ("g"); call write_(self,item%g,"g",by_column,ascii)
      case default; allocate(tonto%known_keywords(3))
      tonto%known_keywords(1) = "r"
      tonto%known_keywords(2) = "u"
      tonto%known_keywords(3) = "g"
      call unknown_(tonto,itemgenre,"ARCHIVE:write")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine write_19(self,item,genre,by_column,ascii)
   ! Write to the archive operator vector "item". "by_column"
   ! indicates the output order for ascii files.
      type(ARCHIVE_TYPE), INOUT :: self
      type(OPMATRIX_TYPE), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: by_column
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: itemgenre
      if (present(genre)) then; itemgenre = genre
      else; itemgenre = genre_(item)
      end if
      select case (itemgenre)

      case ("r              ","restricted     ")
         call write_(self,item%r,"r ",by_column,ascii)

      case ("u              ","unrestriected  ")
         call write_(self,item%a,"a ",by_column,ascii)
         call write_(self,item%b,"b ",by_column,ascii)

      case ("gc             ","general_complex")
         call write_(self,item%gc,"gc",by_column,ascii)

      case default
         DIE("ARCHIVE:write_19 ... unknown genre, "//trim(itemgenre))
      end select
   end subroutine

!  ====================================
!  Parallel whole-object write routines
!  ====================================

   subroutine per_rank_write_0(self,item,genre,ascii)
   ! Allow parallel write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(INT,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_real_fortran_fmt_(self%Afile,"e")
         call parallel_put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call per_rank_write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine per_rank_write_1(self,item,genre,ascii)
   ! Allow parallel write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_real_fortran_fmt_(self%Afile,"e")
         call parallel_put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call per_rank_write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine per_rank_write_2(self,item,genre,ascii)
   ! Allow parallel write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_real_fortran_fmt_(self%Afile,"e")
         call parallel_put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call per_rank_write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine per_rank_write_3(self,item,genre,ascii)
   ! Allow parallel write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_real_fortran_fmt_(self%Afile,"e")
         call parallel_put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call per_rank_write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine per_rank_write_4(self,item,genre,ascii)
   ! Allow parallel write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_real_fortran_fmt_(self%Afile,"e")
         call parallel_put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call per_rank_write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine

   subroutine per_rank_write_5(self,item,genre,ascii)
   ! Allow parallel write to the archive, a single "item".
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(CPX,:,:), IN :: item
      STR(len=*), optional, IN :: genre
      BIN, optional, IN :: ascii
      STR(len=STR_SIZE) :: file_name
      BIN :: text

      ! The following code is inherited from ARCHIVE
      text = FALSE
      if (present(ascii)) text = ascii
      file_name = file_name_(self,genre)
      if (text OR is_ascii_(self)) then
         call create_(self%Afile,trim(file_name))
         call open_for_(self%Afile,"write")
         call set_real_fortran_fmt_(self%Afile,"e")
         call parallel_put_(self%Afile,item)
         call close_(self%Afile)
         call destroy_(self%Afile)
      else
         call create_(self%Bfile,trim(file_name))
         call open_for_(self%Bfile,"write-only")
         call per_rank_write_(self%Bfile,item)
         call close_(self%Bfile)
         call destroy_(self%Bfile)
      end if
   end subroutine


!  ===============================
!  Program-specific write routines
!  ===============================

!   write_ppm(matrix,max_colours)
!   ! Writes a ppm file.  Requires a 2D matrix of data
!   ! values (width of image specified first then height)
!   ! max_colours is the range
!      self :: INOUT
!      matrix :: MAT{REAL}, OUT
!      max_colours :: INT, optional, IN
!
!      name :: STR
!      cf :: COLOUR_FUNCTION@
!      colours :: MAT3{INT}@
!      n_div :: INT
!      minv,maxv :: REAL
!
!      n_div = 256
!      if (present(max_colours)) n_div = max_colours
!
!      name = .file_name.trim // ".ppm"
!      .Afile.create(trim(name))
!      .Afile.open_for("write")
!
!      cf.create
!      colours.create(3,matrix.dim1,matrix.dim2)
!
!      minv = minval(matrix)
!      maxv = maxval(matrix)
!      cf.rescale_data((/minv,maxv/))
!      minv = 0
!      maxv = n_div
!      cf.rescale_RGB((/minv,maxv/))
!      cf.get_RGB_for(matrix,colours)
!
!      .Afile.put_ppm(colours)
!
!      colours.destroy
!      cf.destroy
!
!      .Afile.close
!      .Afile.destroy
!
!   end

   subroutine write_orbital_0(self,gu,gd,n_pt)
   ! Write the up and down component of an orbital with the number of
   ! "n_pt" grid points,
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:) :: gu,gd
      INT :: n_pt
      INT :: n
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)
      do n = 1,n_pt
         call put_(self%Afile,gu(n))
         call put_(self%Afile,gd(n))
         call flush_(self%Afile)
      end do
      call close_(self)
   end subroutine

   subroutine write_orbital_1(self,gcu,gcd,n_pt)
   ! Write the up and down component of an orbital with the number of
   ! "n_pt" grid points,
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(CPX,:), IN :: gcu,gcd
      INT, IN :: n_pt
      INT :: n
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)
      do n = 1,n_pt
         call put_(self%Afile,gcu(n))
         call put_(self%Afile,gcd(n))
         call flush_(self%Afile)
      end do
      call close_(self)
   end subroutine

   subroutine write_gnuplot_0(self,density,n_x,n_y,n_z)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in gnuplot format (suitable for an splot).
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      INT, IN :: n_x,n_y,n_z
      INT :: n,x,y,z
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)
      n = 1
      do z = 1,n_z
         if (n_z>1) call show_(self%Afile," z =",z)
         do y = 1,n_y
            do x = 1,n_x
               call put_(self%Afile,density(n))
               call flush_(self%Afile)
               n = n+1
            end do
            call flush_(self%Afile)
         end do
      end do
      call close_(self)
   end subroutine

   subroutine write_gnuplot_contour(self,density,grid,pos,bonds)
   ! Write a gnuplot command file to display contours for a "density"
   ! vector corresponding to a "grid" and with atom positions "pos"
   ! already rotated into the grid frame, and with "bond" describing
   ! the atom connectivity.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      MAT(REAL,:,:), IN :: pos
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds
      VEC(REAL,3) :: L
      STR(len=STR_SIZE) :: cont_data,cont_tabl
      STR(len=STR_SIZE) :: bond_data,bond_tabl
      STR(len=STR_SIZE) :: eps_file,val
      STR(len=1) :: op,cl
      BIN :: logplot,neg_logplot,pos_logplot,is_2d
      INT :: ny
   ENSURE(size(density)==grid%n_pt,"ARCHIVE:write_gnuplot_contour ... inconsistent density array and grid size")

      ! Type of plot?
      logplot = FALSE
      pos_logplot = FALSE
      neg_logplot = FALSE

      ! Is it a 2D plot
      ny = grid%n_y/2
      is_2d = ny>0
      val = grid%contour_scale
      select case (val)
      case ("linear      ")
      case ("log         "); logplot = TRUE
      case ("negative-log"); logplot = TRUE; neg_logplot = TRUE
      case ("positive-log"); logplot = TRUE; pos_logplot = TRUE
      case default; allocate(tonto%known_keywords(4))
      tonto%known_keywords(1) = "linear      "
      tonto%known_keywords(2) = "log         "
      tonto%known_keywords(3) = "negative-log"
      tonto%known_keywords(4) = "positive-log"
      call unknown_(tonto,val,"ARCHIVE:write_gnuplot_contour")
      deallocate(tonto%known_keywords)
      end select

      ! Constants
      op = achar(91)
      cl = achar(93)
      call set_format_(self,"gnuplot.contour_data")
      cont_data = file_name_(self)

      ! Save bond data file names
      call set_format_(self,"gnuplot.bond_table")
      bond_tabl = file_name_(self)
      call set_format_(self,"gnuplot.bond_data")
      bond_data = file_name_(self)

      ! Write the bond data
      call write_gnuplot_bonds_(self,pos,bonds)

      ! Save contour data file names
      call set_format_(self,"gnuplot.contour_table")
      cont_tabl = file_name_(self)
      if (is_2d) then
         call set_format_(self,"gnuplot.contour_data")
      else
         call set_format_(self,"gnuplot.contour_data_1d")
      end if
      cont_data = file_name_(self)

      ! Write the contour data
      call write_gnuplot_2d_and_1d_(self,density,grid)

      ! Get the .png file name
      if (is_2d) then
         call set_format_(self,"gnuplot.eps")
      else
         call set_format_(self,"gnuplot_1d.eps")
      end if
      eps_file = file_name_(self)

      ! Open & write gnuplot command file
      call set_format_(self,"gnuplot.commands")
      call open_for_(self,"writing",ascii=TRUE)

      ! Write gnuplot command file
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,16)
      call set_real_precision_(self%Afile,8)

      ! Header for user ...
      call text_(self%Afile,"# ======================")
      call text_(self%Afile,"# Gnuplot 4.6 input file")
      call text_(self%Afile,"# ======================")
      call text_(self%Afile,"#")
      call text_(self%Afile,"# 1) To see this plot install gnuplot 4.6 and type:")
      call text_(self%Afile,"#")
      call text_(self%Afile,"#      gnuplot -persist <file>")
      call text_(self%Afile,"#")
      call text_(self%Afile,"# 2) You must have gnuplot 4.6. To install see here:")
      call text_(self%Afile,"#")
      call text_(self%Afile,"#      http://www.miscdebris.net/blog/2010/03/03/install-gnuplot-4-4-0-rc1-on-ubuntu-linux/")
      call text_(self%Afile,"#")
      call text_(self%Afile,"#    - This works even though it is for gnuplot 4.4")
      call text_(self%Afile,"#    - Install newer libraries than those listed above")
      call text_(self%Afile,"#    - Make sure you have the x11 terminal")
      call text_(self%Afile,"#")
      call text_(self%Afile,"# 3) You can alter the lines below to get the plot you want.")
      call text_(self%Afile,"#    - Look for the parts marked CHANGE")
      call text_(self%Afile,"#    - Change the contour levels for a start")
      call text_(self%Afile,"#    - Use a linear or log scale (change format= command in stdin)")
      call text_(self%Afile,"#    - Use different colors!")

      ! Check
      call flush_(self%Afile)
      call text_(self%Afile,"# Check version")
      call text_(self%Afile,"if (GPVAL_VERSION<4.6) print 'You must use gnuplot 4.6'; exit")

      ! Use macros
      call flush_(self%Afile)
      call text_(self%Afile,"# Turn on macros")
      call text_(self%Afile,"set macros")

      ! Set terminal
      call flush_(self%Afile)
      call text_(self%Afile,"# Set terminal - enhanced for sub/superscripts")
      call text_(self%Afile,"set term xterm")

      ! Set contour levels
      call flush_(self%Afile)
      if (logplot) then
         call text_(self%Afile,"# Set the *log10* of +ve contour levels incrementally. (CHANGE)")
         call text_(self%Afile,"# Negative contours are set the opposite of +ve ones")
         call text_(self%Afile,"# . MIN_VALUE - log10 of minimum +ve contour value")
         call text_(self%Afile,"# . MAX_VALUE - log10 of maximum +ve contour value")
      else
         call text_(self%Afile,"# Set the contour levels incrementally. (CHANGE)")
         call text_(self%Afile,"# . MIN_VALUE - minimum contour value")
         call text_(self%Afile,"# . MAX_VALUE - maximum contour value, -ve of MIN_VALUE")
      end if
      call text_(self%Afile,"# . INCREMENT - value to increment MIN_VALUE by")
      call text_(self%Afile,"MIN_VALUE = "//trim(to_str_(grid%contour_min_value,"f8.4")))
      call text_(self%Afile,"INCREMENT = "//trim(to_str_(grid%contour_increment,"f8.4")))
      call text_(self%Afile,"MAX_VALUE = "//trim(to_str_(grid%contour_max_value,"f8.4")))

      ! Sanity check
      call flush_(self%Afile)
      call text_(self%Afile,"# Sanity check ...")
      call text_(self%Afile,"nint(x) = floor(x+0.5)")
      call text_(self%Afile,"deln(x) = abs(x-nint(x))")
      call text_(self%Afile,"delt(x,y) = abs(abs(x)-abs(y))")
      call text_(self%Afile,"if (MIN_VALUE > MAX_VALUE)     print 'MIN_VALUE is too large'; exit")
      if (NOT logplot) then
         call text_(self%Afile,"if (delt(MIN_VALUE,MAX_VALUE)>0.0001) print 'MIN is not -MAX value!'; exit")
      end if
      call text_(self%Afile,"if (INCREMENT<= 0)             print 'INCREMENT must be +ve!'; exit")
      call text_(self%Afile,"RANGE = MAX_VALUE - MIN_VALUE")
      if (logplot) then
         call text_(self%Afile,"n = RANGE/INCREMENT + 1")
         call text_(self%Afile,"if (deln(n)>0.0001)            print 'INCREMENT must divide RANGE'; exit")
      else
         call text_(self%Afile,"n = RANGE/INCREMENT")
         call text_(self%Afile,"if (deln(n)>0.0001)            print 'INCREMENT must divide RANGE'; exit")
         call text_(self%Afile,"n = n/2")
         call text_(self%Afile,"if (deln(n)>0.0001)            print 'No. of contours must be odd'; exit")
      end if
      call text_(self%Afile,"n = nint(n)")
      call text_(self%Afile,"MID_VALUE = MIN_VALUE + RANGE/2")
      if (is_2d) then  ! -- 2D specific -1

      ! Set length scale (ruler) markings in au
         call flush_(self%Afile)
         call text_(self%Afile,"# Ruler length (CHANGE)")
         call text_(self%Afile,"RULER_LENGTH = 1")

      ! Set contour line colors
         call flush_(self%Afile)
         call text_(self%Afile,"# Set MIN, MID and MAX value contour line colors (CHANGE)")
         call text_(self%Afile,"MIN_COLOR_R = "//trim(to_str_(grid%contour_min_color%RGB255(1))))
         call text_(self%Afile,"MIN_COLOR_G = "//trim(to_str_(grid%contour_min_color%RGB255(2))))
         call text_(self%Afile,"MIN_COLOR_B = "//trim(to_str_(grid%contour_min_color%RGB255(3))))
         call flush_(self%Afile)
         call text_(self%Afile,"MID_COLOR_R = "//trim(to_str_(grid%contour_mid_color%RGB255(1))))
         call text_(self%Afile,"MID_COLOR_G = "//trim(to_str_(grid%contour_mid_color%RGB255(2))))
         call text_(self%Afile,"MID_COLOR_B = "//trim(to_str_(grid%contour_mid_color%RGB255(3))))
         call flush_(self%Afile)
         call text_(self%Afile,"MAX_COLOR_R = "//trim(to_str_(grid%contour_max_color%RGB255(1))))
         call text_(self%Afile,"MAX_COLOR_G = "//trim(to_str_(grid%contour_max_color%RGB255(2))))
         call text_(self%Afile,"MAX_COLOR_B = "//trim(to_str_(grid%contour_max_color%RGB255(3))))

      ! Set fade factor
         val = to_str_(grid%contour_fade_factor,"f4.2")
         call flush_(self%Afile)
         call text_(self%Afile,"# Apply this factor to get pastel palette colors (CHANGE)")
         call text_(self%Afile,"FADE_FACTOR = "//trim(val))
      end if  ! -- 2D specific part -1
      call flush_(self%Afile)
      call text_(self%Afile,"# ==== Report and do plot ====")

      ! Report range and no. of contours
      call flush_(self%Afile)
      if (logplot) then
         call text_(self%Afile,"# Range and no. of contours")
         call text_(self%Afile,"print 'Minimum +ve log10 contour = ',MIN_VALUE")
         call text_(self%Afile,"print 'Maximum +ve log10 contour = ',MAX_VALUE")
         call text_(self%Afile,"print 'Contour increment         = ',INCREMENT")
         call text_(self%Afile,"print 'No. of contours           = ',n")
         if (NOT pos_logplot AND NOT neg_logplot) call text_(self%Afile,"print 'Total no. of contours     = ',2*n+1")
      else
         call text_(self%Afile,"# Range and no. of contours")
         call text_(self%Afile,"print 'Minimum contour value = ',MIN_VALUE")
         call text_(self%Afile,"print 'Maximum contour value = ',MAX_VALUE")
         call text_(self%Afile,"print 'Contour increment     = ',INCREMENT")
         call text_(self%Afile,"print 'No. of +ve contours   = ',n")
         call text_(self%Afile,"print 'Total no. of contours = ',2*n+1")
      end if

      ! Map: makes -ve log contours, then 0, then +ve log contours
      call flush_(self%Afile)
      if (logplot) then
         call text_(self%Afile,"# map: makes -ve log contours, then 0, then +ve log contours")
         call text_(self%Afile,"#      it goes from -n .. -1 in integer steps,")
         call text_(self%Afile,"#      then    from +1 .. +n in integer steps,")
         call text_(self%Afile,"#      for every 10**(INCREMENT) factor")
         call text_(self%Afile,"#      The abs( ) makes the transition smoother")
         call text_(self%Afile,"EPS = 10**(MIN_VALUE)")
         call text_(self%Afile,"FAC = 10**(-MIN_VALUE)")
         call text_(self%Afile,"map(x) = abs(x)<EPS ? FAC*x : &
                  &(x<0 ? (-log10(-x)+MIN_VALUE)/INCREMENT-1:(+log10(x)-MIN_VALUE)/INCREMENT+1)")
      else
         call text_(self%Afile,"# map: does nothing for linear plots")
         call text_(self%Afile,"map(x) = x")
      end if

      ! The max and min of the map function
      if (logplot) then

      ! log plot
         call flush_(self%Afile)
         call text_(self%Afile,"# The max and min of the map function")
         if (pos_logplot) then
            call text_(self%Afile,"MIN_MAP = 1")
            call text_(self%Afile,"MAX_MAP = n")
         else if (neg_logplot) then
            call text_(self%Afile,"MIN_MAP = -n")
            call text_(self%Afile,"MAX_MAP = -1")
         else if (logplot) then
            call text_(self%Afile,"MIN_MAP = -n")
            call text_(self%Afile,"MAX_MAP = +n")
            call text_(self%Afile,"MID_MAP = 0")
         end if
      else

      ! linear plot
         call text_(self%Afile,"MIN_MAP = MIN_VALUE")
         call text_(self%Afile,"MID_MAP = MID_VALUE")
         call text_(self%Afile,"MAX_MAP = MAX_VALUE")
      end if
      if (is_2d) then  ! -- 2D specific -2

      ! Set data files
         call flush_(self%Afile)
         call text_(self%Afile,"# Set data files")
         call text_(self%Afile,"CONTOUR_DATA  = '"//trim(cont_data)//"'")
         call text_(self%Afile,"EPS_FILE      = '"//trim(eps_file)//"'")
         call text_(self%Afile,"BOND_DATA     = '"//trim(bond_data)//"'")
         call text_(self%Afile,"CONTOUR_TAB_M = '"//trim(cont_tabl)//"_-'")
         call text_(self%Afile,"CONTOUR_TAB_0 = '"//trim(cont_tabl)//"_0'")
         call text_(self%Afile,"CONTOUR_TAB_P = '"//trim(cont_tabl)//"_+'")
         call text_(self%Afile,"BOND_TABLE    = '"//trim(bond_tabl)//"'")

      ! Make the hex color strings
         call flush_(self%Afile)
         call text_(self%Afile,"# Make hex colors")
         call text_(self%Afile,"MIN_COLOR = sprintf('#%02x%02x%02x',MIN_COLOR_R,MIN_COLOR_G,MIN_COLOR_B)")
         call text_(self%Afile,"MID_COLOR = sprintf('#%02x%02x%02x',MID_COLOR_R,MID_COLOR_G,MID_COLOR_B)")
         call text_(self%Afile,"MAX_COLOR = sprintf('#%02x%02x%02x',MAX_COLOR_R,MAX_COLOR_G,MAX_COLOR_B)")
         call flush_(self%Afile)
         call text_(self%Afile,"print 'Min-contour color     = ',MIN_COLOR")
         call text_(self%Afile,"print 'Mid-contour color     = ',MID_COLOR")
         call text_(self%Afile,"print 'Max-contour color     = ',MAX_COLOR")

      ! Set faded palette colors
         call flush_(self%Afile)
         call text_(self%Afile,"# Set faded palette colors")
         call text_(self%Afile,"fade(x) = FADE_FACTOR*x + (1-FADE_FACTOR)*255")
         call flush_(self%Afile)
         call text_(self%Afile,"MIN_PCOLOR_R = fade(MIN_COLOR_R)")
         call text_(self%Afile,"MIN_PCOLOR_G = fade(MIN_COLOR_G)")
         call text_(self%Afile,"MIN_PCOLOR_B = fade(MIN_COLOR_B)")
         call flush_(self%Afile)
         call text_(self%Afile,"MAX_PCOLOR_R = fade(MAX_COLOR_R)")
         call text_(self%Afile,"MAX_PCOLOR_G = fade(MAX_COLOR_G)")
         call text_(self%Afile,"MAX_PCOLOR_B = fade(MAX_COLOR_B)")
         call flush_(self%Afile)
         call text_(self%Afile,"MIN_PCOLOR = sprintf('#%02x%02x%02x',MIN_PCOLOR_R,MIN_PCOLOR_G,MIN_PCOLOR_B)")
         call text_(self%Afile,"MAX_PCOLOR = sprintf('#%02x%02x%02x',MAX_PCOLOR_R,MAX_PCOLOR_G,MAX_PCOLOR_B)")

      ! Define palette
         call flush_(self%Afile)
         call text_(self%Afile,"# Set background color palette")
         if (pos_logplot OR neg_logplot) then
            call text_(self%Afile,"set palette defined ( MIN_MAP MIN_PCOLOR, MAX_MAP MAX_PCOLOR) ")
         else if (logplot) then
            call text_(self%Afile,"set palette defined ( MIN_MAP MIN_PCOLOR, -1 MID_COLOR, 1 MID_COLOR, MAX_MAP MAX_PCOLOR) ")
         else
            call text_(self%Afile,"set palette defined ( MIN_MAP MIN_PCOLOR, MID_VALUE MID_COLOR, MAX_MAP MAX_PCOLOR) ")
         end if

      ! Set grid width, aspect ratio
         call flush_(self%Afile)
         call text_(self%Afile,"# Plot side lengths ...")
         L = HALF*grid%width
         call flush_(self%Afile)
         call text_(self%Afile,"# Set the correct aspect ratio")
         call text_(self%Afile,"Lx = "//trim(to_str_(L(1))))
         call text_(self%Afile,"Ly = "//trim(to_str_(L(2))))
         call text_(self%Afile,"set xrange "//op//"-Lx:Lx"//cl)
         call text_(self%Afile,"set yrange "//op//"-Lx:Lx"//cl)
         call text_(self%Afile,"set size square")
         call text_(self%Afile,"set view map")

      ! Set no key, no ytics
         call flush_(self%Afile)
         call text_(self%Afile,"# Set no key and ytics")
         call text_(self%Afile,"unset key")
         call text_(self%Afile,"unset ytics")

      ! Set distance ruler on bottom
         call flush_(self%Afile)
         call text_(self%Afile,"# Set xtics as ruler scale?")
         if (grid%use_ruler_scale) then
            val = to_str_(grid%contour_font_size)
            call text_(self%Afile,"set xtics out nomirror 0,RULER_LENGTH,RULER_LENGTH font ',"//trim(val)//"'")
         else
            call text_(self%Afile,"unset xtics")
         end if

      ! Macro for color background
         call flush_(self%Afile)
         call text_(self%Afile,"# Macro for plotting COLOR_BACKGROUND")
         call text_(self%Afile,"COLOR_BACKGROUND = 'CONTOUR_DATA using 1:2:(map($3)) with image'")
         call text_(self%Afile,"# Set contour style, contour line style")
         call text_(self%Afile,"set contour base")
         call text_(self%Afile,"unset surface")
         val = to_str_(grid%contour_line_width,"f4.2")
         call text_(self%Afile,"set style line 1 lw "//trim(val)//" lc rgb MIN_COLOR")
         call text_(self%Afile,"set style line 2 lw "//trim(val)//" lt 0")
         call text_(self%Afile,"set style line 3 lw "//trim(val)//" lc rgb MAX_COLOR")

      ! Plot contours
         if (logplot) then  ! for log plots ...
            if (NOT pos_logplot) then
               call flush_(self%Afile)
               call text_(self%Afile,"# Plot contours:-ve")
               call text_(self%Afile,"set cntrparam level incremental MIN_MAP, 1, -1")
               call text_(self%Afile,"set table CONTOUR_TAB_M")
               call text_(self%Afile,"splot     CONTOUR_DATA  using 1:2:(map($3))")
               call text_(self%Afile,"unset table")
            end if
            call flush_(self%Afile)
            call text_(self%Afile,"# Plot contours: 0")
            call text_(self%Afile,"set cntrparam level incremental 0, 1, 0")
            call text_(self%Afile,"set table CONTOUR_TAB_0")
            call text_(self%Afile,"splot     CONTOUR_DATA  using 1:2:(map($3))")
            call text_(self%Afile,"unset table")
            if (NOT neg_logplot) then
               call flush_(self%Afile)
               call text_(self%Afile,"# Plot contours: +ve")
               call text_(self%Afile,"set cntrparam level incremental 1, 1, MAX_MAP")
               call text_(self%Afile,"set table CONTOUR_TAB_P")
               call text_(self%Afile,"splot     CONTOUR_DATA  using 1:2:(map($3))")
               call text_(self%Afile,"unset table")
            end if
            call flush_(self%Afile)
            call text_(self%Afile,"# Macro for plotting all contours")
            if (neg_logplot) then
               call text_(self%Afile,"CONTOURS = 'CONTOUR_TAB_M with lines ls 1, &
                              &CONTOUR_TAB_0 with lines ls 2'")
            else if (pos_logplot) then
               call text_(self%Afile,"CONTOURS = 'CONTOUR_TAB_0 with lines ls 2, &
                              &CONTOUR_TAB_P with lines ls 3'")
            else
               call text_(self%Afile,"CONTOURS = 'CONTOUR_TAB_M with lines ls 1, &
                              &CONTOUR_TAB_0 with lines ls 2, &
                              &CONTOUR_TAB_P with lines ls 3'")
            end if
         else  ! for linear plot
            call flush_(self%Afile)
            call text_(self%Afile,"# Plot contours: below mid")
            call text_(self%Afile,"set cntrparam level incremental MIN_VALUE, INCREMENT, MID_VALUE-0.5*INCREMENT")
            call text_(self%Afile,"set table CONTOUR_TAB_M")
            call text_(self%Afile,"splot     CONTOUR_DATA  using 1:2:(map($3))")
            call text_(self%Afile,"unset table")
            call flush_(self%Afile)
            call text_(self%Afile,"# Plot contours: mid")
            call text_(self%Afile,"set cntrparam level incremental MID_VALUE-0.5*INCREMENT, INCREMENT, MID_VALUE+0.5*INCREMENT")
            call text_(self%Afile,"set table CONTOUR_TAB_0")
            call text_(self%Afile,"splot     CONTOUR_DATA  using 1:2:(map($3))")
            call text_(self%Afile,"unset table")
            call flush_(self%Afile)
            call text_(self%Afile,"# Plot contours: mid")
            call text_(self%Afile,"set cntrparam level incremental MID_VALUE+0.5*INCREMENT, INCREMENT, MAX_VALUE")
            call text_(self%Afile,"set table CONTOUR_TAB_P")
            call text_(self%Afile,"splot     CONTOUR_DATA  using 1:2:(map($3))")
            call text_(self%Afile,"unset table")
            call flush_(self%Afile)
            call text_(self%Afile,"# Macro for plotting all contours")
            call text_(self%Afile,"CONTOURS = 'CONTOUR_TAB_M with lines ls 1, &
                              &CONTOUR_TAB_0 with lines ls 2, &
                              &CONTOUR_TAB_P with lines ls 3'")
         end if  ! for log plots ...

      ! Make table of atom-bond data
         call flush_(self%Afile)
         call text_(self%Afile,"# This is the table of atom-bond data ...")
         call text_(self%Afile,"set table BOND_TABLE")
         call text_(self%Afile,"plot      BOND_DATA")
         call text_(self%Afile,"unset table")

      ! Macro for atom bonds
         call flush_(self%Afile)
         call text_(self%Afile,"# Macro for plotting ATOM_BONDS")
         call text_(self%Afile,"ATOM_BONDS = 'BOND_TABLE with lines lt -1 lw 1.25'")
      else  ! -- 1D specific -2

      ! Set data files
         call flush_(self%Afile)
         call text_(self%Afile,"# Set data files")
         call text_(self%Afile,"CONTOUR_DATA  = '"//trim(cont_data)//"'")
         call text_(self%Afile,"EPS_FILE      = '"//trim(eps_file)//"'")
      end if  ! -- 2D specific -2

      ! Macro for cbtics
      if (logplot) then  ! --logplot
         val = to_str_(grid%contour_tic_dps)
         call flush_(self%Afile)
         call text_(self%Afile,"# Make CBTICS")
         call text_(self%Afile,"str(val) = gprintf('%."//trim(val)//"f',val)")
         call text_(self%Afile,"add_tic(sign,pow,lev,comma) = '"//'"'//"'.sign.'10^{'.str(pow).'}"//'"'//" '.str(lev).comma")
         call text_(self%Afile,"CBTICS = ''")

      ! -ve log contours
         if (NOT pos_logplot) then
            call text_(self%Afile,"pow = MAX_VALUE")
            call text_(self%Afile,"do for "//op//"i=-n:-1"//cl//" {    # -ve  cbtic")
            call text_(self%Afile,"   CBTICS= CBTICS.add_tic('-',pow,i,', ')")
            call text_(self%Afile,"   pow = pow - INCREMENT")
            call text_(self%Afile,"}")
         end if
         call text_(self%Afile,"   CBTICS= CBTICS.'"//'"0"'//" 0, ' # zero cbtic")
      ! +ve log contours
         if (NOT neg_logplot) then
            call text_(self%Afile,"pow = MIN_VALUE")
            call text_(self%Afile,"do for "//op//"i=1:n-1"//cl//" {    # 0ve  cbtic")
            call text_(self%Afile,"   CBTICS= CBTICS.add_tic('',pow,i,', ')")
            call text_(self%Afile,"   pow = pow + INCREMENT")
            call text_(self%Afile,"}")
         end if
         call text_(self%Afile,"   CBTICS= CBTICS.add_tic('',pow,n,'')")
      end if  ! -- logplot

      ! Set cbtics and cbrange
      if (is_2d) then  ! 2D specific -3
         call flush_(self%Afile)
         call text_(self%Afile,"# Set cbtics and cbrange")
         if (grid%use_colorbox_scale) then  ! use_colorbox_scale
            val = to_str_(grid%contour_font_size)
            if (logplot) then  ! -- logplot
               call text_(self%Afile,"print 'CBTICS = ',CBTICS")
               call text_(self%Afile,"set cbtics in nomirror ( @CBTICS ) font ',"//trim(val)//"'")
               call text_(self%Afile,"set cbrange "//op//"MIN_MAP:MAX_MAP"//cl)
            else
               call text_(self%Afile,"set cbtics in nomirror MIN_VALUE,INCREMENT,MAX_VALUE font ',"//trim(val)//"'")
               call text_(self%Afile,"set cbrange "//op//"MIN_VALUE:MAX_VALUE"//cl)
            end if  ! -- logplot
         else  ! --use_colorbox_scale
            call text_(self%Afile,"unset cbtics")
         end if  ! --use_colorbox_scale
      else  ! 1D specific -3
         if (logplot) then  ! logplot
            call flush_(self%Afile)
            call text_(self%Afile,"# Set ytics")
            call text_(self%Afile,"unset key")
            call text_(self%Afile,"unset xtics")
            call text_(self%Afile,"unset ytics")
            call text_(self%Afile,"set y2tics ( @CBTICS ) font ',10'")
            call text_(self%Afile,"set yrange [MIN_MAP:MAX_MAP]")
         else  ! linear
            DIE("ARCHIVE:write_gnuplot_contour ... Haven't done this yet")
         end if  ! logplot
      end if  ! 2D specific -3
      if (is_2d) then  ! 2D specific -4

      ! Finally do the plot
         call flush_(self%Afile)
         call text_(self%Afile,"# Finally do the plot ...")
         call text_(self%Afile,"plot "//op//"-Lx:Lx"//cl//" "//op//"-Ly:Ly"//cl//" @COLOR_BACKGROUND, @CONTOURS, @ATOM_BONDS")
      else  ! 1D specific -4

      ! Finally do the plot
         call flush_(self%Afile)
         call text_(self%Afile,"# Finally do the plot ...")
         call text_(self%Afile,"plot CONTOUR_DATA using 1:(map($2)) with lines lt -1 lw 1.25")
      end if  ! 2D specific -4

      ! Make png file
      call flush_(self%Afile)
      call text_(self%Afile,"# Make eps")
      call text_(self%Afile,"set term postscript eps color enhanced solid clip")
      call text_(self%Afile,"set output EPS_FILE")
   ! .Afile.text("set term png enhanced")
   ! .Afile.text("set output PNG_FILE")
      call text_(self%Afile,"replot")

      ! Close
      call close_(self)
   end subroutine

   subroutine write_gnuplot_2d_and_1d(self,density,grid)
   ! Write a 2D "density" vector (or 1D) corresponding to a plot grid.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      INT :: nx,ny,i,ix,iy
      REAL :: x,y,sx,sy

      ! Open the ASCII file
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)

      ! Grid points halved
      nx = grid%n_x/2
      ny = grid%n_y/2
      if (ny>0) then
         call text_(self%Afile,"# gnuplot: raw 2D grid for contouring ...")

         ! Grid points & width scale factor
         sx = (ONE/REALIFY(nx))*grid%width(1)*HALF
         sy = (ONE/REALIFY(ny))*grid%width(2)*HALF

         ! Write out the contour grid
         i = 1
         do iy = -ny,ny
            y = sy*iy
            do ix = -nx,nx
               x = sx*ix
               call put_(self%Afile,x)
               call put_(self%Afile,y)
               call put_(self%Afile,density(i))
               call flush_(self%Afile)
               i = i+1
            end do
            call flush_(self%Afile)
         end do

      ! This is actually a 1D plot
      else
         call text_(self%Afile,"# gnuplot: raw 1D grid for contouring ...")

         ! Grid points & width scale factor
         sx = (ONE/REALIFY(nx))*grid%width(1)*HALF

         ! Write out the contour grid
         i = 1
         do ix = -nx,nx
            x = sx*ix
            call put_(self%Afile,x)
            call put_(self%Afile,density(i))
            call flush_(self%Afile)
            i = i+1
         end do
         call flush_(self%Afile)
      end if

      ! Close
      call close_(self)
   end subroutine

   subroutine write_gnuplot_bonds(self,pos,bonds)
   ! Write gnuplot code to draw the atom bonds.  The atom positions
   ! "pos" must have been already projected into the plot plane, and
   ! "bonds" is a list of the atom indices bonded to a given atom.
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: pos
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds
      INT :: a,na,i,b
   ENSURE(size(pos,1)==3,"ARCHIVE:write_gnuplot_bonds ... wrong dim1, pos")
   ENSURE(size(pos,2)==size(bonds),"ARCHIVE:write_gnuplot_bonds ... inconsistent pos and bonds")

      ! Open the ASCII file
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)

      ! Grid data
      call text_(self%Afile,"# gnuplot: atom bonds projected onto plot plane ...")

      ! Loop over atoms a
      do a = 1,size(bonds)

         ! Any bonds?
         if (size(bonds(a)%element)==0) cycle

         ! No of atoms bonded to a
         na = size(bonds(a)%element)

         ! Loop over the bonded atoms
         do i = 1,na

            ! Index of bonded atom
            b = bonds(a)%element(i)

            ! Draw bond only one way
            if (b<=a) cycle

            ! Put the line segment
            call put_(self%Afile,pos(1,a))
            call put_(self%Afile,pos(2,a))
            call flush_(self%Afile)
            call put_(self%Afile,pos(1,b))
            call put_(self%Afile,pos(2,b))
            call flush_(self%Afile)
            call flush_(self%Afile)
         end do
      end do

      ! Close
      call close_(self)
   end subroutine

   subroutine write_mathematica_0(self,density,n_x,n_y,n_z)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in Mathematica format
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      INT, IN :: n_x,n_y,n_z
      INT :: n,x,y,z,l
      REAL :: d
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)
      call text_(self%Afile,"{")
      n = 1
      do z = 1,n_z
         do y = 1,n_y
            call text_(self%Afile,"{")
            do x = 1,n_x
               d = density(n)
               if (d==ZERO) then; l = 0
               else; l = floor(log10(abs(d)))
               end if
               d = d*TEN**(-l)
               if (x==n_x AND y==n_y AND z==n_z) then
                  call text_(self%Afile,trim(to_str_(d))//" * 10^"//trim(to_str_(l))//"}")
               else if (x==n_x) then
                  call text_(self%Afile,trim(to_str_(d))//" * 10^"//trim(to_str_(l))//"},")
               else
                  call text_(self%Afile,trim(to_str_(d))//" * 10^"//trim(to_str_(l))//",")
               end if
               n = n+1
            end do
            call flush_(self%Afile)
         end do
      end do
      call text_(self%Afile,"}")
      call close_(self)
   end subroutine

   subroutine write_mathematica_1(self,density,grid,atom,math_pos,bonds)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y" in Mathematica format. You must supplu the atom list and
   ! the mathematica positions "math_pos" in rotated grid coordinates.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      MAT(REAL,:,:), IN :: math_pos
      VEC(type(EVEC_INT_TYPE),:), IN :: bonds
      BIN :: has_bonds
      INT :: n,x,y,l,a,b,c,nb
      REAL :: d,ax,ay,bx,by,minim,maxim
      STR(len=STR_SIZE) :: at_label,line,hue
      STR(len=1) :: ob,cb
      VEC(REAL,2) :: ba
   ENSURE(size(atom)==size(math_pos,1),"ARCHIVE:write_mathematica_1 ... atom and math_pos incompatible")
   ENSURE(size(math_pos,2)==3,"ARCHIVE:write_mathematica_1 ... wrong dim2, math_pos")
   ENSURE(size(bonds)==size(atom),"ARCHIVE:write_mathematica_1 ... bonds and atom incompatible")

      ! Fix the square brackets
      ob = achar(91)
      cb = achar(93)

      ! Open the file and set the output style
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)

      ! Dump the Mathematica contour data
      call text_(self%Afile,"<<Graphics`Legend`")
      call text_(self%Afile,"ShowLegend"//ob)
      call text_(self%Afile,"Show"//ob)
      call text_(self%Afile,"ContourGraphics"//ob)
      call text_(self%Afile,"{")
      n = 1
      do y = 1,grid%n_y
         call text_(self%Afile,"{")
         do x = 1,grid%n_x
            d = density(n)
            if (d==ZERO) then; l = 0
            else; l = floor(log10(abs(d)))
            end if
            d = d*TEN**(-l)
            if (x==grid%n_x AND y==grid%n_y) then
               call text_(self%Afile,trim(to_str_(d))//" * 10^"//trim(to_str_(l))//"}")
            else if (x==grid%n_x) then
               call text_(self%Afile,trim(to_str_(d))//" * 10^"//trim(to_str_(l))//"},")
            else
               call text_(self%Afile,trim(to_str_(d))//" * 10^"//trim(to_str_(l))//",")
            end if
            n = n+1
         end do
         call flush_(self%Afile)
      end do
      call text_(self%Afile,"}")
      call text_(self%Afile,cb//",")
      maxim = maxval(density)
      minim = minval(density)

      ! Has any bonds?
      has_bonds = FALSE
      do a = 1,n
         if (size(bonds(a)%element)==0) cycle
         has_bonds = TRUE
         exit
      end do

      ! Print the atom labels in Epilog
      call text_(self%Afile,"Epilog->{")
      n = size(math_pos,1)
      do a = 1,n
         at_label = "Text"//ob//'"'//trim(atom(a)%label)//'",{'
         at_label = trim(at_label)//trim(to_str_(math_pos(a,1),"F7.1"))//","
         at_label = trim(at_label)//trim(to_str_(math_pos(a,2),"F7.1"))//"}"//cb
         if (a<n OR has_bonds) at_label = trim(at_label)//","
         call text_(self%Afile,at_label)
      end do

      ! Print the atom bond lines
      if (has_bonds) then

         ! Loop over atoms with bonds
         do a = 1,n
            if (size(bonds(a)%element)==0) cycle

            ! Loop over the bonded atoms
            nb = size(bonds(a)%element)
            do b = 1,nb

               ! Index of bonded atom
               c = bonds(a)%element(b)
               if (c<=a) cycle

               ! Calculate line start and finish
               ba = math_pos(c,1:2)-math_pos(a,1:2)
               ax = math_pos(a,1)+0.1d0*ba(1)
               ay = math_pos(a,2)+0.1d0*ba(2)
               bx = ax+0.8d0*ba(1)
               by = ay+0.8d0*ba(2)

               ! Print out the line
               line = "Line"//ob//"{{"
               line = trim(line)//trim(to_str_(ax,"F7.1"))//","
               line = trim(line)//trim(to_str_(ay,"F7.1"))//"},{"
               line = trim(line)//trim(to_str_(bx,"F7.1"))//","
               line = trim(line)//trim(to_str_(by,"F7.1"))//"}}"//cb
         !    if ((a < n OR b /= nb) AND n>0) line = trim(line)//","
         !    if (a == n AND b == nb) line = trim(line)
               if (b<=nb AND n>0) line = trim(line)//","  ! comma only if there are atoms to follow
               call text_(self%Afile,line)
            end do
         end do
      end if
      ! End Epilog
      call text_(self%Afile,"},")

      ! Print the ContourGraphics options
      call text_(self%Afile,"Frame->False,")
      call text_(self%Afile,"Contours->10,")
      call text_(self%Afile,"ColorFunctionScaling->True,")
      hue = "(Hue"//ob//"0.7-0.7#"//cb//" &)"
      call text_(self%Afile,"ColorFunction->"//trim(hue)//cb//",")

      ! Print the ShowLegend options
      call text_(self%Afile,"{")
      call text_(self%Afile,trim(hue))
      call text_(self%Afile,",20,")
      call text_(self%Afile,'"'//trim(to_str_(minim,"F12.7"))//'",')  !default format
      call text_(self%Afile,'"'//trim(to_str_(maxim,"F12.7"))//'",')
      call text_(self%Afile,"LegendPosition->{1,0},")
      call text_(self%Afile,"LegendShadow->None")
      call text_(self%Afile,"}")
      call text_(self%Afile,cb)
      call close_(self)
   end subroutine

   subroutine write_gnuplot_1(self,j,n_x,n_y,n_z,norm,normalize)
   ! Write a current density vector "j" corresponding to a grid with dimensions
   ! "n_x", "n_y", "n_z" in gnuplot format (suitable for an splot).
   ! If "norm" is present and TRUE write the norm of the vector field
   ! If "normalize" is present and TRUE write the normalized of the vector field
      type(ARCHIVE_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: j
      INT, IN :: n_x,n_y,n_z
      BIN, optional, IN :: norm,normalize
      INT :: n,x,y,z
      BIN :: write_norm,write_normalized
      VEC(REAL,3) :: v
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,14)
      call set_real_precision_(self%Afile,6)
      write_norm = FALSE
      if (present(norm)) write_norm = norm
      write_normalized = FALSE
      if (present(normalize)) write_normalized = normalize
      n = 1
      do z = 1,n_z
         if (n_z>1) then
            call put_(self%Afile," z = ")
            call put_(self%Afile,z)
            call flush_(self%Afile)
         end if
         do y = 1,n_y
            do x = 1,n_x
               v = j(n,:)
               if (write_norm) then
                  call put_(self%Afile,norm_(v))
               else
                  if (write_normalized) call normalize_(v)
                  call put_(self%Afile,v(1))
                  call put_(self%Afile,v(2))
                  call put_(self%Afile,v(3))
               end if
               call flush_(self%Afile)
               n = n+1
            end do
            call flush_(self%Afile)
         end do
      end do
      call close_(self)
   end subroutine

   subroutine write_contour_a_la_nancy(self,density,grid,atom)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in the "contour" format for work in Nancy. This also requires
   ! the "atom" list and plot "grid" as well.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      INT :: n,f,l
      VEC(REAL,3) :: vec
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,20)
      call set_real_precision_(self%Afile,12)
      call text_(self%Afile,"Tonto output for the contour program used in Nancy ...")
      call put_(self%Afile,size(atom),real_width=TRUE)
      vec = grid%width
      call convert_to_(vec,"angstrom")
      call put_(self%Afile,vec(1))
      call put_(self%Afile,vec(2))
      call put_(self%Afile,vec(3))
      call flush_(self%Afile)
      do n = 1,size(atom)
         call put_(self%Afile,atom(n)%label)
         vec = atom(n)%position
         call convert_to_(vec,"angstrom")
         call put_(self%Afile,vec(1))
         call put_(self%Afile,vec(2))
         call put_(self%Afile,vec(3))
         call flush_(self%Afile)
      end do
      call put_(self%Afile,grid%n_x)
      call put_(self%Afile,grid%n_y)
      call put_(self%Afile,grid%n_z)
      call flush_(self%Afile)
      call set_using_array_labels_(self%Afile,FALSE)
      f = 1
      l = grid%n_x
      do n = 1,grid%n_y*grid%n_z
         call put_(self%Afile,density(f:l))
         f = f+grid%n_x
         l = l+grid%n_x
      end do
      call close_(self)
   end subroutine

   subroutine write_contour_a_la_spackman(self,density,grid,atom)
   ! Write a "density" vector corresponding to a grid with dimensions
   ! "n_x", "n_y" in the .contour format of Spackman. This also
   ! requires the "atom" list and plot "grid" as well.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      INT :: n,x,y,an
      REAL :: fac
      MAT(REAL,3,3) :: axes
      VEC(REAL,3) :: pos,p
      MAT(REAL,:,:), allocatable :: density2d
      STR(len=STR_SIZE) :: units
   DIE_IF(grid%n_z>1,"ARCHIVE:write_contour_a_la_spackman ... must be only one plane!")
   DIE_IF(grid%n_z<1,"ARCHIVE:write_contour_a_la_spackman ... must have at least one plane!")
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,13)
      call set_real_precision_(self%Afile,5)

      ! Begin atoms list (dummy line)
      call text_(self%Afile,"ATOMS")

      ! Write out the number of atoms
      call put_(self%Afile,size(atom))
      call flush_(self%Afile)

      ! Shift coordinates to grid center and rotate
      ! Plot only atoms which are within the plot grid box.
      ! To do this we have to change into the plot axis system.
      axes(1,:) = grid%x_axis
      axes(2,:) = grid%y_axis
      axes(3,:) = grid%z_axis
      units = "angstrom"
      fac = conversion_factor_(units)
      do n = 1,size(atom)
         p = atom(n)%position-grid%origin
         call to_scaled_product_of_(pos,axes,p,fac)
         an = atom(n)%atomic_number
         if (an<1) cycle
         call put_(self%Afile,an)  ! Z
         call put_(self%Afile,pos(1))  ! Pos in plot grid axis system
         call put_(self%Afile,pos(2))
         call put_(self%Afile,pos(3))
         call flush_(self%Afile)
      end do

      ! Create the 3d grid
      call create_(density2d,grid%n_x,grid%n_y)
      density2d = reshape(density,[grid%n_x,grid%n_y])

      ! Write out the no. of points
      call text_(self%Afile,"No. of points in each axis direction")
      call put_(self%Afile,grid%n_x)
      call put_(self%Afile,grid%n_y)
      call flush_(self%Afile)

      ! Write out the widths
      call text_(self%Afile,"Width of each axis & grid points")
      call put_(self%Afile,grid%width(1)*fac)
      call flush_(self%Afile)
      call put_(self%Afile,grid%width(2)*fac)
      call flush_(self%Afile)

      ! Write out the grid
      do y = 1,grid%n_y
         do x = 1,grid%n_x
            call put_(self%Afile,density2d(x,y))
            if (mod(x,6)==0 OR x==grid%n_x) call flush_(self%Afile)
         end do
      end do

      ! Clean
      call destroy_(density2d)
      call close_(self)
   end subroutine

   subroutine write_a_la_xdgraph(self,density,grid,atom)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in the "xdgraph" format. This also requires the "atom" list
   ! and plot "grid" as well.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      INT :: n,f,l
      VEC(REAL,3) :: vec,v
      VEC(INT,3) :: atm
      MAT(REAL,3,3) :: axes
      BIN :: has_axis_atoms
      has_axis_atoms = grid%x_atom_1/=0 AND grid%x_atom_2/=0 AND grid%y_atom_1/=0 AND grid%y_atom_2/=0 AND grid%y_atom_1==grid%x_atom_1
      if (has_axis_atoms) then
         call open_for_(self,"writing",ascii=TRUE)
         call set_margin_width_(self%Afile,0)
         if (grid%n_z==1) then; call put_(self%Afile,"2DGRDFIL",width=8)
         else; call put_(self%Afile,"3DGRDFIL",width=8)
         end if
         call put_(self%Afile,0,width=3)
         call flush_(self%Afile)
         call put_(self%Afile,"TONTO",width=5)
         call put_(self%Afile,"FOU",width=7)
         call flush_(self%Afile)
         call flush_(self%Afile)
         call put_(self%Afile,"! Gridpoints, Origin, Physical Dimensions")
         call flush_(self%Afile)
         call put_(self%Afile,grid%n_x,width=15)
         call put_(self%Afile,grid%n_y,width=15)
         call put_(self%Afile,grid%n_z,width=15)
         call flush_(self%Afile)
         call set_real_fortran_fmt_(self%Afile,"f")
         call set_real_width_(self%Afile,11)
         call set_real_precision_(self%Afile,4)
         vec = grid%width
         call convert_to_(vec,"angstrom")
         call put_(self%Afile,vec(1)*HALF)
         call put_(self%Afile,vec(2)*HALF)
         call put_(self%Afile,vec(3)*HALF)
         call flush_(self%Afile)
         call put_(self%Afile,vec(1))
         call put_(self%Afile,vec(2))
         call put_(self%Afile,vec(3))
         call flush_(self%Afile)
         call put_(self%Afile,"! Objects",width=9)
         call flush_(self%Afile)
         call put_(self%Afile,size(atom),width=10)
         call flush_(self%Afile)
         call set_real_fortran_fmt_(self%Afile,"f")
         call set_real_width_(self%Afile,10)
         call set_real_precision_(self%Afile,5)
         axes(1,:) = grid%x_axis
         axes(2,:) = grid%y_axis
         axes(3,:) = grid%z_axis
         atm = [grid%x_atom_1,grid%x_atom_2,grid%y_atom_2]
         do n = 1,size(atom(atm))
            l = min(len_trim(atom(atm(n))%label),8)
            call put_(self%Afile,atom(atm(n))%label,width=l)
            call tab_(self%Afile,width=8-l)
            v = atom(atm(n))%position
            v = v-grid%center
            call to_product_of_(vec,axes,v)
            call convert_to_(vec,"angstrom")
            call put_(self%Afile,vec(1))
            call put_(self%Afile,vec(2))
            call put_(self%Afile,vec(3))
            call put_(self%Afile,"ATOM",width=5)
            call flush_(self%Afile)
         end do
         do n = 1,size(atom)
            if (n==grid%x_atom_1) cycle
            if (n==grid%x_atom_2) cycle
            if (n==grid%y_atom_2) cycle
            l = min(len_trim(atom(n)%label),8)
            call put_(self%Afile,atom(n)%label,width=l)
            call tab_(self%Afile,width=8-l)
            v = atom(n)%position
            v = v-grid%center
            call to_product_of_(vec,axes,v)
            call convert_to_(vec,"angstrom")
            call put_(self%Afile,vec(1))
            call put_(self%Afile,vec(2))
            call put_(self%Afile,vec(3))
            call put_(self%Afile,"ATOM",width=5)
            call flush_(self%Afile)
         end do
         call put_(self%Afile,"! Connections",width=13)
         call flush_(self%Afile)
         call put_(self%Afile,0,width=10)
         call flush_(self%Afile)
         call put_(self%Afile,"! Values",width=8)
         call flush_(self%Afile)
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call set_real_width_(self%Afile,13)
         call set_real_precision_(self%Afile,5)
         call set_n_fields_per_line_(self%Afile,6)
         f = 1
         l = grid%n_x
         do n = 1,grid%n_y*grid%n_z
            call put_(self%Afile,density(f:l),by_row=TRUE)
            f = f+grid%n_x
            l = l+grid%n_x
         end do
         call close_(self)
      else  ! No axis atoms
         call open_for_(self,"writing",ascii=TRUE)
         call set_margin_width_(self%Afile,0)
         if (grid%n_z==1) then; call put_(self%Afile,"2DGRDFIL",width=8)
         else; call put_(self%Afile,"3DGRDFIL",width=8)
         end if
         call put_(self%Afile,0,width=3)
         call flush_(self%Afile)
         call put_(self%Afile,"TONTO",width=5)
         call put_(self%Afile,"FOU",width=7)
         call flush_(self%Afile)
         call flush_(self%Afile)
         call put_(self%Afile,"! Gridpoints, Origin, Physical Dimensions")
         call flush_(self%Afile)
         call put_(self%Afile,grid%n_x,width=15)
         call put_(self%Afile,grid%n_y,width=15)
         call put_(self%Afile,grid%n_z,width=15)
         call flush_(self%Afile)
         call set_real_fortran_fmt_(self%Afile,"f")
         call set_real_width_(self%Afile,11)
         call set_real_precision_(self%Afile,4)
         call put_(self%Afile,HALF)
         call tab_(self%Afile,width=4)
         call put_(self%Afile,HALF)
         call tab_(self%Afile,width=4)
         call put_(self%Afile,HALF)
         call flush_(self%Afile)
         call put_(self%Afile,ONE)
         call tab_(self%Afile,width=4)
         call put_(self%Afile,ONE)
         call tab_(self%Afile,width=4)
         call put_(self%Afile,ONE)
         call flush_(self%Afile)
         call put_(self%Afile,"! Objects",width=9)
         call flush_(self%Afile)
         call put_(self%Afile,0,width=10)
         call flush_(self%Afile)
         call set_real_fortran_fmt_(self%Afile,"f")
         call set_real_width_(self%Afile,10)
         call set_real_precision_(self%Afile,5)
         call put_(self%Afile,"! Connections",width=13)
         call flush_(self%Afile)
         call put_(self%Afile,0,width=10)
         call flush_(self%Afile)
         call put_(self%Afile,"! Values",width=8)
         call flush_(self%Afile)
         call set_using_array_labels_(self%Afile,FALSE)
         call set_real_fortran_fmt_(self%Afile,"e")
         call set_real_width_(self%Afile,13)
         call set_real_precision_(self%Afile,5)
         call set_n_fields_per_line_(self%Afile,6)
         f = 1
         l = grid%n_x
         do n = 1,grid%n_y*grid%n_z
            call put_(self%Afile,density(f:l),by_row=TRUE)
            f = f+grid%n_x
            l = l+grid%n_x
         end do
         call close_(self)
      end if
   end subroutine

   subroutine write_gaussian_cube(self,density,grid,atom,title)
   ! Write a "density" vector corresponding to a grid with dimensions
   ! "n_x", "n_y", "n_z" in the gaussian cube format. This also
   ! requires the "atom" list and plot "grid" as well.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      STR(len=*), IN :: title
      INT :: n,n_atom,x,y,z
      REAL :: an
      MAT(REAL,3,3) :: axes,orth,trans
      VEC(REAL,3) :: low,high,pos,por
      MAT3(REAL,:,:,:), allocatable :: reverse
   ENSURE(grid%n_z>0,"ARCHIVE:write_gaussian_cube ... there are no points to print")
   ENSURE(equals_(norm_(grid%x_axis),ONE),"ARCHIVE:write_gaussian_cube ... grid x axis is not normalized")
   ENSURE(equals_(norm_(grid%y_axis),ONE),"ARCHIVE:write_gaussian_cube ... grid y axis is not normalized")
   ENSURE(equals_(norm_(grid%z_axis),ONE),"ARCHIVE:write_gaussian_cube ... grid z axis is not normalized")
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"f")
      call set_real_width_(self%Afile,12)
      call set_real_precision_(self%Afile,6)
      call text_(self%Afile,"Tonto cube file")
      call text_(self%Afile,trim(title))

      ! Find out the number of atoms which are within the plot grid
      ! box. We have to change into the plot axis system. 
      axes = plot_axes_(grid)
      orth = axes

   ! DIE_IF(NOT orth.is_orthogonal_mx,"gaussian plot axes are not orthogonal")
      trans = transpose(orth)
      low = -HALF*grid%width  ! bottom left front in plot axis system
      high = HALF*grid%width  ! top right back ....

      ! Count atoms in the grid box
      n_atom = 0
      do n = 1,size(atom)

         ! Rotated position
         pos = atom(n)%position-grid%center
         call to_product_of_(por,trans,pos)
         if (NOT is_in_range_(por,low,high)) cycle
         an = atom(n)%atomic_number
         if (an<1) cycle
         n_atom = n_atom+1
      end do
      call put_(self%Afile,n_atom,width=5)
      call put_(self%Afile,low(1))
      call put_(self%Afile,low(2))
      call put_(self%Afile,low(3))
      call flush_(self%Afile)

      ! Transform atomn positions later into 
      ! standard x, y, z axes (unit matrix)
      ! NOT RIGHT FOR-ORTHO SYSTEM
   ! orth.to_unit_mx
      orth(:,1) = orth(:,1)*grid%del_x
      orth(:,2) = orth(:,2)*grid%del_y
      orth(:,3) = orth(:,3)*grid%del_z
      call put_(self%Afile,grid%n_x,width=5)
      call put_(self%Afile,orth(1,1))
      call put_(self%Afile,orth(2,1))
      call put_(self%Afile,orth(3,1))
      call flush_(self%Afile)
      call put_(self%Afile,grid%n_y,width=5)
      call put_(self%Afile,orth(1,2))
      call put_(self%Afile,orth(2,2))
      call put_(self%Afile,orth(3,2))
      call flush_(self%Afile)
      call put_(self%Afile,grid%n_z,width=5)
      call put_(self%Afile,orth(1,3))
      call put_(self%Afile,orth(2,3))
      call put_(self%Afile,orth(3,3))
      call flush_(self%Afile)
      do n = 1,size(atom)

         ! Rotated position
         pos = atom(n)%position-grid%center
         call to_product_of_(por,trans,pos)
         if (NOT is_in_range_(por,low,high)) cycle
         an = atom(n)%atomic_number
         if (an<1) cycle
         call put_(self%Afile,int(an),width=5)  ! Z
         call put_(self%Afile,an)  ! Charge?
         call put_(self%Afile,por(1))  ! Pos in plot grid axis system
         call put_(self%Afile,por(2))
         call put_(self%Afile,por(3))
         call flush_(self%Afile)
      end do
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,13)
      call set_real_precision_(self%Afile,5)
      call create_(reverse,grid%n_x,grid%n_y,grid%n_z)
      reverse = reshape(density,[grid%n_x,grid%n_y,grid%n_z])

      ! Hard coded crap. Sorry.
      where (abs(reverse)<TOL(99)) reverse = ZERO
      do x = 1,grid%n_x
         do y = 1,grid%n_y
            do z = 1,grid%n_z
               call put_(self%Afile,reverse(x,y,z))
               if (mod(z,6)==0) call flush_(self%Afile)
            end do
            call flush_(self%Afile)
         end do
      end do
      call destroy_(reverse)
      call close_(self)
   end subroutine

   subroutine write_gaussian_cell(self,density,grid,atom,title)
   ! Write a "density" vector corresponding to a grid with dimensions
   ! "n_x", "n_y", "n_z" in the gaussian cube format *but* for cases
   ! when a unit cell is needed. This requires the unit cell "atom"
   ! list and plot "grid" as well.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      STR(len=*), IN :: title
      INT :: n,n_atom,nx,ny,nz,x,y,z
      REAL :: an
      MAT(REAL,3,3) :: axes
      VEC(REAL,3) :: pos
      MAT3(REAL,:,:,:), allocatable :: reverse
   ENSURE(grid%n_z>0,"ARCHIVE:write_gaussian_cell ... there are no points to print")
   ENSURE(equals_(norm_(grid%x_axis),ONE),"ARCHIVE:write_gaussian_cell ... grid x axis is not normalized")
   ENSURE(equals_(norm_(grid%y_axis),ONE),"ARCHIVE:write_gaussian_cell ... grid y axis is not normalized")
   ENSURE(equals_(norm_(grid%z_axis),ONE),"ARCHIVE:write_gaussian_cell ... grid z axis is not normalized")
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"f")
      call set_real_width_(self%Afile,12)
      call set_real_precision_(self%Afile,6)
      call text_(self%Afile,"Tonto unit-cell cube file")
      call text_(self%Afile,trim(title))
      n_atom = size(atom)
      call put_(self%Afile,n_atom,width=5)
      call put_(self%Afile,grid%origin(1))
      call put_(self%Afile,grid%origin(2))
      call put_(self%Afile,grid%origin(3))
      call flush_(self%Afile)
      axes = plot_axes_(grid)
      axes(:,1) = axes(:,1)*grid%del_x
      axes(:,2) = axes(:,2)*grid%del_y
      axes(:,3) = axes(:,3)*grid%del_z
      nx = grid%n_x
      ny = grid%n_y
      nz = grid%n_z
      call put_(self%Afile,nx,width=5)
      call put_(self%Afile,axes(1,1))
      call put_(self%Afile,axes(2,1))
      call put_(self%Afile,axes(3,1))
      call flush_(self%Afile)
      call put_(self%Afile,ny,width=5)
      call put_(self%Afile,axes(1,2))
      call put_(self%Afile,axes(2,2))
      call put_(self%Afile,axes(3,2))
      call flush_(self%Afile)
      call put_(self%Afile,nz,width=5)
      call put_(self%Afile,axes(1,3))
      call put_(self%Afile,axes(2,3))
      call put_(self%Afile,axes(3,3))
      call flush_(self%Afile)
      do n = 1,n_atom
         pos = atom(n)%position
         an = atom(n)%atomic_number
         call put_(self%Afile,int(an),width=5)  ! Z
         call put_(self%Afile,an)  ! Charge?
         call put_(self%Afile,pos(1))  ! Pos in plot grid axis system
         call put_(self%Afile,pos(2))
         call put_(self%Afile,pos(3))
         call flush_(self%Afile)
      end do
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,13)
      call set_real_precision_(self%Afile,5)
      call create_(reverse,nx,ny,nz)
      reverse = reshape(density,[nx,ny,nz])

      ! Hard coded crap. Sorry.
      where (abs(reverse)<TOL(99)) reverse = ZERO

      ! Write, fortran style
      if (is_master_processor_(tonto)) write(self%Afile%unit,"(*(6e13.5,/))") (((reverse(x,y,z),z=1,nz),y=1,ny),x=1,nx)

      ! Clean
      call destroy_(reverse)
      call close_(self)
   end subroutine

   subroutine write_xcrysden_xsf(self,density,grid,atom,title)
   ! Write a "density" vector corresponding to a grid with dimensions
   ! "n_x", "n_y", "n_z" in the XSF format. This also requires the
   ! "atom" list and plot "grid" as well.
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      VEC(type(ATOM_TYPE),:), IN :: atom
      STR(len=*), IN :: title
      INT :: n,x,y,z,an
      REAL :: fac
      MAT(REAL,3,3) :: axes
      VEC(REAL,3) :: low,high,pos,p
      MAT3(REAL,:,:,:), allocatable :: density3d
      STR(len=STR_SIZE) :: units
      if (grid%n_z<=1) return
      call open_for_(self,"writing",ascii=TRUE)
      call set_real_fortran_fmt_(self%Afile,"e")
      call set_real_width_(self%Afile,13)
      call set_real_precision_(self%Afile,5)

      ! Coordinates: shift to grid center and rotate
      ! Plot only atoms which are within the plot grid box.
      ! To do this we have to change into the plot axis system.
      call text_(self%Afile,"ATOMS")
      axes(1,:) = grid%x_axis
      axes(2,:) = grid%y_axis
      axes(3,:) = grid%z_axis
      units = "angstrom"
      fac = conversion_factor_(units)
      low = -HALF*grid%width*fac  ! bottom left front in plot axis system
      high = HALF*grid%width*fac  ! top right back ....
      do n = 1,size(atom)
         p = atom(n)%position-grid%center
         call to_scaled_product_of_(pos,axes,p,fac)
         if (NOT is_in_range_(pos(:),low,high)) cycle
         an = atom(n)%atomic_number
         if (an<1) cycle
         call put_(self%Afile,an)  ! Z
         call put_(self%Afile,pos(1))  ! Pos in plot grid axis system
         call put_(self%Afile,pos(2))
         call put_(self%Afile,pos(3))
         call flush_(self%Afile)
      end do

      ! Create the 3d grid
      call create_(density3d,grid%n_x,grid%n_y,grid%n_z)
      density3d = reshape(density,[grid%n_x,grid%n_y,grid%n_z])

      ! Write out the grid
      call flush_(self%Afile)
      call text_(self%Afile,"BEGIN_BLOCK_DATAGRID_3D")
      call text_(self%Afile,title)
      call text_(self%Afile,"BEGIN_DATAGRID_3D_"//trim(title))
      call put_(self%Afile,grid%n_x)
      call put_(self%Afile,grid%n_y)
      call put_(self%Afile,grid%n_z)
      call flush_(self%Afile)
      call put_(self%Afile,low(1))
      call put_(self%Afile,low(2))
      call put_(self%Afile,low(3))
      call flush_(self%Afile)
      call put_(self%Afile,grid%width(1)*fac)
      call put_(self%Afile,ZERO)
      call put_(self%Afile,ZERO)
      call flush_(self%Afile)
      call put_(self%Afile,ZERO)
      call put_(self%Afile,grid%width(2)*fac)
      call put_(self%Afile,ZERO)
      call flush_(self%Afile)
      call put_(self%Afile,ZERO)
      call put_(self%Afile,ZERO)
      call put_(self%Afile,grid%width(3)*fac)
      call flush_(self%Afile)
      do z = 1,grid%n_z
         do y = 1,grid%n_y
            do x = 1,grid%n_x
               call put_(self%Afile,density3d(x,y,z))
               if (mod(x,6)==0 OR x==grid%n_x) call flush_(self%Afile)
            end do
         end do
      end do
      call text_(self%Afile,"END_DATAGRID_3D")
      call text_(self%Afile,"END_BLOCK_DATAGRID_3D")

      ! Clean
      call destroy_(density3d)
      call close_(self)
   end subroutine

   subroutine write_vapor(self,density,n_x,n_y,n_z)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in VAPOR format
      type(ARCHIVE_TYPE), INOUT :: self
      INT, IN :: n_x,n_y,n_z
      VEC(REAL,n_x*n_y*n_z), IN :: density
      INT :: u

      ! NOTE: raw stream I/O, bypassing the FILE/TEXTFILE API. Each rank gets its
      ! OWN valid unit from newunit=, so unlike the redirected-TEXTFILE cases this
      ! does not crash under MPI -- every rank simply opens the SAME filename and
      ! writes concurrently, silently corrupting the output. Master-only is the
      ! right semantics: this is a single output file, replicated data.
      if (IO_IS_ALLOWED) then

         ! Get the size of the record
         open(file=trim(file_name_(self)),newunit=u,form="unformatted",access="stream")

         ! Write code "8" for single precision data
         write(u) density

         ! Close and free
         close(u)
      end if
   end subroutine

   subroutine write_drishti(self,density,n_x,n_y,n_z)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in drishti format
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      INT, IN :: n_x,n_y,n_z
      INT :: u,ios,n,x,y,z,n_xy,offx,offy,offz
      INT :: ival4
      REAL :: rval4

      ! Sizes
      n_xy = n_x*n_y

      ! Master-only: raw stream I/O to a single output file (see above).
      if (IO_IS_ALLOWED) then

      ! Get the size of the record
         open(file=trim(file_name_(self)),newunit=u,access="stream",iostat=ios)
         DIE_IF(ios/=0,"ARCHIVE:write_drishti ... error opening stream file")

      ! Write code "8" for single precision data
         rval4 = EIGHT
         write(u) rval4

      ! Write code "8" for single precision data
         DIE_IF(n_x>INT_HUGE,"ARCHIVE:write_drishti ... n_x dimension too large for unsigned int")
         DIE_IF(n_y>INT_HUGE,"ARCHIVE:write_drishti ... n_y dimension too large for unsigned int")
         DIE_IF(n_z>INT_HUGE,"ARCHIVE:write_drishti ... n_z dimension too large for unsigned int")
         ival4 = n_x; write(u) ival4
         ival4 = n_y; write(u) ival4
         ival4 = n_z; write(u) ival4

      ! Now write out the data in z-y-x order
         offx = 0
         do x = 1,n_x
            offy = 0
            do y = 1,n_y
               offz = 0
               do z = 1,n_z
                  n = offx+offy+offz+1
                  rval4 = density(n)
                  write(u) rval4
                  offz = offz+n_xy
               end do
               offy = offy+n_x
            end do
            offx = offx+1
         end do

      ! Close and free
         close(u)
      end if
   end subroutine

   subroutine write_vtk(self,density,grid,title)
   ! Write a "density" vector corresponding to a grid with dimensions "n_x",
   ! "n_y", "n_z" in VTK format
      type(ARCHIVE_TYPE), INOUT :: self
      VEC(REAL,:), IN :: density
      type(PLOT_GRID_TYPE), IN :: grid
      STR(len=*), IN :: title
      STR(len=1) :: lf
      INT :: u,i,n_x,n_y,n_z
      REAL :: d_x,d_y,d_z,o_x,o_y,o_z

      ! Master-only: raw stream I/O to a single output file (see above).
      if (IO_IS_ALLOWED) then

      ! Open the file in stream mode
         open(file=trim(file_name_(self)),newunit=u,access="stream")

      ! Header info
         lf = char(10)
         n_x = grid%n_x; n_y = grid%n_y; n_z = grid%n_z
         d_x = grid%del_x; d_y = grid%del_y; d_z = grid%del_z
         o_x = grid%origin(1); o_y = grid%origin(2); o_z = grid%origin(3)
         write(u) "# vtk DataFile Version 3.0"//lf
         write(u) trim(title)//lf
         write(u) "BINARY"//lf
         write(u) "DATASET STRUCTURED_POINTS"//lf
         write(u) "DIMENSIONS "//trim(to_str_(n_x))//" "//trim(to_str_(n_y))//" "//trim(to_str_(n_z))//lf
         write(u) "ORIGIN "//trim(to_str_(o_x))//" "//trim(to_str_(o_y))//" "//trim(to_str_(o_z))//lf
         write(u) "SPACING"//trim(to_str_(d_x))//" "//trim(to_str_(d_y))//" "//trim(to_str_(d_z))//lf

      ! Data
         write(u) "SCALARS "//trim(title)//" double"//lf
         write(u) "LOOKUP_TABLE default"//lf
         write(u) (density(i),i=1,n_x*n_y*n_z)

      ! Close and free
         close(u)
      end if
   end subroutine

end module
