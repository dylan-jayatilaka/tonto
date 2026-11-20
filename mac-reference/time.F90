!---------------------------------------------------------------------------
!
! Time: timing routines
!
! This module works with either CPU seconds (for short times) or with
! Julian Days/Hours/Minutes/Seconds/Milliseconds (a so-called 5-time).
! 5-times are based on date_and_time, CPU seconds based on cpu_time.
!
! There are stopwatch routines, and time output strings.
!
! Copyright (C) Daniel Grimwood, 1998
! Copyright (C) Dylam Jayatilaka, 2007
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
!
!---------------------------------------------------------------------------
module TIME_MODULE

#  include "macros"

   include "time.use"

#ifdef MPI
   USE mpi
#endif

   implicit none

   include "time.int"

   type(TIME_TYPE), public :: std_time

contains

! ==========
! Essentials
! ==========

   function current_time8() result (time)
   ! Returns current time as a 8-vector from date_and_time
      VEC(INT,8) :: time

      ! Send time from processor 0
      time = 0
      if (IO_IS_ALLOWED) then
         call date_and_time(values=time)
      end if
      PARALLEL_BROADCAST(time,master_processor_(tonto))
   end function

   function current_time5() result (res)
   ! Returns current time as a 5-vector, using Julian days
      VEC(INT,5) :: res
      VEC(INT,8) :: time
      INT :: y,m,d,j
      time = current_time8()
      y = time(1)
      m = time(2)
      d = time(3)
      call to_julian_day(j,d,m,y)
      res(1) = j  ! Julian day
      res(2) = time(5)  ! hour
      res(3) = time(6)  ! minute
      res(4) = time(7)  ! second
      res(5) = time(8)  ! millisecond
   end function

   function current_cpu_time() result (res)
   ! Return the current cpu_time.
      REAL :: res
      REAL :: time

      ! Get the CPU time
      time = ZERO
      if (IO_IS_ALLOWED) then
         call cpu_time(time)
      end if
      PARALLEL_BROADCAST(time,master_processor_(tonto))
      res = time
   end function


! ============
! Current date
! ============

   function current_date_YMD_str() result (res)
   ! Return the date as a YYYY-MM-DD string
      STR(len=STR_SIZE) :: res
      INT :: d,m,y
      call get_current_date_DMY(d,m,y)
      res = trim(to_str_(y))//"-"//trim(to_str_(m))//"-"//trim(to_str_(d))
   end function

   function current_date_DMY_str() result (res)
   ! Return the date as a DD-MM-YYYY string
      STR(len=STR_SIZE) :: res
      INT :: d,m,y
      call get_current_date_DMY(d,m,y)
      res = trim(to_str_(d))//"-"//trim(to_str_(m))//"-"//trim(to_str_(y))
   end function

   subroutine get_current_date_DMY(d,m,y)
   ! Return the day "d", month "m", year "y"
      INT, OUT :: d,m,y
      VEC(INT,8) :: time
      time = current_time8()
      y = time(1)
      m = time(2)
      d = time(3)
   end subroutine


! ============
! Current time
! ============

   function current_time_message() result (res)
   ! The current time message
      STR(len=40) :: res
      res = "The current time is "//current_time_str()//"."
   end function

   function current_time_str() result (res)
   ! Return a string containing the time.
   ! Formatted as DD/MM/YYYY HH:MM:SS
      STR(len=19) :: res
      res = time5_to_DMY_HMS_str(current_time5())
   end function


! ===================
! Stopwatch functions
! ===================

   subroutine start_timing(self)
   ! Starts the timer
      type(TIME_TYPE), INOUT :: self
      self%start_time5 = current_time5()
      self%cpu_start_time = current_cpu_time()
   end subroutine

   subroutine stop_timing(self)
   ! Stops the timer
      type(TIME_TYPE), INOUT :: self
      self%stop_time5 = current_time5()
      self%cpu_stop_time = current_cpu_time()
   end subroutine


! ================
! Start/stop times
! ================

   function start_time_message(self) result (res)
   ! The start time message
      type(TIME_TYPE), IN :: self
      STR(len=37) :: res
      res = "Timer started at "//start_time_str_(self)//"."
   end function

   function start_time_str(self) result (res)
   ! Return a string containing the start time.
   ! Formatted as DD/MM/YYYY HH:MM:SS
      type(TIME_TYPE), IN :: self
      STR(len=19) :: res
      res = time5_to_DMY_HMS_str(self%start_time5)
   end function

   function stop_time_message(self) result (res)
   ! The stop time message
      type(TIME_TYPE), IN :: self
      STR(len=37) :: res
      res = "Timer stopped at "//stop_time_str_(self)//"."
   end function

   function stop_time_str(self) result (res)
   ! Return a string containing the start time.
   ! Formatted as DD/MM/YYYY HH:MM:SS
      type(TIME_TYPE), IN :: self
      STR(len=19) :: res
      res = time5_to_DMY_HMS_str(self%stop_time5)
   end function


! ================
! Elapsed CPU time
! ================

   function elapsed_cpu_time(self) result (res)
   ! Returns elapsed cpu time taken since .start_timing. 
      type(TIME_TYPE), IN :: self
      REAL :: res
      res = current_cpu_time()-self%cpu_start_time
   end function

   function elapsed_cpu_time_message(self,task) result (res)
   ! Returns elapsed cpu time taken since .start_timing. Message
   ! returned is: "CPU time taken is ... CPU seconds." If "task" is 
   ! given, returns  "CPU time taken for "task" is ... CPU seconds."
      type(TIME_TYPE), IN :: self
      STR(len=*), optional :: task
      STR(len=STR_SIZE) :: res
      STR(len=132) :: time_str
      REAL :: time
      time = elapsed_cpu_time_(self)
      write(time_str,"(f15.3)") time
      if (present(task)) then
         res = "CPU time taken for "//trim(task)//" is "//trim(adjustl(time_str))//" CPU seconds."
      else
         res = "CPU time taken is "//trim(adjustl(time_str))//" CPU seconds."
      end if
   end function


! ============
! Elapsed time
! ============

   function elapsed_time_message(self,task) result (res)
   ! Returns time taken. "Time taken is ..."
   ! If "task" given, "Time taken for task" is ..."
   ! NOTE: YOU MUST STOP THE TIMER FIRST - this is only the message
      type(TIME_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      STR(len=*), optional :: task
      if (present(task)) then
         res = "Wall-clock time taken for "//trim(task)//" is "//trim(elapsed_time_str_(self))//"."
      else
         res = "Wall-clock time taken is "//trim(elapsed_time_str_(self))//"."
      end if
   end function

   function elapsed_time_str(self) result (res)
   ! Return a string containing the elapsed time.
      type(TIME_TYPE), IN :: self
      STR(len=STR_SIZE) :: res
      VEC(INT,5) :: time
      time = elapsed_time5_(self)
      res = time5_to_DHMS_str(time)
   end function

   function elapsed_time5(self) result (time)
   ! Returns elapsed time vector
      type(TIME_TYPE), IN :: self
      VEC(INT,5) :: time
      time = current_time5()
      time = subtract_time5s(time,self%start_time5)
   end function

   function elapsed_time_s(self) result (secs)
   ! Returns elapsed time in seconds
      type(TIME_TYPE), IN :: self
      REAL :: secs
      VEC(INT,5) :: time
      time = current_time5()
      time = subtract_time5s(time,self%start_time5)
      secs = time5_to_seconds(time)
   end function

   function subtract_time5s(time1,time2) result (res)
   ! result = time1 - time2
      VEC(INT,5), IN :: time1,time2
      VEC(INT,5) :: res
      REAL :: secs1,secs2
      secs1 = time5_to_seconds(time1)
      secs2 = time5_to_seconds(time2)
      res = seconds_to_time5(secs1-secs2)
   end function


! =====================
! Julian day conversion
! =====================

   subroutine to_julian_day(j,d,m,y)
   ! Converts the days "d", months "m", years "y", into the Julian date "j".
      INT, OUT :: j
      INT, IN :: d,m,y
      INT :: mm,yy,c
      mm = m
      yy = y
      if (mm>2) then
         mm = mm-3
      else
         mm = mm+9
         yy = yy-1
      end if
      c = yy/100
      yy = yy-100*c
      j = (146097*c)/4+(1461*yy)/4+(153*mm+2)/5+d+1721119
   end subroutine

   subroutine from_julian_day(j,d,m,y)
   ! Converts the days "d", months "m", years "y", from the Julian date "j".
      INT, IN :: j
      INT, OUT :: d,m,y
      INT :: jj
      jj = j-1721119
      y = (4*jj-1)/146097
      jj = 4*jj-1-146097*y
      d = jj/4
      jj = (4*d+3)/1461
      d = 4*d+3-1461*jj
      d = (d+4)/4
      m = (5*d-3)/153
      d = 5*d-3-153*m
      d = (d+5)/5
      y = 100*y+jj
      if (m<10) then
         m = m+3
      else
         m = m-9
         y = y+1
      end if
   end subroutine


! =================
! 5-time conversion
! =================

   function seconds_to_time5(secs) result (res)
   ! Returns the number of days, hours, minutes, seconds, milliseconds from the
   ! number of seconds.
      VEC(INT,5) :: res
      REAL, IN :: secs
      REAL :: sex
      sex = abs(secs)
      res(1) = floor(sex/86400)  ! days
      sex = sex-res(1)*86400
      res(2) = floor(sex/3600)  ! hours
      sex = sex-res(2)*3600
      res(3) = floor(sex/60)  ! minutes
      sex = sex-res(3)*60
      res(4) = floor(sex)  ! seconds
      sex = sex-res(4)
      res(5) = floor(1000*sex)  ! milliseconds
      if (secs<0) res = -res
   end function

   function time5_to_seconds(time) result (res)
   ! Returns the number of seconds from the "time" vector: 
   ! days, hours, minutes, seconds, and milliseconds.
      VEC(INT,5), IN :: time
      REAL :: res
      res = time(1)*86400d0
      res = res+time(2)*3600d0
      res = res+time(3)*60d0
      res = res+time(4)
      res = res+time(5)/1000d0
   end function

   function time5_to_DMY_HMS_str(time) result (res)
   ! Return a string containing the time.
   ! Formatted as DD/MM/YYYY HH:MM:SS
      VEC(INT,5), IN :: time
      STR(len=19) :: res
      STR(len=2) :: day,month,hour,min,sec
      STR(len=4) :: year
      INT :: i,y,m,d
      call from_julian_day(time(1),d,m,y)
      res = " "
      write(day,fmt='(i2)') d
      write(month,fmt='(i2)') m
      write(year,fmt='(i4)') y
      write(hour,fmt='(i2)') time(2)
      write(min,fmt='(i2)') time(3)
      write(sec,fmt='(i2)') time(4)
      res = day//"/"//month//"/"//year//" "//hour//":"//min//":"//sec
      do i = 1,19
         if (res(i:i)==" ") res(i:i) = "0"
      end do
      res(11:11) = " "
   end function

   function time5_to_DHMS_str(time) result (res)
   ! Returns the elapsed time as a string.
   ! Formatted as days, hours, min, sec, msec.
   ! If time is greater than a day, does not output min or smaller.
      VEC(INT,5), IN :: time
      STR(len=STR_SIZE) :: res
      res = " "
      res = number_with_units(time(1),"day")
      res = trim(res)//number_with_units(time(2),"hour")
      if (time(1)==0) then
         res = trim(res)//number_with_units(time(3),"minute")
         if (time(2)==0) then
            res = trim(res)//number_with_units(time(4),"second")
            res = trim(res)//number_with_units(time(5),"millisecond")
         end if
      end if
   end function

   function number_with_units(number,unit) result (res)
   ! Returns the number with its units, if the number is non-zero.
      INT, IN :: number
      STR(len=*), IN :: unit
      STR(len=STR_SIZE) :: res
      BIN :: comma
      INT :: i
      STR(len=STR_SIZE) :: res1
      res = " "
      if (number/=0) then
         comma = TRUE

         ! Align left
         res1 = " "
         write(res1,'(i 20)') number
         res1 = adjustl(res1)
         i = 1

         ! insert comma at start if needed.
         if (comma) then
            res(1:2) = ", "
            i = 3
         end if
         res(i:) = trim(res1)//" "//unit

         ! add "s" to "unit"s if number not 1.
         if (number/=1) then
            i = len_trim(res)+1
            res(i:i) = "s"
         end if
      end if
   end function

end module
