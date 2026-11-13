!====================================================================
!
! COLOUR_FUNCTION:
!
! For generating an RGB colour triple from a given data value.
!
! The module takes as input a series of data values with their
! assigned RGB colours. Any data value is then assigned an RGB colour
! by interpolation from the colour table.
!
! The default data range is [0...1]. The colours assigned are:
! Blue-Green for the range [0...0.5
! Green-Red  for the range [0.5...1].
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
!====================================================================
module COLOUR_FUNCTION_MODULE

#  include "macros"

   include "colour_function.use"

   implicit none

   include "colour_function.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(COLOUR_FUNCTION_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"COLOUR_FUNCTION:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,range)
   ! Create the object
      type(COLOUR_FUNCTION_TYPE), allocatable, OUT :: self
      VEC(REAL,2), IN :: range
      call create_(self)
      call set_defaults_(self,range)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(COLOUR_FUNCTION_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"COLOUR_FUNCTION:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      call destroy_(self%data)
      call destroy_(self%RGB)
   end subroutine


!  ===========
!  Set methods
!  ===========

   PURE subroutine set_defaults(self,range)
   ! Set up a defaults
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(REAL,2), optional, IN :: range
      self%n_data = 3
      call destroy_(self%data)
      call create_(self%data,3)
      self%data = [ZERO,HALF,ONE]
      call destroy_(self%RGB)
      call create_(self%RGB,3,3)
      self%RGB(:,1) = [ZERO,ZERO,ONE]
      self%RGB(:,2) = [ZERO,ONE,ZERO]
      self%RGB(:,3) = [ONE,ZERO,ZERO]
      if (present(range)) call rescale_data_(self,range)
      self%finalized = TRUE
   end subroutine

   PURE subroutine set_reverse_defaults(self,range)
   ! Set up reversed defaults
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(REAL,2), optional, IN :: range
      self%n_data = 3
      call destroy_(self%data)
      call create_(self%data,3)
      self%data = [ZERO,HALF,ONE]
      call destroy_(self%RGB)
      call create_(self%RGB,3,3)
      self%RGB(:,1) = [ONE,ZERO,ZERO]
      self%RGB(:,2) = [ZERO,ONE,ZERO]
      self%RGB(:,3) = [ZERO,ZERO,ONE]
      if (present(range)) call rescale_data_(self,range)
      self%finalized = TRUE
   end subroutine


!  =============
!  Input methods
!  =============

   recursive subroutine read_keywords(self)
   ! Read data from "stdin" using keyword style input.
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      STR(len=STR_SIZE) :: word
   ENSURE(next_item_(stdin)=="{","COLOUR_FUNCTION:read_keywords ... expecting open bracket symbol, {")

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
         DIE_IF(reverted_(stdin),"COLOUR_FUNCTION:read_keywords ... unexpected end of file while reading list keywords")
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
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      STR(len=*), IN :: keyword
      STR(len=STR_SIZE) :: word
      word = keyword
      call to_lower_case_(word)
      select case (word)
      case ("}               ")  ! exit case
   ! case ("data_and_colour="); .read_data_and_colour
      case ("data_and_rgb=   "); call read_data_and_RGB_(self)
      case ("put             "); call put_(self)
      case ("units=          "); call read_units_(self)
      case default; allocate(tonto%known_keywords(4))
      tonto%known_keywords(1) = "}               "
      tonto%known_keywords(2) = "data_and_rgb=   "
      tonto%known_keywords(3) = "put             "
      tonto%known_keywords(4) = "units=          "
      call unknown_(tonto,word,"COLOUR_FUNCTION:process_keyword")
      deallocate(tonto%known_keywords)
      end select
   end subroutine

   subroutine read_units(self)
   ! Read a string which describes the units to be used
      type(COLOUR_FUNCTION_TYPE), INOUT :: self

      ! The following code is inherited from OBJECT
      if (FALSE) self = self
      call set_default_units_(stdin,next_str_(stdin))
   end subroutine

   subroutine read_data_and_RGB(self)
   ! Read in the data-colour pair. The colour must be an RGB real triple
   ! And the list of data and colour triples, a list of four real numbers, must
   ! be entered as a single vector.
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      INT :: length,i,k
      VEC(REAL,:), allocatable :: data_and_colour
      call read_all_(stdin,data_and_colour)
      ENSURE(mod(size(data_and_colour),4)==0,"COLOUR_FUNCTION:read_data_and_RGB ... data and colour array must be divisible by 4")
      length = size(data_and_colour)/4
      call destroy_(self%RGB)
      call destroy_(self%data)
      call create_(self%data,length)
      call create_(self%RGB,3,length)
      k = 0
      do i = 1,length
         self%data(i) = data_and_colour(k+1)
         self%RGB(1,i) = data_and_colour(k+2)
         self%RGB(2,i) = data_and_colour(k+3)
         self%RGB(3,i) = data_and_colour(k+4)
         k = k+4
      end do
      call destroy_(data_and_colour)
   end subroutine

   PURE subroutine update(self)
   ! Check everything is OK after reading data. Saturation must exist
   ! but it is applied into the RGB triples and then destroyed.
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(INT,:), allocatable :: order
   ENSURE(allocated(self%data),"COLOUR_FUNCTION:update ... no data values")
   ENSURE(allocated(self%RGB),"COLOUR_FUNCTION:update ... no RGB values")
   ENSURE(size(self%data)==size(self%RGB,2),"COLOUR_FUNCTION:update ... incompatible #: data and RGB")
   ENSURE(all(self%RGB>=ZERO),"COLOUR_FUNCTION:update ... negative RGB values exist")
   ENSURE(all(self%RGB<=ONE),"COLOUR_FUNCTION:update ... some RGB values greater than 1")
   ENSURE(all(column_norms_(self%RGB)<=THREE),"COLOUR_FUNCTION:update ... some RGB norms larger than 3")
      self%n_data = size(self%data)

      ! Re-order from smallest to largest
      call create_(order,self%n_data)
      call quick_sort_(self%data,order)
      self%data = self%data(order)
      self%RGB = self%RGB(:,order)
      call destroy_(order)
      self%finalized = TRUE
   end subroutine


!  ============
!  Set routines
!  ============

   PURE subroutine set_data(self,data)
   ! Set the data. Make sure that .finalize is called after all set
   ! routines.
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(REAL,:), IN :: data
      self%data = data
   end subroutine

   PURE subroutine set_RGB(self,RGB)
   ! Set the RGB's. Make sure that .finalize is called after all set
   ! routines.
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      MAT(REAL,:,:), IN :: RGB
      self%RGB = RGB
   end subroutine


!  ================
!  Colours for data
!  ================

   PURE function RGB_for(self,value,truncate_to_range) result (res)
   ! Return the RGB triple corresponding to a certain "value".
      type(COLOUR_FUNCTION_TYPE), IN :: self
      REAL, IN :: value
      BIN, optional, IN :: truncate_to_range
      VEC(REAL,3) :: res
      VEC(REAL,3) :: colour
      REAL :: frac,thevalue
      INT :: i1,i2
      BIN :: truncate
   ENSURE(self%finalized,"COLOUR_FUNCTION:RGB_for ... not finalized")
      thevalue = value
      truncate = TRUE
      if (present(truncate_to_range)) truncate = truncate_to_range
      if (NOT truncate) then
         ENSURE(is_in_range_(thevalue,element_range_(self%data)),"COLOUR_FUNCTION:RGB_for ... value out of range")
      else
         thevalue = min(thevalue,maxval(self%data))
         thevalue = max(thevalue,minval(self%data))
      end if
      i1 = count(self%data<=thevalue)  ! assuming .data is ordered !
      i1 = min(i1,self%n_data-1)
      i2 = i1+1
      frac = (thevalue-self%data(i1))/(self%data(i2)-self%data(i1))
      colour = self%RGB(:,i2)-self%RGB(:,i1)
      res = self%RGB(:,i1)+frac*colour
   end function

   PURE function RGB255_for(self,value) result (res)
   ! Return the RGB255 triple corresponding to a certain "value".
      type(COLOUR_FUNCTION_TYPE), IN :: self
      REAL, IN :: value
      VEC(INT,3) :: res
      VEC(REAL,3) :: colour
      REAL :: frac
      INT :: i1,i2
   ENSURE(self%finalized,"COLOUR_FUNCTION:RGB255_for ... not finalized")
   ENSURE(is_in_range_(value,element_range_(self%data)),"COLOUR_FUNCTION:RGB255_for ... value out of range")
      i1 = count(self%data<=value)  ! assuming .data is ordered !
      i1 = min(i1,self%n_data-1)
      i2 = i1+1
      frac = (value-self%data(i1))/(self%data(i2)-self%data(i1))
      colour = self%RGB(:,i2)-self%RGB(:,i1)
      res = nint(255*(self%RGB(:,i1)+frac*colour))
   end function

   PURE subroutine get_RGB_for_0(self,values,RGB)
   ! Return the "RGB" triples corresponding to a set of "values".
      type(COLOUR_FUNCTION_TYPE), IN :: self
      VEC(REAL,:), IN :: values
      MAT(REAL,:,:), OUT :: RGB
      INT :: i
   ENSURE(self%finalized,"COLOUR_FUNCTION:get_RGB_for_0 ... not finalized")
   ENSURE(size(RGB,1)==3,"COLOUR_FUNCTION:get_RGB_for_0 ... first dimension of RGB array must be 3")
   ENSURE(size(RGB,2)==size(values),"COLOUR_FUNCTION:get_RGB_for_0 ... values and RGB are incompatible")
      do i = 1,size(values)
         RGB(:,i) = RGB_for_(self,values(i))
      end do
   end subroutine

   PURE subroutine get_RGB_for_1(self,values,RGB)
   ! Return the "RGB" triples corresponding to a set of "values".
      type(COLOUR_FUNCTION_TYPE), IN :: self
      VEC(REAL,:), IN :: values
      MAT(INT,:,:), OUT :: RGB
      INT :: i
   ENSURE(self%finalized,"COLOUR_FUNCTION:get_RGB_for_1 ... not finalized")
   ENSURE(size(RGB,1)==3,"COLOUR_FUNCTION:get_RGB_for_1 ... first dimension of RGB array must be 3")
   ENSURE(size(RGB,2)==size(values),"COLOUR_FUNCTION:get_RGB_for_1 ... values and RGB are incompatible")
      do i = 1,size(values)
         RGB(:,i) = nint(RGB_for_(self,values(i)))
      end do
   end subroutine

   PURE subroutine get_RGB255_for(self,values,RGB255)
   ! Return the "RGB255" triples corresponding to a set of "values".
      type(COLOUR_FUNCTION_TYPE), IN :: self
      VEC(REAL,:), IN :: values
      MAT(INT,:,:), OUT :: RGB255
      INT :: i
   ENSURE(self%finalized,"COLOUR_FUNCTION:get_RGB255_for ... not finalized")
   ENSURE(size(RGB255,2)==size(values),"COLOUR_FUNCTION:get_RGB255_for ... values and RGB255 are incompatible")
      do i = 1,size(values)
         RGB255(:,i) = RGB255_for_(self,values(i))
      end do
   end subroutine

   PURE subroutine rescale_data(self,range)
   ! Rescale the .data so that the lowest value corresponds to
   ! range(1), and the largest value corresponds to range(2).
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(REAL,2), IN :: range
      REAL :: data1,del,frac
      INT :: i
   ENSURE(allocated(self%data),"COLOUR_FUNCTION:rescale_data ... no data")
   ENSURE(self%n_data>1,"COLOUR_FUNCTION:rescale_data ... not enough data")
      data1 = self%data(1)
      del = self%data(self%n_data)-data1
      do i = 1,self%n_data
         frac = (self%data(i)-data1)/del
         self%data(i) = range(1)+frac*(range(2)-range(1))
      end do
   end subroutine

   PURE subroutine rescale_RGB(self,range)
   ! Rescale the .RGB so that the lowest value corresponds to
   ! range(1), and the largest value corresponds to range(2).
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(REAL,2), IN :: range
      VEC(REAL,3) :: minv,maxv,del,frac
      INT :: i
   ENSURE(allocated(self%RGB),"COLOUR_FUNCTION:rescale_RGB ... no data")
   ENSURE(self%n_data>1,"COLOUR_FUNCTION:rescale_RGB ... not enough data")
      minv = [minval(self%RGB(:,1)),minval(self%RGB(:,2)),minval(self%RGB(:,3))]
      maxv = [maxval(self%RGB(:,1)),maxval(self%RGB(:,2)),maxval(self%RGB(:,3))]
      del = maxv-minv
      do i = 1,self%n_data
         frac = (self%RGB(i,:)-minv)/del
         self%RGB(i,:) = range(1)+frac*(range(2)-range(1))
      end do
   end subroutine

   PURE subroutine set_default_colours(self,values)
   ! Set up default colours for a set of "values".
      type(COLOUR_FUNCTION_TYPE), INOUT :: self
      VEC(REAL,2), IN :: values
      call rescale_data_(self,element_range_(values))
   end subroutine


!  ==============
!  Output methods
!  ==============

   subroutine put(self)
   ! Put the list of interpolating colours.
      type(COLOUR_FUNCTION_TYPE), IN :: self
      INT :: i
      call flush_(stdout)
      call text_(stdout,"Colour function data")
      call flush_(stdout)
      call show_(stdout,"No. of interpolating data points =",self%n_data)
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      call put_(stdout,"#",int_width=TRUE)
      call put_(stdout,"Data")
      call put_(stdout,"Red")
      call put_(stdout,"Green")
      call put_(stdout,"Blue")
      call flush_(stdout)
      call dash_(stdout,int_fields=1,real_fields=4)
      do i = 1,self%n_data
         call put_(stdout,i)
         call put_(stdout,self%data(i))
         call put_(stdout,self%RGB(1,i))
         call put_(stdout,self%RGB(2,i))
         call put_(stdout,self%RGB(3,i))
         call flush_(stdout)
      end do
      call dash_(stdout,int_fields=1,real_fields=4)
   end subroutine

end module
