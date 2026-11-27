!=====================================================================
!
! OUTPUT_STYLE 
! 
! Style parameters used for writing TEXTFILE output.
! This is a very basic module.
!
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
module OUTPUT_STYLE_MODULE

#  include "macros"

   include "output_style.use"

   implicit none

   include "output_style.int"


contains

!  ===
!  Set
!  ===

   pure subroutine set_from(self,saved)
   ! Set style options from a "saved", including any default units.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      type(OUTPUT_STYLE_TYPE), IN :: saved

      ! Format options
      self%using_fields = saved%using_fields
      self%spacing = saved%spacing
      self%n_fields_per_line = saved%n_fields_per_line
      self%int_width = saved%int_width
      self%real_width = saved%real_width
      self%real_precision = saved%real_precision
      self%real_fortran_fmt = saved%real_fortran_fmt
      self%margin_width = saved%margin_width
      self%using_array_labels = saved%using_array_labels
      self%default_units = saved%default_units

      ! Buffer parsing style ...
      self%comment_chars = saved%comment_chars
      self%quote_chars = saved%quote_chars
      self%list_delimiters = saved%list_delimiters
      self%ignore_unmatched_quotes = saved%ignore_unmatched_quotes
      self%eliminate_specials = saved%eliminate_specials
      self%replacement_list = saved%replacement_list
   end subroutine


!  ==================
!  Set format options
!  ==================

   PURE subroutine set_using_fields(self,val)
   ! Set whether to use fields of fixed width or not.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      BIN, IN :: val
      self%using_fields = val
      call set_n_fields_per_line_(self,0)
   end subroutine

   PURE subroutine set_spacing(self,val)
   ! Set the spacing between fields.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"OUTPUT_STYLE:set_spacing ... must be non-negative")
      self%spacing = val
   end subroutine

   PURE subroutine set_n_fields_per_line(self,val)
   ! Set the number of fields per file.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"OUTPUT_STYLE:set_n_fields_per_line ... not enough fields")
      self%n_fields_per_line = val
   end subroutine

   PURE subroutine set_int_width(self,val)
   ! Set the width of an integer field.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"OUTPUT_STYLE:set_int_width ... width less than zero")
      self%int_width = val
   end subroutine

   PURE subroutine set_real_width(self,val)
   ! Set the width of an real-number field.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"OUTPUT_STYLE:set_real_width ... width less than zero")
   ENSURE(val>=self%real_precision,"OUTPUT_STYLE:set_real_width ... width smaller than precision")
      self%real_width = val
   end subroutine

   PURE subroutine set_real_precision(self,val)
   ! Set the real-number precision i.e. no of decimal places.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"OUTPUT_STYLE:set_real_precision ... precision less than zero")
   ENSURE(val<=self%real_width,"OUTPUT_STYLE:set_real_precision ... precision greater than field width")
      self%real_precision = val
   end subroutine

   PURE subroutine set_real_fortran_fmt(self,val)
   ! Set the real-number fortran format string.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      STR(len=*), IN :: val
   ENSURE(is_one_of_(val,["f ","d ","e ","es","en","g "]),"OUTPUT_STYLE:set_real_fortran_fmt ... unrecognized fortran style: "//trim(val))
      self%real_fortran_fmt = val
   end subroutine

   PURE subroutine set_line_fortran_fmt(self,fmt)
   ! Set the line format from fortran format string "fmt". 
   ! It should include the number of fields e.g. "5f10.3". 
   ! The style must be an allowed fortran real style like "f".
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      STR(len=*), IN :: fmt
      INT :: f,l,s
      STR(len=STR_SIZE) :: tmp

      ! First and last character
      tmp = adjustl(fmt)
      call get_next_item_position_(tmp,f,l)
      ENSURE(l/=0,"OUTPUT_STYLE:set_line_fortran_fmt ... fmt is blank")
      tmp = tmp(1:l)

      ! Get the number of columns
      s = index_of_character_not_in_(tmp,"0123456789")
      ENSURE(s/=0,"OUTPUT_STYLE:set_line_fortran_fmt ... fmt has no # of fields")
      call set_n_fields_per_line_(self,to_int_(tmp(1:s-1)))

      ! Get the style
      tmp = tmp(s:)
      s = index_of_character_in_(tmp,"0123456789")
      ENSURE(s/=0,"OUTPUT_STYLE:set_line_fortran_fmt ... fmt has no real style")
      call set_real_fortran_fmt_(self,tmp(1:s-1))

      ! Get the width
      tmp = tmp(s:)
      s = index_of_character_not_in_(tmp,"0123456789")
      ENSURE(s/=0,"OUTPUT_STYLE:set_line_fortran_fmt ... fmt has no width")
      ENSURE(tmp(s:s)==".","OUTPUT_STYLE:set_line_fortran_fmt ... fmt has no dot before precision")
      call set_real_precision_(self,0)
      call set_real_width_(self,to_int_(tmp(1:s-1)))

      ! Get the precision
      tmp = tmp(s+1:)
      call set_real_precision_(self,to_int_(tmp))
   end subroutine

   PURE subroutine set_margin_width(self,val)
   ! Set the width of the margin in the buffer.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      INT, IN :: val
   ENSURE(val>=0,"OUTPUT_STYLE:set_margin_width ... margin width less than zero")
   ENSURE(val<=STR_SIZE,"OUTPUT_STYLE:set_margin_width ... margin width too large")
      self%margin_width = val
   end subroutine

   pure subroutine set_using_array_labels(self,val)
   ! Set whether to use numbered columns, or row labels, on matrix or
   ! vector output.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      BIN, IN :: val
      self%using_array_labels = val
   end subroutine

   PURE subroutine set_default_units(self,units)
   ! Set the .default_units to "units". NOTE: This is reset back to
   ! ONE after a numerical quantity has been read and converted.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      STR(len=*), IN :: units
   ENSURE(units==" " OR is_known_unit_(units),"OUTPUT_STYLE:set_default_units ... Unknown units!")
      self%default_units = units
   end subroutine


!  ==========================
!  Set buffer parsing options
!  ==========================

   pure subroutine set_comment_chars(self,val)
   ! Set the characters which represent comment characters.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%comment_chars = val
   end subroutine

   pure subroutine set_quote_chars(self,val)
   ! Set the characters which represent the quotes.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      STR(len=*), IN :: val
      self%quote_chars = val
   end subroutine

   pure subroutine set_list_delimiters(self,val)
   ! Set the two open- and close- characters which delimit a list.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      STR(len=2), IN :: val
      self%list_delimiters = val(1:2)
   end subroutine

   pure subroutine set_ignore_unmatched_quotes(self,val)
   ! Set whether to ignore unmatched quotes when processing input.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      BIN, IN :: val
      self%ignore_unmatched_quotes = val
   end subroutine

   pure subroutine set_eliminate_specials(self,val)
   ! Set the list of characters which should be treated as whitespace.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      BIN, IN :: val
      self%eliminate_specials = val
   end subroutine

   PURE subroutine set_replacement_list(self,list)
   ! Set the characters which should be replaced.
   ! The first column in "list" is the character to be replaced, and
   ! the second column is the character it should be replaced with.
      type(OUTPUT_STYLE_TYPE), INOUT :: self
      MAT(STR(len=*),:,:), IN :: list
   ENSURE(size(list,2)==2,"OUTPUT_STYLE:set_replacement_list ... second dimension must be 2")
      self%replacement_list = list
   end subroutine

end module
