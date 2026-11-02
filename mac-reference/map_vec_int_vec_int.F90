!---------------------------------------------------------------------------
!
! MAP{VEC{INT},VEC{INT}} : 
!
! A map table (or associative array) with VEC{INT} keys and VEC{INT} values.
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
!
!---------------------------------------------------------------------------
module MAP_VEC_INT_VEC_INT_MODULE

#  include "macros"

   include "map{vec{int},vec{int}}.use"

   implicit none

   include "map{vec{int},vec{int}}.int"


contains

!  ==========
!  Allocation
!  ==========

   PURE subroutine create_0(self)
   ! Allocate an object
      type(MAP_VEC_INT_VEC_INT_TYPE), allocatable, OUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

#ifdef USE_PRECONDITIONS

      ! The following code is inherited from OBJECT
      allocate(self,stat=status)
      ENSURE(status==0,"MAP{VEC{INT},VEC{INT}}:create_0 ... error in allocating memory")
#else
      allocate(self)
#endif
   end subroutine

   PURE subroutine create_1(self,n_size,keydim,valdim)
   ! Create the map with key array and value array with length
   ! "n_size".  The dimension of each key is "keydim". The dimension
   ! of each values is "valdim"
      type(MAP_VEC_INT_VEC_INT_TYPE), allocatable, OUT :: self
      INT, IN :: n_size,keydim,valdim

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      call create_(self)
      self%n_keys = 0
      self%n_size = n_size
      call create_(self%keys,keydim,n_size)
      call create_(self%values,valdim,n_size)
   end subroutine

   PURE subroutine destroy(self)
   ! Deallocate "self"
      type(MAP_VEC_INT_VEC_INT_TYPE), allocatable, INOUT :: self

#ifdef USE_PRECONDITIONS
      INT :: status
#endif

      ! The following code is inherited from OBJECT
      if (NOT allocated(self)) return

#ifdef USE_PRECONDITIONS
      deallocate(self,stat=status)
      ENSURE(status==0,"MAP{VEC{INT},VEC{INT}}:destroy ... error in deallocating memory")
#else
      deallocate(self)
#endif
   end subroutine

   PURE subroutine destroy_ptr_part(self)
   ! Destroy the pointer parts
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      call destroy_(self%keys)
      call destroy_(self%values)
   end subroutine


!  ===
!  Set
!  ===

   PURE subroutine set_defaults(self)
   ! Set defaults
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      self%n_keys = 0
      self%n_size = 0
      self%reverse_search = FALSE
   end subroutine

   PURE subroutine set_reverse_search(self,value)
   ! Set the .reverse_search switch to "value". This determines
   ! whether the search through the keys arrays occurs in reverse
   ! order, which may be useful if you know that the most recent keys
   ! added are more likely to contain the values you are looking for.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      BIN, IN :: value

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      self%reverse_search = value
   end subroutine


!  ======================
!  Resize, append, delete
!  ======================

   PURE subroutine shrink_0(self)
   ! Shrinks the size of the keys and values arrays to dimension
   ! ".n_keys", *only* when ".n_size" is bigger than the number of
   ! keys stored.  Contents are retained. This is used to save memory.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
   ENSURE(self%n_size>0,"MAP{VEC{INT},VEC{INT}}:shrink_0 ... size of map is 0")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      if (self%n_size<=self%n_keys) return
      call shrink_(self,self%n_keys)
   end subroutine

   PURE subroutine shrink_1(self,n_size)
   ! Shrinks the size of the keys and values arrays to dimension
   ! "n_size".  Contents are retained, where possible.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      INT, IN :: n_size
   ENSURE(self%n_size>0,"MAP{VEC{INT},VEC{INT}}:shrink_1 ... size of map is 0")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      self%n_keys = min(self%n_keys,n_size)  ! ???
      self%n_size = n_size  ! ???
      call shrink_columns_(self%keys,n_size)
      call shrink_columns_(self%values,n_size)
   end subroutine

   PURE subroutine expand(self,n_size)
   ! Expands the size of the keys and values arrays to dimension
   ! "n_size".  Contents are retained.  
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      INT, IN :: n_size
   ENSURE(n_size>self%n_size,"MAP{VEC{INT},VEC{INT}}:expand ... keys is already big enough")
   ENSURE(n_size>0,"MAP{VEC{INT},VEC{INT}}:expand ... keys and values not created")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      self%n_size = n_size
      call expand_columns_(self%keys,n_size)
      call expand_columns_(self%values,n_size)
   end subroutine

   PURE subroutine resize(self,n_size)
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      INT, IN :: n_size

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      self%n_size = n_size
      if (n_size>self%n_size) then
         call expand_(self,n_size)
      else if (n_size<self%n_size) then
         call shrink_(self,n_size)
      end if
   end subroutine

   PURE subroutine append_pair(self,key,value)
   ! Append the "key" and the corresponding "value" to the map table.
   ! If the .keys or .values arrays are too small, they are doubled in size.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      VEC(INT,:), IN :: key
      VEC(INT,:), IN :: value
      INT :: n

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      n = self%n_keys+1
      if (n<=0) return
      if (n>self%n_size) call expand_(self,2*n)
      self%n_keys = n
      self%keys(:,n) = key
      self%values(:,n) = value
   end subroutine

   PURE subroutine set_0(self,key,value)
   ! Append the "key" and the corresponding "value" to the map table.
   ! If the .keys or .values arrays are too small, they are doubled in
   ! size.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      VEC(INT,:), IN :: key
      VEC(INT,:), IN :: value

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      call append_pair_(self,key,value)
   end subroutine

   PURE subroutine append_pairs(self,keys,values)
   ! Append the "keys" and the corresponding "values" to the map
   ! table.  If the .keys or .values arrays are too small, they are
   ! doubled in size.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: keys
      MAT(INT,:,:), IN :: values
      INT :: new,f,l
   ENSURE(size(keys,2)==size(values),"MAP{VEC{INT},VEC{INT}}:append_pairs ... keys and values must have same length")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      new = size(keys,2)
      if (self%n_keys+new>self%n_size) call expand_(self,2*(self%n_keys+new))
      f = self%n_keys+1
      l = self%n_keys+new
      self%n_keys = l
      self%keys(:,f:l) = keys
      self%values(:,f:l) = values
   end subroutine

   PURE subroutine set_1(self,keys,values)
   ! Append the "keys" and the corresponding "values" to the map
   ! table.  If the .keys or .values arrays are too small, they are
   ! doubled in size.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      MAT(INT,:,:), IN :: keys
      MAT(INT,:,:), IN :: values
   ENSURE(size(keys,2)==size(values),"MAP{VEC{INT},VEC{INT}}:set_1 ... keys and values must have same length")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      call append_pairs_(self,keys,values)
   end subroutine

   PURE subroutine delete(self,key,has_key)
   ! Delete the "key" and the corresponding "value" from the map
   ! table.  A fatal error occurs if the "key" is not there, unless
   ! "has_key" is present.  If it is present, "has_key" is set to TRUE
   ! if element *was* there (after the delete operation it is not
   ! there!), otherwise it is set false.  There is no change made to
   ! the size of the map table.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      VEC(INT,:), IN :: key
      BIN, optional, OUT :: has_key
      INT :: i
   ENSURE(NOT (index_of_key_(self,key)<=0 AND NOT present(has_key)),"MAP{VEC{INT},VEC{INT}}:delete ... has_key must be present when key does not occur")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      i = index_of_key_(self,key)
      if (i>0) then
         if (present(has_key)) has_key = TRUE
         call delete_item_(self,i)
      else
         if (present(has_key)) has_key = FALSE
      end if
   end subroutine

   PURE subroutine delete_item(self,index)
   ! Delete element "index" from the map table.  No change is made to
   ! the size of the map table.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      INT, IN :: index
      INT :: i,n
   ENSURE(index>0,"MAP{VEC{INT},VEC{INT}}:delete_item ... index must be positive")
   ENSURE(index<=self%n_keys,"MAP{VEC{INT},VEC{INT}}:delete_item ... index must be less than number of keys")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      i = index
      n = self%n_keys-1
      self%keys(:,i:n) = self%keys(:,i+1:self%n_keys)
      self%values(:,i:n) = self%values(:,i+1:self%n_keys)
      self%n_keys = n
   end subroutine

   PURE subroutine delete_items(self,list)
   ! Delete elements "list" from the map table.  No change is made to
   ! the size of the map table.
      type(MAP_VEC_INT_VEC_INT_TYPE), INOUT :: self
      VEC(INT,:), IN :: list
      INT :: k,i0,i1,k0,k1,n
      VEC(INT,:), allocatable :: indx
   ENSURE(all(list>0),"MAP{VEC{INT},VEC{INT}}:delete_items ... list elements must be positive")
   ENSURE(all(list<=self%n_keys),"MAP{VEC{INT},VEC{INT}}:delete_items ... list elements must be less than number of keys")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      indx = list
      call quick_sort_(indx)
      i1 = indx(1)-1
      do k = 1,size(indx)-1
         k0 = indx(k)
         k1 = indx(k+1)
         n = k1-k0-1
         i0 = i1+1
         i1 = i1+n
         if (n<=0) cycle

         !     " <- "//trim((k0+1).to_str)//"-"//trim((k1-1).to_str))
         self%keys(:,i0:i1) = self%keys(:,k0+1:k1-1)
         self%values(:,i0:i1) = self%values(:,k0+1:k1-1)
      end do
      k0 = indx(size(indx))
      n = self%n_keys-k0
      i0 = i1+1
      i1 = i1+n

        !     " <- "//trim((k0+1).to_str)//"-"//trim(.n_keys.to_str))
      if (n>0) then
         self%keys(:,i0:i1) = self%keys(:,k0+1:)
         self%values(:,i0:i1) = self%values(:,k0+1:)
      end if
      self%n_keys = self%n_keys-size(indx)
   end subroutine


!  ===================
!  Search and retrieve
!  ===================

   PURE function index_of_key(self,key) result (res)
   ! Returns the index of any key which matches "key" in the map. If
   ! there is no match, then the result is 0.
      type(MAP_VEC_INT_VEC_INT_TYPE), IN :: self
      VEC(INT,:), IN :: key
      INT :: res
      INT :: i

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      res = 0
      if (self%reverse_search) then
         do i = self%n_keys,1,-1
            if (any(key/=self%keys(:,i))) cycle
            res = i
            exit
         end do
      else
         do i = 1,self%n_keys
            if (any(key/=self%keys(:,i))) cycle
            res = i
            exit
         end do
      end if
   end function

   function has_key(self,key,index) result (res)
   ! Returns TRUE if the map contains a specific "key". If present,
   ! "index" is set to the index of that key.
      type(MAP_VEC_INT_VEC_INT_TYPE), IN :: self
      VEC(INT,:), IN :: key
      INT, optional, OUT :: index
      BIN :: res
      INT :: i

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      i = index_of_key_(self,key)
      res = i>0
      if (present(index)) index = i
   end function

   function value_for_key(self,key,has_key,index) result (res)
   ! Returns the value corresponding to the map "key". If "has_key" is
   ! not present, an error is generated if no matching "key" can be
   ! found in the map; if it is present, it is set to TRUE if the the
   ! key is present, or FALSE otherwise. If "index" is present, then
   ! it is set to the value of the index of "key" in the map table.
      type(MAP_VEC_INT_VEC_INT_TYPE), IN :: self
      VEC(INT,:), IN :: key
      BIN, optional, OUT :: has_key
      INT, optional, OUT :: index
      VEC(INT,size(self%values,1)) :: res
      INT :: i
   ENSURE(NOT (index_of_key_(self,key)<=0 AND NOT present(has_key)),"MAP{VEC{INT},VEC{INT}}:value_for_key ... has_key must be present when key does not occur")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      i = index_of_key_(self,key)
      if (i>0) then
         if (present(has_key)) has_key = TRUE
         res = self%values(:,i)
      else
         if (present(has_key)) has_key = FALSE
      end if
      if (present(index)) index = i
   end function

   PURE function value_for_item(self,index) result (res)
   ! Returns the "index"-th element in the map table. 
      type(MAP_VEC_INT_VEC_INT_TYPE), IN :: self
      INT, IN :: index
      VEC(INT,size(self%values,1)) :: res
   ENSURE(index<=self%n_keys,"MAP{VEC{INT},VEC{INT}}:value_for_item ... index out of range")
   ENSURE(index>0,"MAP{VEC{INT},VEC{INT}}:value_for_item ... index out of range")

      ! The following code is inherited from MAP{VEC{KEY},VEC{VAL}}
      res = self%values(:,index)
   end function

end module
