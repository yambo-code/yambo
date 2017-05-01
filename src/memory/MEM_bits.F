!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
integer function MEM_LIB_finder( name )
 !
 use memory, ONLY:N_MEM_LIBs,MEM_LIBs
 implicit none
 integer      :: i_l
 character(*) :: name
 MEM_LIB_finder=0
 do i_l=1,N_MEM_LIBs
   if ( index(name,trim(MEM_LIBs(i_l)%name)) > 0 ) then
     MEM_lib_finder=i_l
     exit 
   endif
 enddo 
 !
end function
!
integer function MEM_shelf_finder( shelf )
 !
 use memory, ONLY:N_MEM_shelf,MEM_shelf
 implicit none
 integer      :: i_s
 character(*) :: shelf
 MEM_shelf_finder=0
 do i_s=1,N_MEM_shelf
   if ( trim(MEM_shelf(i_s))==shelf ) then
     MEM_shelf_finder=i_s
     exit 
   endif
 enddo 
 !
end function
!
subroutine MEM_element_finder( shelf, name, Sz, i_FOUND, i_FOUND_element, i_FOUND_SAVE )
 !
 use pars,   ONLY:schlen
 use memory, ONLY:N_MEM_elements,MEMs,N_MEM_SAVE_elements,LARGE_MEMs
 implicit none
 integer      :: i_m,i_l,i_FOUND,i_FOUND_SAVE,i_FOUND_element,Sz
 character(*) :: shelf,name
 character(schlen) :: name_pattern,el_pattern
 i_FOUND=0
 i_FOUND_ELEMENT=0
 !
 name_pattern=name
 if (index(name,"%")>0) then
   name_pattern=name(index(name,"%")+1:len(name))
 endif
 !
 LOOP_1: do i_m=1,N_MEM_elements
   if ( trim(MEMs(i_m)%shelf)==shelf ) then
     i_FOUND=i_m
     do i_l=1,MEMs(i_m)%N
       if (index(name,"%")>0) then
         el_pattern=trim(MEMs(i_m)%name(i_l))
         el_pattern=el_pattern(index(el_pattern,"%")+1:len(el_pattern))
       endif
       if (trim(el_pattern)==trim(name_pattern).and.MEMs(i_m)%size(i_l)==Sz) i_FOUND_element=i_l
     enddo
     if (i_FOUND_element==0) then
       do i_l=1,MEMs(i_m)%N
         if (MEMs(i_m)%size(i_l)==Sz) i_FOUND_element=i_l
       enddo
     endif
     exit LOOP_1
   endif
 enddo LOOP_1
 i_FOUND_SAVE=0
 LOOP_2: do i_m=1,N_MEM_SAVE_elements
   if ( trim(LARGE_MEMs(i_m)%shelf)==shelf ) then
     i_FOUND_SAVE=i_m
     exit LOOP_2
   endif
 enddo LOOP_2
 !
end subroutine
!
subroutine MEM_STAT_update
 use memory, ONLY:TOT_MEM_kilobytes_MEMSTAT
 implicit none
 ! Process Memory Update
 !=======================
 call memstat( TOT_MEM_kilobytes_MEMSTAT )
 if (TOT_MEM_kilobytes_MEMSTAT<0) then
   TOT_MEM_kilobytes_MEMSTAT=0
   return
 endif
end subroutine
