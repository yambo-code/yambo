!
!        Copyright (C) 2000-2019 the YAMBO team
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
   if ( name==trim(MEM_LIBs(i_l)%name)) then
     MEM_lib_finder=i_l
     exit 
   endif
 enddo 
 !
end function
!
integer function MEM_shelf_finder( shelf )
 !
 use memory, ONLY:N_MEM_max,MEMs
 implicit none
 integer      :: i_m
 character(*) :: shelf
 MEM_shelf_finder=0
 do i_m=1,N_MEM_max
   if ( trim(MEMs(i_m)%shelf)==shelf ) then
     MEM_shelf_finder=i_m
     exit 
   endif
 enddo 
 !
end function
!
integer function MEM_save_finder( shelf )
 !
 use memory, ONLY:N_MEM_SAVE_elements,LARGE_MEMs
 implicit none
 integer      :: i_m
 character(*) :: shelf
 MEM_save_finder=0
 do i_m=1,N_MEM_SAVE_elements
   if ( trim(LARGE_MEMs(i_m)%shelf)==shelf ) then
     MEM_save_finder=i_m
     exit 
   endif
 enddo 
 !
end function
!
integer function MEM_component_finder( i_SHELF, name, Sz, desc, ADD )
 !
 use pars,   ONLY:schlen,IPL
 use memory, ONLY:MEMs
 implicit none
 integer      :: i_SHELF
 integer(IPL) :: Sz
 character(*) :: name,desc
 logical      :: ADD
 !ws
 integer      :: i_l
 logical      :: condition
 character(schlen) :: name_pattern,el_pattern
 !
 MEM_component_finder=0
 condition = .FALSE.
 !
 name_pattern=name
 if (index(name,"%")>0) then
   name_pattern=name(index(name,"%")+1:len(name))
 endif
 do i_l=1,MEMs(i_SHELF)%N
   el_pattern=trim(MEMs(i_SHELF)%name(i_l))
   if (index(name,"%")>0) then
     el_pattern=el_pattern(index(el_pattern,"%")+1:len(el_pattern))
   endif
   if (trim(el_pattern)==trim(name_pattern)) then
     !
     if (ADD) then
       !
       condition=trim(MEMs(i_SHELF)%desc(i_l))==desc
       !
     else
       !
       if (MEMs(i_SHELF)%composed(i_l)) then
         condition=MEMs(i_SHELF)%size(i_l)>=Sz
       else
         condition=MEMs(i_SHELF)%size(i_l)==Sz
       endif
       !
     endif
     !
     if (condition) then
       MEM_component_finder=i_l
       return
     endif
     !
   endif
 enddo
 !
end function
!
subroutine MEM_STAT_update
 use memory, ONLY:TOT_MEM_Kb_MEMSTAT
 implicit none
 ! Process Memory Update
 !=======================
 call memstat( TOT_MEM_Kb_MEMSTAT )
 if (TOT_MEM_Kb_MEMSTAT<0) then
   TOT_MEM_Kb_MEMSTAT=0
   return
 endif
end subroutine
