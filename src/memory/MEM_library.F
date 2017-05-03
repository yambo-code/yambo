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
subroutine MEM_library(member,shelf)
 !
 use memory,  ONLY:N_MEM_shelf,MEM_shelf,N_MEM_LIBs,MEM_LIBs
 !
 implicit none
 !
 character(*) :: member
 character(*) :: shelf
 ! 
 ! Work Space
 !
 integer           :: i_SHELF,i_LIB,i_s
 integer, external :: MEM_LIB_finder
 !
 i_SHELF=0
 do i_s=1,N_MEM_shelf
   if ( trim(MEM_shelf(i_s))==shelf ) then
     i_SHELF=i_s
     exit 
   endif
 enddo 
 i_LIB  =MEM_LIB_finder(member)
 !
 if (i_LIB==0) then
   !
   N_MEM_LIBs=N_MEM_LIBs+1
   !
   if (i_SHELF==0) then
     N_MEM_shelf=N_MEM_shelf+1
     MEM_shelf(N_MEM_shelf)="["//shelf//"]"
     i_SHELF=N_MEM_shelf
   endif
   !
   MEM_LIBs(N_MEM_LIBs)%name =member
   MEM_LIBs(N_MEM_LIBs)%shelf=i_SHELF
   !
 endif
 !
end subroutine
