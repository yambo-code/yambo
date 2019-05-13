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
subroutine INIT_C_driver_runlevels(defs)
 !
 use it_m,        ONLY:it,initdefs
 use pars,        ONLY:lchlen
 use C_driver,    ONLY:get_runlevel,code_bin,code_tool
 !
 implicit none
 !
 type(initdefs) :: defs
 !
 ! Work Space
 !
 integer           :: i_c,N
 character(lchlen) :: string,desc,bin
 !
 do i_c=0,199
   !
   string=" "
   desc=" "
   bin=" "
   !
   call get_runlevel(bin,3,i_c)
   !
   if (trim(bin)=="EMPTY") cycle
   if (index(trim(bin),trim(code_bin))==0.and.index(trim(bin),trim(code_tool))==0) cycle
   !
   call get_runlevel(string,1,i_c)
   call get_runlevel(desc,2,i_c)
   !
   call it('r',defs,trim(string),'[R] '//trim(desc))
   !
 enddo
 !
end subroutine
