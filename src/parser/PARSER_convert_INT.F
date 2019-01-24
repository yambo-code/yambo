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
subroutine PARSER_convert_INT(ivar,v_descr,force_RL)
 !
 !use units,  ONLY:Gigabyte,Megabyte,Kilobyte
 !
 implicit none
 !
 integer     ::ivar
 character(*)::v_descr
 logical     ::force_RL
 !
 if (ivar<0) return
 !
 if (index(v_descr,'RL')/=0) then
   call PARSER_close_G(ivar,'tRL')
 else if (index(v_descr,'mHa')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tmHa')
 else if (index(v_descr,'Ha')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tHa')
 else if (index(v_descr,'mRy')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tmRy')
 else if (index(v_descr,'Ry')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'tRy')
 else if (index(v_descr,'eV')/=0) then
   if (force_RL     ) call PARSER_close_G(ivar,'tRL')
   if (.not.force_RL) call PARSER_close_G(ivar,'teV')
 !else if (index(v_descr,'Mb')/=0) then
 !  ivar=ivar*Kilobyte/Megabyte
 !else if (index(v_descr,'Gb')/=0) then
 !  ivar=ivar*Kilobyte/Gigabyte
 endif
 !
end subroutine
