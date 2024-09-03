!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine C_driver_transfer()
 !
 use C_driver,       ONLY:code_version,code_revision,code_hash,&
&                         code_libraries,get_version,get_libraries,get_running_tool,&
&                         get_running_project,code_bin,code_tool,code_project
 !
 implicit none
 !
 code_tool=" "
 call get_running_tool(code_tool)
 code_project=" "
 call get_running_project(code_project)
 code_bin=trim(code_tool)
 if (len_trim(code_project)>0) code_bin=trim(code_tool)//"_"//trim(code_project)
 code_libraries=" "
 call get_libraries(code_libraries)
 code_hash=" "
 call get_version(code_version(1),code_version(2),code_version(3),code_revision,code_hash)
 !
end subroutine
