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
module C_driver
 !
 implicit none
 !
 integer      :: code_version(3)
 integer      :: code_GPL_revision
 integer      :: code_revision
 character(50):: code_bin
 character(50):: code_tool
 character(50):: code_project
 character(50):: code_hash
 character(50):: code_libraries
 character(50):: code_branch
 !
 interface  
   !
   subroutine get_version(version,subversion,patchlevel,revision,hash)
     integer     , intent(out):: version
     integer     , intent(out):: subversion
     integer     , intent(out):: patchlevel
     integer     , intent(out):: revision
     character(*), intent(out):: hash
   end subroutine
   !
   subroutine get_libraries(libraries)
     character(*), intent(out):: libraries
   end subroutine
   !
   subroutine get_running_project(code_project)
     character(*), intent(out):: code_project
   end subroutine
   !
   subroutine get_running_tool(code_tool)
     character(*), intent(out):: code_tool
   end subroutine
   !
   subroutine get_runlevel(component,component_id,runlevel_id)
     character(*), intent(out):: component
     integer,      intent(in) :: component_id
     integer,      intent(in) :: runlevel_id
   end subroutine
   !
 end interface
 !
end module C_driver
