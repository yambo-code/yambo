!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine RT_write_descriptions(file_name)
 !
 use RT_control,     ONLY:RT_description,n_RT_descriptions
 use com,            ONLY:msg
 !
 implicit none
 !
 character(*), intent(in)      :: file_name
 !                          
 ! Work Space 
 !
 integer  :: i_fp
 !
 do i_fp=1,n_RT_descriptions
   call msg('o '//trim(file_name),'#',trim(RT_description(i_fp)),INDENT=0)
 enddo
 !
end subroutine RT_write_descriptions

