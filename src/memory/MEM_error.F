!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine MEM_error(what)
 !
 use memory,       ONLY:MEM_err,MEM_msg
 use LIVE_t,       ONLY:LIVE_message
 use stderr,       ONLY:intc
 !
 implicit none
 !
 character(*), intent(in) :: what
 !
 ! Error Message
 !===============
 if (MEM_err/=0) then
   call LIVE_message("[ERROR] Allocation of "//what//" failed with code "//trim(intc(MEM_err)))
   call error(trim(MEM_msg))
 endif
 !
end subroutine MEM_error
