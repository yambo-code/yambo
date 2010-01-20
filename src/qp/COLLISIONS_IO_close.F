!
! Copyright (C) 2000-2009 C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine COLLISIONS_IO_close()
 !
 use collisions_IO,    ONLY:io_COLLISIONS,QP_COLLISIONS_done,TEMP_COLLISIONS_v
 use IO_m,             ONLY:io_control,OP_WR_CL,REP,NONE
 use memory_m,         ONLY:mem_est
 !
 implicit none
 !
 ! Work Space
 !
 integer                  :: ID,COM_,io_err      
 !
 COM_=REP
 if (.not.all(QP_COLLISIONS_done)) COM_=NONE
 call io_control(ACTION=OP_WR_CL,COM=COM_,SEC=(/1/),ID=ID)
 io_err=io_COLLISIONS(0,ID)
 !
 deallocate(QP_COLLISIONS_done)
 call mem_est("QP_COLLISIONS_done")
 !
 deallocate(TEMP_COLLISIONS_v)
 call mem_est("TEMP_COLLISIONS_v")
 !
end subroutine COLLISIONS_IO_close
