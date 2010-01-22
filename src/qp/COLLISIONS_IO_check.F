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
integer function COLLISIONS_IO_check(write_intent)
 use pars,             ONLY:IP,SP,schlen
 use collisions_IO,    ONLY:io_COLLISIONS,QP_COLLISIONS_done,TEMP_COLLISIONS_v
 use IO_m,             ONLY:io_control,OP_RD_CL,VERIFY,REP
 use QP_m,             ONLY:QP_n_states,QP_nk,QP_nb
 use com,              ONLY:msg
 use memory_m,         ONLY:mem_est
 !
 implicit none
 !
 logical :: write_intent
 !
 ! Work Space
 !
 integer                  :: ID
 real(SP)                 :: disk_space
 !
 allocate(QP_COLLISIONS_done(QP_n_states))
 call mem_est("QP_COLLISIONS_done",(/size(QP_COLLISIONS_done)/),(/IP/))
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 COLLISIONS_IO_check=io_COLLISIONS(0,ID,QP_COLLISIONS_done=QP_COLLISIONS_done) 
 !
 if(COLLISIONS_IO_check==0.and.all(QP_COLLISIONS_done)) then
   deallocate(QP_COLLISIONS_done)
   return
 else
   COLLISIONS_IO_check=-1
 endif
 !
 if (.not.write_intent) return
 !
 disk_space=real(QP_n_states*QP_nb**2*QP_nk*(2+0.25)*SP)/1.0e9
 if (disk_space> 0.01) call msg('s','[COLLISIONS I/O] Max Disk space required [Gb]:',disk_space)  
 !
 allocate(TEMP_COLLISIONS_v(QP_nb,QP_nb,QP_nk))
 call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
 !
end function COLLISIONS_IO_check
