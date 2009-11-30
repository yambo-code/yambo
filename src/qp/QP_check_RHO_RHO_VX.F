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
integer function QP_check_RHO_RHO_VX()
 use pars,             ONLY:IP,SP,schlen
 use oscillators_IO,   ONLY:ioRHO_RHO_VX,QP_DB_done,tmp_rho_vx
 use IO_m,             ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP,OP_WR,NONE
 use QP_m,             ONLY:QP_n_states,QP_nk,QP_nb
 use com,              ONLY:msg
 use memory_m,         ONLY:mem_est
 !
 implicit none
 !
 ! Work Space
 !
 integer                  :: ID
 character(schlen)        :: dump_ch
 !
 allocate(QP_DB_done(QP_n_states))
 call mem_est("QP_DB_done",(/size(QP_DB_done)/),(/IP/))
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 QP_check_RHO_RHO_VX=ioRHO_RHO_VX(0,ID,QP_DB_done=QP_DB_done) 
 !
 if(QP_check_RHO_RHO_VX==0.and.all(QP_DB_done)) then
   deallocate(QP_DB_done)
   return
 else
  QP_check_RHO_RHO_VX=-1
 endif 
 !
 write (dump_ch,'(a,f10.5)') &
    & '[RHO] Max Disk space required [Gb]:',real(QP_n_states*QP_nb**2*QP_nk*(2+0.25)*SP)/1.0e9
 call msg('s',dump_ch)  
 !
 allocate(tmp_rho_vx(QP_nb,QP_nb,QP_nk))
 call mem_est("tmp_rho_vx",(/size(tmp_rho_vx)/),(/2*SP/))
 !
end function QP_check_RHO_RHO_VX

subroutine QP_close_RHO_RHO_VX()
 use oscillators_IO,   ONLY:ioRHO_RHO_VX,QP_DB_done,tmp_rho_vx
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
 if (.not.all(QP_DB_done)) COM_=NONE
 call io_control(ACTION=OP_WR_CL,COM=COM_,SEC=(/1/),ID=ID)
 io_err=ioRHO_RHO_VX(0,ID)
 !
 deallocate(QP_DB_done)
 call mem_est("QP_DB_done")
 !
 deallocate(tmp_rho_vx)
 call mem_est("tmp_rho_vx")
 !
end subroutine QP_close_RHO_RHO_VX
