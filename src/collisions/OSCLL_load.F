!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine OSCLL_load(q,k) 
 !
 ! Evaluate oscillators for the Local-SEX
 !
 use LIVE_t,              ONLY:live_timing
 use R_lattice,           ONLY:bz_samp
 use QP_m,                ONLY:QP_nk
 use parallel_m,          ONLY:PAR_IND_Xk_ibz,PAR_IND_Xk_ibz_ID,myid,PAR_COM_Xk_ibz_INDEX,master_cpu
 use parallel_int,        ONLY:PP_wait
 use IO_m,                ONLY:OP_RD,RD_CL_IF_END,VERIFY,REP,manage_action,deliver_IO_error_message
 use IO_int,              ONLY:io_control,IO_and_Messaging_switch
 use com,                 ONLY:msg
 !
#include<memory.h>
 !
 type(bz_samp), intent(in) :: q,k
 !
 ! Work Space
 !
 integer :: i_k,i_sp_pol
 integer :: ID,io_err,IO_ACT
 integer, external :: io_OSCLL
 !
 call PP_wait(COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
 !
 i_sp_pol=1
 !
 call IO_and_Messaging_switch("SAVE")
 call IO_and_Messaging_switch("+io_in",CONDITION=.TRUE.)
 !   
 call msg('s','[OSC] Checking oscillators header ')
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
 io_err=io_OSCLL(q,ID)
 !
 if(io_err/=0) &
&  call error('Oscillators not present, evaluate them before non-linear reponse: "yambo -e -v lsex" ')
 ! 
 ! Load Oscillators
 !
 call live_timing('[OSC] Load Oscillators :',PAR_IND_Xk_ibz%n_of_elements(PAR_IND_Xk_ibz_ID+1))
 !
 do i_k=1,QP_nk
   if (.not.PAR_IND_Xk_ibz%element_1D(i_k)) cycle
   IO_ACT=manage_action(RD_CL_IF_END,i_k,1,(i_sp_pol)*k%nibz)
   call io_control(ACTION=IO_ACT,COM=REP,SEC=(/1+i_k+(i_sp_pol-1)*k%nibz/),ID=ID)
   io_err=io_OSCLL(q,ID)
   call deliver_IO_error_message(io_err,'Oscillators',STOP_it=.TRUE.)
   call live_timing(steps=1)
 enddo
 call live_timing()
 !
 call PP_wait(COMM=PAR_COM_Xk_ibz_INDEX%COMM) 
 call IO_and_Messaging_switch("RESTORE")
 !
 call msg('rs','[OSC] Oscillators loaded')
 !
end subroutine OSCLL_load
