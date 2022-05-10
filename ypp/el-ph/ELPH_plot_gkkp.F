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
subroutine ELPH_plot_gkkp(k,E,q)
 !
 use pars,                ONLY:SP,pi,schlen
 use electrons,           ONLY:levels,spin
 use R_lattice,           ONLY:bz_samp
 use ELPH,                ONLY:PH_freqs_sq,elph_use_q_grid,ph_modes,GKKP,elph_nb,elph_nQ,PH_qpt
 use com,                 ONLY:msg
 use QP_m,                ONLY:QP_table,QP_n_states
 use vec_operate,         ONLY:c2a
 use IO_int,              ONLY:io_control
 use YPPm,                ONLY:coo_out,K_transform
 use com,                 ONLY:msg,of_open_close
 use IO_m,                ONLY:OP_RD_CL,REP,deliver_IO_error_message,DUMP,manage_action,RD_CL_IF_END,OP_RD
 use stderr,              ONLY:intc
 use LIVE_t,              ONLY:live_timing
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work space
 !
 integer               :: ID,IO_ACT,io_err
 integer               :: iq,i_phm,i_qp,i_sp_pol,ik,ib,nq
 integer, external     :: io_ELPH
 !
 real(SP)  :: q_pt(3)
 real(SP), allocatable :: values(:)
 character(schlen), allocatable :: gkkp_fname(:),headings(:)
 !
 call section('+','Plot GKKP functions')
 !
 ! Read phonon-frequencies in the ELPH-DB
 ! ======================================
 call io_control(ACTION=OP_RD,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err=io_ELPH(ID,'gkkp')
 call deliver_IO_error_message(io_err,"GKKP")
 !
 if (io_err/=0) call error('El-Ph database not found')
 !
 YAMBO_ALLOC(values,(3+ph_modes))
 allocate(headings(3+ph_modes))
 allocate(gkkp_fname(QP_n_states))
 !
 headings(1:3)=(/"q_x ("//trim(coo_out)//")","q_y ("//trim(coo_out)//")","q_z ("//trim(coo_out)//")"/)
 !
 do i_phm=1,ph_modes
   headings(3+i_phm)=' m'//intc(i_phm)
 enddo
 !
 do i_qp=1,QP_n_states
   !
   ib    =QP_table(i_qp,1)
   ik    =QP_table(i_qp,3)
   i_sp_pol=spin(QP_table(i_qp,:))
   gkkp_fname(i_qp)=trim('gkkp_ik_'//trim(intc(ik))//'_ib_'//trim(intc(ib))//'_i_sp_'//trim(intc(i_sp_pol)))
   call of_open_close(gkkp_fname(i_qp),'ot')
   call msg('o '//trim(gkkp_fname(i_qp)),"#",headings,INDENT=0,USE_TABS=.true.)
   call msg('o '//trim(gkkp_fname(i_qp)),"#")
   !
 enddo
 !
 if(elph_use_q_grid) then
   nq=q%nibz
 else
   nq=elph_nQ
 endif
 !
 call live_timing('Writing GKKP factors',nq)
 !
 do iq=1,nq
   !
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,nq)
   call io_control(ACTION=IO_ACT,SEC=(/iq+1/),ID=ID)
   io_err=io_ELPH(ID,'gkkp')
   if (io_err<0) call error('Missing Q-database')
   !
   q_pt=PH_qpt(iq,:)
   call K_transform(q_pt,'iku')
   !
   values(1:3)=q_pt(1:3)
   !
   do i_qp=1,QP_n_states
     !
     ib    =QP_table(i_qp,1)
     ik    =QP_table(i_qp,3)
     i_sp_pol=spin(QP_table(i_qp,:))
     !
     values(4:)=abs(GKKP%dVc(:,ib,ib,ik,1))**2
     !
     call msg('o '//gkkp_fname(i_qp),'  ',values,INDENT=0,USE_TABS=.TRUE.)
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 do i_qp=1,QP_n_states
   call of_open_close(gkkp_fname(i_qp))
 enddo
 !
 YAMBO_FREE(values)
 deallocate(headings)
 deallocate(gkkp_fname)
 !
end subroutine
