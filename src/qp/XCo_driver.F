!
! Copyright (C) 2000-2016 A. Marini and the YAMBO team 
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
subroutine XCo_driver(E,k,Xk,q)
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_acfdt,l_sc_run,l_real_time,l_gw0
 use pars,          ONLY:schlen
 use com,           ONLY:msg,of_open_close
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_sp_pol,spin
 use stderr,        ONLY:intc,real2ch
 use R_lattice,     ONLY:bz_samp
 use parallel_m,    ONLY:PP_redux_wait
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:QP_table,QP_Vxc,QP_Vnl_xc,QP_n_states,Vxc_kind,Vnlxc_kind 
 use memory_m,      ONLY:mem_est
 use wave_func,     ONLY:WF_free
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! WorkSpace
 !
 ! Reporting
 !
 integer           ::i_qp,ib,ik,ibp,i_spin
 character(schlen) ::sec_mode_and_message(2),ch,ch_spin,nloc_fmt,loc_fmt
 !
 ! IO
 !
 integer           :: ID,io_err
 integer, external :: io_HF_and_locXC
 real(SP)          :: output(4)
 character(schlen) :: of_name(n_sp_pol)
 !
 ! Section
 !
 sec_mode_and_message(1)='*'
 sec_mode_and_message(2)='Bare local and non-local Exchange-Correlation'
 if (l_acfdt)   sec_mode_and_message(1)='p'
 if (.not.l_sc_run &
&   ) call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
 !
 ! QP states setup
 !
 call QP_state_table_setup(E)
 !
 ! Allocation
 !
 if (.not.allocated(QP_Vnl_xc)) then
   allocate(QP_Vnl_xc(QP_n_states))
   call mem_est("QP_Vnl_xc",(/QP_n_states/))
 endif
 if (.not.allocated(QP_Vxc)) then
   allocate(QP_Vxc(QP_n_states))
   call mem_est("QP_Vxc",(/QP_n_states/))
 endif
 QP_Vnl_xc=(0.,0.)
 QP_Vxc=(0.,0.)
 !
 !
 ! Main DB I/O
 !
 io_err=-1
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_HF_and_locXC(ID)
 !
 if (io_err==0) then
   !
   call XCo_report_and_IO()
   !
   return
   !
 else
   !
   call XCo_Hartree_Fock(E,k,xk,q)
   !
   !
 endif
 !
 ! Vxc
 !
 if (.not.l_sc_run.and..not.l_real_time) then
   !
   call XCo_local(E,Xk)
   !
 endif
 !
 if (.not.l_sc_run)  then
   call WF_free()
   call XCo_report_and_IO()
 endif
 !
 contains
   !
   subroutine XCo_report_and_IO()
     !
     Vxc_kind='DFT'
     !
     if(.not.l_gw0) then
       if(n_sp_pol==2) then
         of_name(1)='HFup'
         of_name(2)='HFdw'
       else
         of_name(1)='HF'
       endif
       !
       do i_spin=1,n_sp_pol
         call of_open_close(of_name(i_spin),'ot')
         call msg('o '//trim(of_name(i_spin)),'#',' HF corrections to be plotted',INDENT=0)    
         call msg('o '//trim(of_name(i_spin)),'#','  E_LDA       E_HF       S_x        V_xc',INDENT=0)    
       enddo
     endif
     !
     ik=QP_table(1,3)
     call msg('nr','XC '//trim(Vnlxc_kind)//' and '//trim(Vxc_kind)//' [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
     !
     do i_qp=1,QP_n_states
       !
       ib =QP_table(i_qp,1)
       ibp=QP_table(i_qp,2)
       if (QP_table(i_qp,3)/=ik) then
         ik=QP_table(i_qp,3)
         call msg('nr','XC '//trim(Vnlxc_kind)//' and '//trim(Vxc_kind)//' [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
       endif
       !
       ch_spin=' '
       !
       if (n_sp_pol==2.and.spin(QP_table(i_qp,:))==1) ch_spin='(up)'
       if (n_sp_pol==2.and.spin(QP_table(i_qp,:))==2) ch_spin='(dn)'
       ! 
       nloc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|'//trim(Vnlxc_kind)//'|'//trim(intc(ibp))&
&         //trim(ch_spin)//'> ='
       loc_fmt='<'//trim(intc(ib))//trim(ch_spin)//'|'//trim(Vxc_kind)//'|'//trim(intc(ibp))&
&         //trim(ch_spin)//'> ='
       !
       ch=trim(nloc_fmt)//' '//&
&        trim(real2ch(real(QP_Vnl_xc(i_qp)*HA2EV)))//' '//&
&        trim(real2ch(aimag(QP_Vnl_xc(i_qp)*HA2EV)))//' '//&
&        trim(loc_fmt)//' '//&
&        trim(real2ch(real(QP_Vxc(i_qp)*HA2EV)))//' '//&
&        trim(real2ch(aimag(QP_Vxc(i_qp)*HA2EV)))
       !
       if (i_qp/=QP_n_states) call msg('r',trim(ch))
       if (i_qp==QP_n_states) call msg('rn',trim(ch))
       !
       if(.not.l_gw0) then
         i_spin=spin(QP_table(i_qp,:))
         output(1)=E%E(ib,ik,i_spin)*HA2EV
         output(2)=output(1)-( QP_Vxc(i_qp)-QP_Vnl_xc(i_qp) )*HA2EV
         output(3)=QP_Vnl_xc(i_qp)*HA2EV
         output(4)=QP_Vxc(i_qp)   *HA2EV
         call msg('o '//trim(of_name(i_spin)),'',output,INDENT=-2,USE_TABS=.true.)
       endif
       !
     enddo
     !
     if(.not.l_gw0) then
       do i_spin=1,n_sp_pol
         call of_open_close(of_name(i_spin))
       enddo
     endif
     !
     ! I/O
     !
     if (io_err/=0) then
       call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
       io_err=io_HF_and_locXC(ID)
     endif
     !
     ! MPI barrier
     !
     call PP_redux_wait
     !
   end subroutine
   !
end subroutine
