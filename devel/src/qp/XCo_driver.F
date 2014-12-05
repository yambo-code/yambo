!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine XCo_driver(E,k,Xk,q)
 !
 use drivers,       ONLY:l_acfdt,l_sc_run,l_real_time
 use pars,          ONLY:schlen,RZERO,SP
 use com,           ONLY:msg
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_sp_pol,spin,E_reset,E_duplicate
 use stderr,        ONLY:intc,real2ch
 use R_lattice,     ONLY:bz_samp
 use parallel_m,    ONLY:PP_redux_wait
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:QP_table,QP_Vxc,QP_Vnl_xc,QP_n_states,Vxc_kind,Vnlxc_kind 
 use memory_m,      ONLY:mem_est
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:WF_xc_functional,WF_kind,WF_exx_fraction
#if defined _SC
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SC_EXX,SC_EXXC,SC_SRPA
#endif
 !
 implicit none
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! WorkSpace
 !
 type(levels) ::E_dummy
 !
 ! Reporting
 !
 integer           ::i_qp,ib,ik,ibp,ikp,i_s
 character(schlen) ::sec_mode_and_message(2),ch,ch_spin,nloc_fmt,loc_fmt
 logical           ::l_local
 !
 ! IO
 !
 integer           :: ID,io_err
 integer, external :: io_HF_and_locXC
 !
 ! Section
 !
 sec_mode_and_message(1)='*'
 sec_mode_and_message(2)='Bare local and non-local Exchange-Correlation'
 if (l_acfdt)   sec_mode_and_message(1)='p'
 l_local=.true.
#if defined _SC
 l_local=.not.l_sc_run.and.WF_kind/=SC_HF 
#endif
 if(l_local) call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
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
#if defined _SC
 if (l_sc_run .and. WF_kind==SC_HF) return
#endif
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
#if defined _SC
   !
   ! The point here is that dependening on the kind of XCo I do/don't need HF
   !
   ! Eqp=Eo+QP_Sc+QP_Vnl_xc-QP_Vxc 
   !
   ! XCo=any local => QP_Vnl_xc=HF           & QP_Vxc=any local
   ! XCo=COHSEX    => QP_Vnl_xc=HF-QP_COHSEX & QP_Vxc= 0.
   ! XCo=HF        => QP_Vnl_xc=0.           & QP_Vxc= 0.
   ! XCo=EXX       => QP_Vnl_xc=HF           & QP_Vxc= EXX
   ! XCo=EXXC      => QP_Vnl_xc=HF           & QP_Vxc= EXXc
   ! XCo=SRPA      => QP_Vnl_xc=HF-QP_COHSEX & QP_Vxc= SRPA
   !
   if (WF_kind==SC_COHSEX) call QP_load_SC_xc()
   !
#endif
   !
 endif
 !
 ! Vxc
 !
 if (.not.l_sc_run.and..not.l_real_time) then
   !
#if defined _SC
   if( WF_kind/= SC_COHSEX ) call XCo_local(E,Xk)
#else
   call XCo_local(E,Xk)
   QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_xc !Hybridize...
#endif
   !
 endif
 !
 if (.not.l_sc_run)  then
   call WF_free(WF)
   call XCo_report_and_IO()
 endif
 !
 contains
   !
   subroutine XCo_report_and_IO()
     !
#if defined _SC
     if (WF_kind < 100) then
       Vxc_kind='LDA'
     elseif  (WF_kind < 200) then
       Vxc_kind='GGA'
     elseif  (WF_kind == SC_EXX) then
       Vxc_kind='OEP-EXX'
     elseif  (WF_kind == SC_EXXC) then
       Vxc_kind='OEP-EXX+C'
     elseif  (WF_kind == SC_SRPA) then
       Vxc_kind='OEP-SRPA'
     else
       Vxc_kind='none'
     endif
     
     if (WF_kind == SC_COHSEX) then 
       Vnlxc_kind='HF-QP_COHSEX'
     else
       Vnlxc_kind='HF'
     endif
#else
     Vxc_kind='DFT'
#endif
     !
     call E_reset(E_dummy)
     call E_duplicate(E,E_dummy)
     !
     ik=QP_table(1,3)
     call msg('nr','XC '//trim(Vnlxc_kind)//' and '//trim(Vxc_kind)//' [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
     !
     do i_qp=1,QP_n_states
       !
       ib =QP_table(i_qp,1)
       ibp=QP_table(i_qp,2)
       ikp=QP_table(i_qp,3)
       i_s=spin(QP_table(i_qp,:))
       !
       if (ib==ibp) then
         E_dummy%E(ib,ikp,i_s)=E%E(ib,ikp,i_s)+QP_Vnl_xc(i_qp)-QP_Vxc(i_qp)
       endif
       !
       if (ikp/=ik) then
         ik=ikp
         call msg('nr','XC '//trim(Vnlxc_kind)//' and '//trim(Vxc_kind)//' [eV] @ K ['//trim(intc(ik))//'] (iku):',k%pt(ik,:))
       endif
       !
       ch_spin=' '
       !
       if (n_sp_pol==2.and.i_s==1) ch_spin='(up)'
       if (n_sp_pol==2.and.i_s==2) ch_spin='(dn)'
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
     enddo
     !
     ! I/O
     !
     if (io_err/=0) then
       call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
       io_err=io_HF_and_locXC(ID)
     endif
     !
     ! Direct GAP
     !
     call section('+',trim(Vnlxc_kind)//' occupations report')
     call REPORT_Occupations(E_dummy)
     call E_reset(E_dummy)
     !
     ! MPI barrier
     !
     call PP_redux_wait
     !
   end subroutine
   !
end subroutine
