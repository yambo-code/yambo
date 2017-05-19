!
!        Copyright (C) 2000-2017 the YAMBO team
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
 use drivers,       ONLY:l_acfdt,l_sc_run,l_real_time,l_gw0
 use pars,          ONLY:schlen,rZERO,SP,cZERO
 use com,           ONLY:msg
 use units,         ONLY:HA2EV
 use electrons,     ONLY:levels,n_sp_pol,spin,E_reset,E_duplicate
 use stderr,        ONLY:intc,real2ch
 use R_lattice,     ONLY:bz_samp
 use parallel_m,    ONLY:PP_redux_wait
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:QP_table,QP_Vxc,QP_Vnl_xc,QP_n_states,Vxc_kind,Vnlxc_kind,&
&                        XCo_Hartree_Fock,QP_t,QP_nb,QP_nk,QP_reset,&
&                        report_Vnlxc,report_Sc,report_dSc
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:WF_exx_fraction,WF_exx_screening
#if defined _SC
 use global_XC,     ONLY:WF_kind,H_SE_FOCK,H_SE_HARTREE,H_SE_COHSEX,H_SE_COH,H_SE_SEX,H_SE_EXX,H_SE_EXXC,H_SE_SRPA
#endif
 !
#include<memory.h>
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! WorkSpace
 !
 type(levels) ::E_dummy
 !
 ! Screened Hybrids
 !
 logical                :: l_screen_hyb
 complex(SP),allocatable:: QP_Vnl_sex(:)
 !
 ! Reporting
 !
 integer           ::i_qp,ib,ik,ibp,ikp,i_s
 character(schlen) ::sec_mode_and_message(2),ch,ch_spin,nloc_fmt,loc_fmt
 logical           ::l_local
 type(QP_t)        ::qp
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
 l_local=.not.l_sc_run.and.(WF_kind/=H_SE_FOCK.and.WF_kind/=H_SE_FOCK+H_SE_HARTREE)
#endif
 if(l_local) call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
 !
 ! QP states setup
 !
 call QP_state_table_setup(E)
 !
 ! Logicals setup
 !
 l_screen_hyb=(WF_exx_screening.gt.rZERO) 
 !
 ! Allocation
 !
 if (.not.allocated(QP_Vnl_xc)) then
   YAMBO_ALLOC(QP_Vnl_xc,(QP_n_states))
 endif
 if (.not.allocated(QP_Vxc)) then
   YAMBO_ALLOC(QP_Vxc,(QP_n_states))
 endif
 if (.not.allocated(QP_Vnl_sex).and.l_screen_hyb) then
   YAMBO_ALLOC(QP_Vnl_sex,(QP_n_states))
 endif
 QP_Vnl_xc=cZERO
 QP_Vxc=cZERO
 if (l_screen_hyb) QP_Vnl_sex=cZERO
 !
#if defined _SC
 if (l_sc_run .and. (WF_kind==H_SE_FOCK.or.WF_kind==H_SE_HARTREE+H_SE_FOCK)) return
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
   if (l_screen_hyb) then                          ! 
     call XCo_Hartree_Fock(E,k,xk,q,mode='hyb')    ! For screened hybrids calculate the 
     QP_Vnl_sex=QP_Vnl_xc                          ! model nonlocal screened exchange part
     QP_Vnl_xc=cZERO                               !  
   end if
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
   ! XCo=COH       => QP_Vnl_xc=HF-QP_COH    & QP_Vxc= 0.
   ! XCo=SEX       => QP_Vnl_xc=HF-QP_SEX    & QP_Vxc= 0.
   ! XCo=COHSEX    => QP_Vnl_xc=HF-QP_COHSEX & QP_Vxc= 0.
   ! XCo=HF        => QP_Vnl_xc=0.           & QP_Vxc= 0.
   ! XCo=EXX       => QP_Vnl_xc=HF           & QP_Vxc= EXX
   ! XCo=EXXC      => QP_Vnl_xc=HF           & QP_Vxc= EXXc
   ! XCo=SRPA      => QP_Vnl_xc=HF-QP_COHSEX & QP_Vxc= SRPA
   !
   if (WF_kind==H_SE_COHSEX.or.WF_kind==H_SE_COH.or.WF_kind==H_SE_SEX) call QP_load_SC_xc()
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
   if( WF_kind/=H_SE_COHSEX .and. WF_kind/=H_SE_COH .and. WF_kind/=H_SE_SEX ) call XCo_local(E,Xk)
#else
   call XCo_local(E,Xk)
   if (.not.l_screen_hyb) then
     QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_xc !Hybridize...
   else
     QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_sex 
     YAMBO_FREE(QP_Vnl_sex)
   endif
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
     elseif  (WF_kind == H_SE_EXX) then
       Vxc_kind='OEP-EXX'
     elseif  (WF_kind == H_SE_EXXC) then
       Vxc_kind='OEP-EXX+C'
     elseif  (WF_kind == H_SE_SRPA) then
       Vxc_kind='OEP-SRPA'
     else
       Vxc_kind='none'
     endif
     if (WF_kind == H_SE_COHSEX) then 
       Vnlxc_kind='HF-QP_COHSEX'
     else if (WF_kind == H_SE_COH) then 
       Vnlxc_kind='HF-QP_COH'
     else if (WF_kind == H_SE_SEX) then 
       Vnlxc_kind='HF-QP_SEX'
     else
       Vnlxc_kind='HF'
     endif
#else
     Vxc_kind='DFT'
#endif
     !
     call QP_reset(qp)
     call E_reset(E_dummy)
     call E_duplicate(E,E_dummy)
     !
     qp%nk=QP_nk
     qp%nb=QP_nb
     qp%n_states=QP_n_states
     qp%n_descs=0
     !
     report_Sc=.FALSE.
     report_Vnlxc=.TRUE.
     report_dSc=.FALSE.
     !
     YAMBO_ALLOC(qp%E,(qp%n_states))
     YAMBO_ALLOC(qp%E_bare,(qp%n_states))
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
         E_dummy%E(ib,ikp,i_s)=E%E(ib,ikp,i_s)+real(QP_Vnl_xc(i_qp)-QP_Vxc(i_qp),SP)
         qp%E(i_qp)           =E_dummy%E(ib,ikp,i_s)
         qp%E_bare(i_qp)      =E%E(ib,ikp,i_s)
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
     !
     ! Output 
     !
     if (.not.l_gw0.and..not.l_real_time) call QP_of(qp,E,-1,'hf')
     !
     ! MPI barrier
     !
     call PP_redux_wait
     !
     ! Clean up
     !
     YAMBO_FREE(qp%E)
     YAMBO_FREE(qp%E_bare)
     call E_reset(E_dummy)
     call QP_reset(qp)
     !
   end subroutine
   !
end subroutine
