!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine XCo_driver(E,k,Xk,q,Dip)
 !
 use zeros,         ONLY:zero_dfl
 use drivers,       ONLY:l_acfdt,l_sc_run,l_real_time
 use pars,          ONLY:schlen,cZERO
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use DIPOLES,       ONLY:DIPOLE_t
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:XCo_Hartree_Fock,QP_t,Vxc_kind,Vnlxc_kind,&
&                        QP_Vxc,QP_Vnl_xc,QP_Vnl_sex,QP_XCo_alloc,QP_XCo_free
 use interfaces,    ONLY:WF_free
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:WF_exx_fraction,WF_exx_screening,EXT_NONE,WF_kind,&
&                        global_XC_string,WF_perturbation,WF_xc_functional
 use parser_m,      ONLY:parser 
 use parallel_m,    ONLY:ncpu,PARALLEL_CPU_role,PARALLEL_CPU_used
#if defined _SC
 use pseudo,        ONLY:l_use_p2_Vloc_becp
 use hamiltonian,   ONLY:l_sc_V_is_local
 use global_XC,     ONLY:H_SE_FOCK,H_SE_HARTREE,H_SE_COH,H_SE_SEX
#endif
 !
#include<y_memory.h>
 !
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 type(DIPOLE_t),optional ::Dip
 !
 ! Reporting
 !
 character(schlen) ::sec_mode_and_message(2),ch
 integer           ::WF_kind_no_H
 logical           ::l_section,l_load_SC_V
 logical           ::l_Vxc,l_Vnl_xc,l_screen_hyb
 ! Vxc from libxc or from H-Hbare
 logical           ::l_use_libxc,l_use_qe_pseudo
 integer           ::i_role
 type(QP_t)        ::qp
 !
 ! IO
 !
 integer           :: ID,io_err
 integer, external :: io_HF_and_locXC
 !
 ! Decide what to do
 !===================
#if defined _SC
 !
 ! The point here is that dependening on the kind of XCo I do/don't need HF
 !
 ! Eqp=Eo+QP_Sc+QP_Vnl_xc-QP_Vxc 
 !
 ! F=FOCK
 !
 ! XCo=any local => QP_Vnl_xc=F             & QP_Vxc=any local
 ! XCo=COH       => QP_Vnl_xc=F-SC_COH      & QP_Vxc= 0.
 ! XCo=SEX       => QP_Vnl_xc=F-SC_SEX      & QP_Vxc= 0.
 ! XCo=COHSEX    => QP_Vnl_xc=F-SC_COHSEX   & QP_Vxc= 0.
 ! XCo=HF        => QP_Vnl_xc=0             & QP_Vxc= 0.  (BUT COULD PRINT FOR INTERNAL CHECK THE <FOCK>)
 ! XCo=EXX       => QP_Vnl_xc=F             & QP_Vxc= EXX
 ! XCo=EXXC      => QP_Vnl_xc=F             & QP_Vxc= EXXc
 ! XCo=SRPA      => QP_Vnl_xc=F             & QP_Vxc= SRPA
 !
#endif
 !
 ! Logicals setup
 !
 l_Vxc   =.not.l_sc_run.and..not.l_real_time
 l_Vnl_xc=.TRUE.
 !
 l_use_libxc=.true.
#if defined _SC
 call parser('BareHfromScratch', l_use_p2_Vloc_becp)
 l_use_libxc=.not.l_use_p2_Vloc_becp
 l_use_qe_pseudo=l_use_p2_Vloc_becp
#endif
 !
 ! AM, 22/4/2021: a tiny WF_exx_screening causes NaN in the call to scatter_ModScr
 l_screen_hyb=(WF_exx_screening > zero_dfl) 
 !
#if defined _SC
 WF_kind_no_H=WF_kind
 if (WF_kind>H_SE_HARTREE) WF_kind_no_H=WF_kind-H_SE_HARTREE
 !
 if (WF_kind_no_H==H_SE_FOCK)         l_Vxc   =.FALSE.
 if (WF_kind_no_H==H_SE_COH)          l_Vxc   =.FALSE.
 if (WF_kind_no_H==H_SE_SEX)          l_Vxc   =.FALSE.
 if (WF_kind_no_H==H_SE_SEX+H_SE_COH) l_Vxc   =.FALSE.
 !
 if (WF_kind_no_H==H_SE_FOCK)      l_Vnl_xc   =.FALSE.
 !
#endif
 !
 if (.not.l_Vxc.and..not.l_Vnl_xc) then
  if (l_sc_run) call QP_XCo_alloc(l_Vxc,l_Vnl_xc,l_screen_hyb)
  return
 endif 
 !
 ! Section
 !
 sec_mode_and_message(1)='*'
 if (l_Vxc     .and.l_Vnl_xc) sec_mode_and_message(2)='Local Exchange-Correlation + Non-Local Fock'
 if (.not.l_Vxc.and.l_Vnl_xc) sec_mode_and_message(2)='Non-Local Fock'
 if (l_Vxc.and..not.l_Vnl_xc) sec_mode_and_message(2)='Local Exchange-Correlation'
 if (l_acfdt)   sec_mode_and_message(1)='p'
 !
 l_section=.TRUE.
#if defined _SC
 l_section=.not.l_sc_run.and.(WF_kind/=H_SE_FOCK.and.WF_kind/=H_SE_FOCK+H_SE_HARTREE)
#endif
 if (l_section) call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
 !
 ! QP states setup
 !
 call QP_state_table_setup(E)
 !
 ! Allocation
 !
 call QP_XCo_alloc(l_Vxc,l_Vnl_xc,l_screen_hyb)
 !
 ! Main DB I/O
 !
 io_err=-1
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_HF_and_locXC(ID)
 !
 if (io_err==0) then
   !
   Vxc_kind=trim(global_XC_string(WF_kind,WF_xc_functional,EXT_NONE))
   Vnlxc_kind="Hartree-Fock"
   !
   call XCo_report(qp,E,k)
   !
   return
   !
 endif
 !
 ! Hartree-Fock 
 !==============
 !
 if (l_Vnl_xc) then
   !
   ! ... hybrids
   !
   if (l_screen_hyb) then                           
     ! For screened hybrids calculate the  model nonlocal screened exchange part
     call XCo_Hartree_Fock(E,k,xk,q,mode='hyb')
     QP_Vnl_sex=QP_Vnl_xc                        
     QP_Vnl_xc=cZERO                                
   endif
   !
   ! ... pure HF
   !
   call XCo_Hartree_Fock(E,k,xk,q)
   !
 endif
 !
 ! Vxc
 !=====
 !
 if (l_Vxc) then
   !
#if defined _SC && defined _MPI
   ! FP 15/11/23: MPI q-tasks lead to problems with DIPOLE_IO of P_square in Bare_Hamiltonian_from_Scracth (to be fixed)
   if (l_use_p2_Vloc_becp) then
     if(ncpu/=1) then
       do i_role=1,size(PARALLEL_CPU_role)
         if (trim(PARALLEL_CPU_role(i_role))=='q') then
           if (PARALLEL_CPU_used(i_role)/=1) then 
             call error("[PARALLEL] Turn off MPI tasks over "//trim(PARALLEL_CPU_role(i_role))//" for Hbare reconstruction")
           endif
           exit
         endif
       enddo
     endif
   endif
#endif
   !
   if (l_use_libxc) then 
     !
     call XCo_local(E,Xk)
     !
     if (.not.l_screen_hyb) then
       QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_xc !Hybridize...
     else
       QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_sex 
       call QP_XCo_free(l_screen_hyb=.true.)
     endif
     !
   endif
   !
#if defined _SC
   if (l_use_qe_pseudo) call XCo_from_H_minus_Hbare(E,Xk,k,q,Dip)
#endif
   !
 endif
 !
 ! Kind transfer
 !===============
 !
 Vxc_kind=trim(global_XC_string(WF_kind,WF_xc_functional,EXT_NONE))
 Vnlxc_kind="Hartree-Fock"
 !
#if defined _SC
 !
 if (.not.l_sc_V_is_local) then
   Vnlxc_kind="Hartree-Fock-("//trim(global_XC_string(WF_kind,WF_xc_functional,WF_perturbation))//")"
 endif
 !
 ! Non-local part of SC xc must be subtracted from V_nlxc
 !========================================================
 !
 l_load_SC_V=.FALSE.
 if (WF_kind_no_H==H_SE_FOCK)         l_load_SC_V   =.TRUE.
 if (WF_kind_no_H==H_SE_COH)          l_load_SC_V   =.TRUE.
 if (WF_kind_no_H==H_SE_SEX)          l_load_SC_V   =.TRUE.
 if (WF_kind_no_H==H_SE_SEX+H_SE_COH) l_load_SC_V   =.TRUE.
 if (l_load_SC_V) call QP_load_SC_xc( )
 !
#endif
 !
 ! I/O
 !=====
 !
 if (.not.l_sc_run)  then
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_HF_and_locXC(ID)
   !
   call WF_free(WF)
   !
   call XCo_report(qp,E,k)
   !
 endif
 !
end subroutine
