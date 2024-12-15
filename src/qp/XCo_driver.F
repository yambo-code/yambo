!
! License-Identifier: GPL
!
! Copyright (C) 2009 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine XCo_driver(E,k,Xk,q,WF_kind_no,l_Vxc,l_sc_mode)
 !
 use zeros,         ONLY:zero_dfl
 use drivers,       ONLY:l_acfdt
 use pars,          ONLY:schlen,cZERO
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:XCo_Hartree_Fock,QP_t,Vxc_kind,Vnlxc_kind,&
&                        QP_Vxc,QP_Vnl_xc,QP_Vnl_sex,QP_XCo_alloc,QP_XCo_free
 use interfaces,    ONLY:WF_free
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PAR_IND_WF_linear
 use global_XC,     ONLY:WF_exx_fraction,WF_exx_screening,EXT_NONE,WF_kind,&
&                        global_XC_string,WF_perturbation,WF_xc_functional
 !
#include<y_memory.h>
 !
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 logical, intent(in)  :: l_Vxc,l_sc_mode
 !
 ! Reporting
 !
 character(schlen) ::sec_mode_and_message(2),ch
 logical           ::l_section
 logical           ::l_Vxc,l_Vnl_xc,l_screen_hyb
 type(QP_t)        ::qp
 !
 ! IO
 !
 integer           :: ID,io_err
 integer, external :: io_HF_and_locXC
 !
 ! Logicals setup
 !
 l_Vnl_xc=.TRUE.
 !
 ! AM, 22/4/2021: a tiny WF_exx_screening causes NaN in the call to scatter_ModScr
 l_screen_hyb=(WF_exx_screening > zero_dfl) 
 !
 if (.not.l_Vxc.and..not.l_Vnl_xc) then
  if (l_sc_mode) call QP_XCo_alloc(l_Vxc,l_Vnl_xc,l_screen_hyb)
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
 if (.not.l_sc_mode) call section(trim(sec_mode_and_message(1)),trim(sec_mode_and_message(2)))
 !
 ! QP states setup
 !
 call QP_state_table_setup(E,1,1,.true.)
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
   end if
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
   call XCo_local(E,Xk,PAR_IND_WF_linear,.false.,.not.l_sc_mode)
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
 ! Kind transfer
 !===============
 !
 Vxc_kind=trim(global_XC_string(WF_kind,WF_xc_functional,EXT_NONE))
 Vnlxc_kind="Hartree-Fock"
 !
 if (l_sc_mode) then
   Vnlxc_kind="Hartree-Fock-("//trim(global_XC_string(WF_kind,WF_xc_functional,WF_perturbation))//")"
 endif
 !
 ! I/O
 !=====
 !
 if (.not.l_sc_mode)  then
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   io_err=io_HF_and_locXC(ID)
   !
   call WF_free(WF)
   call XCo_report(qp,E,k)
   !
 endif
 !
end subroutine
