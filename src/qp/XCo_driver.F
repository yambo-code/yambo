!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,          ONLY:schlen,rZERO,SP,cZERO
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_WR_CL,VERIFY,REP
 use QP_m,          ONLY:QP_Vxc,QP_Vnl_xc,QP_n_states,XCo_Hartree_Fock,QP_t,Vxc_kind,Vnlxc_kind
 use interfaces,    ONLY:WF_free
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:WF_exx_fraction,WF_exx_screening,EXT_NONE,WF_kind,&
&                        global_XC_string,WF_perturbation,WF_xc_functional
 use pseudo,        ONLY:PP_nlcc_free
#if defined _SC
 use hamiltonian,   ONLY:l_sc_V_is_local
 use global_XC,     ONLY:H_SE_FOCK,H_SE_HARTREE,H_SE_COH,H_SE_SEX
#endif
 !
#include<memory.h>
 !
 type(levels) ::E       
 type(bz_samp)::k,Xk,q
 !
 ! Screened Hybrids
 !
 logical                :: l_screen_hyb
 complex(SP),allocatable:: QP_Vnl_sex(:)
 !
 ! Reporting
 !
 character(schlen) ::sec_mode_and_message(2),ch
 integer           ::WF_kind_no_H
 logical           ::l_Vxc,l_Vnl_xc,l_section,l_load_SC_V
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
 l_Vxc   =.not.l_sc_run.and..not.l_real_time
 l_Vnl_xc=.TRUE.
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
  if (l_sc_run) call LOCAL_alloc()
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
 ! Logicals setup
 !
 l_screen_hyb=(WF_exx_screening.gt.rZERO) 
 !
 ! Allocation
 !
 call LOCAL_alloc()
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
     call XCo_Hartree_Fock(E,k,xk,q,mode='hyb')    ! For screened hybrids calculate the  model nonlocal screened exchange part
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
   ! non-linear core corrections (NLCC)
   !
   call XC_nlcc_setup()
   !
   call XCo_local(E,Xk)
   !
   if (.not.l_screen_hyb) then
     QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_xc !Hybridize...
   else
     QP_Vxc = QP_Vxc + WF_exx_fraction*QP_Vnl_sex 
     YAMBO_FREE(QP_Vnl_sex)
   endif
   !
   call PP_nlcc_free()
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
   call XCo_report(qp,E,k)
   !
 endif
 !
 contains
   !
   subroutine LOCAL_alloc()
     if (.not.allocated(QP_Vnl_xc).and.l_Vnl_xc) then
       YAMBO_ALLOC(QP_Vnl_xc,(QP_n_states))
     endif
     if (.not.allocated(QP_Vxc).and.l_Vxc) then
       YAMBO_ALLOC(QP_Vxc,(QP_n_states))
     endif
     if (.not.allocated(QP_Vnl_sex).and.l_screen_hyb) then
       YAMBO_ALLOC(QP_Vnl_sex,(QP_n_states))
       QP_Vnl_sex=cZERO
     endif
     if (l_Vnl_xc) then
       QP_Vnl_xc=cZERO
     endif
     if (l_Vxc) then
       QP_Vxc=cZERO
     endif
     if (l_screen_hyb) then
       QP_Vnl_sex=cZERO
     endif
   end subroutine
   !
end subroutine
