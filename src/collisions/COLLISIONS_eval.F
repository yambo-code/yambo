!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine COLLISIONS_eval(E,X,k,xk,q,Xw)
 !
 ! The EOM looks like:
 !
 ! i\partial_T G^<+[h+\Sigma_s,G^<]=I^<
 !
 ! here I evaluate the components of both \Sigma_s and I^<.
 !
 ! While for \Sigma_s the POTENTIAL can be specified, for I^< only the KIND is possible to define.
 !
 ! So we have, at the command line,
 !
 ! yambo_rt -e -s e/p/h/a  -> I^<
 ! yambo_rt -e -v (hf),(c)ohsex  -> \Sigma_s
 !
 ! In the \Sigma_s case also yambo_sc can evaluate
 !
 use drivers,       ONLY:l_HF_and_locXC,l_cohsex,l_elel_scatt,l_use_collisions,&
&                        l_elphoton_scatt
 use com,           ONLY:error
 use pars,          ONLY:IP,schlen
 use electrons,     ONLY:levels,spin
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_time_rev
 use QP_m,          ONLY:QP_nb,QP_n_states,QP_table,QP_state
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:SC_HF,SC_COHSEX,WF_kind,WF_xc_functional
 use SC,            ONLY:SC_bands,SC_potential
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ,SE_GW_QED
 use collision,     ONLY:COLLISIONS_parallel_SC_structure,V_kind,COLLISIONS_IO_status,n_COLLISIONS,&
&                        COLLISIONS_state
 use X_m,           ONLY:X_t
 use plasma,        ONLY:EH_gas
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP,io_COLLs
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index,io_COLLISIONS
 use parallel_m,    ONLY:PAR_nQ,PAR_nQP,PAR_IND_QP,PAR_IND_Xk_ibz,PAR_IND_G_k,PAR_IND_Bp_mat,PAR_IND_Plasma
 !
 implicit none
 type(levels)  :: E
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t)     :: X(4)
 !
 ! WorkSpace
 !
 integer       :: ID,i_kind,NC,IO_err,i_qp
 !
 call section('*','Extended collisions')
 !=======================================
 !
 ! If I am calculating the collisions then I can use them
 !
 l_use_collisions=.TRUE.
 !
 ! QP states setup
 !
 ! When storing all collisions on file I need all SC bands
 !
 if(allocated(QP_state)) deallocate(QP_state)
 QP_nb=SC_bands(2)
 call QP_state_table_setup(E)
 n_COLLISIONS=QP_n_states
 !
 ! Check for the Time Reversal Symmetry
 !
 if (trim(SC_potential)=="HARTREE-FOCK".or.trim(SC_potential)=="COHSEX".or.l_elel_scatt) then
   if (i_time_rev==1) call error(" COLLISIONS not compatible with Time Reversal Symmetry! ")
 endif
 !
 ! Switch 
 !========
 !
 call XC_switch()
 !
 ! No DB => EVAL
 !===============
 !
 NC=PAR_nQP
 if (io_COLLs) then
   NC=1
   allocate(COLLISIONS_IO_status(n_COLLISIONS))
   call mem_est("COLLISIONS_IO_status",(/n_COLLISIONS/),(/IP/))
   allocate(COLLISIONS_state(n_COLLISIONS,4))
   call mem_est("COLLISIONS_state",(/n_COLLISIONS,4/),(/IP/))
   do i_qp=1,QP_n_states
     COLLISIONS_state(i_qp,:3)=QP_table(i_qp,:3)
     COLLISIONS_state(i_qp, 4)=spin(QP_table(i_qp,:))
   enddo
 endif
 !
 ! PARALLEL Setup
 !================
 if (trim(SC_potential)=="HARTREE-FOCK".or.trim(SC_potential)=="COHSEX".or.l_elphoton_scatt) then
#if defined _SC && defined _RT
   call PARALLEL_global_indexes(E,k,q,"Real_Time")
   call PARALLEL_WF_distribute(K_index=PAR_IND_G_k,Bm_index=PAR_IND_Bp_mat,CLEAN_UP=.TRUE.)
   COLLISIONS_parallel_SC_structure=.FALSE.
#endif
#if defined _SC && !defined _RT
   call PARALLEL_global_indexes(E,k,q,"Self_Energy")
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,Bm_index=PAR_IND_Bp_mat,CLEAN_UP=.TRUE.)
   COLLISIONS_parallel_SC_structure=.TRUE.
#endif
   call PARALLEL_WF_distribute(QP_index=PAR_IND_QP)
   call PARALLEL_WF_index( )
 endif
 !
 if (trim(SC_potential)=="HARTREE-FOCK".or.trim(SC_potential)=="COHSEX") then
   !
   call section('+','Hartree-Fock')
   !================================
   !
   call CHECK_and_ALLOC(SC_HF)
   !
   if (IO_err/=0) then
     !
     call COLLISIONS_Fock(E,k,q)
     !
     ! HF clean
     !----------
     if (io_COLLs) call COLLISIONS_alloc_and_free("Fock","v3 table","clean",1)
     !
   endif
   !
 endif
 !
 if (trim(SC_potential)=="COHSEX") then
   !
   if (l_HF_and_locXC)      call section('=','SEX (e-e correlation)')
   if (.not.l_HF_and_locXC) call section('+','SEX (e-e correlation)')
   !=================================================================
   !
   call CHECK_and_ALLOC(SC_COHSEX)
   !
   if (IO_err/=0) then
     !
     call COLLISIONS_sex(X(2),Xk,E,k,q,Xw(2),i_kind)
     !
     ! COHSEX clean
     !--------------
     if (io_COLLs) call COLLISIONS_alloc_and_free("COHSEX","v3 table","clean",1)
     !
   endif
   !
 endif
 !
#if defined _QED
 !
 if (l_elphoton_scatt) then
   !
   call section('+','Electron-Photon (P)')
   !=======================================
   !
   call CHECK_and_ALLOC(SE_GW_QED)
   !
   if (IO_err/=0) then
     !
     call COLLISIONS_momentum(E,k,q)
     !
     ! Clean
     !-------
     if (io_COLLs) call COLLISIONS_alloc_and_free("Momentum","v3 table","clean",1)
     !
   endif
   !
 endif
 !
#endif
 !
 if (l_elel_scatt) then
   !
   if (l_cohsex)            call section('=','NEQ GW (e-e scattering)')
   if (.not.l_cohsex)       call section('+','NEQ GW (e-e scattering)')
   !=================================================================
   !
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   call PARALLEL_global_indexes(E,k,q,"Real_Time")
   !
   call PLASMA_build_up(E,k,q)
   call PLASMA_parallel_setup(EH_gas)
   !
   call PARALLEL_WF_distribute(QP_index=PAR_IND_QP,CLEAN_UP=.FALSE.)
   call PARALLEL_WF_distribute(Plasma_index=PAR_IND_Plasma)
   call PARALLEL_WF_index( )
   !
   call CHECK_and_ALLOC(SE_GW_NEQ)
   !
   if (IO_err/=0) then 
     !
     call COLLISIONS_NEQ_GW_static(X(2),Xk,E,k,q,Xw(2),i_kind) 
     !
     ! GW_NEQ clean
     !--------------
     if (io_COLLs) call COLLISIONS_alloc_and_free("GW_NEQ","v3 table","clean",1)
     !
   endif
   !
 endif
 !
 ! CLEAN
 !=======
 !
 call WF_free(WF)
 !
 if (io_COLLs) then
   if (allocated(COLLISIONS_IO_status)) deallocate(COLLISIONS_IO_status)
   call mem_est("COLLISIONS_IO_status")
   if (allocated(COLLISIONS_state)) deallocate(COLLISIONS_state)
   call mem_est("COLLISIONS_state")
 else
   if (l_HF_and_locXC)  call COLLISIONS_alloc_and_free("Fock","v3","clean",1)
   if (l_cohsex)        call COLLISIONS_alloc_and_free("SEX","v3","clean",1)
   if (l_elel_scatt)    call COLLISIONS_alloc_and_free("GW_NEQ","v3","clean",1)
#if defined _QED
   if (l_elphoton_scatt) call COLLISIONS_alloc_and_free("Momentum","v3","clean",1)
#endif
 endif
 !
 contains
   !
   subroutine CHECK_and_ALLOC(COLL_kind)
     !
     integer           :: COLL_kind
     character(schlen) :: COLL_name
     !
     IO_err=-1
     !
     if (io_COLLs) then
       COLLISIONS_IO_status=-1
       V_kind=COLL_kind
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       IO_err=io_COLLISIONS(0,ID)
       if (any(COLLISIONS_IO_status/=1)) IO_err=-1
     endif
     !
     if (IO_err==0) return
     !
     if (COLL_kind==SC_HF)     COLL_name="Fock"
     if (COLL_kind==SC_COHSEX) COLL_name="SEX"
     if (COLL_kind==SE_GW_NEQ) COLL_name="GW_NEQ"
     if (COLL_kind==SE_GW_QED) COLL_name="Momentum"
     !
     if (io_COLLs) then
       call COLLISIONS_alloc_and_free(trim(COLL_name),"collision","alloc",1)
       call COLLISIONS_alloc_and_free(trim(COLL_name),"v3 table","alloc",1)
     else
       call COLLISIONS_alloc_and_free(trim(COLL_name),"collision","alloc",0)
       if (COLL_kind==SE_GW_NEQ.or.COLL_kind==SE_GW_QED) then
         call COLLISIONS_alloc_and_free(trim(COLL_name),"v3","alloc distribute",0)
       else
         call COLLISIONS_alloc_and_free(trim(COLL_name),"v3","alloc",1)
         call COLLISIONS_alloc_and_free(trim(COLL_name),"table","alloc",0)
       endif
     endif
     !
   end subroutine
   !
end subroutine
