!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine COLLISIONS_eval(E,X,k,xk,q,Xw)
 !
 use drivers,       ONLY:l_HF_and_locXC,l_cohsex,l_elel_scatt,l_elel_corr,l_use_collisions
 use com,           ONLY:error
 use pars,          ONLY:IP
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_time_rev
 use QP_m,          ONLY:QP_nb,QP_n_states,QP_n_G_bands,QP_state
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SC_EXX,SC_EXXC,WF_kind,WF_xc_functional
 use SC,            ONLY:SC_bands
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
 use collision,     ONLY:COLLISIONS_parallel_SC_structure,V_kind,nGW_NEQ_collisions,&
&                        HF_collisions,COHSEX_collisions,GW_NEQ_collisions,nCOHSEX_collisions
 use X_m,           ONLY:X_t
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP,io_COLLs
 use fragments,     ONLY:fragment_status
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index,io_COLLISIONS
 use parallel_m,    ONLY:PAR_nQ,PAR_nQP,PAR_IND_QP,PAR_IND_Xk_ibz,PAR_IND_G_k,PAR_IND_Bp_mat
 use X_m,           ONLY:X_t
 !
 implicit none
 type(levels)  :: E
 type(w_samp)  :: Xw(2) 
 type(bz_samp) :: k,Xk,q
 type(X_t)     :: X(4)
 !
 ! WorkSpace
 !
 integer           ::ID,i_kind,NC
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
 !
 ! Check for the Time Reversal Symmetry
 !
 if (i_time_rev==1) call error(" COLLISIONS not compatible with Time Reversal Symmetry! ")
 !
 ! No DB => EVAL
 !===============
 !
 NC=PAR_nQP
 if (io_COLLs) then
   NC=1
   allocate(fragment_status(QP_n_states))
   call mem_est("fragment_status",(/QP_n_states/),(/IP/))
 endif
 !
 ! PARALLEL Setup
 !================
 if (l_elel_corr) then
#if defined _SC && defined _RT
   call PARALLEL_global_indexes(E,k,q,"Real_Time")
   call PARALLEL_WF_distribute(K_index=PAR_IND_G_k,Bm_index=PAR_IND_Bp_mat,CLEAN_UP=.TRUE.)
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
 if (l_HF_and_locXC.and.l_elel_corr) then
   !
   call section('+','Hartree-Fock')
   !================================
   !
   V_kind=SC_HF
   fragment_status=-1
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   !
   if (io_COLLISIONS(0,ID)/=0) then
     !
     if (io_COLLs) then
       call COLLISIONS_alloc_and_free("HF","collision","alloc",1)
       call COLLISIONS_alloc_and_free("HF","v3 table","alloc",1)
     else
       call COLLISIONS_alloc_and_free("HF","collision","alloc",0)
       call COLLISIONS_alloc_and_free("HF","v3","alloc",1)
       call COLLISIONS_alloc_and_free("HF","table","alloc",0)
     endif
     !
     call COLLISIONS_Hartree_Fock(E,k,q,NC,HF_collisions)
     !
   endif
   !
 endif
 !
 if (l_cohsex) then
   !
   do i_kind=1,2
     !
     if ((i_kind==1.and..not.l_elel_corr).or.(i_kind==2.and..not.l_elel_scatt)) cycle
     !
     if (i_kind==1.and.l_elel_corr)  then
       if (l_HF_and_locXC)      call section('=','COHSEX (e-e correlation)')
       if (.not.l_HF_and_locXC) call section('+','COHSEX (e-e correlation)')
       V_kind=SC_COHSEX
     else if (i_kind==2.and.l_elel_scatt) then
       if (l_elel_corr)      call section('=','COHSEX (e-e scattering)')
       if (.not.l_elel_corr) call section('+','COHSEX (e-e scattering)')
       V_kind=SE_GW_NEQ
     endif
     !
     fragment_status=-1
     call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
     !
     if (io_COLLISIONS(0,ID)/=0) then
       !
       ! PARALLEL Setup (e-e scattering)
       !=================================
       if (l_elel_scatt) then
         call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
         call PARALLEL_global_indexes(E,k,q,"Real_Time")
         call PARALLEL_WF_distribute(QP_index=PAR_IND_QP,CLEAN_UP=.FALSE.)
         !call PARALLEL_WF_distribute(QP_index=PAR_IND_QPp) ! Davide
         call PARALLEL_WF_index( )
       endif
       !
       if (i_kind==1) then
         if (io_COLLs) then
           call COLLISIONS_alloc_and_free("COHSEX","collision","alloc",1)
           call COLLISIONS_alloc_and_free("COHSEX","v3 table","alloc",1)
         else
           call COLLISIONS_alloc_and_free("COHSEX","collision","alloc",0)
           call COLLISIONS_alloc_and_free("COHSEX","v3","alloc",1)
           call COLLISIONS_alloc_and_free("COHSEX","table","alloc",0)
         endif
       else if (i_kind==2) then
         if (io_COLLs) then
           call COLLISIONS_alloc_and_free("GW_NEQ","collision","alloc",1)
           call COLLISIONS_alloc_and_free("GW_NEQ","v3 table","alloc",1)
         else
           call COLLISIONS_alloc_and_free("GW_NEQ","collision","alloc",0)
           call COLLISIONS_alloc_and_free("GW_NEQ","v3","alloc distribute",0)
         endif
       endif
       !
       QP_n_G_bands(2)=SC_bands(2)
       !  
       if (i_kind==1) call COLLISIONS_cohsex(X(2),Xk,E,k,q,Xw(2),i_kind,COHSEX_collisions,nCOHSEX_collisions) 
       if (i_kind==2) call COLLISIONS_cohsex(X(2),Xk,E,k,q,Xw(2),i_kind,GW_NEQ_collisions,nGW_NEQ_collisions) 
       !
       call WF_free(WF)
       !
     endif
     !
   enddo
   !
 endif
 !
 ! CLEAN
 !=======
 !
 if (io_COLLs) then
   if (allocated(fragment_status)) deallocate(fragment_status)
   call mem_est("fragment_status")
   if (l_elel_corr) then
     call COLLISIONS_alloc_and_free("HF","v3 table","clean",1)
     if (l_cohsex) call COLLISIONS_alloc_and_free("COHSEX","v3 table","clean",1)
   endif
   if (l_elel_scatt) call COLLISIONS_alloc_and_free("GW_NEQ","v3 table","clean",1)
 else
   if (l_elel_corr) then
     call COLLISIONS_alloc_and_free("HF","v3","clean",1)
     if (l_cohsex) call COLLISIONS_alloc_and_free("COHSEX","v3","clean",1)
   endif
 endif
 !
end subroutine
