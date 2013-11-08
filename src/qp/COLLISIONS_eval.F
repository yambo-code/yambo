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
 use drivers,       ONLY:l_HF_and_locXC,l_cohsex,l_elel_scatt,l_elel_corr
 use com,           ONLY:error
 use pars,          ONLY:schlen,SP,IP
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:i_time_rev
 use QP_m,          ONLY:QP_nb,QP_n_states,QP_nk,QP_n_G_bands,QP_state
 use wave_func,     ONLY:WF
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SC_EXX,SC_EXXC,WF_kind,WF_xc_functional
 use SC,            ONLY:SC_bands
 use global_XC,     ONLY:SC_HF,SC_COHSEX,SE_GW_NEQ
 use collisions_IO, ONLY:V_kind,collisions_IO_alloc,collisions_IO_free,TEMP_COLLISIONS_v,&
&                        io_COLLISIONS,collisions_RANGES_setup,COLLISIONS_range
 use collision,     ONLY:COLLISIONS,COLLISIONS_parallel_SC_structure
 use X_m,           ONLY:X_t
 use frequency,     ONLY:w_samp
 use memory_m,      ONLY:mem_est
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP,io_COLLs
 use fragments,     ONLY:fragment_status
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,    ONLY:PAR_nQ,PAR_nQP,PAR_IND_QP,PAR_IND_QPp,PAR_IND_Xk_ibz,PAR_IND_G_k,PAR_IND_Bp_mat
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
 integer           ::ID,i_kind
 character(schlen) ::ch
 !
 call section('*','Extended collisions')
 !=======================================
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
 if (io_COLLs) then
   allocate(fragment_status(QP_n_states))
   call mem_est("fragment_status",(/QP_n_states/),(/IP/))
 endif
 !
 ! PARALLEL Setup
 !================
 if (l_elel_corr) then
#if defined _SC && defined _RT
   call PARALLEL_global_indexes(E,k,q,"Real_Time")
   call PARALLEL_WF_distribute(SC_bands(2),k%nibz,K_index=PAR_IND_G_k,Bm_index=PAR_IND_Bp_mat,CLEAN_UP=.TRUE.)
#endif
#if defined _SC && !defined _RT
   call PARALLEL_global_indexes(E,k,q,"Self_Energy")
   call PARALLEL_WF_distribute(SC_bands(2),k%nibz,K_index=PAR_IND_Xk_ibz,Bm_index=PAR_IND_Bp_mat,CLEAN_UP=.TRUE.)
   COLLISIONS_parallel_SC_structure=.TRUE.
#endif
   call PARALLEL_WF_distribute(SC_bands(2),k%nibz,QP_index=PAR_IND_QP)
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
   if (io_COLLs) call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   !
   if (io_COLLISIONS(0,ID)/=0.or..not.io_COLLs) then
     !
     if (io_COLLs) then
       COLLISIONS_range(1,:)=SC_bands
       COLLISIONS_range(2,:)=SC_bands
       COLLISIONS_range(3,:)=(/1,QP_nk/)
       allocate(TEMP_COLLISIONS_v(QP_nb,QP_nb,QP_nk))
       call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
       call collisions_IO_alloc()
       call report_disk_usage('HF')
     endif
     !
     call COLLISIONS_Hartree_Fock(E,k,q)
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
     ! PARALLEL Setup (e-e scattering)
     !=================================
     if (l_elel_scatt) then
       call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
       call PARALLEL_global_indexes(E,k,q,"Real_Time")
       call PARALLEL_WF_distribute(SC_bands(2),k%nibz,QP_index=PAR_IND_QP,CLEAN_UP=.FALSE.)
       call PARALLEL_WF_distribute(SC_bands(2),k%nibz,QP_index=PAR_IND_QPp)
       call PARALLEL_WF_index( )
     endif
     !
     if (io_COLLs) then
       !
       fragment_status=-1
       call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
       !
       if (io_COLLISIONS(0,ID)/=0) then
         !
         QP_n_G_bands(2)=SC_bands(2)
         !  
         ! RL vectors to dimension the COLLISIONS
         !  
         if (i_kind/=3) call collisions_RANGES_setup("ee_corr")
         if (i_kind==3) call collisions_RANGES_setup("ee_scatt")
         !
         if (.not.allocated(TEMP_COLLISIONS_v)) then
           allocate(TEMP_COLLISIONS_v(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                                     COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                                     COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
           call mem_est("TEMP_COLLISIONS_v",(/size(TEMP_COLLISIONS_v)/),(/2*SP/))
           call collisions_IO_alloc()
         endif
         !
         call report_disk_usage('COHSEX')
         !
         call COLLISIONS_cohsex(X(2),Xk,E,k,q,Xw(2),i_kind) 
         !
       endif
       !
     else
       !
       allocate(COLLISIONS(PAR_nQP))
       !
       call COLLISIONS_cohsex(X(2),Xk,E,k,q,Xw(2),i_kind) 
       !
     endif
     !
     ! CLEAN
     !=======
     if (io_COLLs.or.allocated(TEMP_COLLISIONS_v)) then
       call WF_free(WF)
       call collisions_IO_free()
       deallocate(TEMP_COLLISIONS_v)
       call mem_est("TEMP_COLLISIONS_v")
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
   deallocate(fragment_status)
   call mem_est("fragment_status")
 endif
 !
 contains
   !
   subroutine report_disk_usage(what)
     !
     use com,   ONLY:msg
     use units, ONLY:Gigabyte
     !
     real(SP)     :: disk_space
     character(*) :: what
     !
     disk_space=real(QP_n_states*(QP_nb-SC_bands(1)+1)**2*QP_nk*SP)/Gigabyte
     if (disk_space > 0.05) call msg('s','[COLLISIONS I/O] '//what//' Disk space required [Gb]:',disk_space)  
     !
   end subroutine
   !
end subroutine
