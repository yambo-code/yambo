!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine NL_driver(E,X,k,q,Dip)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use drivers,        ONLY:l_use_Hxc_collisions
 use stderr,         ONLY:set_real_printed_length
 use com,            ONLY:error,warning
 use QP_m,           ONLY: QP_ng_SH,QP_ng_Sx 
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_t
 use DIPOLES,        ONLY:DIPOLE_t,DIP_alloc,DIP_S
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_Go_indx,H_QP_table_setup
 use R_lattice,      ONLY:bz_samp,qindx_free
 use fields,         ONLY:Efield
 use QP_CTL_m,       ONLY:QP_apply
 use R_lattice,      ONLY:qindx_S
 use nl_optics,      ONLY:n_frequencies,Correlation,IPA,NL_alloc,NL_free,V_bands,eval_dG,NL_J,NL_initial_J, &
&                         NL_estep,NL_er,Integrator,l_use_DIPOLES,NL_P_t,JGM,TDDFT,NL_P, &
&                         E_ext_t,E_tot_t,E_ks_t,E_ks,E_tot,E_ext,freqs_done,n_freqs_todo,l_eval_CURRENT,&
&                         HF,SEX,NL_bands,NL_nbands,WF_sample,V_bands_t,NL_alloc_k_distributed
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_step,RT_nbands,RT_bands,eval_DFT
 use IO_m,           ONLY:io_COLLs
 use interfaces,     ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,     ONLY:PAR_IND_freqs,PAR_IND_freqs_ID,PAR_IND_Xk_ibz
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 !
#include<memory.h>
 !
 ! Notice that E is never changed!
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X(2)
 type(DIPOLE_t)    :: Dip
 !
 logical, external :: stop_now
 ! 
 ! Work Space
 !
 integer           :: i_f,i1,N_sample_size,wf_ng_save,io_err
 logical           :: l_dynamics_is_over
 real(SP)          :: Time
 !
 ! In Non-Linear NL_bands always equal to RT_bands
 ! ==============================================
 !
 NL_nbands=NL_bands(2)-NL_bands(1)+1
 !
 RT_bands =NL_bands
 RT_nbands=NL_nbands
 !
 call section('+','Initialization')
 !#################################
 !
 call set_real_printed_length(f_length=10,g_length=10)  
 !
 ! Initialize
 !=========
 call NL_initialize(E,k,q)
 !
 ! Switch 
 !========
 call XC_switch()
 !
 ! Dipoles & WF dimensions
 !=========================
 call WF_and_dipole_dimensions(X)
 !
 ! Deallocate qindx_S (used only in previous subroutine)
 ! ========================
 YAMBO_FREE(qindx_S)
 !
 ! QP dimensions
 !================
 call H_QP_table_setup(E)
 !
 ! Non_Linear parallization
 ! ===================================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.,Dip=Dip,X=X(1))
 call PARALLEL_global_indexes(E,k,q,"Non_Linear",Dip=Dip,X=X(1))
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,CLEAN_UP=.TRUE.)
 call PARALLEL_WF_index( )
 !
 !Load Dipole after the NL parallelization
 ! ========================================
 call DIPOLE_dimensions(E,Dip,NL_bands,(/0._SP,0._SP,0._SP/))
 call DIPOLE_IO(k,E,Dip,'read ',io_err,'NL')
 if(io_err/=0) call error("Error in DIPOLES! ")
 !
 ! Free memory: qindx_* and unused DIPOLES
 ! =====================================================
 call qindx_free()
 !
 call DIP_alloc('P_square')
 if(l_use_DIPOLES) then
   call DIP_alloc('DIP_S')
 else
!    call DIP_alloc('DIP_R')
   if(.not.l_eval_CURRENT) then
     call DIP_alloc('DIP_R')
     call DIP_alloc('DIP_v')
   endif
 endif
 !
 ! WFs loading
 !=============
 !
 wf_ng_save =wf_ng
 if(eval_DFT) then
   wf_ng      =max(QP_ng_Sx,QP_ng_SH)
 else
   wf_ng      =QP_ng_SH
 endif
 WF_Go_indx =1
 !
 if(io_COLLs) then
   !      
   ! check if they exist      
   !
   call COLLISIONS_load(.true.)
   !
   if(.not.l_use_Hxc_collisions .and. (Correlation==SEX.or.Correlation==HF)) &
&    call error(' HF / SEX need Collisions. Cannot be computed on the fly.')
   !
   if(COLLISIONS_have_HARTREE) then
     if(Correlation==JGM) then
        call warning('Collisions not compatible with JGM, turned off')
        l_use_Hxc_collisions=.false.
     elseif(Correlation==TDDFT) then
        call warning('With Collisions TDDFT is approximated to the linear contribution!')
     endif
   endif
   !
 endif
 !
 if(Correlation/=IPA.and..not.COLLISIONS_have_HARTREE)  then
   call WF_load(WF,wf_ng,WF_Go_indx,(/1,NL_bands(2)/),(/1,k%nibz/),space='R',title='-NL')  
 endif
 !
 ! QP-corrections 
 !================
 call QP_apply(NL_bands,E,k,"G",msg_fmt='s')
 !
 ! Load Collisions if necessary
 ! ============================
 !
 if(l_use_Hxc_collisions.and.io_COLLs) then
   !
   eval_dG=.true.
   !
   call COLLISIONS_load(.false.)
   !
 endif
 !
 ! Allocation
 ! ==================
 call NL_alloc(E) 
 call NL_alloc_k_distributed(E)
 !
 ! Initialization
 ! ==================
 call NL_start_and_restart(E,k,Dip)
 !
 ! Check if calculation was already done
 ! =====================================
 if(n_freqs_todo==0) return
 !
 ! Initialize IO
 ! ==================
 call NL_databases_IO(0,'write')
 !
 ! Start-Up & Timing
 !===================
 call live_timing('[NL] Frequencies :',PAR_IND_freqs%n_of_elements(PAR_IND_freqs_ID+1)*NE_steps)
 !
 do i_f=1,n_frequencies
   !
   if(.not.PAR_IND_freqs%element_1D(i_f)) cycle
   !
   if(freqs_done(i_f)) then
     call live_timing(steps=NE_steps)
     cycle
   endif
   !
   ! Re-initialization
   ! ================
   V_bands=cZERO
   forall(i1=1:E%nbf) V_bands(i1,i1,:,:)=cONE
   if(WF_sample%ison) V_bands_t = cZERO
   !
   ! Efield
   ! ================
   Efield(1)%frequency=NL_er(1)+real(i_f-1,SP)*NL_estep
   !
   l_dynamics_is_over=.false.
   !
   NE_i_time=0
   N_sample_size=0
   !
   do while(.not.l_dynamics_is_over)
     !
     NE_i_time=NE_i_time+1
     !
     Time=real(NE_i_time-1,SP)*RT_step
     !
     call NL_Hamiltonian(E,k,q,X(1),Time,NE_i_time,V_bands)
     !
     ! I evaluate current after the Hamiltonian because I need
     ! time-dependent overlaps Sm1_plus, and Sm1_mins
     !
     if(l_eval_CURRENT) then
       call NL_current(k,E,V_bands,NL_J)
       NL_J=NL_J-NL_initial_J
     endif
     !
     ! Store Variables
     !
     NL_P_t (NE_i_time,:)=real(NL_P(1:3),SP)
     E_ext_t(NE_i_time,:)=E_ext
     E_tot_t(NE_i_time,:)=E_tot
     E_ks_t (NE_i_time,:)=E_ks
     !
     call NL_output(NE_i_time,i_f) 
     !
     call NL_Integrator(E,k,q,X(1),Integrator,NE_i_time,V_bands)
     !
     call live_timing(steps=1)
     !
     l_dynamics_is_over= (NE_i_time>=NE_steps) .or. stop_now(.false.)
     !
   enddo
   !
   call NL_databases_IO(i_f,'write')
   !
   if(stop_now(.false.)) exit
   !
 enddo
 !
 call live_timing()
 !
 ! Deallocation
 ! ==========================
 !
 wf_ng     = wf_ng_save
 !
 call WF_free(WF)
 call NL_free()
 call DIP_alloc('DIP_S')
 call DIP_alloc('DIP_R')
 call DIP_alloc('DIP_v')
 !
 call set_real_printed_length()
 !
end subroutine NL_driver
