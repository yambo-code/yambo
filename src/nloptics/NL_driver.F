!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
subroutine NL_driver(E,X,Xw,k,q,Dip)
 !
 use pars,           ONLY:pi,SP,cZERO,cONE,schlen
 use vec_operate,    ONLY:axis_rotation
 use com,            ONLY:msg
 use drivers,        ONLY:l_use_Hxc_collisions
 use QP_m,           ONLY: QP_ng_SH,QP_ng_Sx 
 use LIVE_t,         ONLY:live_timing 
 use electrons,      ONLY:levels,n_sp_pol
 use collision_ext,  ONLY:COLLISIONS_CV_only,HXC_collisions 
 use X_m,            ONLY:global_gauge,X_t
 use DIPOLES,        ONLY:DIPOLE_t,DIP_S
 use ALLOC,          ONLY:DIPOLE_ALLOC_elemental,DIPOLE_ALLOC_global
 use wave_func,      ONLY:WF,wf_ng
 use interfaces,     ONLY:WF_load,WF_free
 use hamiltonian,    ONLY:WF_Go_indx,H_QP_table_setup,H_potential
 use R_lattice,      ONLY:bz_samp,qindx_free
 use fields,         ONLY:Efield
 use frequency,      ONLY:w_samp
 use QP_CTL_m,       ONLY:QP_apply
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:n_frequencies,Correlation,NL_alloc,NL_free,V_bands,eval_dG,NL_J,NL_initial_J,NL_initial_versor, &
&                         NL_estep,NL_er,Integrator,l_use_DIPOLES,NL_P_t,JGM,TDDFT,NL_P,loop_on_angles,n_angles,&
&                         E_ext_t,E_tot_t,E_ks_t,E_ks,E_tot,E_ext,runs_done,n_runs_todo,l_eval_CURRENT,NL_rot_axis,&
&                         HF,SEX,NL_bands,NL_nbands,NL_alloc_k_distributed,NL_J_t,n_runs,loop_on_frequencies, &
&                         l_pump_and_probe,eval_OSCLL,l_test_OSCLL,LSEX,LHF,Sigma_SEX_EQ,Wk_EQ
 use real_time,      ONLY:RT_step,NE_steps,NE_i_time,RT_nbands,RT_bands,eval_DFT,eval_HARTREE
 use IO_m,           ONLY:io_COLLs
 use parallel_int,   ONLY:PARALLEL_global_indexes,PARALLEL_WF_distribute,PARALLEL_WF_index
 use parallel_m,     ONLY:PAR_IND_freqs,PAR_IND_freqs_ID,PAR_IND_Xk_ibz,PAR_Xk_ibz_index
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 use collision_ext,  ONLY:ng_oscll,OSCLL,W
 !
#include<y_memory.h>
 !
 ! Notice that E is never changed!
 !
 type(levels)      :: E
 type(bz_samp)     :: k,q
 type(X_t)         :: X(2)
 type(DIPOLE_t)    :: Dip
 type(w_samp)      :: Xw(2)
 !
 logical, external :: stop_now
 ! 
 ! Work Space
 !
 integer           :: i_f,i1,i_sp_pol,wf_ng_save,io_err,ik,ik_mem
 character(schlen) :: l_message
 logical           :: l_dynamics_is_over
 real(SP)          :: Time,angle
 !
 ! In Non-Linear NL_bands always equal to RT_bands
 ! ==============================================
 !
 NL_nbands=NL_bands(2)-NL_bands(1)+1
 !
 RT_bands =NL_bands
 RT_nbands=NL_nbands
 !
 call section('*','NL Initialization')
 !#################################
 !
 ! QP-corrections 
 !================
 call QP_apply(NL_bands,E,k,"G",msg_fmt='s')
 !
 ! Initialize
 !=========
 call NL_initialize(E,k)
 !
 ! Switch 
 !========
 call XC_potentials(H_potential)
 call XC_additional_SC_potentials()
 !
 ! Dipoles & WF dimensions
 !=========================
 call WF_and_dipole_dimensions(X(1),k)
 !
 ! QP dimensions
 !================
 call H_QP_table_setup(E)
 !
 ! Non_Linear parallization
 ! ===================================
 call setup_NL_parallelization()
 !
 !Load Dipole after the NL parallelization
 ! ========================================
 call DIPOLE_dimensions(E,Dip,NL_bands,(/0._SP,0._SP,0._SP/))
 call DIPOLE_IO(k,E,Dip,'read ',io_err,'NL')
 if(io_err/=0) call error("Error in DIPOLES! ")
 !
 ! Symmetrize dipoles if old-symmetries are present
 ! =================================================
 call DIPOLE_symmetrize(k)
 !
 ! Generate bare_qpg if required
 !=============================
 if(eval_HARTREE.or.CORRELATION==JGM.or.CORRELATION==LSEX.or.CORRELATION==LHF) &
&  call col_driver(maxval((/wf_ng,QP_ng_Sx,QP_ng_SH/)),q)
 !
 ! Free memory: qindx_* and unused DIPOLES
 ! =====================================================
 if(.not.eval_OSCLL) then
   call qindx_free('ALL')
 else
   call qindx_free('X B C')
 endif
 !
 call DIPOLE_ALLOC_elemental('P_square')
 if(l_use_DIPOLES) then
   call DIPOLE_ALLOC_elemental('DIP_S')
 else
   if(.not.l_eval_CURRENT) then
     call DIPOLE_ALLOC_elemental('DIP_v')
   endif
 endif
 !
 ! WFs loading
 !=============
 !
 wf_ng_save =wf_ng
 if(eval_DFT) then
   wf_ng      =max(QP_ng_Sx,QP_ng_SH)
 elseif(eval_OSCLL) then
   wf_ng      =max(wf_ng,QP_ng_Sx,QP_ng_SH)
 else
   wf_ng      =QP_ng_SH
 endif
 WF_Go_indx =1
 !
 if(eval_OSCLL.and..not.l_test_OSCLL) then
    io_COLLs=.false.
    HXC_collisions%N=0
 endif
 !
 if(io_COLLs) then
   !      
   ! check if they exist      
   !
   call COLLISIONS_load(.true.)
   !
   if(l_test_OSCLL.and..not.l_use_Hxc_collisions) &
&    call error("You should calculate collisions before the testing with: yambo_nl -e -v f/sex")          
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
     eval_HARTREE=.FALSE.
   endif
   !
   if(l_use_Hxc_collisions.and..not.COLLISIONS_have_HARTREE) eval_HARTREE=.TRUE.
   !     
   if(COLLISIONS_CV_only) call msg('rs','Collisions contains C<->V ONLY scatterings')
   !
 endif
 !
 if(eval_OSCLL) then
   !
   call setup_OSCLL_parallelization()
   !
   ! Evaluate oscillators if not present
   call OSCLL_eval(q, k, E)
   !
   ! Load the LSEX potential W(q,G,G') or V(q,G,G')
   call LSEX_potential(q, X(2), Xw(2))
   !
   ! Load oscillators
   !
   call setup_NL_parallelization()
   !
   call OSCLL_load(q,k)
   !
 endif
 !
 if(eval_HARTREE.or.eval_DFT) then
   call WF_load(WF,wf_ng,WF_Go_indx,(/1,NL_bands(2)/),(/1,k%nibz/),space='R',title='-NL')  
 endif
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
 if(l_test_OSCLL) then
   call NL_test_collisions(k,q)
   return
 endif
 !
 ! Initialization
 ! ==================
 call section('*','NL Start and Restart')
 call NL_start_and_restart(E,k,Dip)
 !
 !
 if(eval_OSCLL) then
   !      
   ! If I use oscillators I store the equilibrium part of long-range SEX
   !
   Sigma_SEX_EQ=cZERO
   !
   do ik=1,QP_nk
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do i_sp_pol=1,n_sp_pol
       call OSCLL_compose_vbands(V_bands,Sigma_SEX_EQ(:,:,i_sp_pol,ik_mem),k,q,E,ik,i_sp_pol) 
     enddo
   enddo
   !
 endif
 !
 ! Store Equilibrium Wk
 if(.not.l_use_DIPOLES.and..not.trim(global_gauge)=='velocity') then
   do ik=1,QP_nk
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     ik_mem=PAR_Xk_ibz_index(ik)
     do i_sp_pol=1,n_sp_pol
       call Build_W_operator(E,k,ik,i_sp_pol,V_bands(:,:E%nbf(i_sp_pol),:,i_sp_pol), & 
&                            Wk_EQ(:,:,:,i_sp_pol,ik_mem))
     enddo
   enddo
 endif
 !
 !
 ! Check if calculation was already done
 ! =====================================
 if(n_runs_todo==0) return
 !
 ! Initialize IO
 ! ==================
 call NL_databases_IO(0,'write')
 !
 ! Start-Up & Timing
 !===================
 if(loop_on_angles) then
   l_message="[NL] Angles "
 elseif (loop_on_frequencies) then
   l_message="[NL] Frequencies "
 elseif (l_pump_and_probe) then
   l_message="[NL] Time-steps "
 else
   call error("Wrong input: use pump-probe mode 'yambo_nl -u p' or set a number of frequencie/angles with 'yambo_nl -u n'")
 endif
 call live_timing(l_message,PAR_IND_freqs%n_of_elements(PAR_IND_freqs_ID+1)*NE_steps)
 !
 do i_f=1,n_runs
   !
   if(.not.PAR_IND_freqs%element_1D(i_f)) cycle
   !
   if(runs_done(i_f)) then
     call live_timing(steps=NE_steps)
     cycle
   endif
   !
   ! Re-initialization
   ! ================
   V_bands=cZERO
   do i_sp_pol=1,n_sp_pol
     forall(i1=1:E%nbf(n_sp_pol)) V_bands(i1,i1,:,i_sp_pol)=cONE
   enddo
   !
   ! Update Efield
   ! ================
   if(loop_on_frequencies) Efield(1)%frequency=NL_er(1)+real(i_f-1,SP)*NL_estep
   if(loop_on_angles)      then
     angle           =2.0*pi/real(n_angles,SP)*(i_f-1.0)
     Efield(1)%versor=axis_rotation(NL_rot_axis, NL_initial_versor, angle)
   endif
   !
   l_dynamics_is_over=.false.
   !
   NE_i_time=0
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
     NL_J_t (NE_i_time,:)=real(NL_J(1:6),SP)
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
 call DIPOLE_ALLOC_global( )
 !
 contains
 !
 subroutine setup_OSCLL_parallelization
   implicit none
   !
   ! Restore original number of bands
   !
   X(1)%ib       =(/NL_bands(1),NL_bands(2)/)
   !
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
   !
   call PARALLEL_global_indexes(E,k,q,"Oscillators",X=X(1))
   !
 end subroutine setup_OSCLL_parallelization
 !
 subroutine setup_NL_parallelization()
   !
   implicit none
   !
   ! Restore original number of bands
   !
   X(1)%ib       =(/NL_bands(1),NL_bands(2)/)
   !
   call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
   call PARALLEL_WF_distribute(CLEAN_UP=.TRUE.)
   ! 
   call PARALLEL_global_indexes(E,k,q,"Non_Linear",Dip=Dip,X=X(1))
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,CLEAN_UP=.TRUE.)
   !
 end subroutine setup_NL_parallelization

 !
end subroutine NL_driver
