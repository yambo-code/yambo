!
!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AMCA
!
subroutine NL_start_and_restart(E,k,Dip)
 !
 ! This subroutine initialize some variables and 
 ! restart from a previous NL calculation
 !
 ! [1] Initialize the Vbands and potentials
 ! [2] Build Ho
 ! [3] RESTART from a previous run
 ! [4] Initialize the bare Hartree and XC
 ! [5] Initialize the fields
 !
 use pars,           ONLY:cZERO,rZERO,cONE,SP
 use com,            ONLY:msg
 use units,          ONLY:AUT2FS
 use electrons,      ONLY:levels,n_sp_pol
 use real_time,      ONLY:rho_reference,RT_time_status,NE_i_time,RT_step,RT_dyn_step,NE_steps,&
&                         eval_HARTREE,eval_DFT
 use RT_control,     ONLY:Vbands_RT_IO_t,OBS_RT_IO_t,OUTPUT_RT_IO_t,CACHE_OBS_INTERVAL_time,&
&                         N_RT_databases,NE_i_start_time,RT_return_db_ID,RT_DB_ID,RT_DB_name,&
&                         RT_control_alloc,RT_control_free
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_t
 use DIPOLES,        ONLY:DIPOLE_t
 use hamiltonian,    ONLY:V_hartree_sc,V_xc_sc,H_nl_sc,rho_n
 use X_m,            ONLY:X_t 
 use xc_functionals, ONLY:V_xc,XC_potential_driver
 use QP_m,           ONLY:QP_nk
 use global_XC,      ONLY:WF_xc_functional,WF_kind
 use fields,         ONLY:A_tot,A_ext,A_vecpot_reset
 use parallel_m,     ONLY:master_cpu
 use nl_optics,      ONLY:Correlation,IPA,E_full,full_rho,V_bands,TDDFT,n_runs_todo,NL_LRC_alpha,NL_bands, &
&                         V_xc_0,NL_initial_P,l_use_DIPOLES,n_runs,runs_done,JGM,E_xc_0,I_relax,l_use_DIPOLES,&
&                         VAL_BANDS,l_eval_CURRENT,NL_initial_J,NL_P,l_pump_and_probe
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:OP_RD_CL,NONE,file_is_present,VERIFY,DUMP,REP
 use parallel_int,   ONLY:PP_wait,PP_redux_wait
 use stderr,         ONLY:intc
 use collision_ext,  ONLY:COLLISIONS_have_HARTREE
 !
#include<y_memory.h>
 !
 type(levels)      :: E
 type(bz_samp)     :: k
 type(DIPOLE_t)    :: Dip
 !type(X_t)         :: X
 !
 ! Working space
 !
 logical :: RESTART
 integer :: i1,ib,i_f,i_sp_pol,i_db
 integer :: ID,io_NonLinear,COM_MODE,io_err(N_RT_databases)
 integer, external :: io_NL,io_RT_components
 !
 !
 ! Initialize the Gf and potentials
 !==================================
 if(allocated(rho_reference))   rho_reference=rZERO
 if(allocated(full_rho))        full_rho    =rZERO
 if(allocated(V_xc_sc))         V_xc_sc     =rZERO
 if(allocated(V_hartree_sc))    V_hartree_sc=rZERO
 H_nl_sc           =cZERO
 I_relax           =cZERO
 V_bands           =cZERO
 !
 if(NL_bands(2)<=minval(E%nbf).or.NL_bands(1)>maxval(E%nbf)) &
     & call error(" Wrong bands range!")
 !
 do i_sp_pol=1,n_sp_pol
   forall(i1=1:E%nbf(n_sp_pol)) V_bands(i1,i1,:,i_sp_pol)=cONE
 enddo
 !
 if(Correlation/=IPA.and..not.COLLISIONS_have_HARTREE.and.(eval_HARTREE.or.eval_DFT)) then
   !  
   call NL_build_valence_bands(E,V_bands,VAL_BANDS,1)
   call el_density_vbands(E,k,full_rho,VAL_BANDS)
   !
   call NL_build_valence_bands(E,V_bands,VAL_BANDS,NL_bands(1))
   call el_density_vbands(E,k,rho_reference,VAL_BANDS)
   !
   rho_n=full_rho
   !
 endif
 !
 ! Fields
 !=====================
 call A_vecpot_reset(A_tot)
 call A_vecpot_reset(A_ext)
 !
 if(Correlation==TDDFT.and..not.COLLISIONS_have_HARTREE) then
   !
   ! Calculate V_xc
   !
   call XC_potential_driver(E,k,WF_kind,WF_xc_functional,1)
   V_xc_0=V_xc
   !       
 endif
 !
 if(Correlation==JGM) then
   NL_P=cZERO
   call EXC_macroscopic_JGM(E,NL_P(1:3),rho_n,E_xc_0,V_xc_0)
   call msg('sr',' JGM initial alpha ',NL_LRC_alpha)
 endif
 !
 ! Apply QP Corrections
 ! ====================
 do ib=NL_bands(1),NL_bands(2)
   E_full(ib,1:QP_nk,1:n_sp_pol)=E%E(ib,1:QP_nk,1:n_sp_pol)
 enddo
 !
 do ib=1,NL_bands(1)-1
   E_full(ib,1:QP_nk,1:n_sp_pol)=E_full(ib,1:QP_nk,1:n_sp_pol) &
& -E_full(NL_bands(1),1:QP_nk,1:n_sp_pol)+E%E(NL_bands(1),1:QP_nk,1:n_sp_pol)
 enddo
 !
 ! Evaluate Initial Polarization
 ! ====================================
 !
 if(.not.l_use_DIPOLES) then
   call POLARIZATION_output(E,k,Dip)
 endif
 !
 call DIP_polarization(E,k,V_bands,NL_initial_P(4:6))
 !
 if(.not.l_use_DIPOLES) then
   !
   ! This subroutine update also the Sm1_plus,Sm1_minus and All_S_det matrices
   do i_sp_pol=1,n_sp_pol
     call Berry_Polarization_NEQ(E,k,i_sp_pol,V_bands(:,:E%nbf(i_sp_pol),:,i_sp_pol),NL_initial_P(1:3))
   enddo
   !
 else
   NL_initial_P(1:3)=NL_initial_P(4:6)
 endif
 !
 if(l_eval_CURRENT) call NL_current(k,E,V_bands,NL_initial_J)
 !
 call msg('r','')
 call msg('r',' * * * Non-linear dynamics parameters * * * ')
 call msg('r','============================================')
 call msg('r ',' Initial Polarization      ',real(NL_initial_P(1:3)))
 call msg('r ',' Initial <Dip_x/y/z>       ',real(NL_initial_P(4:6)))
 if(l_eval_CURRENT) then
   call msg('r ',' Initial Current         ',real(NL_initial_J(1:3)))
   call msg('r ',' Initial <v>=<[Hloc,r]>  ',real(NL_initial_J(4:6)))
 endif
 !
 ! Restart
 ! ====================================
 YAMBO_ALLOC(runs_done,(n_runs))
 runs_done=.false.
 !
 RESTART=.FALSE.
 !
 if (.not.l_pump_and_probe) then
   !
   if(master_cpu) then
     !
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID)
     io_NonLinear=io_NL("Nonlinear",ID)
     !
     if(io_NonLinear==0) then
       do i_f=1,n_runs
          if(file_is_present('Nonlinear_fragment_'//intc(i_f))) runs_done(i_f)=.true.
       enddo
     endif
     !
   endif
   !
   call PP_redux_wait(runs_done,imode=3)
   !
 else
   !
   NE_i_time=0
   !
   call msg('s','Checking for RESTART in pump and probe mode ')
   !
   ! Perform a first I/O to check which DBs are present.
   !=====================================================
   do i_db=1,N_RT_databases
     COM_MODE=NONE
     if (i_db==RT_return_db_ID("ANY_Vb")) COM_MODE=REP
     call io_control(ACTION=OP_RD_CL,COM=COM_MODE,SEC=(/1/),MODE=VERIFY,ID=RT_DB_ID(i_db))
     io_err(i_db)=io_RT_components(trim(RT_DB_name(i_db)),RT_DB_ID(i_db))
   enddo
   !
   ! To restart I need:
   !
   !  1. The Vbands -> wave-functions
   !  2. The OBSERVABLES -> RT_IO_t
   !
   RESTART = io_err(RT_return_db_ID("V_bands"))==0.and.io_err(RT_return_db_ID("OBSERVABLES"))==0
   !
 endif
 !
 n_runs_todo=n_runs-count(runs_done)
 if(n_runs_todo/=n_runs) call msg('s','RESTART, remaining frequencies/angles ',n_runs_todo)
 !
 if (l_pump_and_probe.and.RESTART) then
   !
   call msg('s','RESTART in pump and probe mode ')
   !
   do i_db=1,N_RT_databases
     call RT_RESTART_database_copy( trim(RT_DB_name(i_db)) )
   enddo
   !
   call PP_wait()
   !
   do i_db=1,N_RT_databases
     if (i_db==RT_return_db_ID("V_bands_K_section")) cycle
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2/),MODE=DUMP,ID=RT_DB_ID(i_db))
     io_err(i_db)=io_RT_components(trim(RT_DB_name(i_db)),RT_DB_ID(i_db))
   enddo
   !
   NE_i_start_time=NE_i_time
   !
   call RT_control_free("all")
   call RT_control_alloc("all")
   !
   YAMBO_FREE(RT_TIME_status)
   !
   ! I use G_lesser(:,:,:) to reconstruct everything
   ! Note that the databases must be loaded at the RESTART time (NE_time)
   !
   i_db=RT_return_db_ID("V_bands_K_section")
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),MODE=DUMP,ID=RT_DB_ID(i_db))
   io_err(i_db)=io_RT_components('V_bands_K_section',RT_DB_ID(i_db))
   !
   call RT_IO_type_time_steps("RESTART")
   !
   call msg( 'nr','---RESTART report---')
   !
   call msg('s','[RESTART] Number of steps done         ',real(NE_i_time)/real(NE_steps)*100._SP,"[o/o]")
   call msg('r','Number of steps done              ',real(NE_i_time)/real(NE_steps)*100._SP,"[o/o]")
   call msg('r','Time reached                      ',real(NE_i_time-1)*RT_step*AUT2FS,"[fs]")
   call msg('r','Time step                         ',RT_dyn_step*AUT2FS,"[fs]")
   !
   !call msg('r',  'dT Update Interval                ',NE_time_step_update_jump*AUT2FS,"[fs]")
   !call msg('r',  'Lifetimes inter/intr times        ',RT_life_extrap_times*AUT2FS,"[fs]")
   call msg('r',  'T cache step  of J,P,M            ',CACHE_OBS_INTERVAL_time*AUT2FS,"[fs]")
   call msg('r',  'T between I/O of J,P,M            ',OBS_RT_IO_t%INTERVAL_time*AUT2FS,"[fs]")
   !call msg('r',  '          I/O of carriers         ',CARR_RT_IO_t%INTERVAL_time*AUT2FS,"[fs]")
   call msg('r',  '          I/O of   WFs            ',Vbands_RT_IO_t%INTERVAL_time*AUT2FS,"[fs]")
   call msg('r',  '               outputs            ',OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS,"[fs]")
   !
   NE_i_time=NE_i_time-1
   !
 endif
 !
end subroutine NL_start_and_restart

