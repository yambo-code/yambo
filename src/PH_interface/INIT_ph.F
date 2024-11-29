!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function INIT_ph(en,q,k,X,Xw,Dip,instr,lnstr,CLOSE_Gs,FINALIZE)
 !
 use drivers,        ONLY:infile_editing,l_em1s,l_acfdt,l_HF_and_locXC,l_col_cut,l_alda_fxc,l_bs_fxc,l_optics,l_bse,l_bss,&
&                         l_chi,l_cohsex,l_life,l_rpa_IP,l_td_hartree,l_tddft,list_dbs,l_td_hf,l_setup,l_sc_run,l_rim,l_rim_w,&
&                         l_real_time,l_ppa,l_lrc_fxc,l_gw0,l_elel_corr,l_elel_scatt,l_elph_corr,l_td_sex,&
&                         l_elph_Hamiltonian,l_elph_scatt,l_elphoton_corr,l_elphoton_scatt,l_em1d,l_eval_collisions,&
&                         l_sc_magnetic,l_sc_electric,l_dipoles,l_pf_fxc,l_nl_optics,l_mpa,l_phel_corr,l_X,l_screen
#if defined  _PHEL
 use PHEL,           ONLY:PH_Q
 use ELPH,           ONLY:PH_W_debye
#endif
#if defined  _ELPH
 use ELPH,           ONLY:elph_nQ,elph_nQ_used
#endif
 !
#include<y_memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::q,k
 type(X_t)       ::X(5)
 type(DIPOLE_t)  ::Dip
 type(w_samp)    ::Xw(5)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE
 !
 if (.not.FINALIZE.and..not.CLOSE_Gs) then
   !
   ! Global Logicals
   !
   call SET_logicals()
   !
 endif
 !
 ! Presets (local)
 !
 if (FINALIZE) then
   call call_INIT_load('Game_Over')
   call initinfio(defs,repfile_unit)
   call ofiles_append(defs=defs)
   close(unit=repfile_unit)
   if(log_as_a_file) close(unit=logfile_unit)
#if defined _YAML_OUTPUT
   call yaml_close_stream(unit=of_yaml_IDs(repfile_index))
   if(log_as_a_file) call yaml_close_stream(unit=of_yaml_IDs(logfile_index))
   call f_lib_finalize()
#endif
   return
 endif
 if (CLOSE_Gs) then
   call call_INIT_load('Close_G_vectors')
   call INIT_barriers( )
   call LOCAL_from_runlevels_to_logicals()
   return
 endif
 !
 ! First vars loading
 !
 call call_INIT_load('create_shadow_vars')
 !
 ! Command line reading
 !
 call INIT_read_command_line(instr,INIT)
 !
 ! DB props listing mode ?
 !
 if (index(instr,'dbpr')>0) then
   list_dbs=.true.
   INIT = 2
   if (log_as_a_file) write (logfile,'(2a)') trim(com_path),'/l_dbs'
   live_timing_is_on=.false.
   write_to_report=.false.
   write_to_log=.true.
 endif
 !
 ! Dump the input file
 !
 if (file_exists(trim(infile))) then
   !
   call infile_dump()
   !
   call INIT_input_file( )
   !
 else if (.not.infile_editing) then
   infile='(none)'
 endif
 !
 call INIT_check_databases(X,Xbsk,qp,Xxcw,Xw,q,k,en,en_sc,Dip,lnstr,instr,INIT,&
&                          io_X_err,io_BS_err,io_BS_Fxc_err,io_QINDX_err,io_ELPH_err,io_SC_E_err)
 !
 if (list_dbs) then
   call msg('s','')
   return
 endif
 !
 ! Runlevels variables activation (Logicals from stdin)
 call LOCAL_from_runlevels_to_logicals()
 !
 ! Given the stding vars here I fix/check the 
 ! corr/scatt logicals
 call LOCAL_scatt_and_correlation_logicals( )
 !
 !Setup on the basis of the DB read/stdin variables
 !Here I can propose values to be written in the input file
 call LOCAL_setup_before_input_IO( )
 !
 ! Scattering/correlation logicals
 !
 !Variables(read from DB files) -> local cache
 !
 !Note that here ('load_defaults') is the latest chance to change
 !a predefined var verbosity and to force its appearance
 !in the input file.
 !
 call call_INIT_load('load_defaults')
 !
 !Input file/local cache -> local cache/Variables
 !
 call call_INIT_load('parser_input_file')
 !
 !RUNLEVELS VARIABLES ACTIVATION (LOGICALS FROM INPUT FILE)
 !
 if (.not.any(rstatus>0)) then
   do i_r=1,nrnlvls
     if (trim(rnlvls(i_r,1))=="setup") rstatus(i_r)=-1
   enddo
 endif
 call LOCAL_from_runlevels_to_logicals()
 call LOCAL_from_runlevels_to_logicals()
 !
 call INIT_activate()
 !
#if defined _RT
 if (check_verbosity("real_time").and.l_real_time) call INIT_DephMatrix()
#endif
 !
 ! Q-points (from input/DB)
 !
 if (l_setup.and.io_QINDX_err/=0) call INIT_q_points( )
 !
 !If qp limits are requested they are added at the end of the input file
 !
 QP_field=any((/l_HF_and_locXC,l_gw0,l_life/)).and..not.l_sc_run.and..not.l_phel_corr
 !
 if (QP_field) then
   !
   ! The QP_state can be read from DB's in DUMP mode or from the input file.
   ! The DB value is used when a new input file is created or when the previous input file
   ! has no QP fields (see inside INIT_QP)
   !
   ! QP_state from DB ?
   !
   if (allocated(QP_state)) call QP_state_table_setup(en)
   !
   call INIT_QP("[GW] QP")
   !
   ! I use it to propose the value in the input file ...
   !
   ! ... but afterword I must deallocate it to use user defined values
   !
   YAMBO_FREE(QP_state)
   YAMBO_FREE(QP_table)
   !
 endif
 !
 ! Are we editing the input file ?
 !
 if (infile_editing) then
   inpfile_unit=12
   open(unit=inpfile_unit,file=trim(infile))
   call initinfio(defs,inpfile_unit)
   close(inpfile_unit)
   call PP_wait()
   return
 endif
 !
 ! Update Logicals (especially the write_to_log)
 !
 call SET_logicals()
 !
 call LOCAL_dipole_bands_ordering()
 !
 ! Report/Log Files
 !
 call INIT_report_and_log_files()
 !
 ! Finalize
 !
 call PP_wait()
 !
 contains
   !
   subroutine call_INIT_load(mode)
     character(*)::mode
     if (mode=='create_shadow_vars') initmode=0
     if (mode=='load_defaults') initmode=1
     if (mode=='Close_G_vectors') initmode=2
     if (mode=='Game_Over') initmode=3
     if (mode=='parser_input_file') initmode=4
     call INIT_load(defs,en,q,k,X,Xw,Dip)
   end subroutine
   !
   subroutine LOCAL_from_runlevels_to_logicals()
     !
     integer     ::i1
     !
     do i1=1,2
       !
       l_elph_Hamiltonian=runlevel_is_on('ElPhHam')
       l_elph_corr=runlevel_is_on('el_ph_corr')
       l_phel_corr=runlevel_is_on('ph_el_corr')
       !
       ! Check if this runlevel is allowed in the
       ! present configuration
       !
       if (i1==1) call INIT_barriers( )
       !
     enddo
     !
   end subroutine LOCAL_from_runlevels_to_logicals
   !
   subroutine LOCAL_dipole_bands_ordering()
     !
     implicit none
     logical :: all_bands,l_local
     !
     call parser('DipBandsALL',all_bands)
     !
     Dip%bands_ordered=.not.all_bands
     l_local=l_sc_run.or.l_real_time.or.l_nl_optics 
     if (l_local)         Dip%bands_ordered=.false.
#if defined _QED
     if (l_elphoton_corr) Dip%bands_ordered=.false.
#endif
     !
   end subroutine LOCAL_dipole_bands_ordering
   !
   subroutine LOCAL_setup_before_input_IO()
     !
     ! After DB reading/stdin logicals I propose here values for the input file
     !
     ! If optics with BS FXC I need to dump on X(3) the F_xc specs
     !
     if (all((/l_bs_fxc,l_optics,l_chi.or.l_bse,io_BS_Fxc_err==0/))) then
       X(3)%ib= BS_bands
       X(3)%ehe=BS_eh_en
       X(3)%q0= BSS_q0
       X(3)%iq= 1
       call W_duplicate(Xxcw,Xw(3))
       call initactivate(2,'XfnQP_E')
     endif
#if defined _ELPH
     elph_nQ_used=elph_nQ
#endif
#if defined _PHEL
     PH_Q=(/1,elph_nQ/)
     QP_G_er=(/0._SP,PH_W_debye/)
#endif
     if (l_gw0.and.l_elel_corr) call initactivate(1,'HF_and_locXC')
     !
     if (any((/l_em1s,l_em1d,l_X,l_optics,l_ppa,l_mpa/)))   call initactivate(1,'dipoles')
     if (l_ppa)    call initactivate(1,'em1d')
     if (l_mpa)    call initactivate(1,'em1d')
     if (l_gw0.and..not.any((/l_ppa,l_cohsex,l_HF_and_locXC,l_mpa,&
&                                     l_elph_corr,l_phel_corr,l_elphoton_corr/))) call initactivate(1,'em1d dipoles')
     if (l_cohsex) call initactivate(1,'em1s dipoles')
     if (l_bss)    call initactivate(1,'optics dipoles bse')
     if (l_bs_fxc) BSS_mode="t"
     !
     if (l_mpa) then
       Xw(5)%n_freqs=4
       Xw(5)%dr=(/0._SP,0.1_SP/) !MPA damping
     endif
     !
     if (l_bse) then
       !
       if (l_alda_fxc) BS_res_K_exchange=.TRUE.
       !
       if (io_X_err(2)==0) then
         if (io_BS_err/=0) BS_n_g_W=X(2)%ng
       else if (io_X_err(4)==0) then
         if (io_BS_err/=0) BS_n_g_W=X(4)%ng
         call initactivate(1,'em1d dipoles ppa')
       endif
       !
     endif
     !
     if (l_gw0.and.l_cohsex) call parser('UseEbands',COHSEX_use_empties)
     !
#if defined _SC
     if (l_sc_magnetic) call initactivate(1,'potential')
     if (l_sc_run.and.io_SC_E_err==0) then
       QP_ng_Sx=SC_ng_Sx
       X(2)%ib=SC_X_bands
       X(2)%ng=SC_X_size
     endif
#endif
#if defined _ELECTRIC
     if (l_sc_electric) call initactivate(1,'dipoles')
#endif
#if defined _OPENMP
     if (l_dipoles) then
       n_threads_DIP=0
     endif
     if (any((/l_optics,l_em1s,l_em1d,l_ppa,l_mpa,l_bse,l_X/))) then
       n_threads_K=0
       n_threads_X=0
       n_threads_DIP=0
     endif
     if (l_HF_and_locXC.or.l_gw0.or.l_cohsex.or.l_sc_run&
&                               .or.l_ppa.or.l_mpa)       n_threads_SE=0
     if (l_real_time)                                     n_threads_RT=0
     if (l_nl_optics)                                     n_threads_NL=0
#endif
#if defined _OPENMP && defined _SC && !defined _RT && !defined _NL
     if (l_eval_collisions)    n_threads_SE=0
#endif
#if defined _OPENMP && defined _SC && defined _RT
     if (l_eval_collisions)    n_threads_RT=0
#endif
#if defined _OPENMP && defined _SC && defined _NL
     if (l_eval_collisions)    n_threads_NL=0
#endif
     !
   end subroutine LOCAL_setup_before_input_IO
   !
   subroutine LOCAL_scatt_and_correlation_logicals( )
     !
     if (l_cohsex) call initactivate(1,'el_el_corr')
#if !defined _ELPH
     if (l_life) call initactivate(1,'el_el_corr')
#endif
     !
     if (io_ELPH_err(1)<0.and.io_ELPH_err(2)<0) call switch_off_runlevel('el_ph_corr ph_el_corr',except=' ')
     if (io_ELPH_err(2)<0                     ) call switch_off_runlevel('el_ph_scatt ph_el_scatt',except=' ')
     !
   end subroutine LOCAL_scatt_and_correlation_logicals
   !
end function
