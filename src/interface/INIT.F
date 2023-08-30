!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function INIT(en,q,k,X,Xw,Dip,instr,lnstr,CLOSE_Gs,FINALIZE)
 !
 ! INIT =-2  : Unsupported Runlevel(s) combination
 ! INIT =-1  : Missing CORE DBs
 ! INIT = 0  : everything is OK. Job continues.
 ! INIT = 1  : input file editing mode
 ! INIT = 2  : DB listing mode
 !
 use drivers,        ONLY:infile_editing,l_em1s,l_acfdt,l_HF_and_locXC,l_col_cut,l_alda_fxc,l_bs_fxc,l_optics,l_bse,l_bss,&
&                         l_chi,l_cohsex,l_life,l_rpa_IP,l_td_hartree,l_tddft,list_dbs,l_td_hf,l_setup,l_sc_run,l_rim,l_rim_w,&
&                         l_real_time,l_ppa,l_lrc_fxc,l_gw0,l_elel_corr,l_elel_scatt,l_elph_corr,l_td_sex,&
&                         l_elph_Hamiltonian,l_elph_scatt,l_elphoton_corr,l_elphoton_scatt,l_em1d,l_eval_collisions,&
&                         l_sc_magnetic,l_sc_electric,l_dipoles,l_pf_fxc,l_nl_optics,l_mpa,l_phel_corr,l_X,l_screen
#if defined _SCALAPACK
 use drivers,        ONLY:l_slk_test
#endif
 use pars,           ONLY:SP
 use parser_m,       ONLY:parser
 use global_XC,      ONLY:WF_exx_fraction,WF_exx_screening
 use electrons,      ONLY:levels,E_reset
 use frequency,      ONLY:w_samp,W_duplicate,W_reset
 use it_tools,       ONLY:it,infile_dump,ofiles_append,switch_off_runlevel,check_verbosity,runlevel_is_on
 use it_m,           ONLY:initdefs,rstatus,initmode,infile,nrnlvls,rnlvls
 use parallel_int,   ONLY:PP_wait
 use com,            ONLY:msg,repfile_unit,inpfile_unit,com_path,write_to_report
 use vec_operate,    ONLY:v_norm
 use LIVE_t,         ONLY:live_timing_is_on
 use QP_m,           ONLY:QP_t,QP_state,QP_reset,QP_table,COHSEX_use_empties,QP_G_er
 use X_m,            ONLY:X_t,X_duplicate,X_reset,Chi_mode,i_X_kind_existing_DB
 use DIPOLES,        ONLY:DIPOLE_t,DIPOLES_reset
 use stderr,         ONLY:write_to_log,log_as_a_file,logfile,logfile_unit,STRING_same
 use R_lattice,      ONLY:q0_def_norm,bz_samp,bz_samp_reset
 use BS,             ONLY:BS_bands,BS_n_g_W,BS_eh_en,BSE_mode,BSK_mode,BS_K_coupling,BS_res_K_corr,BS_res_K_exchange,&
&                         BS_W_is_diagonal,BS_cpl_K_corr,BS_cpl_K_exchange
 use BS_solvers,     ONLY:BSS_mode,BSS_q0
#if defined  _PHEL
 use PHEL,           ONLY:PH_Q
 use ELPH,           ONLY:PH_W_debye
#endif
#if defined  _ELPH
 use ELPH,           ONLY:elph_nQ,elph_nQ_used
#endif
#if defined _SC
 use QP_m,           ONLY:QP_ng_Sx
 use SC,             ONLY:SC_ng_Sx,SC_X_bands,SC_X_size
#endif
#if defined _OPENMP
 use openmp,         ONLY:n_threads_X,n_threads_DIP,n_threads_SE,n_threads_RT,n_threads_K,n_threads_NL
#endif
#if defined _YAML_OUTPUT
 use pars,           ONLY:logfile_index,repfile_index
 use com,            ONLY:of_yaml_IDs
 use yaml_output,    ONLY:yaml_close_stream
#endif
 !
#include<memory.h>
 !
 type(levels)    ::en
 type(bz_samp)   ::q,k
 type(X_t)       ::X(5)
 type(DIPOLE_t)  ::Dip
 type(w_samp)    ::Xw(5)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE,QP_field
 !
 ! Work Space
 !
 type(initdefs)   ::defs
 logical          ::dummy_logical
 !
 ! I/O related
 !
 type(X_t)        ::Xbsk
 type(QP_t)       ::qp
 type(w_samp)     ::Xxcw
 type(levels)     ::en_sc
 integer          :: io_X_err(5),io_BS_err,io_BS_Fxc_err,io_QINDX_err,io_ELPH_err(2),io_SC_E_err,i_r
 !
 logical, external:: file_exists
 !
 ! Presets
 !
 INIT = 0
 !
 if (.not.FINALIZE.and..not.CLOSE_Gs) then
   !
   ! Global Logicals
   !
   call SET_logicals()
   !
   call E_reset(en)
#if defined _SC
   call E_reset(en_sc)
#endif
   call bz_samp_reset(k)
   call bz_samp_reset(q)
   call W_reset(Xw(1),damping=0.001_SP)
   call W_reset(Xw(2),damping=0.001_SP)
   call W_reset(Xw(3))
   call W_reset(Xw(4))
   call W_reset(Xw(5))
   call X_reset(X(1),type=1)
   call X_reset(X(2),type=2)
   call X_reset(X(3),type=3)
   call X_reset(X(4),type=4)
   call X_reset(X(5),type=5)
   Xw(2)%n_freqs=1
   Xw(4)%n_freqs=2
   call DIPOLES_reset(Dip)
 endif
 !
 ! Presets (local)
 !
 call QP_reset(qp)
 call W_reset(Xxcw)
 call X_reset(Xbsk)
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
 !Runlevels variables activation (Logicals from stdin)
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
 call LOCAL_after_input_IO()
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
       l_setup=runlevel_is_on('setup')
       l_screen=runlevel_is_on('screen')
       l_optics=runlevel_is_on('optics')
       l_chi=runlevel_is_on('chi')
       l_bse=runlevel_is_on('bse')
       l_bss=runlevel_is_on('bss')
       l_tddft=runlevel_is_on('tddft')
       l_X=runlevel_is_on('Xx')
       l_em1d=runlevel_is_on('em1d')
       l_em1s=runlevel_is_on('em1s')
       l_ppa=runlevel_is_on('ppa')
       l_mpa=runlevel_is_on('mpa')
       l_cohsex=runlevel_is_on('cohsex')
       l_dipoles=runlevel_is_on('dipoles')
       l_HF_and_locXC=runlevel_is_on('HF_and_locXC')
       l_gw0=runlevel_is_on('gw0')
       l_life=runlevel_is_on('life')
       l_rim=runlevel_is_on('rim_cut')
       l_rim_w=runlevel_is_on('rim_w')
       l_col_cut=l_rim.or.l_rim_w
       l_acfdt=runlevel_is_on('acfdt')
#if defined _SCALAPACK
       l_slk_test =runlevel_is_on('slktest')
#endif
#if defined _RT
       l_real_time=runlevel_is_on('negf')
       l_elel_scatt =runlevel_is_on('el_el_scatt')
       l_elph_scatt=runlevel_is_on('el_ph_scatt')
       l_elphoton_scatt=runlevel_is_on('el_photon_scatt')
#endif
#if defined _SC && !defined _RT
       l_sc_run=runlevel_is_on('scrun')
       l_sc_magnetic=runlevel_is_on('magnetic')
       l_sc_electric=runlevel_is_on('electric')
#endif
#if defined _SC || defined _RT
       l_eval_collisions=runlevel_is_on('collisions')
#endif
#if defined _NL
       l_nl_optics=runlevel_is_on('nloptics')
#endif
#if defined  _ELPH || defined _PHEL
       l_elph_Hamiltonian=runlevel_is_on('ElPhHam')
       l_elph_corr=runlevel_is_on('el_ph_corr')
       l_phel_corr=runlevel_is_on('ph_el_corr')
#endif
#if defined  _QED
       l_elphoton_corr=runlevel_is_on('el_photon_corr')
#endif
       l_elel_corr    =runlevel_is_on('el_el_corr')
       !
       ! Check if this runlevel is allowed in the
       ! present configuration
       !
       if (i1==1) call INIT_barriers( )
       !
     enddo
     !
     ! Setup logicals which are not runlevels
     !
     l_rpa_IP     = STRING_same(trim(Chi_mode),"IP").or.STRING_same(trim(BSK_mode),"IP")
     l_td_hartree = STRING_same(trim(Chi_mode),"HARTREE").or.STRING_same(trim(BSK_mode),"HARTREE")
     l_alda_fxc   = STRING_same(trim(Chi_mode),"ALDA").or.STRING_same(trim(BSK_mode),"ALDA")
     l_lrc_fxc    = STRING_same(trim(Chi_mode),"LRC")
     l_pf_fxc     = STRING_same(Chi_mode,'PF')
     l_bs_fxc     = STRING_same(trim(Chi_mode),"BSFXC").or.STRING_same(trim(BSK_mode),"BSFXC")
     l_tddft      = l_alda_fxc.or.l_lrc_fxc.or.l_bs_fxc
     l_td_hf      = STRING_same(trim(BSK_mode),"HF")
     l_td_sex     = STRING_same(trim(BSK_mode),"SEX")
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
     if (l_ppa.or.l_mpa)                                    call initactivate(1,'em1d')
     if (l_gw0.and..not.any((/l_ppa,l_mpa,l_cohsex,l_HF_and_locXC,&
&                             l_elph_corr,l_phel_corr,l_elphoton_corr/))) call initactivate(1,'em1d dipoles')
     if (l_cohsex) call initactivate(1,'em1s dipoles')
     if (l_bss)    call initactivate(1,'optics dipoles bse')
     if (l_bs_fxc) BSS_mode="t"
     !
     if (l_mpa) then
       Xw(5)%n_freqs=4
       Xw(5)%dr=(/0._SP,0.1_SP/)!/HA2EV !damping for MPA
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
   subroutine LOCAL_after_input_IO
     !
     ! q0 renormalization
     !
     BSS_q0(:) = BSS_q0(:)*q0_def_norm/v_norm( BSS_q0)
     !
     if (len_trim(BSE_mode)==0                ) BSE_mode="retarded"
     if (STRING_same(trim(BSE_mode),"causal") ) BSE_mode="retarded"
     !
     if (l_bse) then
       BS_res_K_exchange=l_td_hartree.or.l_tddft.or.l_td_hf.or.l_td_sex
       BS_res_K_corr    =l_bs_fxc.or.l_td_hf.or.l_td_sex.or.(l_tddft.and.((WF_exx_fraction > 0._SP).or.(WF_exx_screening> 0._SP)))
       BS_W_is_diagonal =l_td_hf .or. (l_tddft.and.(WF_exx_fraction > 0._SP).and. .not.(WF_exx_screening> 0._SP) )
       BS_K_coupling    =index(BSE_mode,'coupling')>0
       if (BS_K_coupling) then
         BS_cpl_K_exchange=BS_res_K_exchange  
         BS_cpl_K_corr    =BS_res_K_corr      
       endif
       call parser('WehDiag',dummy_logical)
       if (     dummy_logical) BS_W_is_diagonal=.TRUE. 
       call parser('WehCpl' ,dummy_logical )
       if (.not.dummy_logical) BS_cpl_K_corr=.FALSE.
     endif
     !
     if (l_bse) then
       !
       ! When running BSE from input file l_bse is FALSE in before_infile_write_setup.
       ! In any case I have to overwrite X(2) with PP X(4) only if em1s=F
       !
       if (io_X_err(2)==0) then
         i_X_kind_existing_DB=2
       else if (io_X_err(2)<0.and.io_X_err(4)==0.and..not.l_em1s) then
         i_X_kind_existing_DB=4
         call X_duplicate(X(4),X(2))
         call W_duplicate(Xw(4),Xw(2))
       endif
       !
       if (io_BS_err==0) then
         if (     l_ppa) call X_duplicate(Xbsk,X(4))
         if (     l_mpa) call X_duplicate(Xbsk,X(5))
         if (.not.l_ppa.and..not.l_mpa) call X_duplicate(Xbsk,X(2))
       endif
       !
     endif
     !
     if ((l_gw0.or.l_life).and..not.any((/l_elel_corr,l_elph_corr,l_elphoton_corr,l_phel_corr/))) call initactivate(1,'el_el_corr')
     !
   end subroutine LOCAL_after_input_IO
   !
   subroutine LOCAL_scatt_and_correlation_logicals( )
     !
     if (l_cohsex) call initactivate(1,'el_el_corr')
#if !defined _ELPH
     if (l_life) call initactivate(1,'el_el_corr')
#endif
     !
     if (io_ELPH_err(1)<0.and.io_ELPH_err(2)<0) call switch_off_runlevel('el_ph_corr ph_el_corr',on_name=' ')
     if (io_ELPH_err(2)<0                     ) call switch_off_runlevel('el_ph_scatt ph_el_scatt',on_name=' ')
     !
   end subroutine LOCAL_scatt_and_correlation_logicals
   !
end function
