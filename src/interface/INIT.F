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
integer function INIT(en,q,k,X,Xw,Dip,instr,lnstr,CLOSE_Gs,FINALIZE)
 !
 ! INIT =-2  : Unsupported Runlevel(s) combination
 ! INIT =-1  : Missing CORE DBs
 ! INIT = 0  : everything is OK. Job continues.
 ! INIT = 1  : input file editing mode
 ! INIT = 2  : DB listing mode
 !
 use drivers,        ONLY:infile_editing,l_em1s,l_acfdt,l_HF_and_locXC,l_col_cut,l_alda_fxc,l_bs_fxc,l_optics,l_bse,l_bss,&
&                         l_chi,l_cohsex,l_life,l_rpa_IP,l_td_hartree,l_tddft,list_dbs,l_td_hf,l_setup,l_sc_run,l_rim,&
&                         l_real_time,l_ppa,l_lrc_fxc,l_gw0,l_elel_corr,l_elel_scatt,l_elph_corr,l_W_eh_diag,l_W_eh_cpl,l_W_eh,&
&                         l_elph_Hamiltonian,l_elph_scatt,l_elphoton_corr,l_elphoton_scatt,l_em1d,l_eval_collisions,&
&                         l_sc_magnetic,l_dipoles,l_pf_fxc,l_nl_optics
#if defined _PL
 use drivers,        ONLY:l_photolum
#endif
#if defined _SCALAPACK
 use drivers,        ONLY:l_slk_test
#endif
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,E_reset
 use frequency,      ONLY:w_samp,W_duplicate,W_reset
 use it_m,           ONLY:it,initactivate,initdefs,rstatus,initmode,initinfio,&
&                         infile,infile_dump,ofiles_append,runlevel_is_on,&
&                         switch_off_runlevel,check_verbosity,V_nl_optics
 use parallel_m,     ONLY:PP_redux_wait,master_cpu
 use com,            ONLY:msg,repfile,com_path,&
&                         write_the_logo,file_exists,rename_file,write_to_report
 use vec_operate,    ONLY:v_norm
 use LIVE_t,         ONLY:live_timing_is_on,what_is_running
 use QP_m,           ONLY:QP_t,QP_solver,QP_state,QP_reset,&
&                         QP_table,COHSEX_use_empties
 use X_m,            ONLY:X_t,X_duplicate,X_reset,Chi_mode
 use DIPOLES,        ONLY:DIPOLE_t,DIPOLES_reset
 use stderr,         ONLY:write_to_log,log_as_a_file,logfile,STRING_add,STRING_remove,STRING_match
 use R_lattice,      ONLY:q0_def_norm,bz_samp,bz_samp_reset 
 use BS,             ONLY:BS_bands,BS_n_g_W,&
&                         BS_eh_en,BSE_mode,BS_res_mode,BS_cpl_mode,BSK_mode
 use BS_solvers,     ONLY:BSS_mode,BSS_q0
#if defined  _ELPH 
 use ELPH,           ONLY:elph_nDBs,elph_nDBs_used
#endif
#if defined _SC
 use QP_m,           ONLY:QP_ng_Sx
 use SC,             ONLY:SC_ng_Sx,SC_X_bands,SC_X_size
#endif
#if defined _OPENMP
 use openmp,         ONLY:n_threads_X,n_threads_DIP,n_threads_SE,n_threads_RT,n_threads_K,n_threads_NL
#endif
 !
#include<memory.h>
 !
 type(levels)    ::en        
 type(bz_samp)   ::q,k
 type(X_t)       ::X(4)
 type(DIPOLE_t)  ::Dip
 type(w_samp)    ::Xw(4)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE,QP_field
 !
 ! Work Space
 !
 !
 type(initdefs)   ::defs
 !
 ! I/O related
 !
 type(X_t)        ::Xbsk
 type(QP_t)       ::qp
 type(w_samp)     ::Xxcw
 type(levels)     ::en_sc
 !
 integer          :: io_X_err(4),io_BS_err,io_BS_Fxc_err,io_QINDX_err,io_ELPH_err,io_SC_E_err
 !
 ! What is running ?
 !
 what_is_running='YAMBO'
#if defined _ELPH 
 what_is_running='YAMBO_PH'
#endif
#if defined _SC
 what_is_running='YAMBO_SC'
#endif
#if defined _RT
 what_is_running='YAMBO_RT'
#endif
#if defined _NL
 what_is_running='YAMBO_NL'
#endif
#if defined _MAGNETIC
 what_is_running='YAMBO_MAGNETIC'
#endif
#if defined _ELECTRIC
 what_is_running='YAMBO_ELECTRIC'
#endif
#if defined _QED
 what_is_running='YAMBO_QED'
#endif
#if defined _PL
 what_is_running='YAMBO_PL'
#endif
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
   call W_reset(Xw(1))
   call W_reset(Xw(2))
   call W_reset(Xw(3))
   call W_reset(Xw(4))
   call X_reset(X(1),type=1)
   call X_reset(X(2),type=2)
   call X_reset(X(3),type=3)
   call X_reset(X(4),type=4)
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
   call initinfio(defs,11)
   call ofiles_append(defs=defs)
   return
 endif
 if (CLOSE_Gs) then
   call call_INIT_load('Close_G_vectors')
   call INIT_barriers( )
   call logicalson
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
&                     io_X_err,io_BS_err,io_BS_Fxc_err,io_QINDX_err,io_ELPH_err,io_SC_E_err)
 !
 if (list_dbs) then
   call msg('s','')
   return
 endif
 !
 !Runlevels variables activation (Logicals from stdin)
 !
 call logicalson
 !
 !Setup on the basis of the DB read/stdin variables
 !Here I can propose values to be written in the input file
 !
 call before_infile_write_setup( )
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
 if (.not.any(rstatus>0)) rstatus(1)=-1
 call logicalson
 call varsetup2
 call logicalson
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
 QP_field=any((/l_HF_and_locXC,l_gw0,l_life/)).and..not.l_sc_run
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
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call PP_redux_wait
   return
 endif
 !
 ! Update Logicals (especially the write_to_log)
 !
 call SET_logicals()
 !
 call Dip_check_ordered
 !
 ! Report/Log Files
 !
 call INIT_report_and_log_files()
 !
 if (log_as_a_file) call rename_file(logfile)
 !
 call rename_file(repfile)
 !
 ! Finalize
 call PP_redux_wait
 !
 if (master_cpu) then
   open(unit=11,file=trim(repfile))
   call write_the_logo(11,' ')
 endif
 !
 contains
   !
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
   subroutine logicalson
     !
     integer     ::i1
     !
     do i1=1,2
       l_setup=runlevel_is_on('setup')
       l_optics=runlevel_is_on('optics')
       l_chi=runlevel_is_on('chi')
       l_bse=runlevel_is_on('bse')
       l_bss=runlevel_is_on('bss')
#if defined _PL
       l_photolum=runlevel_is_on('photolum')
#endif
       l_tddft=runlevel_is_on('tddft')
       l_em1d=runlevel_is_on('em1d')
       l_em1s=runlevel_is_on('em1s')
       l_ppa=runlevel_is_on('ppa')
       l_dipoles=runlevel_is_on('dipoles')
       l_HF_and_locXC=runlevel_is_on('HF_and_locXC')
       l_gw0=runlevel_is_on('gw0')
       l_life=runlevel_is_on('life')
       l_rim=runlevel_is_on('rim_cut')
       l_col_cut=runlevel_is_on('rim_cut')
       l_cohsex=runlevel_is_on('cohsex')
       !
       ! GPL_EXCLUDE_START
       ! 
       l_acfdt=runlevel_is_on('acfdt')
       !
       ! GPL_EXCLUDE_END
       ! 
#if defined _SCALAPACK
       l_slk_test =runlevel_is_on('slktest')
#endif
#if defined _RT
       !
       l_real_time=runlevel_is_on('negf')
       ! GPL_EXCLUDE_START
       l_elel_scatt =runlevel_is_on('el_el_scatt')
       l_elph_scatt=runlevel_is_on('el_ph_scatt')
       l_elphoton_scatt=runlevel_is_on('el_photon_scatt')
       ! GPL_EXCLUDE_END
#endif
#if defined _SC && !defined _RT
       l_sc_run=runlevel_is_on('scrun')
       l_sc_magnetic=runlevel_is_on('magnetic')
#endif
#if defined _SC || defined _RT
       l_eval_collisions=runlevel_is_on('collisions')
#endif
#if defined _NL
       l_nl_optics=runlevel_is_on('nloptics')
#endif
#if defined  _ELPH  
       if (.not.CLOSE_Gs) then
         !
         ! Only in this case ioELPH_err is defined
         !
         if (io_ELPH_err==0.or.l_real_time) then
           l_elph_corr=runlevel_is_on('el_ph_corr')
         else
           l_elph_corr=.FALSE.
           call switch_off_runlevel('el_ph_corr',on_name=' ')
         endif
         !
         ! GPL_EXCLUDE_START
         l_elph_Hamiltonian=runlevel_is_on('ElPhHam').and.io_ELPH_err==0
         ! GPL_EXCLUDE_END
       else
         l_elph_corr=runlevel_is_on('el_ph_corr')
         ! GPL_EXCLUDE_START
         l_elph_Hamiltonian=runlevel_is_on('ElPhHam')
         ! GPL_EXCLUDE_END
       endif
       l_elel_corr=runlevel_is_on('el_el_corr')
#endif
#if defined  _QED  
       l_elel_corr    =runlevel_is_on('el_el_corr')
       l_elphoton_corr=runlevel_is_on('el_photon_corr')
#endif
       !
       ! In some cases l_elel_corr is switched on by default
       !
       if (.not.l_elel_corr) then
         l_elel_corr=(l_gw0.or.l_life).and.(.not.l_elph_corr.and..not.l_elphoton_corr)
       endif
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
     l_rpa_IP     = STRING_match(trim(Chi_mode),"IP").or.STRING_match(trim(BSK_mode),"IP")
     l_td_hartree = STRING_match(trim(Chi_mode),"HARTREE").or.STRING_match(trim(BSK_mode),"HARTREE")
     l_alda_fxc   = STRING_match(trim(Chi_mode),"ALDA").or.STRING_match(trim(BSK_mode),"ALDA")
     l_lrc_fxc    = STRING_match(trim(Chi_mode),"LRC")
     l_pf_fxc     = STRING_match(Chi_mode,'PF')      
     l_bs_fxc     = STRING_match(trim(Chi_mode),"BSFXC").or.STRING_match(trim(BSK_mode),"BSFXC")
     l_tddft      = l_alda_fxc.or.l_lrc_fxc.or.l_bs_fxc
     l_td_hf      = trim(BSK_mode)=="HF"
     l_W_eh       = trim(BSK_mode)=="SEX"
     !
   end subroutine logicalson
   !
   subroutine Dip_check_ordered
     !
     implicit none
     logical :: all_bands
     !
     call parser('DipBandsAll',all_bands)
     !
     Dip%bands_ordered=.not.all_bands
#if defined _SC  || defined _RT || defined _NL
     if ( l_sc_run.or.l_real_time.or.l_nl_optics ) Dip%bands_ordered=.false.
#endif
#if defined _QED 
     if (l_elphoton_corr)                          Dip%bands_ordered=.false.
#endif
     !
   end subroutine Dip_check_ordered
   ! 
   subroutine before_infile_write_setup 
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
     if (.not.l_elel_corr.and..not.l_elph_corr.and..not.l_elphoton_corr) l_elel_corr=.true.
#if defined  _ELPH 
     elph_nDBs_used=elph_nDBs
#endif
     if (l_gw0.and.l_elel_corr) call initactivate(1,'HF_and_locXC')
     !
     if (l_em1s)   call initactivate(1,'dipoles')
     if (l_em1d)   call initactivate(1,'dipoles')
     if (l_optics) call initactivate(1,'dipoles')
     if (l_ppa)    call initactivate(1,'em1d dipoles')
     if (l_gw0.and..not.l_ppa.and..not.l_cohsex.and..not.&
&        l_HF_and_locXC.and..not.l_elph_corr.and..not.l_elphoton_corr)  &
&                  call initactivate(1,'em1d dipoles')
     if (l_cohsex) call initactivate(1,'em1s dipoles')
     if (l_bss)    call initactivate(1,'optics dipoles bse')
     if (l_bs_fxc) BSS_mode="t"
     !
     if (l_bse) then
       !
       if (l_alda_fxc) BS_res_mode='x'     
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
#if defined _PL
     if (l_photolum) call initactivate(1,'bse optics dipoles')
#endif
     !
#if defined _SC
     if (l_sc_magnetic) call initactivate(1,'potential')
     if (l_sc_run.and.io_SC_E_err==0) then
       QP_ng_Sx=SC_ng_Sx
       X(2)%ib=SC_X_bands
       X(2)%ng=SC_X_size
     endif
#endif
#if defined _OPENMP
     if (l_dipoles) then
       n_threads_DIP=0
     endif
     if (l_optics.or.l_em1s.or.l_em1d.or.l_ppa.or.l_bse) then
       n_threads_K=0
       n_threads_X=0
       n_threads_DIP=0
     endif
     if (l_HF_and_locXC.or.l_gw0.or.l_cohsex.or.l_sc_run) n_threads_SE=0
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
   end subroutine before_infile_write_setup 
   !
   subroutine varsetup2 
     !
     ! q0 renormalization
     !
     BSS_q0(:) = BSS_q0(:)*q0_def_norm/v_norm( BSS_q0)
     X(1)%q0(:)=X(1)%q0(:)*q0_def_norm/v_norm(X(1)%q0)
     X(2)%q0(:)=X(2)%q0(:)*q0_def_norm/v_norm(X(2)%q0)
     X(3)%q0(:)=X(3)%q0(:)*q0_def_norm/v_norm(X(3)%q0)
     X(4)%q0(:)=X(4)%q0(:)*q0_def_norm/v_norm(X(4)%q0)
     !
     if (len_trim(BSE_mode)==0                ) BSE_mode="retarded"
     if (STRING_match(trim(BSE_mode),"causal")) BSE_mode="retarded"
     !
     if (l_bse) then
       if (l_rpa_IP)                   BS_res_mode='none'
       if (l_td_hartree)               BS_res_mode='x'
       if (l_tddft)                    BS_res_mode='x'
       if (l_bs_fxc)                   BS_res_mode='c'
       if (l_td_hf)                    BS_res_mode='xcd'
       if (l_W_eh)                     BS_res_mode='xc'
       if (index(BSE_mode,'coupling')>0) BS_cpl_mode=trim(BS_res_mode)
       !
       if(l_W_eh) then
         call parser('WehDiag',l_W_eh_diag)
         call parser('WehCpl' ,l_W_eh_cpl )
         if (     l_W_eh_diag)         BS_res_mode=trim(STRING_add(BS_res_mode,'d'))
         if (.not.l_W_eh_cpl )         BS_cpl_mode=trim(STRING_remove(BS_cpl_mode,'c'))
       endif
     endif
     !
     if (l_bse) then
       !
       ! When running BSE from input file l_bse is FALSE in before_infile_write_setup.
       ! In any case I have to overwrite X(2) with PP X(4) only if em1s=F
       !
       if (io_X_err(2)<0.and.io_X_err(4)==0.and..not.l_em1s) then
         call X_duplicate(X(4),X(2))
         call W_duplicate(Xw(4),Xw(2))
       endif
       !
       if (io_BS_err==0) then
         if (l_ppa)      call X_duplicate(Xbsk,X(4))
         if (.not.l_ppa) call X_duplicate(Xbsk,X(2))
       endif
       !
     endif
#if !defined _ELPH  && !defined _QED
     if (l_gw0) l_elel_corr=.true.
#endif
     !
   end subroutine varsetup2
   !
   !
end function
