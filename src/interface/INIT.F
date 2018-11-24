!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function INIT(en,q,k,X,Xw,instr,lnstr,CLOSE_Gs,FINALIZE)
 !
 ! INIT =-2  : Unsupported Runlevel(s) combination
 ! INIT =-1  : Missing CORE DBs
 ! INIT = 0  : everything is OK. Job continues.
 ! INIT = 1  : input file editing mode
 ! INIT = 2  : DB listing mode
 !
 use drivers,        ONLY:infile_editing,l_em1s,l_acfdt,l_HF_and_locXC,l_col_cut,l_alda_fxc,l_bs_fxc,l_optics,l_bse,l_bsk,l_bss,&
&                         l_chi,l_cohsex,l_life,l_rpa_IP,l_td_hartree,l_tddft,list_dbs,l_td_hf,l_setup,l_sc_run,l_rim,&
&                         l_real_time,l_ppa,l_lrc_fxc,l_gw0,l_elel_corr,l_elel_scatt,l_elph_corr,l_W_eh_diag,l_W_eh_cpl,l_W_eh,&
&                         l_elph_Hamiltonian,l_elph_scatt,l_elphoton_corr,l_elphoton_scatt,l_em1d,l_eval_collisions,&
&                         l_sc_magnetic,l_pf_fxc,l_nl_optics
#if defined _PL
 use drivers,        ONLY:l_photolum
#endif
#if defined _SCALAPACK
 use drivers,        ONLY:l_slk_test
#endif
 use pars,           ONLY:schlen
 use parser_m,       ONLY:parser
 use electrons,      ONLY:levels,E_reset,n_spinor
 use frequency,      ONLY:w_samp,W_duplicate,W_reset
 use it_m,           ONLY:it,initactivate,initdefs,rstatus,nrnlvls,&
&                         initmode,rnlvls,initinfio,infile_verbosity,&
&                         infile,infile_dump,ofiles_append,runlevel_is_on,&
&                         switch_off_runlevel,check_verbosity,V_nl_optics
 use parallel_m,     ONLY:PP_redux_wait,master_cpu,ncpu
 use com,            ONLY:msg,repfile,com_path,jobstr,more_io_path,core_io_path,&
&                         write_the_logo,file_exists,rename_file,write_to_report
 use vec_operate,    ONLY:v_norm
 use LIVE_t,         ONLY:live_timing_is_on,what_is_running
 use QP_m,           ONLY:QP_t,QP_ng_Sx,QP_ng_Sc,QP_ng_SH,QP_solver,QP_state,QP_reset,&
&                         QP_table,COHSEX_use_empties
 use X_m,            ONLY:X_t,X_duplicate,X_reset,Chi_mode
 use stderr,         ONLY:log_as_a_file,logfile,intc,STRING_add,STRING_remove,STRING_match
 use R_lattice,      ONLY:ng_closed,q0_def_norm,bz_samp,nqibz,bz_samp_reset 
 use wave_func,      ONLY:wf_ng,io_WF
 use IO_m,           ONLY:io_control,OP_RD_CL,DUMP,NONE,mk_dir
 use TDDFT,          ONLY:io_BS_Fxc
 use BS,             ONLY:BS_bands,BS_n_g_exch,BS_n_g_W,&
&                         BS_eh_en,BSE_mode,BS_res_mode,BS_cpl_mode,BSK_mode
 use BS_solvers,     ONLY:BSS_mode,BSS_q0
#if defined  _ELPH 
 use ELPH,           ONLY:elph_nDBs,elph_nDBs_used,elph_use_q_grid
#endif
#if defined _SC
 use SC,             ONLY:SC_ng_Sx,SC_X_bands,SC_X_size
#endif
#if defined _NL
 use nl_optics,      ONLY:NL_ng
#endif
#if defined _SC || defined _RT
 use collision_ext,  ONLY:COH_collisions,HXC_collisions,P_collisions,GW_NEQ_collisions
 use COLL_interfaces,ONLY:io_COLLISIONS
 use hamiltonian,    ONLY:H_potential
#endif
#if defined _RT
 use fields,         ONLY:n_ext_fields,n_ext_fields_max
#endif
#if defined _MAGNETIC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_pauli,MAG_landau
#endif
#if defined _OPENMP
 use openmp,         ONLY:n_threads,n_threads_X,n_threads_DIP,n_threads_SE,n_threads_RT,n_threads_K,n_threads_NL
#endif
 !
#include<memory.h>
 !
 type(levels)    ::en        
 type(bz_samp)   ::q,k   
 type(X_t)       ::X(4)
 type(w_samp)    ::Xw(4)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE,QP_field
 !
 ! Work Space
 !
 integer           :: io_err,io_WF_err,io_X_err(4),io_ID,io_BS_err,io_KB_abinit_err,io_KB_pwscf_err,&
&                     io_BS_Fxc_err,io_QINDX_err
#if defined _SC
 type(levels)      :: en_sc
 integer           :: io_SC_E_err,io_SC_V_err
#endif
#if defined _RT
 integer           :: io_G,io_OBS
#endif
#if defined _SC || defined _RT
 integer           :: io_COLLISIONS_err
#endif
 integer, external :: io_X,io_DIPOLES,io_GROT,io_QINDX,io_RIM,&
&                     io_HF_and_locXC,io_QP_and_GF,io_BS,io_DB1,io_KB_abinit,io_KB_pwscf,&
&                     io_COL_CUT
 logical           :: OSTNTS_Vnl_included
 !
 integer, external :: io_Double_Grid
 !
 integer, external :: io_E_SOC_map
 !
#if defined _SC || defined _RT
 integer, external :: io_SC_components,io_RT_components
#endif
#if defined _ELPH 
 integer           :: io_ELPH_err
 integer, external :: io_ELPH
#endif
 !
 type(X_t)        ::Xbsk
 type(QP_t)       ::qp
 type(w_samp)     ::Xxcw
 type(initdefs)   ::defs
 integer          ::i1
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
 call read_command_line(instr,INIT)
 !
 ! DB props listing mode ?
 !
 if (index(instr,'dbpr')>0) then
   list_dbs=.true.
   INIT = 2
   if (log_as_a_file) write (logfile,'(2a)') trim(com_path),'/l_dbs'
   live_timing_is_on=.false.
   write_to_report=.false.
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
 ! BASICAL DATABASES
 !
 ! db1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=io_ID)
 io_err=io_DB1(en,k,io_ID) 
 !
#if defined _MEM_CHECK
 call MEM_treshold_estimate( )
#endif
 !
 ! wf
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,MODE=DUMP,ID=io_ID)
 io_WF_err=io_WF(io_ID) 
 if (io_err/=0.or.io_WF_err/=0) then
   INIT =-1
   return
 else
   call mk_dir(more_io_path)
   call mk_dir(com_path)
   call mk_dir(trim(core_io_path)//"/SAVE")
   call mk_dir(trim(more_io_path)//"/SAVE")
   if (INIT==0.and.ncpu>1) call mk_dir(trim(com_path)//"/LOG")
 endif
 !
 ! Exporting DB1 informations to variables to be
 ! proposed in the input file.
 !
 X(3)%ib=(/1,en%nb/)
 !
 ! gops
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=io_ID)
 io_err=io_GROT(io_ID) 
 !
 ! Updates RL variables
 !
 QP_ng_Sx=ng_closed
 QP_ng_Sc=ng_closed
 QP_ng_SH=ng_closed
 BS_n_g_exch=ng_closed
#if defined _NL
 NL_ng      =wf_ng
#endif
 !
 ! kindx
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_QINDX_err=io_QINDX(k,q,io_ID) 
 if (io_QINDX_err==0) call initactivate(-1,'IkSigLim IkXLim')
 !
 ! If the GROT/KINDX DBs are not present, reset to setup run
 !
 if ((io_err==-1.or.io_QINDX_err==-1).and.infile_editing) then
   !
   ! switch off all logicals loaded in read_command_line
   call switch_off_runlevel('all',on_name="")
   !
   ! force a setup run
   instr="setup"
   call read_command_line(instr,INIT)
   !
 endif
 !
 ! rim
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_RIM(io_ID) 
 !
 ! cutoff
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_COL_CUT(io_ID) 
 !
 ! E_rim
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_Double_Grid(en,k,io_ID)
 !
 ! E SOC perturbative
 ! 
 if(n_spinor==1) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
   io_err=io_E_SOC_map(en,k,io_ID)
 endif
 !
 ! xxvxc
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=io_HF_and_locXC(io_ID) 
 !
 ! QP
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=io_QP_and_GF('QP',qp,io_ID) 
 !
 !In DUMP mode qp%table is dumped as well (to be used in QP_apply).
 !Here, however, qp%table is not needed
 !
 YAMBO_FREE(qp%table)
 !
 ! Green Functions
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=io_QP_and_GF('G',qp,io_ID) 
 !
 ! W
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=io_QP_and_GF('W',qp,io_ID) 
 !
 ! ostnts
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_DIPOLES(X(3),io_ID)
 !
 OSTNTS_Vnl_included=io_err==0.and.X(3)%Vnl_included
 !
 ! PP_PWscf
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_KB_pwscf_err=io_KB_pwscf(io_ID)
 !
 ! kb_pp
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_KB_abinit_err=io_KB_abinit(io_ID)
 !
 if (io_err/=0) OSTNTS_Vnl_included=(io_KB_abinit_err==0.or.io_KB_pwscf_err==0)
 !
 ! I transfer to all X types the X(3) used in the previous io's 
 !
 call X_var_setup
 !
 ! When no setup has been done nqibz=0
 !
 if (nqibz>0) then
   !
   do i1=1,4 ! Xx Xs Xd Xp
     !
     X(i1)%iq=(/1,nqibz/)
     !
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
     !
     io_X_err(i1)=io_X(X(i1),Xw(i1),io_ID)
     !
   enddo
   !
 endif
 !
 ! The GLOBAL vcalue of %Vnl_included is decided on the basis of the contents
 ! of db.OSTENTS OR on the presence of the KB_PP. This means that if the
 ! response functions DBs were made in presence of KB_PP and later this
 ! DB is deleted the X dbs will be recalculated
 !
 forall(i1=1:4) X(i1)%Vnl_included=OSTNTS_Vnl_included
 !
 ! bs
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_BS_err=io_BS(1,Xbsk,io_ID)
 !
 ! When a PPA screen is used to build the kernel the %ppaE component of Xbsk must be overwritten.
 ! Otherwise yambo will re-calculate the ppa interaction when the %ppaE is changed with respect to 
 ! the default (notice by MP, July 2015).
 !
 if (io_X_err(4)>=0) Xbsk%ppaE=X(4)%ppaE
 !
 ! GPL_EXCLUDE_START
 !
 ! bs_fxc
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_BS_Fxc_err=io_BS_Fxc(1,Xxcw,io_ID,X=Xbsk)
 !
 ! GPL_EXCLUDE_END
 !
 ! ELPH 
 !
#if defined _ELPH 
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_ELPH_err=io_ELPH(io_ID,'gkkp')
 !
 if (io_ELPH_err/=0) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
   io_ELPH_err=io_ELPH(io_ID,'gkkp_expanded')
 endif
 !
#endif
 !
 ! RT 
 !
#if defined _RT
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_OBS=io_RT_components('OBSERVABLES',io_ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_G=io_RT_components('G',io_ID)
#endif
 !
 ! SC 
 !
#if defined _SC
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_SC_V_err=io_SC_components('V',en_sc,io_ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_SC_E_err=io_SC_components('E',en_sc,io_ID)
 if (io_SC_E_err==0.and.io_SC_V_err/=0) io_SC_E_err=-1
#endif
 !
 ! COLLISIONS 
 !
#if defined _SC || defined _RT
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_COLLISIONS_err=io_COLLISIONS(io_ID,COH_collisions)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_COLLISIONS_err=io_COLLISIONS(io_ID,HXC_collisions)
#endif
#if defined _RT
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_COLLISIONS_err=io_COLLISIONS(io_ID,GW_NEQ_collisions)
#endif
#if defined _QED && defined _RT
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_COLLISIONS_err=io_COLLISIONS(io_ID,P_collisions)
#endif
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
 !Common
 !
 call initactivate(1,'StdoHash Nelectro ElecTemp BoseTemp OccTresh NLogCPUs DBsIOoff DBsFRAGpm EvalMagn MEM_tresh')
 !
 !FFT
 !
 if (any((/(l_optics.and.l_chi),(l_optics.and.l_bse),l_em1d,&
&          l_em1s,l_acfdt,l_HF_and_locXC,l_col_cut/))) call initactivate(1,'FFTGvecs WFbuffIO')
 !
 ! CPU structure
 !
 if ((l_optics.and..not.l_bse).or.l_em1s.or.l_em1d.or.l_ppa) then
   if (l_optics.and..not.l_bse) then
     call CPU_activate("X_q_0")
     call CPU_activate("X_finite_q")
   else
     call CPU_activate("X_all_q")
   endif
   call initactivate(1,'X_Threads DIP_Threads')
 endif
 !
 if(l_sc_run) then
   call CPU_activate("X_q_0")  ! Required for finite E-field, l_sc_exx, SC_up_W_iter
 endif
 !
 if ( (l_HF_and_locXC.or.l_gw0.or.l_cohsex.or.l_life) .and. .not.(l_sc_run.or.l_eval_collisions) ) then
   call CPU_activate("SE")
   call initactivate(1,'SE_Threads')
 endif
 !
#if defined _SCALAPACK
 if (l_slk_test) then
   call initactivate(1,"SLKdim")
   call CPU_activate("SLK")
 endif
#endif
 !
#if defined _SC
 if (l_eval_collisions .or. l_sc_run) then
   call CPU_activate("SE")
   call initactivate(1,'SE_Threads')
 endif
#endif
 !
#if defined _RT 
 if (l_eval_collisions .or. l_real_time) then
   call CPU_activate("RT")
   call initactivate(1,'RT_Threads')
 endif
#endif
 !
#if defined _NL
 if (l_nl_optics) then
   call CPU_activate("NL")
   call initactivate(1,'NL_Threads')
 endif
#endif
 !
 if (l_bse) then
   call CPU_activate("BS")
   call initactivate(1,'X_Threads DIP_Threads K_Threads')
 endif
 !
 !Setup
 !
 if (l_setup) call initactivate(1,'MaxGvecs IkSigLim IkXLim')
#if defined  _ELPH 
 if (l_setup) call initactivate(1,'BSEscatt')
#endif
 !
 !
 if (any((/(l_optics.and.l_chi),(l_optics.and.l_bse)/)))  call initactivate(1,'NonPDirs')
 !
 !RIM
 !
 if (l_rim) call initactivate(1,'RandQpts RandGvec QpgFull Em1Anys IDEm1Ref')
 !
 !Col CUTOFF 
 !
 if (l_col_cut) call initactivate(1,'CUTGeo CUTBox CUTRadius CUTCylLen CUTwsGvec CUTCol_test')
 !
 !XX
 !
 if (l_HF_and_locXC) call initactivate(1,'EXXRLvcs')  
 if (l_HF_and_locXC) call initactivate(1,'UseNLCC')  
 !
 ! Kernels
 !
 if (l_em1s.or.l_em1d)             call initactivate(1,'Chimod ChiLinAlgMod')
 if (l_optics.and.l_chi)           call initactivate(1,'Chimod ChiLinAlgMod')
 if (l_optics.and.l_bse)           call initactivate(1,'BSEmod')
 if (l_optics.and.l_bse)           call initactivate(1,'BSKmod')
 if (l_optics.and.l_bse.and.l_bss) call initactivate(1,'BSSmod')
 !
 if (l_optics.and.l_chi) then
   !
   ! GPL_EXCLUDE_START
   !
   ! BS based TDDFT (BS fxc is only of q=0 0 0). BS_fxc db exist
   if(l_bs_fxc) call initactivate(1,'FxcGRLc FxcSVdig FxcRetarded')
   !
   ! GPL_EXCLUDE_END
   !
   ! ALDA/LRC Tddft
   if (l_alda_fxc) call initactivate(1,'FxcGRLc')
   if (l_lrc_fxc)  call initactivate(1,'LRC_alpha LRC_beta')
   if (l_pf_fxc)   call initactivate(1,'PF_alpha')
   !
 endif
 !
 ! Optics(not bse) nor GW (no PP)
 !
 if ((l_optics.and.l_chi).or.(l_em1d.and..not.l_life.and..not.l_ppa.and..not.l_gw0)) then
   !
   call X_activate('Xd',(/'NGsBlk','QpntsR','BndsRn',&
&                  'GrFnTp','EnRnge','DmRnge','DmERef','CGrdSp','ETStps','EMStps',&
&                  'DrudeW','EhEngy','LongDr'/))
   call initactivate(1,'DrClassic')
   !
   call INIT_QP_ctl_switch('X')
   !
   ! GPL_EXCLUDE_START
   !
#if defined _RT
   call INIT_RT_ctl_switch('X')
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call initactivate(1,'DipApproach DipPDirect ShiftedPaths Qdirection QShiftOrder')
#if defined _NL | defined _ELECTRIC
   call initactivate(1,'EvPolarization FrSndOrd')
#endif
   !
 endif
 !
 ! Double Grid
 !
 if (l_optics.or.l_life) then
   !
   call initactivate(1,'DbGdQsize')
   !
 endif
 !
 ! BSK
 !
 if (l_optics.and.l_bse.and.l_bsk) then
   !                     
   call initactivate(1,'BSENGexx ALLGexx')
   !
   if(l_td_hf.or.l_W_eh) call initactivate(1,'BSENGBlk')
   if(l_W_eh)            call initactivate(1,'WehDiag WehCpl')
   !
   ! BSE + TDDFT = no BS db, Fxc + LF on-fly
   ! Special case: The BSE equation is used to build up the BSE_Fxc kernel.
   if (l_bs_fxc) then
     call initactivate(1,'FxcGRLc FxcSVdig FxcRetarded FxcMEStps')
     call initactivate(1,'BLongDir BEnRange BDmRange BEnSteps')
   endif
   !
 endif
 !
#if defined _PL
 !
 ! PL
 !
 if (l_photolum) call initactivate(1,'PL_weights')
 !
#endif
 !
 ! BSE
 !
 if (l_optics.and.l_bse) then
   !
   call INIT_QP_ctl_switch('K')
   !
   ! GPL_EXCLUDE_START
   !
#if defined _RT
   call INIT_RT_ctl_switch('K')
   call INIT_RT_ctl_switch('R')
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call initactivate(1,'DipApproach DipPDirect ShiftedPaths Gauge NoCondSumRule MetDamp')
#if defined _KERR
   call initactivate(1,'EvalKerr AnHall')
#endif
   !
   call initactivate(1,'DrudeWBS Reflectivity')
   call initactivate(1,'BoseCut ShiftedPaths')
   call initactivate(1,'BEnRange BDmRange BDmERef BEnSteps BLongDir')
   call initactivate(1,'BSEQptR BSEBands BSEEhEny BSehWind')
   !
#if defined _RT
   call initactivate(1,'ForceEqTrans')
#endif
   !
 endif                    
 !
 ! BSE solver
 !
 if (l_optics.and.l_bse.and.l_bss) then
   !
   ! Special case: the BSE_Fxc kernel has been constructed
   !               Thus I move to g-space to solve the Dyson equation
   !
   if (index(BSS_mode,'t')/=0 .and. l_bs_fxc) call initactivate(-1,'BSENGexx ALLGexx')
   if (index(BSS_mode,'t')/=0 .and. l_bs_fxc) call initactivate(-1,'BSENGBlk')
   !
   if (index(BSS_mode,'i')/=0)  call initactivate(1,'BSSInvMode BSEPSInvTrs BSSInvPFratio BSSInvKdiag')
   if (index(BSS_mode,'d')/=0)  call initactivate(1,'WRbsWF BSSPertWidth')
   if (index(BSS_mode,'h')/=0)  call initactivate(1,'BSHayTrs BSHayTer')
   if (index(BSS_mode,'s')/=0)  then
      call initactivate(1,'BSSNEig')
      call initactivate(1,'BSSEnTarget')
      call initactivate(1,'BSSSlepcExtraction')
      call initactivate(1,'BSSSlepcNCV')
      call initactivate(1,'BSSSlepcTol')
   endif
   !
   ! Special project dependent variables
   !
#if defined _PL 
   if (index(BSS_mode,'i')/=0)  call initactivate(1,'BSPLInvTrs')
#endif
#if defined _ELPH 
   if (l_elph_corr.and..not.elph_use_q_grid) call initactivate(1,'ElPhRndNq')
#endif
 endif
 !
 ! Static screen 
 !
 if (l_em1s) then
   call INIT_QP_ctl_switch('X')
   !
   ! GPL_EXCLUDE_START
   !
#if defined _RT
   call INIT_RT_ctl_switch('X')
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call initactivate(1,'ShiftedPaths')
   call X_activate('Xs',(/'QpntsR','BndsRn','NGsBlk','GrFnTp','DmRnge','CGrdSp','EhEngy','LongDr','DrudeW'/))
   call initactivate(1,'XTermKind')
   call initactivate(1,'XTermEn')
 endif
 !
 ! GW (PPA & COHSEX) 
 !
 if ( (l_em1d.and.l_ppa) .or. (l_em1s.and.l_cohsex)) then
   call INIT_QP_ctl_switch('X')
   !
   ! GPL_EXCLUDE_START
   !
#if defined _RT
   call INIT_RT_ctl_switch('X')
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call initactivate(1,'ShiftedPaths')
   if (l_ppa) then
     call X_activate('Xp',(/'QpntsR','BndsRn','NGsBlk','CGrdSp',&
&                           'EhEngy','LongDr','PPAPnt'/))
   endif
   if (l_cohsex) then
     call X_activate('Xs',(/'QpntsR','BndsRn','NGsBlk','EhEngy','LongDr'/))
   endif
   call initactivate(1,'XTermKind')
   call initactivate(1,'XTermEn')
 endif
 !
 ! ACFDT
 !
 if (l_acfdt) then
   call INIT_QP_ctl_switch('X')
   !
   ! GPL_EXCLUDE_START
   !
#if defined _RT
   call INIT_RT_ctl_switch('X')
#endif
   !
   ! GPL_EXCLUDE_END
   !
   call initactivate(1,'ShiftedPaths')
   call initactivate(1,'EXXRLvcs AC_n_LAM AC_n_FR AC_E_Rng')
   call X_activate('Xx',(/'QpntsR','BndsRn','NGsBlk','CGrdSp','EhEngy','LongDr'/))
 endif
 !
 ! GW/Life
 !
 if (l_gw0.or.l_life) then
   !
#if defined _QED
   if (l_elphoton_corr) then
     if (l_gw0) then
       call initactivate(1,'FFTGvecs RandQpts QEDRLvcs GbndRnge GDamping dScStep DysSolver') 
       if (trim(QP_solver)=="g") then
         call initactivate(1,'GEnSteps GEnRnge GEnMode GDmRnge GreenFTresh GreenF2QP') 
       else
         call initactivate(1,'GWoIter')
         if (.not.l_cohsex) call initactivate(1,'NewtDchk ExtendOut OnMassShell QPExpand')
       endif
     else if (l_life) then
       call initactivate(1,'GbndRnge') 
     endif
   endif
#endif
   !
   if (l_elel_corr) then
     !
     call INIT_QP_ctl_switch('X')
     call INIT_QP_ctl_switch('G')
     !
     ! GPL_EXCLUDE_START
     !
#if defined _RT
     call INIT_RT_ctl_switch('X')
     call INIT_RT_ctl_switch('G')
#endif
     !
     ! GPL_EXCLUDE_END
     !
     call initactivate(1,'BoseCut ShiftedPaths')
     !
     if (l_gw0) then
       if (.not.l_cohsex.or.COHSEX_use_empties) call initactivate(1,'GbndRnge') 
       if (.not.l_cohsex.and.trim(QP_solver)/='g') call initactivate(1,'GDamping') 
       if (.not.l_cohsex) call initactivate(1,'dScStep') 
       if (.not.l_elphoton_corr) then
         if (.not.l_ppa.and..not.l_cohsex) &
&          call X_activate('Xd',(/'BndsRn','NGsBlk',&
&                          'DmRnge','DmERef','CGrdSp','ETStps','EMStps',&
&                          'DrudeW','EhEngy','LongDr'/)) 
         !
         if (.not.l_cohsex) call initactivate(1,'GTermKind GTermEn DysSolver')
         if (     l_cohsex) call initactivate(1,'UseEbands')
         if (trim(QP_solver)=="g") then
           call initactivate(1,'GEnSteps GEnRnge GEnMode GDmRnge GreenFTresh GreenF2QP') 
         else
           call initactivate(1,'GWoIter')
           if (.not.l_cohsex) call initactivate(1,'NewtDchk ExtendOut OnMassShell QPExpand')
         endif
       endif
       !
     endif
     !
     if (l_life) then
       call initactivate(1,'LifeTrCG')
       if (l_elel_corr) call X_activate('Xd',(/'BndsRn','NGsBlk',&
&                                     'DmRnge','CGrdSp',&
&                                     'DrudeW','EhEngy','LongDr'/)) 
     endif
   endif
   !
#if defined  _ELPH 
   !
   if (l_elph_corr) then
     call initactivate(1,'DysSolver')
     call initactivate(1,'GphBRnge FANdEtresh DWdEtresh ElPhModes GDamping dScStep ExtendOut ElPhRndNq RandQpts')
     call initactivate(1,'WRgFsq NewtDchk OnMassShell')
   endif
   if (trim(QP_solver)=="g".and.l_elph_corr) then
     call initactivate(1,'GEnSteps GEnRnge GEnMode GDmRnge GreenFTresh GreenF2QP') 
     call initactivate(-1,'WRgFsq NewtDchk GDamping ExtendOut OnMassShell')
   endif
   !
#endif
   !
 endif
 !
 if(l_alda_fxc.and.any((/l_em1s,l_em1d,l_acfdt,l_ppa,l_cohsex,l_gw0/)) ) call initactivate(1,'FxcGRLc')
 if( l_lrc_fxc.and.any((/l_em1s,l_em1d,l_acfdt,l_ppa,l_cohsex,l_gw0/)) ) call initactivate(1,'LRC_alpha LRC_beta')
 !
 ! El-Ph: Frohlich Hamiltonian
 !
 ! GPL_EXCLUDE_START
 !
#if defined _ELPH 
 !
 if (l_elph_Hamiltonian) then
   call initactivate(1,'ElPhHBRnge ElPhModes ElPhHKpt GDamping REStresh')
 endif
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SC || defined _RT
 !
 ! Collisions
 !
 if (l_eval_collisions) call initactivate( 0,'COLLBands')
 !
 if ( (l_eval_collisions.or.l_real_time).and.l_elel_scatt ) call initactivate(1,'PlasmaPerc')
 !
 if (l_eval_collisions.or.l_real_time.or.l_sc_run) then
   !
   call initactivate( 1,'HXC_Potential FFTGvecs')
   !
   call initactivate(-1,'QpntsRXs')
   !
   if (l_eval_collisions) then
     if( index(H_potential,"HARTREE")>0 )                           call initactivate(1,'HARRLvcs ALLGHAR')
     if( index(H_potential,"SEX")>0.or.index(H_potential,"FOCK")>0) call initactivate(1,'EXXCut EXXRLvcs ALLGexx')
     if( index(H_potential,"SEX")>0.or.index(H_potential,"COH")>0 ) call initactivate(1,'CORRLvcs ALLGexx')
   endif
   !
 endif
 !
#endif
 !
 ! GPL_EXCLUDE_END
 !
#if defined _SC 
 !
 ! Self-Consistency
 !
 if (l_sc_run) then
   !
   call initactivate( 1,'SCBands')
   !
   if( index(H_potential,"SEX")>0.or.index(H_potential,"COH")>0  ) call initactivate(1,'UseEbands SCUpWIter')
   !
   if( index(H_potential,"EXX")>0.or.index(H_potential,"SRPA")>0 ) call initactivate(1,'OEPapprox OEPItSolver') 
   !
   call initactivate(1,'SCIter SCEtresh SCRhoTresh SC_precondition')
   call initactivate(1,'SCmixing SClwBand SCnlMix BandMix')
   call initactivate(1,'SCdiag Mean_Potential')
   !
 endif
 !
#endif
 !
#if defined _RT
 !
 if (l_real_time) then
   call INIT_QP_ctl_switch('G')
   !
   ! GPL_EXCLUDE_START
   !
   call INIT_RT_ctl_switch('G')
   call initactivate(1,'RTskipImposeN RTeeImposeE InducedField RTUpdateSOC RTUpdateE SaveGhistory')
   call initactivate(1,'RTEqScatt RTzeroTempRef RTEvalEnergy RTEvalEntropy')
   call initactivate(1,'dTupdateTime dTupdateJump dTupdateTresh dT_MAX')
   if (l_elel_scatt .or. l_elph_scatt .or. l_elphoton_scatt) &
&    call initactivate(1,'LifeExtrapolation LifeExtrapSteps LifeFitTemp RelaxTimeApprox RTAtemp RTAchem LifeFitTemp')
   if (l_elph_scatt) call initactivate(1,'MemTresh RandQpts RT_T_evol ElPhModes UseDebyeE')
   if (l_elel_scatt) call initactivate( 1,'EERimPerc')
   !
   ! GPL_EXCLUDE_END
   !
   call initactivate(1,'RTBands Integrator GrKind TwoAlpha RADLifeTime PhLifeTime RTehEny ScattTresh') 
   call initactivate(1,'RTstep NEsteps NETime DipoleEtresh')
   call initactivate(1,'IOtime')
   !
   ! n_ext_fields is by default 0.  It is set to the input number of fields when a command line option is provided (below in init.F)
   ! or parsed from the input file in INIT_load.F
   !
   do i1=1,n_ext_fields
     call Afield_activate(i1)
   enddo
   !
 endif
 !
#endif
 !
#if defined _NL
 if (l_nl_optics) then
   call initactivate(1,'FFTGvecs NLBands NLverbosity NLstep NLtime NLintegrator NLCorrelation NLLrcAlpha')
   call initactivate(1,'NLEnRange NLEnSteps NLDamping UseDipoles FrSndOrd EvalCurrent InducedField NLGvecs') 
   call initactivate(1,'ExtF_Dir ExtF_FrStep ExtF_Int ExtF_Width ExtF_kind ExtF_Tstart')
   call init_QP_ctl_switch('G')
 endif
#endif
 !
#if defined _MAGNETIC
 !
 ! Magnetic
 !
 if (l_sc_magnetic) then
   call initactivate(1,'Hamiltonian B_Field B_psi B_theta B_Gauge PhaseTrick')
   if (MAG_landau) call initactivate(1,'B_radius')
 endif
 !
#endif
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
#if defined _RT
   !
   subroutine Afield_activate(i1)
     integer     ::i1
     character(6)::field
     field='Field'//trim(intc(i1))
     call initactivate(1,field//'_Freq')
     call initactivate(1,field//'_MaxFreq')
     call initactivate(1,field//'_DFreq')
     call initactivate(1,field//'_Int')
     call initactivate(1,field//'_Width')
     call initactivate(1,field//'_FWHM')
     call initactivate(1,field//'_kind')
     call initactivate(1,field//'_pol')
     call initactivate(1,field//'_Dir')
     call initactivate(1,field//'_Dir_circ')
     call initactivate(1,field//'_Tstart')
   end subroutine
   !
#endif
   !
   subroutine CPU_activate(what)
     character(*) :: what 
#if defined _MPI
     call initactivate(1,"PAR_def_mode")
     call initactivate(1,what//"_CPU")
     call initactivate(1,what//"_ROLEs")
     if ((what=="X_q_0".and..not.l_real_time).or.&
&         what=="X_finite_q".or.what=="X_all_q".or.what=="BS") call initactivate(1,what//"_nCPU_LinAlg_INV")
     if (what=="BS")                                           call initactivate(1,what//"_nCPU_LinAlg_DIAGO")
     if (what=="SE".and.l_sc_run)                              call initactivate(1,what//"_nCPU_LinAlg_DIAGO")
#endif
#if defined _SCALAPACK &&  defined _MPI
     if (what=="SLK")                                          call initactivate(1,what//"_nCPU_LinAlg_INV")
#endif
     !
   end subroutine
   !
   subroutine X_activate(mode,what)
     character(2)::mode
     character(6)::what(:)
     ! Work Space
     integer     ::i1
     do i1=1,size(what,1)
       call initactivate(1,what(i1)//mode)
     enddo
   end subroutine
   !
   subroutine call_INIT_load(mode)
     character(*)::mode
     if (mode=='create_shadow_vars') initmode=0
     if (mode=='load_defaults') initmode=1
     if (mode=='Close_G_vectors') initmode=2
     if (mode=='Game_Over') initmode=3
     if (mode=='parser_input_file') initmode=4
     call INIT_load(defs,en,q,k,X,Xw)
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
       l_bsk=runlevel_is_on('bsk')
       l_bss=runlevel_is_on('bss')
#if defined _PL
       l_photolum=runlevel_is_on('photolum')
#endif
       l_tddft=runlevel_is_on('tddft')
       l_em1d=runlevel_is_on('em1d')
       l_em1s=runlevel_is_on('em1s')
       l_ppa=runlevel_is_on('ppa')
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
       ! Davide: Why this ? Can't I do SC-IP calculation with v_HXC=0 ?
       if (STRING_match(H_potential,"IP")) H_potential="DEFAULT"
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
   subroutine X_var_setup 
     !
     ! Before any X DB/infile reading
     !
     call X_duplicate(X(3),X(2))
     call X_duplicate(X(3),X(1))
     call X_duplicate(X(3),X(4))
     call W_duplicate(Xw(3),Xw(2))
     call W_duplicate(Xw(3),Xw(1))
     call W_duplicate(Xw(3),Xw(4))
     !
   end subroutine X_var_setup
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
     if (l_ppa)    call initactivate(1,'em1d')
     if (l_gw0.and..not.l_ppa.and..not.l_cohsex.and..not.&
&        l_HF_and_locXC.and..not.l_elph_corr.and..not.l_elphoton_corr)  &
&                  call initactivate(1,'em1d')
     if (l_cohsex) call initactivate(1,'em1s')
     if (l_bsk)    call initactivate(1,'optics')
     if (l_bsk)    call initactivate(1,'bse')
     if (l_bss)    call initactivate(1,'optics')
     if (l_bss)    call initactivate(1,'bse')
     if (l_bss)    call initactivate(1,'bsk')
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
         call initactivate(1,'em1d ppa')
       endif
       !
     endif
     !
     if (l_gw0.and.l_cohsex) call parser('UseEbands',COHSEX_use_empties)
     !
#if defined _PL
     if (l_photolum) call initactivate(1,'bse optics')
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
   subroutine read_command_line(rstr,init_) 
     !
     use com,    ONLY:error
     use LIVE_t, ONLY:USER_wall_time_string,GET_user_WALL_time
     use stderr, ONLY:STRING_split
     use it_m,   ONLY:V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,&
&                     V_real_time,V_all,V_parallel
     implicit none
     integer     :: init_
     character(*):: rstr
     !
     ! Work Space
     !
     integer          ::i1,i2,i_field,n_pieces,i_cycle
     character(schlen)::rstr_piece(2*nrnlvls),strings_to_not_use_as_runlevels(nrnlvls)
     !
     ! Bug fix (17/9/2012). If any string following a -### identifier contains 
     ! a string related to a runlevel this is erronously switched on. 
     !
     strings_to_not_use_as_runlevels=" "
     strings_to_not_use_as_runlevels(1)="jobstr"
     strings_to_not_use_as_runlevels(2)="ifile"
     strings_to_not_use_as_runlevels(3)="idir"
     strings_to_not_use_as_runlevels(4)="odir"
     strings_to_not_use_as_runlevels(5)="cdir"
     strings_to_not_use_as_runlevels(6)="com_path"
     strings_to_not_use_as_runlevels(7)="more_io_path"
     strings_to_not_use_as_runlevels(8)="core_io_path"
     !
     ! Split the string in pieces
     !
     call STRING_split(rstr,rstr_piece)
     n_pieces=0
     do i1=1,2*nrnlvls
       if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
     enddo
     if (n_pieces==0) return
     !
     do i_cycle=1,2
       !
       INPUT_strings_loop: do i1=1,n_pieces
         !
         if (trim(rstr_piece(i1))=='ifile') cycle
         !
         ! String suitable for runlevel switching?
         !
         if (i1>1) then
           do i2=1,nrnlvls
             if (trim(rstr_piece(i1-1)) == trim(strings_to_not_use_as_runlevels(i2))) cycle INPUT_strings_loop
           enddo
         endif
         !
         ! Input File editing?
         !
         do i2=1,nrnlvls
           if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i1))) infile_editing=.true.
         enddo
         !
         ! Run Levels
         !
         call initactivate(1, trim(rstr_piece(i1)) )
         !
         ! Verbosity
         ! V_RL=1
         ! V_kpt=2
         ! V_sc=3
         ! V_qp=4
         ! V_io=5
         ! V_general=6
         ! V_resp=7
         ! V_real_time=8
         ! V_parallel=9
         ! V_nl_optics=10
         ! V_all=99
         !
         if ( trim(rstr_piece(i1)) == 'infver' ) then
           select case (trim(rstr_piece(i1+1)))
             case ('RL','rl')
               infile_verbosity=V_RL
             case ('kpt','k')
               infile_verbosity=V_kpt
             case ('sc','SC')
               infile_verbosity=V_sc
             case ('QP','qp')
               infile_verbosity=V_qp
             case ('IO','io')
               infile_verbosity=V_io
             case ('gen')
               infile_verbosity=V_general
             case ('resp','X')
               infile_verbosity=V_resp
             case ('rt')
               infile_verbosity=V_real_time
             case ('nl')
               infile_verbosity=V_nl_optics
             case ('par')
               infile_verbosity=V_parallel
             case ('all')
               infile_verbosity=V_all
           end select
         endif
         !
         if ( trim(rstr_piece(i1)) == 'wallt' ) then
           USER_wall_time_string=trim(rstr_piece(i1+1))
           if (i_cycle==1) call GET_user_WALL_time()
         endif
         !
         if ( trim(rstr_piece(i1)) == 'em1s' .or.  trim(rstr_piece(i1)) == 'em1d' .or.&
&             runlevel_is_on('em1s')         .or.  runlevel_is_on('em1d')  .or. &
&             runlevel_is_on('life')         .or.  runlevel_is_on('cohsex') .or. &
&             runlevel_is_on('ppa')) &
&           Chi_mode='HARTREE'
         !
         ! BSE/LLR
         !
         if (i_cycle==2.and.trim(rstr_piece(i1)) == 'optics' )  then
           !
           l_chi= (trim(rstr_piece(i1+1))=='g' ).or.(trim(rstr_piece(i1+1))=='c')
           l_bse= (trim(rstr_piece(i1+1))=='eh').or.(trim(rstr_piece(i1+1))=='b')
           !
           if (.not.l_chi.and..not.l_bse) l_chi=.true.
           !
           call initactivate(1,'optics')
           if (l_chi) call initactivate(1,'chi')
           if (l_bse) call initactivate(1,'bse')
           !
           if (l_chi.and.trim(Chi_mode)==' ') Chi_mode='IP'
           if (l_bse.and.trim(BSK_mode)==' ') BSK_mode='IP'
           !
         endif
         !
         ! Approximation used for the BSE/LLR kernel
         !
         if ( i_cycle==2 .and. trim(rstr_piece(i1)) == 'kernel' )  then
           !
           ! if the optics option is not present define defaults
           ! since kernel does not correspond to a runlevel, set 
           ! infile_editing=.true.           
           !
           if (.not.l_bse.and..not.l_chi) then
             l_chi =((trim(rstr_piece(i1+1)) == 'hartree').or.&
                    &( trim(rstr_piece(i1+1)) == 'lrc'))
             l_bse =((trim(rstr_piece(i1+1)) == 'alda').or.&
                    &(trim(rstr_piece(i1+1)) == 'sex').or.&
                    &(trim(rstr_piece(i1+1)) == 'hf').or.&
                    &( trim(rstr_piece(i1+1)) == 'bsfxc'))
             infile_editing=.true.           
             call initactivate(1,'optics')
             if (l_chi) call initactivate(1,'chi')
             if (l_bse) call initactivate(1,'bse')
           end if
           !
           BSK_mode='HARTREE'
           Chi_mode='HARTREE'
           !
           if(l_bse)  then
             if(trim(rstr_piece(i1+1))  == 'hartree')  BSK_mode='Hartree'
             if(trim(rstr_piece(i1+1))  == 'hf')       BSK_mode='HF'
             if( trim(rstr_piece(i1+1)) == 'alda')     BSK_mode='ALDA'
             if(trim(rstr_piece(i1+1))  == 'sex')      BSK_mode='SEX'
             if( trim(rstr_piece(i1+1)) == 'bsfxc')    BSK_mode='BSfxc'
           else if(l_chi) then
             if(trim(rstr_piece(i1+1))  == 'hartree')  Chi_mode='HARTREE'
             if( trim(rstr_piece(i1+1)) == 'alda')     Chi_mode='ALDA'
             if( trim(rstr_piece(i1+1)) == 'lrc')      Chi_mode='LRC'
             if( trim(rstr_piece(i1+1)) == 'bsfxc')    Chi_mode='BSfxc'
           endif
           !
           if((trim(rstr_piece(i1+1)) == 'alda').or.&
&             (trim(rstr_piece(i1+1)) == 'lrc').or.&
&             (trim(rstr_piece(i1+1)) == 'pf').or.&
&             (trim(rstr_piece(i1+1)) == 'bsfxc')) call initactivate(1,'tddft')
           !
           if(l_bse)   call initactivate(1,'bsk')
           !
         endif
         !
         ! BSE Solver
         !
         if ( trim(rstr_piece(i1)) == 'bss' )  then
           BSS_mode=trim(rstr_piece(i1+1))
           if (index(BSS_mode,'h')==0.and.index(BSS_mode,'d')==0.and.&
&              index(BSS_mode,'i')==0.and.index(BSS_mode,'t')==0.and.&
&              index(BSS_mode,'s')==0) BSS_mode='h'
           !
           ! With and ALDA Fxc the t solver is not permitted
           !
           if (l_alda_fxc.and.index(BSS_mode,'t')/=0) BSS_mode='h'
           if (BSK_mode=='IP') call switch_off_runlevel('bss',on_name=' ')
           if (BSK_mode==' ') then
             BSK_mode="SEX"
             Chi_mode="HARTREE"
           endif
           !
         endif
         !
         ! Dyson Solver
         !
         if ( trim(rstr_piece(i1)) == 'gw0' ) then
           QP_solver=trim(rstr_piece(i1+1))
           if (trim(QP_solver)/='n'.and.trim(QP_solver)/='s'.and.&
&              trim(QP_solver)/='g') QP_solver='n'
#if !defined  _ELPH && !defined _QED
           l_elel_corr=.true.
#endif
           !
           if ( i_cycle == 2 .and. trim(Chi_mode)==' ') Chi_mode='HARTREE'
           !
         endif
         !
         ! Lifetimes
         !
         if ( i_cycle==2 .and. trim(rstr_piece(i1)) == 'life' )  then
#if defined  _ELPH || defined _QED
           if ( (.not.l_elel_corr.and..not.l_elphoton_corr) .or. l_elel_corr) then
             call initactivate(1,'em1d')
             call initactivate(1,'el_el_corr')
           endif
#else
           call initactivate(1,'em1d')
           l_elel_corr=.true.
#endif
         endif
         !
         ! GW approximation 
         !
         if ( trim(rstr_piece(i1)) == 'gwapprx' ) then
           !
           if (trim(rstr_piece(i1+1))=='p') then
             infile_editing=.true.
             call initactivate(1,'ppa')
           else if (trim(rstr_piece(i1+1))=='c') then
             !
             infile_editing=.true.
             call initactivate(1,'cohsex gw0') 
             !
           endif
           !
         endif
         !
#if defined _SC
         !
         ! SC
         !
         if ( trim(rstr_piece(i1)) == 'potential' )  then
           !
           H_potential=""
           if ( index(trim(rstr_piece(i1+1)),'exx')>0 ) then
             H_potential='EXX'
             if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
           else if ( index(trim(rstr_piece(i1+1)),'exxc')>0 ) then
             H_potential='EXXC'
           else if ( index(trim(rstr_piece(i1+1)),'srpa')>0 ) then
             H_potential='SRPA'
           else if ( index(trim(rstr_piece(i1+1)),'cohsex')>0 ) then
             H_potential='COH+SEX'
             if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
           else if ( index(trim(rstr_piece(i1+1)),'coh')>0 ) then
             H_potential='COH'
             if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
           else if ( index(trim(rstr_piece(i1+1)),'sex')>0 ) then
             H_potential='SEX'
             if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
           else if ( index(trim(rstr_piece(i1+1)),'f')>0) then
             H_potential='FOCK'
           endif
           !
           if ( index(trim(rstr_piece(i1+1)),'h')==1 ) then
             if(trim(H_potential)/="") H_potential='HARTREE+'//trim(H_potential)
             if(trim(H_potential)=="") H_potential='HARTREE'
           endif
           !
           if ( trim(rstr_piece(i1+1)) == 'd') then
             H_potential='DEFAULT'
           endif
           !
           if ( trim(rstr_piece(i1+1)) == 'ip') then
             H_potential='IP'
           endif
           !
           !
         endif
#endif
         !
#if defined _RT
         !
         ! NEGF
         !
         if ( trim(rstr_piece(i1)) == 'potential' )  then
           !
           H_potential=""
           if ( index(trim(rstr_piece(i1+1)),'cohsex')>0) then
             H_potential='COH+SEX'
             if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
           else if ( index(trim(rstr_piece(i1+1)),'sex')>0 ) then
             H_potential='SEX'
             if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
           else if ( index(trim(rstr_piece(i1+1)),'f')>0) then
             H_potential='FOCK'
           else if ( trim(rstr_piece(i1+1)) == 'ip') then
             H_potential='IP'
           endif
           !
           if ( index(trim(rstr_piece(i1+1)),'h')==1 ) then
             if(trim(H_potential)/="") H_potential='HARTREE+'//trim(H_potential)
             if(trim(H_potential)=="") H_potential='HARTREE'
           endif
           !
           if ( trim(rstr_piece(i1+1)) == 'd') then
             H_potential='DEFAULT'
           endif
           !
           if ( trim(rstr_piece(i1+1)) == 'ip') then
             H_potential='IP'
           endif
           !
         endif
#endif
         !
#if defined _MAGNETIC
         if ( trim(rstr_piece(i1)) == 'magnetic' )  then
           if ( trim(rstr_piece(i1+1)) == 'p' ) then
             MAG_hamiltonian_type='pauli'
             MAG_pauli=.true.
           else if ( trim(rstr_piece(i1+1)) == 'l' ) then
             MAG_hamiltonian_type='landau'
             MAG_landau=.true.
           else if ( trim(rstr_piece(i1+1)) == 'a') then
             MAG_hamiltonian_type='all'
             MAG_landau=.true.
             MAG_pauli=.true.
           endif
         endif
#endif
         !
#if defined  _ELPH || defined _QED
         !
         ! ELPH/QED
         !
         if ( trim(rstr_piece(i1)) == 'corrtp' ) then
           if ( trim(rstr_piece(i1+1)) == 'e') l_elel_corr=.true.
           if ( trim(rstr_piece(i1+1)) == 'p') l_elph_corr=.true.
           if ( trim(rstr_piece(i1+1)) == 'h') l_elphoton_corr=.true.
           if ( trim(rstr_piece(i1+1)) == 'a') then
             l_elel_corr=.true.
             l_elph_corr=.true.
             l_elphoton_corr=.true.
           endif
           if (.not.l_elph_corr.and..not.l_elel_corr.and..not.l_elphoton_corr) l_elel_corr=.true.
           if (l_elph_corr)     call initactivate(1,'el_ph_corr')
           if (l_elel_corr)     call initactivate(1,'el_el_corr')
           if (l_elphoton_corr) call initactivate(1,'el_photon_corr')
         endif
#endif
         !
#if defined _RT 
         !
         ! RT
         !
         if ( trim(rstr_piece(i1)) == 'negf' ) then
           !
           if ( trim(rstr_piece(i1+1)) == 'p'    ) n_ext_fields=1
           if ( trim(rstr_piece(i1+1)) == 'pp'   ) n_ext_fields=2
           !
           do i_field=1,n_ext_fields_max
             if ( index(rstr_piece(i1+1),'p'//trim(intc(i_field))) > 0 ) n_ext_fields=i_field
           enddo
           !
           if( n_ext_fields==0) n_ext_fields=1
           !
         endif
         !
         if ( trim(rstr_piece(i1)) == 'scattp' ) then
           if ( trim(rstr_piece(i1+1)) == 'e'.or. trim(rstr_piece(i1+1)) == 'a') l_elel_scatt=.true.
           if ( trim(rstr_piece(i1+1)) == 'p'.or. trim(rstr_piece(i1+1)) == 'a') l_elph_scatt=.true.
#endif
#if defined _RT 
           if ( trim(rstr_piece(i1+1)) == 'h'.or. trim(rstr_piece(i1+1)) == 'a') l_elphoton_scatt=.true.
           if (l_elphoton_scatt) call initactivate(1,'el_photon_scatt')
#endif
#if defined _RT 
           if (l_elph_scatt)     call initactivate(1,'el_ph_scatt')
           if (l_elel_scatt)     call initactivate(1,'el_el_scatt')
           if (.not.l_elph_scatt.and..not.l_elel_scatt.and..not.l_elphoton_scatt) l_elph_scatt=.true.
         endif
         !
#endif
         !
#if defined _SC || defined _RT 
         !
         ! COLLISIONS
         !
         if (runlevel_is_on('collisions')) then
           if ( l_elel_scatt.or.trim(H_potential)=='COH+SEX'.or.&
&               trim(H_potential)=='COH'.or.trim(H_potential)=='SEX' ) call initactivate(1,'em1s')
         endif
         !
#endif
         !
       enddo INPUT_strings_loop
       !
     enddo
     !
     if (infile_editing) init_=1
     !
   end subroutine
   !
end function
