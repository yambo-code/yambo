!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
integer function init(en,q,k,X,Xw,instr,lnstr,CLOSE_Gs,FINALIZE)
 !
 ! init =-2  : Unsupported Runlevel(s) combination
 ! init =-1  : Missing CORE DBs
 ! init = 0  : everything is OK. Job continues.
 ! init = 1  : input file editing mode
 ! init = 2  : DB listing mode
 !
 use drivers
 use pars,           ONLY:schlen,lchlen
 use parser_m,       ONLY:parser
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels,E_reset,n_sp_pol
 use frequency,      ONLY:w_samp,W_duplicate,W_reset
 use it_m,           ONLY:it,initactivate,initdefs,rstatus,nrnlvls,&
&                         initmode,rnlvls,initinfio,infile_verbosity,&
&                         infile,infile_dump,ofiles_append,runlevel_is_on,&
&                         switch_off_runlevel
#if defined _RT 
 use it_m,           ONLY:V_real_time
#endif
 use parallel_m,     ONLY:PP_redux_wait,master_cpu
 use com,            ONLY:msg,repfile,com_path,jobstr,more_io_path,core_io_path,&
&                         write_the_logo,file_exists,rename_file,write_to_report
 use vec_operate,    ONLY:v_norm
 use timing,         ONLY:live_timing_is_on,what_is_running
 use QP_m,           ONLY:QP_t,QP_ng_Sx,QP_solver,QP_nb,QP_nk,&
&                         QP_state,QP_reset,QP_ctl_E,QP_table
 use X_m,            ONLY:X_t,X_duplicate,X_reset,Chi_mode
 use stderr,         ONLY:tty_size,logfile,string_add,string_remove
 use R_lattice,      ONLY:ng_closed,q0_def_norm,bz_samp,nqibz,bz_samp_reset 
 use wave_func,      ONLY:ioWF
 use IO_m,           ONLY:io_control,OP_RD_CL,DUMP,NONE,mk_dir
 use TDDFT,          ONLY:ioBS_Fxc
 use BS,             ONLY:BS_bands,BS_n_g_exch,BS_n_g_W,BSS_mode,BSS_q0,&
&                         BS_eh_en,BSE_mode,BS_res_mode,BS_cpl_mode,BSK_mode
#if defined _DISTRIBUTED
 use BS,             ONLY:ioBS_DISTRIBUTED
#endif
#if defined  _ELPH 
 use ELPH,           ONLY:elph_nDBs,elph_nDBs_used,elph_use_q_grid
#endif
#if defined _SC
 use global_XC,      ONLY:SC_HF,SC_COHSEX,SC_HF
 use SC,             ONLY:SC_potential,SC_ng_Sx,SC_X_bands,SC_X_size
 use collisions_IO,  ONLY:io_COLLISIONS,V_kind
#endif
#if defined _RT
 use real_time,      ONLY:l_RT_probe,l_RT_pump_and_probe,Dynamics_Kind
 use fields,         ONLY:Efield,i_Pump,i_Probe,Efield_reset,Efield_duplicate
#endif
#if defined _MAGNETIC
 use magnetic,        ONLY:MAG_hamiltonian_type,MAG_pauli,MAG_landau
#endif
#if defined _SURF
 use ras_module,     ONLY:lras, lreels
#endif
 !
 implicit none
 !
 type(levels) ::en        
#if defined _SC
 type(levels) ::en_sc
#endif
 type(bz_samp)::q,k   
 type(X_t)    ::X(4)
 type(w_samp) ::Xw(4)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE
 !
 ! Work Space
 !
 integer           :: io_err,ioWF_err,io_X_err(4),io_ID,ioBS_err,io_KB_abinit_err,io_KB_pwscf_err,&
&                     ioBS_Fxc_err,ioQINDX_err,io_SC_E_err,io_SC_V_err,io_COLLISIONS_err,io_J_and_P,io_G,io_Gr
 integer, external :: ioX,io_DIPOLES,ioGROT,ioQINDX,ioRIM,&
&                     io_HF_and_locXC,ioQP,ioBS,ioDB1,ioKB_abinit,ioKB_pwscf,&
&                     ioCOL_CUT
 logical           :: OSTNTS_Vnl_included,QP_state_in_input_file
 character(6)      :: X_size
 !
 integer, external :: ioE_RIM
 !
#if defined _SC || defined _RT
 integer, external :: io_SC_components,io_G_retarded,io_RT_components
#endif
#if defined _ELPH 
 integer           :: ioELPH_err
 integer, external :: ioELPH
#endif
 !
 type(X_t)        ::Xbsk
 type(QP_t)       ::qp
 type(w_samp)     ::Xxcw
 type(initdefs)   ::defs
 character(lchlen)::jch,rch
 integer          ::i1
 !
 ! What is running ?
 !
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
#if defined _MAGNETIC
 what_is_running='YAMBO_MAGNETIC'
#endif
#if defined _SURF
 what_is_running='YAMBO_SURF'
#endif
 !
 !Presets (input)
 !
 init = 0
 !
 if (.not.FINALIZE.and..not.CLOSE_Gs) then
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
   call call_init_load('Game_Over')
   call initinfio(defs,11)
   if (master_cpu) call ofiles_append(defs=defs)
   return
 endif
 if (CLOSE_Gs) then
   call call_init_load('Close_G_vectors')
   call barriers( )
   call logicalson
   return
 endif
 !
 ! First vars loading
 ! 
 call call_init_load('create_shadow_vars')
 !
 ! DB props listing mode ?
 !
 call read_command_line(instr,init)
 if (index(instr,'dbpr')>0) then
   list_dbs=.true.
   init = 2
   if (tty_size<0) write (logfile,'(2a)') trim(com_path),'/l_dbs'
   live_timing_is_on=.false.
   write_to_report=.false.
 endif
 !
 ! Dump the input file
 !
 if (file_exists(trim(infile))) then
   call infile_dump()
 else if (.not.infile_editing) then
   infile='(none)'
 endif
 !
 ! BASICAL DATABASES
 !
 ! db1
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),COM=NONE,MODE=DUMP,ID=io_ID)
 io_err=ioDB1(en,k,io_ID) 
 !
 ! wf
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),COM=NONE,MODE=DUMP,ID=io_ID)
 ioWF_err=ioWF(io_ID) 
 if (io_err/=0.or.ioWF_err/=0) then
   init =-1
   return
 else
   call mk_dir(more_io_path)
   call mk_dir(com_path)
   call mk_dir(trim(core_io_path)//"/SAVE")
   call mk_dir(trim(more_io_path)//"/SAVE")
 endif
 !
 X(3)%ib=(/1,en%nb/)
 !
 ! Exporting DB1 informations to variables to be
 ! proposed in the input file.
 !
 X(3)%ib=(/1,en%nb/)
 !
 ! gops
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1,2/),ID=io_ID)
 io_err=ioGROT(io_ID) 
 !
 ! Updates RL variables
 !
 QP_ng_Sx=ng_closed
 BS_n_g_exch=ng_closed
 !
 ! kindx
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 ioQINDX_err=ioQINDX(k,q,io_ID) 
 if (ioQINDX_err==0) call initactivate(-1,'IkSigLim IkXLim MinusQ')
 !
 ! If the GOPS/KINDX DBs are not present, reset to setup run
 !
 if ((io_err==-1.or.ioQINDX_err==-1).and.infile_editing) then
   !
   ! switch off all logicals loaded in read_command_line
   call switch_off_runlevel('all',on_name="")
   !
   ! force a setup run
   instr="setup"
   call read_command_line(instr,init)
   !
 endif
 !
 ! rim
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=ioRIM(io_ID) 
 !
 ! cutoff
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=ioCOL_CUT(io_ID) 
 !
 ! E_rim
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=ioE_RIM(en,io_ID) 
 !
 ! xxvxc
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=io_HF_and_locXC(io_ID) 
 !
 ! QP
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=ioQP('QP',qp,io_ID) 
 !
 !In DUMP mode qp%table is dumped as well (to be used in QP_apply).
 !Here, however, qp%table is not needed
 !
 if (associated(qp%table)) then
   deallocate(qp%table)
   nullify(qp%table)
 endif
 !
 ! Green Functions
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=ioQP('G',qp,io_ID) 
 !
 ! W
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=ioQP('W',qp,io_ID) 
 !
 ! ostnts
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_DIPOLES(X(3),en,io_ID)
 !
 OSTNTS_Vnl_included=io_err==0.and.X(3)%Vnl_included
 !
 ! PP_PWscf
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_KB_pwscf_err=ioKB_pwscf(io_ID)
 !
 ! kb_pp
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_KB_abinit_err=ioKB_abinit(io_ID)
 !
 if (io_err/=0) OSTNTS_Vnl_included=(io_KB_abinit_err==0.or.io_KB_pwscf_err==0)
 !
 ! I transfer to all X types the X(3) used in the previous io's 
 !
 call X_var_setup
 !
 do i1=1,4 ! Xx Xs Xp Xd
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
   io_X_err(i1)=ioX(X(i1),Xw(i1),io_ID)
   if (nqibz>0) X(i1)%iq=(/1,nqibz/)
   if (io_X_err(i1)>0) X(i1)%iq(1)=io_X_err(i1)+1
 enddo
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
#if defined _DISTRIBUTED
 ioBS_err=ioBS_DISTRIBUTED(1,Xbsk,io_ID)
#else
 ioBS_err=ioBS(1,Xbsk,io_ID)
#endif
 !
 ! GPL_EXCLUDE_START
 !
 ! bs_fxc
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 ioBS_Fxc_err=ioBS_Fxc(1,Xxcw,io_ID,X=Xbsk)
 !
 ! GPL_EXCLUDE_END
 !
 ! ELPH 
 !
#if defined _ELPH 
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 ioELPH_err=ioELPH(io_ID,'gkkp')
 !
 if (ioELPH_err/=0) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
   ioELPH_err=ioELPH(io_ID,'gkkp_expanded')
 endif
 !
#endif
 !
 ! RT 
 !
#if defined _RT
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_J_and_P=io_RT_components('J_and_P' ,en,io_ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_G=io_RT_components('G' ,en,io_ID)
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_Gr=io_G_retarded(io_ID,"G_retarded")
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_Gr=io_G_retarded(io_ID,"G_retarded_matrix")
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
 !
 ! COLLISIONS 
 !
 V_kind=SC_HF
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 io_COLLISIONS_err=io_COLLISIONS(0,io_ID)
 if (io_COLLISIONS_err==0) then
   V_kind=SC_COHSEX
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
   io_COLLISIONS_err=io_COLLISIONS(0,io_ID)
   if (io_COLLISIONS_err/=0) V_kind=-1
 endif
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
 call varsetup1
 !
 !Variables(read from DB files) -> local cache
 !
 !Note that here ('load_defaults') is the latest chance to change
 !a predefined var verbosity and to force its appearnce
 !in the input file.
 !
 call call_init_load('load_defaults')
 !
 !Input file/local cache -> local cache/Variables
 !
 call call_init_load('parser_input_file')
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
 call initactivate(1,'StdoHash Nelectro ElecTemp BoseTemp OccTresh EvalMagn')
 !
 !FFT
 !
 if (any((/(l_optics.and.l_chi),(l_optics.and.l_bse),l_em1d,&
&          l_em1s,l_acfdt,l_HF_and_locXC,l_col_cut/))) call initactivate(1,'FFTGvecs')
 !
 !Setup
 !
 if (l_setup) call initactivate(1,'MaxGvecs IkSigLim IkXLim')
#if defined  _ELPH 
 if (l_setup) call initactivate(1,'MinusQ')
#endif
 !
 if (any((/(l_optics.and.l_chi),(l_optics.and.l_bse)/)))  call initactivate(1,'NonPDirs')
 !
 !RIM
 !
 if (l_rim) call initactivate(1,'RandQpts RandGvec QpgFull Em1Anys IDEm1Ref')
 !
 !Col CUTOFF 
 !
 if (l_col_cut) call initactivate(1,'CUTGeo CUTBox CUTRadius CUTCylLen CUTCol_test')
 !
 !XX
 !
 if (l_HF_and_locXC) call initactivate(1,'EXXRLvcs')  
 !
 ! Kernels
 !
 if (l_em1s.or.l_em1d)             call initactivate(1,'Chimod')
 if (l_optics.and.l_chi)           call initactivate(1,'Chimod')
 if (l_optics.and.l_bse)           call initactivate(1,'BSEmod')
 if (l_optics.and.l_bse)           call initactivate(1,'BSKmod')
 if (l_optics.and.l_bse.and.l_bss) call initactivate(1,'BSSmod')
 !
 if (l_optics.and.l_chi) then
   !
   ! GPL_EXCLUDE_START
   !
   ! BS based TDDFT (BS fxc is only of q=0 0 0). BS_fxc db exist
   if(l_bs_fxc) call initactivate(1,'FxcGRLc FxcSVdig FxcCausal')
   !
   ! GPL_EXCLUDE_END
   !
   !
   ! ALDA/LRC Tddft
   if (l_alda_fxc) call initactivate(1,'FxcGRLc')
   if (l_lrc_fxc)  call initactivate(1,'LRC_alpha LRC_beta')
   !
 endif
 !
 ! Optics(not bse) nor GW (no PP)
 !
 if ((l_optics.and.l_chi).or.(l_em1d.and..not.l_ppa)) then
   X_size='NGsBlk'
   if(l_rpa_IP) X_size='      '
   call X_activate('Xd',(/X_size,'QpntsR','BndsRn',&
&                  'GrFnTp','EnRnge','DmRnge','DmERef','CGrdSp','ETStps','EMStps',&
&                  'DrudeW','EhEngy','LongDr'/))
   !
   !
   call QP_ctl_switch('X')
   call initactivate(1,'ShiftedPath Qdirection QShiftOrder')
   !
#if defined _SC
   call initactivate(1,'Gauge OCCTime')
#endif
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
     call initactivate(1,'FxcGRLc FxcSVdig FxcCausal FxcMEStps')
     call initactivate(1,'BLongDir BEnRange BDmRange BEnSteps')
   endif
   !
 endif
 !
 ! BSE
 !
 if (l_optics.and.l_bse) then
   !
   call QP_ctl_switch('K')
   !
   call initactivate(1,'ShiftedPath')
#if defined _SC || defined _KERR
   call initactivate(1,'Gauge')
#endif
#if defined _KERR
   if(l_kerr) call initactivate(1,'EvalKerr AnHall')
#endif
   !
   call initactivate(1,'DrudeWBS')
   call initactivate(1,'BoseCut ShiftedPath')
   call initactivate(1,'BEnRange BDmRange BDmERef BEnSteps BLongDir')
   call initactivate(1,'BSEQptR BSEBands BSEEhEny BSehWind')
   !
 endif                    
 !
 ! BSE solver
 !
 if (l_optics.and.l_bse.and.l_bss) then
   !
   ! Special case: the BSE_Fxc kernel has been constructed
   !               Thus I move to g-space to solve the Dyson equation
   if (index(BSS_mode,'t')/=0 .and. l_bs_fxc) call initactivate(-1,'BSENGexx ALLGexx')
   if (index(BSS_mode,'t')/=0 .and. l_bs_fxc) call initactivate(-1,'BSENGBlk')
   !
   if (index(BSS_mode,'i')/=0)  call initactivate(1,'BSSInvMode BSSInvItMax')
   if (index(BSS_mode,'d')/=0)  call initactivate(1,'WRbsWF')
   if (index(BSS_mode,'h')/=0)  call initactivate(1,'BSHayTrs BSHayTer')
   !
   ! Special project dependent variables
   !
#if defined _ELPH 
   if (l_ph_corr.and..not.elph_use_q_grid) call initactivate(1,'ElPhRndNq')
#endif
 endif
 !
 ! Static screen 
 !
 if (l_em1s) then
   call QP_ctl_switch('X')
   call initactivate(1,'ShiftedPath')
   call X_activate('Xs',(/'QpntsR','BndsRn','NGsBlk','CGrdSp','EhEngy',&
&                  'LongDr','DrudeW'/))
 endif
 !
 ! GW (PPA & COHSEX) 
 !
 if ( (l_em1d.and.l_ppa) .or. (l_em1s.and.l_cohsex)) then
   call QP_ctl_switch('X')
   call initactivate(1,'ShiftedPath')
   if (l_ppa) call X_activate('Xp',(/'QpntsR','BndsRn','NGsBlk','CGrdSp',&
&                      'EhEngy','LongDr','PPAPnt'/))
   if (l_cohsex) call X_activate('Xs',(/'QpntsR','BndsRn','NGsBlk',&
&                         'EhEngy','LongDr'/))
 endif
 !
 ! ACFDT
 !
 if (l_acfdt) then
   call QP_ctl_switch('X')
   call initactivate(1,'ShiftedPath')
   call initactivate(1,'EXXRLvcs AC_n_LAM AC_n_FR AC_E_Rng')
   call X_activate('Xx',(/'QpntsR','BndsRn','NGsBlk','CGrdSp','EhEngy','LongDr'/))
 endif
 !
 ! GW/Life
 !
 if (l_gw0.or.l_life) then
   !
   if (l_el_corr) then
     !
     call QP_ctl_switch('X')
     call QP_ctl_switch('G')
     call initactivate(1,'BoseCut ShiftedPath')
     !
     if (l_gw0) then
       call initactivate(1,'GbndRnge') 
       if (.not.l_cohsex.and.trim(QP_solver)/='g') call initactivate(1,'GDamping') 
       if (.not.l_cohsex) call initactivate(1,'dScStep') 
       if (.not.l_ppa.and..not.l_cohsex) call X_activate('Xd',(/'BndsRn','NGsBlk',&
&                      'DmRnge','DmERef','CGrdSp','ETStps','EMStps',&
&                      'DrudeW','EhEngy','LongDr'/)) 
       !
       if (.not.l_cohsex) call initactivate(1,'GWTerm GwEnComp DysSolver')
       if (     l_cohsex) call initactivate(1,'UseEbands')
       if (trim(QP_solver)=="g") then
         call initactivate(1,'GEnSteps GEnRnge GDmRnge GreenFTresh GreenF2QP') 
       else
         call initactivate(1,'GWoIter')
         if (.not.l_cohsex) call initactivate(1,'NewtDchk ExtendOut OnMassShell QPExpand')
       endif
       !
     endif
     !
     if (l_life) then
       call initactivate(1,'LifeTrCG')
       if (l_el_corr) call X_activate('Xd',(/'BndsRn','NGsBlk',&
&                                     'DmRnge','CGrdSp',&
&                                     'DrudeW','EhEngy','LongDr'/)) 
     endif
   endif 
   !
#if defined  _ELPH 
   !
   call QP_ctl_switch('G')
   !
   if (l_ph_corr) then
     call initactivate(1,'DysSolver')
     call initactivate(1,'GphBRnge ElPhModes GDamping dScStep ExtendOut ElPhRndNq RandQpts')
     call initactivate(1,'WRgFsq NewtDchk OnMassShell')
   endif
   if (trim(QP_solver)=="g".and.l_ph_corr) then
     call initactivate(1,'GEnSteps GEnRnge GDmRnge GreenFTresh GreenF2QP') 
     call initactivate(-1,'WRgFsq NewtDchk GDamping ExtendOut OnMassShell')
   endif
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
 !
 ! DFT
 !
#if defined _SC 
 if (l_sc_run) then
   call initactivate(1,'Potential FFTGvecs Gauge SCmixing SCBands SClwBand SCnlMix BandMix EXXCut')
   call initactivate(-1,'QpntsRXs')
   if (l_cohsex) call initactivate(1,'GbndRnge UseEbands SCUpWIter')
   if (l_collisions_IO) call initactivate(1,'DynKind')
   if (.not.l_collisions_IO) call initactivate(1,'CollPath CollCache')
   if (.not.l_real_time.and..not.l_collisions_IO) then
     call initactivate(1,'SCIter SCEtresh SCRhoTresh TF_precondition')
     if (trim(SC_potential)=='EXX'.or.trim(SC_potential)=='EXXC'.or.&
&        trim(SC_potential)=='SRPA') call initactivate(1,'OEPapprox OEPItSolver') 
   endif
   call initactivate(1,'SCdiag MeanPotential')
   if (l_collisions_IO) call initactivate(-1,'GbndRnge UseEbands MeanPotential') 
 endif
#endif
 !
#if defined _RT
 !
 ! REAL-TIME specific
 !
 if (l_real_time) then
   call QP_ctl_switch('G')
   call initactivate(-1,'SCmixing')
   call initactivate(1,'Integrator DynKind RADLifeTime PhLifeTime RTmaxEn RTpumpEhEn RTpumpEhWd') 
   call initactivate(1,'RTstep NEsteps NETime DipoleEtresh')
   if (l_ph_corr) call initactivate(1,'MemTresh RandQpts RT_T_evol UseDebyeE ElPhModes')
   call initactivate(1,'HXCjumps ThermSteps SwitchOnTime RTDiagoSteps DiskSteps')
   call initactivate(1,'RTfreezeH RTfreezeXC Diamagnetic InducedField SkipGFIO SkipOCCIO IOtime')
   if (l_RT_probe.or.l_RT_pump_and_probe) call Afield_activate('Probe')
   if (              l_RT_pump_and_probe) call Afield_activate('Pump')
   !
   call initactivate(1,'ShiftedPath')
   !
 endif
 !
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
 ! Surface spectroscopy
 !
#if defined _SURF
 if (lras.or.lreels) then
!   call X_activate('Xd',(/'QpntsR','BndsRn','NGsBlk',& ! Head only
    call X_activate('Xd',(/'BndsRn',&
&                 'GrFnTp','EnRnge','DmRnge','CGrdSp','ETStps','EMStps',&
&                 'DrudeW','EhEngy'/))
    call initactivate(1,'FFTGvecs XfnQP_E'//&
&     ' BulkFile BulkForm BlkShift BlkBroad Layers'//&  ! Bulk eps data
&     ' q0x q0y'//&                                     ! Polarizations
&     ' Cutoff CutZero CutStep')               ! Cut off fn
 endif
 if (lras) then
    call initactivate(1,&
&     ' LocLimit LocType')               ! Analyse
 endif
 if (lreels) then
    call initactivate(1,&
&     ' E0 Theta0 Thetap Phi DetAngle'//&               ! Kinematics
&     ' LossForm ImpactFt'//&                        ! Model of loss function
&     ' DetIntMd NumIntPt'//&                           ! Det. integration
&     ' PenDepth GausConv')                    ! General flags
 endif
#endif
 !
 ! Are we editing the input file ?
 !
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call PP_redux_wait
 endif
 !
 ! To handle externally defined Q-points I use the init_q_pts
 !
 ! (a) first check if input file contains already a list of Q-points...
 call init_q_pts(.FALSE.)
 !
 ! (b) ... then (re)write the file
 if (l_setup.and.ioQINDX_err/=0.and.infile_editing) call init_q_pts(.TRUE.)
 !
 !If qp limits are requested they are added at the end of the input file
 !
 if ( any((/l_HF_and_locXC,l_gw0,l_life/)).and..not.l_sc_run.and..not.l_collisions_IO ) then
   !
   ! The QP_state can be read from ioxxvxc in DUMP mode or from the input file.
   ! The DB value is used when a new input file is created or when the previous input file
   ! had no QP_state fields.
   !
   ! QP_state from DB ?
   !
   if (allocated(QP_state)) then
     !
     ! QP_state from input file ?
     !
     call parser('QPkrange',QP_state_in_input_file)
     !
     if (QP_state_in_input_file) then
       !
       deallocate(QP_state)
       QP_nb=0
       QP_nk=0
       !
       call QP_state_table_setup(en)
       !
     endif
     !
   else
     !
     call QP_state_table_setup(en)
     !
   endif
   !
   call QP_init(.TRUE.,.TRUE.)
   !
   ! I use it to propose the value in the input file ...
   !
   ! ... but afterword I must deallocate it to use user defined values
   !
   if (allocated(QP_state)) deallocate(QP_state)
   if (allocated(QP_table)) deallocate(QP_table)
   !
 endif
 !
#if defined _RT
 if(infile_verbosity==V_real_time.and.l_real_time) call DephMatrix_init()
#endif
 !
 if (infile_editing) return
 !
 ! Report/Log Files
 !
 if (trim(jobstr)=='') write (repfile,'(2a)') trim(com_path),'/r'
 if (trim(jobstr)/='') write (repfile,'(4a)') trim(com_path),'/','r-',trim(jobstr)
 if (tty_size<0) then
  if (trim(jobstr)=='') write (logfile,'(2a)') trim(com_path),'/l'
  if (trim(jobstr)/='') write (logfile,'(4a)') trim(com_path),'/','l-',trim(jobstr)
 endif
 do i1=1,nrnlvls
   rch=repfile
   if (rstatus(i1)/=0) write (rch,'(3a)') trim(repfile),'_',trim(rnlvls(i1,1))
   repfile=rch
   if (tty_size<0) then
     jch=logfile
     if (rstatus(i1)/=0) write (jch,'(3a)') trim(logfile),'_',trim(rnlvls(i1,1))
     logfile=jch
   endif
 enddo
 !
 ! Finalize
 !
 if (tty_size<0) call rename_file(logfile)
 call rename_file(repfile)
 call PP_redux_wait
 if (master_cpu) open(unit=11,file=trim(repfile))
 call write_the_logo(11,' ')
 !
 contains
   !
#if defined _RT
   !
   subroutine Afield_activate(field)
     character(*)::field
     call initactivate(1,field//'_Freq')
     call initactivate(1,field//'_MaxFreq')
     call initactivate(1,field//'_FrStep')
     call initactivate(1,field//'_Int')
     call initactivate(1,field//'_Damp')
     call initactivate(1,field//'_kind')
     call initactivate(1,field//'_Dir')
     if (l_RT_pump_and_probe.and.field=="Probe") call initactivate(1,field//'_Tstart')
   end subroutine
   !
#endif
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
   subroutine call_init_load(mode)
     character(*)::mode
     if (mode=='create_shadow_vars') initmode=0
     if (mode=='load_defaults') initmode=1
     if (mode=='Close_G_vectors') initmode=2
     if (mode=='Game_Over') initmode=3
     if (mode=='parser_input_file') initmode=4
     call init_load(defs,en,q,k,X,Xw)
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
       ! 
#if defined _SC
       l_sc_run=runlevel_is_on('scpot')
       l_real_time=runlevel_is_on('negf')
       l_sc_magnetic=runlevel_is_on('magnetic')
#endif
#if defined _RT
       l_collisions_IO=runlevel_is_on('collisions_IO')
       l_carrier_dynamics =trim(Dynamics_Kind)=="C"
#endif
#if defined  _SURF
       lreels=runlevel_is_on('reels')
       lras=runlevel_is_on('ras')
#endif
#if defined  _ELPH  
       if (.not.CLOSE_Gs) then
         !
         ! Only in this case ioELPH_err is defined
         !
         l_ph_corr=runlevel_is_on('el_ph').and.(ioELPH_err==0.or.l_real_time)
         ! GPL_EXCLUDE_START
         l_elph_Hamiltonian=runlevel_is_on('ElPhHam').and.ioELPH_err==0
         ! GPL_EXCLUDE_END
       else
         l_ph_corr=runlevel_is_on('el_ph')
         ! GPL_EXCLUDE_START
         l_elph_Hamiltonian=runlevel_is_on('ElPhHam')
         ! GPL_EXCLUDE_END
       endif
       l_el_corr=runlevel_is_on('el_el')
#else
       l_el_corr=l_gw0.or.l_life
#endif
       !
       ! Check if this runlevel is allowed in the 
       ! present configuration
       !
       if (i1==1) call barriers( )
       !
     enddo
     !
     ! Setup logicals which are not runlevels
     !
     l_rpa_IP     = trim(Chi_mode)=='IP'.or.trim(BSK_mode)=='IP'
     l_td_hartree = trim(Chi_mode)=='Hartree'.or.trim(BSK_mode)=='Hartree'
     if (l_tddft) then
       l_alda_fxc = trim(Chi_mode)=='ALDA'.or.trim(BSK_mode)=='ALDA'
       l_lrc_fxc  = trim(Chi_mode)=='LRC' 
       l_bs_fxc   = trim(Chi_mode)=='BSfxc' .or.trim(BSK_mode)=='BSfxc'
     endif
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
   subroutine varsetup1 
     !
     ! After DB reading/stdin logicals
     ! I propose here values for the input file
     !
     ! If optics with BS FXC I need to dump on X(3) the
     ! Fxc specs
     !
     if (all((/l_bs_fxc,l_optics,l_chi.or.l_bse,ioBS_Fxc_err==0/))) then
       QP_ctl_E(1,:,:)=QP_ctl_E(2,:,:) 
       X(3)%ib= BS_bands
       X(3)%ehe=BS_eh_en
       X(3)%q0= BSS_q0
       X(3)%iq= 1
       call W_duplicate(Xxcw,Xw(3))
       call initactivate(2,'XfnQP_E')
     endif
#if defined _SURF
     X(3)%iq = 1  ! 2 components
     X(3)%ordering = 'c'
     Xw(3)%n = 101 ! 2 components
#endif 
#if defined  _ELPH 
     if (.not.l_el_corr.and..not.l_ph_corr) l_el_corr=.true.
     elph_nDBs_used=elph_nDBs
#endif
     if (l_gw0.and.l_el_corr) call initactivate(1,'HF_and_locXC')
     if (l_ppa)    call initactivate(1,'em1d')
     if (l_cohsex) call initactivate(1,'em1s')
     if (l_W_eh)   call initactivate(1,'em1s')
     if (l_bsk)    call initactivate(1,'optics')
     if (l_bsk)    call initactivate(1,'bse')
     if (l_bss)    call initactivate(1,'optics')
     if (l_bss)    call initactivate(1,'bse')
     if (l_bss)    call initactivate(1,'bsk')
     if (l_bs_fxc) BSS_mode="t"
     if (l_bse) then
       !
       if (io_X_err(2)==0) then
         if (ioBS_err/=0) BS_n_g_W=X(2)%ng
       else if (io_X_err(4)==0) then
         if (ioBS_err/=0) BS_n_g_W=X(4)%ng
         call initactivate(1,'em1d ppa')
       endif
       !
     endif
     !
#if defined _SC
     if (l_sc_magnetic) call initactivate(1,'scpot')
     if (l_sc_run.and.io_SC_E_err==0) then
       QP_ng_Sx=SC_ng_Sx
       X(2)%ib=SC_X_bands
       X(2)%ng=SC_X_size
     endif
#endif
     !
   end subroutine varsetup1 
   !
   subroutine varsetup2 
     !
     ! After infile reading. Immediately before infile writing
     ! CAREFUL! Any input file value is overwritten here !
     !
     Xw(2)%n(2)=Xw(2)%n(1)
     Xw(4)%n(2)=Xw(4)%n(1)
     Xw(3)%n(2)=Xw(3)%n(1)
     Xw(1)%n(2)=Xw(1)%n(1)
     !
     ! q0 renormalization
     !
     BSS_q0(:)=BSS_q0(:)*q0_def_norm/v_norm(BSS_q0)
     X(1)%q0(:)=X(1)%q0(:)*q0_def_norm/v_norm(X(1)%q0)
     X(2)%q0(:)=X(2)%q0(:)*q0_def_norm/v_norm(X(2)%q0)
     X(3)%q0(:)=X(3)%q0(:)*q0_def_norm/v_norm(X(3)%q0)
     X(4)%q0(:)=X(4)%q0(:)*q0_def_norm/v_norm(X(4)%q0)
     if (l_em1s) Xw(2)%dr=0.001/HA2EV
     !
     if (len_trim(BSE_mode)==0) BSE_mode="causal"
     !
     if (l_bse) then
       if (l_rpa_IP)                   BS_res_mode='none'
       if (l_td_hartree)               BS_res_mode='x'
       if (l_tddft)                    BS_res_mode='x'
       if (l_bs_fxc)                   BS_res_mode='c'
       if (l_td_hf)                    BS_res_mode='xcd'
       if (l_W_eh)                     BS_res_mode='xc'
       if (trim(BSE_mode)=="coupling") BS_cpl_mode=trim(BS_res_mode)
       !
       if(l_W_eh) then
         call parser('WehDiag',l_W_eh_diag)
         call parser('WehCpl' ,l_W_eh_cpl)
         if (l_W_eh_diag)              BS_res_mode=trim(string_add(BS_res_mode,'d'))
         if (.not.l_W_eh_cpl)          BS_cpl_mode=trim(string_remove(BS_cpl_mode,'c'))
       endif
     endif
     !
     ! When running BSE from input file l_bse is FALSE in varsetup1.
     ! In any case I have to overwrite X(2) with PP X(4) only if em1s=F
     !
     if (l_bse.and.io_X_err(2)/=0.and.io_X_err(4)==0.and..not.l_em1s) then
       call X_duplicate(X(4),X(2))
       call W_duplicate(Xw(4),Xw(2))
     endif
#if !defined _ELPH 
     if (l_gw0) l_el_corr=.true.
#endif
     !
#if defined _RT 
     !
     ! The logicals l_RT_pump_and_probe and l_RT_probe are defined by read_command_line
     ! if infile_editing=T or here when infile_editing=F and yambo is running
     !
     if (.not.infile_editing) then
       !
       if (trim(Efield(1)%ef_name)/='none'.and.trim(Efield(2)%ef_name)/='none') i_Pump=2
       if (trim(Efield(1)%ef_name)=='none'.and.trim(Efield(2)%ef_name)/='none') then
         call Efield_duplicate(Efield(2),Efield(1))
         call Efield_reset(Efield(2))
       endif
       l_RT_probe         =i_Pump==i_Probe
       l_RT_pump_and_probe=i_Pump/=i_Probe
     endif
#endif
     !
   end subroutine varsetup2
   !
   subroutine read_command_line(rstr,init_) 
     !
     use com,    ONLY:error
     use stderr, ONLY:string_split
     use it_m,   ONLY:V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,&
&                     V_real_time,V_all,rstatus
     implicit none
     integer     :: init_
     character(*):: rstr
     !
     ! Work Space
     !
     integer          ::i1,i2,n_pieces,i3
     character(schlen)::rstr_piece(2*nrnlvls),strings_to_not_use_as_runlevels(nrnlvls)
     !
     ! Bug fix (17/9/2012). If any string following a -### identifier contains 
     ! a string related to a runlevel this is erronously switched on. 
     !
     strings_to_not_use_as_runlevels=" "
     strings_to_not_use_as_runlevels(1)="jobstr"
     strings_to_not_use_as_runlevels(2)="ifile"
     strings_to_not_use_as_runlevels(3)="idir"
     strings_to_not_use_as_runlevels(4)="cdir"
     strings_to_not_use_as_runlevels(5)="odir"
     strings_to_not_use_as_runlevels(6)="cdir"
     !
     ! Split the string in pieces
     !
     call string_split(rstr,rstr_piece)
     n_pieces=0
     do i1=1,2*nrnlvls
       if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
     enddo
     if (n_pieces==0) return
     !
     do i3=1,2
       !
       INPUT_strings_loop: do i1=1,n_pieces
         !
         if (trim(rstr_piece(i1))=='ifile') cycle
         if (i1>1) then
           if(trim(rstr_piece(i1-1))=='ifile') cycle
         endif
         !
         do i2=1,nrnlvls
           if (i1==1) then
              if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i1))) infile_editing=.true.
           else
             if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i1)).and.&
&                 trim(rstr_piece(i1-1)) /= 'jobstr' ) infile_editing=.true.
           endif
         enddo
         !
         if (i1>1) then
           do i2=1,nrnlvls
             if (trim(rstr_piece(i1  )) == trim(strings_to_not_use_as_runlevels(i2))) cycle INPUT_strings_loop
             if (trim(rstr_piece(i1-1)) == trim(strings_to_not_use_as_runlevels(i2))) cycle INPUT_strings_loop
           enddo
         endif
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
             case ('all')
               infile_verbosity=V_all
           end select
         endif
         !
         if ( trim(rstr_piece(i1)) == 'em1s' .or.  trim(rstr_piece(i1)) == 'em1d' ) Chi_mode='hartree'
         !
         ! BSE/LLR
         !
         if ( trim(rstr_piece(i1)) == 'optics' )  then
           !
           l_chi= (trim(rstr_piece(i1+1))=='g' ).or.(trim(rstr_piece(i1+1))=='c')
           l_bse= (trim(rstr_piece(i1+1))=='eh').or.(trim(rstr_piece(i1+1))=='b')
           !
           if (.not.l_chi.and..not.l_bse) l_chi=.true.
           !
           call initactivate(1,'optics')
           if (l_chi) call initactivate(1,'chi')
           if (l_bse) call initactivate(1,'bse')
#if defined _KERR
           l_kerr=l_bse
#endif
           if (l_chi.and.trim(Chi_mode)==' ') Chi_mode='IP'
           if (l_bse.and.trim(BSK_mode)==' ') BSK_mode='IP'
           !
         endif
         !
         ! Approximation used for the BSE/LLR kernel
         !
         if ( trim(rstr_piece(i1)) == 'kernel' )  then
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
           BSK_mode='Hartree'
           Chi_mode='Hartree'
           !
           if(l_bse)  then
             if(trim(rstr_piece(i1+1)) == 'hartree')  BSK_mode='Hartree'
             if(trim(rstr_piece(i1+1)) == 'hf')       BSK_mode='HF'
             if( trim(rstr_piece(i1+1)) == 'alda')    BSK_mode='ALDA'
             if(trim(rstr_piece(i1+1)) == 'sex')      BSK_mode='SEX'
             if( trim(rstr_piece(i1+1)) == 'bsfxc')   BSK_mode='BSfxc'
           else if(l_chi) then
             if(trim(rstr_piece(i1+1)) == 'hartree')  Chi_mode='Hartree'
             if( trim(rstr_piece(i1+1)) == 'alda')    Chi_mode='ALDA'
             if( trim(rstr_piece(i1+1)) == 'lrc')     Chi_mode='LRC'
             if( trim(rstr_piece(i1+1)) == 'bsfxc')   Chi_mode='BSfxc'
           endif
           !
           if((trim(rstr_piece(i1+1)) == 'alda').or.&
             &(trim(rstr_piece(i1+1)) == 'lrc').or.&
             &(trim(rstr_piece(i1+1)) == 'bsfxc')) call initactivate(1,'tddft')
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
&              index(BSS_mode,'i')==0.and.index(BSS_mode,'t')==0) BSS_mode='h'
           !
           ! With and ALDA Fxc the t solver is not permitted
           !
           if (l_alda_fxc.and.index(BSS_mode,'t')/=0) BSS_mode='h'
           if (BSK_mode=='IP') call switch_off_runlevel('bss',on_name=' ')
           if (BSK_mode==' ') then
             BSK_mode="SEX"
             Chi_mode="Hartree"
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
#if !defined  _ELPH 
           l_el_corr=.true.
#endif
           !
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
         ! Surface spectroscopy
         !
#if defined _SURF
         if ( trim(rstr_piece(i1)) == 'sursp' )  then
           if ( trim(rstr_piece(i1+1)) == 'r') call initactivate(1,'ras')
           if ( trim(rstr_piece(i1+1)) == 'e') call initactivate(1,'reels')
           if ( trim(rstr_piece(i1+1)) == 'b') call initactivate(1,'ras reels')
         endif
#endif
         !
         ! Sc / NEGF
         !
#if defined _SC
         if ( trim(rstr_piece(i1)) == 'scpot' )  then
           if ( trim(rstr_piece(i1+1)) == 'exx' ) then
             SC_potential='EXX'
             call initactivate(1,'HF_and_locXC')
           else if ( trim(rstr_piece(i1+1)) == 'exxc' ) then
             SC_potential='EXXC'
             call initactivate(1,'HF_and_locXC')
           else if ( trim(rstr_piece(i1+1)) == 'srpa' ) then
             SC_potential='SRPA'
             call initactivate(1,'cohsex HF_and_locXC')
           else if ( trim(rstr_piece(i1+1)) == 'c' ) then
             SC_potential='COHSEX'
             call initactivate(1,'cohsex HF_and_locXC')
           else if ( trim(rstr_piece(i1+1)) == 'hf') then
             SC_potential='HARTREE-FOCK'
             call initactivate(1,'HF_and_locXC')
           else if ( trim(rstr_piece(i1+1)) == 'h') then
             SC_potential='HARTREE'
           else if ( trim(rstr_piece(i1+1)) == 'd') then
             SC_potential='default'
           endif
         endif
#endif
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
         ! ELPH
         !
#if defined  _ELPH 
         if ( trim(rstr_piece(i1)) == 'corrtp' ) then
           if ( trim(rstr_piece(i1+1)) == 'e') l_el_corr=.true.
           if ( trim(rstr_piece(i1+1)) == 'p') l_ph_corr=.true.
           if ( trim(rstr_piece(i1+1)) == 'b') then
             l_el_corr=.true.
             l_ph_corr=.true.
           endif
           if (.not.l_ph_corr.and..not.l_el_corr) l_el_corr=.true.
           if (l_ph_corr) call initactivate(1,'el_ph')
           if (l_el_corr) call initactivate(1,'el_el')
         endif
#endif
         !
         ! RT
         !
#if defined  _RT 
         if ( trim(rstr_piece(i1)) == 'negf' ) then
           if ( trim(rstr_piece(i1+1)) == 'p' ) l_RT_probe=.true.
           if ( trim(rstr_piece(i1+1)) == 'pp') l_RT_pump_and_probe=.true.
         endif
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
