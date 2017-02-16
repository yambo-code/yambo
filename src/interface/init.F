!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
 use pars,           ONLY:schlen,lchlen,HARTREE
 use electrons,      ONLY:levels,E_reset
 use frequency,      ONLY:w_samp,W_duplicate,W_reset
 use it_m,           ONLY:it,initactivate,initdefs,rstatus,nrnlvls,&
&                         initmode,rnlvls,initinfio,infile_verbosity,&
&                         infile,infile_dump,ofiles_append,runlevel_is_on,&
&                         switch_off_runlevel
 use par_proc_m,     ONLY:pp_redux_wait,master_cpu
 use com,            ONLY:msg,repfile,com_path,jobstr,&
&                         write_the_logo,file_exists,rename_file,write_to_report
 use vec_operate,    ONLY:v_norm
 use timing,         ONLY:live_timing_is_on,what_is_running
 use QP_m,           ONLY:QP_t,QP_ng_Sx,QP_solver,&
&                         QP_state,QP_reset,QP_ctl_E,QP_table
 use X_m,            ONLY:X_t,X_duplicate,X_reset
 use stderr,         ONLY:tty_size,logfile
 use R_lattice,      ONLY:ng_closed,q0_def_norm,bz_samp,nqibz,bz_samp_reset 
 use wave_func,      ONLY:ioWF
 use IO_m,           ONLY:io_control,OP_RD_CL,DUMP,NONE
 use TDDFT,          ONLY:ioBS_Fxc
 use BS,             ONLY:BS_bands,BS_n_g_exch,BS_n_g_W,BSS_mode,BSS_q0,&
&                         BS_eh_en,BS_res_mode,BS_cpl_mode
 !
 implicit none
 type(levels) ::en        
 type(bz_samp)::q,k   
 type(X_t)    ::X(4)
 type(w_samp) ::Xw(4)
 integer         ::lnstr
 character(lnstr)::instr
 logical         ::CLOSE_Gs,FINALIZE
 !
 ! Work Space
 !
 integer           :: io_err,ioWF_err,io_X_err(4),io_ID,ioBS_err,&
&                     ioBS_Fxc_err,ioQINDX_err,io_SC_err
 integer, external :: ioX,ioOSTNTS,ioGROT,ioQINDX,ioRIM,ioE_RIM,&
&                     ioXXVXC,ioQP,ioBS,ioDB1,ioKB_PP,&
&                     ioCOL_CUT
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
 what_is_running='YAMBO'
 !
 !Presets (input)
 !
 init = 0
 !
 if (.not.FINALIZE.and..not.CLOSE_Gs) then
   call E_reset(en)
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
 !Presets (local)
 !
 call QP_reset(qp)
 call W_reset(Xxcw)
 call X_reset(Xbsk)
 !
 if (FINALIZE) then
   call call_init_load('GameOver')
   call initinfio(defs,11)
   if (master_cpu) call ofiles_append(defs=defs)
   return
 endif
 if (CLOSE_Gs) then
   call call_init_load('Gclose')
   call barriers( )
   call logicalson
   return
 endif
 !
 ! First vars loading
 ! 
 call call_init_load('load')
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
 !Dump the input file
 !
 if (file_exists(trim(infile))) call infile_dump()
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
 if (io_err==-1.or.ioQINDX_err==-1) then
   !
   ! switch off all logicals loaded in read_command_line
   call switch_off_runlevel('all',on_name="")
   !
   ! force a setup run
   instr="setup"
   call read_command_line(instr,init)
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
 !
 ! xxvxc
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=ioXXVXC(io_ID) 
 !
 ! QP
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),MODE=DUMP,ID=io_ID)
 io_err=ioQP('QP',qp,io_ID) 
 !
 !In DUMP mode qp%table is dumped as well (to be used in QP_apply).
 !Here, however, qp%table is not needed
 !
 if (associated(qp%table)) nullify(qp%table)
 !
 !
 ! ostnts
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=ioOSTNTS(X(3),en,io_ID)
 !
 ! kb_pp
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=ioKB_PP(io_ID)
 !
 ! If the KB non local factors are found set to T the Vnl_included logical
 !
 if (io_err==0) forall(i1=1:4) X(i1)%Vnl_included=.true.
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
 ! bs
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=io_ID)
 ioBS_err=ioBS(1,Xbsk,io_ID)
 !
 !
 ! ELPH 
 !
 !
 ! SC 
 !
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
 !Note that here ('todef') is the latest chance to change
 !a predefined var verbosity and to force its appearnce
 !in the input file.
 !
 call call_init_load('todef')
 !
 !Input file/local cache -> local cache/Variables
 !
 call call_init_load('parserload')
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
 call initactivate(1,'StdoHash Nelectro ElecTemp OccTresh More_IO_Path Com_Path')
 !
 !FFT
 !
 if (any((/(l_optics.and.l_chi),(l_optics.and.l_bse),l_em1d,&
&          l_em1s,l_acfdt,l_xxvxc,l_col_cut/))) call initactivate(1,'FFTGvecs')
 !
 !Setup
 !
 if (l_setup) call initactivate(1,'MaxGvecs IkSigLim IkXLim')
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
 if (l_xxvxc) call initactivate(1,'EXXRLvcs SxLowBnd') 
 !
 ! Optics(not bse) nor GW (no PP)
 !
 if ((l_optics.and.l_chi).or.(l_em1d.and..not.l_ppa)) then
   call initactivate(1,'XfnQPdb XfnQP_N XfnQP_E XfnQP_W XfnQP_Z LongPath')
   call Xon('Xd',(/'QpntsR','BndsRn','NGsBlk',&
&                  'GrFnTp','EnRnge','DmRnge','CGrdSp','ETStps','EMStps',&
&                  'DrudeW','EhEngy','LongDr'/))
 endif
 !
 ! BSE 
 !
 if (l_optics.and.l_bse) then
   call initactivate(1,'KfnQPdb KfnQP_N KfnQP_E KfnQP_W KfnQP_Z LongPath')
   call initactivate(1,'BSresKmod BScplKmod BSEQptR BSEBands')
   call initactivate(1,'BSENGBlk BSENGexx BSEEhEny BSEClmns BSehWind')
 endif
 !
 ! Static screen 
 !
 if (l_em1s) then
   call initactivate(1,'XfnQPdb XfnQP_E XfnQP_W XfnQP_Z LongPath')
   call Xon('Xs',(/'QpntsR','BndsRn','NGsBlk','CGrdSp','EhEngy',&
&                  'LongDr','DrudeW'/))
 endif
 !
 ! GW (PPA & CHOSEX) 
 !
 if ( (l_em1d.and.l_ppa) .or. (l_em1s.and.l_chosex)) then
   call initactivate(1,'XfnQPdb XfnQP_N XfnQP_E XfnQP_W XfnQP_Z LongPath')
   if (l_ppa) call Xon('Xp',(/'QpntsR','BndsRn','NGsBlk','CGrdSp',&
&                      'EhEngy','LongDr','PPAPnt'/))
   if (l_chosex) call Xon('Xs',(/'QpntsR','BndsRn','NGsBlk',&
&                         'EhEngy','LongDr'/))
 endif
 !
 ! ACFDT
 !
 if (l_acfdt) then
   call initactivate(1,'XfnQPdb XfnQP_N XfnQP_E XfnQP_W XfnQP_Z LongPath')
   call initactivate(1,'EXXRLvcs AC_n_LAM AC_n_FR AC_E_Rng')
   call Xon('Xx',(/'QpntsR','BndsRn','NGsBlk','CGrdSp','EhEngy','LongDr'/))
 endif
 !
 ! GW/Life
 !
 if (l_gw0.or.l_life) then
   !
   if (l_el_corr) then
     !
     call initactivate(1,'XfnQPdb XfnQP_N XfnQP_E XfnQP_W XfnQP_Z')
     call initactivate(1,'GfnQPdb GfnQP_N GfnQP_E GfnQP_W GfnQP_Z')
     call initactivate(1,'BoseCut LongPath')
     !
     if (l_gw0) then
       call initactivate(1,'GbndRnge') 
       if (.not.l_chosex.and.trim(QP_solver)/='g') call initactivate(1,'GDamping') 
       if (.not.l_ppa.and..not.l_chosex) call Xon('Xd',(/'BndsRn','NGsBlk',&
&                      'DmRnge','CGrdSp','ETStps','EMStps',&
&                      'DrudeW','EhEngy','LongDr'/)) 
       !
       if (.not.l_chosex) call initactivate(1,'DysSolver')
       if (     l_chosex) call initactivate(1,'UseEbands')
       if (trim(QP_solver)=="g") then
         call initactivate(1,'GEnSteps GEnRnge GDmRnge') 
       else
         call initactivate(1,'GWoIter')
         if (.not.l_chosex) call initactivate(1,'NewtDchk QPreport')
       endif
       !
     endif
     !
     if (l_life) then
       call initactivate(1,'LifeTrCG')
       if (l_el_corr) call Xon('Xd',(/'BndsRn','NGsBlk',&
&                                     'DmRnge','CGrdSp',&
&                                     'DrudeW','EhEngy','LongDr'/)) 
     endif
   endif 
   !
   !
 endif
 !
 ! Optics + BS based TDDFT (BS fxc is only of q=0 0 0)
 !
 if (l_bs_fxc.and.l_optics.and.l_chi) call initactivate(1,'FxcGRLc FxcSVdig FxcCausal')
 !
 ! ALDA/LRC Tddft
 !
 if (l_alda_fxc.and.&
&    any((/l_optics.and.l_chi,l_em1s,l_em1d,l_acfdt,l_ppa,l_chosex,l_gw0/))) &
&    call initactivate(1,'FxcGRLc')
 if (l_lrc_fxc.and.&
&    any((/l_optics.and.l_chi,l_em1s,l_em1d,l_acfdt,l_ppa,l_chosex,l_gw0/))) &
&    call initactivate(1,'LRC_alpha')
 !
 ! BSE + TDDFT = no BS db, Fxc + LF on-fly
 !
 if (l_bs_fxc.and.l_bse) then
   call initactivate(1,'BLongDir BEnRange BDmRange BEnSteps')
   call initactivate(1,'FxcGRLc FxcSVdig FxcCausal FxcMEStps')
   call initactivate(-1,'BScplKmod')
 endif
 !
 ! BSE + ALDA TDDFT = no correlation
 !
 if (l_alda_fxc.and.l_bse) then
   call initactivate(-1,'BSENGBlk BSEEhEny BSEClmns BSehWind')
 endif
 !
 ! BSE solver
 !
 if (l_bss) then
  call initactivate(1,'KfnQPdb KfnQP_N KfnQP_E KfnQP_W KfnQP_Z')
  call initactivate(1,'BSSmod BEnRange BDmRange BDmERef BEnSteps BLongDir WRbsWF')
  if (index(BSS_mode,'h')/=0)  call initactivate(1,'BSHayTrs')
  if (index(BSS_mode,'t')/=0)  call initactivate(1,'FxcGRLc FxcSVdig FxcMEStps FxcCausal')
  if (index(BSS_mode,'i')/=0)  call initactivate(1,'SkipInv FullInv')
 endif
 !
 ! DFT
 !
 !
 ! RAS
 !
 !
 ! REELS
 !
 !
 ! Are we editing the input file ?
 !
 if (infile_editing) then
   open(unit=12,file=trim(infile))
   call initinfio(defs,12)
   close(12)
   call pp_redux_wait
 endif
 !
 ! To handle externally defined Q-points I use the init_q_pts
 !
 if (l_setup.and.ioQINDX_err/=0) call init_q_pts()
 !
 !If qp limits are requested they are added at the end of the input file
 !
 if ( any((/l_xxvxc,l_gw0,l_life/)).and..not.l_sc_run ) then
   !
   ! The QP_state is read from ioxxvxc in DUMP mode.
   ! If it is not allocated I define it on the basis 
   ! of the input file
   !
   if (.not.allocated(QP_state)) call QP_state_table_setup(en)
   !
   ! I use it to propose the value in the input file ...
   !
   call QP_init(.TRUE.,.TRUE.)
   !
   ! ... but afterword I must deallocate it to use user defined values
   !
   deallocate(QP_state)
   if (allocated(QP_table)) deallocate(QP_table)
   !
 endif
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
 call pp_redux_wait
 if (master_cpu) open(unit=11,file=trim(repfile))
 call write_the_logo(11,' ')
 !
 contains
   !
   subroutine Xon(mode,what)
     !
     character(2)::mode
     character(6)::what(:)
     !
     ! Work Space
     !
     character(8)::lch
     integer     ::i1
     !
     do i1=1,size(what,1)
       write (lch,'(2a)') what(i1),mode
       call initactivate(1,lch)
     enddo
   end subroutine
   !
   subroutine call_init_load(mode)
     character(*)::mode
     if (mode=='load') initmode=0
     if (mode=='todef') initmode=1
     if (mode=='Gclose') initmode=2
     if (mode=='GameOver') initmode=3
     if (mode=='parserload') initmode=4
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
       l_rim=runlevel_is_on('rim_cut')
       l_col_cut=runlevel_is_on('rim_cut')
       l_xxvxc=runlevel_is_on('xxvxc')
       l_em1d=runlevel_is_on('em1d')
       l_em1s=runlevel_is_on('em1s')
       l_ppa=runlevel_is_on('ppa')
       l_gw0=runlevel_is_on('gw0')
       l_life=runlevel_is_on('life')
       l_bss=runlevel_is_on('bss')
       l_acfdt=runlevel_is_on('acfdt')
       l_bs_fxc=runlevel_is_on('bs_fxc')
       l_alda_fxc=runlevel_is_on('alda_fxc')
       l_lrc_fxc=runlevel_is_on('lrc_fxc')
       l_chosex=runlevel_is_on('chosex')
       l_el_corr=l_gw0.or.l_life
       !
       ! Check if this runlevel is allowed in the 
       ! present configuration
       !
       if (i1==1) call barriers( )
       !
     enddo
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
       QP_ctl_E(1,:)=QP_ctl_E(2,:)
       X(3)%ib= BS_bands
       X(3)%ehe=BS_eh_en
       X(3)%q0= BSS_q0
       X(3)%iq= 1
       call W_duplicate(Xxcw,Xw(3))
       call initactivate(2,'XfnQP_E')
     endif
     if (l_gw0.and.l_el_corr) call initactivate(1,'xxvxc')
     if (l_ppa)    call initactivate(1,'em1d')
     if (l_chosex) call initactivate(1,'em1s')
     if (l_bss)    call initactivate(1,'bse')
     if (l_bss)    call initactivate(1,'l_optics')
     if (l_bse) then
       !
       if (l_alda_fxc) BS_res_mode='x'     
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
     if (l_em1s) Xw(2)%dr=0.001/HARTREE
     !
     if (len_trim(BS_cpl_mode)==0) BS_cpl_mode="none"
     if (len_trim(BS_res_mode)==0) BS_res_mode="none"
     !
     ! When running BSE from input file l_bse is FALSE in varsetup1.
     ! In any case I have to overwrite X(2) with PP X(4) only if em1s=F
     !
     if (l_bse.and.io_X_err(2)/=0.and.io_X_err(4)==0.and..not.l_em1s) then
       call X_duplicate(X(4),X(2))
       call W_duplicate(Xw(4),Xw(2))
     endif
     if (l_gw0) l_el_corr=.true.
     !
   end subroutine varsetup2
   !
   subroutine read_command_line(rstr,init_) 
     !
     use stderr, ONLY:string_split
     implicit none
     integer     :: init_
     character(*):: rstr
     !
     ! Work Space
     !
     integer          ::i1,i2,n_pieces
     character(schlen)::rstr_piece(2*nrnlvls)
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
     do i1=1,n_pieces
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
&               trim(rstr_piece(i1-1)) /= 'jobstr' ) infile_editing=.true.
         endif
       enddo
       !
       if (i1>1) then
         if (trim(rstr_piece(i1-1)) == 'jobstr') cycle
       endif
       !
       ! Run Level
       !
       call initactivate(1, trim(rstr_piece(i1)) )
       !
       ! Verbosity
       !
       if ( trim(rstr_piece(i1)) == 'infver' )  read(rstr_piece(i1+1),*) infile_verbosity
       if (infile_verbosity<0) infile_verbosity=0
       !
       ! TDDFT
       !
       if ( trim(rstr_piece(i1)) == 'tddft' )  then
         l_alda_fxc=  trim(rstr_piece(i1+1)) == 'a'
         !
         !
         l_lrc_fxc= trim(rstr_piece(i1+1)) == 'l'
         if (.not.l_alda_fxc.and..not.l_bs_fxc.and..not.l_lrc_fxc) l_alda_fxc=.true.
         if (l_alda_fxc)  call initactivate(1,'alda_fxc optics')
         if (l_lrc_fxc)  call initactivate(1,'lrc_fxc chi optics')
         !
         !
         infile_editing=.true.
       endif
       !
       ! BSE/LLR
       !
       if ( trim(rstr_piece(i1)) == 'optics' )  then
         l_chi= trim(rstr_piece(i1+1)) == 'c'
         l_bse= trim(rstr_piece(i1+1)) == 'b'
         if (.not.l_chi.and..not.l_bse) l_chi=.true.
         if (l_chi) call initactivate(1,'chi')
         if (l_bse) call initactivate(1,'bse')
       endif
       !
       ! BSE Solver
       !
       if ( trim(rstr_piece(i1)) == 'bss' )  then
         BSS_mode=trim(rstr_piece(i1+1))
         if (index(BSS_mode,'h')==0.and.index(BSS_mode,'d')==0.and.&
&            index(BSS_mode,'i')==0.and.index(BSS_mode,'t')==0) BSS_mode='h'
         !
         ! With and ALDA Fxc the t solver is not permitted
         !
         if (l_alda_fxc.and.index(BSS_mode,'t')/=0) BSS_mode='h'
         !
       endif
       !
       ! Dyson Solver
       !
       if ( trim(rstr_piece(i1)) == 'gw0' ) then
         QP_solver=trim(rstr_piece(i1+1))
         if (trim(QP_solver)/='n'.and.trim(QP_solver)/='s'.and.&
&            trim(QP_solver)/='g') QP_solver='n'
         l_el_corr=.true.
         !
         
         infile_editing=.true.
         call initactivate(1,'ppa')
         
         !
       endif
       !
       ! GW approximation 
       !
       if ( trim(rstr_piece(i1)) == 'gwapprx' ) then
         !
         !
       endif
       !
       ! RAS
       !
       !
       ! Sc
       !
       !
       ! ELPH
       !
       !
     enddo 
     !
     if (infile_editing) init_=1
     !
   end subroutine
   !
end function
