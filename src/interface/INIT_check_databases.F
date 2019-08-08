!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine INIT_check_databases(X,Xbsk,qp,Xxcw,Xw,q,k,en,en_sc,Dip,lnstr,instr,INIT,&
&                          io_X_err,io_BS_err,io_BS_Fxc_err,io_QINDX_err,io_ELPH_err,io_SC_E_err)
 !
 use electrons,      ONLY:levels,n_spinor
 use LOGO,           ONLY:code_branch
 use drivers,        ONLY:infile_editing
 use X_m,            ONLY:X_t,X_duplicate
 use DIPOLES,        ONLY:DIPOLE_t
 use frequency,      ONLY:w_samp,W_duplicate
 use BS,             ONLY:BS_n_g_exch,BS_q
 use R_lattice,      ONLY:ng_closed,nqibz,bz_samp
 use QP_m,           ONLY:QP_t,QP_ng_Sx,QP_ng_Sc,QP_ng_SH,QP_ng_Vxc
 use wave_func,      ONLY:io_WF
 use TDDFT,          ONLY:io_BS_Fxc
 use it_m,           ONLY:switch_off_runlevel,initactivate
 use IO_m,           ONLY:io_control,OP_RD_CL,DUMP,NONE,mk_dir
 use com,            ONLY:com_path,more_io_path,core_io_path
 use parallel_m,     ONLY:ncpu
#if defined _NL
 use wave_func,      ONLY:wf_ng
#endif
#if defined _SC || defined _RT
 use collision_ext,  ONLY:COH_collisions,HXC_collisions,P_collisions,GW_NEQ_collisions
 use COLL_interfaces,ONLY:io_COLLISIONS
#endif
 !
#include<memory.h>
 !
 type(X_t)        :: X(4),Xbsk
 type(QP_t)       :: qp
 type(w_samp)     :: Xxcw,Xw(4)
 type(levels)     :: en,en_sc
 type(bz_samp)    :: q,k
 type(DIPOLE_t)   :: Dip
 !
 integer          :: lnstr
 character(lnstr) :: instr
 integer          :: INIT
 integer          :: io_X_err(4),io_BS_err,io_BS_Fxc_err,io_QINDX_err,io_ELPH_err,io_SC_E_err
 !
 ! Workspace
 !
 integer           :: i1,io_ID,io_err,io_WF_err,io_KB_abinit_err,io_KB_pwscf_err,io_ATMPROJ_pwscf_err
 !
 integer, external :: io_X,io_DIPOLES,io_GROT,io_QINDX,io_RIM,&
&                     io_HF_and_locXC,io_QP_and_GF,io_BS,io_DB1,io_KB_abinit,io_KB_pwscf,&
&                     io_COL_CUT,io_Double_Grid,io_E_SOC_map,io_ATMPROJ_pwscf
 !
 logical           :: OSTNTS_Vnl_included
 !
#if defined _SC
 integer           :: io_SC_V_err
#endif
#if defined _RT
 integer           :: io_G,io_OBS
#endif
#if defined _SC || defined _RT
 integer           :: io_COLLISIONS_err
#endif
#if defined _SC || defined _RT
 integer, external :: io_SC_components,io_RT_components
#endif
#if defined _ELPH 
 integer, external :: io_ELPH
#endif 
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
 Dip%ib= (/1,en%nb/)
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
 QP_ng_Vxc=ng_closed
 !
 BS_n_g_exch=ng_closed
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
   call INIT_read_command_line(instr,INIT)
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
 ! Vnl PWscf old format
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=io_DIPOLES(Dip,io_ID)
 !
 ! KB PWscf
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_KB_pwscf_err=io_KB_pwscf(io_ID)
 !
 ! KB abinit
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_KB_abinit_err=io_KB_abinit(io_ID)
 !
 ! ATOMIC PROJETIONS pwscf
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_ATMPROJ_pwscf_err=io_ATMPROJ_pwscf(io_ID)
 !
 !
 if (io_err==0) OSTNTS_Vnl_included= Dip%Vnl_included
 if (io_err/=0) OSTNTS_Vnl_included= (io_KB_abinit_err==0.or.io_KB_pwscf_err==0)
 !
 ! I transfer to all X types the X(3) used in the previous io's 
 !
 call X_var_setup
 !
 ! When no setup has been done nqibz=0
 !
 if (nqibz>0) then
   !
   if (trim(code_branch)=="devel-cumulant") BS_q=(/1,nqibz/)
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
 contains
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
end subroutine
