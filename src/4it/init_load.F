!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine init_load(defs,en,q,k,X,Xw)
!======================================
!
! VERBOSITY
! 
! 0 (default)
! 1 (More)
! 2 (QP)
! 3 (I/O)
! 4 (debug)
!
 use pars,          ONLY:SP,schlen
 use electrons,     ONLY:levels,nel,filled_tresh
 use frequency,     ONLY:w_samp
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,T_unit,V_more,&
&                        V_qp,V_io,V_debug,rstatus,initmode
 use X_m,           ONLY:X_t
 use QP_m,          ONLY:QP_rep_string,QP_cg_percent,QP_G_damp,QP_solver,&
&                        pp_taylor_terms,QP_n_G_bands,QP_ng_Sx,QP_Sx_low_band,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps
 use timing,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel
 use R_lattice,     ONLY:RL_v_comp_acc,n_g_shells,ng_closed,QP_states_k,nXkibz,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,sphere_radius,box_length
 use BS,            ONLY:BS_res_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,BS_columns,&
&                        BS_n_g_W,BS_n_g_exch,BSS_mode,Haydock_treshold,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_q0,BS_cpl_mode
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,&
&                        FXC_SVD_cutoff
 use ACFDT,         ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use QP_m,          ONLY:QP_ctl_E,QP_ctl_db,QP_ctl_interp_neigh
 use com,           ONLY:outpath,inpath
 use functions,     ONLY:bose_E_cut
#if defined PJ_RAS
 use bulkeps,              ONLY : init_bulk
 use eels_kinematics,      ONLY : init_kinematics
 use eels_detector,        ONLY : init_detector
 use optcut,               ONLY : init_cutoff
 use surface_geometry,     ONLY : init_surface
 use analyse_peaks,        ONLY : init_analyse_peaks
 use model_loss_function,  ONLY : init_loss_function
 use convolute,            ONLY : init_convolute
#endif
#if defined PJ_REELS
 use bulkeps,              ONLY : init_bulk
 use eels_kinematics,      ONLY : init_kinematics
 use eels_detector,        ONLY : init_detector
 use surface_geometry,     ONLY : init_surface
 use convolute,            ONLY : init_convolute
 use reels_module,         ONLY : init_reels
 use eels_integration,     ONLY : init_reels_integration
#endif
#if defined PJ_PH
 use ELPH,                 ONLY : QP_PH_n_G_bands
#endif
 implicit none
 type(initdefs)::defs
 type(levels)  ::en          
 type(bz_samp) ::q,k
 type(X_t)     ::X(4)
 type(w_samp)  ::Xw(4)
!
!RunLevels  
 call it('r',defs,'setup',   '[R INI] Initialization')  ! [1]
 call it('r',defs,'optics',  '[R OPT] Optics') ! [2]
 call it('r',defs,'linres',  '[R LR] Linear Response.') ! [3]
 call it('r',defs,'bse',     '[R BSK] Bethe Salpeter Equation.') ! [4]
 call it('r',defs,'rim_cut', '[R RIM CUT] Coulomb interaction') ! [5]
 call it('r',defs,'xxvxc',   '[R XX] Exact Exchange Self-energy and Vxc') ! [6]
 call it('r',defs,'em1d',    '[R Xd] Dynamical Inverse Dielectric Matrix') ! [7]
 call it('r',defs,'em1s',    '[R Xs] Static Inverse Dielectric Matrix') ! [8]
 call it('r',defs,'ppa',     '[R Xp] Plasmon Pole Approximation') ! [9]
 call it('r',defs,'gowo',    '[R GW] GoWo Quasiparticle energy levels') ! [10]
 call it('r',defs,'life',    '[R GW] GoWo Quasiparticle lifetimes') ! [11]
 call it('r',defs,'bss',     '[R BSS] Bethe Salpeter Equation solver') ! [12]
 call it('r',defs,'acfdt',   '[R ETOT] ACFDT Total Energy') ! [13]
 call it('r',defs,'bs_fxc',  '[R TDDFT] The BSE TDDFT kernel') ! [14]
 call it('r',defs,'alda_fxc','[R TDDFT] The ALDA TDDFT kernel') ! [15]
 call it('r',defs,'lrc_fxc', '[R TDDFT] The LRC TDDFT kernel') ! [16]
#if defined PJ_RAS
 call it('r',defs,'sursp',  '[R] Surface Spectroscopy ') ! [17]
 call it('r',defs,'reels',  '[R] Reflection EELS (3-layer model) ') ! [18]
 call it('r',defs,'ras',    '[R] Reflectance anisotropy spectroscopy (RAS)')   ! [19]
#endif
#if defined PJ_REELS
 call it('r',defs,'reels',  '[R] Surface Spectroscopy (REELS)') ! [17]
#endif
#if defined PJ_PH
 call it('r',defs,'el_ph',   '[R] Electron-Phonon Correlation') ! [17]
 call it('r',defs,'el_el',   '[R] Electron-Electron Correlation') ! [18]
#endif
!
! Basics
 call it(defs,'Nelectro','Electrons number',nel,verb_level=V_more)
 call it(defs,'ElecTemp','Electronic Temperature',Tel,T_unit,verb_level=V_more)
 call it(defs,'OutDir',  '[IO] Output(s) directory',outpath,verb_level=V_io)
 call it(defs,'InDir',   '[IO] Input(s) directory',inpath,verb_level=V_io)
 call it(defs,'QPreport','[GW] QP info. Keys: kp/bn/xx/xc/s0/sq/e0/eq/ee/zf/ds/lm/lf',&
&                        QP_rep_string)
 call it(defs,'KptsAcc', '[KPT] Accuracy in vector comparison',RL_v_comp_acc,verb_level=V_more)
 call it(defs,'StdoHash','[IO] Live-timing Hashes',nhash,verb_level=V_io)
 call it(defs,'IkSigLim','[KPT] QP K-points indices range',QP_states_k,verb_level=V_more)
 call it(defs,'IkXLim',  '[KPT] X grid last k-point index',nXkibz,verb_level=V_more) 
 call it(defs,'MaxGvecs','[INI] Max number of G-vectors planned to use',&
&                        ng_closed,unit=G_unit,verb_level=V_more)
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit,verb_level=V_more)
 call it(defs,'OccTresh','Occupation treshold (metallic bands)',filled_tresh,verb_level=V_more)
!
!RIM/CUTOFF
 call it(defs,'Em1Anys', '[RIM] X Y Z Static Inverse dielectric matrix',&
&                         RIM_epsm1,verb_level=V_more)
 call it(defs,'IDEm1Ref','[RIM] Dielectric matrix reference component 1(x)/2(y)/3(z)',&
&                         RIM_id_epsm1_reference,verb_level=V_more)
 call it(defs,'RandQpts','[RIM] Number of random q-points in the BZ',RIM_n_rand_pts)
 call it(defs,'RandGvec','[RIM] Coulomb interaction RS components',RIM_ng,G_unit)
 call it(defs,'CUTSphere','[CUT] Coulomb Cutoff Sphere radius',Sphere_radius,&
&                         verb_level=V_debug)
 call it(defs,'CUTBox','[CUT] Coulomb Cutoff Box sides',Box_length,&
&                         verb_level=V_debug)
 call it(defs,'BoseCut', '[BOSE] Finite Tel Bose function cutoff',bose_E_cut,verb_level=V_more)
!
!Sxc
 call it(defs,'EXXRLvcs','[XX] Exchange RL components',QP_ng_Sx,G_unit)
 call it(defs,'SxLowBnd','[XX] Exchange summation low band',QP_Sx_low_band,verb_level=V_more)
 call it(defs,'GbndRnge','[GW] G[W] bands range',QP_n_G_bands)
 call it(defs,'GDamping','[GW] G[W] damping',QP_G_damp,E_unit)
 call it(defs,'LifeTrCG','[GW] [o/o] Lifetime transitions reduction',QP_cg_percent)
 call it(defs,'DysSolver','[GW] Dyson Equation solver (`n`,`s`,`g`)',QP_solver)
 call it(defs,'GEnSteps','[GW] G_gw energy steps',QP_Sc_steps)
 call it(defs,'GEnRnge','[GW] G_gw energy range',QP_G_er,E_unit)
 call it(defs,'GDmRnge','[GW] G_gw damping range',QP_G_dr,E_unit)
!
! El-Ph
#if defined PJ_PH
 call it(defs,'GphBRnge', '[ELPH] G[W] bands range',QP_PH_n_G_bands)
#endif

#if defined PJ_RAS
 call init_bulk(defs)
 call init_surface(defs)
 call init_analyse_peaks(defs)
 call init_cutoff(defs)
 call init_kinematics(defs) 
 call init_loss_function(defs)
 call init_detector(defs)
 call init_convolute(defs)
#endif
#if defined PJ_REELS
 call init_bulk(defs)
 call init_surface(defs) 
 call init_kinematics(defs)
 call init_detector(defs)
 call init_reels(defs)
 call init_reels_integration(defs)
 call init_convolute(defs)
#endif

 call it('f',defs,'QpgFull', '[F RIM] Coulomb interaction: Full matrix',verb_level=V_more)
 call it('f',defs,'NewtDchk','[F GW] Test dSc/dw convergence',verb_level=V_more)
!
!QP parameters
 call it(defs,'XfnQPdb', '[EXTQP Xd] Database',QP_ctl_db(1),verb_level=V_qp)
 call it(defs,'XfnQP_N', '[EXTQP Xd] Interpolation neighbours',&
&                         QP_ctl_interp_neigh(1),verb_level=V_qp)
 call it(defs,'XfnQP_E', '[EXTQP Xd] E parameters (c/v)',QP_ctl_E(1,:),verb_level=V_more)
! call it(defs,'XfnQP_W','[EXTQP Xd] W parameters (c/v)',QP_ctl_W(1,:),verb_level=V_debug)
! call it(defs,'XfnQP_Z','[EXTQP Xd] Z parameters',QP_ctl_Z(1),verb_level=V_debug)
 call it(defs,'KfnQPdb', '[EXTQP BSK BSS] Database',QP_ctl_db(2),verb_level=V_qp)
 call it(defs,'KfnQP_N', '[EXTQP BSK BSS] Interpolation neighbours',&
&                         QP_ctl_interp_neigh(2),verb_level=V_qp)
 call it(defs,'KfnQP_E', '[EXTQP BSK BSS] E parameters (c/v)',QP_ctl_E(2,:),verb_level=V_more)
! call it(defs,'KfnQP_W','[EXTQP BSK BSS] W parameters (c/v)',QP_ctl_W(2,:),verb_level=V_debug)
! call it(defs,'KfnQP_Z','[EXTQP BSK BSS] Z parameters',QP_ctl_Z(2),verb_level=V_debug)
 call it(defs,'GfnQPdb', '[EXTQP G] Database',QP_ctl_db(3),verb_level=V_qp)
 call it(defs,'GfnQP_N', '[EXTQP G] Interpolation neighbours',&
&                         QP_ctl_interp_neigh(3),verb_level=V_qp)
 call it(defs,'GfnQP_E', '[EXTQP G] E parameters (c/v)',QP_ctl_E(3,:),verb_level=V_more)
! call it(defs,'GfnQP_W','[EXTQP G] W parameters (c/v)',QP_ctl_W(3,:),verb_level=V_debug)
! call it(defs,'GfnQP_Z','[EXTQP G] Z parameters',QP_ctl_Z(3),verb_level=V_debug)
!
!Xs Xp Xd Xx
 call Xload(X(1),Xw(1))
 call Xload(X(2),Xw(2))
 call Xload(X(3),Xw(3))
 call Xload(X(4),Xw(4))
!
!ACFDT
 call it(defs,'AC_n_LAM', '[ACFDT] Coupling Costant GL-grid points',ACFDT_n_lambda)
 call it(defs,'AC_n_FR',  '[ACFDT] Integration frequency points',ACFDT_n_freqs)
 call it(defs,'AC_E_Rng', '[ACFDT] Imaginary axis 2nd & 3rd energy points',ACFDT_E_range,E_unit)
!
!BSE
 call it(defs,'BSresKmod',  '[BSK] Resonant Kernel mode. (`x`;`c`;`d`)',BS_res_mode)
 call it(defs,'BScplKmod',  '[BSK] Coupling Kernel mode. (`x`;`c`;`d`)',BS_cpl_mode)
 call it(defs,'BSEEhEny','[BSK] Electron-hole energy range',&
&        BS_eh_en,E_unit,verb_level=V_more)
 call it(defs,'BSehWind','[BSK] [o/o] E/h coupling pairs energy window',&
&        BS_eh_win,verb_level=V_more)
 call it(defs,'BSEQptR', '[BSK] Transferred momenta range',BS_q,verb_level=V_more)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSEClmns','[BSK] Kernel Columns',BS_columns,verb_level=V_more)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSSmod',  '[BSS] Solvers `h/d/i/t`',BSS_mode)
 call it(defs,'BLongDir','[BSS] [cc] Electric Field',BSS_q0)
 call it(defs,'BEnRange','[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange','[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs','[BSS] [o/o] Haydock treshold. Strict(>0)/Average(<0)',&
&              Haydock_treshold,verb_level=V_more)
 call it(defs,'BEnSteps','[BSS] Energy steps',BSS_n_freqs)
!
!Fxc
 call it(defs,'FxcGRLc',  '[TDDFT] XC-kernel RL size',FXC_n_g_corr,G_unit)
 call it(defs,'FxcMEStps','[TDDFT] [o/o] Memory energy steps',&
&                         FXC_per_memstps,verb_level=V_more)
 call it(defs,'LRC_alpha','[TDDFT] LRC alpha factor',FXC_LRC_alpha)
 call it(defs,'FxcSVDcut','[TDDFT] Uses an SVD cutoff to invert Xo',&
&                         FXC_SVD_cutoff,verb_level=V_more)
 call it('f',defs,'FxcCausal', '[TDDFT] Causal TDDFT kernel',verb_level=V_more)
!
 contains 
  subroutine Xload(X,wv)
   type(X_t):: X
   type(w_samp):: wv
!  ws
   integer :: i1
   integer,parameter :: desln=31
   character(8)::nms(14),sfx
   character(40)::des(14)
   character(schlen)::lch1,lch2
   nms=(/'GrFnTp','EnRnge','DmRnge','CGrdSp','ETStps','EMStps',&
&        'DrudeW','LongDr','EhEngy','PPAPnt','PPTylT',&
&        'NGsBlk','QpntsR','BndsRn'/)
   des(1) ='Green`s function t/c/r/a'
   des(2) ='Energy range'
   des(3) ='Damping range'
   des(4) ='[o/o] Coarse grid controller'
   des(5) ='Total Energy steps'
   des(6) ='[o/o] Memory Energy steps'
   des(7) ='Drude plasmon'
   des(8) ='[cc] Electric Field'
   des(9) ='Electron-hole energy range'
   des(10) ='PPA imaginary energy'
   des(11)='Taylor terms in PP expansion'
   des(12)='Response block size'
   des(13)='Transferred momenta'
   des(14)='Polarization function bands'
   do i1=1,14
    lch1=des(i1);lch2=nms(i1)
!   Xs%whoami=2;Xx%whoami=1;Xp%whoami=4;Xd%whoami=3
    if (X%whoami==1) sfx='Xx'
    if (X%whoami==2) sfx='Xs'
    if (X%whoami==3) sfx='Xd'
    if (X%whoami==4) sfx='Xp'
    write (des(i1),'(4a)')  '[',trim(sfx),'] ',trim(lch1)
    write (nms(i1),'(2a)')  lch2(1:6),trim(sfx)
   enddo
   call it(defs,nms( 1),des( 1),X%ordering,verb_level=V_more)
   call it(defs,nms( 2),des( 2),wv%er,E_unit,verb_level=0) 
   call it(defs,nms( 3),des( 3),wv%dr,E_unit,verb_level=0)
   call it(defs,nms( 4),des( 4),X%cg_percentual,verb_level=V_more)
   call it(defs,nms( 5),des( 5),wv%n(1),verb_level=0)
   call it(defs,nms( 6),des( 6),wv%per_memstps,verb_level=1)
   call it(defs,nms( 7),des( 7),X%Wd,E_unit,verb_level=1) 
   call it(defs,nms( 8),des( 8),X%q0,verb_level=0)
   call it(defs,nms( 9),des( 9),X%ehe,E_unit,verb_level=V_more)
   call it(defs,nms(10),des(10),X%ppaE,E_unit,verb_level=0)
   call it(defs,nms(11),des(11),pp_taylor_terms,verb_level=V_more)
   call it(defs,nms(12),des(12),X%ng,G_unit,verb_level=0) 
   call it(defs,nms(13),des(13),X%iq,verb_level=0)
   call it(defs,nms(14),des(14),X%ib,verb_level=0)
  end subroutine
 end subroutine

