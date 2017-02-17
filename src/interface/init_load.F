!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine init_load(defs,en,q,k,X,Xw)
 !
 use pars,          ONLY:schlen
 use electrons,     ONLY:levels,nel,filled_tresh
 use frequency,     ONLY:w_samp
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,T_unit,Bfield_unit,&
&                        Time_unit,I_unit,Angle_unit,&
&                        V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,V_real_time
 use X_m,           ONLY:X_t,q_plus_G_direction,Q_Shift_Order
 use fields,        ONLY:global_gauge,grid_path
 use QP_m,          ONLY:QP_cg_percent,QP_G_damp,QP_solver,&
&                        QP_n_G_bands,QP_ng_Sx,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps,GWo_iterations,SC_band_mixing,&
&                        SC_E_threshold,QP_dSc_delta,QP_G_Zoom_treshold
 use timing,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel,non_periodic_directions,Bose_Temp
 use R_lattice,     ONLY:ng_closed,QP_states_k,nXkibz,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,cyl_ph_radius,box_length,cyl_length,cut_geometry
 use BS,            ONLY:BS_res_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,BS_columns,&
&                        BS_n_g_W,BS_n_g_exch,BSS_mode,Haydock_treshold,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_q0,BS_cpl_mode,BSS_damp_reference
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,FXC_LRC_beta, &
&                        FXC_SVD_digits
 use ACFDT,         ONLY:ACFDT_n_lambda,ACFDT_n_freqs,ACFDT_E_range
 use com,           ONLY:more_io_path,com_path
 use functions,     ONLY:bose_E_cut
 !
 implicit none
 type(initdefs)::defs
 type(levels)  ::en          
 type(bz_samp) ::q,k
 type(X_t)     ::X(4)
 type(w_samp)  ::Xw(4)
 !
 !===============
 ! GPL RUNLEVELS
 !===============
 call it('r',defs,'setup',   '[R INI] Initialization') 
 call it('r',defs,'optics',  '[R OPT] Optics')
 call it('r',defs,'chi',     '[R CHI] Dyson equation for Chi.')
 call it('r',defs,'bse',     '[R BSK] Bethe Salpeter Equation.')
 call it('r',defs,'HF_and_locXC',   '[R XX] Hartree-Fock Self-energy and Vxc')
 call it('r',defs,'em1s',    '[R Xs] Static Inverse Dielectric Matrix')
 call it('r',defs,'em1d',    '[R Xd] Dynamical Inverse Dielectric Matrix')
 call it('r',defs,'ppa',     '[R Xp] Plasmon Pole Approximation')
 call it('r',defs,'gw0',     '[R GW] GoWo Quasiparticle energy levels')
 call it('r',defs,'bss',     '[R BSS] Bethe Salpeter Equation solver')
 call it('r',defs,'alda_fxc','[R TDDFT] The ALDA TDDFT kernel')
 call it('r',defs,'lrc_fxc', '[R TDDFT] The LRC TDDFT kernel')
 call it('r',defs,'rim_cut', '[R RIM CUT] Coulomb interaction')
 call it('r',defs,'tdhf',    '[R TDHF] Unscreened e-h interaction: TD Hartree-Fock')
 call it('r',defs,'cohsex',  '[R Xp] COlumb Hole Screened EXchange')
 call it('r',defs,'life',    '[R GW] GoWo Quasiparticle lifetimes')
 !
 !
 !
 !================
 ! GPL VARIABLES
 !================
 !
 ! Basics
 !
 call it(defs,'StdoHash','[IO] Live-timing Hashes',nhash,verb_level=V_io)
 call it(defs,'MaxGvecs','[INI] Max number of G-vectors planned to use',&
&                        ng_closed,unit=G_unit,verb_level=V_RL)
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit,verb_level=V_RL)
 call it(defs,'NonPDirs','[X/BSS] Non periodic chartesian directions (X,Y,Z,XY...)',non_periodic_directions,verb_level=V_resp)
 call it(defs,'IkSigLim','[KPT] QP K-points indices range',QP_states_k,verb_level=V_kpt)
 call it(defs,'IkXLim',  '[KPT] X grid last k-point index',nXkibz,verb_level=V_kpt) 
 call it(defs,'Nelectro','Electrons number',nel,verb_level=V_general)
 !
 ! S_xc
 !
 call it(defs,'LifeTrCG', '[GW] [o/o] Lifetime transitions reduction',QP_cg_percent)
 call it(defs,'EXXRLvcs', '[XX] Exchange RL components',QP_ng_Sx,G_unit)
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands)
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit)
 call it(defs,'GDmRnge',  '[GW] G_gw damping range',QP_G_dr,E_unit)
 call it(defs,'dScStep',  '[GW] Energy step to evalute Z factors',QP_dSc_delta,E_unit)
 call it(defs,'DysSolver','[GW] Dyson Equation solver (`n`,`s`,`g`)',QP_solver,protect=.FALSE.)
 call it(defs,'GEnSteps',   '[GW] G_gw energy steps',QP_Sc_steps)
 call it(defs,'GEnRnge',    '[GW] G_gw energy range (centered in the bare energy)',QP_G_er,E_unit)
 call it('f',defs,'NewtDchk',   '[F GW] Test dSc/dw convergence',verb_level=V_qp)
 call it('f',defs,'ExtendOut',  '[F GW] Print all variables in the output file',verb_level=V_qp)
 !
 !
 ! BSE
 !
 call it(defs,'BSresKmod',  '[BSK] Resonant Kernel mode. (`x`;`c`;`d`)',BS_res_mode)
 call it(defs,'BScplKmod',  '[BSK] Coupling Kernel mode. (`x`;`c`;`d`;`u`)',BS_cpl_mode)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSSmod',  '[BSS] Solvers `h/d/i/t`',BSS_mode,protect=.FALSE.)
 call it(defs,'BLongDir','[BSS] [cc] Electric Field',BSS_q0)
 call it(defs,'BEnRange','[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange','[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs','[BSS] [o/o] Haydock treshold. Strict(>0)/Average(<0)',&
&              Haydock_treshold,verb_level=V_resp)
 call it(defs,'BEnSteps','[BSS] Energy steps',BSS_n_freqs)
 call it('f',defs,'WRbsWF',  '[BSS] Write to disk excitonic the FWs',verb_level=V_resp)
 call it('f',defs,'ALLGexx', '[BSS] Force the use use all RL vectors for the exchange part',verb_level=V_resp)
 call it('f',defs,'BSHayTer','[BSS] Terminate Haydock continuos fraction',verb_level=V_resp)
 !
 ! F_xc
 !
 call it(defs,'FxcGRLc',  '[TDDFT] XC-kernel RL size',FXC_n_g_corr,G_unit)
 call it(defs,'LRC_alpha','[TDDFT] LRC alpha factor',FXC_LRC_alpha)
 call it(defs,'LRC_beta', '[TDDFT] LRC beta factor',FXC_LRC_beta,verb_level=V_resp)
 !
 ! Xs Xd Xp
 !
 call Xload(X(2),Xw(2))
 call Xload(X(3),Xw(3))
 call Xload(X(4),Xw(4))
 !
 ! Optics: large Q momenta
 !
 call it(defs,'Qdirection', '[Xd] Transferred momentum direction (iku)',q_plus_G_direction,verb_level=V_resp)
 call it(defs,'QShiftOrder', '[Xd] Pick-up the (QShiftOrder)th q+G vector',Q_Shift_Order,verb_level=V_resp)
 !
 ! RIM
 !
 call it(defs,'Em1Anys', '[RIM] X Y Z Static Inverse dielectric matrix',&
&                         RIM_epsm1,verb_level=V_RL)
 call it(defs,'IDEm1Ref','[RIM] Dielectric matrix reference component 1(x)/2(y)/3(z)',&
&                         RIM_id_epsm1_reference,verb_level=V_RL)
 call it(defs,'RandQpts','[RIM] Number of random q-points in the BZ',RIM_n_rand_pts)
 call it(defs,'RandGvec','[RIM] Coulomb interaction RS components',RIM_ng,G_unit)
 call it('f',defs,'QpgFull', '[F RIM] Coulomb interaction: Full matrix',verb_level=V_RL)
 !
 ! CUTOFF
 !
 call it(defs,'CUTGeo',   '[CUT] Coulomb Cutoff geometry: box/cylinder/sphere',cut_geometry)
 call it(defs,'CUTBox',   '[CUT] [au] Box sides',box_length)
 call it(defs,'CUTRadius','[CUT] [au] Sphere/Cylinder radius',cyl_ph_radius)
 call it(defs,'CUTCylLen','[CUT] [au] Cylinder length',cyl_length)
 call it('f',defs,'CUTCol_test','[CUT] Perform a cutoff test in R-space',verb_level=V_RL)
 !
 !
 !
 ! QP ctl 
 !
 call QP_ctl_load(defs,1)
 call QP_ctl_load(defs,2)
 call QP_ctl_load(defs,3)
 !
 !
 ! El-Ph
 !
 ! 
 ! SC
 !
 ! 
 ! RAS
 !
 ! 
 ! REELS
 !
 !
 contains 
   !
   !
   subroutine Xload(X,wv)
     !
     type(X_t):: X
     type(w_samp):: wv
     ! 
     ! Work Space
     ! 
     integer           ::i1
     integer,parameter ::n_des=14
     character(8)      ::nms(n_des),sfx
     character(40)     ::des(n_des)
     character(schlen) ::lch1,lch2
     !
     nms=(/'GrFnTp','EnRnge','DmRnge','CGrdSp','ETStps','EMStps',&
&          'DrudeW','LongDr','EhEngy','PPAPnt',&
&          'NGsBlk','QpntsR','BndsRn','DmERef'/)
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
     des(11)='Response block size'
     des(12)='Transferred momenta'
     des(13)='Polarization function bands'
     des(14)='Damping reference energy'
     !
     do i1=1,n_des
       lch1=des(i1);lch2=nms(i1)
       if (X%whoami==1) sfx='Xx'
       if (X%whoami==2) sfx='Xs'
       if (X%whoami==3) sfx='Xd'
       if (X%whoami==4) sfx='Xp'
       write (des(i1),'(4a)')  '[',trim(sfx),'] ',trim(lch1)
       write (nms(i1),'(2a)')  lch2(1:6),trim(sfx)
     enddo
     !
     call it(defs,nms( 1),des( 1),X%ordering,verb_level=V_resp)
     call it(defs,nms( 2),des( 2),wv%er,E_unit,verb_level=0) 
     call it(defs,nms( 3),des( 3),wv%dr,E_unit,verb_level=0)
     call it(defs,nms( 4),des( 4),X%cg_percentual,verb_level=V_resp)
     call it(defs,nms( 5),des( 5),wv%n(1),verb_level=0)
     call it(defs,nms( 6),des( 6),wv%per_memstps,verb_level=1)
     call it(defs,nms( 7),des( 7),X%Wd,E_unit,verb_level=1) 
     call it(defs,nms( 8),des( 8),X%q0,verb_level=0)
     call it(defs,nms( 9),des( 9),X%ehe,E_unit,verb_level=V_resp)
     call it(defs,nms(10),des(10),X%ppaE,E_unit,verb_level=0)
     call it(defs,nms(11),des(11),X%ng,G_unit,verb_level=0) 
     call it(defs,nms(12),des(12),X%iq,verb_level=0)
     call it(defs,nms(13),des(13),X%ib,verb_level=0)
     call it(defs,nms(14),des(14),wv%damp_reference,E_unit,verb_level=V_resp)
     !
   end subroutine
   !
end subroutine
