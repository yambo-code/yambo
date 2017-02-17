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
subroutine init_load(defs,en,q,k,X,Xw)
 !
 ! VERBOSITY
 ! 
 ! 0 (default)
 ! 1 (More) V_more
 ! 2 (QP)   V_qp
 ! 3 (I/O)  V_io
 ! 4 (testing)  V_testing
 !
 use pars,          ONLY:schlen
 use electrons,     ONLY:levels,nel,filled_tresh
 use frequency,     ONLY:w_samp
 use it_m,          ONLY:it,initdefs,E_unit,G_unit,T_unit,V_more,B_unit,&
&                        V_qp,V_io,V_testing,Time_unit
 use X_m,           ONLY:X_t,long_path
 use QP_m,          ONLY:QP_rep_string,QP_cg_percent,QP_G_damp,QP_solver,&
&                        QP_n_G_bands,QP_ng_Sx,QP_Sx_low_band,&
&                        QP_G_er,QP_G_dr,QP_Sc_steps,GWo_iterations,SC_band_mixing,&
&                        SC_E_threshold
 use timing,        ONLY:nhash
 use wave_func,     ONLY:wf_ng
 use D_lattice,     ONLY:Tel,non_periodic_directions
 use R_lattice,     ONLY:ng_closed,QP_states_k,nXkibz,&
&                        bz_samp,RIM_ng,RIM_epsm1,RIM_id_epsm1_reference,&
&                        RIM_n_rand_pts,cyl_ph_radius,box_length,cyl_length,cut_geometry
 use BS,            ONLY:BS_res_mode,BS_eh_en,BS_eh_win,BS_q,BS_bands,BS_columns,&
&                        BS_n_g_W,BS_n_g_exch,BSS_mode,Haydock_treshold,BSS_n_freqs,&
&                        BSS_dr,BSS_er,BSS_q0,BS_cpl_mode,BSS_damp_reference
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_per_memstps,FXC_LRC_alpha,&
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
 call it('r',defs,'xxvxc',   '[R XX] Hartree-Fock Self-energy and Vxc')
 call it('r',defs,'em1s',    '[R Xs] Static Inverse Dielectric Matrix')
 call it('r',defs,'em1d',    '[R Xd] Dynamical Inverse Dielectric Matrix')
 call it('r',defs,'ppa',     '[R Xp] Plasmon Pole Approximation')
 call it('r',defs,'gw0',     '[R GW] GoWo Quasiparticle energy levels')
 call it('r',defs,'bss',     '[R BSS] Bethe Salpeter Equation solver')
 call it('r',defs,'alda_fxc','[R TDDFT] The ALDA TDDFT kernel')
 call it('r',defs,'lrc_fxc', '[R TDDFT] The LRC TDDFT kernel')
 call it('r',defs,'rim_cut', '[R RIM CUT] Coulomb interaction')
 !
 !
 !
 !
 !================
 ! GPL VARIABLES
 !================
 !
 ! Basics
 !
 call it(defs,'QPreport','[GW] QP info. Keys: kp/bn/xx/xc/s0/sq/e0/eq/ee/zf/ds/lm/lf',&
&                        QP_rep_string)
 call it(defs,'StdoHash','[IO] Live-timing Hashes',nhash,verb_level=V_io)
 call it(defs,'MaxGvecs','[INI] Max number of G-vectors planned to use',&
&                        ng_closed,unit=G_unit,verb_level=V_more)
 call it(defs,'FFTGvecs','[FFT] Plane-waves',wf_ng,G_unit,verb_level=V_more)
 call it(defs,'NonPDirs','[X/BSS] Non periodic chartesian directions (X,Y,Z,XY...)',non_periodic_directions,Verb_level=V_more)
 call it(defs,'IkSigLim','[KPT] QP K-points indices range',QP_states_k,verb_level=V_more)
 call it(defs,'IkXLim',  '[KPT] X grid last k-point index',nXkibz,verb_level=V_more) 
 !
 ! S_xc
 !
 call it(defs,'EXXRLvcs', '[XX] Exchange RL components',QP_ng_Sx,G_unit)
 call it(defs,'GbndRnge', '[GW] G[W] bands range',QP_n_G_bands)
 call it(defs,'GDamping', '[GW] G[W] damping',QP_G_damp,E_unit)
 call it(defs,'GDmRnge',  '[GW] G_gw damping range',QP_G_dr,E_unit)
 !
 !
 !
 ! BSE
 !
 call it(defs,'BSresKmod',  '[BSK] Resonant Kernel mode. (`x`;`c`;`d`)',BS_res_mode)
 call it(defs,'BSEBands','[BSK] Bands range',BS_bands)
 call it(defs,'BSENGBlk','[BSK] Screened interaction block size',BS_n_g_W,G_unit)
 call it(defs,'BSENGexx','[BSK] Exchange components',BS_n_g_exch,G_unit)
 call it(defs,'BSSmod',  '[BSS] Solvers `h/d/i/t`',BSS_mode,protect=.FALSE.)
 call it(defs,'BLongDir','[BSS] [cc] Electric Field',BSS_q0)
 call it(defs,'BEnRange','[BSS] Energy range',BSS_er,E_unit)
 call it(defs,'BDmRange','[BSS] Damping range',BSS_dr,E_unit)
 call it(defs,'BSHayTrs','[BSS] [o/o] Haydock treshold. Strict(>0)/Average(<0)',&
&              Haydock_treshold,verb_level=V_more)
 call it(defs,'BEnSteps','[BSS] Energy steps',BSS_n_freqs)
 call it('f',defs,'WRbsWF', '[BSS] Write to disk excitonic the FWs',verb_level=V_more)
 !
 ! F_xc
 !
 call it(defs,'FxcGRLc',  '[TDDFT] XC-kernel RL size',FXC_n_g_corr,G_unit)
 call it(defs,'LRC_alpha','[TDDFT] LRC alpha factor',FXC_LRC_alpha)
 !
 ! Xs Xd Xp
 !
 call Xload(X(2),Xw(2))
 call Xload(X(3),Xw(3))
 call Xload(X(4),Xw(4))
 !
 ! RIM
 !
 call it(defs,'Em1Anys', '[RIM] X Y Z Static Inverse dielectric matrix',&
&                         RIM_epsm1,verb_level=V_more)
 call it(defs,'IDEm1Ref','[RIM] Dielectric matrix reference component 1(x)/2(y)/3(z)',&
&                         RIM_id_epsm1_reference,verb_level=V_more)
 call it(defs,'RandQpts','[RIM] Number of random q-points in the BZ',RIM_n_rand_pts)
 call it(defs,'RandGvec','[RIM] Coulomb interaction RS components',RIM_ng,G_unit)
 call it('f',defs,'QpgFull', '[F RIM] Coulomb interaction: Full matrix',verb_level=V_more)
 !
 ! CUTOFF
 !
 call it(defs,'CUTGeo',   '[CUT] Coulomb Cutoff geometry: box/cylinder/sphere',cut_geometry)
 call it(defs,'CUTBox',   '[CUT] [au] Box sides',box_length)
 call it(defs,'CUTRadius','[CUT] [au] Sphere/Cylinder radius',cyl_ph_radius)
 call it(defs,'CUTCylLen','[CUT] [au] Cylinder length',cyl_length)
 call it('f',defs,'CUTCol_test','[CUT] Perform a cutoff test in R-space',verb_level=V_more)
 !
 !
 !
 ! QP ctl 
 !
 call QP_ctl_load(1)
 call QP_ctl_load(2)
 call QP_ctl_load(3)
 !
 !
 ! El-Ph
 !
 ! 
 ! SC
 !
 ! 
 ! SC [common with RT]
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
     call it(defs,nms(11),des(11),X%ng,G_unit,verb_level=0) 
     call it(defs,nms(12),des(12),X%iq,verb_level=0)
     call it(defs,nms(13),des(13),X%ib,verb_level=0)
     call it(defs,nms(14),des(14),wv%damp_reference,E_unit,verb_level=V_more)
     !
   end subroutine
   !
   subroutine QP_ctl_load(i_type)
     !
     ! Note that this routine is called in 'load' mode BEFORE
     ! the DB1 is read, so before n_sp_pol is set to the correct value.
     ! 
     ! Thus we must call it on the the no spin, spin + and spin - cases 
     ! because the code does not know if there is
     ! spin unless the DB1 is read. 
     !
     use electrons,     ONLY:n_sp_pol
     use it_m,          ONLY:initmode
     use QP_m,          ONLY:QP_ctl_E,QP_ctl_db,QP_ctl_interp_neigh,QP_ctl_W,QP_ctl_Z
     integer           ::i_type
     ! 
     ! Work Space
     ! 
     integer           ::i_spin,i1,i1_range(2)
     character(1)      ::prefix(3)
     character(3)      ::spin_ch(3)
     character(7)      ::description_ch(3)
     character(9)      ::spin_description_ch(3)
     !
     spin_ch=(/'   ','_up','_dn'/)
     spin_description_ch=(/'       ','spin up','spin dn'/)
     prefix=(/'X','K','G'/)
     description_ch(1)='Xd'
     description_ch(2)='BSK BSS'
     description_ch(3)='G'
     !
     if (initmode==0) i1_range=(/1,3/)
     if (initmode/=0.and.n_sp_pol==1) i1_range=1
     if (initmode/=0.and.n_sp_pol==2) i1_range=(/2,3/)
     !
     do i1=i1_range(1),i1_range(2)
       ! 
       if (i1==1) i_spin=1
       if (i1> 1) i_spin=i1-1
       !
       call it(defs,&
&              prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_E',&
&              '[EXTQP '//trim(description_ch(i_type))//'] E parameters '//&
&              trim(spin_description_ch(i1))//' (c/v)',&
&              QP_ctl_E(i_type,:,i_spin),verb_level=V_qp)
       !
       call it(defs,&
&              prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_W',&
&              '[EXTQP '//trim(description_ch(i_type))//'] W parameters '//&
&              trim(spin_description_ch(i1))//' (c/v)',&
&              QP_ctl_W(i_type,:,i_spin),verb_level=V_testing)
       !
       call it(defs,&
&              prefix(i_type)//'fnQP'//trim(spin_ch(i1))//'_Z',&
&              '[EXTQP '//trim(description_ch(i_type))//'] Z factor '//&
&              trim(spin_description_ch(i1))//' (c/v)',&
&              QP_ctl_Z(i_type,i_spin),verb_level=V_testing)
       !
     enddo
     !
     call it(defs,prefix(i_type)//'fnQPdb',&
&            '[EXTQP '//trim(description_ch(i_type))//'] Database',&
&            QP_ctl_db(i_type),verb_level=V_qp)
     !
     call it(defs,prefix(i_type)//'fnQP_N',&
&            '[EXTQP '//trim(description_ch(i_type))//'] Interpolation neighbours',&
&            QP_ctl_interp_neigh(i_type),verb_level=V_qp)
     !
   end subroutine
   !
end subroutine
