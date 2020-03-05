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
subroutine QP_descriptions(qp,X,Xw,Update)
 ! 
 ! WARNING ! Changes here must be updated in io_QP_and_GF as well.
 !
 use units,         ONLY:HA2EV
 use X_m,           ONLY:X_t,use_X_DbGd
 use QP_m,          ONLY:QP_t,QP_ng_Sx,QP_n_G_bands,QP_cg_percent,&
&                        QP_dSc_delta,QP_G_damp,QP_dSc_steps,GWo_iterations,&
&                        COHSEX_use_empties,QP_Sc_steps,QP_G_er,QP_G_dr,QP_solver,&
&                        use_GreenF_to_eval_QP,GF_is_causal,QP_G_Zoom_treshold,&
&                        GW_terminator_E,l_GW_terminator,GW_terminator_Kind
 use stderr,        ONLY:STRING_pack,gen_fmt
 use drivers,       ONLY:l_life,l_ppa,l_elel_corr,l_cohsex,&
&                        l_HF_and_locXC
 use R_lattice,     ONLY:RIM_qpg,RIM_n_rand_pts,RIM_ng
 use interfaces,    ONLY:QP_state_print
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:BZ_DbGd_tot_nk_levels
 use IO_m,          ONLY:ver_is_gt_or_eq
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr 
 use ELPH,          ONLY:QP_PH_n_G_bands,elph_branches,elph_nDBs_used,&
&                        FAN_deltaE_treshold,DW_deltaE_treshold
#endif
#if defined _QED
 use QP_m,          ONLY:QP_QED_ng
 use drivers,       ONLY:l_elphoton_corr
#endif
 implicit none
 !
 type(QP_t)  ::qp
 type(X_t)   ::X
 type(w_samp)::Xw
 logical     ::Update
 ! 
 ! Work Space
 !
 integer, save     :: n_descs_save
 !
 if (.not.Update) n_descs_save=qp%n_descs
 if (     Update) qp%n_descs  =n_descs_save
 !
 if (allocated(RIM_qpg)) then
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' RIM G`s                '
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=RIM_ng
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' RIM random pts         '
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=RIM_n_rand_pts
   !
 endif
 !
 if (.not.l_life) then
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' GW SC iterations       '
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=GWo_iterations
   !
   if (.not.l_cohsex.and.trim(QP_solver)/='g'.and..not.use_GreenF_to_eval_QP) then
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' dS/dw steps            '
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=QP_dSc_steps
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' dS/dw step         [ev]'
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=QP_dSc_delta*HA2EV
     !
   endif
   !
 endif 
 !
 if (l_elel_corr) then
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' X G`s            [used]'
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=X%ng
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' X G`s            [disk]'
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=X%ng_db
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' X bands                '
   qp%desc_n_values(qp%n_descs)=2
   qp%desc_values(:,qp%n_descs)=X%ib
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' X poles           [o/o]'
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=X%cg_percentual
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' X e/h E range      [ev]'
   qp%desc_n_values(qp%n_descs)=2
   qp%desc_values(:,qp%n_descs)=X%ehe*HA2EV
   !
   if (ver_is_gt_or_eq(-1,(/3,0,5/))) then
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' X xc-Kernel             '//trim(X%f_xc)
     qp%desc_n_values(qp%n_descs)=0
   endif
   !
   if (.not.l_ppa.and..not.l_cohsex) then
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' X damping range    [ev]'
     qp%desc_n_values(qp%n_descs)=2
     qp%desc_values(:,qp%n_descs)=Xw%dr*HA2EV
     !  
     if (.not.l_life) then
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' X Steps                '
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=Xw%n_freqs
     endif
     !
   endif
   !
   if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
     !
     qp%n_descs=qp%n_descs+1
     if (.not.use_X_DbGd) qp%desc_string(qp%n_descs)=' X BZ energy Double Grid: no '
     if (     use_X_DbGd) qp%desc_string(qp%n_descs)=' X BZ energy Double Grid: yes'
     qp%desc_n_values(qp%n_descs)=0
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' X BZ Double Grid points'
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=BZ_DbGd_tot_nk_levels
     !
   endif
   !
 endif
 !
 if (l_life.and.l_elel_corr) then
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' Transitions       [o/o]'
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=QP_cg_percent
   !
 endif
 !
 if (.not.l_life) then 
   if ( (.not.l_cohsex) .or. (l_cohsex.and.COHSEX_use_empties) ) then
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sc/G bands             '
     qp%desc_n_values(qp%n_descs)=2
     qp%desc_values(:,qp%n_descs)=QP_n_G_bands
   endif
   if (l_cohsex) then
     if (ver_is_gt_or_eq(-1,(/4,3,1/))) then
       qp%n_descs=qp%n_descs+1
       if (     COHSEX_use_empties) qp%desc_string(qp%n_descs)=' Sc Empty Bands         : yes'
       if (.not.COHSEX_use_empties) qp%desc_string(qp%n_descs)=' Sc Empty Bands         : no'
       qp%desc_n_values(qp%n_descs)=0
     endif
   else
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sc/G damping       [ev]'
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=QP_G_damp*HA2EV
   endif
   !
   if (l_GW_terminator) then
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sc bands terminator    : yes'
     qp%desc_n_values(qp%n_descs)=0
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sc terminator kind      '//trim(GW_terminator_Kind)
     qp%desc_n_values(qp%n_descs)=0
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sc Terminator pole     '
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=GW_terminator_E*HA2EV
     !
   else
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sc bands terminator    : no'
     qp%desc_n_values(qp%n_descs)=0
     !
   endif
   !
   if (l_HF_and_locXC) then
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' Sx RL components       '
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=QP_ng_Sx
   endif
   !
 endif
 !
#if defined _QED
 !
 if (l_elphoton_corr) then
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' El-Photon correlation  : yes'
   qp%desc_n_values(qp%n_descs)=0
   !
   if (.not.l_life) then
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' QED RL components      '
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=QP_QED_ng
   endif
   !
 endif
 !
#endif
 !
#if defined _ELPH 
 !
 ! El-Ph
 !
 if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
   !
   if (l_elph_corr) then
     !
     qp%n_descs=qp%n_descs+1
     if (.not.l_elel_corr) qp%n_descs=n_descs_save+1
     !
     if (trim(QP_solver)/='g') then
       qp%desc_string(qp%n_descs)=' [ph] dS/dw steps        '
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=QP_dSc_steps
       !
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' [ph] dS/dw step     [ev]'
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=QP_dSc_delta*HA2EV
       !
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' [ph] Sc/G damping   [ev]'
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=QP_G_damp*HA2EV
       !
     endif
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' El-Ph correlation      : yes'
     qp%desc_n_values(qp%n_descs)=0
     !
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' El-Ph Sc. G bands      '
     qp%desc_n_values(qp%n_descs)=2
     qp%desc_values(:,qp%n_descs)=(/1,QP_PH_n_G_bands/)
     !
     if (ver_is_gt_or_eq(-1,(/3,2,2/))) then
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' El-Ph phonon branches  '
       qp%desc_n_values(qp%n_descs)=2
       qp%desc_values(:,qp%n_descs)=elph_branches
     endif
     !
     if (ver_is_gt_or_eq(-1,(/4,0,0/))) then
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' El-Ph momenta used     '
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=elph_nDBs_used
     endif
     !
     if (ver_is_gt_or_eq(-1,revision=4400)) then
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' Energy treshold   [FAN]'
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=FAN_deltaE_treshold*HA2EV
       !
       qp%n_descs=qp%n_descs+1
       qp%desc_string(qp%n_descs)=' Energy treshold    [DW]'
       qp%desc_n_values(qp%n_descs)=1
       qp%desc_values(1,qp%n_descs)=DW_deltaE_treshold*HA2EV
     endif
     !
   endif
   !
 endif
 !
#endif
 !
 ! Green's Functions
 !
 if (ver_is_gt_or_eq(-1,revision=452).and.trim(QP_solver)=='g') then
   !
   qp%n_descs=qp%n_descs+1
   if (.not.GF_is_causal) qp%desc_string(qp%n_descs)=' [GF] GF is causal       : no'
   if (     GF_is_causal) qp%desc_string(qp%n_descs)=' [GF] GF is causal       : yes'
   qp%desc_n_values(qp%n_descs)=0
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' [GF] Real-axis steps    '
   qp%desc_n_values(qp%n_descs)=1
   qp%desc_values(1,qp%n_descs)=QP_Sc_steps
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' [GF] Initial E range  [eV]'
   qp%desc_n_values(qp%n_descs)=2
   qp%desc_values(:,qp%n_descs)=QP_G_er*HA2EV
   !
   if (ver_is_gt_or_eq(-1,revision=530)) then
     qp%n_descs=qp%n_descs+1
     qp%desc_string(qp%n_descs)=' [GF] Zoom treshold   [o/o]'
     qp%desc_n_values(qp%n_descs)=1
     qp%desc_values(1,qp%n_descs)=QP_G_Zoom_treshold
   endif
   !
   qp%n_descs=qp%n_descs+1
   qp%desc_string(qp%n_descs)=' [GF] Damping          [eV]'
   qp%desc_n_values(qp%n_descs)=2
   qp%desc_values(:,qp%n_descs)=QP_G_dr*HA2EV
   !
 endif
 !
 ! QP states
 !
 call QP_state_print( qp )
 !
end subroutine
