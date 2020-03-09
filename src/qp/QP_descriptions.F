!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use stderr,        ONLY:STRING_pack
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
 if (.not.Update) n_descs_save=qp%desc%n
 if (     Update) qp%desc%n  =n_descs_save
 !
 if (allocated(RIM_qpg)) then
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' RIM G`s                '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1,qp%desc%n)=RIM_ng
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' RIM random pts         '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1,qp%desc%n)=RIM_n_rand_pts
   !
 endif
 !
 if (.not.l_life) then
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' GW SC iterations       '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1,qp%desc%n)=GWo_iterations
   !
   if (.not.l_cohsex.and.trim(QP_solver)/='g'.and..not.use_GreenF_to_eval_QP) then
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' dS/dw steps            '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(1,qp%desc%n)=QP_dSc_steps
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' dS/dw step         [ev]'
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='r'
     qp%desc%rval(1,qp%desc%n)=QP_dSc_delta*HA2EV
     !
   endif
   !
 endif 
 !
 if (l_elel_corr) then
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' X G`s            [used]'
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1,qp%desc%n)=X%ng
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' X G`s            [disk]'
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1,qp%desc%n)=X%ng_db
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' X bands                '
   qp%desc%size(qp%desc%n)=2
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1:2,qp%desc%n)=X%ib
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' X poles           [o/o]'
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='r'
   qp%desc%rval(1,qp%desc%n)=X%cg_percentual
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' X e/h E range      [ev]'
   qp%desc%size(qp%desc%n)=2
   qp%desc%kind(qp%desc%n)='r'
   qp%desc%rval(1:2,qp%desc%n)=X%ehe*HA2EV
   !
   if (ver_is_gt_or_eq(-1,(/3,0,5/))) then
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' X xc-Kernel             '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='s'
     qp%desc%sval(qp%desc%n)=trim(X%f_xc)
   endif
   !
   if (.not.l_ppa.and..not.l_cohsex) then
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' X damping range    [ev]'
     qp%desc%size(qp%desc%n)=2
     qp%desc%kind(qp%desc%n)='r'
     qp%desc%rval(1:2,qp%desc%n)=Xw%dr*HA2EV
     !  
     if (.not.l_life) then
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' X Steps                '
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='i'
       qp%desc%ival(1,qp%desc%n)=Xw%n_freqs
     endif
     !
   endif
   !
   if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' X BZ energy Double Grid'
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='l'
     qp%desc%lval(qp%desc%n)=use_X_DbGd
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' X BZ Double Grid points'
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(1,qp%desc%n)=BZ_DbGd_tot_nk_levels
     !
   endif
   !
 endif
 !
 if (l_life.and.l_elel_corr) then
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' Transitions       [o/o]'
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='r'
   qp%desc%rval(1,qp%desc%n)=QP_cg_percent
   !
 endif
 !
 if (.not.l_life) then 
   if ( (.not.l_cohsex) .or. (l_cohsex.and.COHSEX_use_empties) ) then
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' Sc/G bands             '
     qp%desc%size(qp%desc%n)=2
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(1:2,qp%desc%n)=QP_n_G_bands
   endif
   if (l_cohsex) then
     if (ver_is_gt_or_eq(-1,(/4,3,1/))) then
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' Sc Empty Bands         '
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='l'
       qp%desc%lval(qp%desc%n)=COHSEX_use_empties
     endif
   else
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' Sc/G damping       [ev]'
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='r'
     qp%desc%rval(1,qp%desc%n)=QP_G_damp*HA2EV
   endif
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' Sc bands terminator    '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='l'
   qp%desc%lval(qp%desc%n)=l_GW_terminator
   !
   if (l_GW_terminator) then
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' Sc terminator kind      '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='s'
     qp%desc%sval(qp%desc%n)=trim(GW_terminator_Kind)
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' Sc Terminator pole     '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='r'
     qp%desc%rval(1,qp%desc%n)=GW_terminator_E*HA2EV
     !
   endif
   !
   if (l_HF_and_locXC) then
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' Sx RL components       '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(1,qp%desc%n)=QP_ng_Sx
   endif
   !
 endif
 !
#if defined _QED
 !
 if (l_elphoton_corr) then
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' El-Photon correlation  '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='l'
   qp%desc%lval(qp%desc%n)=l_elphoton_corr
   !
   if (.not.l_life) then
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' QED RL components      '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(1,qp%desc%n)=QP_QED_ng
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
     qp%desc%n=qp%desc%n+1
     if (.not.l_elel_corr) qp%desc%n=n_descs_save+1
     !
     if (trim(QP_solver)/='g') then
       qp%desc%str(qp%desc%n)=' [ph] dS/dw steps        '
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='r'
       qp%desc%rval(1,qp%desc%n)=QP_dSc_steps
       !
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' [ph] dS/dw step     [ev]'
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='r'
       qp%desc%rval(1,qp%desc%n)=QP_dSc_delta*HA2EV
       !
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' [ph] Sc/G damping   [ev]'
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='r'
       qp%desc%rval(1,qp%desc%n)=QP_G_damp*HA2EV
       !
     endif
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' El-Ph correlation      '
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='l'
     qp%desc%lval(qp%desc%n)=l_elph_corr
     !
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' El-Ph Sc. G bands      '
     qp%desc%size(qp%desc%n)=2
     qp%desc%kind(qp%desc%n)='i'
     qp%desc%ival(1:2,qp%desc%n)=(/1,QP_PH_n_G_bands/)
     !
     if (ver_is_gt_or_eq(-1,(/3,2,2/))) then
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' El-Ph phonon branches  '
       qp%desc%size(qp%desc%n)=2
       qp%desc%kind(qp%desc%n)='i'
       qp%desc%ival(1:2,qp%desc%n)=elph_branches
     endif
     !
     if (ver_is_gt_or_eq(-1,(/4,0,0/))) then
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' El-Ph momenta used     '
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='i'
       qp%desc%ival(1,qp%desc%n)=elph_nDBs_used
     endif
     !
     if (ver_is_gt_or_eq(-1,revision=4400)) then
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' Energy treshold   [FAN]'
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='r'
       qp%desc%rval(1,qp%desc%n)=FAN_deltaE_treshold*HA2EV
       !
       qp%desc%n=qp%desc%n+1
       qp%desc%str(qp%desc%n)=' Energy treshold    [DW]'
       qp%desc%size(qp%desc%n)=1
       qp%desc%kind(qp%desc%n)='r'
       qp%desc%rval(1,qp%desc%n)=DW_deltaE_treshold*HA2EV
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
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' [GF] GF is causal       '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='l'
   qp%desc%lval(qp%desc%n)=GF_is_causal
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' [GF] Real-axis steps    '
   qp%desc%size(qp%desc%n)=1
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%ival(1,qp%desc%n)=QP_Sc_steps
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' [GF] Initial E range  [eV]'
   qp%desc%size(qp%desc%n)=2
   qp%desc%kind(qp%desc%n)='i'
   qp%desc%rval(1:2,qp%desc%n)=QP_G_er*HA2EV
   !
   if (ver_is_gt_or_eq(-1,revision=530)) then
     qp%desc%n=qp%desc%n+1
     qp%desc%str(qp%desc%n)=' [GF] Zoom treshold   [o/o]'
     qp%desc%size(qp%desc%n)=1
     qp%desc%kind(qp%desc%n)='r'
     qp%desc%rval(1,qp%desc%n)=QP_G_Zoom_treshold
   endif
   !
   qp%desc%n=qp%desc%n+1
   qp%desc%str(qp%desc%n)=' [GF] Damping          [eV]'
   qp%desc%size(qp%desc%n)=2
   qp%desc%kind(qp%desc%n)='r'
   qp%desc%rval(1:2,qp%desc%n)=QP_G_dr*HA2EV
   !
 endif
 !
 ! QP states
 !
 call QP_state_print( qp )
 !
end subroutine
