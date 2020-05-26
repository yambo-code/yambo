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
 use descriptors,   ONLY:IO_desc_add
 !
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
 if (     Update) qp%desc%n   =n_descs_save
 !
 if (allocated(RIM_qpg)) then
   !
   call IO_desc_add(qp%desc,'i','RIM G`s',I=(/RIM_ng/))
   call IO_desc_add(qp%desc,'i','RIM random pts',I=(/RIM_n_rand_pts/))
   !
 endif
 !
 if (.not.l_life) then
   !
   call IO_desc_add(qp%desc,'i','GW SC iterations',I=(/GWo_iterations/))
   !
   if (.not.l_cohsex.and.trim(QP_solver)/='g'.and..not.use_GreenF_to_eval_QP) then
     !
     call IO_desc_add(qp%desc,'i','dS/dw steps',I=(/QP_dSc_steps/))
     !
     call IO_desc_add(qp%desc,'r','dS/dw step',R=(/QP_dSc_delta*HA2EV/),term="ev")
     !
   endif
   !
 endif 
 !
 if (l_elel_corr) then
   !
   call IO_desc_add(qp%desc,str='X G`s',term='used',kind='i',I=(/X%ng/))
   !
   call IO_desc_add(qp%desc,str='X G`s',term='disk',kind='i',I=(/X%ng_db/))
   !
   call IO_desc_add(qp%desc,str='X bands',SZ=2,kind='i',I=X%ib)
   !
   call IO_desc_add(qp%desc,str='X poles',term='o/o',kind='r',R=(/X%cg_percentual/))
   !
   call IO_desc_add(qp%desc,str='X e/h E range',term='ev',SZ=2,kind='r',R=X%ehe*HA2EV)
   !
   if (ver_is_gt_or_eq(-1,(/3,0,5/))) then
     call IO_desc_add(qp%desc,str='X xc-Kernel',kind='s',S=trim(X%f_xc))
   endif
   !
   if (.not.l_ppa.and..not.l_cohsex) then
     !
     call IO_desc_add(qp%desc,str='X damping range',term='ev',SZ=2,kind='r',R=Xw%dr*HA2EV)
     !  
     if (.not.l_life) call IO_desc_add(qp%desc,str='X Steps',kind='i',I=(/Xw%n_freqs/))
     !
   endif
   !
   if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
     !
     call IO_desc_add(qp%desc,str='X BZ energy Double Grid',kind='l',L=use_X_DbGd)
     !
     call IO_desc_add(qp%desc,str='X BZ Double Grid points',kind='i',I=(/BZ_DbGd_tot_nk_levels/))
     !
   endif
   !
 endif
 !
 if (l_life.and.l_elel_corr) then
   !
   call IO_desc_add(qp%desc,str='Transitions',term='o/o',kind='r',R=(/QP_cg_percent/))
   !
 endif
 !
 if (.not.l_life) then 
   if ( (.not.l_cohsex) .or. (l_cohsex.and.COHSEX_use_empties) ) then
     call IO_desc_add(qp%desc,str='Sc/G bands',SZ=2,kind='i',I=QP_n_G_bands)
   endif
   if (l_cohsex) then
     if (ver_is_gt_or_eq(-1,(/4,3,1/))) then
       call IO_desc_add(qp%desc,str='Sc Empty Bands',kind='l',L=COHSEX_use_empties)
     endif
   else
     call IO_desc_add(qp%desc,str='Sc/G damping',term='ev',kind='r',R=(/QP_G_damp*HA2EV/))
   endif
   !
   call IO_desc_add(qp%desc,str='Sc bands terminator',kind='l',L=l_GW_terminator)
   !
   if (l_GW_terminator) then
     !
     call IO_desc_add(qp%desc,str='Sc terminator kind',kind='s',S=trim(GW_terminator_Kind))
     !
     call IO_desc_add(qp%desc,str='Sc Terminator pole',kind='r',R=(/GW_terminator_E*HA2EV/))
     !
   endif
   !
   if (l_HF_and_locXC) then
     call IO_desc_add(qp%desc,str='Sx RL components',kind='i',I=(/QP_ng_Sx/))
   endif
   !
 endif
 !
#if defined _QED
 !
 if (l_elphoton_corr) then
   !
   call IO_desc_add(qp%desc,str='El-Photon correlation',kind='l',L=l_elphoton_corr)
   !
   if (.not.l_life) then
     call IO_desc_add(qp%desc,str='QED RL components',kind='i',I=(/QP_QED_ng/))
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
     if (.not.l_elel_corr) qp%desc%n=n_descs_save
     !
     if (trim(QP_solver)/='g') then
       call IO_desc_add(qp%desc,str='[ph] dS/dw steps',kind='i',I=(/QP_dSc_steps/))
       !
       call IO_desc_add(qp%desc,str='[ph] dS/dw step',term='ev',kind='r',R=(/QP_dSc_delta*HA2EV/))
       !
       call IO_desc_add(qp%desc,str='[ph] Sc/G damping',term='ev',kind='r',R=(/QP_G_damp*HA2EV/))
       !
     endif
     !
     call IO_desc_add(qp%desc,str='El-Ph correlation',kind='l',L=l_elph_corr)
     !
     call IO_desc_add(qp%desc,str='El-Ph Sc. G bands',SZ=2,kind='i',I=(/1,QP_PH_n_G_bands/))
     !
     if (ver_is_gt_or_eq(-1,(/3,2,2/))) then
       call IO_desc_add(qp%desc,str='El-Ph phonon branches',SZ=2,kind='i',I=elph_branches)
     endif
     !
     if (ver_is_gt_or_eq(-1,(/4,0,0/))) then
       call IO_desc_add(qp%desc,str='El-Ph momenta used',kind='i',I=(/elph_nDBs_used/))
     endif
     !
     if (ver_is_gt_or_eq(-1,revision=4400)) then
       call IO_desc_add(qp%desc,str='Energy treshold',term='FAN',kind='r',R=(/FAN_deltaE_treshold*HA2EV/))
       !
       call IO_desc_add(qp%desc,str='Energy treshold',term='DW',kind='r',R=(/DW_deltaE_treshold*HA2EV/))
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
   call IO_desc_add(qp%desc,str='[GF] GF is causal',kind='l',L=GF_is_causal)
   !
   call IO_desc_add(qp%desc,str='[GF] Real-axis steps',kind='i',I=(/QP_Sc_steps/))
   !
   call IO_desc_add(qp%desc,str='[GF] Initial E range',term='ev',SZ=2,kind='r',R=QP_G_er*HA2EV)
   !
   if (ver_is_gt_or_eq(-1,revision=530)) then
     call IO_desc_add(qp%desc,str=' [GF] Zoom treshold',term='o/o',kind='r',R=(/QP_G_Zoom_treshold/))
   endif
   !
   call IO_desc_add(qp%desc,str='[GF] Damping',term='ev',SZ=2,kind='r',R=QP_G_dr*HA2EV)
   !
 endif
 !
 ! QP states
 !
 call QP_state_print( qp )
 !
end subroutine
