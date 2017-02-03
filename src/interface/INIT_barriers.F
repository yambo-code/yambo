!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine INIT_barriers()
 !
 use drivers,    ONLY:l_acfdt,l_bse,l_bss,l_cohsex,l_elph_corr,l_gw0,l_kerr,&
&                     l_eval_collisions,l_life,l_ppa,l_real_time,l_sc_run,l_tddft
#if defined _PL
 use drivers,        ONLY:l_photolum
#endif
 use pars,       ONLY:lchlen
 use D_lattice,  ONLY:i_time_rev,i_space_inv
 use R_lattice,  ONLY:Xk_grid_is_uniform
 use BS,         ONLY:BSK_mode
 use BS_solvers, ONLY:BSS_mode
 use electrons,  ONLY:n_spin,n_met_bands,n_full_bands
 use QP_m,       ONLY:QP_solver
 use it_m,       ONLY:switch_off_runlevel
 implicit none
 ! 
 ! Additional logicals ...
 !
 logical :: metallic_system, newton,QP_secant,green,bss_tddft,bse_bsfxc,&
            TR_or_SI,gw0_raxis,tddft_r_space,bss_diago,bse_sex,&
&           bss_haydock,gw0_ppa,gw0_cohsex,gw0_el_ph,tddft_g_space,bss_inv,l_flag
 character(lchlen) :: always_runlevels,on_runlevels
 !
 ! ...
 !
 metallic_system=n_met_bands/=n_full_bands
 !
 newton=trim(QP_solver)=='n'
 QP_secant=trim(QP_solver)=='s'
 green=trim(QP_solver)=='g'
 !
 TR_or_SI=i_time_rev==1.or.i_space_inv==1
 !
 gw0_raxis=l_gw0.and..not.l_cohsex.and..not.l_ppa
 gw0_ppa=l_gw0.and.l_ppa
 gw0_cohsex=l_gw0.and.l_cohsex
 gw0_el_ph=l_gw0.and.l_elph_corr
 !
 tddft_r_space=l_tddft.and.     l_bse
 tddft_g_space=l_tddft.and..not.l_bse
 !
 bse_sex  =trim(BSK_mode)=='SEX'
 bse_bsfxc=trim(BSK_mode)=='BSfxc'
 !
 bss_diago=l_bss.and.index(BSS_mode,'d')/=0
 bss_inv=l_bss.and.index(BSS_mode,'i')/=0
 bss_haydock=l_bss.and.index(BSS_mode,'h')/=0
 bss_tddft=l_bss.and.index(BSS_mode,'t')/=0
 !
 always_runlevels='setup rim_cut'
 !
 ! Note that switch_off_runlevel defines a barrier to all others runlevels
 ! except the ones specified. switch_off_runlevel switches off
 ! only already activated runlevels
 !
 ! NON-GPL RUNLEVELS that MUST BE SWITCHED OFF  
 ! (those runlevels can be on if a non-gpl input file is being reading)
 !====================================================================
 !
 ! GPL_INCLUDE_START
 !call switch_off_runlevel('acfdt',on_name=' ')
 ! GPL_INCLUDE_END
 !
 ! RUNLEVELS
 !===========
 !
 ! GPL_EXCLUDE_START SC_IGNORE
 !
 if (l_real_time) then
   on_runlevels='HF_and_locXC cohsex em1s negf collisions el_el_corr el_ph_corr '//&
&               'el_el_scatt el_ph_scatt el_photon_scatt el_photon_corr'
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (l_eval_collisions) then
   on_runlevels='scrun HF_and_locXC cohsex em1s collisions el_el_corr el_ph_corr el_el_scatt '//&
&               'el_ph_scatt el_photon_scatt el_photon_corr'
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (l_sc_run) then
   on_runlevels='scrun HF_and_locXC cohsex em1s negf magnetic el_el_corr collisions '
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 ! GPL_EXCLUDE_END SC_IGNORE
 !
 if (gw0_cohsex) then
   call switch_off_runlevel('all',on_name='gw0 em1s cohsex el_el_corr el_ph_corr HF_and_locXC '//trim(always_runlevels))
   goto 1
 endif
 !
 ! GPL_EXCLUDE_START
 !
 if (l_acfdt) then
   call switch_off_runlevel('all',on_name='acfdt HF_and_locXC '//trim(always_runlevels))
   goto 1
 endif
 !
 ! GPL_EXCLUDE_END
 !
 if (l_life) then
   call switch_off_runlevel('all',on_name='life em1d el_el_corr el_photon_corr '//trim(always_runlevels))
   goto 1
 endif
 !
 if (gw0_raxis) then
   ! GPL_EXCLUDE_START
   on_runlevels='gw0 em1d el_el_corr el_ph_corr el_photon_corr HF_and_locXC'
   ! GPL_EXCLUDE_END
   ! GPL_INCLUDE_START 
   !on_runlevels='gw0 em1d el_el_corr el_ph_corr HF_and_locXC'
   ! GPL_INCLUDE_END 
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (gw0_ppa) then
   on_runlevels='gw0 ppa em1d el_el_corr el_ph_corr HF_and_locXC'
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 if (tddft_g_space) then
   call switch_off_runlevel('all',on_name='optics chi tddft '//trim(always_runlevels))
   goto 1
 endif
 if (tddft_r_space) then
   call switch_off_runlevel('all',on_name='optics bse bsk bss tddft '//trim(always_runlevels))
   goto 1
 endif
 !
 l_flag=l_bss.or.l_bse
#if defined _PL
 l_flag=l_bss.or.l_bse.or.l_photolum
#endif
 if (l_flag) then
   on_runlevels='optics bse bsk bss tddft em1s em1d ppa photolum'
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
1 continue
 !
 ! ENVIROMENTS
 !=============
 !
 ! Non Uniform grids
 !===================
 if (.not.Xk_grid_is_uniform) then
   on_runlevels='optics chi bse bsk bss'
   if (l_elph_corr) on_runlevels='optics chi bse bsk bss el_ph_corr gw0'
   if (tddft_r_space.or.bse_sex.or.bse_bsfxc) BSK_mode='hartree'    
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
 endif 
 !
 ! SPIN
 !=============
 if (n_spin>2) then
   on_runlevels='optics chi bse bsk bss em1s em1d ppa HF_and_locXC cohsex gw0 magnetic scrun'
   on_runlevels=trim(on_runlevels)//' negf collisions el_el_corr el_ph_corr el_el_scatt el_ph_scatt'
   ! GPL_INCLUDE_START SC_IGNORE
   !on_runlevels='optics chi bse bsk bss em1s em1d ppa HF_and_locXC cohsex gw0'
   ! GPL_INCLUDE_END SC_IGNORE
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
 endif 
 !
 ! MORE
 !======
 if ( (l_ppa.and.trim(QP_solver)=='s').or.l_cohsex ) QP_solver='n'
 !
#if defined _KERR
 if(l_kerr) then
   on_runlevels='optics chi bse bsk bss em1s em1d ppa HF_and_locXC cohsex gw0 magnetic scrun'
   ! GPL_INCLUDE_START
   !on_runlevels='optics bse'
   ! GPL_INCLUDE_END
   if(bss_inv.or.bss_tddft) on_runlevels=' '
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
 endif
#endif
 !
end subroutine
