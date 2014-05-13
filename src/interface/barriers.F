!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine barriers()
 !
 use drivers
 use pars,       ONLY:schlen
 use D_lattice,  ONLY:i_time_rev,i_space_inv
 use BS,         ONLY:BSS_mode
 use electrons,  ONLY:n_spin,n_met_bands,n_full_bands,n_spinor
 use QP_m,       ONLY:QP_solver
 use it_m,       ONLY:switch_off_runlevel
 implicit none
 ! 
 ! Additional logicals ...
 !
 logical :: metallic_system, newton,secant,green,bss_tddft,&
            TR_or_SI,gw0_raxis,alda_r_space,bss_diago,&
&           bss_haydock,gw0_ppa,gw0_cohsex,gw0_el_ph,alda_g_space,bss_inv
 character(schlen) :: always_runlevels,on_runlevels
 !
 ! ...
 !
 metallic_system=n_met_bands/=n_full_bands
 newton=trim(QP_solver)=='n'
 secant=trim(QP_solver)=='s'
 green=trim(QP_solver)=='g'
 TR_or_SI=i_time_rev==1.or.i_space_inv==1
 gw0_raxis=l_gw0.and..not.l_cohsex.and..not.l_ppa
 gw0_ppa=l_gw0.and.l_ppa
 gw0_cohsex=l_gw0.and.l_cohsex
 gw0_el_ph=l_gw0.and.l_elph_corr
 alda_r_space=l_alda_fxc.and.l_bse
 alda_g_space=l_alda_fxc.and..not.l_bse
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
#if defined _RT
 call switch_off_runlevel('gw0',on_name=' ')
#endif
 !
 ! GPL_EXCLUDE_START SC_IGNORE
 !
 if (l_real_time) then
   on_runlevels='scpot HF_and_locXC cohsex em1s negf collisions el_el_corr el_ph_corr el_el_scatt el_ph_scatt'
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (l_eval_collisions) then
   on_runlevels='scpot HF_and_locXC cohsex em1s collisions el_el_corr el_ph_corr el_el_scatt el_ph_scatt'
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (l_sc_run) then
   on_runlevels='scpot HF_and_locXC cohsex em1s negf magnetic el_el_corr collisions '
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
   call switch_off_runlevel('all',on_name='life em1d '//trim(always_runlevels))
   goto 1
 endif
 !
 if (gw0_raxis) then
   on_runlevels='gw0 em1d el_el_corr el_ph_corr HF_and_locXC'
   ! GPL_INCLUDE_START 
   !on_runlevels='gw0 em1d el_el_corr HF_and_locXC'
   ! GPL_INCLUDE_END 
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (gw0_ppa) then
   on_runlevels='gw0 ppa em1d el_el_corr el_ph_corr HF_and_locXC'
   ! GPL_INCLUDE_START SC_IGNORE
   !on_runlevels='gw0 ppa em1d HF_and_locXC'
   ! GPL_INCLUDE_END SC_IGNORE
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 if (alda_g_space) then
   call switch_off_runlevel('all',on_name='optics chi tddft '//trim(always_runlevels))
   goto 1
 endif
 if (alda_r_space) then
   call switch_off_runlevel('all',on_name='optics bse bsk bss tddft '//trim(always_runlevels))
   goto 1
 endif
 if (l_bss.or.l_bse) then
   on_runlevels='optics bse bsk bss tddft em1s em1d ppa'
   ! GPL_INCLUDE_START SC_IGNORE
   ! !This barrier prevent tddft to work as l_alda_fxc is not defined and alda_r_space does not work...
   ! !Barrier commented (Davide: 25/06/2013)
   ! !on_runlevels='optics bse bsk bss tddft em1s em1d ppa'
   ! GPL_INCLUDE_END SC_IGNORE
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
1 continue
 !
 ! ENVIROMENTS
 !=============
 !
 ! SPIN
 !=============
 if (n_spin>1) then
   on_runlevels='optics chi bse bsk bss em1s em1d ppa HF_and_locXC cohsex gw0 magnetic scpot'
   ! GPL_INCLUDE_START SC_IGNORE
   !on_runlevels='optics chi bse bsk bss em1s em1d ppa HF_and_locXC cohsex gw0'
   ! GPL_INCLUDE_END SC_IGNORE
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   if (n_spinor>1) call switch_off_runlevel('scpot',on_name=' ')
 endif 
 !
 ! MORE
 !======
 if ( (l_ppa.and.trim(QP_solver)=='s').or.l_cohsex ) QP_solver='n'
 !
#if defined _KERR
 if(l_kerr) then
   on_runlevels='optics chi bse bsk bss em1s em1d ppa HF_and_locXC cohsex gw0 magnetic scpot'
   ! GPL_INCLUDE_START
   !on_runlevels='optics bse'
   ! GPL_INCLUDE_END
   if(bss_inv.or.bss_tddft) on_runlevels=' '
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
 endif
#endif
 !
end subroutine
