!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine barriers()
 !
 ! ENVIROMENTS:
 !  Finite T     Finite_Tel
 !  SPIN         n_spin>1
 !  Non Herm K   BS_K_coupling  
 !  Metal        metallic_system
 !  Dyson Solver newton,secant, green
 !  TR or SI     TR_or_SI
 !  Uniform grid Xk_grid_is_uniform
 !  
 ! RUNLEVELS:
 !  RIM         l_rim          COL_CUT     l_col_cut 
 !  ACFDT       l_acfdt        GW PPA      gw0_ppa 
 !  GW RA       gw0_raxis      GW COHSEX   gw0_cohsex 
 !  LIFE        l_life         EL-PH       gw0_el_ph 
 !  SIGMAX      l_xx_vxc       ALDA(G)     alda_g_space 
 !  ALDA(R)     alda_r_space   BS Fxc      l_bs_fxc 
 !  EPSm1 STAT. l_em1s         BSE TDDFT   bss_tddft
 !  OPT G-SPACE l_chi          OPT BSE     l_bse 
 !  BSE DIAGO   bss_diago      BSE INV     bss_inv 
 !  BSE HAYD    bss_haydock    SC          l_sc_run
 !  TDHF
 !  MAGNETIC    l_magnetic
 !  COLL_IO     l_COLLISIONS_IO
 !
 use drivers
 use pars,       ONLY:schlen
 use D_lattice,  ONLY:i_time_rev,i_space_inv
 use BS,         ONLY:BSS_mode
 use electrons,  ONLY:n_spin,n_met_bands,n_full_bands,n_spinor,spin_orbit
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
 gw0_el_ph=l_gw0.and.l_ph_corr
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
 !call switch_off_runlevel('bs_fxc',on_name=' ')
 ! GPL_INCLUDE_END
 !
 ! RUNLEVELS
 !===========
 !
 ! If the system is non magnetic you can safely comment the nex line
 ! Otherwise you have to run ypp_rt to remove the trev symmetry when SO is present
 if(i_time_rev==1.and.spin_orbit) call switch_off_runlevel('all',on_name=' ')
 !
#if defined _RT
 call switch_off_runlevel('gw0',on_name=' ')
#endif
 !
 ! GPL_EXCLUDE_START SC_IGNORE
 !
 if (l_real_time) then
   call switch_off_runlevel('all',on_name='scpot negf el_ph '//trim(always_runlevels))
   goto 1
 endif
 !
 if (l_sc_run) then
   call switch_off_runlevel('all',on_name='scpot HF_and_locXC cohsex em1s negf magnetic collisions_IO '//trim(always_runlevels))
   goto 1
 endif
 !
 ! GPL_EXCLUDE_END SC_IGNORE
 !
 if (gw0_cohsex) then
   call switch_off_runlevel('all',on_name='gw0 em1s cohsex el_el el_ph HF_and_locXC '//trim(always_runlevels))
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
   on_runlevels='gw0 em1d el_el el_ph HF_and_locXC'
   ! GPL_INCLUDE_START ELPH_IGNORE
   !on_runlevels='gw0 em1d el_el HF_and_locXC'
   ! GPL_INCLUDE_END ELPH_IGNORE
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 !
 if (gw0_ppa) then
   on_runlevels='gw0 ppa em1d el_el el_ph HF_and_locXC'
   ! GPL_INCLUDE_START SC_IGNORE
   !on_runlevels='gw0 ppa em1d HF_and_locXC'
   ! GPL_INCLUDE_END SC_IGNORE
   call switch_off_runlevel('all',on_name=trim(on_runlevels)//' '//trim(always_runlevels))
   goto 1
 endif
 if (alda_g_space) then
   call switch_off_runlevel('all',on_name='chi optics alda_fxc '//trim(always_runlevels))
   goto 1
 endif
 if (alda_r_space) then
   call switch_off_runlevel('all',on_name='bse optics alda_fxc bss '//trim(always_runlevels))
   goto 1
 endif
 if (l_bss.or.l_bse) then
   on_runlevels='tdhf bs_fxc bse bss em1s em1d ppa optics'
   ! GPL_INCLUDE_START SC_IGNORE
   !on_runlevels='tdhf bse bss em1s em1d ppa optics'
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
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 !
 !if (n_spin>1) call switch_off_runlevel('all',on_name=' ')
 !
 ! GPL_INCLUDE_END SPIN_IGNORE
 !
 ! GPL_EXCLUDE_START SPIN_INGORE
 !
 if (n_spin>1) then
   call switch_off_runlevel('all',on_name=&
&       'chi optics HF_and_locXC bse em1s em1d ppa gw0 bss alda_fxc bs_fxc cohsex magnetic scpot '//&
&       trim(always_runlevels))
   if (n_spinor>1) call switch_off_runlevel('scpot',on_name=' ')
   if (gw0_raxis) call switch_off_runlevel('all',on_name=' ')
 endif 
 !
 ! GPL_EXCLUDE_END SPIN_INGORE
 !
 ! MORE
 !======
 if ( (l_ppa.and.trim(QP_solver)=='s').or.l_cohsex ) QP_solver='n'
 !
end subroutine
