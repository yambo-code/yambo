!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_barriers()
 !
 use pars,       ONLY:lchlen
 use drivers,    ONLY:l_elph_corr,l_gw0,l_phel_corr,l_real_time
 use R_lattice,  ONLY:Xk_grid_is_uniform
 use QP_m,       ONLY:QP_solver
 use it_tools,   ONLY:switch_off_runlevel
 !
 implicit none
 ! 
 ! Additional logicals ...
 !
 character(lchlen) :: always_on_runlevels,on_runlevels
 !
 always_on_runlevels='setup rim_cut'
 !
 ! Note that switch_off_runlevel defines a barrier to all others runlevels
 ! except the ones specified. switch_off_runlevel switches off
 ! only already activated runlevels
 !
 ! RUNLEVELS
 !===========
 !
 if (l_real_time) then
   on_runlevels='HF_and_locXC cohsex em1s dipoles negf collisions el_el_corr el_ph_corr ph_el_corr '//&
&               'el_el_scatt el_ph_scatt el_photon_scatt el_photon_corr ph_el_scatt'
   call switch_off_runlevel('all',except=trim(on_runlevels)//' '//trim(always_on_runlevels))
 endif
 !
 !
 ! ENVIROMENTS
 !=============
 !
 ! Non Uniform grids
 !===================
 if (.not.Xk_grid_is_uniform) then
   on_runlevels='optics chi bse bss negf'
   if (l_elph_corr) on_runlevels='optics chi bse bss el_ph_corr ph_el_corr dyson gw0'
   call switch_off_runlevel('all',except=trim(on_runlevels)//' '//trim(always_on_runlevels))
 endif 
 !
 ! MORE
 !======
 if ( l_phel_corr.and.trim(QP_solver)=='s' ) QP_solver='n'
 !
end subroutine
