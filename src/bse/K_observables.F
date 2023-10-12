!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine K_observables( )
 !
#if defined _SLEPC && !defined _NL
 use BS_solvers,   ONLY:run_Slepc
#endif
 use drivers,      ONLY:l_rpa_IP
 use X_m,          ONLY:global_gauge
 use BS_solvers,   ONLY:run_Haydock,run_inversion,run_Diago,BSS_mode
 use BS,           ONLY:BS_dip_size,l_BS_anomalous_Hall,l_BS_abs,l_BS_jdos,l_BS_trace,BSE_prop,&
&                       l_BS_esort,l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_anomalous_Hall,l_BS_photolum,&
&                       l_BS_optics,BS_dip_size,l_BS_magnons,l_BS_photolum
 use parser_m,     ONLY:parser
 use stderr,       ONLY:STRING_match
 use electrons,    ONLY:levels,n_sp_pol,n_spin
 !
 implicit none
 !
 type(levels)  ::Ken
 !
 ! What...
 !=========
 l_BS_abs       = STRING_match(BSE_prop,'abs')
 l_BS_jdos      = STRING_match(BSE_prop,'jdos')
 l_BS_dichroism = STRING_match(BSE_prop,'dich')
 l_BS_kerr      = STRING_match(BSE_prop,'kerr')
 l_BS_photolum  = STRING_match(BSE_prop,'photolum')
 l_BS_magnons   = STRING_match(BSE_prop,'magn') .and.n_spin>1
 !
 ! How ...
 !---------
 l_BS_esort     = STRING_match(BSE_prop,'esrt')
 !
 ! How to treat dipoles...
 !------------------------
 l_BS_trace     = STRING_match(BSE_prop,'trace')
 !
 ! Consequences which overwrites the kernel_init definitions...
 !--------------------------------------------------------------
 if (l_BS_magnons.and.n_sp_pol==2) then
   l_BS_abs    = .false.
   l_BS_kerr   = .false.
   l_BS_trace  = .false.
   call warning(" n_sp_pol=2 and magnons requested. Spin flip BSE only is computed")
 endif
 if (l_BS_kerr) then
   call parser('AnHall',l_BS_anomalous_Hall)
   BS_dip_size=2
   l_BS_abs=.TRUE.
 endif
 if (l_BS_photolum) global_gauge="velocity"
 if (l_BS_trace   ) BS_dip_size = 3
 if (l_BS_trace .and. .not.l_rpa_IP) call error(" trace coded only at the IP level")
 !
 l_BS_optics=l_BS_abs.or.l_BS_kerr.or.l_BS_dichroism.or.l_BS_photolum
 !
 ! Solver Logicals
 !=================
 run_Diago     = STRING_match(BSS_mode,'d')
 run_Haydock   = STRING_match(BSS_mode,'h')
 run_inversion = STRING_match(BSS_mode,'i')
#if defined _SLEPC && !defined _NL
 run_Slepc     = STRING_match(BSS_mode,'s')
#endif
 if (l_BS_photolum.and.run_Haydock) then
   call warning('PL currently implemented only using the diagonalization/inversion solver')
   run_Haydock  =.false.
 endif
 if (run_Haydock.and.allocated(Ken%W)) then
   call warning('K with QP widths not supported in the Haydock solver')
   run_Haydock=.false.
 endif
 !
end subroutine
