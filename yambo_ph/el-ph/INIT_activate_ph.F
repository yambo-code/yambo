!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine INIT_activate_ph()
 !
 use ELPH,           ONLY:elph_use_q_grid
 !
 implicit none
 !
 if ((l_life.or.l_gw0).and.l_phel_corr) then
   call CPU_activate("PH_SE")
 endif
 !
 if (l_setup) call initactivate(1,'BSEscatt')
 !
 if (l_optics.and.l_bse.and.l_bss.or.(l_bse.and.l_rpa_IP)) then
   !
   if (l_elph_corr.and..not.elph_use_q_grid) call initactivate(1,'ElPhRndNq')
 endif
 !
 if (l_gw0.or.l_life) then
   !
#if defined  _PHEL 
   !
   if (l_phel_corr) then
     call initactivate(1,'ElecTemp BoseTemp PH_SE_mode GphBRnge GDamping PHELQpts ElPhModes PHDbGdsize DbGdQsize GDamping')
     call initactivate(1,'ExtendOut OnMassShell')
     if (      l_gw0) call initactivate(1,'DysSolver GEnSteps GEnRnge PHEL_QPH_En')
     if (.not. l_gw0) call initactivate(1,'GDamping PHELTrans') 
   endif
   !
#endif
   !
   if (l_elph_corr) then
     call INIT_QP_ctl_switch('G')
     call initactivate(1,'DysSolver')
     call initactivate(1,'GphBRnge FANdEtresh DWdEtresh ElPhModes GDamping dScStep ExtendOut ElPhRndNq RandQpts')
     call initactivate(1,'GkkpDB WRgFsq NewtDchk OnMassShell')
   endif
   if (trim(QP_solver)=="g".and.l_elph_corr) then
     call initactivate(1,'GEnSteps GEnRnge GEnMode GDmRnge GreenFTresh GreenF2QP') 
     call initactivate(-1,'WRgFsq NewtDchk GDamping ExtendOut OnMassShell')
   endif
   !
 endif
 !
 ! El-Ph: Frohlich Hamiltonian
 !
 if (l_elph_Hamiltonian) then
   call initactivate(1,'ElPhHBRnge ElPhModes ElPhHKpt GDamping REStresh')
 endif
 !
end subroutine
