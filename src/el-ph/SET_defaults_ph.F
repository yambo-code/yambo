!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_defaults_ph(INSTR,IND,OD,COM_DIR)
 !
 use pars,       ONLY:SP
 use C_driver,   ONLY:code_branch
 use units,      ONLY:HA2EV
 use ELPH,       ONLY:ph_modes,elph_nb,gsqF_energy_steps,eval_G_using_KK,use_PH_DbGd,EkplusQ_mode,gkkp_db,&
&                     elph_Ham_bands,elph_Ham_ik,elph_branches,RES_tresh,FAN_deltaE_treshold,DW_deltaE_treshold
 !
 implicit none
 !
 character(*) :: INSTR,IND,OD,COM_DIR
 !
 include 'branch.inc'
 !
 call SET_defaults(INSTR,IND,OD,COM_DIR)
 !
 ! ELPH
 !
 ph_modes=0
 elph_nb=0
 elph_branches=0
 gsqF_energy_steps=2
 eval_G_using_KK=.FALSE.
 elph_Ham_bands=0
 elph_Ham_ik=1
 RES_tresh=0.01_SP
 DW_deltaE_treshold=1.E-6_SP/HA2EV   ! This is a cutoff on electronic energy differences
 FAN_deltaE_treshold=1.E-6_SP/HA2EV  ! This is a cutoff on phonon energies
 use_PH_DbGd=.FALSE.
 EkplusQ_mode='interp' ! E(k+q) energies (interp | dftp )
 gkkp_db='gkkp'        ! gkkp_db ( gkkp | gkkp_expanded | genFroh )
 !
end subroutine
