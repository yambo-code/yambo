!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_ypp_load(defs)
 !
 use it_tools,      ONLY:it
 use it_m,          ONLY:initdefs,E_unit,G_unit,V_general,V_RL,&
&                        Time_unit,Bfield_unit,V_real_time,T_unit,V_nl_optics,V_io,V_sc
 implicit none
 !
 type(initdefs) :: defs
 !
 ! RunLevels...
 !==============
 !
 call INIT_load_ypp_rt(defs)
 !
 call it('r',defs,'NL_X',     '[R] NonLinear Optics Response Post-Processing')
 call it('r',defs,'NL_exc',   '[R] NonLinear Optics Excitation Post-Processing')
 !
 ! Variables...
 !==============
 call it('f',defs,'PrtPwrSpec','Print power spectrum',verb_level=V_nl_optics)
 call it(defs,'PumpPATH',"Path of the simulation with the Pump only",Pump_path)
 !
end subroutine
