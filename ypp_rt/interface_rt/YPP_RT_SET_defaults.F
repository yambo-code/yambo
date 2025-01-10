!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine YPP_RT_SET_defaults()
 !
 use pars,           ONLY:SP
 use units,          ONLY:FS2AUT,HA2EV
 use YPPm,           ONLY:E_field
 use YPP_real_time,  ONLY:RT_conf,Eh_pumped_Nel,Eh_pumped_pair_BZ_width,Nel_fac,&
&                         Eh_pumped_pair_deltaE,Eh_pumped_pair_energy,Eh_pumped_pair_width,l_RealTime,l_RT_dos,&
&                         l_RT_abs,l_RT_bands,l_RT_DBs,l_RT_density,l_RT_energy,l_RT_lifetimes,l_RT_occupations,&
&                         l_RT_time,l_RT_X,Eh_pumped_pair_n_BZ_regions,Probe_Keyword ,Pump_Keyword,X_order,&
&                         Eh_mu,Eh_temp,h_mu_autotune_thr,RT_p_format,l_RT_G_two_times,X_kind,Pump_path
 use fields,         ONLY:n_ext_fields
 !
 implicit none
 !
 call YPP_SET_defaults()
 !
 ! RT OCCUPATIONS
 !
 l_RealTime=.FALSE.
 l_RT_DBs  =.FALSE.
 !
 ! Absorption, occupations, lifetimes, density, G_lesser
 !
 l_RT_X           =.FALSE.
 l_RT_abs         =.FALSE.
 l_RT_occupations =.FALSE.
 l_RT_lifetimes   =.FALSE.
 l_RT_density     =.FALSE.
 l_RT_G_two_times =.FALSE.
 !
 ! PP kind
 !
 l_RT_time  =.FALSE.
 l_RT_energy=.FALSE.
 l_RT_dos   =.FALSE.
 l_RT_bands =.FALSE.
 !
 ! BANDS
 !
 Nel_fac=1._SP
 !
 !REMOVE SYMMETRIES
 !
 E_field=0._SP
 !
 !ANALIZE RESPONSE
 !
 RT_conf%T_range(:)  =real((/-FS2AUT,-FS2AUT/),SP)
 RT_conf%W_n_steps   =200
 RT_conf%Carriers_W_range(:)  =(/0._SP,20./HA2EV/)
 RT_conf%X_W_range(:)  =(/0._SP,20./HA2EV/)
 Probe_Keyword ="none"
 Pump_Keyword  ="none"
 X_kind        ="rhorho"
 X_order       =1
 n_ext_fields  =1
 !
 !MANUAL PUMP
 !
 Eh_pumped_pair_n_BZ_regions=0
 Eh_pumped_pair_deltaE=-1._SP/HA2EV
 Eh_pumped_pair_energy=-1._SP/HA2EV
 Eh_pumped_pair_width=0.1_SP/HA2EV
 Eh_pumped_pair_BZ_width=0.1_SP
 Eh_pumped_Nel=1._SP
 !
 !FERMI FUNCTION PUMP
 !
 Eh_mu   =(/-0.001_SP/HA2EV,0.001_SP/HA2EV/)
 Eh_temp =(/0._SP/HA2EV,0._SP/HA2EV/)
 h_mu_autotune_thr = 0._SP
 !
 !TR abs
 !
 RT_p_format="3col" 
 !
end subroutine YPP_RT_SET_defaults
