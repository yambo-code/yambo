! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_occupations_driver(k,q,en)
 !
 use drivers,        ONLY:l_rt_carriers_in_use
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use IO_m,           ONLY:LOG
 use QP_CTL_m,       ONLY:QP_apply
 use YPP_interfaces, ONLY:PROJECTION_setup
 use RT_control,     ONLY:RT_apply,CARR_RT_IO_t
 use real_time,      ONLY:RT_bands
 use YPPm,           ONLY:DIPs_mask,l_dipoles,DIPs,DIPs_V_bands
 use YPP_real_time,  ONLY:l_RT_time,l_RT_energy,l_RT_bands,l_RT_dos,RT_conf
 !
#include<memory.h>
 !
 type(bz_samp) :: k,q
 type(levels)  :: en
 !
 call section('+','Real-Time components (Occupations, Lifetimes,...)')
 !=====================================================================
 !
 ! Pre-setup of carrier components and Time evolution parameters
 !===============================================================
 call RT_apply(RT_bands,en,k,IO_COM=LOG,TIMES_ONLY=.TRUE.,skip_interpolation=.TRUE.)
 !
 if (.not.l_rt_carriers_in_use) return
 !
 if (l_dipoles) then
   call section('=',"The Dipoles `Mask`")
   !=====================================
   DIPs_V_bands=RT_bands
   call DIPOLE_ypp_driver(k,en,q)
 endif
 !
 ! Setups
 !========
 call PROJECTION_setup("setup")
 !
 call RT_time_configuration_setup("CARRIERS",CARR_RT_IO_t)
 !
 ! Additional QP corrections
 !===========================
 call QP_state_table_setup(en)
 !
 call QP_apply(RT_bands,en,k,'G',msg_fmt='rs')
 !
 ! Occupations energy plot 
 !=========================
 if (l_RT_energy) call RT_components_energy_plot(en,k,RT_conf%T_n_steps)
 !
 ! Occupations time plot 
 !=========================
 if (l_RT_time)   call RT_occ_time_plot(en,k,RT_conf%T_n_steps)
 !
 ! Occupations 2D dos plot 
 !=========================
 if (l_RT_dos)    call RT_dos_time_plot(en,k,RT_conf%T_n_steps)
 !
 ! Occupations bands fit 
 !=======================
 if (l_RT_bands) call RT_occ_bands_interpolation(en,k,q,RT_conf%T_n_steps)
 !
 ! Clean..
 YAMBO_FREE(DIPs_mask)
 !
end subroutine RT_occupations_driver
