!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SET_defaults_nl(INSTR,IND,OD,COM_DIR)
 !
 use nl_optics,      ONLY:NL_correlation,NL_er,n_frequencies,NL_damping,NL_LRC_alpha,NL_initial_versor,     &
&                         NL_verb_name,NL_bands,NL_desc,n_angles,NL_rot_axis
 use fields,         ONLY:Efield,Efield_reset
 use collision_ext,  ONLY:COLLISIONS_cutoff
 !
 implicit none
 !
 character(*) :: INSTR,IND,OD,COM_DIR
 !
 call SET_defaults_rt(instr,ind,od,com_dir)
 !
 COLLISIONS_cutoff=0.0005_SP
 RAD_LifeTime = -1._SP*FS2AUT  ! Automatic set equal to the dephasing
 !
 NE_tot_time   =  -1._SP*FS2AUT 
 Integrator_name='INVINT'
 NL_bands      =(/0,0/)
 NL_correlation="IPA"
 NL_er         =(/-1._SP,-1._SP/)/HA2EV
 n_frequencies =0
 n_angles      =0
 NL_rot_axis   =(/0._SP,0._SP,0._SP/)
 NL_initial_versor=(/0._SP,0._SP,0._SP/)
 NL_damping    =0.2_SP/HA2EV
 NL_LRC_alpha  =0._SP
 !
 do i1=1,n_ext_fields_max
   call Efield_reset(Efield(i1))
   Efield(i1)%t_initial=RT_step
   Efield(i1)%frequency=0.1_SP/HA2EV
   Efield(i1)%intensity=0.0_SP
   Efield(i1)%FWHM=2._SP*FS2AUT
   Efield(i1)%ef_name="none"
   Efield(i1)%versor =(/0._SP,0._SP,0._SP/)
   Efield(i1)%ef_pol ="linear"
 enddo
 !
 Efield(1)%versor =(/0._SP,0._SP,0._SP/)
 Efield(1)%intensity=1000._SP*kWCMm22AU
 Efield(1)%ef_name="SOFTSIN"
 NL_verb_name='high'
 !
 call IO_desc_reset(NL_desc)
 !
end subroutine
