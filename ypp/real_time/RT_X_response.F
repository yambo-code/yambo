!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA
!
subroutine RT_X_response(en,k,q)
 !
 use YPP_real_time,  ONLY:Use_FFT,Pol,Curr,N_probe_frequencies,&
&                         N_probes,N_pumps,X_order,l_RT_pol,OBS_el_free,&
&                         l_RT_X_inversion,Probe_path,Pump_path,N_probes,RT_conf,l_skip_OBS_IO
 use YPP_interfaces, ONLY:RT_OBSERVABLES_IO
 use RT_control,     ONLY:OBS_RT_IO_t
 use pars,           ONLY:schlen,n_fields_defs_max
 use stderr,         ONLY:STRING_split,STRING_same
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use fields,         ONLY:Efield
 use parser_m,       ONLY:parser
 !
#include<y_memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer           :: Ndefs,i_o
 character(schlen) :: probe_defs(n_fields_defs_max)
 logical           :: l_RT_LRR_real_field,l_RT_X_effective,l_RT_SHG_qssin,IO_error
 !
 call RT_OBSERVABLES_IO_paths('JP',en)
 !
 ! Here only the first Field is analyzed
 !=======================================
 call STRING_split(Efield(1)%ef_name,probe_defs,n_non_empty_strings=Ndefs)
 !
 if ( trim(probe_defs(1))=='FROM_FILE') then
   call RT_load_field_from_file_init(trim(probe_defs(2)))
   call RT_load_field_from_file(trim(probe_defs(2)),1)
 endif
 !
 l_RT_LRR_real_field= all( (/ N_pumps<=1, N_probes==1, X_order==1/) )
 !
 l_RT_SHG_qssin     = all( (/ trim(probe_defs(1))=="QSSIN" , N_pumps==0, N_probes==1, X_order==2/) )
 !
 l_RT_X_effective   = all((/ trim(probe_defs(1))=="SIN" ,X_order>=0/))
 !
 if (l_RT_X_effective) l_RT_X_inversion=all((/ X_order>=0, N_probe_frequencies==1, N_pumps==0/))
 !
 if ( .not.any((/l_RT_LRR_real_field,l_RT_SHG_qssin,l_RT_X_effective/)) ) return
 !
 ! Time Initialization
 !====================
 call RT_time_configuration_setup('CURRENT',OBS_RT_IO_t)
 !
 ! Field-Dependent actions
 !=========================
 if ( any((/l_RT_LRR_real_field,l_RT_SHG_qssin/)) ) then
   !
   call section("=",'Pump and Probe(s) databases I/O')
   !--------------------------------------------------
   ! Database
   !
   allocate(Pol(N_pumps+N_probes+1))
   allocate(Curr(N_pumps+N_probes+1))
   !
   call IO_temporary_jobdir_and_strings("ALL","SAVE")
   !
   if (N_pumps>0) then
     !
     ! Here the assumption is that
     ! - run 1 contains both the pump(s) and the probe. Probe is Field1
     ! - run 2 contains only the pump(s)
     !
     Pol(2)%title ="pump-polarization"
     Curr(2)%title="pump-current"
     if (.not.STRING_same(Pump_path,"default")) call IO_temporary_jobdir_and_strings("JOBSTR",Pump_path)
     call RT_OBSERVABLES_IO( what='JP',J=Curr(2),P=Pol(2),IO_error=IO_error)
     if (.not.allocated(Curr(2)%F_t)) call error('Pump databases missing/corrupt')
     !
     call IO_temporary_jobdir_and_strings("ALL","RESTORE")
     !
   endif
   !
   ! NB: the full run needs to be loaded as last,
   !     otherwise the wrong fields  are stored
   !
   Pol(1)%title ="polarization"
   Curr(1)%title="current"
   if (.not.STRING_same(Probe_path(1),"default")) call IO_temporary_jobdir_and_strings("JOBSTR",Probe_path(1))
   call RT_OBSERVABLES_IO( what='JP',J=Curr(1),P=Pol(1),IO_error=IO_error)
   if (.not.allocated(Curr(1)%F_t)) call error('Probe databases missing/corrupt')
   !
   call IO_temporary_jobdir_and_strings("ALL","RESTORE")
   !
   if (N_pumps>0) then
     !
     Pol(3)%title ="probe-polarization"
     Curr(3)%title="probe-current"
     Pol(3)%N=3
     Curr(3)%N=3
     YAMBO_ALLOC(Pol(3)%F_t,(RT_conf%T_n_steps,3))
     YAMBO_ALLOC(Curr(3)%F_t,(RT_conf%T_n_steps,3))
     Pol(3)%F_t =Pol(1)%F_t -Pol(2)%F_t
     Curr(3)%F_t=Curr(1)%F_t-Curr(2)%F_t
     !
   endif
   !
   !  Damping and output 
   !---------------------
   call RT_OBSERVABLES_damp_and_write(Pol(1),RT_conf,l_skip_OBS_IO)
   call RT_OBSERVABLES_damp_and_write(Curr(1),RT_conf,l_skip_OBS_IO)
   !
   if (N_pumps>0) then
     !
     call RT_OBSERVABLES_damp_and_write(Pol(2),RT_conf,l_skip_OBS_IO)
     call RT_OBSERVABLES_damp_and_write(Curr(2),RT_conf,l_skip_OBS_IO)
     !
     call RT_OBSERVABLES_damp_and_write(Pol(3),RT_conf,l_skip_OBS_IO)
     call RT_OBSERVABLES_damp_and_write(Curr(3),RT_conf,l_skip_OBS_IO)
     !
     Pol(1)%F_t =Pol(1)%F_t -Pol(2)%F_t
     Curr(1)%F_t=Curr(1)%F_t-Curr(2)%F_t
     !
   endif
   !
   if ( l_RT_pol ) then
     !
     call RT_Polarization(en,k,q) 
     !
   else if ( l_RT_LRR_real_field ) then
     !
     ! CORE Logicals
     !---------------
     ! 
     call parser('UseFFT',Use_FFT)
     !
     ! OPTICS
     !--------
     !
     call RT_X_LRR_real_field(en)
     !
   else if ( l_RT_SHG_qssin ) then
     !
     ! DEBUG>
     !call RT_X_QSSIN(en)
     ! DEBUG<
     !
   endif
   !
 else if ( l_RT_X_effective ) then
   !
   ! DEBUG>
   !call RT_X_effective(en)
   ! DEBUG<
   !
 endif
 !
 ! CLEAN
 !
 do i_o=1,N_probes+N_pumps
   call OBS_el_free(Pol(i_o))
   call OBS_el_free(Curr(i_o))
 enddo
 deallocate(Pol)
 deallocate(Curr)
 !
end subroutine
