!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_update_TIME_CONF_descriptions( )
 ! 
 use units,          ONLY:AUT2FS
 use RT_lifetimes,   ONLY:RT_life_extrap_times
 use real_time,      ONLY:NE_time,RT_dyn_step,RT_is_dynamically_dephased,NE_time_step_update_jump
 use RT_control,     ONLY:CARR_RT_IO_t,OBS_RT_IO_t,OUTPUT_RT_IO_t,Gless_RESTART_RT_IO_t,CACHE_OBS_INTERVAL_time
 use stderr,         ONLY:real2ch
 use RT_output_m,    ONLY:TIME_STEP_desc
 use descriptors,    ONLY:IO_desc_add
 !
 implicit none
 !
 ! Work Space
 !
 character(11) :: ch
 !
 ch=" [T="//trim(real2ch(NE_time*AUT2FS))
 !
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] Time step",R=(/RT_dyn_step*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] Update time",R=(/NE_time_step_update_jump*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O cache interval(J-P-M)",R=(/CACHE_OBS_INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O interval(J-P-M)",R=(/OBS_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O interval(Carr)",R=(/CARR_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O interval(OUT)",R=(/OUTPUT_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 call IO_desc_add(TIME_STEP_desc,kind='r',str=ch//"] I/O interval(GF)",R=(/Gless_RESTART_RT_IO_t%INTERVAL_time*AUT2FS/),term="fs")
 !
 if (RT_is_dynamically_dephased) then
   call IO_desc_add(TIME_STEP_desc,SZ=2,kind='r',str=ch//"] Life Inter-Intra",R=RT_life_extrap_times*AUT2FS,term="fs")
 endif
 !
end subroutine RT_update_TIME_CONF_descriptions
