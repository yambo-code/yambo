!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
subroutine RT_OBSERVABLES_damp_and_write(OBS,RT,IO_skip)
 !
 use YPP_real_time,  ONLY:l_force_min_damp,RT_time,OBS_el,RT_ctl
 use stderr,         ONLY:STRING_match
 use pars,           ONLY:pi,SP
 use units,          ONLY:AUT2FS
 use RT_control,     ONLY:RT_output
 use real_time,      ONLY:RT_ind_J,RT_P
 !
#include<memory.h>
 !
 type(OBS_el) :: OBS
 type(RT_ctl) :: RT
 logical      :: IO_skip
 !
 ! Work Space
 !
 integer  :: i_t
 real(SP) :: local_damping
 logical  :: l_no_damping
 !
 l_no_damping= trim(RT%damping) == "NONE"
 !
 local_damping=RT%damp_factor
 !
 if (l_no_damping) then
   !
   local_damping=0._SP
   !
   ! Force the minimal damping to make finite the Fourier Transform
   !
   if (l_force_min_damp) then
     call section("s",'[RT] Minimum damping applied to '//trim(OBS%title))
     call RT_damp_it("LORENTZIAN",RT%Field_damp_factor,OBS%F_t,OBS%N)
   endif
   !
 else
   !
   ! If there is a time-windows the damping function
   ! has be applied also the initial part otherwise 
   ! there will problems doing FFT for the non-zero starting point 
   !
   call RT_damp_it(RT%damping,local_damping,OBS%F_t,OBS%N)
   !
 endif
 !
 if (IO_skip) return
 !
 call section("s",'[RT] '//trim(OBS%title)//' dumped on file')
 !=============================================================
 call RT_output(what="open "//trim(OBS%title))
 do i_t=1,RT%T_n_steps
   if (STRING_match(OBS%title,"polarization")) RT_P=OBS%F_t(i_t,1:3)
   if (STRING_match(OBS%title,"current")     ) RT_ind_J=OBS%F_t(i_t,1:3)
   call RT_output(trim(OBS%title),TIME=RT_time(i_t)*AUT2FS)
 enddo
 call RT_output(what="close "//trim(OBS%title))
 !
end subroutine
