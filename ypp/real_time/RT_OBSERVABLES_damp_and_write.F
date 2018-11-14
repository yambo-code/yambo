!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine RT_OBSERVABLES_damp_and_write(OBS,RT,IO_skip)
 !
 use YPP_real_time,  ONLY:l_force_min_damp,RT_time,OBS_el,RT_ctl
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
 logical  :: l_no_damping
 !
 l_no_damping= trim(RT%damping) == "NONE"
 !
 if (l_no_damping) then
   !
   RT%damp_factor=0._SP
   !
   ! Force the minimal damping to make finite the Fourier Transform
   !
   if(l_force_min_damp) then
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
   call RT_damp_it(RT%damping,RT%damp_factor,OBS%F_t,OBS%N)
   !
 endif
 !
 if (IO_skip) return
 !
 call section("s",'[RT] '//trim(OBS%title)//' dumped on file')
 !=============================================================
 !
 select case (trim(OBS%title))
   !
   case("Polarization") 
     call RT_output(what="open pol")
     do i_t=1,RT%T_n_steps
       RT_P=OBS%F_t(i_t,1:3)
       call RT_output('pol',TIME=RT_time(i_t)*AUT2FS)
     enddo
   case("Current") 
     call RT_output(what="open curr")
     do i_t=1,RT%T_n_steps
       RT_ind_J=OBS%F_t(i_t,1:3)
       call RT_output('curr',TIME=RT_time(i_t)*AUT2FS)
     enddo
     !
 end select
 !
 call RT_output(what="close pol curr")
 !
end subroutine
