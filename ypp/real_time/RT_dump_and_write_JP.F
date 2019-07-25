!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_dump_and_write_JP()
 !
 use YPP_real_time,  ONLY:RT_P_t,RT_conf,RT_J_t,l_force_min_damp,l_skip_pol_and_curr_IO,RT_time
 use com,            ONLY:msg
 use pars,           ONLY:pi,SP
 use units,          ONLY:AUT2FS
 use RT_control,     ONLY:RT_output
 use real_time,      ONLY:RT_ind_J,RT_P
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: i_t
 logical  :: l_no_damping
 !
 l_no_damping= trim(RT_conf%damping) == "NONE"
 !
 if (l_no_damping) then
   !
   RT_conf%damp_factor=0._SP
   !
   call msg('s','[RT] Applied Field derived damping')
   !
   ! Force the minimal damping to make finite the Fourier Transform
   !
   if(l_force_min_damp) then
     call RT_damp_it("LORENTZIAN",RT_conf%Field_damp_factor,RT_J_t,3)
     call RT_damp_it("LORENTZIAN",RT_conf%Field_damp_factor,RT_P_t,6)
   endif
   !
 else
   !
   ! If there is a time-windows the damping function
   ! has be applied also the initial part otherwise 
   ! there will problems doing FFT for the non-zero starting point 
   !
   call RT_damp_it(RT_conf%damping,RT_conf%damp_factor,RT_J_t,3)
   call RT_damp_it(RT_conf%damping,RT_conf%damp_factor,RT_P_t,6)
   !
 endif
 !
 if (l_skip_pol_and_curr_IO) return
 !
 call section("=",'Writing input current and Polarization')
 !=========================================================
 call RT_output(what="initialize")
 call RT_output(what="open cur pol")
 !
 do i_t=1,RT_conf%T_n_steps
   RT_ind_J    =RT_J_t(i_t,:)
   RT_P        =RT_P_t(i_t,1:3)
   call RT_output('pol',TIME=RT_time(i_t)*AUT2FS)
   if (allocated(RT_J_t)) call RT_output('cur',TIME=RT_time(i_t)*AUT2FS)
 enddo
 !
end subroutine
