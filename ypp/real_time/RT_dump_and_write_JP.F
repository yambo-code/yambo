!
!        Copyright (C) 2000-2016 the YAMBO team
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
 use YPP,            ONLY:RT_P_t,X_rt,RT_J_t,i_t_start,i_t_end,&
&                         W_RT_induced_step,RED_steps,l_force_min_damp,&
&                         N_pumps,l_skip_pol_and_curr_IO,J_steps,J_time_step
 use com,            ONLY:msg
 use pars,           ONLY:pi,SP
 use units,          ONLY:FS2AUT,HA2EV
 use stderr,         ONLY:real2ch
 use RT_control,     ONLY:RT_output,RT_IO_JPO_steps
 use real_time,      ONLY:RT_step,RT_ind_J,RT_P,NE_steps
 use fields,         ONLY:Efield
 !
 implicit none
 !
 ! Work Space
 !
 real(SP) :: Total_time
 integer  :: i_t
 logical  :: l_no_damping
 !
 l_no_damping= trim(X_rt%damp) == "NONE"
 !
 if(l_no_damping) then
   !
   X_rt%damp_factor=0._SP
   !
   ! Force the minimal damping to make finite the Fourier Transform
   !
   if(l_force_min_damp) then
     call RT_damp_it("LORENTZIAN",W_RT_induced_step,RT_J_t(i_t_start:i_t_end,:),RED_steps,J_time_step,3)
     call RT_damp_it("LORENTZIAN",W_RT_induced_step,RT_P_t(i_t_start:i_t_end,:),RED_steps,J_time_step,6)
   endif
   !
 else
   !
   ! If there is a time-windows the damping function
   ! has be applied also the initial part otherwise 
   ! there will problems doing FFT for the non-zero starting point 
   !
   call msg('s','[RT] Optimal damping factor: '//trim(real2ch(W_RT_induced_step*HA2EV))//' eV')
   !
   call RT_damp_it(X_rt%damp,X_rt%damp_factor,RT_J_t(i_t_start:i_t_end,:),RED_steps,J_time_step,3)
   call RT_damp_it(X_rt%damp,X_rt%damp_factor,RT_P_t(i_t_start:i_t_end,:),RED_steps,J_time_step,6)
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
 do i_t=1,J_steps
   RT_ind_J    =RT_J_t(i_t,:)
   RT_P        =RT_P_t(i_t,1:3)
   call RT_output('pol',TIME=(i_t-1)*J_time_step/FS2AUT)
   if (allocated(RT_J_t)) call RT_output('cur',TIME=(i_t-1)*J_time_step/FS2AUT)
 enddo
 !
end subroutine
