!
! Copyright (C) 2000-2010 C. Attaccalite, A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
! Partialy inspired from Octopus v. 3.1.0
!
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
subroutine RT_init_optics()
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,RT_J_t,i_t_start,i_t_end,A_vecpot_vel_t
 use com,            ONLY:warning
 use pars,           ONLY:pi,SP
 use units,          ONLY:FS2AUT,HARTREE
 use stderr,         ONLY:real2ch
 use real_time,      ONLY:NE_steps,RT_step,RT_ind_J,RT_output,RT_P,RT_output
 implicit none
 !
 ! Work Space
 !
 integer  :: i_t
 real(SP) :: W_RT_induced_step
 !
 call section("=",'Current and Polarization')
 !=========================          
 !
 !  Reduced Time range
 !====================
 !
 i_t_start=int(X_rt%time_range(1)/RT_step)
 i_t_end  =int(X_rt%time_range(2)/RT_step)
 !
 if(i_t_start<=0.or.i_t_start>NE_steps) i_t_start=1
 if(i_t_end<=0.or.i_t_end>NE_steps)     i_t_end  =NE_steps
 !
 W_RT_induced_step=2.*pi/(real(NE_steps,SP)*RT_step)  ! Frequency step in real-time dynamics
 ! 
 ! DAMPING
 !=========
 !
 ! If there is a time-windows the damping function
 ! has be applied also the initial part otherwise 
 ! there will problems doing FFT for the non-zero starting point 
 !
 if(X_rt%damp_factor<W_RT_induced_step) then
   X_rt%damp_factor=W_RT_induced_step
   call warning('Damping factor redefined to be '//trim(real2ch(W_RT_induced_step*HARTREE))//' eV')
 endif
 ! 
 ! apply the damping
 !===================
 !
 call apply_damping(X_rt%damp,X_rt%damp_factor,RT_J_t,NE_steps,RT_step,3)
 call apply_damping(X_rt%damp,X_rt%damp_factor,RT_P_t,NE_steps,RT_step,3)
 call apply_damping(X_rt%damp,X_rt%damp_factor,A_vecpot_vel_t,NE_steps,RT_step,3)
 !
 ! Print the dumped current and polarization 
 !-------------------------------------------
 call RT_output("open cur pol")
 !
 do i_t=1,NE_steps
   RT_ind_J=RT_J_t(i_t,:)
   RT_P    =RT_P_t(i_t,:)
   call RT_output('cur pol',TIME_or_E=(i_t-1)*RT_step/FS2AUT)
 enddo
 !
 call RT_output("close cur pol")
 !
end subroutine
