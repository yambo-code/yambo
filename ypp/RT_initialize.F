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
subroutine RT_initialize(dump_and_write)
 !
 use YPP,            ONLY:RT_P_t,X_rt,RT_J_t,i_t_start,i_t_end,&
&                         W_RT_induced_step,RED_steps,l_force_min_damp,&
&                         N_pumps,l_skip_pol_and_curr_IO
 use com,            ONLY:msg
 use pars,           ONLY:pi,SP
 use units,          ONLY:FS2AUT,HA2EV
 use stderr,         ONLY:real2ch
 use real_time,      ONLY:NE_steps,RT_step,RT_ind_J,RT_output,RT_P,RT_P_2levels
 use fields,         ONLY:Efield,i_Probe
 use parser_m,       ONLY:parser
 implicit none
 !
 logical, optional  :: dump_and_write
 !
 ! Work Space
 !
 integer  :: i_t,i_t_field_range(2)
 logical  :: l_no_damping
 !
 l_no_damping= trim(X_rt%damp) == "NONE"
 !
 !  Reduced Time range
 !====================
 !
 i_t_start=int(X_rt%time_range(1)/RT_step)+1
 i_t_end  =int(X_rt%time_range(2)/RT_step)
 !
 i_t_field_range= (/int(Efield(i_Probe)%t_initial/RT_step)+1,NE_steps/)
 !
 if (N_pumps>0.and.trim(Efield(i_Probe)%ef_name)=='DELTA') i_t_start=i_t_field_range(1)
 !
 if (i_t_start<=0.or.i_t_start> i_t_field_range(2)) i_t_start=i_t_field_range(1)
 if (i_t_end<=0  .or.i_t_end>   i_t_field_range(2)) i_t_end  =i_t_field_range(2)
 !
 X_rt%time_range=(/i_t_start-1,i_t_end/)*RT_step
 !
 RED_steps=i_t_end-i_t_start+1
 W_RT_induced_step=2.*pi/(real(RED_steps,SP)*RT_step)  ! Frequency step in real-time dynamics
 !
 call msg('s','[RT] Time range [fs]:',(/real(i_t_start),real(i_t_end)/)*RT_step/FS2AUT)
 !
 if (present(dump_and_write)) then
   if (.not.dump_and_write) return
 endif
 !
 ! DAMPING
 !=========
 !
 if(l_no_damping) then
   !
   X_rt%damp_factor=0._SP
   !
   ! Force the minimal damping to make finite the Fourier Transform
   !
   if(l_force_min_damp) then
     call RT_damp_it("LORENTZIAN",W_RT_induced_step,RT_J_t(i_t_start:i_t_end,:),RED_steps,RT_step,3)
     call RT_damp_it("LORENTZIAN",W_RT_induced_step,RT_P_t(i_t_start:i_t_end,:),RED_steps,RT_step,6)
   endif
   !
 else
   !
   ! If there is a time-windows the damping function
   ! has be applied also the initial part otherwise 
   ! there will problems doing FFT for the non-zero starting point 
   !
   call msg('r','[RT] Optimal damping factor: '//trim(real2ch(W_RT_induced_step*HA2EV))//' eV')
   !
   call RT_damp_it(X_rt%damp,X_rt%damp_factor,RT_J_t(i_t_start:i_t_end,:),RED_steps,RT_step,3)
   call RT_damp_it(X_rt%damp,X_rt%damp_factor,RT_P_t(i_t_start:i_t_end,:),RED_steps,RT_step,6)
   !
 endif
 !
 if (l_skip_pol_and_curr_IO) return
 !
 call section("=",'Writing input current and Polarization')
 !=========================================================
 call RT_output("open cur pol")
 !
 do i_t=1,NE_steps
   RT_ind_J    =RT_J_t(i_t,:)
   RT_P        =RT_P_t(i_t,1:3)
   RT_P_2levels=RT_P_t(i_t,4:6)
   call RT_output('pol',TIME=(i_t-1)*RT_step/FS2AUT)
   if (allocated(RT_J_t)) call RT_output('cur',TIME=(i_t-1)*RT_step/FS2AUT)
 enddo
 !
 call RT_output("close cur pol")
 !
end subroutine
