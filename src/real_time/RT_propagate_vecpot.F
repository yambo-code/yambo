!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_propagate_vecpot(A_new,A_old,T_step,RT_time)
 !
 ! This routine integrates the EOM for A(t). Note that
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use real_time,      ONLY:j_current,gauge_field,Ext_fields_initial_time,&
&                         l_RT_induced_field,j_current_ext
 implicit none
 !
 real(SP),             intent(in) :: T_step,RT_time
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field), intent(inout) :: A_new
 ! 
 ! Integrate the eq. for the vector potential
 !
 ! d^2 A / d t^2 = 4 pi J(t)  = 4 pi ( J_ext(t) + J_induced(t) )
 !
 ! using 
 !
 ! A''(t+dt)  = 4 pi * j(t) / c 
 ! A'(t + dt) = A'(t) + A''(t) * dt
 ! A(t + dt)  = A(t)  + A'(t)  * dt + A''(t) * dt^2/2
 !
 if(RT_time<Ext_fields_initial_time) return
 !
 A_new%vecpot_acc =  j_current_ext*4._SP*pi*SPEED_OF_LIGHT
 !
 ! if l_RT_induced_field the induced field is included in Maxweel equations
 !
 if(l_RT_induced_field) then 
   A_new%vecpot_acc = A_new%vecpot_acc + j_current*4._SP*pi*SPEED_OF_LIGHT
 endif
 !
 A_new%vecpot_vel =  A_old%vecpot_vel + (A_new%vecpot_acc+A_old%vecpot_acc)*T_step/2._SP
 A_new%vecpot     =  A_old%vecpot     + A_new%vecpot_vel*T_step
 !
end subroutine RT_propagate_vecpot

