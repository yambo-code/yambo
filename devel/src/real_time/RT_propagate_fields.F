!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine RT_propagate_fields(k,E,A_new,A_old,T,T_step)
 !
 ! The external and total fields in Yambo are defined and organized as follow:
 !
 ! The general vector potential is defined as
 !
 !   A(t)=-c Eo a(t) theta(t-to) [1]
 !  
 ! the function a(t) and theta(t) are defined in mod_fields. From [1] the first and
 ! second order  derivatives are easily defined:
 !
 !   A'(t)=-c Eo ( a'(t) theta(t-to)+ a(0) delta(t-to)) [2]
 !   A"(t)=-c Eo ( a"(t) theta(t-to)+ a'(0) delta(t-to) - a"(0) sign(t)) [3]
 !
 ! Using [1-3] yambo :
 !
 ! a. calculates the analytical form of A/A'/A" in RT_Ext_fields 
 ! b. uses A" (the ext current) to evolve the Maxwell equations when
 !    the induced current is included in the dynamics ( RT_propagate_fields )
 ! 
 !----------------------------------------------------------------------------
 !
 ! This routine behaves differently if the l_RT_induced_field is TRUE or FALSE
 !
 ! if FALSE the analytic expression for A(t) is obtained from RT_Ext_Fields
 ! 
 ! if TRUE the A"(t) is used to integrate th Maxwell equation. A"(t) is 
 ! written in  terms of the TOTAL current
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:RT_ind_J,l_RT_induced_field,RT_nk,integrator_step,l_RT_EULER
 use fields,         ONLY:gauge_field,A_ind,A_ind_previous,A_ext
 !
 implicit none
 !
 type(bz_samp),        intent(in) :: k
 type(levels),         intent(in) :: E
 real(SP),             intent(in) :: T,T_step
 type(gauge_field),    intent(in) :: A_old
 type(gauge_field),   intent(out) :: A_new
 !
 !
 ! External field
 !
 call RT_Ext_fields(A_ext,T)
 !
 if (l_RT_induced_field) then
   !
   ! Induced current j(t) from A(t)
   !
   ! Integrate the eq. for the vector potential
   !
   ! d^2 A_ind / d t^2 = 4 pi * c * J_ind(t)
   !
   ! A''(t+dt)  = 4 pi * j(t) * c 
   ! A'(t + dt) = A'(t) + A''(t) * dt
   ! A(t + dt)  = A(t)  + A'(t)  * dt + A''(t) * dt^2/2
   !
   A_ind%vecpot_acc =  real(RT_ind_J)*4._SP*pi*SPEED_OF_LIGHT
   !
   A_ind%vecpot_vel =  A_ind_previous%vecpot_vel + (A_ind%vecpot_acc+A_ind_previous%vecpot_acc)*T_step/2._SP
   !
   A_ind%vecpot     =  A_ind_previous%vecpot     + (A_ind%vecpot_vel+A_ind_previous%vecpot_vel)*T_step/2._SP + &
   &                                               (A_ind%vecpot_acc+A_ind_previous%vecpot_acc)*T_step**2._SP/4._SP
   !
   if(integrator_step==1) A_ind_previous  =A_ind
   !
   A_new%vecpot    =A_ext%vecpot    +A_ind%vecpot
   A_new%vecpot_vel=A_ext%vecpot_vel+A_ind%vecpot_vel
   A_new%vecpot_acc=A_ext%vecpot_acc+A_ind%vecpot_acc
   !
 else
   !
   A_new=A_ext
   !
 endif
 !
end subroutine RT_propagate_fields
