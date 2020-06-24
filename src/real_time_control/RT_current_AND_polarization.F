!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine RT_current_AND_polarization(k,E,G_input,A_input)
 !
 ! Evalute current and polarization.
 ! The current includes the diamagnetic contribution only in the velocity gauge.
 !
 ! j(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 ! p(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) r \psi_j(k)
 !
 ! 2017/06/23 DS WARNING
 !  Running TD-IP simulations this subroutine becomes one of the most time
 !  consuming one because the dipoles are defined in the IBZ and are expanded at
 !  each time step. When devel-dipoles will be finally merged inside the master
 !  this will be avoided by expanding the dipoles once for all
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:SPEED_OF_LIGHT
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:nel,levels,n_spin,Spin_magn,Orbital_magn,Itinerant_magn
 use real_time,      ONLY:l_NE_dynamics,RT_ind_J,RT_ind_J_prev,RT_bands,&
&                         RT_P,RT_magn,RT_P_prev,RT_nk,G_lesser_reference,NE_i_time,&
&                         RT_dyn_step,l_P_integrating_J,l_velocity_gauge_diam,integrator_step
 use fields,         ONLY:gauge_field
 use DIPOLES,        ONLY:DIP_iR,DIP_v,DIP_spin,DIP_orbital
 use parallel_m,     ONLY:PAR_DIPk_nibz
 use RT_interfaces,  ONLY:RT_average_operator
 use RT_control,     ONLY:RT_do_it
 !
 implicit none
 !
 type(bz_samp),     intent(in) :: k
 type(levels),      intent(in) :: E
 complex(SP),       intent(in) :: G_input(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk)
 type(gauge_field), intent(in) :: A_input
 !
 ! Work Space
 !
 complex(SP)   :: RT_M_tmp(3)
 !
 if ( .not. l_NE_dynamics ) return
 !
 ! Store the current and the polarization at the previous time-step
 !
 RT_ind_J_prev=RT_ind_J
 RT_P_prev    =RT_P
 ! 
 ! A : Current
 !
 call RT_average_operator(N_V=3,V_VALUE=RT_ind_J, V_MATRIX=-DIP_v,G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
 !
 ! Velocity gauge I must use the correct definition of the J operator
 !
 ! *warning* DS: The present comment holds for when RT_current is called by RT_propagate fields
 !               this is the current at time t, I should use the vecpot at time t
 !               However I still do not have it here when the induced field is included
 !               (it will be computed from the current in the velocity gauge)
 !               The proper value is set in RT_propagate_fields
 !
 if(l_velocity_gauge_diam) RT_ind_J(:)=RT_ind_J(:)+A_input%vecpot(:)*real(nel,SP)/(2._SP*SPEED_OF_LIGHT)
 !
 RT_ind_J=RT_ind_J/DL_vol
 !
 if (.not.l_P_integrating_J) then
   !
   ! B : Polarization in terms of the off-diagonal dipoles x_ij.
   !     This expression is exact up to linear order in the field.
   !
   call RT_average_operator(N_V=3,V_VALUE=RT_P, V_MATRIX=cI*DIP_iR,G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
   !
   RT_P    =RT_P/DL_vol
   !
 else
   !
   ! B': Polarization as an integral of the current.
   !     This expression is exact to all orders in the field.
   !     (to be fixed, it is not stable ...)
   !
   if(integrator_step==1) RT_P=RT_P_prev+RT_ind_J_prev*RT_dyn_step/2._SP
   if(integrator_step==2) RT_P=RT_P_prev+RT_ind_J     *RT_dyn_step/2._SP
   !
 endif
 !
 !
end subroutine RT_current_AND_polarization
