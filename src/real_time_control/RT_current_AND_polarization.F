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
&                         RT_P,RT_magn,RT_P_prev,RT_nk,G_lesser_reference,NE_i_time
 use fields,         ONLY:gauge_field
 use DIPOLES,        ONLY:DIP_iR,DIP_P,DIP_spin,DIP_orbital
 use X_m,            ONLY:global_gauge
 use parallel_m,     ONLY:PAR_DIPk_nibz
 use RT_interfaces,  ONLY:RT_average_operator
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
 !complex(SP)   :: RT_P_tmp(3)
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
 call RT_average_operator(V_VALUE=RT_ind_J, V_MATRIX=-DIP_P,G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
 !
 ! Velocity gauge I must use the correct definition of the J operator
 !
 ! *warning* DS: since this is the current at time t, I should use the vecpot at time t
 !               However I still do not have it here (it will be computed from the current in the velocity gauge)
 !
 if(trim(global_gauge)=='velocity') RT_ind_J(:)=RT_ind_J(:)+A_input%vecpot(:)*real(nel,SP)/(2._SP*SPEED_OF_LIGHT)
 !
 RT_ind_J=RT_ind_J/DL_vol
 !
 ! B : Polarization
 !
 call RT_average_operator(V_VALUE=RT_P, V_MATRIX=cI*DIP_iR,G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
 !
 RT_P    =RT_P/DL_vol
 !
 ! GPL_EXCLUDE_START
 !
 ! B': Polarization as an integral of the current (to be fixed, it is not stable anyway)
 !
 !if(integrator_step==1) RT_P_tmp=RT_P_prev+RT_ind_J_prev*RT_dyn_step
 !if(integrator_step==2) RT_P_tmp=RT_P_prev+RT_ind_J     *RT_dyn_step/2._SP
 !
 !RT_P=RT_P_tmp
 !
 if(n_spin>1) then
   !
   ! C : Spin magnetization
   !
   call RT_average_operator(V_VALUE=RT_M_tmp,PV_MATRIX=DIP_spin,G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
   RT_magn(:,1)=RT_M_tmp
   !
   if(NE_i_time==1) then
     call RT_average_operator(V_VALUE=RT_M_tmp,PV_MATRIX=DIP_spin,G=G_lesser_reference,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
     Spin_magn=real(RT_M_tmp,SP)
   endif
   !
   ! D : Orbital magentization (local and itinerant contributions)
   !
   call RT_average_operator(V_VALUE=RT_M_tmp,PV_MATRIX=DIP_orbital(:,:,:,:,:,1),&
&                            G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
   RT_magn(:,2)=RT_M_tmp/(2._SP*SPEED_OF_LIGHT)
   !
   if(NE_i_time==1) then
     call RT_average_operator(V_VALUE=RT_M_tmp,PV_MATRIX=DIP_orbital(:,:,:,:,:,1),&
&                              G=G_lesser_reference,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
     Orbital_magn=real(RT_M_tmp,SP)/(2._SP*SPEED_OF_LIGHT)
   endif
   !
   call RT_average_operator(V_VALUE=RT_M_tmp,PV_MATRIX=DIP_orbital(:,:,:,:,:,2),&
&                            G=G_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
   RT_magn(:,3)=RT_M_tmp/(2._SP*SPEED_OF_LIGHT)
   ! 
   if(NE_i_time==1) then
     call RT_average_operator(V_VALUE=RT_M_tmp,PV_MATRIX=DIP_orbital(:,:,:,:,:,2),&
&                              G=G_lesser_reference,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_DIPk_nibz)
     Itinerant_magn=real(RT_M_tmp,SP)/(2._SP*SPEED_OF_LIGHT)
   endif
   ! 
 endif
 !
 ! GPL_EXCLUDE_END
 !
end subroutine RT_current_AND_polarization
