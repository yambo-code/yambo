!
! Copyright (C) 2000-2014 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_current_AND_polarization(k,E,G_input,A_input)
 !
 ! Evalute current and polarization. The current includes the diamagnetic contribution.
 !
 ! j(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 ! p(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) r \psi_j(k)
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:SPEED_OF_LIGHT
 use electrons,      ONLY:n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use SC,             ONLY:SC_bands
 use electrons,      ONLY:nel,levels
 use real_time,      ONLY:l_RT_diamagnetic,NE_dynamics,l_NE_with_fields,RT_ind_J,RT_average_operator,RT_P
 use fields,         ONLY:gauge_field
 use X_m,            ONLY:DIP_iR,DIP_P
 use QP_m,           ONLY:QP_nk
 !
 implicit none           
 type(bz_samp),     intent(in) :: k
 type(levels),      intent(in) :: E
 complex(SP),       intent(in) :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 type(gauge_field), intent(in) :: A_input
 !
 ! Work Space
 !
 integer       :: id
 !
 if(.not.l_NE_with_fields.or..not.NE_dynamics) return
 !
 do id=1,3
   !
   call RT_average_operator(VALUE=RT_ind_J(id),O_MATRIX=-DIP_P(id,1:SC_bands(2),1:SC_bands(2),1:QP_nk,1:n_sp_pol),&
&                           G=G_input,k=k,en=E,TRANS=.TRUE.)
   call RT_average_operator(VALUE=RT_P(id),    O_MATRIX=cI*DIP_iR(id,1:SC_bands(2),1:SC_bands(2),1:QP_nk,1:n_sp_pol),&
&                           G=G_input,k=k,en=E,TRANS=.TRUE.)

 enddo
 !
 ! Diamagnetic term
 !
 if(l_RT_diamagnetic) then
   !
   RT_ind_J(:)=RT_ind_J(:)-A_input%vecpot(:)*real(nel,SP)/SPEED_OF_LIGHT
   !
 endif
 !
 RT_ind_J=RT_ind_J/DL_vol
 RT_P    =RT_P/DL_vol
 !
end subroutine RT_current_AND_polarization
