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
subroutine RT_current(Xk,G_input,A_input)
 !
 ! Evalute current including also the diamagnetic contribution
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT,FS2AUT
 use R_lattice,      ONLY:bz_samp,RL_vol
 use SC,             ONLY:SC_bands
 use electrons,      ONLY:nel
 use real_time,      ONLY:l_RT_no_diamagnetic,i_time,l_NE_with_fields,j_current, &
&                         gauge_field,RT_average_operator,RT_step
 use X_m,            ONLY:DIP_iR_or_P
 use QP_m,           ONLY:QP_nk
 !
 implicit none           
 type(bz_samp),     intent(in) :: Xk
 complex(SP),       intent(in) :: G_input(SC_bands,SC_bands,QP_nk)
 type(gauge_field), intent(in) :: A_input
 !
 ! Work Space
 !
 integer       :: id
 !
 ! j(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 !
 ! j_current(t) = int dr j(r,t)
 !
 if(.not.l_NE_with_fields.or.i_time<0) return
 !
 do id=1,3
   !
   call RT_average_operator(VALUE=j_current(id),O_MATRIX=DIP_iR_or_P(id,:,:,:,:),G=G_input,k=Xk)
   !
 enddo
!DEBUG (TEST RT) >
!write (100,*) i_time*RT_step/FS2AUT,real(j_current(2)),aimag(j_current(2))
!DEBUG (TEST RT) <
 !
 ! Diamagnetic term
 !
 if(.not.l_RT_no_diamagnetic) then
   !
   j_current(:)=j_current(:)-A_input%vecpot(:)*real(nel,SP)/SPEED_OF_LIGHT
   !
 endif
 !
 j_current=j_current*RL_vol/(2._SP*pi)**3
 !
end subroutine RT_current
