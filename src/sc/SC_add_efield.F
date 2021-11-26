!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA MG
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
! This subroutine fill the W_electric matrix 
!
subroutine SC_add_efield(E,k,i_sp_pol,V_bands,Pol)
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use fields,         ONLY:A_tot,A_ext,Efield
 use R_lattice,      ONLY:bz_samp
 use QP_m,           ONLY:QP_nk
 use electrons,      ONLY:n_sp_pol,levels
 use electric,       ONLY:W_electric
 use SC,             ONLY:SC_bands
 !
 implicit none
 !
 type(bz_samp), intent(in)  :: k
 type(levels),  intent(in)  :: E
 integer,       intent(in)  :: i_sp_pol
 complex(SP),   intent(in)  :: V_bands(SC_bands(1):SC_bands(2),E%nbf(i_sp_pol),QP_nk)
 complex(SP),   intent(out) :: Pol(3)
 !
 ! Working space
 !
 integer :: ik
 logical :: l_induced_field=.false.
 !
 ! Calculate the Berry Polarization and update the Overlaps
 !
 if (i_sp_pol==1) Pol=cZERO
 !
 call Berry_polarization_NEQ(E,k,i_sp_pol,V_bands,Pol)
 !
 if( Efield(1)%ef_name=="NONE") return
 !
 ! External Field
 !
 A_ext%vecpot_vel=-SPEED_OF_LIGHT*Efield(1)%amplitude*Efield(1)%versor
 !
 A_tot%vecpot_vel=A_ext%vecpot_vel
 if (l_induced_field) A_tot%vecpot_vel=A_tot%vecpot_vel+Pol
 !
 ! Calculated in the WFo basis
 !
 W_electric=cZERO
 !
 do ik=1,QP_nk
   call Build_W_operator(E,k,A_tot,ik,i_sp_pol,V_bands,W_electric(:,:,ik,i_sp_pol))
 enddo
 !
end subroutine SC_add_efield
