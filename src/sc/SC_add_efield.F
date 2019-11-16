!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine SC_add_efield(V_bands,E,k,Pol)
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
 complex(SP),   intent(in)  :: V_bands(SC_bands(1):SC_bands(2),QP_nk,n_sp_pol)
 complex(SP),   intent(out) :: Pol(3)
 !
 ! Working space
 !
 integer :: i_sp_pol,ik
 logical :: l_induced_field=.false.
 !
 ! Calculate the Berry Polarization and update the Overlaps
 !
 Pol=cZERO
 call Berry_polarization_NEQ(E,k,V_bands,Pol)
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
 do i_sp_pol=1,n_sp_pol
   do ik=1,QP_nk
     call Build_W_operator(V_bands,E,k,A_tot,ik,i_sp_pol,W_electric(:,:,ik,i_sp_pol))
   enddo
 enddo
 !
end subroutine SC_add_efield
