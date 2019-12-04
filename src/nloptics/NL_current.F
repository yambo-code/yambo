!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine NL_current(k,E,V_input,NL_J)
 !
 ! Evalute current 
 ! The current includes the diamagnetic contribution only in the velocity gauge.
 !
 ! j(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) p \psi_j(k) - e^2/(cm) n(r) A(r,t)
 ! p(r,t) = -\sum_ijk e/(2m) \psi^*_i(k) r \psi_j(k)
 !
 use pars,           ONLY:SP,cZERO
 use units,          ONLY:SPEED_OF_LIGHT
 use R_lattice,      ONLY:bz_samp,k_map
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:nel,levels,n_sp_pol
 use fields,         ONLY:A_ext
 use X_m,            ONLY:global_gauge
 use DIPOLES,        ONLY:DIP_v
 use nl_optics,      ONLY:NL_bands,l_use_DIPOLES
 use QP_m,           ONLY:QP_nk
 use NL_interfaces,  ONLY:NL_average_operator
 use parallel_m,     ONLY:PAR_Xk_nibz
 !
 implicit none          
 ! 
 type(bz_samp),     intent(in)  :: k
 type(levels),      intent(in)  :: E
 complex(SP),       intent(in)  :: V_input(NL_bands(2),E%nbf,QP_nk,n_sp_pol) 
 complex(SP),       intent(out) :: NL_J(6)
 !
 ! Work Space
 !
 NL_J=cZERO
 !
 ! A : Current
 !
 call NL_average_operator(V_VALUE=NL_J(4:6), V_MATRIX=DIP_v(:,NL_bands(1):NL_bands(2),NL_bands(1):NL_bands(2),:,:),  &
     &  V_bands=V_input,k=k,en=E,TRANS=.TRUE.,nK_mem=PAR_Xk_nibz)
 !
 if(trim(global_gauge)=='velocity') then
   !
   ! Velocity gauge I must use the correct definition of the J operator
   !
   NL_J(4:6)=NL_J(4:6)+A_ext%vecpot(:)*real(nel,SP)/(2._SP*SPEED_OF_LIGHT)
   !
   NL_J(1:3)=NL_J(4:6)
   !
 else
   !
   if(l_use_DIPOLES) then
     NL_J(1:3)=NL_J(4:6)
   else
     call NL_Berry_current(E,k,k_map,V_input,NL_J(1:3))
   endif
   !
 endif
 !
 NL_J=NL_J/DL_vol
 !
end subroutine NL_current
