!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
subroutine DIP_polarization(Xen,Xk,V_bands,Polarization)
 !
 use pars,           ONLY:SP,cI,cZERO
 use electrons,      ONLY:n_sp_pol,levels
 use D_lattice,      ONLY:a,DL_vol
 use R_lattice,      ONLY:bz_samp,k_map,nXkbz
 use QP_m,           ONLY:QP_nk
 use nl_optics,      ONLY:DIP_bz,NL_bands
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_Xk_bz,PAR_COM_Xk_ibz_INDEX
 !
 implicit none           
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 complex(SP),   intent(in)  :: V_bands(NL_bands(2),Xen%nbf,QP_nk,n_sp_pol)
 complex(SP),   intent(out) :: Polarization(3)
 !
 ! Work Space
 !
 integer       :: id,is,ik_bz
 complex(SP)   :: Polarization_red(3)
 !
 ! Evaluate polarization in terms of dipoles only
 ! for testing porpouses
 !
 ! p(r,t) = \sum_ijk e/(2m) \psi^*_i(k) r \psi_j(k)
 !
 do id=1,3
     call NL_average_operator(Xen,Xk,V_bands, &
 &      cI*DIP_bz(id,1:NL_bands(2),1:NL_bands(2),1:nXkbz,1:n_sp_pol),.true.,Polarization(id))  
 enddo
 !
 Polarization=Polarization/DL_Vol
 !
end subroutine DIP_polarization
