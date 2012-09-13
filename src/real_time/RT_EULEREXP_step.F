!
! Copyright (C) 2000-2012 C. Attaccalite and the YAMBO team 
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
subroutine RT_EULEREXP_step(G_new_k,G_old_k,U1_k,U2_k,ik,deltaT)
 !
 use pars,           ONLY:SP,cI,cZERO
 use wrapper,        ONLY:M_by_M
 use SC,             ONLY:SC_bands,SC_nbands
 use real_time,      ONLY:Sigma_relax
 !
 implicit none
 !
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: G_old_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: U1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: U2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer,     intent(in)  :: ik
 !
 complex(SP) :: M_tmp(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2)) 
 !
 ! G(T+dt) = U1(dt)G(T) U2(dt) - i dt * Sigma_relax
 !
 G_new_k=cZERO
 M_tmp=cZERO
 call M_by_M('n','n',SC_nbands,U1_k,G_old_k,M_tmp)
 call M_by_M('n','n',SC_nbands,M_tmp,U2_k,G_new_k)
 !
 if(allocated(Sigma_relax))  G_new_k = G_new_k - cI*deltaT*Sigma_relax(:,:,ik)
 !
end subroutine RT_EULEREXP_step
