!
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team 
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
subroutine RT_EULER_step(G_new_k,G_old_k,I1_k,I2_k,deltaT)
 !
 use pars,           ONLY:SP,cI
 use wrapper,        ONLY:V_by_V_plus_V
 use SC,             ONLY:SC_bands,SC_nbands
 !
 implicit none
 !
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: G_old_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: I1_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: I2_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 ! G(T+dt) = G(T)
 !
 G_new_k=G_old_k
 !
 ! G(T+dt) = G(T) - i dt *(I1(T) - I2(T))
 !
 call V_by_V_plus_V(SC_nbands, -cI*deltaT, I1_k-I2_k, G_new_k)
 !
end subroutine RT_EULER_step   

