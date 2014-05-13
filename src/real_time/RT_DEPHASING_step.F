!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_DEPHASING_step(G_new_k,ik,deltaT)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:spin_occ
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:Sigma_relax
 !
 implicit none
 !
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 !
 !
 ! dG = dG - i dT Sigma
 !====================
 if (allocated(Sigma_relax)) G_new_k = G_new_k - cI*deltaT*Sigma_relax(:,:,ik)
 !
end subroutine RT_DEPHASING_step
