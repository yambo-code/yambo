!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
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
subroutine RT_DEPHASING_step(G_new_k,ik,deltaT)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:spin_occ
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:Sigma_relax,RT_E_occupations,RT_H_occupations
 !
 implicit none
 !
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 !
 ! Work Space
 !
 integer                  :: ib
 !
 ! G = G - i dT Sigma
 !====================
 if (allocated(Sigma_relax)) G_new_k = G_new_k - cI*deltaT*Sigma_relax(:,:,ik)
 !
 ! Symmetrization 
 !================
 do ib=SC_bands(1),SC_bands(2)
   G_new_k(ib,ib)=cI*aimag(G_new_k(ib,ib))
   if( aimag(G_new_k(ib,ib))<0._SP    ) G_new_k(ib,ib)=cmplx(0._SP,0._SP    )
   if( aimag(G_new_k(ib,ib))>spin_occ ) G_new_k(ib,ib)=cmplx(0._SP,spin_occ )
   RT_E_occupations(ib,ik)=         aimag(G_new_k(ib,ib))
   RT_H_occupations(ib,ik)=spin_occ-aimag(G_new_k(ib,ib))
 enddo
 !
end subroutine RT_DEPHASING_step
