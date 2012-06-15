!
! Copyright (C) 2000-2012 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_relaxation(G_input,Xen,k,q,Time)
 !
 ! This subroutine calculate relaxation term coming from off-diagonal part
 ! of the self-energy in time, from the electron-phonon coupling or simple 
 ! radiative and dephasing term in the kinetic equation 
 !
 ! Output: Sigma_relax
 !
 use pars,           ONLY:SP,cI,cZERO
 use units,          ONLY:HBAR_eVfs,FS2AUT,HA2EV
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_ph_corr
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use real_time,      ONLY:Sigma_relax,G_lesser_reference,RT_is_dumped,NE_dynamics,RT_Deph_Matrix
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(in)    :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer          :: i1,i2,ik
 !
 ! Damping
 !=========
 !
 ! Fictitious lifetime equivalent to the broadening
 ! in linear optics
 !   
 ! This term kill the induced polarization, namely off-diagonal term in the
 ! Green function
 !
 ! This term send the electron distribution into the initial one
 ! G_lesser_refence(:,:,:) 
 !
 if(.not.RT_is_dumped.and..not.l_ph_corr) return 
 !
 Sigma_relax=cZERO
 !
 if(RT_is_dumped.and.NE_dynamics) then
   !
   forall(i1=SC_bands(1):SC_bands(2),i2=SC_bands(1):SC_bands(2)) 
     Sigma_relax(i1,i2,:)=Sigma_relax(i1,i2,:)-cI*RT_Deph_Matrix(i1,i2,:)*(G_input(i1,i2,:)-G_lesser_reference(i1,i2,:))
   end forall
     !
 endif
 !
#if defined _ELPH
 !
 ! El-Ph
 !=======
 if (l_ph_corr) call RT_ELPH_Kernel(ik,Xen,k,q,Time)
 !
#endif
 !
end subroutine RT_relaxation

