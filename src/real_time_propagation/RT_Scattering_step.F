!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_Scattering_step(dG,ik,deltaT)
 !
 use pars,           ONLY:SP,cI
 use real_time,      ONLY:RT_bands,I_relax_static,RT_is_dynamically_dephased
 !
 implicit none
 !
 complex(SP), intent(out) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 !
 integer  :: ib
 !
 ! dG = dG - i dT Sigma
 !=======================
 if( .not. RT_is_dynamically_dephased) return
 !
 forall(ib=RT_bands(1):RT_bands(2))  dG(ib,ib) = dG(ib,ib) - cI*deltaT*I_relax_static(ib,ib,ik)
 !
 !if (l_RT_EXP.and.l_RT_RWA) then
 !  !
 !  do ib=RT_bands(1),RT_bands(2)
 !    !
 !    GAMMA_h=REF_lifetime(ib,ik,1)+REF_lifetime(ib,ik,3)+REF_lifetime(ib,ik,5)
 !    GAMMA_e=REF_lifetime(ib,ik,2)+REF_lifetime(ib,ik,4)+REF_lifetime(ib,ik,6)
 !    dGAMMA_e=0._SP
 !    dGAMMA_h=0._SP
 !    !
 !    if (l_elel_scatt) then
 !      dGAMMA_h=dGAMMA_h+E_E_Hole_lifetime(ib,ik)
 !      dGAMMA_e=dGAMMA_e+E_E_Electron_lifetime(ib,ik)
 !    endif
 !    !
 !    if (l_RT_CCA_Kernel.and.l_elph_scatt) then
 !      dGAMMA_h=dGAMMA_h+E_P_Hole_lifetime(ib,ik)    +E_P_Ho_abs_lifetime(ib,ik)
 !      dGAMMA_e=dGAMMA_e+E_P_Electron_lifetime(ib,ik)+E_P_El_abs_lifetime(ib,ik)
 !    endif
 !    !
 !    GAMMA_e=GAMMA_e+dGAMMA_e
 !    GAMMA_h=GAMMA_h+dGAMMA_h
 !    GAMMA_TOT=GAMMA_e+GAMMA_h+dGAMMA_e+dGAMMA_h
 !    !
 !    f0=aimag(G_lesser_reference(ib,ib,ik))
 !    !
 !    if(       (GAMMA_h+GAMMA_e)>0._SP  ) f_inf=GAMMA_h/(GAMMA_h+GAMMA_e)*spin_occ
 !    if(.not. ((GAMMA_h+GAMMA_e)>0._SP) ) f_inf=f0
 !    !
 !    dG(ib,ib)=dG(ib,ib)*exp(-GAMMA_TOT*deltaT)+cI*(f0-f_inf)*(exp(-GAMMA_TOT*deltaT)-1._SP)
 !    !
 !  enddo
 !  !
 !endif
 !
end subroutine RT_Scattering_step
