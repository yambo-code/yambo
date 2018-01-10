!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS,AM
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
subroutine RT_Dephasing_step(dG_out,dG_in,ik,deltaT)
 !
 use pars,           ONLY:SP
 use real_time,      ONLY:RT_is_statically_dephased,RT_Deph_Matrix,RT_all2ibz,l_RT_EXP,RT_bands
 !
 implicit none
 !
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: dG_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 !
 real(SP) :: GAMMA_e,GAMMA_h,dGAMMA_e,dGAMMA_h,GAMMA_TOT,f_inf,f0
 integer  :: ib1,ib2
 !
 ! Note that dG_in is ONLY the coherent part of the equation. It must be added
 ! to dG_out if we are not using the EXP integrator. This is done here or in RT_apply_RWA
 !
 dG_out(:,:)=dG_out(:,:)+dG_in(:,:)
 !
 if (.not.RT_is_statically_dephased) return
 !
 ! dG = dG * exp( -dT i*Sigma)
 !=============================
 if (     l_RT_EXP)  dG_out(:,:) = dG_out(:,:) *  exp(-RT_Deph_Matrix(:,:,RT_all2ibz(ik))*deltaT)
 !
 ! dG = dG * (1 - dT i*Sigma)
 !============================
 if (.not.l_RT_EXP)  dG_out(:,:) = dG_out(:,:) * (1._SP - deltaT*RT_Deph_Matrix(:,:,RT_all2ibz(ik)))
 !
end subroutine RT_Dephasing_step
