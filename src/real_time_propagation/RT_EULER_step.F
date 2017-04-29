!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine RT_EULER_step(dG,I1_k,I2_k,deltaT)
 !
 ! dG(T) = G(T) - G(0)
 !
 ! dG(T+dt) - dG(T) =  G(T+dt) - G(T)
 !                  = -i dt *(I1(T) - I2(T))
 !
 use pars,           ONLY:SP,cI
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use real_time,      ONLY:RT_bands,RT_nbands
 !
 implicit none
 !
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 call V_by_V_plus_V_omp(RT_nbands, -cI*deltaT, I1_k-I2_k, dG)
 !
end subroutine RT_EULER_step
