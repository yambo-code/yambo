!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine RT_build_T1_and_T2(ik,T1_k,T2_k,I1_matrix,dT)
 !
 ! dG(T) = G(T) - G(0)
 !
 ! T1 = (1-iHdt/2) / (1+iHdt/2)-1
 ! T2 = (1+iHdt/2) / (1-iHdt/2)-1
 !
 use pars,           ONLY:SP,cI
 use wrapper_omp,    ONLY:V_by_V_plus_V_omp
 use linear_algebra, ONLY:INV,LIN_SYS,MAT_MUL
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver 
 use real_time,      ONLY:RT_bands,RT_nbands,Ho_plus_Sigma
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: dT
 complex(SP), intent(in)  :: I1_matrix(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: T1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: T2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 complex(SP)  :: H1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: H2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 complex(SP)  :: X1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: X2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 logical      :: l_use_lin_sys=.true.
 !
end subroutine RT_build_T1_and_T2
