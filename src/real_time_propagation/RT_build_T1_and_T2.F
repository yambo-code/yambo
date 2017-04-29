!
!        Copyright (C) 2000-2017 the YAMBO team
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
 ! dG(T+dt) - dG(T) =  G(T+dt) - G(T)
 !                  =  T1(dt)*dG(T)*T1(dt) +              order dG*1
 !                   +(T1(dt)-1)*G0 + G0*(T2(dt)-1) +     order G0*dt
 !                   +(T1(dt)-1)*G0*(T2(dt)-1)            order G0*dt**2
 !                   -dG(T)                               remove dG(T)
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
 H1_k=I1_matrix-cI*dT/2._SP*Ho_plus_Sigma(:,:,ik,1)
 H2_k=I1_matrix+cI*dT/2._SP*Ho_plus_Sigma(:,:,ik,1)
 !
 if(l_use_lin_sys) then
   !
   ! formulation as a linear system
   !
   ! X1
   !
   X1_k=H1_k
   call LINEAR_ALGEBRA_driver(LIN_SYS,M=H2_k,B=X1_k)
   !
   ! X1
   !
   X2_k=H2_k
   call LINEAR_ALGEBRA_driver(LIN_SYS,M=H1_k,B=X2_k)
   !
 else
   !
   ! formulation matrix inversion + matmul
   !
   ! X1
   !
   T1_k=H2_k
   call LINEAR_ALGEBRA_driver(INV,M=T1_k)
   call LINEAR_ALGEBRA_driver(MAT_MUL,M=H1_k,B=T1_k,C=X1_k)
   !
   ! X2
   !
   T2_k=H1_k
   call LINEAR_ALGEBRA_driver(INV,M=T2_k)
   call LINEAR_ALGEBRA_driver(MAT_MUL,M=H2_k,B=T2_k,C=X2_k)
   !
 endif
 !
 T1_k=X1_k-I1_matrix
 T2_k=X2_k-I1_matrix
 !
end subroutine RT_build_T1_and_T2
