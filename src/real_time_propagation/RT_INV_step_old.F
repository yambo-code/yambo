!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS
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
subroutine RT_INV_step_old(dG_old,dG_inp,dG_out,ik,deltaT)
 !
 use pars,           ONLY:SP,cI
 use wrapper_omp,    ONLY:M_by_M_omp
 use linear_algebra, ONLY:INV,LIN_SYS,MAT_MUL
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver 
 use real_time,      ONLY:Ho_plus_Sigma,RT_bands,RT_nk,RT_nbands,&
&                         integrator_exp_order,G_lesser_reference,I1_matrix
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: dG_inp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 complex(SP) :: T1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: T2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: H1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: H2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: M1_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: M2_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 logical :: l_RT_low_pumping=.true.,l_use_lin_sys=.true.
 !
 ! Taylor expansion
 !
 H1_k=I1_matrix-cI*deltaT/2._SP*Ho_plus_Sigma(:,:,ik,1)
 H2_k=I1_matrix+cI*deltaT/2._SP*Ho_plus_Sigma(:,:,ik,1)
 !
 if(l_use_lin_sys) then
   !
   ! formulation as a linear system
   !
   ! X1
   !
   M1_tmp=H1_k
   M2_tmp=H2_k
   call SERIAL_lin_system(RT_nbands,RT_nbands,M2_tmp,M1_tmp)
   T1_k=M1_tmp
   !
   ! X1
   !
   M1_tmp=H1_k
   M2_tmp=H2_k
   call SERIAL_lin_system(RT_nbands,RT_nbands,M1_tmp,M2_tmp)
   T2_k=M2_tmp
   !
 else
   !
   ! formulation matrix inversion + matmul
   !
   ! X1
   !
   M1_tmp=H2_k
   call LINEAR_ALGEBRA_driver(INV,M=M1_tmp)
   call LINEAR_ALGEBRA_driver(MAT_MUL,M=H1_k,B=M1_tmp,C=T1_k)
   !
   ! X2
   !
   M2_tmp=H1_k
   call LINEAR_ALGEBRA_driver(INV,M=M2_tmp)
   call LINEAR_ALGEBRA_driver(MAT_MUL,M=H2_k,B=M2_tmp,C=T2_k)
   !
 endif
 !
 if (l_RT_low_pumping) then
   !
   ! dG(T+dt) + G0 =  T1(dt)*dG(T)*T2(dt) T1(dt)*G0(T)*T2(dt)
   ! dG(T+dt)      =  T1(dt)*dG(T)*T2(dt) T1(dt)*G0(T)*T2(dt) - G0
   !               =  T1(dt)*dG(T)*T2(dt)   +     order dG*1
   !                 +T1(dt)*G0 - G0*T2(dt) +     order G0*dt
   !                 +(T1(dt)-1)*G0*(T2(dt)+1)            order G0*dt**2
   !
   ! dG_out = G(T+dt) -G(t)
   !
   call M_by_M_omp('n','n',RT_nbands,T1_k,dG_old,M1_tmp)
   call M_by_M_omp('n','n',RT_nbands,M1_tmp,T2_k,dG_out)
   !
   call M_by_M_omp('n','n',RT_nbands,-cI*T1_k,G_lesser_reference(:,:,ik),M1_tmp)
   call M_by_M_omp('n','n',RT_nbands,G_lesser_reference(:,:,ik),-cI*T2_k,M2_tmp)
   dG_out=dG_out+M1_tmp+M2_tmp
   !
   call M_by_M_omp('n','n',RT_nbands,T1_k-cI*I1_matrix,G_lesser_reference(:,:,ik),M1_tmp)
   call M_by_M_omp('n','n',RT_nbands,M1_tmp,T2_k-cI*I1_matrix,M2_tmp)
   dG_out=dG_out+M2_tmp-dG_old
   !
 else
   !
   ! dG_out = G(T0+dt) -G(T0) = U1(T;dt) G(T0) U2(T;-dt) - G(T0)
   !
   call M_by_M_omp('n','n',RT_nbands,T1_k,dG_old+G_lesser_reference(:,:,ik),M1_tmp)
   call M_by_M_omp('n','n',RT_nbands,M1_tmp,T2_k,dG_out)
   dG_out=dG_out-dG_old-G_lesser_reference(:,:,ik)
   !
 endif
 !
end subroutine RT_INV_step_old
