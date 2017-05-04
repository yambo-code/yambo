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
subroutine RT_INV_step_alternative(dG_old,dG_inp,dG_out,ik,deltaT)
 !
 ! G(T+dt)-G(T)=-idt/2[H,G(T+dt)+G(T)]-idt[H,G0]
 !
 ! (1+idt*H)G(t+dt)+G(t+dt)(1-idt*H)=(1-idt*H)G(t)+G(t)(1+idt*H)-2idt[H,G0]
 !
 ! A^dag G(t+dt) + G(t+dt) A = A G(t) + G(t) A^dag
 !
 ! Diagonalize A and express the commutator as  
 ! (a) M U G(t+dt) U^dag = U G_com U^dag
 ! with M_{ij}=alpha*_i+alpha_j
 ! I can solve (a) or rotate it back in the initial space
 !
 use pars,           ONLY:SP,cI,cZERO
 use wrapper_omp,    ONLY:M_by_M_omp
 !use linear_algebra, ONLY:LIN_SYS
 !use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use pointers,       ONLY:linkM2V
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
 ! Work space
 !
 integer     :: ib1,ib2
 real(SP)    :: En(RT_bands(1):RT_bands(2))
 complex(SP) :: det_tmp
 complex(SP) :: U_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: H1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: H2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: M1_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: M2_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: G_com(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 logical     :: l_use_superindex=.true.
 !
 H1_k=-cI*2*deltaT*Ho_plus_Sigma(:,:,ik,1)
 H2_k=+cI*2*deltaT*Ho_plus_Sigma(:,:,ik,1)
 !
 call M_by_M_omp('n','n',RT_nbands,H1_k,G_lesser_reference(:,:,ik),M1_tmp)
 call M_by_M_omp('n','n',RT_nbands,G_lesser_reference(:,:,ik),H2_k,M2_tmp)
 G_com=M1_tmp+M2_tmp
 !
 H1_k=I1_matrix-cI*deltaT*Ho_plus_Sigma(:,:,ik,1)
 H2_k=I1_matrix+cI*deltaT*Ho_plus_Sigma(:,:,ik,1)
 !
 call M_by_M_omp('n','n',RT_nbands,H1_k,dG_old,M1_tmp)
 call M_by_M_omp('n','n',RT_nbands,dG_old,H2_k,M2_tmp)
 G_com=G_com+M1_tmp+M2_tmp
 !
 ! Diagonalize U_k
 !
 U_k=Ho_plus_Sigma(:,:,ik,1)
 call SERIAL_HERMITIAN_diagonalization(RT_nbands,U_k,En)
 !
 ! Implementation works, and it is much faster than before
 ! However it is not sensitive
 !U_k=cmplx(Ho_plus_Sigma(:,:,ik,1),kind=DP)
 !call ZHEEV('V','U',RT_nbands,U_k,RT_nbands,En,v_cmplx,lwork,v_real,i_fail)
 !
 ! Rotate G_com in the new basis set
 !
 call M_by_M_omp('n','n',RT_nbands,U_k   ,G_com,M1_tmp)
 call M_by_M_omp('n','c',RT_nbands,M1_tmp,U_k  ,G_com)
 !
 ! Generate the matrix  
 !
 do ib1=RT_bands(1),RT_bands(2)
   do ib2=RT_bands(1),RT_bands(2)
     G_com(ib2,ib1)=G_com(ib2,ib1)/(2._SP+cI*deltaT*real((En(ib2)-En(ib1)),SP))
   enddo
 enddo
 !
 ! Rotate G_com back the old basis set
 !
 call M_by_M_omp('c','n',RT_nbands,U_k   ,G_com,M1_tmp)
 call M_by_M_omp('n','n',RT_nbands,M1_tmp,U_k  ,dG_out)
 !
 dG_out=dG_out-dG_old
 !
end subroutine RT_INV_step_alternative
