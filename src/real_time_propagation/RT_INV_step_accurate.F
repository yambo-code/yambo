!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_INV_step_accurate(dG_old,dG_inp,dG_out,ik,deltaT)
 !
 ! The last term in the following come from the fact that G0=G_reference
 ! is not included in G(t) nor in G(t+dt)
 !
 ! G(T+dt)-G(T)=-idt/2[H,G(T+dt)+G(T)]-idt[H,G0]
 !
 ! (1+idt*H)G(t+dt)+G(t+dt)(1-idt*H)=(1-idt*H)G(t)+G(t)(1+idt*H)-2idt*[H,G0]
 !                                  =G(t)+G(t)-idt*[H,G(t)]-2idt*[H,G0]
 !
 ! A^dag G(t+dt) + G(t+dt) A = A G(t) + G(t) A^dag -2idt*[H,G0]
 !
 ! AdA = 1xA + A^tx1
 !
 ! AdA v[G(t+dt)] = G_com
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
 integer     :: ib1,ib2,ib3,ib4,ibb1,ibb2,i_fail,v_tmp(RT_nbands**2)
 real(SP)    :: En(RT_bands(1):RT_bands(2))
 complex(SP) :: AdA_k(RT_nbands**2,RT_nbands**2)
 complex(SP) :: U_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: H1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: H2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: M1_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: M2_tmp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: G_com(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 complex(SP), pointer :: vec_dG_out(:)
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
 dG_out=G_com+M1_tmp+M2_tmp
 !
 !*******************************************************
 !U_k=Ho_plus_Sigma(:,:,ik,1)
 !call SERIAL_HERMITIAN_diagonalization(RT_nbands,U_k,En)
 !!
 !! Generate the matrix  
 !!
 !H1_k=cZERO
 !H2_k=cZERO
 !do ib1=RT_bands(1),RT_bands(2)
 !  H1_k(ib1,ib1)=1._SP+cI*deltaT*En(ib1)
 !  H2_k(ib1,ib1)=1._SP-cI*deltaT*En(ib1)
 !enddo
 !!
 !! Rotate dG_out in the new basis set
 !!
 !call M_by_M_omp('c','n',RT_nbands,U_k   ,dG_out,M2_tmp)
 !call M_by_M_omp('n','n',RT_nbands,M2_tmp,U_k   ,dG_out)
 !!*******************************************************
 !
 vec_dG_out => linkM2V(dG_out,(/RT_nbands**2/))
 !
 AdA_k=cZERO
 do ib1=RT_bands(1),RT_bands(2)
   do ib2=RT_bands(1),RT_bands(2)
     do ib3=RT_bands(1),RT_bands(2)
       do ib4=RT_bands(1),RT_bands(2)
         ibb1=(ib3-RT_bands(1)+1)+RT_nbands*(ib1-RT_bands(1))
         ibb2=(ib4-RT_bands(1)+1)+RT_nbands*(ib2-RT_bands(1))
         AdA_k(ibb1,ibb2)=I1_matrix(ib1,ib2)*H2_k(ib3,ib4)+H1_k(ib2,ib1)*I1_matrix(ib3,ib4)
       enddo
     enddo
   enddo
 enddo
 !
 !call LINEAR_ALGEBRA_driver(LIN_SYS,M=AdA_k,B=vec_dG_out)
 call SERIAL_lin_system(RT_nbands**2,1,AdA_k,vec_dG_out)
 !
 nullify(vec_dG_out)
 !!*******************************************************
 !!
 !! Rotate G_com back the old basis set
 !!
 !call M_by_M_omp('n','n',RT_nbands,U_k   ,G_com,M2_tmp)
 !call M_by_M_omp('n','c',RT_nbands,M2_tmp,U_k  ,dG_out)
 !!
 !!*******************************************************
 dG_out=dG_out-dG_old
 !
end subroutine RT_INV_step_accurate
