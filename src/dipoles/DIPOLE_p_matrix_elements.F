!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
!
! Copyright (C) 1992-2005, Lucia Reining, Valerio Olevano,
!   Francesco Sottile, Stefan Albrecht, Giovanni Onida,
!                    Fabien Bruneval
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
subroutine DIPOLE_p_matrix_elements(Xk,iv_wf,ic_wf,ik,rho,rho_spinor,P2,l_eval_P2)
 !
 use pars,          ONLY:SP,cZERO,cONE,pi
 use wrapper_omp,   ONLY:Vstar_dot_V_omp
 use D_lattice,     ONLY:alat
 use wave_func,     ONLY:WF,wf_ng
 use electrons,     ONLY:n_spinor
 use R_lattice,     ONLY:g_vec,bz_samp
#ifdef _CUDA
 use R_lattice,     ONLY:g_vec_d
#endif
#if defined _RT
 use drivers,       ONLY:l_real_time
 use real_time,     ONLY:l_update_SOC
#endif
 !
 implicit none
 !
 type(bz_samp), intent(in)  :: Xk
 integer,       intent(in)  :: iv_wf,ic_wf,ik
 logical,       intent(in)  :: l_eval_P2
 complex(SP),   intent(out) :: rho(3),rho_spinor(3,n_spinor,n_spinor),P2
 ! 
 ! Work Space
 !
 integer      :: ig,ierr
 logical      :: eval_spinor_matrix
 integer      :: i_spinor,j_spinor,i1
 complex(SP)  :: wf_grad(wf_ng,n_spinor),wf_der_sq(wf_ng,n_spinor)
#ifdef _CUDA
 complex(SP), pointer, device :: WF_c_d(:,:,:)
 complex(SP)  :: rho_acc
 real(SP)     :: rho_acc_r,rho_acc_i
 real(SP)     :: alat_i1,xk_i1
#endif
 !
 rho=cZERO
 rho_spinor=cZERO
 P2=cZERO
 ! 
 eval_spinor_matrix=.false.
#if defined _RT
 eval_spinor_matrix=l_real_time.and.n_spinor==2.and.l_update_SOC
#endif
 !
 do i1=1,3
   !
   ! wf_grad = -i grad fdo(iv,ik)
   !
#ifdef _CUDA
   !
   WF_c_d => WF%c_d
   xk_i1=Xk%pt(ik,i1)
   alat_i1=alat(i1)
   !
   if(eval_spinor_matrix) then
     !
     do i_spinor=1,n_spinor
       do j_spinor=1,n_spinor
         !
         rho_acc_r=0.0_SP
         rho_acc_i=0.0_SP
         !
         !$cuf kernel do(1)
         do ig=1,wf_ng
           rho_acc= conjg(WF_c_d(ig,i_spinor,iv_wf))*(xk_i1+g_vec_d(ig,i1))*2.0_SP*pi/alat_i1* &
&                         WF_c_d(ig,j_spinor,ic_wf)
           rho_acc_r=rho_acc_r + real(rho_acc,SP)
           rho_acc_i=rho_acc_i + aimag(rho_acc)
         enddo
         !
         rho_spinor(i1,i_spinor,j_spinor)=cmplx(rho_acc_r,rho_acc_i,SP)
       enddo
       rho(i1)=rho(i1)+rho_spinor(i1,i_spinor,i_spinor)
     enddo
     !
   else
     !
     rho_acc_r=0.0_SP
     rho_acc_i=0.0_SP
     ! 
     !$cuf kernel do(2)
     do i_spinor=1,n_spinor
     do ig=1,wf_ng
        rho_acc= conjg(WF_c_d(ig,i_spinor,iv_wf))*(xk_i1+g_vec_d(ig,i1))*2.0_SP*pi/alat_i1* &
&                      WF_c_d(ig,i_spinor,ic_wf)
        rho_acc_r=rho_acc_r + real(rho_acc,SP)
        rho_acc_i=rho_acc_i + aimag(rho_acc)
     enddo
     enddo
     !
     rho(i1)=cmplx(rho_acc_r,rho_acc_i,SP)
     !
   endif
   !
   !
#else
   !$omp parallel do default(shared), private(ig,i_spinor), collapse(2)
   do i_spinor=1,n_spinor
     do ig=1,wf_ng
       wf_grad(ig,i_spinor)=WF%c(ig,i_spinor,iv_wf)*(Xk%pt(ik,i1)+g_vec(ig,i1))*2.0_SP*pi/alat(i1)
     enddo
   enddo
   !$omp end parallel do
   !
   ! WARNING: 
   ! ========
   ! The convention is to revert the order of the bands
   ! indexes in the definition of DIP_P and DIP_iR :
   ! DIP_P(iv,ic)  = P_cv ; DIP_iR(iv,ic) = (iR)_cv 
   !
   ! Note that P is Hermitian and iR anti-hermitian.
   !
   ! rho = <v|p|c> = P_vc =
   !     = wf_xyz^* fdo(ic,ik)= - i < fdo(iv,ik)| grad |  fdo(ic,ik) > =
   !       \sum_G -i u^*_{iv ik}(G) i ( k + G ) u_{ic ik}(G)
   !     = \sum_G u^*_{iv ik}(G) ( k + G ) u_{ic ik}(G)
   !
   ! P2 =  \sum_G u^*_{iv ik}(G) ( k + G )^2 u_{ic ik}(G)
   !
   ! spinor indexes do not follow band indexes, i.e. they are not reverted
   !
   if(eval_spinor_matrix) then
     !
     do i_spinor=1,n_spinor
       do j_spinor=1,n_spinor
         rho_spinor(i1,i_spinor,j_spinor)=Vstar_dot_V_omp(wf_ng,wf_grad(:,i_spinor),WF%c(:,j_spinor,ic_wf))
       enddo
       rho(i1)=rho(i1)+rho_spinor(i1,i_spinor,i_spinor)
     enddo
     !
   else
     !
     rho(i1)=Vstar_dot_V_omp(wf_ng*n_spinor,wf_grad(:,:),WF%c(:,:,ic_wf))
     !
   endif
   !
#endif
   !
   !
 enddo
 !
end subroutine
