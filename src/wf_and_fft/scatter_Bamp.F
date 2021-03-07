!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM,DS,AF
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
subroutine DEV_SUB(scatter_Bamp)(isc)
 !
 ! rhotw(G)=<ib ik_is i_sp| e^[I(RsG-G0).r] | ob ok_os o_sp>   
 !
 !  is(3)=(ib,ik,is,i_sp) --<--:--<-- os(3)=(ob,ok,os,o_sp)
 !                             :
 !                            /:\ iqs=(ig0,iq,qs)
 !                             :
 !
 !                     \:/
 ! REMEMBER that     ->-:->-  =  conjg [-<-:-<-]
 !                                        /:\
 !  iq_is = ik_is-ok_os-Go
 !
 use pars,           ONLY:cZERO,cONE,DP,SP
 use electrons,      ONLY:n_spinor
 use collision_el,   ONLY:elemental_collision
 use D_lattice,      ONLY:idt_index
 use R_lattice,      ONLY:DEV_VAR(g_rot)
 use pseudo,         ONLY:pp_is_uspp
 use FFT_m,          ONLY:fft_size,fft_dim,DEV_VAR(fft_g_table)
 use FFT_m,          ONLY:fftw_plan,cufft_plan
 use interfaces,     ONLY:WF_apply_symm
 !
 implicit none
 !
 type(elemental_collision), target::isc
 
 integer :: qs,iGo
 integer :: ig,ir,isc_ngrho,ib,ibp
 complex(SP), pointer DEV_ATTR :: WF_symm_i_p(:,:), WF_symm_o_p(:,:)
 complex(SP), pointer DEV_ATTR :: rhotw_p(:)
 complex(DP), pointer DEV_ATTR :: rho_tw_rs_p(:)
 logical :: have_cuda_loc

 !
 ! define pointers to enable CUF kernels
 ! when compiling using CUDA-Fortran
 !
 WF_symm_i_p => DEV_VAR(isc%WF_symm_i)
 WF_symm_o_p => DEV_VAR(isc%WF_symm_o)
 rho_tw_rs_p => DEV_VAR(isc%rho_tw_rs)
 rhotw_p     => DEV_VAR(isc%rhotw)
 !
 have_cuda_loc=.false.
#ifdef _CUDA_LOC
 have_cuda_loc=.true.
#endif

 !
 ! |ib ik_is i_sp>
 call WF_apply_symm(isc%is,WF_symm_i_p)
 !
 ! | ob ok_os o_sp>   
 call WF_apply_symm(isc%os,WF_symm_o_p)

 !
 ! \tilde{\rho} in Real Space
 !
#if defined _FFTQE && !defined _USE_3D_FFT && !defined _CUDA_LOC
 ! 
 ! add one extra conjg since FFTQE cannot account for the conjg in one go
 ! 
 !$omp parallel default(shared), private(ir)
 !$omp do
 do ir = 1, fft_size
   isc%rho_tw_rs(ir) = cmplx(isc%WF_symm_i(ir,1)*conjg(isc%WF_symm_o(ir,1)),kind=DP)
 enddo
 !
 if (n_spinor==2) then
   !$omp do
   do ir = 1, fft_size
     isc%rho_tw_rs(ir) = isc%rho_tw_rs(ir)+cmplx(isc%WF_symm_i(ir,2)*conjg(isc%WF_symm_o(ir,2)),kind=DP)
   enddo
 endif
 !$omp end parallel
 !
#else
 !
 ! ordinary implementation
 !
#ifdef _CUDA_LOC
 !$cuf kernel do(1) <<<*,*>>>
#else
 !$omp parallel default(shared), private(ir)
 !$omp do
#endif
 do ir = 1, fft_size
   rho_tw_rs_p(ir) = cmplx(conjg(WF_symm_i_p(ir,1))*WF_symm_o_p(ir,1),kind=DP)
 enddo
 !
 if (n_spinor==2) then
#ifdef _CUDA_LOC
   !$cuf kernel do(1) <<<*,*>>>
#else
   !$omp do
#endif
   do ir = 1, fft_size
     rho_tw_rs_p(ir) = rho_tw_rs_p(ir)+cmplx(conjg(WF_symm_i_p(ir,2))*WF_symm_o_p(ir,2),kind=DP)
   enddo
 endif
#if !defined _CUDA_LOC
 !$omp end parallel
#endif
 !
#endif


 !
 ! perform the actual FFT
 !
#if defined _CUDA_LOC
 !
 call fft_3d_cuda(rho_tw_rs_p,fft_dim,+1,cufft_plan)
 !
#else 
 !
#  if defined _FFTW
 call fft_3d(isc%rho_tw_rs,fft_dim,+1,fftw_plan)
#  elif defined _FFTSG || (defined _USE_3D_FFT && defined _FFTQE)
 call fft_3d(isc%rho_tw_rs,fft_dim,+1)
#  elif defined _FFTQE
 !
 ! the QE fft must be performed in the right
 ! direction. conjg is taken care separately
 ! AF: please note the different definition of isc%rho_tw_rs above
 !     (one extra conjg has been added)
 !
 call fft_3d(isc%rho_tw_rs,fft_dim,-1)
 !
#  else
#    error "[CPP] Inconsistent FFT environment"
#  endif
 !
#endif
 !
 !
 ! e^[I(Rs(G-G0)).r]
 !
 iGo=isc%qs(1)
 qs =isc%qs(3)
 isc_ngrho=isc%ngrho
 !
#if defined _FFTQE && !defined _USE_3D_FFT && !defined _CUDA_LOC
 !
 ! one extra conjg is performed on rho_tw_rs
 !
 if (qs==idt_index) then 
   !$omp parallel do default(shared), private(ig)
   do ig = 1, isc%ngrho
     isc%rhotw(ig)= cmplx(conjg(isc%rho_tw_rs(fft_g_table(ig,iGo))),kind=SP)
   enddo
 else
   !$omp parallel do default(shared), private(ig)
   do ig = 1, isc%ngrho
     isc%rhotw(ig)= cmplx(conjg(isc%rho_tw_rs(fft_g_table(g_rot(ig,qs),iGo))),kind=SP)
   enddo
 endif
 !
#else
 !
 if (qs==idt_index) then 
   !
#ifdef _CUDA_LOC
   !$cuf kernel do(1) <<<*,*>>>
#else
   !$omp parallel do default(shared), private(ig)
#endif
   do ig = 1, isc_ngrho
     rhotw_p(ig)= cmplx(rho_tw_rs_p(DEV_VAR(fft_g_table)(ig,iGo)),kind=SP)
   enddo
   !
 else
   !
#ifdef _CUDA_LOC
   !$cuf kernel do(1) <<<*,*>>>
#else
   !$omp parallel do default(shared), private(ig)
#endif
   do ig = 1, isc_ngrho
     rhotw_p(ig)= cmplx(rho_tw_rs_p(DEV_VAR(fft_g_table)(DEV_VAR(g_rot)(ig,qs),iGo)),kind=SP)
   enddo
   !
 endif
 !
#endif

 !
 ! add USPP augmentation
 !
 if (pp_is_uspp) then
   !
   if (have_cuda_loc) call error("[USPP] CUDA and USPP not implemented")
   !
   ib=isc%is(1)
   ibp=isc%os(1)
   call PP_becprod_calc(isc%becprod,1,1,.false.,ib,ibp,isc%ibec,isc%obec)
   !
   call PP_addus_rhotw_aug(isc)
   !
 endif

 !
 ! q=0, G=0 case
 !
 if (isc%qs(2)==1 .and. isc%is(1)==isc%os(1)) rhotw_p(1)=cONE
 if (isc%qs(2)==1 .and. isc%is(1)/=isc%os(1)) rhotw_p(1)=cZERO

end subroutine DEV_SUB(scatter_Bamp)

