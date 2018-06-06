!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
subroutine PP_vloc_augment(QP_Vloc,Vloc,k,ik,ib,ibp,wf,wfp)
 !
 ! Add the UPSS augmentation to the matrix element of a local potential
 ! All quantities given in real space.
 !
 use pars,          ONLY:SP,DP
 use com,           ONLY:error
 use vec_operate,   ONLY:c2a
 use electrons,     ONLY:n_spinor,n_sp_pol
 use R_lattice,     ONLY:bz_samp,b
 use pseudo,        ONLY:pp_is_uspp,qe_pseudo_alloc
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table
#if defined _FFTW
 use FFT_m,         ONLY:fftw_plan
#endif
 use wave_func,     ONLY:wf_nc_k,wf_igk,wf_ncx!,WF
 use qe_pseudo_m,   ONLY:qe_omega=>omega,qe_ngm=>ngm, nkb,vkb,becp,becprod,tpiba,&
&                        allocate_bec_type,deallocate_bec_type,calbec
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 ! vars
 !
 complex(SP), intent(inout) :: QP_Vloc
 type(bz_samp), intent(in)  ::k
 integer,     intent(in)    :: ik,ib,ibp
 real(SP),    intent(in)    :: Vloc(fft_size,n_sp_pol)
 complex(SP), intent(in)    :: wf(fft_size,n_spinor)
 complex(SP), intent(in)    :: wfp(fft_size,n_spinor)
 !
 ! Workspace
 !
 integer :: ig,igs,ifft,i_sp_pol,i_spinor
 integer :: npwk
 real(SP)   :: xk(3)
 complex(SP):: QP_aug
 complex(DP), allocatable :: wf_DP(:)
 complex(DP), allocatable :: rhog_aug_DP(:,:), aux_DP(:)
 complex(SP), allocatable :: wf_g(:,:)
 

 !
 ! checks
 !
 if (.not.pp_is_uspp) return
 if (.not.qe_pseudo_alloc) call error(' [PP] qe_pseudo not alloc PP_vloc_aug')
 !
 if (n_sp_pol/=1) call error(" [PP] n_spin>1 not implemented in PP_Vloc_augment")
 
 !
 ! allocations
 !----------------------
 !
 YAMBO_ALLOC(wf_DP,(fft_size))
 YAMBO_ALLOC(wf_g,(wf_ncx*n_spinor,2))
 !
 becprod=0.0
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
#endif

 !
 ! get beta projectors
 !----------------------
 !
 call c2a(b,k%pt(ik,:),xk,mode="ki2c")
 xk=xk/real(tpiba,SP)
 !
 ! allocate becp for 2 bands at a time, 
 ! this should be ideally improved, 
 ! but requires rearranging ahead
 !
 call allocate_bec_type(nkb, 2, becp)
 becp%k=0.0
 !
 npwk=wf_nc_k(ik)
 call init_us_2(npwk,wf_igk(1:npwk,ik),xk,vkb)
 !

 !
 ! wfc to reciprocal space
 !--------------------------------
 !
 do i_spinor = 1, n_spinor
   !
   wf_DP(:)=wf(:,i_spinor)
#if defined _FFTW
   call fft_3d(wf_DP,fft_dim,-1,fftw_plan)
#else
   call fft_3d(wf_DP,fft_dim,-2)
#endif
   !
   igs= (i_spinor-1)*wf_ncx
   do ig = 1, npwk
      wf_g(igs+ig,1)=cmplx(wf_DP(fft_g_table(wf_igk(ig,ik),1)),kind=SP)/sqrt(real(fft_size,SP))
   enddo
   !
 enddo
 !
 if (ib==ibp) then
   !
   wf_g(:,2)=wf_g(:,1)
   !
 else
   !
   do i_spinor = 1, n_spinor
     !
     wf_DP(:)=wfp(:,i_spinor)
#if defined _FFTW
     call fft_3d(wf_DP,fft_dim,-1,fftw_plan)
#else
     call fft_3d(wf_DP,fft_dim,-2)
#endif
     !
     igs= (i_spinor-1)*wf_ncx
     do ig = 1, npwk
       wf_g(igs+ig,2)=cmplx(wf_DP(fft_g_table(wf_igk(ig,ik),1)),kind=SP)/sqrt(real(fft_size,SP))
     enddo
     !
   enddo
   !
 endif


 !
 ! calbec and becprod
 !--------------------------------
 !
 if (allocated(becp%k))  becp%k=0.0
 if (allocated(becp%nc)) becp%nc=0.0
 !
 call calbec(npwk,vkb,wf_g,becp,2)
 !
 i_sp_pol=1
 call becprod_calc(ik, i_sp_pol, 1, 2 )
 !
 call deallocate_bec_type(becp)
 !
 YAMBO_FREE(wf_DP)
 YAMBO_FREE(wf_g)
 
 !
 ! augmentation
 !--------------------------------
 !
 YAMBO_ALLOC(aux_DP,(fft_size))
 YAMBO_ALLOC(rhog_aug_DP,(qe_ngm,n_sp_pol))
 rhog_aug_DP=0.0
 !
 call addus_vloc_aug(qe_ngm,n_sp_pol,qe_ngm,rhog_aug_DP)
 !
 QP_aug=0.0
 !
 do i_sp_pol=1,n_sp_pol
    !
    aux_DP=0.0
    do ig = 1, qe_ngm
       aux_DP(fft_g_table(ig,1)) = rhog_aug_DP(ig,i_sp_pol)
    enddo
    !
#if defined _FFTW
    call dfftw_destroy_plan(fftw_plan)
    fftw_plan = 0
    call fft_3d(aux_DP,fft_dim,+1,fftw_plan)
#else
    call fft_3d(aux_DP,fft_dim,+1)
#endif
    !
    aux_DP=aux_DP*qe_omega/real(fft_size,DP)
    !
    do ifft=1,fft_size
       QP_aug = QP_aug + Vloc(ifft,i_sp_pol)*aux_DP(ifft)
    enddo
    !
 enddo
 !
 YAMBO_FREE(rhog_aug_DP)
 YAMBO_FREE(aux_DP)
 !
 QP_Vloc = QP_Vloc + QP_aug
 return
 !
end subroutine PP_vloc_augment
