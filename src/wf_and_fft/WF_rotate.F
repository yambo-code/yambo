!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine WF_rotate(ik,i_sp_pol,WFo,wf_size)
 !
 ! <r|n'>= <m|n'><r|m>
 !
 ! with <m|n'>=SC_R(m,n')
 !      <:|m>=WFo(:,m)
 !
 ! wf(:,n')= sum_m WFo(:,m) SC_R(m,n')
 !
 use pars,           ONLY:cZERO
 use electrons,      ONLY:n_spinor
 use pars,           ONLY:SP
 use wave_func,      ONLY:WAVEs,WF
 use hamiltonian,    ONLY:H_rotation,H_ref_bands
 use cuda_m,         ONLY:have_cuda
 !
 implicit none
 !
 integer     :: i_sp_pol,ik,wf_size
 type(WAVEs) :: WFo
 !
 ! Work Space
 !
 integer     :: ib,ibp,i_wf,i_wfp,i_spinor,i_c
 complex(SP) :: WS_wf(wf_size,n_spinor,H_ref_bands(1):H_ref_bands(2))
 !
 WS_wf=cZERO
 !
 do ib = H_ref_bands(1),H_ref_bands(2)
   !
   i_wf = WFo%index(ib,ik,i_sp_pol)
   if (i_wf==0) cycle
   !
   do ibp = H_ref_bands(1),H_ref_bands(2)
     i_wfp = WFo%index(ibp,ik,i_sp_pol)
     if (i_wfp==0) cycle
     WS_wf(:,:,ib)=WS_wf(:,:,ib)+H_rotation(ibp,ib,ik,i_sp_pol)*WFo%c(:,:,i_wfp)
   end do
   !
 enddo
 !
 do ib = H_ref_bands(1),H_ref_bands(2)
   !
   i_wf = WFo%index(ib,ik,i_sp_pol)
   if (i_wf==0) cycle
   !
   !$omp parallel do default(shared), private(i_c,i_spinor)
   do i_spinor=1,n_spinor
     do i_c=1,wf_size
       WF%c(i_c,i_spinor,i_wf)=WS_wf(i_c,i_spinor,ib)
     enddo
   enddo
   !$omp end parallel do
   !
   if (have_cuda) WF%c_d(:,:,i_wf) = WF%c(:,:,i_wf)
   !
 enddo
 !
end subroutine
