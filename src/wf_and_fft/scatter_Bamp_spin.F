!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine scatter_Bamp_spin(isc,which)
 !
 ! It computes
 !
 ! rhotw(G)=<ib ik_is i_sp| e^[I(RsG-G0).r] S_i | ob ok_os o_sp>    ! which ="i"
 !
 !
 !  is(3)=(ib,ik,is,i_sp) --<--:--<-- os(3)=(ob,ok,os,o_sp)
 !                             :
 !                            /:\ iqs=(ig0,iq,qs)
 !                             :
 !
 !  iq_is = ik_is-ok_os-Go
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:n_spinor,n_sp_pol
 use collision_el,   ONLY:elemental_collision
 use interfaces,     ONLY:WF_apply_symm
 use D_lattice,      ONLY:idt_index
 use R_lattice,      ONLY:g_rot
 use FFT_m,          ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 !
 implicit none
 !
 type(elemental_collision)::isc
 character(1)             ::which
 !
 ! Work Space
 !
 complex(SP) :: WF_spin(fft_size,n_spinor)
 !
 integer     :: qs,iGo,ig, ir,ic,ig_
 !
 ! |ib ik_is i_sp>
 call WF_apply_symm(isc%is,isc%WF_symm_i)
 !
 ! | ob ok_os o_sp>   
 call WF_apply_symm(isc%os,isc%WF_symm_o)
 !
 if (n_sp_pol==2) then
   if (which=="x") WF_spin=isc%WF_symm_i
   if (which=="y") WF_spin=isc%WF_symm_i*cI*(-1)**(isc%is(4)-1)
   if (which=="z") WF_spin=isc%WF_symm_i   *(-1)**(isc%is(4)-1)
 else
   if (which=="x") then
     WF_spin(:,1)=    isc%WF_symm_i(:,2)
     WF_spin(:,2)=    isc%WF_symm_i(:,1)
   endif
   if (which=="y") then
     WF_spin(:,1)= cI*isc%WF_symm_i(:,2)
     WF_spin(:,2)=-cI*isc%WF_symm_i(:,1)
   endif
   if (which=="z") then
     WF_spin(:,1)=    isc%WF_symm_i(:,2)
     WF_spin(:,2)=-   isc%WF_symm_i(:,1)
   endif
 endif
 !
 ! Chartesian components of the gradient loop
 !
 !$omp parallel default(shared), private(ir)
 !$omp do
 do ir = 1, fft_size
   isc%rho_tw_rs(ir) = conjg(WF_spin(ir,1))*isc%WF_symm_o(ir,1)
 enddo
 !$omp end do
 !
 if (n_spinor==2) then
 !$omp do
   do ir = 1, fft_size
     isc%rho_tw_rs(ir) = isc%rho_tw_rs(ir) + conjg(WF_spin(ir,2))*isc%WF_symm_o(ir,2)
   enddo
 !$omp end do
 endif
 !$omp end parallel
 !
 ! perform the actual FFT
 !
#if defined _FFTW
 call fft_3d(isc%rho_tw_rs,fft_dim,+1,fftw_plan)
#else
 call fft_3d(isc%rho_tw_rs,fft_dim,+1)
#endif
 !
 ! e^[I(Rs(G-G0)).r]
 !
 iGo=isc%qs(1)
 qs =isc%qs(3)
 !
#if defined _OPENMP
 if (qs==idt_index) then 
   !$omp parallel do default(shared), private(ig,ig_)
   do ig = 1, isc%ngrho
     isc%rhotw(ig)= isc%rho_tw_rs(fft_g_table(ig,iGo))
   enddo
   !$omp end parallel do
 else
   !$omp parallel do default(shared), private(ig,ig_)
   do ig = 1, isc%ngrho
     isc%rhotw(ig)= isc%rho_tw_rs(fft_g_table(g_rot(qs,ig),iGo))
   enddo
   !$omp end parallel do
 endif
#else
 do ig = 1,isc%ngrho
   if (qs==idt_index) isc%rhotw(ig)= isc%rho_tw_rs(fft_g_table(ig,iGo))
   if (qs/=idt_index) isc%rhotw(ig)= isc%rho_tw_rs(fft_g_table(g_rot(qs,ig),iGo))
 enddo
#endif
 !
end subroutine scatter_Bamp_spin
