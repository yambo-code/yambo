!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
#include<dev_defs.h>
!
subroutine TDDFT_ALDA_eh_space_G_collisions_R(iq,Xk,i_T_grp,NG,l_bs_tddft_wf_in_loop,tddft_wf,mode,l_bare)
 !
 ! Calculates the F_xc scattering 
 !
 !  (ic(1),ik(1),is(1)) --<--:...:--<-- (ic(2),ik(2),is(2))
 !                       :sqrt(Fxc):
 !
 use pars,           ONLY:SP,DP,pi,cZERO
 use FFT_m,          ONLY:fft_size
 use wave_func,      ONLY:WF
 use xc_functionals, ONLY:F_xc_mat
 use interfaces,     ONLY:WF_load,WF_free
 use R_lattice,      ONLY:qindx_X,bz_samp,minus_G
 use BS,             ONLY:l_BS_magnons,BS_T_grp,BS_bands,BS_K_is_Chi,BS_n_g_fxc
 use BS_solvers,     ONLY:BSS_q0
 use DIPOLES,        ONLY:DIP_iR
 use electrons,      ONLY:n_spinor,n_sp_pol,n_spin
 use timing_m,       ONLY:timing
 use openmp,         ONLY:OPENMP_update,master_thread
 use interfaces,     ONLY:WF_apply_symm
 use TDDFT,          ONLY:tddft_wf_t
 use collision_el,   ONLY:elemental_collision,elemental_collision_alloc,elemental_collision_free
 !
 implicit none
 !
 type(bz_samp),intent(in) :: Xk
 integer,      intent(in) :: iq,i_T_grp,NG(2)
 logical,      intent(in) :: l_bs_tddft_wf_in_loop
 character(1), intent(in) :: mode
 type(tddft_wf_t), target, intent(inout) :: tddft_wf 
 logical, intent(in) :: l_bare
 ! 
 ! Work Space
 !
 logical     :: l_load_WFs
 logical     :: bare_vertex
 integer     :: i_spinor,j_spinor,ip_spinor,jp_spinor,ifft,irhotw,is_yambo,os_yambo,i_spinor_y,j_spinor_y,&
&               i_sp_loop,o_sp_loop
 integer     :: i_T_el,i_T_el_p,N_T_el_p,i_c,i_v,i_sp_c,i_sp_v,i_k_bz,i_k,i_s,i_g0,i_g1,i_g2,i_p_bz,i_g_p,i_p,&
&               i_sp,i_T_grp_p,is(4),os(4),qs(3),NK(2)
 !
 complex(DP), pointer :: rhotwr_p(:)
 complex(SP), pointer :: WF_symm1_p(:,:)
 complex(SP), pointer :: WF_symm2_p(:,:)
 complex(SP) :: chi_dipole(3),chi_head
 type(elemental_collision) :: chi_scatt
 !
#if defined _CUDA
 call error("TDDFT ALDA Osc. not GPU ported")
#endif
 !
 call timing('T_space ALDA Osc.',OPR='start')
 bare_vertex=l_bare
 ! The bare vertex is the transition density rho_cv(q+G): scatter_Bamp sums it over the
 ! spinor components, so scalar and spinor (SOC) states are both supported; collinear spin
 ! polarization (a spin index per transition) and magnons are not.
 if (bare_vertex.and.(n_sp_pol/=1.or.l_BS_magnons)) &
& call error('[Chi/BSK] bare transition vertices: scalar or spinor states only (no spin polarization, no magnons)')
 !
 ! Allocate
 !
 WF_symm1_p => tddft_wf%WF_symm1
 WF_symm2_p => tddft_wf%WF_symm2
 rhotwr_p   => tddft_wf%rhotwr_DP
 !
 WF_symm1_p=cZERO
 WF_symm2_p=cZERO
 rhotwr_p=  cZERO
 if (bare_vertex) call elemental_collision_alloc(chi_scatt,NG=BS_n_g_fxc,&
&                                              CPU_ONLY=.TRUE.,TITLE='Chi/BSK')
 !
 ! Use pointers both for CUDA and to avoid continuous allocation and de-allocation
 !
#if defined(__NOTNOW) && ! defined(_CUDA)
 !$omp parallel default(shared), private( K_EXCH_collision, &
 !$omp &        i_T_el,i_k_bz,i_k,i_s, i_p_bz,i_p,i_sp, i_v,i_c,i_sp_c,i_sp_v, &
 !$omp &        i_T_grp_last_with_identity_sym, i_T_el_last_with_identity_sym, &
 !$omp &        i_T_grp_p, N_T_el_p, i_T_el_p, i_g1, i_g2, Nk, is, os)
#endif
 !
 call OPENMP_update(master_thread)
 !
#if defined(__NOTNOW) && ! defined(_CUDA)
 !$omp do schedule(dynamic)
#endif
 NK=-1
 do i_T_el=1,BS_T_grp(i_T_grp)%size
   !
   i_k_bz=BS_T_grp(i_T_grp)%table(i_T_el,1)
   i_k   =Xk%sstar(i_k_bz,1)
   i_s   =Xk%sstar(i_k_bz,2)
   !
   i_p_bz=qindx_X(iq,i_k_bz,1)
   i_g_p =qindx_X(iq,i_k_bz,2)
   !
   i_p   =Xk%sstar(i_p_bz,1)
   i_sp  =Xk%sstar(i_p_bz,2)
   !
   i_v   =BS_T_grp(i_T_grp)%table(i_T_el,2)
   i_c   =BS_T_grp(i_T_grp)%table(i_T_el,3)
   i_sp_c=BS_T_grp(i_T_grp)%table(i_T_el,4)
   i_sp_v=BS_T_grp(i_T_grp)%table(i_T_el,5)
   !
   is=(/i_c,i_k,i_s ,i_sp_c/)
   os=(/i_v,i_p,i_sp,i_sp_v/)
   qs=(/minus_G(i_g_p),iq,1/)
   !
   if (mode=="R".or.mode=="Q".or.mode=="F") then
     is(1)=i_c
     os(1)=i_v
   else if (mode=="A".or.mode=="C") then
     ! Same momenta and spin indexes, inverted bands
     is(1)=i_v
     os(1)=i_c
   endif
   !
   l_load_WFs= l_bs_tddft_wf_in_loop .and. (NK(1)/=min(i_k,i_p).or.NK(2)/=max(i_k,i_p))
   if (l_load_WFs) then
#if defined(__NOTNOW) && ! defined(_CUDA)
     !$omp critical
#endif
     if (NK(2)/=-1) call WF_free(WF,keep_fft=.true.,keep_states_to_load=.true.)
     NK=(/min(i_k,i_p),max(i_k,i_p)/)
     call WF_load(WF,NG(1),NG(2),BS_bands,NK,k_extrema_only=.true.,quiet=.true.,&
     &            space='R',title="Kernel exch",keep_states_to_load=.true.)
#if defined(__NOTNOW) && ! defined(_CUDA)
     !$omp end critical
#endif
   endif
   !
   if ( (is(4)/=os(4)) .and. ((.not.l_BS_magnons).or. n_sp_pol==1) ) call error(" TDDFT_ALDA_R_space: wrong spin index")
   if ( (is(4)==os(4)) .and. ((     l_BS_magnons).and.n_sp_pol==2) ) call error(" TDDFT_ALDA_R_space: wrong spin index")
   !
   if (.not.bare_vertex) then
     call WF_apply_symm(is,WF_symm1_p)
     call WF_apply_symm(os,WF_symm2_p)
   endif
   !
   if (bare_vertex) then
     !
     ! Native Chi uses rho_cv(G) from scatter_Bamp (summed over the spinor components,
     ! USPP augmentation included). The transition charge is conjg(rho_cv(G)). One
     ! density vertex per transition, in the (1,1) slot, for scalar and spinor states.
     chi_scatt%is=is
     chi_scatt%os=os
     chi_scatt%qs=(/i_g_p,iq,1/)
     call scatter_Bamp(chi_scatt)
     BS_T_grp(i_T_grp)%O_tddft_R(:,i_T_el,1,1)=conjg(chi_scatt%rhotw)
     if (iq==1) then
       if (.not.allocated(DIP_iR)) call error('[Chi/BSK] optical position dipoles were not loaded')
       call DIPOLE_rotate(i_c,i_v,i_k_bz,i_sp_c,'DIP_iR',Xk,chi_dipole)
       chi_head=-dot_product(BSS_q0,chi_dipole)
       if (mode=='A'.or.mode=='C') chi_head=-conjg(chi_head)
       BS_T_grp(i_T_grp)%O_tddft_R(1,i_T_el,1,1)=chi_head
     endif
     !
   else if( n_spin==1 ) then
     !
     rhotwr_p(:)=cmplx(F_xc_mat(:,1,1,1,1)*WF_symm1_p(:,1)*conjg(WF_symm2_p(:,1)),kind=DP)
     call perform_fft_3d(qs,rhotwr_p,BS_T_grp(i_T_grp)%O_tddft_R(:,i_T_el,1,1))
     !
   endif
   !
   if(n_sp_pol==2.and..not.bare_vertex) then
     !
     do i_sp_loop=1,n_sp_pol
       do o_sp_loop=1,n_sp_pol
         ! This takes into account the different conventions between the definition of the spin indexes
         ! between yambo and the derivation of fxc in spin space for magnons
         ! This happens because in the yambo convention the left indexes "v sigma_v, c sigma_c, k" are inverted
         ! with respect to the standard convention, i.e. "c sigma_c, v sigma_v, k"
         ! As a result f_\up\dn,\dn\up is in yambo f_\dn\up,\dn\up and so on
         is_yambo=o_sp_loop
         os_yambo=i_sp_loop
         ! To check/fix the spinorial version 
         rhotwr_p(:)=cmplx(F_xc_mat(:,is_yambo,os_yambo,is(4),os(4))* &
         &                                             WF_symm1_p(:,1)*conjg(WF_symm2_p(:,1)),kind=DP)
         call perform_fft_3d(qs,rhotwr_p,BS_T_grp(i_T_grp)%O_tddft_R(:,i_T_el,i_sp_loop,o_sp_loop))
         !
       enddo
     enddo
   endif
   !
   if(n_spinor==2.and..not.bare_vertex) then
     !
     do i_spinor=1,n_spinor
       do j_spinor=1,n_spinor
         i_spinor_y=j_spinor
         j_spinor_y=i_spinor
         rhotwr_p=cZERO
         do ip_spinor=1,n_spinor
           do jp_spinor=1,n_spinor
             do ifft=1,fft_size
               rhotwr_p(ifft)=rhotwr_p(ifft)+                        &
               & cmplx(F_xc_mat(ifft,i_spinor_y,j_spinor_y,ip_spinor,jp_spinor)* &
	       & WF_symm1_p(ifft,ip_spinor)*conjg(WF_symm2_p(ifft,jp_spinor)),kind=DP)
             enddo
           enddo
         enddo
         call perform_fft_3d(qs,rhotwr_p,BS_T_grp(i_T_grp)%O_tddft_R(:,i_T_el,i_spinor,j_spinor))
         !
       enddo
     enddo
   endif
   !
 enddo
 !
 if(l_bs_tddft_wf_in_loop) call WF_free(WF,keep_fft=.true.,keep_states_to_load=.true.)
 if (bare_vertex) call elemental_collision_free(chi_scatt)
 !
 nullify(WF_symm1_p)
 nullify(WF_symm2_p)
 nullify(rhotwr_p)
 !
 call timing('T_space ALDA Osc.',OPR='stop')
 !
end subroutine TDDFT_ALDA_eh_space_G_collisions_R
