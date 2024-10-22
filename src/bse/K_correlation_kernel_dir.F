!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
#include<dev_defs.h>
! 
function K_correlation_kernel_dir(i_block,i_p,i_pmq,                               &
&                                 i_k_s,i_p_s,i_n_k,i_n_p,i_kmq_s,i_pmq_s,i_m_k,i_m_p,  &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol_n,i_p_sp_pol_n,i_k_sp_pol_m,i_p_sp_pol_m, &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1,&
&                                 BS_n_g_W,O1,O2,O_times_W)
 !
 use pars,         ONLY:SP,cZERO,pi
 use BS,           ONLY:BS_blk,BS_W,BS_W_is_diagonal
 use R_lattice,    ONLY:DEV_VAR(G_m_G),DEV_VAR(g_rot)
 use wrapper,      ONLY:V_by_V_pwise_gpu,Vstar_dot_V_gpu,M_by_V_gpu
 !
 implicit none
 !
 integer,     intent(in) :: i_block,i_p,i_pmq,                                            &
&                                 i_k_s,i_p_s,i_n_k,i_n_p,i_kmq_s,i_pmq_s,i_m_k,i_m_p, &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol_n,i_p_sp_pol_n,i_k_sp_pol_m,i_p_sp_pol_m, &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1
 integer,     intent(in) :: BS_n_g_W
 complex(SP), intent(inout) DEV_ATTR :: O1(BS_n_g_W),O2(BS_n_g_W),O_times_W(BS_n_g_W)
 !
 complex(SP)  :: K_correlation_kernel_dir
 !
 integer      :: iO1,iO2,i_g1,i_g2,i_g3
 complex(SP), pointer DEV_ATTR :: O_c_d_iO1(:)
 complex(SP), pointer DEV_ATTR :: O_c_d_iO2(:)
 !
 if (i_k_sp_pol_n/=i_p_sp_pol_n .or. i_k_sp_pol_m/=i_p_sp_pol_m) then
   K_correlation_kernel_dir=cZERO
   return
 endif
 !
 iO1 = BS_blk(i_block)%O_table(i_k_s,i_p_s,1,1,i_n_k,i_n_p,i_k_sp_pol_n)
 iO2 = BS_blk(i_block)%O_table(i_kmq_s,i_pmq_s,i_kmq_t,i_pmq_t,i_m_k,i_m_p,i_k_sp_pol_m)
 !
 O_c_d_iO1 => DEV_VAR(BS_blk(i_block)%O_c)(:,iO1)
 O_c_d_iO2 => DEV_VAR(BS_blk(i_block)%O_c)(:,iO2)
 !
 !DEV_ACC_DEBUG data present(O_c_d_iO1,O_c_d_iO2,G_m_G,g_rot,O1,O2)
 !DEV_ACC parallel loop
 !DEV_CUF kernel do(1) <<<*,*>>>
 !DEV_OMP parallel do default(shared), private(i_g1,i_g2,i_g3)
 do i_g1=1,BS_n_g_W
   i_g2=DEV_VAR(G_m_G)(DEV_VAR(g_rot)(i_g1,iq_W_s)   ,ig_W   )
   i_g3=DEV_VAR(G_m_G)(DEV_VAR(g_rot)(i_g1,iq_W_s_mq),ig_W_mq)
   O1(i_g1)=O_c_d_iO1(i_g2)
   O2(i_g1)=O_c_d_iO2(i_g3)
 enddo
 !DEV_OMP end parallel do
 !DEV_ACC_DEBUG end data
 !
 if (     BS_W_is_diagonal) call V_by_V_pwise_gpu(BS_n_g_W,O_times_W,O1,BS_W(:,1,iq_W))
 if (.not.BS_W_is_diagonal) call M_by_V_gpu('T',BS_n_g_W,BS_W(:,:,iq_W),O1,O_times_W)
 !
 K_correlation_kernel_dir= Vstar_dot_V_gpu(BS_n_g_W,O2,O_times_W)*4._SP*pi
 !
end function K_correlation_kernel_dir
