!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
function K_correlation_kernel(i_block,i_p,i_pmq,                               &
&                                 i_k_s,i_kp_s,i_n_k,i_n_p,i_kmq_s,i_kp_mq_s,i_m_k,i_m_p,  &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol,i_p_sp_pol,                   &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1)
 !
 use pars,         ONLY:SP,cZERO,pi
 use BS,           ONLY:BS_blk,BS_n_g_W,BS_W,BS_W_is_diagonal,WF_phase
 use D_lattice,    ONLY:nsym,i_time_rev
 use R_lattice,    ONLY:G_m_G,g_rot
#ifdef _CUDA
 use R_lattice,    ONLY:G_m_G_d,g_rot_d
#endif
 use wrapper,      ONLY:V_by_V_pwise,M_by_V,Vstar_dot_V
 use device_util_m,ONLY:dev_conjg
 !
 implicit none
 !
 integer,     intent(in) :: i_block,i_p,i_pmq,                                            &
&                                 i_k_s,i_kp_s,i_n_k,i_n_p,i_kmq_s,i_kp_mq_s,i_m_k,i_m_p, &
&                                 i_kmq_t,i_pmq_t,i_k_sp_pol,i_p_sp_pol,                  &
&                                 iq_W,iq_W_s,ig_W,i_k_s_m1,iq_W_s_mq,ig_W_mq,i_kmq_s_m1
 !
 complex(SP)  :: K_correlation_kernel
 !
 integer      :: iO1,i_block_1,iO2,i_block_2,i_g1,i_g2,i_g3
 complex(SP)  :: PHASE_1,PHASE_2,O1(BS_n_g_W),O2(BS_n_g_W),O_times_W(BS_n_g_W)
#ifdef _CUDA
 attributes(device)   :: O1,O2,O_times_W
 complex(SP), pointer, device :: O_c_d_iO1(:)
 complex(SP), pointer, device :: O_c_d_iO2(:)
#endif
 !
 if (i_k_sp_pol/=i_p_sp_pol) then
   K_correlation_kernel=cZERO
   return
 endif
 !
 iO1       = BS_blk(i_block)%O_table(1,i_kp_s,1,1,i_n_k,i_n_p,i_k_sp_pol)
 i_block_1 = BS_blk(i_block)%O_table(2,i_kp_s,1,1,i_n_k,i_n_p,i_k_sp_pol)
 PHASE_1=WF_phase(i_p,i_kp_s,i_n_p,i_p_sp_pol)
 if (PHASE_1==cZERO  ) PHASE_1=1._SP
 if (PHASE_1==-99._SP) PHASE_1=1._SP
 !
 iO2       = BS_blk(i_block)%O_table(1,i_kp_mq_s,i_kmq_t,i_pmq_t,i_m_k,i_m_p,i_k_sp_pol)
 i_block_2 = BS_blk(i_block)%O_table(2,i_kp_mq_s,i_kmq_t,i_pmq_t,i_m_k,i_m_p,i_k_sp_pol)
 PHASE_2=WF_phase(i_pmq,i_kp_mq_s,i_m_p,i_p_sp_pol)
 if (PHASE_2==cZERO  ) PHASE_2=1._SP
 if (PHASE_2==-99._SP) PHASE_2=1._SP
 !
#ifdef _CUDA
 !
 O_c_d_iO1 => BS_blk(i_block_1)%O_c_d(:,iO1)
 O_c_d_iO2 => BS_blk(i_block_2)%O_c_d(:,iO2)
 !$cuf kernel do(1) <<<*,*>>>
 do i_g1=1,BS_n_g_W
   i_g2=g_rot_d(G_m_G_d(g_rot_d(i_g1,iq_W_s)   ,ig_W   ),i_k_s_m1  )
   i_g3=g_rot_d(G_m_G_d(g_rot_d(i_g1,iq_W_s_mq),ig_W_mq),i_kmq_s_m1)
   O1(i_g1)=O_c_d_iO1(i_g2)*PHASE_1
   O2(i_g1)=O_c_d_iO2(i_g3)*PHASE_2
 enddo
 !
#else
 !
 !$omp parallel do default(shared), private(i_g1,i_g2,i_g3)
 do i_g1=1,BS_n_g_W
   i_g2=g_rot(G_m_G(g_rot(i_g1,iq_W_s)   ,ig_W   ),i_k_s_m1  )
   i_g3=g_rot(G_m_G(g_rot(i_g1,iq_W_s_mq),ig_W_mq),i_kmq_s_m1)
   O1(i_g1)=BS_blk(i_block_1)%O_c(i_g2,iO1)*PHASE_1
   O2(i_g1)=BS_blk(i_block_2)%O_c(i_g3,iO2)*PHASE_2
 enddo
 !$omp end parallel do
 !
#endif
 !
 if (i_k_s  >nsym/(i_time_rev+1)) call dev_conjg(O1,(/1,BS_n_g_W/))
 if (i_kmq_s>nsym/(i_time_rev+1)) call dev_conjg(O2,(/1,BS_n_g_W/))
 !
 if (     BS_W_is_diagonal) call V_by_V_pwise(BS_n_g_W,O_times_W,O1,BS_W(:,1,iq_W))
 if (.not.BS_W_is_diagonal) call M_by_V('T',BS_n_g_W,BS_W(:,:,iq_W),O1,O_times_W)
 !
 K_correlation_kernel=Vstar_dot_V(BS_n_g_W,O2,O_times_W)*4._SP*pi
 !
end function K_correlation_kernel
