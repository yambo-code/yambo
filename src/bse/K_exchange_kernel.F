!
! License-Identifier: GPL
!
! Copyright (C) 2019 The Yambo Team
!
! Authors (see AUTHORS file for details): AF DS
!
! headers
!
#include<dev_defs.h>
!
!
function K_exchange_kernel_resonant(iq, BS_n_g_exch, BS_T_grp_ip, i_Tp, BS_T_grp_ik, i_Tk) result(H_x)
  !==============================
  !
  ! :::EXCHANGE    (resonant):::
  !=============================
  !
  use pars,         ONLY: SP,rZERO,cZERO
  use electrons,    ONLY: n_sp_pol
  use BS,           ONLY: BS_T_group,l_BS_magnons
  use wrapper_omp,  ONLY: Vstar_dot_V_omp
  use R_lattice,    ONLY: bare_qpg, bare_qpg_d
  implicit none
  !
  integer,                  intent(in) :: iq,BS_n_g_exch,i_Tp,i_Tk
  type(BS_T_group), target, intent(in) :: BS_T_grp_ip,BS_T_grp_ik
  complex(SP) :: H_x
  !
  complex(SP), pointer DEV_ATTR :: BS_T_grp_ip_O_x_p(:,:)
  complex(SP), pointer DEV_ATTR :: BS_T_grp_ik_O_x_p(:,:)
  integer :: ig
  real(SP):: H_x_r,H_x_i
  !
  if (l_BS_magnons.and.n_sp_pol==2) then
    H_x=cZERO
    return
  endif
  !
#ifdef _GPU
  !
  BS_T_grp_ip_O_x_p => DEV_VAR(BS_T_grp_ip%O_x)
  BS_T_grp_ik_O_x_p => DEV_VAR(BS_T_grp_ik%O_x)
  !
  H_x_r=rZERO
  H_x_i=rZERO
  !
  !DEV_ACC_DEBUG data present(BS_T_grp_ip_O_x_p,BS_T_grp_ik_O_x_p,bare_qpg)
  !DEV_ACC parallel loop private(ig,H_x) reduction(+:H_x_r,H_x_i)
  !DEV_CUF kernel do(1) <<<*,*>>>
  !DEV_OMPGPU target map(present,alloc:BS_T_grp_ip_O_x_p,BS_T_grp_ik_O_x_p,bare_qpg)
  !DEV_OMPGPU teams loop private(ig,H_x) reduction(+:H_x_r,H_x_i)
  do ig = 1, BS_n_g_exch
    H_x=conjg(BS_T_grp_ip_O_x_p(ig,i_Tp))*BS_T_grp_ik_O_x_p(ig,i_Tk)/DEV_VAR(bare_qpg)(iq,ig)**2
    H_x_r=H_x_r+real(H_x,SP)
    H_x_i=H_x_i+aimag(H_x)
  enddo
  !DEV_OMPGPU end target
  !DEV_ACC_DEBUG end data
  !
  H_x=cmplx(H_x_r,H_x_i,kind=SP)
  !
#else
  H_x=Vstar_dot_V_omp(BS_n_g_exch, BS_T_grp_ip%O_x(:,i_Tp),&
                                   BS_T_grp_ik%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
#endif
  !
end function K_exchange_kernel_resonant
!
!
!
!
function K_exchange_kernel_coupling(iq, BS_n_g_exch, BS_T_grp_ip, i_Tp, BS_T_grp_ik, i_Tk) result(H_x)
  !==============================
  !
  ! :::EXCHANGE    (coupling):::
  !=============================
  !
  use pars,         ONLY: SP,rZERO,cZERO
  use electrons,    ONLY: n_sp_pol
  use BS,           ONLY: BS_T_group,l_BS_magnons
  use wrapper_omp,  ONLY: V_dot_V_omp
  use R_lattice,    ONLY: bare_qpg, minus_G, bare_qpg_d, minus_G_d
  !
  implicit none
  !
  integer,                  intent(in) :: iq, BS_n_g_exch,i_Tp,i_Tk
  type(BS_T_group), target, intent(in) :: BS_T_grp_ip,BS_T_grp_ik
  complex(SP) :: H_x
  !
  complex(SP), pointer DEV_ATTR :: BS_T_grp_ip_O_x_p(:,:)
  complex(SP), pointer DEV_ATTR :: BS_T_grp_ik_O_x_p(:,:)
  integer :: ig
  real(SP):: H_x_r,H_x_i
  !
  if (l_BS_magnons.and.n_sp_pol==2) then
    H_x=cZERO
    return
  endif
  !
#ifdef _GPU
  !
  BS_T_grp_ip_O_x_p => DEV_VAR(BS_T_grp_ip%O_x)
  BS_T_grp_ik_O_x_p => DEV_VAR(BS_T_grp_ik%O_x)
  !
  H_x_r=rZERO
  H_x_i=rZERO
  !
  !DEV_ACC_DEBUG data present(BS_T_grp_ip_O_x_p,BS_T_grp_ik_O_x_p,minus_G,bare_qpg)
  !DEV_ACC parallel loop private(ig,H_x) reduction(+:H_x_r,H_x_i)
  !DEV_CUF kernel do(1) <<<*,*>>>
  !DEV_OMPGPU target map(present,alloc:BS_T_grp_ip_O_x_p,BS_T_grp_ik_O_x_p,minus_G,bare_qpg)
  !DEV_OMPGPU teams loop private(H_x) reduction(+:H_x_r,H_x_i)
  do ig = 1, BS_n_g_exch
    H_x = BS_T_grp_ip_O_x_p( DEV_VAR(minus_G)(ig),i_Tp) *BS_T_grp_ik_O_x_p(ig,i_Tk)/DEV_VAR(bare_qpg)(iq,ig)**2
    H_x_r=H_x_r+real(H_x,SP)
    H_x_i=H_x_i+aimag(H_x)
  enddo
  !DEV_OMPGPU end target
  !DEV_ACC_DEBUG end data
  !
  H_x=cmplx(H_x_r,H_x_i,SP)
  !
#else
  H_x=V_dot_V_omp(BS_n_g_exch, BS_T_grp_ip%O_x( minus_G(1:BS_n_g_exch),i_Tp),&
                               BS_T_grp_ik%O_x(:,i_Tk)/bare_qpg(iq,:BS_n_g_exch)**2)
#endif
 !
end function K_exchange_kernel_coupling
