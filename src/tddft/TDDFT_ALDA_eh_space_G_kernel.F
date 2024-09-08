!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
function TDDFT_ALDA_eh_space_G_kernel(BS_n_g_fxc, BS_T_grp_ip, i_Tp, BS_T_grp_ik, i_Tk) result(H_x)
 !
 ! Calculates the F_xc scattering 
 !
 !  (ic(1),ik(1),is(1)) --<--:...:--<-- (ic(2),ik(2),is(2))
 !                              :Fxc:
 !  (iv(1),ik(3),is(3)) -->--:...:-->-- (iv(2),ik(4),is(4))
 !
 use pars,           ONLY:SP,pi,cZERO
 use FFT_m,          ONLY:fft_size
 use BS,             ONLY:BS_T_group
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 use electrons,      ONLY:n_spinor,spin_occ
 !
 implicit none
 !
 integer,                  intent(in) :: BS_n_g_fxc,i_Tp,i_Tk
 type(BS_T_group), target, intent(in) :: BS_T_grp_ip,BS_T_grp_ik
 !
 complex(SP) :: H_x
 !
 integer     :: i_sp_xc,o_sp_xc,i_sp_c,i_sp_v
 !
 i_sp_c=BS_T_grp_ik%table(i_Tk,4)
 i_sp_v=BS_T_grp_ik%table(i_Tk,5)
 !
 if (n_spinor==1) then
   H_x=Vstar_dot_V_omp(BS_n_g_fxc,BS_T_grp_ik%O_tddft_L(:,i_Tk,  1   ,  1   ),&
   &                              BS_T_grp_ip%O_tddft_R(:,i_Tp,i_sp_c,i_sp_v) )
 else if (n_spinor==2) then
   H_x=cZERO
   do i_sp_xc=1,n_spinor
     do o_sp_xc=1,n_spinor
       H_x=H_x+Vstar_dot_V_omp(BS_n_g_fxc,BS_T_grp_ik%O_tddft_L(:,i_Tk,i_sp_xc,o_sp_xc),&
       &                                  BS_T_grp_ip%O_tddft_R(:,i_Tp,i_sp_xc,o_sp_xc) )
     enddo
   enddo
 endif
 !
 ! tddft_alda_r_space should be mutiplied by X, it is mutiplied by Co in K
 ! X  =      spin_occ/DL_vol/Nq 
 ! Co = 4*pi*spin_occ/DL_vol/Nq
 ! --> X/Co = 1/4/pi
 !
 H_x=H_x/4._SP/pi
 !
end function TDDFT_ALDA_eh_space_G_kernel
