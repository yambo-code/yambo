!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine plot_and_project_1D(V,direction)
 !
 use pars,        ONLY:SP,DP,pi,cI
 use YPPm,        ONLY:output_fname
 use FFT_m,       ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 use wave_func,   ONLY:wf_ng
 use R_lattice,   ONLY:g_vec
 use D_lattice,   ONLY:alat
 use LIVE_t,      ONLY:live_timing
 use vec_operate, ONLY:v_norm
 use com,         ONLY:msg
 !
 implicit none
 real(SP)   :: V(fft_size,3),direction(3)
 ! 
 ! Work Space...
 !
 integer            :: i_c,i_g,i_r,n_r
 integer, parameter :: n_r_steps=10000
 real(SP)           :: G(3),R(3),delta_R
 complex(SP)        :: V_to_plot(3)
 complex(DP)        :: V_DP(fft_size,3)
 !
 ! BACKWARD FFT (R->G)
 !=====================
 !
 fftw_plan=0
 !
 do i_c=1,3
   !
   V_DP(:,i_c)=V(:,i_c)
   !
#if defined _FFTW
   call fft_3d(V_DP(:,i_c),fft_dim,-1,fftw_plan)
#else
   call fft_3d(V_DP(:,i_c),fft_dim,-1)
#endif
   !
 enddo
 !
 delta_R=4.*maxval(alat)/float(n_r_steps)
 !
 call msg('o '//output_fname,'#',(/"Proj.[au]","J(x) [au]","J(y) [au]","J(z) [au]"/),INDENT=0,USE_TABS=.true.)
 call msg('o '//output_fname,'#')
 !
 call live_timing('1D projection',n_r_steps)
 !
 do i_r=1,n_r_steps
   !
   R(:)=(float(i_r)*delta_R-2.*maxval(alat))*direction(:)/v_norm(direction)
   !
   V_to_plot=0._SP
   !
   do i_g=1,wf_ng
     !
     G=g_vec(i_g ,:)*2.0_SP*pi/alat(:)
     !
     V_to_plot(:)=V_to_plot(:)+V_DP(fft_g_table(i_g,1),:)*exp(cI*dot_product(G,R))
     !
   enddo
   !
   call live_timing(steps=1)
   !
   call msg('o '//output_fname,'',(/dot_product(R,direction)/v_norm(direction),&
&                                   real(V_to_plot(:))/),INDENT=-2,USE_TABS=.true.)
   !
 enddo
 !
 call live_timing( )
 !
end subroutine
