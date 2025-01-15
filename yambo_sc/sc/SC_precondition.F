!
! License-Identifier: GPL
!
! Copyright (C) 2007 The Yambo Team
!
! Authors (see AUTHORS file for details): DV
!
subroutine SC_precondition(delta_rho)
  !
  ! Thomas Fermi precondition for the mixing of the charge density
  ! delta_rho(G)=delta_rho(G)*G**2/(G**2+q_TF**2) 
  !
 use pars,          ONLY: pi,DP,SP
 use R_lattice,     ONLY: bare_qpg
 use D_lattice,     ONLY: DL_vol
 use fft_m,         ONLY: fft_size,fft_dim,fftw_plan,fft_g_table
 use wave_func,     ONLY: wf_ng
 use electrons,     ONLY: nel
 !
 implicit none
 real :: delta_rho(fft_size)
 !
 ! Work Space
 !
 real(SP)    ::  rs
 real(SP)    :: qTF2
 complex(DP) :: D_rho_G(fft_size)
 !
 D_rho_G  = delta_rho
 !
 ! Define Qtf 
 !
 rs = (3._SP*DL_vol/4./pi/nel)**(1._SP/3._SP)
 qtf2 = (12._SP/pi)**(2._SP/3._SP)/rs
 !
 ! FFT call: delta_rho in reciprocal space
 !
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(D_rho_G,fft_dim,-1,fftw_plan)
#else
 call fft_3d(D_rho_G,fft_dim,-1)
#endif
 !
 D_rho_G(fft_g_table(2:wf_ng,1))=D_rho_G(fft_g_table(2:wf_ng,1))*bare_qpg(1,2:wf_ng)**2/(bare_qpg(1,2:wf_ng)**2+qTF2)
 D_rho_G(1) = (0._DP,0._DP)
 !
 ! FFT call: delta_rho in direct space
 !
#if defined _FFTW
 fftw_plan = 0
 call fft_3d(D_rho_G,fft_dim,1,fftw_plan)
#else
 call fft_3d(D_rho_G,fft_dim,1)
#endif
 !
 fftw_plan = 0
 !
 delta_rho = real(D_rho_G,SP)/fft_size
 !
end subroutine SC_precondition
