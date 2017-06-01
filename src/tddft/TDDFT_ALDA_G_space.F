!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine TDDFT_ALDA_G_space(E,k)
 !
 use pars,          ONLY:DP,SP
 use TDDFT,         ONLY:FXC_n_g_corr,FXC
 use com,           ONLY:msg
 use FFT_m,         ONLY:fft_size,fftw_plan,fft_dim,fft_g_table
 use electrons,     ONLY:levels,n_spin
 use R_lattice,     ONLY:bz_samp,G_m_G
 use wave_func,     ONLY:WF
 use xc_functionals,ONLY:F_xc,magn,XC_potential_driver
 use global_XC,     ONLY:WF_kind,WF_xc_functional
 use interfaces,    ONLY:WF_load,WF_free,eval_G_minus_G
 !
#include<memory.h>
 !
 type(levels) ::E
 type(bz_samp)::k
 ! 
 ! Work Space
 !
 integer                  :: i1,i2
 complex(DP), allocatable :: F_xc_DP(:)
 !
 if (allocated(FXC)) return
 !
 ! Fxc RL size setup looking at the max index of G-G'
 !
 if (FXC_n_g_corr/= eval_G_minus_G(FXC_n_g_corr,0) ) then
   FXC_n_g_corr   = eval_G_minus_G(FXC_n_g_corr,0)
   call msg('s','[TDDFT] ALDA Fxc size forced by symmetry to be :',FXC_n_g_corr)
 endif
 !
 ! WF distributed & load
 !=======================
 !
 call WF_load(WF,0,1,(/1,E%nbm/),(/1,k%nibz/),title='-Rho')
 !
 ! ALDA functional
 !
 YAMBO_ALLOC(F_xc,(fft_size,n_spin,n_spin))
 YAMBO_ALLOC(F_xc_DP,(fft_size))
 if (n_spin>1) then
   YAMBO_ALLOC(magn,(fft_size,3))
 endif
 call XC_potential_driver(E,k,WF_KIND,WF_xc_functional,2)
 do i1=1,fft_size
   F_xc_DP(i1)=sum(cmplx(F_xc(i1,:,:),kind=DP))
 enddo
 !
 ! need to reset the plan to do a BACKWARD FFT (R->G)
 !
 fftw_plan=0
#if defined _FFTW
 call fft_3d(F_xc_DP,fft_dim,-1,fftw_plan)
#else
 call fft_3d(F_xc_DP,fft_dim,-1)
#endif
 !
 ! Fxc
 !
 YAMBO_ALLOC(FXC,(FXC_n_g_corr,FXC_n_g_corr,1))
 do i1=1,FXC_n_g_corr
   do i2=1,FXC_n_g_corr
     FXC(i1,i2,1)=cmplx(F_xc_DP( fft_g_table( G_m_G(i1,i2),1) ),kind=SP)/real(fft_size,SP)
   enddo
 enddo
 !
 ! Clean
 !
 YAMBO_FREE(F_xc)
 YAMBO_FREE(F_xc_DP)
 YAMBO_FREE(magn)
 call WF_free(WF)
 !
end subroutine
