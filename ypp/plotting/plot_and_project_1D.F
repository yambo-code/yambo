!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine plot_and_project_1D(V,direction)
 !
 use pars,        ONLY:SP,DP,pi,cI
 use YPP,         ONLY:output_fname
 use FFT_m,       ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 use wave_func,   ONLY:wf_ng
 use R_lattice,   ONLY:g_vec
 use D_lattice,   ONLY:alat,a
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
