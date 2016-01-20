!
! Copyright (C) 2000-2016 A. Marini, M. Gruning and the YAMBO team
!              http://www.yambo-code.org
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
subroutine eval_gradient(f,f_gradient)
 !
 ! The gradient of a periodic function f(r):
 ! 
 ! f(r) = \sum f(G) exp(iGr) => FFT: f(G)
 !
 ! f'(r) = \sum iGf(G) exp(iGr) =           
 !
 ! \sum f'(G) exp(iGr)       => FFT^-1
 !
 use pars,          ONLY:SP,DP,cI
 use FFT_m,         ONLY:fft_size,fft_dim,fftw_plan,fft_g_table
 use R_lattice,     ONLY:g_vec
 use D_lattice,     ONLY:alat
 use wave_func,     ONLY:wf_ng
 !
 implicit none
 real(SP),intent(in)     ::f(fft_size)
 real(SP),intent(out)    ::f_gradient(3,fft_size)
 !
 ! Work Space 
 !
 integer       :: ic,ig
 complex(DP)   :: Vr(fft_size), V3g(3,wf_ng)
 !
 V3g(:,:) = (0._SP,0._SP)
 Vr=f
 !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(Vr,fft_dim,-1,fftw_plan)
#else
 call fft_3d(Vr,fft_dim,-1)
#endif
 !
 forall (ic=1:3,ig=1:wf_ng) V3g(ic,ig)=cI*g_vec(ig,ic)*Vr(fft_g_table(ig,1))/real(fft_size,DP)
 !
 do ic = 1,3
   Vr = (0._SP,0._SP)
   Vr(fft_g_table(1:wf_ng,1)) = V3g(ic,1:wf_ng)
#if defined _FFTW
   call dfftw_destroy_plan(fftw_plan)
   fftw_plan = 0
   call fft_3d(Vr,fft_dim,1,fftw_plan)
#else
   call fft_3d(Vr,fft_dim,1)
#endif
   f_gradient(ic,:) = real(Vr(:),SP)
 enddo
 !
end subroutine eval_gradient
