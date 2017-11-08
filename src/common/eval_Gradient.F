!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG AF
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
subroutine eval_Gradient(f,f_gradient,spinor_size,f_type)
 !
 ! The gradient of a periodic function f(r):
 ! 
 ! f(r) = \sum f(G) exp(iGr) => FFT: f(G)
 !
 ! f'(r) = \sum iGf(G) exp(iGr) =           
 !
 ! \sum f'(G) exp(iGr)       => FFT^-1
 !
 use pars,          ONLY:SP,DP,cI,cZERO,cZERO_DP
 use com,           ONLY:error
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table
#if defined _FFTW
 use FFT_m,         ONLY:fftw_plan
#endif
 use R_lattice,     ONLY:g_vec,ng_vec
 use wave_func,     ONLY:wf_ng
 !
#include<memory.h>
 !
 integer,          intent(in) ::spinor_size
 complex(SP),      intent(in) ::f(fft_size,spinor_size)
 complex(SP),      intent(out)::f_gradient(fft_size,spinor_size,3)
 character(len=*), intent(in) ::f_type       ! "wave", "density" or "potential"
 !
 ! Work Space 
 !
 integer :: ng_,ic,ig,i_spinor
 complex(DP), allocatable :: Vr(:), V3g(:,:)
 !
 select case( trim(f_type) )
 case( "wave" )
   ng_=min(wf_ng,size(fft_g_table,1))
 case( "density")
   ng_=min(ng_vec,size(fft_g_table,1))
 case( "potential")
   ng_=min(ng_vec,size(fft_g_table,1))
 case default
   call error(' [FFT] unknown f_type when computing gradient')
 end select
 !
 YAMBO_ALLOC(Vr,(fft_size))
 YAMBO_ALLOC(V3g,(ng_,3))
 V3g(:,:) = cZERO_DP
 !
 f_gradient = cZERO
 !
 do i_spinor=1,spinor_size
   !
   Vr=cmplx(f(:,i_spinor),kind=DP)
   !
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan = 0
 call fft_3d(Vr,fft_dim,-1,fftw_plan)
#else
 call fft_3d(Vr,fft_dim,-1)
#endif
   !
   forall (ic=1:3,ig=1:ng_) V3g(ig,ic)=cI*g_vec(ig,ic)*Vr(fft_g_table(ig,1))/real(fft_size,DP)
   !
   do ic = 1,3
     Vr = cZERO_DP
     Vr(fft_g_table(1:ng_,1)) = V3g(1:ng_,ic)
#if defined _FFTW
   call dfftw_destroy_plan(fftw_plan)
   fftw_plan = 0
   call fft_3d(Vr,fft_dim,1,fftw_plan)
#else
   call fft_3d(Vr,fft_dim,1)
!   call fft_3d(Vr,fft_dim,2) ???
#endif
     f_gradient(:,i_spinor,ic) = cmplx(Vr(:),kind=SP)
   enddo
   !
 enddo
 !
 YAMBO_FREE(Vr)
 YAMBO_FREE(V3g)
 !
end subroutine eval_Gradient
