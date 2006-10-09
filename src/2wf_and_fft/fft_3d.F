!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
#if defined FFTW
 subroutine fft_3d(c,n,fft_sign,fftw_plan)
!=========================================
 use pars,ONLY:DP
 implicit none
 integer     :: fft_sign,n(3)
 integer(8)  :: fftw_plan
 complex(DP) :: c(n(1),n(2),n(3))
 integer , parameter :: &
&          FFTW_R2HC=0,FFTW_HC2R=1,FFTW_DHT=2,FFTW_REDFT00=3,&
&          FFTW_REDFT01=4,FFTW_REDFT10=5,FFTW_REDFT11=6,FFTW_RODFT00=7,&
&          FFTW_RODFT01=8,FFTW_RODFT10=9,FFTW_RODFT11=10,FFTW_FORWARD=-1,&
&          FFTW_BACKWARD=+1,FFTW_MEASURE=0,FFTW_DESTROY_INPUT=1,&
&          FFTW_UNALIGNED=2,FFTW_CONSERVE_MEMORY=4,FFTW_EXHAUSTIVE=8,&
&          FFTW_PRESERVE_INPUT=16,FFTW_PATIENT=32,FFTW_ESTIMATE=64,&
&          FFTW_ESTIMATE_PATIENT=128,FFTW_BELIEVE_PCOST=256,&
&          FFTW_NO_DFT_R2HC=512,FFTW_NO_NONTHREADED=1024,FFTW_NO_BUFFERING=2048,&
&          FFTW_NO_INDIRECT_OP=4096,FFTW_ALLOW_LARGE_GENERIC=8192,&
&          FFTW_NO_RANK_SPLITS=16384,FFTW_NO_VRANK_SPLITS=32768,&
&          FFTW_NO_VRECURSE=65536,FFTW_NO_SIMD=131072,FFTW_NO_SLOW=262144,&
&          FFTW_NO_FIXED_RADIX_LARGE_N=524288,FFTW_ALLOW_PRUNING=1048576

 if (fftw_plan==0) call dfftw_plan_dft_3d(fftw_plan,n(1),n(2),n(3),&
&                       c,c,fft_sign,FFTW_ESTIMATE)

 call dfftw_execute_dft(fftw_plan,c,c)
 end subroutine
#else
 subroutine fft_3d(c,n,fft_sign)
!===============================
!
! Computes the forward fourier transform of a complex 
! function c. The fourier transform is returned in c
!
!    fft_sign <0  inverse  fft_sign >0  direct    
!-------------------------------------------------------
 use pars, ONLY:DP
 implicit none
 integer    :: n(3),fft_sign
 complex(DP):: c(n(1),n(2),n(3))
!ws
 integer    :: i1,ln(3),ipos
 real(DP), allocatable :: zi(:,:,:,:,:)
!
! ln(:):memory dimension of Z. ndi must always be greater or
!       equal than ni. On a vector machine, it is recomended
!       to chose ndi=ni if ni is odd and ndi=ni+1 if ni is
!       even to obtain optimal execution speed. On RISC
!       machines ndi=ni is usually fine for odd ni, for even
!       ni one should try ndi=ni+1, ni+2, ni+4 to find the
!       optimal performance.
 ln=n
 do i1=1,3
  if (n(i1)/2*2==n(i1)) ln(i1)=n(i1)+1
 enddo
 allocate(zi(2,ln(1),ln(2),ln(3),2))
 ipos=1
 zi(1,:n(1),:n(2),:n(3),ipos)=real(c(:,:,:))
 zi(2,:n(1),:n(2),:n(3),ipos)=aimag(c(:,:,:))
 call fft(n(1),n(2),n(3),ln(1),ln(2),ln(3),zi,fft_sign,ipos)
 c(:,:,:)=cmplx(zi(1,:n(1),:n(2),:n(3),ipos),zi(2,:n(1),:n(2),:n(3),ipos),DP)
 deallocate(zi)
 end subroutine
#endif
