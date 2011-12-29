!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
!=====================================================================
! Driver to 3D FFT: FFTW, Goedecker
!
! fft_sign = +1  : G-space to R-space, output = \sum_G f(G)exp(+iG*R) (FW)
! fft_sign = -1  : R-space to G-space, output = \int_R f(R)exp(-iG*R) (BW)
!
! Note that as the YAMBO convention for the oscillators is
! 
!  <n k | e ^{iq.r} | n' k-q> 
!
! the +1 sign (FW) is used in scatter_Bamp as well.
!
! Note that that inverse operation of 
!
! call fft_3d(wf,fft_dim, 1,bw_plan)
!
! is
!
! call fft_3d(wf/real(fft_size,SP),fft_dim, 1,fw_plan)
!
!=====================================================================
!
#if defined _FFTW
subroutine fft_3d(c,n,fft_sign,fftw_plan)
 !
 use pars,ONLY:DP
 implicit none
 integer     :: fft_sign,n(3)
 integer(8)  :: fftw_plan
 complex(DP) :: c(n(1),n(2),n(3))
 ! 
 ! Work Space
 !
 integer             :: i_sign
 integer , parameter :: FFTW_ESTIMATE=64
 !
 if (fftw_plan==0) then
   if (fft_sign>0) i_sign=+1
   if (fft_sign<0) i_sign=-1
   call dfftw_plan_dft_3d(fftw_plan,n(1),n(2),n(3),c,c,i_sign,FFTW_ESTIMATE)
 endif
 !
 call dfftw_execute_dft(fftw_plan,c,c)
 !
 end subroutine
 !
#else
 !
 subroutine fft_3d(c,n,fft_sign)
 !
 use pars,ONLY:DP
 implicit none
 integer     :: n(3),fft_sign
 complex(DP) :: c(n(1),n(2),n(3))
 ! 
 ! Work Space
 !
 integer     :: i1,ln(3),ipos
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
 !
#endif
