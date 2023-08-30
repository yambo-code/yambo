!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
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
subroutine fft_3d(c,n,fft_sign)
 !
 use pars,          ONLY:DP
 implicit none
 integer     :: n(3),fft_sign
 complex(DP) :: c(n(1),n(2),n(3))
 ! 
 ! Work Space
 !
 integer     :: i1,ln(3),ipos
 integer     :: i_sign
 real(DP), allocatable :: zi(:,:,:,:,:)
#if defined _OPENMP
 integer     :: i, j, k
#endif
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
 !
 ! allow for fft_sign to be larger than 1
 i_sign=0
 if (fft_sign > 0 ) i_sign=+1
 if (fft_sign < 0 ) i_sign=-1
 !
#if defined _OPENMP
 !
!$omp parallel do default(shared), private(i,j,k)
 do k = 1, n(3)
   do j = 1, n(2)
     do i = 1, n(1)
       zi(1,i,j,k,ipos)=real(c(i,j,k))
       zi(2,i,j,k,ipos)=aimag(c(i,j,k))
     enddo
   enddo
 enddo
!$omp end parallel do
 
 !
 call fft(n(1),n(2),n(3),ln(1),ln(2),ln(3),zi,i_sign,ipos)
 !
!$omp parallel do default(shared), private(i,j,k)
 do k = 1, n(3)
   do j = 1, n(2)
     do i = 1, n(1)
       c(i,j,k)=cmplx(zi(1,i,j,k,ipos),zi(2,i,j,k,ipos),DP) 
     enddo
   enddo
 enddo
!$omp end parallel do
 !
#else
 !
 zi(1,:n(1),:n(2),:n(3),ipos)=real(c(:,:,:))
 zi(2,:n(1),:n(2),:n(3),ipos)=aimag(c(:,:,:))
 call fft(n(1),n(2),n(3),ln(1),ln(2),ln(3),zi,i_sign,ipos)
 c(:,:,:)=cmplx(zi(1,:n(1),:n(2),:n(3),ipos),zi(2,:n(1),:n(2),:n(3),ipos),DP)
#endif
 !
 deallocate(zi)
 !
end subroutine
