!
! License-Identifier: GPL
!
! Copyright (C) 2023 The Yambo Team
!
! Authors (see AUTHORS file for details): FP DS CA
!
subroutine FFT_vloc_G_to_R(vloc_full_r)
  !
  use pars,       ONLY:DP,SP,cZERO
  use stderr,     ONLY:intc
  use vlocal,     ONLY:vloc_full
  use fft_m,      ONLY:fft_dim,fftw_plan,fft_g_table,fft_size
  use gvect,      ONLY:ngm
  !
#include<y_memory.h>
  !  
  complex(SP), intent(out) :: vloc_full_r(fft_size)
  !
  ! Work space
  !
  integer                   :: ig,ir
  integer                   :: shape_fft_g_table(2)
  complex(DP), allocatable  :: vloc_aux(:)
  !
  vloc_full_r=cZERO
  !
  YAMBO_ALLOC(vloc_aux,(fft_size))
  !
  shape_fft_g_table=shape(fft_g_table)
  if (ngm/=shape_fft_g_table(1)) then
   !
   call warning('[FFT-vloc] Mismatch in QE ('//trim(intc(ngm))//') '// &
   &            'and Yambo ('//trim(intc(shape_fft_g_table(1)))//') nGvecs')
   call warning('Check Gthresh warning during setup') 
   !
  endif
  !
  vloc_aux=cZERO
  ! 
  !$omp parallel do default(shared), private(ig)
  do ig=1,shape_fft_g_table(1) !  ngm
    vloc_aux(fft_g_table(ig,1))=vloc_full(ig)
    !write(201,*) ig,real(vloc_full(ig)),aimag(vloc_full(ig))
  enddo
  !$omp end parallel do
  !
#if defined _FFTW
  call dfftw_destroy_plan(fftw_plan)
  fftw_plan = 0
  call fft_3d(vloc_aux,fft_dim,1,fftw_plan)
#else
  call fft_3d(vloc_aux,fft_dim,1)
#endif
  !
  ! DEBUG <
  !do ir=1,fft_size
  !  write(101,*) ir,real(vloc_aux(ir)),aimag(vloc_aux(ir))
  !enddo
  ! DEBUG >
  !
  ! Convert to single precision and take real part to remove noise 
  ! but keep complex type even though vloc(r) is real, as required by Vstar_dot_VV
  vloc_full_r = cmplx(real(vloc_aux,SP),0.,kind=SP)
  !
  YAMBO_FREE(vloc_aux)
  !
end subroutine FFT_vloc_G_to_R
