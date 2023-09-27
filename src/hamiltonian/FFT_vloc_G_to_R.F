!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP, DS, CA
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
subroutine FFT_vloc_G_to_R(vloc_full_r)
  !
  use pars,       ONLY:DP,SP,cZERO
  use vlocal,     ONLY:vloc_full
  use fft_m,      ONLY:fft_dim,fftw_plan,fft_g_table,fft_size
  use gvect,      ONLY:ngm
  !
#include<memory.h>
  !  
  complex(SP), intent(out) :: vloc_full_r(fft_size)
  !
  ! Work space
  !
  integer                   :: ig,ir
  complex(DP), allocatable  :: vloc_aux(:)
  !
  vloc_full_r=cZERO
  !
  YAMBO_ALLOC(vloc_aux,(fft_size))
  !
  if (ngm/=size(fft_g_table)) then
   !
   call errore('[FFT-vloc] Mismatch in Gvecs number between QE and Yambo.&
& Check Gthresh warning during setup') 
   !
  endif
  !
  vloc_aux=cZERO
  ! 
  !$omp parallel do default(shared), private(ig)
  do ig=1,ngm
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
  vloc_full_r = real(cmplx(vloc_aux,kind=SP))
  !
  YAMBO_FREE(vloc_aux)
  !
end subroutine FFT_vloc_G_to_R
