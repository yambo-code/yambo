!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine fft_free()
 !
 use FFT_m,    ONLY:fft_size,fft_dim,fft_dim_loaded,fft_g_table,&
&                   fft_rot_r,fft_rot_r_inv,fft_multiplier
#if defined _FFTW
 use FFT_m,    ONLY:fftw_plan
#endif
#if defined _CUDA
 use FFT_m,    ONLY:cufft_plan,fft_g_table_d,fft_rot_r_d,fft_rot_r_inv_d
 use cufft
#endif
 !
#include<memory.h>
 !
 integer :: ierr
 !
 fft_size=0
 fft_dim=0
 fft_dim_loaded=0
 fft_multiplier=1
 YAMBO_FREE(fft_g_table)
 YAMBO_FREE(fft_rot_r)
 YAMBO_FREE(fft_rot_r_inv)
#if defined _FFTW
 call dfftw_destroy_plan(fftw_plan)
 fftw_plan=0
#endif
#if defined _CUDA
 if(allocated(fft_g_table_d))   deallocate(fft_g_table_d)
 if(allocated(fft_rot_r_d))     deallocate(fft_rot_r_d)
 if(allocated(fft_rot_r_inv_d)) deallocate(fft_rot_r_inv_d)
 ierr=cufftDestroy(cufft_plan) 
 cufft_plan=0
#endif
 !
end subroutine fft_free
