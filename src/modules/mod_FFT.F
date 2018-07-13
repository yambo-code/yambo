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
module FFT_m
 use pars,         ONLY: SP
 implicit none
 !
 real(SP)             :: fft_norm
 integer              :: fft_size
 integer              :: fft_dim(3)
 integer              :: fft_multiplier(3)
 integer              :: fft_dim_loaded(3)
 integer, allocatable :: fft_g_table(:,:)
 integer, allocatable :: fft_rot_r(:,:)
 integer, allocatable :: fft_rot_r_inv(:)
#ifdef _CUDA
 integer, allocatable, device :: fft_g_table_d(:,:)
 integer, allocatable, device :: fft_rot_r_d(:,:)
 integer, allocatable, device :: fft_rot_r_inv_d(:)
#endif
 !
 ! FFTW support
 !
 integer(8)  :: fftw_plan !(FW) +1 sign: G-space to R-space & oscillators
                          !(BW) -1 sign: R-space to G-space
#ifdef _CUDA
 integer     :: cufft_plan !(FW) +1 sign: G-space to R-space & oscillators
                           !(BW) -1 sign: R-space to G-space
#endif

 contains
   !
   integer function modx(i,j)
     !
     !  Calculate a function similar to MOD:
     !  modx(i,j) = mod (i,j) i>=0
     !            = mod ( mod(i,j)+j , j) i<0
     !  Thus 
     !   modx(3,2)=1 
     !   modx(-1,2)=1.
     !  This is different from MOD for i negative.
     !
     implicit none
     integer i,j
     if (i>=0) then
       modx = mod(i,j)
     else
       modx = mod( mod(i,j) + j , j)
     endif
     !
   end function
   !
end module
