!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
module FFT_m
 use pars,         ONLY: SP
 real(SP)             :: fft_norm
 integer              :: fft_size
 integer              :: fft_dim(3)
 integer              :: fft_dim_loaded(3)
 integer, allocatable :: fft_g_table(:,:)
 integer, allocatable :: fft_rot_r(:,:)
 !
 ! FFTW support
 !
 integer(8)  :: fftw_plan !(FW) +1 sign: G-space to R-space & oscillators
                          !(BW) -1 sign: R-space to G-space
end module
