!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
#if defined _FFTW
subroutine fft_1d(c,n,fft_sign,fftw_plan)
 !
 use pars,ONLY:DP
 implicit none
 integer,     intent(in)     :: fft_sign,n
 integer(8),  intent(inout)  :: fftw_plan
 complex(DP), intent(inout)  :: c(n)
 ! 
 ! Work Space
 !
 integer             :: i_sign
 integer , parameter :: FFTW_ESTIMATE=64
 !
 if (fftw_plan==0) then
   if (fft_sign>0) i_sign=+1
   if (fft_sign<0) i_sign=-1
   call dfftw_plan_dft_1d(fftw_plan,n,c,c,i_sign,FFTW_ESTIMATE)
 endif
 !
 call dfftw_execute_dft(fftw_plan,c,c)
 !
 end subroutine
 !
#else
 !
 subroutine fft_1d(c,n,fft_sign)
 !
 use pars,ONLY:DP
 implicit none
 integer, intent(in)        :: n,fft_sign
 complex(DP), intent(inout) :: c(n)
 !
 ! Work Space
 !
 integer  :: i_sign
 real(DP), allocatable :: real_c(:),imag_c(:)
 !
 allocate(real_c(n),imag_c(n))
 !
 if (fft_sign>0) i_sign=+1
 if (fft_sign<0) i_sign=-1
 !        
 real_c(:) = real(c(:))
 imag_c(:) = aimag(c(:))
 !
 call fft_singleton(real_c,imag_c,n,n,n,i_sign)
 !
 c(:)=DCMPLX(real_c(:),imag_c(:))
 !
 deallocate(real_c,imag_c)
 !
 end subroutine
 !
#endif
