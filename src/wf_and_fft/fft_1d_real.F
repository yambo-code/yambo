!                         
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
#if defined _FFTW
 !
 subroutine fft_1d_real(real_data,complex_data,n,fft_sign,fftw_plan)
 !
 use pars,ONLY:DP
 implicit none
 !
 integer,    intent(in)    :: n,fft_sign
 integer(8), intent(inout) :: fftw_plan
 real(DP),   intent(inout) :: real_data(n)
 complex(DP),intent(inout) :: complex_data(n/2 +1)
 !
 ! work space
 !
 integer , parameter :: FFTW_ESTIMATE=64
 !
 if(fft_sign<0) then
   if(fftw_plan==0) &
  &  call dfftw_plan_dft_r2c_1d(fftw_plan ,n , real_data, complex_data, FFTW_ESTIMATE)
   call dfftw_execute_dft_r2c(fftw_plan, real_data, complex_data)
 else
   if(fftw_plan==0) &
 &   call dfftw_plan_dft_c2r_1d(fftw_plan ,n , complex_data, real_data, FFTW_ESTIMATE)
   call dfftw_execute_dft_c2r(fftw_plan, complex_data, real_data)
 endif
 !
 end subroutine fft_1d_real
 !
#else
 !
 subroutine fft_1d_real(real_data, complex_data, n,fft_sign)
 !
 use pars,      ONLY:DP
 implicit none
 !
 integer,    intent(in)    :: n,fft_sign
 real(DP),   intent(inout) :: real_data(n)
 complex(DP),intent(inout) :: complex_data(n/2 +1)
 !
 ! Work Space
 !
 real(DP), allocatable    :: wsave(:)
 real(DP), allocatable    :: real_data_store(:)
 integer :: i1
 !
 allocate(wsave(3*n+15))
 call RFFTI(n,wsave)  
 !
 if(fft_sign<0) then
   !      
   allocate(real_data_store(n))
   real_data_store=real_data
   call RFFTF(n, real_data, WSAVE)
   !
   complex_data(1)=DCMPLX(real_data(1),0._DP)
   !
   do i1=2,n/2
     complex_data(i1)=DCMPLX(real_data(2*i1-2),real_data(2*i1-1))
   enddo
   !
   real_data=real_data_store
   deallocate(real_data_store)
   !
 else
   !
   real_data(1)=real(complex_data(1))
   !
   do i1=2,n/2
     real_data(2*i1-2)  =real(complex_data(i1))
     real_data(2*i1-1)  =aimag(complex_data(i1))
   enddo
   !
   call RFFTB(n, real_data, WSAVE)
   !
 endif
 !
 deallocate(wsave)
 !
 end subroutine fft_1d_real
 !
#endif
