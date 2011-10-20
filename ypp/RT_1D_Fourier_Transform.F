!
! Copyright (C) 2000-2011 C. Attaccalite, A. Marini and the YAMBO team 
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
!
subroutine RT_1D_Fourier_Transform(title,freqs,fw,nw,ft,nt,dt,T0,USE_FFT,I_sign)
 !
 ! Here I calculate 
 !
 ! \tilde{FT}[ft] = \int_{T0}{\infty} ft(t) exp^(i w t) = exp^(i w T0) * FT[ft]   (USE_FFT=.TRUE.)
 !
 ! \tilde{FT}[ft] = \int_{T0}{\infty} ft(t) exp^(i I_sign w t) = exp^(i I_sign w T0) * FT[ft]   (USE_FFT=.FALSE.)
 !
 use pars,           ONLY:SP,DP,cI
 use timing,         ONLY:live_timing
 implicit none
 !
 integer,     intent(in)   :: nw,nt,I_sign
 logical,     intent(in)   :: USE_FFT
 real(SP),    intent(in)   :: dt,T0
 complex(SP), intent(in)   :: freqs(nw)
 character(*),intent(in)   :: title
 complex(DP), intent(in)   :: ft(nt)
 complex(DP), intent(out)  :: fw(nw)
 !
 ! Work Space 
 !
 integer    :: i_w,i_t,fft_sign
 integer(8) :: fftw_plan
 real(SP)   :: Time_reached
 !
 fw=(0._DP,0._DP)
 !
 ! Numerical FT
 !
 if (USE_FFT) then
   !
   fftw_plan= 0
   fft_sign =-1
   !
#if defined _FFTW
    call fft_1d(ft,nt,fft_sign,fftw_plan)
#else
    call fft_1d(ft,nt,fft_sign)
#endif
   !
   fw(:)=conjg(ft(:nw))
   !
 else
   !
   call live_timing('FT'//title//':',nw)
   do i_w = 1, nw
     do i_t = 1, nt
       Time_reached = i_t*dt       
       fw(i_w) = fw(i_w)+ft(i_t)*exp(cI*I_sign*freqs(i_w)*Time_reached)
     enddo
     call live_timing(steps=1)
   enddo
   call live_timing()
   !
 endif
 !
 do i_w = 1, nw
   fw(i_w)=fw(i_w)*dt*exp(cI*freqs(i_w)*T0) 
 enddo
 !
end subroutine RT_1D_Fourier_Transform

