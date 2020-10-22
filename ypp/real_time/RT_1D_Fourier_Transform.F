!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_1D_Fourier_Transform(mode,FT,RT,I_sign)
 !
 ! Here I calculate:
 !
 ! mode="T2W"
 !
 ! \tilde{FT}[F_t] = \int_{T0}{\inF_t} F_t(t) exp^(i w t) =  FT[F_t]   (Use_FFT=.TRUE.)
 !
 ! \tilde{FT}[F_t] = \int_{T0}{\inF_t} F_t(t) exp^(i I_sign w t) =  FT[F_t]   (Use_FFT=.FALSE.)
 !
 ! This definition follows from the fact that F_t(t) is defined in input only for t>0.
 !
 ! mode="W2T" => no FFT support 
 !
 ! F_t(t) = \int F_w(w) exp^(-i w t) 
 !
 use YPP_real_time,  ONLY:RT_time,Use_FFT,OBS_el,RT_ctl
 use pars,           ONLY:DP,cI,cZERO_DP
 use LIVE_t,         ONLY:live_timing
 !
 implicit none
 !
 character(*),intent(in)   ::mode
 type(OBS_el),intent(inout)::FT
 type(RT_ctl),intent(in)   ::RT
 integer,     intent(in)   ::I_sign
 !
 ! Work Space 
 !
 integer    :: i_w,i_t,fft_sign,i_c
 integer(8) :: fftw_plan
 logical    :: l_live_timing
 !
 l_live_timing=len_trim(FT%title)>0
 !
 ! Time to Frequency
 !===================
 if (mode=="T2W") then
   !
   if (l_live_timing) call live_timing('FT@'//trim(FT%title)//'',RT%W_n_steps*FT%N)
   !
   FT%F_w=cZERO_DP
   !
   ! Numerical FT
   !
   do i_c=1,FT%N
     !
     if (Use_FFT) then
       !
       fftw_plan= 0
       fft_sign =-1
       !
#if defined _FFTW
       call fft_1d(FT%F_t(:,i_c),RT%T_n_steps,fft_sign,fftw_plan)
#else
       call fft_1d(FT%F_t(:,i_c),RT%T_n_steps,fft_sign)
#endif
       !
       FT%F_w(:,i_c)=conjg(FT%F_t(:RT%W_n_steps,i_c))
       !
       if (l_live_timing) call live_timing(steps=RT%W_n_steps)
       !
     else
       !
       do i_w = 1, RT%W_n_steps
         do i_t = 1, RT%T_n_steps-1
           FT%F_w(i_w,i_c) = FT%F_w(i_w,i_c)+FT%F_t(i_t,i_c)*exp(cI*I_sign*FT%W(i_w)*RT_time(i_t))*(RT_time(i_t+1)-RT_time(i_t))
         enddo
         if (l_live_timing) call live_timing(steps=1)
       enddo
       !
     endif
     !
   enddo
   !
   if (l_live_timing) call live_timing()
   !
 endif
 !
 ! Frequency to Time
 !===================
 if (mode=="W2T") then
   !
   FT%F_t=cZERO_DP
   !
   if (l_live_timing) call live_timing('FT'//trim(FT%title)//'',RT%T_n_steps*FT%N)
   do i_c=1,FT%N
     do i_t = 1, RT%T_n_steps
       do i_w = 1, RT%W_n_steps
         FT%F_t(i_t,i_c) = FT%F_t(i_t,i_c)+FT%F_w(i_w,i_c)*(FT%W(2)-FT%W(1))*exp(-cI*FT%W(i_w)*RT_time(i_t))
       enddo
       if (l_live_timing) call live_timing(steps=1)
     enddo
   enddo
   if (l_live_timing) call live_timing()
   !
 endif
 !
end subroutine RT_1D_Fourier_Transform

