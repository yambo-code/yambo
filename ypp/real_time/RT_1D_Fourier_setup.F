!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_1D_Fourier_setup(FT,RT,N)
 !
 use YPP_real_time,  ONLY:OBS_el,Use_FFT,RT_ctl
 use pars,           ONLY:SP,cZERO_DP,cZERO
 !
#include<memory.h>
 !
 type(OBS_el) :: FT
 type(RT_ctl) :: RT
 integer      :: N,i_w
 !
 if (Use_FFT) then
   !
   ! I want to print only the positive frequencies
   ! i_w > energy_steps/2 correspond to the negative ones
   !
   RT%W_n_steps=(RT%T_n_steps+1)/2
   RT%X_W_range=(/0._SP,RT%Field_damp_factor*real(RT%T_n_steps-1,SP)/2._SP/)
   !
   if(mod(RT%T_n_steps,2)==0) RT%X_W_range=RT%X_W_range+RT%Field_damp_factor/2._SP
   !
   YAMBO_ALLOC(FT%F_w,(RT%T_n_steps,N))
   !
 else
   !
   YAMBO_ALLOC(FT%F_w,(RT%W_n_steps,N))
   !
 endif
 !
 FT%F_w=cZERO_DP
 FT%N  =N
 !
 if (.not.allocated(FT%F_t)) then
   YAMBO_ALLOC(FT%F_t,(RT%T_n_steps,N))
   FT%F_t=cZERO_DP
 endif
 !
 RT%delta_W=(RT%X_W_range(2) - RT%X_W_range(1))/(RT%W_n_steps-1)
 !
 YAMBO_ALLOC(FT%W,(RT%W_n_steps))
 FT%W=cZERO
 !
 do i_w = 1, RT%W_n_steps
   FT%W(i_w)=cmplx((i_w-1)*RT%delta_W+RT%X_W_range(1),0._SP)
 enddo
 !
end subroutine RT_1D_Fourier_setup
