!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_split_Polarization(nh,nt,W,X,P,what)
 !
 use pars,           ONLY:SP,cI,schlen,DP
 use YPP_real_time,  ONLY:l_RT_X_inversion,l_skip_pol_and_curr_IO,RT_conf
 use units,          ONLY:HA2EV,AUT2FS
 use RT_control,     ONLY:RT_output
 use real_time,      ONLY:RT_P,RAD_LifeTime,Phase_LifeTime
 use stderr,         ONLY:intc
 use LIVE_t,         ONLY:live_timing
 !
 ! Fourier
 !
#include<memory.h>
 !
 integer,     intent(in)   :: nt,nh
 real(SP),    intent(inout):: P(nt,3),W(nh)
 complex(SP), intent(inout):: X(nh,3)
 character(*),intent(in)   :: what
 !
 ! Work Space
 !
 integer          :: i_n,i_t,i_c,nh_min
 real(SP)         :: P_num(nh,nt,3),T_i
 character(schlen):: P_file_name
 ! 
 ! Fourier 
 ! 
 integer                 :: i_w,iT_o
 complex(SP),allocatable :: freqs(:)
 complex(DP),allocatable :: P_DP(:,:),P_W(:)
 real(SP)                :: T_o,P_W_SP(2)
 real(SP),   parameter   :: F_damp=0.2/HA2EV
 character(schlen)       :: FT_title
 !
 nh_min=1
 if (what=="subtract") nh_min=nh
 if (what=="dump".and..not.l_RT_X_inversion) call live_timing('Partial P(t):',nt*nh)
 !
 do i_n=nh_min,nh
   !
   if (what=="dump") then
     P_file_name="dP_order_"//trim(intc(i_n-1))
     if (.not.l_skip_pol_and_curr_IO) call RT_output("open "//trim(P_file_name) )
   endif
   !
   do i_t=1,nt
     !
     T_i = (i_t-1)*RT_conf%delta_T
     P_num(i_n,i_t,:)=X(i_n,:)*exp(cI*W(i_n)*T_i)+conjg(X(i_n,:))*exp(-cI*W(i_n)*T_i)
     if (W(i_n)<1.E-5) P_num(i_n,i_t,:)=real(X(i_n,:))
     !
     if (what=="dump".and..not.l_skip_pol_and_curr_IO) then
       do i_c=1,3
         RT_P(i_c) = P(i_t,i_c)-sum( P_num(:i_n,i_t,i_c) )
       enddo
       call RT_output(trim(p_file_name),time=(i_t-1)*RT_conf%delta_T*AUT2FS)
     endif
     !
     if (what=="subtract") P(i_t,:)=P(i_t,:)-P_num(i_n,i_t,:)
     if (what=="dump".and..not.l_rt_x_inversion) call live_timing(steps=1)
     !
   enddo
   !
   if (what=="dump".and..not.l_rt_x_inversion) call RT_output("close "//trim(p_file_name))
   !
 enddo
 !
 if (what=="dump".and..not.l_rt_x_inversion) call live_timing()
 if (what/="dump") return
 !
 ! Fourier Transformation
 !========================
 !
 T_o=5.*max(RAD_LifeTime,Phase_LifeTime)
 iT_o=nint(T_o/RT_conf%delta_T)+1
 !
 YAMBO_ALLOC(P_DP,(nt,3))
 YAMBO_ALLOC(P_W,(RT_conf%W_n_steps))
 YAMBO_ALLOC(freqs,(RT_conf%W_n_steps))
 !
 do i_w = 1, RT_conf%W_n_steps
   freqs(i_w)=cmplx((i_w-1)*(RT_conf%W_range(2)-RT_conf%W_range(1))/RT_conf%W_n_steps+RT_conf%W_range(1),0._SP)
 enddo
 !
 do i_n=0,nh,1
   !
   if (i_n==0) P_file_name="Polarization_W"
   if (i_n/=0) P_file_name="dP_W_order_"//trim(intc(i_n-1))
   call RT_output("open "//trim(P_file_name) )
   !
   do i_t=1,nt
     do i_c=1,3
       if (i_n==0) P_DP(i_t,i_c)=P(i_t,i_c)
       if (i_n/=0) P_DP(i_t,i_c)=P(i_t,i_c)-sum(P_num(:i_n,i_t,i_c))
     enddo
   enddo
   call RT_damp_it("LORENTZIAN",F_damp,P_DP,3)
   !
   if (i_n==0) FT_title="P(t)"
   if (i_n/=0) FT_title="P(t)-Sum[P_harm^(n<"//trim(intc(i_n))//"])"
   !
   call RT_1D_Fourier_Transform('T2W'," "//trim(FT_title),freqs,P_W,RT_conf%W_n_steps,P_DP(iT_o:,1),RT_conf%T_n_steps-iT_o+1,&
&                               RT_conf%delta_T,(iT_o-1)*RT_conf%delta_T,.FALSE.,1)
   !
   do i_w=1,RT_conf%W_n_steps
     !
     P_W(i_w)=cI*exp(-cI*(freqs(i_w)+cI*F_damp)*T_o)*P_W(i_w)
     !
     P_W_SP=(/aimag(P_W(i_w)),real(P_W(i_w))/)
     !
     call RT_output(trim(P_file_name),VALUEs=P_W_SP,E=real(freqs(i_w))*HA2EV)
     !
   enddo
   !
   call RT_output("close "//trim(P_file_name))
   !
 enddo
 !
 YAMBO_FREE(P_DP)
 YAMBO_FREE(P_W)
 YAMBO_FREE(freqs)
 !
end subroutine RT_split_Polarization
