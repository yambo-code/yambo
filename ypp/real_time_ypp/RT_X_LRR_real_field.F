!
!        Copyright (C) 2000-2015 the YAMBO team
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
! Partialy inspired from Octopus v. 3.1.0
!
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
subroutine RT_X_LRR_real_field(en)
 !
 use YPP,            ONLY:RT_P_t,X_rt,Use_FFT,W_RT_induced_step, &
&                         i_t_start,i_t_end,RED_steps,l_force_min_damp,&
&                         J_steps,J_time_step
 use pars,           ONLY:SP,DP,pi,cI
 use units,          ONLY:HA2EV,FS2AUT
 use electrons,      ONLY:levels
 use memory_m,       ONLY:mem_est
 use rt_ctl,         ONLY:RT_output,RT_IO_JPO_steps
 use real_time,      ONLY:l_RT_induced_field
 use fields,         ONLY:Efield,small_a,i_Probe,theta_function
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels)  :: en
 !
 ! Work Space
 !
 integer  :: i_t,i_w,i_c,iE_err
 character(9) :: FT_title
 logical      :: E_from_FT,l_rm_asympt_limit
 real(SP)     :: W_step,W_reached
 complex(SP), allocatable :: EELS(:,:),EPS(:,:),freqs(:)
 complex(SP)  :: Field_time,Field_freq,E_W_SP
 complex(DP), allocatable :: alpha(:,:),E_w(:),E_t(:)
 !
 call section("=",'Optics')
 !=========================
 !
 call parser('RmAsymLim',l_rm_asympt_limit)
 !
 ! 1D FFT setup
 !--------------
 !
 if(Use_FFT) then
   !
   ! I want to print only the positive frequencies
   ! i_w > energy_steps/2 correspond to the negative ones
   !
   X_rt%energy_steps=(RED_steps+1)/2
   X_rt%energy_range=(/0._SP,W_RT_induced_step*real(RED_steps-1,SP)/2._SP/)
   !
   if(mod(RED_steps,2)==0) X_rt%energy_range=X_rt%energy_range+W_RT_induced_step/2._SP
   !
   allocate(EELS(RED_steps,2),EPS(RED_steps,2),alpha(RED_steps,6),E_w(RED_steps))
   !
 else
   !
   allocate(EELS(X_rt%energy_steps,2),EPS(X_rt%energy_steps,2),alpha(X_rt%energy_steps,6)) 
   allocate(E_w(X_rt%energy_steps))
   !
 endif
 !
 call mem_est("EELS EPS alpha",(/size(EELS),size(EPS),size(alpha)/),(/2*SP,2*SP,2*DP/))
 call mem_est("E_w",(/size(E_w)/),(/2*DP/))
 !
 EELS   =(0._SP,0._SP)
 EPS    =(0._SP,0._SP)
 alpha  =(0._DP,0._DP)
 E_w    =(0._DP,0._DP)
 !
 ! Fourier Transform 
 !====================
 !
 W_step=(X_rt%energy_range(2) - X_rt%energy_range(1))/(X_rt%energy_steps-1)
 !
 allocate(freqs(X_rt%energy_steps))
 freqs=(0._SP,0._SP)
 !
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx((i_w-1)*W_step+X_rt%energy_range(1),0._SP)
 enddo
 !
 ! Electric Field
 !
 iE_err=-1
 do i_w = 1, X_rt%energy_steps
   !
   if(trim(X_rt%damp) /= "NONE")                      Field_freq=freqs(i_w)-(0._SP,1._SP)*X_rt%damp_factor
   if(trim(X_rt%damp) == "NONE".and.l_force_min_damp) Field_freq=freqs(i_w)-(0._SP,1._SP)*W_RT_induced_step
   !
   E_w(i_w)=Efield(i_Probe)%amplitude*small_a(Field_freq,Efield(i_Probe),iE_err)
   E_w(i_w)=E_w(i_w)*exp(cI*freqs(i_w)*(i_t_start-1)*J_time_step)  
   !
   if (iE_err==-2) exit
   !
 enddo 
 !
 E_from_FT=iE_err==-2
 if (E_from_FT) then
   !
   allocate(E_t(J_steps))
   do i_t=1,J_steps
     Field_time=cmplx((i_t-1)*J_time_step-Efield(i_Probe)%t_initial,0._SP)
     E_t(i_t)=Efield(i_Probe)%amplitude* &
&                         (small_a(Field_time,Efield(i_Probe),1)*theta_function(real(Field_time),J_time_step,0)+&
&                          small_a(Field_time,Efield(i_Probe),0)*theta_function(real(Field_time),J_time_step,1))
   enddo
   !
   if(trim(X_rt%damp) == "NONE".and.l_force_min_damp) then
     call RT_damp_it("LORENTZIAN",W_RT_induced_step,E_t(i_t_start-1:i_t_end),RED_steps+1,J_time_step,1)
   else
     call RT_damp_it(X_rt%damp,X_rt%damp_factor,E_t(i_t_start-1:i_t_end),RED_steps+1,J_time_step,1)
   endif
   !
   ! For the field I do the FFT starting always 1 time step before to correctly describe the
   ! theta and the delta function.
   call RT_1D_Fourier_Transform('T2W',' (E field)',freqs,E_w(:),X_rt%energy_steps,&
&                               E_t(i_t_start:i_t_end),RED_steps,J_time_step,&
&                               (i_t_start-1)*J_time_step,USE_FFT,1)
   deallocate(E_t)
   !
 endif
 !
 if(l_rm_asympt_limit) then
   !
   ! Remove the asymptotic limit of RT_P_t
   !
   do i_c=1,3
     RT_P_t(:,i_c)= RT_P_t(:,i_c)-RT_P_t(J_steps,i_c)
   enddo
   !
 endif
 !
 do i_c=1,3
   !
   if (i_c==1) FT_title=' (X axis)'
   if (i_c==2) FT_title=' (Y axis)'
   if (i_c==3) FT_title=' (Z axis)'
   !
   ! Polarization
   !
   call RT_1D_Fourier_Transform('T2W',FT_title,freqs,alpha(:,i_c),X_rt%energy_steps,&
&                               RT_P_t(i_t_start:i_t_end,i_c),RED_steps,J_time_step,&
&                               (i_t_start-1)*J_time_step,USE_FFT,1)
   !
 enddo
 !
 ! a) If the induced field is     computed: E_input=E_ext
 ! b) If the induced field is NOT computed: E_input=E_tot
 !
 ! a) P(w,ic) = E_ext(w,ic') alpha_RID(w,ic',ic) = |E_ext(w)| E_versor(ic') alpha_RID(w,ic,ic')
 ! b) P(w,ic) = E_tot(w,ic') alpha_IRR(w,ic',ic) = |E_tot(w)| E_versor(ic') alpha_IRR(w,ic,ic')
 ! 
 ! a) alpha_RID(w,ic,ic') = ( P(w,ic),E_versor(ic') )/|E_ext(w)| 
 ! b) alpha_IRR(w,ic,ic') = ( P(w,ic),E_versor(ic') )/|E_tot(w)|
 !
 ! Then I use 
 ! a) EPS_{ic,ic'}(w) = (1-4 pi alpha_RID(w,ic))^{-1}
 ! b) EPS_{ic,ic'}(w) =  1+4 pi alpha_IRR(w,ic)
 !
 ! to calculate EELS I use
 !
 ! EPS=-1./EELS
 !
 call RT_output(what="initialize")
 call RT_output(what="open E_frequency eps_along_E eels_along_E")
 do i_w = 1, X_rt%energy_steps
   !
   if(     l_RT_induced_field) EPS(i_w,1) = 1._SP/(1._SP-4._SP*pi*dot_product(Efield(i_Probe)%versor(:),alpha(i_w,:3))/E_w(i_w))
   if(.not.l_RT_induced_field) EPS(i_w,1) =        1._SP+4._SP*pi*dot_product(Efield(i_Probe)%versor(:),alpha(i_w,:3))/E_w(i_w)
   !
   EELS(i_w,:)=-1._SP/EPS(i_w,:)
   !
   W_reached = real(freqs(i_w))
   E_w_SP=E_w(i_w)
   call RT_output('E_frequency' ,VALUEs=(/ aimag(  E_w_SP   ),real(  E_w_SP   )/),E=W_reached*HA2EV)
   call RT_output('eps_along_E' ,VALUEs=(/ aimag(EPS(i_w,1) ),real(EPS(i_w,1) )/),E=W_reached*HA2EV)
   call RT_output('eels_along_E',VALUEs=(/ aimag(EELS(i_w,1)),real(EELS(i_w,1))/),E=W_reached*HA2EV)
   !
 enddo
 call RT_output(what="close E_frequency eps_along_E eels_along_E")
 !
 deallocate(EELS,EPS,alpha,E_w)
 call mem_est("EELS EPS alpha E_w")
 !
end subroutine
