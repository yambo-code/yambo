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
 use pars,           ONLY:SP,DP,pi,cI,cZERO
 use units,          ONLY:HA2EV,FS2AUT
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:v_norm
 use memory_m,       ONLY:mem_est
 use rt_ctl,         ONLY:RT_output,RT_IO_JPO_steps
 use real_time,      ONLY:l_RT_induced_field,NE_steps,RT_step
 use fields,         ONLY:Efield,small_a_frequency,small_a,theta_function
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels)  :: en
 !
 ! Work Space
 !
 integer  :: i_t,i_w,i_p_dir,i_f_dir,iE_err,i_Probe,i_t_field(2),RED_steps_field,&
&            i_field,n_fields
 character(10):: FT_title
 logical      :: l_rm_asympt_limit
 real(SP)     :: W_step,W_reached,Field_time,outdata(6),E_ver(3,3)
 complex(SP)  :: Pol_proj(3),E_w_SP(3),Field,E_of_t(2),A_of_t(2),theta,delta,E_ver_cmplx(3)
 complex(SP), allocatable :: EELS(:,:),EPS(:,:),freqs(:)
 complex(SP)  :: Field_freq
 complex(DP), allocatable :: Pol(:,:),E_w(:,:),E_t(:,:)
 !
 call section("=",'Optics')
 !=========================
 !
 call parser('RmAsymLim',l_rm_asympt_limit)
 !
 i_Probe=1
 if(trim(Efield(i_Probe)%ef_pol)=="linear"  ) n_fields=1
 if(trim(Efield(i_Probe)%ef_pol)=="circular") n_fields=2
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
   allocate(EELS(RED_steps,3),EPS(RED_steps,3),Pol(RED_steps,3),E_w(RED_steps,n_fields))
   !
 else
   !
   allocate(EELS(X_rt%energy_steps,3),EPS(X_rt%energy_steps,3),Pol(X_rt%energy_steps,3)) 
   allocate(E_w(X_rt%energy_steps,n_fields))
   !
 endif
 !
 call mem_est("EELS EPS Pol",(/size(EELS),size(EPS),size(Pol)/),(/2*SP,2*SP,2*DP/))
 call mem_est("E_w",(/size(E_w)/),(/2*DP/))
 !
 EELS   =(0._SP,0._SP)
 EPS    =(0._SP,0._SP)
 Pol    =(0._DP,0._DP)
 E_w    =(0._DP,0._DP)
 !
 ! Fourier Transform 
 !====================
 !
 W_step=(X_rt%energy_range(2) - X_rt%energy_range(1))/(X_rt%energy_steps-1)
 !
 allocate(freqs(X_rt%energy_steps))
 freqs=cZERO
 !
 do i_w = 1, X_rt%energy_steps
   freqs(i_w)=cmplx((i_w-1)*W_step+X_rt%energy_range(1),0._SP)
 enddo
 !
 ! Electric Field
 !
 iE_err=0
 do i_w = 1, X_rt%energy_steps
   !
   if(trim(X_rt%damp) /= "NONE")                      Field_freq=freqs(i_w)-cI*X_rt%damp_factor
   if(trim(X_rt%damp) == "NONE".and.l_force_min_damp) Field_freq=freqs(i_w)-cI*W_RT_induced_step
   !
   E_w(i_w,1)=Efield(i_Probe)%amplitude*small_a_frequency(Field_freq,Efield(i_Probe),iE_err)
   E_w(i_w,1)=E_w(i_w,1)*exp(cI*freqs(i_w)*(i_t_start-1)*J_time_step)  
   !
   if (iE_err/=0) exit
   !
 enddo 
 !
 iE_err=-1
 if (iE_err/=0) then
   !
   i_t_field(1)=(i_t_start-1)*nint(J_time_step/RT_step)+1
   i_t_field(2)=(i_t_end  -1)*nint(J_time_step/RT_step)+1
   RED_steps_field=NE_steps-i_t_field(1)+1
   !
   allocate(E_t(NE_steps,n_fields))
   do i_field=1,n_fields
     do i_t=1,NE_steps
       Field_time=(i_t-1)*RT_step-Efield(i_Probe)%t_initial
       A_of_t =small_a(Field_time,Efield(i_Probe),0)
       E_of_t =small_a(Field_time,Efield(i_Probe),1)
       theta  =theta_function(Field_time,RT_step,0)
       delta  =theta_function(Field_time,RT_step,1)
       E_t(i_t,i_field)=Efield(i_Probe)%amplitude*(E_of_t(i_field)*theta+A_of_t(i_field)*delta)
     enddo
     !
     if(trim(X_rt%damp) == "NONE".and.l_force_min_damp) then
       call RT_damp_it("LORENTZIAN",W_RT_induced_step,E_t(i_t_field(1)-1:i_t_field(2),i_field),RED_steps_field+1,RT_step,1)
     else
       call RT_damp_it(X_rt%damp,X_rt%damp_factor,    E_t(i_t_field(1)-1:i_t_field(2),i_field),RED_steps_field+1,RT_step,1)
     endif
     !
     if (i_field==1) FT_title='E1'
     if (i_field==2) FT_title='E2'
     !
     ! For the field I do the FFT starting always 1 time step before to correctly describe the
     ! theta and the delta function.
     call RT_1D_Fourier_Transform('T2W',FT_title,freqs,E_w(:,i_field),X_rt%energy_steps,&
&                               E_t(i_t_field(1):i_t_field(2),i_field),RED_steps_field,RT_step,&
&                              (i_t_field(1)-1)*RT_step,USE_FFT,1)
     !
   enddo
   !
   deallocate(E_t)
   !
 endif
 !
 if(l_rm_asympt_limit) then
   !
   ! Remove the asymptotic limit of RT_P_t
   !
   do i_p_dir=1,3
     RT_P_t(:,i_p_dir)= RT_P_t(:,i_p_dir)-RT_P_t(J_steps,i_p_dir)
   enddo
   !
 endif
 !
 do i_p_dir=1,3
   !
   if (i_p_dir==1) FT_title='P (X axis)'
   if (i_p_dir==2) FT_title='P (Y axis)'
   if (i_p_dir==3) FT_title='P (Z axis)'
   !
   ! Polarization
   !
   call RT_1D_Fourier_Transform('T2W',FT_title,freqs,Pol(:,i_p_dir),X_rt%energy_steps,&
&                               RT_P_t(i_t_start:i_t_end,i_p_dir),RED_steps,J_time_step,&
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
 !
 if(Efield(i_Probe)%ef_pol=="linear"  ) then
   E_ver(:,1) = Efield(i_Probe)%versor
   E_ver(:,2)=(/0,1,0/)
   E_ver(:,3)=(/0,0,1/)
   !
   if( (1._SP-dot_product( E_ver(:,1),E_ver(:,2) ))<1.E-5) E_ver(:,2)=(/1,0,0/)
   E_ver(:,2) = E_ver(:,2)-dot_product( E_ver(:,1),E_ver(:,2) )*E_ver(:,1)
   E_ver(:,2) = E_ver(:,2)/v_norm(E_ver(:,2))
   !
   if( (1._SP-dot_product( E_ver(:,1),E_ver(:,3) ))<1.E-5) E_ver(:,3)=(/1,0,0/)
   E_ver(:,3) = E_ver(:,3)-dot_product( E_ver(:,1),E_ver(:,3) )*E_ver(:,1) &
&                         -dot_product( E_ver(:,2),E_ver(:,3) )*E_ver(:,2)
   E_ver(:,3) = E_ver(:,3)/v_norm(E_ver(:,3))
   !
 endif
 !
 if(Efield(i_Probe)%ef_pol=="circular"  ) then
   E_ver(:,1) = 0._SP
   E_ver(:,2) = Efield(i_Probe)%versor
   E_ver(:,3) = Efield(i_Probe)%versor_circ
 endif
 !
 do i_w = 1, X_rt%energy_steps
   !
   if(Efield(i_Probe)%ef_pol=="linear"  ) Field = cmplx(E_w(i_w,1),kind=SP)
   !
   if(Efield(i_Probe)%ef_pol=="circular") then
     E_w_SP = cZERO
     E_w_SP = Efield(i_Probe)%versor*cmplx(E_w(i_w,1),kind=SP)+Efield(i_Probe)%versor_circ*cmplx(E_w(i_w,2),kind=SP)
     E_ver(:,1)=abs(E_w_SP/v_norm(abs(E_w_SP)))
   endif
   !
   do i_p_dir=1,3
     !
     Pol_proj(i_p_dir)=dot_product(E_ver(:,i_p_dir),cmplx(Pol(i_w,:),kind=SP) )
     if(Efield(i_Probe)%ef_pol=="circular") Field=dot_product(E_ver(:,i_p_dir),E_w_SP )
     !
     if(     l_RT_induced_field) EELS(i_w,i_p_dir) = 4._SP*pi*Pol_proj(i_p_dir)/Field
     if(.not.l_RT_induced_field) EPS( i_w,i_p_dir) = 4._SP*pi*Pol_proj(i_p_dir)/Field
     !
   enddo
   !
 enddo
 !
 if(     l_RT_induced_field) then
   EELS(:,1) = -1._SP+EELS(:,1)
   EPS(:,:)  = -1._SP/EELS(:,:)
 else
   EPS(:,1)  =  1._SP+EPS(:,1)
   EELS(:,:) = -1._SP/EPS(:,:)
 endif
 !
 do i_w = 1, X_rt%energy_steps
   !
   W_reached = real(freqs(i_w))*HA2EV
   !
   E_w_SP = cZERO
   if(Efield(i_Probe)%ef_pol=="linear"  ) E_w_SP=Efield(i_Probe)%versor*cmplx(E_w(i_w,1),kind=SP)
   if(Efield(i_Probe)%ef_pol=="circular") E_w_SP=Efield(i_Probe)%versor     *cmplx(E_w(i_w,1),kind=SP) &
&                                               +Efield(i_Probe)%versor_circ*cmplx(E_w(i_w,2),kind=SP)
   !
   outdata(1:3)=(/ real(E_w_SP(1) ),real(E_w_SP(2) ),real(E_w_SP(3) ) /)
   outdata(4:6)=(/ aimag(E_w_SP(1)),aimag(E_w_SP(2)),aimag(E_w_SP(3)) /)
   call RT_output('E_frequency',VALUEs=outdata,E=W_reached)
   outdata(1:3)=(/ aimag(EPS(i_w,1) ),aimag(EPS(i_w,2) ),aimag(EPS(i_w,3) ) /)
   outdata(4:6)=(/ real( EPS(i_w,1) ),real( EPS(i_w,2) ),real( EPS(i_w,3) ) /)
   call RT_output('eps_along_E' ,VALUEs=outdata ,E=W_reached)
   outdata(1:3)=(/ aimag(EELS(i_w,1) ),aimag(EELS(i_w,2) ),aimag(EELS(i_w,3) ) /)
   outdata(4:6)=(/ real( EELS(i_w,1) ),real( EELS(i_w,2) ),real( EELS(i_w,3) ) /)
   call RT_output('eels_along_E',VALUEs=outdata ,E=W_reached)
   !
 enddo
 call RT_output(what="close E_frequency eps_along_E eels_along_E")
 !
 deallocate(EELS,EPS,Pol,E_w)
 call mem_est("EELS EPS Pol E_w")
 !
end subroutine
