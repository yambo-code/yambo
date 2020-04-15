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
! Partialy inspired from Octopus v. 3.1.0
!
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
subroutine RT_X_LRR_real_field(en)
 !
 use YPP_real_time,  ONLY:RT_conf,Use_FFT,l_force_min_damp,RT_time,Pol,ElField,FT_free
 use pars,           ONLY:SP,pi,cI,cZERO
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:v_norm
 use RT_control,     ONLY:RT_output
 use real_time,      ONLY:l_RT_induced_field
 use fields,         ONLY:Efield,small_a_frequency,small_a
 use functions,      ONLY:theta_function
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(levels)  :: en
 !
 ! Work Space
 !
 integer      :: i_t,i_w,i_p_dir,iE_err,i_Probe,i_field,n_fields,NW
 logical      :: l_rm_asympt_limit
 real(SP)     :: W_reached,Field_time,outdata(6),E_ver(3,3)
 complex(SP)  :: Pol_proj(3),E_w_SP(3),Field,E_of_t(2),A_of_t(2),theta,delta,Field_freq
 complex(SP), allocatable :: EELS(:,:),EPS(:,:)
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
 call RT_1D_Fourier_setup(ElField,RT_conf,n_fields) 
 call RT_1D_Fourier_setup(Pol,RT_conf,3) 
 !
 NW=RT_conf%W_n_steps
 if (Use_FFT) NW=RT_conf%T_n_steps
 !
 YAMBO_ALLOC(EELS,(NW,3))
 YAMBO_ALLOC(EPS,(NW,3))
 EELS   =(0._SP,0._SP)
 EPS    =(0._SP,0._SP)
 !
 ! Electric Field...
 !-------------------
 !
 ElField%title="Field"
 ElField%N    =n_fields
 !
 !... definition
 !
 do i_t=1,RT_conf%T_n_steps
   !
   Field_time=RT_time(i_t)-Efield(i_Probe)%t_initial
   A_of_t =small_a(Field_time,Efield(i_Probe),0)
   E_of_t =small_a(Field_time,Efield(i_Probe),1)
   theta  =theta_function(Field_time,RT_time(2)-RT_time(1),0)
   delta  =theta_function(Field_time,RT_time(2)-RT_time(1),1)
   !
   do i_field=1,n_fields
     ElField%F_t(i_t,i_field)=Efield(i_Probe)%amplitude*(E_of_t(i_field)*theta+A_of_t(i_field)*delta)
   enddo
   !
 enddo
 !
 ! ... FT
 !--------
 !
 call RT_OBSERVABLES_damp_and_write(ElField,RT_conf,.TRUE.)
 !
 !... Analytic ...
 !
 iE_err=-1
 if (n_fields==1) then
   iE_err=0
   do i_w = 1, RT_conf%W_n_steps
     !
     if(trim(RT_conf%damping) /= "NONE")                      Field_freq=ElField%W(i_w)-cI*RT_conf%damp_factor
     if(trim(RT_conf%damping) == "NONE".and.l_force_min_damp) Field_freq=ElField%W(i_w)-cI*RT_conf%Field_damp_factor
     !
     ElField%F_w(i_w,1)=Efield(i_Probe)%amplitude*small_a_frequency(Field_freq,Efield(i_Probe),iE_err)*&
&                       exp(cI*ElField%W(i_w)*Efield(i_Probe)%t_initial)
     !
     if (iE_err<0) exit
     !
   enddo 
 endif
 !
 !... Numerical ...
 !
 if (iE_err<0) call RT_1D_Fourier_Transform('T2W',ElField,RT_conf,1)
 !
 ! Check of finiteness of E field
 !--------------------------------
 if (maxval( abs(ElField%F_w(:,:)) )<epsilon(1._SP)) call error("Null Electric Field in this time-range. Increase it.")
 !
 ! Damp and (eventually) remove the asymptotic limit of Pol
 !
 do i_p_dir=1,3
   if (l_rm_asympt_limit) Pol%F_t(:,i_p_dir)= Pol%F_t(:,i_p_dir)-Pol%F_t(RT_conf%T_n_steps,i_p_dir)
 enddo
 !
 ! Pol FT
 !--------
 !
 Pol%title="PolFFT"
 call RT_1D_Fourier_Transform('T2W',Pol,RT_conf,1)
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
 do i_w = 1, RT_conf%W_n_steps
   !
   if(Efield(i_Probe)%ef_pol=="linear"  ) Field = cmplx(ElField%F_w(i_w,1),kind=SP)
   !
   if(Efield(i_Probe)%ef_pol=="circular") then
     E_w_SP = cZERO
     E_w_SP = Efield(i_Probe)%versor*cmplx(ElField%F_w(i_w,1),kind=SP)+Efield(i_Probe)%versor_circ*cmplx(ElField%F_w(i_w,2),kind=SP)
     E_ver(:,1)=abs(E_w_SP/v_norm(abs(E_w_SP)))
   endif
   !
   do i_p_dir=1,3
     !
     Pol_proj(i_p_dir)=dot_product(E_ver(:,i_p_dir),cmplx(Pol%F_w(i_w,:),kind=SP) )
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
   EPS(:,1)  = -1._SP/EELS(:,1)
 else
   EPS(:,1)  =  1._SP+EPS(:,1)
   EELS(:,1) = -1._SP/EPS(:,1)
 endif
 !
 do i_w = 1, RT_conf%W_n_steps
   !
   W_reached = real(ElField%W(i_w))*HA2EV
   !
   E_w_SP = cZERO
   if(Efield(i_Probe)%ef_pol=="linear"  ) E_w_SP=Efield(i_Probe)%versor*cmplx(ElField%F_w(i_w,1),kind=SP)
   if(Efield(i_Probe)%ef_pol=="circular") E_w_SP=Efield(i_Probe)%versor     *cmplx(ElField%F_w(i_w,1),kind=SP) &
&                                               +Efield(i_Probe)%versor_circ*cmplx(ElField%F_w(i_w,2),kind=SP)
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
 ! Clean up
 !==========
 !
 YAMBO_FREE(EELS)
 YAMBO_FREE(EPS)
 !
 call FT_free(Pol)
 call FT_free(ElField)
 !
end subroutine
