!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM CA DS
!
! Partialy inspired from Octopus v. 3.1.0
!
! Copyright (C) 2002 M. Marques, A. Castro, A. Rubio, G. Bertsch
!
subroutine RT_X_LRR_real_field(en)
 !
 use YPP_real_time,  ONLY:RT_conf,Use_FFT,l_force_min_damp,RT_time,Pol,Curr,ElField,&
&                         OBS_el_free,X_kind,N_pumps
 use pars,           ONLY:SP,pi,cI,cZERO,schlen
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels
 use vec_operate,    ONLY:v_norm
 use RT_control,     ONLY:RT_output
 use real_time,      ONLY:l_RT_induced_field
 use fields,         ONLY:Efield,small_a_frequency,small_a
 use com,            ONLY:of_open_close,msg
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
 real(SP)     :: W_reached,Field_time,outdata(6),E_ver(3,3),Fac
 complex(SP)  :: OBS_proj,OBS_vec(3),E_w_SP(3),Field,E_of_t(2),A_of_t(2),theta,delta,Field_freq
 complex(SP), allocatable :: EELS(:,:),EPS(:,:)
 !
#if defined _YPP_NL
 character(schlen)        :: headings(7)
 real(SP)                 :: data_to_dump(7)
#endif
 !
 call section("=",'Optics')
 !=========================
 call parser('RmAsymLim',l_rm_asympt_limit)
 !
 i_Probe=1
 if(trim(Efield(i_Probe)%ef_pol)=="linear"  ) n_fields=1
 if(trim(Efield(i_Probe)%ef_pol)=="circular") n_fields=2
 !
 ! Pump removal (if any)
 !-----------------------
 if (N_pumps==1) then
   do i_t=1,RT_conf%T_n_steps
     Curr(i_Probe)%F_t(i_t,:)=Curr(i_Probe)%F_t(i_t,:)-Curr(i_Probe+1)%F_t(i_t,:)
     Pol(i_Probe)%F_t(i_t,:)=Pol(i_Probe)%F_t(i_t,:)-Pol(i_Probe+1)%F_t(i_t,:)
   enddo
 endif
 !
 ! 1D FFT setup
 !--------------
 call RT_1D_Fourier_setup(ElField,RT_conf,n_fields) 
 if (trim(X_kind)=="rhorho") then
   call RT_1D_Fourier_setup(Pol(1),RT_conf,3) 
 else if(trim(X_kind)=="jj") then
   call RT_1D_Fourier_setup(Curr(1),RT_conf,3) 
 else
   call error(" wrong Xkind in input")
 endif
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
     Fac=Efield(i_Probe)%amplitude
     if(trim(X_kind)=="jj")     ElField%F_t(i_t,i_field)=-Fac*(A_of_t(i_field)*theta)
     if(trim(X_kind)=="rhorho") ElField%F_t(i_t,i_field)= Fac*(E_of_t(i_field)*theta+A_of_t(i_field)*delta)
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
 if (n_fields==1.and.trim(X_kind)=="rhorho") then
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
 if (l_rm_asympt_limit) then
   call msg("s","Removing asymptotic limit")
   do i_p_dir=1,3
     if (trim(X_kind)=="rhorho") Pol(1)%F_t(:,i_p_dir) =  Pol(1)%F_t(:,i_p_dir)- Pol(1)%F_t(RT_conf%T_n_steps,i_p_dir)
     if (trim(X_kind)=="jj")     Curr(1)%F_t(:,i_p_dir)= Curr(1)%F_t(:,i_p_dir)-Curr(1)%F_t(RT_conf%T_n_steps,i_p_dir)
   enddo
 endif
 !
 !
 ! Pol FT
 !--------
 !
 if (trim(X_kind)=="rhorho") then
   Pol(1)%title="PolFFT"
   call RT_1D_Fourier_Transform('T2W',Pol(1),RT_conf,1)
 else if(trim(X_kind)=="jj") then
   Curr(1)%title="CurrFFT"
   call RT_1D_Fourier_Transform('T2W',Curr(1),RT_conf,1)
 endif
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
#if defined _YPP_NL
 call of_open_close("YPP-E_frequency" ,'ot')
 headings=(/'E [eV]   ','E_x(w)/Re','E_y(w)/Re','E_z(w)/Re','E_x(w)/Im','E_y(w)/Im','E_z(w)/Im'/)
 call msg('o frequency','#',headings,INDENT=0,USE_TABS=.TRUE.)
 call msg('o frequency','#')
 !
 call of_open_close("YPP-eps_along_E" ,'ot')
 headings=(/'E [eV]   ','Im/eps_d1','Im/eps_d2','Im/eps_d3','Re/eps_d1','Re/eps_d2','Re/eps_d3'/)
 call msg('o eps','#',headings,INDENT=0,USE_TABS=.TRUE.)
 call msg('o eps','#')
 !
 call of_open_close("YPP-eels_along_E",'ot')
 headings=(/'E [eV]   ','Im/eel_d1','Im/eel_d2','Im/eel_d3','Re/eel_d1','Re/eel_d2','Re/eel_d3'/)
 call msg('o eels','#',headings,INDENT=0,USE_TABS=.TRUE.)
 call msg('o eels','#')
#else
 call RT_output(what="open E_frequency eps_along_E eels_along_E")
#endif
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
     E_w_SP = Efield(i_Probe)%versor*cmplx(ElField%F_w(i_w,1),kind=SP)+&
&             Efield(i_Probe)%versor_circ*cmplx(ElField%F_w(i_w,2),kind=SP)
     E_ver(:,1)=abs(E_w_SP/v_norm(abs(E_w_SP)))
   endif
   !
   do i_p_dir=1,3
     !
     if(trim(X_kind)=="rhorho") OBS_vec=cmplx( Pol(1)%F_w(i_w,:),kind=SP)
     if(trim(X_kind)=="jj")     OBS_vec=cmplx(Curr(1)%F_w(i_w,:),kind=SP)/ElField%W(i_w)**2
     !
     OBS_proj=dot_product(E_ver(:,i_p_dir),OBS_vec)
     if(Efield(i_Probe)%ef_pol=="circular") Field=dot_product(E_ver(:,i_p_dir),E_w_SP )
     !
     if(     l_RT_induced_field) EELS(i_w,i_p_dir) = 4._SP*pi*OBS_proj/Field
     if(.not.l_RT_induced_field) EPS( i_w,i_p_dir) = 4._SP*pi*OBS_proj/Field
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
   if(Efield(i_Probe)%ef_pol=="circular") E_w_SP=Efield(i_Probe)%versor*cmplx(ElField%F_w(i_w,1),kind=SP) &
&                                               +Efield(i_Probe)%versor_circ*cmplx(ElField%F_w(i_w,2),kind=SP)
   !
#if defined _YPP_NL
   data_to_dump(1)=W_reached
#endif
   !
#if defined _YPP_NL
   data_to_dump(2:4)=(/ real(E_w_SP(1) ),real(E_w_SP(2) ),real(E_w_SP(3) ) /)
   data_to_dump(5:7)=(/ aimag(E_w_SP(1)),aimag(E_w_SP(2)),aimag(E_w_SP(3)) /)
   call msg('o frequency','',data_to_dump,USE_TABS=.TRUE.)
#else
   outdata(1:3)=(/ real(E_w_SP(1) ),real(E_w_SP(2) ),real(E_w_SP(3) ) /)
   outdata(4:6)=(/ aimag(E_w_SP(1)),aimag(E_w_SP(2)),aimag(E_w_SP(3)) /)
   call RT_output('E_frequency',VALUEs=outdata,E=W_reached)
#endif
#if defined _YPP_NL
   data_to_dump(2:4)=(/ aimag(EPS(i_w,1) ),aimag(EPS(i_w,2) ),aimag(EPS(i_w,3) ) /)
   data_to_dump(5:7)=(/ real( EPS(i_w,1) ),real( EPS(i_w,2) ),real( EPS(i_w,3) ) /)
   call msg('o eps','',data_to_dump,USE_TABS=.TRUE.)
#else
   outdata(1:3)=(/ aimag(EPS(i_w,1) ),aimag(EPS(i_w,2) ),aimag(EPS(i_w,3) ) /)
   outdata(4:6)=(/ real( EPS(i_w,1) ),real( EPS(i_w,2) ),real( EPS(i_w,3) ) /)
   call RT_output('eps_along_E' ,VALUEs=outdata ,E=W_reached)
#endif
#if defined _YPP_NL
   data_to_dump(2:4)=(/ aimag(EELS(i_w,1) ),aimag(EELS(i_w,2) ),aimag(EELS(i_w,3) ) /)
   data_to_dump(5:7)=(/ real( EELS(i_w,1) ),real( EELS(i_w,2) ),real( EELS(i_w,3) ) /)
   call msg('o eels','',data_to_dump,USE_TABS=.TRUE.)
#else
   outdata(1:3)=(/ aimag(EELS(i_w,1) ),aimag(EELS(i_w,2) ),aimag(EELS(i_w,3) ) /)
   outdata(4:6)=(/ real( EELS(i_w,1) ),real( EELS(i_w,2) ),real( EELS(i_w,3) ) /)
   call RT_output('eels_along_E',VALUEs=outdata ,E=W_reached)
#endif
   !
 enddo
#if defined _YPP_NL
 call of_open_close("YPP-E_frequency")
 call of_open_close("YPP-eps_along_E")
 call of_open_close("YPP-eels_along_E")
#else
 call RT_output(what="close E_frequency eps_along_E eels_along_E")
#endif
 !
 ! Clean up
 !==========
 !
 YAMBO_FREE(EELS)
 YAMBO_FREE(EPS)
 !
 if (trim(X_kind)=="rhorho") call OBS_el_free(Pol(1))
 if (trim(X_kind)=="jj")     call OBS_el_free(Curr(1))
 call OBS_el_free(ElField)
 !
end subroutine
