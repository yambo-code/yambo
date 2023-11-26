!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine X_OUTPUT(iq,Xen,Xk,wv,X,spectra)
 !
 use pars,          ONLY:pi,SP
 use D_lattice,     ONLY:l_0D,l_1D,l_2D,l_3D
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc,l_alda_fxc,l_pf_fxc,l_col_cut
 use IO_int,        ONLY:IO_and_Messaging_switch
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use DIPOLES,       ONLY:DIP_desc
 use TDDFT,         ONLY:FXC_desc,FXC_type
 use R_lattice,     ONLY:bz_samp,q_norm,bare_qpg,FineGd_desc
 use parallel_m,    ONLY:master_cpu
 use X_m,           ONLY:X_t,Epsilon_ii,EEL,Alpha,X_fxc,&
&                        eps_2_alpha,X_OUTPUT_messages,X_OUTPUT_driver,X_do_obs,&
&                        i_G_shift,Q_plus_G_pt,Q_plus_G_sq_modulus
 !
 implicit none
 !
 integer, intent(in) :: iq
 type(levels)  :: Xen
 type(bz_samp) :: Xk
 type(X_t)     :: X
 type(w_samp)  :: wv 
 complex(SP), intent(in)  :: spectra(wv%n_freqs,2)
 !
 ! Workspace
 !
 complex(SP)      :: fac
 real(SP)         :: output_data(5),a_xc,b_xc
 logical          :: l_ext_eps,l_alpha_prop_to_chi_full,l_eels_from_ext_eps
 integer          :: iw,n_data
 !
 logical,external :: EPS_check
 !
 ! Output files init 
 !===================
 call X_OUTPUT_driver("DEF INIT")
 !
 ! Observables allocation
 call X_ALLOC_global("ALLOC",wv)
 !
 ! In 0D,1D,2D, chi=chibar and eps=eels=1 in the limit V--> infinity
 ! However we perform simulations at finite volume and there are differences
 ! * Without cotoff (old yambo implementation)
 !   it is convinient to define alpha as proportional to chibar
 !   since the latter converges faster to the V --> infinity limit
 ! * With coulomb cut-off the small difference should correctly describe
 !   the effect of the region around G=0 of this integral which one
 !   would obtained in the V --> infinity limit (to be checked)
 l_alpha_prop_to_chi_full=l_col_cut
 !
 ! Check if EELS can be evaluated, e.g. if we are using only w>0 frequencies
 ! This allows to include the v_{G=0} term with coupling also for "r" or "a" orderings.
 ! Not needed for "T" or  "R" orderings. Used only for 3D materials now
 l_ext_eps=.false.
 if (X_do_obs("eel")) l_ext_eps=EPS_check(wv%p,wv%n_freqs,.TRUE.,X%ordering)
 !! l_ext_eps is for now ignored since  for back compatibility in the test-suite
 !l_eels_from_ext_eps=.not.(trim(X%ordering)=='R'.or.trim(X%ordering)=="T") .and. l_ext_eps .and. l_3D
 l_eels_from_ext_eps=.not.(trim(X%ordering)=='R'.or.trim(X%ordering)=="T") .and. l_3D
 !
 ! OutPut files...
 if (.not.l_rpa_IP) call X_OUTPUT_driver("keys ALL",IQ=iq,keys="inv "//trim(FXC_type)//" dyson",IG=i_G_shift)
 if (     l_rpa_IP) call X_OUTPUT_driver("keys ALL",IQ=iq,keys="ip",IG=i_G_shift)
 !
 ! ... permissions ...
 if (iq==1) call IO_and_Messaging_switch("+output SAVE",CONDITION=master_cpu)
 !
 ! ... open ...
 call X_OUTPUT_driver("open ALL")
 if (iq==1) call IO_and_Messaging_switch("+output RESTORE")
 !
 ! Initial Messages
 call X_OUTPUT_messages(iq,Q_plus_G=Q_plus_G_pt,ig=i_G_shift,MORE="X_size")
 if (l_bs_fxc) call X_OUTPUT_messages(iq,MORE="FXC_size")
 !
 ! Epsilon. This is always the 3D one
 if (X_do_obs("eps")) Epsilon_ii(:,1)=spectra(:,1)+1._SP
 !
 ! Alpha
 if (X_do_obs("alpha")) Alpha(:,1)=spectra(:,1)/4._SP/pi
 !
 ! EEL: TO FIX EEL as function of dimensions
 if (X_do_obs("eel")) then
   !
   ! The RPA part is always defined in the same way otherwise I get IP instead of RPA without LF.
   ! This is like the Lbar case in BSE.
   ! Via EPS_extend v_{G=0} is included in coupling mode regardless or the ordering
   call EPS_extend(wv%p,wv%n_freqs,.TRUE.,X%ordering,Epsilon_ii(:,1)-1._SP,EEL(:,1))
   EEL(:,1)=-1._SP/(1._SP+EEL(:,1))
   !
 endif
 !
 if (     l_rpa_IP) then
   if (X_do_obs("eps"))   Epsilon_ii(:,2)=Epsilon_ii(:,1)
   if (X_do_obs("alpha")) Alpha(:,2)=Alpha(:,1)
   if (X_do_obs("eel"))   EEL(:,2) = EEL(:,1)
 else
   !
   ! Epsilon. Procedure to get epsilon from Lfull
   if (X_do_obs("eps")) then 
     Epsilon_ii(:,2)=1._SP-spectra(:,2)/(spectra(:,2)+1._SP)!*fac
   endif
   !
   ! Alpha
   if (X_do_obs("alpha")) then 
     if (l_alpha_prop_to_chi_full) then
       ! Alpha proportional to Lfull
       Alpha(:,2)=spectra(:,2)
       ! Maybe we could extend Alpha ??
       if ( .not.(trim(X%ordering)=='T'.or.trim(X%ordering)=='R') ) &
       & call warning(" Res (or ares) only alpha")
     else
       ! Alpha proportional to Lbar
       Alpha(:,2)=-spectra(:,2)/(spectra(:,2)+1._SP)
     endif
     !
     ! As for the IP definition, I change bare_qpg into Q_plus_G_sq_modulus
     ! To check
     !  (i) if this is correct
     !  (ii) why it is done for alpha and not for Epsilon_ii
     fac=bare_qpg(iq,i_G_shift)**2/Q_plus_G_sq_modulus
     Alpha(:,2)=Alpha(:,2)*fac/4._SP/pi
   endif
   !
   if (X_do_obs("eel")) then
     !
     if(l_eels_from_ext_eps) then
       !
       ! I performer a double inversion, plus KK to extend epsilon
       ! Doing so, the v_{G=0} term is included in coupling mode regardless or the ordering
       !
       EEL(:,2)=-spectra(:,2)/(spectra(:,2)+1._SP)
       call warning(" G=0 term accounted for in coupling mode for eels")
       call EPS_extend(wv%p,wv%n_freqs,.FALSE.,X%ordering,EEL(:,2),EEL(:,2))
       EEL(:,2)=-1._SP/(1._SP+EEL(:,2))
       !
     else
       !
       ! eels proportional to Lfull as it is computed
       !
       EEL(:,2)=-(spectra(:,2)+1._SP)
       !
     endif
     !
   endif
   !
   if (l_pf_fxc) then
     call TDDFT_PF_coefficient(Xen,Xk,a_xc,b_xc)
     if (abs(b_xc).le.1.e-5) then
       Epsilon_ii(:,2)= 1._SP + 4._SP*pi*(Epsilon_ii(:,2)-1._SP)/(4._SP*pi -a_xc*(Epsilon_ii(:,2)-1._SP))
     else
       do iw=1,wv%n_freqs
         Epsilon_ii(iw,2)= 1._SP + 4._SP*pi*(Epsilon_ii(iw,2)-1._SP)/& 
&                          (4._SP*pi -(a_xc + b_xc*real(wv%p(iw))**2) *(Epsilon_ii(iw,2)-1._SP))
       enddo
     end if
   end if
   !
 endif
 !
 ! Prepare output
 ! Unfortunately some of the variables need in this second bunch of messages is setup only in X_os
 call X_OUTPUT_messages(iq,GF=allocated(Xen%GreenF),Vnl=X%Vnl_included,ordering=X%ordering)
 !
 ! Descriptors 
 call X_OUTPUT_driver("DESC",DESC=DIP_desc)
 call X_OUTPUT_driver("DESC",DESC=FineGd_desc)
 if (l_bs_fxc) call X_OUTPUT_driver("DESC",DESC=FXC_desc)
 !
 if (     l_rpa_IP) n_data=3
 if (.not.l_rpa_IP) n_data=5
 !
 ! Epsilon output
 if (X_do_obs("eps")) then 
   do iw=1,wv%n_freqs
     output_data=(/real(wv%p(iw)),aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),&
&                  aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1))/)
     call X_OUTPUT_driver("WRITE eps",data=output_data(:n_data))
   enddo
 endif
 !
 ! Alpha output
 if (X_do_obs("alpha")) then 
   Alpha=Alpha*eps_2_alpha
   do iw=1,wv%n_freqs
     output_data=(/real(wv%p(iw)),aimag(Alpha(iw,2)),real(Alpha(iw,2)),aimag(Alpha(iw,1)),real(Alpha(iw,1))/)
     call X_OUTPUT_driver("WRITE alpha",data=output_data(:n_data))
   enddo
 endif
 !
 ! EEL output
 if (X_do_obs("eel")) then
   do iw=1,wv%n_freqs
     output_data=(/real(wv%p(iw)),aimag(EEL(iw,2)),real(EEL(iw,2)),aimag(EEL(iw,1)),real(EEL(iw,1))/)
     call X_OUTPUT_driver("WRITE eel",data=output_data(:n_data))
   enddo
 endif
 !
 ! TDDFT Kernel output
 if (l_bs_fxc) then
   do iw=1,wv%n_freqs
     output_data(1:3)=(/real(wv%p(iw)),real(X_fxc(iw)*q_norm(iq)**2),aimag(X_fxc(iw)*q_norm(iq)**2)/)
     call X_OUTPUT_driver("WRITE fxc",data=output_data(:3))
   enddo
 endif
 !
 call X_OUTPUT_driver("close ALL")
 !
 call X_ALLOC_global("FREE",wv)
 !
end subroutine X_OUTPUT
