!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine OPTICS_driver(Xen,Xk,q,wv,X,Dip)
 ! 
 ! Optics driver
 !
 ! Calculates dielectric function for a generic q vector.
 ! 
 use pars,          ONLY:pi,SP,cZERO
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc,l_alda_fxc,l_pf_fxc,l_col_cut
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use D_lattice,     ONLY:l_0D,l_1D,l_2D,l_3D
 use R_lattice,     ONLY:bz_samp,q_norm,bare_qpg,FineGd_desc
 use DIPOLES,       ONLY:DIPOLE_t,DIP_desc
 use X_m,           ONLY:X_t,Epsilon_ii,EEL,Alpha,X_fxc,X_par,X_Double_Grid_setup,&
&                        use_X_DbGd,eps_2_alpha,X_par_lower_triangle,X_OUTPUT_messages,&
&                        X_DbGd_nkpt_at_q,X_DbGd_nkpt_at_q_table,X_OUTPUT_driver,X_do_obs
 use parallel_int,  ONLY:PP_wait,PP_redux_wait,PARALLEL_global_indexes,PARALLEL_WF_index,PARALLEL_WF_distribute
 use parallel_m,    ONLY:PAR_IND_Q_ibz,PAR_COM_Q_A2A,PAR_COM_RL_INDEX,PAR_IND_CON_BANDS_X,&
&                        PAR_IND_Xk_ibz,PAR_IND_VAL_BANDS_X,master_cpu
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use TDDFT,         ONLY:FXC_desc,FXC_type,&
&                        F_xc_gspace,FXC_K_diagonal
 use interfaces,    ONLY:WF_load,WF_free,WF_load
 use matrix,        ONLY:MATRIX_reset
 use IO_int,        ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 type(levels)  :: Xen
 type(bz_samp) :: Xk,q
 type(X_t)     :: X
 type(DIPOLE_t):: Dip
 type(w_samp)  :: wv 
 ! 
 ! Work Space
 !
 integer          :: iw,iq,i_G_shift,n_data
 complex(SP)      :: fac,spectra(wv%n_freqs,2)
 real(SP)         :: output_data(5),Q_plus_G_pt(3),Q_plus_G_sq_modulus,a_xc,b_xc
 logical          :: l_skip_non_int_eps,Macroscopic_CPU,l_eels,l_alpha_from_eps,&
&                    l_eels_from_ext_eps
 !
 integer,external :: OPTICS_select_q_and_G
 logical,external :: EPS_check
 !
 call section('*','Optics')
 !
 ! Basic Checks
 call X_pre_setup(Xen,Xk,X)
 !
 allocate(X_par(1))
 !
 do iq=X%iq(1),X%iq(2)
   !
   ! Output files init 
   !===================
   call X_OUTPUT_driver("DEF INIT")
   !
   ! PARALLEL indexes
   !==================
   call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space",X=X)
   !
   ! WF distribution 
   !=================
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,B_index=PAR_IND_CON_BANDS_X(X%whoami),&
&                              Bp_index=PAR_IND_VAL_BANDS_X(X%whoami),CLEAN_UP=.TRUE.)
   !
   call PARALLEL_WF_index(COMM=PAR_COM_Q_A2A)
   !
   if (WF_buffered_IO) call WF_load(WF_buffer,0,1,X%ib,(/1,Xk%nibz/),space='B',title='-BUFFER')
   !
   ! TDDFT setup
   call TDDFT_do_X_W_typs(iq,X,wv)
   !
   ! DbGd setup
   call X_Double_Grid_setup(iq,Xk,Xk%FGbz)
   !
   ! ALDA (only once) 
   if (l_alda_fxc.and..not.allocated(F_xc_gspace)) then
     !
     call TDDFT_ALDA_G_space(Xen,Xk)
     !
   endif
   !
   ! workaround for possible compiler bug
#if defined _PGI || defined _NV
   if (iq>1.and.size(PAR_IND_Q_ibz%n_of_elements)>1) then
#else
   if (iq>1) then
#endif
     if (.not.PAR_IND_Q_ibz%element_1D(iq)) cycle
   endif
   !
   ! Check if this q is compatible with the user defined direction in chartesian coordinates
   ! and if an additional RL vectors must be added
   i_G_shift=OPTICS_select_q_and_G(iq,q,X,Q_plus_G_pt,Q_plus_G_sq_modulus)
   if (i_G_shift<0) cycle
   !
   ! In TDDFT with BS based kernel eps0 is non-sense
   ! as shifted of the K diagonal
   l_skip_non_int_eps=l_bs_fxc
   !
   ! Frequencies (AFTER TDDFT SETUP!)
   call FREQUENCIES_setup(wv)
   !
   ! Response function parallel allocation
   call X_half_mat_check(Xen,Xk,wv)
   !
   call X_ALLOC_parallel(X_par(1),X%ng,wv%n_freqs,"XUP")
   !
   Macroscopic_CPU=X_par(1)%rows(1)<=i_G_shift.and.i_G_shift<=X_par(1)%rows(2).and.&
&                  X_par(1)%cols(1)<=i_G_shift.and.i_G_shift<=X_par(1)%cols(2)
   !
   ! Observables allocation
   call X_ALLOC_global("ALLOC",wv)
   !
   ! Old Yambo implementation, needed without cut-off
   ! In 0D, and in 2D/1D at q=0, eps=eels.
   ! However it is convinient to define alpha as proportional to epsilon
   ! since the latter converges faster to the V --> infinity limit
   ! This is not needed anymore with coulomb cut-off
   l_alpha_from_eps=.false.
   if ( ( l_0D .or. (  (l_1D.or.l_2D) .and. iq==1 ) ) .and. .not. l_col_cut ) l_alpha_from_eps=.true.
   !
   ! Check if EELS can be evaluated
   ! This allows to include the v_{G=0} term with coupling also for "r" or "a" orderings.
   ! Not needed for "R" or "T" orderings. Used only for 3D materials now
   l_eels=.false.
   if (X_do_obs("eel")) l_eels=EPS_check(wv%p,wv%n_freqs,.TRUE.,X%ordering)
   l_eels_from_ext_eps=.not.(trim(X%ordering)=='T'.or.trim(X%ordering)=='R') .and. l_eels .and. l_3D
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
   if (iq==1) call PP_wait()
   if (iq> 1) call PP_wait(COMM=PAR_COM_Q_A2A%COMM)
   !
   ! Irredux (Xo) 
   call X_irredux(iq,"Xo",X_par(1),Xen,Xk,wv,X,Dip)
   !
   ! Without LF. I start defining spectra=eps-1
   spectra=cZERO
   if (Macroscopic_CPU) then
     ! I start defining spectra=eps-1
     ! using the definition of epsilon-1 in terms of chi-bar
     fac=4._SP*pi/Q_plus_G_sq_modulus
     spectra(:,1)=-X_par(1)%blc(i_G_shift,i_G_shift,:)*fac
   endif
   call PP_redux_wait(spectra(:,1),COMM=PAR_COM_RL_INDEX%COMM)
   !
   ! Epsilon. This is always the 3D one (for now)
   Epsilon_ii(:,1)=spectra(:,1)+1._SP
   !
   ! Alpha
   if(.not.l_alpha_from_eps) then
     ! ******* Point 1 to discuss *********
     ! Do I need to extend spectra to get alpha ...
     !   In case the alpha is defined from the extended eps,
     !   which includes v(G=0) in coupling mode regardless or the X%ordering.
     !   This is like the Lbar case in BSE.
     call EPS_extend(wv%p,wv%n_freqs,.FALSE.,X%ordering,spectra(:,1),Alpha(:,1))
     ! .. or not ??
     !   In case I'm not sure at which level the v(G=0) term is accounted for
     !Alpha(:,1)=spectra(:,1)
     ! ******* End of Point 1     *********
     Alpha(:,1)=Alpha(:,1)/(1._SP+Alpha(:,1))/4._SP/pi
   else
     Alpha(:,1)=spectra(:,1)/4._SP/pi
   endif
   !
   ! EEL: TO FIX EEL as function of dimensions
   if (X_do_obs("eel")) then
     !
     ! The RPA part is always defined in the same way
     ! otherwise I get IP instead of RPA without LF.
     ! This is like the Lbar case in BSE. Via EPS_extend v_{G=0} is
     ! included in coupling mode regardless or the ordering
     call EPS_extend(wv%p,wv%n_freqs,.TRUE.,X%ordering,Epsilon_ii(:,1)-1._SP,EEL(:,1))
     EEL(:,1)=-1._SP/(1._SP+EEL(:,1))
     !
   endif
   !
   if (     l_rpa_IP) then
     Epsilon_ii(:,2)=Epsilon_ii(:,1)
     Alpha(:,2)=Alpha(:,1)
     if(X_do_obs("eel")) EEL(:,2) = EEL(:,1)
   else
     !
     ! X Dyson equation solver
     call X_redux(iq,"X",X_par(1),wv,X)
     !
     !
     ! X_redux can change the matrix distribution. Need to recheck who is Macroscopic CPU
     Macroscopic_CPU=X_par(1)%rows(1)<=i_G_shift.and.i_G_shift<=X_par(1)%rows(2).and.&
&                    X_par(1)%cols(1)<=i_G_shift.and.i_G_shift<=X_par(1)%cols(2)
     !
     if (Macroscopic_CPU) then
       !
       ! With LF. I start defining spectra=eps-1
       ! using the definition of epsilon-1 in terms of chi
       ! (1) at variance with the IP case, after the call to X_redux,
       !     X_par now contains the factor 4*pi/bare_qpg**2
       ! (2) the factor is with the coulomb cutoff-term
       spectra(:,2)=-X_par(1)%blc(i_G_shift,i_G_shift,:)/(X_par(1)%blc(i_G_shift,i_G_shift,:)+1._SP) 
       !
     endif
     call PP_redux_wait(spectra(:,2),COMM=PAR_COM_RL_INDEX%COMM)
     !
     ! ******* Point 2 to discuss *********
     ! As for the IP definition, I change bare_qpg into Q_plus_G_sq_modulus
     ! To check
     !  (i) if this is correct
     !  (ii) why it is done for alpha and not for Epsilon_ii
     fac=bare_qpg(iq,i_G_shift)**2/Q_plus_G_sq_modulus
     !
     ! Epsilon. This is always the 3D one (for now)
     Epsilon_ii(:,2)=spectra(:,2)+1._SP
     !Epsilon_ii(:,2)=spectra(:,2)*fac+1._SP
     ! ******* End of Point 2     *********
     !
     ! Alpha
     if (.not.l_alpha_from_eps) then
       ! ******* Point 1 to discuss *********
       ! See IP above.
       ! Here the opposite choice, compared to IP, was done by yambo
       !call EPS_extend(wv%p,wv%n_freqs,.FALSE.,X%ordering,spectra(:,2),Alpha(:,2))
       Alpha(:,2)=spectra(:,2)
       ! ******* End of Point 1     *********
       Alpha(:,2)=Alpha(:,2)/(1._SP+Alpha(:,2))/4._SP/pi
     else
       Alpha(:,2)=spectra(:,2)/4._SP/pi
     endif
     !
     ! ******* Point 2 to discuss *********
     ! See above
     !Alpha(:,2)=Alpha(:,2)
     Alpha(:,2)=Alpha(:,2)*fac
     ! ******* End of Point 2     *********
     !
     ! EEL: TO FIX EEL as function of dimensions
     if (X_do_obs("eel")) then
       !
       if(l_alpha_from_eps .or. l_eels_from_ext_eps) then
         !
         ! Yambo original version
         ! This is now used only without coulomb cutoff
         ! a) for 0D, or 1D and 2D at q=0
         ! (see l_alpha_from_eps definition)
         ! b) for 3D when ordering not R or T
	 !
         call EPS_extend(wv%p,wv%n_freqs,.FALSE.,X%ordering,spectra(:,2),EEL(:,2))
         EEL(:,2)=-1._SP/(1._SP+EEL(:,2))
	 !
       else
	 !
	 ! In the other cases we obtain EEL directly
	 ! from the definition of Alpha, using exact equations.
         ! The relation between eels and alpha is dimension dependent.
	 ! Accordingly this corresponds to the above procedure only in 3D
         !
         ! I'm using:
         ! alpha = ( 1 +eels )/4/pi  
         ! 3D case: eels =    1 -alpha*4*pi  
         ! 2D case: eels = ??? Alpha ???
         ! 1D case: eels = ??? Alpha ???
         ! 0D case: eels cannot be measured
         !! 2D, 1D and 0D cases to be fixed
	 !
	 ! Maybe we could extend Alpha ??
         if ( .not.(trim(X%ordering)=='T'.or.trim(X%ordering)=='R') ) &
         & call warning(" Res (or ares) only eels")
         !
         if(l_3D) EEL(:,2)=Alpha(:,2)*4._SP*pi-1._SP
         if(l_2D) EEL(:,2)=Alpha(:,2)*4._SP*pi-1._SP ! To be fixed
         if(l_1D) EEL(:,2)=Alpha(:,2)*4._SP*pi-1._SP ! To be fixed
	 ! In 0D EEL X_do_obs("eel") is set to falso by X_output_driver 
	 !
       endif
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
   do iw=1,wv%n_freqs
     output_data=(/real(wv%p(iw)),aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),&
&                  aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1))/)
     call X_OUTPUT_driver("WRITE eps",data=output_data(:n_data))
   enddo
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
     if (iq==1) call PP_redux_wait(X_fxc)
     if (iq> 1) call PP_redux_wait(X_fxc,COMM=PAR_COM_Q_A2A%COMM)
     do iw=1,wv%n_freqs
       output_data(1:3)=(/real(wv%p(iw)),real(X_fxc(iw)*q_norm(iq)**2),aimag(X_fxc(iw)*q_norm(iq)**2)/)
       call X_OUTPUT_driver("WRITE fxc",data=output_data(:3))
     enddo
   endif
   !
   call X_OUTPUT_driver("close ALL")
   !
   ! CLEAN (each q)
   call X_ALLOC_global("FREE",wv)
   call FREQUENCIES_reset(wv,"all")
   call MATRIX_reset(X_par(1))
   YAMBO_FREE(X_par_lower_triangle%blc)
   if (use_X_DbGd) then
     YAMBO_FREE(X_DbGd_nkpt_at_q_table)
     YAMBO_FREE(X_DbGd_nkpt_at_q)
   endif
   !
 enddo
 !
 ! CLEAN
 !
 call MATRIX_reset(X_par_lower_triangle)
 deallocate(X_par)
 call WF_free(WF)
 call PP_wait()
 if (allocated(F_xc_gspace)) then
   YAMBO_FREE(F_xc_gspace)
 endif
 if (l_bs_fxc) then
   YAMBO_FREE(FXC_K_diagonal)
 endif
 !
end subroutine
