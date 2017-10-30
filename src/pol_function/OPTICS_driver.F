!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine OPTICS_driver(Xen,Xk,q,wv,X)
 ! 
 ! Optics driver
 !
 ! Calculates dielectric function for a generic q vector.
 ! 
 use pars,          ONLY:pi,SP,cZERO
 use units,         ONLY:HA2EV
 use stderr,        ONLY:intc,set_real_printed_length
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc,l_alda_fxc
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,q_norm,bare_qpg
 use com,           ONLY:msg,of_open_close
 use X_m,           ONLY:X_t,Epsilon_ii,EEL,Alpha,X_alloc,X_fxc,X_par,&
&                        use_X_DbGd,eval_alpha,eps_2_alpha,OPTICS_eels,X_par_lower_triangle
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Q,PAR_COM_Q_A2A,PAR_IND_CON_BANDS_X,&
&                        PAR_IND_Xk_ibz,PAR_IND_VAL_BANDS_X,PAR_COM_X_WORLD
 use wave_func,     ONLY:WF,WF_buffer,WF_buffered_IO
 use TDDFT,         ONLY:FXC_n_descs,FXC_description,FXC_type,&
&                        FXC,FXC_K_diagonal,FXC_n_g_corr
 use X_output,      ONLY:X_setup_file_names,eps_file_name,eel_file_name,alpha_file_name,fxc_file_name,&
&                        X_write_q_plus_G,X_write_descriptions,X_write_messages_before_headers,headers,&
&                        X_setup_headers
 use interfaces,    ONLY:PARALLEL_global_indexes,PARALLEL_WF_index,PARALLEL_WF_distribute,WF_load
 use matrix,        ONLY:MATRIX_reset
 !
#include<memory.h>
 !
 type(levels)  :: Xen
 type(bz_samp) :: Xk,q
 type(X_t)     :: X
 type(w_samp)  :: wv 
 ! 
 ! Work Space
 !
 integer          :: iw,iq,i_G_shift,n_output
 real(SP)         :: output_data(5),Q_plus_G_pt(3),Q_plus_G_sq_modulus
 logical          :: l_skip_non_int_eps,eval_eels,Macroscopic_CPU
 !
 integer,external :: O_select_q_and_G
 !
 call section('*','Optics')
 !
 ! Basical Checks
 !
 call X_pre_setup(Xen,Xk,X)
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 allocate(X_par(1))
 !
 do iq=X%iq(1),X%iq(2)
   !
   ! PARALLEL indexes
   !==================
   !
   if (X%iq(1)==1) then
     if (iq==1) call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space_Zero_Momentum",X=X) 
     if (iq==2) call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space_Finite_Momentum",X=X) 
   else 
     if (iq==X%iq(1)) call PARALLEL_global_indexes(Xen,Xk,q,"Response_G_space_Finite_Momentum",X=X) 
   endif
   !
   ! WF distribution 
   !=================
   !
   call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,&
&                              B_index=PAR_IND_CON_BANDS_X(X%whoami),&
&                              Bp_index=PAR_IND_VAL_BANDS_X(X%whoami),&
&                              CLEAN_UP=.TRUE.)
   !
   if (iq==1) call PARALLEL_WF_index(COMM=PAR_COM_X_WORLD)
   if (iq> 1) call PARALLEL_WF_index(COMM=PAR_COM_Q_A2A)
   !
   if (WF_buffered_IO) call WF_load(WF_buffer,0,1,X%ib,(/1,Xk%nibz/),space='B',title='-BUFFER')
   !
   ! TDDFT setup
   !
   call TDDFT_do_X_W_typs(iq,X,wv)
   !
   ! ALDA (only once) 
   !
   if (l_alda_fxc.and..not.allocated(FXC)) then
     !
     call TDDFT_ALDA_G_space(Xen,Xk)
     !
   endif
   !
   if (iq>1) then
     if (.not.PAR_IND_Q%element_1D(iq)) cycle
   endif
   !
   ! Check if this q is compatible with the user defined direction in chartesian coordinates
   ! and if an additional RL vectors must be added
   !
   i_G_shift=O_select_q_and_G(iq,q,X,Q_plus_G_pt,Q_plus_G_sq_modulus)
   if (i_G_shift<0) cycle
   !
   ! In TDDFT with BS based kernel eps0 is non-sense
   ! as shifted of the K diagonal
   !
   l_skip_non_int_eps=l_bs_fxc
   !
   ! Arrays to be written in the o. files
   !
   YAMBO_ALLOC(Epsilon_ii,(wv%n_freqs,2))
   YAMBO_ALLOC(EEL,(wv%n_freqs,2))
   YAMBO_ALLOC(Alpha,(wv%n_freqs,2))
   if (l_bs_fxc) then
     YAMBO_ALLOC(X_fxc,(wv%n_freqs))
     X_fxc=cZERO
   endif
   !
   ! Frequencies (AFTER TDDFT SETUP!)
   !
   call FREQUENCIES_setup(wv)
   !
   ! Response function parallel allocation
   !
   call X_half_mat_check(Xen,Xk,wv)
   !
   call X_PARALLEL_alloc(X_par(1),X%ng,wv%n_freqs,"XUP")
   !
   Macroscopic_CPU=X_par(1)%rows(1)<=i_G_shift.and.i_G_shift<=X_par(1)%rows(2).and.&
&                  X_par(1)%cols(1)<=i_G_shift.and.i_G_shift<=X_par(1)%cols(2)
   !
   ! Check if EELS can be evaluated
   !
   eval_eels= OPTICS_eels(wv%p,wv%n_freqs,.TRUE.,X%ordering)
   !
   if (eval_alpha) eval_alpha=eval_eels
   !
   ! OutPut files...
   !
   if(.not.l_rpa_IP) call X_setup_file_names(iq,'inv',trim(FXC_type),'dyson',ig=i_G_shift)
   if(     l_rpa_IP) call X_setup_file_names(iq,'ip','','',ig=i_G_shift)
   !
   ! ... open ...
   !
   call of_open_close(eps_file_name,'ot')
   if (eval_eels)       call of_open_close(eel_file_name,'ot')
   if (.not.eval_eels)  eel_file_name=' '
   if (eval_alpha)      call of_open_close(alpha_file_name,'ot')
   if (.not.eval_alpha) alpha_file_name=' '
   if (l_bs_fxc)        call of_open_close(fxc_file_name,'ot')
   !
   ! Initial Messages
   !
   call X_write_q_plus_G(iq,Q_plus_G_pt,ig=i_G_shift)
   !
   ! Fxc descriptions
   !
   if (l_bs_fxc) then
     call X_write_descriptions(FXC_n_descs,FXC_description)
     call msg('o eps_q eel_q alpha','#',' TDDFT|Fxc size             :'//trim(intc(FXC_n_g_corr)),INDENT=0)    
     call msg('o eps_q eel_q alpha','#','      |Hartree size         :'//trim(intc(X%ng)),INDENT=0)    
     call msg('o eps_q eel_q alpha','#','      |Ordering             :'//trim(X%ordering),INDENT=0)    
     !
     ! Fxc file titles
     !
     call X_setup_headers('q^2Fxc')
     call msg('o fxc','#',(/headers(1),headers(3),headers(2)/),INDENT=0,USE_TABS=.true.)    
     call msg('o fxc','#')
   endif
   !
   ! BZ Double Grid
   !
   if (use_X_DbGd) then
     call msg('o eps_q eel_q fxc alpha','#')
     call msg('o eps_q eel_q fxc alpha','# IBZ Energy DbGd points:',Xk%nibz+Xk%nibz_FineGd,INDENT=0)
     call msg('o eps_q eel_q fxc alpha','#  BZ Energy DbGd points:',Xk%nbz+Xk%nbz_FineGd,INDENT=0)
   endif
   !
   if (iq==1) call PP_redux_wait
   if (iq> 1) call PP_redux_wait(COMM=PAR_COM_Q_A2A%COMM)
   !
   call of_open_close(eps_file_name)
   call of_open_close(eel_file_name)
   call of_open_close(alpha_file_name)
   call of_open_close(fxc_file_name)
   !
   ! e Table
   ! 1:e0     2:e     ==> Epsilon_ii(iw,1:2)
   ! 1:eel0   2:eel   ==> EEL(iw,1:2)
   ! 1:alpha0 6:alpha ==> Alpha(iw,1:2)
   !
   Epsilon_ii=cZERO
   EEL       =cZERO
   Alpha     =cZERO
   !
   call X_irredux(iq,X_par(1),Xen,Xk,wv,X)     
   !
   ! Without LF eps
   !
   if (Macroscopic_CPU) Epsilon_ii(:,1)=1.0_SP-X_par(1)%blc(i_G_shift,i_G_shift,:)*4.0_SP*pi/Q_plus_G_sq_modulus
   !
   ! X Dyson equation solver
   !
   if (l_rpa_IP) then
     !
     if (Macroscopic_CPU) Epsilon_ii(:,2)=Epsilon_ii(:,1)
     !
   else
     !
     call X_redux(iq,X_par(1),wv,X)
     !
     if (Macroscopic_CPU) Epsilon_ii(:,2)=1._SP/(X_par(1)%blc(i_G_shift,i_G_shift,:)*&
&                                                bare_qpg(iq,i_G_shift)**2/Q_plus_G_sq_modulus+1._SP)
     !
   endif
   !
   call of_open_close(eps_file_name,'oa')
   call of_open_close(eel_file_name,'oa')
   call of_open_close(alpha_file_name,'oa')
   !
   ! Unfortunately some of the variables need in this second bunch of messages is setup only in X_os
   !
   call X_write_messages_before_headers(iq,allocated(Xen%GreenF),X%Vnl_included,X%ordering)
   !
   ! Titles 
   !
   n_output=5
   if(l_rpa_IP.or.l_skip_non_int_eps) n_output=3
   call msg('o eps_q eel_q fxc alpha','#')
   call X_setup_headers('EPS')
   call msg('o eps_q',  '#',headers(:n_output),INDENT=0,USE_TABS=.true.)    
   call X_setup_headers('EEL')
   call msg('o eel_q',  '#',headers(:n_output),INDENT=0,USE_TABS=.true.)    
   call X_setup_headers('ALPHA')
   call msg('o alpha','#',headers(:n_output),INDENT=0,USE_TABS=.true.)    
   call msg('o eps_q eel_q alpha','#')
   !
   do iw=1,wv%n_freqs
     !
     ! Eps
     !
     output_data=(/real(wv%p(iw))*HA2EV,aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),&
&                  aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1))/)
     call msg('o eps_q','',output_data(:n_output),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   call of_open_close(eps_file_name)
   call of_open_close(eel_file_name)
   call of_open_close(alpha_file_name)
   !
   if (eval_eels) then 
     !
     call of_open_close(eel_file_name,'oa')
     call of_open_close(alpha_file_name,'oa')
     !
     eval_eels= OPTICS_eels(wv%p,wv%n_freqs,.TRUE.,X%ordering,Epsilon_ii(:,1),EEL(:,1))
     if(     l_rpa_IP) EEL(:,2) = EEL(:,1)
     if(.not.l_rpa_IP) eval_eels= OPTICS_eels(wv%p,wv%n_freqs,.FALSE.,X%ordering,Epsilon_ii(:,2),EEL(:,2))
     !
     if (eval_alpha) then
       !
       ! alpha = -eps_2_alpha X(1,1) / |q|^2    
       !       =  (1 -eps_M^-1) eps_2_alpha/4/pi
       !       =  (1 +eels    ) eps_2_alpha/4/pi  
       !
       Alpha(:,1)=(1._SP+EEL(:,1))/4._SP/pi*eps_2_alpha
       if(     l_rpa_IP) Alpha(:,2)= Alpha(:,1)
       if(.not.l_rpa_IP) Alpha(:,2)=(1._SP+EEL(:,2))/4._SP/pi*eps_2_alpha
       !
     endif
     !
     do iw=1,wv%n_freqs
       !
       ! EEL
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(EEL(iw,2)),real(EEL(iw,2)),&
&                    aimag(EEL(iw,1)),real(EEL(iw,1))/)
       call msg('o eel_q','',output_data(:n_output),INDENT=-2,USE_TABS=.true.)
       !
       ! Alpha
       !
       output_data=(/real(wv%p(iw))*HA2EV,aimag(Alpha(iw,2)),real(Alpha(iw,2)),&
&                    aimag(Alpha(iw,1)),real(Alpha(iw,1))/)
       !
       call msg('o alpha','',output_data(:n_output),INDENT=-2,USE_TABS=.true.)
     enddo
     !
     call of_open_close(eel_file_name)
     call of_open_close(alpha_file_name)
     !
   endif
   !
   ! TDDFT Kernel output
   !
   if (l_bs_fxc) then
     if (iq==1) call PP_redux_wait(X_fxc)
     if (iq> 1) call PP_redux_wait(X_fxc,COMM=PAR_COM_Q_A2A%COMM)
     call of_open_close(fxc_file_name,'oa')
     do iw=1,wv%n_freqs
       call msg('o fxc','',(/real(wv%p(iw))*HA2EV,real(X_fxc(iw)*q_norm(iq)**2),&
&                           aimag(X_fxc(iw)*q_norm(iq)**2)/),INDENT=-2,USE_TABS=.true.)
     enddo
     call of_open_close(fxc_file_name)
   endif
   !
   ! CLEAN (each q)
   !
   YAMBO_FREE(EEL)
   YAMBO_FREE(Alpha)
   YAMBO_FREE(Epsilon_ii)
   YAMBO_FREE(X_fxc)
   call FREQUENCIES_reset(wv,"all")
   call X_PARALLEL_alloc(X_par(1),0,0,"CLEAN")
   YAMBO_FREE(X_par_lower_triangle%blc)
   !
 enddo
 !
 ! CLEAN
 !
 call MATRIX_reset(X_par(1))
 call MATRIX_reset(X_par_lower_triangle)
 deallocate(X_par)
 call X_alloc('DIP_projected')
 call WF_free(WF)
 call PP_redux_wait
 call set_real_printed_length()
 if (allocated(FXC)) then
   YAMBO_FREE(FXC)
 endif
 if (l_bs_fxc) then
   YAMBO_FREE(FXC)
   YAMBO_FREE(FXC_K_diagonal)
 endif
 !
end subroutine
