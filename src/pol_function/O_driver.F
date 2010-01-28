!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine O_driver(Xen,Xk,q,wv,X)
 ! 
 ! Optics driver.
 !
 ! Calculates dielectric function for a generic q vector.
 ! 
 use pars,          ONLY:pi,SP
 use units,         ONLY:HARTREE
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:intc,set_real_printed_length
 use drivers,       ONLY:l_bs_fxc,l_alda_fxc
 use frequency,     ONLY:w_samp,rg_index_bg,bg_npts,cg_pt,cg_index_bg
 use electrons,     ONLY:levels,BZ_RIM_tot_nkpts
 use R_lattice,     ONLY:bz_samp,q_norm,bare_qpg
 use com,           ONLY:msg,of_open_close
 use X_m,           ONLY:X_t,X_epsilon,X_mat,X_alloc,X_poles_tab,X_fxc,&
&                        use_X_RIM,eval_alpha,eps_2_alpha,O_eels
 use par_proc_m,    ONLY:pp_redux_wait
 use wave_func,     ONLY:WF_free
 use TDDFT,         ONLY:FXC_n_descs,FXC_description,FXC_type,&
&                        FXC,FXC_K_diagonal,FXC_n_g_corr
 use X_output,      ONLY:X_setup_file_names,eps_file_name,eel_file_name,alpha_file_name,fxc_file_name,&
&                        X_write_q_plus_G,X_write_descriptions,X_write_messages_before_headers,headers,&
&                        X_setup_headers
 !
 implicit none
 type(levels)  ::Xen
 type(bz_samp) ::Xk,q
 type(X_t)     :: X
 type(w_samp)  :: wv 
 ! 
 ! Work Space
 !
 integer          :: iw_group,iw,i2,frequencies_range(2),iq,i_G_shift
 real(SP)         :: output_data(5),Q_plus_G_pt(3),Q_plus_G_sq_modulus
 logical          :: l_skip_non_int_eps,eval_eels
 !
 integer,external :: O_select_q_and_G
 !
 call section('*','Optics')
 !
 ! Basical Checks
 !
 call X_pre_setup(Xen,X)
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 do iq=X%iq(1),X%iq(2)
   !
   ! Check if this q is compatible with the user defined direction in chartesian coordinates
   ! and if an additional RL vectors must be added
   !
   i_G_shift=O_select_q_and_G(iq,q,X,Q_plus_G_pt,Q_plus_G_sq_modulus)
   if (i_G_shift<0) cycle
   !
   ! TDDFT setup
   !
   call tddft_do_X_W_typs(iq,X,wv)
   !
   if (l_alda_fxc) call tddft_alda_g_space(Xen,Xk)
   !
   ! In TDDFT with BS based kernel eps0 is non-sense
   ! as shifted of the K diagonal
   !
   l_skip_non_int_eps=l_bs_fxc
   !
   ! Arrays to be written in the o. files
   !
   allocate(X_epsilon(8,wv%n(1)))
   if (l_bs_fxc) then
     allocate(X_fxc(wv%n(1)))
     X_fxc=(0._SP,0._SP)
   endif
   !
   ! X_mat allocation
   !
   call X_alloc('X',(/X%ng,X%ng,wv%n(2)/))
   !
   ! Frequencies (AFTER TDDFT SETUP!)
   !
   call FREQUENCIES_setup(wv)
   !
   ! Check if EELS can be evaluated
   !
   eval_eels= O_eels(wv%p,wv%n(1),.TRUE.,X%ordering)
   if (eval_alpha) eval_alpha=eval_eels
   !
   ! OutPut files...
   !
   call X_setup_file_names(iq,'inv',trim(FXC_type),'dyson',ig=i_G_shift)
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
     call msg('o eps eel alpha','#',' TDDFT|Fxc size             :'//trim(intc(FXC_n_g_corr)),INDENT=0)    
     call msg('o eps eel alpha','#','      |Hartree size         :'//trim(intc(X%ng)),INDENT=0)    
     call msg('o eps eel alpha','#','      |Ordering             :'//trim(X%ordering),INDENT=0)    
     !
     ! Fxc file titles
     !
     call X_setup_headers('q2Fxc')
     call msg('o fxc','#',(/headers(1),headers(3),headers(2)/),INDENT=0,USE_TABS=.true.)    
     call msg('o fxc','#')
   endif
   !
   ! BZ RIM
   !
   if (use_X_RIM) then
     call msg('o eps eel fxc alpha','#')
     call msg('o eps eel fxc alpha','# BZ Energy RIM points :',BZ_RIM_tot_nkpts,INDENT=0)
   endif
   !
   call pp_redux_wait
   !
   call of_open_close(eps_file_name)
   call of_open_close(eel_file_name)
   call of_open_close(alpha_file_name)
   call of_open_close(fxc_file_name)
   !
   ! e Table
   ! 1:e0 2:e 3:eel0 4:eel 5:alpha0 6:alpha 
   !
   X_epsilon=(0.,0.)
   do iw_group=1,wv%n(1),wv%n(2)
     !
     frequencies_range=(/iw_group,iw_group+wv%n(2)-1/)
     call X_os(X_mat,iq,frequencies_range,Xen,Xk,wv,X)     
     !
     ! Without LF eps/alpha
     !
     X_epsilon(1,frequencies_range(1):frequencies_range(2))=1.0_SP-X_mat(i_G_shift,i_G_shift,:)*4.0_SP*pi/Q_plus_G_sq_modulus
     !
     ! X Dyson equation solver
     !
     call X_s(iq,frequencies_range,X,wv)
     !
     X_epsilon(2,frequencies_range(1):frequencies_range(2))=1./(X_mat(i_G_shift,i_G_shift,:)*&
&                                                           bare_qpg(iq,1)**2/Q_plus_G_sq_modulus+1.)
     !
     call of_open_close(eps_file_name,'oa')
     call of_open_close(eel_file_name,'oa')
     call of_open_close(alpha_file_name,'oa')
     !
     if (iw_group==1) then
       !
       ! Unfortunately some of the variables need in this second bunch of messages is setup only in X_os
       !
       call X_write_messages_before_headers(iq,associated(Xen%GreenF),X%Vnl_included,X%long_gauge,X%ordering)
       !
       ! Titles 
       !
       call X_setup_headers('eps')
       call msg('o eps eel fxc alpha','#')
       if (.not.l_skip_non_int_eps) call msg('o eps','#',headers(:5),INDENT=0,USE_TABS=.true.)    
       if (l_skip_non_int_eps)      call msg('o eps','#',headers(:3),INDENT=0,USE_TABS=.true.)    
       call X_setup_headers('eel')
       if (.not.l_skip_non_int_eps) call msg('o eel','#',headers(:5),INDENT=0,USE_TABS=.true.)    
       if (l_skip_non_int_eps)      call msg('o eel','#',headers(:3),INDENT=0,USE_TABS=.true.)    
       call X_setup_headers('alpha')
       if (.not.l_skip_non_int_eps) call msg('o alpha','#',headers(:5),INDENT=0,USE_TABS=.true.)    
       if (l_skip_non_int_eps)      call msg('o alpha','#',headers(:3),INDENT=0,USE_TABS=.true.)    
       call msg('o eps eel alpha','#')
       !
     endif
     !
     do i2=frequencies_range(1),frequencies_range(2)
       !
       ! Eps
       !
       output_data=(/real(wv%p(i2))*HARTREE,aimag(X_epsilon(2,i2)),real(X_epsilon(2,i2)),&
&                    aimag(X_epsilon(1,i2)),real(X_epsilon(1,i2))/)
       if (l_skip_non_int_eps)      call msg('o eps','',output_data(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.l_skip_non_int_eps) call msg('o eps','',output_data,INDENT=-2,USE_TABS=.true.)
       !
     enddo
     call of_open_close(eps_file_name)
     call of_open_close(eel_file_name)
     call of_open_close(alpha_file_name)
   enddo
   !
   if (eval_eels) then 
     !
     call of_open_close(eel_file_name,'oa')
     call of_open_close(alpha_file_name,'oa')
     !
     eval_eels= O_eels(wv%p,wv%n(1),.TRUE.,X%ordering,X_epsilon(1,:),X_epsilon(3,:))
     eval_eels= O_eels(wv%p,wv%n(1),.FALSE.,X%ordering,X_epsilon(2,:),X_epsilon(4,:))
     !
     if (eval_alpha) then
       !
       ! alpha = -eps_2_alpha X(1,1) / |q|^2    
       !       =  (1 -eps_M^-1) eps_2_alpha/4/pi
       !       =  (1 +eels    ) eps_2_alpha/4/pi  
       !
       X_epsilon(5,:)=(1.+X_epsilon(3,:))/4./pi*eps_2_alpha
       X_epsilon(6,:)=(1.+X_epsilon(4,:))/4./pi*eps_2_alpha 
       !
     endif
     !
     do iw=1,wv%n(1)
       !
       ! EEL
       !
       output_data=(/real(wv%p(iw))*HARTREE,aimag(X_epsilon(4,iw)),real(X_epsilon(4,iw)),&
&                    aimag(X_epsilon(3,iw)),real(X_epsilon(3,iw))/)
       if (l_skip_non_int_eps)      call msg('o eel','',output_data(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.l_skip_non_int_eps) call msg('o eel','',output_data,INDENT=-2,USE_TABS=.true.)
       !
       ! Alpha
       !
       output_data=(/real(wv%p(iw))*HARTREE,aimag(X_epsilon(6,iw)),real(X_epsilon(6,iw)),&
&                    aimag(X_epsilon(5,iw)),real(X_epsilon(5,iw))/)
       !
       if (l_skip_non_int_eps)      call msg('o alpha','',output_data(:3),INDENT=-2,USE_TABS=.true.)
       if (.not.l_skip_non_int_eps) call msg('o alpha','',output_data,INDENT=-2,USE_TABS=.true.)
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
     call pp_redux_wait(X_fxc)
     call of_open_close(fxc_file_name,'oa')
     do iw=1,wv%n(1)
       call msg('o fxc','',(/real(wv%p(iw))*HARTREE,real(X_fxc(iw)*q_norm(iq)**2.),&
&                           aimag(X_fxc(iw)*q_norm(iq)**2.)/),INDENT=-2,USE_TABS=.true.)
     enddo
     call of_open_close(fxc_file_name)
   endif
   !
   ! CLEAN (each q)
   !
   call X_alloc('X')
   !
   deallocate(X_epsilon,wv%p)
   call mem_est("W-p")
   if (allocated(X_fxc)) deallocate(X_fxc)
   if (allocated(rg_index_bg)) deallocate(rg_index_bg)
   deallocate(X_poles_tab,bg_npts,cg_pt,cg_index_bg)
   call mem_est("X_poles_tab RGi BGn CGp CGi")
 enddo
 !
 ! CLEAN
 !
 call X_alloc('OptOsc')
 call WF_free()
 call pp_redux_wait
 call set_real_printed_length()
 !
 if (l_bs_fxc.and.allocated(FXC)) then
   deallocate(FXC,FXC_K_diagonal)
   call mem_est("FXC FXC_K_diagonal")
 endif
 !
end subroutine
