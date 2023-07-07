!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine OPTICS_driver(Xen,Xk,q,wv,X,Dip)
 ! 
 ! Optics driver
 !
 ! Calculates dielectric function for a generic q vector.
 ! 
 use pars,          ONLY:pi,SP
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc,l_alda_fxc,l_pf_fxc,l_col_cut
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels
 use D_lattice,     ONLY:l_0D
 use R_lattice,     ONLY:bz_samp,q_norm,bare_qpg,FineGd_desc
 use DIPOLES,       ONLY:DIPOLE_t,DIP_desc
 use X_m,           ONLY:X_t,Epsilon_ii,EEL,Alpha,X_fxc,X_par,X_Double_Grid_setup,&
&                        use_X_DbGd,eps_2_alpha,EPS_2_eels,X_par_lower_triangle,X_OUTPUT_messages,&
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
 integer          :: iw,iq,i_G_shift
 real(SP)         :: output_data(5),Q_plus_G_pt(3),Q_plus_G_sq_modulus,a_xc,b_xc
 logical          :: l_skip_non_int_eps,Macroscopic_CPU,l_eels
 !
 integer,external :: OPTICS_select_q_and_G
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
   ! Check if EELS can be evaluated
   if (X_do_obs("eel")) l_eels=EPS_2_eels(wv%p,wv%n_freqs,.TRUE.,X%ordering)
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
   if (iq==1) call PP_wait
   if (iq> 1) call PP_wait(COMM=PAR_COM_Q_A2A%COMM)
   !
   ! Irredux (Xo) 
   call X_irredux(iq,"Xo",X_par(1),Xen,Xk,wv,X,Dip)
   !
   ! Without LF eps
   if (Macroscopic_CPU) Alpha(:,1)=-X_par(1)%blc(i_G_shift,i_G_shift,:)*4.0_SP*pi/Q_plus_G_sq_modulus
   call PP_redux_wait(Alpha(:,1),COMM=PAR_COM_RL_INDEX%COMM)
   Epsilon_ii(:,1)=1._SP+Alpha(:,1)
   !
   ! X Dyson equation solver
   if (.not.l_rpa_IP) call X_redux(iq,"X",X_par(1),wv,X)
   !
   ! X_redux can change the matrix distribution. Need to recheck who is Macroscopic CPU
   Macroscopic_CPU=X_par(1)%rows(1)<=i_G_shift.and.i_G_shift<=X_par(1)%rows(2).and.&
&                  X_par(1)%cols(1)<=i_G_shift.and.i_G_shift<=X_par(1)%cols(2)
   !
   if (Macroscopic_CPU) then
     !
     if (     l_rpa_IP) then
       Alpha(:,2)=Alpha(:,1)
     else
       if (.not.l_col_cut) Alpha(:,2)=1._SP/(X_par(1)%blc(i_G_shift,i_G_shift,:)+1._SP)-1._SP
       ! Here I assume X_par is small and I use 1/(1+x)~1-x 
       if (     l_col_cut) Alpha(:,2)=-X_par(1)%blc(i_G_shift,i_G_shift,:)
     endif
   endif
   call PP_redux_wait(Alpha(:,2),COMM=PAR_COM_RL_INDEX%COMM)
   !
   Epsilon_ii(:,2)=1._SP+Alpha(:,2)
   !
   ! I make alpha consistent with the above, IP, definition
   ! To check if this need to be fixed with i_G_shift/=1
   if (l_col_cut.or.l_0D) Alpha(:,2)=Alpha(:,2)*bare_qpg(iq,1)**2/q_norm(iq)**2
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
   !
   ! Unfortunately some of the variables need in this second bunch of messages is setup only in X_os
   call X_OUTPUT_messages(iq,GF=allocated(Xen%GreenF),Vnl=X%Vnl_included,ordering=X%ordering)
   !
   ! Descriptors 
   call X_OUTPUT_driver("DESC",DESC=DIP_desc)
   call X_OUTPUT_driver("DESC",DESC=FineGd_desc)
   if (l_bs_fxc) call X_OUTPUT_driver("DESC",DESC=FXC_desc)
   !
   ! Epsilon
   do iw=1,wv%n_freqs
     output_data=(/real(wv%p(iw)),aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),&
&                  aimag(Epsilon_ii(iw,1)),real(Epsilon_ii(iw,1))/)
     if (     l_rpa_IP) call X_OUTPUT_driver("WRITE eps",data=output_data(:3))
     if (.not.l_rpa_IP) call X_OUTPUT_driver("WRITE eps",data=output_data)
   enddo
   !
   ! EEL
   if (X_do_obs("eel")) then 
     l_eels= EPS_2_eels(wv%p,wv%n_freqs,.TRUE.,X%ordering,Epsilon_ii(:,1),EEL(:,1))
     if(     l_rpa_IP) EEL(:,2) = EEL(:,1)
     if(.not.l_rpa_IP) l_eels= EPS_2_eels(wv%p,wv%n_freqs,.FALSE.,X%ordering,Epsilon_ii(:,2),EEL(:,2))
     do iw=1,wv%n_freqs
       output_data=(/real(wv%p(iw)),aimag(EEL(iw,2)),real(EEL(iw,2)),aimag(EEL(iw,1)),real(EEL(iw,1))/)
       if (     l_rpa_IP) call X_OUTPUT_driver("WRITE eel",data=output_data(:3))
       if (.not.l_rpa_IP) call X_OUTPUT_driver("WRITE eel",data=output_data)
     enddo
   endif
   !
   ! Alpha
   if (X_do_obs("alpha")) then 
     ! alpha = -eps_2_alpha X(1,1) / |q|^2    
     !       =  (1 -eps_M^-1) eps_2_alpha/4/pi
     !       =  (1 +eels    ) eps_2_alpha/4/pi  
     if (      l_col_cut.or.l_0D ) Alpha(:,1:2)=Alpha(:,1:2)*eps_2_alpha/4._SP/pi
     if (.not.(l_col_cut.or.l_0D)) Alpha(:,1:2)=(1._SP+EEL(:,1:2))*eps_2_alpha/4._SP/pi
     do iw=1,wv%n_freqs
       output_data=(/real(wv%p(iw)),aimag(Alpha(iw,2)),real(Alpha(iw,2)),aimag(Alpha(iw,1)),real(Alpha(iw,1))/)
       if (     l_rpa_IP) call X_OUTPUT_driver("WRITE alpha",data=output_data(:3))
       if (.not.l_rpa_IP) call X_OUTPUT_driver("WRITE alpha",data=output_data)
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
