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
subroutine K_output_file(iq,mode)
 !
 use pars,      ONLY:pi,SP
 use units,     ONLY:HA2EV
 use BS,        ONLY:BSS_description, BSS_uses_GreenF,BSS_Vnl_included,&
&                    BSS_n_descs,BS_K_coupling,BS_K_is_ALDA,BSS_n_freqs,BSS_q0
 use X_m,       ONLY:X_epsilon,eval_alpha,eps_2_alpha,O_eels
 use com,       ONLY:msg,of_open_close
 use X_output,  ONLY:headers,X_setup_headers,X_setup_file_names,eps_file_name,&
&                    eel_file_name,alpha_file_name,X_write_descriptions,&
&                    X_write_messages_before_headers,X_write_q_plus_g
 implicit none
 integer    :: mode,iq
 !
 ! Work Space
 !
 character(1)      :: ordering
 character(2)      :: title_mode
 integer           :: iw,n_output_data
 real(SP)          :: output_data(7)
 complex(SP)       :: eels(3,BSS_n_freqs),alpha(3,BSS_n_freqs)
 logical           :: eval_eels
 !
 ! File Names 
 !
 !
 select case (-mode) 
   case(1)
     ! Haydock
     call X_setup_file_names(1,'haydock',' ','bse')
     if (BS_K_is_ALDA)  call X_setup_file_names(1,'haydock','alda','bse')
   case(2)
     ! Diago
     call X_setup_file_names(1,'diago',' ','bse')
     if (BS_K_is_ALDA)  call X_setup_file_names(1,'diago','alda','bse')
   case(3)
     ! Inversion
     call X_setup_file_names(1,'inv',' ','bse')
     if (BS_K_is_ALDA)  call X_setup_file_names(1,'inv','alda','bse')
 end select
 !
 ! EELS 
 !
 ordering='r'
 if (BS_K_coupling) ordering='c'
 eval_eels= O_eels(X_epsilon(1,:),BSS_n_freqs,.TRUE.,ordering)
 !
 title_mode='Ot'
 if (mode<0) title_mode='ot'
 !
 call of_open_close(eps_file_name,title_mode)
 if (eval_eels) then 
   call of_open_close(eel_file_name,title_mode)
   if (eval_alpha) call of_open_close(alpha_file_name,title_mode)
 endif
 !
 call X_write_q_plus_g(1,BSS_q0,ig=1)
 call X_write_messages_before_headers(iq,BSS_uses_GreenF,BSS_Vnl_included,ordering)
 !
 ! Description of the run and headers for output files:
 !
 call X_write_descriptions(BSS_n_descs,BSS_description)
 !
 if (mode>0) then 
   !
   ! Headers
   !
   call X_setup_headers('eps')
   if (mode==1) call msg("o eps",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (mode/=1) call msg("o eps",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   !
   if (eval_eels) then 
     call X_setup_headers('eel')
     if (mode==1) call msg("o eel",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (mode/=1) call msg("o eel",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   end if   
   !
   if (eval_alpha) then 
     call X_setup_headers('alpha')
     if (mode==1) call msg("o alpha",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (mode/=1) call msg("o alpha",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   endif
   call msg("o eps eel alpha",'#')
   !
   ! ABSORPTION
   !------------
   !
   do iw=1,BSS_n_freqs
     !
     ! X_epsilon: 2 (int) 3 (non int) 4 (n-1 Haydock)
     !
     output_data(1:5)=(/real(X_epsilon(1,iw))*HA2EV,aimag(X_epsilon(2,iw)),real(X_epsilon(2,iw)),&
&              aimag(X_epsilon(3,iw)),real(X_epsilon(3,iw))/)
     !
     ! Haydock n-1 iteration
     !
     n_output_data=5
     if (mode==1) n_output_data=7
     if (mode==1) output_data(6:7)=(/aimag(X_epsilon(4,iw)),real(X_epsilon(4,iw))/)
     !
     call msg("o eps",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   ! EELS & POLARIZABILITY
   !---------------------
   !
   if (eval_eels) then
     !
     eval_eels=O_eels(X_epsilon(1,:),BSS_n_freqs,.FALSE.,ordering,X_epsilon(2,:),eels(1,:)) 
     eval_eels=O_eels(X_epsilon(1,:),BSS_n_freqs,.FALSE.,ordering,X_epsilon(3,:),eels(2,:)) 
     if (mode==1) eval_eels=O_eels(X_epsilon(1,:),BSS_n_freqs,.FALSE.,ordering,X_epsilon(4,:),eels(3,:)) 
     !
     alpha(1,:)=(1.+eels(1,:))/4./pi*eps_2_alpha
     alpha(2,:)=(1.+eels(2,:))/4./pi*eps_2_alpha
     if (mode==1) alpha(3,:)=(1.+eels(3,:))/4./pi*eps_2_alpha
     !
     do iw=1,BSS_n_freqs
       !
       ! EELS
       !
       output_data(1:5)=(/real(X_epsilon(1,iw))*HA2EV,aimag(eels(1,iw)),real(eels(1,iw)),&
&                aimag(eels(2,iw)),real(eels(2,iw))/)
       n_output_data=5
       if (mode==1) n_output_data=7
       if (mode==1) output_data(6:7)=(/aimag(eels(3,iw)),real(eels(3,iw))/)
       call msg("o eel",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
       !
       ! ALPHA
       !
       output_data(1:5)=(/real(X_epsilon(1,iw))*HA2EV,aimag(alpha(1,iw)),real(alpha(1,iw)),&
&                aimag(alpha(2,iw)),real(alpha(2,iw))/)
       n_output_data=5
       if (mode==1) n_output_data=7
       if (mode==1) output_data(6:7)=(/aimag(alpha(3,iw)),real(alpha(3,iw))/)
       call msg("o alpha",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
       !
     enddo
   endif
   !
 endif
 !
 call of_open_close(eps_file_name)
 call of_open_close(eel_file_name)
 call of_open_close(alpha_file_name)
 !
end subroutine
