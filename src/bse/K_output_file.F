!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
#if defined _KERR
 use pars,      ONLY:cI
 use units,     ONLY:DEG2RAD,HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI,e_SI,hbar_SI
 use drivers,   ONLY:l_kerr,l_anomalous_Hall
 use X_output,  ONLY:moke_file_name,off_file_name,beta_file_name
 use KERR,      ONLY:B_Hall
#endif
 !
 implicit none
 integer    :: mode,iq
 !
 ! Work Space
 !
 character(1)      :: ordering
 character(2)      :: title_mode
 integer           :: iw,n_output_data
 real(SP)          :: output_data(7)
 complex(SP)       :: eels(3,BSS_n_freqs),alpha(3,BSS_n_freqs),beta(3,BSS_n_freqs)
 logical           :: eval_eels
 !
 ! File Names 
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
   case(4)
     ! Independent-Particle
     call X_setup_file_names(1,'IP',' ',' ')
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
#if defined _KERR
 if(l_kerr) then
   call of_open_close(off_file_name,title_mode)
   if (eval_alpha) call of_open_close(beta_file_name,title_mode)
   call of_open_close(moke_file_name,title_mode)
 endif
#endif
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
   if (mode/=1.and.mode/=4) call msg("o eps",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (mode==4) call msg("o eps",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   !
   if (eval_eels) then 
     call X_setup_headers('eel')
     if (mode==1) call msg("o eel",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (mode/=1.and.mode/=4) call msg("o eel",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
     if (mode==4) call msg("o eel",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   end if   
   !
   if (eval_alpha) then 
     call X_setup_headers('alpha')
     if (mode==1) call msg("o alpha",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (mode/=1.and.mode/=4) call msg("o alpha",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
     if (mode==4) call msg("o alpha",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   endif
   !
#if defined _KERR
   if(l_kerr) then
     !
     ! Hanomalous Hall term
     !
     if(l_anomalous_Hall) then
       call msg('o off','#','Anomalous Hall effect sH',INDENT=0,USE_TABS=.true.)
       !
       n_output_data=6
       if(mode==4) n_output_data=3
       headers(:6)=(/'sH ','   ','   ','sH0','   ','   '/)
       call msg("o off",'#',headers(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
       headers(:6)=(/'  [fs]^-1  ','[Ohm*cm]^-1','[e^2/hbar] ','  [fs]^-1  ','[Ohm*cm]^-1','[e^2/hbar] '/)
       call msg("o off",'#',headers(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
       !
       B_Hall(:)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
       output_data(1)=real(B_Hall(2))
       output_data(4)=real(B_Hall(1))
       B_Hall(:)=B_Hall(:)*1.E5/(SPEED_OF_LIGHT_SI)**2/FEMTO
       output_data(2)=real(B_Hall(2))
       output_data(5)=real(B_Hall(1))
       B_Hall(:)=B_Hall(:)*1.E2*HBAR_SI/e_SI**2
       output_data(3)=real(B_Hall(2))
       output_data(6)=real(B_Hall(1))
       if(mode==4) output_data(1:3)=output_data(4:6)
       call msg('o off','#',output_data(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
       call msg('o off','#')
       !
     endif
     !
     call X_setup_headers('off')
     if (mode==1) call msg("o off",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (mode/=1.and.mode/=4) call msg("o off",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
     if (mode==4) call msg("o off",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
     !
     if (eval_alpha) then 
       call X_setup_headers('beta')
       if (mode==1) call msg("o beta",'#',headers,INDENT=0,USE_TABS=.TRUE.)
       if (mode/=1.and.mode/=4) call msg("o beta",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
       if (mode==4) call msg("o beta",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
     endif
     !
     call X_setup_headers('moke')
     if (mode==1) call msg("o moke",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (mode/=1.and.mode/=4) call msg("o moke",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
     if (mode==4) call msg("o moke",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
     !
   endif
#endif
   !
   call msg("o eps eel alpha off beta moke",'#')
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
     if (mode==4) n_output_data=3
     if (mode==4) output_data(2:3)=output_data(4:5)
     !
     call msg("o eps",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   ! EELS & POLARIZABILITY
   !-----------------------
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
       if (mode==4) n_output_data=3
       if (mode==4) output_data(2:3)=output_data(4:5)
       call msg("o eel",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
       !
       ! ALPHA
       !
       output_data(1:5)=(/real(X_epsilon(1,iw))*HA2EV,aimag(alpha(1,iw)),real(alpha(1,iw)),&
&                aimag(alpha(2,iw)),real(alpha(2,iw))/)
       n_output_data=5
       if (mode==1) n_output_data=7
       if (mode==1) output_data(6:7)=(/aimag(alpha(3,iw)),real(alpha(3,iw))/)
       if (mode==4) n_output_data=3
       if (mode==4) output_data(2:3)=output_data(4:5)
       call msg("o alpha",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
       !
     enddo
     !
   endif
   !
#if defined _KERR
   !
   ! KERR 
   !------
   if(l_kerr) then
     !
     if(eval_alpha) then
       beta(1,:)=X_epsilon(5,:)/4./pi*eps_2_alpha
       beta(2,:)=X_epsilon(6,:)/4./pi*eps_2_alpha
     endif
     !
     ! eps_xy
     !
     do iw=1,BSS_n_freqs 
       output_data(1:5)=(/real(X_epsilon(1,iw))*HA2EV,aimag(X_epsilon(5,iw)),real(X_epsilon(5,iw)),&
&                    aimag(X_epsilon(6,iw)),real(X_epsilon(6,iw))/)
       n_output_data=5
       if (mode==1) n_output_data=7
       if (mode==1) output_data(6:7)=(/aimag(X_epsilon(7,iw)),real(X_epsilon(7,iw))/)
       if (mode==4) n_output_data=3
       if (mode==4) output_data(2:3)=output_data(4:5)
       call msg('o off','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
       !
       if(eval_alpha) then
         !
         ! alpha_xy
         !
         output_data(1:5)=(/real(X_epsilon(1,iw))*HA2EV,aimag(beta(1,iw)),real(beta(1,iw)),&
&                  aimag(beta(2,iw)),real(beta(2,iw))/)
         n_output_data=5
         if (mode==1) n_output_data=7
         if (mode==1) output_data(6:7)=(/aimag(beta(3,iw)),real(beta(3,iw))/)
         if (mode==4) n_output_data=3
         if (mode==4) output_data(2:3)=output_data(4:5)
         call msg("o beta",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
         !
       endif
       !
     enddo
     !
     ! Kerr parameters
     !
     do iw=1,BSS_n_freqs 
       output_data(1)= real(X_epsilon(1,iw))*HA2EV
       output_data(2)=-real(X_epsilon(5,iw)/( (X_epsilon(2,iw)-1.0_SP)*sqrt(X_epsilon(2,iw)) ) /DEG2RAD)
       output_data(3)=-aimag(X_epsilon(5,iw)/( (X_epsilon(2,iw)-1.0_SP)*sqrt(X_epsilon(2,iw)) ) /DEG2RAD)
       output_data(4)=-real(X_epsilon(6,iw)/( (X_epsilon(3,iw)-1.0_SP)*sqrt(X_epsilon(3,iw)) ) /DEG2RAD)
       output_data(5)=-aimag(X_epsilon(6,iw)/( (X_epsilon(3,iw)-1.0_SP)*sqrt(X_epsilon(3,iw)) ) /DEG2RAD)
       !
       n_output_data=5
       if (mode==1) n_output_data=7
       if (mode==1) then
         output_data(6)=-real(X_epsilon(7,iw)/( (X_epsilon(4,iw)-1.0_SP)*sqrt(X_epsilon(4,iw)) ) /DEG2RAD)
         output_data(7)=-aimag(X_epsilon(7,iw)/( (X_epsilon(4,iw)-1.0_SP)*sqrt(X_epsilon(4,iw)) ) /DEG2RAD)
       endif
       if (mode==4) n_output_data=3
       if (mode==4) output_data(2:3)=output_data(4:5)
       call msg('o moke','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     enddo
     !
   endif
   !
#endif
   !
 endif
 !
 call of_open_close(eps_file_name)
 call of_open_close(eel_file_name)
 call of_open_close(alpha_file_name)
 !
#if defined _KERR
 if(l_kerr) then
   call of_open_close(off_file_name)
   call of_open_close(beta_file_name)
   call of_open_close(moke_file_name)
 endif
#endif
 !
end subroutine
