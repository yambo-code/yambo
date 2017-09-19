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
subroutine K_output_file(iq,mode)
 !
 use pars,      ONLY:pi,SP
 use units,     ONLY:HA2EV
 use BS_solvers,ONLY:BSS_description, BSS_uses_GreenF,BSS_Vnl_included,&
&                    BSS_n_descs,BSS_n_freqs,BSS_q0,K_diago_in_BS_mat
 use BS,        ONLY:BSE_mode,BS_K_is_ALDA
 use X_m,       ONLY:Epsilon_ii,Joint_DOS,eval_alpha,eps_2_alpha,OPTICS_eels
 use com,       ONLY:msg,of_open_close
 use parser_m,  ONLY:parser
 use X_output,  ONLY:headers,X_setup_headers,X_setup_file_names,eps_file_name,jdos_file_name,&
&                    eel_file_name,alpha_file_name,refl_file_name,X_write_descriptions,&
&                    X_write_messages_before_headers,X_write_q_plus_g
#if defined _KERR
 use pars,      ONLY:cI
 use electrons, ONLY:spin_occ
 use R_lattice, ONLY:nkbz
 use units,     ONLY:DEG2RAD,HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI,e_SI,HBAR_SI
 use drivers,   ONLY:l_kerr
 use X_m,       ONLY:Epsilon_ij
 use X_output,  ONLY:moke_file_name,off_file_name,beta_file_name
 use KERR,      ONLY:B_Hall,A_drude
#endif
 !
 implicit none
 integer      :: iq
 character(*) :: mode
 !
 ! Work Space
 !
 character(2)      :: ordering
 character(2)      :: title_mode
 character(10)     :: name_
 integer           :: iw,n_output_data
 real(SP)          :: output_data(7)
 complex(SP)       :: eels(BSS_n_freqs,3),alpha(BSS_n_freqs,3),refl(BSS_n_freqs,3)
#if defined _KERR
 complex(SP)       :: beta(BSS_n_freqs,3)
#endif
 logical           :: eval_eels,eval_reflectivity,l_Haydock,l_Diago,l_Inversion,l_IP,l_INIT
 !
 l_INIT      = index(mode,"open")/=0
 l_Haydock   = index(mode,"Haydock")/=0
 l_Diago     = index(mode,"Diago")/=0
 l_Inversion = index(mode,"Inversion")/=0
 l_IP        = index(mode,"IP")/=0
 !
 ! File Names 
 !
 if (l_Haydock)   name_="haydock"
 if (l_Diago)     name_="diago"
 if (l_Inversion) name_="inv"
 if (l_IP)        name_="IP"
 !
 if (l_INIT) then
   if (l_IP) then
     call X_setup_file_names(1,trim(name_),' ',' ')
   else
     call X_setup_file_names(1,trim(name_),' ','bse')
     if (BS_K_is_ALDA) call X_setup_file_names(1,trim(name_),'alda','bse')
   endif
 endif
 !
 if (     trim(BSE_mode)=='resonant' ) ordering='r'
 if (.not.trim(BSE_mode)=='resonant' ) ordering='R'
 !
 ! EELS / REFLECTIVITY logicals 
 !
 call parser('Reflectivity',eval_reflectivity)
 eval_eels= OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.TRUE.,ordering)
 !
 title_mode='Ot'
 if (l_INIT) title_mode='ot'
 !
 call of_open_close(eps_file_name,title_mode)
 if (eval_reflectivity) call of_open_close(refl_file_name,title_mode)
 if (eval_eels)         call of_open_close(eel_file_name,title_mode)
 if (eval_alpha)        call of_open_close(alpha_file_name,title_mode)
 !
#if defined _KERR
 if(l_kerr) then
   call of_open_close(off_file_name,title_mode)
   call of_open_close(moke_file_name,title_mode)
   if (eval_alpha) call of_open_close(beta_file_name,title_mode)
 endif
#endif
 if(.not.l_Haydock.and..not.l_Inversion) call of_open_close(jdos_file_name,title_mode)
 !
 call X_write_q_plus_g(1,BSS_q0,ig=1)
 call X_write_messages_before_headers(iq,BSS_uses_GreenF,BSS_Vnl_included,ordering)
 !
 ! Description of the run and headers for output files:
 !
 call X_write_descriptions(BSS_n_descs,BSS_description)
 !
 if (l_INIT) then 
   call CLOSE_the_files( )
   return
 endif
 !
#if defined _KERR
 !
 ! Diamagnetic term
 !
 call msg('o eps','#','Diamagnetic term',INDENT=0,USE_TABS=.TRUE.)
 !
 n_output_data=2
 if(l_IP) n_output_data=1
 headers(:2)=(/'A [nel]','A0[nel]'/)
 call msg("o eps",'#',headers(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
 !
 A_drude(:)=A_drude(:)/real(nkbz,SP)*spin_occ
 output_data(1)=real(A_drude(2))
 output_data(2)=real(A_drude(1))
 if (l_IP) output_data(1)=output_data(2)
 call msg('o eps','#',output_data(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
 call msg('o eps','#')
 !
#endif
 !
 ! Headers
 !
 call X_setup_headers('EPS')
 if (l_Haydock.or.l_Inversion) call msg("o eps",'#',headers,INDENT=0,USE_TABS=.TRUE.)
 if (l_Diago) call msg("o eps",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
 if (l_IP) call msg("o eps",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
 !
 !
 if (eval_reflectivity) then 
   call X_setup_headers('REFL')
   if (l_Haydock.or.l_Inversion) call msg("o refl",'#',headers(:4),INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago) call msg("o refl",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o refl",'#',headers(:2),INDENT=0,USE_TABS=.TRUE.)
 end if   
 !
 if (eval_eels) then 
   call X_setup_headers('EEL')
   if (l_Haydock.or.l_Inversion) call msg("o eel",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago) call msg("o eel",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o eel",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
 end if   
 !
 if (eval_alpha) then 
   call X_setup_headers('ALPHA')
   if (l_Haydock.or.l_Inversion) call msg("o alpha",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago) call msg("o alpha",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o alpha",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
#if defined _KERR
 if(l_kerr) then
   !
   ! Hanomalous Hall term
   !
   call msg('o off','#','Anomalous Hall effect sH',INDENT=0,USE_TABS=.true.)
   !
   n_output_data=6
   if(l_IP) n_output_data=3
   headers(:6)=(/'sH ','   ','   ','sH0','   ','   '/)
   call msg("o off",'#',headers(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
   headers(:6)=(/'  [fs]^-1  ','[Ohm*cm]^-1','[e^2/hbar] ','  [fs]^-1  ','[Ohm*cm]^-1','[e^2/hbar] '/)
   call msg("o off",'#',headers(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
   !
   B_Hall(:)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
   output_data(1)=real(B_Hall(2))
   output_data(4)=real(B_Hall(1))
   B_Hall(:)=B_Hall(:)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   output_data(2)=real(B_Hall(2))
   output_data(5)=real(B_Hall(1))
   B_Hall(:)=B_Hall(:)*1.E2_SP*HBAR_SI/e_SI**2
   output_data(3)=real(B_Hall(2))
   output_data(6)=real(B_Hall(1))
   if(l_IP) output_data(1:3)=output_data(4:6)
   call msg('o off','#',output_data(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
   call msg('o off','#')
   !
   call X_setup_headers('off')
   if (l_Haydock.or.l_Inversion) call msg("o off",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago) call msg("o off",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o off",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   !
   if (eval_alpha) then 
     call X_setup_headers('beta')
     if (l_Haydock.or.l_Inversion) call msg("o beta",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (l_Diago) call msg("o beta",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
     if (l_IP) call msg("o beta",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   endif
   !
   call X_setup_headers('moke')
   if (l_Haydock.or.l_Inversion) call msg("o moke",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago) call msg("o moke",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o moke",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   !
 endif
#endif
 !
 if (.not.l_Haydock) then
   call X_setup_headers('jdos')
   if (l_Diago) call msg("o jdos",'#',headers(:7),INDENT=0,USE_TABS=.TRUE.)
   if (l_Inversion) call msg("o jdos",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o jdos",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
 call msg("o eps refl eel alpha off beta moke jdos pl_",'#')
 !
 !
 ! ABSORPTION
 !------------
 !
 do iw=1,BSS_n_freqs
   !
   ! Epsilon_ii: 2 (int) 3 (non int) 4 (n-1 Haydock/inv shifted)
   !
   output_data(1:5)=(/real(Epsilon_ii(iw,1))*HA2EV,aimag(Epsilon_ii(iw,2)),real(Epsilon_ii(iw,2)),&
&            aimag(Epsilon_ii(iw,3)),real(Epsilon_ii(iw,3))/)
   !
   ! Haydock n-1 iteration / Inversion with diagonal shift from BSE matrix
   !
   n_output_data=5
   if (l_Haydock.or.(l_Inversion.and..not.K_diago_in_BS_mat)) then
     n_output_data=7
     output_data(6:7)=(/aimag(Epsilon_ii(iw,4)),real(Epsilon_ii(iw,4))/)
   endif
   if (l_IP) n_output_data=3
   if (l_IP) output_data(2:3)=output_data(4:5)
   !
   call msg("o eps",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
   !
 enddo
 !
 ! REFLECTIVITY
 !--------------
 !
 if (eval_reflectivity) then
   !
   ! Reflectance is assumed for a 3D system
   ! r=(sqrt(eps)-1)/(sqrt(eps)+1)
   ! R=|r|^2
   !
   refl(:,1)=(sqrt(Epsilon_ii(:,2))-1._SP)/(sqrt(Epsilon_ii(:,2))+1._SP)
   refl(:,2)=(sqrt(Epsilon_ii(:,3))-1._SP)/(sqrt(Epsilon_ii(:,3))+1._SP)
   if (l_Haydock) refl(:,3)=(sqrt(Epsilon_ii(:,4))-1._SP)/(sqrt(Epsilon_ii(:,4))+1._SP)
   !
   do iw=1,BSS_n_freqs
     !
     output_data(1:3)=(/real(Epsilon_ii(iw,1))*HA2EV,real(refl(iw,1)*conjg(refl(iw,1))),real(refl(iw,2)*conjg(refl(iw,2)))/)     
     !
     n_output_data=3
     if (l_Haydock.or.l_Inversion) n_output_data=4
     if (l_Haydock.or.l_Inversion) output_data(4)=real(refl(iw,3)*conjg(refl(iw,3)))
     if (l_IP) n_output_data=2
     if (l_IP) output_data(2)=output_data(3)
     !
     call msg("o refl",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
 !
 ! EELS & POLARIZABILITY
 !-----------------------
 !
 if (eval_eels) then
   !
   eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,2),eels(:,1)) 
   eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,3),eels(:,2)) 
   if (l_Haydock.or.l_Inversion) eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,4),eels(:,3)) 
   !
   alpha(:,1)=(1._SP+eels(:,1))/4._SP/pi*eps_2_alpha
   alpha(:,2)=(1._SP+eels(:,2))/4._SP/pi*eps_2_alpha
   if (l_Haydock.or.l_Inversion) alpha(:,3)=(1._SP+eels(:,3))/4._SP/pi*eps_2_alpha
   !
   do iw=1,BSS_n_freqs
     !
     ! EELS
     !
     output_data(1:5)=(/real(Epsilon_ii(iw,1))*HA2EV,aimag(eels(iw,1)),real(eels(iw,1)),&
&              aimag(eels(iw,2)),real(eels(iw,2))/)
     n_output_data=5
     if (l_Haydock.or.l_Inversion) n_output_data=7
     if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(eels(iw,3)),real(eels(iw,3))/)
     if (l_IP) n_output_data=3
     if (l_IP) output_data(2:3)=output_data(4:5)
     call msg("o eel",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
     ! ALPHA
     !
     output_data(1:5)=(/real(Epsilon_ii(iw,1))*HA2EV,aimag(alpha(iw,1)),real(alpha(iw,1)),&
&              aimag(alpha(iw,2)),real(alpha(iw,2))/)
     n_output_data=5
     if (l_Haydock.or.l_Inversion) n_output_data=7
     if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(alpha(iw,3)),real(alpha(iw,3))/)
     if (l_IP) n_output_data=3
     if (l_IP) output_data(2:3)=output_data(4:5)
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
     beta(:,1)=Epsilon_ij(:,2)/4._SP/pi*eps_2_alpha
     beta(:,2)=Epsilon_ij(:,3)/4._SP/pi*eps_2_alpha
   endif
   !
   ! eps_xy
   !
   do iw=1,BSS_n_freqs 
     output_data(1:5)=(/real(Epsilon_ij(iw,1))*HA2EV,aimag(Epsilon_ij(iw,2)),real(Epsilon_ij(iw,2)),&
&                  aimag(Epsilon_ij(iw,3)),real(Epsilon_ij(iw,3))/)
     n_output_data=5
     if (l_Haydock.or.l_Inversion) n_output_data=7
     if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(Epsilon_ij(iw,4)),real(Epsilon_ij(iw,4))/)
     if (l_IP) n_output_data=3
     if (l_IP) output_data(2:3)=output_data(4:5)
     call msg('o off','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     !
     if(eval_alpha) then
       !
       ! alpha_xy
       !
       output_data(1:5)=(/real(Epsilon_ij(iw,1))*HA2EV,aimag(beta(iw,1)),real(beta(iw,1)),&
&                aimag(beta(iw,3)),real(beta(iw,3))/)
       n_output_data=5
       if (l_Haydock.or.l_Inversion) n_output_data=7
       if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(beta(iw,3)),real(beta(iw,3))/)
       if (l_IP) n_output_data=3
       if (l_IP) output_data(2:3)=output_data(4:5)
       call msg("o beta",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
       !
     endif
     !
   enddo
   !
   ! Kerr parameters
   !
   do iw=1,BSS_n_freqs 
     output_data(1)= real(Epsilon_ii(iw,1))*HA2EV
     output_data(2)=-aimag(Epsilon_ij(iw,2)/( (Epsilon_ii(iw,2)-1.0_SP)*sqrt(Epsilon_ii(iw,2)) ) /DEG2RAD)
     output_data(3)=-real(Epsilon_ij(iw,2)/( (Epsilon_ii(iw,2)-1.0_SP)*sqrt(Epsilon_ii(iw,2)) ) /DEG2RAD)
     output_data(4)=-aimag(Epsilon_ij(iw,3)/( (Epsilon_ii(iw,3)-1.0_SP)*sqrt(Epsilon_ii(iw,3)) ) /DEG2RAD)
     output_data(5)=-real(Epsilon_ij(iw,3)/( (Epsilon_ii(iw,3)-1.0_SP)*sqrt(Epsilon_ii(iw,3)) ) /DEG2RAD)
     !
     if( (real(Epsilon_ii(iw,2))==1._SP .or. real(Epsilon_ii(iw,2))==0._SP ) &
&         .and. aimag(Epsilon_ii(iw,2))==0._SP ) then
       output_data(2)=0._SP
       output_data(3)=0._SP
     endif
     if( (real(Epsilon_ii(iw,3))==1._SP .or. real(Epsilon_ii(iw,3))==0._SP ) &
&         .and. aimag(Epsilon_ii(iw,3))==0._SP ) then
       output_data(4)=0._SP
       output_data(5)=0._SP
     endif         
     !
     n_output_data=5
     if (l_Haydock.or.l_Inversion) then
       n_output_data=7
       output_data(6)=-aimag(Epsilon_ij(iw,4)/( (Epsilon_ii(iw,4)-1.0_SP)*sqrt(Epsilon_ii(iw,4)) ) /DEG2RAD)
       output_data(7)=-real(Epsilon_ij(iw,4)/( (Epsilon_ii(iw,4)-1.0_SP)*sqrt(Epsilon_ii(iw,4)) ) /DEG2RAD)
     endif
     if (l_IP) n_output_data=3
     if (l_IP) output_data(2:3)=output_data(4:5)
     call msg('o moke','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
   enddo
   !
 endif
 !
#endif
 !
 ! JOINT DOS
 !------------
 !
 if (.not.l_Haydock.and..not.l_Inversion) then 
   do iw=1,BSS_n_freqs
     !
     ! Joint DOS: 2 (int) 3 (non int)
     !
     output_data(1:5)=(/real(Joint_DOS(iw,1))*HA2EV,aimag(Joint_DOS(iw,2)),real(Joint_DOS(iw,2)),&
&                       aimag(Joint_DOS(iw,3)),real(Joint_DOS(iw,3))/)
     !
     ! Haydock n-1 iteration
     !
     n_output_data=5
     if (l_Inversion) n_output_data=7
     if (l_Inversion) output_data(6:7)=(/aimag(Joint_DOS(iw,4)),real(Joint_DOS(iw,4))/)
     if (l_IP) n_output_data=3
     if (l_IP) output_data(2:3)=output_data(4:5)
     !
     call msg("o jdos",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
 !
 call CLOSE_the_files( )
 !
 contains 
   !
   subroutine CLOSE_the_files( )
     !
     call of_open_close(eps_file_name)
     call of_open_close(refl_file_name)
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
     if(.not.l_Haydock.and..not.l_Inversion) call of_open_close(jdos_file_name)
     !
   end subroutine
   !
 end subroutine
