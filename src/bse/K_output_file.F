!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine K_output_file(iq,mode)
 !
 use pars,      ONLY:pi,SP
 use drivers,   ONLY:l_col_cut
 use BS_solvers,ONLY:BSS_desc,BSS_uses_GreenF,BSS_Vnl_included,&
&                    BSS_n_freqs,K_diago_in_BS_mat  
 use BS,        ONLY:l_BS_abs,BSE_mode,BS_K_is_ALDA,BSqpts,BS_H_dim,l_BS_esort,&
 &                   l_BS_jdos,BS_K_coupling
 use X_m,       ONLY:Epsilon_ii,Joint_DOS,BS_E_sorted,eval_alpha,eps_2_alpha,alpha_dim,OPTICS_eels
 use com,       ONLY:msg,of_open_close
 use parser_m,  ONLY:parser
 use X_output,  ONLY:headers,X_setup_headers,X_setup_file_names,eps_file_name,jdos_file_name,&
&                    eel_file_name,alpha_file_name,refl_file_name,esort_file_name,e_ip_file_name,&
&                    X_write_messages_before_headers,X_write_q_plus_g
 use R_lattice, ONLY:bare_qpg,q_norm
 use interfaces,ONLY:DESC_write
 use pars,      ONLY:cI
 use electrons, ONLY:spin_occ,n_spinor
 use R_lattice, ONLY:nkbz
 use units,     ONLY:DEG2RAD,HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI,e_SI,HBAR_SI,HA2EV
 use BS,        ONLY:l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_photolum,BS_field_direction,BS_dip_size
 use X_m,       ONLY:Epsilon_ij,X_magnons,X_dichroism
 use X_output,  ONLY:moke_file_name,off_file_name,beta_file_name,dichr_file_name,&
 &                   magp_file_name,magm_file_name
 use BS_solvers,ONLY:B_Hall,A_drude
 use X_output,  ONLY:pl_file_name
 use PHOTOLUM,  ONLY:PL
 !
 implicit none
 integer      :: iq
 character(*) :: mode
 !
 ! Work Space
 !
 character(2)      :: ordering,title_mode
 character(10)     :: name_
 integer           :: iw,i1,i_IP_BSE,n_IP_BSE,E_size,n_output_data,n_headers
 real(SP)          :: output_data(9),w(BSS_n_freqs)
 complex(SP)       :: eps(BSS_n_freqs,3),eels(BSS_n_freqs,3),alpha(BSS_n_freqs,3),refl(BSS_n_freqs,3),alpha_factor,&
&                     beta(BSS_n_freqs,3)
 logical           :: l_kerr_out,l_dich_out,l_phot_out,l_magn_out,eval_moke,&
&                     eval_alpha_loc,eval_beta,eval_eels,eval_reflectivity,l_Haydock,l_Diago,l_Inversion,l_Slepc,&
&                     l_IP,l_INIT,l_FINALIZE,l_print_col4,l_jdos_out,l_esrt_out,l_abs_out
 !
 l_INIT      = index(mode,"open")/=0
 l_FINALIZE  = index(mode,"final")/=0
 !
 l_Haydock   = index(mode,"Haydock")/=0
 l_Diago     = index(mode,"Diago")/=0
 l_Slepc     = index(mode,"Slepc")/=0
 l_Inversion = index(mode,"Inversion")/=0
 l_IP        = index(mode,"IP")/=0
 !
 ! File Names 
 !
 if (l_Haydock)   name_="haydock"
 if (l_Diago)     name_="diago"
 if (l_Slepc)     name_="slepc"
 if (l_Inversion) name_="inv"
 if (l_IP)        name_="IP"
 !
 l_esrt_out =l_BS_esort    .and.(.not.(l_Inversion.or.l_Haydock))
 l_abs_out  =l_BS_abs      .and.((.not.l_Haydock).or.index(mode,"abs")>0 )
 l_jdos_out =l_BS_jdos     .and.((.not.l_Haydock).or.index(mode,"jdos")>0).and.(.not.l_Inversion)
 l_magn_out =l_BS_magnons  .and.((.not.l_Haydock).or.index(mode,"magn")>0).and.(.not.l_Inversion)
 l_kerr_out =l_BS_kerr     .and.((.not.l_Haydock).or.index(mode,"kerr")>0)
 l_dich_out =l_BS_dichroism.and.((.not.l_Haydock).or.index(mode,"dich")>0).and.(.not.l_Inversion)
 l_phot_out =l_BS_photolum .and.l_Inversion
 !
 l_print_col4=l_Haydock.or.(l_Inversion.and..not.K_diago_in_BS_mat)
 !
 if (l_INIT) then
   if (l_IP) then
     call X_setup_file_names(iq,trim(name_),' ',' ')
   else
     call X_setup_file_names(iq,trim(name_),' ','bse')
     if (BS_K_is_ALDA) call X_setup_file_names(iq,trim(name_),'alda','bse')
   endif
 endif
 !
 if (     trim(BSE_mode)=='resonant' ) ordering='r'
 if (.not.trim(BSE_mode)=='resonant' ) ordering='R'
 !
 ! EELS / ALPHA / REFLECTIVITY logicals
 !
 if (l_abs_out) then
   call parser('Reflectivity',eval_reflectivity)
   eval_eels = ((.not.l_Haydock) .or. l_FINALIZE.or.l_INIT) .and. (.not.l_Slepc) .and. (.not.trim(alpha_dim)=="volume")
   if (eval_eels) eval_eels= OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.TRUE.,ordering)
   eval_alpha_loc=eval_alpha .and. ((l_col_cut) .or. (eval_eels) .or. trim(alpha_dim)=="volume" )
   eval_reflectivity=eval_reflectivity .and. .not. eval_alpha
 else
   eval_eels=.false.
   eval_alpha_loc=.false.
   eval_reflectivity=.false.
 endif
 !
 eval_beta =      eval_alpha .and. l_kerr_out
 eval_moke = .not.eval_alpha .and. l_kerr_out
 if ( l_kerr_out.and.l_Haydock ) then
   eval_beta = eval_alpha .and. (l_col_cut .or. trim(alpha_dim)=="volume" )
 endif
 !
 n_headers=5
 if (l_print_col4) n_headers=7
 if (l_IP) n_headers=3
 !
 title_mode='Ot'
 if (l_INIT) title_mode='ot'
 !
 if (l_abs_out)         call of_open_close(eps_file_name,title_mode)
 if (eval_reflectivity) call of_open_close(refl_file_name,title_mode)
 if (eval_eels)         call of_open_close(eel_file_name,title_mode)
 if (eval_alpha_loc)    call of_open_close(alpha_file_name,title_mode)
 !
 if(l_kerr_out) then
   call of_open_close(off_file_name,title_mode)
   if (eval_moke) call of_open_close(moke_file_name,title_mode)
   if (eval_beta) call of_open_close(beta_file_name,title_mode)
 endif
 if(l_dich_out) call of_open_close(dichr_file_name,title_mode)
 if(l_phot_out) call of_open_close(pl_file_name,title_mode)
 if(l_magn_out) then
   call of_open_close(magp_file_name,title_mode)
   if(n_spinor==2) call of_open_close(magm_file_name,title_mode)
 endif
 if(l_jdos_out) call of_open_close(jdos_file_name,title_mode)
 if(l_esrt_out) call of_open_close(e_ip_file_name,title_mode)
 if(l_esrt_out.and..not.l_IP) call of_open_close(esort_file_name,title_mode)
 !
 if (BS_dip_size==1) call X_write_q_plus_g(iq,BSqpts(:,iq),ig=1)
 if (BS_dip_size==2) call X_write_q_plus_g(iq,BSqpts(:,iq),ig=1,Qprime=BS_field_direction(:,2))
 call X_write_messages_before_headers(iq,BSS_uses_GreenF,BSS_Vnl_included,ordering)
 !
 call msg('o eps_q refl eel_q fxc alpha off beta moke jdos Esort','#')
 !
 ! Description of the run and headers for output files:
 !
 call DESC_write('o eps_q refl eel_q fxc alpha off beta moke jdos Esort','#',BSS_desc)
 !
 if (l_INIT) then 
   call CLOSE_the_files( )
   return
 endif
 !
 if(eval_alpha_loc) then
   ! Alpha units
   call msg('o alpha','#',' Alpha is in a.u. of '//trim(alpha_dim)//' ',INDENT=0)
   call msg('o alpha','#')
 endif
 !
 ! Diamagnetic term
 !
 if (l_abs_out) then
   call msg('o eps','#',' Diamagnetic term',INDENT=0)
   call msg('o eps','#')
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
 endif
 !
 if(l_kerr_out .and. eval_beta) then
   ! Alpha units
   call msg('o beta','#',' Beta is in a.u. of '//trim(alpha_dim)//' ',INDENT=0)
   call msg('o beta','#')
 endif
 !
 if(l_kerr_out) then
   !
   ! Hanomalous Hall term
   !
   call msg('o off','# ','Anomalous Hall effect (B_Hall):',INDENT=0)
   call msg('o off','#')
   !
   ! [fs]^-1 -> 1: BS 2: IP
   B_Hall(:)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
   output_data(1)=real(B_Hall(2))
   output_data(2)=real(B_Hall(1))
   !
   ! [Ohm*cm]^-1 -> 3: BS 4: IP
   B_Hall(:)=B_Hall(:)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   output_data(3)=real(B_Hall(2))
   output_data(4)=real(B_Hall(1))
   !
   ! [e^2/hbar] -> 5: BS 6: IP
   B_Hall(:)=B_Hall(:)*1.E2_SP*HBAR_SI/e_SI**2
   output_data(5)=real(B_Hall(2))
   output_data(6)=real(B_Hall(1))
   !
   if (l_IP) then
     call msg('o off','# B_Hall (IP)',output_data(2),INDENT=0,after_msg="[fs]^-1")
     call msg('o off','#            ',output_data(4),INDENT=0,after_msg="[Ohm*cm]^-1")
     call msg('o off','#            ',output_data(6),INDENT=0,after_msg="[e^2/hbar]")
   else
     call msg('o off','# B_Hall (BS,IP)',output_data(1:2),INDENT=0,after_msg="[fs]^-1")
     call msg('o off','#               ',output_data(3:4),INDENT=0,after_msg="[Ohm*cm]^-1")
     call msg('o off','#               ',output_data(5:6),INDENT=0,after_msg="[e^2/hbar]")
   endif
   !
   call msg('o off','#')
   !
 endif
 !
 ! Headers
 !
 if(l_abs_out) then
   call X_setup_headers('EPS')
   call msg("o eps",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
 if (eval_reflectivity) then 
   call X_setup_headers('REFL')
   call msg("o refl",'#',headers(:(n_headers-1)/2+1),INDENT=0,USE_TABS=.TRUE.)
 end if   
 !
 if (eval_eels) then 
   call X_setup_headers('EEL')
   call msg("o eel",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
 end if   
 !
 if (eval_alpha_loc) then 
   call X_setup_headers('ALPHA')
   call msg("o alpha",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
 if(l_kerr_out) then
   !
   call X_setup_headers('off')
   call msg("o off",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
   !
   if (eval_beta) then 
     call X_setup_headers('beta')
   call msg("o beta",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
   endif
   !
   if (eval_moke)  then
     call X_setup_headers('moke')
     call msg("o moke",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
   endif
   !
 endif
 !
 if (l_phot_out) then
   call X_setup_headers('PL')
   call msg("o pl_",'#',headers(:(n_headers-1)/2+1),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
 if(l_magn_out) then
   !
   call X_setup_headers('Spm ')
   if (l_Haydock.or.l_Inversion) call msg("o Spm",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago.or.l_Slepc) call msg("o Spm",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o Spm",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   !
   if(n_spinor==2) then
     call X_setup_headers('Smp')
     if (l_Haydock.or.l_Inversion) call msg("o Smp",'#',headers,INDENT=0,USE_TABS=.TRUE.)
     if (l_Diago.or.l_Slepc) call msg("o Smp",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
     if (l_IP) call msg("o Smp",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   endif
   !
 endif
 !
 if(l_dich_out) then
   !
   call X_setup_headers('CD ')
   if (l_Haydock.or.l_Inversion) call msg("o CD",'#',headers,INDENT=0,USE_TABS=.TRUE.)
   if (l_Diago.or.l_Slepc) call msg("o CD",'#',headers(:5),INDENT=0,USE_TABS=.TRUE.)
   if (l_IP) call msg("o CD",'#',headers(:3),INDENT=0,USE_TABS=.TRUE.)
   !
 endif
 !
 if (.not.l_Haydock) then
   call X_setup_headers('jdos')
   call msg("o jdos",'#',headers(:n_headers),INDENT=0,USE_TABS=.TRUE.)
 endif
 !
 call msg("o eps refl eel alpha off beta moke jdos pl_ Smp Spm Esort",'#')
 !
 ! ABSORPTION
 !------------
 !
 if (l_abs_out) then
   !
   do iw=1,BSS_n_freqs
     !
     ! Epsilon_ii: 2 (int) 3 (non int) 4 (n-1 Haydock/inv shifted)
     !
     w(:)    =real(Epsilon_ii(:,1))*HA2EV
     eps(:,1)=Epsilon_ii(:,2)+1._SP
     eps(:,2)=Epsilon_ii(:,3)+1._SP
     ! Haydock n-1 iteration / Inversion with diagonal shift from BSE matrix
     if (l_print_col4) eps(:,3)=Epsilon_ii(:,4)+1._SP
     !
     output_data(1:5)=(/w(iw),aimag(eps(iw,1)),real(eps(iw,1)),aimag(eps(iw,2)),real(eps(iw,2))/)
     n_output_data=5
     if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(eps(iw,3)),real(eps(iw,3))/) ; endif
     if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                     ; endif
     !
     call msg("o eps",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
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
   refl=(sqrt(eps)-1._SP)/(sqrt(eps)+1._SP)
   !
   do iw=1,BSS_n_freqs
     !
     output_data(1:3)=(/w(iw),real(refl(iw,1)*conjg(refl(iw,1))),real(refl(iw,2)*conjg(refl(iw,2)))/)     
     n_output_data=3
     if (l_print_col4) then ; n_output_data=4 ; output_data(4)=real(refl(iw,3)*conjg(refl(iw,3))) ; endif
     if (l_IP)         then ; n_output_data=2 ; output_data(2)=output_data(3)                     ; endif
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
   eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,eps(:,1),eels(:,1)) 
   eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,eps(:,2),eels(:,2)) 
   if (l_print_col4) eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,eps(:,3),eels(:,3)) 
   !
   do iw=1,BSS_n_freqs
     !
     ! EELS
     !
     output_data(1:5)=(/w(iw),aimag(eels(iw,1)),real(eels(iw,1)),aimag(eels(iw,2)),real(eels(iw,2))/)
     n_output_data=5
     if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(eels(iw,3)),real(eels(iw,3))/) ; endif
     if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                       ; endif
     call msg("o eel",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
 !
 ! POLARIZABILITY
 !----------------
 !
 if (eval_alpha_loc) then
   !
   if(l_col_cut.or.trim(alpha_dim)=="volume") then
     alpha_factor=eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
     alpha(:,1)=Epsilon_ii(:,2)*alpha_factor
     alpha(:,2)=Epsilon_ii(:,3)*alpha_factor
     if (l_print_col4) alpha(:,3)=Epsilon_ii(:,4)*alpha_factor
   else
     alpha_factor=eps_2_alpha/4._SP/pi
     alpha=(1._SP+eels)*alpha_factor
   endif
   !
   do iw=1,BSS_n_freqs
     !
     output_data(1:5)=(/w(iw),aimag(alpha(iw,1)),real(alpha(iw,1)),aimag(alpha(iw,2)),real(alpha(iw,2))/)
     n_output_data=5
     if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(alpha(iw,3)),real(alpha(iw,3))/) ; endif
     if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                         ; endif
     call msg("o alpha",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
 !
 ! PHOTOLUMINESCENCE
 !--------------------
 if (l_phot_out) then
   !
   do iw=1,BSS_n_freqs
     !
     ! PL: 2 (int) 3 (non int) 
     !
     if (     l_IP) n_output_data=2 ; output_data(1:2)=(/real(PL(iw,1))*HA2EV,PL(iw,3)/)
     if (.not.l_IP) n_output_data=3 ; output_data(1:3)=(/real(PL(iw,1))*HA2EV,PL(iw,2),PL(iw,3)/)
     call msg("o pl_",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
 !
 ! KERR 
 !------
 if(l_kerr_out) then
   !
   ! eps_xy
   !
   w(:)    =real(Epsilon_ii(:,1))*HA2EV
   !
   do iw=1,BSS_n_freqs 
     output_data(1:5)=(/w(iw),aimag(Epsilon_ij(iw,2)),real(Epsilon_ij(iw,2)),&
&                  aimag(Epsilon_ij(iw,3)),real(Epsilon_ij(iw,3))/)
     n_output_data=5
     if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(Epsilon_ij(iw,4)),real(Epsilon_ij(iw,4))/) ; endif
     if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                                   ; endif
     call msg('o off','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   if(eval_beta) then
     !
     ! beta=alpha_xy
     !
     if(l_col_cut.or.trim(alpha_dim)=="volume") then
       alpha_factor=eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
       beta(:,1)=Epsilon_ij(:,2)*alpha_factor
       beta(:,2)=Epsilon_ij(:,3)*alpha_factor
       if (l_print_col4) beta(:,3)=Epsilon_ij(:,4)*alpha_factor
     else
       alpha_factor=eps_2_alpha/4._SP/pi
       beta(:,1)=0._SP
       beta(:,2)=0._SP
       if (l_print_col4) beta(:,3)=0._SP
     endif
     !
     do iw=1,BSS_n_freqs 
       !
       output_data(1:5)=(/w(iw),aimag(beta(iw,1)),real(beta(iw,1)),&
&                aimag(beta(iw,2)),real(beta(iw,2))/)
       n_output_data=5
       if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(beta(iw,3)),real(beta(iw,3))/) ; endif
       if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                       ; endif
       call msg("o beta",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
       !
     enddo
     !
   endif
   !
   if(eval_moke) then
     !
     ! Kerr parameters
     !
     do iw=1,BSS_n_freqs 
       !
       output_data(1)= w(iw)
       output_data(2)=-aimag(Epsilon_ij(iw,2)/( Epsilon_ii(iw,2)*sqrt(Epsilon_ii(iw,2)+1._SP) ) /DEG2RAD)
       output_data(3)= -real(Epsilon_ij(iw,2)/( Epsilon_ii(iw,2)*sqrt(Epsilon_ii(iw,2)+1._SP) ) /DEG2RAD)
       output_data(4)=-aimag(Epsilon_ij(iw,3)/( Epsilon_ii(iw,3)*sqrt(Epsilon_ii(iw,3)+1._SP) ) /DEG2RAD)
       output_data(5)= -real(Epsilon_ij(iw,3)/( Epsilon_ii(iw,3)*sqrt(Epsilon_ii(iw,3)+1._SP) ) /DEG2RAD)
       !
       if( (real(Epsilon_ii(iw,2))==0._SP .or. real(Epsilon_ii(iw,2))==-1._SP ) &
  &         .and. aimag(Epsilon_ii(iw,2))==0._SP ) then
         output_data(2)=0._SP
         output_data(3)=0._SP
       endif
       if( (real(Epsilon_ii(iw,3))==0._SP .or. real(Epsilon_ii(iw,3))==-1._SP ) &
  &         .and. aimag(Epsilon_ii(iw,3))==0._SP ) then
         output_data(4)=0._SP
         output_data(5)=0._SP
       endif
       !
       n_output_data=5
       if (l_print_col4) then
         n_output_data=7
         output_data(6)=-aimag(Epsilon_ij(iw,4)/( Epsilon_ii(iw,4)*sqrt(Epsilon_ii(iw,4)+1._SP) ) /DEG2RAD)
         output_data(7)= -real(Epsilon_ij(iw,4)/( Epsilon_ii(iw,4)*sqrt(Epsilon_ii(iw,4)+1._SP) ) /DEG2RAD)
       endif
       if (l_IP) then  
         n_output_data=3  
         output_data(2:3)=output_data(4:5)  
       endif
       call msg('o moke','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     enddo
     !
   endif
   !
 endif
 !
 ! MAGNONS
 !------
 if(l_magn_out) then
   !
   ! magnons Spm
   !
   n_output_data=5
   if (l_Haydock.or.l_Inversion) n_output_data=7
   if (l_IP) n_output_data=3
   !
   do iw=1,BSS_n_freqs 
     output_data(1:5)=(/real(X_magnons(iw,1,1))*HA2EV,aimag(X_magnons(iw,1,2)),real(X_magnons(iw,1,2)),&
&                  aimag(X_magnons(iw,1,3)),real(X_magnons(iw,1,3))/)
     if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(X_magnons(iw,1,4)),real(X_magnons(iw,1,4))/)
     if (l_IP) output_data(2:3)=output_data(4:5)
     call msg('o Spm','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   if (n_spinor==2) then
     !
     ! magnons Smp
     !
     do iw=1,BSS_n_freqs 
       output_data(1:5)=(/real(X_magnons(iw,2,1))*HA2EV,aimag(X_magnons(iw,2,2)),real(X_magnons(iw,2,2)),&
&                    aimag(X_magnons(iw,2,3)),real(X_magnons(iw,2,3))/)
       if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(X_magnons(iw,2,4)),real(X_magnons(iw,2,4))/)
       if (l_IP) output_data(2:3)=output_data(4:5)
       call msg('o Smp','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     enddo
     !
   endif
   !
 endif
 !
 !  DICHROISM
 !------
 if(l_dich_out) then
   !
   n_output_data=5
   if (l_Haydock.or.l_Inversion) n_output_data=7
   if (l_IP) n_output_data=3
   !
   do iw=1,BSS_n_freqs 
     output_data(1:5)=(/real(X_dichroism(iw,1))*HA2EV,aimag(X_dichroism(iw,2)),real(X_dichroism(iw,2)),&
&                  aimag(X_dichroism(iw,3)),real(X_dichroism(iw,3))/)
     if (l_Haydock.or.l_Inversion) output_data(6:7)=(/aimag(X_dichroism(iw,4)),real(X_dichroism(iw,4))/)
     if (l_IP) output_data(2:3)=output_data(4:5)
     call msg('o CD','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
 endif
 !
 ! JOINT DOS
 !------------
 !
 if (l_jdos_out) then
   !
   n_output_data=5
   if (l_IP) n_output_data=3
   !
   do iw=1,BSS_n_freqs
     !
     ! Joint DOS: 2 (int) 3 (non int)
     !
     output_data(1:5)=(/real(Joint_DOS(iw,1))*HA2EV,aimag(Joint_DOS(iw,2)), &
     &                  real(Joint_DOS(iw,2)),aimag(Joint_DOS(iw,3)),real(Joint_DOS(iw,3))/)
     if (l_IP) output_data(2:3)=output_data(4:5)
     !
     call msg("o jdos",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
 endif
 !
 if (l_esrt_out) then
   !
   n_output_data=2
   headers(1)="E [eV]"
   headers(2)="n_deg"
   if(l_abs_out) then
     headers(n_output_data+1:n_output_data+1)="abs"
     n_output_data=n_output_data+1
   endif
   if (l_kerr_out) then
     headers(n_output_data+1:n_output_data+1)="kerr"
     n_output_data=n_output_data+1
   endif
   if (l_dich_out) then
     headers(n_output_data+1:n_output_data+1)="dich"
     n_output_data=n_output_data+1
   endif
   if (l_magn_out) then
     headers(n_output_data+1:n_output_data+2)=(/"S+-","S-+"/)
     n_output_data=n_output_data+2
   endif
   !
   n_IP_BSE=2
   if (l_IP) n_IP_BSE=1
   !
   E_size=1
   !
   do i_IP_BSE=1,n_IP_BSE
     ! BS_E_sorted(:,:,2) (BSE) ; BS_E_sorted(:,:,1) (IP)
     if (BS_K_coupling.and.i_IP_BSE==2) then
       do i1=n_output_data+1,3,-1
         headers(i1)=headers(i1-1)
       enddo
       E_size=2
       headers(1)="Re(E) [eV]"
       headers(2)="Im(E) [eV]"
     endif
     if(i_IP_BSE==1) name_="E_IP"
     if(i_IP_BSE==2) name_="Esort"
     call msg("o "//trim(name_),'#')
     call msg("o "//trim(name_),'#',headers(:n_output_data+E_size-1),INDENT=0,USE_TABS=.TRUE.)
     call msg("o "//trim(name_),'#')
     do i1=1,min(BS_H_dim,1000)
       if(E_size==1) output_data(1)=real(BS_E_sorted(i1,1,i_IP_BSE),SP)*HA2EV
       if(E_size==2) output_data(1:2)=(/real(BS_E_sorted(i1,1,i_IP_BSE),SP),aimag(BS_E_sorted(i1,1,i_IP_BSE))/)*HA2EV
       if (output_data(1)==0._SP) cycle
       output_data(E_size+1:n_output_data+E_size-1)=real(BS_E_sorted(i1,2:n_output_data,i_IP_BSE),SP)
       call msg("o "//trim(name_) ,'',output_data(:n_output_data+E_size-1),INDENT=-2,USE_TABS=.TRUE.)
     enddo
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
     if(l_abs_out) then
       call of_open_close(eps_file_name)
       call of_open_close(refl_file_name)
       call of_open_close(eel_file_name)
       call of_open_close(alpha_file_name)
     endif
     !
     if(l_phot_out) then
       call of_open_close(pl_file_name)
     endif
     !
     if(l_kerr_out) then
       call of_open_close(off_file_name)
       call of_open_close(beta_file_name)
       call of_open_close(moke_file_name)
     endif
     !
     if(l_magn_out) then
       call of_open_close(magp_file_name)
       call of_open_close(magm_file_name)
     endif
     !
     if(l_dich_out) then
       call of_open_close(dichr_file_name)
     endif
     !
     if(l_jdos_out) call of_open_close(jdos_file_name)
     if(l_esrt_out) call of_open_close(e_ip_file_name)
     if(l_esrt_out.and..not.l_IP) call of_open_close(esort_file_name)
     !
   end subroutine
   !
 end subroutine
