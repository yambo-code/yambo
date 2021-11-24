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
subroutine K_OUTPUT(iq,W,WHAT,OBS)
 !
 use pars,          ONLY:pi,SP
 use drivers,       ONLY:l_col_cut
 use frequency,     ONLY:w_samp
 use BS_solvers,    ONLY:BSS_desc,BSS_uses_GreenF,BSS_Vnl_included,BSS_n_freqs,K_diago_in_BS_mat  
 use BS,            ONLY:BSE_mode,BS_K_is_ALDA,BSqpts,BS_field_direction,BS_dip_size
 use stderr,        ONLY:STRING_match
 use X_m,           ONLY:Epsilon_ii,eps_2_alpha,alpha_dim,OPTICS_eels,X_OUTPUT_driver,&
&                        X_do_obs,X_OUTPUT_messages
 use R_lattice,     ONLY:bare_qpg,q_norm
 !
 implicit none
 integer      :: iq
 type(w_samp) :: W
 character(*) :: WHAT,OBS
 !
 ! Work Space
 !
 character(2)      :: ordering
 character(10)     :: solver
 integer           :: iw
 complex(SP)       :: spectra(BSS_n_freqs,3),alpha_factor
 logical           :: l_last_col,eval_eels

! if ( l_kerr_out.and.l_Haydock ) then
!   eval_beta = eval_alpha .and. (l_col_cut .or. trim(alpha_dim)=="volume" )
! endif
!
! if (l_abs_out) then
!   eval_eels = ((.not.l_Haydock) .or. l_FINALIZE.or.l_INIT) .and. (.not.l_Slepc) .and. (.not.trim(alpha_dim)=="volume")
!   eval_alpha_loc=eval_alpha .and. ((l_col_cut) .or. (eval_eels) .or. trim(alpha_dim)=="volume" )
! endif
! !
 if (STRING_match(WHAT,"open")) then
   !
   if (STRING_match(WHAT,"Haydock"))   solver="haydock"
   if (STRING_match(WHAT,"Diago"))     solver="diago"
   if (STRING_match(WHAT,"Slepc"))     solver="slepc"
   if (STRING_match(WHAT,"Inversion")) solver="inv"
   if (STRING_match(WHAT,"IP"))        solver="IP"
   !
   if (     STRING_match(WHAT,"IP")                      ) call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq,keys='IP')
   if (.not.STRING_match(WHAT,"IP").and..not.BS_K_is_ALDA) call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq,keys=trim(solver)//' bse')
   if (                   BS_K_is_ALDA) call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq,keys=trim(solver)//' alda bse')
   !
   call X_OUTPUT_driver("open "//OBS)
 endif
 !
 if (STRING_match(WHAT,"open").or.STRING_match(WHAT,"override")) then
   if (BS_dip_size==1) call X_OUTPUT_messages(iq,Q_plus_G=BSqpts(:,iq),ig=1)
   if (BS_dip_size==2) call X_OUTPUT_messages(iq,Q_plus_G=BSqpts(:,iq),ig=1,Qprime=BS_field_direction(:,2))
   !
   if (     trim(BSE_mode)=='resonant' ) ordering='r'
   if (.not.trim(BSE_mode)=='resonant' ) ordering='R'
   call X_OUTPUT_messages(iq,GF=BSS_uses_GreenF,Vnl=BSS_Vnl_included,ordering=ordering)
   !
   call X_OUTPUT_driver("DESC "//OBS,DESC=BSS_desc)
   !
 endif
 !
 if (STRING_match(WHAT,"append"))    call X_OUTPUT_driver("append "//OBS)
 if (STRING_match(WHAT,"override"))  call X_OUTPUT_driver("override "//OBS)
 !
 l_last_col=STRING_match(WHAT,"Haydock").or.( STRING_match(WHAT,"Inversion").and..not.K_diago_in_BS_mat)
 !
 if (STRING_match(WHAT,"close").and..not.STRING_match(WHAT,"dump")) then
   call X_OUTPUT_driver("close "//OBS)
   return
 endif
 !
 if (.not.STRING_match(WHAT,"dump")) return 
 !
 ! ABSORPTION
 !------------
 if (X_do_obs("eps")) call ELEMENTAL_dump("eps",Epsilon_ii(:,2:4)+1._SP)
 !
 ! EELS 
 !-------
 if (X_do_obs("eel")) then
   eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,2),spectra(:,1)) 
   eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,3),spectra(:,2)) 
   if (l_last_col) eval_eels=OPTICS_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,4),spectra(:,3)) 
   !call ELEMENTAL_dump("eel",spectra)
 endif
 !
 ! POLARIZABILITY
 !----------------
 if (X_do_obs("alpha")) then
   if(l_col_cut.or.trim(alpha_dim)=="volume") then
     alpha_factor=eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
     spectra(:,1:2)=Epsilon_ii(:,2:3)*alpha_factor
     if (l_last_col) spectra(:,3)=Epsilon_ii(:,4)*alpha_factor
   else
     alpha_factor=eps_2_alpha/4._SP/pi
     spectra=(1._SP+spectra)*alpha_factor ! Here input spectra is eels
   endif
   call ELEMENTAL_dump("alpha",spectra)
 endif
 !
! !
! ! Diamagnetic term
! !
! if (l_abs_out) then
!   call msg('o eps','#',' Diamagnetic term',INDENT=0)
!   call msg('o eps','#')
!   n_output_data=2
!   if(l_IP) n_output_data=1
!   headers(:2)=(/'A [nel]','A0[nel]'/)
!   call msg("o eps",'#',headers(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
!   !
!   A_drude(:)=A_drude(:)/real(nkbz,SP)*spin_occ
!   output_data(1)=real(A_drude(2))
!   output_data(2)=real(A_drude(1))
!   if (l_IP) output_data(1)=output_data(2)
!   call msg('o eps','#',output_data(:n_output_data),INDENT=0,USE_TABS=.TRUE.)
!   call msg('o eps','#')
! endif
! !
! if(l_kerr_out .and. eval_beta) then
!   ! Alpha units
!   call msg('o beta','#',' Beta is in a.u. of '//trim(alpha_dim)//' ',INDENT=0)
!   call msg('o beta','#')
! endif
! !
! if(l_kerr_out) then
!   !
!   ! Hanomalous Hall term
!   !
!   call msg('o off','# ','Anomalous Hall effect (B_Hall):',INDENT=0)
!   call msg('o off','#')
!   !
!   ! [fs]^-1 -> 1: BS 2: IP
!   B_Hall(:)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
!   output_data(1)=real(B_Hall(2))
!   output_data(2)=real(B_Hall(1))
!   !
!   ! [Ohm*cm]^-1 -> 3: BS 4: IP
!   B_Hall(:)=B_Hall(:)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
!   output_data(3)=real(B_Hall(2))
!   output_data(4)=real(B_Hall(1))
!   !
!   ! [e^2/hbar] -> 5: BS 6: IP
!   B_Hall(:)=B_Hall(:)*1.E2_SP*HBAR_SI/e_SI**2
!   output_data(5)=real(B_Hall(2))
!   output_data(6)=real(B_Hall(1))
!   !
!   if (l_IP) then
!     call msg('o off','# B_Hall (IP)',output_data(2),INDENT=0,after_msg="[fs]^-1")
!     call msg('o off','#            ',output_data(4),INDENT=0,after_msg="[Ohm*cm]^-1")
!     call msg('o off','#            ',output_data(6),INDENT=0,after_msg="[e^2/hbar]")
!   else
!     call msg('o off','# B_Hall (BS,IP)',output_data(1:2),INDENT=0,after_msg="[fs]^-1")
!     call msg('o off','#               ',output_data(3:4),INDENT=0,after_msg="[Ohm*cm]^-1")
!     call msg('o off','#               ',output_data(5:6),INDENT=0,after_msg="[e^2/hbar]")
!   endif
!   !
!   call msg('o off','#')
!   !
! endif

! !
! ! REFLECTIVITY
! !--------------
! !
! if (eval_reflectivity) then
!   !
!   ! Reflectance is assumed for a 3D system
!   ! r=(sqrt(eps)-1)/(sqrt(eps)+1)
!   ! R=|r|^2
!   !
!   refl=(sqrt(eps)-1._SP)/(sqrt(eps)+1._SP)
!   !
!   do iw=1,BSS_n_freqs
!     !
!     output_data(1:3)=(/w(iw),real(refl(iw,1)*conjg(refl(iw,1))),real(refl(iw,2)*conjg(refl(iw,2)))/)     
!     n_output_data=3
!     if (l_print_col4) then ; n_output_data=4 ; output_data(4)=real(refl(iw,3)*conjg(refl(iw,3))) ; endif
!     if (l_IP)         then ; n_output_data=2 ; output_data(2)=output_data(3)                     ; endif
!     !
!     call msg("o refl",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
!     !
!   enddo
!   !
! endif

! !
! ! PHOTOLUMINESCENCE
! !--------------------
! if (l_phot_out) then
!   !
!   do iw=1,BSS_n_freqs
!     !
!     ! PL: 2 (int) 3 (non int) 
!     !
!     if (     l_IP) n_output_data=2 ; output_data(1:2)=(/real(PL(iw,1))*HA2EV,PL(iw,3)/)
!     if (.not.l_IP) n_output_data=3 ; output_data(1:3)=(/real(PL(iw,1))*HA2EV,PL(iw,2),PL(iw,3)/)
!     call msg("o pl_",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
!     !
!   enddo
!   !
! endif
! !
! ! KERR 
! !------
! if(l_kerr_out) then
!   !
!   ! eps_xy
!   !
!   w(:)    =real(Epsilon_ii(:,1))*HA2EV
!   !
!   do iw=1,BSS_n_freqs 
!     output_data(1:5)=(/w(iw),aimag(Epsilon_ij(iw,2)),real(Epsilon_ij(iw,2)),&
!&                  aimag(Epsilon_ij(iw,3)),real(Epsilon_ij(iw,3))/)
!     n_output_data=5
!     if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(Epsilon_ij(iw,4)),real(Epsilon_ij(iw,4))/) ; endif
!     if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                                   ; endif
!     call msg('o off','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
!     !
!   enddo
!   !
!   if(eval_beta) then
!     !
!     ! beta=alpha_xy
!     !
!     if(l_col_cut.or.trim(alpha_dim)=="volume") then
!       alpha_factor=eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
!       beta(:,1)=Epsilon_ij(:,2)*alpha_factor
!       beta(:,2)=Epsilon_ij(:,3)*alpha_factor
!       if (l_print_col4) beta(:,3)=Epsilon_ij(:,4)*alpha_factor
!     else
!       alpha_factor=eps_2_alpha/4._SP/pi
!       beta(:,1)=0._SP
!       beta(:,2)=0._SP
!       if (l_print_col4) beta(:,3)=0._SP
!     endif
!     !
!     do iw=1,BSS_n_freqs 
!       !
!       output_data(1:5)=(/w(iw),aimag(beta(iw,1)),real(beta(iw,1)),&
!&                aimag(beta(iw,2)),real(beta(iw,2))/)
!       n_output_data=5
!       if (l_print_col4) then ; n_output_data=7 ; output_data(6:7)=(/aimag(beta(iw,3)),real(beta(iw,3))/) ; endif
!       if (l_IP)         then ; n_output_data=3 ; output_data(2:3)=output_data(4:5)                       ; endif
!       call msg("o beta",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
!       !
!     enddo
!     !
!   endif
!   !
!   if(eval_moke) then
!     !
!     ! Kerr parameters
!     !
!     do iw=1,BSS_n_freqs 
!       !
!       output_data(1)= w(iw)
!       output_data(2)=-aimag(Epsilon_ij(iw,2)/( Epsilon_ii(iw,2)*sqrt(Epsilon_ii(iw,2)+1._SP) ) /DEG2RAD)
!       output_data(3)= -real(Epsilon_ij(iw,2)/( Epsilon_ii(iw,2)*sqrt(Epsilon_ii(iw,2)+1._SP) ) /DEG2RAD)
!       output_data(4)=-aimag(Epsilon_ij(iw,3)/( Epsilon_ii(iw,3)*sqrt(Epsilon_ii(iw,3)+1._SP) ) /DEG2RAD)
!       output_data(5)= -real(Epsilon_ij(iw,3)/( Epsilon_ii(iw,3)*sqrt(Epsilon_ii(iw,3)+1._SP) ) /DEG2RAD)
!       !
!       if( (real(Epsilon_ii(iw,2))==0._SP .or. real(Epsilon_ii(iw,2))==-1._SP ) &
!  &         .and. aimag(Epsilon_ii(iw,2))==0._SP ) then
!         output_data(2)=0._SP
!         output_data(3)=0._SP
!       endif
!       if( (real(Epsilon_ii(iw,3))==0._SP .or. real(Epsilon_ii(iw,3))==-1._SP ) &
!  &         .and. aimag(Epsilon_ii(iw,3))==0._SP ) then
!         output_data(4)=0._SP
!         output_data(5)=0._SP
!       endif
!       !
!       n_output_data=5
!       if (l_print_col4) then
!         n_output_data=7
!         output_data(6)=-aimag(Epsilon_ij(iw,4)/( Epsilon_ii(iw,4)*sqrt(Epsilon_ii(iw,4)+1._SP) ) /DEG2RAD)
!         output_data(7)= -real(Epsilon_ij(iw,4)/( Epsilon_ii(iw,4)*sqrt(Epsilon_ii(iw,4)+1._SP) ) /DEG2RAD)
!       endif
!       if (l_IP) then  
!         n_output_data=3  
!         output_data(2:3)=output_data(4:5)  
!       endif
!       call msg('o moke','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
!     enddo
!     !
!   endif
!   !
! endif
! !
! ! MAGNONS
! !------
! if(l_magn_out) then
!   !
!   ! magnons Spm
!   !
!   n_output_data=5
!   if (STRING_match(WHAT,"Haydock").or.l_Inversion) n_output_data=7
!   if (l_IP) n_output_data=3
!   !
!   do iw=1,BSS_n_freqs 
!     output_data(1:5)=(/real(X_magnons(iw,1,1))*HA2EV,aimag(X_magnons(iw,1,2)),real(X_magnons(iw,1,2)),&
!&                  aimag(X_magnons(iw,1,3)),real(X_magnons(iw,1,3))/)
!     if (STRING_match(WHAT,"Haydock").or.l_Inversion) output_data(6:7)=(/aimag(X_magnons(iw,1,4)),real(X_magnons(iw,1,4))/)
!     if (l_IP) output_data(2:3)=output_data(4:5)
!     call msg('o Spm','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
!     !
!   enddo
!   !
!   if (n_spinor==2) then
!     !
!     ! magnons Smp
!     !
!     do iw=1,BSS_n_freqs 
!       output_data(1:5)=(/real(X_magnons(iw,2,1))*HA2EV,aimag(X_magnons(iw,2,2)),real(X_magnons(iw,2,2)),&
!&                    aimag(X_magnons(iw,2,3)),real(X_magnons(iw,2,3))/)
!       if (STRING_match(WHAT,"Haydock").or.l_Inversion) output_data(6:7)=(/aimag(X_magnons(iw,2,4)),real(X_magnons(iw,2,4))/)
!       if (l_IP) output_data(2:3)=output_data(4:5)
!       call msg('o Smp','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
!     enddo
!     !
!   endif
!   !
! endif
! !
! !  DICHROISM
! !------
! if(l_dich_out) then
!   !
!   n_output_data=5
!   if (STRING_match(WHAT,"Haydock").or.l_Inversion) n_output_data=7
!   if (l_IP) n_output_data=3
!   !
!   do iw=1,BSS_n_freqs 
!     output_data(1:5)=(/real(X_dichroism(iw,1))*HA2EV,aimag(X_dichroism(iw,2)),real(X_dichroism(iw,2)),&
!&                  aimag(X_dichroism(iw,3)),real(X_dichroism(iw,3))/)
!     if (STRING_match(WHAT,"Haydock").or.l_Inversion) output_data(6:7)=(/aimag(X_dichroism(iw,4)),real(X_dichroism(iw,4))/)
!     if (l_IP) output_data(2:3)=output_data(4:5)
!     call msg('o CD','',output_data(:n_output_data),INDENT=-2,USE_TABS=.true.)
!     !
!   enddo
!   !
! endif
! !
! ! JOINT DOS
! !------------
! !
! if (l_jdos_out) then
!   !
!   n_output_data=5
!   if (l_IP) n_output_data=3
!   !
!   do iw=1,BSS_n_freqs
!     !
!     ! Joint DOS: 2 (int) 3 (non int)
!     !
!     output_data(1:5)=(/real(Joint_DOS(iw,1))*HA2EV,aimag(Joint_DOS(iw,2)), &
!     &                  real(Joint_DOS(iw,2)),aimag(Joint_DOS(iw,3)),real(Joint_DOS(iw,3))/)
!     if (l_IP) output_data(2:3)=output_data(4:5)
!     !
!     call msg("o jdos",'',output_data(:n_output_data),INDENT=-2,USE_TABS=.TRUE.)
!     !
!   enddo
!   !
! endif
! !
! if (l_esrt_out) then
!   !
!   n_output_data=2
!   headers(1)="E [eV]"
!   headers(2)="n_deg"
!   if(l_abs_out) then
!     headers(n_output_data+1:n_output_data+1)="abs"
!     n_output_data=n_output_data+1
!   endif
!   if (l_kerr_out) then
!     headers(n_output_data+1:n_output_data+1)="kerr"
!     n_output_data=n_output_data+1
!   endif
!   if (l_dich_out) then
!     headers(n_output_data+1:n_output_data+1)="dich"
!     n_output_data=n_output_data+1
!   endif
!   if (l_magn_out) then
!     headers(n_output_data+1:n_output_data+2)=(/"S+-","S-+"/)
!     n_output_data=n_output_data+2
!   endif
!   !
!   n_IP_BSE=2
!   if (l_IP) n_IP_BSE=1
!   !
!   E_size=1
!   !
!   do i_IP_BSE=1,n_IP_BSE
!     ! BS_E_sorted(:,:,2) (BSE) ; BS_E_sorted(:,:,1) (IP)
!     if (BS_K_coupling.and.i_IP_BSE==2) then
!       do i1=n_output_data+1,3,-1
!         headers(i1)=headers(i1-1)
!       enddo
!       E_size=2
!       headers(1)="Re(E) [eV]"
!       headers(2)="Im(E) [eV]"
!     endif
!     if(i_IP_BSE==1) name_="E_IP"
!     if(i_IP_BSE==2) name_="Esort"
!     call msg("o "//trim(name_),'#')
!     call msg("o "//trim(name_),'#',headers(:n_output_data+E_size-1),INDENT=0,USE_TABS=.TRUE.)
!     call msg("o "//trim(name_),'#')
!     do i1=1,min(BS_H_dim,1000)
!       if(E_size==1) output_data(1)=real(BS_E_sorted(i1,1,i_IP_BSE),SP)*HA2EV
!       if(E_size==2) output_data(1:2)=(/real(BS_E_sorted(i1,1,i_IP_BSE),SP),aimag(BS_E_sorted(i1,1,i_IP_BSE))/)*HA2EV
!       if (output_data(1)==0._SP) cycle
!       output_data(E_size+1:n_output_data+E_size-1)=real(BS_E_sorted(i1,2:n_output_data,i_IP_BSE),SP)
!       call msg("o "//trim(name_) ,'',output_data(:n_output_data+E_size-1),INDENT=-2,USE_TABS=.TRUE.)
!     enddo
!   enddo
!   !
! endif
! !
 !
 if (STRING_match(WHAT,"close")) call X_OUTPUT_driver("close "//OBS)
 if (STRING_match(WHAT,"reset")) call X_OUTPUT_driver("reset "//OBS)
 !
 contains 
   !
   subroutine ELEMENTAL_dump(OBS_local,F)
     character(*) :: OBS_local
     integer      :: iw,I
     complex(SP)  :: F(BSS_n_freqs,3)
     real(SP)     :: data(7)
     do iw=1,BSS_n_freqs
       data(1 )=real(W%p(iw))
       do I=1,3
         data(2*(I-1)+2)=aimag(F(iw,I))
         data(2*(I-1)+3)=real(F(iw,I))
       enddo
       if (STRING_match(WHAT,"IP")) then
         call X_OUTPUT_driver("WRITE "//OBS,data=(/data(1),data(4),data(5)/))
       else
         if (.not.l_last_col) call X_OUTPUT_driver("WRITE "//OBS,data=data(:5))
         if (     l_last_col) call X_OUTPUT_driver("WRITE "//OBS,data=data(:7))
       endif
     enddo
   end subroutine
   !
end subroutine
