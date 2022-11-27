!
!        Copyright (C) 2000-2022 the YAMBO team
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
 use pars,           ONLY:pi,SP,cI
 use drivers,        ONLY:l_col_cut
 use frequency,      ONLY:w_samp
 use BS_solvers,     ONLY:BSS_desc,BSS_uses_GreenF,BSS_Vnl_included,BSS_n_freqs,K_diago_in_BS_mat,A_drude,B_Hall
 use BS,             ONLY:BSE_mode,BS_K_is_ALDA,BSqpts,BS_field_direction,BS_dip_size,BS_H_dim,BS_K_coupling
 use stderr,         ONLY:STRING_match
 use X_m,            ONLY:Epsilon_ii,eps_2_alpha,EPS_2_eels,X_OUTPUT_driver,Epsilon_ij,X_obs_onoff,&
&                         X_do_obs,X_OUTPUT_messages,X_dichroism,Joint_DOS,BS_E_sorted,N_BS_E_sorted,BS_E_sorted_indx,X_magnons
 use com,            ONLY:msg,com_compose_msg
 use R_lattice,      ONLY:bare_qpg,q_norm,FineGd_desc,nkbz
 use D_lattice,      ONLY:l_0D
 use electrons,      ONLY:spin_occ
 use DIPOLES,        ONLY:DIP_desc
 use PHOTOLUM,       ONLY:PL
 use units,          ONLY:HBAR_eVfs,FEMTO,SPEED_OF_LIGHT_SI,e_SI,HBAR_SI,HA2EV
 !
 implicit none
 integer      :: iq
 type(w_samp) :: W
 character(*) :: WHAT,OBS
 !
 ! Work Space
 !
 character(2)      :: ordering
 integer           :: id,it,index_sort(5)
 character(10)     :: solver
 complex(SP)       :: spectra(BSS_n_freqs,3),eel(BSS_n_freqs,3)
 real(SP)          :: data_sort(N_BS_E_sorted+1)
 logical           :: l_last_col,eval_eels
 ! 
 if (STRING_match(WHAT,"open")) then
   !
   if (STRING_match(WHAT,"Haydock"))   solver="haydock"
   if (STRING_match(WHAT,"Diago"))     solver="diago"
   if (STRING_match(WHAT,"Slepc"))     solver="slepc"
   if (STRING_match(WHAT,"Inversion")) solver="inv"
   if (STRING_match(WHAT,"IP"))        solver="IP"
   !
   if (STRING_match(WHAT,"bare")) then
     call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq)
   else
     if (     STRING_match(WHAT,"IP")                      ) call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq,keys='IP')
     if (.not.STRING_match(WHAT,"IP").and..not.BS_K_is_ALDA) call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq,keys=trim(solver)//' bse')
     if (                   BS_K_is_ALDA) call X_OUTPUT_driver(WHAT="keys "//OBS,IQ=iq,keys=trim(solver)//' alda bse')
   endif
   !
   call X_OUTPUT_driver("open "//OBS)
 endif
 !
 if (STRING_match(WHAT,"append"))    call X_OUTPUT_driver("append "//OBS)
 if (STRING_match(WHAT,"override"))  call X_OUTPUT_driver("override "//OBS)
 !
 if (STRING_match(WHAT,"open").or.STRING_match(WHAT,"override")) then
   call X_OUTPUT_messages(iq,Q_plus_G=BSqpts(:,iq),ig=1)
   !
   if (     trim(BSE_mode)=='resonant' ) ordering='r'
   if (.not.trim(BSE_mode)=='resonant' ) ordering='R'
   call X_OUTPUT_messages(iq,GF=BSS_uses_GreenF,Vnl=BSS_Vnl_included,ordering=ordering)
   !
   call X_OUTPUT_driver("DESC "//OBS,DESC=DIP_desc)
   call X_OUTPUT_driver("DESC "//OBS,DESC=FineGd_desc)
   call X_OUTPUT_driver("DESC "//OBS,DESC=BSS_desc)
 endif
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
 if (X_do_obs("eps")) then
   !
   ! ... Diamagnetic term
   call msg("o eps",com_compose_msg(pre="# ",body="Diamagnetic term")," ",INDENT=0)
   call msg("o eps",com_compose_msg(pre="# ",body="Ao"),real(A_drude(1))/real(nkbz,SP)*spin_occ,INDENT=0,after_msg="[nel]")
   if (.not.STRING_match(WHAT,"IP")) call msg("o eps",com_compose_msg(pre="# ",body="A"),&
&                                             real(A_drude(2))/real(nkbz,SP)*spin_occ,INDENT=0,after_msg="[nel]")
   !
   ! ... Epsilon
   call ELEMENTAL_dump("eps",Epsilon_ii(:,2:4)+1._SP)
 endif
 !
 ! EELS 
 !-------
 if (X_do_obs("eel")) then
   eval_eels=EPS_2_eels(Epsilon_ii(:,1),BSS_n_freqs,.TRUE.,ordering)
   if (.not.eval_eels) call X_obs_onoff("- eel")
 endif
 if (X_do_obs("eel")) then
   eval_eels=EPS_2_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,2)+1._SP,eel(:,1)) 
   eval_eels=EPS_2_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,3)+1._SP,eel(:,2)) 
   if (l_last_col) eval_eels=EPS_2_eels(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,4)+1._SP,eel(:,3)) 
   call ELEMENTAL_dump("eel",eel)
 endif
 !
 ! POLARIZABILITY
 !----------------
 if (X_do_obs("alpha")) then
   if (      l_col_cut.or.l_0D ) spectra(:,1:3)=Epsilon_ii(:,2:4)*eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
   if (.not.(l_col_cut.or.l_0D)) spectra(:,1:3)=(1._SP+eel(:,1:3))*eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
   call ELEMENTAL_dump("alpha",spectra)
 endif
 !
 !  DICHROISM
 !------
 if (X_do_obs("CD")) call ELEMENTAL_dump("CD",X_dichroism(:,2:))
 !
 ! JOINT DOS
 !------------
 if (X_do_obs("jdos")) call ELEMENTAL_dump("jdos",Joint_DOS(:,2:))
 !
 ! E_IP
 !------
 if (X_do_obs("E_IP")) then
   do id=1,min(BS_H_dim,1000)
     data_sort(:N_BS_E_sorted)=real(BS_E_sorted(id,:N_BS_E_sorted,1),SP)
     if (data_sort(1)==0._SP) cycle
     call X_OUTPUT_driver("WRITE E_IP",data=data_sort(:N_BS_E_sorted))
   enddo
   do id=1,min(BS_H_dim,1000)
     data_sort(1)=real(BS_E_sorted(id,1,1),SP)
     if (data_sort(1)==0._SP) cycle
     do it=1,18
       if (BS_E_sorted_indx(id,it,1)==-1) exit
       data_sort(1)=real(BS_E_sorted(id,1,1),SP)
       index_sort(1:5)=real(BS_E_sorted_indx(id,it,1:5),SP)
       call X_OUTPUT_driver("WRITE E_INDX_IP",data=data_sort(1:1),indexes=index_sort)
     enddo
   enddo
 endif
 !
 ! E sorted
 !----------
 if (X_do_obs("Esort")) then
   do id=1,min(BS_H_dim,1000)
     data_sort(:N_BS_E_sorted)=real(BS_E_sorted(id,:N_BS_E_sorted,2),SP)
     if (BS_K_coupling)  then
       data_sort(1)=real(BS_E_sorted(id,1,2),SP)
       data_sort(2)=aimag(BS_E_sorted(id,1,2))
       data_sort(3:)=real(BS_E_sorted(id,2:N_BS_E_sorted,2),SP)
     endif
     if (data_sort(1)==0._SP) cycle
     if (     BS_K_coupling) call X_OUTPUT_driver("WRITE Esort",data=data_sort(:N_BS_E_sorted+1))
     if (.not.BS_K_coupling) call X_OUTPUT_driver("WRITE Esort",data=data_sort(:N_BS_E_sorted))
   enddo
 endif
 !
 ! MAGNONS
 !---------
 if (X_do_obs("Spm")) call ELEMENTAL_dump("Spm",X_magnons(:,1,2:))
 if (X_do_obs("Smp")) call ELEMENTAL_dump("Smp",X_magnons(:,2,2:))
 !
 ! Off-diagonal
 !--------------
 if (X_do_obs("off")) then
   !
   ! ... Anomalous Hall term
   call msg("o eps",com_compose_msg(pre="# ",body="Anomalous Hall effect (B_Hall)")," ",INDENT=0)
   spectra(1,1:2)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
   spectra(2,1:2)=spectra(1,1:2)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   spectra(3,1:2)=spectra(2,1:2)*1.E2_SP*HBAR_SI/e_SI**2
   call msg("o eps",com_compose_msg(pre="# ",body="B_Hall (IP)"),real(spectra(1,1)),INDENT=0,after_msg="[fs]^-1")
   call msg("o eps",com_compose_msg(pre="# ",body="           "),real(spectra(2,1)),INDENT=0,after_msg="[Ohm*cm]^-1")
   call msg("o eps",com_compose_msg(pre="# ",body="           "),real(spectra(3,1)),INDENT=0,after_msg="[e^2/hbar]")
   if (.not.STRING_match(WHAT,"IP")) then
     call msg("o eps",com_compose_msg(pre="# ",body="B_Hall"),real(spectra(1,2)),INDENT=0,after_msg="[fs]^-1")
     call msg("o eps",com_compose_msg(pre="# ",body="      "),real(spectra(2,2)),INDENT=0,after_msg="[Ohm*cm]^-1")
     call msg("o eps",com_compose_msg(pre="# ",body="      "),real(spectra(3,2)),INDENT=0,after_msg="[e^2/hbar]")
   endif
   !
   ! ... Eps_ij
   call ELEMENTAL_dump("off",Epsilon_ij(:,2:))
   !
 endif
 !
 ! Beta
 !------
 if (X_do_obs("beta")) then
   spectra=0._SP
   if (l_col_cut) spectra(:,1:3)=Epsilon_ij(:,2:4)*eps_2_alpha/4._SP/pi*bare_qpg(1,1)**2/q_norm(1)**2
   call ELEMENTAL_dump("beta",spectra)
 endif
 ! 
 ! Moke
 !------
 if (X_do_obs("moke")) then
   call EPS_2_moke(Epsilon_ii(:,1),BSS_n_freqs,Epsilon_ii(:,2),Epsilon_ij(:,2),spectra(:,1)) 
   call EPS_2_moke(Epsilon_ii(:,1),BSS_n_freqs,Epsilon_ii(:,3),Epsilon_ij(:,3),spectra(:,2)) 
   if (l_last_col) call EPS_2_moke(Epsilon_ii(:,1),BSS_n_freqs,Epsilon_ii(:,4),Epsilon_ij(:,4),spectra(:,3)) 
   call ELEMENTAL_dump("moke",spectra)
 endif
 !
 ! REFLECTIVITY
 !--------------
 if (X_do_obs("refl")) then
   ! Reflectance is assumed for a 3D system
   ! r=(sqrt(eps)-1)/(sqrt(eps)+1)
   ! R=|r|^2
   spectra(:,1:3)=(sqrt(Epsilon_ii(:,2:4)+1._SP)-1._SP)/(sqrt(Epsilon_ii(:,2:4)+1._SP)+1._SP)
   call ELEMENTAL_dump("refl",spectra)
 endif
 !
 ! PHOTOLUMINESCENCE
 !--------------------
 if (X_do_obs("PL")) then
   spectra=PL
   call ELEMENTAL_dump("PL",spectra)
 endif
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
         call X_OUTPUT_driver("WRITE "//OBS_local,data=(/data(1),data(4),data(5)/))
       else
         if (.not.l_last_col) call X_OUTPUT_driver("WRITE "//OBS_local,data=data(:5))
         if (     l_last_col) call X_OUTPUT_driver("WRITE "//OBS_local,data=data(:7))
       endif
     enddo
   end subroutine
   !
end subroutine
