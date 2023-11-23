!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine K_OUTPUT(iq,W,WHAT,OBS)
 !
 use pars,           ONLY:pi,SP,cI,cZERO,schlen
 use drivers,        ONLY:l_col_cut
 use frequency,      ONLY:w_samp
 use BS_solvers,     ONLY:BSS_desc,BSS_uses_GreenF,BSS_Vnl_included,BSS_n_freqs,K_diago_in_BS_mat,A_drude,B_Hall,&
&                         l_eps_res_ares,l_ab_from_eels
 use BS,             ONLY:BSE_mode,BS_K_is_ALDA,BS_dip_size,BS_H_dim,BS_K_coupling
 use stderr,         ONLY:STRING_match
 use X_m,            ONLY:Epsilon_ii,eps_2_alpha,X_OUTPUT_driver,Epsilon_ij,X_obs_onoff,&
&                         X_do_obs,X_OUTPUT_messages,X_dichroism,Joint_DOS,BS_E_sorted,N_BS_E_sorted,&
&                         X_magnons,i_G_shift,Q_plus_G_pt,Q_plus_G_sq_modulus
 use com,            ONLY:msg,com_compose_msg
 use R_lattice,      ONLY:bare_qpg,FineGd_desc,nkbz
 use D_lattice,      ONLY:l_0D,l_1D,l_2D,l_3D
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
 integer           :: id
 character(10)     :: solver
 character(schlen) :: out_string
 complex(SP)       :: hall(3,2),eps(BSS_n_freqs,3),spectra(BSS_n_freqs,3),alpha(BSS_n_freqs,3),fac
 real(SP)          :: data_sort(N_BS_E_sorted+1)
 logical           :: l_last_col
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
     out_string=""
   else
     if (     STRING_match(WHAT,"IP")                      ) out_string='IP'
     if (.not.STRING_match(WHAT,"IP").and..not.BS_K_is_ALDA) out_string=trim(solver)//' bse'
     if (                                      BS_K_is_ALDA) out_string=trim(solver)//' alda bse'
   endif
   call X_OUTPUT_driver(WHAT="keys"//OBS,IQ=iq,IG=i_G_shift,keys=trim(out_string))
   !
   ! If epsilon is not evaluated or extendable to the negative axis, eels is not evaluated
   ! This is not really needed and could be removed with just a warning
   if (X_do_obs("eel")   .and. .not.l_eps_res_ares     ) call X_obs_onoff("- eel")
   !
   call X_OUTPUT_driver("open "//OBS)
   !
 endif
 !
 if (STRING_match(WHAT,"append"))    call X_OUTPUT_driver("append "//OBS)
 if (STRING_match(WHAT,"override"))  call X_OUTPUT_driver("override "//OBS)
 !
 if (STRING_match(WHAT,"open").or.STRING_match(WHAT,"override")) then
   call X_OUTPUT_messages(iq,Q_plus_G=Q_plus_G_pt,ig=i_G_shift)
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
 fac=eps_2_alpha/4._SP/pi
 if (l_col_cut) fac=fac*bare_qpg(iq,i_G_shift)**2/Q_plus_G_sq_modulus
 !
 ! ABSORPTION
 !
 ! Epsilon_ii
 !------------
 if (X_do_obs("eps")) then
   !
   ! ... Diamagnetic term
   call msg("o eps",com_compose_msg(pre="# ",body="Diamagnetic term")," ",INDENT=0)
   call msg("o eps",com_compose_msg(pre="# ",body="Ao"),real(A_drude(1))/real(nkbz,SP)*spin_occ,INDENT=0,after_msg="[nel]")
   if (.not.STRING_match(WHAT,"IP")) call msg("o eps",com_compose_msg(pre="# ",body="A"),&
&                                             real(A_drude(2))/real(nkbz,SP)*spin_occ,INDENT=0,after_msg="[nel]")
   !
   ! ... Epsilon_ii
   call ELEMENTAL_dump("eps",Epsilon_ii(:,2:4)+1._SP)
 endif
 !
 ! Epsilon_ij
 !------------
 if (X_do_obs("off")) then
   !
   ! ... Anomalous Hall term
   call msg("o off",com_compose_msg(pre="# ",body="Anomalous Hall effect (B_Hall)")," ",INDENT=0)
   hall(1,1:2)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
   hall(2,1:2)=hall(1,1:2)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   hall(3,1:2)=hall(2,1:2)*1.E2_SP*HBAR_SI/e_SI**2
   call msg("o off",com_compose_msg(pre="# ",body="B_Hall (IP)"),real(hall(1,1)),INDENT=0,after_msg="[fs]^-1")
   call msg("o off",com_compose_msg(pre="# ",body="           "),real(hall(2,1)),INDENT=0,after_msg="[Ohm*cm]^-1")
   call msg("o off",com_compose_msg(pre="# ",body="           "),real(hall(3,1)),INDENT=0,after_msg="[e^2/hbar]")
   if (.not.STRING_match(WHAT,"IP")) then
     call msg("o off",com_compose_msg(pre="# ",body="B_Hall"),real(hall(1,2)),INDENT=0,after_msg="[fs]^-1")
     call msg("o off",com_compose_msg(pre="# ",body="      "),real(hall(2,2)),INDENT=0,after_msg="[Ohm*cm]^-1")
     call msg("o off",com_compose_msg(pre="# ",body="      "),real(hall(3,2)),INDENT=0,after_msg="[e^2/hbar]")
   endif
   !
   ! ... Eps_ij
   call ELEMENTAL_dump("off",Epsilon_ij(:,2:))
   !
 endif
 !
 ! POLARIZABILITY
 !----------------
 if (X_do_obs("alpha")) then
   !
   eps(:,1)=Epsilon_ii(:,2)
   eps(:,2)=Epsilon_ii(:,3)
   if (l_last_col) eps(:,3)=Epsilon_ii(:,4)
   !
   if (.not.l_ab_from_eels) alpha=eps*fac
   ! With eel the 3D eel, this is true in any dimension
   ! Remember that here eps does not contain the "+1"
   ! eel3D=-1./(eps+1.) ;  alpha=(1+eel3D)*fac
   ! accordingly alpha=eps/(1+eps)*fac
   if (     l_ab_from_eels) alpha=eps/(eps+1._SP)*fac
   call ELEMENTAL_dump("alpha",alpha)
 endif
 !
 ! Beta
 !------
 if (X_do_obs("beta")) then
   !
   spectra(:,1)=Epsilon_ij(:,2)
   spectra(:,2)=Epsilon_ij(:,3)
   if (l_last_col) spectra(:,3)=Epsilon_ij(:,4)
   !
   ! This would be the equivalent of alpha
   ! but it needs to be checked
   !! eel3D_ij=-1./(eps_ij) ;  beta=(eel3D_ij)*fac
   !! accordingly beta=-1./(eps_ij)*fac
   !if (.not.l_ab_from_eels) spectra=       spectra*fac
   !if (     l_ab_from_eels) spectra=-1._SP/spectra*fac
   spectra=       spectra*fac
   call ELEMENTAL_dump("beta",spectra)
 endif
 !
 ! EELS 
 !------
 if (X_do_obs("eel")) then
   !
   call EPS_extend(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,2),eps(:,1))
   call EPS_extend(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,3),eps(:,2))
   if (l_last_col) call EPS_extend(Epsilon_ii(:,1),BSS_n_freqs,.FALSE.,ordering,Epsilon_ii(:,4),eps(:,3))
   !
   alpha=eps/(eps+1._SP)*fac
   if(l_3D) spectra= -1._SP/(eps+1._SP)
   !if(l_3D) spectra=alpha*4._SP*pi-1._SP ! This should be equivalent to the previous
   if(l_2D) spectra= -1._SP/(eps+1._SP) ! To be fixed using alpha
   if(l_1D) spectra= -1._SP/(eps+1._SP) ! To be fixed using alpha
   call ELEMENTAL_dump("eel",spectra)
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
