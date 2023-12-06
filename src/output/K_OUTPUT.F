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
&                         diam_term_exact,para_term_w0,l_abs_prop_chi_bse,&
&                         l_eels_from_inversion,Co_factor
 use BS,             ONLY:BSE_mode,BS_K_is_ALDA,BS_dip_size,BS_H_dim,BS_K_coupling,&
&                         l_BS_anomalous_Hall
 use stderr,         ONLY:STRING_match
 use X_m,            ONLY:Epsilon_ii,eps_2_alpha,X_OUTPUT_driver,Epsilon_ij,X_obs_onoff,&
&                         X_do_obs,X_OUTPUT_messages,X_dichroism,Joint_DOS,BS_E_sorted,N_BS_E_sorted,&
&                         X_magnons,i_G_shift,Q_plus_G_pt,Q_plus_G_sq_modulus,Resp_ii,Resp_ij,&
&                         l_drude,skip_cond_sum_rule,X_drude_term,global_gauge
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
 integer           :: id,i_rsp,n_resp,iw_ref
 character(10)     :: solver
 character(schlen) :: out_string
 complex(SP)       :: hall(3,2),tmp_var(BSS_n_freqs,3),spectra(BSS_n_freqs,3),&
 &                    fac,diam_term_used(3),spectra_ij(BSS_n_freqs,3)
 real(SP)          :: data_sort(N_BS_E_sorted+1),nel_diam(3)
 logical           :: l_last_col,l_inv,l_hay,l_sle
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
   call X_OUTPUT_driver("open "//OBS)
   !
 endif
 !
 l_inv=STRING_match(WHAT,"Inversion")
 l_hay=STRING_match(WHAT,"Haydock")
 l_sle=STRING_match(WHAT,"Slepc")
 l_last_col=l_inv.or.l_hay.and.(.not.K_diago_in_BS_mat)
 !
 if(.not.l_last_col) n_resp=2
 if(     l_last_col) n_resp=3
 !
 if (     trim(BSE_mode)=='resonant' ) ordering='r'
 if (.not.trim(BSE_mode)=='resonant' ) ordering='R'
 !
 if (STRING_match(WHAT,"append"))    call X_OUTPUT_driver("append "//OBS)
 if (STRING_match(WHAT,"override"))  call X_OUTPUT_driver("override "//OBS)
 !
 if (STRING_match(WHAT,"open").or.STRING_match(WHAT,"override")) then
   call X_OUTPUT_messages(iq,Q_plus_G=Q_plus_G_pt,ig=i_G_shift)
   !
   call X_OUTPUT_messages(iq,GF=BSS_uses_GreenF,Vnl=BSS_Vnl_included,ordering=ordering)
   !
   call X_OUTPUT_driver("DESC "//OBS,DESC=DIP_desc)
   call X_OUTPUT_driver("DESC "//OBS,DESC=FineGd_desc)
   call X_OUTPUT_driver("DESC "//OBS,DESC=BSS_desc)
 endif
 !
 if (STRING_match(WHAT,"close").and..not.STRING_match(WHAT,"dump")) then
   call X_OUTPUT_driver("close "//OBS)
   return
 endif
 !
 if (.not.STRING_match(WHAT,"dump")) return
 !
 if (X_do_obs("eps").or.X_do_obs("Alpha").or.X_do_obs("eel")) then
   !
   fac=eps_2_alpha/4._SP/pi
   if (l_col_cut) fac=fac*bare_qpg(iq,i_G_shift)**2/Q_plus_G_sq_modulus
   !
   ! Resp = -vX
   do i_rsp=1,n_resp
     spectra(:,i_rsp)=Resp_ii(:,i_rsp+1)
     spectra(:,i_rsp)=Resp_ii(:,i_rsp+1)
     if (X_do_obs("off").or.X_do_obs("beta")) then
       spectra_ij(:,i_rsp)=Resp_ij(:,i_rsp+1)
       spectra_ij(:,i_rsp)=Resp_ij(:,i_rsp+1)
     endif
   enddo
   !
   ! Eq.(1.12) PRB 48, 11705 (1993)
   !
   A_drude=para_term_w0-diam_term_exact
   !
   if (trim(global_gauge)=='velocity') then
     if (     skip_cond_sum_rule) diam_term_used=diam_term_exact
     if (.not.skip_cond_sum_rule) diam_term_used=para_term_w0
     spectra(:,1)=spectra(:,1)-diam_term_used(2)
     spectra(:,2)=spectra(:,2)-diam_term_used(1)
     spectra(:,3)=spectra(:,3)-diam_term_used(3)
   endif
   !
   do i_rsp=1,n_resp
     if(trim(global_gauge)=='length'  ) spectra(:,i_rsp)= spectra(:,i_rsp)/bare_qpg(iq,i_G_shift)**2
     if(trim(global_gauge)=='velocity') spectra(:,i_rsp)= spectra(:,i_rsp)/ W%p(:)**2
     !
     if (X_do_obs("off").or.X_do_obs("beta")) then
       if(trim(global_gauge)=='length'  ) spectra_ij(:,i_rsp)=spectra_ij(:,i_rsp)/bare_qpg(iq,i_G_shift)**2
       if(trim(global_gauge)=='velocity') spectra_ij(:,i_rsp)=spectra_ij(:,i_rsp)/ W%p(:)**2
     endif
   enddo
   !
 endif
 !
 ! ABSORPTION
 !
 ! Epsilon_ii
 !------------
 if (X_do_obs("eps")) then
   !
   call get_from_Lbar_or_Lfull_inverted(Epsilon_ii(:,2:4),spectra)
   !
   call add_drude_term(Epsilon_ii(:,2:4))
   !
   nel_diam(:)=real(A_drude(:))/real(nkbz,SP)*spin_occ/Co_factor
   !
   ! ... Diamagnetic term
   call msg("o eps",com_compose_msg(pre="# ",body="Diamagnetic term")," ",INDENT=0)
   call msg("o eps",com_compose_msg(pre="# ",body="Ao"),nel_diam(1),INDENT=0,after_msg="[nel]")
   if (.not.STRING_match(WHAT,"IP")) &
   &call msg("o eps",com_compose_msg(pre="# ",body="A"),nel_diam(2),INDENT=0,after_msg="[nel]")
   if (l_last_col) &
   &call msg("o eps",com_compose_msg(pre="# ",body="A'"),nel_diam(3),INDENT=0,after_msg="[nel]")
   !
   ! ... Epsilon_ii
   call ELEMENTAL_dump("eps",Epsilon_ii(:,2:4)+1._SP)
 endif
 !
 ! Epsilon_ij
 !------------
 if (X_do_obs("off")) then
   !
   ! Resp = -vX
   Epsilon_ij(:,2:n_resp+1)= spectra_ij(:,1:n_resp)
   !
   if(l_BS_anomalous_Hall.and.trim(global_gauge)=='length') then
     !
     ! I add the term describing the Anomalous Hall effect,
     ! that is missing in the length gauge
     ! This is zero for systems with zero chern number
     !
     tmp_var=cZERO
     tmp_var(:,1)=B_Hall(2)/W%p(:)
     tmp_var(:,2)=B_Hall(1)/W%p(:)
     if(l_last_col) then
       if(l_hay) tmp_var(:,3)=tmp_var(:,1)
       if(l_inv) tmp_var(:,3)=tmp_var(:,2)
     endif
     !
     Epsilon_ij(:,2:n_resp+1)=Epsilon_ij(:,2:n_resp+1)+tmp_var(:,1:n_resp)
     !
   endif
   !
   ! ... Anomalous Hall term
   call msg("o off",com_compose_msg(pre="# ",body="Anomalous Hall effect (B_Hall)")," ",INDENT=0)
   hall(1,1:2)=B_Hall(:)*HA2EV/(4*pi*cI*HBAR_eVfs)
   hall(2,1:2)=hall(1,1:2)*1.E5_SP/(SPEED_OF_LIGHT_SI)**2/FEMTO
   hall(3,1:2)=hall(2,1:2)*1.E2_SP*HBAR_SI/e_SI**2
   !
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
   ! WARNING: case with flag ImposeAlphaChiBar from input to be fixed
   if(.not.l_col_cut) call get_from_Lbar_or_Lfull_inverted(tmp_var,spectra)
   if(     l_col_cut) call get_from_Lfull_or_Lbar_inverted(tmp_var,spectra)
   tmp_var=tmp_var*fac
   call ELEMENTAL_dump("alpha",tmp_var)
 endif
 !
 ! Beta
 !------
 if (X_do_obs("beta")) then
   tmp_var(:,1:n_resp)=spectra_ij(:,1:n_resp)*fac
   call ELEMENTAL_dump("beta",tmp_var)
 endif
 !
 ! EELS 
 !------
 if (X_do_obs("eel")) then
   call FREQUENCIES_set_reference_point(W%p,W%n_freqs,iw_ref)
   call get_eel_from_spectra(tmp_var,spectra)
   call ELEMENTAL_dump("eel",tmp_var)
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
   call EPS_2_moke(Epsilon_ii(:,1),BSS_n_freqs,Epsilon_ii(:,2),Epsilon_ij(:,2),tmp_var(:,1))
   call EPS_2_moke(Epsilon_ii(:,1),BSS_n_freqs,Epsilon_ii(:,3),Epsilon_ij(:,3),tmp_var(:,2))
   if (l_last_col) call EPS_2_moke(Epsilon_ii(:,1),BSS_n_freqs,Epsilon_ii(:,4),Epsilon_ij(:,4),tmp_var(:,3))
   call ELEMENTAL_dump("moke",tmp_var)
 endif
 !
 ! REFLECTIVITY
 !--------------
 if (X_do_obs("refl")) then
   ! Reflectance is assumed for a 3D system
   ! r=(sqrt(eps)-1)/(sqrt(eps)+1)
   ! R=|r|^2
   tmp_var(:,1:3)=(sqrt(Epsilon_ii(:,2:4)+1._SP)-1._SP)/(sqrt(Epsilon_ii(:,2:4)+1._SP)+1._SP)
   call ELEMENTAL_dump("refl",tmp_var)
 endif
 !
 ! PHOTOLUMINESCENCE
 !--------------------
 if (X_do_obs("PL")) then
   tmp_var=PL
   call ELEMENTAL_dump("PL",tmp_var)
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
   end subroutine ELEMENTAL_dump
   !
   !
   subroutine get_from_Lbar_or_Lfull_inverted(spec_o,spec_i)
     !
     complex(SP), intent(in)  :: spec_i(BSS_n_freqs,3)
     complex(SP), intent(out) :: spec_o(BSS_n_freqs,3)
     !
     ! IP part is always proportional to Xo
     spec_o(:,2)= spec_i(:,2)
     if (l_last_col.and.l_inv) spec_o(:,3)= spec_i(:,3)
     !
     ! BSE_part, equations depends if Lbse contasin Lfull or Lbar
     if (.not.l_abs_prop_chi_bse) spec_o(:,1)=-spec_i(:,1)/(spec_i(:,1)-1._SP)
     if (     l_abs_prop_chi_bse) spec_o(:,1)= spec_i(:,1)
     if (l_last_col .and. l_hay) then
       if (.not.l_abs_prop_chi_bse) spec_o(:,3)=-spec_i(:,3)/(spec_i(:,3)-1._SP)
       if (     l_abs_prop_chi_bse) spec_o(:,3)= spec_i(:,3)
     endif
     !
   end subroutine get_from_Lbar_or_Lfull_inverted
   !
   !
   subroutine get_from_Lfull_or_Lbar_inverted(spec_o,spec_i)
     !
     complex(SP), intent(in)  :: spec_i(BSS_n_freqs,3)
     complex(SP), intent(out) :: spec_o(BSS_n_freqs,3)
     !
     ! IP part is always proportional to Xo/(Xo+1._SP)
     ! To discuss and agree what to put here
     spec_o(:,2)= spec_i(:,2)/(spec_i(:,2)+1._SP)
     if (l_last_col.and.l_inv) spec_o(:,3)= spec_i(:,3)/(spec_i(:,3)+1._SP)
     !
     ! BSE_part, equations depends if Lbse contasin Lfull or Lbar
     if (.not.l_abs_prop_chi_bse) spec_o(:,1)= spec_i(:,1)/(spec_i(:,1)+1._SP)
     if (     l_abs_prop_chi_bse) spec_o(:,1)= spec_i(:,1)
     if (l_last_col .and. l_hay) then
       if (.not.l_abs_prop_chi_bse) spec_o(:,3)= spec_i(:,3)/(spec_i(:,3)+1._SP)
       if (     l_abs_prop_chi_bse) spec_o(:,3)= spec_i(:,3)
     endif
     !
   end subroutine get_from_Lfull_or_Lbar_inverted
   !
   !
   subroutine get_eel_from_spectra(spec_o,spec_i)
     !
     complex(SP), intent(in)  :: spec_i(BSS_n_freqs,3)
     complex(SP), intent(out) :: spec_o(BSS_n_freqs,3)
     !
     complex(SP)  :: tmp_spec(BSS_n_freqs),drude(BSS_n_freqs,3)
     !
     drude=cZERO
     call add_drude_term(drude)
     !
     spec_o(:,1:n_resp)=spec_i(:,1:n_resp)
     !
     ! IP part
     spec_o(:,2)= spec_o(:,2)+drude(:,2)
     call EPS_extend(W%p,BSS_n_freqs,iw_ref,.true.,ordering,spec_o(:,2),tmp_spec)
     spec_o(:,2)= -1._SP/(tmp_spec+1._SP)
     !WARNING, If I replace the above two lines with the following two
     !I get spec_o=0 after the first line. This seems a compiler bug.
     !EPS_extend(W%p,BSS_n_freqs,iw_ref,.true.,ordering,spec_o(:,2),spec_o(:,2))
     !spec_o(:,2)= -1._SP/(spec_o(:,2)+1._SP)
     if(l_last_col .and. l_inv) then
       spec_o(:,3)= spec_o(:,3)+drude(:,3)
       call EPS_extend(W%p,BSS_n_freqs,iw_ref,.false.,ordering,(spec_o(:,3)),tmp_spec)
       spec_o(:,3)= -1._SP/(tmp_spec+1._SP)
     endif
     !
     ! BSE part
     if (l_eels_from_inversion .and. .not. l_sle ) then
       if (.not.l_abs_prop_chi_bse) spec_o(:,1)=-spec_o(:,1)/(spec_o(:,1)-1._SP)
       spec_o(:,1)= spec_o(:,1)+drude(:,1)
       call EPS_extend(W%p,BSS_n_freqs,iw_ref,.false.,ordering,spec_o(:,1),tmp_spec)
       spec_o(:,1)= -1._SP/(tmp_spec+1._SP)
       if(l_last_col.and.l_hay) then
         if (.not.l_abs_prop_chi_bse) spec_o(:,3)=-spec_o(:,3)/(spec_o(:,3)-1._SP)
         spec_o(:,3)= spec_o(:,3)+drude(:,3)
         call EPS_extend(W%p,BSS_n_freqs,iw_ref,.false.,ordering,spec_o(:,3),tmp_spec)
         spec_o(:,3)= -1._SP/(tmp_spec+1._SP)
       endif
     endif
     !
   end subroutine get_eel_from_spectra
   !
   !
   subroutine add_drude_term(spec_io)
     !
     complex(SP), intent(inout)  :: spec_io(BSS_n_freqs,3)
     !
     complex(SP)  :: tmp_dru(BSS_n_freqs,3)
     !
     tmp_dru=cZERO
     ! ... Drude term (if l_drude is computed previously by X_drude call in K_IP.F)
     if (l_drude) then
       tmp_dru(:,1)=-X_drude_term(:)*4._SP*pi/bare_qpg(iq,i_G_shift)**2
       tmp_dru(:,2)=tmp_dru(:,1)
       if(l_last_col) tmp_dru(:,3)=tmp_dru(:,1)
     endif
     !
     ! ... Drude term via sum rule breaking
     if (skip_cond_sum_rule) then
       tmp_dru(:,1)=A_drude(2)/W%p(:)**2
       tmp_dru(:,2)=A_drude(1)/W%p(:)**2
       tmp_dru(:,3)=A_drude(3)/W%p(:)**2
     endif
     !
     if (l_drude.or.skip_cond_sum_rule) then
       spec_io(:,1)=spec_io(:,1)+tmp_dru(:,1)
       spec_io(:,2)=spec_io(:,2)+tmp_dru(:,2)
       if(l_last_col) spec_io(:,3)=spec_io(:,3)+tmp_dru(:,3)
     endif
     !
   end subroutine add_drude_term
   !
end subroutine
