!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine K_IP(iq,Ken,Xk,Dip,W_bss)
 !
 use pars,          ONLY:SP,IP,pi,cZERO,cI
 use IO_m,          ONLY:io_DIP
 use frequency,     ONLY:w_samp
 use LIVE_t,        ONLY:live_timing
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc
 use com,           ONLY:msg
 use vec_operate,   ONLY:v_norm
 use R_lattice,     ONLY:bz_samp,d3k_factor,bare_qpg,q0_def_norm,qindx_X
 use stderr,        ONLY:intc
 use wave_func,     ONLY:WF
 use interfaces,    ONLY:WF_load,WF_free
 use electrons,     ONLY:levels,spin_occ,spin,nel,n_sp_pol
 use X_m,           ONLY:Epsilon_ii,Joint_DOS,alpha_dim,eps_2_alpha,&
&                        X_drude_term,global_gauge,   &
&                        skip_cond_sum_rule,l_drude
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc,DIP_rotated
 use BS_solvers,    ONLY:BSS_Vnl_included,BSS_add_antiresonant,&
&                        Co_factor,diam_term_exact,BSS_Wd
 use BS,            ONLY:BS_T_grp,BS_K_coupling,BS_nT_grps,BS_perturbative_SOC,L_kind,BS_bands,&
 &                       BS_dip_size,l_BS_jdos,l_BS_trace
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                        PAR_COM_Xk_ibz_INDEX
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use parser_m,      ONLY:parser
 use timing_m,      ONLY:timing
#if defined _PL
 use drivers,       ONLY:l_photolum
 use PHOTOLUM,      ONLY:PL,PL_prefactor,PL_weights
#endif
#if defined _KERR
 use vec_operate,   ONLY:rot_v_xy_plane
 use BS,            ONLY:l_BS_kerr,l_BS_magnons,l_BS_anomalous_Hall
 use KERR,          ONLY:A_drude,B_Hall
 use X_m,           ONLY:X_magnons,Epsilon_ij
#endif
#include<memory.h>
 type(levels) ::Ken
 type(bz_samp)::Xk
 type(DIPOLE_t)::Dip
 type(w_samp) ::W_bss
 integer      ::iq
 !
 ! Work space
 !
 integer           ::ik_bz,ok_bz,ik,ok,isymm,osymm,iGo,iv,ic,i_sp_pol_c,i_sp_pol_v,i_T_g,i_T,n_SOC,i_pert_SOC,io_err
 real(SP)          ::conjg_fac,Z_,E_,f_
 complex(SP)       ::drude_GreenF(W_bss%n_freqs),res_abs,E_plus_W0,E_plus_W(2/n_sp_pol),&
&                    para_term_w0,diam_term_used
#if defined _PL
 complex(SP)       ::P_weighted(3)
 real(SP)          ::res_PL(2)
#endif
 !
#if defined _KERR
 complex(SP)       ::DIP_Splus,DIP_Smins
 complex(SP)       ::res_kerr,res_magn(2)
#endif
 !
 character(6)             :: DIP_kind
 complex(SP)              :: DIP_projected(BS_dip_size),DIP_expanded(3)
 real(SP)                 :: field_dir(3,BS_dip_size)
 type(elemental_collision):: BSE_scatt
 !
 !
 call section('=','Independent Particles absorption @q'//trim(intc(iq)))
 !
 if (l_bs_fxc) return
 !
 !===================================================
 !
 call timing('T_space IP',OPR='start')
 !
 ! Polarizability ?
 !==================
 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization   [au]',eps_2_alpha)
   call msg('rn','Polarizability dimension      ',trim(alpha_dim))
 endif
 !
 if(trim(global_gauge)=="velocity") call parser('NoCondSumRule',skip_cond_sum_rule)
 !
 !...Drude
 call X_Drude(1,Ken,Xk,W_bss,BSS_Wd,drude_GreenF)
 !
 ! Oscillators I/O & Eps0
 !========================
 !
 if((.not.BS_perturbative_SOC).or.n_sp_pol==2) then
   n_SOC=1
   Co_factor=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi
 endif
 !
 !
 if( BS_perturbative_SOC.and.n_sp_pol==1) then
   n_SOC=2
   Co_factor=    1._SP     /(2._SP*pi)**3*d3k_factor*4._SP*pi
 endif
 !
 diam_term_exact=cmplx(nel*real(Xk%nbz,SP)/spin_occ,0._SP,SP)
 !
 para_term_w0=cZERO
 !
 ! The residuals of the BSE are <wf_k|e^{iqr}|wf_k+q>
 ! These are trivial to compute at finite q, instead they are 
 ! obtained via the dipoles in the q->0 limit and stored to disk
 !
 if (iq==1) then
   call DIPOLE_IO(Xk,Ken,Dip,'read ',io_err,'K')
 else
   call WF_load(WF,1,maxval(qindx_X(:,:,2)),BS_bands,(/1,Xk%nibz/),title='-Oscillators/R space/K_IP')
   call elemental_collision_free(BSE_scatt)
   call elemental_collision_alloc(BSE_scatt,NG=1,TITLE="BSE") 
 endif
 !
 BSS_Vnl_included=Dip%Vnl_included
 !
 if (iq==1) then
   !
   ! Directions for the external field
   !
   field_dir(:,1)=Dip%q0/v_norm(Dip%q0)
   !
#if defined _KERR
   !
   if(l_BS_kerr) call rot_v_xy_plane(-pi/2._SP,field_dir(:,1),v_out=field_dir(:,2))
   !
   if (l_BS_trace) then
     field_dir(:,1)=(/1._SP,0._SP,0._SP/)
     field_dir(:,2)=(/0._SP,1._SP,0._SP/)
     field_dir(:,3)=(/0._SP,0._SP,1._SP/)
   endif
   !
#endif
   !
 endif
 !
 if (trim(global_gauge)=='length'  ) then
   conjg_fac=-1._SP
   DIP_kind ="DIP_iR"
   field_dir=field_dir*q0_def_norm
 endif
 if (trim(global_gauge)=='velocity') then
   conjg_fac=+1._SP
   DIP_kind ="DIP_v"
 endif
 !
 if (l_rpa_IP.or.iq>1) call live_timing('IP absorption',PAR_BS_nT_col_grps)
 !
 do i_T_g=1,BS_nT_grps
   !
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !
   do i_T=1,BS_T_grp(i_T_g)%size
     !
     ! a.Indexes
     !==========
     !
     ik_bz = BS_T_grp(i_T_g)%table(i_T,1)
     iv    = BS_T_grp(i_T_g)%table(i_T,2)
     ic    = BS_T_grp(i_T_g)%table(i_T,3)
     i_sp_pol_c = BS_T_grp(i_T_g)%table(i_T,4)
     i_sp_pol_v = BS_T_grp(i_T_g)%table(i_T,5)
     !
     if(iq/=1) then
       !
       ok_bz=qindx_X(iq,ik_bz,1)
       iGo  =qindx_X(iq,ik_bz,2)
       !
       ik   =Xk%sstar(ik_bz,1) 
       isymm=Xk%sstar(ik_bz,2) 
       !
       ok   =Xk%sstar(ok_bz,1)
       osymm=Xk%sstar(ok_bz,2)
       !
       BSE_scatt%is=(/ic,ik,isymm,i_sp_pol_c/)
       BSE_scatt%os=(/iv,ok,osymm,i_sp_pol_v/)
       BSE_scatt%qs=(/iGo,iq,1/)
       !
     endif
     !
     ! b.Energy
     !=========
     !
     if(.not.allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)
     if(     allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)-cI*BS_T_grp(i_T_g)%W(i_T)
     !
     if (.not.BS_perturbative_SOC) E_plus_W=E_plus_W0
     !
     if (     BS_perturbative_SOC) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     ! c1.Compute "dipoles"
     !=====================
     !
     ! *length*   : DIP_projected(c,v,k) is    iq*<v|r|c> while I need    iq*<c|r|v> = -conjg(   iq*<v|r|c>)
     ! *velocity* : DIP_projected(c,v,k) is q_ver*<v|p|c> while I need q_ver*<c|r|v> =  conjg(q_ver*<v|r|c>)
     !              see definition of conjg_fac few lines before for the -1
     !
     ! Absorption, PL & KERR
     !
     if(iq==1) then
       !
       DIP_expanded=DIP_rotated(ic,iv,ik_bz,i_sp_pol_c,DIP_kind,Xk)
       DIP_projected(1)=dot_product(field_dir(:,1),DIP_expanded)
       if(BS_dip_size>1) DIP_projected(2)=dot_product(field_dir(:,2),DIP_expanded)
       if(BS_dip_size>2) DIP_projected(3)=dot_product(field_dir(:,3),DIP_expanded)
       !
     else
       !
       if(trim(global_gauge)=="length")   call scatter_Bamp(BSE_scatt)
       if(trim(global_gauge)=="velocity") call scatter_Bamp_using_the_gradient(BSE_scatt,"o")
       DIP_projected(1)=-conjg(BSE_scatt%rhotw(1))
       !
     endif
     !
#if defined _KERR
     !
     ! Magnons
     !
     ! DIP_Smins(c,v,k) is  <v|S-|c>(i_sp_pol_c) while I need  <c|S-|v>(i_sp_pol_c) = conjg(<v|S+|c>(i_sp_pol_v))
     ! DIP_Splus(c,v,k) is  <v|S+|c>(i_sp_pol_c) while I need  <c|S+|v>(i_sp_pol_c) = conjg(<v|S-|c>(i_sp_pol_v))
     !
     if (l_BS_magnons) then
       !
       ! notice:
       ! In optics q=0 requires a special treatment due to the non analytic behaviour
       ! This is not needed for magnons.
       ! However to follow the same structure in K_IP.F fow WFs load / DIPOLE_IO here I keep it
       !
       if(iq==1) then
         !
         DIP_expanded   = DIP_rotated(ic,iv,ik_bz,i_sp_pol_c,"DIP_S",Xk)
         !
       else
         !
         call scatter_Bamp_spin(BSE_scatt,'x')
         DIP_expanded(1)=-BSE_scatt%rhotw(1)
         call scatter_Bamp_spin(BSE_scatt,'y')
         DIP_expanded(2)=-BSE_scatt%rhotw(1)
         !The following is commented because it is not used at present
         !BSE_scatt%os(4)=(i_sp_pol)
         !call scatter_Bamp_spin(BSE_scatt,'z')
         !DIP_expanded(3)=-BSE_scatt%rhotw(1)
         !
       endif
       !
       DIP_Splus = 0.5_SP*(DIP_expanded(1)+cI*DIP_expanded(2))
       DIP_Smins = 0.5_SP*(DIP_expanded(1)-cI*DIP_expanded(2))
       !
     endif
     !
#endif   
     !
     ! c.2 Store Dipoles
     !====================
     !
     ! Absorption, & KERR
     !
     BS_T_grp(i_T_g)%dipoles_R(:BS_dip_size,i_T)=   conjg_fac*conjg(DIP_projected(:BS_dip_size))
     if (BS_K_coupling) BS_T_grp(i_T_g)%dipoles_A(:BS_dip_size,i_T)=DIP_projected(:BS_dip_size)
     !
#if defined _PL
     ! PL
     if (l_photolum) then
       BS_T_grp(i_T_g)%dipoles_R_PL(i_T,:)=conjg(DIP_rotated(ic,iv,ik_bz,i_sp_pol_c,"DIP_v",Xk))
       if (BS_K_coupling) BS_T_grp(i_T_g)%dipoles_A_PL(i_T,:)=conjg(BS_T_grp(i_T_g)%dipoles_R_PL(i_T,:))
     endif
#endif
     !
#if defined _KERR
     ! Magnons
     if (l_BS_magnons) then
       BS_T_grp(i_T_g)%dipoles_R_mag(1,i_T)=conjg(DIP_Smins)
       BS_T_grp(i_T_g)%dipoles_R_mag(2,i_T)=conjg(DIP_Splus)
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         BS_T_grp(i_T_g)%dipoles_A_mag(1,i_T)=DIP_Splus ! Here I'm using again <c|S-|v>(i_sp_pol_c) = conjg(<v|S+|c>(i_sp_pol_v))
         BS_T_grp(i_T_g)%dipoles_A_mag(2,i_T)=DIP_Smins ! Here I'm using again <c|S+|v>(i_sp_pol_c) = conjg(<v|S-|c>(i_sp_pol_v))
       endif
     endif
#endif
     !
     ! c.3 Residuals from "Dipoles"
     !==============================
     !
     Z_=1._SP
     E_=1._SP
     f_=BS_T_grp(i_T_g)%f_RES(i_T)
     if (allocated(BS_T_grp(i_T_g)%Z))                                      Z_=BS_T_grp(i_T_g)%Z(i_T)
     if (allocated(BS_T_grp(i_T_g)%W) .and. trim(global_gauge)=="velocity") E_=E_plus_W0/conjg(E_plus_W0)
     !
     ! Residuals DIP_cv * DIP_vc
     !
     res_abs=BS_T_grp(i_T_g)%dipoles_R(1,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R(1,i_T))
     if(l_BS_trace) then
       res_abs=res_abs+BS_T_grp(i_T_g)%dipoles_R(2,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R(2,i_T))
       res_abs=res_abs+BS_T_grp(i_T_g)%dipoles_R(3,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R(3,i_T))
     endif
     res_abs=res_abs*f_*Z_*E_
     !
#if defined _KERR
     if (l_BS_kerr) then
       res_kerr=BS_T_grp(i_T_g)%dipoles_R(1,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R(2,i_T))
       res_kerr=res_kerr*Z_*E_*f_
     endif
#endif
     !
#if defined _PL
     if (l_photolum) then
       !
       ! Note that res_PL is always positive as f_PL>0
       !
       f_PL=BS_T_grp(i_T_g)%f_PL(:,i_T)
       P_weighted(:)=BS_T_grp(i_T_g)%dipoles_R_PL(i_T,:)*PL_weights(:)
       res_PL=dot_product(P_weighted,P_weighted)*f_*Z_*E_
     endif
#endif
     !
     ! Magnons
     !
     ! Res (RES): (Smp) <v|S-|c>(i_sp_pol_c)<c|S+|v>(i_sp_pol_c) = dip_R_magn(1)*conjg(dip_R_magn(1))
     !            (Spm) <v|S+|c>(i_sp_pol_c)<c|S-|v>(i_sp_pol_c) = dip_R_magn(2)*conjg(dip_R_magn(2))
     !
#if defined _KERR
     if (l_BS_magnons) then
       res_magn(:)=BS_T_grp(i_T_g)%dipoles_R_mag(:,i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R_mag(:,i_T))
       res_magn=res_magn*f_*Z_
     endif
#endif
     !
     if ( abs(real(E_plus_W0)) < Dip%Energy_treshold .and. iq==1 ) then
       res_abs =0._SP
#if defined _KERR
       res_kerr=0._SP
#endif
#if defined _PL
       res_PL  =0._SP
#endif
     endif
     !
     ! d.Response functions and pert SOC loop     
     !=========================================
     !
     do i_pert_SOC=1,n_SOC
       !
       Epsilon_ii(:,3)=Epsilon_ii(:,3)-res_abs/(W_bss%p(:)-E_plus_W(i_pert_SOC))
       if (l_BS_jdos) Joint_DOS(:,3) =Joint_DOS(:,3) -1._SP/(W_bss%p(:)-E_plus_W(i_pert_SOC))
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         Epsilon_ii(:,3)=Epsilon_ii(:,3)+conjg(res_abs)/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
         if (l_BS_jdos) Joint_DOS(:,3) =Joint_DOS(:,3) +    1._SP/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
       endif
       !
       if(trim(global_gauge)=='length')   para_term_w0=para_term_w0+res_abs*E_plus_W(i_pert_SOC)/bare_qpg(iq,1)**2
       if(trim(global_gauge)=='velocity') para_term_w0=para_term_w0+res_abs/E_plus_W(i_pert_SOC)
       if (BSS_add_antiresonant.or.BS_K_coupling) then
         if(trim(global_gauge)=='length')   para_term_w0=para_term_w0+conjg(res_abs*E_plus_W(i_pert_SOC))/bare_qpg(iq,1)**2
         if(trim(global_gauge)=='velocity') para_term_w0=para_term_w0+conjg(res_abs/E_plus_W(i_pert_SOC))
       endif
       !
#if defined _KERR
       !
       if (l_BS_kerr) then
         !
         Epsilon_ij(:,3)=Epsilon_ij(:,3)-  res_kerr/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         if (BSS_add_antiresonant.or.BS_K_coupling) then
           Epsilon_ij(:,3)=Epsilon_ij(:,3)+conjg(res_kerr)/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
         endif
         !
         ! Anomalous Hal term, Eq.(1.12) PRB 48, 11705 (1993)
         if(trim(global_gauge)=='length')   B_Hall(1)=B_Hall(1)+res_kerr/   bare_qpg(1,1)    **2
         if(trim(global_gauge)=='velocity') B_Hall(1)=B_Hall(1)+res_kerr/E_plus_W(i_pert_SOC)**2
         !
       endif
       !
#endif
       !
#if defined _PL
       if (l_photolum) then
         PL(:,3)=PL(:,3)  +PL_prefactor*res_PL(1)*aimag(-1._SP/(W_bss%p(:)-E_plus_W(i_pert_SOC)))/pi
         if (BSS_add_antiresonant.or.BS_K_coupling) then
           PL(:,3)=PL(:,3)+PL_prefactor*res_PL(2)*aimag(-1._SP/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC))))/pi
         endif
       endif
#endif
       !
#if defined _KERR
       !
       if (l_BS_magnons) then
         X_magnons(:,1,3)=X_magnons(:,1,3)-res_magn(1)/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         X_magnons(:,2,3)=X_magnons(:,2,3)-res_magn(2)/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         !
         if (BSS_add_antiresonant.or.BS_K_coupling) then
           !
           ! Both -1 factors appear explicitly in the code
           !
           X_magnons(:,1,3)=X_magnons(:,1,3)+conjg(res_magn(2))/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
           X_magnons(:,2,3)=X_magnons(:,2,3)+conjg(res_magn(1))/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
           !
         endif
       endif
       !
#endif
       !
     enddo
     !
   enddo
   !
   if (l_rpa_IP.or.iq>1) call live_timing(steps=1)
   !
 enddo
 !
 if (l_rpa_IP.or.iq>1) call live_timing()
 !
 ! ALL 2 ALL
 !==============
 !
 call PP_redux_wait( Epsilon_ii(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( Epsilon_ii(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 !
 if (l_BS_jdos) then
   call PP_redux_wait( Joint_DOS(:,3)  ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( Joint_DOS(:,3)  ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 call PP_redux_wait( para_term_w0 ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( para_term_w0 ,COMM=PAR_COM_eh_INDEX%COMM )
 !
#if defined _KERR
 if(l_BS_kerr) then
   call PP_redux_wait( Epsilon_ij(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( Epsilon_ij(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
   !
   call PP_redux_wait( B_Hall(1) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( B_Hall(1) ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
#endif
 !
#if defined _PL
 if (l_photolum) then
   call PP_redux_wait(PL(:,3)        ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(PL(:,3)        ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
#endif
 !
#if defined _KERR
 if(l_BS_magnons) then
   call PP_redux_wait( X_magnons(:,:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( X_magnons(:,:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
#endif
 !
 if (trim(L_kind)=="full") then
   do i_T_g=1,BS_nT_grps
     if (.not.allocated(BS_T_grp(i_T_g)%dipoles_R) ) then
       YAMBO_ALLOC(BS_T_grp(i_T_g)%dipoles_R,(BS_dip_size,BS_T_grp(i_T_g)%size))
       BS_T_grp(i_T_g)%dipoles_R=cZERO
     endif
     call PP_redux_wait(BS_T_grp(i_T_g)%dipoles_R, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
     call PP_redux_wait(BS_T_grp(i_T_g)%dipoles_R, COMM=PAR_COM_eh_INDEX%COMM )
   enddo
 endif
 !
 ! Factors and more
 !====================
 !
 ! DOS
 !
 if (l_BS_jdos) Joint_DOS(:,3)=Joint_DOS(:,3)*Co_factor !/(4._SP*pi) This factor is needed for the correct Jdos
 !                                                          ! However it is commented for now since all test-suite
 !                                                          ! references needs to be updated before insterting it
 if (trim(global_gauge)=='velocity') then
   if (     skip_cond_sum_rule) diam_term_used=diam_term_exact
   if (.not.skip_cond_sum_rule) diam_term_used=para_term_w0
 endif
 !
 if (trim(global_gauge)=='length'  ) Epsilon_ii(:,3)=Co_factor* Epsilon_ii(:,3)                / bare_qpg(iq,1)**2
 if (trim(global_gauge)=='velocity') Epsilon_ii(:,3)=Co_factor*(Epsilon_ii(:,3)-diam_term_used)/W_bss%p(:)**2
 !
 ! Drude term
 !
 if (l_drude           ) X_drude_term(:)=       -drude_GreenF(:)       *4._SP*pi/ bare_qpg(iq,1)**2
 if (skip_cond_sum_rule) X_drude_term(:)=(para_term_w0-diam_term_exact)*Co_factor/W_bss%p(:)**2
 !
 Epsilon_ii(:,3)=Epsilon_ii(:,3)+X_drude_term(:)
 !
#if defined _KERR
 !
 ! Eq.(1.12) PRB 48, 11705 (1993)
 !
 A_drude(1)=para_term_w0-diam_term_exact
 !
 if (l_BS_kerr) then
   !
   B_Hall(1)=B_Hall(1)*Co_factor
   if (BSS_add_antiresonant.or.BS_K_coupling) B_Hall(1)=B_Hall(1)-conjg(B_Hall(1))
   !
   if(trim(global_gauge)=='length'   ) Epsilon_ij(:,3)= Epsilon_ij(:,3)*Co_factor/bare_qpg(iq,1)**2
   if(trim(global_gauge)=='velocity' ) Epsilon_ij(:,3)= Epsilon_ij(:,3)*Co_factor/ W_bss%p(:)   **2
   !
   ! I add the term describing the Anomalous Hall effect which is
   ! missing in the length gauge (this should be zero for systems with a gap)
   if (l_BS_anomalous_Hall.and.trim(global_gauge)=='length') Epsilon_ij(:,3)=Epsilon_ij(:,3)+B_Hall(1)/W_bss%p(:)
   !
 endif
 !
 if (l_BS_magnons) X_magnons(:,:,3)=X_magnons(:,:,3)*Co_factor/(4._SP*pi)
 !
#endif
 !
 ! CLEAN
 !
 if (iq==1.and.io_DIP) then
   call DIP_alloc('DIP_iR')
   call DIP_alloc('DIP_P')
   call DIP_alloc('DIP_v')
   call DIP_alloc('DIP_S')
   call DIP_alloc('DIP_spin')
   call DIP_alloc('DIP_orbital')
   call DIP_alloc('P_square') 
   call DIP_alloc('DIP_P_spinor')
 endif
 !
 !if(iq>1) then
   call elemental_collision_free(BSE_scatt)
   call WF_free(WF)
 !endif
 !
 if (l_rpa_IP) then
   !
   ! Initialize & write the output file
   !
   call K_output_file(iq,"open IP")
   call K_output_file(iq,"IP")
   !
 endif
 !
 call timing('T_space IP',OPR='stop')
 !
end subroutine
