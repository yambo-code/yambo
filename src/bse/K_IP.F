!
! License-Identifier: GPL
!
! Copyright (C) 2012 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM IMA
!
subroutine K_IP(iq,Ken,Xk,Dip,W_bss)
 !
 use pars,          ONLY:SP,IP,pi,cZERO,cONE,cI
 use frequency,     ONLY:w_samp
 use LIVE_t,        ONLY:live_timing
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,d3k_factor,bare_qpg,q0_def_norm
 use DIPOLES,       ONLY:DIPOLE_t
 use stderr,        ONLY:intc
 use electrons,     ONLY:levels,spin_occ,spin,nel,n_sp_pol,n_spinor
 use X_m,           ONLY:Resp_ii,Joint_DOS,alpha_dim,eps_2_alpha,&
&                        X_drude_term,global_gauge,N_BS_E_sorted,i_G_shift,&
&                        skip_cond_sum_rule,X_magnons,Resp_ij,X_dichroism
 use BS_solvers,    ONLY:Co_factor,diam_term_exact,para_term_w0,BSS_Wd,BSS_mode,BSS_uses_DbGd,FG_factor
 use BS,            ONLY:BS_T_grp,BS_nkFGbz_in_Tgrp,BS_nT_grps,BS_perturbative_SOC,&
 &                       BS_dip_size,l_BS_jdos,l_BS_esort,l_BS_trace,BS_H_dim,&
 &                       l_BS_abs,l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_photolum,&
&                        BS_K_anti_resonant,l_BS_ares_from_res
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                        PAR_COM_Xk_ibz_INDEX
 use parser_m,      ONLY:parser
 use timing_m,      ONLY:timing
 use BS_solvers,    ONLY:B_Hall
 use PHOTOLUM,      ONLY:PL,PL_prefactor,PL_weights
 use units,         ONLY:SPEED_OF_LIGHT
 !
#include<memory.h>
 !
 integer       ::iq
 type(levels)  ::Ken
 type(bz_samp) ::Xk
 type(w_samp)  ::W_bss
 type(DIPOLE_t)::Dip
 !
 ! Work space
 !
 integer           ::iv,ic,i_T_g,i_T,n_SOC,i_pert_SOC,i_res_ares,i_sort,nVAR,i_dip
 real(SP)          ::f_eh,f_eh_PL,res_PL
 logical           ::dip_v,dip_r
 complex(SP)       ::res_abs,E_plus_W0,E_plus_W(2/n_sp_pol),Z_eh,E_eh,g_fac
 !
 integer       :: i_ColFG
 complex(SP), allocatable :: E_DbGd(:)
 !
 complex(SP)       ::P_weighted(3),res_kerr,res_magn(2),res_dich
 complex(SP),    allocatable :: vtmpC(:,:)
 !
 if (l_bs_fxc) return
 !
 call section('=','Independent Particles properties @q'//trim(intc(iq)))
 !======================================================================
 call timing('T_space IP',OPR='start')
 !
 ! Polarizability ?
 !------------------
 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization       ',eps_2_alpha,"[a.u.]")
   call msg('rn','Polarizability dimension      ',trim(alpha_dim))
 endif
 if(trim(global_gauge)=="velocity") call parser('NoCondSumRule',skip_cond_sum_rule)
 !
 !...Drude
 if(l_BS_abs) call X_Drude(1,Ken,Xk,W_bss,BSS_Wd,X_drude_term)
 !
 ! Prefactors
 !------------
 if((.not.BS_perturbative_SOC).or.n_sp_pol==2) then
   n_SOC=1
   Co_factor=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi
 endif
 if( BS_perturbative_SOC.and.n_sp_pol==1) then
   n_SOC=2
   Co_factor=    1._SP     /(2._SP*pi)**3*d3k_factor*4._SP*pi
 endif
 !
 ! Setup
 !-------
 diam_term_exact=cONE*Co_factor*nel*real(Xk%nbz,SP)/spin_occ
 para_term_w0=cZERO
 if (trim(global_gauge)=='length') g_fac=cONE*Co_factor/bare_qpg(iq,i_G_shift)**2
 dip_r=.true.
 dip_v=.false.
 if (trim(global_gauge)=='velocity') then
   dip_r=.false.
   dip_v=.true.
 endif
 !
 ! Sorting pre-setup
 !-------------------
 if(l_BS_esort) then
   N_BS_E_sorted=2 ! Energies (IP + degeneracy)
   if(l_BS_abs)       n_BS_E_sorted=n_BS_E_sorted+1
   if(l_BS_kerr)      n_BS_E_sorted=n_BS_E_sorted+1
   if(l_BS_dichroism) n_BS_E_sorted=n_BS_E_sorted+1
   if(l_BS_magnons)   n_BS_E_sorted=n_BS_E_sorted+n_spinor
   YAMBO_ALLOC(vtmpC,(BS_H_dim,n_BS_E_sorted))
   vtmpC=cZERO
 endif
 !
 if (BSS_uses_DbGd .and. (index(BSS_mode,'h')/=0)) then
   FG_factor = real(Xk%nbz)/real(Xk%FGbz%N)
 else
   FG_factor = 1._SP
 end if
 !
 call live_timing('IP properties',PAR_BS_nT_col_grps)
 !
 do i_T_g=1,BS_nT_grps
   !
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !
   i_res_ares=BS_T_grp(i_T_g)%i_res_ares
   !
   do i_T=1,BS_T_grp(i_T_g)%size
     !
     i_sort =sum(BS_T_grp(BS_T_grp(i_T_g)%i_T_ref:i_T_g-1)%size)+i_T 
     nVAR   =0
     !
     ! Energy
     !========
     !
     E_plus_W0=BS_T_grp(i_T_g)%E(i_T,1)
     if(allocated(BS_T_grp(i_T_g)%W)) then
       E_plus_W0=BS_T_grp(i_T_g)%E(i_T,1)-cI*BS_T_grp(i_T_g)%W(i_T)
     endif
     !
     if (.not.BS_perturbative_SOC) E_plus_W=E_plus_W0
     if (     BS_perturbative_SOC) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
     !
     ! IA (2022/01/19):
     ! Energies in DbGd as difference with respect to coarse grid
     ! BS_nkFGbz_in_Tgrp(i_T_g) > 1 only for Haydock + DbGd
     !
     allocate(E_DbGd(BS_nkFGbz_in_Tgrp(i_T_g)))
     E_DbGd = cZERO
     do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_T_g)
       E_DbGd(i_ColFG) = BS_T_grp(i_T_g)%E(i_T,i_ColFG)-BS_T_grp(i_T_g)%E(i_T,1)
     enddo
     !
     if (l_BS_esort.and.i_res_ares==1) then
       call K_IP_sort("add",i_sort,nVAR,E_plus_W0,vtmpC)
       nVAR=nVAR+1
     endif
     !
     ! Residuals from "Dipoles"
     !==========================
     !
     Z_eh=cONE
     E_eh=cONE
     f_eh=BS_T_grp(i_T_g)%f_RES(i_T)
     if (allocated(BS_T_grp(i_T_g)%Z))                                      Z_eh=BS_T_grp(i_T_g)%Z(i_T)
     if (allocated(BS_T_grp(i_T_g)%W) .and. trim(global_gauge)=="velocity") E_eh=E_plus_W0/conjg(E_plus_W0)
     !
     ! Residuals DIP_cv * DIP_vc
     !
     if(l_BS_abs) then
       res_abs=abs(BS_T_grp(i_T_g)%dipoles_opt(1,i_T,1))**2
       if(l_BS_trace) then
         do i_dip=2,BS_dip_size
           res_abs=res_abs+abs(BS_T_grp(i_T_g)%dipoles_opt(i_dip,i_T,1))**2
         enddo
         res_abs=res_abs/real(BS_dip_size,SP)
       endif
       res_abs=res_abs*f_eh*Z_eh*E_eh
       if (l_BS_esort.and.i_res_ares==1) call K_IP_sort("add",i_sort,nVAR,res_abs,vtmpC)
     endif
     !
     if (l_BS_kerr) then
       res_kerr=BS_T_grp(i_T_g)%dipoles_opt(1,i_T,1)*conjg(BS_T_grp(i_T_g)%dipoles_opt(2,i_T,1))
       res_kerr=res_kerr*Z_eh*E_eh*f_eh
       if (l_BS_esort.and.i_res_ares==1) call K_IP_sort("add",i_sort,nVAR,res_kerr,vtmpC)
     endif
     !
     if (l_BS_photolum) then
       !
       ! Note that res_PL is always positive as f_eh_PL>0
       !
       f_eh_PL=BS_T_grp(i_T_g)%f_PL(i_T,1)
       P_weighted(:)=BS_T_grp(i_T_g)%dipoles_opt(:,i_T,1)*PL_weights(:)
       res_PL=real(dot_product(P_weighted,P_weighted)*f_eh_PL*Z_eh*E_eh)
     endif
     !
     ! Dichroism
     !
     if(l_BS_dichroism) then
       res_dich=BS_T_grp(i_T_g)%dipoles_opt(1,i_T,1)*conjg(BS_T_grp(i_T_g)%dipoles_dic(1,i_T,1))
       if(l_BS_trace) then
         do i_dip=2,BS_dip_size
           ! x*L = DIP_x*DIP_orbital;
           res_dich=res_dich+BS_T_grp(i_T_g)%dipoles_opt(i_dip,i_T,1)*conjg(BS_T_grp(i_T_g)%dipoles_dic(i_dip,i_T,1))
         enddo
         res_dich=res_dich/real(BS_dip_size,SP)
       endif
       res_dich=res_dich*f_eh*Z_eh/(q0_def_norm)
       if (l_BS_esort.and.i_res_ares==1) call K_IP_sort("add",i_sort,nVAR,res_dich,vtmpC)
     endif
     !
     ! Magnons
     !
     ! Res (RES): (Smp) <v|S-|c>(i_sp_pol_c)<c|S+|v>(i_sp_pol_c) = dip_R_magn(1)*conjg(dip_R_magn(1))
     !            (Spm) <v|S+|c>(i_sp_pol_c)<c|S-|v>(i_sp_pol_c) = dip_R_magn(2)*conjg(dip_R_magn(2))
     !
     if (l_BS_magnons) then
       res_magn(:)=BS_T_grp(i_T_g)%dipoles_mag(:,i_T,1)*conjg(BS_T_grp(i_T_g)%dipoles_mag(:,i_T,1))
       res_magn=res_magn*f_eh*Z_eh
       if (l_BS_esort.and.i_res_ares==1) then
         call K_IP_sort("add",i_sort,nVAR,res_magn(1),vtmpC)
	 if(n_spinor==2) call K_IP_sort("add",i_sort,nVAR,res_magn(2),vtmpC)
       endif
     endif
     !
     if ( abs(real(E_plus_W0)) < Dip%Energy_treshold .and. iq==1 .and. dip_r ) then
       res_abs =0._SP
       res_kerr=0._SP
       res_PL  =0._SP
       res_dich=0._SP
     endif
     !
     ! Response functions and pert SOC loop     
     !=======================================
     !
     do i_pert_SOC=1,n_SOC
       !
       if(trim(global_gauge)=='velocity') g_fac=Co_factor/E_plus_W(i_pert_SOC)**2
       !
       if (l_BS_jdos) then
         Joint_DOS(:,3) =Joint_DOS(:,3)+(-1._SP)**(i_res_ares)/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           Joint_DOS(:,3) =Joint_DOS(:,3) +    1._SP/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
         endif
       endif
       !
       if (l_BS_abs) then
         do i_ColFG=1,BS_nkFGbz_in_Tgrp(i_T_g)
           Resp_ii(:,3)=Resp_ii(:,3)-res_abs/(W_bss%p(:)-E_plus_W(i_pert_SOC)-E_DbGd(i_ColFG))
           if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
             Resp_ii(:,3)=Resp_ii(:,3)+conjg(res_abs)/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)+E_DbGd(i_ColFG)))
           endif
         enddo
       endif
       !
       if(abs(E_plus_W(i_pert_SOC))>0._SP) then
         para_term_w0(1)=para_term_w0(1)+res_abs*g_fac*E_plus_W(i_pert_SOC)
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           para_term_w0(1)=para_term_w0(1)+conjg(res_abs*g_fac*E_plus_W(i_pert_SOC))
         endif
       endif
       !
       if (l_BS_kerr) then
         !
         Resp_ij(:,3)=Resp_ij(:,3)-  res_kerr/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           Resp_ij(:,3)=Resp_ij(:,3)+conjg(res_kerr)/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
         endif
         !
         ! Anomalous Hal term, Eq.(1.12) PRB 48, 11705 (1993)
         B_Hall(1)=B_Hall(1)+res_kerr*g_fac
         !
       endif
       !
       if (l_BS_photolum) then
         PL(:,3)=PL(:,3) +PL_prefactor*res_PL*aimag(-1._SP/(W_bss%p(:)-E_plus_W(i_pert_SOC)))/pi
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           PL(:,3)=PL(:,3)-PL_prefactor*res_PL*aimag(-1._SP/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC))))/pi
         endif
       endif
       !
       if (l_BS_dichroism) then
         X_dichroism(:,3)=X_dichroism(:,3)-res_dich/(W_bss%p(:)-E_plus_W(i_pert_SOC)) 
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           X_dichroism(:,3)=X_dichroism(:,3)+res_dich/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC))) ! DS to check conjg factor
         endif
       endif
       !
       if (l_BS_magnons) then
         X_magnons(:,1,3)=X_magnons(:,1,3)-res_magn(1)/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         X_magnons(:,2,3)=X_magnons(:,2,3)-res_magn(2)/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         if (BS_K_anti_resonant.and.l_BS_ares_from_res) then
           X_magnons(:,1,3)=X_magnons(:,1,3)+conjg(res_magn(2))/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
           X_magnons(:,2,3)=X_magnons(:,2,3)+conjg(res_magn(1))/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
         endif
       endif
       !
     enddo
     !
     deallocate(E_DbGd)
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 if (l_BS_abs) Resp_ii(:,3) = Resp_ii(:,3) * FG_factor
 !
 ! Sorting procedure
 !===================
 if (l_BS_esort) call K_IP_sort("sort",0,nVAR,cZERO,vtmpC)
 !
 ! ALL 2 ALL
 !==============
 if (l_BS_abs) then
   call PP_redux_wait( Resp_ii(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( Resp_ii(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
   call PP_redux_wait( para_term_w0(1) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( para_term_w0(1) ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 if (l_BS_jdos) then
   call PP_redux_wait( Joint_DOS(:,3),COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( Joint_DOS(:,3),COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 if(l_BS_kerr) then
   call PP_redux_wait( Resp_ij(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( Resp_ij(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
   !
   call PP_redux_wait( B_Hall(1) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( B_Hall(1) ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 if (l_BS_photolum) then
   call PP_redux_wait(PL(:,3)        ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(PL(:,3)        ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 if(l_BS_dichroism) then
   call PP_redux_wait( X_dichroism(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( X_dichroism(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 if(l_BS_magnons) then
   call PP_redux_wait( X_magnons(:,:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( X_magnons(:,:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
 endif
 !
 ! Factors and more
 !==================
 !
 ! DOS
 !
 if (l_BS_jdos) Joint_DOS(:,3)=Joint_DOS(:,3)*Co_factor !/(4._SP*pi) This factor is needed for the correct Jdos
 !                                                      ! However it is commented for now since all test-suite
 !                                                      ! references needs to be updated before insterting it
 if (l_BS_abs) Resp_ii(:,3)=Resp_ii(:,3)*Co_factor
 !
 if (BS_K_anti_resonant) B_Hall(1)=B_Hall(1)-conjg(B_Hall(1))
 !
 if (l_BS_kerr) Resp_ij(:,3)=Resp_ij(:,3)*Co_factor
 !
 if (l_BS_dichroism) then
   !
   !  beta_ij=-((i*hbar*me)/(omega*qe^2))*G_ij;
   !  DeltaN=((8*pi*N*omega/(3*c))*Tr(beta_ij);
   !  N = molecular density = ??
   !  SPEED_OF_LIGHT =  c in a.u.
   !
   X_dichroism(:,3)=X_dichroism(:,3)*(cI/SPEED_OF_LIGHT)*Co_factor/(4._SP*pi)   !  this is (omega/c)*beta, with beta=-((i*hbar*me)/(omega*qe^2))*G_ij
   !
 endif
 !
 if (l_BS_magnons) X_magnons(:,:,3)=X_magnons(:,:,3)*Co_factor/(4._SP*pi)
 !
 if (     l_rpa_IP) call K_OUTPUT(iq,W_bss,"open dump IP close reset","optics kerr magnons PL dichroism")
 call K_OUTPUT(iq,W_bss,"open dump close reset bare","E_IP")
 !
 ! Clean
 ! 
 YAMBO_FREE(vtmpC)
 !
 call timing('T_space IP',OPR='stop')
 !
end subroutine
