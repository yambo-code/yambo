!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine K_IP(iq,Ken,Xk,X_oscillators,W_bss)
 !
 use pars,          ONLY:SP,IP,pi,cZERO,cI
 use units,         ONLY:HA2EV
 use frequency,     ONLY:w_samp
 use LIVE_t,        ONLY:live_timing
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use R_lattice,     ONLY:bz_samp,d3k_factor,q_norm
 use electrons,     ONLY:levels,spin_occ,spin,E_SOC_nbands,nel,n_sp_pol
 use interfaces,    ONLY:PARALLEL_WF_distribute
 use X_m,           ONLY:X_t,Epsilon_ii,Joint_DOS,alpha_dim,eps_2_alpha,&
&                        DIP_projected,X_drude_term,X_alloc,global_gauge,   &
&                        skip_cond_sum_rule,l_drude,Dipole_Energy_treshold
 use BS_solvers,    ONLY:BSS_Vnl_included,BSS_add_antiresonant,BSS_eval_JDOS,&
&                        BSS_n_freqs,BSS_q0,Co_factor,diam_term_exact,BSS_Wd
 use BS,            ONLY:BS_T_grp,BS_K_coupling,BS_nT_grps,L_kind
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,&
&                        PAR_Xk_bz_index,PAR_COM_Xk_ibz_INDEX
 use parser_m,      ONLY:parser
 use timing_m,      ONLY:timing
#if defined _PL
 use drivers,       ONLY:l_photolum
 use X_m,           ONLY:DIP_P
 use PHOTOLUM,      ONLY:PL,PL_prefactor,PL_weights,PL_init
#endif
#if defined _KERR
 use drivers,       ONLY:l_kerr
 use X_m,           ONLY:Epsilon_ij
 use KERR,          ONLY:A_drude
#endif
#include<memory.h>
 type(levels) ::Ken
 type(bz_samp)::Xk
 type(X_t)    ::X_oscillators
 type(w_samp) ::W_bss
 integer      ::iq
 !
 ! Work space
 !
 integer           ::ik_bz,iv,ic,i_sp_pol,i_T_g,i_T,ik_bz_mem,n_SOC,i_pert_SOC
 real(SP)          ::conjg_fac,Z_
 complex(SP)       ::drude_GreenF(W_bss%n_freqs),res,E_plus_W0,E_plus_W(2/n_sp_pol),&
&                    para_term_w0,diam_term_used
#if defined _PL
 complex(SP)       ::rotated_P(3),P_weighted(3)
 real(SP)          ::res_PL(2)
#endif
 !
 call section('=','BSE - Independent Particles @q'//trim(intc(iq))) 
 !===================================================
 !
 call timing('T_space IP',OPR='start')
 !
 if (l_bs_fxc) goto 1
 !
 ! Polarizability ?
 !==================
 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization   [au]:',eps_2_alpha)
   call msg('rn','Polarizability dimension      :',trim(alpha_dim))
 endif
 !
 if(trim(global_gauge)=="velocity") call parser('NoCondSumRule',skip_cond_sum_rule)
 !
 ! Eps_0
 !=======
 YAMBO_ALLOC(Epsilon_ii,(BSS_n_freqs,4))!2(int)3(nonint)4(n-1 Haydock/inv shifted)
 YAMBO_ALLOC(X_drude_term,(BSS_n_freqs))
 Epsilon_ii   = cZERO
 X_drude_term = cZERO
 !
 Epsilon_ii(:,1)=W_bss%p(:)
 if (BSS_eval_JDOS) then
   YAMBO_ALLOC(Joint_DOS,(BSS_n_freqs,4))
   Joint_DOS=cZERO
   Joint_DOS(:,1) =W_bss%p(:)
 endif
 !
#if defined _PL
 !
 ! PL_0
 !=======
 call PL_init( )
 !
 PL(:,1) = W_bss%p(:)  
 !
#endif
 !
 !...Drude
 call X_Drude(1,Ken,Xk,W_bss,BSS_Wd,drude_GreenF)
 if (l_drude) drude_GreenF=drude_GreenF*(Ken%f(Ken%bf,Ken%kf,Ken%sf)*(spin_occ-Ken%f(Ken%bf,Ken%kf,Ken%sf)))
 !
 ! WF distribution
 !=================
 call PARALLEL_WF_distribute(K_index=PAR_IND_Xk_ibz,&
&                            B_index=PAR_IND_CON_BANDS_X(X_oscillators%whoami),&
&                            Bp_index=PAR_IND_VAL_BANDS_X(X_oscillators%whoami),&
&                            CLEAN_UP=.TRUE.)
 !
 ! Oscillators I/O & Eps0
 !========================
 !
 !if (iq==1) then
   !
   if( E_SOC_nbands==0.or.n_sp_pol==2) then
     n_SOC=1
     Co_factor=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi
   endif
   !
   if( E_SOC_nbands/=0.and.n_sp_pol==1) then
     n_SOC=2
     Co_factor=    1._SP     /(2._SP*pi)**3*d3k_factor*4._SP*pi
   endif
   !
   diam_term_exact=cmplx(nel*real(Xk%nbz,SP)/spin_occ,0._SP,SP)
   !
   para_term_w0=cZERO
   !
   BSS_Vnl_included=X_oscillators%Vnl_included
   !
   if (trim(global_gauge)=='length'  ) conjg_fac=-1._SP
   if (trim(global_gauge)=='velocity') conjg_fac=+1._SP
   !
 !endif
   !
   call DIPOLE_driver(Ken,Xk,X_oscillators,BSS_q0,iq)
   !
   if (l_rpa_IP) call live_timing('IP absorption',PAR_BS_nT_col_grps)
   !
   do i_T_g=1,BS_nT_grps
     !
     if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
     !
     do i_T=1,BS_T_grp(i_T_g)%size
       !
       ik_bz = BS_T_grp(i_T_g)%table(i_T,1)
       iv    = BS_T_grp(i_T_g)%table(i_T,2)
       ic    = BS_T_grp(i_T_g)%table(i_T,3)
       i_sp_pol = BS_T_grp(i_T_g)%table(i_T,4)
       !
       ik_bz_mem= PAR_Xk_bz_index(ik_bz)
       !
       if(.not.allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)
       if(     allocated(BS_T_grp(i_T_g)%W)) E_plus_W0=BS_T_grp(i_T_g)%E(i_T)-cI*BS_T_grp(i_T_g)%W(i_T)
       !
       if (E_SOC_nbands==0) E_plus_W=E_plus_W0
       !
       if (E_SOC_nbands/=0) E_plus_W(:)=E_plus_W0+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
       !
       ! *length*   : DIP_projected(c,v,k) is    iq*<v|r|c> while I need    iq*<c|r|v> = -conjg(   iq*<v|r|c>)
       ! *velocity* : DIP_projected(c,v,k) is q_ver*<v|p|c> while I need q_ver*<c|r|v> =  conjg(q_ver*<v|r|c>)
       !              see definition of conjg_fac few lines before for the -1
       !
       BS_T_grp(i_T_g)%dipoles_R(i_T)=conjg_fac*conjg(DIP_projected(ic,iv,ik_bz_mem,i_sp_pol))
       if (BS_K_coupling) BS_T_grp(i_T_g)%dipoles_A(i_T)=DIP_projected(ic,iv,ik_bz_mem,i_sp_pol)
#if defined _PL
       if (l_photolum) then
         !
         call GET_the_rotated_P( )
         !
         BS_T_grp(i_T_g)%dipoles_R_PL(i_T,:)=conjg(rotated_P(:))
         if (BS_K_coupling) then
           BS_T_grp(i_T_g)%dipoles_A_PL(i_T,:)=rotated_P(:)
         endif
         !
       endif
#endif
       !
       if ( abs(real(E_plus_W0)) < Dipole_Energy_treshold .and. iq==1) cycle
       !
       ! Residuals DIP_cv * DIP_vc
       !
       Z_=1._SP
       if (allocated(BS_T_grp(i_T_g)%Z)) Z_=BS_T_grp(i_T_g)%Z(i_T)
       !
       res=BS_T_grp(i_T_g)%dipoles_R(i_T)*conjg(BS_T_grp(i_T_g)%dipoles_R(i_T))*BS_T_grp(i_T_g)%f_RES(i_T)*Z_
       if (allocated(BS_T_grp(i_T_g)%W) .and. trim(global_gauge)=="velocity") res=res*E_plus_W0/conjg(E_plus_W0)
       !
#if defined _PL
       if (l_photolum) then
         do ic=1,3
           P_weighted(ic)=BS_T_grp(i_T_g)%dipoles_R_PL(i_T,ic)*PL_weights(ic)
         enddo
         !
         ! Note that res_PL is always positive as f_PL>0
         !
         res_PL=Z_*dot_product(P_weighted,P_weighted)*BS_T_grp(i_T_g)%f_PL(:,i_T)
       endif
#endif
       !
       do i_pert_SOC=1,n_SOC
         !
         Epsilon_ii(:,3)=Epsilon_ii(:,3)-res/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         if (BSS_eval_JDOS) Joint_DOS(:,3) =Joint_DOS(:,3) -1._SP/(W_bss%p(:)-E_plus_W(i_pert_SOC))
         if (BSS_add_antiresonant.or.BS_K_coupling) then
           Epsilon_ii(:,3)=Epsilon_ii(:,3)+conjg(res)/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
           if (BSS_eval_JDOS) Joint_DOS(:,3) =Joint_DOS(:,3) +    1._SP/(W_bss%p(:)+conjg(E_plus_W(i_pert_SOC)))
         endif
         !
         if(trim(global_gauge)=='length')   para_term_w0=para_term_w0+res*E_plus_W(i_pert_SOC)/q_norm(iq)**2
         if(trim(global_gauge)=='velocity') para_term_w0=para_term_w0+res/E_plus_W(i_pert_SOC)
         if (BSS_add_antiresonant.or.BS_K_coupling) then
           if(trim(global_gauge)=='length')   para_term_w0=para_term_w0+conjg(res*E_plus_W(i_pert_SOC))/q_norm(iq)**2
           if(trim(global_gauge)=='velocity') para_term_w0=para_term_w0+conjg(res/E_plus_W(i_pert_SOC))
         endif
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
       enddo
       !
     enddo
     !
     if (l_rpa_IP) call live_timing(steps=1)
     !
   enddo
   !
   if (l_rpa_IP) call live_timing()
   !
   ! ALL 2 ALL
   !
   call PP_redux_wait( Epsilon_ii(:,3) ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( Epsilon_ii(:,3) ,COMM=PAR_COM_eh_INDEX%COMM )
   !
   if (BSS_eval_JDOS) call PP_redux_wait( Joint_DOS(:,3)  ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   if (BSS_eval_JDOS) call PP_redux_wait( Joint_DOS(:,3)  ,COMM=PAR_COM_eh_INDEX%COMM )
   !
   call PP_redux_wait( para_term_w0 ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( para_term_w0 ,COMM=PAR_COM_eh_INDEX%COMM )
   !
#if defined _PL
   if (l_photolum) then
     call PP_redux_wait(PL(:,3)        ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
     call PP_redux_wait(PL(:,3)        ,COMM=PAR_COM_eh_INDEX%COMM )
   endif
#endif
   !
   if (trim(L_kind)=="full") then
     do i_T_g=1,BS_nT_grps
       if (.not.allocated(BS_T_grp(i_T_g)%dipoles_R) ) then
         YAMBO_ALLOC(BS_T_grp(i_T_g)%dipoles_R,(BS_T_grp(i_T_g)%size))
         BS_T_grp(i_T_g)%dipoles_R=cZERO
       endif
       call PP_redux_wait(BS_T_grp(i_T_g)%dipoles_R(:), COMM=PAR_COM_Xk_ibz_INDEX%COMM )
       call PP_redux_wait(BS_T_grp(i_T_g)%dipoles_R(:), COMM=PAR_COM_eh_INDEX%COMM )
     enddo
   endif
   !
   ! DOS
   !=====
   if (BSS_eval_JDOS) Joint_DOS(:,3)=Joint_DOS(:,3)*Co_factor
   !
   if (trim(global_gauge)=='velocity') then
     if (     skip_cond_sum_rule) diam_term_used=diam_term_exact
     if (.not.skip_cond_sum_rule) diam_term_used=para_term_w0
   endif
   !
   if (trim(global_gauge)=='length'  ) Epsilon_ii(:,3)=1._SP+Co_factor* Epsilon_ii(:,3)                /q_norm(iq)**2
   if (trim(global_gauge)=='velocity') Epsilon_ii(:,3)=1._SP+Co_factor*(Epsilon_ii(:,3)-diam_term_used)/W_bss%p(:)**2
   !
   ! Drude term
   !============
   if (l_drude           ) X_drude_term(:)=       -drude_GreenF(:)       *Co_factor/q_norm(iq)**2
   if (skip_cond_sum_rule) X_drude_term(:)=(para_term_w0-diam_term_exact)*Co_factor/W_bss%p(:)**2
   !
   Epsilon_ii(:,3)=Epsilon_ii(:,3)+X_drude_term(:)
   !
#if defined _KERR
   !
   ! Eq.(1.12) PRB 48, 11705 (1993)
   !
   if (l_kerr) call K_kerr_IP(iq,W_bss%p)
   !
#endif
   !
 !
 ! CLEAN
 !
 call X_alloc('DIP_projected')
 !
 if (l_rpa_IP) then
   !
   ! Initialize & write the output file
   !
   call K_output_file(iq,"open IP")
   call K_output_file(iq,"IP")
   !
   YAMBO_FREE(Epsilon_ii)
   YAMBO_FREE(X_drude_term)
   YAMBO_FREE(Joint_DOS)
#if defined _KERR
   YAMBO_FREE(Epsilon_ij)
#endif
#if defined _PL
   YAMBO_FREE(PL)
#endif
   !
 endif
 !
1 continue
 !
 call timing('T_space IP',OPR='stop')
 !
#if defined _PL
 !
 contains
   !
   subroutine GET_the_rotated_P( )
     !
     use D_lattice,      ONLY:i_time_rev,dl_sop,sop_inv,nsym
     use parallel_m,     ONLY:PAR_DIPk_ibz_index
     !
     integer :: ik,is,ik_mem
     logical :: t_rev
     !
     ik=Xk%sstar(ik_bz,1)
     is=Xk%sstar(ik_bz,2)
     !
     if ( is<= nsym/(i_time_rev+1) ) t_rev=.false.
     if ( is > nsym/(i_time_rev+1) ) t_rev=.true.
     !
     is = sop_inv( is )
     !      
     ik_mem    = PAR_DIPk_ibz_index(ik)
     rotated_P = matmul( dl_sop(:,:,is) , DIP_P(:,ic,iv,ik_mem,i_sp_pol) )
     !
     if (t_rev) rotated_P = conjg( rotated_P )
     !
   end subroutine
   !
#endif
   !
   !
end subroutine
