!
!        Copyright (C) 2000-2015 the YAMBO team
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
 use pars,          ONLY:SP,IP,pi,cZERO
 use units,         ONLY:HA2EV
 use frequency,     ONLY:w_samp
 use LIVE_t,        ONLY:live_timing
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,d3k_factor,q_norm
 use electrons,     ONLY:levels,spin_occ,spin,E_SOC_nbands,nel
 use interfaces,    ONLY:PARALLEL_WF_distribute
 use X_m,           ONLY:X_t,X_epsilon,X_drude_term,alpha_dim,eps_2_alpha,&
&                        DIP_projected,X_drude_term,X_alloc,iw_ref,global_gauge, &
&                        skip_cond_sum_rule,A_drude,l_drude
 use BS,            ONLY:BSS_Vnl_included,BS_anti_res,BS_drude_f_eh,BSS_Wd,&
&                        BSS_n_freqs,BSS_q0,BS_T_grp,BS_K_coupling,BS_nT_grps
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X,&
&                        PAR_Xk_bz_index,PAR_COM_Xk_ibz_INDEX
#if defined _KERR
 use drivers,       ONLY:l_kerr
 use KERR,          ONLY:DIP_projected_r90,KERR_alloc
#endif
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 type(levels) ::Ken
 type(bz_samp)::Xk
 type(X_t)    ::X_oscillators
 type(w_samp) ::W_bss
 integer      ::iq
 !
 ! Work space
 !
 integer           ::ik_bz,iv,ic,i_sp,epsilon_dim,i_T_g,i_T,ik_mem,&
&                    n_soc,i_pert_SOC
 real(SP)          ::Co,conjg_fac,IP_E(2)
 complex(SP)       ::drude_GreenF(W_bss%n_freqs),local_Z,diam_term
 !
 call section('=','Independent Particles absorption') 
 !===================================================
 !
#if defined _TIMING
 call timing('T_space IP',OPR='start')
#endif
 !
 ! Setups
 !
! if (BS_res_K_corr) then
!   !
!   allocate(O_n_c_states(Xk%nbz,n_sp_pol),O_n_v_states(Xk%nbz,n_sp_pol))
!   call mem_est("O_n_c_states O_n_v_states",&
!&               (/Xk%nbz,n_sp_pol,Xk%nbz,n_sp_pol/),(/IP,IP,IP,IP/))
!   !
! endif
 !
 ! Look for the W(iw_ref) closest 0
 !==================================
 call FREQUENCIES_set_iw_ref(W_bss%p,BSS_n_freqs)
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
 ! Eps_0
 !=======
 epsilon_dim=4
#if defined _KERR
 if(l_kerr) epsilon_dim=7
#endif
 allocate(X_epsilon(epsilon_dim,BSS_n_freqs))
 allocate(X_drude_term(BSS_n_freqs))
 X_epsilon   =cZERO
 X_drude_term=cZERO
 !
 !...Drude
 call X_Drude(1,Ken,Xk,W_bss,BSS_Wd,drude_GreenF,'c')
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
 if (iq==1) then
   !
   n_SOC=1
   if( E_SOC_nbands/=0) n_SOC=2
   !
   if (trim(global_gauge)=='velocity') A_drude(1)=cZERO
   !
   call DIPOLE_driver(Ken,Xk,X_oscillators,BSS_q0)
   !
   BSS_Vnl_included=X_oscillators%Vnl_included
   !
   local_Z=1._SP
   if (trim(global_gauge)=='length'  ) conjg_fac=-1._SP
   if (trim(global_gauge)=='velocity') conjg_fac=+1._SP
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
       i_sp  = BS_T_grp(i_T_g)%table(i_T,4)
       !
       ik_mem= PAR_Xk_bz_index(ik_bz)
       !
       if (E_SOC_nbands==0) IP_E(1)=BS_T_grp(i_T_g)%E(i_T)
       if (E_SOC_nbands/=0) IP_E(:)=BS_T_grp(i_T_g)%E(i_T)+BS_T_grp(i_T_g)%E_SOC_corr(:,i_T)
       !
       if (allocated(BS_T_grp(i_T_g)%Z)) local_Z=BS_T_grp(i_T_g)%Z(i_T)
       !
       ! *length*   : DIP_projected(c,v,k) is    iq*<v|r|c> while I need    iq*<c|r|v> = -conjg(   iq*<v|r|c>)
       ! *velocity* : DIP_projected(c,v,k) is q_ver*<v|p|c> while I need q_ver*<c|r|v> =  conjg(q_ver*<v|r|c>)
       !              see definition of conjg_fac few lines before for the -1
       !
       BS_T_grp(i_T_g)%dipoles_R(i_T)=conjg_fac*conjg(DIP_projected(ic,iv,ik_mem,i_sp))*sqrt(BS_T_grp(i_T_g)%f(i_T))
       if (BS_K_coupling) BS_T_grp(i_T_g)%dipoles_A(i_T)=DIP_projected(ic,iv,ik_mem,i_sp)*sqrt(BS_T_grp(i_T_g)%f(i_T))
       !
       if ( (abs(IP_E(1))<1.E-5 .or. (abs(IP_E(2))<1.E-5.and.E_SOC_nbands/=0))  .and. l_drude) cycle
       !
       do i_pert_SOC=1,n_SOC
         X_epsilon(3,:)=X_epsilon(3,:)-BS_T_grp(i_T_g)%dipoles_R(i_T)*conjg( BS_T_grp(i_T_g)%dipoles_R(i_T) )*&
&                       local_Z/(W_bss%p(:)-IP_E(i_pert_SOC))
         !
         if(trim(global_gauge)=='length')   A_drude(1)=A_drude(1)+BS_T_grp(i_T_g)%dipoles_R(i_T)* &
&                                             conjg( BS_T_grp(i_T_g)%dipoles_R(i_T) )*real(IP_E(i_pert_SOC))
         if(trim(global_gauge)=='velocity') A_drude(1)=A_drude(1)+BS_T_grp(i_T_g)%dipoles_R(i_T)* &
&                                             conjg( BS_T_grp(i_T_g)%dipoles_R(i_T) )/real(IP_E(i_pert_SOC))
         !
       enddo
       !
       if (BS_anti_res.or.BS_K_coupling) then
         !
         ! Note the plus in "+" coming from the change of sign in the occupation factor
         !
         do i_pert_SOC=1,n_SOC
           X_epsilon(3,:)=X_epsilon(3,:)+conjg(BS_T_grp(i_T_g)%dipoles_R(i_T))*BS_T_grp(i_T_g)%dipoles_R(i_T)*&
&                       local_Z/(W_bss%p(:)+IP_E(i_pert_SOC))
         enddo
         !
       endif
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
   call PP_redux_wait( X_epsilon ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait( X_epsilon ,COMM=PAR_COM_eh_INDEX%COMM )
   !
   if (E_SOC_nbands==0) Co=real(spin_occ)/(2._SP*pi)**3*d3k_factor*4._SP*pi
   if (E_SOC_nbands/=0) Co=    1._SP     /(2._SP*pi)**3*d3k_factor*4._SP*pi
   !
   ! Pure diamagnetic term
   diam_term=cmplx(nel*real(Xk%nbz,SP)/spin_occ,0._SP,SP)
   !
   ! Drude term, Eq.(1.12) PRB 48, 11705 (1993)
   if (BS_anti_res) A_drude(1)=A_drude(1)+conjg(A_drude(1))
   !
   if(trim(global_gauge)=='length'  ) A_drude(1)=A_drude(1)/q_norm(1)**2-diam_term
   if(trim(global_gauge)=='velocity') A_drude(1)=A_drude(1)             -diam_term
   !
   ! Here I impose the conductivity sum rule for the velocity gauge
   if (trim(global_gauge)=='velocity'.and..not.skip_cond_sum_rule) then
     call msg('nsr','Gauge velocity: reference freq. for w=0 is [eV]:',real(W_bss%p(iw_ref))*HA2EV )
     diam_term=X_epsilon(3,iw_ref)
   endif
   !
   if (trim(global_gauge)=='length'  ) X_epsilon(3,:)=1._SP+ X_epsilon(3,:)           *Co/q_norm(1) **2
   if (trim(global_gauge)=='velocity') X_epsilon(3,:)=1._SP+(X_epsilon(3,:)-diam_term)*Co/W_bss%p(:)**2
   !
   ! I add the term describing the intraband transitions which is
   ! missing in the length gauge (this should be zero for systems with a gap)
   if (trim(global_gauge)=='length'.and.skip_cond_sum_rule) X_epsilon(3,:)=X_epsilon(3,:)+A_drude(1)*Co/W_bss%p(:)**2
   !
   if (l_drude.and..not.skip_cond_sum_rule) then
     X_drude_term(:)=-BS_drude_f_eh*drude_GreenF(:)
     X_epsilon(3,:)=X_epsilon(3,:)+X_drude_term(:)*Co/q_norm(1)**2
   endif
   !
#if defined _KERR
   if (l_kerr) call K_kerr_IP(W_bss%p)
#endif
   !
 endif
 !
 ! CLEAN
 !
 if (iq==1) call X_alloc('DIP_projected')
#if defined _KERR
 if (iq==1.and.l_kerr) call KERR_alloc('DIP_projected')
#endif
 !
 if (l_rpa_IP) then
   !  
   X_epsilon(1,:)=W_bss%p(:)
   !
   ! Initialize & write the output file
   !
   call K_output_file(iq,-4)
   call K_output_file(iq, 4)
   !
   deallocate(X_epsilon,X_drude_term)
   !
 endif
 !
1 continue
 !
#if defined _TIMING
 call timing('T_space IP',OPR='stop')
#endif
 !
end subroutine
