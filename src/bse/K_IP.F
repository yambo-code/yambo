!
!        Copyright (C) 2000-2014 the YAMBO team
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
 use pars,          ONLY:SP,IP,pi
 use units,         ONLY:HA2EV
 use frequency,     ONLY:w_samp
 use LIVE_t,        ONLY:live_timing
 use drivers,       ONLY:l_rpa_IP,l_bs_fxc
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp,d3k_factor,q_norm
 use electrons,     ONLY:levels,spin_occ,spin
 use interfaces,    ONLY:PARALLEL_WF_distribute
 use X_m,           ONLY:X_t,X_epsilon,X_drude_term,alpha_dim,eps_2_alpha,&
&                        DIP_q_dot_iR,X_drude_term,X_alloc,iw_ref
 use BS,            ONLY:BSS_Vnl_included,BS_anti_res,BS_drude_f_eh,&
&                        BS_K_dim,BSS_n_freqs,BSS_q0,BS_T_grp,BS_K_coupling,BS_nT_grps
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                        PAR_IND_Xk_ibz,PAR_IND_CON_BANDS_X,PAR_IND_VAL_BANDS_X
#if defined _KERR
 use drivers,       ONLY:l_kerr
 use com,           ONLY:warning
 use fields,        ONLY:global_gauge
 use KERR,          ONLY:DIP_P_symm,KERR_alloc
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
 integer           ::ik,iv,ic,i_sp,epsilon_dim,iw,i_T_g,i_T
 real(SP)          ::Co
 complex(SP)       ::drude_GreenF(W_bss%n_freqs),local_Z
#if defined _KERR
 complex(SP)       ::factor_jj
#endif
 !
 call section('=','Independent Particles absorption') 
 !===================================================
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
#if defined _KERR
 !===============
 ! Check if it is possible to use the velocity gauge
 !                                    ==============
 if(all(abs(real(W_bss%p(:)))>0.01)) then
   !
   call warning(' No w close to 0 in the frequency grid. Length gauge imposed')
   global_gauge='length'
   !
 endif
#endif
 !
 ! Look for the W(iw_ref) closest 0
 !==================================
 iw_ref=1
 !
 if(any(abs(real(W_bss%p(:)))<0.01)) then 
   !
   do iw=1,BSS_n_freqs
     if(abs(real(W_bss%p(iw)))>0.01) cycle
     if(abs(W_bss%p(iw))<abs(W_bss%p(iw_ref))) iw_ref=iw
   enddo 
   !
 endif
 !
 if (l_bs_fxc) return
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
 X_epsilon=cmplx(0.,0.,SP)
 X_drude_term=cmplx(0.,0.,SP)
 !
 !...Drude
 call X_Drude(1,Ken,Xk,W_bss,X_oscillators,drude_GreenF,'c')
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
   call Dipole_driver(Ken,Xk,X_oscillators,BSS_q0)
   !
   BSS_Vnl_included=X_oscillators%Vnl_included
   !
   if (l_rpa_IP) call live_timing('IP absorption',PAR_BS_nT_col_grps)
   !
   do i_T_g=1,BS_nT_grps
     !    
     if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
     !    
     do i_T=1,BS_T_grp(i_T_g)%size    
       !    
       ik    = BS_T_grp(i_T_g)%table(i_T,1)
       iv    = BS_T_grp(i_T_g)%table(i_T,2)
       ic    = BS_T_grp(i_T_g)%table(i_T,3)
       i_sp  = BS_T_grp(i_T_g)%table(i_T,4)
       !
       local_Z=1.
       if (allocated(BS_T_grp(i_T_g)%Z)) local_Z=BS_T_grp(i_T_g)%Z(i_T)
       !
       ! DIP_iq_dot_r(c,v,k) is iq . <v | r |c> while I need 
       !
       !   iq . <c|r|v> = - conjg( iq . <v | r |c> )
       !
#if defined _KERR
       if (trim(global_gauge)=='length') then
         BS_T_grp(i_T_g)%dipoles_R(i_T)=-conjg(DIP_q_dot_iR(ic,iv,ik,i_sp))
       else if (trim(global_gauge)=='velocity') then
         BS_T_grp(i_T_g)%dipoles_R(i_T)= conjg(DIP_P_symm(1,ic,iv,ik,i_sp))
       endif
       BS_T_grp(i_T_g)%dipoles_R(i_T)=BS_T_grp(i_T_g)%dipoles_R(i_T)*sqrt(BS_T_grp(i_T_g)%f(i_T))
#else
       BS_T_grp(i_T_g)%dipoles_R(i_T)=-conjg(DIP_q_dot_iR(ic,iv,ik,i_sp))*sqrt(BS_T_grp(i_T_g)%f(i_T))
#endif
       !
       ! minus comes from the occupation factor 
       !
       if (BS_K_coupling) then
#if defined _KERR
         if (trim(global_gauge)=='length') then
           BS_T_grp(i_T_g)%dipoles_A(i_T)=DIP_q_dot_iR(ic,iv,ik,i_sp)
         else if (trim(global_gauge)=='velocity') then
           BS_T_grp(i_T_g)%dipoles_A(i_T)=DIP_P_symm(1,ic,iv,ik,i_sp)
         endif
         BS_T_grp(i_T_g)%dipoles_A(i_T)=BS_T_grp(i_T_g)%dipoles_A(i_T)*sqrt(BS_T_grp(i_T_g)%f(i_T))
#else
         BS_T_grp(i_T_g)%dipoles_A(i_T)=DIP_q_dot_iR(ic,iv,ik,i_sp)*sqrt(BS_T_grp(i_T_g)%f(i_T))
#endif
       endif
       !    
       X_epsilon(3,:)=X_epsilon(3,:)-BS_T_grp(i_T_g)%dipoles_R(i_T)*conjg( BS_T_grp(i_T_g)%dipoles_R(i_T) )*&
&                     local_Z/(W_bss%p(:)-BS_T_grp(i_T_g)%E(i_T))
       !
       if (BS_anti_res) then
         !
         ! Note the plus in "+BSS_rhoq0" coming from the BS_eh_f change of sign
         !
         X_epsilon(3,:)=X_epsilon(3,:)+conjg(BS_T_grp(i_T_g)%dipoles_R(i_T))*BS_T_grp(i_T_g)%dipoles_R(i_T)*&
&                       local_Z/(W_bss%p(:)+BS_T_grp(i_T_g)%E(i_T))
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
   Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi
   !
#if defined _KERR
   if (trim(global_gauge)=='velocity') then
     call msg('nsr','Gauge velocity: reference freq. for w=0 is [eV]:',real(W_bss%p(iw_ref))*HA2EV )
     factor_jj=X_epsilon(3,iw_ref)
   endif
   !
   if (trim(global_gauge)=='length')   X_epsilon(3,:)=1.+X_epsilon(3,:)*Co/q_norm(1)**2
   if (trim(global_gauge)=='velocity') X_epsilon(3,:)=1.+(X_epsilon(3,:)-factor_jj)*Co/&
&                                                       (real(W_bss%p(:))**2+epsilon(1.))
   !
   if (l_kerr) call K_kerr_IP(W_bss%p)
#else
   !
   X_epsilon(3,:)=1.+X_epsilon(3,:)*Co/(q_norm(1))**2
#endif
   !
   X_drude_term(:)=-BS_drude_f_eh*drude_GreenF(:)
   X_epsilon(3,:)=X_epsilon(3,:)+X_drude_term(:)*Co/q_norm(1)**2
   !
 endif
 !
 ! CLEAN
 !
 if (iq==1) call X_alloc('DIP_q_dot_iR')
#if defined _KERR
 if (iq==1) call KERR_alloc('DIP_P_symm')
#endif
 !
 ! ALL 2 ALL
 !
 call PP_redux_wait( X_epsilon ,COMM=PAR_COM_eh_INDEX%COMM )
 !
 if (l_rpa_IP) then
   !  
   X_epsilon(1,:)=W_bss%p(:)
   !
   ! Initialize & write the output file
   !
   call K_output_file(iq,-4)
   call K_output_file(iq,4)
   !
   deallocate(X_epsilon,X_drude_term)
   !
 endif
 !
end subroutine
