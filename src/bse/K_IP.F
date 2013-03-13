!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine K_IP(iq,Ken,Xk,X_static,W_bss)
 !
 use pars,          ONLY:SP,IP,pi
 use units,         ONLY:HA2EV
 use LOGO,          ONLY:pickup_a_random
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp
 use timing,        ONLY:live_timing
 use drivers,       ONLY:l_rpa_IP
 use com,           ONLY:error,msg
 use R_lattice,     ONLY:bz_samp,d3k_factor,q_norm
 use electrons,     ONLY:levels,spin_occ,spin,n_sp_pol
 use X_m,           ONLY:X_t,X_epsilon,X_drude_term,alpha_dim,eps_2_alpha,           &
&                        DIP_q_dot_iR,X_duplicate,X_drude_term,X_alloc,iw_ref
 use BS,            ONLY:BS_columns,BS_identifier,BS_blk_dim,BS_k_and_row_restart,   &
&                        BSS_rhoq0,BS_eh_f,BS_eh_E,BS_eh_table,BSS_Vnl_included,     &
&                        BS_bands,BS_eh_W,BS_anti_res,BS_drude_f_eh,BS_K_coupling, &
&                        BS_K_dim,BSS_n_freqs,BSS_q0,BS_res_K_corr,O_n_c_states,     &
&                        O_n_v_states
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
 type(X_t)    ::X_static
 type(w_samp) ::W_bss
 integer      ::iq
 !
 ! Work space
 !
 type(X_t)         ::X_oscillators
 integer           ::ik,iv,ic,i1,i_sp,ioBS_err,ID, epsilon_dim
 real(SP)          ::Co
 complex(SP)       ::drude_GreenF(W_bss%n(2))
 integer           ::iw
#if defined _KERR
 complex(SP)       ::factor_jj
#endif
 !
 ! Setups
 !
 if (BS_columns<=0.or.BS_columns>Xk%nibz) BS_columns=Xk%nibz
 BS_k_and_row_restart=0
 !
 ! Dimensions and Tables
 ! 
 allocate(BS_blk_dim(Xk%nibz))
 call mem_est("BS_blk_dim",(/Xk%nbz/),(/IP/))
 !
 if (BS_res_K_corr) then
   !
   allocate(O_n_c_states(Xk%nbz,n_sp_pol),O_n_v_states(Xk%nbz,n_sp_pol))
   call mem_est("O_n_c_states O_n_v_states",&
&               (/Xk%nbz,n_sp_pol,Xk%nbz,n_sp_pol/),(/IP,IP,IP,IP/))
   !
 endif
 !
#if defined _KERR
 !
 ! Check if it is possible to use the velocity gauge
 if(all(abs(real(W_bss%p(:)))>0.01)) then
   call warning(' No w close to 0 in the frequency grid. Length gauge imposed')
   global_gauge='length'
 endif
#endif
 !
 ! Look for the W(iw_ref) closest 0
 iw_ref=1
 if(any(abs(real(W_bss%p(:)))<0.01)) then
   do iw=1,BSS_n_freqs
     if(abs(real(W_bss%p(iw)))>0.01) cycle
     if(abs(W_bss%p(iw))<abs(W_bss%p(iw_ref))) iw_ref=iw
   enddo
   call msg('nsr','Reference freq. for w=0 is:',real(W_bss%p(iw_ref))*HA2EV )
 endif
 !
 call K_eh_setup(iq,Ken,Xk,X_static%Wd)
 if (any(BS_blk_dim==0)) then
   call error(' Null BSE kernel block dimension(s) found. Increase e/h range')
 endif
 !
 ! DB identifier 
 !
 BS_identifier=pickup_a_random(10000._SP)
 !
 !
 ! Polarizability ?
 !
 if (trim(alpha_dim)/='adim') then
   call msg('r', 'Optical renormalization   [au]:',eps_2_alpha)
   call msg('rn','Polarizability dimension      :',trim(alpha_dim))
 endif
 !
 ! Eps_0
 !
 epsilon_dim=4
#if defined _KERR
 if(l_kerr) epsilon_dim=7
#endif
 allocate(X_epsilon(epsilon_dim,BSS_n_freqs))
 !
 allocate(X_drude_term(BSS_n_freqs))
 X_epsilon=cmplx(0.,0.,SP)
 X_drude_term=cmplx(0.,0.,SP)
 !
 call X_duplicate(X_static,X_oscillators) 
 call Drude(1,(/1,BSS_n_freqs/),Ken,Xk,W_bss,X_static,drude_GreenF,'c')
 !
 X_oscillators%ib=BS_bands
 if (BS_K_coupling.or.allocated(BS_eh_W)) then
   allocate(BSS_rhoq0(2*BS_K_dim)) 
   call mem_est("BSS_rhoq0",(/2*BS_K_dim/))
 else
   allocate(BSS_rhoq0(BS_K_dim)) 
   call mem_est("BSS_rhoq0",(/BS_K_dim/))
 endif
 if (iq==1) then
   X_oscillators%q0=BSS_q0
   call Dipole_driver(Ken,Xk,X_oscillators,BSS_q0)
   BSS_Vnl_included=X_oscillators%Vnl_included
   if(l_rpa_IP) call live_timing('Eps0',BS_K_dim)
   do i1=1,BS_K_dim
     ik  =BS_eh_table(i1,1)
     iv  =BS_eh_table(i1,2)
     ic  =BS_eh_table(i1,3)
     i_sp=spin(BS_eh_table(i1,:))
     !
     ! DIP_iq_dot_r(c,v,k) is iq . <v | r |c> while I need 
     !
     !   iq . <c|r|v> = - conjg( iq . <v | r |c> )
     !
#if defined _KERR
     if(trim(global_gauge)=='length')   BSS_rhoq0(i1)=-conjg(DIP_q_dot_iR(ic,iv,ik,i_sp))
     if(trim(global_gauge)=='velocity') BSS_rhoq0(i1)= conjg(DIP_P_symm(1,ic,iv,ik,i_sp))
#else
     BSS_rhoq0(i1)=-conjg(DIP_q_dot_iR(ic,iv,ik,i_sp))
#endif
     !
     ! minus comes from the occupation factor 
     !
#if defined _KERR
     if (BS_K_coupling) then
       if(trim(global_gauge)=='length')   BSS_rhoq0(BS_K_dim+i1)=DIP_q_dot_iR(ic,iv,ik,i_sp)
       if(trim(global_gauge)=='velocity') BSS_rhoq0(BS_K_dim+i1)=DIP_P_symm(1,ic,iv,ik,i_sp)
     endif
#else
     if (BS_K_coupling) BSS_rhoq0(BS_K_dim+i1)=DIP_q_dot_iR(ic,iv,ik,i_sp)
#endif
     !
     X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*BS_eh_f(i1)/&
&                                  (W_bss%p(:)-BS_eh_E(i1))
     !
     if (BS_anti_res) then
       !
       ! Note the plus in "+BSS_rhoq0" coming from the BS_eh_f change of sign
       !
       X_epsilon(3,:)=X_epsilon(3,:)+conjg(BSS_rhoq0(i1))*BSS_rhoq0(i1)*BS_eh_f(i1)/&
&                                    (W_bss%p(:)+BS_eh_E(i1))
     endif
     !
     if(l_rpa_IP) call live_timing(steps=1)
     !
   enddo
   !
   if(l_rpa_IP) call live_timing()
   !
   Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi
   !
#if defined _KERR
   if(trim(global_gauge)=='velocity') factor_jj=X_epsilon(3,iw_ref)
   !
   if(trim(global_gauge)=='length') X_epsilon(3,:)=1.+X_epsilon(3,:)*Co/q_norm(1)**2
   if(trim(global_gauge)=='velocity')&
&     X_epsilon(3,:)=1.+(X_epsilon(3,:)-factor_jj)*Co/(real(W_bss%p(:))**2+epsilon(1.))
   !
   if(l_kerr) call K_kerr_IP(W_bss%p)
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
end subroutine
