!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine K_solvers(iq,Ken,Xk,q,X_static,W_bss)
 !
 use pars,           ONLY:SP,pi,schlen
 use memory_m,       ONLY:mem_est
 use X_m,            ONLY:X_t,X_epsilon,X_alloc,DIP_q_dot_iR,&
&                         alpha_dim,eps_2_alpha,X_duplicate,X_drude_term
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BSS_rhoq0,BS_eh_f,BS_not_const_eh_f,&
&                         BS_bands,BSS_q0,BS_eh_table,BSS_mode,BS_eh_E,&
&                         BSS_n_descs,BS_K_coupling,BSS_Vnl_included,BS_eh_W,&
&                         BS_drude_f_eh,BS_IP_antires
#if defined _DISTRIBUTED
 use BS,             ONLY:ioBS_DISTRIBUTED
#endif
 use com,            ONLY:msg,warning
 use drivers,        ONLY:l_bs_fxc
#if defined _KERR
 use units,          ONLY:HA2EV
 use com,            ONLY:error
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:DIP_P_symm,KERR_alloc,iw_ref
#endif
 use frequency,      ONLY:w_samp
 use electrons,      ONLY:levels,spin_occ,spin
 use R_lattice,      ONLY:d3k_factor,bz_samp,q_norm
 use IO_m,           ONLY:io_control,OP_RD_CL,NONE
#if defined _SC || defined _KERR
 use fields,         ONLY:global_gauge
#endif
 !
 implicit none
 type(levels) ::Ken
 type(bz_samp)::Xk,q
 type(X_t)    ::X_static
 type(w_samp) ::W_bss
 integer      ::iq
 !
 ! Work Space
 !
 integer           ::ik,iv,ic,i1,i_sp,ioBS_err,ID, epsilon_dim
#if defined _KERR
 integer           ::iw
 complex(SP)       ::factor_jj
#endif
 real(SP)          ::factor
 complex(SP)       ::drude_GreenF(W_bss%n(2))
 real(SP)          ::v(3)
 logical           ::run_Haydock,run_Diago,Pert_inversion,BSE_IP
 integer, external ::ioBS
 type(X_t)         ::X_oscillators
 character(schlen) ::ch
 !
 call section('*','BSE solver(s)')
 !
 if(.not.trim(BSS_mode)=='none') then
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)
#if defined _DISTRIBUTED
   ioBS_err=ioBS_DISTRIBUTED(iq,X_static,ID)
#else
   ioBS_err=ioBS(iq,X_static,ID)
#endif
   if (ioBS_err/=0) return
 endif
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
 if(l_kerr) epsilon_dim=10
#endif
 allocate(X_epsilon(epsilon_dim,BSS_n_freqs))
 !
 allocate(X_drude_term(BSS_n_freqs))
 X_epsilon=cmplx(0.,0.,SP)
 X_drude_term=cmplx(0.,0.,SP)
 !
 call X_duplicate(X_static,X_oscillators) 
 call Drude(1,(/1,BSS_n_freqs/),Ken,Xk,W_bss,X_static,drude_GreenF)
 !
 X_oscillators%ib=BS_bands
 if (BS_K_coupling.or.allocated(BS_eh_W).or.BS_IP_antires) then
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
     if (BS_K_coupling.or.BS_IP_antires) then
       if(trim(global_gauge)=='length')   BSS_rhoq0(BS_K_dim+i1)=DIP_q_dot_iR(ic,iv,ik,i_sp)
       if(trim(global_gauge)=='velocity') BSS_rhoq0(BS_K_dim+i1)=DIP_P_symm(1,ic,iv,ik,i_sp)
     endif
#else
     if (BS_K_coupling.or.BS_IP_antires) BSS_rhoq0(BS_K_dim+i1)=DIP_q_dot_iR(ic,iv,ik,i_sp)
#endif
     !
     X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*BS_eh_f(i1)/&
&                                  (W_bss%p(:)-BS_eh_E(i1))
     !
     if (BS_K_coupling.or.BS_IP_antires) then
       !
       ! Note the plus in "+BSS_rhoq0" coming from the BS_eh_f change of sign
       !
       X_epsilon(3,:)=X_epsilon(3,:)+conjg(BSS_rhoq0(i1))*BSS_rhoq0(i1)*BS_eh_f(i1)/&
&                                    (W_bss%p(:)+BS_eh_E(i1))
     endif
     !
   enddo
   !
   factor=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi
   !
#if defined _KERR
   if(trim(global_gauge)=='velocity') then
     if(all(abs(real(W_bss%p(:)))>0.01))&
&      call error(' No w close to 0 included in your frequency grid')
     iw_ref=1
     do iw=1,BSS_n_freqs
       if(abs(real(W_bss%p(iw)))>0.01) cycle
       if(abs(W_bss%p(iw))<abs(W_bss%p(iw_ref))) iw_ref=iw
     enddo
     call msg('nsr','Reference freq. for w=0 is:',real(W_bss%p(iw_ref))*HA2EV )
     factor_jj=X_epsilon(3,iw_ref)
   endif
   !
   if(trim(global_gauge)=='length') X_epsilon(3,:)=1.+X_epsilon(3,:)*factor/q_norm(1)**2
   if(trim(global_gauge)=='velocity') then
     X_epsilon(3,:)=1.+(X_epsilon(3,:)-factor_jj)*factor/(real(W_bss%p(:))**2+epsilon(1.))
   endif
   !
   ! Causal Drude term
   !
   X_drude_term(:)=-BS_drude_f_eh*drude_GreenF(:)
   do iw=1,iw_ref-1
     X_drude_term(iw_ref-iw)=cmplx(real(X_drude_term(iw_ref-iw)),-aimag(X_drude_term(iw_ref+iw)))
   enddo
   X_epsilon(3,:)=X_epsilon(3,:)+X_drude_term(:)*factor/q_norm(1)**2
   !
   if(l_kerr) call K_kerr_IP(W_bss%p)
#else
   !
   X_epsilon(3,:)=1.+X_epsilon(3,:)*factor/(q_norm(1))**2
#endif
 !
 endif
 !
 ! GPL_EXCLUDE_START
 !
 ! The TDDFT way 
 !
 if (index(BSS_mode,'t')/=0) then
   call tddft_BSK_disk_2_FXC(iq,Xk,W_bss,X_static) 
   l_bs_fxc=.true.
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Solvers
 !
 run_Diago           =index(BSS_mode,'d')/=0
#if defined _SC
 run_Haydock         =index(BSS_mode,'h')/=0.and.trim(global_gauge)/='velocity'
#else
 run_Haydock         =index(BSS_mode,'h')/=0
#endif
 !
#if defined _SC
 Pert_inversion      =index(BSS_mode,'i')/=0.and.trim(global_gauge)/='velocity'
#else
 Pert_inversion      =index(BSS_mode,'i')/=0
#endif
 !
 BSE_IP              =trim(BSS_mode)=='none'
 !
 if (run_Haydock.and.associated(Ken%W)) then
   call warning('K with QP widths not supported in the Haydock solver')
   run_Haydock=.false.
 endif
 !
 if (run_Haydock) then
   !
   if (BS_not_const_eh_f) call warning('Fractional occupations still unsupported (Haydock)')
   !
   if (.not.BS_not_const_eh_f) then
     call K_output_file(iq,-1)
     call K_Haydock(iq,W_bss)
     BSS_n_descs=BSS_n_descs-4 
   endif
 endif
 !
 if (run_Diago)          call K_diagonalization(iq,W_bss)
 !
 if (Pert_inversion)     call K_inversion(iq,W_bss,Ken,Xk,q)
 !
 if (BSE_IP) then
   !  
   X_epsilon(1,:)=W_bss%p(:)
#if defined _KERR
   X_epsilon(5,:)=0.
#endif
   X_epsilon(2,:)=0.
   !
   ! Initialize & write the output file
   !
   call K_output_file(iq,-4)
   call K_output_file(iq,4)
   !
 endif
 !
 ! CLEAN
 !
 deallocate(BSS_rhoq0,X_epsilon,X_drude_term)
 call mem_est("BSS_rhoq0")
 if (iq==1) call X_alloc('DIP_q_dot_iR')
#if defined _KERR
 if (iq==1) call KERR_alloc('DIP_P_symm')
#endif
 !
end subroutine
