!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
&                         alpha_dim,eps_2_alpha,X_duplicate
 use BS,             ONLY:BSS_n_freqs,BS_K_dim,BSS_rhoq0,BS_eh_f,BS_not_const_eh_f,&
&                         BS_bands,BSS_q0,BS_eh_table,BSS_mode,BS_eh_E,&
&                         BSS_n_descs,BS_K_coupling,BSS_Vnl_included
 use com,            ONLY:msg,warning
 use drivers,        ONLY:l_bs_fxc
 use frequency,      ONLY:w_samp
 use electrons,      ONLY:levels,spin_occ,spin
 use R_lattice,      ONLY:d3k_factor,bz_samp,q_norm
 use IO_m,           ONLY:io_control,OP_RD_CL,NONE
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
 integer           ::ik,iv,ic,i1,i_sp,ioBS_err,ID 
 real(SP)          ::v(3)
 logical           ::run_Haydock,run_Diago,Pert_inversion
 integer, external ::ioBS
 type(X_t)         ::X_oscillators
 character(schlen) ::ch
 !
 call section('*','BSE solver(s)')
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),ID=ID)
 ioBS_err=ioBS(iq,X_static,ID)
 if (ioBS_err/=0) return
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
 allocate(X_epsilon(4,BSS_n_freqs))
 X_epsilon=(0.,0.)
 !
 call X_duplicate(X_static,X_oscillators) 
 !
 X_oscillators%ib=BS_bands
 if (BS_K_coupling) then
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
     BSS_rhoq0(i1)=-conjg(DIP_q_dot_iR(ic,iv,ik,i_sp))
     !
     ! minus comes from the occupation factor 
     !
     if (BS_K_coupling) BSS_rhoq0(BS_K_dim+i1)=DIP_q_dot_iR(ic,iv,ik,i_sp)
     !
     X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*BS_eh_f(i1)/&
&                                  (W_bss%p(:)-BS_eh_E(i1))
     if (BS_K_coupling) then
       !
       ! Note the plus in "+BSS_rhoq0" coming from the BS_eh_f change of sign
       !
       X_epsilon(3,:)=X_epsilon(3,:)+BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*BS_eh_f(i1)/&
&                                    (W_bss%p(:)+BS_eh_E(i1))
     endif
   enddo
   !
   X_epsilon(3,:)=1.+X_epsilon(3,:)*real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/(q_norm(1))**2
   !
 endif
 !
 !
 ! Solvers
 !
 run_Diago           =index(BSS_mode,'d')/=0
 run_Haydock         =index(BSS_mode,'h')/=0
 !
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
 !
 ! CLEAN
 !
 deallocate(BSS_rhoq0,X_epsilon)
 call mem_est("BSS_rhoq0")
 if (iq==1) call X_alloc('DIP_q_dot_iR')
 !
end subroutine
