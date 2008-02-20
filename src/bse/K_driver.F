!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine K_driver(Xen,Xk,q,X_static,Xw)
 !
 use pars,           ONLY:SP,IP
 use parser_m,       ONLY:parser
 use com,            ONLY:msg
 use memory_m,       ONLY:mem_est
 use stderr,         ONLY:string_remove
 use electrons,      ONLY:levels,E_duplicate,E_reset,n_sp_pol
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_duplicate,X_reset
 use BS,             ONLY:BS_bands,BS_q,BS_blk_dim,BS_eh_table,BS_eh_E,BS_eh_f,&
&                         BS_n_g_exch,BSS_n_freqs,BSS_er,BSS_dr,&
&                         BS_K_coupling,BS_res_K_corr,BS_res_K_exchange,&
&                         BS_W_is_diagonal,BS_res_mode,BSS_mode,BS_n_g_W,&
&                         BS_K_is_ALDA,BS_cpl_mode,BS_cpl_K_corr,BS_cpl_K_exchange,&
&                         BSS_damp_reference,BS_eh_Z,BS_eh_W
 use TDDFT,          ONLY:FXC,FXC_per_memstps,FXC_n_mem_freqs,&
&                         FXC_is_causal
 use QP_CTL,         ONLY:QP_apply
 use drivers,        ONLY:l_bss,l_bs_fxc,l_alda_fxc
 use par_proc_m,     ONLY:pp_redux_wait
 use R_lattice,      ONLY:bz_samp,qindx_B
 use parser_m,       ONLY:parser
#if defined _SC
 use IO_m,           ONLY:NONE,DUMP
 use SC,             ONLY:load_SC_components
 use global_XC,      ONLY:setup_global_XC,MORE_NONE,K_kind,K_xc_functional
#endif
 implicit none
 type(levels)  ::Xen
 type(bz_samp) ::Xk,q
 type(X_t)     ::X_static
 type(w_samp)  ::Xw,W_bss 
 !
 ! Work Space
 !
 integer     :: iq
 type(levels):: Ken
 type(X_t)   :: X_tddft
 integer, external :: eval_G_minus_G
 !
 call E_reset(Ken)
 call X_reset(X_tddft)
 call W_reset(W_bss)
 !
 ! As I do not want to transfer to Ken any QP correction
 ! embodied in Xen I define
 !
 Ken%nk=Xen%nk
 !
 call E_duplicate(Xen,Ken)
#if defined _SC
 !
 ! Load the SC energies
 !
 call load_SC_components('E',E=Ken,COM_=NONE,MODE_=DUMP,ik=0,&
&                         kind=K_kind,xc_functional=K_xc_functional)
 call setup_global_XC('K',K_kind,MORE_NONE,K_xc_functional)
#endif
 !
 ! In these two lines I ensure the at least all the metallic
 ! bands are included in the X summation
 !
 if (BS_bands(1)>Ken%nbf+1) BS_bands(1)=Ken%nbf+1
 if (BS_bands(2)<Ken%nbm)   BS_bands(2)=Ken%nbm
 !
 ! If using ALDA enough bands must be loaded to
 ! be able to calculate the density
 !
 if (l_alda_fxc) BS_bands(1)=1
 !
 call QP_apply(BS_bands,Ken,Xk,2,msg_fmt='s')
 !
 ! If the em1s DB has been calculated in the same run
 ! of a BS-Fxc kernel construction that l_bs_fxc is set .FALSE.
 ! in tddft_do_X_W_typs. So here we need to check again if
 ! l_bs_fxc has been provided
 !
 if (.not.l_bs_fxc) call parser('bs_fxc',l_bs_fxc)
 !
 ! W RL size setup looking at the max index of G-G' 
 !
 if (BS_n_g_W/= eval_G_minus_G(BS_n_g_W,maxval(qindx_B(:,:,2))) ) then
   call msg('s','[BSE-W] W size forced by symmetry to be :',BS_n_g_W)
 endif
 !
 if (l_bs_fxc.or.index(BSS_mode,'t')/=0) then
   !  
   ! Remove the exchange from the BS_mode
   ! if TDDFT Fxc is obtained from K
   !
   if (index(BSS_mode,'t')==0) BS_res_mode=trim( string_remove(BS_res_mode,'x') )
   BS_cpl_mode=" "
   call parser('FxcCausal',FXC_is_causal)
   !
 else if (l_alda_fxc) then
   !  
   ! Remove the correlation from the BS_mode
   !
   BS_res_mode=trim( string_remove(BS_res_mode,'c') )
   BS_cpl_mode=trim( string_remove(BS_cpl_mode,'c') )
   !
 endif
 !
 ! correlation still unsupported in coupling
 !
 BS_cpl_mode=trim( string_remove(BS_cpl_mode,'c') )
 !
 ! Logicals (Drivers)
 !
 BS_res_K_corr=index(BS_res_mode,'c')/=0
 BS_res_K_exchange=index(BS_res_mode,'x')/=0
 BS_cpl_K_corr=index(BS_cpl_mode,'c')/=0
 BS_cpl_K_exchange=index(BS_cpl_mode,'x')/=0
 BS_K_coupling=any((/BS_cpl_K_corr,BS_cpl_K_exchange/)) 
 BS_W_is_diagonal=index(BS_res_mode,'d')/=0
 BS_K_is_ALDA=l_alda_fxc
 !
 ! Energy points
 !
 W_bss%n=BSS_n_freqs
 W_bss%er=BSS_er
 W_bss%dr=BSS_dr
 W_bss%damp_reference=BSS_damp_reference
 !
 ! Note that %n(2) is not used in standard BSE solvers.
 ! Only when we are using the BS derived FXC FXC_per_memstps
 ! defines %n(2) and the memory allocation of the kernel
 !
 W_bss%per_memstps=FXC_per_memstps
 call freqs_setup(W_bss)
 FXC_n_mem_freqs=W_bss%n(2)
 !
 ! KERNEL
 !
 iq=BS_q(1)
 !
 call K(iq,Ken,Xk,q,X_static,Xw,W_bss)
 !
 ! SOLVER
 !
 if (l_bss) call K_solvers(iq,Ken,Xk,q,X_static,W_bss)
 !
 ! `TDDFT` SOLVER
 !
 if (l_bs_fxc) then
   !
   call X_duplicate(X_static,X_tddft) ! initialize X_tddft to X_static
   !
   X_tddft%ordering='r'
   if (FXC_is_causal) X_tddft%ordering='c'
   !
   ! If the kernel contains QP corrections I have to equally correct Xen
   !
   if (associated(Ken%qp_done)) then
     allocate(Xen%qp_done(Xen%nb,Xen%nk),Xen%Eo(Xen%nb,Xen%nk,n_sp_pol))
     call mem_est("E-qp_done E-Eo",(/size(Xen%qp_done),size(Xen%Eo)/),(/IP,SP/))
     Xen%qp_done=Ken%qp_done
     Xen%Eo=Xen%E
     Xen%E =Ken%E
   endif
   !
   call pp_redux_wait
   X_tddft%iq=iq
   X_tddft%ng=BS_n_g_exch
   call O_driver(Xen,Xk,q,W_bss,X_tddft) 
   !
 endif
 !
 ! CLEAN
 !
 deallocate(BS_blk_dim,BS_eh_table,BS_eh_E,BS_eh_f)
 if (allocated(BS_eh_W)) deallocate(BS_eh_W)
 if (allocated(BS_eh_Z)) deallocate(BS_eh_Z)
 call mem_est("BS_blk_dim BS_eh_table BS_eh_E BS_eh_f BS_eh_W BS_eh_Z")
 !
end subroutine
