!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine K_driver(Xen,Ken,Xk,q,X_static_pp,Xw)
 !
 use pars,           ONLY:SP,IP
 use units,          ONLY:HA2EV
 use parser_m,       ONLY:parser
 use com,            ONLY:msg,warning
 use memory_m,       ONLY:mem_est
 use stderr,         ONLY:string_remove,string_add,intc,real2ch
 use electrons,      ONLY:levels,n_sp_pol
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_duplicate,X_reset
 use BS,             ONLY:BS_bands,BS_q,BS_blk_dim,BS_eh_table,BS_eh_E,BS_eh_f,&
&                         BS_n_g_exch,BSS_n_freqs,BSS_er,BSS_dr,BSS_rhoq0,&
&                         BS_K_coupling,BS_res_K_corr,BS_res_K_exchange,&
&                         BS_W_is_diagonal,BS_res_mode,BSS_mode,BS_n_g_W,BS_anti_res,&
&                         BS_K_is_ALDA,BS_cpl_mode,BS_cpl_K_corr,BS_cpl_K_exchange,&
&                         BSS_damp_reference,BS_eh_Z,BS_eh_W,BS_columns,BS_eh_en,BSE_mode
 use TDDFT,          ONLY:FXC,FXC_per_memstps,FXC_n_mem_freqs,&
&                         FXC_is_causal
 use QP_CTL_m,       ONLY:QP_apply
 use drivers,        ONLY:l_rpa_IP,l_alda_fxc,l_bs_fxc,l_bss
 use parallel_m,     ONLY:PP_redux_wait
 use R_lattice,      ONLY:bz_samp,qindx_B
 use parser_m,       ONLY:parser
#if defined _SC
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate
 use QP_m,           ONLY:OCC_T_ref
 use IO_m,           ONLY:REP
#endif
#if defined _KERR
 use drivers,        ONLY:l_kerr,l_anomalous_Hall
#endif
 implicit none
 type(levels)  ::Xen,Ken
 type(bz_samp) ::Xk,q
 type(X_t)     ::X_static_pp
 type(w_samp)  ::Xw,W_bss 
 !
 ! Work Space
 !
 type(X_t)   :: X_tddft
 integer     :: iq,bands_to_correct(2)
 integer     :: Max_G_m_G,ik
 integer, external :: eval_G_minus_G
 !
#if defined _SC
 !
 ! The NE occupations globally redefine the metallic/full bands. So they must
 ! be loaded first.
 !
 call OCCUPATIONS_IO_and_interpolate(Ken,'K',k=Xk,Time=OCC_T_ref,imposed_COM=REP)
#endif
 !
 call X_reset(X_tddft)
 call W_reset(W_bss)
 ! TDDFT setup
 call tddft_do_X_W_typs(-1,X_tddft,Xw)
 !
 ! In these two lines I ensure the at least all the metallic
 ! states are included in the X summation
 !
 if (any((/BS_bands(1)>Ken%nbf+1,BS_bands(2)<Ken%nbm/))) then
   BS_bands=(/Ken%nbf+1,Ken%nbm/)
   BS_eh_en=(/1.,-1./)
   do ik=1,Xk%nibz
     if (Ken%f(Ken%nbm,ik,1)  >1.E-5) BS_eh_en(2)=max(BS_eh_en(2),Ken%E(Ken%nbm,ik,1))
     if (Ken%f(Ken%nbf+1,ik,1)>1.E-5) BS_eh_en(1)=min(BS_eh_en(1),Ken%E(Ken%nbf,ik,1))
   enddo
   call warning('BS bands redefined to be :'//trim(intc(BS_bands(1)))//' - '//trim(intc(BS_bands(2))))
   call warning('E/h energy range :'//trim(real2ch(BS_eh_en(1)*HA2EV))//' - '//trim(real2ch(BS_eh_en(2)*HA2EV))//' eV')
 endif
 !
 ! If using ALDA enough bands must be loaded to
 ! be able to calculate the density
 !
 bands_to_correct=BS_bands
 if (l_alda_fxc) bands_to_correct=(/1,BS_bands(2)/)
 !
 ! ... and the QP corrections
 !
 call QP_apply(bands_to_correct,Ken,Xk,"K",msg_fmt='s')
 !
 ! GPL_EXCLUDE_START
 !
 ! If the em1s DB has been calculated in the same run
 ! of a BS-Fxc kernel construction that l_bs_fxc is set .FALSE.
 ! in tddft_do_X_W_typs. So here we need to check again if
 ! l_bs_fxc has been provided
 !
 if (.not.l_bs_fxc) call parser('bs_fxc',l_bs_fxc)
 !
 ! GPL_EXCLUDE_END
 !
 ! W RL size setup looking at the max index of G-G' 
 !
 Max_G_m_G=eval_G_minus_G(BS_n_g_W,maxval(qindx_B(:,:,2)))
 if (BS_n_g_W/= Max_G_m_G ) then
   BS_n_g_W= Max_G_m_G
   call warning (' W size reduced due to symmetry constraints')
   call msg('s','[BSE-W] W size forced by symmetry to be :',BS_n_g_W)
 endif
 !
#if defined _KERR
 call parser('EvalKerr',l_kerr)
 if(l_kerr) call parser('AnHall',l_anomalous_Hall)
#endif
 !
 if(l_rpa_IP) then
   l_bss=.true.
   BSS_mode=''
   if(trim(BSE_mode)=='coupling') BSE_mode='causal'
 endif
 !
 if( any((/index(BSS_mode,'h')/=0,index(BSS_mode,'i')/=0/)) &
&    .and. trim(BSE_mode)=='causal') BSE_mode='resonant'
 !
 ! GPL_EXCLUDE_START  
 !
 if (l_bs_fxc.or.index(BSS_mode,'t')/=0) then
   ! Remove the exchange from the BS_mode
   ! if TDDFT Fxc is obtained from K
   !
   if (index(BSS_mode,'t')==0) BS_res_mode=trim( string_remove(BS_res_mode,'x') )
   BSE_mode='causal'
   call parser('FxcCausal',FXC_is_causal)
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! Logicals (Drivers)
 !
 BS_res_K_corr=index(BS_res_mode,'c')/=0
 BS_res_K_exchange=index(BS_res_mode,'x')/=0
 BS_cpl_K_corr=index(BS_cpl_mode,'c')/=0
 BS_cpl_K_exchange=index(BS_cpl_mode,'x')/=0
 BS_K_coupling=any((/BS_cpl_K_corr,BS_cpl_K_exchange/)) 
 BS_anti_res=index(BSE_mode,'c')/=0
 !
 BS_W_is_diagonal=index(BS_res_mode,'d')/=0
 !
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
 call FREQUENCIES_setup(W_bss)
 FXC_n_mem_freqs=W_bss%n(2)
 !
 iq=BS_q(1)
 !
 call section('+','Main loop')
 !
 ! 1. The IP Part (Dipoles and eh-energies)...
 ! ================
 call K_IP(iq,Ken,Xk,X_static_pp,W_bss)
 !
 ! 2. The KERNEL
 ! ================
 if(.not.l_rpa_IP) call K(iq,Ken,Xk,q,X_static_pp,Xw,W_bss)
 !
 ! 3. The SOLVER
 ! =================
 if (l_bss.and.BS_columns==Xk%nibz) call K_solvers(iq,Ken,Xk,q,X_static_pp,W_bss)
 !
 ! GPL_EXCLUDE_START
 !
 ! `TDDFT` SOLVER
 !
 if (index(BSS_mode,'t')/=0) then
   !
   call X_duplicate(X_static_pp,X_tddft) ! initialize X_tddft to X_static_pp
   !
   X_tddft%ordering='r'
   if (FXC_is_causal) X_tddft%ordering='c'
   !
   ! If the kernel contains QP corrections I have to equally correct Xen
   !
   if (associated(Ken%QP_corrected)) then
     allocate(Xen%QP_corrected(Xen%nb,Xen%nk,n_sp_pol),Xen%Eo(Xen%nb,Xen%nk,n_sp_pol))
     call mem_est("E-QP_corrected E-Eo",(/size(Xen%QP_corrected),size(Xen%Eo)/),(/IP,SP/))
     Xen%QP_corrected=Ken%QP_corrected
     Xen%Eo=Xen%E
     Xen%E =Ken%E
   endif
   !
   call PP_redux_wait
   X_tddft%iq=iq
   X_tddft%ng=BS_n_g_exch
   call O_driver(Xen,Xk,q,W_bss,X_tddft) 
   !
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! CLEAN
 !
 deallocate(BSS_rhoq0,BS_blk_dim)
 call mem_est("BS_blk_dim BSS_rhoq0")
 !
 if (allocated(BS_eh_table)) deallocate(BS_eh_table)
 if (allocated(BS_eh_E))     deallocate(BS_eh_E)
 if (allocated(BS_eh_f))     deallocate(BS_eh_f)
 if (allocated(BS_eh_W))     deallocate(BS_eh_W)
 if (allocated(BS_eh_Z))     deallocate(BS_eh_Z)
 call mem_est("BS_eh_table BS_eh_E BS_eh_f BS_eh_W BS_eh_Z")
 !
end subroutine
