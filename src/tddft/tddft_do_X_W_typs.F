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
subroutine tddft_do_X_W_typs(iq,X,wv)
 !
 ! Prepare X and wv types for a TDDFT calculation
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_bs_fxc,l_bss,l_alda_fxc,l_lrc_fxc
 use memory_m,      ONLY:mem_est
 use stderr,        ONLY:real2ch,intc
 use frequency,     ONLY:w_samp
 use X_m,           ONLY:X_t,X_poles
 use BS,            ONLY:BS_bands,BS_eh_en,BSS_q0,BS_K_dim
 use TDDFT,         ONLY:FXC_type,FXC_K_diagonal,FXC,FXC_n_g_corr,&
&                        FXC_per_memstps,FXC_n_mem_freqs,FXC_is_causal,&
&                        FXC_LRC_alpha,FXC_LRC_beta,ioBS_Fxc
 use IO_m,          ONLY:io_control,OP_RD_CL,VERIFY,REP
 implicit none
 type(X_t)   :: X
 type(w_samp):: wv
 integer     :: iq
 !
 ! Fxc I/O
 !
 integer           ::ioFxc_err,ID
 !
 ! Reset X%f_xc as it is dumped when reading the DB during
 ! the initilization
 !
 X%f_xc='none'
 !
 if (.not.any((/l_alda_fxc,l_bs_fxc,l_lrc_fxc/))) return
 !
 ! LRC setup
 !
 if (l_lrc_fxc) then
   FXC_type='LRC'
   X%f_xc='LRC '//trim(real2ch(FXC_LRC_alpha))//'(alpha) '//trim(real2ch(FXC_LRC_beta))//'(beta)'
   return
 endif
 !
 ! ALDA setup
 !
 if (l_alda_fxc) then
   FXC_type='alda'
   X%f_xc='ALDA '//trim(intc(FXC_n_g_corr))//'(Gs)'
   return
 endif
 !
 if (iq<0) return
 !
 ! GPL_EXCLUDE_START
 !
 ! Fxc DB
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 ioFxc_err=ioBS_Fxc(iq,wv,ID)
 if (ioFxc_err/=0) then
   FXC_type='rpa'
   l_bs_fxc=.FALSE.
   return
 endif
 !
 ! FXC_K_diagonal has been allocated in ioFxc_err
 !
 allocate(FXC(FXC_n_g_corr,FXC_n_g_corr,FXC_n_mem_freqs))
 !
 call mem_est("FXC_K_diagonal FXC",(/BS_K_dim,size(FXC)/),(/SP,2*SP/))
 !
 ! As wv%p is read from DB %n(2) is not setup in freqs_setup.
 ! Note that wv%n(2) MUST respect the FXC_n_mem_freqs value.
 ! Tnis means that wv%n(2) CANNOT assume generic values.
 ! It can be either wv%n(1) OR FXC_n_mem_freqs.
 ! 
 if (l_bss) wv%n(2)=wv%n(1)

 if (.not.l_bss.and.wv%per_memstps/=100.) then
   wv%n(2)=FXC_n_mem_freqs
   wv%per_memstps=FXC_per_memstps
 endif
 !
 X%ng=max(X%ng,FXC_n_g_corr)
 !
 ! Note that independently of the lbss value (TRUE/FALSE)
 ! the BS/BSS and FXC components are read from the FXC file
 !
 FXC_type='BSFxc'
 X%f_xc='BS'
 !
 X%ib= BS_bands
 X%ehe=BS_eh_en
 X%q0= BSS_q0
 X%ordering="c"
 if (.not.FXC_is_causal) X%ordering="r"
 !
 allocate(X_poles(BS_K_dim))
 X_poles=FXC_K_diagonal
 !
 ! GPL_EXCLUDE_END
 !
end subroutine
