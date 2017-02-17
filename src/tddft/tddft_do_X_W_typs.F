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
subroutine tddft_do_X_W_typs(iq,X,wv)
 !
 ! Prepare X and wv types for a TDDFT calculation
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_bs_fxc,l_bss,l_alda_fxc,l_lrc_fxc
 use memory_m,      ONLY:mem_est
 use frequency,     ONLY:w_samp
 use X_m,           ONLY:X_t,X_poles
 use BS,            ONLY:BS_bands,BS_eh_en,BSS_q0,BS_K_dim
 use TDDFT,         ONLY:FXC_type,FXC_K_diagonal,FXC,FXC_n_g_corr,&
&                        FXC_per_memstps,FXC_n_mem_freqs,FXC_is_causal,&
&                        FXC_LRC_alpha,ioBS_Fxc
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
   write (X%f_xc,'(a,1x,f10.4)') 'LRC ',FXC_LRC_alpha
   return
 endif
 !
 ! ALDA setup
 !
 if (l_alda_fxc) then
   FXC_type='alda'
   write (X%f_xc,'(a,1x,i4.4,a)') 'ALDA',FXC_n_g_corr,'G`s'
   return
 endif
 !
 if (iq<0) return
 !
 !
end subroutine
