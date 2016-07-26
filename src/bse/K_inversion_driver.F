!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine K_inversion_driver(iq,W,E,k,q) 
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:real2ch
 use X_m,            ONLY:Epsilon_ii,X_drude_term,l_drude
 use BS,             ONLY:BS_mat,BS_H_dim,BSE_mode,BSS_inversion_mode,&
&                         BSS_INV_err,BSS_n_descs,BSS_description,Co_factor,&
&                         BS_mat_free,BSS_inversion_PERT_FULL_ratio,BSS_INV_eps_pl,BSS_INV_iw_full
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:q_norm,bz_samp
 use com,            ONLY:isec,msg,warning
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset,ncpu
 use interfaces,     ONLY:PARALLEL_index
 use IO_m,           ONLY:io_control,REP,NONE,OP_WR_CL
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#if defined _RT
 use RT_control,     ONLY:NEQ_Residuals,NEQ_Kernel
#endif
 !
 ! GPL_EXCLUDE_START
 !
#if defined _KERR
 use KERR,           ONLY:BS_KERR_free
#endif
#if defined _PL
 use PHOTOLUM,       ONLY:BS_PL_free
#endif
 !
 ! GPL_EXCLUDE_END
 !
 implicit none
 !
 type(w_samp) :: W
 integer      :: iq
 type(levels) :: E
 type(bz_samp):: k,q
 !
 ! Work Space...
 !
 ! ... dummies
 !
 type(PP_indexes) ::px
 integer          ::i1,i2,iw
 logical, external::stop_now
 complex(SP), allocatable  ::Lo(:,:,:)
 !
 ! ... frequencies
 !
 integer     :: full_inf_freqs,nw,nw_conv,&
&               min_dist_non_conv_freqs ! Every W%n_freqs/min_dist_non_conv_freqs
                                        ! I perform a full inversion
 logical    ::do_it_full,full_inf_freqs_found,FULL,PERTURBATIVE
 !
 ! I/O
 !
 integer           :: i_err,ID,Lo_dim
 integer, external :: io_BSS_invert
 !
#if defined _TIMING
 call timing('Invertion Solver',OPR='start')
#endif
 !
 ! Sectioning
 !============
 if (isec(2)/=0) then
   call section('=','Inversion solver')
 else if (isec(2)==0) then
   call section('+','Inversion solver')
 endif
 !
 ! Full inversion ?
 !==================
 FULL         = index(BSS_inversion_mode,'f')/=0
 PERTURBATIVE = index(BSS_inversion_mode,'p')/=0
 full_inf_freqs_found=.FALSE.
 !
#if defined _RT
 if (NEQ_Residuals.neqv.NEQ_Kernel) then
   call warning("[INV] Different occupation configurations for Kernel and Residuals not supported")
 endif
#endif
 !
 ! Restart database
 !==================
 call K_inversion_restart(iq,W)
 if (count(BSS_INV_err/=0)==0) then
   call LOCAL_clean()
   return
 endif
 !
 ! Kernel Loading 
 !================
 call K_stored_in_a_BIG_matrix(iq)
 !
 ! IP-part Lo [also recompute Epsilon_xx(:,3)]
 !===========================================
 if (.not.trim(BSE_mode)=="causal") Lo_dim=1
 if (     trim(BSE_mode)=="causal") Lo_dim=2
 !
 allocate(Lo(BS_H_dim,W%n_freqs,Lo_dim))
 call mem_est("Lo",(/size(Lo)/))
 !
 call K_inversion_Lo(iq,W,E,k,q,Lo_dim,Lo)
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open Inversion")
 !
 ! Perturbative Invertion
 !========================
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/W%n_freqs/))
 !
 if (PERTURBATIVE) then
   !
   do_it_full=.FALSE.
   call live_timing('Perturbative inversion ',px%n_of_elements(myid+1))
   !
   call K_inversion_engine(1,W%n_freqs,px,W,Lo,Lo_dim,do_it_full)
   !
   call PP_redux_wait(BSS_INV_err)
   call PP_redux_wait(Epsilon_ii(:,2))
   !
   ! I/O [after pert inversion]
   !
   BSS_INV_eps_pl(:,1)=Epsilon_ii(:,2)
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W,ID)
   !
 endif
 !
 ! Full Invertion
 !=================
 !
 ! First I define the group of frequencies not converged
 !
 if (FULL) then
   !
   full_inf_freqs=0
   do i1=1,W%n_freqs
     if (BSS_INV_err(i1)/=0) then
       full_inf_freqs=full_inf_freqs+1
       BSS_INV_iw_full(full_inf_freqs)=i1
     endif
   enddo
   !
 endif
 !
 if (PERTURBATIVE) then
   !
   min_dist_non_conv_freqs=W%n_freqs
   !
   do while (.not.full_inf_freqs_found) 
     !
     full_inf_freqs=0
     !
     iw=-1
     nw=0
     do i1=1,W%n_freqs
       if (BSS_INV_err(i1)/= 0.and.iw<0) iw=i1
       if (BSS_INV_err(i1)/= 0.and.iw>0) nw=nw+1
       if ((BSS_INV_err(i1)== 0.or.(BSS_INV_err(i1)/=0.and.i1==W%n_freqs)).and.iw>0) then
         !
         if (nw>=min_dist_non_conv_freqs) then
           !
           do i2=iw,iw+nw-1,min_dist_non_conv_freqs
             full_inf_freqs=full_inf_freqs+1
             BSS_INV_iw_full(full_inf_freqs)=i2
           enddo
           !
         endif
         iw=-1
         nw=0
       endif
     enddo
     min_dist_non_conv_freqs=min_dist_non_conv_freqs-1
     if (min_dist_non_conv_freqs==0) full_inf_freqs_found=.TRUE.
     !
     if (full_inf_freqs>=nint(real(W%n_freqs)/BSS_inversion_PERT_FULL_ratio)) full_inf_freqs_found=.TRUE.
     !
   enddo
   !
   if (full_inf_freqs>0.and.FULL) Epsilon_ii(:,2)=Epsilon_ii(:,2)/real(ncpu,SP)
   !
 endif
 !
 if (full_inf_freqs>0.and.FULL) then
   !
   call msg('rs','[BSE INV (before full INV)] Non converged frequencies :',full_inf_freqs)
   !
   ! Then loop on the groups performing full inversion
   !
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/full_inf_freqs/))
   call live_timing('Exact inversion ',px%n_of_elements(myid+1))
   !
   do_it_full=.true.
   !
   do i1=1,full_inf_freqs
     !
     if (stop_now(.FALSE.)) then
       call live_timing()
       exit
     endif
     !
     if (.not.px%element_1D(i1)) cycle
     !
     iw=BSS_INV_iw_full(i1)
     !
     call K_inversion_engine(iw,1,px,W,Lo,Lo_dim,do_it_full)
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call live_timing()
   call PP_redux_wait(BSS_INV_err,imode=2)
   call PP_redux_wait(Epsilon_ii(:,2))
   !
 endif
 !
 nw_conv=0
 do iw=1,W%n_freqs
   if (BSS_INV_err(iw)==0) nw_conv=nw_conv+1
 enddo
 call msg('rs','[BSE INV  (after full INV)] Converged frequencies [o/o]:',real(nw_conv)/real(W%n_freqs)*100._SP)
 !
 if (nw_conv>0) then
   !
   ! I/O [after full inversion]
   !
   BSS_INV_eps_pl(:,1)=Epsilon_ii(:,2)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W,ID)
   !
   ! Try a polinomial interpolation on the null values
   !===================================================
   call K_eps_interpolate(W%n_freqs,real(W%p),BSS_INV_err)
   !
 else
   !
   call warning('[BSE INV] Zero converged frequencies')
   !
 endif
 !
 call PP_redux_wait()
 !
 Epsilon_ii(:,2)=1._SP+Epsilon_ii(:,2)
 !
 if (l_drude) Epsilon_ii(:,2)=Epsilon_ii(:,2)+X_drude_term(:)*Co_factor/q_norm(1)**2
 !
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='    |Converged frequencies [o/o]:'//&
&                             trim(real2ch(real(nw_conv)/real(W%n_freqs)*100._SP))
 !
 call K_output_file(iq,"Inversion")
 !
 ! CLEAN
 !
 deallocate(BSS_INV_err,BSS_INV_eps_pl,BSS_INV_iw_full)
 call PP_indexes_reset(px)
 deallocate(Lo)
 call mem_est("Lo")
 !
 !
#if defined _TIMING
 call timing('Invertion Solver',OPR='stop')
#endif
 !
 contains 
   !
   subroutine LOCAL_clean
     call BS_mat_free( )
#if defined _PL
     call BS_PL_free( )
#endif
#if defined _KERR
     call BS_KERR_free( )
#endif
   end subroutine
   !
end subroutine
