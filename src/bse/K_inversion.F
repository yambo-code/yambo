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
subroutine K_inversion(iq,W,E,k,q) 
 !
 use pars,           ONLY:SP,cZERO
 use X_m,            ONLY:Epsilon_ii,Joint_DOS,X_drude_term,global_gauge,l_drude
 use stderr,         ONLY:real2ch
 use BS,             ONLY:BS_mat,BSS_rhoq0,BS_H_dim,&
&                         BSE_mode,BSS_inversion_mode,&
&                         BSS_er,BSS_dr,BSS_n_freqs,&
&                         BSS_n_descs,BSS_description,Co_factor,&
&                         BS_mat_free,BSS_inversion_PERT_FULL_ratio
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:levels
 use frequency,      ONLY:w_samp,W_reset,W_merge
 use R_lattice,      ONLY:q_norm,bz_samp
 use com,            ONLY:isec,msg,warning
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset,ncpu
 use interfaces,     ONLY:PARALLEL_index
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,NONE,VERIFY,OP_WR_CL
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
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
 integer          ::i1,i2,iw,of_mode
 logical, external::stop_now
 complex(SP)      ::gauge_factor
 complex(SP), allocatable  ::Lo(:,:,:)
 !
 ! ... frequencies
 !
 integer,allocatable :: inv_err(:),iw_full(:)
 integer             :: full_inf_freqs,nw,nw_conv,&
&                       min_dist_non_conv_freqs ! Every W%n_freqs/min_dist_non_conv_freqs
                                                ! I perform a full inversion
 logical    ::do_it_full,full_inf_freqs_found,FULL,PERTURBATIVE
 !
 ! I/O
 !
 integer           :: i_err,ID,Lo_dim
 type(w_samp)      :: W_disk
 integer, external :: io_BSS_invert
 integer,     allocatable :: inv_err_disk(:),disk_freq_table(:)
 complex(SP), allocatable :: eps_disk(:)
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
 call W_reset(W_disk)
 !
 ! I/O of the restart database
 !=============================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 i_err=io_BSS_invert(iq,1,(/0/),(/cZERO/),W_disk,ID)
 !
 ! When the damping range is not uniform then changing
 ! the energy borders
 !
 if (W_disk%dr(1)/=W_disk%dr(2).or.W%dr(1)/=W%dr(2)) then
   if (W%er(1)/=W_disk%er(1).or.W%er(2)/=W_disk%er(2)) i_err=-1
 endif
 !
 if (i_err==0) then
   allocate(inv_err_disk(W_disk%n_freqs),disk_freq_table(W_disk%n_freqs+W%n_freqs))
   allocate(eps_disk(W_disk%n_freqs))
   eps_disk=cZERO
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W_disk%n_freqs,inv_err_disk,eps_disk,W_disk,ID)
   !
   ! Here I create a new energy array that merges the disk 
   ! and present energies. In addition the corresponding inv_err
   ! and eps2 are alligned to the disk values.
   !
   call W_merge(W_disk,W,disk_freq_table)
   !
   BSS_n_freqs=W%n_freqs
   BSS_er=W%er
   BSS_dr=W%dr
   !
   allocate(inv_err(W%n_freqs),iw_full(W%n_freqs))
   Epsilon_ii(:,2)=cZERO
   Joint_DOS(:,2) =cZERO
   inv_err=-1
   !
   nw_conv=0
   do iw=1,W%n_freqs
     !
     ! Load eps only for converged frequencies
     !
     if (disk_freq_table(iw)>0) then
       inv_err(iw)=inv_err_disk(disk_freq_table(iw))
       if (inv_err(iw)==0) then
         Epsilon_ii(iw,2)=eps_disk(disk_freq_table(iw))/real(ncpu,SP)
         nw_conv=nw_conv+1
       endif
     endif
   enddo
   call msg('rs','[BSE INV] Frequencies read    [o/o]:',real(nw_conv)/real(W%n_freqs)*100._SP)
   deallocate(inv_err_disk,eps_disk,disk_freq_table)
 else
   allocate(inv_err(W%n_freqs),iw_full(W%n_freqs))
   inv_err=-1
   Epsilon_ii(:,2)=cZERO
   Joint_DOS(:,2) =cZERO
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
 of_mode=3
 call K_output_file(iq,-of_mode)
 !
 allocate(eps_disk(W%n_freqs))
 eps_disk=cZERO
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
   call perform_inversion(1,W%n_freqs)
   do i1=1,W%n_freqs
     if (inv_err(i1)<0) inv_err(i1)=0
   enddo 
   call PP_redux_wait(inv_err)
   !
   ! I/O [after pert inversion]
   !
   call PP_redux_wait(Epsilon_ii(:,2))
   !
   eps_disk(:)=Epsilon_ii(:,2)
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W%n_freqs,inv_err,eps_disk,W,ID)
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
     if (inv_err(i1)/=0) then
       full_inf_freqs=full_inf_freqs+1
       iw_full(full_inf_freqs)=i1
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
       if (inv_err(i1)/= 0.and.iw<0) iw=i1
       if (inv_err(i1)/= 0.and.iw>0) nw=nw+1
       if ((inv_err(i1)== 0.or.(inv_err(i1)==1.and.i1==W%n_freqs)).and.iw>0) then
         !
         if (nw>=min_dist_non_conv_freqs) then
           !
           do i2=iw,iw+nw-1,min_dist_non_conv_freqs
             full_inf_freqs=full_inf_freqs+1
             iw_full(full_inf_freqs)=i2
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
   if (full_inf_freqs_found.and.FULL) Epsilon_ii(:,2)=Epsilon_ii(:,2)/real(ncpu,SP)
   !
 endif
 !
 if (full_inf_freqs_found.and.FULL) then
   !
   call msg('rs','[BSE INV] Non converged frequencies :',full_inf_freqs)
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
     iw=iw_full(i1)
     !
     call perform_inversion(iw,1)
     call live_timing(steps=1)
     !
   enddo
   call live_timing()
   call PP_redux_wait(inv_err,imode=2)
   call PP_redux_wait(Epsilon_ii(:,2))
   !
 endif
 !
 nw_conv=0
 do iw=1,W%n_freqs
   if (inv_err(iw)==0) nw_conv=nw_conv+1
 enddo
 call msg('rs','[BSE INV] Converged frequencies [o/o]:',real(nw_conv)/real(W%n_freqs)*100._SP)
 !
 if (nw_conv>0) then
   !
   ! I/O [after full inversion]
   !
   eps_disk(:)=Epsilon_ii(:,2)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W%n_freqs,inv_err,eps_disk,W,ID)
   !
   ! Try a polinomial interpolation on the null values
   !===================================================
   call K_eps_interpolate(W%n_freqs,real(W%p),inv_err)
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
 call K_output_file(iq,of_mode)
 !
 ! CLEAN
 !
 deallocate(inv_err,eps_disk)
 call PP_indexes_reset(px)
 deallocate(iw_full,Lo)
 call mem_est("Lo")
 !
 call BS_mat_free( )
 !
#if defined _TIMING
 call timing('Invertion Solver',OPR='stop')
#endif
 !
 contains
   !
   subroutine perform_inversion(iw_2start,iw_2do)
     integer     :: iw_2start,iw_2do
     !
     integer :: i2,iw
     logical :: perform_inv
     !
     integer, external::MATRIX_resolvant_via_perturbative_inversion
     integer, external::MATRIX_resolvant_via_exact_inversion
     !
     do i2=1,iw_2do
       !
       iw=i2+iw_2start-1
       !
       ! The parallel loop is controlled in the manin body loop 
       ! when a full inversion is performed
       !
       if (.not.do_it_full) then
         if(.not.px%element_1D(iw)) cycle
       endif
       !
       if (     do_it_full) perform_inv= inv_err(iw)/= 0
       if (.not.do_it_full) perform_inv= inv_err(iw)==-1
       !
       if (trim(global_gauge)=='length'  ) gauge_factor=-Co_factor/q_norm(1)**2
       if (trim(global_gauge)=='velocity') gauge_factor=-Co_factor/ W%p(iw) **2
       !       
       if (perform_inv) then
         !
         if (do_it_full) then
           inv_err(iw)=MATRIX_resolvant_via_exact_inversion(BS_H_dim,Lo_dim,BSS_rhoq0,gauge_factor,&
&                                                           Epsilon_ii(iw,2),Lo(:,iw,:Lo_dim),BS_mat)
         else
           inv_err(iw)=MATRIX_resolvant_via_perturbative_inversion(BS_H_dim,Lo_dim,BSS_rhoq0,gauge_factor,&
&                                                                  Epsilon_ii(iw,2),Lo(:,iw,:Lo_dim),BS_mat)
         endif
         !
       endif       
       !
       if (.not.do_it_full) call live_timing(steps=1)
       !
     enddo
     !
     if (.not.do_it_full) call live_timing()
     !
   end subroutine
   !
end subroutine
