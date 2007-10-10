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
subroutine K_inversion(iq,W,E,k,q) 
 !
 use pars,           ONLY:SP,pi
#if defined PJ_PH
 use drivers,        ONLY:l_ph_corr
#endif
 use X_m,            ONLY:X_epsilon
 use stderr,         ONLY:intc,real2ch
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,BS_not_const_eh_f,&
&                         BS_K_coupling,BS_eh_table,BS_eh_E,BS_bands,Kernel_by_V,&
&                         BSS_uses_RIM,BSS_er,BSS_dr,BSS_n_freqs,&
&                         BSS_n_descs,BSS_description,BS_eh_W,BS_eh_Z
 use memory_m,       ONLY:mem_est
 use parser_m,       ONLY:parser
 use electrons,      ONLY:spin_occ,levels,BZ_RIM_nbands,BZ_RIM_tot_nkpts
 use frequency,      ONLY:w_samp,W_reset,W_merge,W_duplicate
 use R_lattice,      ONLY:d3k_factor,q_norm,bz_samp
 use com,            ONLY:isec,warning,msg
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset,ncpu
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,NONE,VERIFY,OP_WR_CL
#if defined PJ_PH
 use ELPH,           ONLY:EXCPH_mat,add_ELPH_K
#endif
 implicit none
 type(w_samp) :: W
 integer      :: iq
 type(levels) :: E
 type(bz_samp):: k,q
 !
 ! Work Space...
 !
 ! ... dummies
 !
 type(pp_indexes) ::px
 type(w_samp)     ::W_shifted
 integer          ::i1,i2,ik_bz,iv,ic,iw,icv,of_mode,i_sp,ik_ibz,WS_alloc_err
 integer, external::pert_inversion,eval_ELPH_K
 complex(SP)      ::Co,QP_Z,QP_width
 complex(SP), allocatable  ::Lo(:,:),WS(:,:)
 !
 ! ... frequencies
 !
 integer, parameter :: max_freqs_full_inv_ratio=10
 integer,  allocatable ::inv_err(:),iw_full(:)
 integer    ::full_inf_freqs,nw,nw_conv,&
&             min_dist_non_conv_freqs ! Every W%n(1)/min_dist_non_conv_freqs
                                      ! I perform a full inversion
 logical    ::full_diago,skip_full_inv,force_full_inv,full_inf_freqs_found
 !
#if defined _DOUBLE
 complex(DP)::zdotu,zdotc
#else
 complex(SP)::cdotu,cdotc
#endif
 !
 ! I/O
 !
 integer           ::i_err,ID
 type(w_samp)      ::W_disk
 integer, external ::ioBSS_invert
 integer, allocatable     ::inv_err_disk(:),disk_freq_table(:)
 complex(SP), allocatable ::eps_2start(:)
 !
 ! Sectioning
 !
 if (isec(2)/=0) then
   call section('=','Inversion solver')
 else if (isec(2)==0) then
   call section('+','Inversion solver')
 endif
 !
 if (BS_not_const_eh_f) call warning('Inversion solver: fractional occupations unsupported')
 if (BS_K_coupling) call warning('Inversion solver: unsupported for non-resonant K')
 if (any((/BS_not_const_eh_f,BS_K_coupling/))) return
 !
 ! RIM support ? Full invertion ?
 !
 BSS_uses_RIM=BZ_RIM_nbands>=BS_bands(2)
 call parser('SkipInv',skip_full_inv)
 call parser('FullInv',force_full_inv)
 !
 if (force_full_inv) skip_full_inv=.FALSE.
 !
 ! I/O
 !
 call W_reset(W_disk)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 i_err=ioBSS_invert(iq,1,(/0/),(/(0.,0.)/),W_disk,ID)
 !
 ! When the damping range is not uniform then changing
 ! the energy borders
 !
 if (W_disk%dr(1)/=W_disk%dr(2).or.W%dr(1)/=W%dr(2)) then
   if (W%er(1)/=W_disk%er(1).or.W%er(2)/=W_disk%er(2)) i_err=-1
 endif
 !
 if (i_err==0) then
   allocate(inv_err_disk(W_disk%n(1)),eps_2start(W_disk%n(1)),&
&           disk_freq_table(W_disk%n(1)+W%n(1)))
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=ioBSS_invert(iq,W_disk%n(1),inv_err_disk,eps_2start,W_disk,ID)
   !
   ! Here I create a new energy array that merges the disk 
   ! and present energies. In addition the corresponding inv_err
   ! and eps2 are alligned to the disk values.
   !
   call W_merge(W_disk,W,disk_freq_table)
   !
   deallocate(X_epsilon)
   !
   BSS_n_freqs=W%n(1)
   BSS_er=W%er
   BSS_dr=W%dr
   !
   ALLOCate(X_epsilon(4,BSS_n_freqs))
   allocate(inv_err(W%n(1)),iw_full(W%n(1)))
   X_epsilon=(0.,0.)
   inv_err=-1
   !
   nw_conv=0
   do iw=1,W%n(1)
     !
     ! Load eps only for converged frequencies
     !
     if (disk_freq_table(iw)>0) then
       inv_err(iw)=inv_err_disk(disk_freq_table(iw))
       if (inv_err(iw)==0) then
         X_epsilon(2,iw)=eps_2start(disk_freq_table(iw))/real(ncpu,SP)
         nw_conv=nw_conv+1
       endif
     endif
   enddo
   call msg('rs','[BSE INV] Frequencies read    [o/o]:',real(nw_conv)/real(W%n(1))*100.)
   deallocate(inv_err_disk,eps_2start,disk_freq_table)
 else
   allocate(inv_err(W%n(1)),iw_full(W%n(1)))
   inv_err=-1
   X_epsilon=(0.,0.)
 endif
 !
 !  mem est
 !
 call mem_est("Lo",(/BS_K_dim*W%n(1),BS_K_dim**2/))
 !
 ! Kernel loading
 !
 call Kernel_by_V(iq)
 forall(i1=1:BS_K_dim) BS_mat(i1,i1)=BS_mat(i1,i1)-BS_eh_E(i1)
 !
#if defined PJ_PH
 !
 allocate(WS(BS_K_dim,BS_K_dim),stat=WS_alloc_err)
 if (WS_alloc_err==0) then
   call mem_est("WS",(/BS_K_dim**2/))
   WS=BS_mat
 else 
   call warning('[ELPH K] Work Space not allocated')
 endif
 !
 ! SAVE BS_mat & calculaye ELPH_K
 !
 add_ELPH_K=.false.
 if (l_ph_corr) add_ELPH_K=eval_ELPH_K(E,k,q,.TRUE.,(0.,0.))==0
#endif
 !
 ! Initialize the output file 
 !
 of_mode=3
#if defined PJ_PH
 if (add_ELPH_K) of_mode=4
#endif
 call K_dump_to_o_file(iq,-of_mode)
 !
 ! Green`s function and non-interacting eps2
 !
 Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/q_norm(1)**2
 !
 allocate(Lo(BS_K_dim,W%n(1)))
 Lo=(0.,0.)
 call W_reset(W_shifted)
 call W_duplicate(W,W_shifted)
 !
 call live_timing('Green`s function',BS_K_dim)
 !
 do i1=1,BS_K_dim
   !
   ik_bz =BS_eh_table(i1,1)
   ik_ibz=k%sstar(ik_bz,1)
   iv    =BS_eh_table(i1,2)
   ic    =BS_eh_table(i1,3)
#if defined PJ_SPIN
   i_sp=BS_eh_table(i1,4)
#else
   i_sp=1
#endif
   !
   if (.not.BSS_uses_RIM) then
     QP_width=0.
     QP_Z=(1.,0.)
     if (allocated(BS_eh_W)) QP_width=BS_eh_W(i1)
     if (allocated(BS_eh_Z)) QP_Z=BS_eh_Z(i1)
     Lo(i1,:)=QP_Z/(W%p(:)-BS_eh_E(i1)-(0.,1.)*QP_width)
   else
     call X_bare_RIM_Gf(1,(/ik_bz,iv,ic,i_sp/),(/1,W%n(1)/),W,E,k,Lo(i1,:),"r")
   endif
   X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*Lo(i1,:)*Co
   !
   ! Include the Kernel diagonal as a rigid shift
   !==============================================
   !
   if (BSS_uses_RIM) then
     forall(iw=1:W%n(1)) W_shifted%p(iw)=W%p(iw)-BS_mat(i1,i1)
#if defined PJ_PH
     if (add_ELPH_K) forall(iw=1:W%n(1)) W_shifted%p(iw)=W_shifted%p(iw)-EXCPH_mat(i1,i1)
#endif
     call X_bare_RIM_Gf(1,(/ik_bz,iv,ic,i_sp/),(/1,W%n(1)/),W_shifted,E,k,Lo(i1,:),"r")
     BS_mat(i1,i1)=(0.,0.)
#if defined PJ_PH
     if (add_ELPH_K) EXCPH_mat(i1,i1)=(0.,0.)
     if (WS_alloc_err==0) WS(i1,i1)=(0.,0.)
#endif
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 X_epsilon(3,:)=(1.+X_epsilon(3,:))/real(ncpu,SP)
 call live_timing()
 !
 ! Perturbative Invertion
 !========================
 !
 full_diago=.false.
 call pp_indexes_reset(px)
 call par_indexes(px,(/W%n(1)/))
 call live_timing('Freq loop [pert]',px%stps(myid+1))
 call perform_invertion(1,W%n(1))
 call pp_redux_wait(inv_err,imode=2)
 !
 ! I/O [after pert invertion]
 !
 call pp_redux_wait(X_epsilon)
 call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
 i_err=ioBSS_invert(iq,W%n(1),inv_err,X_epsilon(2,:),W,ID)
 !
 ! Full Invertion
 !=================
 !
 ! First I define the group of frequencies not converged
 !
 if (force_full_inv) then
   full_inf_freqs=0
   do i1=1,W%n(1)
     if (inv_err(i1)/=0) then
       full_inf_freqs=full_inf_freqs+1
       iw_full(full_inf_freqs)=i1
     endif
   enddo
 else
   !
   min_dist_non_conv_freqs=W%n(1)
   full_inf_freqs_found=.FALSE.
   !
   do while (.not.full_inf_freqs_found) 
     !
     full_inf_freqs=0
     !
     iw=-1
     nw=0
     do i1=1,W%n(1)
       if (inv_err(i1)/= 0.and.iw<0) iw=i1
       if (inv_err(i1)/= 0.and.iw>0) nw=nw+1
       if ((inv_err(i1)== 0.or.(inv_err(i1)==1.and.i1==W%n(1))).and.iw>0) then
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
     if (full_inf_freqs>=W%n(1)/max_freqs_full_inv_ratio) full_inf_freqs_found=.TRUE.
   enddo
 endif
 !
 if (full_inf_freqs>0.and..not.skip_full_inv) then
   !
   call msg('rs','[BSE INV] Non converged frequencies :',full_inf_freqs)
   !
   X_epsilon(2:3,:)=X_epsilon(2:3,:)/real(ncpu,SP)
   !
   ! Then loop on the groups performing full inversion
   !
   call pp_indexes_reset(px)
   call par_indexes(px,(/full_inf_freqs/))
   call live_timing('Freq loop [full]',px%stps(myid+1))
   full_diago=.true.
   do i1=1,full_inf_freqs
     if (.not.px%i1p(i1)) cycle
     iw=iw_full(i1)
     call perform_invertion(iw,1)
     call live_timing(steps=1)
   enddo
   call live_timing()
   call pp_redux_wait(inv_err,imode=2)
   call pp_redux_wait(X_epsilon)
 endif
 !
 nw_conv=0
 do iw=1,W%n(1)
   if (inv_err(iw)==0) nw_conv=nw_conv+1
 enddo
 call msg('rs','[BSE INV] Converged frequencies (after full inv) [o/o]:',real(nw_conv)/real(W%n(1))*100.)
 !
 ! I/O [after full invertion]
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 i_err=ioBSS_invert(iq,W%n(1),inv_err,X_epsilon(2,:),W,ID)
 !
 ! Try a polinomial interpolation on the null values
 !
 call K_eps_interpolate(W%n(1),real(W%p),inv_err)
 call pp_redux_wait()
 !
 X_epsilon(2,:)=1.+X_epsilon(2,:)
 X_epsilon(1,:)=W%p(:)
 !
#if defined PJ_PH
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='    |Phononic Kernel      : no'
 if (add_ELPH_K) BSS_description(BSS_n_descs)='    |Phononic Kernel      : yes'
#endif
 BSS_n_descs=BSS_n_descs+2
 BSS_description(BSS_n_descs-1)=' '
 BSS_description(BSS_n_descs)=' BSS|BZ RIM points              :'//intc(BZ_RIM_tot_nkpts)
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='    |Converged frequencies [o/o]:'//&
&                             trim(real2ch(real(nw_conv)/real(W%n(1))*100.))
 !
 call K_dump_to_o_file(iq,of_mode)
 !
 ! CLEAN
 !
 deallocate(inv_err)
 call pp_indexes_reset(px)
 deallocate(BS_mat,iw_full,Lo)
 call mem_est("BS_mat Lo")
#if defined PJ_PH
 if (allocated(EXCPH_mat)) then
   deallocate(EXCPH_mat)
   call mem_est("EXCPH_mat")
 endif
 if (WS_alloc_err==0) then
   deallocate(WS)
   call mem_est("WS")
 endif
#endif
 !
contains
  !
  subroutine perform_invertion(iw_2start,iw_2do)
    integer     :: iw_2start,iw_2do
    !
    integer :: i2,i3
    logical :: perform_inv
    !
    do i2=1,iw_2do
      !
#if defined PJ_PH
      if (add_ELPH_K) then
        if (WS_alloc_err==0) then
          BS_mat=WS
        else
          BS_mat=(0.,0.)
          call Kernel_by_V(iq)
          forall(i3=1:BS_K_dim) BS_mat(i3,i3)=0.
        endif
        call static2dyn_ELPH_K(E,k,q,W%p(i2),BS_mat)
      endif
#endif
      !
      if(.not.full_diago) then
        !
        if (.not.px%i1p(i2)) cycle
        !
      endif
      !
      if (full_diago) perform_inv=inv_err(i2+iw_2start-1)/=0
      if (.not.full_diago) perform_inv=inv_err(i2+iw_2start-1)==-1
      !
      if (perform_inv) &
&       inv_err(i2+iw_2start-1)=pert_inversion(BS_K_dim,BSS_rhoq0,-1.*Co,&
&              X_epsilon(2,i2+iw_2start-1),Lo(:,i2+iw_2start-1),BS_mat,full_diago)
      !
      if (.not.full_diago) call live_timing(steps=1)
      !
    enddo
    if (.not.full_diago) call live_timing()
    !
  end subroutine
  !
end subroutine
