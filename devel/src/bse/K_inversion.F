!
!        Copyright (C) 2000-2015 the YAMBO team
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
 use pars,           ONLY:SP,pi
 use X_m,            ONLY:X_epsilon,X_drude_term
 use stderr,         ONLY:intc,real2ch
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,BS_H_dim,&
&                         BSE_mode,BSS_eh_table,BSS_eh_E,BSS_inversion_mode,&
&                         BSS_uses_RIM,BSS_er,BSS_dr,BSS_n_freqs,BSS_eh_f,&
&                         BSS_n_descs,BSS_description,BSS_eh_W,BSS_eh_Z,BS_mat_diagonal,&
&                         BS_mat_free,BSS_inversion_PERT_FULL_ratio
#if defined _ELPH
 use BS,             ONLY:BSS_uses_GreenF,BS_bands
#endif
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:spin_occ,levels,BZ_RIM_tot_nkpts,spin
 use frequency,      ONLY:w_samp,W_reset,W_merge,W_duplicate
 use R_lattice,      ONLY:d3k_factor,q_norm,bz_samp
 use com,            ONLY:isec,msg,warning
 use LIVE_t,         ONLY:live_timing
 use parallel_m,     ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset,ncpu
 use interfaces,     ONLY:PARALLEL_index
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,NONE,VERIFY,OP_WR_CL
#if defined _TIMING
 use timing_m,       ONLY:timing
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
 type(PP_indexes) ::px
 type(w_samp)     ::W_shifted
 integer          ::i1,i2,ik_bz,iv,ic,iw,of_mode,i_sp,ik_ibz
 logical, external::stop_now
 complex(SP)      ::Co,QP_Z,QP_width
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
 integer           :: i_err,ID,IO_dim
 type(w_samp)      :: W_disk
 integer, external :: io_BSS_invert
 integer,     allocatable :: inv_err_disk(:),disk_freq_table(:)
 complex(SP), allocatable :: eps_disk(:,:)
 !
#if defined _TIMING
 call timing('Invertion Solver',OPR='start')
#endif
 !
 ! Sectioning
 !============
 !
 if (isec(2)/=0) then
   call section('=','Inversion solver')
 else if (isec(2)==0) then
   call section('+','Inversion solver')
 endif
 !
 IO_dim=1
 if (trim(BSE_mode)=="causal")   IO_dim=2
 !
 ! Full inversion ?
 !==================
 !
 FULL         = .TRUE.
 PERTURBATIVE = .TRUE.
 !
 if (trim(BSS_inversion_mode)/="i") then
   FULL         = index(BSS_inversion_mode,'f')/=0
   PERTURBATIVE = index(BSS_inversion_mode,'p')/=0
 endif
 !
 call W_reset(W_disk)
 !
 ! I/O of the restart database
 !=============================
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 i_err=io_BSS_invert(iq,1,1,(/0/),(/(0._SP,0._SP)/),W_disk,ID)
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
   allocate(eps_disk(W_disk%n_freqs,IO_dim))
   eps_disk=(0._SP,0._SP)
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W_disk%n_freqs,IO_dim,inv_err_disk,eps_disk,W_disk,ID)
   !
   ! Here I create a new energy array that merges the disk 
   ! and present energies. In addition the corresponding inv_err
   ! and eps2 are alligned to the disk values.
   !
   call W_merge(W_disk,W,disk_freq_table)
   !
   deallocate(X_epsilon)
   !
   BSS_n_freqs=W%n_freqs
   BSS_er=W%er
   BSS_dr=W%dr
   !
   allocate(X_epsilon(4,BSS_n_freqs))
   allocate(inv_err(W%n_freqs),iw_full(W%n_freqs))
   X_epsilon=(0.,0.)
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
         X_epsilon(2,iw)=eps_disk(disk_freq_table(iw),1)/real(ncpu,SP)
         if(IO_dim==2) X_epsilon(4,iw)=eps_disk(disk_freq_table(iw),2)/real(ncpu,SP)
         nw_conv=nw_conv+1
       endif
     endif
   enddo
   call msg('rs','[BSE INV] Frequencies read    [o/o]:',real(nw_conv)/real(W%n_freqs)*100._SP)
   deallocate(inv_err_disk,eps_disk,disk_freq_table)
 else
   allocate(inv_err(W%n_freqs),iw_full(W%n_freqs))
   inv_err=-1
   X_epsilon=(0.,0.)
 endif
 !
 ! Kernel Loading 
 !================
 !
 call K_stored_in_a_BIG_matrix(iq)
 !
 if (BSS_uses_RIM) then
   !
   forall(i1=1:BS_K_dim) BS_mat(i1,i1)=(0.,0.)
   if (index(BSE_mode,'coupling')>0) then
     forall(i1=BS_K_dim+1:BS_H_dim) BS_mat(i1,i1)=(0.,0.)
     forall(i1=BS_K_dim+1:BS_H_dim) BS_mat(i1,:) =-BS_mat(i1,:)
   endif
   !
 else
   !
   forall(i1=1:BS_K_dim) BS_mat(i1,i1)=BS_mat(i1,i1)-BSS_eh_E(i1)
   if (index(BSE_mode,'coupling')>0) then
     forall(i1=BS_K_dim+1:BS_H_dim) BS_mat(i1,i1)=BS_mat(i1,i1)+BSS_eh_E(i1-BS_K_dim)
     forall(i1=BS_K_dim+1:BS_H_dim) BS_mat(i1,:) =-BS_mat(i1,:)
   endif
   !
 endif
 !
 !
 allocate(eps_disk(W%n_freqs,IO_dim))
 eps_disk=(0._SP,0._SP)
 !
 ! I need to remove the occupations from the oscillators strenght
 ! These are now embodied in the Green's function 
 !
 BSS_rhoq0(1:BS_K_dim)=BSS_rhoq0(1:BS_K_dim)/sqrt(BSS_eh_f(1:BS_K_dim))
 !
 if (index(BSE_mode,'coupling')>0) then
   BSS_rhoq0(BS_K_dim+1:2*BS_K_dim)=BSS_rhoq0(BS_K_dim+1:2*BS_K_dim)/sqrt(BSS_eh_f(1:BS_K_dim))
 endif
 !
 ! Initialize the output file 
 !============================
 of_mode=3
 call K_output_file(iq,-of_mode)
 !
 ! Green`s function and non-interacting eps2
 !
 Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/q_norm(1)**2
 !
 allocate(Lo(BS_H_dim,W%n_freqs,IO_dim))
 call mem_est("Lo",(/BS_H_dim*W%n_freqs*IO_dim*2/))
 Lo=(0.,0.)
 !
 call W_reset(W_shifted)
 call W_duplicate(W,W_shifted)
 !
#if defined _ELPH
 !
 ! Green Functions must be all mapped to the Xw range so
 ! to be easily convoluted
 !
 if (associated(E%GreenF)) then
   call X_GreenF_remap(BS_bands,E,W)
   BSS_uses_GreenF=.TRUE.
 endif
 !
#endif
 !
 call live_timing('Green`s function',BS_K_dim)
 !
 do i1=1,BS_K_dim
   !
   ik_bz =BSS_eh_table(i1,1)
   ik_ibz=k%sstar(ik_bz,1)
   iv    =BSS_eh_table(i1,2)
   ic    =BSS_eh_table(i1,3)
   i_sp  =spin(BSS_eh_table(i1,:))
   !
   if (.not.BSS_uses_RIM) then
     !
     QP_width=0.
     QP_Z=(1.,0.)
     !
     if (allocated(BSS_eh_W)) QP_width=BSS_eh_W(i1)
     !
     if (allocated(BSS_eh_Z)) QP_Z=BSS_eh_Z(i1)
     !
     if (associated(E%GreenF)) then
       !
       ! No RIM but GFs
       !================
       call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W,E,k,Lo(i1,:,1),"r")
       !
       if (trim(BSE_mode)=='causal') then
         call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W,E,k,Lo(i1,:,2),"ca")
       else if (index(BSE_mode,'coupling')>0) then
         call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W,E,k,Lo(i1+BS_K_dim,:,1),"ca")
       endif
       !
     else
       !
       ! No RIM No GFs
       !===============
       Lo(i1,:,1)=QP_Z*BSS_eh_f(i1)/(W%p(:)-BSS_eh_E(i1)-(0.,1.)*QP_width)
       if (trim(BSE_mode)=='causal') then
         Lo(i1,:,2)=-QP_Z*BSS_eh_f(i1)/(W%p(:)+BSS_eh_E(i1)-(0.,1.)*QP_width)
       else if (index(BSE_mode,'coupling')>0) then
         Lo(i1+BS_K_dim,:,1)=-QP_Z*BSS_eh_f(i1)/(W%p(:)+BSS_eh_E(i1)-(0.,1.)*QP_width)
       endif
       !
     endif
     !
   else
     !
     ! Energy RIM 
     !============
     call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W,E,k,Lo(i1,:,1),"sr")
     Lo(i1,:,1)=Lo(i1,:,1)*BSS_eh_f(i1)
     !
     if (trim(BSE_mode)=='causal') then
       call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W,E,k,Lo(i1,:,2),"sa")
       Lo(i1,:,2)=Lo(i1,:,2)*BSS_eh_f(i1)
     else if (index(BSE_mode,'coupling')>0) then
       call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W,E,k,Lo(i1+BS_K_dim,:,1),"sa")
       Lo(i1+BS_K_dim,:,1)=Lo(i1+BS_K_dim,:,2)*BSS_eh_f(i1)
     endif
     !
   endif
   !
   X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*Lo(i1,:,1)*Co
   !
   if (trim(BSE_mode)=='causal') then
     X_epsilon(3,:)=X_epsilon(3,:)-conjg(BSS_rhoq0(i1))*BSS_rhoq0(i1)*Lo(i1,:,2)*Co
   else if (index(BSE_mode,'coupling')>0) then
     X_epsilon(3,:)=X_epsilon(3,:)-conjg(BSS_rhoq0(i1+BS_K_dim))*BSS_rhoq0(i1+BS_K_dim)*Lo(i1+BS_K_dim,:,1)*Co
   endif
   !
   ! When the RIM is used the Kernel diagonal is included as a rigid shift
   !=======================================================================
   if (BSS_uses_RIM) then
     forall(iw=1:W%n_freqs) W_shifted%p(iw)=W%p(iw)-BS_mat_diagonal(i1)
     call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W_shifted,E,k,Lo(i1,:,1),"sr")
     Lo(i1,:,1)=Lo(i1,:,1)*BSS_eh_f(i1)
     if (trim(BSE_mode)=='causal') then
       forall(iw=1:W%n_freqs) W_shifted%p(iw)=W%p(iw)+conjg(BS_mat_diagonal(i1))
       call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W_shifted,E,k,Lo(i1,:,2),"sa")
       Lo(i1,:,2)=Lo(i1,:,2)*BSS_eh_f(i1)
     endif
     if (index(BSE_mode,'coupling')>0) then
       call X_bare_RIM_GreenF(1,(/ik_bz,iv,ic,i_sp/),W_shifted,E,k,Lo(i1+BS_K_dim,:,1),"sa")
       Lo(i1+BS_K_dim,:,1)=Lo(i1+BS_K_dim,:,2)*BSS_eh_f(i1)
     endif
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 X_epsilon(3,:)=(1.+X_epsilon(3,:))/real(ncpu,SP)
 !
 ! Perturbative Invertion
 !========================
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/W%n_freqs/))
 !
 if (PERTURBATIVE) then
   !
   do_it_full=.FALSE.
   call live_timing('Freq loop [pert]',px%n_of_elements(myid+1))
   call perform_inversion(1,W%n_freqs)
   do i1=1,W%n_freqs
     if (inv_err(i1)<0) inv_err(i1)=0
   enddo 
   call PP_redux_wait(inv_err)
   !
   ! I/O [after pert inversion]
   !
   call PP_redux_wait(X_epsilon)
   !
   eps_disk(:,1)=X_epsilon(2,:)
   if (trim(BSE_mode)=="causal") eps_disk(:,2)=X_epsilon(4,:)
   !
   call io_control(ACTION=OP_WR_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W%n_freqs,IO_dim,inv_err,eps_disk,W,ID)
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
   full_inf_freqs_found=.FALSE.
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
 endif
 !
 if (full_inf_freqs>0.and.FULL) then
   !
   call msg('rs','[BSE INV] Non converged frequencies :',full_inf_freqs)
   !
   X_epsilon(2:4,:)=X_epsilon(2:4,:)/real(ncpu,SP)
   !
   ! Then loop on the groups performing full inversion
   !
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/full_inf_freqs/))
   call live_timing('Freq loop [full]',px%n_of_elements(myid+1))
   do_it_full=.true.
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
   call PP_redux_wait(X_epsilon)
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
   eps_disk(:,1)=X_epsilon(2,:)
   !
   if (trim(BSE_mode)=="causal") eps_disk(:,2)=X_epsilon(4,:)
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
   i_err=io_BSS_invert(iq,W%n_freqs,IO_dim,inv_err,eps_disk,W,ID)
   !
   ! Try a polinomial interpolation on the null values
   !===================================================
   !
   call K_eps_interpolate(W%n_freqs,real(W%p),inv_err,2)
   !
   if(trim(BSE_mode)=="causal") then
     !
     call K_eps_interpolate(W%n_freqs,real(W%p),inv_err,4)
     !
     X_epsilon(2,:)=X_epsilon(2,:)+X_epsilon(4,:)
     !
   endif
   !
 else
   !
   call warning('[BSE INV] Zero converged frequencies')
   !
 endif
 !
 call PP_redux_wait()
 !
 X_epsilon(2,:)=1.+X_epsilon(2,:)+X_drude_term(:)*Co
 X_epsilon(1,:)=W%p(:)
 !
 BSS_n_descs=BSS_n_descs+2
 BSS_description(BSS_n_descs-1)=' '
 BSS_description(BSS_n_descs)=' BSS|BZ RIM points              :'//intc(BZ_RIM_tot_nkpts)
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
     integer :: i2,i3,tmp_err
     logical :: perform_inv
     !
     integer, external::MATRIX_resolvant_via_perturbative_inversion
     integer, external::MATRIX_resolvant_via_exact_inversion
     !
     tmp_err=0
     !
     do i2=1,iw_2do
       !
       i3=i2+iw_2start-1
       !
       ! The parallel loop is controlled in the manin body loop 
       ! when a full inversion is performed
       !
       if (.not.do_it_full.and..not.px%element_1D(i3)) cycle
       !
       if (     do_it_full) perform_inv= inv_err(i3)/= 0
       if (.not.do_it_full) perform_inv= inv_err(i3)==-1
       !       
       if (perform_inv) then
         !
         if (do_it_full) then
           !
           inv_err(i3)=MATRIX_resolvant_via_exact_inversion(BS_H_dim,BSS_rhoq0,-1.*Co,&
&                                                           X_epsilon(2,i3),Lo(:,i3,1),BS_mat)
           !
           if (trim(BSE_mode)=='causal') then
             tmp_err=MATRIX_resolvant_via_exact_inversion(BS_H_dim,conjg(BSS_rhoq0),-1.*Co,&
&                                                         X_epsilon(4,i3),Lo(:,i3,2),conjg(BS_mat))
           endif
           !
         else
           !
           inv_err(i3)=MATRIX_resolvant_via_perturbative_inversion(BS_H_dim,BSS_rhoq0,-1.*Co,&
&                                                                  X_epsilon(2,i3),Lo(:,i3,1))
           !
           if (trim(BSE_mode)=='causal') then
             !
             ! Here instead of using conjg(BS_mat) I use conjg of
             ! everything else, the output included (see after the enddo)
             !
             tmp_err=MATRIX_resolvant_via_perturbative_inversion(BS_H_dim,BSS_rhoq0,-1.*conjg(Co),&
&                                                                X_epsilon(4,i3),conjg(Lo(:,i3,2)))
             !
           endif      
           !
         endif
         !
       endif       
       !
       if(inv_err(i3)==0.and.tmp_err==0) inv_err(i3)=0
       if(inv_err(i3)==1.or. tmp_err==1) inv_err(i3)=1
       !
       if (.not.do_it_full) call live_timing(steps=1)
       !
     enddo
     !
     if (.not.do_it_full) then
       if (trim(BSE_mode)=='causal')  X_epsilon(4,:)=conjg(X_epsilon(4,:))
       call live_timing()
     endif
     !
   end subroutine
   !
end subroutine
