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
 use pars,           ONLY:SP,pi,HARTREE
 use drivers,        ONLY:l_ph_corr
 use X_m,            ONLY:X_epsilon
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,BS_not_const_eh_f,&
&                         BS_K_coupling,BS_eh_table,BS_eh_E,BS_bands,Kernel_by_V,&
&                         BSS_uses_RIM,BSS_er,BSS_dr,BSS_n_freqs,BSS_skip_full_inv
 use memory_m,       ONLY:mem_est
 use parser_m,       ONLY:parser
 use electrons,      ONLY:spin_occ,levels,BZ_RIM_nbands
 use frequency,      ONLY:w_samp,W_reset,W_merge
 use R_lattice,      ONLY:d3k_factor,q_norm,bz_samp
 use com,            ONLY:isec,warning,msg
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset,ncpu
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,NONE,VERIFY,OP_WR_CL
#if defined PJ_PH
 use ELPH,           ONLY:Rnd_Qpts_at,Rnd_Qpts_corresp,EXCPH_mat
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
 integer          ::i1,i2,ik_bz,iv,ic,iw,icv,of_mode,i_sp,ik_ibz
 integer, external::pert_inversion,eval_ELPH_K
 complex(SP)      ::Co,QP_Z,QP_width
 complex(SP), allocatable  ::Lo(:,:)
 !
 ! ... frequencies
 !
 integer,  allocatable ::inv_err(:),iw_full(:),nw_full(:)
 logical,  allocatable ::p_inv_2do(:)
 integer    ::full_grp,nw,nw_read,nw_conv
 logical    ::full_diago
 !
#if defined _DOUBLE
 complex(DP)::zdotu,zdotc
#else
 complex(SP)::cdotu,cdotc
#endif
#if defined PJ_PH
 complex(SP)::WS(BS_K_dim,BS_K_dim)
 logical    ::add_ELPH_K
#endif
 !
 ! I/O
 !
 integer           ::io_err,ID
 type(w_samp)      ::W_disk
 integer, external ::ioBSS_invert
 integer, allocatable     ::inv_err_disk(:),disk_freq_table(:)
 complex(SP), allocatable ::eps_disk(:)
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
 call parser('SkipInv',BSS_skip_full_inv)
 !
 ! I/O
 !
 call W_reset(W_disk)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 io_err=ioBSS_invert(iq,1,(/0/),(/(0.,0.)/),W_disk,ID)
 !
 if (io_err==0) then
   allocate(inv_err_disk(W_disk%n(1)),eps_disk(W_disk%n(1)),&
&           disk_freq_table(W_disk%n(1)+W%n(1)))
   !
   call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,2/),ID=ID)
   io_err=ioBSS_invert(iq,W_disk%n(1),inv_err_disk,eps_disk,W_disk,ID)
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
   allocate(inv_err(W%n(1)),iw_full(W%n(1)),nw_full(W%n(1)),p_inv_2do(W%n(1)))
   p_inv_2do=.TRUE.
   X_epsilon=(0.,0.)
   inv_err=1
   !
   nw_read=0
   nw_conv=0
   do iw=1,W%n(1)
     if (disk_freq_table(iw)>0) then
       inv_err(iw)=inv_err_disk(disk_freq_table(iw))
       X_epsilon(2,iw)=eps_disk(disk_freq_table(iw))/real(ncpu,SP)
       nw_read=nw_read+1
       if (inv_err(iw)==1) p_inv_2do(iw)=.false.
       if (inv_err(iw)==0) nw_conv=nw_conv+1
     endif
   enddo
   call msg('rs','[BSE INV] Frequency read      [o/o]:',real(nw_read)/real(W%n(1))*100.)
   call msg('rs','[BSE INV] Converged frequency [o/o]:',real(nw_conv)/real(W%n(1))*100.)
   deallocate(inv_err_disk,eps_disk,disk_freq_table)
 else
   allocate(inv_err(W%n(1)),iw_full(W%n(1)),nw_full(W%n(1)),p_inv_2do(W%n(1)))
   inv_err=1
   p_inv_2do=.TRUE.
   X_epsilon=(0.,0.)
 endif
 !
 !  mem est
 !
#if defined PJ_PH
 call mem_est("Lo WS",(/BS_K_dim*W%n(1),2*BS_K_dim**2/))
#else
 call mem_est("Lo WS",(/BS_K_dim*W%n(1),BS_K_dim**2/))
#endif
 !
 ! Kernel loading
 !
 call Kernel_by_V(iq)
 !
 forall(i1=1:BS_K_dim) BS_mat(i1,i1)=BS_mat(i1,i1)-BS_eh_E(i1)
 !
#if defined PJ_PH
 !
 ! SAVE BS_mat & calculaye ELPH_K
 !
 WS=BS_mat
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
 Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/(q_norm(1))**2
 !
 allocate(Lo(BS_K_dim,W%n(1)))
 Lo=(0.,0.)
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
     if (associated(E%W)) QP_width=E%W(ic,ik_ibz,i_sp)-E%W(iv,ik_ibz,i_sp)
     if (associated(E%Z)) QP_Z=E%Z(ic,ik_ibz,i_sp)*E%Z(iv,ik_ibz,i_sp)
     Lo(i1,:)=QP_Z/(W%p(:)-BS_eh_E(i1)-(0.,1.)*QP_width)
   else
     call X_bare_RIM_Gf(1,(/ik_bz,iv,ic,i_sp/),(/1,W%n(1)/),W,E,k,Lo(i1,:),"r")
   endif
   X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*Lo(i1,:)*Co
   !
   call live_timing(steps=1)
   !
 enddo
 X_epsilon(3,:)=X_epsilon(3,:)/real(ncpu,SP)
 call live_timing()
 !
 ! Perturbative Invertion
 !========================
 !
 full_diago=.false.
 call pp_indexes_reset(px)
 call par_indexes(px,(/W%n(1)/))
 call live_timing('Freq loop [pert]',px%stps(myid+1))
 call perform_invertion(W%n(1),W%p,1,X_epsilon(2,:))
 call pp_redux_wait(inv_err,imode=2)
 !
 ! I/O [after pert invertion]
 !
 call pp_redux_wait(X_epsilon)
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=ioBSS_invert(iq,W%n(1),inv_err,X_epsilon(2,:),W,ID)
 !
 ! Full Invertion
 !=================
 !
 ! First I define the group of frequencies not converged
 !
 iw=-1
 nw=0
 iw_full=-1
 nw_full=0
 full_grp=0
 do i1=1,W%n(1)
   if (inv_err(i1)/= 0.and.iw<0) iw=i1
   if (inv_err(i1)/= 0.and.iw>0) nw=nw+1
   if (inv_err(i1)== 0.and.iw>0) then
     full_grp=full_grp+1
     iw_full(full_grp)=iw
     nw_full(full_grp)=nw
     iw=-1
     nw=0
   endif
 enddo
 !
 if (full_grp>0.and..not.BSS_skip_full_inv) then
   !
   call msg('rs','[BSE INV] Non converged frequency groups:',full_grp)
   !
   X_epsilon(2:3,:)=X_epsilon(2:3,:)/real(ncpu,SP)
   !
   ! Then loop on the groups performing full inversion
   !
   call pp_indexes_reset(px)
   call par_indexes(px,(/full_grp/))
   call live_timing('Freq loop [full]',px%stps(myid+1))
   full_diago=.true.
   do i1=1,full_grp
     if (.not.px%i1p(i1)) cycle
     iw=iw_full(i1)+nw_full(i1)/2
     call perform_invertion(1,W%p(iw),iw,X_epsilon(2,iw))
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
 call msg('rs','[BSE INV] Converged frequency [o/o]:',real(nw_conv)/real(W%n(1))*100.)
 !
 ! Try a polinomial interpolation on the null values
 !
 call K_eps_interpolate(W%n(1),real(W%p),inv_err)
 call pp_redux_wait()
 !
 ! I/O [after full invertion & interpolation]
 !
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2/),ID=ID)
 io_err=ioBSS_invert(iq,W%n(1),inv_err,X_epsilon(2,:),W,ID)
 !
 X_epsilon(2,:)=1.+X_epsilon(2,:)
 X_epsilon(1,:)=W%p(:)
 !
 call K_dump_to_o_file(iq,of_mode)
 !
 ! CLEAN
 !
 deallocate(inv_err,p_inv_2do)
 call pp_indexes_reset(px)
 deallocate(BS_mat,iw_full,nw_full,Lo)
 call mem_est("BS_mat Lo WS")
#if defined PJ_PH
 if (allocated(EXCPH_mat)) then
   deallocate(Rnd_Qpts_corresp,Rnd_Qpts_at,EXCPH_mat)
   call mem_est("EXCPH_mat")
 endif
#endif
 !
contains
  !
  subroutine perform_invertion(nW_,W_,iw_start,e2)
    integer     :: nW_,iw_start
    complex(SP) :: W_(nW_),e2(nW_)
    !
    integer :: i2
    !
    do i2=1,nW_
      !
#if defined PJ_PH
      if (add_ELPH_K) then
        BS_mat=WS
        call static2dyn_ELPH_K(E,k,q,W_(i2),BS_mat)
      endif
#endif
      !
      if(.not.full_diago) then
        if (.not.px%i1p(i2)) cycle
        if (.not.p_inv_2do(i2)) then
          call live_timing(steps=1)
          cycle
        endif
      endif
      !
      if (inv_err(i2+iw_start-1)/=0) &
&       inv_err(i2+iw_start-1)=pert_inversion(BS_K_dim,BSS_rhoq0,-1.*Co,&
&                              e2(i2),Lo(:,i2+iw_start-1),BS_mat,full_diago)
      !
      if (.not.full_diago) call live_timing(steps=1)
      !
    enddo
    if (.not.full_diago) call live_timing()
    !
  end subroutine
  !
end subroutine
