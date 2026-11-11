!
! License-Identifier: GPL
!
! Copyright (C) 2026 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
! headers
!
#include<dev_defs.h>
!
module K_batched_work
 !
 ! Work space of K_correlation_batched_chunk. It is kept between the calls (the allocation
 ! of device memory is expensive) and it grows when needed. Released by K_correlation_batched_free.
 !
 use pars,          ONLY:SP
 !
 implicit none
 !
 integer,     allocatable DEV_ATTR :: d_ipack(:)
 complex(SP), allocatable DEV_ATTR :: d_cpack(:)
 complex(SP), allocatable DEV_ATTR :: O1mat(:,:),O2mat(:,:),Amat(:,:)
 !
 contains
   !
   subroutine ensure_i1(x,n)
#include<y_memory.h>
     integer, allocatable DEV_ATTR, intent(inout) :: x(:)
     integer, intent(in) :: n
     if (allocated(x)) then
       if (size(x)>=n) return
       YAMBO_FREE_GPU(x)
       YAMBO_FREE(x)
     endif
     YAMBO_ALLOC_GPU(x,(max(n,256)+n/4))
   end subroutine ensure_i1
   !
   subroutine ensure_i2(x,m,n)
#include<y_memory.h>
     integer, allocatable DEV_ATTR, intent(inout) :: x(:,:)
     integer, intent(in) :: m,n
     if (allocated(x)) then
       if (size(x,2)>=n) return
       YAMBO_FREE_GPU(x)
       YAMBO_FREE(x)
     endif
     YAMBO_ALLOC_GPU(x,(m,max(n,256)+n/4))
   end subroutine ensure_i2
   !
   subroutine ensure_c1(x,n)
#include<y_memory.h>
     complex(SP), allocatable DEV_ATTR, intent(inout) :: x(:)
     integer, intent(in) :: n
     if (allocated(x)) then
       if (size(x)>=n) return
       YAMBO_FREE_GPU(x)
       YAMBO_FREE(x)
     endif
     YAMBO_ALLOC_GPU(x,(max(n,256)+n/4))
   end subroutine ensure_c1
   !
   subroutine ensure_c2(x,m,n)
#include<y_memory.h>
     complex(SP), allocatable DEV_ATTR, intent(inout) :: x(:,:)
     integer, intent(in) :: m,n
     if (allocated(x)) then
       if (size(x,1)==m.and.size(x,2)>=n) return
       YAMBO_FREE_GPU(x)
       YAMBO_FREE(x)
     endif
     YAMBO_ALLOC_GPU(x,(m,max(n,16)+n/4))
   end subroutine ensure_c2
   !
end module K_batched_work
!
!
subroutine K_correlation_batched_free()
 !
 ! Releases the work space of K_correlation_batched_chunk
 !
 use K_batched_work
#include<y_memory.h>
 !
 !
 if (allocated(d_ipack)) then
   YAMBO_FREE_GPU(d_ipack)
   YAMBO_FREE(d_ipack)
 endif
 if (allocated(d_cpack)) then
   YAMBO_FREE_GPU(d_cpack)
   YAMBO_FREE(d_cpack)
 endif
 if (allocated(O1mat)) then
   YAMBO_FREE_GPU(O1mat)
   YAMBO_FREE(O1mat)
 endif
 if (allocated(O2mat)) then
   YAMBO_FREE_GPU(O2mat)
   YAMBO_FREE(O2mat)
 endif
 if (allocated(Amat)) then
   YAMBO_FREE_GPU(Amat)
   YAMBO_FREE(Amat)
 endif
 !
end subroutine K_correlation_batched_free
!
!
subroutine K_correlation_batched(iq,i_block,i_Tgrp_k,i_Tgrp_p,H_pos_start,mode_now,Xk,q,l_std_alg,qindx_ID_frag,H_c_batched)
 !
 ! Correlation matrix elements H_c of the kernel block i_block, for all the transitions
 ! (i_Tk,i_Tp) of the block. Same quantity that K_correlation_kernel_std/dir return element by element:
 !
 !   H_c(i_Tk,i_Tp) = 4 pi sum_G conj(O2(G)) [W^T O1](G)
 !
 ! O1, O2 are rotated and dephased copies of the stored oscillators, that depend on the bands of the
 ! two transitions (n_k,n_p for O1; m_k,m_p for O2) and on the pair of BZ points (i_k_bz,i_p_bz)
 ! of the transitions. Everything but the bands is therefore the same for the elements of a
 ! "sub-block", i.e. all the transitions at i_k_bz (rows) vs all those at i_p_bz (columns).
 ! For each sub-block:
 !
 !  - O1 and O2 are built only once for each pair of bands (a "column"), and not for each element,
 !  - W^T O1 is evaluated only once per column, with one GEMM for all the columns sharing the same W,
 !  - the elements are dot products between a column of O2 and a column of W^T O1.
 !
 ! The sub-blocks are processed in chunks, to limit the memory used (device memory on GPU).
 !
 use pars,          ONLY:SP,cZERO,cONE
 use BS,            ONLY:BS_blk,BS_T_grp,BS_W_is_diagonal,WF_phase,BS_n_g_W
 use D_lattice,     ONLY:nsym,i_time_rev,sop_inv,sop_tab,i_space_inv
 use R_lattice,     ONLY:bz_samp,qindx_X,qindx_B,qindx_B_load
 use parallel_m,    ONLY:PAR_K_scheme
 use devxlib,       ONLY:devxlib_memcpy_d2h
#include<y_memory.h>
 !
 !
 integer,       intent(in)  :: iq,i_block,i_Tgrp_k,i_Tgrp_p,H_pos_start(2),qindx_ID_frag
 character(1),  intent(in)  :: mode_now
 type(bz_samp), intent(in)  :: Xk,q
 logical,       intent(in)  :: l_std_alg
 complex(SP),   intent(out) :: H_c_batched(BS_T_grp(i_Tgrp_k)%size,BS_T_grp(i_Tgrp_p)%size)
 !
 ! Work Space
 !
 integer :: nTk,nTp,nG,i,j,iseg,i_Tk,i_Tp,iS,jS,i_ch
 logical :: l_coupling
 !
 ! BZ points of the rows (k) and of the columns (p)
 !
 integer :: nks,nps,nS
 integer, allocatable :: ukb(:),upb(:),seg_k(:),seg_p(:),kseg_n(:),kseg_off(:),kseg_idx(:),pseg_n(:),pseg_off(:),pseg_idx(:)
 integer :: ir,ic
 !
 ! Keys of the oscillators: (band,spin) of the rows and of the columns, for O1 and O2
 !
 integer :: nA_k,nB_k,nA_p,nB_p
 integer, allocatable :: kA(:,:),kB(:,:),pA(:,:),pB(:,:)    ! distinct (band,spin) lists, (2,n)
 integer, allocatable :: ra(:),rb(:),ca(:),cb(:)             ! index of each row/column in the lists
 !
 ! Sub-block constants
 !
 integer, allocatable :: sb_k_bz(:),sb_p_bz(:),sb_k_s(:),sb_p_s(:),sb_k_s_m1(:),sb_kp_s(:),&
&                        sb_kmq_s(:),sb_pmq_s(:),sb_kmq_s_m1(:),sb_kp_mq_s(:),sb_kmq_t(:),sb_pmq_t(:),sb_pmq(:),&
&                        sb_iq_W(:),sb_iq_W_s(:),sb_ig_W(:),sb_iq_W_s_mq(:),sb_ig_W_mq(:),sb_order(:)
 !
 integer :: i_k_bz,i_p_bz,i_k_s,i_p_s,i_k_s_m1,i_kp_s,i_kmq_bz,i_pmq_bz,i_kmq,i_pmq,i_kmq_s,i_pmq_s,i_kmq_s_m1,&
&           i_kp_mq_s,i_kmq_t,i_pmq_t,i_k_bz_mem,i_kmq_bz_mem,iq_W_bz,iq_W_bz_mq,ig_W,ig_W_mq,iq_W,iq_W_s,&
&           iq_W_mq,iq_W_s_mq,qindx_tmp(2)
 !
 ! Chunks
 !
 integer :: n_chunk_max,max_cols_sb,max_rows,max_cols,nsub_chunk,i_sub,i_sub_start,i_sub_end,i_o
 integer :: n1,n2,ne
 integer, allocatable :: c_iO1(:),c_sb1(:),c_p1(:,:),c_cj1(:),c_iq1(:)
 integer, allocatable :: c_iO2(:),c_sb2(:),c_p2(:,:),c_cj2(:)
 complex(SP), allocatable :: c_ph1(:),c_ph2(:)
 integer, allocatable :: e_row(:),e_col(:),e_c1(:),e_c2(:)
 integer, allocatable :: id1(:,:),id2(:,:)
 complex(SP), allocatable DEV_ATTR :: H_g(:,:)
 !
 nTk=BS_T_grp(i_Tgrp_k)%size
 nTp=BS_T_grp(i_Tgrp_p)%size
 nG =BS_n_g_W
 l_coupling=(mode_now=="C")
 !
 H_c_batched=cZERO
 !
 ! 1. BZ points (and transitions) of the sub-blocks
 !==================================================
 allocate(ukb(nTk),upb(nTp),seg_k(nTk),seg_p(nTp))
 nks=0
 do i=1,nTk
   j=BS_T_grp(i_Tgrp_k)%table(i,1)
   do iseg=1,nks
     if (ukb(iseg)==j) exit
   enddo
   if (iseg>nks) then
     nks=nks+1
     ukb(nks)=j
   endif
   seg_k(i)=iseg
 enddo
 nps=0
 do i=1,nTp
   j=BS_T_grp(i_Tgrp_p)%table(i,1)
   do iseg=1,nps
     if (upb(iseg)==j) exit
   enddo
   if (iseg>nps) then
     nps=nps+1
     upb(nps)=j
   endif
   seg_p(i)=iseg
 enddo
 nS=nks*nps
 !
 ! Rows (columns) of each BZ point of k (p)
 allocate(kseg_n(nks),kseg_off(nks+1),kseg_idx(nTk),pseg_n(nps),pseg_off(nps+1),pseg_idx(nTp))
 kseg_n=0
 do i=1,nTk
   kseg_n(seg_k(i))=kseg_n(seg_k(i))+1
 enddo
 kseg_off(1)=0
 do iS=1,nks
   kseg_off(iS+1)=kseg_off(iS)+kseg_n(iS)
 enddo
 kseg_n=0
 do i=1,nTk
   kseg_n(seg_k(i))=kseg_n(seg_k(i))+1
   kseg_idx(kseg_off(seg_k(i))+kseg_n(seg_k(i)))=i
 enddo
 pseg_n=0
 do i=1,nTp
   pseg_n(seg_p(i))=pseg_n(seg_p(i))+1
 enddo
 pseg_off(1)=0
 do jS=1,nps
   pseg_off(jS+1)=pseg_off(jS)+pseg_n(jS)
 enddo
 pseg_n=0
 do i=1,nTp
   pseg_n(seg_p(i))=pseg_n(seg_p(i))+1
   pseg_idx(pseg_off(seg_p(i))+pseg_n(seg_p(i)))=i
 enddo
 !
 ! 2. (band,spin) keys of rows and columns
 !=========================================
 ! O1 uses the conduction states of k (rows) and the conduction (resonant) or valence (coupling)
 ! states of p (columns); O2 the valence states of k and the valence (resonant) or conduction (coupling)
 ! states of p. See the calls to K_correlation_kernel_std/dir in K_kernel.
 !
 allocate(kA(2,nTk),kB(2,nTk),pA(2,nTp),pB(2,nTp),ra(nTk),rb(nTk),ca(nTp),cb(nTp))
 nA_k=0 ; nB_k=0 ; nA_p=0 ; nB_p=0
 do i=1,nTk
   call add_key(kA,nA_k,BS_T_grp(i_Tgrp_k)%table(i,3),BS_T_grp(i_Tgrp_k)%table(i,4),ra(i))
   call add_key(kB,nB_k,BS_T_grp(i_Tgrp_k)%table(i,2),BS_T_grp(i_Tgrp_k)%table(i,5),rb(i))
 enddo
 do i=1,nTp
   if (.not.l_coupling) then
     call add_key(pA,nA_p,BS_T_grp(i_Tgrp_p)%table(i,3),BS_T_grp(i_Tgrp_p)%table(i,4),ca(i))
     call add_key(pB,nB_p,BS_T_grp(i_Tgrp_p)%table(i,2),BS_T_grp(i_Tgrp_p)%table(i,5),cb(i))
   else
     call add_key(pA,nA_p,BS_T_grp(i_Tgrp_p)%table(i,2),BS_T_grp(i_Tgrp_p)%table(i,5),ca(i))
     call add_key(pB,nB_p,BS_T_grp(i_Tgrp_p)%table(i,3),BS_T_grp(i_Tgrp_p)%table(i,4),cb(i))
   endif
 enddo
 !
 ! 3. Constants of the sub-blocks
 !================================
 allocate(sb_k_bz(nS),sb_p_bz(nS),sb_k_s(nS),sb_p_s(nS),sb_k_s_m1(nS),sb_kp_s(nS),sb_kmq_s(nS),sb_pmq_s(nS),&
&         sb_kmq_s_m1(nS),sb_kp_mq_s(nS),sb_kmq_t(nS),sb_pmq_t(nS),sb_pmq(nS),sb_iq_W(nS),sb_iq_W_s(nS),&
&         sb_ig_W(nS),sb_iq_W_s_mq(nS),sb_ig_W_mq(nS),sb_order(nS))
 !
 do jS=1,nps
   do iS=1,nks
     i=iS+(jS-1)*nks
     i_k_bz=ukb(iS)
     i_p_bz=upb(jS)
     i_k_s =Xk%sstar(i_k_bz,2)
     i_p_s =Xk%sstar(i_p_bz,2)
     i_k_s_m1=sop_inv(i_k_s)
     i_kp_s  =sop_tab(i_k_s_m1,i_p_s)
     i_kmq_bz=qindx_X(iq,i_k_bz,1)
     i_pmq_bz=qindx_X(iq,i_p_bz,1)
     i_kmq   =Xk%sstar(i_kmq_bz,1)
     i_pmq   =Xk%sstar(i_pmq_bz,1)
     i_kmq_s =Xk%sstar(i_kmq_bz,2)
     i_pmq_s =Xk%sstar(i_pmq_bz,2)
     i_kmq_t =BS_blk(i_block)%kp_table(1,i_kmq)
     i_pmq_t =BS_blk(i_block)%kp_table(2,i_pmq)
     i_kmq_s_m1=sop_inv(i_kmq_s)
     i_kp_mq_s =sop_tab(i_kmq_s_m1,i_pmq_s)
     !
     i_k_bz_mem=PAR_K_scheme%bz_index(i_k_bz)
     if (i_k_bz_mem==0) then
       qindx_tmp=qindx_B_load(i_p_bz,i_k_bz,qindx_ID_frag)
       iq_W_bz=qindx_tmp(1)
       ig_W   =qindx_tmp(2)
     else
       iq_W_bz=qindx_B(i_p_bz,i_k_bz_mem,1)
       ig_W   =qindx_B(i_p_bz,i_k_bz_mem,2)
     endif
     iq_W   =q%sstar( iq_W_bz ,1)
     iq_W_s =q%sstar( iq_W_bz ,2)
     !
     i_kmq_bz_mem=PAR_K_scheme%bz_index(i_kmq_bz)
     if (i_kmq_bz_mem==0) then
       qindx_tmp=qindx_B_load(i_pmq_bz,i_kmq_bz,qindx_ID_frag)
       iq_W_bz_mq=qindx_tmp(1)
       ig_W_mq   =qindx_tmp(2)
     else
       iq_W_bz_mq=qindx_B(i_pmq_bz,i_kmq_bz_mem,1)
       ig_W_mq   =qindx_B(i_pmq_bz,i_kmq_bz_mem,2)
     endif
     iq_W_mq   =q%sstar( iq_W_bz_mq,1)
     iq_W_s_mq =q%sstar( iq_W_bz_mq,2)
     !
     if (iq_W_bz/=iq_W_bz_mq.or.iq_W/=iq_W_mq.or.iq_W_s/=iq_W_s_mq) call error("Wrong transferred momentum")
     !
     if ( (.not.BS_W_is_diagonal) .and. iq_W_s>nsym/(i_time_rev+1) .and. i_space_inv == 0 ) iq_W=q%nibz+iq_W
     !
     sb_k_bz(i)=i_k_bz ; sb_p_bz(i)=i_p_bz ; sb_k_s(i)=i_k_s ; sb_p_s(i)=i_p_s
     sb_k_s_m1(i)=i_k_s_m1 ; sb_kp_s(i)=i_kp_s ; sb_kmq_s(i)=i_kmq_s ; sb_pmq_s(i)=i_pmq_s
     sb_kmq_s_m1(i)=i_kmq_s_m1 ; sb_kp_mq_s(i)=i_kp_mq_s ; sb_kmq_t(i)=i_kmq_t ; sb_pmq_t(i)=i_pmq_t
     sb_pmq(i)=i_pmq ; sb_iq_W(i)=iq_W ; sb_iq_W_s(i)=iq_W_s ; sb_ig_W(i)=ig_W
     sb_iq_W_s_mq(i)=iq_W_s_mq ; sb_ig_W_mq(i)=ig_W_mq
   enddo
 enddo
 !
 ! The sub-blocks are processed ordered by W (iq_W), so that the columns sharing the same W are contiguous
 !
 i_o=0
 do i=minval(sb_iq_W),maxval(sb_iq_W)
   do j=1,nS
     if (sb_iq_W(j)==i) then
       i_o=i_o+1
       sb_order(i_o)=j
     endif
   enddo
 enddo
 !
 ! 4. Chunks
 !===========
 max_rows=0 ; max_cols=0
 do iS=1,nks
   max_rows=max(max_rows,kseg_n(iS))
 enddo
 do jS=1,nps
   max_cols=max(max_cols,pseg_n(jS))
 enddo
 max_cols_sb=max(nA_k*nA_p,nB_k*nB_p)
 n_chunk_max=max(max_cols_sb,(2**27)/(8*nG))
 nsub_chunk=max(1,n_chunk_max/max(1,max_cols_sb))
 !
 allocate(c_iO1(nsub_chunk*nA_k*nA_p),c_sb1(nsub_chunk*nA_k*nA_p),c_p1(3,nsub_chunk*nA_k*nA_p),&
&         c_cj1(nsub_chunk*nA_k*nA_p),c_iq1(nsub_chunk*nA_k*nA_p),c_ph1(nsub_chunk*nA_k*nA_p))
 allocate(c_iO2(nsub_chunk*nB_k*nB_p),c_sb2(nsub_chunk*nB_k*nB_p),c_p2(3,nsub_chunk*nB_k*nB_p),&
&         c_cj2(nsub_chunk*nB_k*nB_p),c_ph2(nsub_chunk*nB_k*nB_p))
 allocate(e_row(nsub_chunk*max_rows*max_cols),e_col(nsub_chunk*max_rows*max_cols),&
&         e_c1(nsub_chunk*max_rows*max_cols),e_c2(nsub_chunk*max_rows*max_cols))
 allocate(id1(nA_k,nA_p),id2(nB_k,nB_p))
 !
 YAMBO_ALLOC_GPU(H_g,(nTk,nTp))
 H_g=cZERO
 !
 i_sub_start=1
 do while (i_sub_start<=nS)
   !
   i_sub_end=min(nS,i_sub_start+nsub_chunk-1)
   n1=0 ; n2=0 ; ne=0
   !
   do i_sub=i_sub_start,i_sub_end
     !
     i=sb_order(i_sub)
     iS=mod(i-1,nks)+1
     jS=(i-1)/nks+1
     id1=0 ; id2=0
     !
     do ic=1,pseg_n(jS)
       i_Tp=pseg_idx(pseg_off(jS)+ic)
       do ir=1,kseg_n(iS)
         i_Tk=kseg_idx(kseg_off(iS)+ir)
         !
         if (H_pos_start(1)+i_Tk>H_pos_start(2)+i_Tp) cycle
         !
         ! Same spin of the two transitions, otherwise the element is zero
         if (kA(2,ra(i_Tk))/=pA(2,ca(i_Tp)).or.kB(2,rb(i_Tk))/=pB(2,cb(i_Tp))) cycle
         !
         if (id1(ra(i_Tk),ca(i_Tp))==0) then
           n1=n1+1
           id1(ra(i_Tk),ca(i_Tp))=n1
           call new_column_1(n1,i)
         endif
         if (id2(rb(i_Tk),cb(i_Tp))==0) then
           n2=n2+1
           id2(rb(i_Tk),cb(i_Tp))=n2
           call new_column_2(n2,i)
         endif
         ne=ne+1
         e_row(ne)=i_Tk
         e_col(ne)=i_Tp
         e_c1(ne)=id1(ra(i_Tk),ca(i_Tp))
         e_c2(ne)=id2(rb(i_Tk),cb(i_Tp))
         !
       enddo
     enddo
     !
   enddo
   !
   if (ne>0) call K_correlation_batched_chunk(l_std_alg,nG,n1,n2,ne,nTk,nTp,c_iO1,c_sb1,c_p1,c_cj1,c_iq1,c_ph1,&
&                                               c_iO2,c_sb2,c_p2,c_cj2,c_ph2,e_row,e_col,e_c1,e_c2,i_block,H_g)
   !
   i_sub_start=i_sub_end+1
   !
 enddo
 !
 call devxlib_memcpy_d2h(H_c_batched,H_g)
 !
 YAMBO_FREE_GPU(H_g)
 YAMBO_FREE(H_g)
 !
 contains
   !
   subroutine add_key(list,n,band,spin,index_in_list)
     integer, intent(inout) :: list(:,:),n
     integer, intent(in)    :: band,spin
     integer, intent(out)   :: index_in_list
     integer :: l
     do l=1,n
       if (list(1,l)==band.and.list(2,l)==spin) then
         index_in_list=l
         return
       endif
     enddo
     n=n+1
     list(1,n)=band
     list(2,n)=spin
     index_in_list=n
   end subroutine add_key
   !
   subroutine new_column_1(c,isb)
     ! Column of O1 for the bands/spin (i_n_k,i_n_p) of the transitions (i_Tk,i_Tp) of the sub-block isb
     integer, intent(in) :: c,isb
     integer :: i_n_k,i_n_p,i_sp,iO,iblk
     complex(SP) :: PHASE
     !
     i_n_k=kA(1,ra(i_Tk))
     i_n_p=pA(1,ca(i_Tp))
     i_sp =kA(2,ra(i_Tk))
     !
     if (l_std_alg) then
       iO  =BS_blk(i_block)%O_table(1,sb_kp_s(isb),1,1,i_n_k,i_n_p,i_sp)
       iblk=BS_blk(i_block)%O_table(2,sb_kp_s(isb),1,1,i_n_k,i_n_p,i_sp)
       PHASE=WF_phase(block_ip(isb),sb_kp_s(isb),i_n_p,pA(2,ca(i_Tp)))
       if (PHASE==cZERO  ) PHASE=1._SP
       if (PHASE==-99._SP) PHASE=1._SP
       c_ph1(c)=PHASE
       c_p1(:,c)=(/sb_iq_W_s(isb),sb_ig_W(isb),sb_k_s_m1(isb)/)
       c_cj1(c)=0
       if (sb_k_s(isb)>nsym/(i_time_rev+1)) c_cj1(c)=1
     else
       iO  =BS_blk(i_block)%O_table(sb_k_s(isb),sb_p_s(isb),1,1,i_n_k,i_n_p,i_sp)
       iblk=i_block
       c_ph1(c)=cONE
       c_p1(:,c)=(/sb_iq_W_s(isb),sb_ig_W(isb),sop_inv(sb_iq_W_s(isb))/)
       c_cj1(c)=0
     endif
     c_iO1(c)=iO
     c_sb1(c)=iblk
     c_iq1(c)=sb_iq_W(isb)
   end subroutine new_column_1
   !
   subroutine new_column_2(c,isb)
     ! Column of O2 for the bands/spin (i_m_k,i_m_p) of the transitions (i_Tk,i_Tp) of the sub-block isb
     integer, intent(in) :: c,isb
     integer :: i_m_k,i_m_p,i_sp,iO,iblk
     complex(SP) :: PHASE
     !
     i_m_k=kB(1,rb(i_Tk))
     i_m_p=pB(1,cb(i_Tp))
     i_sp =kB(2,rb(i_Tk))
     !
     if (l_std_alg) then
       iO  =BS_blk(i_block)%O_table(1,sb_kp_mq_s(isb),sb_kmq_t(isb),sb_pmq_t(isb),i_m_k,i_m_p,i_sp)
       iblk=BS_blk(i_block)%O_table(2,sb_kp_mq_s(isb),sb_kmq_t(isb),sb_pmq_t(isb),i_m_k,i_m_p,i_sp)
       PHASE=WF_phase(sb_pmq(isb),sb_kp_mq_s(isb),i_m_p,pB(2,cb(i_Tp)))
       if (PHASE==cZERO  ) PHASE=1._SP
       if (PHASE==-99._SP) PHASE=1._SP
       c_ph2(c)=PHASE
       c_p2(:,c)=(/sb_iq_W_s_mq(isb),sb_ig_W_mq(isb),sb_kmq_s_m1(isb)/)
       c_cj2(c)=0
       if (sb_kmq_s(isb)>nsym/(i_time_rev+1)) c_cj2(c)=1
     else
       iO  =BS_blk(i_block)%O_table(sb_kmq_s(isb),sb_pmq_s(isb),sb_kmq_t(isb),sb_pmq_t(isb),i_m_k,i_m_p,i_sp)
       iblk=i_block
       c_ph2(c)=cONE
       c_p2(:,c)=(/sb_iq_W_s_mq(isb),sb_ig_W_mq(isb),sop_inv(sb_iq_W_s_mq(isb))/)
       c_cj2(c)=0
     endif
     c_iO2(c)=iO
     c_sb2(c)=iblk
   end subroutine new_column_2
   !
   integer function block_ip(isb)
     integer, intent(in) :: isb
     block_ip=BS_blk(i_block)%ip
   end function block_ip
   !
end subroutine K_correlation_batched
!
!
!
!
subroutine K_correlation_batched_chunk(l_std_alg,nG,n1,n2,ne,nTk,nTp,c_iO1,c_sb1,c_p1,c_cj1,c_iq1,c_ph1,&
&                                      c_iO2,c_sb2,c_p2,c_cj2,c_ph2,e_row,e_col,e_c1,e_c2,i_block,H_g)
 !
 ! Chunk of sub-blocks of K_correlation_batched:
 !  - builds the n1 columns of O1 and the n2 columns of O2,
 !  - applies W to O1,
 !  - evaluates the ne matrix elements H_g(e_row,e_col) = 4 pi conj(O2(:,e_c2)) . (W^T O1)(:,e_c1)
 !
 ! All the integer (complex) parameters are packed in a single array and moved to the device
 ! with a single copy, since the copies of many small arrays are expensive.
 !
 use pars,          ONLY:SP,pi,cZERO,cONE
 use BS,            ONLY:BS_blk,BS_W,BS_W_is_diagonal
 use devxlib,       ONLY:devxlib_xGEMM_gpu,devxlib_memcpy_h2d
 use K_batched_work
#include<y_memory.h>
 !
 !
 logical,     intent(in) :: l_std_alg
 integer,     intent(in) :: nG,n1,n2,ne,nTk,nTp,i_block
 integer,     intent(in) :: c_iO1(*),c_sb1(*),c_p1(3,*),c_cj1(*),c_iq1(*),c_iO2(*),c_sb2(*),c_p2(3,*),c_cj2(*)
 integer,     intent(in) :: e_row(*),e_col(*),e_c1(*),e_c2(*)
 complex(SP), intent(in) :: c_ph1(*),c_ph2(*)
 complex(SP), intent(inout) DEV_ATTR :: H_g(nTk,nTp)
 !
 ! Work Space
 !
 integer :: c,c0,c1,iq_W,nc,nl,sb,i,ig1,ig2,ip,n_int
 integer :: o_iO1,o_p1,o_cj1,o_iO2,o_p2,o_cj2,o_row,o_col,o_c1,o_c2,o_l1,o_l2
 logical, allocatable :: l_done(:)
 integer, allocatable :: ipack(:),g1_sb(:),g1_n(:),g1_o(:),g2_sb(:),g2_n(:),g2_o(:)
 complex(SP), allocatable :: cpack(:)
 !
 ! Offsets in the packed integer array
 o_iO1=1
 o_p1 =o_iO1+n1
 o_cj1=o_p1+3*n1
 o_iO2=o_cj1+n1
 o_p2 =o_iO2+n2
 o_cj2=o_p2+3*n2
 o_row=o_cj2+n2
 o_col=o_row+ne
 o_c1 =o_col+ne
 o_c2 =o_c1+ne
 o_l1 =o_c2+ne
 o_l2 =o_l1+n1
 n_int=o_l2+n2-1
 !
 allocate(ipack(n_int),cpack(n1+n2),l_done(max(n1,n2)),g1_sb(n1),g1_n(n1),g1_o(n1),g2_sb(n2),g2_n(n2),g2_o(n2))
 !
 ipack(o_iO1:o_iO1+n1-1)=c_iO1(1:n1)
 ipack(o_p1:o_p1+3*n1-1)=reshape(c_p1(1:3,1:n1),(/3*n1/))
 ipack(o_cj1:o_cj1+n1-1)=c_cj1(1:n1)
 ipack(o_iO2:o_iO2+n2-1)=c_iO2(1:n2)
 ipack(o_p2:o_p2+3*n2-1)=reshape(c_p2(1:3,1:n2),(/3*n2/))
 ipack(o_cj2:o_cj2+n2-1)=c_cj2(1:n2)
 ipack(o_row:o_row+ne-1)=e_row(1:ne)
 ipack(o_col:o_col+ne-1)=e_col(1:ne)
 ipack(o_c1:o_c1+ne-1)=e_c1(1:ne)
 ipack(o_c2:o_c2+ne-1)=e_c2(1:ne)
 cpack(1:n1)=c_ph1(1:n1)
 cpack(n1+1:n1+n2)=c_ph2(1:n2)
 !
 ! Columns grouped by the block that stores the oscillator, O1 first
 !
 l_done=.false.
 ig1=0
 ip=o_l1-1
 do c0=1,n1
   if (l_done(c0)) cycle
   ig1=ig1+1
   g1_sb(ig1)=c_sb1(c0)
   g1_o(ig1)=ip+1
   nl=0
   do c=c0,n1
     if (c_sb1(c)==g1_sb(ig1)) then
       nl=nl+1
       ip=ip+1
       ipack(ip)=c
       l_done(c)=.true.
     endif
   enddo
   g1_n(ig1)=nl
 enddo
 l_done=.false.
 ig2=0
 ip=o_l2-1
 do c0=1,n2
   if (l_done(c0)) cycle
   ig2=ig2+1
   g2_sb(ig2)=c_sb2(c0)
   g2_o(ig2)=ip+1
   nl=0
   do c=c0,n2
     if (c_sb2(c)==g2_sb(ig2)) then
       nl=nl+1
       ip=ip+1
       ipack(ip)=c
       l_done(c)=.true.
     endif
   enddo
   g2_n(ig2)=nl
 enddo
 !
 call ensure_i1(d_ipack,n_int)
 call ensure_c1(d_cpack,n1+n2)
 call ensure_c2(O1mat,nG,n1)
 call ensure_c2(O2mat,nG,n2)
 call ensure_c2(Amat,nG,n1)
 !
 call h2d_int(d_ipack,ipack,n_int)
 call h2d_cmp(d_cpack,cpack,n1+n2)
 !
 ! O1 and O2, from the stored oscillators (possibly of different blocks)
 !
 do i=1,ig1
   sb=g1_sb(i)
   call K_gather_batched(l_std_alg,nG,g1_n(i),d_ipack(g1_o(i)),d_ipack(o_iO1),d_ipack(o_p1),d_ipack(o_cj1),&
&                        d_cpack(1),O1mat,n1,DEV_VAR(BS_blk(sb)%O_c),size(DEV_VAR(BS_blk(sb)%O_c),1))
 enddo
 do i=1,ig2
   sb=g2_sb(i)
   call K_gather_batched(l_std_alg,nG,g2_n(i),d_ipack(g2_o(i)),d_ipack(o_iO2),d_ipack(o_p2),d_ipack(o_cj2),&
&                        d_cpack(n1+1),O2mat,n2,DEV_VAR(BS_blk(sb)%O_c),size(DEV_VAR(BS_blk(sb)%O_c),1))
 enddo
 !
 ! W^T O1: the columns with the same W are contiguous
 !
 c0=1
 do while (c0<=n1)
   iq_W=c_iq1(c0)
   c1=c0
   do while (c1<n1)
     if (c_iq1(c1+1)/=iq_W) exit
     c1=c1+1
   enddo
   nc=c1-c0+1
   if (BS_W_is_diagonal) then
     call K_pwise_batched(nG,nc,O1mat(1,c0),Amat(1,c0),BS_W(1,1,iq_W))
   else
     call devxlib_xGEMM_gpu('T','N',nG,nc,nG,cONE,BS_W(:,:,iq_W),nG,O1mat(:,c0:c1),nG,cZERO,Amat(:,c0:c1),nG)
   endif
   c0=c1+1
 enddo
 !
 ! Matrix elements
 !
 call K_dot_batched(nG,ne,nTk,nTp,n1,n2,d_ipack(o_row),d_ipack(o_col),d_ipack(o_c1),d_ipack(o_c2),O2mat,Amat,H_g)
 !
 deallocate(ipack,cpack,l_done,g1_sb,g1_n,g1_o,g2_sb,g2_n,g2_o)
 !
 contains
   !
   subroutine h2d_int(dev,host,n)
     use devxlib, ONLY:devxlib_memcpy_h2d
     integer, intent(in) :: n,host(n)
     integer DEV_ATTR :: dev(n)
     call devxlib_memcpy_h2d(dev,host)
   end subroutine h2d_int
   !
   subroutine h2d_cmp(dev,host,n)
     use devxlib, ONLY:devxlib_memcpy_h2d
     integer, intent(in) :: n
     complex(SP), intent(in) :: host(n)
     complex(SP) DEV_ATTR :: dev(n)
     call devxlib_memcpy_h2d(dev,host)
   end subroutine h2d_cmp
   !
end subroutine K_correlation_batched_chunk
!
!
subroutine K_gather_batched(l_std_alg,nG,nl,lst,iO,p,cj,ph,Omat,ncol,O_c,ldO)
 !
 ! Columns lst(1:nl) of Omat(nG,ncol): O(G)=O_c(G',iO) rotated, with the phases, as done element by
 ! element in K_correlation_kernel_std (l_std_alg) and K_correlation_kernel_dir
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:DEV_VAR(G_m_G),DEV_VAR(g_rot)
 !
 implicit none
 !
 logical,     intent(in) :: l_std_alg
 integer,     intent(in) :: nG,nl,ncol,ldO
 integer,     intent(in) DEV_ATTR :: lst(nl),iO(*),p(3,*),cj(*)
 complex(SP), intent(in) DEV_ATTR :: ph(*),O_c(ldO,*)
 complex(SP), intent(inout) DEV_ATTR :: Omat(nG,ncol)
 !
 integer     :: jj,c,g,i_g2,i_g2p,i_g3,i_g4
 complex(SP) :: val
 !
 if (l_std_alg) then
   !
   !DEV_ACC_DEBUG data present(lst,iO,p,cj,ph,O_c,Omat,G_m_G,g_rot)
   !DEV_ACC parallel loop collapse(2) private(c,i_g2,i_g2p,val)
   !DEV_CUF kernel do(2) <<<*,*>>>
   !DEV_OMPGPU target map(present,alloc:lst,iO,p,cj,ph,O_c,Omat,G_m_G,g_rot)
   !DEV_OMPGPU teams loop collapse(2) private(c,i_g2,i_g2p,val)
   !DEV_OMP parallel do default(shared), private(jj,g,c,i_g2,i_g2p,val) collapse(2)
   do jj=1,nl
     do g=1,nG
       c=lst(jj)
       i_g2=DEV_VAR(G_m_G)(DEV_VAR(g_rot)(g,p(1,c)),p(2,c))
       i_g2p=DEV_VAR(g_rot)(i_g2,p(3,c))
       val=O_c(i_g2p,iO(c))*ph(c)
       if (cj(c)==1) val=conjg(val)
       Omat(g,c)=val
     enddo
   enddo
   !DEV_OMPGPU end target
   !DEV_ACC_DEBUG end data
   !
 else
   !
   !DEV_ACC_DEBUG data present(lst,iO,p,O_c,Omat,G_m_G,g_rot)
   !DEV_ACC parallel loop collapse(2) private(c,i_g3,i_g4)
   !DEV_CUF kernel do(2) <<<*,*>>>
   !DEV_OMPGPU target map(present,alloc:lst,iO,p,O_c,Omat,G_m_G,g_rot)
   !DEV_OMPGPU teams loop collapse(2) private(c,i_g3,i_g4)
   !DEV_OMP parallel do default(shared), private(jj,g,c,i_g3,i_g4) collapse(2)
   do jj=1,nl
     do g=1,nG
       c=lst(jj)
       i_g3=DEV_VAR(g_rot)(g,p(1,c))
       i_g4=DEV_VAR(G_m_G)(i_g3,p(2,c))
       Omat(g,c)=O_c(i_g4,iO(c))
     enddo
   enddo
   !DEV_OMPGPU end target
   !DEV_ACC_DEBUG end data
   !
 endif
 !
end subroutine K_gather_batched
!
!
subroutine K_pwise_batched(nG,nc,O,A,W)
 !
 ! A(G,c)=O(G,c)*W(G), W diagonal
 !
 use pars,          ONLY:SP
 !
 implicit none
 !
 integer,     intent(in) :: nG,nc
 complex(SP), intent(in) DEV_ATTR :: O(nG,nc),W(nG)
 complex(SP), intent(inout) DEV_ATTR :: A(nG,nc)
 !
 integer :: c,g
 !
 !DEV_ACC_DEBUG data present(O,A,W)
 !DEV_ACC parallel loop collapse(2)
 !DEV_CUF kernel do(2) <<<*,*>>>
 !DEV_OMPGPU target map(present,alloc:O,A,W)
 !DEV_OMPGPU teams loop collapse(2)
 !DEV_OMP parallel do default(shared), private(c,g) collapse(2)
 do c=1,nc
   do g=1,nG
     A(g,c)=O(g,c)*W(g)
   enddo
 enddo
 !DEV_OMPGPU end target
 !DEV_ACC_DEBUG end data
 !
end subroutine K_pwise_batched
!
!
subroutine K_dot_batched(nG,ne,nTk,nTp,n1,n2,e_row,e_col,e_c1,e_c2,O2mat,Amat,H_g)
 !
 ! H_g(e_row(e),e_col(e)) = 4 pi sum_G conj(O2mat(G,e_c2(e))) Amat(G,e_c1(e))
 !
 use pars,          ONLY:SP,pi
 !
 implicit none
 !
 integer,     intent(in) :: nG,ne,nTk,nTp,n1,n2
 integer,     intent(in) DEV_ATTR :: e_row(ne),e_col(ne),e_c1(ne),e_c2(ne)
 complex(SP), intent(in) DEV_ATTR :: O2mat(nG,n2),Amat(nG,n1)
 complex(SP), intent(inout) DEV_ATTR :: H_g(nTk,nTp)
 !
 integer  :: e,g
 real(SP) :: acc_r,acc_i
 complex(SP) :: v
 !
 !DEV_ACC_DEBUG data present(e_row,e_col,e_c1,e_c2,O2mat,Amat,H_g)
 !DEV_ACC parallel loop private(g,acc_r,acc_i,v)
 !DEV_CUF kernel do(1) <<<*,*>>>
 !DEV_OMPGPU target map(present,alloc:e_row,e_col,e_c1,e_c2,O2mat,Amat,H_g)
 !DEV_OMPGPU teams loop private(g,acc_r,acc_i,v)
 !DEV_OMP parallel do default(shared), private(e,g,acc_r,acc_i,v)
 do e=1,ne
   acc_r=0._SP
   acc_i=0._SP
   do g=1,nG
     v=conjg(O2mat(g,e_c2(e)))*Amat(g,e_c1(e))
     acc_r=acc_r+real(v,SP)
     acc_i=acc_i+aimag(v)
   enddo
   H_g(e_row(e),e_col(e))=cmplx(acc_r,acc_i,SP)*4._SP*pi
 enddo
 !DEV_OMPGPU end target
 !DEV_ACC_DEBUG end data
 !
end subroutine K_dot_batched
