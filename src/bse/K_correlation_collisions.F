!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine K_correlation_collisions(iq,iB,Xk,q)
 !
 ! This routine evaluates the correlation collisions:
 !
 !  <n k1_bz |exp iG.r|m k2_bz> = <n k1_ibz s1 |exp iG.r|m k2_ibz s2>  =
 !                                <n k1_ibz |exp iG.r|m k2_ibz s1^{-1} x s2 > 
 !
 ! now, in general, s1^{-1} x s2 is not a symmetry in the star of k2_ibz, so we
 ! define and use the table Xk%s_table:
 !
 ! is_in_the_star_of_k2=Xk%s_table(k2,s1^{-1} x s2 )
 !
 ! to get
 !
 !  <n k1_bz |exp iG.r|m k2_bz> =  <n k1_ibz |exp iG.r|m k2_ibz  is_in_the_star_of_k2>*PHASE
 !
 ! where |k2 ib s1^{-1} x s2 > = PHASE * |k2 ib is_in_the_star_of_k2>
 !
 use pars,          ONLY:SP
 use parallel_m,    ONLY:myid
 use D_lattice,     ONLY:nsym,sop_inv,sop_tab
 use R_lattice,     ONLY:qindx_B,qindx_X,bz_samp!,G_m_G
 use BS,            ONLY:O_ng,WF_phase,BS_bands,BS_res_K_corr,BS_blk,&
&                        BS_oscillators_alloc,K_CORR_collision,BS_T_grp
 use electrons,     ONLY:n_sp_pol
 use collision_el,  ONLY:elemental_collision_free,elemental_collision_alloc
 use openmp,        ONLY:OPENMP_update,master_thread
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer,       intent(in)  :: iq,iB
 type(bz_samp), intent(in)  :: Xk,q
 !
 ! Work Space
 !
 integer     :: i_k_s,i_p_s,i_k_bz,i_p_bz,i_k,i_p,i_kp_s,i_star_s,&
&               i_kmq_s,i_pmq_s,i_kmq_bz,i_pmq_bz,i_kmq,i_pmq,i_kp_mq_s,i_s_mq_star,&
&               i_Tk,i_Tp,i_Tgrp_k,i_Tgrp_p,H_pos(2),&
&               i_v_k,i_v_p,i_c_k,i_c_p,i_k_sp,i_p_sp,i_s_collision,iq_W,iq_W_bz,&!iq_W_s,&
&               N_kmq,N_pmq,i_kmq_t,i_pmq_t
 !
 logical, allocatable :: O_todo_table(:,:,:,:,:,:)
 !
 if (.not.BS_res_K_corr) return 
 !
 call timing('T_space CORR Osc.',OPR='start')
 !
 ! Every block is at k (ibz) fixed
 !
 i_Tgrp_k=BS_blk(iB)%iT_k
 i_Tgrp_p=BS_blk(iB)%iT_p
 i_k     =BS_blk(iB)%ik
 i_p     =BS_blk(iB)%ip
 !
 ! Table allocation
 !------------------
 YAMBO_ALLOC(BS_blk(iB)%kp_table,(2,Xk%nibz))
 BS_blk(iB)%kp_table=0
 !
 N_kmq=0
 N_pmq=0
 !
 do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     !
     i_k_bz=BS_T_grp(i_Tgrp_k)%table(i_Tk,1) ! k
     i_p_bz=BS_T_grp(i_Tgrp_p)%table(i_Tp,1) ! p = k - q_W
     !
     i_kmq_bz=qindx_X(iq,i_k_bz,1) ! k-q
     i_pmq_bz=qindx_X(iq,i_p_bz,1) ! p-q = k - q_W -q
     !
     i_kmq   =Xk%sstar(i_kmq_bz,1)
     i_pmq   =Xk%sstar(i_pmq_bz,1)
     !
     if( (BS_blk(iB)%kp_table(1,i_kmq)/=0) .and. (BS_blk(iB)%kp_table(2,i_pmq)/=0) ) cycle
     !
     if( BS_blk(iB)%kp_table(1,i_kmq)==0 ) then
       N_kmq=N_kmq+1
       BS_blk(iB)%kp_table(1,i_kmq)=N_kmq
     endif
     !
     if( BS_blk(iB)%kp_table(2,i_pmq)==0 ) then
       N_pmq=N_pmq+1
       BS_blk(iB)%kp_table(2,i_pmq)=N_pmq
     endif
     !
   enddo
 enddo
 !
 YAMBO_ALLOC(BS_blk(iB)%O_table,(2,nsym,N_kmq,N_pmq,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol))
 YAMBO_ALLOC(O_todo_table,(nsym,N_kmq,N_pmq,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol))
 !
 BS_blk(iB)%N_oscillators=0
 BS_blk(iB)%O_table=0
 O_todo_table=.false.
 !
 do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
   !
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     !
     H_pos = (/sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk,sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp/)
     !
     if (H_pos(1)>H_pos(2)) cycle
     !
     ! k and p indexes
     !
     i_k_bz=BS_T_grp(i_Tgrp_k)%table(i_Tk,1) ! k
     i_p_bz=BS_T_grp(i_Tgrp_p)%table(i_Tp,1) ! p = k - q_W
     !
     i_k_s =Xk%sstar(i_k_bz,2)
     i_p_s =Xk%sstar(i_p_bz,2)
     !
     i_kp_s  =sop_tab(sop_inv(i_k_s),i_p_s)
     i_star_s=Xk%s_table(i_p,i_kp_s)
     !
     ! k-q and p-q indexes
     !
     i_kmq_bz=qindx_X(iq,i_k_bz,1) ! k-q
     i_pmq_bz=qindx_X(iq,i_p_bz,1) ! p-q = k - q_W -q
     !
     !ig_kmq=qindx_X(iq,i_k_bz,2)
     !ig_pmq=qindx_X(iq,i_p_bz,2)
     !ig_kp_mq=G_m_G(ig_kmq,ig_pmq)
     !
     i_kmq   =Xk%sstar(i_kmq_bz,1)
     i_pmq   =Xk%sstar(i_pmq_bz,1)
     !
     i_kmq_s =Xk%sstar(i_kmq_bz,2)
     i_pmq_s =Xk%sstar(i_pmq_bz,2)
     !      
     i_kmq_t=BS_blk(iB)%kp_table(1,i_kmq)
     i_pmq_t=BS_blk(iB)%kp_table(2,i_pmq)
     !
     i_kp_mq_s =sop_tab(sop_inv(i_kmq_s),i_pmq_s)
     i_s_mq_star=Xk%s_table(i_pmq,i_kp_mq_s)
     !
     ! q_W indexes
     !
     iq_W_bz=qindx_B(i_k_bz,i_p_bz,1)
     !ig_W   =qindx_B(i_k_bz,i_p_bz,2)
     !
     iq_W   =q%sstar( iq_W_bz ,1)
     !iq_W_s =q%sstar( iq_W_bz ,2)
     !
     ! Bands and spin indexes
     !
     i_v_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,2)
     i_c_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,3)
     !
     i_v_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,2)
     i_c_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,3)
     !
     i_k_sp=BS_T_grp(i_Tgrp_k)%table(i_Tk,4)
     i_p_sp=BS_T_grp(i_Tgrp_p)%table(i_Tp,4)
     !
     if(i_k_sp/=i_p_sp) cycle
     !
     if (BS_blk(iB)%mode=="R") then
       !
       ! v->v', i.e. k-q -> p-q=k-q_W-q transitions
       !
       i_s_collision=collision_star(i_pmq,i_kp_mq_s,i_v_p,i_p_sp,i_s_mq_star,i_kp_mq_s)
       call EVAL_table(iB,i_kmq_t,i_pmq_t,i_v_k,i_v_p,i_k_sp,i_kp_mq_s,i_s_collision)
       !
       ! c->c', i.e. k -> p=k-q_w transitions
       !
       i_s_collision=collision_star(i_p,i_kp_s,i_c_p,i_p_sp,i_star_s,i_kp_s)
       call EVAL_table(iB,1,1,i_c_k,i_c_p,i_k_sp,i_kp_s,i_s_collision)
       !
       !
     else if (BS_blk(iB)%mode=="C") then
       !
       ! v->c', i.e. k-q -> p-q=k-q_W-q transitions
       !
       i_s_collision=collision_star(i_pmq,i_kp_mq_s,i_c_p,i_p_sp,i_s_mq_star,i_kp_mq_s)
       call EVAL_table(iB,i_kmq_t,i_pmq_t,i_v_k,i_c_p,i_k_sp,i_kp_mq_s,i_s_collision)
       !
       ! c->v', i.e. k -> p=k-q_W transitions
       !
       i_s_collision=collision_star(i_p,i_kp_s,i_v_p,i_p_sp,i_star_s,i_kp_s)
       call EVAL_table(iB,1,1,i_c_k,i_v_p,i_k_sp,i_kp_s,i_s_collision)
       !
     endif
     !
   enddo
 enddo
 !
 call BS_oscillators_alloc(.FALSE.,0,.TRUE.,iB)
 !
 !$omp parallel default(shared), private(i_Tk,i_Tp, &
 !$omp &          i_k_bz,i_p_bz,i_k_s,i_p_s, i_v_k,i_c_k,i_v_p,i_c_p, &
 !$omp &          i_k_sp, i_p_sp, i_s_k_p,i_s_star,H_pos,  &
 !$omp &          K_CORR_collision, i_s_collision)
 !
 call OPENMP_update(master_thread)
 !
 call elemental_collision_alloc(K_CORR_collision,NG=O_ng, TITLE="CORR_COLL")
 !
 !$omp do collapse(2), schedule(dynamic)
 do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
   !
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     !
     H_pos = (/sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk,sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp/)
     !
     if (H_pos(1)>H_pos(2)) cycle
     !
     ! k and p indexes
     !
     i_k_bz=BS_T_grp(i_Tgrp_k)%table(i_Tk,1) ! k
     i_p_bz=BS_T_grp(i_Tgrp_p)%table(i_Tp,1) ! p = k - q_W
     !
     i_k_s =Xk%sstar(i_k_bz,2)
     i_p_s =Xk%sstar(i_p_bz,2)
     !
     i_kp_s  =sop_tab(sop_inv(i_k_s),i_p_s)
     i_star_s=Xk%s_table(i_p,i_kp_s)
     !
     ! k-q and p-q indexes
     !
     i_kmq_bz=qindx_X(iq,i_k_bz,1) ! k-q
     i_pmq_bz=qindx_X(iq,i_p_bz,1) ! p-q = k - q_W -q
     !
     !ig_kmq=qindx_X(iq,i_k_bz,2)
     !ig_pmq=qindx_X(iq,i_p_bz,2)
     !ig_kp_mq=G_m_G(ig_kmq,ig_pmq)
     !
     i_kmq   =Xk%sstar(i_kmq_bz,1)
     i_pmq   =Xk%sstar(i_pmq_bz,1)
     !
     i_kmq_s =Xk%sstar(i_kmq_bz,2)
     i_pmq_s =Xk%sstar(i_pmq_bz,2)
     !      
     i_kmq_t=BS_blk(iB)%kp_table(1,i_kmq)
     i_pmq_t=BS_blk(iB)%kp_table(2,i_pmq)
     !
     i_kp_mq_s =sop_tab(sop_inv(i_kmq_s),i_pmq_s)
     i_s_mq_star=Xk%s_table(i_pmq,i_kp_mq_s)
     !
     ! q_W indexes
     !
     iq_W_bz=qindx_B(i_k_bz,i_p_bz,1)
     !ig_W   =qindx_B(i_k_bz,i_p_bz,2)
     !
     iq_W   =q%sstar( iq_W_bz ,1)
     !iq_W_s =q%sstar( iq_W_bz ,2)
     !
     ! Bands and spin indexes
     !
     i_v_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,2)
     i_c_k=BS_T_grp(i_Tgrp_k)%table(i_Tk,3)
     !
     i_v_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,2)
     i_c_p=BS_T_grp(i_Tgrp_p)%table(i_Tp,3)
     !
     i_k_sp=BS_T_grp(i_Tgrp_k)%table(i_Tk,4)
     i_p_sp=BS_T_grp(i_Tgrp_p)%table(i_Tp,4)
     !
     if(i_k_sp/=i_p_sp) cycle
     !
     if (BS_blk(iB)%mode=="R") then
       !
       ! v->v', i.e. k-q -> p-q=k-q_W-q transitions
       !
       !
       i_s_collision=collision_star(i_pmq,i_kp_mq_s,i_v_p,i_p_sp,i_s_mq_star,i_kp_mq_s)
       call EVAL_collision(iB,i_kmq,i_pmq,i_kmq_t,i_pmq_t,i_k_sp,i_p_sp,i_v_k,i_v_p,i_s_collision,K_CORR_collision)
       !
       ! c->c', i.e. k -> p=k-q_w transitions
       !
       i_s_collision=collision_star(i_p,i_kp_s,i_c_p,i_p_sp,i_star_s,i_kp_s)
       call EVAL_collision(iB,i_k,i_p,1,1,i_k_sp,i_p_sp,i_c_k,i_c_p,i_s_collision,K_CORR_collision)
       !
     else if (BS_blk(iB)%mode=="C") then
       !
       ! v->c', i.e. k-q -> p-q=k-q_W-q transitions
       !
       i_s_collision=collision_star(i_pmq,i_kp_mq_s,i_c_p,i_p_sp,i_s_mq_star,i_kp_mq_s)
       call EVAL_collision(iB,i_kmq,i_pmq,i_kmq_t,i_pmq_t,i_k_sp,i_p_sp,i_v_k,i_c_p,i_s_collision,K_CORR_collision)
       !
       ! c->v', i.e. k -> p=k-q_W transitions
       !
       i_s_collision=collision_star(i_p,i_kp_s,i_v_p,i_p_sp,i_star_s,i_kp_s)
       call EVAL_collision(iB,i_k,i_p,1,1,i_k_sp,i_p_sp,i_c_k,i_v_p,i_s_collision,K_CORR_collision)
       !
     endif
     !
   enddo
 enddo
 !$omp end do
 !
 call elemental_collision_free(K_CORR_collision)
 !$omp end parallel
 !
 call timing('T_space CORR Osc.',OPR='stop')
 !
 contains
   !
   function collision_star(ik,is,ib,i_sp,i_s_star,i_s_new)
       implicit none
       integer,intent(in) :: ik,is,ib,i_sp,i_s_star,i_s_new
       integer :: collision_star
       collision_star=i_s_star
       if (WF_phase(ik,is,ib,i_sp)==-99._SP) collision_star=i_s_new
   end function
   !
   subroutine EVAL_table(iB,iki,iko,ibi,ibo,i_sp,i_s_ki_ko,i_s_collision)
     implicit none
     integer, intent(in) :: iB,iki,iko,ibi,ibo,i_sp,i_s_ki_ko,i_s_collision
     integer :: iB_ref, iB_p
     iB_ref=-1
     do iB_p=iB,1,-1
       if(BS_blk(iB_p)%ik/=BS_blk(iB)%ik .or. BS_blk(iB_p)%ip/=BS_blk(iB)%ip) exit
       if(.not.allocated(BS_blk(iB_p)%O_table)) cycle
       if(BS_blk(iB_p)%O_table(1,i_s_collision,iki,iko,ibi,ibo,i_sp)==0) cycle
       iB_ref=iB_p
       exit
     enddo
     if ( iB_ref==-1 ) then
       iB_ref=iB
       BS_blk(iB)%N_oscillators=BS_blk(iB)%N_oscillators+1 
       BS_blk(iB)%O_table(1,i_s_collision,iki,iko,ibi,ibo,i_sp)=BS_blk(iB)%N_oscillators
       BS_blk(iB)%O_table(2,i_s_collision,iki,iko,ibi,ibo,i_sp)=iB
       O_todo_table(i_s_collision,iki,iko,ibi,ibo,i_sp)=.true.
     endif
     BS_blk(iB)%O_table(:,i_s_ki_ko,iki,iko,ibi,ibo,i_sp)= &
&      BS_blk(iB_ref)%O_table(:,i_s_collision,iki,iko,ibi,ibo,i_sp)
   end subroutine
   !
   subroutine EVAL_collision(iB,iki,iko,ikit,ikot,i_spi,i_spo,ibi,ibo,i_s_collision,isc)
     use collision_el,  ONLY:elemental_collision
     implicit none
     integer,intent(in) :: iB,iki,iko,ikit,ikot,ibi,ibo,i_spi,i_spo,i_s_collision
     type(elemental_collision),intent(inout) :: isc
     integer :: iB_oscillator
     if (.not.O_todo_table(i_s_collision,ikit,ikot,ibi,ibo,i_spi)) return 
     isc%is=(/ibi,iki,1,i_spi/)
     isc%os=(/ibo,iko,i_s_collision,i_spo/)
     isc%qs=(/1,iq_W,1/)
     call scatter_Bamp(isc)
     iB_oscillator = BS_blk(iB)%O_table(1,i_s_collision,ikit,ikot,ibi,ibo,i_spi)
     BS_blk(iB)%O_c(:,iB_oscillator)=isc%rhotw(:)
     O_todo_table(i_s_collision,ikit,ikot,ibi,ibo,i_spi) = .false.
   end subroutine
   !
end subroutine K_correlation_collisions
