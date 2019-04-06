!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine K_correlation_collisions(iB,Xk,q)
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
 use R_lattice,     ONLY:qindx_B,bz_samp
 use BS,            ONLY:O_ng,WF_phase,BS_bands,BS_res_K_corr,BS_blk,&
&                        BS_oscillators_alloc,K_CORR_collision,BS_T_grp
 use electrons,     ONLY:n_sp_pol
 use collision_el,  ONLY:elemental_collision_free,elemental_collision_alloc
 use openmp,        ONLY:OPENMP_update,master_thread
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer        :: iB
 type(bz_samp)  :: Xk,q
 !
 ! Work Space
 !
 integer     :: i_k_s,i_p_s,i_k_bz,i_p_bz,i_p,i_k,i_Tk,i_Tp,iB_ref,iB_oscillator,iB_p,&
&               i_v_k,i_v_p,i_c_k,i_c_p,i_k_sp,i_p_sp,i_s_collision,&
                i_Tgrp_k,i_Tgrp_p,i_s_k_p,i_s_star,H_pos(2),i_cycle
 logical, allocatable :: O_todo_table(:,:,:,:)
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
 !
 YAMBO_ALLOC(BS_blk(iB)%O_table,(2,nsym,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol))
 YAMBO_ALLOC(O_todo_table,(nsym,BS_bands(1):BS_bands(2),BS_bands(1):BS_bands(2),n_sp_pol))
 !
 BS_blk(iB)%N_oscillators=0
 BS_blk(iB)%O_table=0
 O_todo_table=.false.
 !
 do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
   !
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     !
     i_k_bz=BS_T_grp(i_Tgrp_k)%table(i_Tk,1)
     i_p_bz=BS_T_grp(i_Tgrp_p)%table(i_Tp,1)
     i_k_s =Xk%sstar(i_k_bz,2)
     i_p_s =Xk%sstar(i_p_bz,2)
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
     i_s_k_p =sop_tab(sop_inv(i_k_s),i_p_s)
     i_s_star=Xk%s_table(i_p,i_s_k_p)
     !
     H_pos = (/sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk,sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp/)
     !
     if (H_pos(1)>H_pos(2)) cycle
     !
     if (BS_blk(iB)%mode=="R") then
       !
       ! v->v'
       !
       i_s_collision=collision_star(i_v_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_table(iB,i_v_k,i_v_p,i_k_sp,i_s_collision)
       !
       ! c->c'
       !
       i_s_collision=collision_star(i_c_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_table(iB,i_c_k,i_c_p,i_k_sp,i_s_collision)
       !
     else if (BS_blk(iB)%mode=="A") then
       !
       ! v->c'
       !
       i_s_collision=collision_star(i_c_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_table(iB,i_v_k,i_c_p,i_k_sp,i_s_collision)
       !
       ! c->v'
       !
       i_s_collision=collision_star(i_v_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_table(iB,i_c_k,i_v_p,i_k_sp,i_s_collision)
       !
     endif
     !
   enddo
 enddo
 !
 call BS_oscillators_alloc(.FALSE.,0,.TRUE.,iB)
 !
#if !defined _CUDA
 !$omp parallel default(shared), private(i_Tk,i_Tp, &
 !$omp &          i_k_bz,i_p_bz,i_k_s,i_p_s, i_v_k,i_c_k,i_v_p,i_c_p, &
 !$omp &          i_k_sp, i_p_sp, i_s_k_p,i_s_star,H_pos,  &
 !$omp &          K_CORR_collision, i_s_collision)
#endif
 !
 call OPENMP_update(master_thread)
 !
 call elemental_collision_alloc(K_CORR_collision,NG=O_ng, TITLE="CORR_COLL")
 !
#if !defined _CUDA
 !$omp do collapse(2), schedule(dynamic)
#endif
 do i_Tk=1,BS_T_grp(i_Tgrp_k)%size
   do i_Tp=1,BS_T_grp(i_Tgrp_p)%size
     !
     i_k_bz=BS_T_grp(i_Tgrp_k)%table(i_Tk,1)
     i_p_bz=BS_T_grp(i_Tgrp_p)%table(i_Tp,1)
     i_k_s =Xk%sstar(i_k_bz,2)
     i_p_s =Xk%sstar(i_p_bz,2)
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
     i_s_k_p =sop_tab(sop_inv(i_k_s),i_p_s)
     i_s_star=Xk%s_table(i_p,i_s_k_p)
     !
     H_pos = (/sum(BS_T_grp(:I_Tgrp_k-1)%size)+i_Tk,sum(BS_T_grp(:I_Tgrp_p-1)%size)+i_Tp/)
     !
     if (H_pos(1)>H_pos(2)) cycle
     !
     K_CORR_collision%is=(/0,i_k,1,i_k_sp/)
     K_CORR_collision%os=(/0,i_p,1,i_p_sp/)
     K_CORR_collision%qs=(/1, q%sstar( qindx_B(i_k_bz,i_p_bz,1) ,1) ,1/)
     !
     if (BS_blk(iB)%mode=="R") then
       !
       ! v->v'
       !
       i_s_collision=collision_star(i_v_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_collision(iB,i_v_k,i_v_p,i_k_sp,i_s_collision,K_CORR_collision)
       !
       ! c->c'
       !
       i_s_collision=collision_star(i_c_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_collision(iB,i_c_k,i_c_p,i_k_sp,i_s_collision,K_CORR_collision)
       !
     else if (BS_blk(iB)%mode=="A") then
       !
       ! v->c'
       !
       i_s_collision=collision_star(i_c_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_collision(iB,i_v_k,i_c_p,i_k_sp,i_s_collision,K_CORR_collision)
       !
       ! c->v'
       !
       i_s_collision=collision_star(i_v_p,i_p,i_p_sp,i_s_star,i_s_k_p)
       call EVAL_collision(iB,i_c_k,i_v_p,i_k_sp,i_s_collision,K_CORR_collision)
       !
     endif
     !
   enddo
 enddo
#if !defined _CUDA
 !$omp end do
#endif
 !
 call elemental_collision_free(K_CORR_collision)
#if !defined _CUDA
 !$omp end parallel
#endif
 !
 call timing('T_space CORR Osc.',OPR='stop')
 !
 contains
   !
   function collision_star(i_b,i_p,i_p_sp,i_s_star,i_s_k_p)
       integer,intent(in) :: i_b, i_p, i_p_sp, i_s_star, i_s_k_p
       integer :: collision_star
       collision_star=i_s_star
       if (WF_phase(i_p,i_s_k_p,i_b,i_p_sp)==-99._SP) collision_star=i_s_k_p
   end function
   !
   subroutine EVAL_table(iB,i_b,i_b_p,i_k_sp,i_s_collision)
     implicit none
     integer, intent(in) :: iB,i_b,i_b_p,i_k_sp,i_s_collision
     integer :: iB_ref, iB_p
     iB_ref=-1
     do iB_p=iB,1,-1
       if(BS_blk(iB_p)%ik/=BS_blk(iB)%ik .or. BS_blk(iB_p)%ip/=BS_blk(iB)%ip) exit
       if(.not.allocated(BS_blk(iB_p)%O_table)) cycle
       if(BS_blk(iB_p)%O_table(1,i_s_collision,i_b,i_b_p,i_k_sp)==0) cycle
       iB_ref=iB_p
       exit
     enddo
     if ( iB_ref==-1 ) then
       iB_ref=iB
       BS_blk(iB)%N_oscillators=BS_blk(iB)%N_oscillators+1 
       BS_blk(iB)%O_table(1,i_s_collision,i_b,i_b_p,i_k_sp)=BS_blk(iB)%N_oscillators
       BS_blk(iB)%O_table(2,i_s_collision,i_b,i_b_p,i_k_sp)=iB
       O_todo_table(i_s_collision,i_b,i_b_p,i_k_sp)=.true.
     endif
     BS_blk(iB)%O_table(:,i_s_k_p,i_b,i_b_p,i_k_sp)=BS_blk(iB_ref)%O_table(:,i_s_collision,i_b,i_b_p,i_k_sp)
   end subroutine
   !
   subroutine EVAL_collision(iB,i_b,i_b_p,i_k_sp,i_s_collision,isc)
     use collision_el,  ONLY:elemental_collision
     implicit none
     integer,intent(in) :: iB,i_b,i_b_p,i_s_collision, i_k_sp
     type(elemental_collision),intent(inout) :: isc
     integer :: iB_oscillator
#ifdef _CUDA
     integer :: ig,ngrho
     complex(SP), pointer, device :: rhotw_d(:),O_c_d(:,:)
#endif
     if (.not.O_todo_table(i_s_collision,i_b,i_b_p,i_k_sp)) return 
     iB_oscillator = BS_blk(iB)%O_table(1,i_s_collision,i_b,i_b_p,i_k_sp)
     isc%is(1)=i_b
     isc%os(1)=i_b_p
     isc%os(3)=i_s_collision
     call scatter_Bamp(isc)
#ifdef _CUDA
     ngrho=isc%ngrho
     rhotw_d=>isc%rhotw_d
     O_c_d=>BS_blk(iB)%O_c_d
     !
     !$cuf kernel do (1)
     do ig =1,ngrho
       O_c_d(ig,iB_oscillator)=rhotw_d(ig)
     enddo
#else
     BS_blk(iB)%O_c(:,iB_oscillator)=isc%rhotw(:)
#endif
     O_todo_table(i_s_collision,i_b,i_b_p,i_k_sp) = .false.
   end subroutine
   !
end subroutine K_correlation_collisions
