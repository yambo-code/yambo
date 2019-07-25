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
 use D_lattice,     ONLY:nsym,sop_inv,sop_tab
 use R_lattice,     ONLY:qindx_B,bz_samp
 use BS,            ONLY:WF_phase,BS_bands,BS_res_K_corr,BS_blk,&
&                        BS_oscillators_alloc,K_CORR_collision,BS_T_grp
 use electrons,     ONLY:n_sp_pol
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
#include<memory.h>
 !
 integer        :: iB
 type(bz_samp)  :: Xk,q
 !
 ! Work Space
 !
 integer     :: i_k_s,i_p_s,i_k_bz,i_p_bz,i_p,i_k,i_Tk,i_Tp,iB_ref,iB_p,&
&               i_v_k,i_v_p,i_c_k,i_c_p,i_k_sp,i_p_sp,i_s_collision,&
                i_Tgrp_k,i_Tgrp_p,i_s_k_p,i_s_star,H_pos(2),i_cycle
 logical     :: l_eval_collision
 !
 if (.not.BS_res_K_corr) return 
 !
#if defined _TIMING
 call timing('T_space CORR Osc.',OPR='start')
#endif
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
 !
 do i_cycle=1,2
   !
   if (i_cycle==2) call BS_oscillators_alloc(.FALSE.,0,.TRUE.,iB)
   !
   BS_blk(iB)%N_oscillators=0
   BS_blk(iB)%O_table=0
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
       if (i_k_sp/=i_p_sp) cycle
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
       l_eval_collision=(i_cycle==2)
       !
       if (BS_blk(iB)%mode=="R") then
         !
         ! v->v'
         !
         i_s_collision=i_s_star
         if (WF_phase(i_p,i_s_k_p,i_v_p,i_p_sp)==-99._SP) i_s_collision=i_s_k_p
         iB_ref=-1
         do iB_p=iB,1,-1
           if(BS_blk(iB_p)%ik/=BS_blk(iB)%ik .or. BS_blk(iB_p)%ip/=BS_blk(iB)%ip) exit
           if(.not.allocated(BS_blk(iB_p)%O_table)) cycle
           if(BS_blk(iB_p)%O_table(1,i_s_collision,i_v_k,i_v_p,i_k_sp)==0) cycle
           iB_ref=iB_p
           exit
         enddo
         if ( iB_ref==-1 ) then
           iB_ref=iB
           BS_blk(iB)%N_oscillators=BS_blk(iB)%N_oscillators+1 
           BS_blk(iB)%O_table(1,i_s_collision,i_v_k,i_v_p,i_k_sp)=BS_blk(iB)%N_oscillators
           BS_blk(iB)%O_table(2,i_s_collision,i_v_k,i_v_p,i_k_sp)=iB_ref
           call EVAL_collision(i_v_k,i_v_p)
         endif
         BS_blk(iB)%O_table(:,i_s_k_p,i_v_k,i_v_p,i_k_sp)=BS_blk(iB_ref)%O_table(:,i_s_collision,i_v_k,i_v_p,i_k_sp)
         !
         ! c->c'
         !
         i_s_collision=i_s_star
         if (WF_phase(i_p,i_s_k_p,i_c_p,i_p_sp)==-99._SP) i_s_collision=i_s_k_p
         iB_ref=-1
         do iB_p=iB,1,-1
           if(BS_blk(iB_p)%ik/=BS_blk(iB)%ik .or. BS_blk(iB_p)%ip/=BS_blk(iB)%ip) exit
           if(.not.allocated(BS_blk(iB_p)%O_table)) cycle
           if(BS_blk(iB_p)%O_table(1,i_s_collision,i_c_k,i_c_p,i_k_sp)==0) cycle
           iB_ref=iB_p
           exit
         enddo
         if ( iB_ref==-1 ) then
           iB_ref=iB
           BS_blk(iB)%N_oscillators=BS_blk(iB)%N_oscillators+1 
           BS_blk(iB)%O_table(1,i_s_collision,i_c_k,i_c_p,i_k_sp)=BS_blk(iB)%N_oscillators
           BS_blk(iB)%O_table(2,i_s_collision,i_c_k,i_c_p,i_k_sp)=iB_ref
           call EVAL_collision(i_c_k,i_c_p)
         endif
         BS_blk(iB)%O_table(:,i_s_k_p,i_c_k,i_c_p,i_k_sp)=BS_blk(iB_ref)%O_table(:,i_s_collision,i_c_k,i_c_p,i_k_sp)
         !
       else if (BS_blk(iB)%mode=="A") then
         !
         ! v->c'
         !
         i_s_collision=i_s_star
         if (WF_phase(i_p,i_s_k_p,i_c_p,i_p_sp)==-99._SP) i_s_collision=i_s_k_p
         iB_ref=-1
         do iB_p=iB,1,-1
           if(BS_blk(iB_p)%ik/=BS_blk(iB)%ik .or. BS_blk(iB_p)%ip/=BS_blk(iB)%ip) exit
           if(.not.allocated(BS_blk(iB_p)%O_table)) cycle
           if(BS_blk(iB_p)%O_table(1,i_s_collision,i_v_k,i_c_p,i_k_sp)==0) cycle
           iB_ref=iB_p
           exit
         enddo
         if ( iB_ref==-1 ) then
           iB_ref=iB
           BS_blk(iB)%N_oscillators=BS_blk(iB)%N_oscillators+1 
           BS_blk(iB)%O_table(1,i_s_collision,i_v_k,i_c_p,i_k_sp)=BS_blk(iB)%N_oscillators
           BS_blk(iB)%O_table(2,i_s_collision,i_v_k,i_c_p,i_k_sp)=iB_ref
           call EVAL_collision(i_v_k,i_c_p)
         endif
         BS_blk(iB)%O_table(:,i_s_k_p,i_v_k,i_c_p,i_k_sp)=BS_blk(iB_ref)%O_table(:,i_s_collision,i_v_k,i_c_p,i_k_sp)
         !
         ! c->v'
         !
         i_s_collision=i_s_star
         if (WF_phase(i_p,i_s_k_p,i_v_p,i_p_sp)==-99._SP) i_s_collision=i_s_k_p
         iB_ref=-1
         do iB_p=iB,1,-1
           if(BS_blk(iB_p)%ik/=BS_blk(iB)%ik .or. BS_blk(iB_p)%ip/=BS_blk(iB)%ip) exit
           if(.not.allocated(BS_blk(iB_p)%O_table)) cycle
           if(BS_blk(iB_p)%O_table(1,i_s_collision,i_c_k,i_v_p,i_k_sp)==0) cycle
           iB_ref=iB_p
           exit
         enddo
         if ( iB_ref==-1 ) then
           iB_ref=iB
           BS_blk(iB)%N_oscillators=BS_blk(iB)%N_oscillators+1 
           BS_blk(iB)%O_table(1,i_s_collision,i_c_k,i_v_p,i_k_sp)=BS_blk(iB)%N_oscillators
           BS_blk(iB)%O_table(2,i_s_collision,i_c_k,i_v_p,i_k_sp)=iB_ref
           call EVAL_collision(i_c_k,i_v_p)
         endif
         BS_blk(iB)%O_table(:,i_s_k_p,i_c_k,i_v_p,i_k_sp)=BS_blk(iB_ref)%O_table(:,i_s_collision,i_c_k,i_v_p,i_k_sp)
         !
       endif
       !
     enddo
   enddo
   !
 enddo
 !
#if defined _TIMING
 call timing('T_space CORR Osc.',OPR='stop')
#endif
 !
 contains
   !
   subroutine EVAL_collision(i_b,i_b_p)
     integer :: i_b,i_b_p
     if (.not.l_eval_collision) return
     K_CORR_collision%is(1)=i_b
     K_CORR_collision%os(1)=i_b_p
     K_CORR_collision%os(3)=i_s_collision
     call scatter_Bamp(K_CORR_collision)
     BS_blk(iB)%O_c(:,BS_blk(iB)%N_oscillators)=K_CORR_collision%rhotw(:)
   end subroutine
end subroutine K_correlation_collisions
