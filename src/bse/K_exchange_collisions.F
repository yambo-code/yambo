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
subroutine K_exchange_collisions(iq,Xk,i_T_grp)
 !
 use pars,           ONLY:cZERO
 use D_lattice,      ONLY:nsym,i_time_rev,sop_inv
 use BS,             ONLY:BS_T_grp,K_EXCH_collision,BS_oscillators_alloc,BS_n_g_exch,L_kind
 use R_lattice,      ONLY:g_rot,qindx_X,bz_samp
 use timing_m,       ONLY:timing
 use openmp,         ONLY:OPENMP_update,master_thread
 use collision_el,   ONLY:elemental_collision_free,elemental_collision_alloc
 !
 implicit none
 !
 type(bz_samp) :: Xk
 integer       :: iq,i_T_grp
 !
 ! Work Space
 !
 integer :: i_T_el,i_T_el_p,N_T_el_p,i_c,i_v,i_spin,i_k_bz,i_k,i_s,i_g0,i_g1,i_g2,i_p_bz,i_g_p,i_p,&
&           i_sp,i_T_el_last_with_identity_sym,i_T_grp_last_with_identity_sym,i_T_grp_p
 !
 ! Oscillators already done ?
 !
 if (allocated( BS_T_grp(i_T_grp)%O_x )) return
 !
 call timing('T_space EXX Osc.',OPR='start')
 !
 ! Allocate
 !
 call elemental_collision_free(K_EXCH_collision)
 call BS_oscillators_alloc(.TRUE.,i_T_grp,.FALSE.,0)
 !
#ifdef __NOTNOW
 !$omp parallel default(shared), private( K_EXCH_collision, &
 !$omp &        i_T_el,i_k_bz,i_k,i_s, i_p_bz,i_p,i_sp, i_v,i_c,i_spin, &
 !$omp &        i_T_grp_last_with_identity_sym, i_T_el_last_with_identity_sym, &
 !$omp &        i_T_grp_p, N_T_el_p, i_T_el_p, i_g1, i_g2)
#endif
 !
 call OPENMP_update(master_thread)
 !
 call elemental_collision_alloc(K_EXCH_collision,NG=BS_n_g_exch,TITLE="EXCH_COLL")
 !
#ifdef __NOTNOW
 !$omp do schedule(dynamic)
#endif
 T_loop: do i_T_el=1,BS_T_grp(i_T_grp)%size
   !
   i_k_bz=BS_T_grp(i_T_grp)%table(i_T_el,1)
   i_k   =Xk%sstar(i_k_bz,1)
   i_s   =Xk%sstar(i_k_bz,2)
   !
   i_p_bz=qindx_X(iq,i_k_bz,1)
   i_g_p =qindx_X(iq,i_k_bz,2)
   !
   i_p   =Xk%sstar(i_p_bz,1)
   i_sp  =Xk%sstar(i_p_bz,2)
   !
   i_v   =BS_T_grp(i_T_grp)%table(i_T_el,2)
   i_c   =BS_T_grp(i_T_grp)%table(i_T_el,3)
   i_spin=BS_T_grp(i_T_grp)%table(i_T_el,4)
   !
   K_EXCH_collision%is=(/i_c,i_k,i_s,i_spin/)
   K_EXCH_collision%os=(/i_v,i_p,i_sp,i_spin/)
   K_EXCH_collision%qs=(/i_g_p,iq,1/)
   !
   if (iq==1) then
     !
     ! G==0 term
     !
     if(trim(L_kind)=="full") BS_T_grp(i_T_grp)%O_x(1,i_T_el)=BS_T_grp(i_T_grp)%dipoles_R(i_T_el)
     if(trim(L_kind)=="Lbar") BS_T_grp(i_T_grp)%O_x(1,i_T_el)=cZERO
     !
     ! G/=0 terms
     !
     i_T_grp_last_with_identity_sym=-1
     i_T_el_last_with_identity_sym=-1
     if (i_s>1) then
       do i_T_grp_p=i_T_grp,1,-1
         if ( .not.allocated(BS_T_grp(i_T_grp_p)%O_x)      ) cycle
         if ( BS_T_grp(i_T_grp_p)%ik/=BS_T_grp(i_T_grp)%ik ) exit
         if ( i_T_grp_p==i_T_grp ) N_T_el_p=i_T_el-1
         if ( i_T_grp_p/=i_T_grp ) N_T_el_p=BS_T_grp(i_T_grp_p)%size
         do i_T_el_p=N_T_el_p,1,-1
           if ( all( BS_T_grp(i_T_grp_p)%table(i_T_el_p,:)==(/Xk%k_table(i_k,1),i_v,i_c,i_spin/)) ) then
             i_T_grp_last_with_identity_sym=i_T_grp_p
             i_T_el_last_with_identity_sym =i_T_el_p
             exit
           endif
         enddo
       enddo
     endif
     !
     if  (i_s==1.or.i_T_grp_last_with_identity_sym<0) then
       !
       call scatter_Bamp(K_EXCH_collision)
       !
       !$omp parallel do default(shared), private(i_g1)
       do i_g1=2,BS_n_g_exch
         BS_T_grp(i_T_grp)%O_x(i_g1,i_T_el)=K_EXCH_collision%rhotw(i_g1)
       enddo
       !$omp end parallel do
     else
       !$omp parallel do default(shared), private(i_g1,i_g2)
       do i_g1=2,BS_n_g_exch
         i_g2=g_rot(i_g1,sop_inv(i_s))
         BS_T_grp(i_T_grp)%O_x(i_g1,i_T_el)=BS_T_grp(i_T_grp_last_with_identity_sym)%O_x(i_g2,i_T_el_last_with_identity_sym)
         if (i_s>nsym/(i_time_rev+1)) BS_T_grp(i_T_grp)%O_x(i_g1,i_T_el)=conjg(BS_T_grp(i_T_grp)%O_x(i_g1,i_T_el))
       enddo
       !$omp end parallel do
     endif
     !
   else
     !
     call scatter_Bamp(K_EXCH_collision)
     !
     BS_T_grp(i_T_grp)%O_x(1,i_T_el)=cZERO
     if(trim(L_kind)=="full") i_g0=1
     if(trim(L_kind)=="Lbar") i_g0=2
     !$omp parallel do default(shared), private(i_g1)
     do i_g1=i_g0,BS_n_g_exch
       BS_T_grp(i_T_grp)%O_x(i_g1,i_T_el)=K_EXCH_collision%rhotw(i_g1)
     enddo
     !$omp end parallel do
     !
   endif
   !
 enddo T_loop
#ifdef __NOTNOW
 !$omp end do
#endif
 !
 call elemental_collision_free(K_EXCH_collision) 
#ifdef _NOTNOW
 !$omp end parallel
#endif
 !
 call timing('T_space EXX Osc.',OPR='stop')
 !
end subroutine K_exchange_collisions
