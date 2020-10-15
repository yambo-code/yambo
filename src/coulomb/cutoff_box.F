!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine cutoff_box(q,is_cut)
 !
 ! Vc(q,G)=1/(Vdl Nq)\sum_{q' G'} V(q'+G') F(q'+G',q+G) [3D BOX] 
 !
 ! Note that q\in BZ and
 !
 ! F(v,w)= \prod_i 2 sin[(v_i-w_i)L_i/2]/(v_i-w_i)
 !
 use pars,          ONLY:SP,pi,rZERO,cI
 use wave_func,     ONLY:wf_ng
 use vec_operate,   ONLY:iku_v_norm,v_norm
 use D_lattice,     ONLY:DL_vol,alat,sop_inv,a
 use R_lattice,     ONLY:bare_qpg,RL_vol,RIM_ng,RIM_qpg,bz_samp,&
&                        box_length,g_vec,g_rot,ng_closed,RIM_anisotropy,&
&                        CUTOFF_plus_RIM
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 use LIVE_t,        ONLY:live_timing
 use zeros,         ONLY:k_iku_zero,G_iku_zero
 use openmp,        ONLY:OPENMP_update,n_threads,master_thread,OPENMP_set_threads
 !
#include<memory.h>
 !
 logical      ::is_cut(3)
 type(bz_samp)::q
 !
 ! Work Space
 !
 integer      :: iq,ig,iqbz,ig_p,is,ig_r,iqibz,i1,nq_looped
 integer      :: ig_first,ig_last
 real(SP)     :: v1(3),v2(3),vol_factor,V_cut_tmp
 type(PP_indexes)  :: px
 real(SP),    allocatable :: V_bare(:,:),V_cut(:,:)
 real(SP),    allocatable :: g_vec_r(:,:),q_pt_r(:,:),q_ptbz_r(:,:)
 real(SP)     :: k_zero(3),G_zero(3),F_box(3)
 !
 call PP_indexes_reset(px)
 !
 YAMBO_ALLOC(V_bare,(ng_closed,q%nibz))
 YAMBO_ALLOC(V_cut,(wf_ng,q%nibz))
 !
 ! Bare interaction
 !
 ! AF: iku_v_norm is thread safe
 !
 !$omp parallel do default(shared), private(ig,iq), collapse(2)
 do iq=1,q%nibz
   do ig=1,ng_closed
     if (iq==1.and.ig==1) cycle
     V_bare(ig,iq)=1._SP/iku_v_norm(q%pt(iq,:)+g_vec(ig,:))**2
   enddo
 enddo
 !$omp end parallel do
 V_bare(1,1)=7.7956_SP*(RL_vol/real(q%nbz))**(-2._SP/3._SP)
 !
 ! RIM contribution
 !
 if (allocated(RIM_qpg).and.RIM_anisotropy==0.) then
   CUTOFF_plus_RIM=.true.
   !
   !$omp parallel do default(shared), private(ig,iq), collapse(2)
   do iq=1,q%nibz
     do ig=1,RIM_ng
       V_bare(ig,iq)=RIM_qpg(iq,ig,ig)*DL_vol*real(q%nbz,SP)/2._SP
     enddo
   enddo
   !$omp end parallel do
   !
 endif
 !
 ! BOX
 !
 call k_ibz2bz(q,'i',.true.)
 !
 ! memory handling
 !
 YAMBO_ALLOC(g_vec_r,(3,max(wf_ng,ng_closed)))
 YAMBO_ALLOC(q_pt_r,(3,q%nibz))
 YAMBO_ALLOC(q_ptbz_r,(3,q%nbz))
 !
 do ig=1,max(wf_ng,ng_closed)
   g_vec_r(:,ig)=g_vec(ig,:)*2._SP*pi/alat(:)
 enddo
 do iq=1,q%nibz
   q_pt_r(:,iq)=q%pt(iq,:)*2._SP*pi/alat(:)
 enddo
 do iq=1,q%nbz
   q_ptbz_r(:,iq)=q%ptbz(iq,:)*2._SP*pi/alat(:)
 enddo
 k_zero(:)=k_iku_zero(:)*2._SP*pi/alat(:)
 G_zero(:)=G_iku_zero(:)*2._SP*pi/alat(:)
 !
 ! main loop
 !
 V_cut=rZERO
 !
 call PARALLEL_index(px,(/wf_ng/),CONSECUTIVE=.true.)
 ig_first=px%first_of_1D(myid+1)
 ig_last=px%last_of_1D(myid+1)
 !
 call OPENMP_set_threads( )
 !
!$omp parallel num_threads(n_threads) default(shared), &
!$omp &    private(ig,iq, v1,nq_looped, iqbz,iqibz,is,i1,V_cut_tmp, &
!$omp &            ig_p,ig_r,v2,F_box)
 ! 
 ! Local Threads involved
 !
 call OPENMP_update(master_thread)
 !
 call live_timing('Box',px%n_of_elements(myid+1)/n_threads)
 !
!$omp do schedule(dynamic)
 do ig=ig_first,ig_last
   !
   do iq=1,q%nibz
     !
     nq_looped=0
     !
     q_loop: do iqbz=1,q%nbz
       iqibz=q%sstar(iqbz,1)
       is   =q%sstar(iqbz,2)
       !
       do i1=1,3
         if (.not.is_cut(i1).and.&
&            abs(q_pt_r(i1,iq)-q_ptbz_r(i1,iqbz))>k_zero(i1)) cycle q_loop
       enddo
       !
       nq_looped=nq_looped+1
       !
       V_cut_tmp = 0.0_SP
       !
       g_loop: do ig_p=1,ng_closed
         !
         do i1=1,3
           if (.not.is_cut(i1).and.&
&              abs(g_vec_r(i1,ig)-g_vec_r(i1,ig_p))>G_zero(i1)) cycle g_loop
         enddo
         !
         ig_r=g_rot( ig_p, sop_inv(is) )
         v1= ( q_pt_r(:,iq)+g_vec_r(:,ig) )
         v2= ( q_ptbz_r(:,iqbz)+g_vec_r(:,ig_p) )
         !
         F_box=1._SP
         do i1=1,3
           if (.not.is_cut(i1)) cycle
           if (abs(v1(i1)-v2(i1))<=1.E-5_SP) then
             F_box(i1)=box_length(i1)
           else
             F_box(i1)=2._SP*sin((v1(i1)-v2(i1))*box_length(i1)/2._SP)/(v1(i1)-v2(i1))
           endif
         enddo
         !
         V_cut_tmp=V_cut_tmp+V_bare(ig_r,iqibz)*F_box(1)*F_box(2)*F_box(3)
         !
       enddo g_loop
       !
       V_cut(ig,iq)=V_cut(ig,iq)+V_cut_tmp
       !
     enddo q_loop
     !
     V_cut(ig,iq)=V_cut(ig,iq)/nq_looped
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
!$omp end do
!$omp end parallel
 !
 YAMBO_FREE(g_vec_r)
 YAMBO_FREE(q_pt_r)
 YAMBO_FREE(q_ptbz_r)
 !
 call OPENMP_update(master_thread)
 !
 call live_timing()
 !
 ! MPI 2 all
 !
 call PP_redux_wait(V_cut)
 !
 ! Volume Factor
 !
 if (all(is_cut)) then ! BOX XYZ
   vol_factor=DL_vol
 else if (all((/is_cut(1),is_cut(2),.not.is_cut(3)/)))  then ! BOX XY
   vol_factor=DL_vol/v_norm(a(3,:))
 else if (all((/is_cut(1),.not.is_cut(2),is_cut(3)/)))  then ! BOX XZ
   vol_factor=DL_vol/v_norm(a(2,:))
 else if (all((/.not.is_cut(1),is_cut(2),is_cut(3)/)))  then ! BOX YZ
   vol_factor=DL_vol/v_norm(a(1,:))
 else if (all((/is_cut(1),.not.is_cut(2),.not.is_cut(3)/)))  then ! BOX X
   vol_factor=v_norm(a(1,:))
 else if (all((/.not.is_cut(1),is_cut(2),.not.is_cut(3)/)))  then ! BOX Y
   vol_factor=v_norm(a(2,:))
 else if (all((/.not.is_cut(1),.not.is_cut(2),is_cut(3)/)))  then ! BOX Z
   vol_factor=v_norm(a(3,:))
 endif
 !
!$omp parallel do default(shared), private(ig,iq), collapse(2)
 do iq=1,q%nibz
   do ig=1,wf_ng
     !
     ! take care of the sign of the cmplx root
     if (V_cut(ig,iq) >=0.0_SP) then
        bare_qpg(iq,ig)=sqrt(vol_factor/V_cut(ig,iq))
     else
        bare_qpg(iq,ig)=-cI*sqrt(-(vol_factor/V_cut(ig,iq)))
     endif
   enddo
 enddo
!$omp end parallel do
 !
 call k_ibz2bz(q,'d',.true.)
 call PP_indexes_reset(px)
 !
 YAMBO_FREE(V_bare)
 YAMBO_FREE(V_cut)
 call PP_indexes_reset(px)
 !
end subroutine
