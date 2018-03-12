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
subroutine cutoff_box(q,is_cut)
 !
 ! Vc(q,G)=1/(Vdl Nq)\sum_{q' G'} V(q'+G') F(q'+G',q+G) [3D BOX] 
 !
 ! Note that q\in BZ and
 !
 ! F(v,w)= \prod_i 2 sin[(v_i-w_i)L_i/2]/(v_i-w_i)
 !
 use pars,          ONLY:SP,pi,cZERO
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
 implicit none
 logical      ::is_cut(3)
 type(bz_samp)::q
 !
 ! Work Space
 !
 integer      ::iq,ig,iqbz,ig_p,is,ig_r,iqibz,i1,nq_looped
 real(SP)     ::v1(3),v2(3),V_bare(q%nibz,ng_closed),vol_factor
 complex(SP)  ::V_cut(q%nibz,wf_ng),V_cut_tmp
 type(PP_indexes) ::px
 !
 call PP_indexes_reset(px)
 !
 ! Bare interaction
 !
 ! AF: iku_v_norm is thread safe
 !
#if defined _OPENMP
!$omp parallel do default(shared), private(ig,iq)
#endif
 do ig=1,ng_closed
   do iq=1,q%nibz
     if (iq==1.and.ig==1) cycle
     V_bare(iq,ig)=1./iku_v_norm(q%pt(iq,:)+g_vec(ig,:))**2
   enddo
 enddo
#if defined _OPENMP
!$omp end parallel do
#endif
 V_bare(1,1)=7.7956*(RL_vol/real(q%nbz))**(-2._SP/3._SP)
 !
 ! RIM contribution
 !
 if (allocated(RIM_qpg).and.RIM_anisotropy==0.) then
   CUTOFF_plus_RIM=.true.
   !
#if defined _OPENMP
!$omp parallel do default(shared), private(ig,iq)
#endif
   do ig=1,RIM_ng
     do iq=1,q%nibz
       V_bare(iq,ig)=RIM_qpg(iq,ig,ig)*DL_vol*real(q%nbz,SP)/2._SP
     enddo
   enddo
#if defined _OPENMP
!$omp end parallel do
#endif
   !
 endif
 !
 ! BOX
 !
 call k_ibz2bz(q,'i',.true.)
 !
 V_cut=cZERO
 !
 call PARALLEL_index(px,(/q%nibz,wf_ng/))
 !
 call OPENMP_set_threads( )
 !
#if defined _OPENMP
!$omp parallel num_threads(n_threads) default(shared), &
!$omp &    private(ig,iq, v1,nq_looped, iqbz,iqibz,is,i1,V_cut_tmp, &
!$omp &            ig_p,ig_r,v2)
#endif
 ! 
 ! Local Threads involved
 !
 call OPENMP_update(master_thread)
 !
 call live_timing('Box',px%n_of_elements(myid+1)/n_threads)
 !
#if defined _OPENMP
!$omp do
#endif
 do ig=1,wf_ng
   do iq=1,q%nibz
     !
     if (.not.px%element_2D(iq,ig)) cycle
     !
     v1= ( q%pt(iq,:)+g_vec(ig,:) )*2.*pi/alat(:)
     !
     nq_looped=0
     !
     q_loop: do iqbz=1,q%nbz
       iqibz=q%sstar(iqbz,1)
       is   =q%sstar(iqbz,2)
       !
       do i1=1,3
         if (.not.is_cut(i1).and.&
&            abs(q%pt(iq,i1)-q%ptbz(iqbz,i1))>k_iku_zero(i1)) cycle q_loop
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
&              abs(g_vec(ig,i1)-g_vec(ig_p,i1))>G_iku_zero(i1)) cycle g_loop
         enddo
         !
         ig_r=g_rot( ig_p, sop_inv(is) )
         v2= ( q%ptbz(iqbz,:)+g_vec(ig_p,:) )*2._SP*pi/alat(:)
         !
         V_cut_tmp=V_cut_tmp+V_bare(iqibz,ig_r)*F_box(v1,v2)
         !
       enddo g_loop
       !
       V_cut(iq,ig)=V_cut(iq,ig)+V_cut_tmp
       !
     enddo q_loop
     !
     V_cut(iq,ig)=V_cut(iq,ig)/nq_looped
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
#if defined _OPENMP
!$omp end do
!$omp end parallel
#endif
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
#if defined _OPENMP
!$omp parallel do default(shared), private(ig,iq)
 do ig=1,wf_ng
   do iq=1,q%nibz
     bare_qpg(iq,ig)=sqrt(vol_factor/V_cut(iq,ig))
   enddo
 enddo
!$omp end parallel do
#else
 forall (iq=1:q%nibz,ig=1:wf_ng) bare_qpg(iq,ig)=sqrt(vol_factor/V_cut(iq,ig))
#endif
 !
 call k_ibz2bz(q,'d',.true.)
 !
 contains
   !
   real(SP) function F_box(v1,v2)
     !
     real(SP) :: v1(3),v2(3)
     integer  :: i_s
     !
     F_box=1.
     do i_s=1,3
       if (.not.is_cut(i_s)) cycle
       if (abs(v1(i_s)-v2(i_s))<=1.E-5_SP) then
         F_box=F_box*box_length(i_s)
       else
         F_box=F_box*2._SP*sin((v1(i_s)-v2(i_s))*box_length(i_s)/2.)/(v1(i_s)-v2(i_s))
       endif
     enddo
   end function
   !
end subroutine
