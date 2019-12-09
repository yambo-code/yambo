!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV,AF
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
subroutine cutoff_ws(q)
 !
 use pars,          ONLY:DP,SP,PI
 use wave_func,     ONLY:wf_ng
 use R_lattice,     ONLY:ws_cutoff,bare_qpg,nqibz,bz_samp,g_vec
 use D_lattice,     ONLY:a,alat
 use cutoff_ws_m,   ONLY:vcut_type,e2
 use LIVE_t,        ONLY:live_timing
 use openmp,        ONLY:OPENMP_update,n_threads,master_thread,n_threads_now
 use parallel_m,    ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,    ONLY:PARALLEL_index
 implicit none
 type(bz_samp)     ::q
 type(vcut_type)   ::vcut
 !
 ! Work Space
 ! 
 integer            ::iq,ig,i1,nq(3)
 real(DP)           ::atws(3,3)
 real(DP)           ::dummy,rdum(3)
 real(SP)           ::v1(3)
 type(PP_indexes)   ::px
 complex(SP), allocatable :: V_cut(:,:)
 !
 real(dp), external ::vcut_get
 !
 ! build the superperiodicity direct lattice
 !
 nq=1
 rdum=1000
 do iq=1,nqibz
   if(abs(q%pt(iq,1))/=0..and.(abs(q%pt(iq,1))<rdum(1))) rdum(1)=q%pt(iq,1)
   if(abs(q%pt(iq,2))/=0..and.(abs(q%pt(iq,2))<rdum(2))) rdum(2)=q%pt(iq,2)
   if(abs(q%pt(iq,3))/=0..and.(abs(q%pt(iq,3))<rdum(3))) rdum(3)=q%pt(iq,3)
 enddo
 nq=abs(nint(1./rdum))
 do i1=1,3
   if (nq(i1)==0) nq(i1)=1
 enddo
 !
 ! write nq on the report
 do i1=1,3
   atws(:,i1) = a(i1,:)
   atws(:,i1) = atws(:,i1) * nq(i1)
 enddo
 !
 call vcut_init(vcut,atws,real(ws_cutoff,DP))
 call vcut_info(vcut)
 !
 ! workspace and further init
 !
 allocate(V_cut(q%nibz,wf_ng))
 V_cut=0.0_SP
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/q%nibz,wf_ng/))
 !
!$omp parallel default(shared), private(ig,iq,v1,dummy)
 !
 call OPENMP_update(master_thread)
 !
 if (master_thread) call live_timing('WS-cutoff',px%n_of_elements(myid+1)/n_threads_now)
 !
!$omp do collapse(2), schedule(dynamic)
 do ig=1,wf_ng
   do iq=1,nqibz
     !
     if (.not.px%element_2D(iq,ig)) cycle 
     !
     v1=( q%pt(iq,:)+g_vec(ig,:) )*2.0_SP*pi/alat(:)
     !
     dummy=vcut_get(vcut,real(v1,DP))
     V_cut(iq,ig)=real(dummy/(4.0_SP*PI*e2))
     !
     if (master_thread) call live_timing(steps=1)
     !
   enddo
 enddo
!$omp end do
 !
 if(master_thread) call live_timing()
 !
!$omp end parallel
 !
 call OPENMP_update(master_thread)
 !
 ! MPI 2 all
 !
 call PP_redux_wait(V_cut)
 !
 forall (iq=1:q%nibz,ig=1:wf_ng) bare_qpg(iq,ig)=sqrt(1.0_SP/V_cut(iq,ig))
 !
 ! cleanup
 !
 call PP_indexes_reset(px)
 deallocate(V_cut)
 !
end subroutine
