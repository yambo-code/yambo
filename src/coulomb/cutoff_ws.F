!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): DVAF
!
subroutine cutoff_ws(q)
 !
 use pars,          ONLY:DP,SP,PI
 use wave_func,     ONLY:wf_ng
 use R_lattice,     ONLY:ws_cutoff,bare_qpg,nqibz,bz_samp,g_vec
 use D_lattice,     ONLY:a,alat
 use cutoff_ws_m,   ONLY:vcut_type,e2
 use LIVE_t,        ONLY:live_timing
 use openmp,        ONLY:OPENMP_update,n_threads,master_thread
 use parallel_m,    ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index 
 !
 implicit none
 !
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
 call live_timing('WS-cutoff',max(px%n_of_elements(myid+1)/n_threads,1))
 !
!$omp parallel default(shared), private(ig,iq,v1,dummy)
 !
 call OPENMP_update(master_thread)
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
!$omp end parallel
 !
 call OPENMP_update(master_thread)
 !
 call live_timing()
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
