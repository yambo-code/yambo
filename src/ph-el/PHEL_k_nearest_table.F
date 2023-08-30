!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): FP
!
subroutine PHEL_k_nearest_table(k)
 !
 ! In the case of custom/random qpoints, prepare a table of 
 ! the k-q indices as ik_m_q ~ ikp where ikp is the closest point
 ! in the k grid.
 !
 use R_lattice,      ONLY:bz_samp,k_the_nearest
 use ELPH,           ONLY:PH_qpt
 use PHEL,           ONLY:KpQ_io_map,PH_Q
 use parallel_m,     ONLY:PARs_PH_K_bz,PARs_PH_Q_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: k
 !
 ! Work Space ...
 !
 integer :: iq,ikbz,i_err
 !
 call live_timing('Finding K-Q_custom indices',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
 !
 call timing("K-Q indexing",OPR='start')
 !
 KpQ_io_map=0
 !
 do iq=PH_Q(1),PH_Q(2)
   !
   if (.not.PARs_PH_Q_ibz%IND%element_1D(iq)) cycle
   !
   do ikbz=1,k%nbz
     !
     if (.not.PARs_PH_K_bz%IND%element_1D(ikbz)) cycle
     !
     ! Notice that k-q_yambo ==> k+q_PH because of the MinusQ convention
     KpQ_io_map(iq,ikbz) = k_the_nearest(k%ptbz(ikbz,:)+PH_qpt(iq,:),k%ptbz,k%nbz,i_err=i_err)
     !
     call live_timing( steps=1 )
     !
   enddo
   !
 enddo
 !
 ! Only PARALLEL_wait in k and q, NOT in bands
 call PP_redux_wait(KpQ_io_map,COMM=PARs_PH_K_bz%COMM_i%COMM)
 call PP_redux_wait(KpQ_io_map,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 !
 call live_timing( )
 !
 call timing("K-Q indexing",OPR='stop')
 !
end subroutine PHEL_k_nearest_table
