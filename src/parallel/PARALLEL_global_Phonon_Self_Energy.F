!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_global_Phonon_Self_Energy( )
 !
 use R_lattice,     ONLY:nXkbz
 use ELPH,          ONLY:QP_PH_n_G_bands
 use PHEL,          ONLY:PH_Q
 use parallel_int,  ONLY:PARALLEL_assign_chains_and_COMMs,PARALLEL_scheme_initialize
 use parallel_m,    ONLY:COMM_copy,PARs_PH_eh_bands,PARs_PH_Q_ibz,PARs_PH_K_bz,PAR_n_bands,&
&                        PAR_K_range,PAR_Q_ibz_range
 !
#include<memory.h>
 !
 CALL PARALLEL_structure(3,(/"q","k","b"/))
 !
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PARs_PH_Q_ibz%COMM_i,&
&                                        COMM_index_2=PARs_PH_K_bz%COMM_i,&
&                                        COMM_index_3=PARs_PH_eh_bands%COMM_i,&
&                                        COMM_A2A_1=PARs_PH_Q_ibz%COMM_a2a,&
&                                        COMM_A2A_2=PARs_PH_K_bz%COMM_a2a)
 !
 ! COMMs setup
 !
 ! The routine PARALLEL_assign_chains_and_COMMs cannot define COMMUNICATORS for internal
 ! A2A when there is no internal distribution
 !
 if (PARs_PH_eh_bands%COMM_i%n_CPU==1) call COMM_copy(PARs_PH_Q_ibz%COMM_a2a,PARs_PH_K_bz%COMM_a2a)
 !
 !.........................................................................
 !   "q" -> Q-points (IBZ)
 !.........................................................................
 call PARALLEL_scheme_initialize("Q(ibz)","Phonon_Self_Energy",PARs_PH_Q_ibz,PH_Q,TABLE=.TRUE.)
 PAR_Q_ibz_range=PH_Q
 !.........................................................................
 ! Eh bands loop
 !.........................................................................
 call PARALLEL_scheme_initialize("eh-bands","Phonon_Self_Energy",PARs_PH_eh_bands,&
&                                (/QP_PH_n_G_bands(2)-QP_PH_n_G_bands(1)+1/),MATRIX=.TRUE.)
 PAR_n_bands=(/1,(QP_PH_n_G_bands(2)-QP_PH_n_G_bands(1)+1)**2/)
 !.........................................................................
 ! K-points (BZ) for e-h pairs
 !.........................................................................
 call PARALLEL_scheme_initialize("K(bz)","Phonon_Self_Energy",PARs_PH_K_bz,(/nXkbz/),TABLE=.TRUE.)
 PAR_K_range=nXkbz
 !
end subroutine Parallel_global_Phonon_Self_Energy
