!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine PARALLEL_global_DIPOLES(E,Xk,q,ENVIRONMENT)
 !
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_int,        ONLY:IO_and_Messaging_switch
 use openmp,        ONLY:n_threads_DIP,OPENMP_set_threads
 use parallel_int,  ONLY:PARALLEL_index,PARALLEL_assign_chains_and_COMMs,PARALLEL_live_message
 use parallel_m,    ONLY:PARALLEL_n_structures_active,COMM_copy,PAR_build_index,&
&                        PAR_n_c_bands,PAR_n_v_bands
 ! COMMUNICATORS
 use parallel_m,    ONLY:PAR_COM_DIPk_ibz_A2A,PAR_COM_DIPk_ibz_INDEX, &
&                        PAR_COM_CON_INDEX_DIP,PAR_COM_VAL_INDEX_DIP, &
&                        PAR_COM_CON_INDEX_OVLP,PAR_COM_VAL_INDEX_OVLP
 ! IND
 use parallel_m,    ONLY:PAR_IND_DIPk_ibz,PAR_IND_CON_BANDS_DIP,PAR_IND_OVLPk_ibz,&
&                        PAR_IND_VAL_BANDS_DIP,PAR_IND_DIPk_bz,&
&                        PAR_IND_VAL_BANDS_OVLP,PAR_IND_CON_BANDS_OVLP
 ! INDEX
 use parallel_m,    ONLY:PAR_DIPk_ibz_index,PAR_DIPk_bz_index
 ! DIMENSIONS
 use parallel_m,    ONLY:PAR_DIPk_nibz,PAR_DIPk_nbz
 ! ID's
 use parallel_m,    ONLY:PAR_IND_DIPk_bz_ID,PAR_IND_DIPk_ibz_ID,PAR_IND_OVLPk_ibz_ID,&
&                        PAR_IND_CON_BANDS_DIP_ID,PAR_IND_VAL_BANDS_DIP_ID,&
&                        PAR_IND_CON_BANDS_OVLP_ID,PAR_IND_VAL_BANDS_OVLP_ID
 use DIPOLES,       ONLY:eval_OVERLAPS
 use parallel_m,    ONLY:PAR_Q_bz_range,PAR_IND_Q_bz
 !
 implicit none
 !
 type(levels)         :: E
 type(bz_samp)        :: Xk,q
 character(*)         :: ENVIRONMENT
 !
 call PARALLEL_structure(3,(/"k","c","v"/))
 !
 call PARALLEL_assign_chains_and_COMMs(3,COMM_index_1=PAR_COM_DIPk_ibz_INDEX,&
&                                        COMM_index_2=PAR_COM_CON_INDEX_DIP, &
&                                        COMM_index_3=PAR_COM_VAL_INDEX_DIP, &
&                                        COMM_A2A_1=PAR_COM_DIPk_ibz_A2A)
 !
 ! K-points (IBZ)
 !
 call PARALLEL_index(PAR_IND_DIPk_ibz,(/Xk%nibz/),COMM=PAR_COM_DIPk_ibz_INDEX,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_DIPk_ibz_ID=PAR_COM_DIPk_ibz_INDEX%CPU_id
 !
 ! ... indexes
 !
 allocate(PAR_DIPk_bz_index(Xk%nbz))
 call PARALLEL_distribute_BZk_using_IBZk(PAR_COM_DIPk_ibz_INDEX,Xk,PAR_IND_DIPk_ibz,PAR_IND_DIPk_ibz_ID,&
&                                                       PAR_IND_DIPk_bz, PAR_IND_DIPk_bz_ID,&
&                                                       PAR_DIPk_bz_index,PAR_DIPk_nbz)
 !
 allocate(PAR_DIPk_ibz_index(Xk%nibz))
 call PAR_build_index(PAR_IND_DIPk_ibz,Xk%nibz,PAR_DIPk_ibz_index,PAR_DIPk_nibz)
 !
 call PARALLEL_live_message("K(ibz)",ENVIRONMENT=ENVIRONMENT,LOADED=PAR_DIPk_nibz,TOTAL=Xk%nibz,&
&                           NCPU=PAR_COM_DIPk_ibz_INDEX%n_CPU)
 !
 ! I/O privileges
 !
 if (PARALLEL_n_structures_active>1) then
   call IO_and_Messaging_switch("+io_out",CONDITION=PAR_COM_DIPk_ibz_A2A%CPU_id==0)
 else
   call IO_and_Messaging_switch("+io_out",CONDITION=.TRUE.)
 endif
 !
 call OPENMP_set_threads(n_threads_in=n_threads_DIP)
 !
 ! Dipoles conduction bands
 !
 call PARALLEL_index(PAR_IND_CON_BANDS_DIP,(/PAR_n_c_bands(2)/),low_range=(/PAR_n_c_bands(1)/),&
&                    COMM=PAR_COM_CON_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_CON_BANDS_DIP_ID=PAR_COM_CON_INDEX_DIP%CPU_id
 !
 call PARALLEL_live_message("CON bands",ENVIRONMENT=ENVIRONMENT,&
&                           LOADED=PAR_IND_CON_BANDS_DIP%n_of_elements(PAR_COM_CON_INDEX_DIP%CPU_id+1),&
&                           TOTAL=PAR_n_c_bands(2)-PAR_n_c_bands(1)+1,&
&                           NCPU=PAR_COM_CON_INDEX_DIP%n_CPU)
 !
 ! Dipoles valence bands
 !
 call PARALLEL_index(PAR_IND_VAL_BANDS_DIP,(/PAR_n_v_bands(2)/),low_range=(/PAR_n_v_bands(1)/),&
&                    COMM=PAR_COM_VAL_INDEX_DIP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
 PAR_IND_VAL_BANDS_DIP_ID=PAR_COM_VAL_INDEX_DIP%CPU_id
 !
 call PARALLEL_live_message("VAL bands",ENVIRONMENT=ENVIRONMENT,&
&                           LOADED=PAR_IND_VAL_BANDS_DIP%n_of_elements(PAR_COM_VAL_INDEX_DIP%CPU_id+1),&
&                           TOTAL=PAR_n_v_bands(2)-PAR_n_v_bands(1)+1,&
&                           NCPU=PAR_COM_VAL_INDEX_DIP%n_CPU)
 !
 if (eval_OVERLAPS) then
   !
   ! Add the neighboars k-points
   !
   call PARALLEL_add_Q_to_K_list("overlaps",PAR_IND_DIPk_bz,PAR_IND_DIPk_bz_ID,PAR_IND_OVLPk_ibz,PAR_IND_OVLPk_ibz_ID,&
&                                PAR_IND_Q_bz,PAR_COM_DIPk_ibz_INDEX,PAR_Q_bz_range,Xk,q)
 
   ! Overlaps conduction bands
   !
   call COMM_copy(PAR_COM_CON_INDEX_DIP,PAR_COM_CON_INDEX_OVLP)
   call PARALLEL_index(PAR_IND_CON_BANDS_OVLP,(/PAR_n_c_bands(2)/),&
                       COMM=PAR_COM_CON_INDEX_OVLP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_CON_BANDS_OVLP_ID=PAR_COM_CON_INDEX_OVLP%CPU_id
   !
   call PARALLEL_live_message("CON bands (OVLP)",ENVIRONMENT=ENVIRONMENT,&
                              LOADED=PAR_IND_CON_BANDS_OVLP%n_of_elements(PAR_COM_CON_INDEX_OVLP%CPU_id+1),&
                              TOTAL=PAR_n_c_bands(2),&
                              NCPU=PAR_COM_CON_INDEX_OVLP%n_CPU)
   !
   ! Overlaps valence bands
   !
   call COMM_copy(PAR_COM_VAL_INDEX_DIP,PAR_COM_VAL_INDEX_OVLP)
   call PARALLEL_index(PAR_IND_VAL_BANDS_OVLP,(/PAR_n_c_bands(2)/),&
                       COMM=PAR_COM_VAL_INDEX_OVLP,CONSECUTIVE=.TRUE.,NO_EMPTIES=.TRUE.)
   PAR_IND_VAL_BANDS_OVLP_ID=PAR_COM_VAL_INDEX_OVLP%CPU_id
   !
   call PARALLEL_live_message("VAL bands (OVLP)",ENVIRONMENT=ENVIRONMENT,&
                              LOADED=PAR_IND_VAL_BANDS_OVLP%n_of_elements(PAR_COM_VAL_INDEX_OVLP%CPU_id+1),&
                              TOTAL=PAR_n_c_bands(2),&
                              NCPU=PAR_COM_VAL_INDEX_OVLP%n_CPU)
   !
 endif
 !
end subroutine PARALLEL_global_DIPOLES
