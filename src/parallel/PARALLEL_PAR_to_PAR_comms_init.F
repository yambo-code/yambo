!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!    
subroutine PARALLEL_PAR_to_PAR_comms_init()
 !
 use parallel_m,    ONLY:COMM_reset,PAR_COM_SLK_INDEX_local
 use matrix,        ONLY:N_PAR_to_PAR_comms,PAR_to_PAR_comm_SAVE,PAR_to_PAR_comm_SAVE_NAME,&
&                        N_MAX_PAR_to_PAR_comms
 !
 implicit none
 !
 integer :: i_comm
 !
 ! PAR to PAR communicators
 !
 if (allocated(PAR_to_PAR_comm_SAVE_NAME)) then
   deallocate(PAR_to_PAR_comm_SAVE_NAME)
   do i_comm=1,N_MAX_PAR_to_PAR_comms
     call COMM_reset(PAR_to_PAR_comm_SAVE(i_comm))
   enddo  
   deallocate(PAR_to_PAR_comm_SAVE)
 endif
 !
 N_MAX_PAR_to_PAR_comms=PAR_COM_SLK_INDEX_local%n_CPU*5
 !                                                    |_ are the potential transfer structures
 !                                                       3  in X_redux, 1 in X_dielectric_matrix, 1 in BSE
 !
 allocate(PAR_to_PAR_comm_SAVE(N_MAX_PAR_to_PAR_comms))
 allocate(PAR_to_PAR_comm_SAVE_NAME(N_MAX_PAR_to_PAR_comms))
 PAR_to_PAR_comm_SAVE_NAME=" "
 N_PAR_to_PAR_comms=0
 !
end subroutine
