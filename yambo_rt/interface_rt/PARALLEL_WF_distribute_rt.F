!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_WF_distribute_plasma(PLASMA_index,CLEAN_distr,CLEAN_UP)
 !
 use parallel_m,      ONLY:PAR_IND_Q_bz,PAR_Q_bz_index,PP_indexes
 use wave_func,       ONLY:states_to_load
 use electrons,       ONLY:n_bands
 use plasma,          ONLY:EH_gas
 use R_lattice,       ONLY:nkbz,nkibz
 use collision_ext,   ONLY:GW_NEQ_collisions
 !
#include<y_memory.h>
 !
 type(PP_indexes)    :: PLASMA_index
 logical, intent(in):: CLEAN_distr
 logical, intent(in):: CLEAN_UP
 !
 ! Work Space
 !
 integer :: i_J,i_J_bg,iqbz,i_q_mem,i_coll
 integer :: i_k,i_b,i_bp,NB,NK,NBp
 logical :: condition
 !
 NBp=n_bands
 NK=nkibz 
 !
 if (CLEAN_distr) call PARALLEL_WF_clean_distr(NB,NK,CLEAN_UP)
 !
 !if (present(PLASMA_index)) then
   ! 
   do iqbz=1,nkbz
     !
     if (.not.PAR_IND_Q_bz%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_bz_index(iqbz)
     !
     do i_J=1,EH_gas%N_poles(iqbz)
       !
       if (.not.PLASMA_index%element_1D(i_J)) cycle
       !
       do i_J_bg=EH_gas%grid(i_J,i_q_mem,1),EH_gas%grid(i_J,i_q_mem,2)
         !
         i_coll=EH_gas%tab(i_J_bg,i_q_mem,1)
         !
         i_b  = GW_NEQ_collisions%state(i_coll,1)
         i_bp = GW_NEQ_collisions%state(i_coll,2)
         i_k  = GW_NEQ_collisions%state(i_coll,3)
         !
         states_to_load(i_b ,i_k,:)=.TRUE.
         states_to_load(i_bp,i_k,:)=.TRUE.
         !
       enddo
       !
     enddo
     !
   enddo
   !
 !endif
 !
end subroutine
