!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_WF_distribute(B_index,Bp_index,K_index,B_and_K_index,QP_index,CLEAN_UP)
 !
 use parallel_m,      ONLY:PP_indexes,ncpu
 use wave_func,       ONLY:states_to_load
 use electrons,       ONLY:n_spin,n_bands
 use R_lattice,       ONLY:nkibz
 use QP_m,            ONLY:QP_n_states,QP_table
 !
#include<y_memory.h>
 !
 type(PP_indexes), optional    :: K_index
 type(PP_indexes), optional    :: B_index
 type(PP_indexes), optional    :: B_and_K_index
 type(PP_indexes), optional    :: Bp_index
 type(PP_indexes), optional    :: QP_index
 logical, intent(in), optional :: CLEAN_UP
 !
 ! Work Space
 !
 integer :: i_k,i_b,i_bp,i_qp,NB,NK,NBp
 logical :: condition
 !
 NB =n_bands
 NBp=n_bands
 NK=nkibz 
 if (present(K_index)) then 
   NK=size(K_index%element_1D)
 endif
 if (present(B_index)) then 
   NB=size(B_index%element_1D)
 endif
 if (present(Bp_index)) then 
   NBp=size(Bp_index%element_1D)
 endif
 if (present(B_and_K_index)) then 
   NB=size(B_and_K_index%element_2D,1)
   NK=size(B_and_K_index%element_2D,2)
 endif
 !
 if (present(CLEAN_UP)) call PARALLEL_WF_clean_distr(NB,NK,CLEAN_UP)
 !
 if (ncpu==1) return
 !
 if (present(K_index).and..not.present(B_index)) then
   do i_k=1,NK
     if (.not.K_index%element_1D(i_k)) states_to_load(:,i_k,:)=.FALSE.
   enddo
 else if (.not.present(K_index).and.present(B_index)) then
   do i_b=1,NB
     if (.not.B_index%element_1D(i_b)) states_to_load(i_b,:,:)=.FALSE.
   enddo
 else if (present(K_index).and.present(B_index)) then
   do i_k=1,NK
     !
     if (.not.K_index%element_1D(i_k)) states_to_load(:,i_k,:)=.FALSE.
     !
     do i_b=1,NB
       !
       condition=K_index%element_1D(i_k).and..not.B_index%element_1D(i_b)
       !
       if (present(Bp_index)) then
         if (i_b<=NBp) condition=condition.and..not.Bp_index%element_1D(i_b)
       endif
       !
       if (condition) states_to_load(i_b,i_k,:)=.FALSE.
       !
     enddo
     !
   enddo
 endif
 !
 if (present(B_and_K_index)) then
   do i_k=1,NK
     do i_b=1,NB
       states_to_load(i_b,i_k,:)=B_and_K_index%element_2D(i_b,i_k)
     enddo
   enddo
 endif
 !
 if (present(QP_index)) then
   !
   do i_qp=1,QP_n_states
     !
     if (.not.QP_index%element_1D(i_qp)) cycle
     !
     i_b =QP_table(i_qp,1)
     i_bp=QP_table(i_qp,2)
     i_k =QP_table(i_qp,3)
     !
     states_to_load(i_b ,i_k,:)=.TRUE.
     states_to_load(i_bp,i_k,:)=.TRUE.
     !
   enddo
   !
 endif
 !
end subroutine
!
!
subroutine PARALLEL_WF_clean_distr(NB,NK,CLEAN_UP)
 !
 use parallel_m,      ONLY:ncpu
 use wave_func,       ONLY:states_to_load
 use electrons,       ONLY:n_spin
 !
#include<y_memory.h>
 !
 integer, intent(in) :: NB,NK
 logical, intent(in) :: CLEAN_UP
 !
 YAMBO_FREE(states_to_load)
 YAMBO_ALLOC(states_to_load,(NB,NK,n_spin))
 !
 states_to_load=CLEAN_UP
 if (ncpu==1) states_to_load=.TRUE.
 !
end subroutine
