!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PARALLEL_WF_distribute_ham(K_index,Bm_index,CLEAN_distr,CLEAN_UP)
 !
 use parallel_m,      ONLY:PP_indexes,ncpu
 use wave_func,       ONLY:states_to_load
 use electrons,       ONLY:n_spin,n_bands
 use R_lattice,       ONLY:nkibz
 use QP_m,            ONLY:QP_n_states,QP_table
 use hamiltonian,     ONLY:B_mat_index,H_ref_bands
 !
#include<y_memory.h>
 !
 type(PP_indexes)   :: K_index
 type(PP_indexes)   :: Bm_index
 logical, intent(in):: CLEAN_distr
 logical, intent(in):: CLEAN_UP
 !
 ! Work Space
 !
 integer :: i_k,i_b,i_bp,i_qp,NB,NK,NBp
 logical :: condition
 logical :: Vl(H_ref_bands(2))
 !
 NK=size(K_index%element_1D)
 NB =n_bands
 !
 if (CLEAN_distr) call PARALLEL_WF_clean_distr(NB,NK,CLEAN_UP)
 !
 if (ncpu==1) return
 !
  do i_k=1,NK
    !
    if (K_index%element_1D(i_k)) then
      !
      Vl=.FALSE. 
      !
      do i_b=H_ref_bands(1),H_ref_bands(2)
        do i_bp=H_ref_bands(1),H_ref_bands(2)
          if (Bm_index%element_1D( B_mat_index(i_b,i_bp,H_ref_bands) ) ) then
            Vl(i_b )=.TRUE.
            Vl(i_bp)=.TRUE.
          endif
        enddo
      enddo
      do i_b=H_ref_bands(1),H_ref_bands(2)
        if (.not.Vl(i_b)) states_to_load(i_b,i_k,:)=.FALSE.
      enddo
    else
      states_to_load(:,i_k,:)=.FALSE.
    endif
    !
  enddo
 !
end subroutine
