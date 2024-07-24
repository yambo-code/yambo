!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
!
!
module residuals
 !
 implicit none
 !
 interface
   !
   subroutine K_diago_residuals(i_BS_mat, BS_Energies, BS_Res_right, BS_VR, &
&                  neigs_this_cpu, neig_shift, BS_Res_left, BS_VL, BS_Overlapp) 
     use pars,           ONLY:SP,cZERO
     use wrapper_omp,    ONLY:Vstar_dot_V_omp,V_dot_V_omp
     use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
     use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
     use LIVE_t,         ONLY:live_timing
     use X_m,            ONLY:global_gauge
     use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f, BSS_eh_f_RES
     use BS,             ONLY:BS_K_coupling,BS_K_dim,BS_H_dim
     implicit none
     integer,intent(in)      :: i_BS_mat, neigs_this_cpu, neig_shift
     complex(SP), allocatable :: BS_Energies(:)
     complex(SP), pointer     :: BS_Res_right(:)
     complex(SP), target,allocatable     :: BS_VR(:,:)
     complex(SP), pointer     :: BS_Res_left(:)
     complex(SP), target, allocatable     :: BS_VL(:,:)
     complex(SP), pointer    :: BS_Overlapp(:,:)
   end subroutine K_diago_residuals
   !
 end interface 
 !
end module residuals