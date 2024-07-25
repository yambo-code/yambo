!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
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

   subroutine K_kerr_residuals(BS_E,BS_V_right,BS_R_right_kerr,neigs_this_cpu, neig_shift) 
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_Z,BSS_eh_f,BSS_n_eig,BSS_dipoles_opt
 use BS,             ONLY:BS_K_dim,BS_H_dim
 implicit none
 complex(SP), allocatable             :: BS_E(:)
 complex(SP), target,allocatable      :: BS_V_right(:,:)
 complex(SP), pointer                 :: BS_R_right_kerr(:)
 integer,intent(in)                   :: neigs_this_cpu, neig_shift
 end subroutine K_kerr_residuals

subroutine K_magnons_residuals(i_BS_mat, neigs_this_cpu, neig_shift, &
&                 BS_R_left,BS_R_right,BS_V_left,BS_V_right,BS_overlap) 
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp,Vstar_dot_V_omp
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use LIVE_t,         ONLY:live_timing
 use BS_solvers,     ONLY:BSS_eh_Z,BSS_eh_f,BSS_n_eig
 use BS,             ONLY:BS_H_dim, BS_K_coupling, BS_K_dim
 use MAGNONS,        ONLY:BSS_dipoles_magn
 implicit none
 integer,intent(in)      :: i_BS_mat, neigs_this_cpu, neig_shift
 complex(SP), pointer :: BS_R_left(:,:),BS_R_right(:,:)
 complex(SP), target,allocatable  :: BS_V_left(:,:),BS_V_right(:,:)
 complex(SP), pointer  :: BS_overlap(:,:)
end subroutine K_magnons_residuals

subroutine K_PL_residuals(BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian,neigs_this_cpu, neig_shift,BS_overlap) 
 use pars,           ONLY:SP,rZERO,cZERO,cI
 use wrapper_omp,    ONLY:V_dot_V_omp,Vstar_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim,BS_K_dim
 use BS_solvers,     ONLY:BSS_eh_f
 use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_f
 implicit none
 logical,    intent(in)             :: K_is_not_hermitian
 complex(SP), target, allocatable   :: BS_V_left(:,:),BS_V_right(:,:)
 real(SP), pointer                  :: BS_R_PL(:,:)
 complex(SP), pointer               :: BS_overlap(:,:)
 integer,intent(in)                 :: neigs_this_cpu, neig_shift
 end subroutine K_PL_residuals
 !
 !
 end interface 
 !
end module residuals