!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine K_diago_hermitian_residuals(i_BS_mat,BS_E_real,BS_R_left,BS_R_right) 
 !
 ! Resonant Residuals
 !====================
 ! BS_R_right(i) = \sum_k <q0|k><k|i> =
 !                 \sum_k BS_mat(k,i) x conjg( BSS_dipoles(k) )
 ! BS_R_left(i)  = conjg( BS_R_right(i) )
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f_RES,BS_mat,BSS_n_eig
 use BS,             ONLY:BS_K_dim
 !
 implicit none
 !
 integer,    intent(in)  :: i_BS_mat
 real(SP)   ,intent(in)  :: BS_E_real(BSS_n_eig)
 complex(SP),intent(out) :: BS_R_left(BSS_n_eig),BS_R_right(BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_lambda,i_range(2)
 complex(SP)          ::tmp_res(BS_K_dim(i_BS_mat))
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 i_range=(/1,BS_K_dim(i_BS_mat)/)+BS_K_dim(1)*(i_BS_mat-1)
 !
 BS_R_left =cZERO
 BS_R_right=cZERO
 call live_timing('EPS R residuals',px%n_of_elements(myid+1))
 !
 tmp_res = BSS_dipoles_opt(1,i_range(1):i_range(2))*sqrt(cmplx(BSS_eh_f_RES(i_range(1):i_range(2)),kind=SP))
 !
 ! DS. Note. Velocity gauge fixing does not work if neq occupations are loaded. Not sure why.
 if (trim(global_gauge)=="velocity") tmp_res =tmp_res/BSS_eh_E(i_range(1):i_range(2))
 if (allocated(BSS_eh_Z))            tmp_res =tmp_res*sqrt(BSS_eh_Z(i_range(1):i_range(2)))
 !
 do i_lambda=1,BSS_n_eig
   !
   if (.not.px%element_1D(i_lambda)) cycle
   !
   BS_R_left(i_lambda) =V_dot_V_omp(BS_K_dim(i_BS_mat),tmp_res,conjg(BS_mat(:,i_lambda)))
   if (trim(global_gauge)=="velocity") then
     BS_R_left(i_lambda) = BS_R_left(i_lambda)*BS_E_real(i_lambda)
   endif
   !
   BS_R_right(i_lambda)=conjg(BS_R_left(i_lambda))
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 call PP_redux_wait(BS_R_left)
 call PP_redux_wait(BS_R_right)
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_hermitian_residuals
