!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine K_diago_left_residuals(mode,i_BS_mat,BS_E,BS_R_left,BS_V_left,BS_overlap)
 !
 !  The left eigenvector u(j) of A satisfies
 !                u(j)**H * A = lambda(j) * u(j)**H
 !  where u(j)**H denotes the conjugate transpose of u(j).
 !
 !  Remember: 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
 !
 use pars,           ONLY:SP,cZERO,cI
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS,             ONLY:BS_H_dim,BS_K_dim,BS_K_coupling
 use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f,BSS_n_eig
 use MAGNONS,        ONLY:BSS_dipoles_magn
 use DICHROISM,      ONLY:BSS_dipoles_dich
 !
 implicit none
 !
 character(*),intent(in)  :: mode
 integer,     intent(in)  :: i_BS_mat
 complex(SP), intent(in)  :: BS_E(BSS_n_eig)
 complex(SP),target,  intent(out) :: BS_R_left(BSS_n_eig)
 complex(SP),         intent(in)  :: BS_V_left(:,:)
 complex(SP),optional,intent(in)  :: BS_overlap(BSS_n_eig,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     :: px
 integer              :: i_lambda,n_res,res_range(2)
 logical              :: velocity_correction
 character(13)        :: scheme
 !
 complex(SP),allocatable :: tmp_res(:)
 complex(SP),pointer     :: BS_R_tmp(:) => null()
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 if (.not.present(BS_overlap)) scheme="hermitian"
 if (     present(BS_overlap)) scheme="non-hermitian"
 !
 velocity_correction=(index(mode,"opt")>0).and.(trim(global_gauge)=="velocity")
 !
 if (.not.BS_K_coupling) res_range=(/1,BS_K_dim(i_BS_mat)/)+BS_K_dim(1)*(i_BS_mat-1)
 if (     BS_K_coupling) res_range=(/1,BS_H_dim/)
 !
 allocate(tmp_res(res_range(1):res_range(2)))
 n_res=res_range(2)-res_range(1)+1
 !
 if (trim(scheme)=="hermitian")     BS_R_tmp => BS_R_left
 if (trim(scheme)=="non-hermitian") allocate(BS_R_tmp(BSS_n_eig))
 !
 ! [1] BS_R_tmp(i)= <i_K|j><j|q0> 
 !                = conjg( BS_V_left(k,i))*BSS_dipoles(k)*R_k
 BS_R_tmp=cZERO
 !
 call live_timing('EPS L residuals',px%n_of_elements(myid+1))
 !
 if(trim(mode)=="opt1") tmp_res=BSS_dipoles_opt(1,res_range(1):res_range(2)) 
 if(trim(mode)=="opt2") tmp_res=BSS_dipoles_opt(2,res_range(1):res_range(2)) 
 if(trim(mode)=="dic1") tmp_res=BSS_dipoles_dich(1,res_range(1):res_range(2)) 
 if(trim(mode)=="dic2") tmp_res=BSS_dipoles_dich(2,res_range(1):res_range(2)) 
 if(trim(mode)=="mag1") tmp_res=BSS_dipoles_magn(1,res_range(1):res_range(2))
 if(trim(mode)=="mag2") tmp_res=BSS_dipoles_magn(2,res_range(1):res_range(2))
 !
 tmp_res = tmp_res*sqrt(cmplx(BSS_eh_f(res_range(1):res_range(2)),kind=SP)) 
 if (allocated(BSS_eh_Z)) tmp_res=tmp_res*sqrt(BSS_eh_Z(res_range(1):res_range(2)))
 if (velocity_correction) tmp_res=tmp_res/BSS_eh_E(res_range(1):res_range(2))
 !
 do i_lambda=1,BSS_n_eig
   !
   if (.not.px%element_1D(i_lambda)) cycle
   !
   BS_R_tmp(i_lambda)=Vstar_dot_V_omp(n_res,BS_V_left(:,i_lambda),tmp_res)
   if (velocity_correction) BS_R_tmp(i_lambda)=BS_R_tmp(i_lambda)*BS_E(i_lambda)
   !
   call live_timing(steps=1)
   !
 enddo
 call PP_redux_wait(BS_R_tmp)
 !
 call live_timing()
 !
 if (trim(scheme)=="hermitian")  return
 !
 ! [2] BS_R_left(i)=BS_overlap(i,j)BS_R_tmp(j)
 !
 BS_R_left=cZERO
 !
 call live_timing('EPS L x overlap',px%n_of_elements(myid+1))
 do i_lambda=1,BSS_n_eig
   if (.not.px%element_1D(i_lambda)) cycle
   !
   BS_R_left(i_lambda)=V_dot_V_omp(BSS_n_eig,BS_overlap(i_lambda,:),BS_R_tmp)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_left)
 call live_timing
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_left_residuals
