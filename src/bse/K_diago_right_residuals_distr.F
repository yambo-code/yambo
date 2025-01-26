!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine K_diago_R_res(mode, i_BS_mat, BS_E, &
  &                  neigs_this_cpu, neig_shift, BS_R_right, BS_V_right) 
 !
 !  The right eigenvector v(j) of A satisfies
 !                   A * v(j) = lambda(j) * v(j)
 !  where lambda(j) is its eigenvalue.
 !
 ! BS_R_right_XX(i)=<q0_XX|j><j|i_R>
 !                 =conjg(BSS_dipoles(j))*BS_V_right(j,i)
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_Z,BSS_eh_f,BSS_dipoles_opt
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_K_coupling
 use MAGNONS,        ONLY:BSS_dipoles_magn
 use DICHROISM,      ONLY:BSS_dipoles_dich
 !
 implicit none
 !
 character(*),intent(in)  :: mode
 integer,     intent(in)  :: i_BS_mat, neigs_this_cpu, neig_shift
 complex(SP), allocatable,         intent(in)  :: BS_E(:)
 complex(SP), pointer,             intent(out) :: BS_R_right(:)
 complex(SP), target, allocatable, intent(in)  :: BS_V_right(:,:)
 !
 ! Workspace
 !
 integer                  ::i1, i_lambda, neig_total, SL_H_dim, res_range(2)
 !
 complex(SP), allocatable :: tmp_res(:), BS_R_tmp(:) 
 logical                  :: velocity_correction
 !
 ! Sanity checks
 if (.not. allocated(BS_E)) return 
 if (.not. associated(BS_R_right) .or. .not. allocated(BS_V_right)) return 
 !
 neig_total = size(BS_E)
 !
 if (size(BS_R_right)       /= neig_total)     call error("Size mismatch between eigvals and R.residuals.")
 if (size(BS_V_right,dim=2) /= neigs_this_cpu) call error("Size mismatch between eigvecs and R.residuals.")
 !
 BS_R_right=cZERO
 velocity_correction=(index(mode,"opt")>0).and.(trim(global_gauge)=="velocity")
 !
 if(BS_K_coupling) then 
   SL_H_dim=BS_H_dim
   res_range=(/1,SL_H_dim/)
 else 
   SL_H_dim=BS_K_dim(i_BS_mat)
   res_range=(/1,SL_H_dim/) + BS_K_dim(1)*(i_BS_mat-1)
 endif
 !
 allocate(tmp_res(SL_H_dim))
 !
 call live_timing(trim(mode)//' residuals ',neigs_this_cpu)
 !
 if(trim(mode)=="opt1") tmp_res=BSS_dipoles_opt(1,res_range(1):res_range(2))
 if(trim(mode)=="opt2") tmp_res=BSS_dipoles_opt(2,res_range(1):res_range(2))
 if(trim(mode)=="dic1") tmp_res=BSS_dipoles_dich(1,res_range(1):res_range(2)) 
 if(trim(mode)=="dic2") tmp_res=BSS_dipoles_dich(2,res_range(1):res_range(2)) 
 if(trim(mode)=="mag1") tmp_res=BSS_dipoles_magn(1,res_range(1):res_range(2))
 if(trim(mode)=="mag2") tmp_res=BSS_dipoles_magn(2,res_range(1):res_range(2))
 !
 tmp_res = conjg(tmp_res)*sqrt(cmplx(BSS_eh_f(res_range(1):res_range(2)),kind=SP)) 
 if (allocated(BSS_eh_Z)) tmp_res=tmp_res*sqrt(BSS_eh_Z(res_range(1):res_range(2)))
 if (velocity_correction) tmp_res=tmp_res/BSS_eh_E(res_range(1):res_range(2))
 !
 do i1=1, neigs_this_cpu
   i_lambda = i1 + neig_shift
   !
   BS_R_right(i_lambda)=V_dot_V_omp(SL_H_dim,tmp_res,BS_V_right(:,i1))
   if (velocity_correction) BS_R_right(i_lambda) = BS_R_right(i_lambda)*BS_E(i_lambda)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_right)
 call live_timing
 !
 deallocate(tmp_res)
 !
end subroutine K_diago_R_res
