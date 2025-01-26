!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Originally by AM, MG, DS
!
! Modified by NM for taking care of new parallel distribution.
!
subroutine K_diago_L_res(mode, i_BS_mat, BS_E, &
&                  neigs_this_cpu, neig_shift, BS_R_left, BS_V_left, BS_Overlap) 
 !
 !  The left eigenvector u(j) of A satisfies
 !                u(j)**H * A = lambda(j) * u(j)**H
 !  where u(j)**H denotes the conjugate transpose of u(j).
 !
 !  Remember: 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS,             ONLY:BS_H_dim,BS_K_dim,BS_K_coupling
 use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f
 use MAGNONS,        ONLY:BSS_dipoles_magn
 use DICHROISM,      ONLY:BSS_dipoles_dich
 !
 implicit none
 !
 character(*),intent(in)  :: mode
 integer,     intent(in)  :: i_BS_mat, neigs_this_cpu, neig_shift
 complex(SP), allocatable,         intent(in)  :: BS_E(:)
 complex(SP), pointer,             intent(out) :: BS_R_left(:)
 complex(SP), target, allocatable, intent(in)  :: BS_V_left(:,:)
 complex(SP), pointer, optional,   intent(in)  :: BS_Overlap(:,:)
 !
 ! If BS_Overlap is null(), then it is treated as a identity matrix
 ! Incase Ydiago solver is used, BS_Overlap is always null()
 !
 ! Workspace
 !
 integer              :: i1, i_lambda, neig_total, SL_H_dim, res_range(2)
 logical              :: velocity_correction
 character(13)        :: scheme
 !
 complex(SP),allocatable :: tmp_res(:)
 complex(SP),pointer     :: BS_R_tmp(:) => null()
 !
 ! Sanity checks
 if (.not. allocated(BS_E))      return 
 if (.not. allocated(BS_V_left)) return 
 !
 neig_total = size(BS_E)
 !
 if (size(BS_R_left)       /= neig_total)     call error("Size mismatch between eigvals and R.residuals.")
 if (size(BS_V_left,dim=2) /= neigs_this_cpu) call error("Size mismatch between eigvecs and R.residuals.")
 !
 if (.not.(present(BS_overlap).and.associated(BS_overlap))) scheme="hermitian"
 if (     (present(BS_overlap).and.associated(BS_overlap))) scheme="non-hermitian"
 !
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
 if (trim(scheme)=="hermitian")     BS_R_tmp => BS_R_left
 if (trim(scheme)=="non-hermitian") allocate(BS_R_tmp(neig_total))
 !
 ! [1] BS_R_tmp(i)= <i_K|j><j|q0> 
 !                = conjg( BS_V_left(k,i))*BSS_dipoles(k)*R_k
 BS_R_tmp=cZERO
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
   BS_R_tmp(i_lambda)=Vstar_dot_V_omp(SL_H_dim,BS_V_left(:,i1),tmp_res)
   if (velocity_correction) BS_R_tmp(i_lambda) = BS_R_tmp(i_lambda)*BS_E(i_lambda)
   !
   call live_timing(steps=1)
   !
 enddo
 call PP_redux_wait(BS_R_tmp)
 !
 call live_timing()
 !
 ! Incase of Ydiago solver Overlap is always identity.
 !
 if (trim(scheme)=="hermitian")  return
 !
 ! [2] BS_R_left(i)=BS_overlap(i,j)BS_R_tmp(j)
 !
 BS_R_left=cZERO
 !
 call live_timing('EPS L x overlap',neigs_this_cpu)
 do i1=1, neigs_this_cpu
   i_lambda = i1 + neig_shift
   !
   if (neigs_this_cpu > 0) BS_R_left(i_lambda)=&
   & V_dot_V_omp(neigs_this_cpu,BS_overlap(i1,:),BS_R_tmp(neig_shift+1 : neig_shift+neigs_this_cpu))
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_left)
 call live_timing
 !
end subroutine K_diago_L_res