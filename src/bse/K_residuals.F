!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
subroutine K_diago_residuals(i_BS_mat, BS_Energies, BS_Res_right, BS_VR, &
&                  neigs_this_cpu, neig_shift, BS_Res_left, BS_VL, BS_Overlapp) 
 !
 !  The right eigenvector v(j) of A satisfies
 !                   A * v(j) = lambda(j) * v(j)
 !  where lambda(j) is its eigenvalue.
 !
 !  The left eigenvector u(j) of A satisfies
 !                u(j)**H * A = lambda(j) * u(j)**H
 !  where u(j)**H denotes the conjugate transpose of u(j).
 !
 !  Remember: 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
 !
 !
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f, BSS_eh_f_RES
 use BS,             ONLY:BS_K_coupling,BS_K_dim,BS_H_dim
 !
 implicit none
 !
 integer,intent(in)      :: i_BS_mat, neigs_this_cpu, neig_shift
 complex(SP), allocatable :: BS_Energies(:) ! Full set of eigvals are required
 complex(SP), pointer     :: BS_Res_right(:)
 complex(SP), target,allocatable     :: BS_VR(:,:)
 complex(SP), pointer     :: BS_Res_left(:)
 complex(SP), target, allocatable     :: BS_VL(:,:)
 complex(SP), pointer    :: BS_Overlapp(:,:)
 ! NM : if BS_Overlapp is null(), then it is treated as a identity matrix
 ! optional
 ! Workspace
 integer              ::i1, neig_total, SL_H_dim, i_range(2)
 !
 complex(SP), allocatable :: tmp_res(:), BS_R_tmp(:) 
 !

 neig_total = size(BS_Energies)
 !
 if(BS_K_coupling) then 
  SL_H_dim=BS_H_dim
  i_range=(/1,SL_H_dim/)
 else 
  SL_H_dim=BS_K_dim(i_BS_mat)
  i_range=(/1,SL_H_dim/) + BS_K_dim(1)*(i_BS_mat-1)
 endif

 if (associated(BS_Res_right)) deallocate(BS_Res_right)
 if (associated(BS_Res_left)) deallocate(BS_Res_left)

 allocate(BS_Res_right(neig_total))
 allocate(BS_Res_left(neig_total))

 allocate(tmp_res(SL_H_dim))
 allocate(BS_R_tmp(neig_total))
 !
 ! [1] BS_Res_right(i)=<q0|j><j|i_R>
 !                  =conjg(BSS_dipoles(j))*BS_VR(j,i)
 !
 BS_Res_right= cZERO ! This must be initilaized to 0 else behaviour is undefined
 BS_Res_left = cZERO
 !
 call live_timing('EPS R residuals',neigs_this_cpu)
 !
 if(BS_K_coupling) then 
  tmp_res = conjg(BSS_dipoles_opt(1,:))*sqrt(cmplx(BSS_eh_f,kind=SP))
 else 
  tmp_res = conjg(BSS_dipoles_opt(1,i_range(1):i_range(2))) * &
&           sqrt(cmplx(BSS_eh_f_RES(i_range(1):i_range(2)),kind=SP))
 endif
 !
 ! DS. Note. Velocity gauge fixing does not work if neq occupations are loaded. Not sure why.
 if (trim(global_gauge)=="velocity") tmp_res=tmp_res/BSS_eh_E(i_range(1):i_range(2))
 if (allocated(BSS_eh_Z)           ) tmp_res=tmp_res*sqrt(BSS_eh_Z(i_range(1):i_range(2)))
 !
 do i1=1, neigs_this_cpu
   !
   BS_Res_right(i1 + neig_shift)=V_dot_V_omp(SL_H_dim,tmp_res,BS_VR(:,i1))
   if (trim(global_gauge)=="velocity") BS_Res_right(i1 + neig_shift) = &
&                     BS_Res_right(i1 + neig_shift)*BS_Energies(i1 + neig_shift)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_Res_right)
 call live_timing
 ! In case of hermitian, return now
 if (.not. BS_K_coupling) then 
    BS_Res_left = conjg(BS_Res_right)
    return 
 endif
 !
 ! [2] BS_R_tmp(i)= <i_K|j><j|q0> 
 !                = conjg( BS_VL(k,i))*BSS_dipoles(k)*R_k
 !
 BS_R_tmp=cZERO
 !
 call live_timing('EPS L residuals',neigs_this_cpu)
 !
 tmp_res = BSS_dipoles_opt(1,:)*sqrt(cmplx(BSS_eh_f,kind=SP))
 !
 do i1=1, neigs_this_cpu
   !
   BS_R_tmp(i1 + neig_shift)=Vstar_dot_V_omp(SL_H_dim,BS_VL(:,i1),tmp_res)
   if (trim(global_gauge)=="velocity") BS_R_tmp(i1 + neig_shift) = &
&                     BS_R_tmp(i1 + neig_shift)*BS_Energies(i1 + neig_shift)
   !
   call live_timing(steps=1)
   !
 enddo
 call PP_redux_wait(BS_R_tmp)
 !
 call live_timing()
 !
 ! [3] BS_Res_left(i)=BS_Overlapp(i,j)BS_R_tmp(j)
 !
 BS_Res_left=cZERO
 if (.not. associated(BS_Overlapp)) then 
  ! In this case overlap matrix is simply identity
  BS_Res_left = BS_R_tmp
 else 
  call live_timing('EPS L x overlap',neigs_this_cpu)
  if (neigs_this_cpu>0) then
    do i1=1,neig_total
      !
      BS_Res_left(i1)=V_dot_V_omp(neigs_this_cpu,BS_Overlapp(i1,:), &
&                BS_R_tmp(neig_shift+1 : neig_shift+neigs_this_cpu))
      !
      call live_timing(steps=1)
    enddo
  endif
    call PP_redux_wait(BS_Res_left)
    call live_timing
  endif
 !
 ! free stuff
 deallocate(tmp_res)
 deallocate(BS_R_tmp)
 !
end subroutine K_diago_residuals
!
!
