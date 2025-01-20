!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Originally by AM, MG, DS
!
! Modified by NM for taking care of new parallel distribution.
!
!
subroutine K_diago_residuals(mode, i_BS_mat, BS_E, BS_R_right, BS_VR, &
      &                  neigs_this_cpu, neig_shift, BS_R_left, BS_VL, BS_Overlap) 
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
   use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f
   use BS,             ONLY:BS_K_coupling,BS_K_dim,BS_H_dim
   use MAGNONS,        ONLY:BSS_dipoles_magn
   use DICHROISM,      ONLY:BSS_dipoles_dich
   !
   implicit none
   !
   character(*),intent(in)  :: mode
   integer,intent(in)       :: i_BS_mat, neigs_this_cpu, neig_shift
   complex(SP), allocatable :: BS_E(:) ! Full set of eigvals are required
   complex(SP), pointer     :: BS_R_right(:)
   complex(SP), target,allocatable     :: BS_VR(:,:)
   complex(SP), pointer     :: BS_R_left(:)
   complex(SP), target, allocatable     :: BS_VL(:,:)
   complex(SP), pointer     :: BS_Overlap(:,:)
   ! NM : if BS_Overlap is null(), then it is treated as a identity matrix
   ! Incase Ydiago solver is used, BS_Overlap is always null()
   ! optional
   ! Workspace
   integer                  ::i1, neig_total, SL_H_dim, i_range(2)
   !
   complex(SP), allocatable :: tmp_res(:), BS_R_tmp(:) 
   logical                  :: velocity_correction, compute_left
   !

   compute_left = .false. 
   if (associated(BS_R_left)) compute_left = .true.
   !
   ! Sanity checks
   if (.not. allocated(BS_E)) return 
   if (.not. associated(BS_R_right) .or. .not. allocated(BS_VR)) return 
   if (BS_K_coupling .and. .not. allocated(BS_VL)) return 
   !
   neig_total = size(BS_E)
   !
   if (size(BS_R_right)  /= neig_total) call error("Size mismatch between eigvals and R.residuals.")
   if (size(BS_VR,dim=2) /= neigs_this_cpu) call error("Size mismatch between eigvecs and R.residuals.")
   !
   !
   !
   if(BS_K_coupling) then 
      SL_H_dim=BS_H_dim
      i_range=(/1,SL_H_dim/)
   else 
      SL_H_dim=BS_K_dim(i_BS_mat)
      i_range=(/1,SL_H_dim/) + BS_K_dim(1)*(i_BS_mat-1)
   endif

   allocate(tmp_res(SL_H_dim))
   !
   ! [1] BS_R_right(i)=<q0|j><j|i_R>
   !                  =conjg(BSS_dipoles(j))*BS_VR(j,i)
   !
   BS_R_right= cZERO ! This must be initilaized to 0 else behaviour is undefined
   if (compute_left) BS_R_left = cZERO
   !
   velocity_correction=(index(mode,"opt")>0).and.(trim(global_gauge)=="velocity")
   !
   if(trim(mode)=="opt1") tmp_res=BSS_dipoles_opt(1,i_range(1):i_range(2))
   if(trim(mode)=="opt2") tmp_res=BSS_dipoles_opt(2,i_range(1):i_range(2))
   if(trim(mode)=="dic1") tmp_res=BSS_dipoles_dich(1,i_range(1):i_range(2)) 
   if(trim(mode)=="dic2") tmp_res=BSS_dipoles_dich(2,i_range(1):i_range(2)) 
   if(trim(mode)=="mag1") tmp_res=BSS_dipoles_magn(1,i_range(1):i_range(2))
   if(trim(mode)=="mag2") tmp_res=BSS_dipoles_magn(2,i_range(1):i_range(2))
   !
   call live_timing(trim(mode)//' residuals ',neigs_this_cpu)
   !
   tmp_res = conjg(tmp_res)*sqrt(cmplx(BSS_eh_f(i_range(1):i_range(2)),kind=SP)) 
   if (allocated(BSS_eh_Z)) tmp_res=tmp_res*sqrt(BSS_eh_Z(i_range(1):i_range(2)))
   ! DS. Note. Velocity gauge fixing does not work if neq occupations are loaded. Not sure why.
   if (velocity_correction) tmp_res=tmp_res/BSS_eh_E(i_range(1):i_range(2))
   !
   do i1=1, neigs_this_cpu
      !
      BS_R_right(i1 + neig_shift)=V_dot_V_omp(SL_H_dim,tmp_res,BS_VR(:,i1))
      if (velocity_correction) BS_R_right(i1 + neig_shift) = &
         &                     BS_R_right(i1 + neig_shift)*BS_E(i1 + neig_shift)
      !
      call live_timing(steps=1)
   enddo
   call PP_redux_wait(BS_R_right)
   call live_timing
   ! In case of hermitian, return now
   if (.not. BS_K_coupling .or. .not. compute_left) then 
      if (compute_left) BS_R_left = conjg(BS_R_right)
      deallocate(tmp_res)
      return 
   endif
   !
   ! [2] BS_R_tmp(i)= <i_K|j><j|q0> 
   !                = conjg( BS_VL(k,i))*BSS_dipoles(k)*R_k
   !
   allocate(BS_R_tmp(neig_total))
   BS_R_tmp=cZERO
   !
   call live_timing(trim(mode)//' L residuals',neigs_this_cpu)
   !
   if(trim(mode)=="opt1") tmp_res=BSS_dipoles_opt(1,i_range(1):i_range(2)) 
   if(trim(mode)=="opt2") tmp_res=BSS_dipoles_opt(2,i_range(1):i_range(2)) 
   if(trim(mode)=="dic1") tmp_res=BSS_dipoles_dich(1,i_range(1):i_range(2)) 
   if(trim(mode)=="dic2") tmp_res=BSS_dipoles_dich(2,i_range(1):i_range(2)) 
   if(trim(mode)=="mag1") tmp_res=BSS_dipoles_magn(1,i_range(1):i_range(2))
   if(trim(mode)=="mag2") tmp_res=BSS_dipoles_magn(2,i_range(1):i_range(2))
   !
   tmp_res = tmp_res*sqrt(cmplx(BSS_eh_f(i_range(1):i_range(2)),kind=SP)) 
   if (allocated(BSS_eh_Z)) tmp_res=tmp_res*sqrt(BSS_eh_Z(i_range(1):i_range(2)))
   if (velocity_correction) tmp_res=tmp_res/BSS_eh_E(i_range(1):i_range(2))
   !
   do i1=1, neigs_this_cpu
      !
      BS_R_tmp(i1 + neig_shift)=Vstar_dot_V_omp(SL_H_dim,BS_VL(:,i1),tmp_res)
      if (velocity_correction) BS_R_tmp(i1 + neig_shift) = &
         &                     BS_R_tmp(i1 + neig_shift)*BS_E(i1 + neig_shift)
      !
      call live_timing(steps=1)
      !
   enddo
   call PP_redux_wait(BS_R_tmp)
   !
   call live_timing()
   !
   ! [3] BS_R_left(i)=BS_Overlap(i,j)BS_R_tmp(j)
   !
   BS_R_left = BS_R_tmp
   ! Incase of Ydiago solver Overlap is always identity.
   !
   if (associated(BS_Overlap)) then 
      ! This is for backward compatibilty. You should never get into this if using 
      ! Ydiago solver
      BS_R_left=cZERO
      call live_timing(trim(mode)//' overlap^{-1} x L',neig_total)
      do i1=1,neig_total
         !
         if (neigs_this_cpu > 0) BS_R_left(i1)=V_dot_V_omp(neigs_this_cpu,BS_Overlap(i1,:), &
&                                BS_R_tmp(neig_shift+1 : neig_shift+neigs_this_cpu))
         !
         call live_timing(steps=1)
      enddo
      call PP_redux_wait(BS_R_left)
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
subroutine K_perturbative_residuals(pert_dim, what, i_BS_mat, &
&           BS_corrections, BS_VR, neigs_this_cpu, neig_shift, BS_VL, BS_Overlap) 
!
   ! Diagonal Perturbative corrections. We assume that KS corrections are  
   ! are diagonal.
   !
   ! Hermitian = \sum_{kcv} |A^S_kcv|^2*BS_corrections_{kcv}
   ! 
   ! Non-hermitian : (overlap^-1)_SS' * \sum_{kcv} conj(A_L^S'_kcv) * (A_R^S_kcv) * BS_corrections_{kcv}
   !
   use pars,           ONLY:SP,cZERO
   use wrapper_omp,    ONLY:V_dot_V_omp
   use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
   use LIVE_t,         ONLY:live_timing
   use BS_solvers,     ONLY:BSS_eh_E_SOC_corr,BSS_eh_W,BSS_eh_E,BSS_n_eig
   use BS,             ONLY:BS_K_coupling,BS_K_dim,BS_H_dim
   !
   implicit none
   !
   integer,     intent(in)                   :: pert_dim
   character(3),intent(in)                   :: what
   complex(SP),allocatable                   :: BS_corrections(:,:) !(pert_dim,BSS_n_eig)
   integer,intent(in)                        :: i_BS_mat, neigs_this_cpu, neig_shift
   complex(SP), target,allocatable           :: BS_VR(:,:)
   complex(SP), target, allocatable          :: BS_VL(:,:)
   complex(SP), pointer                      :: BS_Overlap(:,:)

   ! Workspace
   integer                                   :: i1,i2,SL_H_dim,neig_total
   complex(SP),pointer                       :: tmp_v_left(:,:) => null()
   complex(SP),allocatable                   :: tmp_BS_RL(:)
   complex(SP)                               :: aux1

   BS_corrections=cZERO

   neig_total = BSS_n_eig
   if(BS_K_coupling) then 
      SL_H_dim=BS_H_dim
      tmp_v_left => BS_VL
   else 
      SL_H_dim=BS_K_dim(i_BS_mat)
      tmp_v_left => BS_VR
   endif
   !
   allocate(tmp_BS_RL(SL_H_dim))
   !
   call live_timing('Perturb.    '//what//' ',neigs_this_cpu)
   !
   do i1=1, neigs_this_cpu
      !
      tmp_BS_RL = conjg(tmp_v_left(:,i1))*BS_VR(:,i1)
      !
      if(what=='SOC') then
         do i2=1,pert_dim
            BS_corrections(i2,i1+neig_shift)=V_dot_V_omp(SL_H_dim, &
&                                            cmplx(BSS_eh_E_SOC_corr(i2,:),0._SP,SP), tmp_BS_RL)
         enddo
      endif
      !
      if(what=='WID') then
         BS_corrections(1,i1+neig_shift)=V_dot_V_omp(SL_H_dim,cmplx(0._SP,BSS_eh_W(:),SP),tmp_BS_RL)
      endif
      !
      call live_timing(steps=1)
      !
   enddo
   !
   call PP_redux_wait(BS_corrections) 
   !
   call live_timing()
   !
   ! In case of overlap multiply with inverse of overlap
   !
   if (BS_K_coupling .and. associated(BS_Overlap)) then 
      !
      ! NM : If you use Ydiago solver, you should never see this because, Ydiago outputs 
      ! left and right eigenvectors with overlap = null() i.e identity.
      call error(" BS pertubative residuals not implemented in case of Overlap matrix")
   endif
   !
   deallocate(tmp_BS_RL)
   !
end subroutine K_perturbative_residuals
!
!
!
!
!subroutine K_PL_residuals(BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian,neigs_this_cpu, neig_shift,BS_overlap) 
!   !
!   ! PL Residuals
!   !
!end subroutine K_PL_residuals

