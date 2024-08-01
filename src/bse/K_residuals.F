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
   BS_R_left=cZERO
   if (.not. associated(BS_Overlap)) then 
      ! In this case overlap matrix is simply identity
      BS_R_left = BS_R_tmp
   else
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
   ! Non-hermitian : (overlap^-1)_SS' * \sum_{kcv} conj(A_L^S'_kcv) * (A_R^S'_kcv) * BS_corrections_{kcv}
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
      call live_timing('Overlap^{-1} x Perturb.    '//what//' ',neig_total*pert_dim)
      
      do i2=1,pert_dim
         if (neigs_this_cpu < 1) cycle
         !
         do i1=1, neig_total
         !
            tmp_BS_RL(i1)=V_dot_V_omp(neigs_this_cpu,BS_Overlap(i1,:), &
&                                BS_corrections(i2,neig_shift+1 : neig_shift+neigs_this_cpu))
         !
            call live_timing(steps=1)
         !
         end do
         BS_corrections(i2,:) = tmp_BS_RL(1:neig_total)
      enddo
      !
      call PP_redux_wait(BS_corrections) 
      call live_timing()

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
!   use pars,           ONLY:SP,rZERO,cZERO,cI
!   use wrapper_omp,    ONLY:V_dot_V_omp,Vstar_dot_V_omp
!   use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
!   use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
!   use LIVE_t,         ONLY:live_timing
!   use BS,             ONLY:BS_H_dim,BS_K_dim
!   use BS_solvers,     ONLY:BSS_eh_f, BSS_n_eig
!   use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_f
!   !
!   implicit none
!   !
!   logical,    intent(in)             :: K_is_not_hermitian
!   complex(SP), target, allocatable   :: BS_V_left(:,:),BS_V_right(:,:)
!   real(SP), pointer                  :: BS_R_PL(:,:)
!   complex(SP), pointer               :: BS_overlap(:,:)
!   integer,intent(in)                 :: neigs_this_cpu, neig_shift
!   !
!   ! Workspace
!   !
!   integer              ::i_l,i_c,i_K,i_lr,i_ll, neig_total
!   complex(SP)          ::Pi_x_R_x_adA_left(3),Pi_x_R_x_adA_right(3),adA_x_R_over_R_x_A(2)
!   complex(SP), allocatable   :: tmp_rvec(:), R_over_R_x_A(:,:), B(:), P_x_fZ(:,:,:)
!   logical              :: work_cpu = .false.
!   !
!   ! sqrt(R_i) Re[ (Pi_i)* A^l_i (A^l_j)* R<_j/R_j A^l_j (A^l_k)* Pi_k ] sqrt(R_k)
!   !
!   ! Zeroing
!   !
!   neig_total = BSS_n_eig
!   ! 
!   allocate(tmp_rvec(neig_total))
!   allocate(R_over_R_x_A(2,BS_H_dim))
!   allocate(B(BS_H_dim))
!   allocate(P_x_fZ(2,BS_H_dim,3))

!   if (associated(BS_R_PL)) deallocate(BS_R_PL)
!   allocate(BS_R_PL(2,neig_total))

!   BS_R_PL       =rZERO
!   P_x_fZ        =cZERO
!   R_over_R_x_A  =cZERO
!   !
!   call live_timing('PL    residuals',neigs_this_cpu)
!   !
!   ! Non-Hermitian residual
!   !========================
!   !
!   if (K_is_not_hermitian) then
!      !
!      ! 1=> left
!      ! 2=> right
!      !
!      do i_c=1,3
!         do i_K=1,BS_K_dim(1)
!            P_x_fZ(1,i_K,i_c)=conjg(BSS_dipoles_PL(i_K,i_c))*sqrt(BSS_eh_f(i_K))
!            P_x_fZ(2,i_K,i_c)=      BSS_dipoles_PL(i_K,i_c) *sqrt(BSS_eh_f(i_K))
!         enddo
!         do i_K=BS_K_dim(1)+1,BS_H_dim
!            P_x_fZ(1,i_K,i_c)=cI*conjg(BSS_dipoles_PL(i_K,i_c))*sqrt(BSS_eh_f(i_K-BS_K_dim(1)))
!            P_x_fZ(2,i_K,i_c)=cI*      BSS_dipoles_PL(i_K,i_c) *sqrt(BSS_eh_f(i_K-BS_K_dim(1)))
!         enddo
!      enddo
!      !
!      do i_lr=1,neig_total
!         !
!         !
!         ! B_i = \sum_lp Overlap^*(lp,l) A^ll_i
!         !
!         if ( (i_lr - neig_shift) >= 1 .and. (i_lr - neig_shift) <= neigs_this_cpu) work_cpu = .true.
!         B=cZERO
!         if (associated(BS_overlap)) then 
!            !
!            ! gather the vector 
!            tmp_rvec = cZERO
!            if (work_cpu) tmp_rvec = conjg(BS_overlap(:,i_lr - neig_shift))
!            !
!            call PP_redux_wait(tmp_rvec)
!            !
!            do i_ll=1 + neig_shift, neigs_this_cpu + neig_shift
!               B(:)=B(:)+tmp_rvec(i_ll)*BS_V_left(:,i_ll-neig_shift)
!            enddo
!            !
!         else 
!            !
!            do i_ll= 1 + neig_shift, neigs_this_cpu + neig_shift
!               if (i_lr == i_ll) B(:)=B(:)+ BS_V_left(:,i_ll-neig_shift)
!            enddo
!            !
!         endif
!         call PP_redux_wait(B)
!         !
!         ! R_over_R_x_A = R<_j/R_j B^l_j
!         !
!         R_over_R_x_A(1,:) = BSS_PL_f(:)*B(:)/BSS_eh_f(:)
!         !
!         ! adA_x_R_over_R_x_A = B^l*_j    R<_j/R_j B^l_j
!         !
!         adA_x_R_over_R_x_A(1)=Vstar_dot_V_omp(BS_H_dim,B,R_over_R_x_A(1,:))
!         !
!         if (work_cpu) then
!            do i_c=1,3
!               !
!               ! Pi_x_R_x_adA_left =   sqrt(R) Pi_i* A^lr_i
!               !
!               Pi_x_R_x_adA_left(i_c) =V_dot_V_omp(BS_H_dim,P_x_fZ(1,:,i_c),BS_V_right(:,i_lr-neig_shift))
!               !
!               ! Pi_x_R_x_adA_right=   A^lr*_i Pi_i sqrt(R)  
!               !
!               Pi_x_R_x_adA_right(i_c)=Vstar_dot_V_omp(BS_H_dim,BS_V_right(:,i_lr-neig_shift),P_x_fZ(2,:,i_c))
!               !
!            enddo
!            !
!            BS_R_PL(1,i_lr)=real((Pi_x_R_x_adA_left(1)*Pi_x_R_x_adA_right(1)+&
!               &                          Pi_x_R_x_adA_left(2)*Pi_x_R_x_adA_right(2)+&
!               &                          Pi_x_R_x_adA_left(3)*Pi_x_R_x_adA_right(3))*adA_x_R_over_R_x_A(1))
!            !
!         endif
!         call live_timing(steps=1)
!         !
!      enddo
!      !
!   else
!      !
!      ! Hermitian residual
!      !====================
!      !
!      do i_c=1,3
!         P_x_fZ(1,:,i_c)=BSS_dipoles_PL(:,i_c)*sqrt(BSS_eh_f(:))
!      enddo
!      !
!      do i_l=1,neigs_this_cpu
!         !
!         !                       
!         ! R_over_R_x_A = R<_j/R_j A^l_j
!         ! NM : FIX ME URGENT !
!         R_over_R_x_A(1,:) =  BSS_PL_f(1:BS_K_dim(1)) *BS_V_right(:,i_l)/BSS_eh_f(:)
!         R_over_R_x_A(2,:) =  BSS_PL_f(BS_K_dim(1)+1:)*BS_V_right(:,i_l)/BSS_eh_f(:)
!         !
!         ! adA_x_R_over_R_x_A = (A^l_j)* R<_j/R_j A^l_j
!         !
!         adA_x_R_over_R_x_A(1)=V_dot_V_omp(BS_K_dim(1),conjg(BS_V_right(:,i_l)),R_over_R_x_A(1,:))
!         adA_x_R_over_R_x_A(2)=V_dot_V_omp(BS_K_dim(1),conjg(BS_V_right(:,i_l)),R_over_R_x_A(2,:))
!         !
!         do i_c=1,3
!            !                                   
!            ! Pi_x_R_x_adA_right =   Pi sqrt(R)  A^l_i
!            !
!            Pi_x_R_x_adA_right(i_c)=V_dot_V_omp(BS_K_dim(1),BS_V_right(:,i_l),P_x_fZ(1,:,i_c))
!            !
!         enddo
!         !
!         ! Note that the 2nd component (anti-resonant) should take a -1  from BSS_eh_f and a i^2 from sqrt(R)
!         ! that cancel out.
!         !
!         BS_R_PL(:,i_l + neig_shift)=dot_product(Pi_x_R_x_adA_right,Pi_x_R_x_adA_right)*adA_x_R_over_R_x_A(:)
!         !
!         call live_timing(steps=1)
!         !
!      enddo
!      !
!   endif
!   !
!   call live_timing()
!   !
!   call PP_redux_wait(BS_R_PL)
!   !
!   deallocate(tmp_rvec)
!   deallocate(R_over_R_x_A)
!   deallocate(B)
!   deallocate(P_x_fZ)
!   !
!end subroutine K_PL_residuals

