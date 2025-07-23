!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PL_diago_herm_residual(BS_mat,BS_R_PL) 
 !
 ! Resonant PL Residuals
 !
  ! Resonant Residuals
 !====================
 ! BS_R_right(i) = \sum_k <q0|k><k|i> =
 !                 \sum_k BS_mat(k,i) x conjg( BSS_dipoles(k) )
 ! BS_R_left(i)  = conjg( BS_R_right(i) )
 !
 use pars,           ONLY:SP,rZERO,cZERO,cI
 use wrapper,        ONLY:V_dot_V
 use wrapper_omp,    ONLY:V_dot_V_omp,Vstar_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use com,            ONLY:msg
 use BS,             ONLY:BS_H_dim,BS_K_dim,BS_dip_size
 use BS_solvers,     ONLY:BSS_eh_f_RES,BSS_n_eig, BS_E
 use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_f
 use functions,      ONLY:boltzman_f
#if defined _RT
 use RT_control,     ONLY:NEQ_Residuals
#endif
 use D_lattice,      ONLY:Boltz_Temp
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_mat(BS_K_dim(1),BSS_n_eig)
 real(SP)   ,intent(out) :: BS_R_PL(2,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 logical              ::compute_ares_explicitly
 integer              ::i_l,i_c,i_K,i_lr,i_ll, ik
 complex(SP)          ::P_x_fZ(BS_K_dim(1),3,2),Pi_x_R_x_adA_left(3),Pi_x_R_x_adA_right(3),adA_x_R_over_R_x_A(2),&
&                       R_over_R_x_A(BS_K_dim(1),2)
!Write to a file
 ! Parallel indexes
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 ! sqrt(R_i) Re[ (Pi_i)* A^l_i (A^l_j)* R<_j/R_j A^l_j (A^l_k)* Pi_k ] sqrt(R_k)
 !
 ! Zeroing
 !
 BS_R_PL       =rZERO
 P_x_fZ        =cZERO
 R_over_R_x_A  =cZERO
 !
 ! Live-Timing
 !
 call live_timing('PL    residuals',px%n_of_elements(myid+1))
 !
 ! Hermitian residual
 !====================
 !
 ! 1=> left
 ! 2=> right
 !
#if defined _RT
 if (NEQ_Residuals .and. (Boltz_Temp > 0._SP) ) then
   call error('The user can specify either boltzmann or external residual occupations, not both')
 endif
 ! 
 if (NEQ_Residuals) call msg('s','Running with NEQ Residuals')
#else
 if (.not. (Boltz_Temp > 0._SP) ) call error("Boltz temperature not set in input")
#endif
 if (       Boltz_Temp > 0._SP  ) call msg('s','Running with Boltz excitonic occupations')
 !
 do i_c=1,BS_dip_size
   do i_K=1,BS_K_dim(1)
     P_x_fZ(i_K,i_c,1)=conjg(BSS_dipoles_PL(i_c,i_K))*sqrt(cmplx(BSS_eh_f_RES(i_K))) !Pi^* left sqrt(R)
     P_x_fZ(i_K,i_c,2)=      BSS_dipoles_PL(i_c,i_K) *sqrt(cmplx(BSS_eh_f_RES(i_K))) !Pi^* left sqrt(R)
   enddo
 enddo
 !
 do i_l=1,BSS_n_eig
  !
  if (.not.px%element_1D(i_l)) cycle !parallelization
  !
  do i_c=1,BS_dip_size
    !dim(P_x_fz) = BS_H_dim
    ! we have a problem in this multiplication
    Pi_x_R_x_adA_left(i_c) =V_dot_V_omp(BS_K_dim(1),P_x_fZ(:,i_c,2),      BS_mat(:,i_l) ) !resonan Pi^* Ar*sqrt(R)                                   
    Pi_x_R_x_adA_right(i_c)=V_dot_V_omp(BS_K_dim(1),P_x_fZ(:,i_c,1),conjg(BS_mat(:,i_l))) !resonan Pi^* Ar*sqrt(R)                                   
    ! Pi_x_R_x_adA_left =   Pi sqrt(R) dipoles_PL*BS_mat/R
  enddo
  !
  !Check dimensions for these vectors to be consistent that we have only resonant and antiresonant
  BS_R_PL(:,i_l)=real(V_dot_V(BS_dip_size,Pi_x_R_x_adA_left,Pi_x_R_x_adA_right))
  !
#if defined _RT
  ! R_over_R_x_A = R<_j/R_j A^r_ij
  ! Here I create an If statement if I want to populate L lesser with electornic or excitonic populations
  if (NEQ_Residuals) then
    !                       
    ! R_over_R_x_A = R<_j/R_j A^l_j
    !
    R_over_R_x_A(:,1) = BSS_PL_f(:,1)*BS_mat(:,i_l)/BSS_eh_f_RES(:)
    R_over_R_x_A(:,2) = BSS_PL_f(:,2)*BS_mat(:,i_l)/BSS_eh_f_RES(:)
    !
    ! adA_x_R_over_R_x_A = (A^l_j)* R<_j/R_j A^l_j
    !
    adA_x_R_over_R_x_A(1)=V_dot_V_omp(BS_K_dim(1),conjg(BS_mat(:,i_l)),R_over_R_x_A(:,1))
    adA_x_R_over_R_x_A(2)=V_dot_V_omp(BS_K_dim(1),conjg(BS_mat(:,i_l)),R_over_R_x_A(:,2))
    !
  else if (Boltz_Temp .gt. 0) then
#endif
    !
    adA_x_R_over_R_x_A=boltzman_f(real(BS_E(i_l)-BS_E(1)))
    !
#if defined _RT
  endif
#endif
  !
  BS_R_PL(1,i_l)=BS_R_PL(1,i_l)*adA_x_R_over_R_x_A(1) !resonant
  BS_R_PL(2,i_l)=BS_R_PL(2,i_l)*adA_x_R_over_R_x_A(2) !resonant
  !Pi*Al*Pi^**Ar*Al*R</R*Ar
  !
 enddo
 !
 call live_timing()
 !
 call PP_redux_wait(BS_R_PL)
 !
 call PP_indexes_reset(px)
 !
end subroutine PL_diago_herm_residual

subroutine PL_diago_non_herm_residual(BS_V_left,BS_V_right,BS_R_PL,BS_overlap) 
 !
 ! Non-Hermitian residual
 !========================
 !
 ! Resonant PL Residuals
 !
 use pars,           ONLY:SP,rZERO,cZERO,cI
 use wrapper_omp,    ONLY:V_dot_V_omp,Vstar_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim,BS_K_dim,BS_dip_size
 use BS_solvers,     ONLY:BSS_eh_f_RES,BSS_n_eig
 use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_f
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_V_left(BS_H_dim,BSS_n_eig),BS_V_right(BS_H_dim,BSS_n_eig)
 real(SP)   ,intent(out) :: BS_R_PL(2,BSS_n_eig)
 complex(SP),optional,intent(in)  :: BS_overlap(BSS_n_eig,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_l,i_c,i_K,i_lr,i_ll, ik
 complex(SP)          ::P_x_fZ(BS_H_dim,3,2),Pi_x_R_x_adA_left(3),Pi_x_R_x_adA_right(3),adA_x_R_over_R_x_A,&
&                       R_over_R_x_A(BS_H_dim),B_var(BS_H_dim)
 !
 ! Parallel indexes
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 ! sqrt(R_i) Re[ (Pi_i)* A^l_i (A^l_j)* R<_j/R_j A^l_j (A^l_k)* Pi_k ] sqrt(R_k)
 !
 ! Zeroing
 !
 BS_R_PL       =rZERO
 P_x_fZ        =cZERO
 R_over_R_x_A  =cZERO
 !
 ! Live-Timing
 !
 call live_timing('PL    residuals',px%n_of_elements(myid+1))
 !
 ! 1=> left
 ! 2=> right
 !
 do i_c=1,BS_dip_size
    do i_K=1,BS_H_dim
      P_x_fZ(i_K,i_c,1)=conjg(BSS_dipoles_PL(i_c,i_K))*sqrt(cmplx(BSS_eh_f_RES(i_K)))
      P_x_fZ(i_K,i_c,2)=      BSS_dipoles_PL(i_c,i_K)* sqrt(cmplx(BSS_eh_f_RES(i_K)))
    enddo
  enddo
  !
  do i_lr=1,BS_H_dim
    !
    if (.not.px%element_1D(i_lr)) cycle
    !
    ! B_i = \sum_ll Overlap^*(ll,lr) A^ll_i
    !
    B_var=cZERO
    do i_ll=1,BS_H_dim
      B_var(:)=B_var(:)+conjg(BS_overlap(i_ll,i_lr))*BS_V_left(:,i_ll)
    enddo
    !
    ! R_over_R_x_A = R<_j/R_j B^l_j
    !
    do i_ll=1,BS_H_dim
      R_over_R_x_A(i_ll) = BSS_PL_f(i_ll,1)*B_var(i_ll)/BSS_eh_f_RES(i_ll)
    enddo
    !
    ! adA_x_R_over_R_x_A = B^l*_j    R<_j/R_j B^l_j
    !
    adA_x_R_over_R_x_A=Vstar_dot_V_omp(BS_H_dim,B_var,R_over_R_x_A)
    !
    do i_c=1,BS_dip_size
      !
      ! Pi_x_R_x_adA_left =   sqrt(R) Pi_i* A^lr_i
      !
      Pi_x_R_x_adA_left(i_c) =V_dot_V_omp(BS_H_dim,P_x_fZ(:,i_c,1),BS_V_right(:,i_lr))
      !
      ! Pi_x_R_x_adA_right=   A^lr*_i Pi_i sqrt(R)  
      !
      Pi_x_R_x_adA_right(i_c)=Vstar_dot_V_omp(BS_H_dim,BS_V_right(:,i_lr),P_x_fZ(:,i_c,2))
      !
      BS_R_PL(1,i_lr)=BS_R_PL(1,i_lr)+real(Pi_x_R_x_adA_left(i_c)*Pi_x_R_x_adA_right(i_c))
      !
    enddo
    !
    BS_R_PL(1,i_lr)=BS_R_PL(1,i_lr)*adA_x_R_over_R_x_A
    !
    call live_timing(steps=1)
    !
  enddo
 !
 call live_timing()
 !
 call PP_redux_wait(BS_R_PL)
 !
 ! In the coupling case, BSS_add_ares=.false. always
 BS_R_PL(2,:)=cZERO
 !
 call PP_indexes_reset(px)
 !
end subroutine PL_diago_non_herm_residual
