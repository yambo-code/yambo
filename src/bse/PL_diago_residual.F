!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PL_diago_residual(BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian,BS_overlap) 
 !
 ! PL Residuals
 !
 use pars,           ONLY:SP,rZERO,cZERO,cI
 use wrapper_omp,    ONLY:V_dot_V_omp,Vstar_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use BS,             ONLY:BS_H_dim,BS_K_dim
 use BS_solvers,     ONLY:BSS_eh_f
 use PHOTOLUM,       ONLY:BSS_dipoles_PL,BSS_PL_f
 !
 implicit none
 !
 logical,    intent(in)  :: K_is_not_hermitian
 complex(SP),intent(in)  :: BS_V_left(BS_H_dim,BS_H_dim),BS_V_right(BS_H_dim,BS_H_dim)
 real(SP)   ,intent(out) :: BS_R_PL(2,BS_H_dim)
 complex(SP),optional,intent(in)  :: BS_overlap(BS_H_dim,BS_H_dim)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i_l,i_c,i_K,i_lr,i_ll
 complex(SP)          ::P_x_fZ(2,BS_H_dim,3),Pi_x_R_x_adA_left(3),Pi_x_R_x_adA_right(3),adA_x_R_over_R_x_A(2),&
&                       R_over_R_x_A(2,BS_H_dim),B(BS_H_dim)
 !
 ! Parallel indexes
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BS_H_dim/))
 !
 ! sqrt(R_i) Re[ (Pi_i)* A^l_i (A^l_j)* R<_j/R_j A^l_j (A^l_k)* Pi_k ] sqrt(R_k)
 !
 ! Zeroing
 !
 BS_R_PL       =rZERO
 P_x_fZ        =cZERO
 R_over_R_x_A  =cZERO
 !
 !
 ! Live-Timing
 !
 call live_timing('PL    residuals',px%n_of_elements(myid+1))
 !
 ! Non-Hermitian residual
 !========================
 !
 if (K_is_not_hermitian) then
   !
   ! 1=> left
   ! 2=> right
   !
   do i_c=1,3
     do i_K=1,BS_K_dim(1)
       P_x_fZ(1,i_K,i_c)=conjg(BSS_dipoles_PL(i_K,i_c))*sqrt(BSS_eh_f(i_K))
       P_x_fZ(2,i_K,i_c)=      BSS_dipoles_PL(i_K,i_c) *sqrt(BSS_eh_f(i_K))
     enddo
     do i_K=BS_K_dim(1)+1,BS_H_dim
       P_x_fZ(1,i_K,i_c)=cI*conjg(BSS_dipoles_PL(i_K,i_c))*sqrt(BSS_eh_f(i_K-BS_K_dim(1)))
       P_x_fZ(2,i_K,i_c)=cI*      BSS_dipoles_PL(i_K,i_c) *sqrt(BSS_eh_f(i_K-BS_K_dim(1)))
     enddo
   enddo
   !
   do i_lr=1,BS_H_dim
     !
     if (.not.px%element_1D(i_lr)) cycle
     !
     ! B_i = \sum_lp Overlap^*(lp,l) A^ll_i
     !
     B=cZERO
     do i_ll=1,BS_H_dim
       B(:)=B(:)+conjg(BS_overlap(i_ll,i_lr))*BS_V_left(:,i_ll)
     enddo
     !
     ! R_over_R_x_A = R<_j/R_j B^l_j
     !
     R_over_R_x_A(1,:) = BSS_PL_f(:)*B(:)/BSS_eh_f(:)
     !
     ! adA_x_R_over_R_x_A = B^l*_j    R<_j/R_j B^l_j
     !
     adA_x_R_over_R_x_A(1)=Vstar_dot_V_omp(BS_H_dim,B,R_over_R_x_A(1,:))
     !
     do i_c=1,3
       !
       ! Pi_x_R_x_adA_left =   sqrt(R) Pi_i* A^lr_i
       !
       Pi_x_R_x_adA_left(i_c) =V_dot_V_omp(BS_H_dim,P_x_fZ(1,:,i_c),BS_V_right(:,i_lr))
       !
       ! Pi_x_R_x_adA_right=   A^lr*_i Pi_i sqrt(R)  
       !
       Pi_x_R_x_adA_right(i_c)=Vstar_dot_V_omp(BS_H_dim,BS_V_right(:,i_lr),P_x_fZ(2,:,i_c))
       !
     enddo
     !
     BS_R_PL(1,i_lr)=real((Pi_x_R_x_adA_left(1)*Pi_x_R_x_adA_right(1)+&
&                          Pi_x_R_x_adA_left(2)*Pi_x_R_x_adA_right(2)+&
&                          Pi_x_R_x_adA_left(3)*Pi_x_R_x_adA_right(3))*adA_x_R_over_R_x_A(1))
     !
     call live_timing(steps=1)
     !
   enddo
   !
 else
   !
   ! Hermitian residual
   !====================
   !
   do i_c=1,3
     P_x_fZ(1,:,i_c)=BSS_dipoles_PL(:,i_c)*sqrt(BSS_eh_f(:))
   enddo
   !
   do i_l=1,BS_K_dim(1)
     !
     if (.not.px%element_1D(i_l)) cycle
     !                       
     ! R_over_R_x_A = R<_j/R_j A^l_j
     !
     R_over_R_x_A(1,:) =  BSS_PL_f(1:BS_K_dim(1)) *BS_V_right(:,i_l)/BSS_eh_f(:)
     R_over_R_x_A(2,:) =  BSS_PL_f(BS_K_dim(1)+1:)*BS_V_right(:,i_l)/BSS_eh_f(:)
     !
     ! adA_x_R_over_R_x_A = (A^l_j)* R<_j/R_j A^l_j
     !
     adA_x_R_over_R_x_A(1)=V_dot_V_omp(BS_K_dim(1),conjg(BS_V_right(:,i_l)),R_over_R_x_A(1,:))
     adA_x_R_over_R_x_A(2)=V_dot_V_omp(BS_K_dim(1),conjg(BS_V_right(:,i_l)),R_over_R_x_A(2,:))
     !
     do i_c=1,3
       !                                   
       ! Pi_x_R_x_adA_right =   Pi sqrt(R)  A^l_i
       !
       Pi_x_R_x_adA_right(i_c)=V_dot_V_omp(BS_K_dim(1),BS_V_right(:,i_l),P_x_fZ(1,:,i_c))
       !
     enddo
     !
     ! Note that the 2nd component (anti-resonant) should take a -1  from BSS_eh_f and a i^2 from sqrt(R)
     ! that cancel out.
     !
     BS_R_PL(:,i_l)=dot_product(Pi_x_R_x_adA_right,Pi_x_R_x_adA_right)*adA_x_R_over_R_x_A(:)
     !
     call live_timing(steps=1)
     !
   enddo
   !
 endif
 !
 call live_timing()
 !
 call PP_redux_wait(BS_R_PL)
 !
 call PP_indexes_reset(px)
 !
end subroutine PL_diago_residual
