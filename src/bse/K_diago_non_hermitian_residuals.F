!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine K_diago_non_hermitian_residuals(BS_E,BS_R_left,BS_R_right,BS_V_left,BS_V_right,BS_overlap) 
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
 use pars,           ONLY:SP,cZERO
 use wrapper_omp,    ONLY:Vstar_dot_V_omp,V_dot_V_omp
 use parallel_m,     ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,   ONLY:PP_redux_wait,PARALLEL_index 
 use LIVE_t,         ONLY:live_timing
 use X_m,            ONLY:global_gauge
 use BS,             ONLY:BS_H_dim
 use BS_solvers,     ONLY:BSS_dipoles_opt,BSS_eh_E,BSS_eh_Z,BSS_eh_f,BSS_n_eig
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_E(BSS_n_eig)
 complex(SP),intent(out) :: BS_R_left(BSS_n_eig),BS_R_right(BSS_n_eig)
 complex(SP),intent(in)  :: BS_overlap(BSS_n_eig,BSS_n_eig)
 complex(SP),intent(in)  :: BS_V_left(BS_H_dim,BSS_n_eig),BS_V_right(BS_H_dim,BSS_n_eig)
 !
 ! Workspace
 !
 type(PP_indexes)     ::px
 integer              ::i1
 !
 complex(SP) :: tmp_res(BS_H_dim)
 complex(SP) :: BS_R_tmp(BSS_n_eig)       
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/BSS_n_eig/))
 !
 ! [1] BS_R_right(i)=<q0|j><j|i_R>
 !                  =conjg(BSS_dipoles(j))*BS_V_right(j,i)
 !
 BS_R_right=cZERO
 !
 call live_timing('EPS R residuals',px%n_of_elements(myid+1))
 !
 tmp_res = conjg(BSS_dipoles_opt(1,:))*sqrt(cmplx(BSS_eh_f,kind=SP))
 !
 ! DS. Note. Velocity gauge fixing does not work if neq occupations are loaded. Not sure why.
 if (trim(global_gauge)=="velocity") tmp_res=tmp_res/BSS_eh_E
 if (allocated(BSS_eh_Z)           ) tmp_res=tmp_res*sqrt(BSS_eh_Z)
 !
 do i1=1,BSS_n_eig
   if (.not.px%element_1D(i1)) cycle
   !
   BS_R_right(i1)=V_dot_V_omp(BS_H_dim,tmp_res,BS_V_right(:,i1))
   if (trim(global_gauge)=="velocity") BS_R_right(i1)=BS_R_right(i1)*BS_E(i1)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_right)
 call live_timing
 !
 ! [2] BS_R_tmp(i)= <i_K|j><j|q0> 
 !                = conjg( BS_V_left(k,i))*BSS_dipoles(k)*R_k
 !
 BS_R_tmp=cZERO
 !
 call live_timing('EPS L residuals',px%n_of_elements(myid+1))
 !
 tmp_res = BSS_dipoles_opt(1,:)*sqrt(cmplx(BSS_eh_f,kind=SP))
 !
 do i1=1,BSS_n_eig
   !
   if (.not.px%element_1D(i1)) cycle
   !
   BS_R_tmp(i1)=Vstar_dot_V_omp(BS_H_dim,BS_V_left(:,i1),tmp_res)
   if (trim(global_gauge)=="velocity") BS_R_tmp(i1)=BS_R_tmp(i1)*BS_E(i1)
   !
   call live_timing(steps=1)
   !
 enddo
 call PP_redux_wait(BS_R_tmp)
 !
 call live_timing()
 !
 ! [3] BS_R_left(i)=BS_overlap(i,j)BS_R_tmp(j)
 !
 BS_R_left=cZERO
 !
 call live_timing('EPS L x overlap',px%n_of_elements(myid+1))
 do i1=1,BSS_n_eig
   if (.not.px%element_1D(i1)) cycle
   !
   BS_R_left(i1)=V_dot_V_omp(BSS_n_eig,BS_overlap(i1,:),BS_R_tmp)
   !
   call live_timing(steps=1)
 enddo
 call PP_redux_wait(BS_R_left)
 call live_timing
 !
 call PP_indexes_reset(px)
 !
end subroutine K_diago_non_hermitian_residuals
!
!
subroutine K_diago_overlap_matrix(BS_V_left,BS_V_right,BS_overlap)
 !
 use pars,           ONLY:SP,cZERO,cONE
 use wrapper_omp,    ONLY:M_by_M_omp
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver 
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:INV
 use BS,             ONLY:BS_H_dim
 use BS_solvers,     ONLY:BSS_n_eig
 !
 implicit none
 !
 complex(SP),intent(in)  :: BS_V_left(BS_H_dim,BSS_n_eig),BS_V_right(BS_H_dim,BSS_n_eig)
 complex(SP),intent(out) :: BS_overlap(BSS_n_eig,BSS_n_eig)
 !
 ! BS_overlap(i,j)=sum_k conjg(BS_V_left(k,i))*BS_V_right(k,j)
 !
 BS_overlap=cZERO
 !
 call live_timing('BSK overlap mat',1)
 !
 call M_by_M_omp('c','n',BSS_n_eig,BSS_n_eig,BS_H_dim,cONE,BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,cZERO,BS_overlap,BSS_n_eig)
 !
 call LINEAR_ALGEBRA_driver(INV,M=BS_overlap)
 call live_timing(steps=1)
 call live_timing()
 !
end subroutine K_diago_overlap_matrix
