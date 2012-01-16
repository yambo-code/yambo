!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine K_diagonalization(iq,W) 
 !
 ! Generic diagonalization method to solve resonant and non resonant
 ! Hamiltonians.
 !
 use pars,           ONLY:SP,DP,pi
 use wrapper,        ONLY:M_by_M,Vstar_dot_V,V_dot_V
 use X_m,            ONLY:X_epsilon,X_drude
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,BS_K_coupling,&
&                         BSS_write_eig_2_db,BS_eh_f,BS_not_const_eh_f,&
&                         BS_eh_W,K_BSmat_by_V,BS_eh_E
#if defined _KERR
 use KERR,           ONLY:BSS_rhoq0_kerr,chi_nn,chi_jj
 use drivers,        ONLY:l_kerr
#endif
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:spin_occ
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK,USE_SLK,INV
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:d3k_factor,q_norm
 use com,            ONLY:isec
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,ncpu,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
#if defined _SC
 use fields,         ONLY:global_gauge
#endif
 implicit none
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 type(pp_indexes)     ::px
 integer              ::i1,lib_driver,BS_H_dim
 logical              ::K_is_not_hermitian
 !
 ! Residuals & Energies
 !
 complex(SP), allocatable ::BS_R(:) 
#if defined _KERR
 complex(SP), allocatable ::BS_R_kerr(:) 
#endif
 complex(SP), allocatable ::BS_E(:)
 !
 ! Resonant K
 !
 real(SP),    allocatable ::BS_E_real(:)
 !
 ! Coupling
 !
 complex(SP), allocatable ::BS_V_left(:,:)
 complex(SP), allocatable ::BS_V_right(:,:)
 complex(SP), allocatable ::BS_R_left(:)
 complex(SP), allocatable ::BS_R_right(:)
#if defined _KERR
 complex(SP), allocatable ::BS_R_right_kerr(:)
#endif
 complex(SP), allocatable ::BS_overlap(:,:)
 !
 ! I/O
 !
 integer              ::io_err,ID
 integer, external    ::ioBSS_diago
 !
#if defined _KERR
 real(SP)             :: factor,factor_jj
#endif
 !  
 ! Sectioning
 !
 if (isec(2)/=0) then
   call section('=','Diagonalization solver')
 else if (isec(2)==0) then
   call section('+','Diagonalization solver')
 endif
 !
 ! Eigenstates 2 DB ?
 !
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Allocation & Par Procs
 !
 K_is_not_hermitian=BS_K_coupling.or.allocated(BS_eh_W)
 !
 BS_H_dim=BS_K_dim
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 !
 !
 allocate(BS_R(BS_H_dim),BS_E(BS_H_dim))
 call mem_est("BS_R BS_E",(/BS_H_dim,BS_H_dim/))
#if defined _KERR
 if(l_kerr) then
   allocate(BS_R_kerr(BS_H_dim))
   call mem_est("BS_R_kerr",(/BS_H_dim/))
 endif
#endif
 !
 if (K_is_not_hermitian) then
   allocate(BS_V_left(BS_H_dim,BS_H_dim),BS_V_right(BS_H_dim,BS_H_dim))
   call mem_est("BS_V_left BS_V_right",(/BS_H_dim,size(BS_V_left),size(BS_V_right)/))
   allocate(BS_R_left(BS_H_dim),BS_R_right(BS_H_dim),BS_overlap(BS_H_dim,BS_H_dim))
   call mem_est("BS_R_left BS_R_right BS_overlap",(/BS_H_dim,BS_H_dim,size(BS_overlap)/))
   BS_overlap=(0.,0.)
#if defined _KERR
   if(l_kerr) then
     allocate(BS_R_right_kerr(BS_H_dim))
     call mem_est("BS_R_right_kerr",(/BS_H_dim/))
  endif
#endif
 else
   allocate(BS_E_real(BS_H_dim))
   call mem_est("BS_E_real",(/BS_H_dim/),(/SP/))
 endif
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/BS_H_dim/))
 !
 ! Diagonalization DB (IN)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=ioBSS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
 !
 if(io_err<0) then
   !
   ! Kernel loading
   !
#if defined _TEST_1
   call K_BSload(iq)
#else
   call K_BSmat_by_V(iq)
#endif
   !
   if (BS_not_const_eh_f) then
     !
     ! In case the f_i-f_j occupation factors are state-dependent
     ! here I multiply the kernel by BS_eh_f
     !
     do i1=1,BS_H_dim
       !
       if (i1<=BS_K_dim) then
         BS_mat(i1,i1)=BS_mat(i1,i1)-BS_eh_E(i1)
         BS_mat(i1,:) =BS_mat(i1,:)*BS_eh_f(i1)
         BS_mat(i1,i1)=BS_mat(i1,i1)+BS_eh_E(i1)
       else
         !
         ! Note that the -1 coming from the change in the sign of BS_eh_f
         ! is already embodied in BS_mat.
         !
         BS_mat(i1,i1)=BS_mat(i1,i1)+BS_eh_E(i1-BS_K_dim)
         BS_mat(i1,:) =BS_mat(i1,:)*BS_eh_f(i1-BS_K_dim)
         BS_mat(i1,i1)=BS_mat(i1,i1)-BS_eh_E(i1-BS_K_dim)
         !
       endif
       !
     enddo
   endif
   !
   ! eh damping 
   !
   if (allocated(BS_eh_W)) &
&     forall(i1=1:BS_K_dim) BS_mat(i1,i1)=BS_mat(i1,i1)+(0.,1.)*BS_eh_W(i1)
   !
 endif
 !
 ! Initialize the output file 
 !
 call K_output_file(iq,-2)
 !
 if(io_err<0) then
   !
   ! Diagonalization of the excitonic hamiltonian
   !
   lib_driver=USE_LK
   if (ncpu>1) lib_driver=USE_SLK
   call live_timing('BSK diagonalize',1)
   if (K_is_not_hermitian) then
     call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_cmpl=BS_E,&
&                     V_left=BS_V_left,V_right=BS_V_right)
     !
     if (BSS_write_eig_2_db) BS_mat=BS_V_right
     !
   else
     call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_real=BS_E_real)
     BS_E=BS_E_real
   endif
   call live_timing(steps=1)
   call live_timing
   !
   if (K_is_not_hermitian) then
     !
     !   The right eigenvector v(j) of A satisfies
     !                   A * v(j) = lambda(j) * v(j)
     !  where lambda(j) is its eigenvalue.
     !  The left eigenvector u(j) of A satisfies
     !                u(j)**H * A = lambda(j) * u(j)**H
     !  where u(j)**H denotes the conjugate transpose of u(j).
     !
     ! Remember that
     !
     ! 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
     !
     ! [1] BS_R_right(i)=<q0|j><j|i_R>
     !                  =conjg(BSS_rhoq0(j))*BS_V_right(j,i)
     !
     call live_timing('BSK R residuals',px%n_of_elements(myid+1))
     !
     do i1=1,BS_H_dim
       BS_R_right(i1)=cmplx(0.,0.,SP)
#if defined _KERR
       if(l_kerr) BS_R_right_kerr(i1)=cmplx(0.,0.,SP)
#endif
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R_right(i1)=Vstar_dot_V(BS_H_dim,BSS_rhoq0,BS_V_right(:,i1))
#if defined _KERR
       if(l_kerr) BS_R_right_kerr(i1)=Vstar_dot_V(BS_H_dim,BSS_rhoq0_kerr,BS_V_right(:,i1))
#endif
       !
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R_right)
#if defined _KERR
     if(l_kerr) call pp_redux_wait(BS_R_right_kerr)
#endif
     call live_timing
     !
     ! [2] BS_R(i)= <i_K|j><j|q0> 
     !            = conjg( BS_V_left(k,i))*BSS_rhoq0(k)*R_k
     !
     !     R(j) = f_v - f_c
     !
     ! In the right residuals I include the occupation factors
     ! contribution
     !
     BSS_rhoq0(:BS_K_dim)  = BSS_rhoq0(:BS_K_dim)  *BS_eh_f(:BS_K_dim)
     BSS_rhoq0(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)*BS_eh_f(:BS_K_dim)
     !
     call live_timing('BSK L residuals',px%n_of_elements(myid+1))
     !
     do i1=1,BS_H_dim
       BS_R(i1)=cmplx(0.,0.,SP)
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R(i1)=Vstar_dot_V(BS_H_dim,BS_V_left(:,i1),BSS_rhoq0)
       !
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R)
     !
     ! Remove the occupation factors from the oscillators (in case
     ! other solvers are used)
     !
     BSS_rhoq0(:BS_K_dim)  = BSS_rhoq0(:BS_K_dim)/  BS_eh_f(:BS_K_dim)
     BSS_rhoq0(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)/BS_eh_f(:BS_K_dim)
     !
     call live_timing
     !
     ! [3] BS_overlap(i,j)=conjg(BS_V_left(k,i))*BS_V_right(k,j)
     !
     call live_timing('BSK overlap mat',1)
     !
     call M_by_M('c','n',BS_H_dim,BS_V_left,BS_V_right,BS_overlap)
     !
     call mat_dia_inv(INV,USE_SLK,BS_overlap)
     call live_timing(steps=1)
     call live_timing()
     !
     ! [4] BS_R_left(i)=BS_overlap(i,j)BS_R(j)
     !
     call live_timing('BSK L x overlap',px%n_of_elements(myid+1))
     do i1=1,BS_H_dim
       BS_R_left(i1)=cmplx(0.,0.,SP)
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R_left(i1)=V_dot_V(BS_H_dim,BS_overlap(i1,:),BS_R)
       !
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R_left)
     call live_timing
     !
     ! [5] BS_R(i)=BS_R_left(i)BS_R_right(i)
     !
     do i1=1,BS_H_dim
       BS_R(i1)=BS_R_left(i1)*BS_R_right(i1)
#if defined _KERR
       if(l_kerr) BS_R_kerr(i1)=BS_R_left(i1)*BS_R_right_kerr(i1)
#endif
     enddo
     !
   else
     !
     !Resonant Residuals
     !------------------
     !
     ! BS_R(i)=\sum_k <q0|k><k|i> =
     !         \sum_k BS_mat(k,i) x conjg( BSS_rhoq0(k) )
     !
     call live_timing('BSK   residuals',px%n_of_elements(myid+1))
     BS_R=(0.,0.)
#if defined _KERR
     if(l_kerr) BS_R_kerr=(0.,0.)
#endif
     !
     do i1=1,BS_K_dim
       if (.not.px%element_1D(i1)) cycle
       !
       BS_R(i1)=Vstar_dot_V(BS_K_dim,BSS_rhoq0,BS_mat(:,i1))
#if defined _KERR
       if(l_kerr) BS_R_kerr(i1)=Vstar_dot_V(BS_K_dim,BS_mat(i1,:),BSS_rhoq0_kerr)
#endif
       !
       call live_timing(steps=1)
     enddo
     call live_timing
     !
     call pp_redux_wait(BS_R)
#if defined _KERR
     if(l_kerr) call pp_redux_wait(BS_R_kerr)
#endif
     !
   endif
 endif
 !
 !Now I calculate epsilon
 !
 ! eps2(iw)= 1 - Sum  BS_R(K)/(w +I eta - E)     (coupling or non-hermitian)
 !           1 - Sum |BS_R(K)|^2/(w +I eta - E)  (resonant)
 !
 X_epsilon(1,:)=(0.,0.)
 X_epsilon(2,:)=(0.,0.)
 X_epsilon(3,:)=X_epsilon(3,:)/real(ncpu,SP)
#if defined _KERR
 if(l_kerr) then
   X_epsilon(4,:)=X_epsilon(4,:)/real(ncpu,SP)
   X_epsilon(5,:)=(0.,0.)
 endif
#endif
 !
 call live_timing('BSK res epsilon',px%n_of_elements(myid+1))
 do i1=1,BS_H_dim
   if (.not.px%element_1D(i1)) cycle
   !
   ! Note the use of a causal expression here needed to avoid any
   ! distinction between the resonant and antiresonant e/h Green's
   ! functions.
   !
#if defined _SC
   if (trim(global_gauge)=='velocity') then
     if (K_is_not_hermitian)      X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)/(W%p(:)-BS_E(i1))/BS_E(i1)**2.
     if (.not.K_is_not_hermitian) X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)*conjg(BS_R(i1))/(W%p(:)-BS_E(i1))/real(BS_E(i1))**2.
  else
#endif
    if (K_is_not_hermitian)      X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)/(W%p(:)-BS_E(i1))
    if (.not.K_is_not_hermitian) X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)*conjg(BS_R(i1))/(W%p(:)-BS_E(i1))
#if defined _SC
  endif
#endif
   !
#if defined _KERR
   if(l_kerr) then
     if (K_is_not_hermitian)      X_epsilon(5,:)=X_epsilon(5,:)-BS_R_kerr(i1)/(W%p(:)-BS_E(i1))
     if (.not.K_is_not_hermitian) X_epsilon(5,:)=X_epsilon(5,:)-BS_R(i1)*BS_R_kerr(i1)/(W%p(:)-BS_E(i1))
   endif
#endif
   !
   call live_timing(steps=1)
 enddo
 call live_timing
 call pp_redux_wait(X_epsilon)
 !
#if defined _KERR
 factor=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi
 factor_jj=X_epsilon(3,1)
 !
 if(chi_nn) X_epsilon(2,:)=1.+(X_epsilon(2,:)+X_drude(:))*factor/   q_norm(1)**2
 if(chi_jj) X_epsilon(2,:)=1.+(X_epsilon(2,:)-factor_jj)*factor/real(W%p(:))**2 &
&                                            +X_drude(:)*factor
 !
 if(l_kerr) then
   if(chi_nn) X_epsilon(5,:)=X_epsilon(5,:)*factor/   q_norm(1)**2
   if(chi_jj) X_epsilon(5,:)=X_epsilon(5,:)*factor/real(W%p(:))**2
 endif
#else
 X_epsilon(2,:)=1.+X_epsilon(2,:)*&
&                  real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/(q_norm(1))**2
#endif
 !
 X_epsilon(1,:)=W%p(:)
 !
 ! I write the output file 
 !
 call K_output_file(iq,2)
 !
#if defined _KERR
 if(l_kerr) call K_kerr_output()
#endif
 !
 ! Diagonalization DB (OUT)
 !
 if (io_err/=0) then
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=ioBSS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
 endif
 !
 ! CLEAN
 !
 if (allocated(BS_mat)) deallocate(BS_mat)
 call mem_est("BS_mat")
 deallocate(BS_R,BS_E)
 call mem_est("BS_R BS_E")
#if defined _KERR
 if(l_kerr) then
   deallocate(BS_R_kerr)
   call mem_est("BS_R_kerr")
 endif
#endif
 if (K_is_not_hermitian) then
   deallocate(BS_V_left,BS_V_right,BS_R_left,BS_R_right)
   call mem_est("BS_V_left BS_V_right BS_R_left BS_R_right")
#if defined _KERR
   if(l_kerr) then
     deallocate(BS_R_right_kerr)
     call mem_est("BS_R_right_kerr")
   endif
#endif
 else
   deallocate(BS_E_real)
   call mem_est("BS_E_real")
 endif
 call pp_indexes_reset(px)
 !
end subroutine
