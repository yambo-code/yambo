!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DS, HM
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
subroutine K_slepc(iq,W) 
 !
 ! 10/06/2015 Added by HM
 ! Generic diagonalization method to solve hermitian hamiltonians.
 !
 ! Based on the routine K_diago_driver.F modified to use Slepc
 !
 ! The implementation can be done in three different ways:
 ! 1. Generate the hamiltonian as for diagonalization with lapack/scalapack and 
 ! call the slepc routine
 ! 2. The relevant parts of the matrix are generated among the different mpi
 ! processes and slepc is called to diagonalize it
 ! 3. The matrix is not generated but instead calls to K_dot_product are used
 !
 ! The moficiations in this file only become relevant once we start steps 2 and
 ! 3 of the implementation
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_K_coupling,io_BSS_diago,BSS_perturbative_width,&
&                         BSS_write_eig_2_db,BSS_eh_W,BS_H_dim,BSS_n_eig,&
&                         BS_mat_free,BS_mat
 use electrons,      ONLY:E_SOC_nbands
 use matrix_operate, ONLY:DIAGO
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec,warning
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:MATRIX_slepc,K_diago_compute_epsilon
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 use com,            ONLY:msg
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
 implicit none
 !
#include <yambo_common.h>
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 type(Mat)      slepc_mat ! Slepc matrix where the hamiltonian will be stored
 type(w_samp)  :: W
 integer       :: iq
 logical       :: K_is_not_hermitian
 integer       :: ierr
 !
 ! Residuals & Energies
 !
 complex(SP),allocatable :: BS_E(:)          !Eigenvalues
 complex(SP),allocatable :: BS_R_left(:)     !Left eigenvectors
 complex(SP),allocatable :: BS_R_right(:)    !Right eigenvectors
 real(SP),   pointer     :: BS_R_PL(:)         => null()
 real(SP)   ,pointer     :: BS_E_SOC_corr(:,:) => null()
 complex(SP),pointer     :: BS_R_right_kerr(:) => null()
 !
 real(SP),allocatable    :: BS_E_real(:)        ! Resonant K
 complex(SP),allocatable :: BS_corrections(:,:) ! Perturbative BSE
 complex(SP),allocatable :: BS_V_left(:,:)      ! Coupling
 complex(SP),allocatable :: BS_V_right(:,:)     ! Coupling
 !
 integer              ::io_err,ID  ! I/O
 !
#if defined _TIMING
 call timing('Slepc Solver',OPR='start')
#endif
 !  
 ! Sectioning
 !============
 if (isec(2)/=0) then
   call section('=','Slepc solver')
 else if (isec(2)==0) then
   call section('+','Slepc solver')
 endif
 !
 ! Number of states for partial diagonalization
 !================================================
 if (BSS_n_eig==0) then
   BSS_n_eig = BS_H_dim*.01
   call warning(' Calculating only 1% of the eigenvalues.')
 endif
 !
 ! if the number of eigenvalues to calculate is larger than matrix dimensions
 if (BSS_n_eig>BS_H_dim) then
   call warning(' Number of eigenvalues is larger than hamiltonian. Calculating only 1% of the eigenvalues.')
   BSS_n_eig = BS_H_dim*.01
 endif
 !
 ! if the number of eigenvalued to calculate is smaller than 1
 if (BSS_n_eig<1) then
   call warning(' Number of eigenvalues is smaller than hamiltonian. Calculating 1 eigenvalue.')
   BSS_n_eig = 1
 endif
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Allocation (BS_R and BS_E)
 !===========================
 call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
 YAMBO_ALLOCATE( BS_E(BSS_n_eig) )
 if(E_SOC_nbands/=0) then
   YAMBO_ALLOCATE( BS_corrections(2,BS_H_dim) )
   YAMBO_ALLOCATE( BS_E_SOC_corr(2,BS_H_dim) )
 endif
 YAMBO_ALLOCATE( BS_R_left(BS_H_dim) )
 YAMBO_ALLOCATE( BS_R_right(BS_H_dim) )
 !
 ! Diagonalization DB (IN)
 !========================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_BSS_diago( iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,&
 &                    BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL )
 !
 ! Kernel loading
 !================
 if(io_err<0) call K_stored_in_a_slepc_matrix(iq,slepc_mat)
 !
 K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
 !
 ! Allocation (BS_E_real or BS_V)
 !===============================
 if(K_is_not_hermitian) then
   YAMBO_ALLOCATE( BS_V_left(BS_H_dim,BSS_n_eig) )
   YAMBO_ALLOCATE( BS_V_right(BS_H_dim,BSS_n_eig) )
 else
   YAMBO_ALLOCATE( BS_V_right(BS_H_dim,BSS_n_eig) )
   YAMBO_ALLOCATE( BS_E_real(BSS_n_eig) )
   if(BSS_perturbative_width.and..not.allocated(BS_corrections)) then
     YAMBO_ALLOCATE( BS_corrections(1,BS_H_dim) )
   endif
 endif
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open Diago")
 !
 if(io_err<0) then
   !
   ! Diagonalization of the excitonic hamiltonian
   !==============================================
   call live_timing('BSK diagonalize',1)
   !
   if (K_is_not_hermitian) then
     !
     call MATRIX_slepc(slepc_mat,BSS_n_eig,E_cmpl=BS_E, V_right=BS_V_right,V_left=BS_V_left)
     !
   else
     !
     call MATRIX_slepc(slepc_mat,BSS_n_eig,E_real=BS_E_real, V_right=BS_V_right)
     BS_mat = BS_V_right
     !
     BS_E=cmplx(BS_E_real,0._SP,SP)
     !
   endif
   !
   call live_timing(steps=1)
   call live_timing
   !
   ! Construct the residuals of epsilon
   !====================================
   if (K_is_not_hermitian) then
     !
     call K_diago_non_hermitian_residuals(BS_E,BS_R_left,BS_R_right,BS_V_left,BS_V_right)
     !
   else
     !
     call K_diago_hermitian_residuals(BS_E_real,BS_R_left,BS_R_right)
     !write(*,*) BS_R_left(1)
     !
   endif
   !
 endif
 !
 ! Now I calculate epsilon
 !=========================
 call K_diago_compute_epsilon(iq,W,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 !
 ! Write the output file 
 !
 call K_output_file(iq,"Diago")
 call msg('s','Diagonalization DB (OUT)')
 !
 ! Diagonalization DB (OUT)
 !==========================
 if (io_err/=0) then
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=io_BSS_diago(iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,&
                      &BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 endif
 !
 ! CLEAN
 !=======
 call PetscFinalize(ierr)
 call BS_mat_free( )
 YAMBO_DEALLOCATE_A(BS_E)
 YAMBO_DEALLOCATE_A(BS_E_real)
 YAMBO_DEALLOCATE_A(BS_corrections)
 YAMBO_DEALLOCATE_A(BS_R_right)
 YAMBO_DEALLOCATE_A(BS_R_left)
 YAMBO_DEALLOCATE_A(BS_V_right)
 YAMBO_DEALLOCATE_A(BS_V_left)
 if(associated(BS_E_SOC_corr)) then
   deallocate(BS_E_SOC_corr)
   nullify(BS_E_SOC_corr)
 endif
 !
#if defined _TIMING
 call timing('Slepc Solver',OPR='stop')
#endif
 !
end subroutine
  
subroutine K_stored_in_a_slepc_matrix(iq,slepc_mat)
 !
 ! Here I fill the kernel (coupling included) in a slepc matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r)     (K_c)    |  
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 !
 use pars,           ONLY:SP,cI,cONE
 use electrons,      ONLY:n_sp_pol,E_SOC_nbands
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_COM_Xk_ibz_INDEX
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,BS_K_coupling,&
&                         BSS_dipoles,BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,BSS_eh_Z,&
&                         BSS_eh_E_SOC_corr,BSS_perturbative_width,BS_mat_alloc_BSS
 use com,            ONLY:error,msg
 !
 implicit none
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 complex(SP) :: E_plus_W
 integer     :: iq,i_c,i_r,H_pos(2),i_Tk,i_Tp,i_B,i_g,i_k_bz,i_v,i_cond,i_spin
 type(Mat)   :: slepc_mat
 complex     :: val
 !
 integer     :: ierr
 !
 ! ALLOCATE SLEPC MATRIX
 !==================================
 call MatCreate(PETSC_COMM_WORLD,slepc_mat,ierr)
 call MatSetSizes(slepc_mat,PETSC_DECIDE,PETSC_DECIDE,BS_H_dim,BS_H_dim,ierr)
 call MatSetType(slepc_mat, MATMPIDENSE,ierr)
 call MatSetUp(slepc_mat,ierr)
 
 call BS_mat_alloc_BSS()
 ! 
 ! FILLING BSS_eh_* and BSS_dipoles
 !==================================
 do i_g=1,BS_nT_grps
   !
   if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle
   !
   do i_c=1,BS_T_grp(i_g)%size
     !
     i_k_bz=BS_T_grp(i_g)%table(i_c,1)
     i_v   =BS_T_grp(i_g)%table(i_c,2)
     i_cond=BS_T_grp(i_g)%table(i_c,3)
     i_spin=BS_T_grp(i_g)%table(i_c,4)
     !
     H_pos(1)=sum(BS_T_grp(:I_g-1)%size)+i_c
     !
     if (n_sp_pol==2) BSS_eh_table(H_pos(1),:)=(/i_k_bz,i_v,i_cond,i_spin/)
     if (n_sp_pol==1) BSS_eh_table(H_pos(1),:)=(/i_k_bz,i_v,i_cond/)
     !
     BSS_eh_E(H_pos(1))=BS_T_grp(i_g)%E(i_c)
     BSS_eh_f(H_pos(1))=BS_T_grp(i_g)%f(i_c)
     if (allocated(BS_T_grp(i_g)%Z)) BSS_eh_Z(H_pos(1))=BS_T_grp(i_g)%Z(i_c)
     if (allocated(BS_T_grp(i_g)%W)) BSS_eh_W(H_pos(1))=BS_T_grp(i_g)%W(i_c)
     !
     BSS_dipoles(H_pos(1))=BS_T_grp(i_g)%dipoles_R(i_c)
     if (BS_K_coupling) BSS_dipoles(H_pos(1)+BS_K_dim)=BS_T_grp(i_g)%dipoles_A(i_c)
     !
     ! GPL_EXCLUDE_START
     !
     if (E_SOC_nbands/=0) BSS_eh_E_SOC_corr(:,H_pos(1))=BS_T_grp(i_g)%E_SOC_corr(:,i_c)
     !
     ! GPL_EXCLUDE_END
     !
   enddo
 enddo
 !
 ! ALL 2 ALL
 !===========
 !
 ! BSS_eh_* and BSS_dipoles are distributed ONLY among k-points and eh transitions.
 !
 call PP_redux_wait(BSS_eh_E,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_eh_E,COMM=PAR_COM_eh_INDEX%COMM)
 call PP_redux_wait(BSS_eh_f,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_eh_f,COMM=PAR_COM_eh_INDEX%COMM)
 call PP_redux_wait(BSS_eh_table,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_eh_table,COMM=PAR_COM_eh_INDEX%COMM)
 if (allocated(BSS_eh_W)) then
   call PP_redux_wait(BSS_eh_W,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_eh_W,COMM=PAR_COM_eh_INDEX%COMM)
 endif
 if (allocated(BSS_eh_Z)) then
   call PP_redux_wait(BSS_eh_Z,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_eh_Z,COMM=PAR_COM_eh_INDEX%COMM)
 endif
 !
 call PP_redux_wait(BSS_dipoles,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait(BSS_dipoles,COMM=PAR_COM_eh_INDEX%COMM)
 !
 ! GPL_EXCLUDE_START
 !
 if (E_SOC_nbands/=0) then
   call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
   call PP_redux_wait(BSS_eh_E_SOC_corr,COMM=PAR_COM_eh_INDEX%COMM)
 endif
 !
 ! GPL_EXCLUDE_END
 !
 ! FILLING the slepc_mat
 !====================
 !
 call msg('s','Filling the slepc_mat')
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   do i_r=1,BS_blk(i_B)%size(1)
     !
     H_pos(1)=sum(BS_T_grp(:I_Tk-1)%size)+i_r-1
     !
     ! First the diagonal
     !
     !val = 123
     !call MatSetValue( slepc_mat, 0, 0, val, INSERT_VALUES, ierr )
     if (i_Tk==i_Tp) then
       if (BS_blk(i_B)%mode=="R") then
         E_plus_W=BSS_eh_E(H_pos(1)+1)*cONE
         if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
            E_plus_W=E_plus_W-cI*BSS_eh_W(H_pos(1)+1)
         endif
         val = E_plus_W+real(BS_blk(i_B)%mat(i_r,i_r),SP)
         call MatSetValue( slepc_mat, H_pos(1),          H_pos(1), val, INSERT_VALUES, ierr )
       else
         val = BS_blk(i_B)%mat(i_r,i_r)
         call MatSetValue( slepc_mat, H_pos(1)+BS_K_dim, H_pos(1), val, INSERT_VALUES, ierr )
       endif
       !
     endif
     !
     ! Then the upper triangle of each block
     !
     do i_c=1,BS_blk(i_B)%size(2)
       !
       H_pos(2)=sum(BS_T_grp(:I_Tp-1)%size)+i_c-1
       !
       if (H_pos(1)>=H_pos(2)) cycle
       !
       val = BS_blk(i_B)%mat(i_r,i_c)
       if (BS_blk(i_B)%mode=="R") then
         call MatSetValue( slepc_mat, H_pos(1), H_pos(2), val,        INSERT_VALUES, ierr )
         call MatSetValue( slepc_mat, H_pos(2), H_pos(1), conjg(val), INSERT_VALUES, ierr )
       endif
       !
       if (BS_blk(i_B)%mode=="A") then
         call MatSetValue( slepc_mat, H_pos(1), H_pos(2)+BS_K_dim, val,        INSERT_VALUES, ierr )
         call MatSetValue( slepc_mat, H_pos(2)+BS_K_dim, H_pos(1), conjg(val), INSERT_VALUES, ierr )
       endif
       !
     enddo
     !
   enddo
   !
 enddo
 !
 if (BS_K_coupling) then
   call error('BSE coupling with slepc not implemented')
   !
   ! Symmetrization: Coupling part is symmetric
   !
   !do i_r=1,BS_K_dim
   !  do i_c=i_r+1,BS_K_dim
   !    slepc_mat(i_c,i_r+BS_K_dim)=slepc_mat(i_r,i_c+BS_K_dim)
   !  enddo
   !enddo
   !
   ! If Coupling the half lower part of K is empty
   ! Filling the anti-resonant and anti-coupling parts
   !
   !forall(i_r=1:BS_K_dim,i_c=1:BS_K_dim)
     ! Anti-Coupling
     !slepc_mat(i_r+BS_K_dim,i_c)=-conjg(slepc_mat(i_r,i_c+BS_K_dim))
     ! Anti-resonant
     !slepc_mat(i_r+BS_K_dim,i_c+BS_K_dim)=-conjg(slepc_mat(i_r,i_c))
   !end forall
   !
 endif
 !
 call MatAssemblyBegin(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 !
 call msg('s','done!')
 !
end subroutine K_stored_in_a_slepc_matrix
