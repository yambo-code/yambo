!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine K_slepc_driver(iq,W) 
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
 use pars,           ONLY:SP,cZERO,cONE,DP
 use BS,             ONLY:BS_K_coupling,BS_H_dim
 use BS_solvers,     ONLY:io_BSS_diago,BSS_perturbative_width,&
&                         BS_mat,BSS_n_eig,BSS_target_E, &
&                         BSS_write_eig_2_db,BSS_eh_W, Slepc_shell, &
&                         BSS_slepc_extraction, BSS_slepc_ncv, BSS_slepc_tol
 use electrons,      ONLY:E_SOC_nbands
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec,warning
 use interfaces,     ONLY:MATRIX_slepc,K_diago_compute_epsilon
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 use com,            ONLY:msg
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 !
#include <memory.h>
 ! 
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 Mat           slepc_mat ! Slepc matrix where the hamiltonian will be stored
 Vec           vin,vout
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
 real(SP),   pointer     :: BS_R_PL(:,:)       => null()
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
 PetscFortranInt     :: vec_index(4)
 PetscFortranComplex :: vec_value(4)
 !
#if defined _TIMING
 call timing('Slepc Solver',OPR='start')
#endif
 !  
 ! Sectioning
 !============
 !
 if (isec(2)/=0) then
   call section('=','Slepc solver')
 else if (isec(2)==0) then
   call section('+','Slepc solver')
 endif
 !
 ! Number of states for partial diagonalization
 !================================================
 !
 if (BSS_n_eig==0) then
   BSS_n_eig = nint(BS_H_dim*.01)
   call warning(' Calculating only 1% of the eigenvalues.')
 endif
 !
 ! if the number of eigenvalues to calculate is larger than matrix dimensions
 !
 if (BSS_n_eig>BS_H_dim) then
   call warning(' Number of eigenvalues is larger than hamiltonian. Calculating only 1% of the eigenvalues.')
   BSS_n_eig = nint(BS_H_dim*.01)
 endif
 !
 ! if the number of eigenvalued to calculate is smaller than 1
 !
 if (BSS_n_eig<1) then
   call warning(' Number of eigenvalues is smaller than hamiltonian. Calculating 1 eigenvalue.')
   BSS_n_eig = 1
 endif
 !
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Allocation (BS_R and BS_E)
 !===========================
 call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
 allocate( BS_E(BSS_n_eig) )
 if(E_SOC_nbands/=0) then
   YAMBO_ALLOC( BS_corrections, (2,BS_H_dim) )
   YAMBO_ALLOC_P( BS_E_SOC_corr, (2,BS_H_dim) )
 endif
 YAMBO_ALLOC( BS_R_left, (BS_H_dim) )
 YAMBO_ALLOC( BS_R_right, (BS_H_dim) )
 !
 ! Diagonalization DB (IN)
 !========================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_BSS_diago( iq,ID,BS_H_dim,BSS_n_eig,BS_E,BS_R_left,BS_R_right,&
 &                    BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL )
 !
 ! Kernel loading
 !================
 !
 if(io_err<0) then
 !
 ! Here we will have two options to initialize the slepc matrix:
    call parser('BSSSlepcShell',Slepc_shell)
 !
 ! 1. Here we will define a PetscShell matrix and define the matrix-vector multiplication
 ! using the K_multiply_by_V function used in the Haydock method
    if (Slepc_shell) then
        call K_shell_matrix(iq,slepc_mat)
 !
 ! 2. Here we create a distributed PETSC matrix from the BS_blks
    else
        call K_stored_in_a_slepc_matrix(iq,slepc_mat)
    endif
 endif
 !
 K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
 !
 ! Allocation (BS_E_real or BS_V)
 !===============================
 if(K_is_not_hermitian) then
   YAMBO_ALLOC( BS_V_left, (BS_H_dim,BSS_n_eig) )
   YAMBO_ALLOC( BS_V_right, (BS_H_dim,BSS_n_eig) )
 else
   YAMBO_ALLOC( BS_mat, (BS_H_dim,BSS_n_eig) )
   YAMBO_ALLOC( BS_E_real, (BSS_n_eig) )
   if(BSS_perturbative_width.and..not.allocated(BS_corrections)) then
     YAMBO_ALLOC( BS_corrections, (1,BS_H_dim) )
   endif
 endif
 !
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open Diago")
 !
 if(io_err<0) then
   !
   ! Diagonalization of the excitonic hamiltonian
   !==============================================
   !
   if (K_is_not_hermitian) then
     !
     call MATRIX_slepc(slepc_mat,BSS_n_eig,E_cmpl=BS_E,&
&                      V_right=BS_V_right,V_left=BS_V_left,&
&                      target_energy=BSS_target_E, extraction=BSS_slepc_extraction,&
&                      slepc_ncv=BSS_slepc_ncv,slepc_tol=BSS_slepc_tol)
     !
   else
     !
     call MATRIX_slepc(slepc_mat,BSS_n_eig,E_real=BS_E_real,&
&                      V_right=BS_mat,&
&                      target_energy=BSS_target_E, extraction=BSS_slepc_extraction,&
&                      slepc_ncv=BSS_slepc_ncv,slepc_tol=BSS_slepc_tol)
     !
     BS_E=cmplx(BS_E_real,0._SP,SP)
     !
   endif
   !
   ! Destroy the matrix
   !============================
   call MatDestroy(slepc_mat,ierr)
   !
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
 YAMBO_FREE(BS_E)
 YAMBO_FREE(BS_E_real)
 YAMBO_FREE(BS_corrections)
 YAMBO_FREE(BS_R_right)
 YAMBO_FREE(BS_R_left)
 YAMBO_FREE(BS_V_right)
 YAMBO_FREE(BS_V_left)
 YAMBO_FREE(BS_mat)
 if(associated(BS_E_SOC_corr)) then
   deallocate(BS_E_SOC_corr)
   nullify(BS_E_SOC_corr)
 endif
 !
 call PARALLEL_Haydock_VEC_COMMs('reset')
 !
#if defined _TIMING
 call timing('Slepc Solver',OPR='stop')
#endif
 !
end subroutine
 
subroutine fill_eh_dipoles()
 !
 ! Fill dipoles and eh matrices
 !
 use pars,           ONLY:SP,cI,cONE
 use parallel_m,     ONLY:PP_redux_wait,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                         PAR_COM_Xk_ibz_INDEX
 use electrons,      ONLY:n_sp_pol,E_SOC_nbands
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,BS_K_coupling
 use BS_solvers,     ONLY:BSS_dipoles,BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,BSS_eh_Z,&
&                         BSS_eh_E_SOC_corr,BSS_perturbative_width
 use com,            ONLY:error,msg
 !
 implicit none
 !
 integer :: H_pos(2), i_c, i_v, i_spin, i_k_bz, i_g, i_cond
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
end subroutine
 
subroutine K_stored_in_a_slepc_matrix(iq,slepc_mat)
 !
 ! Here I fill the kernel (coupling not yet included) in a slepc matrix to be
 ! used for diagonalization and/or invertion
 !
 !      | (K_r)     (K_c)    |  
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 !
 use pars,           ONLY:SP,DP,cI,cONE
 use electrons,      ONLY:n_sp_pol,E_SOC_nbands
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,BS_K_coupling
 use BS_solvers,     ONLY:BSS_dipoles,BSS_eh_table,BSS_eh_E,BSS_eh_f,BSS_eh_W,BSS_eh_Z,&
&                         BSS_eh_E_SOC_corr,BSS_perturbative_width,BSS_alloc
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
 integer     :: ierr
 Mat   :: slepc_mat
 PetscFortranComplex :: val
 !
 ! Allocate the slepc Matrix
 !
 call MatCreate(PETSC_COMM_WORLD,slepc_mat,ierr)
 call MatSetSizes(slepc_mat,PETSC_DECIDE,PETSC_DECIDE,BS_H_dim,BS_H_dim,ierr)
 call MatSetType(slepc_mat, MATMPIDENSE,ierr)
 call MatSetUp(slepc_mat,ierr)
 ! 
 call BSS_alloc()
 !
 call fill_eh_dipoles()
 ! 
 ! filling of the slepc_mat
 !
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
         call MatSetValue( slepc_mat, H_pos(1), H_pos(2),       val , INSERT_VALUES, ierr )
         call MatSetValue( slepc_mat, H_pos(2), H_pos(1), conjg(val), INSERT_VALUES, ierr )
       endif
       !
       if (BS_blk(i_B)%mode=="C") then
         call MatSetValue( slepc_mat, H_pos(1), H_pos(2)+BS_K_dim,       val , INSERT_VALUES, ierr )
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
 endif
 !
 call MatAssemblyBegin(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 !
end subroutine K_stored_in_a_slepc_matrix





subroutine K_shell_matrix(iq,slepc_mat)
 !
 ! Create a PETSC ShellMatrix and define the matrix-vector product
 ! function using the routine K_multiply_by_V 
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_H_dim, BS_nT_grps, BS_blk, n_BS_blks
 use BS_solvers,     ONLY:Slepc_v,BS_HAYVEC_alloc,BSS_alloc
 !REMOVE
 use BS,            ONLY : BS_T_grp,BS_nT_grps,BS_blk,n_BS_blks,BS_H_dim
 use parallel_m,    ONLY : PAR_IND_T_Haydock, myid
 !REMOVE
 !
#include <memory.h>
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 !REMOVE
 integer        i_g
 !REMOVE 
 integer        iq, i_B, i_r, i_c, i_Tk, i_Tp
 Mat            slepc_mat
 PetscErrorCode ierr
 external K_multiply_by_V_slepc
 !
 ! Allocate haydock vectors
 ! 
 call PARALLEL_Haydock_VEC_COMMs('assign')
 allocate(Slepc_v%Vi(BS_nT_grps))
 allocate(Slepc_v%Vo(BS_nT_grps))
 call BS_HAYVEC_alloc(Slepc_v%Vi)
 call BS_HAYVEC_alloc(Slepc_v%Vo) 
 !
 ! Allocate slepc shell matrix
 !
 ! We let petsc decide which part of the matrix in each core.
 ! TODO: In the future it should be done acording to the BS parallelization
 ! to avoid the scattering vi (distributed) -> x (local) in K_multiply_by_V_slepc
 !
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,BS_H_dim,BS_H_dim,0,slepc_mat,ierr)
 call MatAssemblyBegin(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatShellSetOperation(slepc_mat,MATOP_MULT,K_multiply_by_V_slepc,ierr)
 call MatSetOption(slepc_mat,MAT_HERMITIAN,PETSC_TRUE,ierr)
 call MatSetUp(slepc_mat,ierr)
 !
 ! Allocate BS stuff
 !
 call BSS_alloc()
 call fill_eh_dipoles()
 ! 
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   if (i_Tk==i_Tp) then
     do i_r=1,BS_blk(i_B)%size(1)
       ! make the diagonal of the matrix real
       BS_blk(i_B)%mat(i_r,i_r) = real(BS_blk(i_B)%mat(i_r,i_r))
       ! make the matrix hermitian
       do i_c=i_r,BS_blk(i_B)%size(2)
         BS_blk(i_B)%mat(i_c,i_r) = conjg(BS_blk(i_B)%mat(i_r,i_c))
       enddo
     enddo
     !
   endif
   !
 enddo
 !
end subroutine K_shell_matrix



subroutine K_multiply_by_V_slepc(slepc_mat,vi,vo,ierr)
 !
 ! Interface with the K_multiply_by_V from the Haydock method
 ! This function has to take the same arguments as MatMult from PETSC
 !
 use pars,          ONLY : SP,DP,cZERO
 use BS_solvers,    ONLY : Slepc_v
 use BS,            ONLY : BS_T_grp,BS_nT_grps,BS_blk,n_BS_blks,BS_H_dim
 use parallel_m,    ONLY : PP_redux_wait,PAR_IND_T_Haydock,PAR_COM_T_Haydock
 !
 implicit none
 !
 PetscFortranComplex   :: tmp_value(1)
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 VecScatter           :: ctx
 PetscScalar, pointer :: xx(:)
 Vec                  :: vi,vo,x
 Mat                  :: slepc_mat
 integer              :: n, i_g, i_c, ierr, H_pos, start_index, low, high
 !
 ! create scattering context vi (distributed) -> x (local)
 !
 call VecScatterCreateToAll(vi,ctx,x,ierr);
 !
 ! scatter from vi (distributed) -> x (local)
 !
 call VecScatterBegin(ctx,vi,x,INSERT_VALUES,SCATTER_FORWARD,ierr);
 call VecScatterEnd(ctx,vi,x,INSERT_VALUES,SCATTER_FORWARD,ierr);
 !
 ! from x (local) build Slepc%Vi
 !
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
   start_index = sum(BS_T_grp(:i_g-1)%size)-1
   do i_c=1,BS_T_grp(i_g)%size
     H_pos=start_index+i_c
     !SLEPC funcitons expect C indexes both in Fortran and C
     call VecGetValues( x, 1, H_pos, tmp_value, ierr )
     Slepc_v%Vi(i_g)%fragment(i_c)=cmplx(tmp_value(1),kind=SP)
   enddo
 enddo
 !
 ! destroy scatter context and local vector when no longer needed
 !
 call VecScatterDestroy(ctx,ierr);
 call VecDestroy(x,ierr);
 !
 ! use the haydock routine to calculate the BSE auxiliaxy hamiltonian
 ! matrix-vector product
 ! Slepc_v%Vo = H Slepc_v%Vi
 !
 call K_multiply_by_V( Slepc_v%Vi, Slepc_v%Vo, 1 )
 !
 ! copy output vector Vo -> vo (distributed)
 !
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
   start_index = sum(BS_T_grp(:i_g-1)%size)-1
   do i_c=1,BS_T_grp(i_g)%size
     H_pos=start_index+i_c
     tmp_value=cmplx(Slepc_v%Vo(i_g)%fragment(i_c))
     !SLEPC funcitons expect C indexes both in Fortran and C
     call VecSetValues( vo, 1, H_pos, tmp_value, INSERT_VALUES, ierr )
   enddo
 enddo
 !
 call VecAssemblyBegin(vo,ierr)
 call VecAssemblyEnd(vo,ierr)
 !
end subroutine

