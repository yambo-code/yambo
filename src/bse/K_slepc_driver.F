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
 use timing_m,       ONLY:timing
 ! 
#include<memory.h>
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 Mat           slepc_mat ! Slepc matrix where the hamiltonian will be stored
 type(w_samp)  :: W
 integer       :: iq
 logical       :: K_is_not_hermitian
 !
 PetscErrorCode        :: ierr
 PetscFortranInt       :: slepc_ncv
#if defined _DOUBLE
 PetscFortranDouble    :: slepc_tol
 PetscFortranDouble    :: slepc_target_E
#else
 PetscFortranFloat     :: slepc_tol
 PetscFortranFloat     :: slepc_target_E
#endif
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
 call timing('Slepc Solver',OPR='start')
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
 slepc_ncv      = BSS_slepc_ncv
 slepc_tol      = BSS_slepc_tol
 slepc_target_E = BSS_target_E
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 !
 ! Allocation (BS_R and BS_E)
 !===========================
 call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
 YAMBO_ALLOC(BS_E,(BSS_n_eig))
 if(E_SOC_nbands/=0) then
   YAMBO_ALLOC(BS_corrections,(2,BS_H_dim))
   YAMBO_ALLOC_P(BS_E_SOC_corr,(2,BS_H_dim))
 endif
 YAMBO_ALLOC(BS_R_left,(BS_H_dim))
 YAMBO_ALLOC(BS_R_right,(BS_H_dim))
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
   YAMBO_ALLOC(BS_V_left,(BS_H_dim,BSS_n_eig))
   YAMBO_ALLOC(BS_V_right,(BS_H_dim,BSS_n_eig))
 else
   YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
   YAMBO_ALLOC(BS_E_real,(BSS_n_eig))
   if(BSS_perturbative_width.and..not.allocated(BS_corrections)) then
     YAMBO_ALLOC(BS_corrections,(1,BS_H_dim))
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
&                      target_energy=slepc_target_E, extraction=BSS_slepc_extraction,&
&                      slepc_ncv=slepc_ncv,slepc_tol=slepc_tol)
     !
   else
     !
     call MATRIX_slepc(slepc_mat,BSS_n_eig,E_real=BS_E_real,&
&                      V_right=BS_mat,&
&                      target_energy=slepc_target_E, extraction=BSS_slepc_extraction,&
&                      slepc_ncv=slepc_ncv,slepc_tol=slepc_tol)
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
 YAMBO_FREE_P(BS_E_SOC_corr)
 !
 call timing('Slepc Solver',OPR='stop')
 !
end subroutine K_slepc_driver
