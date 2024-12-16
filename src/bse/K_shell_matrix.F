!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): HM IMA
!
subroutine K_shell_matrix(i_BS_mat,slepc_mat)
 !
 ! Create a PETSC ShellMatrix and define the matrix-vector product
 ! function using the routine K_multiply_by_V 
 !
 use BS,             ONLY:BS_H_dim, BS_nT_grps,BS_res_ares_n_mat,BS_K_coupling,&
&                         BS_Blocks_symmetrize_K,l_BS_ares_from_res
 use BS_solvers,     ONLY:BSS_kernel_IO_on_the_fly,BSS_slepc_pseudo_herm
 !
 !      | (K_r)     (cI*K_c)    |  
 !  K = |                       |
 !      | (-cI*K_c^*)  (-K_r^*) |
 !
 use BS_solvers,     ONLY:Slepc_v,BS_HAYVEC_alloc
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 use petscmat
 use slepceps
 use petscmatdef
 use slepcepsdef
 !
#include <y_memory.h>
 !
 integer, intent(in)  :: i_BS_mat
 Mat,     intent(out) :: slepc_mat
 Mat                  :: slepc_mat_R,slepc_mat_C
 !
 integer          :: i_B,domk(BS_nT_grps)
 PetscInt         :: SL_H_dim,SL_K_dim
 PetscErrorCode   :: ierr
 external K_multiply_by_V_slepc
 external K_multiply_by_V_slepc_C
 external K_multiply_by_V_slepc_R
 external K_multiply_by_V_transpose_slepc
 !
 if (BS_res_ares_n_mat==2) call error(" Slepc with shells and multiple BS_mat not coded")
 !
 SL_H_dim=BS_H_dim
 !
 ! Allocate haydock vectors
 ! 
 call PARALLEL_Haydock_VEC_COMMs('assign')
 Slepc_v%it=1
 allocate(Slepc_v%Vi(BS_nT_grps))
 allocate(Slepc_v%Vo(BS_nT_grps))
 domk=1
 call BS_HAYVEC_alloc(Slepc_v%Vi,domk)
 call BS_HAYVEC_alloc(Slepc_v%Vo,domk)
 !
 ! Allocate slepc shell matrix
 !
 ! We let petsc decide which part of the matrix in each core.
 ! TODO: In the future it should be done acording to the BS parallelization
 ! to avoid the scattering vi (distributed) -> x (local) in K_multiply_by_V_slepc
 !
 if(BS_K_coupling) then
   if (BSS_slepc_pseudo_herm) then
     call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,slepc_mat_R,ierr)
     call MatSetOption(slepc_mat_R,MAT_HERMITIAN,PETSC_TRUE,ierr)
     call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,slepc_mat_C,ierr)
     call MatSetOption(slepc_mat_C,MAT_SYMMETRIC,PETSC_TRUE,ierr)
     call MatShellSetOperation(slepc_mat_R,MATOP_MULT,K_multiply_by_V_slepc_R,ierr)
     call MatShellSetOperation(slepc_mat_C,MATOP_MULT,K_multiply_by_V_slepc_C,ierr)
     call MatCreateBSE(slepc_mat_R,slepc_mat_C,slepc_mat,ierr)
     call MatDestroy(slepc_mat_R,ierr)
     call MatDestroy(slepc_mat_C,ierr)
   else
     call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim,SL_H_dim,0,slepc_mat,ierr)
     call MatShellSetOperation(slepc_mat,MATOP_MULT,K_multiply_by_V_slepc,ierr)
     call MatShellSetOperation(slepc_mat,MATOP_MULT_TRANSPOSE,K_multiply_by_V_transpose_slepc,ierr)
     call MatSetOption(slepc_mat,MAT_HERMITIAN,PETSC_FALSE,ierr)
   endif
 else
   call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim,SL_H_dim,0,slepc_mat,ierr)
   call MatShellSetOperation(slepc_mat,MATOP_MULT,K_multiply_by_V_slepc,ierr)
   call MatSetOption(slepc_mat,MAT_HERMITIAN,PETSC_TRUE,ierr)
 endif
 !
 ! Symmetrize BS_blk%mat to be used in K_multiply_by_V
 !
 if(.not.BSS_kernel_IO_on_the_fly) call BS_blocks_symmetrize_K()
 !
end subroutine K_shell_matrix
