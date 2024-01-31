!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): BM
!
module shellmat_module
#include <petsc/finclude/petscmat.h>
  use petscmat
  Mat::R,C
end module shellmat_module

subroutine ignore_petsc_error_handler(comm,line,fun,file,n,p,mess,ctx,ierr)
!
! Handler to ignore error if MatMultHermitianTranspose cannot be set.
! This operation is only available for shell matrices in PETSc from v3.21.
! Setting MatMultTranspose is enough, but it is preferable to set both.
!
  use petscsysdef
  integer line,n,p
  PetscInt ctx 
  PetscErrorCode ierr
  MPI_Comm comm
  character*(*) fun,file,mess
  ierr = 0
  return
end subroutine ignore_petsc_error_handler

subroutine K_stored_in_a_nest_matrix(i_BS_mat,slepc_mat)
!
! BS_K_coupling and l_BS_ares_from_res are assumed TRUE
!
! K is stored in a PETSc nest matrix, taking advantage of the four block
! structure of the matrix in this case:
!
! | R     C  |
! |-C^*  -R^T|
!
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
!
  use pars,           ONLY:cI,cONE
  use BS,             ONLY:BS_K_dim,BS_H_dim,BS_blk,n_BS_blks
  use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_W,BSS_perturbative_width
  use cuda_m,         ONLY:have_cuda
  !
  use petscmat
  use slepceps
  use shellmat_module
  !
  implicit none
  !
  integer, intent(in)  :: i_BS_mat
  Mat,     intent(out) :: slepc_mat
  Mat                  :: RT,CHT
  Mat                  :: matArray(4)
  !
  integer     :: i_c,i_r,i_Tk,i_Tp,i_B,H_shift(2)
  PetscScalar         :: Mij,Mij_star
  PetscScalar         :: mone
  PetscInt            :: H_pos(2),SL_K_dim(2),SL_H_dim
  PetscInt            :: itwo
  PetscErrorCode      :: ierr
  !
  external RT_mat_mult,CHT_mat_mult,& 
           RT_mat_mult_transpose,CHT_mat_mult_transpose,&
           RT_mat_mult_hermitian_transpose,CHT_mat_mult_hermitian_transpose,&
           RT_get_diagonal,CHT_get_diagonal,&
           ignore_petsc_error_handler
  !
  SL_H_dim=BS_H_dim
  SL_K_dim=BS_K_dim
  !
  ! Allocate the explicit submatrices of the nest matrix
  !
  call MatCreate(PETSC_COMM_WORLD,R,ierr)
  call MatSetSizes(R,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
  call MatSetType(R,MATMPIDENSE,ierr)
  if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
  call MatSetType(R,MATDENSECUDA,ierr)
#endif
  endif
  call MatSetFromOptions(R,ierr)
  call MatSetUp(R,ierr)
  !
  call MatCreate(PETSC_COMM_WORLD,C,ierr)
  call MatSetSizes(C,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
  call MatSetType(C,MATMPIDENSE,ierr)
  if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
  call MatSetType(C,MATDENSECUDA,ierr)
#endif
  endif
  call MatSetFromOptions(C,ierr)
  call MatSetUp(C,ierr)
  !
  ! Fill the values of the explicit submatrices of the nest matrix
  !
  do i_B=1,n_BS_blks
    i_Tk  =BS_blk(i_B)%iT_k
    i_Tp  =BS_blk(i_B)%iT_p
    !
    H_shift=0
    if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
    !
    do i_r=1,BS_blk(i_B)%size(1)
      !
      H_pos(1)=BS_blk(i_B)%coordinate(1)+i_r-2
      !
      do i_c=1,BS_blk(i_B)%size(2)
        !
        H_pos(2)=BS_blk(i_B)%coordinate(2)+i_c-2
        !
        ! Then the upper triangle of each block and direct symmetrization
        !
        if (H_pos(1)+H_shift(1)>H_pos(2)+H_shift(2)) cycle
        if (H_pos(1)>H_pos(2)) cycle
        !
        Mij     =      BS_blk(i_B)%mat(i_r,i_c)
        Mij_star= real(BS_blk(i_B)%mat(i_r,i_c))-cI*aimag(BS_blk(i_B)%mat(i_r,i_c))
        !
        ! Add energies to the diagonal
        !
        if(H_pos(1)+H_shift(1)==H_pos(2)+H_shift(2)) then
          Mij     =real(Mij)     +BSS_eh_E(H_pos(1)+H_shift(1)+1)*cONE
          Mij_star=real(Mij_star)+BSS_eh_E(H_pos(1)+H_shift(1)+1)*cONE
          if (allocated(BSS_eh_W).and..not.BSS_perturbative_width) then
            Mij     =Mij            +cI*BSS_eh_W(H_pos(1)+H_shift(1)+1)
            Mij_star=Mij_star       -cI*BSS_eh_W(H_pos(1)+H_shift(1)+1)
          endif
        endif
        !
        select case(BS_blk(i_B)%mode)
        case("R")
           call MatSetValue( R, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! The resonant block is hermitian
           call MatSetValue( R, H_pos(2), H_pos(1),   Mij_star, INSERT_VALUES, ierr )
        case("C")
           call MatSetValue( C, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian
           ! The coupling block and the anti-coupling block are symmetric
           call MatSetValue( C, H_pos(2), H_pos(1),       Mij , INSERT_VALUES, ierr )
        end select
        !
      enddo
      !
    enddo
    !
  enddo
  !
 call MatAssemblyBegin(R,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(R,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY,ierr)
 !
 ! Create the two shell submatrices and define the required operations
 !
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,RT,ierr)
 if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
 call MatSetVecType(RT,VECCUDA,ierr)
#endif
 endif
 call MatShellSetOperation(RT,MATOP_GET_DIAGONAL,RT_get_diagonal,ierr)
 call MatShellSetOperation(RT,MATOP_MULT,RT_mat_mult,ierr)
 call MatShellSetOperation(RT,MATOP_MULT_TRANSPOSE,RT_mat_mult_transpose,ierr)
 call PetscPushErrorHandler(ignore_petsc_error_handler,PETSC_NULL_INTEGER,ierr)
 call MatShellSetOperation(RT,MATOP_MULT_HERMITIAN_TRANSPOSE,RT_mat_mult_hermitian_transpose,ierr)
 call PetscPopErrorHandler(ierr)
 !
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,CHT,ierr)
 if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
 call MatSetVecType(CHT,VECCUDA,ierr)
#endif
 endif
 call MatShellSetOperation(CHT,MATOP_GET_DIAGONAL,CHT_get_diagonal,ierr)
 call MatShellSetOperation(CHT,MATOP_MULT,CHT_mat_mult,ierr)
 call MatShellSetOperation(CHT,MATOP_MULT_TRANSPOSE,CHT_mat_mult_transpose,ierr)
 call PetscPushErrorHandler(ignore_petsc_error_handler,PETSC_NULL_INTEGER,ierr)
 call MatShellSetOperation(CHT,MATOP_MULT_HERMITIAN_TRANSPOSE,CHT_mat_mult_hermitian_transpose,ierr)
 call PetscPopErrorHandler(ierr)
 !
 ! Build the nest matrix
 !
 matArray(1) = R
 matArray(2) = C
 matArray(3) = CHT
 matArray(4) = RT
 itwo = 2
 call MatCreateNest(PETSC_COMM_WORLD,itwo,PETSC_NULL_INTEGER,itwo,PETSC_NULL_INTEGER,matArray,slepc_mat,ierr)
 if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
 call MatSetVecType(slepc_mat,VECCUDA,ierr)
#endif
 endif
 !
end subroutine K_stored_in_a_nest_matrix

subroutine RT_mat_mult(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMultTranspose(R,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine RT_mat_mult

subroutine CHT_mat_mult(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMultHermitianTranspose(C,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine CHT_mat_mult

subroutine RT_mat_mult_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMult(R,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine RT_mat_mult_transpose

subroutine CHT_mat_mult_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,X_conjugate,F
  PetscScalar mone
  PetscErrorCode ierr
  call VecDuplicate(X,X_conjugate,ierr)
  call VecCopy(X,X_conjugate,ierr)
  call VecConjugate(X_conjugate,ierr)
  call MatMult(C,X_conjugate,F,ierr)
  mone = -1.0
  call VecConjugate(F,ierr)
  call VecScale(F,mone,ierr)
  call VecDestroy(X_conjugate,ierr)
  return
end subroutine CHT_mat_mult_transpose

subroutine RT_mat_mult_hermitian_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,X_conjugate,F
  PetscScalar mone
  PetscErrorCode ierr
  call VecDuplicate(X,X_conjugate,ierr)
  call VecCopy(X,X_conjugate,ierr)
  call VecConjugate(X_conjugate,ierr)
  call MatMult(R,X_conjugate,F,ierr)
  mone = -1.0
  call VecConjugate(F,ierr)
  call VecScale(F,mone,ierr)
  call VecDestroy(X_conjugate,ierr)
  return
end subroutine RT_mat_mult_hermitian_transpose

subroutine CHT_mat_mult_hermitian_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMult(C,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine CHT_mat_mult_hermitian_transpose

subroutine RT_get_diagonal(M,D)
  use shellmat_module
  implicit none
  Mat     M
  Vec     D
  PetscScalar mone
  PetscErrorCode ierr
  call MatGetDiagonal(R,D,ierr)
  mone = -1.0
  call VecScale(D,mone,ierr)
  return
end subroutine RT_get_diagonal

subroutine CHT_get_diagonal(M,D)
  use shellmat_module
  implicit none
  Mat     M
  Vec     D
  PetscScalar mone
  PetscErrorCode ierr
  call MatGetDiagonal(C,D,ierr)
  mone = -1.0
  call VecScale(D,mone,ierr)
  return
end subroutine CHT_get_diagonal
