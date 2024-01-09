!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): 
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

module shellmat_module
#include <petsc/finclude/petscmat.h>
  use petscmat
  Mat::A,B
end module shellmat_module

subroutine ignore_petsc_error_handler(comm,line,fun,file,n,p,mess,ctx,ierr)
!
! Handler to ignore error if MatMultHermitianTranspose cannot be set.
! This operation is only available for shell matrices in PETSc from v.3.21
! Setting MatMultTranspose is enough, but it is preferable to set both.
!
  use petscsysdef
  integer line,n,p
  PetscInt ctx 
  PetscErrorCode ierr
  MPI_Comm comm
  character*(*) fun,file,mess
end subroutine ignore_petsc_error_handler

subroutine K_stored_in_a_nest_matrix(i_BS_mat,slepc_mat)
!
! K is stored in a PETSc nest matrix, taking advantage of the four block structure
! BS_K_coupling and l_BS_ares_from_res are assumed TRUE
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
  use slepcepsdef
  use petscmatdef
  use shellmat_module
  !
  implicit none
  !
  integer, intent(in)  :: i_BS_mat
  Mat,     intent(out) :: slepc_mat
  Mat                  :: AT,BHT
  Mat                  :: matArray(4)
  !
  integer     :: i_c,i_r,i_Tk,i_Tp,i_B,H_shift(2)
  PetscScalar         :: Mij,Mij_star
  PetscScalar         :: mone
  PetscInt            :: H_pos(2),SL_K_dim(2),SL_H_dim
  PetscInt            :: itwo
  PetscErrorCode      :: ierr
  !
  external AT_mat_mult,BHT_mat_mult,& 
           AT_mat_mult_transpose,BHT_mat_mult_transpose,&
           AT_mat_mult_hermitian_transpose,BHT_mat_mult_hermitian_transpose,&
           ignore_petsc_error_handler
  !
  SL_H_dim=BS_H_dim
  SL_K_dim=BS_K_dim
  !
  ! Allocate the explicit submatrices of the nest matrix
  !
  call MatCreate(PETSC_COMM_WORLD,A,ierr)
  call MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
  call MatSetType(A,MATMPIDENSE,ierr)
  if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
  call MatSetType(A,MATDENSECUDA,ierr)
  call MatSetVecType(A,VECCUDA,ierr)
#endif
  endif
  call MatSetFromOptions(A,ierr)
  call MatSetUp(A,ierr)
  !
  call MatCreate(PETSC_COMM_WORLD,B,ierr)
  call MatSetSizes(B,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
  call MatSetType(B,MATMPIDENSE,ierr)
  if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
  call MatSetType(B,MATDENSECUDA,ierr)
  call MatSetVecType(B,VECCUDA,ierr)
#endif
  endif
  call MatSetFromOptions(B,ierr)
  call MatSetUp(B,ierr)
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
           call MatSetValue( A, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! The resonant block is hermitian
           call MatSetValue( A, H_pos(2), H_pos(1),   Mij_star, INSERT_VALUES, ierr )
        case("C")
           call MatSetValue( B, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian
           ! The coupling block and the anti-coupling block are symmetric
           call MatSetValue( B, H_pos(2), H_pos(1),       Mij , INSERT_VALUES, ierr )
        end select
        !
      enddo
      !
    enddo
    !
  enddo
  !
 call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY,ierr)
 !
 ! Create the two shell submatrices and define the required operations
 !
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,AT,ierr)
 if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
 call MatSetVecType(AT,VECCUDA,ierr)
#endif
 endif
 call MatShellSetOperation(AT,MATOP_MULT,AT_mat_mult,ierr)
 call MatShellSetOperation(AT,MATOP_MULT_TRANSPOSE,AT_mat_mult_transpose,ierr)
 call PetscPushErrorHandler(ignore_petsc_error_handler,PETSC_NULL_INTEGER,ierr)
 call MatShellSetOperation(AT,MATOP_MULT_HERMITIAN_TRANSPOSE,AT_mat_mult_hermitian_transpose,ierr)
 call PetscPopErrorHandler(ierr)
 !
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,BHT,ierr)
 if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
 call MatSetVecType(BHT,VECCUDA,ierr)
#endif
 endif
 call MatShellSetOperation(BHT,MATOP_MULT,BHT_mat_mult,ierr)
 call MatShellSetOperation(BHT,MATOP_MULT_TRANSPOSE,BHT_mat_mult_transpose,ierr)
 call PetscPushErrorHandler(ignore_petsc_error_handler,PETSC_NULL_INTEGER,ierr)
 call MatShellSetOperation(BHT,MATOP_MULT_HERMITIAN_TRANSPOSE,BHT_mat_mult_hermitian_transpose,ierr)
 call PetscPopErrorHandler(ierr)
 !
 ! Build the nest matrix
 !
 matArray(1) = A
 matArray(2) = B
 matArray(3) = BHT
 matArray(4) = AT
 itwo = 2
 call MatCreateNest(PETSC_COMM_WORLD,itwo,PETSC_NULL_INTEGER,itwo,PETSC_NULL_INTEGER,matArray,slepc_mat,ierr)
 if (have_cuda) then
#ifdef PETSC_HAVE_CUDA
 call MatSetVecType(slepc_mat,VECCUDA,ierr)
#endif
 endif
 !
end subroutine K_stored_in_a_nest_matrix

subroutine AT_mat_mult(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMultTranspose(A,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine AT_mat_mult

subroutine BHT_mat_mult(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMultHermitianTranspose(B,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine BHT_mat_mult

subroutine AT_mat_mult_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMult(A,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine AT_mat_mult_transpose

subroutine BHT_mat_mult_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,X_conjugate,F
  PetscScalar mone
  PetscErrorCode ierr
  call VecDuplicate(X,X_conjugate,ierr)
  call VecCopy(X,X_conjugate,ierr)
  call VecConjugate(X_conjugate,ierr)
  call MatMult(B,X_conjugate,F,ierr)
  mone = -1.0
  call VecConjugate(F,ierr)
  call VecScale(F,mone,ierr)
  call VecDestroy(X_conjugate,ierr)
  return
end subroutine BHT_mat_mult_transpose

subroutine AT_mat_mult_hermitian_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,X_conjugate,F
  PetscScalar mone
  PetscErrorCode ierr
  call VecDuplicate(X,X_conjugate,ierr)
  call VecCopy(X,X_conjugate,ierr)
  call VecConjugate(X_conjugate,ierr)
  call MatMult(A,X_conjugate,F,ierr)
  mone = -1.0
  call VecConjugate(F,ierr)
  call VecScale(F,mone,ierr)
  call VecDestroy(X_conjugate,ierr)
  return
end subroutine AT_mat_mult_hermitian_transpose

subroutine BHT_mat_mult_hermitian_transpose(M,X,F,ierr)
  use shellmat_module
  implicit none
  Mat     M
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call MatMult(B,X,F,ierr)
  mone = -1.0
  call VecScale(F,mone,ierr)
  return
end subroutine BHT_mat_mult_hermitian_transpose
