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

subroutine K_stored_in_a_nest_matrix(i_BS_mat,slepc_mat)
! K is stored in a PETSc nest matrix, taking advantage of the four block structure
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
!
  use pars,           ONLY:cI,cONE
  use BS,             ONLY:BS_K_dim,BS_H_dim,BS_blk,n_BS_blks,BS_K_coupling,&
  &                        BS_res_ares_n_mat,l_BS_ares_from_res
  use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_W,BSS_perturbative_width
  use com,            ONLY:msg
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
  character (len = 70) :: str_num_block
  !
  external AT_mat_mult, BHT_mat_mult, AT_mat_mult_transpose, BHT_mat_mult_transpose
  !
  call msg( 'nsr', '[SLEPC] Matrix is nest')
  !
  if(     BS_K_coupling) SL_H_dim=BS_H_dim
  if(.not.BS_K_coupling) SL_H_dim=BS_K_dim(i_BS_mat)
  SL_K_dim=BS_K_dim
  !
  ! Allocate the slepc Matrix
  !
  call MatCreate(PETSC_COMM_WORLD,A,ierr)
  call MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
  call MatSetType(A, MATMPIDENSE,ierr)
  call MatSetFromOptions(A,ierr)
  call MatSetUp(A,ierr)
  !
  call MatCreate(PETSC_COMM_WORLD,B,ierr)
  call MatSetSizes(B,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
  call MatSetType(B, MATMPIDENSE,ierr)
  call MatSetFromOptions(B,ierr)
  call MatSetUp(B,ierr)
  !
  ! Filling of the slepc_mat
  !
  do i_B=1,n_BS_blks
    i_Tk  =BS_blk(i_B)%iT_k
    i_Tp  =BS_blk(i_B)%iT_p
    !
    if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_res_ares_n_mat==2) cycle
    !
    H_shift=0
    if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
    if(BS_blk(i_B)%mode=="A") H_shift(:)=BS_K_dim(1)
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
        if (l_BS_ares_from_res.and.H_pos(1)>H_pos(2)) cycle
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
         if (l_BS_ares_from_res.and.BS_K_coupling) then
           ! The anti-resonant block is A=-R*
           !call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2)+SL_K_dim(1), -Mij_star, INSERT_VALUES, ierr )
           ! The anti-resonant block is hermitian
           !call MatSetValue( slepc_mat, H_pos(2)+SL_K_dim(1), H_pos(1)+SL_K_dim(1),      -Mij, INSERT_VALUES, ierr )
         endif
        case("C")
           call MatSetValue( B, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian
           if (l_BS_ares_from_res) then
             ! The coupling block and the anti-coupling block are symmetric
             call MatSetValue( B, H_pos(2), H_pos(1),       Mij , INSERT_VALUES, ierr )
           endif
        case("A")
         ! The anti-resonant block is hermitial
         if(BS_res_ares_n_mat==1) then
           !call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2)+SL_K_dim(1),       Mij , INSERT_VALUES, ierr )
           !call MatSetValue( slepc_mat, H_pos(2)+SL_K_dim(1), H_pos(1)+SL_K_dim(1),  Mij_star , INSERT_VALUES, ierr )
         else
           !call MatSetValue( slepc_mat, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           !call MatSetValue( slepc_mat, H_pos(2), H_pos(1),  Mij_star , INSERT_VALUES, ierr )
         endif
        case("Q")
           !call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! Coupling from anti-coupling: the whole BSE matrix is Pseudo-HErmitian
           !call MatSetValue( slepc_mat, H_pos(2), H_pos(1)+SL_K_dim(1), -Mij_star , INSERT_VALUES, ierr )
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
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,AT,ierr)
 call MatShellSetOperation(AT,MATOP_MULT,AT_mat_mult,ierr)
 call MatShellSetOperation(AT,MATOP_MULT_TRANSPOSE,AT_mat_mult_transpose,ierr)
 !
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,0,BHT,ierr)
 call MatShellSetOperation(BHT,MATOP_MULT,BHT_mat_mult,ierr)
 call MatShellSetOperation(BHT,MATOP_MULT_TRANSPOSE,BHT_mat_mult_transpose,ierr)
 !
 matArray(1) = A
 matArray(2) = B
 matArray(3) = BHT
 matArray(4) = AT
 itwo = 2
 call MatCreateNest(PETSC_COMM_WORLD,itwo,PETSC_NULL_INTEGER,itwo,PETSC_NULL_INTEGER,matArray,slepc_mat,ierr)
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
  Vec     X,F
  PetscScalar mone
  PetscErrorCode ierr
  call VecConjugate(X,ierr)
  call MatMult(B,X,F,ierr)
  mone = -1.0
  call VecConjugate(F,ierr)
  call VecScale(F,mone,ierr)
  call VecConjugate(X,ierr)
  return
end subroutine BHT_mat_mult_transpose

