!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): BM DS
!
! headers
!
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
!
subroutine K_stored_in_a_nest_matrix(i_BS_mat,iq,slepc_mat)
 !
 ! BS_K_coupling and l_BS_ares_from_res are assumed TRUE
 !
 ! K is stored in a PETSc nest matrix, taking advantage of the four block
 ! structure of the matrix in this case:
 !
 ! | R     C  |
 ! |-C^*  -R^T|
 !
 !
 use pars,           ONLY:cI,cONE,lchlen
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_blk,n_BS_blks,BS_ID
 use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_W,BSS_perturbative_width,&
 &                        run_inversion,run_Haydock,               &
 &                        BSS_kernel_last_stored_block,BSS_slepc_pseudo_herm
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:RD
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 use gpu_m,          ONLY:have_cuda
 !
 use petscmat
 use slepceps
 !
 implicit none
 !
 integer, intent(in)  :: i_BS_mat,iq
 Mat,     intent(out) :: slepc_mat
 !
 integer     :: i_c,i_r,i_Tk,i_Tp,i_B,H_shift(2)
 character(lchlen)   :: live_msg
 Mat                 :: R,C
 PetscScalar         :: Mij,Mij_star
 PetscInt            :: H_pos(2),SL_K_dim(2),SL_H_dim
 PetscErrorCode      :: ierr
 !
 call timing('BSE mat slepc redist',OPR='start')
 !
 SL_H_dim=BS_H_dim
 SL_K_dim=BS_K_dim
 !
 ! Allocate the explicit submatrices of the nest matrix
 !
 call MatCreate(PETSC_COMM_WORLD,R,ierr)
 call MatSetSizes(R,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
 call MatSetType(R,MATDENSE,ierr)
#if defined(PETSC_HAVE_CUDA)
 if (have_cuda) call MatSetType(R,MATDENSECUDA,ierr)
#endif
 call MatSetFromOptions(R,ierr)
 call MatSetUp(R,ierr)
 !
 call MatCreate(PETSC_COMM_WORLD,C,ierr)
 call MatSetSizes(C,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim/2,SL_H_dim/2,ierr)
 call MatSetType(C,MATDENSE,ierr)
#ifdef PETSC_HAVE_CUDA
 if (have_cuda) call MatSetType(C,MATDENSECUDA,ierr)
#endif
 call MatSetFromOptions(C,ierr)
 call MatSetUp(C,ierr)
 !
 ! Fill the values of the explicit submatrices of the nest matrix
 !
 live_msg="Redistribute BSE in slepc_mat"
 if (BSS_kernel_last_stored_block<n_BS_blks) &
 & live_msg="Load and distribute BSE in slepc mat"
 if (n_BS_blks>0) call live_timing(trim(live_msg),n_BS_blks)
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   H_shift=0
   if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
   !
   if (i_B>BSS_kernel_last_stored_block) then
     call io_control(ACTION=RD,ID=BS_ID(2))
     call io_BS_PAR_block(iq,i_B,BS_ID(2),"matonly")
   endif
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
   ! After the re-distribution into slepc_mat the %mat are (in some cases) not needed anymore 
   !
   if ( i_B>BSS_kernel_last_stored_block)  then
     call K_block_free(i_B,"matonly")
   else if ( (.not.(run_Haydock.or.run_inversion)) ) then
     call K_block_free(i_B,"all")
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 if (n_BS_blks>0) call live_timing()
 !
 call MatAssemblyBegin(R,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(R,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY,ierr)
 !
 call MatCreateBSE(R,C,slepc_mat,ierr);
 !
 call timing('BSE mat slepc redist',OPR='stop')
 !
end subroutine K_stored_in_a_nest_matrix
