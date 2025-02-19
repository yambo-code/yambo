!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): HM DS
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
subroutine K_stored_in_a_slepc_matrix(i_BS_mat,iq,slepc_mat)
 !
 ! Here I fill the kernel in a slepc matrix
 !
 !      | (K_r)     (cI*K_c)    |  
 !  K = |                       |
 !      | (-cI*K_c^*)  (-K_r^*) |
 !
 use pars,           ONLY:cI,cONE,lchlen
 use BS,             ONLY:BS_K_dim,BS_H_dim,BS_blk,n_BS_blks,BS_K_coupling,&
 &                        BS_res_ares_n_mat,l_BS_ares_from_res,BS_ID
 use BS_solvers,     ONLY:BSS_eh_E,BSS_eh_W,BSS_perturbative_width,&
 &                        run_inversion,run_Haydock,BSS_kernel_last_stored_block
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
 PetscScalar         :: Mij,Mij_star
 PetscInt            :: H_pos(2),SL_K_dim(2),SL_H_dim
 PetscErrorCode      :: ierr
 !
 call timing('BSE mat slepc redist',OPR='start')
 !
 if(     BS_K_coupling) SL_H_dim=BS_H_dim
 if(.not.BS_K_coupling) SL_H_dim=BS_K_dim(i_BS_mat)
 SL_K_dim=BS_K_dim
 !
 ! Allocate the slepc Matrix
 !
 call MatCreate(PETSC_COMM_WORLD,slepc_mat,ierr)
 call MatSetSizes(slepc_mat,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim,SL_H_dim,ierr)
 call MatSetType(slepc_mat, MATDENSE,ierr)

 if (have_cuda) then
#if defined(PETSC_HAVE_CUDA)
   call MatSetType(slepc_mat,MATDENSECUDA,ierr)
#else
   call warning("Yambo has cuda support. Petsc doesn't. Look for a petsc with gpu support")
#endif
 endif

 call MatSetUp(slepc_mat,ierr)
 ! 
 ! filling of the slepc_mat
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
   if (i_BS_mat/=BS_blk(i_B)%ira_k .and. BS_res_ares_n_mat==2) cycle
   !
   H_shift=0
   if(BS_blk(i_B)%mode=="C") H_shift(2)=BS_K_dim(1)
   if(BS_blk(i_B)%mode=="A") H_shift(:)=BS_K_dim(1)
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
           call MatSetValue( slepc_mat, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! The resonant block is hermitial
           call MatSetValue( slepc_mat, H_pos(2), H_pos(1),   Mij_star, INSERT_VALUES, ierr )
         if (l_BS_ares_from_res.and.BS_K_coupling) then
           ! The anti-resonant block is A=-R*
           call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2)+SL_K_dim(1), -Mij_star, INSERT_VALUES, ierr )
           ! The anti-resonant block is hermitian
           call MatSetValue( slepc_mat, H_pos(2)+SL_K_dim(1), H_pos(1)+SL_K_dim(1),      -Mij, INSERT_VALUES, ierr )
         endif
       case("C")
           call MatSetValue( slepc_mat, H_pos(1), H_pos(2)+SL_K_dim(1),       Mij , INSERT_VALUES, ierr )
           ! Anti-coupling from coupling: the whole BSE matrix is Pseudo-HErmitian
           call MatSetValue( slepc_mat, H_pos(2)+SL_K_dim(1), H_pos(1), -Mij_star , INSERT_VALUES, ierr )
           if (l_BS_ares_from_res) then
             ! The coupling block and the anti-coupling block are symmetric
             call MatSetValue( slepc_mat, H_pos(2), H_pos(1)+SL_K_dim(1),       Mij , INSERT_VALUES, ierr )
             call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2), -Mij_star , INSERT_VALUES, ierr )
           endif
       case("A")
         ! The anti-resonant block is hermitial
         if(BS_res_ares_n_mat==1) then
           call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2)+SL_K_dim(1),       Mij , INSERT_VALUES, ierr )
           call MatSetValue( slepc_mat, H_pos(2)+SL_K_dim(1), H_pos(1)+SL_K_dim(1),  Mij_star , INSERT_VALUES, ierr )
         else
           call MatSetValue( slepc_mat, H_pos(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           call MatSetValue( slepc_mat, H_pos(2), H_pos(1),  Mij_star , INSERT_VALUES, ierr )
         endif
       case("Q")
           call MatSetValue( slepc_mat, H_pos(1)+SL_K_dim(1), H_pos(2),       Mij , INSERT_VALUES, ierr )
           ! Coupling from anti-coupling: the whole BSE matrix is Pseudo-HErmitian
           call MatSetValue( slepc_mat, H_pos(2), H_pos(1)+SL_K_dim(1), -Mij_star , INSERT_VALUES, ierr )
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
 call MatAssemblyBegin(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 !
 call timing('BSE mat slepc redist',OPR='stop')
 !
end subroutine K_stored_in_a_slepc_matrix
