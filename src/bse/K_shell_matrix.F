!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM
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
subroutine K_shell_matrix(slepc_mat)
 !
 ! Create a PETSC ShellMatrix and define the matrix-vector product
 ! function using the routine K_multiply_by_V 
 !
 !      | (K_r)     (cI*K_c)    |  
 !  K = |                       |
 !      | (-cI*K_c^*)  (-K_r^*) |
 !
 use BS,             ONLY:BS_H_dim, BS_nT_grps, BS_blk, n_BS_blks
 use BS_solvers,     ONLY:Slepc_v,BS_HAYVEC_alloc
 use petscmatdef
 use slepcepsdef
 !
#include <memory.h>
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 integer          :: i_B, i_r, i_c, i_Tk, i_Tp
 PetscFortranInt  :: SL_H_dim,SL_K_dim
 Mat              :: slepc_mat
 PetscErrorCode   :: ierr
 external K_multiply_by_V_slepc
 !
 SL_H_dim=BS_H_dim
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
 call MatCreateShell(PETSC_COMM_WORLD,PETSC_DECIDE,PETSC_DECIDE,SL_H_dim,SL_H_dim,0,slepc_mat,ierr)
 call MatAssemblyBegin(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatAssemblyEnd(slepc_mat,MAT_FINAL_ASSEMBLY,ierr)
 call MatShellSetOperation(slepc_mat,MATOP_MULT,K_multiply_by_V_slepc,ierr)
 call MatSetOption(slepc_mat,MAT_HERMITIAN,PETSC_TRUE,ierr)
 call MatSetUp(slepc_mat,ierr)
 !
 ! Allocate BS stuff
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
