!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM, DS
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
subroutine K_multiply_by_V_slepc(slepc_mat,vi,vo,ierr)
 !
 ! Interface with the K_multiply_by_V from the Haydock method
 ! This function has to take the same arguments as MatMult from PETSC
 !
 use pars,          ONLY : SP
 use BS_solvers,    ONLY : Slepc_v,BSS_Slepc_double_grp
 use BS,            ONLY : BS_T_grp,BS_nT_grps,BS_K_dim
 use parallel_m,    ONLY : PAR_IND_T_Haydock
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 use petscmat
 use slepcsys
 use petscmatdef
 use slepcsysdef
 !
 implicit none
 !
 PetscScalar          :: tmp_value(1)
 PetscInt             :: H_pos(1), pet_one
 PetscErrorCode       :: ierr
 !
 VecScatter           :: ctx
 PetscScalar, pointer :: xx(:)
 Vec                  :: vi,vo,x
 Mat                  :: slepc_mat
 !
 integer              :: fac
 integer              :: n, i_g, i_g_start, i_c, start_index, start_index_dg
 !
 ! create scattering context vi (distributed) -> x (local)
 !
 pet_one = 1
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
   i_g_start=BS_T_grp(i_g)%i_T_ref
   start_index = sum(BS_T_grp(i_g_start:i_g-1)%size)-1+(BS_T_grp(i_g)%i_res_ares-1)*BS_K_dim(1)
   do i_c=1,BS_T_grp(i_g)%size
     H_pos=start_index+i_c
     !SLEPC funcitons expect C indexes both in Fortran and C
     call VecGetValues( x, pet_one, H_pos, tmp_value, ierr )
     Slepc_v%Vi(i_g)%fragment(i_c)=cmplx(tmp_value(1),kind=SP)
   enddo
 enddo
 !
 ! destroy scatter context and local vector when no longer needed
 !
 call VecScatterDestroy(ctx,ierr);
 call VecDestroy(x,ierr);
 !
 ! fac needed for K_multiply_by_v
 fac=1
 if(BSS_Slepc_double_grp) fac=(-1)**(Slepc_v%it(1)+1)
 ! use the haydock routine to calculate the BSE auxiliaxy hamiltonian matrix-vector product
 ! Slepc_v%Vo = H Slepc_v%Vi
 call K_multiply_by_V( Slepc_v%Vi, Slepc_v%Vo,fac)
 !
 ! copy output vector Vo -> vo (distributed)
 !
 ! fac needed for expanding vo to anti-resonant block
 if(BSS_Slepc_double_grp) then
   Slepc_v%it(1)=Slepc_v%it(1)+1
   fac=(-1)**(Slepc_v%it(1)+1)
 endif
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
   i_g_start=BS_T_grp(i_g)%i_T_ref
   start_index = sum(BS_T_grp(i_g_start:i_g-1)%size)-1+(BS_T_grp(i_g)%i_res_ares-1)*BS_K_dim(1)
   if(BSS_slepc_double_grp) start_index_dg = sum(BS_T_grp(i_g_start:i_g-1)%size)-1+BS_K_dim(1)
   do i_c=1,BS_T_grp(i_g)%size
     H_pos=start_index+i_c
     tmp_value=cmplx(Slepc_v%Vo(i_g)%fragment(i_c))
     !SLEPC funcitons expect C indexes both in Fortran and C
     call VecSetValues( vo, pet_one, H_pos, tmp_value, INSERT_VALUES, ierr )
     if(BSS_slepc_double_grp) then
       ! Expand vo to anti-resonant block
       H_pos=start_index_dg+i_c
       call VecSetValues( vo, pet_one, H_pos, fac*conjg(tmp_value), INSERT_VALUES, ierr )
     endif
   enddo
 enddo
 !
 call VecAssemblyBegin(vo,ierr)
 call VecAssemblyEnd(vo,ierr)
 !
end subroutine K_multiply_by_V_slepc
