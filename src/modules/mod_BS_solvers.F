!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
module BS_solvers
 !
 use descriptors,    ONLY:IO_desc
 use pars,           ONLY:SP,schlen,lchlen
 use BS,             ONLY:BS_T_grp,BS_nT_grps,BS_K_dim,BS_H_dim,BS_block,n_BS_blks,BS_blk
 use matrix,         ONLY:PAR_matrix
 !
#include <memory.h>
 !
 ! Epsilon factor
 !==================
 real(SP)    :: Co_factor
 complex(SP) :: diam_term_exact
 complex(SP) :: A_drude(2)
 complex(SP) :: B_Hall(2)
 !
 ! Haydock
 !========== 
 type Haydock_vector
   complex(SP), allocatable :: fragment(:,:) ! (/ i_Tr; maxval(Xk%FGbz%N_mapped('kpts_in_T_gr')) /)
 end type Haydock_vector
 !
#if defined _SLEPC && !defined _NL
 !
 ! Slepc
 !==========
 type Slepc_vector
   integer :: it(2)
   type(Haydock_vector), allocatable :: Vi(:)
   type(Haydock_vector), allocatable :: Vo(:)
 end type Slepc_vector
 !
 type(Slepc_vector) :: Slepc_v
 !
#endif
 !
 integer, allocatable :: Haydock_parallel_group_comm(:)
 !
 ! SCALAPACK shadow matrix
 !=========================
 type(PAR_matrix), SAVE:: K_slk
 !
 ! Solvers (General)
 !=========
 type(IO_desc)    :: BSS_desc
 character(schlen):: BSS_mode
 complex(SP)      :: BSS_Wd
 integer  :: BSS_n_freqs
 integer  :: BSS_n_eig            !number of eigenvalues to find
 real(SP) :: BSS_er(2)
 real(SP) :: BSS_dr(2)
 real(SP) :: BSS_damp_reference
 real(SP) :: BSS_q0(3)
 real(SP) :: BSS_E_dir(3)
 real(SP) :: BSS_Q_dir(3)
 logical  :: BSS_uses_DbGd
 logical  :: BSS_Vnl_included
 logical  :: BSS_uses_GreenF
 logical  :: BSS_perturbative_width
 logical  :: BSS_eels_to_eps
 logical  :: BSS_uses_pert_SOC=.FALSE.
 logical  :: run_Haydock=.FALSE.
 logical  :: run_inversion=.FALSE.
 logical  :: run_Diago=.FALSE.
#if defined _SLEPC && !defined _NL
 logical  :: run_Slepc=.FALSE.
#endif
 !
 ! Solvers (Diago)
 !=========
 logical  :: BSS_write_eig_2_db
 !
 ! Solvers (Haydock)
 !=========
 integer  :: HAYVEC_size
 logical  :: Haydock_terminator
 integer  :: Haydock_iterIO
 integer  :: Haydock_iterMAX
 real(SP) :: Haydock_threshold
 real(SP) :: Haydock_v0_mod
 real(SP) :: FG_factor
 logical  :: BSS_uses_FKE
 !
 ! Solvers (Slepc)
 !=========
 character(schlen):: BSS_slepc_approach     !choose slepc approach ("Krylov-Schur","Generalized-Davidson","Jacob-Davidson")
 character(schlen):: BSS_slepc_precondition !choose slepc eigenvalue precondition method (none,bcgs+jacobi)
 character(schlen):: BSS_slepc_extraction   !choose slepc eigenvalue extraction method (ritz,harmonic)
 integer          :: BSS_slepc_ncv        !dimension of the subspace
 integer          :: BSS_slepc_maxit      !maximum number of iterations
 real(SP)         :: BSS_slepc_tol        !tolerance for the iterative solver
 real(SP)         :: BSS_slepc_target_E   !find eigenvalues close to this energy
 logical          :: BSS_slepc_matrix     !use a shell matrix for slepc
 logical          :: BSS_slepc_double_grp !duplicate the number of groups for slepc shells solver
 !
 ! Solvers (Inversion)
 !=========
 character(schlen)     :: BSS_inversion_mode
 logical               :: BSS_uses_partial_diago
 logical               :: K_diago_in_BS_mat
 type(PAR_matrix),SAVE :: K_inv_slk
 !
 type K_Inversion_data
   character(3)             :: what
   integer                  :: n_freqs = 0
   integer                  :: n_full_inv_freqs = 0
   real(SP)                 :: treshold 
   real(SP)                 :: max_treshold 
   real(SP)                 :: PERT_FULL_ratio = 1. 
   integer    , allocatable :: err(:)
   integer    , allocatable :: iw_full(:)
   complex(SP), allocatable :: spectra(:,:)
 end type K_Inversion_data
 !
 type(K_Inversion_data), save :: K_INV_PI_PH
 type(K_Inversion_data), save :: K_INV_EPS
 type(K_Inversion_data), save :: K_INV_PL
 !
 ! Solvers (Temporary (LARGE) arrays to be used in solvers ONLY)
 !=========
 integer    , allocatable :: BSS_eh_table(:,:)
 real(SP)   , allocatable :: BSS_eh_E(:)
 real(SP)   , allocatable :: BSS_eh_f(:)
 real(SP)   , allocatable :: BSS_eh_W(:)
 complex(SP), allocatable :: BSS_eh_Z(:)
 real(SP)   , allocatable :: BSS_eh_E_SOC_corr(:,:)
 complex(SP), allocatable :: BSS_dipoles_opt(:,:)
 !
 complex(SP), allocatable :: BS_E(:)
 complex(SP), allocatable :: BS_diagonal(:)
 complex(SP), allocatable :: BS_mat(:,:)
 !
 interface 
   !
   integer function variables_BS(ID,iq,local_desc,CLOSE_the_menu,X)
     use pars,       ONLY:lchlen,SP
     use X_m,        ONLY:X_t
     use descriptors,ONLY:IO_desc
     implicit none
     integer            ::ID,iq
     type(IO_desc)      ::local_desc
     logical            ::CLOSE_the_menu
     type(X_t), optional::X
   end function
   !
   integer function io_BSS_diago(iq,i_BS_mat,ID,X_static,bsE,bsRl,BsRr,BsE_corr,&
   &                                    bsL_magn,bsR_magn,bsL_kerr,bsR_kerr,bsR_dich,bsR_pl)
     use pars,          ONLY:SP
     use X_m,           ONLY:X_t
     implicit none
     type(X_t),intent(in)         :: X_static
     integer                      :: iq,ID,i_BS_mat
     complex(SP)        ,optional :: bsE(:)
     real(SP),   pointer,optional :: BsE_corr(:,:),bsR_pl(:,:)
     complex(SP),pointer,optional :: bsL_kerr(:),bsR_kerr(:),bsR_dich(:,:),bsL_magn(:,:),bsR_magn(:,:),bsRl(:),bsRr(:)
   end function
   !
   integer function io_BSS_Haydock(ID,iq,it,reached_treshold,mode,Af,Bf,Cf,Vnm1,Vn,Vnp1)
     use pars,          ONLY:SP
     implicit none
     integer             :: ID,iq,it
     character(*)        :: mode
     real(SP)            :: reached_treshold,Bf(it+1),Af(it)
     complex(SP)         :: Cf(:)
     complex(SP),pointer :: Vnm1(:),Vn(:),Vnp1(:)
   end function
   !
 end interface
 !
 interface BSS_resize
   module procedure resizeR1
   module procedure resizeR1p, resizeR2p
   module procedure resizeC1,  resizeC2
   module procedure resizeC1p, resizeC2p
   module procedure resizeC22p
 end interface

 !
 contains
   !
   ! Start BSS_resize interface
   !
   subroutine resizeR1(VAR_R1)
     !
     real(SP), allocatable, intent(inout) :: VAR_R1(:)
     real(SP)                             :: TMP_R1(BSS_n_eig)
     !
     TMP_R1=VAR_R1(:BSS_n_eig)
     deallocate(VAR_R1)
     allocate(VAR_R1(BSS_n_eig))
     VAR_R1=TMP_R1
     !
   end subroutine resizeR1
   !
   subroutine resizeR1p(VAR_R1)
     !
     real(SP), pointer, intent(inout) :: VAR_R1(:)
     real(SP)                         :: TMP_R1(BSS_n_eig)
     !
     TMP_R1=VAR_R1(:BSS_n_eig)
     deallocate(VAR_R1)
     allocate(VAR_R1(BSS_n_eig))
     VAR_R1=TMP_R1
     !
   end subroutine resizeR1p
   !
   subroutine resizeR2p(VAR_R2)
     !
     real(SP), pointer, intent(inout) :: VAR_R2(:,:)
     real(SP), allocatable            :: TMP_R2(:,:)
     integer :: dim1
     !
     dim1=size(VAR_R2(:,1))
     allocate(TMP_R2(dim1,BSS_n_eig))
     TMP_R2=VAR_R2(:,:BSS_n_eig)
     deallocate(VAR_r2); nullify(VAR_R2)
     allocate(VAR_R2(dim1,BSS_n_eig))
     VAR_R2=TMP_R2
     !
   end subroutine resizeR2p
   !
   subroutine resizeC1(VAR_C1)
     !
     complex(SP), allocatable, intent(inout) :: VAR_C1(:)
     complex(SP)                             :: TMP_C1(BSS_n_eig)
     !
     TMP_C1=VAR_C1(:BSS_n_eig)
     deallocate(VAR_C1)
     allocate(VAR_C1(BSS_n_eig))
     VAR_C1=TMP_C1
     !
   end subroutine resizeC1
   !
   subroutine resizeC1p(VAR_C1)
     !
     complex(SP), pointer, intent(inout) :: VAR_C1(:)
     complex(SP)                         :: TMP_C1(BSS_n_eig)
     !
     TMP_C1=VAR_C1(:BSS_n_eig)
     deallocate(VAR_C1); nullify(VAR_C1)
     allocate(VAR_C1(BSS_n_eig))
     VAR_C1=TMP_C1
     !
   end subroutine resizeC1p
   !
   subroutine resizeC2(VAR_C2)
     !
     complex(SP), allocatable, intent(inout) :: VAR_C2(:,:)
     complex(SP), allocatable                :: TMP_C2(:,:)
     integer :: dim1
     !
     dim1=size(VAR_C2(:,1))
     allocate(TMP_C2(dim1,BSS_n_eig))
     TMP_C2=VAR_C2(:,:BSS_n_eig)
     deallocate(VAR_C2)
     allocate(VAR_C2(dim1,BSS_n_eig))
     VAR_C2=TMP_C2
     !
   end subroutine resizeC2
   !
   subroutine resizeC2p(VAR_C2)
     !
     complex(SP), pointer, intent(inout) :: VAR_C2(:,:)
     complex(SP), allocatable            :: TMP_C2(:,:)
     integer :: dim1
     !
     dim1=size(VAR_C2(:,1))
     allocate(TMP_C2(dim1,BSS_n_eig))
     TMP_C2=VAR_C2(:,:BSS_n_eig)
     deallocate(VAR_C2); nullify(VAR_C2)
     allocate(VAR_C2(dim1,BSS_n_eig))
     VAR_C2=TMP_C2
     !
   end subroutine resizeC2p
   !
   subroutine resizeC22p(VAR_C22,both)
     !
     logical, intent(in)                 :: both
     complex(SP), pointer, intent(inout) :: VAR_C22(:,:)
     complex(SP)                         :: TMP_C22(BSS_n_eig,BSS_n_eig)
     !
     TMP_C22=VAR_C22(:BSS_n_eig,:BSS_n_eig)
     deallocate(VAR_C22); nullify(VAR_C22)
     allocate(VAR_C22(BSS_n_eig,BSS_n_eig))
     VAR_C22=TMP_C22
     !
   end subroutine resizeC22p
   !
   ! End BSS_resize interface
   !
   subroutine BSS_alloc( )
     use pars,      ONLY:IP,cZERO,rZERO
     use electrons, ONLY:n_sp_pol
     use BS,        ONLY:BS_dip_size,l_BS_abs,l_BS_kerr,l_BS_dichroism
     integer :: i_g,i_size
     logical :: W_is_allocated,Z_is_allocated,E_SOC_is_allocated
     YAMBO_ALLOC(BSS_eh_table,(BS_H_dim,5))
     YAMBO_ALLOC(BSS_eh_E,(BS_H_dim))
     YAMBO_ALLOC(BSS_eh_f,(BS_H_dim))
     BSS_eh_table = 0
     BSS_eh_E     = rZERO
     BSS_eh_f     = rZERO
     if (l_BS_abs.or.l_BS_kerr.or.l_BS_dichroism) then
       YAMBO_ALLOC(BSS_dipoles_opt,(BS_dip_size,BS_H_dim))
       BSS_dipoles_opt = cZERO
     endif
     W_is_allocated     = .FALSE.
     Z_is_allocated     = .FALSE.
     E_SOC_is_allocated = .FALSE.
     do i_g=1,BS_nT_grps
       if (.not.W_is_allocated    ) W_is_allocated     = allocated(BS_T_grp(i_g)%W)
       if (.not.Z_is_allocated    ) Z_is_allocated     = allocated(BS_T_grp(i_g)%Z)
       if (.not.E_SOC_is_allocated) E_SOC_is_allocated = allocated(BS_T_grp(i_g)%E_SOC_corr)
     enddo
     if (W_is_allocated) then
       YAMBO_ALLOC(BSS_eh_W,(BS_H_dim))
       BSS_eh_W = rZERO
     endif
     if (Z_is_allocated) then
       YAMBO_ALLOC(BSS_eh_Z,(BS_H_dim))
       BSS_eh_Z = cZERO
     endif
     if (E_SOC_is_allocated) then
       YAMBO_ALLOC(BSS_eh_E_SOC_corr,(2/n_sp_pol,BS_H_dim))
       BSS_eh_E_SOC_corr = rZERO
     endif
   end subroutine
   !
   subroutine BSS_free()
     YAMBO_FREE(BSS_eh_table)
     YAMBO_FREE(BSS_dipoles_opt)
     YAMBO_FREE(BSS_eh_E)
     YAMBO_FREE(BSS_eh_W)
     YAMBO_FREE(BSS_eh_f)
     YAMBO_FREE(BSS_eh_Z)
     YAMBO_FREE(BSS_eh_E_SOC_corr)
   end subroutine
   !
   subroutine BS_HAYVEC_alloc(V,dom_k)
     use parallel_m,    ONLY:PAR_IND_T_Haydock
     type(Haydock_vector) :: V(BS_nT_grps)
     ! ws:
     integer :: i_g
     integer :: dom_k(BS_nT_grps)
     !
     do i_g=1,BS_nT_grps
       if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
       YAMBO_ALLOC(V(i_g)%fragment,(BS_T_grp(i_g)%size,dom_k(i_g)))
     enddo
   end subroutine BS_HAYVEC_alloc
   !
   subroutine BS_HAYVEC_free(V)
     use stderr,     ONLY:intc 
     type(Haydock_vector) :: V(:)
     ! ws:
     integer :: i_g
     !
     do i_g=1,BS_nT_grps
       YAMBO_FREE(V(i_g)%fragment)
     end do
   end subroutine BS_HAYVEC_free
   !
end module BS_solvers
