!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, MG, DS
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
subroutine K_diago_driver(iq,W,X_static) 
 !
 ! Generic diagonalization method to solve resonant and non resonant Hamiltonians.
 !
 ! 10/06/2015 Added by HM: Generic slepc method to solve 
 !
 use pars,           ONLY:SP
 use BS,             ONLY:BS_K_coupling,BS_H_dim,BS_Blocks_symmetrize_K
 use BS_solvers,     ONLY:io_BSS_diago,BSS_perturbative_width,BSS_mode,&
&                         BSS_write_eig_2_db,BSS_eh_W,BS_mat,BSS_n_eig
 use X_m,            ONLY:X_t
 use electrons,      ONLY:E_SOC_nbands,n_sp_pol
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:K_diago_compute_epsilon,LINEAR_ALGEBRA_driver
 use IO_m,           ONLY:io_control,REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
#if defined _SLEPC
 use com,            ONLY:warning
 use interfaces,     ONLY:MATRIX_slepc
 use BS_solvers,     ONLY:BSS_slepc_extraction,Slepc_shell,BSS_slepc_ncv,&
&                         BSS_slepc_tol,BSS_target_E
#endif
#if defined _KERR
 use drivers,        ONLY:l_kerr
 use KERR,           ONLY:BSS_KERR_free
#endif
 !
#if defined _PL
 use drivers,        ONLY:l_photolum
 use PHOTOLUM,       ONLY:BSS_PL_free
#endif
 !
#include<memory.h>
 ! 
#if defined _SLEPC
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
#endif
 !
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 logical       :: K_is_not_hermitian,l_diago,l_slepc
 character(5)  :: Solver
 !
 ! Slepc specific
 !
#if defined _SLEPC
 Mat                   :: slepc_mat
 PetscErrorCode        :: ierr
 PetscFortranInt       :: slepc_ncv
 PetscReal             :: slepc_tol
 PetscReal             :: slepc_target_E
#endif
 !
 ! Residuals & Energies
 !
 complex(SP),allocatable :: BS_E(:)
 complex(SP),allocatable :: BS_R_left(:)       
 complex(SP),allocatable :: BS_R_right(:)      
 real(SP),   pointer     :: BS_R_PL(:,:) => null()
 real(SP)   ,pointer     :: BS_E_SOC_corr(:,:) => null()
 complex(SP),pointer     :: BS_R_right_kerr(:) => null()
 complex(SP),pointer     :: BS_overlap(:,:) => null()
 !
 ! Resonant K
 !
 real(SP),allocatable    :: BS_E_real(:)
 !
 ! Perturbative BSE
 complex(SP),allocatable :: BS_corrections(:,:)
 !
 ! Coupling
 !
 complex(SP),allocatable :: BS_V_left(:,:)     
 complex(SP),allocatable :: BS_V_right(:,:)    
 !
 ! I/O
 !
 integer                 :: io_err,ID
 type(X_t),intent(in)    :: X_static
 !
 l_diago     = index(BSS_mode,'d')/=0
#if defined _SLEPC
 l_slepc     = index(BSS_mode,'s')/=0
#endif
 !
 if(l_diago) BSS_n_eig = BS_H_dim
 !
 if(l_diago) Solver="Diago"
#if defined _SLEPC
 if(l_slepc) Solver="Slepc"
#endif
 !
 call timing(trim(Solver)//' Solver',OPR='start')
 !  
 ! Sectioning
 !============
 if (isec(2)/=0) call section('=',trim(Solver)//' solver')
 if (isec(2)==0) call section('+',trim(Solver)//' solver')
 !
#if defined _SLEPC
 if(l_slepc) call K_slepc_local_init()
#endif
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 call parser('BSSPertWidth',BSS_perturbative_width)
 !
 ! Allocation (BS_R and BS_E)
 !===========================
 call local_alloc("E")
 !
 ! Diagonalization DB (IN)
 !========================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=io_BSS_diago(iq,ID,X_static,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 !
 ! Kernel and/or BSS components loading
 !======================================
 if (io_err<0) then
   !
   call K_components_folded_in_serial_arrays(iq)
   !
   if(l_diago) call K_stored_in_a_big_matrix(iq,"DIAGO")
   !
#if defined _SLEPC
   if(l_slepc) then
     ! 1. Here we will define a PetscShell matrix and define the matrix-vector multiplication
     ! using the K_multiply_by_V function used in the Haydock method
     if (     Slepc_shell) call K_shell_matrix(iq,slepc_mat)
     ! 2. Here we create a distributed PETSC matrix from the BS_blks
     if (.not.Slepc_shell) call K_stored_in_a_slepc_matrix(iq,slepc_mat)
   endif
#endif
   !
 endif
 !
 K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
 !
 ! Allocation (BS_E_real or BS_V)
 !===============================
 call local_alloc("V")
 !
 ! Initialize the output file 
 !============================
 call K_output_file(iq,"open "//trim(Solver))
 !
 if (io_err<0) then
   !
#if defined _SCALAPACK
   !
   ! Symmetrize the K blocks (to be eventually sent to the BLACS)
   !
   ! This line will be needed when the BS kernel will not be folded in BS_mat. But in order to do so
   ! also linear_algebra_is_parallel must be swiched off when K is not hermitian.
   !=========================
   !if (linear_algebra_is_parallel) call BS_Blocks_symmetrize_K()
   !
#endif
   !
   ! Diagonalization of the excitonic hamiltonian
   !==============================================
   if(l_diago) call live_timing('BSK diagonalize',1)
   !
   if (K_is_not_hermitian) then
     !
     if(l_diago) call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_cmpl=BS_E,V_left=BS_V_left,V_right=BS_V_right)
#if defined _SLEPC
     if(l_slepc) call MATRIX_slepc(slepc_mat,BSS_n_eig,slepc_target_E,BSS_slepc_extraction,slepc_ncv,slepc_tol,&
&                                  BS_V_right,V_left=BS_V_left,E_cmpl=BS_E)
#endif
     !
     if (BSS_write_eig_2_db) BS_mat=BS_V_right
     !
   else
     !
     if(l_diago) call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_real=BS_E_real)
#if defined _SLEPC
     if(l_slepc) call MATRIX_slepc(slepc_mat,BSS_n_eig,slepc_target_E,BSS_slepc_extraction,slepc_ncv,slepc_tol,&
&                                  BS_mat,E_real=BS_E_real)
#endif
     !
     BS_E=cmplx(BS_E_real,0._SP,SP)
     !
   endif
   !
#if defined _SLEPC
   !
   ! Destroy the matrix
   !============================
   if(l_slepc) call MatDestroy(slepc_mat,ierr)
#endif
   !
   if(l_diago) call live_timing( )
   !
   ! Construct the residuals of epsilon
   !====================================
   if (K_is_not_hermitian) then
     !
     call K_diago_non_hermitian_residuals(BS_E,BS_R_left,BS_R_right,BS_V_left,BS_V_right,BS_overlap)
     !
#if defined _KERR
     if(l_kerr         ) call K_diago_kerr_residual(BS_E,BS_V_right,BS_R_right_kerr)
#endif
     !
#if defined _PL
     if(l_photolum     ) call PL_diago_residual(BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian,BS_overlap)
#endif
     !
   else
     !
     call K_diago_hermitian_residuals(BS_E_real,BS_R_left,BS_R_right)
     !
#if defined _PL
     if(l_photolum     ) call PL_diago_residual(BS_mat,BS_mat,BS_R_PL,K_is_not_hermitian)
#endif
     !
#if defined _KERR
     if(l_kerr         ) call K_diago_kerr_residual(BS_E,BS_mat,BS_R_right_kerr)
#endif
     !
   endif
   !
 endif
 !
 ! Define perturbative corrections
 !=================================
 if(E_SOC_nbands/=0) then
   !
   ! To check if the expression for the non hermitian case is correct
   if (     K_is_not_hermitian) call K_diago_perturbative(transpose(BS_V_left)*BS_V_right,BS_corrections,2/n_sp_pol,'SOC')
   if (.not.K_is_not_hermitian) call K_diago_perturbative(    conjg(BS_mat)   *BS_mat    ,BS_corrections,2/n_sp_pol,'SOC')
   BS_E_SOC_corr=real(BS_corrections)
   !
 endif
 !
 if(BSS_perturbative_width) then
   !
   if(     K_is_not_hermitian) call K_diago_perturbative(transpose(BS_V_left)*BS_V_right,BS_corrections(1,:),1,'WID')
   if(.not.K_is_not_hermitian) call K_diago_perturbative(    conjg(BS_mat)   *BS_mat    ,BS_corrections(1,:),1,'WID')
   BS_E=BS_E-aimag(BS_corrections(1,:))
   !
 endif
 !
 ! Now I calculate epsilon
 !=========================
 call K_diago_compute_epsilon(iq,W,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
 !
 ! I write the output file 
 !
 call K_output_file(iq,"Diago")
 !
 ! Diagonalization DB (OUT)
 !==========================
 if (io_err/=0) then
   !
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=io_BSS_diago(iq,ID,X_static,BS_E,BS_R_left,BS_R_right,BS_E_SOC_corr,BS_R_right_kerr,BS_R_PL)
   !
 endif
 !
 ! CLEAN
 !=======
 call local_free( )
 !
#if defined _PL
 call BSS_PL_free( )
#endif
 !
#if defined _KERR
 call BSS_KERR_free( )
#endif
 !
 call timing(trim(Solver)//' Solver',OPR='stop')
 !
 contains
   !
   subroutine local_alloc(what)
     !
     character(*) :: what
     !
     select case(what)
     case("E")
       !
       YAMBO_ALLOC(BS_E,(BSS_n_eig))
       if(E_SOC_nbands/=0) then
         YAMBO_ALLOC(BS_corrections,(2/n_sp_pol,BSS_n_eig))
         YAMBO_ALLOC_P(BS_E_SOC_corr,(2/n_sp_pol,BSS_n_eig))
       endif
       YAMBO_ALLOC(BS_R_left,(BSS_n_eig))
       YAMBO_ALLOC(BS_R_right,(BSS_n_eig))
       !
#if defined _KERR
       YAMBO_ALLOC_P(BS_R_right_kerr,(BSS_n_eig))
#endif
       !
#if defined _PL
       YAMBO_ALLOC_P(BS_R_PL,(2,BSS_n_eig))
#endif
       !
     case("V")
       !
       if(K_is_not_hermitian) then
         YAMBO_ALLOC(BS_V_left,(BS_H_dim,BSS_n_eig))
         YAMBO_ALLOC(BS_V_right,(BS_H_dim,BSS_n_eig))
         YAMBO_ALLOC_P(BS_overlap,(BS_H_dim,BSS_n_eig))
       else
         YAMBO_ALLOC(BS_E_real,(BSS_n_eig))
#if defined _SLEPC
         if(l_slepc) then
           YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
         endif
#endif
         if(BSS_perturbative_width.and..not.allocated(BS_corrections)) then
           YAMBO_ALLOC(BS_corrections,(1,BSS_n_eig))
         endif
       endif
       !
     end select
     !
   end subroutine local_alloc
   !
   subroutine local_free()
     !
     YAMBO_FREE(BS_E)
     YAMBO_FREE(BS_E_real)
     YAMBO_FREE(BS_corrections)
     YAMBO_FREE(BS_R_right)
     YAMBO_FREE(BS_R_left)
     YAMBO_FREE(BS_V_right)
     YAMBO_FREE(BS_V_left)
     YAMBO_FREE_P(BS_overlap)
     YAMBO_FREE_P(BS_E_SOC_corr)
#if defined _KERR
     YAMBO_FREE_P(BS_R_right_kerr)
#endif
#if defined _PL
     YAMBO_FREE_P(BS_R_PL)
#endif
     !
     ! BS_mat must be cleaned as it contains the eigenvectors. 
     !
     YAMBO_FREE(BS_mat)
     !
   end subroutine local_free
   !
#if defined _SLEPC
   subroutine K_slepc_local_init()
     !
     ! Number of states for partial diagonalization
     !================================================
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
     ! Here we will have two options to initialize the slepc matrix:
     call parser('BSSSlepcShell',Slepc_shell)
     !
     slepc_ncv      = BSS_slepc_ncv
     slepc_tol      = BSS_slepc_tol
     slepc_target_E = BSS_target_E
     !
     call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
     !
   end subroutine K_slepc_local_init
#endif
   !
end subroutine
