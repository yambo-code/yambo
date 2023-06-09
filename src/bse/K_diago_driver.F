!
!        Copyright (C) 2000-2023 the YAMBO team
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
! headers
!
#if defined _SLEPC && !defined _NL
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
#endif
!
subroutine K_diago_driver(iq,W,X_static) 
 !
 ! Generic diagonalization method to solve resonant and non resonant Hamiltonians.
 !
 ! 10/06/2015 Added by HM: Generic slepc method to solve 
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:intc
 use BS,             ONLY:BS_K_coupling,BS_H_dim,BS_K_dim,BS_perturbative_SOC,&
&                         BS_res_ares_n_mat,l_BS_optics,l_BS_dichroism !,BS_Blocks_symmetrize_K,&
 use BS_solvers,     ONLY:io_BSS_diago,BSS_perturbative_width,BSS_mode,&
&                         BSS_write_eig_2_db,BSS_eh_W,BS_mat,BSS_n_eig,BS_E
 use X_m,            ONLY:X_t,X_OUTPUT_driver
 use electrons,      ONLY:n_sp_pol,n_spinor
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:K_diago_response_functions,LINEAR_ALGEBRA_driver,&
 &                        K_diago_left_residuals,K_diago_right_residuals,PL_diago_residual
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
#if defined _SLEPC && !defined _NL
 use interfaces,     ONLY:MATRIX_slepc
 !
 use petscmat
 use slepceps
 use slepcepsdef
 use petscmatdef
 !
 use BS_solvers,     ONLY:BSS_slepc_matrix,Slepc_v,BS_HAYVEC_free
#endif
 use BS,             ONLY:l_BS_kerr,l_BS_magnons,l_BS_photolum
 use MAGNONS,        ONLY:BSS_MAGN_free
 use PHOTOLUM,       ONLY:BSS_PL_free
 !
#include<memory.h>
 ! 
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 integer       :: i_BS_mat,Input_n_eig
 logical       :: K_is_not_hermitian,l_diago,l_slepc,l_target_energy,l_dip_more
 character(1)  :: sec
 character(5)  :: Solver
 !
 ! Slepc specific
 !
#if defined _SLEPC && !defined _NL
 Mat                   :: slepc_mat
 PetscErrorCode        :: ierr
#endif
 !
 ! Excitonic hamiltonian, residuals independent
 !==============================================
 real(SP),allocatable    :: BS_E_real(:)
 real(SP)   ,pointer     :: BS_E_SOC_corr(:,:) => null()
 complex(SP),allocatable :: BS_corrections(:,:)
 !
 ! Coupling
 complex(SP),allocatable :: BS_V_left(:,:)
 complex(SP),allocatable :: BS_V_right(:,:)
 complex(SP),pointer     :: BS_overlap(:,:)    => null()
 !
 ! Residuals, different for each response function
 !=================================================
 complex(SP),pointer     :: BS_R_left_abs(:)     => null()
 complex(SP),pointer     :: BS_R_right_abs(:)    => null()
 !
 real(SP),   pointer     :: BS_R_PL(:,:)         => null()
 !
 complex(SP),pointer     :: BS_R_left_kerr(:)    => null()
 complex(SP),pointer     :: BS_R_right_kerr(:)   => null()
 complex(SP),pointer     :: BS_R_right_dich(:,:) => null()
 !
 complex(SP),pointer     :: BS_R_left_magn(:,:)  => null()
 complex(SP),pointer     :: BS_R_right_magn(:,:) => null()
 !
 ! I/O
 integer                 :: io_err,ID
 type(X_t),intent(in)    :: X_static
 !
 l_diago     = index(BSS_mode,'d')/=0
#if defined _SLEPC && !defined _NL
 l_slepc     = index(BSS_mode,'s')/=0
#endif
 !
 l_dip_more=l_BS_kerr.or.l_BS_dichroism
 !
 if(l_diago) Solver="Diago"
#if defined _SLEPC && !defined _NL
 if(l_slepc) Solver="Slepc"
#endif
 !
 call timing(trim(Solver)//' Solver',OPR='start')
 !
 ! Output files initialization
 !=============================
 call X_OUTPUT_driver("DEF INIT",solver=Solver) 
 !
 ! Sectioning
 !============
 if (isec(2)/=0) sec="="
 if (isec(2)==0) sec="+"
 call section(sec,trim(Solver)//' Solver @q'//trim(intc(iq)))
 !  
#if defined _SLEPC && !defined _NL
 if(l_slepc) call K_slepc_local_init()
#endif
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 call parser('BSSPertWidth',BSS_perturbative_width)
 !
 do i_BS_mat=1,BS_res_ares_n_mat
   !
   if(l_diago) then
     if(.not.BS_K_coupling) BSS_n_eig = BS_K_dim(i_BS_mat)
     if(     BS_K_coupling) BSS_n_eig = BS_H_dim
   endif
   Input_n_eig=BSS_n_eig
   !
   ! Allocation (BS_R and BS_E)
   !===========================
   YAMBO_ALLOC(BS_E,(BSS_n_eig))
   call local_alloc("E")
   !
   ! Diagonalization DB (IN)
   !========================
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
   io_err=io_BSS_diago(iq,i_BS_mat,ID,X_static,BS_E,BS_R_left_abs,BS_R_right_abs,BS_E_SOC_corr,&
  &                    BS_R_left_magn,BS_R_right_magn,BS_R_left_kerr,BS_R_right_kerr,BS_R_right_dich,BS_R_PL)
   !
   K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
   !
   ! Kernel and/or BSS components loading
   !======================================
   if (io_err<0) then
     !
     call K_components_folded_in_serial_arrays(iq)
     !
     ! Below two operations:
     ! - The blocks are sent  either to a big matrix or
     !   to a distributed (SLEPC shells / or eventually BLACS) matrix
     ! - The resulting matrix is symmetrized
     !
     if (l_diago) then
       !
       call K_stored_in_a_big_matrix(i_BS_mat,iq,"DIAGO")
       !
       ! These line will be needed when the BS kernel will not be folded in BS_mat. But in order to do so
       ! also linear_algebra_is_parallel must be swiched off when K is not hermitian.
       !=========================
       !
       !if (.not.linear_algebra_is_parallel) call K_stored_in_a_big_matrix(iq,"DIAGO")
       !
#if defined _SCALAPACK
       ! Here maybe the name "K_blacs_matrix(iq)" would probably be more appropiate
       !if (     linear_algebra_is_parallel) call BS_Blocks_symmetrize_K()
#endif
       !
     endif
     !
#if defined _SLEPC && !defined _NL
     if(l_slepc) then
       ! 1. Here we will define a PetscShell matrix and define the matrix-vector multiplication
       ! using the K_multiply_by_V function used in the Haydock method
       if (.not.BSS_slepc_matrix) call K_shell_matrix(i_BS_mat,slepc_mat)
       ! 2. Here we create a distributed PETSC matrix from the BS_blks
       if (     BSS_slepc_matrix) call K_stored_in_a_slepc_matrix(i_BS_mat,slepc_mat)
     endif
#endif
     !
   endif
   !
   ! Allocation (BS_E_real or BS_V)
   !===============================
   call local_alloc("V")
   !
   if (io_err<0) then
     !
     ! Diagonalization of the excitonic hamiltonian
     !==============================================
     if(l_diago) call live_timing('BSK diagonalize',1)
     !
     if (K_is_not_hermitian) then
       !
       if(l_diago) call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_cmpl=BS_E,V_left=BS_V_left,V_right=BS_V_right)
#if defined _SLEPC && !defined _NL
       if(l_slepc) call MATRIX_slepc(slepc_mat,l_target_energy,BSS_n_eig,BS_V_right,V_left=BS_V_left,E_cmpl=BS_E)
#endif
       !
       if (BSS_write_eig_2_db) BS_mat=BS_V_right
       !
     else
       !
       if(l_diago) call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_real=BS_E_real)
#if defined _SLEPC && !defined _NL
       if(l_slepc) call MATRIX_slepc(slepc_mat,l_target_energy,BSS_n_eig,BS_mat,E_real=BS_E_real)
#endif
       !
       BS_E=cmplx(BS_E_real,0._SP,kind=SP)
       !
     endif
     !
#if defined _SLEPC && !defined _NL
     !
     if (l_slepc) then
       ! BSS_n_eig could be lowered by MATRIX_slepc
       if (Input_n_eig>BSS_n_eig) call K_slepc_resize_variables()
       ! Destroy the matrix
       call MatDestroy(slepc_mat,ierr)
     endif
     !
#endif
     !
     if(l_diago) call live_timing( )
     !
     ! Construct the residuals of epsilon
     !====================================
     if (K_is_not_hermitian) then
       !
       ! Right residuals
       if(l_BS_optics.or.l_BS_kerr.or.l_BS_dichroism) then
                        call K_diago_right_residuals("opt1",i_BS_mat,BS_E,BS_R_right_abs, BS_V_right)
         if(l_dip_more) call K_diago_right_residuals("opt2",i_BS_mat,BS_E,BS_R_right_kerr,BS_V_right)
       endif
       if(l_BS_dichroism) then
         call K_diago_right_residuals("dic1",i_BS_mat,BS_E,BS_R_right_dich(:,1),BS_V_right)
         call K_diago_right_residuals("dic2",i_BS_mat,BS_E,BS_R_right_dich(:,2),BS_V_right)
       endif
       if(l_BS_magnons)   then
                          call K_diago_right_residuals("mag1",i_BS_mat,BS_E,BS_R_right_magn(1,:),BS_V_right)
         if(n_spinor==2)  call K_diago_right_residuals("mag2",i_BS_mat,BS_E,BS_R_right_magn(2,:),BS_V_right)
       endif
       !
       call K_diago_overlap_matrix(BS_V_left,BS_V_right,BS_overlap)
       !
       ! Left residuals
       if(l_BS_optics.or.l_BS_kerr.or.l_BS_dichroism) then
                        call K_diago_left_residuals("opt1",i_BS_mat,BS_E,BS_R_left_abs, BS_V_left,BS_overlap)
         if(l_dip_more) call K_diago_left_residuals("opt2",i_BS_mat,BS_E,BS_R_left_kerr,BS_V_left,BS_overlap)
       endif
       if(l_BS_magnons) then
                         call K_diago_left_residuals("mag1",i_BS_mat,BS_E,BS_R_left_magn(1,:),BS_V_left,BS_overlap)
         if(n_spinor==2) call K_diago_left_residuals("mag2",i_BS_mat,BS_E,BS_R_left_magn(2,:),BS_V_left,BS_overlap)
       endif
       !
       ! DS: this also should be changed according to the general structure
       if(l_BS_photolum) call PL_diago_residual(BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian,BS_overlap)
       !
     else
       !
       ! Right residuals
       if(l_BS_optics.or.l_BS_kerr.or.l_BS_dichroism) then
                        call K_diago_right_residuals("opt1", i_BS_mat,BS_E,BS_R_right_abs, BS_mat)
         if(l_dip_more) call K_diago_right_residuals("opt2",i_BS_mat,BS_E,BS_R_right_kerr,BS_mat)
       endif
       if(l_BS_dichroism) then
         call K_diago_right_residuals("dic1",i_BS_mat,BS_E,BS_R_right_dich(:,1),BS_mat)
         call K_diago_right_residuals("dic2",i_BS_mat,BS_E,BS_R_right_dich(:,2),BS_mat)
       endif
       if(l_BS_magnons) then
                          call K_diago_right_residuals("mag1",i_BS_mat,BS_E,BS_R_right_magn(1,:),BS_mat)
         if(n_spinor==2)  call K_diago_right_residuals("mag2",i_BS_mat,BS_E,BS_R_right_magn(2,:),BS_mat)
       endif
       !
       ! Left residuals
       if(l_BS_optics.or.l_BS_kerr.or.l_BS_dichroism)  BS_R_left_abs =conjg(BS_R_right_abs)
       if(l_BS_magnons)                                BS_R_left_magn=conjg(BS_R_right_magn)
       !
       ! DS: this also should be changed according to the general structure
       if(l_BS_photolum) call PL_diago_residual(BS_mat,BS_mat,BS_R_PL,K_is_not_hermitian)
       !
     endif
     !
   endif
   !
   ! Define perturbative corrections
   !=================================
   if( BS_perturbative_SOC ) then
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
   ! Now I calculate the physical quantities
   !=========================================
   call K_diago_response_functions(iq,i_BS_mat,W,BS_E,BS_R_left_abs,BS_R_right_abs,BS_E_SOC_corr,&
   &               BS_R_left_magn,BS_R_right_magn,BS_R_left_kerr,BS_R_right_kerr,BS_R_right_dich,BS_R_PL)
   !
   ! Diagonalization DB (OUT)
   !==========================
   if (io_err/=0) then
     !
     call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
     io_err=io_BSS_diago(iq,i_BS_mat,ID,X_static,BS_E,BS_R_left_abs,BS_R_right_abs,BS_E_SOC_corr,&
     &             BS_R_left_magn,BS_R_right_magn,BS_R_left_kerr,BS_R_right_kerr,BS_R_right_dich,BS_R_PL)
     !
   endif
   !
   YAMBO_FREE(BS_E)
   call local_free( )
   !
 enddo
 !
 ! Initialize & write the output file 
 !====================================
 call K_OUTPUT(iq,W,"open dump close "//trim(Solver),"ALL")
 !
 ! CLEAN
 !
#if defined _SLEPC && !defined _NL
 if(l_slepc) call K_slepc_local_free()
#endif
 !
 call BSS_PL_free( )
 call BSS_MAGN_free( )
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
       if( BS_perturbative_SOC) then
         YAMBO_ALLOC(BS_corrections,(2/n_sp_pol,BSS_n_eig))
         YAMBO_ALLOC_P(BS_E_SOC_corr,(2/n_sp_pol,BSS_n_eig))
       endif
       if (l_BS_optics) then
         YAMBO_ALLOC_P(BS_R_left_abs,(BSS_n_eig))
         YAMBO_ALLOC_P(BS_R_right_abs,(BSS_n_eig))
       endif
       if (l_BS_kerr) then
         YAMBO_ALLOC_P(BS_R_right_kerr,(BSS_n_eig))
       endif
       if (l_dip_more) then
         YAMBO_ALLOC_P(BS_R_left_kerr,(BSS_n_eig))
       endif
       if (l_BS_dichroism) then
         YAMBO_ALLOC_P(BS_R_right_dich,(BSS_n_eig,2))
       endif
       if (l_BS_magnons) then
         YAMBO_ALLOC_P(BS_R_left_magn,(2,BSS_n_eig))
         YAMBO_ALLOC_P(BS_R_right_magn,(2,BSS_n_eig))
       endif
       if(l_BS_photolum) then
         YAMBO_ALLOC_P(BS_R_PL,(2,BSS_n_eig))
       endif
       !
     case("V")
       !
       if(K_is_not_hermitian) then
         YAMBO_ALLOC(BS_V_left,(BS_H_dim,BSS_n_eig))
         YAMBO_ALLOC(BS_V_right,(BS_H_dim,BSS_n_eig))
         YAMBO_ALLOC_P(BS_overlap,(BSS_n_eig,BSS_n_eig))
       else
         YAMBO_ALLOC(BS_E_real,(BSS_n_eig))
#if defined _SLEPC && !defined _NL
         if(l_slepc) then
           if (BS_K_coupling) then
             YAMBO_ALLOC(BS_mat,(BS_H_dim,BSS_n_eig))
           else
             YAMBO_ALLOC(BS_mat,(BS_K_dim(i_BS_mat),BSS_n_eig))
           endif
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
     YAMBO_FREE(BS_E_real)
     YAMBO_FREE(BS_corrections)
     YAMBO_FREE(BS_V_right)
     YAMBO_FREE(BS_V_left)
     YAMBO_FREE_P(BS_overlap)
     YAMBO_FREE_P(BS_E_SOC_corr)
     YAMBO_FREE_P(BS_R_right_abs)
     YAMBO_FREE_P(BS_R_left_abs)
     YAMBO_FREE_P(BS_R_right_kerr)
     YAMBO_FREE_P(BS_R_left_kerr)
     YAMBO_FREE_P(BS_R_right_dich)
     YAMBO_FREE_P(BS_R_right_magn)
     YAMBO_FREE_P(BS_R_left_magn)
     YAMBO_FREE_P(BS_R_PL)
     !
     ! BS_mat must be cleaned as it contains the eigenvectors. 
     !
     YAMBO_FREE(BS_mat)
     !
   end subroutine local_free
   !
#if defined _SLEPC && !defined _NL
   !
   subroutine K_slepc_resize_variables()
     !
     use BS_solvers,   ONLY:BSS_resize
     !
     call warning(" resizing alla variables whose size depends on BSS_n_eig ")
     if(allocated(BS_E))          call BSS_resize(BS_E)
     !
     if(allocated(BS_corrections)) call BSS_resize(BS_corrections)
     if(associated(BS_E_SOC_corr)) call BSS_resize(BS_E_SOC_corr)
     !
     if(associated(BS_R_left_abs) )  call BSS_resize(BS_R_left_abs)
     if(associated(BS_R_right_abs))  call BSS_resize(BS_R_right_abs)
     !
     if(associated(BS_R_left_kerr) ) call BSS_resize(BS_R_left_kerr)
     if(associated(BS_R_right_kerr)) call BSS_resize(BS_R_right_kerr)
     if(associated(BS_R_right_dich)) call BSS_resize(BS_R_right_dich)
     if(associated(BS_R_left_magn) ) call BSS_resize(BS_R_left_magn)
     if(associated(BS_R_right_magn)) call BSS_resize(BS_R_right_magn)
     if(associated(BS_R_PL)        ) call BSS_resize(BS_R_PL)
     !
     if(allocated(BS_V_left))   call BSS_resize(BS_V_left)
     if(allocated(BS_V_right))  call BSS_resize(BS_V_right)
     if(associated(BS_overlap)) call BSS_resize(BS_overlap,.true.)
     !
     if(allocated(BS_E_real))   call BSS_resize(BS_E_real)
     if(allocated(BS_mat))      call BSS_resize(BS_mat)
     !
   end subroutine
   !
   subroutine K_slepc_local_init()
     !
     use com,           ONLY:msg
     !
     call parser('BSSEnTarget',l_target_energy)
     !
     if (     BSS_slepc_matrix)  call msg( 's', '[SLEPC] Faster alogorithm but BSE matrix diuplicated over MPI tasks')
     if (.not.BSS_slepc_matrix)  call msg( 's', '[SLEPC] Slower alogorithm but BSE matrix distributed over MPI tasks')
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
     call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
     !
   end subroutine K_slepc_local_init
   !
   subroutine K_slepc_local_free()
     !
     if (.not.BSS_slepc_matrix) then
       call BS_HAYVEC_free(Slepc_v%Vi)
       call BS_HAYVEC_free(Slepc_v%Vo)
       deallocate(Slepc_v%Vi)
       deallocate(Slepc_v%Vo)
       call PARALLEL_Haydock_VEC_COMMs('reset')
     endif
     !
   end subroutine K_slepc_local_free
#endif
   !
end subroutine
