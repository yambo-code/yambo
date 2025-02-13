!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS BM NM
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
 use com,            ONLY:msg
 use stderr,         ONLY:intc
 use units,          ONLY:HA2EV
 use BS,             ONLY:BS_K_coupling,BS_H_dim,BS_K_dim,BS_perturbative_SOC,&
&                         BS_res_ares_n_mat,l_BS_optics,l_BS_dichroism,l_BS_ares_from_res !,BS_Blocks_symmetrize_K,&
 use BS_solvers,     ONLY:io_BSS_diago,BSS_perturbative_width,BSS_mode, BSS_resize, &
&                         BSS_write_eig_2_db,BSS_eh_W,BSS_n_eig,BS_E,BSS_slepc_pseudo_herm,write_bs_ev_par,&
&                         BSS_ydiago_solver,BSS_first_eig,BSS_target_E,BSS_trange_E,BSS_er,BSS_slepc_precondition
 use X_m,            ONLY:X_t,X_OUTPUT_driver
 use electrons,      ONLY:n_sp_pol,n_spinor
 use frequency,      ONLY:w_samp
 use com,            ONLY:isec
 use LIVE_t,         ONLY:live_timing
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:K_diago_response_functions,LINEAR_ALGEBRA_driver,YDIAGO_driver,&
 &                        PL_diago_residual,K_diago_L_res,K_diago_R_res,&
 &                        K_diago_perturbative, K_stored_in_a_big_matrix
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:REP,VERIFY,OP_WR_CL,OP_RD_CL
 use parser_m,       ONLY:parser
 use timing_m,       ONLY:timing
 use parallel_int,   ONLY:PP_wait
#if defined _SLEPC && !defined _NL
 use interfaces,     ONLY:MATRIX_slepc
 use petscmat
 use slepceps
 use slepcepsdef
 use petscmatdef
 !
 use BS_solvers,     ONLY:BSS_slepc_approach,BSS_slepc_matrix_format,&
&                         Slepc_v,BS_HAYVEC_free
#endif
 use BS,             ONLY:l_BS_abs,l_BS_kerr,l_BS_kerr_asymm,l_BS_magnons,l_BS_photolum,l_BS_dichroism
 use MAGNONS,        ONLY:BSS_MAGN_free
 use PHOTOLUM,       ONLY:BSS_PL_free
 !
#if defined _SLEPC && !defined _NL
 use slepceps,       ONLY:SlepcInitialize
 use petscmat,       ONLY:PETSC_NULL_CHARACTER
 use petscmatdef,    ONLY:tMat
#endif
 !
#include<y_memory.h>
 ! 
 type(w_samp)  :: W
 integer       :: iq
 !
 ! Work Space
 !
 integer          :: i_BS_mat,Input_n_eig, BS_mat_dim, neigs_this_cpu, neig_shift, elpa_solver = 2
 logical          :: K_is_not_hermitian,l_diago,l_slepc,l_dip_more,l_target_erange,l_neig_input, &
 &                   l_ydiago,l_kerr_lft,l_kerr_rgt,l_abs_res
 character(1)     :: sec
 character(10)    :: Solver
 ! Ydiago varibles
 integer, target  :: neigs_range(2)
 real(SP), target :: eigvals_range(2)
 !
 ! Slepc specific
 !
#if defined _SLEPC && !defined _NL
 ! Mat becomes type(tMat) after source is pre-processed
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
 complex(SP),allocatable         :: BS_mat(:,:)
 complex(SP),allocatable, target :: BS_V_left(:,:)
 complex(SP),allocatable, target :: BS_V_right(:,:)
 complex(SP),pointer             :: BS_overlap(:,:)    => null()
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
 ! Temporary pointer 
 complex(SP),pointer     :: tmp_res_ptrR(:)     => null()
 complex(SP),pointer     :: tmp_res_ptrL(:)     => null()
 !
 ! I/O
 integer                 :: io_err,ID
 type(X_t),intent(in)    :: X_static
 !
 l_abs_res =l_BS_abs.or.l_BS_kerr.or.l_BS_dichroism
 l_kerr_lft=l_BS_kerr_asymm.or.l_BS_dichroism
 l_kerr_rgt=l_BS_kerr      .or.l_BS_dichroism
 !
 l_diago     = index(BSS_mode,'o')/=0 ! old diago
#if defined _SLEPC && !defined _NL
 l_slepc     = index(BSS_mode,'s')/=0
#endif
 l_ydiago    = index(BSS_mode,'d')/=0 
 !
 if(l_diago) Solver="Diago"
#if defined _SLEPC && !defined _NL
 if(l_slepc) Solver="Slepc"
#endif
 if(l_ydiago) Solver="Ydiago" 
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
 call K_local_init()
 !
 ! Eigenstates 2 DB ?
 !====================
 if (.not.BSS_write_eig_2_db) call parser('WRbsWF',BSS_write_eig_2_db)
 call parser('BSSPertWidth',BSS_perturbative_width)
 !
 do i_BS_mat=1,BS_res_ares_n_mat
   !
   if(.not.BS_K_coupling) BS_mat_dim = BS_K_dim(i_BS_mat)
   if(     BS_K_coupling) BS_mat_dim = BS_H_dim
   !
   call BSS_n_eig_init()
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
       call K_stored_in_a_big_matrix(i_BS_mat,BS_mat_dim,BS_V_right,iq,"DIAGO")
       !
       ! These line will be needed when the BS kernel will not be folded in BS_mat. But in order to do so
       ! also linear_algebra_is_parallel must be swiched off when K is not hermitian.
       !=========================
       !
       !if (.not.linear_algebra_is_parallel) call K_stored_in_a_big_matrix(i_BS_mat,BS_V_right,iq,"DIAGO")
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
       !
       if (index(BSS_slepc_matrix_format,"shell")>0) then
         ! 1. Here we will define a PetscShell matrix and define the matrix-vector multiplication
         ! using the K_multiply_by_V function used in the Haydock method
         ! control of hermitian / pseudo-hermitian / non hermitian is inside K_shell_matrix
         call K_shell_matrix(i_BS_mat,slepc_mat)
       else if (index(BSS_slepc_matrix_format,"explicit")>0) then
         ! 2. Here we create a distributed PETSC matrix from the BS_blks
         !    There are 2 cases
         ! control of hermitian / pseudo-hermitian / non hermitian is in part explicit here
         if (BSS_slepc_pseudo_herm) then
           ! 2.1. Nest matrix uses pseudo-hermitian structure of BSE, only for the coupling case
           call K_stored_in_a_nest_matrix(i_BS_mat,iq,slepc_mat)
         else
           ! 2.2. Single PETSc matrix, for resonant case or coupling
           call K_stored_in_a_slepc_matrix(i_BS_mat,iq,slepc_mat)
         endif
       endif        
     endif
#endif
     !
   endif
   !
   ! Allocation (BS_E_real or BS_V)
   !===============================
   if (.not. l_ydiago) call local_alloc("V")
   !
   if (io_err<0) then
     !
     ! Diagonalization of the excitonic hamiltonian
     !==============================================
     if(l_diago) call live_timing('BSK diagonalize',1)
     !
     if (K_is_not_hermitian) then
       !
       if (l_ydiago) then 
         ! BS_V_left are only referenced in non-hermitian case
         ! In coupling case, if the user requested n values,
         ! we compute the first min(n/2,BS_H_dim/2) positive and 
         ! min(n/2,BS_ham/2) negative eigvals and eigvectors, making it a total of min(n,BS_ham)
         call YDIAGO_driver(i_BS_mat, BS_E, BS_V_right, neigs_this_cpu, neig_shift, &
         &                   neigs_range=neigs_range, eigvals_range=eigvals_range,  &
         &                   BS_VL=BS_V_left,BS_overlap=BS_overlap,                 &
         &                   solver_type=BSS_ydiago_solver, elpasolver=elpa_solver)
         BSS_n_eig = size(BS_E)
      endif
      !
      if(l_diago) then
         ! call LINEAR_ALGEBRA_driver(DIAGO,M=BS_V_right,E_cmpl=BS_E,V_left=BS_V_left)
         ! The following code should be replaced by the above line wich a change in LINEAR_ALGEBRA_driver
         YAMBO_ALLOC(BS_mat,(BS_mat_dim,BS_mat_dim))
         BS_mat=BS_V_right
         call LINEAR_ALGEBRA_driver(DIAGO,M=BS_mat,E_cmpl=BS_E,V_left=BS_V_left,V_right=BS_V_right)
         YAMBO_FREE(BS_mat)
       endif
       !
#if defined _SLEPC && !defined _NL
       if(l_slepc) then
         call MATRIX_slepc(slepc_mat,BSS_n_eig,BS_V_right,V_left=BS_V_left,E_cmpl=BS_E)
       endif
#endif
       !
     else
       !
       if (l_ydiago) then
         call YDIAGO_driver(i_BS_mat, BS_E, BS_V_right, neigs_this_cpu, neig_shift, &
&                           neigs_range=neigs_range, eigvals_range=eigvals_range,   &
&                           solver_type=BSS_ydiago_solver, elpasolver=elpa_solver)
         BSS_n_eig = size(BS_E)
       endif
       !
       if(l_diago) then
         call LINEAR_ALGEBRA_driver(DIAGO,M=BS_V_right,E_real=BS_E_real)
         BS_E=cmplx(BS_E_real,0._SP,kind=SP)
         neigs_this_cpu = BSS_n_eig
       endif
       !
#if defined _SLEPC && !defined _NL
       if(l_slepc) then
         call MATRIX_slepc(slepc_mat,BSS_n_eig,BS_V_right,E_real=BS_E_real)
         BS_E=cmplx(BS_E_real,0._SP,kind=SP)
         neigs_this_cpu = BSS_n_eig
       endif
#endif
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
     if (l_diago) call live_timing( )
     !
     if (l_ydiago) then
       if (Input_n_eig>BSS_n_eig) call K_ydiago_resize_variables()
     endif
     !
     ! Compute residuals
     !
     YAMBO_ALLOC_P(tmp_res_ptrR,(BSS_n_eig))
     !
     ! Construct the residuals of epsilon
     !====================================
     !
     ! Right residuals
     if(l_abs_res) then
                      call K_diago_R_res("opt1",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,BS_R_right_abs, BS_V_right)
       if(l_kerr_rgt) call K_diago_R_res("opt2",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,BS_R_right_kerr,BS_V_right)
     endif
     !
     if(l_BS_dichroism) then
       call K_diago_R_res("dic1",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,tmp_res_ptrR,BS_V_right)
       BS_R_right_dich(:,1) = tmp_res_ptrR
       call K_diago_R_res("dic2",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,tmp_res_ptrR,BS_V_right)
       BS_R_right_dich(:,2) = tmp_res_ptrR
     endif
     !
     if(l_BS_magnons) then
       call K_diago_R_res("mag1",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,tmp_res_ptrR,BS_V_right)
       BS_R_right_magn(:,1) = tmp_res_ptrR
       if(n_spinor==2) then
         call K_diago_R_res("mag2",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,tmp_res_ptrR,BS_V_right)
         BS_R_right_magn(:,2) = tmp_res_ptrR
       endif
     endif
     !
     ! Left residuals needs to be computed only if the kernel is not hermitian
     if (K_is_not_hermitian) then
       !
       ! In the ydiago solver, the BS_overlap is an identity matrix.
       if (.not.l_ydiago) call K_diago_overlap_matrix(BS_V_left,BS_V_right,BS_overlap)
       !
       ! Left residuals
       if(l_abs_res) then
         call K_diago_L_res("opt1",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,BS_R_left_abs,BS_V_left,BS_overlap)
         if(l_kerr_lft) call K_diago_L_res("opt2",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,BS_R_left_kerr,BS_V_left,BS_overlap)
       endif
       !
       if(l_BS_magnons) then
         call K_diago_L_res("mag1",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,tmp_res_ptrR,BS_V_left,BS_overlap)
         BS_R_left_magn(:,1) = tmp_res_ptrR
         if(n_spinor==2) then
           call K_diago_L_res("mag2",i_BS_mat,BS_E,neigs_this_cpu,neig_shift,tmp_res_ptrR,BS_V_left,BS_overlap)
           BS_R_left_magn(:,2) = tmp_res_ptrR
         endif
       endif
       !
     else
       !
       ! Left residuals
       if( l_abs_res  ) BS_R_left_abs =conjg(BS_R_right_abs)
       if( l_kerr_lft ) BS_R_left_kerr=conjg(BS_R_right_kerr)
       if(l_BS_magnons) BS_R_left_magn=conjg(BS_R_right_magn)
       !
     endif
     !
     YAMBO_FREE_P(tmp_res_ptrR)
     !
     ! DS: this also should be changed according to the general structure
     if(l_BS_photolum) then
       if (l_ydiago) call error("PL not implemented in Ydiago solver")
       if (K_is_not_hermitian) then
         call PL_diago_residual(BS_V_left,BS_V_right,BS_R_PL,K_is_not_hermitian,BS_overlap)
       else
         call PL_diago_residual(BS_V_right,BS_V_right,BS_R_PL,K_is_not_hermitian)
       endif
     endif
     !
   endif
   !
   ! Define perturbative corrections
   !=================================
   if( BS_perturbative_SOC ) then
      call K_diago_perturbative(2/n_sp_pol,'SOC', i_BS_mat, &
      &           BS_corrections, BS_V_right, neigs_this_cpu, neig_shift, BS_V_left, BS_Overlap) 
      BS_E_SOC_corr=real(BS_corrections)
   endif
   !
   if(BSS_perturbative_width) then 
      call K_diago_perturbative(1,'WID', i_BS_mat, &
      &           BS_corrections, BS_V_right, neigs_this_cpu, neig_shift, BS_V_left, BS_Overlap) 
      BS_E=BS_E-aimag(BS_corrections(1,:))
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
     &     BS_R_left_magn,BS_R_right_magn,BS_R_left_kerr,BS_R_right_kerr,BS_R_right_dich,BS_R_PL,&
     &                                                   BS_mat=BS_V_right,write_ev=.not.l_ydiago)
     !
     call PP_wait()
     if (l_ydiago .and. BSS_write_eig_2_db) call write_bs_ev_par(iq, i_BS_mat, BS_V_right, BS_V_left)
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
       if (l_abs_res) then
         YAMBO_ALLOC_P(BS_R_left_abs,(BSS_n_eig))
         YAMBO_ALLOC_P(BS_R_right_abs,(BSS_n_eig))
       endif
       if (l_kerr_rgt) then
         YAMBO_ALLOC_P(BS_R_right_kerr,(BSS_n_eig))
       endif
       if (l_kerr_lft) then
         YAMBO_ALLOC_P(BS_R_left_kerr,(BSS_n_eig))
       endif
       if (l_BS_dichroism) then
         YAMBO_ALLOC_P(BS_R_right_dich,(BSS_n_eig,2))
       endif
       if (l_BS_magnons) then
         YAMBO_ALLOC_P(BS_R_left_magn,(BSS_n_eig,2))
         YAMBO_ALLOC_P(BS_R_right_magn,(BSS_n_eig,2))
       endif
       if(l_BS_photolum) then
         YAMBO_ALLOC_P(BS_R_PL,(2,BSS_n_eig))
       endif
       !
     case("V")
       !
       if(K_is_not_hermitian) then
         YAMBO_ALLOC(BS_V_left,(BS_H_dim,BSS_n_eig))
         YAMBO_ALLOC_P(BS_overlap,(BSS_n_eig,BSS_n_eig))
       else
         YAMBO_ALLOC(BS_E_real,(BSS_n_eig))
         if(BSS_perturbative_width.and..not.allocated(BS_corrections)) then
           YAMBO_ALLOC(BS_corrections,(1,BSS_n_eig))
         endif
       endif
       !
#if defined _SLEPC && !defined _NL
       ! In the old diago case BS_V_right is allocated by K_stored_in_a_big_matrix
       if(l_slepc) then
         YAMBO_ALLOC(BS_V_right,(BS_mat_dim,BSS_n_eig))
       endif
#endif
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
   end subroutine local_free
   !
   !
   subroutine K_ydiago_resize_variables()
    !
    ! We need to realloc as sizes can change.
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
   end subroutine K_ydiago_resize_variables
   !
#if defined _SLEPC && !defined _NL
   !
   subroutine K_slepc_resize_variables()
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
     !
   end subroutine
   !
#endif
   !
   subroutine K_local_init()
     !
     K_is_not_hermitian=BS_K_coupling.or.(allocated(BSS_eh_W).and..not.BSS_perturbative_width)
     !
#if defined _SLEPC && !defined _NL
     !
     if (l_slepc) then
       !
       if (index(BSS_slepc_matrix_format,'explicit')>0) &
       & call msg( 's', '[SLEPC] Faster algorithm, BSE matrix re-distributed from internal to slepc parallel scheme')
       if (BSS_slepc_matrix_format=='shell')            &
       & call msg( 's', '[SLEPC] Slower algorithm, BSE matrix used with yambo parallel distribution scheme')
       !
       call SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
       !
     endif
     !
#endif
     !
     if (BSS_target_E/=0._SP .and. BSS_slepc_pseudo_herm) then 
       BSS_slepc_pseudo_herm = .false. 
       call warning('Target energy not supported in pseudo-hermitian case. Switching to non-hermitian diagonalization')
     endif
     if (BSS_slepc_precondition/="none" .and. BSS_slepc_pseudo_herm) then 
       BSS_slepc_pseudo_herm = .false. 
       call warning('Preconditioning not supported in pseudo-hermitian case. Switching to non-hermitian diagonalization')
     endif

     !
     if (l_ydiago) then
       !
       if (BSS_ydiago_solver/="s" .and. BSS_ydiago_solver/="e") then
         call warning("Invalid ydiago solver. Switching to default")
#if defined _ELPA
         BSS_ydiago_solver="e"
#else
         BSS_ydiago_solver="s"
#endif
       endif
       !
#if defined _GPU && defined _ELPA
       if (BSS_ydiago_solver=="s") then
         call warning("gpu ported code. Switching to Elpa solver")
         BSS_ydiago_solver="e"
       endif
#endif
       !
#if !defined _ELPA
       if (BSS_ydiago_solver=="e") then
         call warning("Elpa solver selected, but elpa not linked. Switching to scalapack")
         BSS_ydiago_solver="s"
       endif
#endif
       !
       l_target_erange=BSS_trange_E(1)>0._SP .and. BSS_trange_E(2)>BSS_trange_E(1)
       l_neig_input=BSS_n_eig>0
       ! Here I double the number of n_eig to get correct allocations
       if (l_neig_input .and. K_is_not_hermitian) BSS_n_eig=BSS_n_eig*2
       !
       if (l_target_erange .and. BSS_ydiago_solver=="e") then
#if defined _GPU && defined _ELPA
         call warning(' energy range not compatible with elpa. Switching it off on gpu')
         l_target_erange=.false.
#else
         call warning(' energy range not compatible with elpa. Switching to scalapack on cpu')
         BSS_ydiago_solver="s"
#endif
       endif
       !
       if (BSS_first_eig>1 .and. BSS_ydiago_solver=="e") then
#if defined _GPU && defined _ELPA
         call warning(' eigenvalues range not compatible with elpa. Switching it off on gpu')
         BSS_first_eig=1
#else
         call warning(' eigenvalues range not compatible with elpa. Switching to scalapack on cpu')
         BSS_ydiago_solver="s"
#endif
       endif
       !
       if (l_neig_input .and. BSS_ydiago_solver=="e" .and. K_is_not_hermitian) then
#if defined _GPU && defined _ELPA
         call warning(' Few eigenvalues not compatible with non hermitian elpa. Switching it off on gpu')
         l_neig_input=.false.
#else
         call warning(' Few eigenvalues not compatible with non hermitian elpa. Switching to scalapack on cpu')
         BSS_ydiago_solver="s"
#endif
       endif
       !
     endif
     !
   end subroutine K_local_init
   !
   subroutine BSS_n_eig_init()
     !
     ! Number of states to be computed
     !=================================
     !
     if (l_slepc) then
       !
       if (BSS_n_eig<1) then
         BSS_n_eig = nint(BS_H_dim*0.01)
         if (BSS_n_eig<1) BSS_n_eig = 1
         call warning('N_eigen < 1. Calculating only 1% of the eigenvalues.')
       endif
       !
       if (BSS_n_eig>BS_H_dim) then
         call warning('N_eigen > BS_H_dim. Calculating 10% of the eigenvalues.')
         BSS_n_eig = nint(BS_H_dim*0.1)
       endif
       !
       if (BSS_n_eig>BS_K_dim(1) .and. BSS_slepc_pseudo_herm) then
         call warning('N_eigen > BS_K_dim and pseudo-hermitian case. Calculating 10% of the eigenvalues.')
         BSS_n_eig = nint(BS_K_dim(1)*0.1)
       endif
       !
     endif
     !
     if(l_diago) BSS_n_eig = BS_mat_dim
     !
     ! set a Ydiago defaults.
     if(l_ydiago) then
       !
       ! In case of ydiago, BSS_n_eig will be overwritten after the solver is called 
       if( BSS_n_eig==0 ) BSS_n_eig = BS_mat_dim
       !
       neigs_range  = 0
       eigvals_range= -1._SP
       !
       if( l_neig_input ) then
         ! in case the user specified the number of eigenvalues
         neigs_range(1)=1
         if(BSS_First_Eig>0) neigs_range(1)=BSS_First_eig
         neigs_range(2)=neigs_range(1)+BSS_n_eig-1
         eigvals_range= 0._SP
         if (neigs_range(1)<1)          neigs_range(1) = 1
         if (neigs_range(2)>BS_mat_dim) neigs_range(2) = BS_mat_dim
       else if (l_target_erange) then
         ! in case the user specified an energy range
         neigs_range  = 0
         eigvals_range= BSS_trange_E
       ! The following is a good idea but it is the source of many fails in the
       ! test-suite. Commenting it for now
       !else if (BSS_ydiago_solver=="s") then
       !  ! with scalapack use the energy range needed by default
       !  neigs_range  = 0
       !  eigvals_range= BSS_er
       endif
       !
     endif
     !
     Input_n_eig=BSS_n_eig
     !
     ! For backward compatibitly I set  
     neig_shift = 0
     neigs_this_cpu = BSS_n_eig
     !
     ! These will be changed by the YDIAGO_driver routine
     ! each cpu contains eigen-vectors from [neig_shift + 1, neigs_this_cpu + neig_shift]
     !
   end subroutine BSS_n_eig_init
   !
#if defined _SLEPC && !defined _NL
   !
   subroutine K_slepc_local_free()
     !
     if (BSS_slepc_matrix_format=='shell') then
       call BS_HAYVEC_free(Slepc_v%Vi)
       call BS_HAYVEC_free(Slepc_v%Vo)
       deallocate(Slepc_v%Vi)
       deallocate(Slepc_v%Vo)
       call PARALLEL_Haydock_VEC_COMMs('reset')
     endif
     !
     call SlepcFinalize(ierr)
   end subroutine K_slepc_local_free
#endif
   !
end subroutine
