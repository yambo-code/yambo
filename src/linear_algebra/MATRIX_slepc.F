!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): HM DS
!
! headers
!
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscpc.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
!
module ContextSTShell
#include <slepc/finclude/slepceps.h>
  use slepceps
  implicit none

  Mat :: S,A_factor
end module
!
subroutine MATRIX_slepc(M_slepc,l_target_energy,n_eig,V_right,V_left,E_real,E_cmpl)
 !
 ! 10/06/2016 HM
 ! Use slepc to obtain the first few eigenpairs of a large matrix
 ! This routine can be called from within Matrix_driver.F but should be kept in
 ! a separate source file for clarity.
 !
 use pars,          ONLY : SP,cI
 use units,         ONLY : HA2EV
 use com,           ONLY : msg
 use stderr,        ONLY : intc
 use BS_solvers,    ONLY : BSS_slepc_extraction,BSS_slepc_ncv,BSS_slepc_maxit,&
 &                         BSS_slepc_tol,BSS_slepc_target_E,BSS_slepc_precondition,&
 &                         BSS_slepc_approach
 !
 use petscsys
 use petscmat
 use petscsysdef
 use petscmatdef
 !
 use slepcsys
 use slepceps
 use slepcsysdef
 use slepcepsdef
 !
 implicit none
 !
 ! arguments
 !
 Mat                         :: M_slepc
 logical                     :: l_target_energy
 integer                     :: n_eig         ! number of eigenvalues to compute
 complex(SP)                 :: V_right(:,:)  ! right eigenvalues
 complex(SP),  optional      :: V_left(:,:)   ! left eigenvalues
 complex(SP),  optional      :: E_cmpl(:)     ! complex eigenvalues
 real(SP),     optional      :: E_real(:)     ! real eigenvalues
 ! 
 ! internal variables
 !
 ! slepc
 !
 external :: MyEPSMonitor !function to monitor the convergence
 external :: STSetUp_User,STApply_User,STBackTransform_User,STDestroy_User
 ! 
 EPS                                :: eps
 ST                                 :: st    ! spectral transformation context
 KSP                                :: ksp
 PC                                 :: pc
 !
 EPSType                            :: epskind
 STType                             :: stkind
 KSPType                            :: kspkind
 PCType                             :: pckind
 !
 EPSExtraction                      :: extr
 PetscReal                          :: tol, ferror
 PetscScalar                        :: target_energy
 PetscErrorCode                     :: ierr
 PetscInt                           :: nev, ncv, mpd, maxit, its, nconv, n, i, j
 PetscInt                           :: idx(2)
 PetscScalar                        :: kr, ki
 PetscScalar, pointer               :: xsr(:), xsi(:), xsr_left(:), xsi_left(:)
 PetscScalar, pointer               :: M(:,:)          !pointer to matrix
 Vec                                :: xr, xi, xr_left, xi_left
 Vec, dimension (:), allocatable    :: voutr, vouti, voutr_left, vouti_left
                                    !These are local varibles and are destroyed after used
 PetscViewer                        :: viewer, hdf5v
 PetscMPIInt                        :: rank
 VecScatter, dimension (:), allocatable     ::  ctxr, ctxi, ctxr_left, ctxi_left
                                    !These are local varibles and are destroyed after used
 !
 logical           :: l_precondition
 character(len=30) :: rowfmt
 !
 ! Non hermitian not implemented yet!
 !
 PetscCallA(MatGetSize(M_slepc,n,j,ierr))
 call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
 !
 PetscCallA(MatCreateVecs(M_slepc,xr,xr_left,ierr))
 !
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Create the eigensolver and display info
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !
 !Create eigensolver context
 PetscCallA(EPSCreate(PETSC_COMM_WORLD,eps,ierr))
 !
 !Set operators. In this case, it is a standard eigenvalue problem
 !
 PetscCallA(EPSSetOperators(eps,M_slepc,PETSC_NULL_MAT,ierr))
 if(     present(V_left)) then
   ! To do: to check if it can be cast in the form of a generalized hermitian problem
   PetscCallA(EPSSetProblemType(eps,EPS_NHEP,ierr))
   PetscCallA(EPSSetFromOptions(eps,ierr))
   PetscCallA(EPSSetTwoSided(eps,PETSC_TRUE,ierr))
 else
   PetscCallA(EPSSetProblemType(eps,EPS_HEP,ierr))
 endif
 !
 ! See end of file for options
 !
 ! * Krylov subspaces: EPSKRYLOVSCHUR, STPPRECOND not accepted,
 !                        STSINVERT + KSPPREONLY+ PCJACOBI gives wrong eigenvalues. Too strong (?)
 !                                  +  KSPBCGS  + PCJACOBI works fine
 !
 ! * Generalized-Davidson: EPSGD, RIGHT VECTORS ONLY
 !                       STPPRECOND + KSPPREONLY + PCJACOBY very fast for few eigenvectors
 !
 ! * Jacob-Davidson: EPSJD, NOT WORKING WITH KSP PREONLY
 !                    STPPRECOND + KSPCG SUPER-SLOW + PCJACOBY super slow
 !
 ! Default
 epskind=EPSKRYLOVSCHUR
 ! From user
 if(BSS_slepc_approach=="Krylov-Schur")          epskind=EPSKRYLOVSCHUR
 if(BSS_slepc_approach=="KS-sinvert") then
   epskind=EPSKRYLOVSCHUR
   PetscCallA(EPSGetST(eps,st,ierr))
   stkind=STSHELL
   PetscCallA(STSetType(st,stkind,ierr))
   PetscCallA(STShellSetApply(st,STApply_User,ierr))
   PetscCallA(STShellSetBackTransform(st,STBackTransform_User,ierr))
   PetscCallA(PetscObjectSetName(st,"STShellTransformation",ierr))
   PetscCallA(STSetUp_User(st,ierr))
 endif
 if(BSS_slepc_approach=="Generalized-Davidson")  epskind=EPSGD
 if(BSS_slepc_approach=="Jacob-Davidson")        epskind=EPSJD
 !if(BSS_slepc_approach=="Scalapack")             epskind=EPSSCALAPACK
 !if(BSS_slepc_approach=="ELPA")                  epskind=EPSELPA
 ! In case input is mispelled fallback to Krylov-Schur
 if (epskind==EPSKRYLOVSCHUR) BSS_slepc_approach="Krylov-Schur"
 !
 call msg( 'sr', '[SLEPC] Approach                          ',BSS_slepc_approach)
 !
 PetscCallA(EPSSetType(eps,epskind,ierr))
 !
 l_precondition=BSS_slepc_precondition/="none"
 !
 if (l_precondition) then
   !
   if (epskind==EPSKRYLOVSCHUR) stkind=STSINVERT
   if (epskind==EPSGD)          stkind=STPRECOND
   if (epskind==EPSJD)          stkind=STPRECOND
   !
   ! Default
   if (stkind==STSINVERT)                           kspkind=KSPBCGS
   if (stkind==STPRECOND .and. epskind==EPSGD)      kspkind=KSPPREONLY
   if (stkind==STPRECOND .and. epskind==EPSJD)      kspkind=KSPBCGS
   ! From user
   if(index(BSS_slepc_precondition,KSPPREONLY)/=0)  kspkind=KSPPREONLY
   if(index(BSS_slepc_precondition,KSPBCGS)/=0)     kspkind=KSPBCGS
   !
   ! Default
   pckind=PCJACOBI
   ! From user
   if(index(BSS_slepc_precondition,PCJACOBI)/=0)    pckind=PCJACOBI
   !
   BSS_slepc_precondition=trim(kspkind)//"+"//trim(pckind)
   !
   call msg( 'sr', '[SLEPC] Precondition method               ',BSS_slepc_precondition)
   !
   PetscCallA(EPSGetST(eps,st,ierr))
   PetscCallA(STGetKSP(st,ksp,ierr))
   PetscCallA(KSPGetPC(ksp,pc,ierr))
   !
   PetscCallA(STSetType(st,stkind,ierr))
   PetscCallA(KSPSetType(ksp,kspkind,ierr))
   PetscCallA(PCSetType(pc,pckind,ierr))
   !
 endif
 !
 call msg( 'sr', '[SLEPC] Extraction method                 ',BSS_slepc_extraction)
 !
 if (BSS_slepc_extraction == 'ritz')              extr = EPS_RITZ
 if (BSS_slepc_extraction == 'harmonic')          extr = EPS_HARMONIC         
 if (BSS_slepc_extraction == 'harmonic_relative') extr = EPS_HARMONIC_RELATIVE
 if (BSS_slepc_extraction == 'harmonic_right')    extr = EPS_HARMONIC_RIGHT
 if (BSS_slepc_extraction == 'harmonic_largest')  extr = EPS_HARMONIC_LARGEST
 if (BSS_slepc_extraction == 'refined')           extr = EPS_REFINED
 if (BSS_slepc_extraction == 'refined_harmonic')  extr = EPS_REFINED_HARMONIC
 !
 PetscCallA(EPSSetExtraction(eps, extr, ierr))
 !
 call msg( 'sr', '[SLEPC] Number of requested eigenvalues   ', n_eig ) 
 !
 if (l_target_energy.or.l_precondition) then
   target_energy=0._SP
   if(l_target_energy) target_energy=BSS_slepc_target_E
   call msg( 'nsr', '[SLEPC] Criterion is target energy        ', real(target_energy,SP)*HA2EV,'[eV]')
   PetscCallA(EPSSetTarget(eps,target_energy,ierr))
   PetscCallA(EPSSetWhichEigenpairs(eps,EPS_TARGET_REAL,ierr))
 else
   call msg( 'nsr', '[SLEPC] Criterion is smaller eigenvalues')
   PetscCallA(EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr))
 endif
 !
 ! Set solver parameters at runtime
 !
 nev = n_eig
 if ( BSS_slepc_ncv/=0 ) ncv = BSS_slepc_ncv
 if ( BSS_slepc_ncv==0 ) ncv = PETSC_DECIDE
 !
 PetscCallA(EPSSetDimensions(eps,nev,ncv,PETSC_DECIDE,ierr))
 !
 if ( BSS_slepc_maxit/=0 ) maxit = BSS_slepc_maxit
 if ( BSS_slepc_maxit==0 ) maxit = PETSC_DECIDE
 !
 tol      = BSS_slepc_tol
 PetscCallA(EPSSetTolerances(eps,tol,maxit, ierr))
 !
 !
 !free the M_slepc matrix
 PetscCallA(MatDestroy(M_slepc,ierr))
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Optional: Get some information from the solver and display it
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !PetscCallA(EPSGetType(eps,tname,ierr))
 PetscCallA(EPSGetTolerances(eps,tol,maxit,ierr))
 !
 call msg( 'sr', '[SLEPC] Stopping condition tolerance      ', real(tol,SP) )
 call msg( 'sr', '[SLEPC] Stopping condition max iterations ', int(maxit) )

 !Set monitor
 PetscCallA(EPSMonitorSet(eps,MyEPSMonitor,0,PETSC_NULL_FUNCTION,ierr))
 !

 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Solve the eigensystem
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 PetscCallA(EPSSolve(eps,ierr))
 PetscCallA(EPSGetIterationNumber(eps,its,ierr))
 call msg( 'sr', '[SLEPC] Number of iterations              ', int(its) )
 PetscCallA(EPSGetDimensions(eps,nev,ncv,mpd,ierr))
 call msg( 'sr', '[SLEPC] Number of eigenvalues        [NEV]', int(nev) )
 call msg( 'sr', '[SLEPC] Max. subspace size of solver [NCV]', int(ncv) )
 call msg( 'sr', '[SLEPC] Max. allowed dim             [MPD]', int(mpd) )

 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Display solution and clean up
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !
 PetscCallA(EPSGetConverged(eps,nconv,ierr))
 call msg( 'srn', '[SLEPC] Number of converged states        ', int(nconv))
 if ( nconv < n_eig ) then
   call warning(' [SLEPC] requested '//trim(intc(n_eig))//' but converged '//trim(intc(int(nconv,4)))//' eigenvalues')
   n_eig = nconv
 endif
 !
 if (n_eig==0) call error(' [SLEPC] 0 eigenvectors converged')
 !
 !
 !
 !open file for output
 !call PetscViewerHDF5Open(PETSC_COMM_WORLD, 'hdb.BS_slepc', FILE_MODE_WRITE, hdf5v, ierr)
 !
 !calculate eigenvalues and relative errors
 !
 allocate(voutr(n_eig),ctxr(n_eig)) ! Allocate n_eig structs

 if (present(V_left)) allocate(ctxr_left(n_eig),voutr_left(n_eig))

 do i=0,n_eig-1
   !Get converged eigenpairs: i-th eigenvalue is stored in kr
   !(real part) and ki (imaginary part)
   if(present(V_left)) then
     !PetscCallA(EPSGetEigenpair(eps,i,kr,ki,xr,xi,ierr))
     PetscCallA(EPSGetEigenpair(eps,i,kr,PETSC_NULL_SCALAR,xr,PETSC_NULL_VEC,ierr))
     !PetscCallA(EPSGetLeftEigenvector(eps,i,xr_left,xi_left,ierr))
     PetscCallA(EPSGetLeftEigenvector(eps,i,xr_left,PETSC_NULL_VEC,ierr))
   else
     PetscCallA(EPSGetEigenpair(eps,i,kr,PETSC_NULL_SCALAR,xr,PETSC_NULL_VEC,ierr))
   endif
   !
   !save the eigenvalues and eigenvectors in the matrix of the hamiltonian
   if(present(E_real)) E_real(i+1) = real(kr,SP)
   if(present(E_cmpl)) E_cmpl(i+1) = cmplx(kr,kind=SP)
   !
   ! this is to write the vector to hdf5 directly
   !PetscCallA(VecView(xr,hdf5v,ierr))
   !
   if (present(V_left)) then
     !save the eigenvectors
     PetscCallA(VecScatterCreateToAll(xr,ctxr(i+1),voutr(i+1),ierr))
     !PetscCallA(VecScatterCreateToAll(xi,ctxi,vouti,ierr))
     ! scatter as many times as you need
     PetscCallA(VecScatterBegin(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr))
     PetscCallA(VecScatterEnd(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr))
     !PetscCallA(VecScatterBegin(ctxi,xi,vouti,INSERT_VALUES,SCATTER_FORWARD,ierr))
     !PetscCallA(VecScatterEnd(ctxi,xi,vouti,INSERT_VALUES,SCATTER_FORWARD,ierr))
     !
     PetscCallA(VecGetArrayReadF90(voutr(i+1),xsr,ierr))
     !PetscCallA(VecGetArrayReadF90(vouti,xsi,ierr))
     V_right(:, i+1) = cmplx(xsr,kind=SP)
     !if (BSS_slepc_double_grp) V_right(BS_K_dim(1)+1:,i+1) =cI*V_right(BS_K_dim(1)+1:,i+1)
     PetscCallA(VecRestoreArrayReadF90(voutr(i+1),xsr,ierr))
     !PetscCallA(VecRestoreArrayReadF90(vouti,xsi,ierr))
     !
     PetscCallA(VecScatterDestroy(ctxr(i+1),ierr))
     PetscCallA(VecDestroy(voutr(i+1),ierr))
     !PetscCallA(VecScatterDestroy(ctxi,ierr))
     !PetscCallA(VecDestroy(vouti,ierr))
     !
     !save the eigenvectors
     PetscCallA(VecScatterCreateToAll(xr_left,ctxr_left(i+1),voutr_left(i+1),ierr))
     !PetscCallA(VecScatterCreateToAll(xi_left,ctxi_left,vouti_left,ierr))
     ! scatter as many times as you need
     PetscCallA(VecScatterBegin(ctxr_left(i+1),xr_left,voutr_left(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr))
     PetscCallA(VecScatterEnd(ctxr_left(i+1),xr_left,voutr_left(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr))
     !PetscCallA(VecScatterBegin(ctxi_left,xi_left,vouti_left,INSERT_VALUES,SCATTER_FORWARD,ierr))
     !PetscCallA(VecScatterEnd(ctxi_left,xi_left,vouti_left,INSERT_VALUES,SCATTER_FORWARD,ierr))
     !
     PetscCallA(VecGetArrayReadF90(voutr_left(i+1),xsr_left,ierr))
     !PetscCallA(VecGetArrayReadF90(vouti_left,xsi_left,ierr))
     V_left(:, i+1) = cmplx(xsr_left,kind=SP)
     !if (BSS_slepc_double_grp) V_left(BS_K_dim(1)+1:,i+1) =cI*V_left(BS_K_dim(1)+1:,i+1)
     PetscCallA(VecRestoreArrayReadF90(voutr_left(i+1),xsr_left,ierr))
     !PetscCallA(VecRestoreArrayReadF90(vouti_left,xsi_left,ierr))
     !
     !
     PetscCallA(VecScatterDestroy(ctxr_left(i+1),ierr))
     PetscCallA(VecDestroy(voutr_left(i+1),ierr))
     !PetscCallA(VecScatterDestroy(ctxi_left,ierr))
     !PetscCallA(VecDestroy(vouti_left,ierr))
   else
     !save the eigenvectors
     PetscCallA(VecScatterCreateToAll(xr,ctxr(i+1),voutr(i+1),ierr))
     ! scatter as many times as you need
     PetscCallA(VecScatterBegin(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr))
     PetscCallA(VecScatterEnd(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr))
     !
     PetscCallA(VecGetArrayReadF90(voutr(i+1),xsr,ierr))
     V_right(:, i+1) = xsr
     PetscCallA(VecRestoreArrayReadF90(voutr(i+1),xsr,ierr))
     PetscCallA(VecScatterDestroy(ctxr(i+1),ierr))
     PetscCallA(VecDestroy(voutr(i+1),ierr))
   endif
   !
 enddo
 !
 deallocate(voutr,ctxr)
 !
 if (present(V_left)) deallocate(ctxr_left,voutr_left)
 !
 ! destroy scatter context and local vector when no longer needed
 !
 !
 call EPSDestroy(eps,ierr)
 call VecDestroy(xr,ierr)
 call VecDestroy(xr_left,ierr)
 !
end subroutine
!
subroutine STSetUp_User(st,ierr)
#include <slepc/finclude/slepceps.h>
  use slepceps
  use ContextSTShell
  implicit none
  !
  ST                    :: st
  Mat                   :: M,A,C,B,D,W
  Mat, dimension(2,2)   :: M_submats
  IS, dimension(2)      :: rows
  PetscScalar           :: one,mone
  PetscReal             :: info(MAT_FACTORINFO_SIZE)
  PetscErrorCode        :: ierr
  !
  PetscCall(STGetMatrix(st,0,M,ierr))
  PetscCall(MatNestGetSubMats(M, PETSC_NULL_INTEGER, PETSC_NULL_INTEGER, M_submats,ierr))
  A = M_submats(1,1)
  B = M_submats(1,2)
  C = M_submats(2,1)
  D = M_submats(2,2)
  !
  PetscCall(MatDuplicate(A,MAT_COPY_VALUES,A_factor,ierr))
  PetscCall(MatFactorInfoInitialize(info,ierr))
  PetscCall(MatCholeskyFactor(A_factor,PETSC_NULL_IS,info,ierr))
  PetscCall(MatDuplicate(B,MAT_DO_NOT_COPY_VALUES,W,ierr))
  PetscCall(MatMatSolve(A_factor,B,W,ierr))
  !PetscCall(MatMatMult(C,W,MAT_INITIAL_MATRIX,PETSC_DEFAULT,S,ierr)) !No support Scalapack -> MatProduct?
  PetscCall(MatProductCreate(C,W,PETSC_NULL_MAT,S,ierr))
  PetscCall(MatProductSetType(S,MATPRODUCT_AtB,ierr))
  !PetscCall(MatProductSetFromOptions(S,ierr))
  PetscCall(MatProductSymbolic(S,ierr))
  PetscCall(MatProductNumeric(S,ierr))
  mone = -1
  PetscCall(MatScale(S,mone,ierr))
  one = 1
  PetscCall(MatAXPY(S,one,D,SAME_NONZERO_PATTERN,ierr))
  PetscCall(MatCholeskyFactor(S,PETSC_NULL_IS,info,ierr))
  !
  PetscCall(MatDestroy(W,ierr))
  !
  ierr=0
  return
end subroutine
!
subroutine STApply_User(st,vin,vout,ierr)
#include <slepc/finclude/slepceps.h>
  use slepceps
  use ContextSTShell
  implicit none

  Mat                :: M,A,C,B,D
  Mat,dimension(2,2) :: M_submats
  Vec                :: f,g,x,y,y_tilde,w,vin,vout
  ST                 :: st
  IS,dimension(2)    :: rows
  PetscScalar        :: mone
  PetscErrorCode     :: ierr

  PetscCall(STGetMatrix(st,0,M,ierr))
  PetscCall(MatNestGetSubMats(M,PETSC_NULL_INTEGER,PETSC_NULL_INTEGER,M_submats,ierr))
  A = M_submats(1,1)
  B = M_submats(1,2)
  C = M_submats(2,1)
  D = M_submats(2,2)

  ! Solve
  PetscCall(MatNestGetISs(M,rows,PETSC_NULL_INTEGER,ierr))
  PetscCall(VecGetSubVector(vin,rows(1),f,ierr))
  PetscCall(VecGetSubVector(vin,rows(2),g,ierr))
  PetscCall(VecGetSubVector(vout,rows(1),x,ierr))
  PetscCall(VecGetSubVector(vout,rows(2),y,ierr))

  ! 1
  PetscCall(VecDuplicate(y,y_tilde,ierr))
  PetscCall(VecDuplicate(y,w,ierr))

  PetscCall(MatSolve(A_factor,f,w,ierr))
  mone = -1
  PetscCall(VecScale(w,mone,ierr))
  PetscCall(MatMultAdd(C,w,g,y_tilde,ierr))
  PetscCall(MatSolve(S,y_tilde,y,ierr))

  ! 2
  PetscCall(VecCopy(y,y_tilde,ierr))
  PetscCall(VecScale(y_tilde,mone,ierr))
  PetscCall(MatMultAdd(B,y_tilde,f,w,ierr))
  PetscCall(MatSolve(A_factor,w,x,ierr))

  PetscCall(VecDestroy(f,ierr))
  PetscCall(VecDestroy(g,ierr))
  PetscCall(VecDestroy(x,ierr))
  PetscCall(VecDestroy(y,ierr))
  PetscCall(VecDestroy(w,ierr))
  PetscCall(VecDestroy(y_tilde,ierr))

  ierr=0
  return
end subroutine
!
subroutine STBackTransform_User(st,n,eigr,eigi,ierr)
#include <slepc/finclude/slepceps.h>
  use slepceps
  use ContextSTShell

  ST                       :: st
  PetscInt                 :: n
  PetscScalar,dimension(n) :: eigr,eigi
  PetscErrorCode           :: ierr
  PetscInt                 :: j

  do j=1,n,1 
    eigr(j) = 1.0 / eigr(j)
  end do

  ierr=0
  return
end subroutine
!
subroutine STDestroy_User(ierr)
#include <slepc/finclude/slepceps.h>
  use slepceps
  use ContextSTShell

  PetscCall(MatDestroy(A_factor,ierr))
  PetscCall(MatDestroy(S,ierr))

  ierr=0
  return
end subroutine
!
subroutine MyEPSMonitor(eps,its,nconv,eigr,eigi,errest,nest,dummy,ierr)
 ! 
 ! Monitor the convergence of the iterative diagonalization
 ! 
 use pars,          ONLY:SP
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_H_dim
 !
 use petscsys
 use petscmat
 use petscsysdef
 use petscmatdef
 !
 use slepceps
 use slepcepsdef
 !
 implicit none
 !
 EPS            :: eps
 Vec            :: x
 PetscErrorCode :: ierr
 PetscInt       :: its,nconv,nest,dummy
 PetscScalar    :: eigr(*),eigi(*)
 PetscReal      :: re,errest(*)
 PetscMPIInt    :: rank
 !
 real(SP)       :: maxerror(2)
 !
 if(nconv==0) then
   maxerror=real(errest(nconv+1:nconv+2),SP)
 else
   if(nconv< BS_H_dim) maxerror=real(errest(nconv:nconv+1),SP)
   if(nconv==BS_H_dim) maxerror=real(errest(nconv-1:nconv),SP)
 endif
 !
 ! write the number of converged eigenvalues
 call msg('s', '[SLEPC] Iteration #'// trim(intc(int(its,4)))// ' - converged States '&
&           // trim(intc(int(nconv,4))) //' - error ', maxerror)
 !
 !Slepc_v%it=0 !Slepc_v%it+1 !its
 !
 ! DEBUG <
 !if (nconv>0) then
 !  write(*,*) eigr(:nconv)*HA2EV
 !  write(*,*) " "
 !  write(*,*) errest(:nconv)
 !endif
 ! DEBUG >
 !
 ierr = 0
 !
end subroutine
!
!
 !
 ! eps type
 !============
 !EPSPOWER       "power"
 !EPSSUBSPACE    "subspace"
 !EPSARNOLDI     "arnoldi"
 !EPSLANCZOS     "lanczos"         ! RIGHT VECTORS ONLY
 !EPSKRYLOVSCHUR "krylovschur"     ! NO STPPRECOND, WITH STSINVERT+KSPPREONLY + PCJACOBY gives wrong eigenvalues. Too strong (?)
                                   !                     STSINVERT+KSPBCGS + PCHYPRE (+ BOOMERANG)
 !EPSGD          "gd"              ! RIGHT VECTORS ONLY, PREONLY, VERY FAST FOR FEW EIGEN
 !EPSJD          "jd"              ! NOT WORKING WITH KSP PREONLY, WITH KSPCG SUPE-SLOW
 !EPSRQCG        "rqcg"            ! HERMITIAN ONLY
 !EPSLOBPCG      "lobpcg"          ! HERMITIAN ONLY
 !EPSCISS        "ciss"
 !EPSLYAPII      "lyapii"
 !EPSLAPACK      "lapack"
 !EPSARPACK      "arpack"
 !EPSBLZPACK     "blzpack"
 !EPSTRLAN       "trlan"
 !EPSBLOPEX      "blopex"
 !EPSPRIMME      "primme"
 !EPSFEAST       "feast"
 !EPSSCALAPACK   "scalapack"
 !EPSELPA        "elpa"
 !EPSELEMENTAL   "elemental"
 !
 !
 ! st type
 !===========
 ! STSHELL     "shell"
 ! STSHIFT     "shift"
 ! STSINVERT   "sinvert"
 ! STCAYLEY    "cayley"
 ! STPRECOND   "precond"
 ! STFILTER    "filter"
 !
 !
 ! ksp type
 !============
 !KSPRICHARDSON "richardson"
 !KSPCHEBYSHEV  "chebyshev"
 !KSPCG         "cg"
 !KSPGROPPCG    "groppcg"
 !KSPPIPECG     "pipecg"
 !KSPPIPECGRR   "pipecgrr"
 !KSPPIPELCG     "pipelcg"
 !KSPPIPEPRCG    "pipeprcg"
 !KSPPIPECG2     "pipecg2"
 !  KSPCGNE       "cgne"
 !  KSPNASH       "nash"
 !  KSPSTCG       "stcg"
 !  KSPGLTR       "gltr"
 !    KSPCGNASH  PETSC_DEPRECATED_MACRO("GCC warning \"KSPCGNASH macro is deprecated use KSPNASH (since version 3.11)\"")  "nash"
 !    KSPCGSTCG  PETSC_DEPRECATED_MACRO("GCC warning \"KSPCGSTCG macro is deprecated use KSPSTCG (since version 3.11)\"")  "stcg"
 !    KSPCGGLTR  PETSC_DEPRECATED_MACRO("GCC warning \"KSPCGGLTR macro is deprecated use KSPSGLTR (since version 3.11)\"") "gltr"
 !KSPFCG        "fcg"
 !KSPPIPEFCG    "pipefcg"
 !KSPGMRES      "gmres"
 !KSPPIPEFGMRES "pipefgmres"
 !  KSPFGMRES     "fgmres"
 !  KSPLGMRES     "lgmres"
 !  KSPDGMRES     "dgmres"
 !  KSPPGMRES     "pgmres"
 !KSPTCQMR      "tcqmr"
 !KSPBCGS       "bcgs"
 !  KSPIBCGS      "ibcgs"
 !  KSPFBCGS      "fbcgs"
 !  KSPFBCGSR     "fbcgsr"
 !  KSPBCGSL      "bcgsl"
 !  KSPPIPEBCGS   "pipebcgs"
 !KSPCGS        "cgs"
 !KSPTFQMR      "tfqmr"
 !KSPCR         "cr"
 !KSPPIPECR     "pipecr"
 !KSPLSQR       "lsqr"
 !KSPPREONLY    "preonly"
 !KSPQCG        "qcg"
 !KSPBICG       "bicg"
 !KSPMINRES     "minres"
 !KSPSYMMLQ     "symmlq"
 !KSPLCD        "lcd"
 !KSPPYTHON     "python"
 !KSPGCR        "gcr"
 !KSPPIPEGCR    "pipegcr"
 !KSPTSIRM      "tsirm"
 !KSPCGLS       "cgls"
 !KSPFETIDP     "fetidp"
 !KSPHPDDM      "hpddm"
 !
 !
 ! pc type
 !============
 !PCNONE            "none"
 !PCJACOBI          "jacobi"
 !PCSOR             "sor"
 !PCLU              "lu"
 !PCSHELL           "shell"
 !PCBJACOBI         "bjacobi"
 !PCMG              "mg"
 !PCEISENSTAT       "eisenstat"
 !PCILU             "ilu"
 !PCICC             "icc"
 !PCASM             "asm"
 !PCGASM            "gasm"
 !PCKSP             "ksp"
 !PCCOMPOSITE       "composite"
 !PCREDUNDANT       "redundant"
 !PCSPAI            "spai"
 !PCNN              "nn"
 !PCCHOLESKY        "cholesky"
 !PCPBJACOBI        "pbjacobi"
 !PCVPBJACOBI       "vpbjacobi"
 !PCMAT             "mat"
 !PCHYPRE           "hypre"
 !PCPARMS           "parms"
 !PCFIELDSPLIT      "fieldsplit"
 !PCTFS             "tfs"
 !PCML              "ml"
 !PCGALERKIN        "galerkin"
 !PCEXOTIC          "exotic"
 !PCCP              "cp"
 !PCBFBT            "bfbt"
 !PCLSC             "lsc"
 !PCPYTHON          "python"
 !PCPFMG            "pfmg"
 !PCSYSPFMG         "syspfmg"
 !PCREDISTRIBUTE    "redistribute"
 !PCSVD             "svd"
 !PCGAMG            "gamg"
 !PCCHOWILUVIENNACL "chowiluviennacl"
 !PCROWSCALINGVIENNACL "rowscalingviennacl"
 !PCSAVIENNACL      "saviennacl"
 !PCBDDC            "bddc"
 !PCKACZMARZ        "kaczmarz"
 !PCTELESCOPE       "telescope"
 !PCPATCH           "patch"
 !PCLMVM            "lmvm"
 !PCHMG             "hmg"
 !PCDEFLATION       "deflation"
 !PCHPDDM           "hpddm"
 !PCHARA            "hara"
