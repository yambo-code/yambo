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
 &                         BSS_slepc_approach,BSS_slepc_matrix
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
 call MatGetSize(M_slepc,n,j,ierr)
 call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
 !
 call MatCreateVecs(M_slepc,xr,xr_left,ierr)
 !
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Create the eigensolver and display info
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !
 !Create eigensolver context
 call EPSCreate(PETSC_COMM_WORLD,eps,ierr)
 !
 !Set operators. In this case, it is a standard eigenvalue problem
 !
 call EPSSetOperators(eps,M_slepc,PETSC_NULL_MAT,ierr) 
 if(     present(V_left)) then
   ! To do: to check if it can be cast in the form of a generalized hermitian problem
   call EPSSetProblemType(eps,EPS_NHEP,ierr)
   call EPSSetFromOptions(eps,ierr)
   call EPSSetTwoSided(eps,PETSC_TRUE,ierr)
 else
   call EPSSetProblemType(eps,EPS_HEP,ierr)
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
 if(BSS_slepc_approach=="Generalized-Davidson")  epskind=EPSGD
 if(BSS_slepc_approach=="Jacob-Davidson")        epskind=EPSJD
 !if(BSS_slepc_approach=="Scalapack")             epskind=EPSSCALAPACK
 !if(BSS_slepc_approach=="ELPA")                  epskind=EPSELPA
 ! In case input is mispelled fallback to Krylov-Schur
 if (epskind==EPSKRYLOVSCHUR) BSS_slepc_approach="Krylov-Schur"
 !
 call msg( 'sr', '[SLEPC] Approach                          ',BSS_slepc_approach)
 !
 call EPSSetType(eps,epskind,ierr)
 !
 l_precondition=BSS_slepc_precondition/="none"
 !
 if (l_precondition.and. .not.BSS_slepc_matrix) then
   !
   call warning("Preconditioning not compatible with shell matrix, disabled")
   BSS_slepc_precondition="none"
   l_precondition=.false.
   !
 endif
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
   call EPSGetST(eps,st,ierr)
   call STGetKSP(st,ksp,ierr)
   call KSPGetPC(ksp,pc,ierr)
   !
   call STSetType(st,stkind,ierr)
   call KSPSetType(ksp,kspkind,ierr)
   call PCSetType(pc,pckind,ierr)
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
 call EPSSetExtraction(eps, extr, ierr)
 !
 call msg( 'sr', '[SLEPC] Number of requested eigenvalues   ', n_eig ) 
 !
 if (l_target_energy.or.l_precondition) then
   target_energy=0._SP
   if(l_target_energy) target_energy=BSS_slepc_target_E
   call msg( 'nsr', '[SLEPC] Criterion is target energy        ', real(target_energy,SP)*HA2EV,'[eV]')
   call EPSSetTarget(eps,target_energy,ierr)
   call EPSSetWhichEigenpairs(eps,EPS_TARGET_REAL,ierr)
 else
   call msg( 'nsr', '[SLEPC] Criterion is smaller eigenvalues')
   call EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr)
 endif
 !
 ! Set solver parameters at runtime
 !
 nev = n_eig
 if ( BSS_slepc_ncv/=0 ) ncv = BSS_slepc_ncv
 if ( BSS_slepc_ncv==0 ) ncv = PETSC_DECIDE
 !
 call EPSSetDimensions(eps,nev,ncv,PETSC_DECIDE,ierr)
 !
 if ( BSS_slepc_maxit/=0 ) maxit = BSS_slepc_maxit
 if ( BSS_slepc_maxit==0 ) maxit = PETSC_DECIDE
 !
 tol      = BSS_slepc_tol
 call EPSSetTolerances(eps,tol,maxit, ierr)
 !
 !
 !free the M_slepc matrix
 call MatDestroy(M_slepc,ierr)
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Optional: Get some information from the solver and display it
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !call EPSGetType(eps,tname,ierr)
 call EPSGetTolerances(eps,tol,maxit,ierr)
 !
 call msg( 'sr', '[SLEPC] Stopping condition tolerance      ', real(tol,SP) )
 call msg( 'sr', '[SLEPC] Stopping condition max iterations ', int(maxit) )

 !Set monitor
 call EPSMonitorSet(eps,MyEPSMonitor,0,PETSC_NULL_FUNCTION,ierr)
 !

 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Solve the eigensystem
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 call EPSSolve(eps,ierr)
 call EPSGetIterationNumber(eps,its,ierr)
 call msg( 'sr', '[SLEPC] Number of iterations              ', int(its) )
 call EPSGetDimensions(eps,nev,ncv,mpd,ierr)
 call msg( 'sr', '[SLEPC] Number of eigenvalues        [NEV]', int(nev) )
 call msg( 'sr', '[SLEPC] Max. subspace size of solver [NCV]', int(ncv) )
 call msg( 'sr', '[SLEPC] Max. allowed dim             [MPD]', int(mpd) )

 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Display solution and clean up
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !
 call EPSGetConverged(eps,nconv,ierr)
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
     !call EPSGetEigenpair(eps,i,kr,ki,xr,xi,ierr)
     call EPSGetEigenpair(eps,i,kr,PETSC_NULL_SCALAR,xr,PETSC_NULL_VEC,ierr)
     !call EPSGetLeftEigenvector(eps,i,xr_left,xi_left,ierr)
     call EPSGetLeftEigenvector(eps,i,xr_left,PETSC_NULL_VEC,ierr)
   else
     call EPSGetEigenpair(eps,i,kr,PETSC_NULL_SCALAR,xr,PETSC_NULL_VEC,ierr)
   endif
   !
   !save the eigenvalues and eigenvectors in the matrix of the hamiltonian
   if(present(E_real)) E_real(i+1) = real(kr,SP)
   if(present(E_cmpl)) E_cmpl(i+1) = cmplx(kr,kind=SP)
   !
   ! this is to write the vector to hdf5 directly
   !call VecView(xr,hdf5v,ierr)
   !
   if (present(V_left)) then
     !save the eigenvectors
     call VecScatterCreateToAll(xr,ctxr(i+1),voutr(i+1),ierr)
     !call VecScatterCreateToAll(xi,ctxi,vouti,ierr)
     ! scatter as many times as you need
     call VecScatterBegin(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterEnd(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr)
     !call VecScatterBegin(ctxi,xi,vouti,INSERT_VALUES,SCATTER_FORWARD,ierr)
     !call VecScatterEnd(ctxi,xi,vouti,INSERT_VALUES,SCATTER_FORWARD,ierr)
     !
     call VecGetArrayReadF90(voutr(i+1),xsr,ierr)
     !call VecGetArrayReadF90(vouti,xsi,ierr)
     V_right(:, i+1) = cmplx(xsr,kind=SP)
     !if (BSS_slepc_double_grp) V_right(BS_K_dim(1)+1:,i+1) =cI*V_right(BS_K_dim(1)+1:,i+1)
     call VecRestoreArrayReadF90(voutr(i+1),xsr,ierr)
     !call VecRestoreArrayReadF90(vouti,xsi,ierr)
     !
     call VecScatterDestroy(ctxr(i+1),ierr)
     call VecDestroy(voutr(i+1),ierr)
     !call VecScatterDestroy(ctxi,ierr)
     !call VecDestroy(vouti,ierr)
     !
     !save the eigenvectors
     call VecScatterCreateToAll(xr_left,ctxr_left(i+1),voutr_left(i+1),ierr)
     !call VecScatterCreateToAll(xi_left,ctxi_left,vouti_left,ierr)
     ! scatter as many times as you need
     call VecScatterBegin(ctxr_left(i+1),xr_left,voutr_left(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterEnd(ctxr_left(i+1),xr_left,voutr_left(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr)
     !call VecScatterBegin(ctxi_left,xi_left,vouti_left,INSERT_VALUES,SCATTER_FORWARD,ierr)
     !call VecScatterEnd(ctxi_left,xi_left,vouti_left,INSERT_VALUES,SCATTER_FORWARD,ierr)
     !
     call VecGetArrayReadF90(voutr_left(i+1),xsr_left,ierr)
     !call VecGetArrayReadF90(vouti_left,xsi_left,ierr)
     V_left(:, i+1) = cmplx(xsr_left,kind=SP)
     !if (BSS_slepc_double_grp) V_left(BS_K_dim(1)+1:,i+1) =cI*V_left(BS_K_dim(1)+1:,i+1)
     call VecRestoreArrayReadF90(voutr_left(i+1),xsr_left,ierr)
     !call VecRestoreArrayReadF90(vouti_left,xsi_left,ierr)
     !
     !
     call VecScatterDestroy(ctxr_left(i+1),ierr)
     call VecDestroy(voutr_left(i+1),ierr)
     !call VecScatterDestroy(ctxi_left,ierr)
     !call VecDestroy(vouti_left,ierr)
   else
     !save the eigenvectors
     call VecScatterCreateToAll(xr,ctxr(i+1),voutr(i+1),ierr)
     ! scatter as many times as you need
     call VecScatterBegin(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterEnd(ctxr(i+1),xr,voutr(i+1),INSERT_VALUES,SCATTER_FORWARD,ierr)
     !
     call VecGetArrayReadF90(voutr(i+1),xsr,ierr)
     V_right(:, i+1) = xsr
     call VecRestoreArrayReadF90(voutr(i+1),xsr,ierr)
     call VecScatterDestroy(ctxr(i+1),ierr)
     call VecDestroy(voutr(i+1),ierr)
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
 
