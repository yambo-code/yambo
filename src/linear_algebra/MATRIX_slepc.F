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
subroutine MATRIX_slepc(M_slepc,n_eig,V_right,V_left,E_real,E_cmpl)
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
 &                         BSS_slepc_approach,BSS_slepc_matrix_format,BSS_slepc_mpd,&
 &                         BSS_slepc_pseudo_herm
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
 MatType                            :: mtype
 !
 EPSExtraction                      :: extr
 EPSKrylovSchurBSEType              :: epsbsetype
 PetscReal                          :: tol, ferror
 PetscScalar                        :: target_energy
 PetscErrorCode                     :: ierr
 PetscInt                           :: nev, ncv, mpd, maxit, its, nconv, i_eig, Nsize, Msize
 PetscInt                           :: idx(2)
 PetscScalar                        :: kr, ki
 PetscScalar, pointer               :: xsr(:), xsi(:), xsr_left(:), xsi_left(:)
 PetscScalar, pointer               :: M(:,:)          !pointer to matrix
 Vec                                :: xr, xi, xr_left, xi_left
 Vec                                :: vout, vout2, xup, xdown
 IS                                 :: is(2)
 PetscViewer                        :: viewer, hdf5v
 PetscMPIInt                        :: rank
 VecScatter                         :: vs, vs2
 !
 logical           :: l_precondition
 character(len=30) :: rowfmt
 !
 !
 call MatGetSize(M_slepc,Nsize,Msize,ierr)
 !
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
   if (BSS_slepc_pseudo_herm) then
     ! Uses the pseudo-hermitian properties of the bse matrix
     call EPSSetProblemType(eps,EPS_BSE,ierr)
   else
     ! One could check if the problem can be cast in the form of a generalized hermitian problem
     ! However, given the existence of the pseudo-hermitian scheme, this is not useful anymore
     call EPSSetProblemType(eps,EPS_NHEP,ierr)
     call EPSSetTwoSided(eps,PETSC_TRUE,ierr)
   endif
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
 ! * Jacobi-Davidson: EPSJD, NOT WORKING WITH KSP PREONLY
 !                    STPPRECOND + KSPCG SUPER-SLOW + PCJACOBY super slow
 !
 ! Default Krylov-Schur used in case input is mispelled
 epskind=EPSKRYLOVSCHUR
 !
 ! Load eps from command line options. It overwrites input file
 !call EPSSetFromOptions(eps,ierr)
 ! From input. It overwrites command line options.
 if(index(BSS_slepc_approach,"Krylov-Schur")>0)          epskind=EPSKRYLOVSCHUR
 if(index(BSS_slepc_approach,"Generalized-Davidson")>0)  epskind=EPSGD
 if(index(BSS_slepc_approach,"Jacobi-Davidson")>0)       epskind=EPSJD
 !if(index(BSS_slepc_approach,"Scalapack")>0)            epskind=EPSSCALAPACK
 !if(index(BSS_slepc_approach,"ELPA")>0)                 epskind=EPSELPA
 !
 if (.not. (epskind==EPSKRYLOVSCHUR .or. epskind==EPSGD .or. epskind==EPSJD )) then
   call error("wrong slepc approach in input "//trim(BSS_slepc_approach))
 endif
 !
 call EPSSetType(eps,epskind,ierr)
 !
 l_precondition=BSS_slepc_precondition/="none"
 !
 if (l_precondition.and. trim(BSS_slepc_matrix_format)=="shell") then
   !
   call warning("Preconditioning not compatible with shell matrix, disabled")
   BSS_slepc_precondition="none"
   l_precondition=.false.
   !
 endif
 !
 stkind="none"
 kspkind="none"
 pckind="nopc"
 !
 if (epskind==EPSGD.or.epskind==EPSJD) then
  !
  ! Define stkind
  ! GD and JD always want stkind to be defined. This is done
  ! automatically by slepc, however with this line, I get the info in output 
  stkind=STPRECOND
  !
  ! Define kspkind
  if (epskind==EPSGD)      kspkind=KSPPREONLY
  if (epskind==EPSJD)      kspkind=KSPBCGS
  !
 endif
 !
 if (l_precondition) then
  !
  ! (re)Define stkind
  ! Default
  if (epskind==EPSKRYLOVSCHUR) stkind=STSINVERT
  ! From user
  if(index(BSS_slepc_precondition,STSINVERT)/=0)   stkind=STSINVERT
  !
  ! (re)Define stkind
  ! Default
  !if (stkind==STSINVERT)                           kspkind=KSPBCGS
  if (stkind==STSINVERT)                           kspkind=KSPPREONLY
  ! From user
  if(index(BSS_slepc_precondition,KSPPREONLY)/=0)  kspkind=KSPPREONLY
  if(index(BSS_slepc_precondition,KSPBCGS)/=0)     kspkind=KSPBCGS
  !
 endif
 !
 if (epskind==EPSGD.or.epskind==EPSJD.or.l_precondition) then
  call EPSGetST(eps,st,ierr)
  call STGetKSP(st,ksp,ierr)
  ! Pass stkind and kspkind to slepc
  call STSetType(st,stkind,ierr)
  call KSPSetType(ksp,kspkind,ierr)
 endif
 !
 if (l_precondition) then
   !
   ! Define pckind
   !
   ! Default
   if (stkind==STSINVERT)   pckind=PCLU
   if (stkind==STPRECOND)   pckind=PCJACOBI
   ! From user
   if(index(BSS_slepc_precondition,PCJACOBI)/=0)      pckind=PCJACOBI
   if(index(BSS_slepc_precondition,PCFIELDSPLIT)/=0)  pckind=PCFIELDSPLIT
   if(index(BSS_slepc_precondition,PCLU)/=0)          pckind=PCLU
   !
   call KSPGetPC(ksp,pc,ierr)
   ! Pass pckind to slepc
   call PCSetType(pc,pckind,ierr)
   !
   !call STSetType(st,stkind,ierr)
   !call KSPSetType(ksp,kspkind,ierr)
   !if (l_precondition) call PCSetType(pc,pckind,ierr)
 endif
 !
 BSS_slepc_precondition=trim(stkind)//"+"//trim(kspkind)//"+"//trim(pckind)
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
 if (BSS_slepc_target_E/=0._SP.or.l_precondition) then
   target_energy=BSS_slepc_target_E
   call EPSSetTarget(eps,target_energy,ierr)
   call EPSSetWhichEigenpairs(eps,EPS_TARGET_REAL,ierr)
 else
   call EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr)
 endif
 !
 ! Set solver parameters at runtime
 !
 nev = n_eig
 if ( BSS_slepc_ncv/=0 ) ncv = BSS_slepc_ncv
 if ( BSS_slepc_ncv==0 ) ncv = PETSC_DECIDE
 if ( BSS_slepc_mpd/=0 ) mpd = BSS_slepc_mpd
 if ( BSS_slepc_mpd==0 ) mpd = PETSC_DECIDE
 !
 call EPSSetDimensions(eps,nev,ncv,mpd,ierr)
 !
 if ( BSS_slepc_maxit/=0 ) maxit = BSS_slepc_maxit
 if ( BSS_slepc_maxit==0 ) maxit = PETSC_DECIDE
 !
 tol      = BSS_slepc_tol
 call EPSSetTolerances(eps,tol,maxit, ierr)
 !
 !
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Optional: Get some information from the solver and display it
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !call EPSGetType(eps,tname,ierr)
 call EPSGetTolerances(eps,tol,maxit,ierr)
 !
 !Set monitor
 call EPSMonitorSet(eps,MyEPSMonitor,0,PETSC_NULL_FUNCTION,ierr)
 !
 if(present(V_left).and.BSS_slepc_pseudo_herm.and.epskind==EPSKRYLOVSCHUR) then
   ! Default Shao used in case input is mispelled or not set
   epsbsetype=EPS_KRYLOVSCHUR_BSE_SHAO
   ! Eventially load epsbsetype from command line options
   call EPSKrylovSchurGetBSEType(eps,epsbsetype,ierr)
   ! From input file. If set, it owrites command line options.
   if(index(BSS_slepc_approach,"Shao")>0)        epsbsetype=EPS_KRYLOVSCHUR_BSE_SHAO
   if(index(BSS_slepc_approach,"Gruning")>0)     epsbsetype=EPS_KRYLOVSCHUR_BSE_GRUNING
   if(index(BSS_slepc_approach,"Projected")>0)   epsbsetype=EPS_KRYLOVSCHUR_BSE_PROJECTEDBSE
   call EPSKrylovSchurSetBSEType(eps,epsbsetype,ierr)
 endif
 !
 ! Fix the BSS_slepc_approach string
 if (epskind==EPSKRYLOVSCHUR) BSS_slepc_approach="Krylov-Schur"
 !
 if(present(V_left)) then
   if(BSS_slepc_pseudo_herm.and.epskind==EPSKRYLOVSCHUR) then
     if(epsbsetype==EPS_KRYLOVSCHUR_BSE_SHAO)         BSS_slepc_approach=trim(BSS_slepc_approach)//"+Shao" 
     if(epsbsetype==EPS_KRYLOVSCHUR_BSE_GRUNING)      BSS_slepc_approach=trim(BSS_slepc_approach)//"+Gruning" 
     if(epsbsetype==EPS_KRYLOVSCHUR_BSE_PROJECTEDBSE) BSS_slepc_approach=trim(BSS_slepc_approach)//"+ProjectedBSE"
   else
     BSS_slepc_approach=trim(BSS_slepc_approach)//"+NonHermitian"
   endif
 endif
 !
 call msg( 'sr', '[SLEPC] Approach                          ',BSS_slepc_approach)
 !
 call msg( 'sr', '[SLEPC] Precondition method               ',BSS_slepc_precondition)
 call msg( 'sr', '[SLEPC] Extraction method                 ',BSS_slepc_extraction)
 call msg( 'sr', '[SLEPC] Number of requested eigenvalues   ', n_eig ) 
 !
 if (BSS_slepc_target_E/=0._SP.or.l_precondition) then
   call msg( 'nsr', '[SLEPC] Criterion is target energy        ', real(target_energy,SP)*HA2EV,'[eV]')
 else
   call msg( 'nsr', '[SLEPC] Criterion is smaller eigenvalues')
 endif
 !
 call msg( 'sr', '[SLEPC] Stopping condition tolerance      ', real(tol,SP) )
 call msg( 'sr', '[SLEPC] Stopping condition max iterations ', int(maxit) )
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
 call MatGetType(M_slepc,mtype,ierr)
 if(mtype==MATNEST) then
   call MatNestGetISs(M_slepc,is,is,ierr)
   call VecGetSubVector(xr,is(1),xup,ierr)
   call VecGetSubVector(xr,is(2),xdown,ierr)
   call VecScatterCreateToAll(xup,vs,vout,ierr)
   call VecScatterCreateToAll(xdown,vs2,vout2,ierr)
   call VecRestoreSubVector(xr,is(1),xup,ierr)
   call VecRestoreSubVector(xr,is(2),xdown,ierr)
 else
   call VecScatterCreateToAll(xr,vs,vout,ierr)
 endif

 do i_eig=0,n_eig-1
   !Get converged eigenpairs: i-th eigenvalue is stored in kr
   !(real part) and ki (imaginary part)
   call EPSGetEigenpair(eps,i_eig,kr,PETSC_NULL_SCALAR,xr,PETSC_NULL_VEC,ierr)
   if(present(V_left)) call EPSGetLeftEigenvector(eps,i_eig,xr_left,PETSC_NULL_VEC,ierr)
   !
   !save the eigenvalues and eigenvectors in the matrix of the hamiltonian
   if(present(E_real)) E_real(i_eig+1) = real(kr,SP)
   if(present(E_cmpl)) E_cmpl(i_eig+1) = cmplx(kr,kind=SP)
   !
   ! this is to write the vector to hdf5 directly
   !call VecView(xr,hdf5v,ierr)
   !
   if(mtype==MATNEST) then
     !save the right eigenvectors
     ! scatter as many times as you need
     call VecGetSubVector(xr,is(1),xup,ierr)
     call VecGetSubVector(xr,is(2),xdown,ierr)
     call VecScatterBegin(vs,xup,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterBegin(vs2,xdown,vout2,INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterEnd(vs,xup,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterEnd(vs2,xdown,vout2,INSERT_VALUES,SCATTER_FORWARD,ierr)
     !
     call VecGetArrayReadF90(vout,xsr,ierr)
     V_right(1:Nsize/2, i_eig+1) = cmplx(xsr,kind=SP)
     call VecRestoreArrayReadF90(vout,xsr,ierr)
     call VecGetArrayReadF90(vout2,xsr,ierr)
     V_right(Nsize/2+1:Nsize, i_eig+1) = cmplx(xsr,kind=SP)
     call VecRestoreArrayReadF90(vout2,xsr,ierr)
     !
     call VecRestoreSubVector(xr,is(1),xup,ierr)
     call VecRestoreSubVector(xr,is(2),xdown,ierr)
     !
     if (present(V_left)) then
       !save the left eigenvectors
       ! scatter as many times as you need
       call VecGetSubVector(xr_left,is(1),xup,ierr)
       call VecGetSubVector(xr_left,is(2),xdown,ierr)
       call VecScatterBegin(vs,xup,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
       call VecScatterBegin(vs2,xdown,vout2,INSERT_VALUES,SCATTER_FORWARD,ierr)
       call VecScatterEnd(vs,xup,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
       call VecScatterEnd(vs2,xdown,vout2,INSERT_VALUES,SCATTER_FORWARD,ierr)
       !
       call VecGetArrayReadF90(vout,xsr_left,ierr)
       V_left(1:Nsize/2, i_eig+1) = cmplx(xsr_left,kind=SP)
       call VecRestoreArrayReadF90(vout,xsr_left,ierr)
       call VecGetArrayReadF90(vout2,xsr_left,ierr)
       V_left(Nsize/2+1:Nsize, i_eig+1) = cmplx(xsr_left,kind=SP)
       call VecRestoreArrayReadF90(vout2,xsr_left,ierr)
       !
       call VecRestoreSubVector(xr_left,is(1),xup,ierr)
       call VecRestoreSubVector(xr_left,is(2),xdown,ierr)
     endif
   else
     !save the right eigenvectors
     ! scatter as many times as you need
     call VecScatterBegin(vs,xr,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
     call VecScatterEnd(vs,xr,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
     !
     call VecGetArrayReadF90(vout,xsr,ierr)
     V_right(:, i_eig+1) = cmplx(xsr,kind=SP)
     !if (BSS_slepc_double_grp) V_right(BS_K_dim(1)+1:,i_eig+1)=cI*V_right(BS_K_dim(1)+1:,i_eig+1)
     call VecRestoreArrayReadF90(vout,xsr,ierr)
     !
     if (present(V_left)) then
       !save the left eigenvectors
       ! scatter as many times as you need
       call VecScatterBegin(vs,xr_left,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
       call VecScatterEnd(vs,xr_left,vout,INSERT_VALUES,SCATTER_FORWARD,ierr)
       !
       call VecGetArrayReadF90(vout,xsr_left,ierr)
       V_left(:, i_eig+1) = cmplx(xsr_left,kind=SP)
       !if (BSS_slepc_double_grp) V_left(BS_K_dim(1)+1:,i_eig+1) =cI*V_left(BS_K_dim(1)+1:,i_eig+1)
       call VecRestoreArrayReadF90(vout,xsr_left,ierr)
     endif
   endif
   !
 enddo
 !
 ! destroy scatter context and local vector when no longer needed
 !
 call VecScatterDestroy(vs,ierr)
 call VecDestroy(vout,ierr)
 if(mtype==MATNEST) then
   call VecScatterDestroy(vs2,ierr)
   call VecDestroy(vout2,ierr)
 endif
 !
 !free the M_slepc matrix
 call MatDestroy(M_slepc,ierr)
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
 
