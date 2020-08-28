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
subroutine MATRIX_slepc(A,n_eig,target_energy,extraction,slepc_ncv,slepc_tol,V_right,V_left,E_real,E_cmpl)
 !
 ! 10/06/2016 HM
 ! Use slepc to obtain the first few eigenpairs of a large matrix
 ! This routine can be called from within Matrix_driver.F but should be kept in
 ! a separate source file for clarity.
 !
 use pars,          ONLY : SP,schlen
 use com,           ONLY : msg
 use petscmat
 use slepceps
 use slepcepsdef
 use petscmatdef
 !
 implicit none
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
 ! arguments
 !
 integer                     :: n_eig         ! number of eigenvalues to compute
 complex(SP),  optional      :: V_right(:,:)  ! right eigenvalues
 complex(SP),  optional      :: V_left(:,:)   ! left eigenvalues
 complex(SP),  optional      :: E_cmpl(:)     ! complex eigenvalues
 real(SP),     optional      :: E_real(:)     ! real eigenvalues
 character(schlen)           :: extraction    ! extraction method for the eigenvalues
 !
 PetscComplex                 :: target_energy
 PetscReal                    :: slepc_tol
 PetscFortranInt              :: slepc_ncv
 ! 
 ! internal variables
 !
 ! slepc
 !
 external :: MyEPSMonitor !function to monitor the convergence
 ! 
 Mat            :: A
 EPS            :: eps
 EPSExtraction  :: extr
 PetscReal      :: tol, ferror
 PetscErrorCode ::ierr
 PetscInt       :: nev, ncv, mpd, maxit, its, nconv, n, i, j
 PetscInt       :: idx(2)
 PetscScalar    :: kr, ki
 PetscScalar, pointer :: xsr(:), xsi(:)
 PetscScalar, pointer :: M(:,:)          !pointer to matrix
 Vec            ::  xr, xi, vout
 PetscViewer    ::  viewer, hdf5v
 PetscMPIInt    ::  rank
 VecScatter     ::  ctx
 character(len=30) :: rowfmt
 !
 ! Non hermitian not implemented yet!
 !
 if (present(V_left)) then
   call error( 'diagonalisation of non-hermitian matrices with SLEPC is not implemented in yambo yet' )
   call exit(0)
 endif
 !
 call MatGetSize(A,n,j,ierr)
 call SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
 call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
 !
 call MatCreateVecs(A,xr,xi,ierr)
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
 call EPSSetOperators(eps,A,PETSC_NULL_MAT,ierr)
 call EPSSetProblemType(eps,EPS_HEP,ierr)
 !
 if (real(target_energy,SP) .ne. 0) then
   call msg( 'nr', '[SLEPC] Criterion is target energy       ', real(target_energy,SP) )
   call EPSSetTarget(eps,target_energy,ierr)
   call EPSSetWhichEigenpairs(eps,EPS_TARGET_REAL,ierr)
 else
   call msg( 'nr', '[SLEPC] Criterion is smaller eigenvalues')
   call EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr)
 endif
 !
 if      (extraction == 'ritz')              then
   extr = EPS_RITZ
 else if (extraction == 'harmonic')          then
   extr = EPS_HARMONIC         
 else if (extraction == 'harmonic_relative') then
   extr = EPS_HARMONIC_RELATIVE
 else if (extraction == 'harmonic_right')    then
   extr = EPS_HARMONIC_RIGHT
 else if (extraction == 'harmonic_largest')  then
   extr = EPS_HARMONIC_LARGEST
 else if (extraction == 'refined')           then
   extr = EPS_REFINED
 else if (extraction == 'refined_harmonic')  then
   extr = EPS_REFINED_HARMONIC
 endif
 call msg( 'r', '[SLEPC] Extraction method                ', extraction)
 !
 call EPSSetExtraction(eps, extr, ierr)
 !
 !set type of problem
 !call EPSSetType(eps,EPSLAPACK,ierr)
 !call EPSSetType(eps,EPSKRYLOVSCHUR,ierr)

 !
 ! Set solver parameters at runtime
 !
 nev = n_eig
 !if ( (present(slepc_ncv)) .and. (slepc_ncv.ne.0) ) then
 if ( slepc_ncv.ne.0 ) then
   ncv = slepc_ncv
 else
   ncv = PETSC_DECIDE
 endif 
 call EPSSetDimensions(eps,nev,ncv,PETSC_DECIDE,ierr)
 !
 call EPSSetTolerances(eps,slepc_tol,PETSC_DECIDE, ierr)
 !
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Optional: Get some information from the solver and display it
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !call EPSGetType(eps,tname,ierr)
 call EPSGetTolerances(eps,tol,maxit,ierr)
 !
 call msg( 'r', '[SLEPC] Number of requested eigenvalues   ', int(nev) )
 call msg( 'r', '[SLEPC] Stopping condition tolerance      ', real(tol,SP) )
 call msg( 'r', '[SLEPC] Stopping condition max iterations ', int(maxit) )

 !Set monitor
 call EPSMonitorSet(eps,MyEPSMonitor,ierr)
 !&                   PETSC_NULL_INTEGER,&
 !&                   PETSC_NULL_FUNCTION,ierr)
 !

 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Solve the eigensystem
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 call EPSSolve(eps,ierr)
 call EPSGetIterationNumber(eps,its,ierr)
 call msg( 'r', '[SLEPC] Number of iterations              ', int(its) )
 call EPSGetDimensions(eps,nev,ncv,mpd,ierr)
 call msg( 'r', '[SLEPC] Number of eigenvalues        [NEV]', int(nev) )
 call msg( 'r', '[SLEPC]                              [NCV]', int(ncv) )
 call msg( 'r', '[SLEPC]                              [MPD]', int(mpd) )

 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !     Display solution and clean up
 ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 !
 call EPSGetConverged(eps,nconv,ierr)
 call msg( 'rn', '[SLEPC] Number of converged states        ', int(nconv))
 if ( nconv < n_eig ) then
   n_eig = nconv
 endif
 !
 !free the A matrix
 call MatDestroy(A,ierr)
 !
 !open file for output
 !call PetscViewerHDF5Open(PETSC_COMM_WORLD, 'hdb.BS_slepc', FILE_MODE_WRITE, hdf5v, ierr)
 !
 !calculate eigenvalues and relative errors
 !
 if (nconv.gt.0) then
   do i=0,nev-1
      !
     !Get converged eigenpairs: i-th eigenvalue is stored in kr
     !(real part) and ki (imaginary part)
     call EPSGetEigenpair(eps,i,kr,PETSC_NULL_SCALAR,xr,PETSC_NULL_VEC,ierr)
     !
     !save the eigenvalues and eigenvectors in the matrix of the hamiltonian\
     !E_real(i+1) = PetscRealPart(kr)
     E_real(i+1) = real(kr,SP)
     !  
     ! this is to write the vector to hdf5 directly
     !call VecView(xr,hdf5v,ierr)
     !  
     call VecScatterCreateToAll(xr,ctx,vout,ierr);
     ! scatter as many times as you need
     call VecScatterBegin(ctx,xr,vout,INSERT_VALUES,SCATTER_FORWARD,ierr);
     call VecScatterEnd(ctx,xr,vout,INSERT_VALUES,SCATTER_FORWARD,ierr);
     !  
     call VecGetArrayReadF90(vout,xsr,ierr)
     V_right(:, i+1) = xsr
     call VecRestoreArrayReadF90(vout,xsr,ierr)
     !
     ! destroy scatter context and local vector when no longer needed
     call VecScatterDestroy(ctx,ierr);
     call VecDestroy(vout,ierr);
     !
   enddo
 endif
 !
 call EPSDestroy(eps,ierr)
 call VecDestroy(xr,ierr)
 call VecDestroy(xi,ierr)
 call SlepcFinalize(ierr)
 !
end subroutine
!
subroutine MyEPSMonitor(eps,its,nconv,eigr,eigi,errest,nest,dummy,ierr)
 ! 
 ! Monitor the convergence of the iterative diagonalization
 ! 
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use petscmat
 use slepceps
 use slepcepsdef
 use petscmatdef
 !
 implicit none
 !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
 !
 EPS            :: eps
 Vec            :: x
 PetscErrorCode :: ierr
 PetscInt       :: its,nconv,nest,dummy
 PetscScalar    :: eigr(*),eigi(*)
 PetscReal      :: re,errest(*)
 PetscMPIInt    :: rank
 !
 ! write the number of converged eigenvalues
 call msg('s', '[SLEPC] Iteration #'// trim(intc(its))// ' - converged States '&
&           // trim(intc(nconv)) //' - error ', errest(nconv+1))

 ierr = 0
 !
end subroutine
