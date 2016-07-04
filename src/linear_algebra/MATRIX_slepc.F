!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine MATRIX_slepc(A,n_eig,E_real,E_cmpl,V_right,V_left)
  !
  ! 10/06/2016 HM
  ! Use slepc to obtain the first few eigenpairs of a large matrix
  ! This routine can be called from within Matrix_driver.F but should be kept in
  ! a separate souce file for clarity.
  !
  use pars,          ONLY:SP,cONE,cZERO
  implicit none
  !
  ! headers
  !
#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>
  !
  ! arguments
  !
  integer                :: n_eig        ! number of eigenvalues to compute
  complex(SP),  optional :: V_right(:,:) ! right eigenvalues
  complex(SP),  optional :: V_left(:,:)  ! left eigenvalues
  complex(SP),  optional :: E_cmpl(:)    ! complex eigenvalues
  real(SP),     optional :: E_real(:)    ! real eigenvalues
  ! 
  ! internal variables
  !
  integer                :: n_mat        !size of the matrix
  !
  ! slepc
  !
  external MyEPSMonitor !function to monitor the convergence
  !
  type(Mat)      A
  type(EPS)      eps
  EPSType        tname
  PetscReal      tol, error
  PetscErrorCode ierr
  PetscInt       nev, ncv, mpd, maxit, its, nconv, n, i, j
  PetscInt       idx(2)
  PetscScalar    kr, ki
  PetscScalar, pointer :: xsr(:), xsi(:)
  PetscScalar, pointer :: M(:,:)          !pointer to matrix
  Vec            xr, xi, vout
  PetscViewer    viewer, hdf5v
  PetscMPIInt    rank
  VecScatter     ctx
  character(len=30) :: rowfmt
  !
  ! Non hermitian not implemented yet!
  !
  if (present(V_left)) then
    write(*,*) 'Diagonalisation of non-hermitian matrices with SLEPC is not implemented in yambo yet'
    call exit(0)
  endif
  !
  call MatGetSize(A,n,j,ierr)
  if (rank .eq. 0) write(*,*) 'initializing slepc...'
  call SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
  if (rank .eq. 0) write(*,*) 'done!'
  call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
  !
  ! write the matrix if wanted
  if (.false.) then
    call MatView(A,PETSC_VIEWER_STDOUT_WORLD,ierr)
  endif
  !
  if (rank .eq. 0) write(*,*) 'creating vecs...'
  call MatCreateVecs(A,xr,xi,ierr)
  if (rank .eq. 0) write(*,*) 'done!'
  
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !     Create the eigensolver and display info
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !Create eigensolver context
  call EPSCreate(PETSC_COMM_WORLD,eps,ierr)
  
  !Set monitor
  call EPSMonitorSet(eps,MyEPSMonitor,PETSC_NULL_OBJECT, PETSC_NULL_FUNCTION,ierr)
  
  !Set operators. In this case, it is a standard eigenvalue problem
  call EPSSetOperators(eps,A,PETSC_NULL_OBJECT,ierr)
  call EPSSetProblemType(eps,EPS_HEP,ierr)
  call EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr)

  !set type of problem
  !call EPSSetType(eps,EPSLAPACK,ierr)

  !Set solver parameters at runtime
  nev = n_eig
  ncv = 2*nev
  mpd = 2*nev
  call EPSSetDimensions(eps,nev,ncv,mpd,ierr)

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !     Solve the eigensystem
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (rank .eq. 0) write(*,*) 'solving...'
  call EPSSolve(eps,ierr)
  call EPSGetIterationNumber(eps,its,ierr)
  if (rank .eq. 0) then
    write(*,110) its
  end if  
  110  format (/' Number of iterations of the method:',I4)

  !Optional: Get some information from the solver and display it
  call EPSGetType(eps,tname,ierr)
  if (rank .eq. 0) then
    write(*,120) tname
  end if
  120  format (' Solution method: ',A)
  call EPSGetDimensions(eps,nev,PETSC_NULL_INTEGER,PETSC_NULL_INTEGER,ierr)
  if (rank .eq. 0) then
    write(*,130) nev
  end if
  130  format (' Number of requested eigenvalues:',I4)
  call EPSGetTolerances(eps,tol,maxit,ierr)
  if (rank .eq. 0) then
    write(*,140) tol, maxit
  end if 
  140  format (' Stopping condition: tol=',1P,E10.4,', maxit=',I4)

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !     Display solution and clean up
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !Get number of converged eigenpairs
  call EPSGetConverged(eps,nconv,ierr)
  if (rank .eq. 0) then
    write(*,150) nconv
  end if
  150  format (' Number of converged eigenpairs:',I4/)

  !free the A matrix
  call MatDestroy(A,ierr)
  
  !open file for output
  !call PetscViewerHDF5Open(PETSC_COMM_WORLD, 'hdb.BS_slepc', FILE_MODE_WRITE, hdf5v, ierr)
  
  !Display eigenvalues and relative errors
  if (nconv.gt.0) then
    if (rank .eq. 0) then
       write(*,*) '         k          ||Ax-kx||/||kx||'
       write(*,*) ' ----------------- ------------------'
    end if
    do i=0,nev-1
         !Get converged eigenpairs: i-th eigenvalue is stored in kr
         !(real part) and ki (imaginary part)
         call EPSGetEigenpair(eps,i,kr,PETSC_NULL_OBJECT,xr,PETSC_NULL_OBJECT,ierr)

         !Compute the relative error associated to each eigenpair
         call EPSComputeError(eps,i,EPS_ERROR_RELATIVE,error,ierr)
         if (rank .eq. 0) then
           write(*,160) PetscRealPart(kr), error
         end if
         160      format (1P,'   ',E12.4,'       ',E12.4)
         
         !save the eigenvalues and eigenvectors in the matrix of the hamiltonian\
         !E_real(i+1) = PetscRealPart(kr)
         E_real(i+1) = kr
         
         ! this is to write the vector to hdf5 directly
         !call VecView(xr,hdf5v,ierr)
         
         call VecScatterCreateToAll(xr,ctx,vout,ierr);
         ! scatter as many times as you need
         call VecScatterBegin(ctx,xr,vout,INSERT_VALUES,SCATTER_FORWARD,ierr);
         call VecScatterEnd(ctx,xr,vout,INSERT_VALUES,SCATTER_FORWARD,ierr);
         
         call VecGetArrayReadF90(vout,xsr,ierr)
         V_right(:, i+1) = xsr
         call VecRestoreArrayReadF90(vout,xsr,ierr)
         
         ! destroy scatter context and local vector when no longer needed
         call VecScatterDestroy(ctx,ierr);
         call VecDestroy(vout,ierr);
      
     enddo
  endif
  !call PetscViewerDestroy(hdf5v,ierr)
  if (rank .eq. 0) write(*,*) 'done!'
  
  call EPSDestroy(eps,ierr)
  call VecDestroy(xr,ierr)
  call VecDestroy(xi,ierr)
  call SlepcFinalize(ierr)
end subroutine

subroutine MyEPSMonitor(eps,its,nconv,eigr,eigi,errest,nest,dummy,ierr)
  implicit none

#include <petsc/finclude/petscsys.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h90>
#include <slepc/finclude/slepcsys.h>
#include <slepc/finclude/slepceps.h>

  EPS            eps
  Vec            x
  PetscErrorCode ierr
  PetscInt       its,nconv,nest,dummy
  PetscScalar    eigr(*),eigi(*)
  PetscReal      re,errest(*)
  PetscMPIInt    rank

  call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
  if (its .gt. 0 .and. rank .eq. 0) then
   re = PetscRealPart(eigr(nconv+1))
   write(6,140) its,nconv,re,errest(nconv+1)
  endif

  140  format(i3,' EPS nconv=',i4,' first unconverged value (error) ', f6.4,' (',g9.3,')')
  ierr = 0
end subroutine
