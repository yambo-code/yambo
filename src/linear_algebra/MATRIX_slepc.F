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
subroutine MATRIX_slepc(M,BS_H_eig,E_real,E_cmpl,V_right,V_left)
  !
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
  complex(SP)            :: M(:,:)       ! matrix to diagonalize
  integer                :: BS_H_eig     ! number of eigenvalues to compute
  complex(SP),  optional :: V_right(:,:) ! right eigenvalues
  complex(SP),  optional :: V_left(:,:)  ! left eigenvalues
  complex(SP),  optional :: E_cmpl(:)    ! complex eigenvalues
  real(SP),     optional :: E_real(:)    ! real eigenvalues
  !
  ! other variables
  !
  integer :: M_size    ! size of the matrix
  !
  ! ...slepc
  !
  type(Mat)      A
  type(EPS)      eps
  EPSType        tname
  PetscReal      tol, error
  PetscErrorCode ierr
  PetscInt       nev, ncv, mpd, maxit, its, nconv, n, i, j, one
  PetscInt       idx(2)
  PetscScalar    kr, ki
  PetscScalar, pointer :: xsr(:), xsi(:)
  Vec            xr, xi
  PetscViewer    viewer
  PetscMPIInt    rank
  character(len=30) :: rowfmt
  !
  ! Non hermitian not implemented yet!
  if (present(V_right) .or. present(V_left)) then
    write(*,*) 'non-hermitian matrices with slepc is not implemented'
    call exit(0)
  endif
  !
  one = 1
  write(*,*)
  write(*,*) 'We will use the SLEPC driver'
  n = size(M,1)
  write(*,*) 'The matrix size is', n
  write(*,*) 'initializing slepc...'
  call SlepcInitialize(PETSC_NULL_CHARACTER,ierr)
  call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
  !
  ! write the matrix if wanted
  if (.false.) then
    write(rowfmt,'(A,I4,A)') '(',n,'(1X,E12.4,E12.4))'
    do, i=1,n
      write(*,rowfmt) (real(M(i,j)),aimag(M(i,j)),j=1,n)
    enddo
  endif

  write(*,*) 'creating dense matrix...'
  call MatCreateDense(PETSC_COMM_WORLD,n,n,PETSC_DECIDE,PETSC_DECIDE,M,A,ierr)
  call MatSetOption(A,MAT_HERMITIAN,PETSC_TRUE)
  write(*,*) 'done!'
  !
  !
  write(*,*) 'filling up elements...'
  if (.false.) then
    do i=1,n
     do j=1,n
      !if (abs(M(i,j)) > 1e-8) then
        call MatSetValue(A,i-1,j-1,M(j,i),INSERT_VALUES,ierr)
      !endif
    enddo
    enddo
  endif
  one = 1
  !call MatSetValues(A,one,n,one,n,M,INSERT_VALUES,ierr)
  write(*,*) 'done...'

  call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr)
  call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr)
  
  if (.false.) then
    !call MatView(A,PETSC_VIEWER_DRAW_WORLD)
    !call MatView(A,PETSC_VIEWER_STDOUT_SELF)
  end if

  write(*,*) 'creating vecs...'
  call MatCreateVecs(A,xr,xi,ierr)
  write(*,*) 'done!'

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !     Create the eigensolver and display info
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !     ** Create eigensolver context
  call EPSCreate(PETSC_COMM_WORLD,eps,ierr)
  
  !     ** Set operators. In this case, it is a standard eigenvalue problem
  call EPSSetOperators(eps,A,PETSC_NULL_OBJECT,ierr)
  call EPSSetProblemType(eps,EPS_HEP,ierr)
  call EPSSetWhichEigenpairs(eps,EPS_SMALLEST_MAGNITUDE,ierr)

  !set type of problem
  !call EPSSetType(eps,EPSLAPACK,ierr)

  !     ** Set solver parameters at runtime
  nev = BS_H_eig
  ncv = 2*nev
  mpd = 2*nev
  call EPSSetDimensions(eps,nev,ncv,mpd,ierr)

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !     Solve the eigensystem
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  call EPSSolve(eps,ierr)
  call EPSGetIterationNumber(eps,its,ierr)
  write(*,110) its
  110  format (/' Number of iterations of the method:',I4)

  !     ** Optional: Get some information from the solver and display it
  call EPSGetType(eps,tname,ierr)
  write(*,120) tname
  120  format (' Solution method: ',A)
  call EPSGetDimensions(eps,nev,PETSC_NULL_INTEGER,PETSC_NULL_INTEGER,ierr)
  write(*,130) nev
  130  format (' Number of requested eigenvalues:',I2)
  call EPSGetTolerances(eps,tol,maxit,ierr)
  write(*,140) tol, maxit
  140  format (' Stopping condition: tol=',1P,E10.4,', maxit=',I4)

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !     Display solution and clean up
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  !     ** Get number of converged eigenpairs
  call EPSGetConverged(eps,nconv,ierr)
  write(*,150) nconv
  150  format (' Number of converged eigenpairs:',I2/)

  !     ** Display eigenvalues and relative errors
  if (nconv.gt.0) then
     write(*,*) '         k          ||Ax-kx||/||kx||'
     write(*,*) ' ----------------- ------------------'
     do i=0,nconv-1
         !         ** Get converged eigenpairs: i-th eigenvalue is stored in kr
         !         ** (real part) and ki (imaginary part)
         call EPSGetEigenpair(eps,i,kr,PETSC_NULL_OBJECT,xr,PETSC_NULL_OBJECT,ierr)

         !         ** Compute the relative error associated to each eigenpair
         call EPSComputeError(eps,i,EPS_ERROR_RELATIVE,error,ierr)
         write(*,160) PetscRealPart(kr), error
         160      format (1P,'   ',E12.4,'       ',E12.4)
         
         !save the eigenvalues and eigenvectors in the matrix of the hamiltonian\
         !E_real(i+1) = PetscRealPart(kr)
         E_real(i+1) = kr
         
         call VecGetArrayF90(xr,xsr,ierr)
         M(i+1,:) = xsr
         call VecRestoreArrayF90(xr,xsr,ierr)
         
     enddo
  endif
  
  write(*,*) 'freeing memory...'
  call MatDestroy(A,ierr)
  write(*,*) 'matrix done'
  call EPSDestroy(eps,ierr)
  write(*,*) 'eps done'
  call VecDestroy(xr,ierr)
  call VecDestroy(xi,ierr)
  write(*,*) 'vec done'
  call SlepcFinalize(ierr)
  write(*,*) 'slepc done'
end subroutine
