!
! Copyright (C) 2001 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!-----------------------------------------------------------------------
SUBROUTINE setqf (qfcoef, rho, r, nqf, ltot, mesh)
  !-----------------------------------------------------------------------
  !
  !   This routine compute the first part of the Q function up to rinner.
  !   On output it contains r^2 Q
  !
  !
  USE pars, ONLY : SP
  IMPLICIT NONE
  !
  !     first the dummy variables
  !
  INTEGER :: nqf, ltot, mesh
  ! input: the number of coefficients
  ! input: the angular momentum
  ! input: the number of mesh point
  REAL(SP) :: r (mesh), qfcoef (nqf), rho (mesh)
  ! input: the radial mesh
  ! input: the coefficients of Q
  ! output: the function to be computed
  !
  !     here the local variables
  !
  integer :: ir, i
  ! counter on  mesh points
  ! counter on the coeffients

  REAL(SP) :: rr
  ! the square of the radius
  DO ir = 1, mesh
     rr = r (ir) **2
     rho (ir) = qfcoef (1)
     DO i = 2, nqf
        rho (ir) = rho (ir) + qfcoef (i) * rr** (i - 1)
     ENDDO
     rho (ir) = rho (ir) * r (ir) ** (ltot + 2)

  ENDDO
  RETURN
END SUBROUTINE setqf
