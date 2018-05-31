!
! Copyright (C) 2001-2006 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
SUBROUTINE pre_init()
  !----------------------------------------------------------------------------
  !
  USE ions_base,        ONLY : nat, nsp, ityp
  USE uspp_param,       ONLY : upf, lmaxkb, nh, nhm, nbetam
  USE uspp,             ONLY : nkb, nkbus
  IMPLICIT NONE
  INTEGER :: na, nt, nb
  !
  !     calculate the number of beta functions for each atomic type
  !
  lmaxkb = - 1
  DO nt = 1, nsp
     !
     nh (nt) = 0
     !
     ! do not add any beta projector if pseudo in 1/r fmt (AF)
     IF ( upf(nt)%tcoulombp ) CYCLE 
     !
     DO nb = 1, upf(nt)%nbeta
        nh (nt) = nh (nt) + 2 * upf(nt)%lll(nb) + 1
        lmaxkb = MAX (lmaxkb, upf(nt)%lll(nb) )
     ENDDO
     !
  ENDDO
  !
  ! calculate the maximum number of beta functions
  !
  nhm = MAXVAL (nh (1:nsp))
  nbetam = MAXVAL (upf(:)%nbeta)
  !
  ! calculate the number of beta functions of the solid
  !
  nkb = 0
  nkbus = 0
  do na = 1, nat
     nt = ityp(na)
     nkb = nkb + nh (nt)
     if (upf(nt)%tvanp) nkbus = nkbus + nh (nt)
  enddo

END SUBROUTINE pre_init
