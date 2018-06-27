!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
SUBROUTINE PP_addus_rhotw_aug(isc)
  !----------------------------------------------------------------------
  !
  ! ... Add US contribution to the rhotw matrix elements
  !
  USE pars,         ONLY: SP,DP
  USE ions_base,    ONLY: nat, ntyp => nsp, ityp
  USE gvect,        ONLY: eigts1, eigts2, eigts3, mill
  USE uspp,         ONLY: okvan
  USE uspp_param,   ONLY: upf, nh, nhm
  use collision_el, ONLY: elemental_collision
  use timing_m,     ONLY: timing
  !
  IMPLICIT NONE
  !
  type(elemental_collision) :: isc
  !
  ! workspace
  !
  INTEGER :: ngm_s, ngm_e, ngm_l
  INTEGER :: ig, na, nt, ih, jh, nab, nb, nh_l
  COMPLEX(DP), ALLOCATABLE :: becprod_DP(:,:,:)
  COMPLEX(DP), ALLOCATABLE :: skk(:,:), aux2(:,:,:)

  IF (.not.okvan) RETURN
  !
  ngm_s=1
  ngm_e=isc%ngrho
  ngm_l=ngm_e-ngm_s+1
  !
  DO nt = 1, ntyp
     IF ( upf(nt)%tvanp ) THEN
        !
        ! nij = max number of (ih,jh) pairs per atom type nt
        !
call timing("rhotw_aug1","start")
        nh_l = nh(nt)
        !
        ! count max number of atoms of type nt
        !
        nab = 0
        DO na = 1, nat
           IF ( ityp(na) == nt ) nab = nab + 1
        ENDDO
        !
        ALLOCATE ( skk(ngm_l,nab), becprod_DP(nh_l,nh_l,nab), aux2(ngm_l,nh_l,nh_l) )
        !
        nb = 0
        DO na = 1, nat
           IF ( ityp(na) == nt ) THEN
              nb = nb + 1
              becprod_DP(:,:,nb) = isc%becprod(1:nh_l,1:nh_l,na,1)
!!!!!!!$omp parallel do default(shared) private(ig)
              DO ig = 1, ngm_l
                 skk(ig,nb) = eigts1 (mill (1,ngm_s+ig-1), na) * &
                              eigts2 (mill (2,ngm_s+ig-1), na) * &
                              eigts3 (mill (3,ngm_s+ig-1), na)
              ENDDO
!!!!!!!$omp end parallel do
           ENDIF
        ENDDO
call timing("rhotw_aug1","stop")

call timing("rhotw_aug2","start")
        ! sum over atoms
        CALL zgemm( 'N', 'T', ngm_l, nh_l*nh_l, nab, (1.0_dp,0.0_dp), skk, ngm_l,&
                    becprod_DP, nh_l*nh_l, (0.0_dp,0.0_dp), aux2, ngm_l )
        ! sum over lm indices of Q_{lm}
call timing("rhotw_aug2","stop")
        !
call timing("rhotw_aug3","start")
        DO jh = 1, nh (nt)
        DO ih = 1, nh (nt)
           !
           CALL qvan2 (ngm_l, ih, jh, nt, isc%qmod, isc%qgm, isc%ylmk0)
!!!!!!$omp parallel do default(shared) private(ig)
           DO ig = 1, ngm_l
              isc%rhotw(ngm_s+ig-1) = isc%rhotw(ngm_s+ig-1)+aux2(ig,ih,jh)*isc%qgm(ig)
           ENDDO
!!!!!!$omp end parallel do
           !
        ENDDO
        ENDDO
call timing("rhotw_aug3","stop")
        !
        DEALLOCATE (aux2, becprod_DP, skk )
     ENDIF
  ENDDO
  !
END SUBROUTINE PP_addus_rhotw_aug

