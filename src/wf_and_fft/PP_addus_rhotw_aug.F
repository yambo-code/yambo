!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
subroutine PP_addus_rhotw_aug(isc)
  !----------------------------------------------------------------------
  !
  ! ... Add US contribution to the rhotw matrix elements
  !
  USE pars,         ONLY: SP,DP
  USE ions_base,    ONLY: nat, nsp, ityp
  USE gvect,        ONLY: eigts1, eigts2, eigts3, mill
  USE uspp,         ONLY: okvan
  USE uspp_param,   ONLY: upf, nh, nhm
  use cell_base,    ONLY: omega
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
  call timing("PP_rhotw_aug","start")
  !
  ngm_s=1
  ngm_e=isc%ngrho
  ngm_l=ngm_e-ngm_s+1
  !
  DO nt = 1, nsp
     IF ( upf(nt)%tvanp ) THEN
        !
        ! nij = max number of (ih,jh) pairs per atom type nt
        !
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
              !
              DO ig = 1, ngm_l
                 skk(ig,nb) = eigts1( -mill(1,ngm_s+ig-1), na) * &
                              eigts2( -mill(2,ngm_s+ig-1), na) * &
                              eigts3( -mill(3,ngm_s+ig-1), na) * isc%eiqtau(na)
              ENDDO
           ENDIF
        ENDDO
        !
        ! sum over atoms
        !
        CALL ZGEMM( 'N', 'T', ngm_l, nh_l*nh_l, nab, (1.0_dp,0.0_dp), skk, ngm_l,&
                    becprod_DP, nh_l*nh_l, (0.0_dp,0.0_dp), aux2, ngm_l )
        !
        ! sum over lm indices of Q_{lm}
        !
        DO jh = 1, nh(nt)
        DO ih = 1, jh
           !
           DO ig = 1, ngm_l
              isc%rhotw(ngm_s+ig-1) = isc%rhotw(ngm_s+ig-1) + aux2(ig,ih,jh)* &
&                                     isc%qgm(ig,ih,jh,nt)
           ENDDO
           !
        ENDDO
        ENDDO
        !
        DEALLOCATE (aux2, becprod_DP, skk )
     ENDIF
  ENDDO
  !
  call timing("PP_rhotw_aug","stop")
  !
end subroutine PP_addus_rhotw_aug

