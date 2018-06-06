!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------
SUBROUTINE addus_vloc_aug(ngm,nspin_mag,ldrho,rhog)
  !----------------------------------------------------------------------
  !
  ! ... Add US contribution to the matrix elements of a local operator
  !
  USE kinds,                ONLY : DP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE gvect,                ONLY : gg, g, &
                                   eigts1, eigts2, eigts3, mill
  USE uspp,                 ONLY : becprod, okvan
  USE uspp_param,           ONLY : upf, lmaxq, nh
  !
  IMPLICIT NONE
  !
  INTEGER,     INTENT(in)    :: ngm,nspin_mag,ldrho
  COMPLEX(DP), INTENT(inout) :: rhog(ldrho,nspin_mag)
  !
  !     here the local variables
  !
  INTEGER :: ngm_s, ngm_e, ngm_l
  ! starting/ending indices, local number of G-vectors
  INTEGER :: ig, na, nt, ih, jh, ijh, is, nab, nb, nh_l
  ! counters

  COMPLEX(DP), ALLOCATABLE :: tbecprod(:,:,:,:)
  ! \sum_kv <\psi_kv|\beta_l><beta_m|\psi_kv> for each species of atoms
  REAL(DP), ALLOCATABLE :: qmod (:), ylmk0 (:,:)
  ! modulus of G, spherical harmonics
  COMPLEX(DP), ALLOCATABLE :: skk(:,:), aux2(:,:,:)
  ! structure factors, US contribution to rho
  COMPLEX(DP), ALLOCATABLE ::  aux (:,:), qgm(:)
  ! work space for rho(G,nspin), Fourier transform of q

  IF (.not.okvan) RETURN
  !
  ALLOCATE (aux (ngm, nspin_mag) )
  aux (:,:) = (0.d0, 0.d0)
  !
  ! With k-point parallelization, distribute G-vectors across processors
  ! ngm_s = index of first G-vector for this processor
  ! ngm_e = index of last  G-vector for this processor
  ! ngm_l = local number of G-vectors 
  !
!  CALL divide (inter_pool_comm, ngm, ngm_s, ngm_e)
  ngm_s=1
  ngm_e=ngm
  !
  ngm_l = ngm_e-ngm_s+1
  !
  ! for the extraordinary unlikely case of more processors than G-vectors
  IF ( ngm_l <= 0 ) GO TO 10
  !
  ALLOCATE (qmod(ngm_l), qgm(ngm_l) )
  ALLOCATE (ylmk0(ngm_l, lmaxq * lmaxq) )

  CALL ylmr2 (lmaxq * lmaxq, ngm_l, g(1,ngm_s), gg(ngm_s), ylmk0)
  DO ig = 1, ngm_l
     qmod (ig) = sqrt (gg (ngm_s+ig-1) )
  ENDDO
  !
  DO nt = 1, ntyp
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
        ALLOCATE ( skk(ngm_l,nab), tbecprod(nh_l,nh_l,nab,nspin_mag), aux2(ngm_l,nh_l,nh_l) )
        !
        nb = 0
        DO na = 1, nat
           IF ( ityp(na) == nt ) THEN
              nb = nb + 1
              tbecprod(:,:,nb,:) = becprod(1:nh_l,1:nh_l,na,1:nspin_mag)
!$omp parallel do default(shared) private(ig)
              DO ig = 1, ngm_l
                 skk(ig,nb) = eigts1 (mill (1,ngm_s+ig-1), na) * &
                              eigts2 (mill (2,ngm_s+ig-1), na) * &
                              eigts3 (mill (3,ngm_s+ig-1), na)
              ENDDO
!$omp end parallel do
           ENDIF
        ENDDO

        DO is = 1, nspin_mag
           ! sum over atoms
           CALL zgemm( 'N', 'T', ngm_l, nh_l*nh_l, nab, (1.0_dp,0.0_dp), skk, ngm_l,&
                tbecprod(1,1,1,is), nh_l*nh_l, (0.0_dp,0.0_dp), aux2, ngm_l )
           ! sum over lm indices of Q_{lm}
           !
           DO jh = 1, nh (nt)
              DO ih = 1, nh (nt)
                 !
                 CALL qvan2 (ngm_l, ih, jh, nt, qmod, qgm, ylmk0)
!$omp parallel do default(shared) private(ig)
                 DO ig = 1, ngm_l
                    aux(ngm_s+ig-1,is) = aux(ngm_s+ig-1,is)+aux2(ig,ih,jh)*qgm(ig)
                 ENDDO
!$omp end parallel do
             ENDDO
           ENDDO
           !
        ENDDO
        DEALLOCATE (aux2, tbecprod, skk )
     ENDIF
  ENDDO
  !
  DEALLOCATE (ylmk0)
  DEALLOCATE (qgm, qmod)
  !
  10 CONTINUE
!  CALL mp_sum( aux, inter_pool_comm )
  !
  !     add aux to the charge density in reciprocal space
  !
  rhog(1:ngm,1:nspin_mag) = rhog(1:ngm,1:nspin_mag) + aux(:,:)
  !
  DEALLOCATE (aux)
  !
  RETURN
END SUBROUTINE addus_vloc_aug

