!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
!SUBROUTINE sum_bec ( ik, current_spin, ibnd_start, ibnd_end, this_bgrp_nbnd ) 
SUBROUTINE sum_bec ( ik, current_spin, ibnd_start, ibnd_end, this_bgrp_nbnd, wg )
  !----------------------------------------------------------------------------
  !
  ! This routine computes the sum over bands
  !     \sum_i <\psi_i|\beta_l>w_i<\beta_m|\psi_i>
  ! for point "ik" and, for LSDA, spin "current_spin" 
  ! Calls calbec to compute "becp"=<beta_m|psi_i> 
  ! Output is accumulated (unsymmetrized) into "becsum", module "uspp"
  !
  ! Routine used in sum_band (if okvan) and in compute_becsum, called by hinit1 (if okpaw)
  !
  USE kinds,         ONLY : DP, SP
!  USE becmod,        ONLY : becp, calbec, allocate_bec_type
  USE becmod,        ONLY : becp
  USE control_flags, ONLY : gamma_only, tqr
  USE ions_base,     ONLY : nat, ntyp => nsp, ityp
  USE uspp,          ONLY : nkb, vkb, becsum, ebecsum, indv_ijkb0
  USE uspp_param,    ONLY : upf, nh, nhm
!  USE wvfct,         ONLY : nbnd, wg, et, current_k
!  USE klist,         ONLY : ngk
  USE noncollin_module,     ONLY : noncolin, npol
!  USE wavefunctions_module, ONLY : evc
!  USE realus,        ONLY : real_space, &
!                            invfft_orbital_gamma, calbec_rs_gamma, &
!                            invfft_orbital_k, calbec_rs_k
!  USE us_exx,        ONLY : store_becxx0
!  USE mp_bands,      ONLY : nbgrp,inter_bgrp_comm
!  USE mp,            ONLY : mp_sum
  !
  IMPLICIT NONE
  INTEGER,  INTENT(IN) :: ik, current_spin, ibnd_start, ibnd_end, this_bgrp_nbnd
  REAL(SP), INTENT(IN) :: wg(ibnd_start:ibnd_end)
  !
  COMPLEX(SP), ALLOCATABLE :: becsum_nc(:,:,:,:)
  COMPLEX(SP), ALLOCATABLE :: auxk1(:,:), auxk2(:,:), aux_nc(:,:)
  REAL(SP), ALLOCATABLE :: auxg(:,:), aux_gk(:,:), aux_egk(:,:)
  INTEGER :: ibnd, ibnd_loc, nbnd_loc  ! counters on bands
  INTEGER :: ikb, jkb, ih, jh, ijh, na, np, is, js
  ! counters on beta functions, atoms, atom types, spin
  !
! AF: calbec is assumed to be already computed before
  !
!  npw = ngk(ik)
!  IF ( .NOT. real_space ) THEN
!     ! calbec computes becp = <vkb_i|psi_j>
!     CALL calbec( npw, vkb, evc, becp )
!  ELSE
!     if (gamma_only) then
!        do ibnd = ibnd_start, ibnd_end, 2
!           call invfft_orbital_gamma(evc,ibnd,ibnd_end) 
!           call calbec_rs_gamma(ibnd,ibnd_end,becp%r)
!        enddo
!        call mp_sum(becp%r,inter_bgrp_comm)
!     else
!        current_k = ik
!        becp%k = (0.d0,0.d0)
!        do ibnd = ibnd_start, ibnd_end
!           call invfft_orbital_k(evc,ibnd,ibnd_end) 
!           call calbec_rs_k(ibnd,ibnd_end)
!        enddo
!       call mp_sum(becp%k,inter_bgrp_comm)
!     endif
!  ENDIF
!  !
!  ! In the EXX case with ultrasoft or PAW, a copy of becp will be
!  ! saved in a global variable to be rotated later
!  CALL store_becxx0(ik, becp)
!  !
!  CALL start_clock( 'sum_band:becsum' )

  IF (noncolin) THEN
     ALLOCATE(becsum_nc(nhm*(nhm+1)/2,nat,npol,npol))
     becsum_nc=(0.d0, 0.d0)
  ENDIF
  !
  DO np = 1, ntyp
     !
     IF ( upf(np)%tvanp ) THEN
        !
        ! allocate work space used to perform GEMM operations
        !
        IF ( gamma_only ) THEN
           nbnd_loc = becp%nbnd_loc
           ALLOCATE( auxg( nbnd_loc, nh(np) ) )
        ELSE
           ALLOCATE( auxk1( ibnd_start:ibnd_end, nh(np)*npol ), &
                     auxk2( ibnd_start:ibnd_end, nh(np)*npol ) )
        END IF
        IF ( noncolin ) THEN
           ALLOCATE ( aux_nc( nh(np)*npol,nh(np)*npol ) ) 
        ELSE
           ALLOCATE ( aux_gk( nh(np),nh(np) ) ) 
           if (tqr) ALLOCATE ( aux_egk( nh(np),nh(np) ) ) 
        END IF
        !
        !   In becp=<vkb_i|psi_j> terms corresponding to atom na of type nt
        !   run from index i=indv_ijkb0(na)+1 to i=indv_ijkb0(na)+nh(nt)
        !
        DO na = 1, nat
           !
           IF (ityp(na)==np) THEN
              !
              ! sum over bands: \sum_i <psi_i|beta_l><beta_m|psi_i> w_i
              ! copy into aux1, aux2 the needed data to perform a GEMM
              !
              IF ( noncolin ) THEN
                 !
!$omp parallel do default(shared), private(is,ih,ikb,ibnd)
                 DO is = 1, npol
                    DO ih = 1, nh(np)
                       ikb = indv_ijkb0(na) + ih
                       DO ibnd = ibnd_start, ibnd_end
                          auxk1(ibnd,ih+(is-1)*nh(np))= becp%nc(ikb,is,ibnd)
                          auxk2(ibnd,ih+(is-1)*nh(np))= wg(ibnd) * &
                                                        becp%nc(ikb,is,ibnd)
                       END DO
                    END DO
                 END DO
!$omp end parallel do
                 !
#if defined _DOUBLE
                 CALL ZGEMM ( 'C', 'N', npol*nh(np), npol*nh(np), this_bgrp_nbnd, &
                      (1.0_dp,0.0_dp), auxk1, this_bgrp_nbnd, auxk2, this_bgrp_nbnd, &
                      (0.0_dp,0.0_dp), aux_nc, npol*nh(np) )
#else
                 CALL CGEMM ( 'C', 'N', npol*nh(np), npol*nh(np), this_bgrp_nbnd, &
                      (1.0_SP,0.0_SP), auxk1, this_bgrp_nbnd, auxk2, this_bgrp_nbnd, &
                      (0.0_SP,0.0_SP), aux_nc, npol*nh(np) )
#endif
                 !
              ELSE IF ( gamma_only ) THEN
                 !
                 CALL errore("sum_bec","not implemented in Yambo")
!                 !
!!$omp parallel do default(shared), private(ih,ikb,ibnd,ibnd_loc)
!                 DO ih = 1, nh(np)
!                    ikb = indv_ijkb0(na) + ih
!                    DO ibnd_loc = 1, nbnd_loc
!                       ibnd = ibnd_loc + becp%ibnd_begin - 1
!                       auxg(ibnd_loc,ih)= wg(ibnd)*becp%r(ikb,ibnd_loc) 
!                    END DO
!                 END DO
!!$omp end parallel do
!                 !
!                 ! NB: band parallelizazion has not been performed in this case because 
!                 !     bands were already distributed across R&G processors.
!                 !     Contribution to aux_gk is scaled by 1.d0/nbgrp so that the becsum
!                 !     summation across bgrps performed outside will give the right value.
!                 !
!#if defined _DOUBLE
!                 CALL DGEMM ( 'N', 'N', nh(np), nh(np), nbnd_loc, &
!                      1.0_dp/nbgrp, becp%r(indv_ijkb0(na)+1,1), nkb,    &
!                      auxg, nbnd_loc, 0.0_dp, aux_gk, nh(np) )
!#else
!                 CALL SGEMM ( 'N', 'N', nh(np), nh(np), nbnd_loc, &
!                      1.0_SP/nbgrp, becp%r(indv_ijkb0(na)+1,1), nkb,    &
!                      auxg, nbnd_loc, 0.0_SP, aux_gk, nh(np) )
!#endif
                 !
!               if (tqr) then
!!$omp parallel do default(shared), private(ih,ikb,ibnd,ibnd_loc)
!                 DO ih = 1, nh(np)
!                    ikb = indv_ijkb0(na) + ih
!                    DO ibnd_loc = 1, nbnd_loc
!                    auxg(ibnd_loc,ih) = et(ibnd_loc,ik) * auxg(ibnd_loc,ih)
!                    END DO
!                 END DO
!!$omp end parallel do
!                 CALL DGEMM ( 'N', 'N', nh(np), nh(np), nbnd_loc, &
!                      1.0_dp/nbgrp, becp%r(indv_ijkb0(na)+1,1), nkb,    &
!                      auxg, nbnd_loc, 0.0_dp, aux_egk, nh(np) )
!               end if
                 !
              ELSE
                 !
!$omp parallel do default(shared), private(ih,ikb,ibnd)
                 DO ih = 1, nh(np)
                    ikb = indv_ijkb0(na) + ih
                    DO ibnd = ibnd_start, ibnd_end
                       auxk1(ibnd,ih) = becp%k(ikb,ibnd) 
                       auxk2(ibnd,ih) = wg(ibnd)*becp%k(ikb,ibnd)
                    END DO
                 END DO
!$omp end parallel do
                 !
                 ! only the real part is computed
                 !
#if defined _DOUBLE
                 CALL DGEMM ( 'C', 'N', nh(np), nh(np), 2*this_bgrp_nbnd, &
                      1.0_dp, auxk1, 2*this_bgrp_nbnd, auxk2, 2*this_bgrp_nbnd, &
                      0.0_dp, aux_gk, nh(np) )
#else
                 CALL SGEMM ( 'C', 'N', nh(np), nh(np), 2*this_bgrp_nbnd, &
                      1.0_SP, auxk1, 2*this_bgrp_nbnd, auxk2, 2*this_bgrp_nbnd, &
                      0.0_SP, aux_gk, nh(np) )
#endif
                 !
!               if (tqr) then
!!$omp parallel do default(shared), private(ih,ikb,ibnd)
!                 DO ih = 1, nh(np)
!                    ikb = indv_ijkb0(na) + ih
!                    DO ibnd = ibnd_start, ibnd_end
!                       auxk2(ibnd,ih) = et(ibnd,ik)*auxk2(ibnd,ih)
!                    END DO
!                 END DO
!!$omp end parallel do
!                 CALL DGEMM ( 'C', 'N', nh(np), nh(np), 2*this_bgrp_nbnd, &
!                      1.0_dp, auxk1, 2*this_bgrp_nbnd, auxk2, 2*this_bgrp_nbnd, &
!                      0.0_dp, aux_egk, nh(np) )
!               end if

              END IF
              !
              ! copy output from GEMM into desired format
              !
              IF (noncolin .AND. .NOT. upf(np)%has_so) THEN
                 CALL add_becsum_nc (na, np, aux_nc, becsum )
              ELSE IF (noncolin .AND. upf(np)%has_so) THEN
                 CALL add_becsum_so (na, np, aux_nc,becsum )
              ELSE
                 ijh = 0
                 DO ih = 1, nh(np)
                    DO jh = ih, nh(np)
                       ijh = ijh + 1
                       !
                       ! nondiagonal terms summed and collapsed into a
                       ! single index (matrix is symmetric wrt (ih,jh))
                       !
                       IF ( jh == ih ) THEN
                          becsum(ijh,na,current_spin) = &
                               becsum(ijh,na,current_spin) + aux_gk (ih,jh)
                          if (tqr) ebecsum(ijh,na,current_spin) = &
                               ebecsum(ijh,na,current_spin) + aux_egk (ih,jh)
                       ELSE
                          becsum(ijh,na,current_spin) = &
                               becsum(ijh,na,current_spin) + aux_gk(ih,jh)*2.0_SP
                          if (tqr) ebecsum(ijh,na,current_spin) = &
                               ebecsum(ijh,na,current_spin) + aux_egk(ih,jh)*2.0_SP
                       END IF
                    END DO
                 END DO
                 !
              END IF
           END IF
           !
        END DO
        !
        IF ( noncolin ) THEN
           DEALLOCATE ( aux_nc )
        ELSE
           DEALLOCATE ( aux_gk  ) 
           if (tqr) DEALLOCATE ( aux_egk  ) 
        END IF
        IF ( gamma_only ) THEN
           DEALLOCATE( auxg )
        ELSE
           DEALLOCATE( auxk2, auxk1 )
        END IF
        !
     END IF
     !
  END DO
  !
  IF ( noncolin ) DEALLOCATE ( becsum_nc )
  !
!  CALL stop_clock( 'sum_band:becsum' )
  !
END SUBROUTINE sum_bec
!
!----------------------------------------------------------------------------
SUBROUTINE add_becsum_nc ( na, np, becsum_nc, becsum )
!----------------------------------------------------------------------------
  !
  ! This routine multiplies becsum_nc by the identity and the Pauli matrices,
  ! saves it in becsum for the calculation of augmentation charge and
  ! magnetization.
  !
  USE kinds,                ONLY : SP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE uspp_param,           ONLY : nh, nhm
  USE lsda_mod,             ONLY : nspin
  USE noncollin_module,     ONLY : npol, nspin_mag
  USE spin_orb,             ONLY : domag
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: na, np
  COMPLEX(SP), INTENT(IN) :: becsum_nc(nh(np),npol,nh(np),npol)
  REAL(SP), INTENT(INOUT) :: becsum(nhm*(nhm+1)/2,nat,nspin_mag)
  !
  ! ... local variables
  !
  INTEGER :: ih, jh, ijh
  REAL(SP) :: fac
  !
  ijh=0
  DO ih = 1, nh(np)
     DO jh = ih, nh(np)
        ijh=ijh+1
        IF ( ih == jh ) THEN
           fac = 1.0_SP
        ELSE
           fac = 2.0_SP
        END IF
        becsum(ijh,na,1)= becsum(ijh,na,1) + fac * &
                REAL( becsum_nc(ih,1,jh,1) + becsum_nc(ih,2,jh,2), SP)
        IF (domag) THEN
           becsum(ijh,na,2)= becsum(ijh,na,2) + fac *  &
                REAL( becsum_nc(ih,1,jh,2) + becsum_nc(ih,2,jh,1), SP)
           becsum(ijh,na,3)= becsum(ijh,na,3) + fac * REAL( (0.0_SP,-1.0_SP)* &
               (becsum_nc(ih,1,jh,2) - becsum_nc(ih,2,jh,1)), SP)
           becsum(ijh,na,4)= becsum(ijh,na,4) + fac * &
                REAL( becsum_nc(ih,1,jh,1) - becsum_nc(ih,2,jh,2), SP)
        END IF
     END DO
  END DO
  
END SUBROUTINE add_becsum_nc
!
!----------------------------------------------------------------------------
SUBROUTINE add_becsum_so( na, np, becsum_nc, becsum )
  !----------------------------------------------------------------------------
  !
  ! This routine multiplies becsum_nc by the identity and the Pauli matrices,
  ! rotates it as appropriate for the spin-orbit case, saves it in becsum
  ! for the calculation of augmentation charge and magnetization.
  !
  USE kinds,                ONLY : SP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE uspp_param,           ONLY : nh, nhm
  USE uspp,                 ONLY : ijtoh, nhtol, nhtoj, indv
  USE noncollin_module,     ONLY : npol, nspin_mag
  USE spin_orb,             ONLY : fcoef, domag
  !
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: na, np
  COMPLEX(SP), INTENT(IN) :: becsum_nc(nh(np),npol,nh(np),npol)
  REAL(SP), INTENT(INOUT) :: becsum(nhm*(nhm+1)/2,nat,nspin_mag)
  !
  ! ... local variables
  !
  INTEGER :: ih, jh, lh, kh, ijh, is1, is2
  COMPLEX(SP) :: fac
  
  DO ih = 1, nh(np)
     DO jh = 1, nh(np)
        ijh=ijtoh(ih,jh,np)
        DO kh = 1, nh(np)
           IF (same_lj(kh,ih,np)) THEN
              DO lh=1,nh(np)
                 IF (same_lj(lh,jh,np)) THEN
                    DO is1=1,npol
                       DO is2=1,npol
                          fac=becsum_nc(kh,is1,lh,is2)
                          becsum(ijh,na,1)=becsum(ijh,na,1) + fac * &
                               (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,1,is2,np) + &
                               fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,2,is2,np)  )
                          IF (domag) THEN
                            becsum(ijh,na,2)=becsum(ijh,na,2)+fac * &
                                (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,2,is2,np) +&
                                fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,1,is2,np)  )
                            becsum(ijh,na,3)=becsum(ijh,na,3)+fac*(0.0_SP,-1.0_SP)*&
                               (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,2,is2,np) - &
                                fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,1,is2,np)  )
                           becsum(ijh,na,4)=becsum(ijh,na,4) + fac * &
                               (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,1,is2,np) - &
                                fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,2,is2,np)  )
                        END IF
                     END DO
                  END DO
               END IF
            END DO
         END IF
      END DO
   END DO
END DO
!
CONTAINS
   LOGICAL FUNCTION same_lj(ih,jh,np)
   INTEGER :: ih, jh, np
   !
   same_lj = ((nhtol(ih,np)==nhtol(jh,np)).AND. &
             (ABS(nhtoj(ih,np)-nhtoj(jh,np))<1.d8).AND. &
             (indv(ih,np)==indv(jh,np)) )
   !
   END FUNCTION same_lj

END SUBROUTINE add_becsum_so

