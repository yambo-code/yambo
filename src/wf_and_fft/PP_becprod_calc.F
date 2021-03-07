!
! Copyright (C) 2001-2015 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
subroutine PP_becprod_calc ( becprod, nspin, current_spin, domag, mb, nb, becp, becpp )
  !----------------------------------------------------------------------------
  !
  ! This routine computes
  !
  !     becprod_mn_Iij =  <\psi_m|\beta_iI> <\beta_jI|\psi_n>
  !
  ! Calls to calbec to compute "becp"=<beta_i|psi_m> are supposed to be already done 
  ! output written in becprod
  !
  USE kinds,         ONLY : DP, SP
  USE control_flags, ONLY : gamma_only, tqr
  USE ions_base,     ONLY : nat, ntyp => nsp, ityp
  USE uspp,          ONLY : nkb, vkb, indv_ijkb0
  USE uspp_param,    ONLY : upf, nh, nhm
  USE noncollin_module,     ONLY : noncolin, npol
  USE becmod,        ONLY : bec_type
  !
  IMPLICIT NONE
  COMPLEX(SP),    INTENT(OUT):: becprod(nhm,nhm,nat,nspin)
  INTEGER,        INTENT(IN) :: nspin, current_spin, mb, nb
  LOGICAL,        INTENT(IN) :: domag
  TYPE(bec_type), INTENT(IN) :: becp, becpp
  !
  COMPLEX(SP), ALLOCATABLE :: auxk1(:,:), auxk2(:,:), aux_nc(:,:)
  COMPLEX(SP), ALLOCATABLE :: aux_gk(:,:)
  INTEGER :: ikb, ih, jh, na, np, is,  i_err
  
  !
  DO np = 1, ntyp
     !
     IF ( upf(np)%tvanp ) THEN
        !
        ! allocate work space used to perform GEMM operations
        !
        ALLOCATE( auxk1( 1, nh(np)*npol ), auxk2( 1, nh(np)*npol ) )
        !
        IF ( noncolin ) THEN
           ALLOCATE ( aux_nc( nh(np)*npol,nh(np)*npol ) ) 
        ELSE
           ALLOCATE ( aux_gk( nh(np),nh(np) ) ) 
        END IF
        !
        !   In becp=<vkb_i|psi_j> terms corresponding to atom na of type nt
        !   run from index i=indv_ijkb0(na)+1 to i=indv_ijkb0(na)+nh(nt)
        !
        DO na = 1, nat
           !
           IF (ityp(na)==np) THEN
              !
              !  <psi_i|beta_l><beta_m|psi_i>
              ! copy into aux1, aux2 the needed data to perform a GEMM
              !
              IF ( noncolin ) THEN
                 !
                 DO is = 1, npol
                    DO ih = 1, nh(np)
                       ikb = indv_ijkb0(na) + ih
                       !
                       auxk1(1,ih+(is-1)*nh(np))= becp%nc(ikb,is,mb)
                       auxk2(1,ih+(is-1)*nh(np))= becpp%nc(ikb,is,nb)
                       !
                    END DO
                 END DO
                 !
#if defined _DOUBLE
                 CALL ZGEMM ( 'C', 'N', npol*nh(np), npol*nh(np), 1, &
                      (1.0_dp,0.0_dp), auxk1, 1, auxk2, 1, &
                      (0.0_dp,0.0_dp), aux_nc, npol*nh(np) )
#else
                 CALL CGEMM ( 'C', 'N', npol*nh(np), npol*nh(np), 1, &
                      (1.0_SP,0.0_SP), auxk1, 1, auxk2, 1, &
                      (0.0_SP,0.0_SP), aux_nc, npol*nh(np) )
#endif
                 !
              ELSE IF ( gamma_only ) THEN
                 !
                 CALL errore("becprod_calc","not implemented in Yambo",i_err)
                 !
              ELSE
                 !
                 DO ih = 1, nh(np)
                    ikb = indv_ijkb0(na) + ih
                    !
                    auxk1(1,ih) = becp%k(ikb,mb) 
                    auxk2(1,ih) = becpp%k(ikb,nb)
                    !
                 END DO
                 !
                 ! only the real part is computed
                 !
#if defined _DOUBLE
                 CALL ZGEMM ( 'C', 'N', nh(np), nh(np), 1, &
                      (1.0_dp,0.0_dp), auxk1, 1, auxk2, 1, &
                      (0.0_dp,0.0_dp), aux_gk, nh(np) )
#else
                 CALL CGEMM ( 'C', 'N', nh(np), nh(np), 1, &
                      (1.0_SP,0.0_SP), auxk1, 1, auxk2, 1, &
                      (0.0_SP,0.0_SP), aux_gk, nh(np) )
#endif
                 !
              ENDIF
              !
              ! copy output from GEMM into desired format
              !
              IF (noncolin .AND. .NOT. upf(np)%has_so) THEN
                 CALL add_becprod_nc (na, np, aux_nc, becprod, nspin, domag )
              ELSE IF (noncolin .AND. upf(np)%has_so) THEN
                 CALL add_becprod_so (na, np, aux_nc, becprod, nspin, domag )
              ELSE
                 
                 DO jh = 1, nh(np)
                    DO ih = 1, jh
                       becprod(ih,jh,na,current_spin) = aux_gk(ih,jh)+aux_gk(jh,ih)
                    ENDDO
                    becprod(jh,jh,na,current_spin)=becprod(jh,jh,na,current_spin)*0.5_SP
                 ENDDO
                 !
              ENDIF
           ENDIF
           !
        ENDDO
        !
        IF ( noncolin ) THEN
           DEALLOCATE ( aux_nc )
        ELSE
           DEALLOCATE ( aux_gk  ) 
        END IF
        DEALLOCATE( auxk2, auxk1 )
        !
     END IF
     !
  END DO
  !
end subroutine PP_becprod_calc
!
!----------------------------------------------------------------------------
subroutine add_becprod_nc ( na, np, aux_nc, becprod, nspin_mag, domag )
!----------------------------------------------------------------------------
  !
  ! This routine multiplies aux_nc by the identity and the Pauli matrices,
  ! saves it in becprod for the calculation of augmentation charge and
  ! magnetization.
  !
  USE kinds,                ONLY : SP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE uspp_param,           ONLY : nh, nhm
  USE noncollin_module,     ONLY : npol
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: na, np, nspin_mag
  LOGICAL, INTENT(IN) :: domag
  COMPLEX(SP), INTENT(IN)    :: aux_nc(nh(np),npol,nh(np),npol)
  COMPLEX(SP), INTENT(INOUT) :: becprod(nhm,nhm,nat,nspin_mag)
  !
  ! ... local variables
  !
  INTEGER :: ih, jh
  !
  DO jh = 1, nh(np)
  DO ih = 1, nh(np)
     !
     becprod(ih,jh,na,1)= aux_nc(ih,1,jh,1) + aux_nc(ih,2,jh,2)
     !
     IF (domag) THEN
        becprod(ih,jh,na,2)= aux_nc(ih,1,jh,2) + aux_nc(ih,2,jh,1)
        becprod(ih,jh,na,3)= (0.0_SP,-1.0_SP)* (aux_nc(ih,1,jh,2) - aux_nc(ih,2,jh,1))
        becprod(ih,jh,na,4)= aux_nc(ih,1,jh,1) - aux_nc(ih,2,jh,2)
     ENDIF
     !
  ENDDO
  ENDDO
  !
  DO jh = 1, nh(np)
  DO ih = 1, jh-1
     becprod(ih,jh,na,:) = becprod(ih,jh,na,:) + becprod(jh,ih,na,:)
  ENDDO
  ENDDO
  
end subroutine add_becprod_nc
!
!----------------------------------------------------------------------------
subroutine add_becprod_so( na, np, aux_so, becprod, nspin_mag, domag )
  !----------------------------------------------------------------------------
  !
  ! This routine multiplies aux_so by the identity and the Pauli matrices,
  ! rotates it as appropriate for the spin-orbit case, saves it in becprod
  ! for the calculation of the augmentation of local operator matrix elements
  !
  USE kinds,                ONLY : SP
  USE ions_base,            ONLY : nat, ntyp => nsp, ityp
  USE uspp_param,           ONLY : nh, nhm
  USE uspp,                 ONLY : ijtoh, nhtol, nhtoj, indv
  USE noncollin_module,     ONLY : npol
  USE spin_orb,             ONLY : fcoef
  !
  IMPLICIT NONE
  
  INTEGER, INTENT(IN) :: na, np, nspin_mag
  LOGICAL, INTENT(IN) :: domag
  COMPLEX(SP), INTENT(IN)    :: aux_so(nh(np),npol,nh(np),npol)
  COMPLEX(SP), INTENT(INOUT) :: becprod(nhm,nhm,nat,nspin_mag)
  !
  ! ... local variables
  !
  INTEGER :: ih, jh, lh, kh,  is1, is2
  COMPLEX(SP) :: fac
  
  becprod(:,:,na,:) = 0.0
  !
  DO jh = 1, nh(np)
  DO ih = 1, nh(np)
     !
     DO kh = 1, nh(np)
        IF (same_lj(kh,ih,np)) THEN
           !
           DO lh=1,nh(np)
              IF (same_lj(lh,jh,np)) THEN
                 !
                 DO is1=1,npol
                 DO is2=1,npol
                    !
                    fac=aux_so(kh,is1,lh,is2)
                    !
                    becprod(ih,jh,na,1)=becprod(ih,jh,na,1) + fac * &
                         (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,1,is2,np) + &
                          fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,2,is2,np)  )
                    !
                    IF (domag) THEN
                      becprod(ih,jh,na,2)=becprod(ih,jh,na,2)+fac * &
                          (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,2,is2,np) +&
                           fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,1,is2,np)  )
                      becprod(ih,jh,na,3)=becprod(ih,jh,na,3)+fac*(0.0_SP,-1.0_SP)*&
                          (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,2,is2,np) - &
                           fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,1,is2,np)  )
                      becprod(ih,jh,na,4)=becprod(ih,jh,na,4) + fac * &
                          (fcoef(kh,ih,is1,1,np)*fcoef(jh,lh,1,is2,np) - &
                           fcoef(kh,ih,is1,2,np)*fcoef(jh,lh,2,is2,np)  )
                    ENDIF
                    !
                 ENDDO
                 ENDDO
                 !
              ENDIF
           ENDDO
           !
        ENDIF
     ENDDO
     !
  ENDDO
  ENDDO
  !
  DO jh = 1, nh(np)
  DO ih = 1, jh-1
     becprod(ih,jh,na,:) = becprod(ih,jh,na,:) + becprod(jh,ih,na,:)
  ENDDO
  ENDDO
  !
contains
   LOGICAL function same_lj(ih,jh,np)
   INTEGER :: ih, jh, np
   !
   same_lj = ((nhtol(ih,np)==nhtol(jh,np)).AND. &
             (ABS(nhtoj(ih,np)-nhtoj(jh,np))<1.d8).AND. &
             (indv(ih,np)==indv(jh,np)) )
   !
   end function same_lj

end subroutine add_becprod_so

