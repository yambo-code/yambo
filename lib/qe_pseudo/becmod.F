!
! Copyright (C) 2001-2007 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
!
MODULE becmod
  !
  ! ... *bec* contain <beta|psi> - used in h_psi, s_psi, many other places
  ! ... calbec( npw, beta, psi, betapsi [, nbnd ] ) is an interface calculating
  ! ...    betapsi(i,j)  = <beta(i)|psi(j)>   (the sum is over npw components)
  ! ... or betapsi(i,s,j)= <beta(i)|psi(s,j)> (s=polarization index)
  !
  USE kinds,            ONLY : SP, DP
  USE control_flags,    ONLY : gamma_only, smallmem
  USE gvect,            ONLY : gstart
  USE noncollin_module, ONLY : noncolin, npol
  !
  SAVE
  !
  TYPE bec_type
     REAL(SP),   ALLOCATABLE :: r(:,:)    ! appropriate for gammaonly
     COMPLEX(SP),ALLOCATABLE :: k(:,:)    ! appropriate for generic k
     COMPLEX(SP),ALLOCATABLE :: nc(:,:,:) ! appropriate for noncolin
     LOGICAL,    ALLOCATABLE :: init(:)   ! whether a given band has been filled in
     INTEGER :: comm
     INTEGER :: nkb
     INTEGER :: nbnd
     INTEGER :: nproc
     INTEGER :: mype
     INTEGER :: nbnd_loc
     INTEGER :: ibnd_begin
     LOGICAL :: alloc=.false.
  END TYPE bec_type
  !
  TYPE (bec_type) :: becp  ! <beta|psi>

  PRIVATE
  !
  INTERFACE calbec
     !
!     MODULE PROCEDURE calbec_k, calbec_gamma, calbec_gamma_nocomm, calbec_nc, calbec_bec_type
     MODULE PROCEDURE calbec_k, calbec_gamma, calbec_nc, calbec_bec_type
     !
  END INTERFACE

  INTERFACE becscal
     !
     MODULE PROCEDURE becscal_nck, becscal_gamma
     !
  END INTERFACE
  !
  PUBLIC :: bec_type, becp, allocate_bec_type, deallocate_bec_type, calbec, &
            beccopy, becscal, is_allocated_bec_type
  !
CONTAINS
  !-----------------------------------------------------------------------
  SUBROUTINE calbec_bec_type ( npw, beta, psi, betapsi, ibnd_s, nbnd)
    !-----------------------------------------------------------------------
    !_
!    USE mp_bands, ONLY: intra_bgrp_comm
!    USE mp,       ONLY: mp_get_comm_null
    !
    IMPLICIT NONE
    COMPLEX (SP),    INTENT (in) :: beta(:,:), psi(:,:)
    TYPE (bec_type), INTENT (inout) :: betapsi ! NB: must be INOUT otherwise
                                               !  the allocatd array is lost
    INTEGER, INTENT (in) :: npw, ibnd_s
    INTEGER, OPTIONAL :: nbnd
    !
    INTEGER :: local_nbnd
    !
    IF ( present (nbnd) ) THEN
        local_nbnd = nbnd
    ELSE
        local_nbnd = size ( psi, 2)
    ENDIF

    IF ( gamma_only ) THEN
       !
       CALL calbec_gamma ( npw, beta, psi, betapsi%r, ibnd_s, local_nbnd )
       !
    ELSEIF ( noncolin) THEN
       !
       CALL  calbec_nc ( npw, beta, psi, betapsi%nc, ibnd_s, local_nbnd )
       !
    ELSE
       !
       CALL  calbec_k ( npw, beta, psi, betapsi%k, ibnd_s, local_nbnd )
       !
    ENDIF
    !
    RETURN
    !
  END SUBROUTINE calbec_bec_type

!  !-----------------------------------------------------------------------
!  SUBROUTINE calbec_gamma_nocomm ( npw, beta, psi, betapsi, nbnd )
!    !-----------------------------------------------------------------------
!    USE mp_bands, ONLY: intra_bgrp_comm
!    IMPLICIT NONE
!    COMPLEX (SP), INTENT (in) :: beta(:,:), psi(:,:)
!   REAL (SP), INTENT (out) :: betapsi(:,:)
!   INTEGER, INTENT (in) :: npw
!   INTEGER, OPTIONAL :: nbnd
!   INTEGER :: m
!   IF ( present (nbnd) ) THEN
!       m = nbnd
!   ELSE
!       m = size ( psi, 2)
!   ENDIF
!   CALL calbec_gamma ( npw, beta, psi, betapsi, m, intra_bgrp_comm )
!   RETURN
!   !
! END SUBROUTINE calbec_gamma_nocomm

  !-----------------------------------------------------------------------
  SUBROUTINE calbec_gamma ( npw, beta, psi, betapsi, ibnd_s, nbnd )
    !-----------------------------------------------------------------------
    !
    ! ... matrix times matrix with summation index (k=1,npw) running on
    ! ... half of the G-vectors or PWs - assuming k=0 is the G=0 component:
    ! ... betapsi(i,j) = 2Re(\sum_k beta^*(i,k)psi(k,j)) + beta^*(i,0)psi(0,j)
    !
!    USE mp,        ONLY : mp_sum

    IMPLICIT NONE
    COMPLEX (SP), INTENT (in) :: beta(:,:), psi(:,:)
    REAL (SP), INTENT (out) :: betapsi(:,:)
    INTEGER, INTENT (in) :: npw
    INTEGER, INTENT (in) :: nbnd, ibnd_s
!    INTEGER, INTENT (in) :: comm 
    !
    INTEGER :: nkb, npwx, m
    !
    m = nbnd
    !
    nkb = size (beta, 2)
    IF ( nkb == 0 ) RETURN
    !
!    CALL start_clock( 'calbec' )
    IF ( npw == 0 ) betapsi(:,ibnd_s:)=0.0_SP
    npwx= size (beta, 1)
    IF ( npwx /= size (psi, 1) ) CALL errore ('calbec', 'size mismatch', 1)
    IF ( npwx < npw ) CALL errore ('calbec', 'size mismatch', 2)
#if defined(DEBUG)
    WRITE (*,*) 'calbec gamma'
    WRITE (*,*)  nkb,  size (betapsi,1) , m , size (betapsi, 2)
#endif
    IF ( nkb /= size (betapsi,1) .or. ibnd_s+m-1 > size (betapsi, 2) ) &
      CALL errore ('calbec', 'size mismatch', 3)
    !
    IF ( m == 1 ) THEN
        !
#if defined _DOUBLE
        CALL DGEMV( 'C', 2*npw, nkb, 2.0_DP, beta, 2*npwx, psi, 1, 0.0_DP, &
                     betapsi(:,ibnd_s:ibnd_s), 1 )
#else
        CALL SGEMV( 'C', 2*npw, nkb, 2.0_SP, beta, 2*npwx, psi, 1, 0.0_SP, &
                     betapsi(:,ibnd_s:ibnd_s), 1 )
#endif
        IF ( gstart == 2 ) betapsi(:,ibnd_s) = betapsi(:,ibnd_s) - beta(1,:)*psi(1,1)
        !
    ELSE
        !
#if defined _DOUBLE
        CALL DGEMM( 'C', 'N', nkb, m, 2*npw, 2.0_DP, beta, 2*npwx, psi, &
                    2*npwx, 0.0_DP, betapsi(:,ibnd_s:), nkb )
        IF ( gstart == 2 ) &
           CALL DGER( nkb, m, -1.0_DP, beta, 2*npwx, psi, 2*npwx, betapsi(:,ibnd_s:), nkb )
#else
        CALL SGEMM( 'C', 'N', nkb, m, 2*npw, 2.0_SP, beta, 2*npwx, psi, &
                    2*npwx, 0.0_SP, betapsi(:,ibnd_s:), nkb )
        IF ( gstart == 2 ) &
           CALL SGER( nkb, m, -1.0_SP, beta, 2*npwx, psi, 2*npwx, betapsi(:,ibnd_s:), nkb )
#endif
        !
    ENDIF
    !
!    CALL mp_sum( betapsi( :, 1:m ), comm )
!    CALL stop_clock( 'calbec' )
    !
    RETURN
    !
  END SUBROUTINE calbec_gamma
  !
  !-----------------------------------------------------------------------
  SUBROUTINE calbec_k ( npw, beta, psi, betapsi, ibnd_s, nbnd )
    !-----------------------------------------------------------------------
    !
    ! ... matrix times matrix with summation index (k=1,npw) running on
    ! ... G-vectors or PWs : betapsi(i,j) = \sum_k beta^*(i,k) psi(k,j)
    !
!    USE mp_bands, ONLY : intra_bgrp_comm
!    USE mp,       ONLY : mp_sum

    IMPLICIT NONE
    COMPLEX (SP), INTENT (in) :: beta(:,:), psi(:,:)
    COMPLEX (SP), INTENT (out) :: betapsi(:,:)
    INTEGER, INTENT (in) :: npw, ibnd_s
    INTEGER, OPTIONAL :: nbnd
    !
    INTEGER :: nkb, npwx, m
    !
    nkb = size (beta, 2)
    IF ( nkb == 0 ) RETURN
    !
!    CALL start_clock( 'calbec' )
    IF ( npw == 0 ) betapsi(:,:)=(0.0_SP,0.0_SP)
    npwx= size (beta, 1)
    IF ( npwx /= size (psi, 1) ) CALL errore ('calbec', 'size mismatch', 1)
    IF ( npwx < npw ) CALL errore ('calbec', 'size mismatch', 2)
    IF ( present (nbnd) ) THEN
        m = nbnd
    ELSE
        m = size ( psi, 2)
    ENDIF
#if defined(DEBUG)
    WRITE (*,*) 'calbec k'
    WRITE (*,*)  nkb,  size (betapsi,1) , m , size (betapsi, 2)
#endif
    IF ( nkb /= size (betapsi,1) .or. ibnd_s+m-1 > size (betapsi, 2) ) &
      CALL errore ('calbec', 'size mismatch', 3)
    !
    IF ( m == 1 ) THEN
       !
#if defined _DOUBLE
       CALL ZGEMV( 'C', npw, nkb, (1.0_DP,0.0_DP), beta, npwx, psi, 1, &
                   (0.0_DP, 0.0_DP), betapsi(:,ibnd_s:ibnd_s), 1 )
#else
       CALL CGEMV( 'C', npw, nkb, (1.0_SP,0.0_SP), beta, npwx, psi, 1, &
                   (0.0_SP, 0.0_SP), betapsi(:,ibnd_s:ibnd_s), 1 )
#endif
       !
    ELSE
       !
#if defined _DOUBLE
       CALL ZGEMM( 'C', 'N', nkb, m, npw, (1.0_DP,0.0_DP), &
                 beta, npwx, psi, npwx, (0.0_DP,0.0_DP), betapsi(:,ibnd_s:), nkb )
#else
       CALL CGEMM( 'C', 'N', nkb, m, npw, (1.0_SP,0.0_SP), &
                 beta, npwx, psi, npwx, (0.0_SP,0.0_SP), betapsi(:,ibnd_s:), nkb )
#endif
       !
    ENDIF
    !
!    CALL mp_sum( betapsi( :, 1:m ), intra_bgrp_comm )
!    CALL stop_clock( 'calbec' )
    !
    RETURN
    !
  END SUBROUTINE calbec_k
  !
  !-----------------------------------------------------------------------
  SUBROUTINE calbec_nc ( npw, beta, psi, betapsi, ibnd_s, nbnd )
    !-----------------------------------------------------------------------
    !
    ! ... matrix times matrix with summation index (k below) running on
    ! ... G-vectors or PWs corresponding to two different polarizations:
    ! ... betapsi(i,1,j) = \sum_k=1,npw beta^*(i,k) psi(k,j)
    ! ... betapsi(i,2,j) = \sum_k=1,npw beta^*(i,k) psi(k+npwx,j)
    !
!    USE mp_bands, ONLY : intra_bgrp_comm
!    USE mp,       ONLY : mp_sum

    IMPLICIT NONE
    COMPLEX (SP), INTENT (in) :: beta(:,:), psi(:,:)
    COMPLEX (SP), INTENT (out) :: betapsi(:,:,:)
    INTEGER, INTENT (in) :: npw, ibnd_s
    INTEGER, OPTIONAL :: nbnd
    !
    INTEGER :: nkb, npwx, npol, m
    !
    nkb = size (beta, 2)
    IF ( nkb == 0 ) RETURN
    !
!    CALL start_clock ('calbec')
    IF ( npw == 0 ) betapsi(:,:,:)=(0.0_SP,0.0_SP)
    npwx= size (beta, 1)
    IF ( 2*npwx /= size (psi, 1) ) CALL errore ('calbec', 'size mismatch', 1)
    IF ( npwx < npw ) CALL errore ('calbec', 'size mismatch', 2)
    IF ( present (nbnd) ) THEN
        m = nbnd
    ELSE
        m = size ( psi, 2)
    ENDIF
    npol= size (betapsi, 2)
#if defined(DEBUG)
    WRITE (*,*) 'calbec nc'
    WRITE (*,*)  nkb,  size (betapsi,1) , m , size (betapsi, 3)
#endif
    IF ( nkb /= size (betapsi,1) .or. ibnd_s+m-1 > size (betapsi, 3) ) &
      CALL errore ('calbec', 'size mismatch', 3)
    !
#if defined _DOUBLE
    CALL ZGEMM ('C', 'N', nkb, m*npol, npw, (1.0_DP, 0.0_DP), beta, &
              npwx, psi, npwx, (0.0_DP, 0.0_DP),  betapsi(:,:,ibnd_s:), nkb)
#else
    CALL CGEMM ('C', 'N', nkb, m*npol, npw, (1.0_SP, 0.0_SP), beta, &
              npwx, psi, npwx, (0.0_SP, 0.0_SP),  betapsi(:,:,ibnd_s:), nkb)
#endif
    !
!    CALL mp_sum( betapsi( :, :, 1:m ), intra_bgrp_comm )
!    CALL stop_clock( 'calbec' )
    !
    RETURN
    !
  END SUBROUTINE calbec_nc
  !
  !
  !-----------------------------------------------------------------------
  FUNCTION is_allocated_bec_type (bec) RESULT (isalloc)
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    TYPE (bec_type) :: bec
    LOGICAL :: isalloc
    isalloc = bec%alloc
    RETURN
    !
    !-----------------------------------------------------------------------
  END FUNCTION is_allocated_bec_type
  !-----------------------------------------------------------------------
  !
  !-----------------------------------------------------------------------
  SUBROUTINE allocate_bec_type ( nkb, nbnd, bec )
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    TYPE (bec_type) :: bec
    INTEGER, INTENT (in) :: nkb, nbnd
    INTEGER :: ierr, nbnd_siz
!    INTEGER, INTENT (in), OPTIONAL :: comm
!    INTEGER, EXTERNAL :: ldim_block, gind_block
    !
    nbnd_siz = nbnd
!    bec%comm = mp_get_comm_null()
    bec%comm = 0
    bec%nkb = nkb
    bec%nbnd = nbnd
    bec%mype = 0
    bec%nproc = 1
    bec%nbnd_loc = nbnd
    bec%ibnd_begin = 1
    bec%alloc=.true.
    !
!    IF( PRESENT( comm ) .AND. gamma_only .AND. smallmem ) THEN
!       bec%comm = comm
!       bec%nproc = mp_size( comm )
!       IF( bec%nproc > 1 ) THEN
!          nbnd_siz   = nbnd / bec%nproc
!          IF( MOD( nbnd, bec%nproc ) /= 0 ) nbnd_siz = nbnd_siz + 1
!          bec%mype  = mp_rank( bec%comm )
!          bec%nbnd_loc   = ldim_block( becp%nbnd , bec%nproc, bec%mype )
!          bec%ibnd_begin = gind_block( 1,  becp%nbnd, bec%nproc, bec%mype )
!       END IF
!    END IF
    !
    IF ( gamma_only ) THEN
       !
       ALLOCATE( bec%r( nkb, nbnd_siz ), STAT=ierr )
       IF( ierr /= 0 ) &
          CALL errore( ' allocate_bec_type ', ' cannot allocate bec%r ', ABS(ierr) )
       !
       bec%r(:,:)=0.0_SP
       !
    ELSEIF ( noncolin) THEN
       !
       ALLOCATE( bec%nc( nkb, npol, nbnd_siz ), STAT=ierr )
       IF( ierr /= 0 ) &
          CALL errore( ' allocate_bec_type ', ' cannot allocate bec%nc ', ABS(ierr) )
       !
       bec%nc(:,:,:)=(0.0_SP,0.0_SP)
       !
    ELSE
       !
       ALLOCATE( bec%k( nkb, nbnd_siz ), STAT=ierr )
       IF( ierr /= 0 ) &
          CALL errore( ' allocate_bec_type ', ' cannot allocate bec%k ', ABS(ierr) )
       !
       bec%k(:,:)=(0.0_SP,0.0_SP)
       !
    ENDIF
    !
    allocate(bec%init(nbnd_siz), STAT=ierr)
    if (ierr/=0) CALL errore( ' allocate_bec_type ', ' cannot allocate bec%init ', ABS(ierr) )
    bec%init(:)=.FALSE.
    !
    RETURN
    !
  END SUBROUTINE allocate_bec_type
  !
  !-----------------------------------------------------------------------
  SUBROUTINE deallocate_bec_type (bec)
    !-----------------------------------------------------------------------
    !
!    USE mp, ONLY: mp_get_comm_null
    IMPLICIT NONE
    TYPE (bec_type) :: bec
    !
!    bec%comm = mp_get_comm_null()
    bec%comm = 0
    bec%nbnd = 0
    bec%nkb = 0
    bec%alloc=.false.
    !
    IF (allocated(bec%r))  DEALLOCATE(bec%r)
    IF (allocated(bec%nc)) DEALLOCATE(bec%nc)
    IF (allocated(bec%k))  DEALLOCATE(bec%k)
    IF (allocated(bec%init))  DEALLOCATE(bec%init)
    !
    RETURN
    !
  END SUBROUTINE deallocate_bec_type

  SUBROUTINE beccopy(bec_in, bec_out)
    IMPLICIT NONE
    TYPE(bec_type), INTENT(in) :: bec_in
    TYPE(bec_type)  :: bec_out
    integer :: nkb, nbnd

    nkb=bec_in%nkb
    nbnd=bec_in%nbnd
    if (bec_out%alloc) call deallocate_bec_type(bec_out)
    call allocate_bec_type(nkb, nbnd, bec_out)
    !
#if defined _DOUBLE
    IF (gamma_only) THEN
       CALL DCOPY(nkb*nbnd, bec_in%r, 1, bec_out%r, 1)
    ELSEIF (noncolin) THEN
       CALL ZCOPY(nkb*npol*nbnd, bec_in%nc, 1, bec_out%nc,  1)
    ELSE
       CALL ZCOPY(nkb*nbnd, bec_in%k, 1, bec_out%k, 1)
    ENDIF
#else
    IF (gamma_only) THEN
       CALL SCOPY(nkb*nbnd, bec_in%r, 1, bec_out%r, 1)
    ELSEIF (noncolin) THEN
       CALL CCOPY(nkb*npol*nbnd, bec_in%nc, 1, bec_out%nc,  1)
    ELSE
       CALL CCOPY(nkb*nbnd, bec_in%k, 1, bec_out%k, 1)
    ENDIF
#endif
    !
    bec_out%init(:)=bec_in%init(:)
    !
    RETURN
  END SUBROUTINE beccopy

  SUBROUTINE becscal_nck(alpha, bec, nkb, nbnd)
    IMPLICIT NONE
    TYPE(bec_type), INTENT(INOUT) :: bec
    COMPLEX(SP), INTENT(IN) :: alpha
    INTEGER, INTENT(IN) :: nkb, nbnd

#if defined _DOUBLE
    IF (gamma_only) THEN
       CALL errore('becscal_nck','called in the wrong case',1)
    ELSEIF (noncolin) THEN
       CALL ZSCAL(nkb*npol*nbnd, alpha, bec%nc, 1)
    ELSE
       CALL ZSCAL(nkb*nbnd, alpha, bec%k, 1)
    ENDIF
#else
    IF (gamma_only) THEN
       CALL errore('becscal_nck','called in the wrong case',1)
    ELSEIF (noncolin) THEN
       CALL CSCAL(nkb*npol*nbnd, alpha, bec%nc, 1)
    ELSE
       CALL CSCAL(nkb*nbnd, alpha, bec%k, 1)
    ENDIF
#endif

    RETURN
  END SUBROUTINE becscal_nck

  SUBROUTINE becscal_gamma(alpha, bec, nkb, nbnd)
    IMPLICIT NONE
    TYPE(bec_type), INTENT(INOUT) :: bec
    REAL(SP), INTENT(IN) :: alpha
    INTEGER, INTENT(IN) :: nkb, nbnd

    IF (gamma_only) THEN
#if defined _DOUBLE
       CALL DSCAL(nkb*nbnd, alpha, bec%r, 1)
#else
       CALL SSCAL(nkb*nbnd, alpha, bec%r, 1)
#endif
    ELSE
       CALL errore('becscal_gamma','called in the wrong case',1)
    ENDIF

    RETURN
  END SUBROUTINE becscal_gamma

END MODULE becmod
