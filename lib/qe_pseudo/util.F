!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
! <INFO>
!*********************************************
  MODULE util_module
  !*********************************************
  !
  USE kinds
  USE constants, ONLY : ZERO, ONE
  !
  use pars,      ONLY:cZERO,cONE,cI
  !
#if defined(__CUDA) && defined(__PHIGEMM)
  USE phigemm,            ONLY : DGEMM => phidgemm , ZGEMM => phizgemm
#endif
!
#if defined(__CUDA) && defined(__MAGMA)
  USE magma,              ONLY : DGESVD => magmaf_dgesvd, &
                                 ZGESVD => magmaf_zgesvd, &
                                 DGESV  => magmaf_dgesv,  &
                                 !ZGBSV  => magmaf_zgbsv,  &
                                 !DGELSD => magmaf_dgelsd, &
                                 ZGEEV  => magmaf_zgeev,  &
                                 !ZHPEVX => magmaf_zhpevx, &
                                 !DSPEVX => magmaf_dspevx, &
                                 !DSBEVX => magmaf_dsbevx, &
                                 DGETRF => magmaf_dgetrf, &
                                 !DGETRI => magmaf_dgetri, &
                                 ZGETRF => magmaf_zgetrf!, &
                                 !ZGETRI => magmaf_zgetri
  USE iso_c_binding
!  USE cuda_mem_alloc
#endif
  !
  IMPLICIT NONE
  PRIVATE

! General purpose utilities
!
! routines in this module:
! SUBROUTINE  zmat_pack( zp, z, n)
! SUBROUTINE  zmat_unpack( z, zp, n)
! SUBROUTINE   mat_bnd_pack( zb, z, n, kl, ku)
! SUBROUTINE   mat_bnd_unpack( z, zb, n, kl, ku)
! SUBROUTINE   mat_bnd_getdims( z, thr, n, kl, ku)
! SUBROUTINE   mat_herm( z, n)
! SUBROUTINE   mat_antiherm( z, n)
! SUBROUTINE   mat_svd( m, n, a, s, u, vt)
! SUBROUTINE   mat_sv ( n, nrhs, a, b [,ierr])
! SUBROUTINE   mat_lsd( m, n, nrhs, a, b, rcond [, sv, rank, ierr])
! SUBROUTINE   mat_mul( c, a, opa, b, opb, m, n, k)
! SUBROUTINE   mat_hdiag( z, w, a, n[, uplo][, il, iu])
! SUBROUTINE   mat_inv( n, a, z [, kl, ku, ldab] [,det_a] [,ierr] )
! SUBROUTINE   mat_hsqrt( n, a, [,ldab, ka] z [,ierr] )
! SUBROUTINE  zmat_diag( z, w, a, n, side)
! COMPLEX/REAL FUNCTION   mat_dotp( m, n, a, b)
! COMPLEX/REAL FUNCTION   mat_hdotp( m, a, b)
! LOGICAL FUNCTION  zmat_is_unitary( m, n, z [,side] [,toll])
! LOGICAL FUNCTION   mat_is_herm( n, z [,toll])
! INTEGER FUNCTION   mat_rank( m, n, a, toll)
! 
! </INFO>
!

!
! banded matrix tools
INTERFACE mat_bnd_getdims
   MODULE PROCEDURE zmat_bnd_getdims
   MODULE PROCEDURE dmat_bnd_getdims
END INTERFACE
INTERFACE mat_bnd_pack
   MODULE PROCEDURE zmat_bnd_pack
   MODULE PROCEDURE dmat_bnd_pack
   MODULE PROCEDURE dmat_hbnd_pack
END INTERFACE
INTERFACE mat_bnd_unpack
   MODULE PROCEDURE zmat_bnd_unpack
   MODULE PROCEDURE dmat_bnd_unpack
   MODULE PROCEDURE dmat_hbnd_unpack
END INTERFACE
!
! hermitian, antohermitian parts
INTERFACE mat_herm
   MODULE PROCEDURE zmat_herm
   MODULE PROCEDURE dmat_herm
END INTERFACE
INTERFACE mat_antiherm
   MODULE PROCEDURE zmat_antiherm
END INTERFACE
!
INTERFACE mat_is_herm
   MODULE PROCEDURE zmat_is_herm
   MODULE PROCEDURE dmat_is_herm
END INTERFACE
!
! matrix multiplication
INTERFACE mat_mul
   MODULE PROCEDURE zmat_mul
   !MODULE PROCEDURE zmat_mul_1
   MODULE PROCEDURE dmat_mul
   MODULE PROCEDURE dmat_mul_1
END INTERFACE
!
! singular value decomposition
INTERFACE mat_svd
   MODULE PROCEDURE zmat_svd
   MODULE PROCEDURE dmat_svd
END INTERFACE
!
! simple linear system solver
INTERFACE mat_sv
   MODULE PROCEDURE zmat_sv
   MODULE PROCEDURE zmat_sv_1
   MODULE PROCEDURE dmat_sv
   MODULE PROCEDURE dmat_sv_1
END INTERFACE
!
INTERFACE mat_lsd
   MODULE PROCEDURE dmat_lsd
   MODULE PROCEDURE dmat_lsd_1
   MODULE PROCEDURE zmat_lsd
   MODULE PROCEDURE zmat_lsd_1
END INTERFACE
!
! rank calculation
INTERFACE mat_rank
   MODULE PROCEDURE zmat_rank
   MODULE PROCEDURE dmat_rank
END INTERFACE
!
! matrix diagonalization
INTERFACE mat_hdiag
   MODULE PROCEDURE zmat_hdiag
   MODULE PROCEDURE zmat_hdiag_pack
   MODULE PROCEDURE zmat_hdiag_gen
   MODULE PROCEDURE dmat_hdiag
   MODULE PROCEDURE dmat_bnd_hdiag
   MODULE PROCEDURE dmat_bnd_hdiag_ext
END INTERFACE
!
! matrix inversion
INTERFACE mat_inv
   MODULE PROCEDURE zmat_inv
   MODULE PROCEDURE zmat_bnd_inv
   MODULE PROCEDURE dmat_inv
   MODULE PROCEDURE dmat_bnd_inv
END INTERFACE
!
! matrix sqrt
INTERFACE mat_hsqrt
   MODULE PROCEDURE zmat_hsqrt
   MODULE PROCEDURE dmat_bnd_hsqrt
   MODULE PROCEDURE dmat_hsqrt
END INTERFACE
!
! matrix dot product
INTERFACE  mat_dotp
   MODULE PROCEDURE zmat_ge_dotp
   MODULE PROCEDURE dmat_ge_dotp
END INTERFACE
!
! matrix herm dot product
INTERFACE  mat_hdotp
   MODULE PROCEDURE zmat_hp_dotp
   MODULE PROCEDURE zmat_he_dotp
   MODULE PROCEDURE dmat_sy_dotp
END INTERFACE


PUBLIC :: zmat_pack, zmat_unpack
PUBLIC ::  mat_bnd_pack,  mat_bnd_unpack
PUBLIC ::  mat_bnd_getdims
PUBLIC ::  mat_herm
PUBLIC ::  mat_antiherm
PUBLIC ::  mat_svd
PUBLIC ::  mat_sv
PUBLIC ::  mat_lsd
PUBLIC ::  mat_mul
PUBLIC ::  mat_hdiag
PUBLIC ::  mat_inv
PUBLIC ::  mat_hsqrt
PUBLIC :: zmat_diag
PUBLIC :: zmat_is_unitary
PUBLIC ::  mat_is_herm
PUBLIC ::  mat_rank
PUBLIC ::  mat_hdotp
PUBLIC ::  mat_dotp

CONTAINS

!
! Subroutines
!

!**********************************************************
   SUBROUTINE zmat_pack( zp, z, n )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(DP), INTENT(OUT) :: zp(:)
    COMPLEX(DP), INTENT(IN)  :: z(:,:)
    INTEGER,      INTENT(IN)  :: n
    !
    INTEGER :: i, j, ind

    ind = 1
    DO j = 1, n
      DO i = 1, n
        zp(ind) = z(i,j)
        ind = ind + 1
      END DO
    END DO

    RETURN
  END SUBROUTINE


!**********************************************************
   SUBROUTINE zmat_unpack( z, zp, n )
   !**********************************************************
   IMPLICIT NONE
   COMPLEX(DP), INTENT(IN)  :: zp(:)
   COMPLEX(DP), INTENT(OUT) :: z(:,:)
   INTEGER,      INTENT(IN)  :: n
   INTEGER :: i, j, ind

   ind = 1
   DO j = 1, n
     DO i = 1, n
       z(i,j) = zp(ind)
       ind = ind + 1
     END DO
   END DO

   RETURN
END SUBROUTINE


!**********************************************************
   SUBROUTINE zmat_bnd_pack( zb, z, n, kl, ku )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(DP), INTENT(OUT)  :: zb(:,:)
    COMPLEX(DP), INTENT(IN)   :: z(:,:)
    INTEGER,      INTENT(IN)   :: n
    INTEGER,      INTENT(IN)   :: kl, ku
    !
    ! kl :   # of subdiagonals
    ! ku :   # of supradiagonals
    ! ldzb must be >= 2*kl + ku +1
    !
    INTEGER :: i, j, ind

    IF ( SIZE(zb,1) < 2 *kl + ku + 1 ) CALL errore('zmat_bnd_pack','invalid ldzb',10) 
    !
    zb(:,:) = CZERO
    !
    DO j = 1, n
    DO i = MAX(1,j-ku),MIN(n,j+kl)
        ind = kl+ku+1+i-j
        zb(ind,j) = z(i,j)
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE zmat_bnd_pack

    
!**********************************************************
   SUBROUTINE zmat_bnd_unpack( z, zb, n, kl, ku )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(DP), INTENT(OUT) :: z(:,:)
    COMPLEX(DP), INTENT(IN)  :: zb(:,:)
    INTEGER,      INTENT(IN)  :: n
    INTEGER,      INTENT(IN)  :: kl, ku
    !
    ! kl :   # of subdiagonals
    ! ku :   # of supradiagonals
    ! ldzb must be >= 2*kl + ku +1
    !
    INTEGER :: i, j, ind

    IF ( SIZE(zb,1) < 2 *kl + ku + 1 ) CALL errore('zmat_bnd_unpack','invalid ldzb',10) 
    !
    z(:,:) = CZERO
    !
    DO j = 1, n
    DO i = MAX(1,j-ku),MIN(n,j+kl)
        ind = kl+ku+1+i-j
        z(i,j) = zb(ind,j)
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE zmat_bnd_unpack


!**********************************************************
   SUBROUTINE dmat_bnd_pack( zb, z, n, kl, ku )
   !**********************************************************
    IMPLICIT NONE
    REAL(DP),    INTENT(OUT)  :: zb(:,:)
    REAL(DP),    INTENT(IN)   :: z(:,:)
    INTEGER,      INTENT(IN)   :: n
    INTEGER,      INTENT(IN)   :: kl, ku
    !
    ! kl :   # of subdiagonals
    ! ku :   # of supradiagonals
    ! ldzb must be >= 2*kl + ku +1
    !
    INTEGER :: i, j, ind

    IF ( SIZE(zb,1) < 2 *kl + ku + 1 ) CALL errore('dmat_bnd_pack','invalid ldzb',10) 
    !
    zb(:,:) = ZERO
    !
    DO j = 1, n
    DO i = MAX(1,j-ku),MIN(n,j+kl)
        ind = kl+ku+1+i-j
        zb(ind,j) = z(i,j)
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE dmat_bnd_pack

    
!**********************************************************
   SUBROUTINE dmat_bnd_unpack( z, zb, n, kl, ku )
   !**********************************************************
    IMPLICIT NONE
    REAL(DP),    INTENT(OUT) :: z(:,:)
    REAL(DP),    INTENT(IN)  :: zb(:,:)
    INTEGER,      INTENT(IN)  :: n
    INTEGER,      INTENT(IN)  :: kl, ku
    !
    ! kl :   # of subdiagonals
    ! ku :   # of supradiagonals
    ! ldzb must be >= 2*kl + ku +1
    !
    INTEGER :: i, j, ind

    IF ( SIZE(zb,1) < 2 *kl + ku + 1 ) CALL errore('dmat_bnd_unpack','invalid ldzb',10) 
    !
    z(:,:) = ZERO
    !
    DO j = 1, n
    DO i = MAX(1,j-ku),MIN(n,j+kl)
        ind = kl+ku+1+i-j
        z(i,j) = zb(ind,j)
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE dmat_bnd_unpack


!**********************************************************
   SUBROUTINE dmat_hbnd_pack( zb, z, n, kd )
   !**********************************************************
    IMPLICIT NONE
    REAL(DP),    INTENT(OUT)  :: zb(:,:)
    REAL(DP),    INTENT(IN)   :: z(:,:)
    INTEGER,      INTENT(IN)   :: n
    INTEGER,      INTENT(IN)   :: kd
    !
    ! kd :   # of supradiagonals
    !
    INTEGER :: i, j, ind

    IF ( SIZE(zb,1) < kd+1 ) CALL errore('dmat_hbnd_pack','invalid ldzb',10) 
    !
    zb(:,:) = ZERO
    !
    DO j = 1, n
    DO i = MAX(1,j-kd),j
        ind = kd+1+i-j
        zb(ind,j) = z(i,j)
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE dmat_hbnd_pack

    
!**********************************************************
   SUBROUTINE dmat_hbnd_unpack( z, zb, n, kd )
   !**********************************************************
    IMPLICIT NONE
    REAL(DP),    INTENT(OUT) :: z(:,:)
    REAL(DP),    INTENT(IN)  :: zb(:,:)
    INTEGER,      INTENT(IN)  :: n
    INTEGER,      INTENT(IN)  :: kd
    !
    ! kd :   # of supradiagonals
    !
    INTEGER :: i, j, ind

    IF ( SIZE(zb,1) < kd + 1 ) CALL errore('dmat_hbnd_unpack','invalid ldzb',10) 
    !
    z(:,:) = ZERO
    !
    DO j = 1, n
    DO i = MAX(1,j-kd),j
        ind = kd+1+i-j
        z(i,j) = zb(ind,j)
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE dmat_hbnd_unpack


!**********************************************************
   SUBROUTINE zmat_bnd_getdims( a, thr, n, kl, ku )
   !**********************************************************
    IMPLICIT NONE
    COMPLEX(DP), INTENT(IN)   :: a(:,:)
    REAL(DP),    INTENT(IN)   :: thr
    INTEGER,      INTENT(IN)   :: n
    INTEGER,      INTENT(OUT)  :: kl, ku
    !
    ! kl :   # of subdiagonals
    ! ku :   # of supradiagonals
    !
    INTEGER :: i, j
    !
    kl = 0
    ku = 0
    !
    DO j = 1, n
    DO i = 1, j-1
        IF ( REAL( a(i,j)*CONJG(a(i,j) ) ) > thr .AND. &
             j-i > kl ) kl = j-i  
    ENDDO
    ENDDO

    DO j = 1, n
    DO i = j+1, n
        IF ( REAL( a(i,j)*CONJG(a(i,j) ) ) > thr .AND. &
             i-j > ku ) ku = i-j
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE zmat_bnd_getdims


!**********************************************************
   SUBROUTINE dmat_bnd_getdims( a, thr, n, kl, ku )
   !**********************************************************
    IMPLICIT NONE
    REAL(DP),    INTENT(IN)   :: a(:,:)
    REAL(DP),    INTENT(IN)   :: thr
    INTEGER,      INTENT(IN)   :: n
    INTEGER,      INTENT(OUT)  :: kl, ku
    !
    ! kl :   # of subdiagonals
    ! ku :   # of supradiagonals
    !
    INTEGER :: i, j
    !
    kl = 0
    ku = 0
    !
    DO j = 1, n
    DO i = 1, j-1
        IF ( a(i,j)*a(i,j)  > thr .AND. &
             j-i > kl ) kl = j-i  
    ENDDO
    ENDDO

    DO j = 1, n
    DO i = j+1, n
        IF ( a(i,j)*a(i,j) > thr .AND. &
             i-j > ku ) ku = i-j
    ENDDO
    ENDDO

    RETURN
  END SUBROUTINE dmat_bnd_getdims


!**********************************************************
   SUBROUTINE zmat_herm( z, n )
   !**********************************************************
   !
   ! overwrite the input matrix with its hermitean part
   !
   IMPLICIT NONE
   COMPLEX(DP), INTENT(INOUT) :: z(:,:)
   INTEGER,      INTENT(IN)    :: n
   INTEGER :: i, j

   DO j = 1, n
   DO i = j, n
       z(i,j) = 0.5_DP * ( z(i,j) + CONJG( z(j,i) ) )
       z(j,i) = CONJG( z(i,j) )
   ENDDO
   ENDDO

   RETURN
END SUBROUTINE zmat_herm


!**********************************************************
   SUBROUTINE dmat_herm( z, n )
   !**********************************************************
   !
   ! overwrite the input matrix with its hermitean part
   !
   IMPLICIT NONE
   REAL(DP),    INTENT(INOUT) :: z(:,:)
   INTEGER,      INTENT(IN)    :: n
   INTEGER :: i, j

   DO j = 1, n
   DO i = j, n
       z(i,j) = 0.5_DP * ( z(i,j) + z(j,i) )
       z(j,i) = z(i,j)
   ENDDO
   ENDDO

   RETURN
END SUBROUTINE dmat_herm


!**********************************************************
   SUBROUTINE zmat_antiherm( z, n )
   !**********************************************************
   !
   ! overwrite the input matrix with its antihermitean part
   !
   IMPLICIT NONE
   COMPLEX(DP), INTENT(INOUT) :: z(:,:)
   INTEGER,      INTENT(IN)    :: n
   INTEGER :: i, j

   DO j = 1, n
   DO i = j, n
       z(i,j) = -CI * 0.5_DP * ( z(i,j) - CONJG( z(j,i) ) )
       z(j,i) = CONJG( z(i,j) )
   ENDDO
   ENDDO

   RETURN
END SUBROUTINE


!**********************************************************
   SUBROUTINE dmat_svd(m, n, a, s, u, vt)
   !**********************************************************
   !
   !  computes the singular value decomposition (SVD) of a REAL(DP)
   !  M-by-N matrix A. The SVD is written
   !
   !       A = U * SIGMA * transpose(V)
   !
   !  where SIGMA is an M-by-N matrix which is zero except for its
   !  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
   !  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
   !  are the singular values of A; they are real and non-negative, and
   !  are returned in descending order. 
   !  Note that the routine returns V**T, not V.
   !
   IMPLICIT NONE
   INTEGER,   INTENT(IN)  :: m, n
   REAL(DP), INTENT(IN)  :: a(:,:)
   REAL(DP), INTENT(OUT) :: s(:)
   REAL(DP), INTENT(OUT) :: u(:,:), vt(:,:)

   INTEGER   :: ierr, info, lwork
   !REAL(DP) :: raux 
   REAL(DP), ALLOCATABLE :: atmp(:,:), work(:)

   IF ( m <= 0 .OR. n<=0 ) CALL errore('dmat_svd','Invalid DIMs',1)
   IF ( m > SIZE(a,1) .OR. m > SIZE(u,1) .OR. m > SIZE(u,2) ) &
           CALL errore('dmat_svd','m too large',m)
   IF ( n > SIZE(a,2) .OR. n > SIZE(vt,1) .OR. n > SIZE(vt,2) ) &
           CALL errore('dmat_svd','n too large',n)
   IF ( SIZE(s) < MIN(m,n) ) CALL errore('dmat_svd','s dimension too small',1)

   !
   ! allocate local variables and workspace
   !
   ALLOCATE( atmp(m,n), STAT=ierr )
   IF (ierr/=0)  CALL errore('dmat_svd','allocating atmp',ABS(ierr))
   !
   ! save A (which is intent IN)
   atmp(:,:) = a(1:m, 1:n)

   !
   ! get lwork
   !
   lwork = MAX( 3*MIN(m,n) + MAX(m,n), 5*MIN(m,n) )
   !
!   lwork = -1
!   !
!   CALL DGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
!                raux, lwork, info)
!   !
!   lwork = NINT( raux )
!   !
   ALLOCATE( work(lwork), STAT=ierr )
   IF (ierr/=0)  CALL errore('dmat_svd','allocating work',ABS(ierr))


   ! use magma if possible
   CALL DGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
                work, lwork, info)

   IF ( info < 0 ) CALL errore('dmat_svd', 'DGESVD: info illegal value', -info )
   IF ( info > 0 ) CALL errore('dmat_svd', 'DGESVD: DBESQR not converged', info )
    
   DEALLOCATE( atmp, work, STAT=ierr)
   IF(ierr/=0) CALL errore('dmat_svd','deallocating atpm, work',ABS(ierr))

   RETURN
END SUBROUTINE dmat_svd


!**********************************************************
   SUBROUTINE zmat_svd(m, n, a, s, u, vt)
   !**********************************************************
   !
   !  computes the singular value decomposition (SVD) of a complex
   !  M-by-N matrix A. The SVD is written
   !
   !       A = U * SIGMA * conjugate-transpose(V)
   !
   !  where SIGMA is an M-by-N matrix which is zero except for its
   !  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
   !  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
   !  are the singular values of A; they are real and non-negative, and
   !  are returned in descending order. 
   !  Note that the routine returns V**H, not V.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)       :: m, n
   COMPLEX(DP), INTENT(IN)  :: a(:,:)
   REAL(DP),    INTENT(OUT) :: s(:)
   COMPLEX(DP), INTENT(OUT) :: u(:,:), vt(:,:)

   INTEGER    :: ierr, info, lwork
   !REAL(DP)  :: raux
   REAL(DP),    ALLOCATABLE :: rwork(:)
   COMPLEX(DP), ALLOCATABLE :: atmp(:,:), work(:)

   IF ( m <= 0 .OR. n<=0 ) CALL errore('zmat_svd','Invalid DIMs',1)
   IF ( m > SIZE(a,1) .OR. m > SIZE(u,1) .OR. m > SIZE(u,2) ) &
           CALL errore('zmat_svd','m too large',m)
   IF ( n > SIZE(a,2) .OR. n > SIZE(vt,1) .OR. n > SIZE(vt,2) ) &
           CALL errore('zmat_svd','n too large',n)
   IF ( SIZE(s) < MIN(m,n) ) CALL errore('zmat_svd','s dimension too small',1)

   !
   ! allocate local variables and workspace
   !
   ALLOCATE( atmp(m,n), STAT=ierr )
   IF (ierr/=0)  CALL errore('zmat_svd','allocating atmp',ABS(ierr))
   !
   ALLOCATE( rwork(5 * MIN(m,n) ), STAT=ierr )
   IF (ierr/=0)  CALL errore('zmat_svd','allocating rwork',ABS(ierr))
   !
   ! save A (which is intent IN)
   atmp(:,:) = a(1:m, 1:n)
   
   !
   ! determine lwork
   !
   lwork = 2 * MIN(m,n) + MAX(m,n)
!   !
!   lwork = -1
!   !
!   CALL ZGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
!                raux, lwork, rwork, info)
!   !
!   lwork = NINT( raux )
!   ! 
   ALLOCATE( work(lwork), STAT=ierr )
   IF (ierr/=0)  CALL errore('zmat_svd','allocating work',ABS(ierr))


   ! use magma if possible
   CALL ZGESVD('A','A', m, n, atmp, m, s, u, SIZE(u,1), vt, SIZE(vt,1), &
                work, lwork, rwork, info)

   IF ( info < 0 ) CALL errore('zmat_svd', 'ZGESVD: info illegal value', -info )
   IF ( info > 0 ) CALL errore('zmat_svd', 'ZGESVD: ZBESQR not converged', info )
    
   DEALLOCATE( atmp, work, rwork, STAT=ierr)
   IF(ierr/=0) CALL errore('zmat_svd','deallocating atmp, work, rwork',ABS(ierr))

   RETURN
END SUBROUTINE zmat_svd


!**********************************************************
   SUBROUTINE dmat_sv(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   !  Computes the solution of the real system of linear equations
   !     A * X = B,
   !  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The LU decomposition with partial pivoting and row interchanges is
   !  used to factor A as
   !     A = P * L * U,
   !  where P is a permutation matrix, L is unit lower triangular, and U is
   !  upper triangular.  The factored form of A is then used to solve the
   !  system of equations A * X = B.
   !
   IMPLICIT NONE
   INTEGER,   INTENT(IN)       :: n, nrhs
   REAL(DP), INTENT(IN)       :: a(:,:)
   REAL(DP), INTENT(INOUT)    :: b(:,:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr

   INTEGER :: ierr_, info
   INTEGER,      ALLOCATABLE :: ipiv(:)
   REAL(DP), ALLOCATABLE    :: atmp(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore('dmat_sv','matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore('dmat_sv','matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore('dmat_sv','matrix B too small (II)',1)

   ALLOCATE( atmp(n,n), ipiv(n), STAT=ierr_ )
     IF (ierr_/=0) CALL errore('dmat_sv','allocating atmp, ipiv',ABS(ierr_))
   !
   ! make a local copy of a
   atmp(:,:) = a(1:n,1:n) 

   ! use magma if possible
   CALL DGESV( n, nrhs, atmp, n, ipiv, b, SIZE(b,1), info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore('dmat_sv', 'DGESV: info illegal value', -info )
        IF ( info > 0 ) CALL errore('dmat_sv', 'DGESV: singular matrix', info )
   ENDIF
    
   DEALLOCATE( atmp, ipiv, STAT=ierr_)
      IF(ierr_/=0) CALL errore('dmat_sv','deallocating atmp, ipiv',ABS(ierr_))

   RETURN
END SUBROUTINE dmat_sv


!**********************************************************
   SUBROUTINE dmat_sv_1(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   ! Interface to dmat_sv when nrhs = 1
   !
   IMPLICIT NONE
   INTEGER,         INTENT(IN)    :: n, nrhs
   REAL(DP),       INTENT(IN)    :: a(:,:)
   REAL(DP),       INTENT(INOUT) :: b(:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr
   !
   INTEGER  :: ierr_, info
   REAL(DP),   ALLOCATABLE :: bl(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( nrhs /= 1) CALL errore('dmat_sv_1','more than 1 rhs ? ',ABS(nrhs)+1)
   IF ( n > SIZE(b,1) ) CALL errore('dmat_sv_1','vector b too small',2)
   !
   ALLOCATE( bl(n,1), STAT=ierr_ )
   IF (ierr_/=0) CALL errore('dmat_sv_1','allocating bl',ABS(ierr_))

   bl(:,1) = b(1:n)
   CALL dmat_sv( n, 1, a, bl, IERR=info)
   !
   IF ( PRESENT(ierr) ) THEN 
       ierr=info
   ELSE
       IF ( info /=0 ) CALL errore('dmat_sv_1','info/=0',ABS(info))
   ENDIF
   !
   b(1:n) = bl(1:n,1)

   DEALLOCATE( bl, STAT=ierr_)
   IF (ierr_/=0) CALL errore('dmat_sv_1','deallocating bl',ABS(ierr_))

   RETURN
END SUBROUTINE dmat_sv_1


!**********************************************************
   SUBROUTINE dmat_lsd(m, n, nrhs, a, b, rcond, sv, rank, ierr)
   !**********************************************************
   !
   !  Computes the minimum-norm solution of the real system of linear equations
   !     A * X = B,
   !  where A is an M-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The solutions found are:   min 2-norm| B-AX |
   !
   !  The SVD method is used, see the manual of DGELSD
   !  Singular values and rank can be provided in output
   !
   !  Only square matrices are treated
   !
   IMPLICIT NONE
   INTEGER,   INTENT(IN)       :: m, n, nrhs
   REAL(DP), INTENT(IN)       :: rcond
   REAL(DP), INTENT(IN)       :: a(:,:)
   REAL(DP), INTENT(INOUT)    :: b(:,:)
   REAL(DP), OPTIONAL, INTENT(OUT) :: sv(:)
   INTEGER,   OPTIONAL, INTENT(OUT) :: rank
   INTEGER,   OPTIONAL, INTENT(OUT) :: ierr
   !
   CHARACTER(8)              :: subname="dmat_lsd"
   INTEGER                   :: ierr_, rank_, info
   INTEGER,      ALLOCATABLE :: iwork(:)
   REAL(DP),    ALLOCATABLE :: atmp(:,:), work(:), sv_(:)
   !
   REAL(DP)         :: workl
   INTEGER           :: smlsiz, minmn, nlvl, lwork, liwork
   INTEGER, EXTERNAL :: ILAENV



   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( m > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore(subname,'matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore(subname,'matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore(subname,'matrix B too small (II)',1)

   !
   ! dimensions
   !
   minmn  = MIN(m,n)
   smlsiz = ILAENV( 9, 'DGELSD', ' ', 0, 0, 0, 0 )
   nlvl   = MAX( INT( LOG( DBLE( minmn ) / DBLE( smlsiz+1 ) ) /  &
            LOG( 2.0d0 ) ) + 1, 0 )
   !
   liwork = MAX(1, 3 * minmn * nlvl + 11 * minmn)
   !

   !
   ! workspace
   !
   ALLOCATE( atmp(m,n), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating atmp',ABS(ierr_))
   ALLOCATE( sv_(MIN(m,n)), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating sv_',ABS(ierr_))
   ALLOCATE( iwork(liwork), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating iwork',ABS(ierr_))
   !
   ! get lwork
   !
   info   =  0
   lwork  = -1
   !
   ! use magma if possible
   ! get proper dimensions here
   CALL DGELSD( m, n, nrhs, atmp, m, b, SIZE(b,1), sv_, rcond, rank_, &
                workl, lwork, iwork, info)
   !
   lwork  = INT( workl )
   !
   ALLOCATE( work(lwork), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating work',ABS(ierr_))

   !
   ! make a local copy of a
   atmp(:,:) = a(1:m,1:n) 

   !
   ! use magma if possible
   CALL DGELSD( m, n, nrhs, atmp, m, b, SIZE(b,1), sv_, rcond, rank_, &
                work, lwork, iwork, info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore(subname, 'DGELSD: info illegal value', -info )
        IF ( info > 0 ) CALL errore(subname, 'DGELSD: algorithm failure', info )
   ENDIF
   !
   IF ( PRESENT(sv) )     sv(1:minmn) = sv_(1:minmn)
   IF ( PRESENT(rank) )   rank        = rank_
    

   DEALLOCATE( atmp, STAT=ierr_)
   IF(ierr_/=0) CALL errore(subname,'deallocating atmp',ABS(ierr_))
   DEALLOCATE( sv_, STAT=ierr_)
   IF(ierr_/=0) CALL errore(subname,'deallocating atmp',ABS(ierr_))
   DEALLOCATE( work, iwork, STAT=ierr_)
   IF(ierr_/=0) CALL errore(subname,'deallocating work, iwork',ABS(ierr_))

   RETURN
END SUBROUTINE dmat_lsd


!**********************************************************
   SUBROUTINE dmat_lsd_1(m, n, nrhs, a, b, rcond, sv, rank, ierr)
   !**********************************************************
   !
   ! Interface to dmat_lsd when nrhs = 1
   !
   IMPLICIT NONE
   INTEGER,   INTENT(IN)       :: m, n, nrhs
   REAL(DP), INTENT(IN)       :: rcond
   REAL(DP), INTENT(IN)       :: a(:,:)
   REAL(DP), INTENT(INOUT)    :: b(:)
   REAL(DP), OPTIONAL, INTENT(OUT) :: sv(:)
   INTEGER,   OPTIONAL, INTENT(OUT) :: rank
   INTEGER,   OPTIONAL, INTENT(OUT) :: ierr
   !
   CHARACTER(10)             :: subname="dmat_lsd_1"
   INTEGER                   :: ierr_, rank_, info
   REAL(DP),    ALLOCATABLE :: bl(:,:)
   REAL(DP),    ALLOCATABLE :: sv_(:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( nrhs /= 1) CALL errore(subname,'more than 1 rhs ? ',ABS(nrhs)+1)
   IF ( n > SIZE(b,1) ) CALL errore(subname,'vector b too small',2)
   !
   ALLOCATE( bl(n,1), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating bl',ABS(ierr_))
   ALLOCATE( sv_(MIN(m,n)), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating sv_',ABS(ierr_))

   bl(:,1) = b(1:n)
   CALL dmat_lsd( m, n, 1, a, bl, rcond, SV=sv_, RANK=rank_, IERR=info)
   !
   IF ( PRESENT(ierr) ) THEN 
       ierr=info
   ELSE
       IF (info /=0) CALL errore(subname,'info/=0',ABS(info))
   ENDIF
   IF ( PRESENT(sv) )    sv(1:MIN(m,n))  = sv_(:)
   IF ( PRESENT(rank) )  rank            = rank_
   !
   b(1:n) = bl(1:n,1)

   DEALLOCATE( bl, STAT=ierr_)
   IF (ierr_/=0) CALL errore(subname,'deallocating bl',ABS(ierr_))
   DEALLOCATE( sv_, STAT=ierr_)
   IF (ierr_/=0) CALL errore(subname,'deallocating sv_',ABS(ierr_))

   RETURN
END SUBROUTINE dmat_lsd_1


!**********************************************************
   SUBROUTINE zmat_lsd(m, n, nrhs, a, b, rcond, sv, rank, ierr)
   !**********************************************************
   !
   !  Computes the minimum-norm solution of the real system of linear equations
   !     A * X = B,
   !  where A is an M-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The solutions found are:   min 2-norm| B-AX |
   !
   !  The SVD method is used, see the manual of ZGELSD
   !  Singular values and rank can be provided in output
   !
   !  Only square matrices are treated
   !
   IMPLICIT NONE
   INTEGER,      INTENT(IN)       :: m, n, nrhs
   REAL(DP),    INTENT(IN)       :: rcond
   COMPLEX(DP), INTENT(IN)       :: a(:,:)
   COMPLEX(DP), INTENT(INOUT)    :: b(:,:)
   REAL(DP),    OPTIONAL, INTENT(OUT) :: sv(:)
   INTEGER,      OPTIONAL, INTENT(OUT) :: rank
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   CHARACTER(8)              :: subname="zmat_lsd"
   INTEGER                   :: ierr_, rank_, info
   INTEGER,      ALLOCATABLE :: iwork(:)
   REAL(DP),    ALLOCATABLE :: rwork(:), sv_(:)
   COMPLEX(DP), ALLOCATABLE :: atmp(:,:), work(:)
   !
   REAL(DP)         :: rworkl
   COMPLEX(DP)      :: workl
   INTEGER           :: smlsiz, minmn, nlvl, lwork, lrwork, liwork
   INTEGER, EXTERNAL :: ILAENV



   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( m > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore(subname,'matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore(subname,'matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore(subname,'matrix B too small (II)',1)

   !
   ! dimensions
   !
   minmn  = MIN(m,n)
   smlsiz = ILAENV( 9, 'ZGELSD', ' ', 0, 0, 0, 0 )
   nlvl   = MAX( INT( LOG( DBLE( minmn ) / DBLE( smlsiz+1 ) ) /  &
            LOG( 2.0d0 ) ) + 1, 0 )
   !
   !mnthr  = ILAENV( 6, 'ZGELSD', ' ', m, n, nrhs, -1 )
   liwork = MAX(1, 3 * minmn * nlvl + 11 * minmn)
   !

   !
   ! workspace
   !
   ALLOCATE( atmp(m,n), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating atmp',ABS(ierr_))
   ALLOCATE( sv_(MIN(m,n)), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating sv_',ABS(ierr_))
   ALLOCATE( iwork(liwork), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating iwork',ABS(ierr_))
   !
   ! get lwork
   !
   info   =  0
   lwork  = -1
   !
   ! use magma if possible
   ! get proper dimensions here
   CALL ZGELSD( m, n, nrhs, atmp, m, b, SIZE(b,1), sv_, rcond, rank_, &
                workl, lwork, rworkl, iwork, info)
   !
   lwork  = INT( workl )
   lrwork = INT( rworkl )
   !
   ALLOCATE( work(lwork), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating work',ABS(ierr_))
   ALLOCATE( rwork(lrwork), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating rwork',ABS(ierr_))

   !
   ! make a local copy of a
   atmp(:,:) = a(1:m,1:n) 

   !
   ! use magma if possible
   CALL ZGELSD( m, n, nrhs, atmp, m, b, SIZE(b,1), sv_, rcond, rank_, &
                work, lwork, rwork, iwork, info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore(subname, 'ZGELSD: info illegal value', -info )
        IF ( info > 0 ) CALL errore(subname, 'ZGELSD: algorithm failure', info )
   ENDIF
   !
   IF ( PRESENT(sv) )     sv(1:minmn) = sv_(1:minmn)
   IF ( PRESENT(rank) )   rank        = rank_
    

   DEALLOCATE( atmp, STAT=ierr_)
   IF(ierr_/=0) CALL errore(subname,'deallocating atmp',ABS(ierr_))
   DEALLOCATE( sv_, STAT=ierr_)
   IF(ierr_/=0) CALL errore(subname,'deallocating atmp',ABS(ierr_))
   DEALLOCATE( work, rwork, iwork, STAT=ierr_)
   IF(ierr_/=0) CALL errore(subname,'deallocating work, rwork, iwork',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_lsd


!**********************************************************
   SUBROUTINE zmat_lsd_1(m, n, nrhs, a, b, rcond, sv, rank, ierr)
   !**********************************************************
   !
   ! Interface to zmat_lsd when nrhs = 1
   !
   IMPLICIT NONE
   INTEGER,   INTENT(IN)       :: m, n, nrhs
   REAL(DP), INTENT(IN)       :: rcond
   COMPLEX(DP), INTENT(IN)    :: a(:,:)
   COMPLEX(DP), INTENT(INOUT) :: b(:)
   REAL(DP), OPTIONAL, INTENT(OUT) :: sv(:)
   INTEGER,   OPTIONAL, INTENT(OUT) :: rank
   INTEGER,   OPTIONAL, INTENT(OUT) :: ierr
   !
   CHARACTER(10)             :: subname="dmat_lsd_1"
   INTEGER                   :: ierr_, rank_, info
   COMPLEX(DP), ALLOCATABLE :: bl(:,:)
   REAL(DP),    ALLOCATABLE :: sv_(:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( nrhs /= 1) CALL errore(subname,'more than 1 rhs ? ',ABS(nrhs)+1)
   IF ( n > SIZE(b,1) ) CALL errore(subname,'vector b too small',2)
   !
   ALLOCATE( bl(n,1), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating bl',ABS(ierr_))
   ALLOCATE( sv_(MIN(m,n)), STAT=ierr_ )
   IF (ierr_/=0) CALL errore(subname,'allocating sv_',ABS(ierr_))

   bl(:,1) = b(1:n)
   CALL zmat_lsd( m, n, 1, a, bl, rcond, SV=sv_, RANK=rank_, IERR=info)
   !
   IF ( PRESENT(ierr) ) THEN 
       ierr=info
   ELSE
       IF (info /=0) CALL errore(subname,'info/=0',ABS(info))
   ENDIF
   IF ( PRESENT(sv) )    sv(1:MIN(m,n))  = sv_(:)
   IF ( PRESENT(rank) )  rank            = rank_
   !
   b(1:n) = bl(1:n,1)

   DEALLOCATE( bl, STAT=ierr_)
   IF (ierr_/=0) CALL errore(subname,'deallocating bl',ABS(ierr_))
   DEALLOCATE( sv_, STAT=ierr_)
   IF (ierr_/=0) CALL errore(subname,'deallocating sv_',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_lsd_1


!**********************************************************
   SUBROUTINE zmat_sv(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   !  Computes the solution of the complex system of linear equations
   !     A * X = B,
   !  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
   !
   !  The LU decomposition with partial pivoting and row interchanges is
   !  used to factor A as
   !     A = P * L * U,
   !  where P is a permutation matrix, L is unit lower triangular, and U is
   !  upper triangular.  The factored form of A is then used to solve the
   !  system of equations A * X = B.
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)         :: n, nrhs
   COMPLEX(DP), INTENT(IN)    :: a(:,:)
   COMPLEX(DP), INTENT(INOUT) :: b(:,:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr

   INTEGER :: ierr_, info
   INTEGER,      ALLOCATABLE :: ipiv(:)
   COMPLEX(DP), ALLOCATABLE :: atmp(:,:)

   IF ( PRESENT(ierr) ) ierr=0
   !
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) CALL errore('zmat_sv','matrix A too small',1)
   IF ( n > SIZE(b,1) ) CALL errore('zmat_sv','matrix B too small (I)',1)
   IF ( nrhs > SIZE(b,2) ) CALL errore('zmat_sv','matrix B too small (II)',1)

   ALLOCATE( atmp(n,n), ipiv(n), STAT=ierr_ )
     IF (ierr_/=0) CALL errore('zmat_sv','allocating atmp, ipiv',ABS(ierr_))
   !
   ! make a local copy of a
   atmp(:,:) = a(1:n,1:n) 

   ! use magma if possible
   CALL ZGESV( n, nrhs, atmp, n, ipiv, b, SIZE(b,1), info)

   IF ( PRESENT(ierr) ) THEN
        IF (info/=0) ierr= info
   ELSE
        IF ( info < 0 ) CALL errore('zmat_sv', 'ZGESV: info illegal value', -info )
        IF ( info > 0 ) CALL errore('zmat_sv', 'ZGESV: singular matrix', info )
   ENDIF

   DEALLOCATE( atmp, ipiv, STAT=ierr_)
      IF(ierr_/=0) CALL errore('zmat_sv','deallocating atmp, ipiv',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_sv


!**********************************************************
   SUBROUTINE zmat_sv_1(n, nrhs, a, b, ierr)
   !**********************************************************
   !
   ! Interface to zmat_sv when nrhs = 1
   !
   IMPLICIT NONE
   INTEGER, INTENT(IN)         :: n, nrhs
   COMPLEX(DP), INTENT(IN)    :: a(:,:)
   COMPLEX(DP), INTENT(INOUT) :: b(:)
   INTEGER, OPTIONAL, INTENT(out) :: ierr
   !
   INTEGER  :: ierr_, info
   COMPLEX(DP),   ALLOCATABLE :: bl(:,:)

   IF ( PRESENT(ierr) ) ierr = 0
   !
   IF ( nrhs /= 1) CALL errore('zmat_sv_1','more than 1 rhs ?',ABS(nrhs)+1)
   IF ( n > SIZE(b,1) ) CALL errore('zmat_sv_1','vector b too small',2)
   !
   ALLOCATE( bl(n,1), STAT=ierr_ )
   IF (ierr_/=0) CALL errore('zmat_sv_1','allocating bl',ABS(ierr_))

   bl(:,1) = b(1:n)
   CALL zmat_sv( n, 1, a, bl, IERR=info)
   !
   IF ( PRESENT(ierr) ) THEN 
       ierr=info
   ELSE
       IF ( info /=0 ) CALL errore('zmat_sv_1','info/=0',ABS(info))
   ENDIF
   !
   b(1:n) = bl(1:n,1)

   DEALLOCATE( bl, STAT=ierr_)
   IF (ierr_/=0) CALL errore('zmat_sv_1','deallocating bl',ABS(ierr_))

   RETURN
END SUBROUTINE zmat_sv_1


!**********************************************************
   SUBROUTINE zmat_mul( c, a, opa, b, opb, m, n, k )
   !**********************************************************
   IMPLICIT NONE
   COMPLEX(DP), INTENT(IN)  :: a(:,:)
   COMPLEX(DP), INTENT(IN)  :: b(:,:)
   COMPLEX(DP), INTENT(OUT) :: c(:,:)
   CHARACTER,    INTENT(IN)  :: opa, opb
   INTEGER,      INTENT(IN)  :: m, n, k
   !
   INTEGER :: i, j, l, ierr
   COMPLEX(DP), ALLOCATABLE :: c_(:,:)
   !
   ! According to BLAS convention:
   ! C = opa(A) * opb(B)      op* = 'N' normal, 'C' complx conjg (i.e. herm conjg)
   !                          C is m*n,   opa(A) is m*k, opb(B) = k*n
   !
   IF ( m <= 0 .OR. n<=0 .OR. k<=0) CALL errore('zmat_mul','Invalid DIM',1)
   IF( opb /= 'N' .AND. opb /= 'C' ) &
     CALL errore('zmat_mul','argument value not allowed', 5 )

   ALLOCATE( c_( SIZE(c,1),SIZE(c,2) ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore('zmat_mul','allocating c_',ABS(ierr))

   !
   ! this filter value has to be checked. Here we try to use BLAS
   ! as much as we can.
   !
   IF( k <= 3 ) THEN
       IF( ( opb == 'N' ) .AND. ( opa == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = CZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + a(i,l) * b(l,j)
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'N' ) .AND. ( opb == 'C' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = CZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + a(i,l) * CONJG( b(j,l) )
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'C' ) .AND. ( opb == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = CZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + CONJG( a(l,i) )* b(l,j) 
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opb == 'C' ) .AND. ( opa == 'C' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('zmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('zmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,2) ) CALL  errore('zmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = CZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + CONJG( a(l,i) )* CONJG( b(j,l) )
               ENDDO
           ENDDO
           ENDDO
       ENDIF
   ELSE

       CALL ZGEMM( opa, opb, m, n, k, CONE, a, SIZE(a,1), &
                   b, SIZE(b,1), CZERO, c_, SIZE(c,1) )
   ENDIF
   !
   c(:,:) = c_(:,:)
   !
   DEALLOCATE( c_, STAT=ierr)
   IF ( ierr/=0 ) CALL errore('zmat_mul','deallocating c_', ABS(ierr))
   !
END SUBROUTINE zmat_mul


!**********************************************************
   SUBROUTINE dmat_mul( c, a, opa, b, opb, m, n, k )
   !**********************************************************
   IMPLICIT NONE
   REAL(DP), INTENT(IN)  :: a(:,:)
   REAL(DP), INTENT(IN)  :: b(:,:)
   REAL(DP), INTENT(OUT) :: c(:,:)
   CHARACTER, INTENT(IN)  :: opa, opb
   INTEGER,   INTENT(IN)  :: m, n, k
   !
   INTEGER :: i, j, l, ierr
   REAL(DP), ALLOCATABLE :: c_(:,:)
   !
   ! According to BLAS convention:
   ! C = opa(A) * opb(B)      op* = 'N' normal, 'T' transpose 
   !                          C is m*n,   opa(A) is m*k, opb(B) = k*n
   !
   IF ( m <= 0 .OR. n<=0 .OR. k<=0) CALL errore('dmat_mul','Invalid DIM',1)
   IF( opb /= 'N' .AND. opb /= 'T' ) &
     CALL errore('dmat_mul','argument value not allowed ', 5 )

   ALLOCATE( c_( SIZE(c,1),SIZE(c,2) ), STAT=ierr )
   IF ( ierr/=0 ) CALL errore('dmat_mul','allocating c_',ABS(ierr))

   !
   ! this filter value has to be checked. Here we try to use BLAS
   ! as much as we can.
   !
   IF( k <= 3 ) THEN
       IF( ( opb == 'N' ) .AND. ( opa == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = ZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + a(i,l) * b(l,j)
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'N' ) .AND. ( opb == 'T' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,1) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,2) .OR. k > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = ZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + a(i,l) * b(j,l)
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opa == 'T' ) .AND. ( opb == 'N' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = ZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + a(l,i) * b(l,j) 
               ENDDO
           ENDDO
           ENDDO
       ELSE IF( ( opb == 'T' ) .AND. ( opa == 'T' ) ) THEN
           !
           IF ( m > SIZE(c,1) .OR. m > SIZE(a,2) ) CALL  errore('dmat_mul','Invalid C,A',m)
           IF ( n > SIZE(c,2) .OR. n > SIZE(b,1) ) CALL  errore('dmat_mul','Invalid C,B',n)
           IF ( k > SIZE(a,1) .OR. k > SIZE(b,2) ) CALL  errore('dmat_mul','Invalid A,B',k)
           !
           DO j = 1, n
           DO i = 1, m
               c_(i,j) = ZERO
               DO l = 1, k
                   c_(i,j) = c_(i,j) + a(l,i) * b(j,l) 
               ENDDO
           ENDDO
           ENDDO
       ENDIF
   ELSE

       CALL DGEMM( opa, opb, m, n, k, ONE, a, SIZE(a,1), &
                   b, SIZE(b,1), ZERO, c_, SIZE(c,1) )
   ENDIF
   !
   c(:,:) = c_(:,:)
   !
   DEALLOCATE( c_, STAT=ierr)
   IF ( ierr/=0 ) CALL errore('dmat_mul','deallocating c_', ABS(ierr))
   !
END SUBROUTINE dmat_mul


!**********************************************************
   SUBROUTINE dmat_mul_1( c, a, opa, b, m, k )
   !**********************************************************
   IMPLICIT NONE
   REAL(DP), INTENT(IN)  :: a(:,:)
   REAL(DP), INTENT(IN)  :: b(:)
   REAL(DP), INTENT(OUT) :: c(:)
   CHARACTER, INTENT(IN)  :: opa
   INTEGER,   INTENT(IN)  :: m, k
   !
   INTEGER :: ierr
   REAL(DP), ALLOCATABLE :: b_(:,:), c_(:,:)
   
   ALLOCATE( b_(SIZE(b),1), c_(SIZE(c),1), STAT=ierr ) 
   IF ( ierr/=0 ) CALL errore('dmat_mul_1', 'allocating b_, c_', ABS(ierr))
   !
   b_(:,1) = b(:)
   c_(:,1) = c(:)
   !
   CALL mat_mul( c_, a, opa, b_, "N", m, 1, k )
   !
   c(:) = c_(:,1)
   !
   DEALLOCATE( b_, c_, STAT=ierr ) 
   IF ( ierr/=0 ) CALL errore('dmat_mul_1', 'deallocating b_, c_', ABS(ierr))
   !
END SUBROUTINE dmat_mul_1


!**********************************************************
   SUBROUTINE zmat_hdiag( z, w, a, n )
   !**********************************************************
   !
   ! utility to diagonalize complex hermitean matrices
   !
   IMPLICIT NONE
   COMPLEX(DP), INTENT(IN)  :: a(:,:)
   COMPLEX(DP), INTENT(OUT) :: z(:,:)
   REAL(DP),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: n

   INTEGER :: i, j, ierr, info
   COMPLEX(DP), ALLOCATABLE :: ap(:)
   COMPLEX(DP), ALLOCATABLE :: work(:)
   REAL(DP), ALLOCATABLE :: rwork(:)
   INTEGER, ALLOCATABLE :: ifail(:)
   INTEGER, ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('zmat_hdiag','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('zmat_hdiag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('zmat_hdiag','Invalid Z dimensions',ABS(n)+1)
   
   ALLOCATE( ap(n*(n+1)/2), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ap',ABS(ierr))
   ALLOCATE( work(2*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating work',ABS(ierr))
   ALLOCATE( rwork(7*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating rwork',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating iwork',ABS(ierr))

   DO j = 1, n
   DO i = 1, j
      ap(i + ( (j-1)*j)/2 ) = a(i,j)
   ENDDO
   ENDDO

   ! use magma if possible
   CALL ZHPEVX( 'v', 'a', 'u', n, ap, ZERO, ZERO, 0, 0, -ONE, i, w, &
                 z, SIZE(z,1), work, rwork, iwork, ifail, info )

   IF ( info < 0 ) CALL errore('zmat_hdiag', 'zhpevx: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore('zmat_hdiag', 'zhpevx: eigenvectors not converged', info )
    
   DEALLOCATE( ap, work, rwork, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_hdiag','deallocating ap...ifail',ABS(ierr))

   RETURN
END SUBROUTINE zmat_hdiag


!**********************************************************
   SUBROUTINE zmat_hdiag_pack( z, w, ap, n, uplo )
   !**********************************************************
   !
   ! utility to diagonalize complex hermitean matrices, in packed form
   !
   IMPLICIT NONE
   COMPLEX(DP), INTENT(IN)  :: ap(:)
   COMPLEX(DP), INTENT(OUT) :: z(:,:)
   REAL(DP),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(IN)  :: n
   CHARACTER,    INTENT(IN)  :: uplo

   INTEGER :: i, ierr, info
   COMPLEX(DP), ALLOCATABLE :: work(:)
   REAL(DP), ALLOCATABLE :: rwork(:)
   INTEGER, ALLOCATABLE :: ifail(:)
   INTEGER, ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('zmat_hdiag','Invalid N',ABS(n)+1)
   IF ( n*(n+1)/2 > SIZE(ap) .OR. n*(n+1)/2 > SIZE(ap) ) &
        CALL errore('zmat_hdiag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('zmat_hdiag','Invalid Z dimensions',ABS(n)+1)
   
   ALLOCATE( work(2*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating work',ABS(ierr))
   ALLOCATE( rwork(7*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating rwork',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating iwork',ABS(ierr))

   ! use magma if possible
   CALL ZHPEVX( 'v', 'a', uplo, n, ap, ZERO, ZERO, 0, 0, -ONE, i, w, &
                 z, SIZE(z,1), work, rwork, iwork, ifail, info )

   IF ( info < 0 ) CALL errore('zmat_hdiag', 'zhpevx: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore('zmat_hdiag', 'zhpevx: eigenvectors not converged', info )
    
   DEALLOCATE( work, rwork, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_hdiag','deallocating work...ifail',ABS(ierr))

   RETURN
END SUBROUTINE zmat_hdiag_pack


!**********************************************************
   SUBROUTINE zmat_hdiag_gen( z, w, a, b, n )
   !**********************************************************
   !
   ! utility to solve the generalized eigenvalue problem with
   ! A,B hermitean matrices:
   !  A * z = w * B * z
   !
   IMPLICIT NONE
   COMPLEX(DP), INTENT(IN)  :: a(:,:)
   COMPLEX(DP), INTENT(IN)  :: b(:,:)
   COMPLEX(DP), INTENT(OUT) :: z(:,:)
   REAL(DP),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: n

   INTEGER :: i, j, ierr, info
   COMPLEX(DP), ALLOCATABLE :: ap(:), bp(:)
   COMPLEX(DP), ALLOCATABLE :: work(:)
   REAL(DP),    ALLOCATABLE :: rwork(:)
   INTEGER,      ALLOCATABLE :: ifail(:)
   INTEGER,      ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('zmat_hdiag_gen','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('zmat_hdiag_gen','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(b,1) .OR. n > SIZE(b,2) ) &
        CALL errore('zmat_hdiag_gen','Invalid B dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('zmat_hdiag_gen','Invalid Z dimensions',ABS(n)+1)
   
   ALLOCATE( ap(n*(n+1)/2), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ap',ABS(ierr))
   ALLOCATE( bp(n*(n+1)/2), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating bp',ABS(ierr))
   ALLOCATE( work(2*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating work',ABS(ierr))
   ALLOCATE( rwork(7*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating rwork',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating iwork',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_hdiag','allocating ifail',ABS(ierr))

   DO j = 1, n
   DO i = 1, j
      ap(i + ( (j-1)*j)/2 ) = a(i,j)
      bp(i + ( (j-1)*j)/2 ) = b(i,j)
   ENDDO
   ENDDO

   ! use magma if possible
   CALL ZHPGVX( 1, 'v', 'a', 'u', n, ap, bp, ZERO, ZERO, 0, 0, -ONE, i, w, &
                 z, SIZE(z,1), work, rwork, iwork, ifail, info )

   IF ( info < 0 ) CALL errore('zmat_hdiag', 'zhpevx: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore('zmat_hdiag', 'zhpevx: eigenvectors not converged', info )
    
   DEALLOCATE( ap, bp, work, rwork, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_hdiag','deallocating ap...ifail',ABS(ierr))

   RETURN
END SUBROUTINE zmat_hdiag_gen


!**********************************************************
   SUBROUTINE dmat_hdiag( z, w, a, n )
   !**********************************************************
   !
   ! utility to diagonalize real symmetric matrices
   !
   IMPLICIT NONE
   REAL(DP),    INTENT(IN)  :: a(:,:)
   REAL(DP),    INTENT(OUT) :: z(:,:)
   REAL(DP),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: n

   INTEGER :: i, j, ierr, info
   REAL(DP), ALLOCATABLE :: ap(:)
   REAL(DP), ALLOCATABLE :: work(:)
   INTEGER,   ALLOCATABLE :: ifail(:)
   INTEGER,   ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('dmat_hdiag','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('dmat_hdiag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('dmat_hdiag','Invalid Z dimensions',ABS(n)+1)
   
   ALLOCATE( ap(n*(n+1)/2), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating ap',ABS(ierr))
   ALLOCATE( work(8*n), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating work',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
      IF(ierr/=0) CALL errore('dmat_hdiag','allocating iwork',ABS(ierr))

   DO j = 1, n
   DO i = 1, j
      ap(i + ( (j-1)*j)/2 ) = a(i,j)
   ENDDO
   ENDDO

   ! use magma if possible
   CALL DSPEVX( 'v', 'a', 'u', n, ap(1), ZERO, ZERO, 0, 0, -ONE, i, w(1), &
                 z(1,1), SIZE(z,1), work(1), iwork(1), ifail(1), info )

   IF ( info < 0 ) CALL errore('dmat_hdiag', 'zhpevx: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore('dmat_hdiag', 'zhpevx: eigenvectors not converged', info )
    
   DEALLOCATE( ap, work, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore('dmat_hdiag','deallocating ap...ifail',ABS(ierr))

   RETURN
END SUBROUTINE dmat_hdiag


!**********************************************************
   SUBROUTINE dmat_bnd_hdiag( z, w, ab, ldab, n, kd )
   !**********************************************************
   !
   ! utility to diagonalize real symmetric matrices
   !
   IMPLICIT NONE
   REAL(DP),    INTENT(IN)  :: ab(:,:)
   REAL(DP),    INTENT(OUT) :: z(:,:)
   REAL(DP),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: ldab, n, kd

   CHARACTER(14) :: subname="dmat_bnd_hdiag"
   INTEGER :: i, j, m, ierr, info
   REAL(DP), ALLOCATABLE :: work(:), qmat(:,:)
   INTEGER,   ALLOCATABLE :: ifail(:)
   INTEGER,   ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore(subname,'Invalid N',ABS(n)+1)
   !
   IF ( ldab < kd + 1)  CALL errore (subname, 'invalid ldab', 10)
   IF ( SIZE(z,1) < n)  CALL errore (subname, 'invalid ldz', 10)
   IF ( SIZE(z,2) < n)  CALL errore (subname, 'invalid ldz II', 11)
   !
   ALLOCATE( qmat(n,n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating qmat',ABS(ierr))
   ALLOCATE( work(7*n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating work',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating iwork',ABS(ierr))


   ! use magma if possible
   CALL DSBEVX( 'v', 'a', 'u', n, kd, ab, ldab, qmat, SIZE(qmat,1), ZERO, ZERO, 0, 0, ZERO, &
                 m, w, z, SIZE(z,1), work, iwork, ifail, info )

   IF ( info < 0 ) CALL errore(subname, 'DSBEVX: info illegal value', -info )
   IF ( info > 0 ) CALL errore(subname, 'DSBEVX: eigenvectors not converged', info )
    
   DEALLOCATE( qmat, work, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore(subname,'deallocating qmat...ifail',ABS(ierr))

   RETURN
END SUBROUTINE dmat_bnd_hdiag


!**********************************************************
   SUBROUTINE dmat_bnd_hdiag_ext( z, w, ab, ldab, n, kd, il, iu )
   !**********************************************************
   !
   ! utility to diagonalize real symmetric matrices
   !
   IMPLICIT NONE
   REAL(DP),    INTENT(IN)  :: ab(:,:)
   REAL(DP),    INTENT(OUT) :: z(:,:)
   REAL(DP),    INTENT(OUT) :: w(:)
   INTEGER,      INTENT(in)  :: ldab, n, kd
   INTEGER,      INTENT(in)  :: il, iu

   CHARACTER(18) :: subname="dmat_bnd_hdiag_ext"
   INTEGER :: i, j, m, ierr, info
   REAL(DP), ALLOCATABLE :: work(:), qmat(:,:)
   INTEGER,   ALLOCATABLE :: ifail(:)
   INTEGER,   ALLOCATABLE :: iwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore(subname,'Invalid N',ABS(n)+1)
   !
   IF ( ldab < kd + 1)  CALL errore (subname, 'invalid ldab', 10)
   IF ( SIZE(z,1) < n)  CALL errore (subname, 'invalid ldz I', 10)
   IF ( SIZE(z,2) < iu-il+1)  &
                        CALL errore (subname, 'invalid ldz II', 11)
   IF ( il <= 0 )       CALL errore (subname, 'invalid il', 12)
   IF ( iu <= 0 )       CALL errore (subname, 'invalid iu', 12)
   !
   ALLOCATE( qmat(n,n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating qmat',ABS(ierr))
   ALLOCATE( work(7*n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating work',ABS(ierr))
   ALLOCATE( ifail(n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating ifail',ABS(ierr))
   ALLOCATE( iwork(5*n), STAT=ierr )
   IF(ierr/=0) CALL errore(subname,'allocating iwork',ABS(ierr))


   ! use magma if possible
   CALL DSBEVX( 'v', 'i', 'u', n, kd, ab, ldab, qmat, SIZE(qmat,1), ZERO, ZERO, il, iu, ZERO, &
                 m, w, z, SIZE(z,1), work, iwork, ifail, info )

   IF ( info < 0 ) CALL errore(subname, 'DSBEVX: info illegal value', -info )
   IF ( info > 0 ) &
        CALL errore(subname, 'DSBEVX: eigenvectors not converged', info )
    
   DEALLOCATE( qmat, work, iwork, ifail, STAT=ierr)
      IF(ierr/=0) CALL errore(subname,'deallocating qmat...ifail',ABS(ierr))

   RETURN
END SUBROUTINE dmat_bnd_hdiag_ext


!**********************************************************
   SUBROUTINE zmat_diag( z, w, a, n, side )
   !**********************************************************
   !
   ! utility to diagonalize complex non-hermitean matrices
   !
   IMPLICIT NONE
   COMPLEX(DP),        INTENT(IN)  :: a(:,:)
   COMPLEX(DP),        INTENT(OUT) :: z(:,:)
   COMPLEX(DP),        INTENT(OUT) :: w(:)
   INTEGER,             INTENT(in)  :: n
   CHARACTER,           INTENT(in)  :: side

   INTEGER   :: ierr, info, lwork
   CHARACTER :: jobvl, jobvr
   COMPLEX(DP), ALLOCATABLE :: work(:), vl(:,:), vr(:,:)
   COMPLEX(DP), ALLOCATABLE :: a_(:,:)
   REAL(DP),    ALLOCATABLE :: rwork(:)

   ! get the dimension of the problem
   IF ( n <= 0 ) CALL errore('zmat_diag','Invalid N',ABS(n)+1)
   IF ( n > SIZE(a,1) .OR. n > SIZE(a,2) ) &
        CALL errore('zmat_diag','Invalid A dimensions',ABS(n)+1)
   IF ( n > SIZE(z,1) .OR. n > SIZE(z,2) ) &
        CALL errore('zmat_diag','Invalid Z dimensions',ABS(n)+1)

   SELECT CASE ( side )
   CASE ( 'L', 'l' )
       jobvl = 'V'
       jobvr = 'N'
   CASE ( 'R', 'r' )
       jobvl = 'N'
       jobvr = 'V'
   CASE DEFAULT
       CALL errore('zmat_diag','Invalid side',3)
   END SELECT

   lwork = 2 * n
   ALLOCATE( work(lwork), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating work',ABS(ierr))
   ALLOCATE( rwork(2*n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating rwork',ABS(ierr))
   ALLOCATE( vl(n,n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating vl',ABS(ierr))
   ALLOCATE( vr(n,n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating vr',ABS(ierr))

   ALLOCATE( a_(n,n), STAT=ierr )
      IF(ierr/=0) CALL errore('zmat_diag','allocating a_',ABS(ierr))

   a_(1:n,1:n) = a(1:n,1:n)

   ! use magma if possible
   CALL ZGEEV( jobvl, jobvr, n, a_, SIZE(a_,1), w, vl, n, vr, n, work, &
               lwork, rwork, info )

   IF ( info < 0 ) CALL errore('zmat_diag', 'zgeev: info illegal value', -info )
   IF ( info > 0 ) CALL errore('zmat_diag', 'zgeev: eigenvectors not converged', info )

   SELECT CASE ( side )
   CASE ( 'L', 'l' )
       z(1:n,1:n) = vl
   CASE ( 'R', 'r' )
       z(1:n,1:n) = vr
   END SELECT

   DEALLOCATE( a_, work, rwork, vl, vr, STAT=ierr)
      IF(ierr/=0) CALL errore('zmat_diag','deallocating a_, work--vr',ABS(ierr))

   RETURN
END SUBROUTINE zmat_diag


!**********************************************************
   SUBROUTINE zmat_inv( n, a, z, det_a, ierr )
   !**********************************************************
   !
   ! compute Z = inv( A ), and, if required, 
   ! the determinant of A
   ! The current implementation is generalized from invmat.f90 
   ! of the Quantum-Espresso distribution
   !
   IMPLICIT NONE
   INTEGER,                INTENT(IN)  :: n
   COMPLEX(DP),           INTENT(IN)  :: a(:,:)
   COMPLEX(DP),           INTENT(OUT) :: z(:,:)
   COMPLEX(DP), OPTIONAL, INTENT(OUT) :: det_a
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   INTEGER           :: i, nb, info, ierr_, ldz, lwork, ipiv (n)
   INTEGER, EXTERNAL :: ILAENV
   ! info=0: inversion was successful
   ! ldz   : leading dimension (the same as n)
   ! ipiv  : work space for pivoting 
   COMPLEX(DP), ALLOCATABLE :: work(:)
   !
   !
   IF ( PRESENT( ierr ) ) ierr=0
   !
   ldz = SIZE(z,1)
   z(1:n,1:n) = a(1:n,1:n)

   !
   ! perform matrix inversion according to LAPACK
   !
   ! First get the optimum LWORK
   ! check with the use of magma
   !
   nb = ILAENV( 1, 'ZGETRI', ' ', n, -1, -1, -1 )
   !
   lwork = n * nb
   !
   ALLOCATE( work( lwork ), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore ('zmat_inv', 'allocating work', ABS (ierr_) )
   ! 
   !
   ! use magma if possible
   CALL ZGETRF (n, n, z, ldz, ipiv, info)
   !
   IF ( PRESENT(ierr) ) THEN
       ! 
       IF ( info/=0 ) THEN
           ierr = info
           RETURN
       ENDIF
       ! 
   ELSE
       IF ( info/=0 ) CALL errore ('zmat_inv', 'error in ZGETRF', ABS (info) )
   ENDIF

   !
   ! compute the determinan if required
   !
   IF ( PRESENT( det_a ) ) THEN
      !
      det_a = ONE
      DO i = 1, n
         det_a = det_a * z(i,i)
      ENDDO
      !
   ENDIF
   !
   ! use magma if possible
   CALL ZGETRI (n, z, ldz, ipiv, work, lwork, info)
   !
   IF ( PRESENT(ierr) ) THEN
       IF ( info/=0 ) ierr = info
   ELSE
       IF ( info/=0 ) CALL errore ('zmat_inv', 'error in ZGETRI', ABS (info) )
   ENDIF
   !
   ! 
   DEALLOCATE( work, STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore ('zmat_inv', 'deallocating work', ABS (ierr_) )
   !
END SUBROUTINE zmat_inv 


!**********************************************************
   SUBROUTINE zmat_bnd_inv( n, kl, ku, ab, ldab, z, ierr )
   !**********************************************************
   !
   ! compute Z = inv( AB ) for a banded matrix AB 
   ! AB should be entry in a band matrix format
   !
   IMPLICIT NONE
   INTEGER,                INTENT(IN)  :: n
   INTEGER,                INTENT(IN)  :: kl, ku, ldab
   COMPLEX(DP),           INTENT(IN)  :: ab(ldab,n)
   COMPLEX(DP),           INTENT(OUT) :: z(n,n)
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   INTEGER           :: i, info, ldz, ipiv (n)
   !INTEGER           :: ierr_, lwork
   INTEGER, EXTERNAL :: ILAENV
   ! info=0: inversion was successful
   ! ldz   : leading dimension (the same as n)
   ! ipiv  : work space for pivoting 
   !
   !
   IF ( PRESENT( ierr ) ) ierr=0
   !
   ldz = n
   !
   ! perform matrix inversion according to LAPACK
   !
   IF ( ldab < 2 * kl + ku + 1) CALL errore ('zmat_bnd_inv', 'invalid ldab', 10 )
   !
   z = CZERO
   DO i = 1, n
       z(i,i) = CONE
   ENDDO
   !
   ! use magma if possible
   CALL ZGBSV (n, kl, ku, n, ab, ldab, ipiv, z, ldz, info)


   IF ( PRESENT(ierr) ) THEN
       ! 
       IF ( info/=0 ) THEN
           ierr = info
           RETURN
       ENDIF
       ! 
   ELSE
       IF ( info/=0 ) CALL errore ('zmat_bnd_inv', 'error in ZGBTRF', ABS (info) )
   ENDIF
   !
END SUBROUTINE zmat_bnd_inv 


!**********************************************************
   SUBROUTINE dmat_inv( n, a, z, det_a, ierr )
   !**********************************************************
   !
   ! compute Z = inv( A ), and, if required, 
   ! the determinant of A
   ! The current implementation is generalized from invmat.f90 
   ! of the Quantum-Espresso distribution
   !
   IMPLICIT NONE
   INTEGER,             INTENT(IN)    :: n
   REAL(DP),           INTENT(IN)    :: a(:,:)
   REAL(DP),           INTENT(OUT)   :: z(:,:)
   REAL(DP), OPTIONAL, INTENT(OUT)   :: det_a
   INTEGER,   OPTIONAL, INTENT(OUT)   :: ierr
   !
   INTEGER           :: i, nb, info, ierr_, ldz, lwork, ipiv (n)
   INTEGER, EXTERNAL :: ILAENV
   ! info=0: inversion was successful
   ! ldz   : leading dimension (the same as n)
   ! ipiv  : work space for pivoting 
   REAL(DP), ALLOCATABLE :: work(:)
   !
   !
   IF ( PRESENT( ierr ) ) ierr = 0
   !
   ldz = SIZE(z,1)
   z(1:n,1:n) = a(1:n,1:n)
   !
   ! perform matrix inversion according to LAPACK
   !
   ! First get the optimum LWORK
   ! check with magma
   !
   nb = ILAENV( 1, 'DGETRI', ' ', n, -1, -1, -1 )
   lwork = n * nb
   !
   !
   ALLOCATE( work( lwork ), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore ('dmat_inv', 'allocating work', ABS(ierr_) )
   ! 
   !
   ! use magma if possible
   CALL DGETRF (n, n, z, ldz, ipiv, info)
   !
   IF ( PRESENT(ierr) ) THEN
       ! 
       IF ( info/=0 ) THEN
           ierr = info
           RETURN
       ENDIF
       ! 
   ELSE
       IF ( info/=0 ) CALL errore ('dmat_inv', 'error in DGETRF', ABS (info) )
   ENDIF

   !
   ! compute the determinan if required
   !
   IF ( PRESENT( det_a ) ) THEN
       !
       det_a = ONE
       DO i = 1, n
          det_a = det_a * z(i,i)
       ENDDO
       !
   ENDIF
   !
   ! use magma if possible
   CALL DGETRI (n, z, ldz, ipiv, work, lwork, info)
   !
   IF ( PRESENT(ierr) ) THEN
       IF ( info/=0 ) ierr = info
   ELSE
       IF ( info/=0 ) CALL errore ('dmat_inv', 'error in ZGETRI', ABS (info) )
   ENDIF
   !
   ! 
   DEALLOCATE( work, STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore ('dmat_inv', 'deallocating work', ABS (ierr_) )
   !
END SUBROUTINE dmat_inv 


!**********************************************************
   SUBROUTINE dmat_bnd_inv( n, kl, ku, ab, ldab, z, ierr )
   !**********************************************************
   !
   ! compute Z = inv( AB ) for a banded matrix AB 
   ! AB should be entry in a band matrix format
   !
   IMPLICIT NONE
   INTEGER,                INTENT(IN)  :: n
   INTEGER,                INTENT(IN)  :: kl, ku, ldab
   REAL(DP),              INTENT(IN)  :: ab(ldab,n)
   REAL(DP),              INTENT(OUT) :: z(n,n)
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   INTEGER           :: i, info, ldz, ipiv (n)
   !INTEGER           :: ierr_, lwork
   INTEGER, EXTERNAL :: ILAENV
   ! info=0: inversion was successful
   ! ldz   : leading dimension (the same as n)
   ! ipiv  : work space for pivoting 
   !
   !
   IF ( PRESENT( ierr ) ) ierr=0
   !
   ldz = n
   !
   ! perform matrix inversion according to LAPACK
   !
   IF ( ldab < 2 * kl + ku + 1) CALL errore ('dmat_bnd_inv', 'invalid ldab', 10 )
   !
   z = ZERO
   DO i = 1, n
       z(i,i) = ONE
   ENDDO
   !
   ! use magma if possible
   CALL DGBSV (n, kl, ku, n, ab, ldab, ipiv, z, ldz, info)


   IF ( PRESENT(ierr) ) THEN
       ! 
       IF ( info/=0 ) THEN
           ierr = info
           RETURN
       ENDIF
       ! 
   ELSE
       IF ( info/=0 ) CALL errore ('dmat_bnd_inv', 'error in DGBTRF', ABS (info) )
   ENDIF
   !
END SUBROUTINE dmat_bnd_inv 


!**********************************************************
   SUBROUTINE zmat_hsqrt( n, a, z, ierr )
   !**********************************************************
   !
   ! compute Z = sqrt( A ) for a Hermitean matrix A
   !
   IMPLICIT NONE
   INTEGER,                INTENT(IN)  :: n
   COMPLEX(DP),           INTENT(IN)  :: a(:,:)
   COMPLEX(DP),           INTENT(OUT) :: z(:,:)
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   COMPLEX(DP), ALLOCATABLE :: work(:,:), work1(:,:)
   REAL(DP),    ALLOCATABLE :: w(:)
   INTEGER  :: ierr_, i, j
   !
   IF ( PRESENT( ierr ) ) ierr=0
   !
   ALLOCATE( work(n,n), w(n), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("zmat_hsqrt","allocating work, w", ABS(ierr_))
   ALLOCATE( work1(n,n), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("zmat_hsqrt","allocating work1", ABS(ierr_))
   !
   ! diagonalize A
   CALL mat_hdiag( work, w, a, n )
   !
   DO i = 1, n
       IF ( w(i) <= 0.0d0 ) THEN
           ierr=1
           RETURN
       ELSE
           w(i) = SQRT(w(i))
       ENDIF
   ENDDO
   !
   DO j = 1, n
   DO i = 1, n
       !   
       work1(i,j) = work(i,j) * w(j)
       !   
   ENDDO
   ENDDO
   !   
   CALL mat_mul( z, work1, 'N', work, 'C', n, n, n)   
   !
   DEALLOCATE( work, work1, w, STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("zmat_hsqrt","deallocating work -- w", ABS(ierr_))
   !
   RETURN
   !
END SUBROUTINE zmat_hsqrt


!**********************************************************
   SUBROUTINE dmat_hsqrt( n, a, z, ierr )
   !**********************************************************
   !
   ! compute Z = sqrt( A ) for a real hermitean (symmetric) matrix A
   !
   IMPLICIT NONE
   INTEGER,                INTENT(IN)  :: n
   REAL(DP),              INTENT(IN)  :: a(:,:)
   REAL(DP),              INTENT(OUT) :: z(:,:)
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   REAL(DP),  ALLOCATABLE :: work(:,:), work1(:,:)
   REAL(DP),  ALLOCATABLE :: w(:)
   INTEGER  :: ierr_, i, j
   !
   IF ( PRESENT( ierr ) ) ierr=0
   !
   ALLOCATE( work(n,n), w(n), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("dmat_hsqrt","allocating work, w", ABS(ierr_))
   ALLOCATE( work1(n,n), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("dmat_hsqrt","allocating work1", ABS(ierr_))
   !
   ! diagonalize A
   CALL mat_hdiag( work, w, a, n )
   !
   DO i = 1, n
       IF ( w(i) <= 0.0d0 ) THEN
           ierr=1
           RETURN
       ELSE
           w(i) = SQRT(w(i))
       ENDIF
   ENDDO
   !
   DO j = 1, n
   DO i = 1, n
       !   
       work1(i,j) = work(i,j) * w(j)
       !   
   ENDDO
   ENDDO
   !   
   CALL mat_mul( z, work1, 'N', work, 'T', n, n, n)   
   !
   DEALLOCATE( work, work1, w, STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("dmat_hsqrt","deallocating work -- w", ABS(ierr_))
   !
   RETURN
   !
END SUBROUTINE dmat_hsqrt


!**********************************************************
   !SUBROUTINE dmat_bnd_hsqrt( n, ab, ldab, ka, zb, ldzb, kz, ierr )
   SUBROUTINE dmat_bnd_hsqrt( n, ab, ldab, ka, z, ierr )
   !**********************************************************
   !
   ! compute Z = sqrt( A ) for a real BANDED hermitean (symmetric) matrix A
   !
   IMPLICIT NONE
   INTEGER,                INTENT(IN)  :: n, ldab, ka
   REAL(DP),              INTENT(IN)  :: ab(:,:)
   REAL(DP),              INTENT(OUT) :: z(:,:)
   !INTEGER,                INTENT(OUT) :: kz
   INTEGER,      OPTIONAL, INTENT(OUT) :: ierr
   !
   REAL(DP),  ALLOCATABLE :: work(:,:), work1(:,:)
   REAL(DP),  ALLOCATABLE :: w(:)
   INTEGER  :: ierr_, i, j
   !
   IF ( PRESENT( ierr ) ) ierr=0
   !
   ALLOCATE( work(n,n), w(n), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("dmat_bnd_hsqrt","allocating work, w", ABS(ierr_))
   !
   ! diagonalize A
   CALL dmat_bnd_hdiag( work, w, ab, ldab, n, ka )
   !
   ALLOCATE( work1(n,n), STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("dmat_bnd_hsqrt","allocating work1", ABS(ierr_))
   !ALLOCATE( z(n,n), STAT=ierr_ )
   !IF ( ierr_/=0 ) CALL errore("dmat_bnd_hsqrt","allocating z", ABS(ierr_))
   !
   DO i = 1, n
       IF ( w(i) <= 0.0d0 ) THEN
           ierr=1
           RETURN
       ELSE
           w(i) = SQRT(w(i))
       ENDIF
   ENDDO
   !
   DO j = 1, n
   DO i = 1, n
       !   
       work1(i,j) = work(i,j) * w(j)
       !   
   ENDDO
   ENDDO
   !   
   CALL mat_mul( z, work1, 'N', work, 'T', n, n, n)   
   !
   DEALLOCATE( work, work1, w, STAT=ierr_ )
   IF ( ierr_/=0 ) CALL errore("dmat_hsqrt","deallocating work -- w", ABS(ierr_))
   !
   RETURN
   !
END SUBROUTINE dmat_bnd_hsqrt


!**********************************************************
   FUNCTION  zmat_is_herm( n, z, toll )
   !**********************************************************
   IMPLICIT NONE
   LOGICAL               :: zmat_is_herm
   INTEGER               :: n
   COMPLEX(DP)          :: z(:,:)
   REAL(DP), OPTIONAL   :: toll
   !
   ! n : actual dimension of Z
   ! check if a complex matrix is hermitean.
   !
   REAL(DP)     :: toll_
   CHARACTER(12) :: subname='zmat_is_herm'
   REAL(DP)     :: rtmp
   COMPLEX(DP)  :: ztmp
   INTEGER       :: i, j
   

   toll_ = TINY(ZERO)  
   IF ( PRESENT(toll) ) toll_ = toll
   IF ( toll_ <= 0.0 ) CALL errore(subname,'Invalid TOLL',1)
  
   IF ( n > SIZE( z, 1) ) CALL errore(subname,'Invalid n I',n)
   IF ( n > SIZE( z, 2) ) CALL errore(subname,'Invalid n II',n)

   zmat_is_herm = .TRUE.
   !
   main_loop: &
   DO j = 1, n
   DO i = 1, j
       !
       ztmp = z(i,j) - CONJG( z(j,i) )
       rtmp = REAL( ztmp * CONJG( ztmp ), DP ) 
       !
       IF ( rtmp > toll_ ) THEN
          zmat_is_herm = .FALSE. 
          EXIT main_loop
       ENDIF
       !
   ENDDO
   ENDDO main_loop
       
   RETURN
END FUNCTION zmat_is_herm


!**********************************************************
   FUNCTION  dmat_is_herm( n, A, toll )
   !**********************************************************
   IMPLICIT NONE
   LOGICAL               :: dmat_is_herm
   INTEGER               :: n
   REAL(DP)             :: A(:,:)
   REAL(DP), OPTIONAL   :: toll
   !
   ! n : actual dimension of Z
   ! check if a real matrix is hermitean.
   !
   REAL(DP)     :: toll_
   CHARACTER(12) :: subname='dmat_is_herm'
   REAL(DP)     :: rtmp
   INTEGER       :: i, j
   

   toll_ = TINY(ZERO)  
   IF ( PRESENT(toll) ) toll_ = toll
   IF ( toll_ <= 0.0 ) CALL errore(subname,'Invalid TOLL',1)
  
   IF ( n > SIZE( A, 1) ) CALL errore(subname,'Invalid n I',n)
   IF ( n > SIZE( A, 2) ) CALL errore(subname,'Invalid n II',n)

   dmat_is_herm = .TRUE.
   !
   main_loop: &
   DO j = 1, n
   DO i = 1, j
       !
       rtmp = A(i,j) - A(j,i)
       rtmp = rtmp**2
       !
       IF ( rtmp > toll_ ) THEN
          dmat_is_herm = .FALSE. 
          EXIT main_loop
       ENDIF
       !
   ENDDO
   ENDDO main_loop
       
   RETURN
END FUNCTION dmat_is_herm


!**********************************************************
   FUNCTION  zmat_is_unitary( m, n, z, side, toll )
   !**********************************************************
   IMPLICIT NONE
   LOGICAL                :: zmat_is_unitary
   INTEGER                :: m,n
   COMPLEX(DP)           :: z(:,:)
   CHARACTER(*), OPTIONAL :: side 
   REAL(DP),    OPTIONAL :: toll
   !
   ! m, n : actual dimensions of A
   ! check if a complex matrix is unitary.
   ! SIDE='left'  only   A^{\dag} * A = I
   ! SIDE='right' only   A * A^{\dag} = I
   ! SIDE='both'  both sides (DEFAULT)
   !
   REAL(DP)     :: toll_
   CHARACTER(10) :: side_
   CHARACTER(15) :: subname='zmat_is_unitary'
   INTEGER       :: dim1,dim2
   INTEGER       :: i, j, ierr
   COMPLEX(DP), ALLOCATABLE  :: result(:,:)
   
   zmat_is_unitary = .TRUE. 

   toll_ = TINY(ZERO)  
   side_ = 'both'
   IF ( PRESENT(side) ) side_ = TRIM(side)
   IF ( PRESENT(toll) ) toll_ = toll
   IF ( toll_ <= 0 ) CALL errore(subname,'Invalid TOLL',1)
  
   IF ( m > SIZE( z, 1) ) CALL errore(subname,'Invalid m',m)
   IF ( n > SIZE( z, 2) ) CALL errore(subname,'Invalid n',n)
   dim1 = m
   dim2 = n
   IF ( dim1 <= 0) CALL errore(subname,'Invalid dim1',ABS(dim1)+1)
   IF ( dim2 <= 0) CALL errore(subname,'Invalid dim2',ABS(dim2)+1)


   !
   ! check side LEFT
   !
   IF ( TRIM(side_) == 'both' .OR. TRIM(side_) == 'BOTH' .OR. &
        TRIM(side_) == 'left' .OR. TRIM(side_) == 'LEFT'  ) THEN 

       ALLOCATE( result(dim2,dim2), STAT=ierr )
          IF ( ierr /= 0 ) CALL errore(subname,'allocating result',ABS(ierr))
       ! 
       ! matrix mult
       CALL zmat_mul( result, z, 'C', z, 'N', dim2,dim2,dim1)

       DO j=1,dim2
       DO i=1,dim2
           IF ( i==j ) THEN
                IF ( ABS( result(i,j) -CONE ) > toll_ ) zmat_is_unitary = .FALSE.
           ELSE
                IF ( ABS( result(i,j) ) > toll_ ) zmat_is_unitary = .FALSE.
           ENDIF
       ENDDO
       ENDDO

       DEALLOCATE( result, STAT=ierr)
          IF ( ierr /= 0 ) CALL errore(subname,'deallocating result',ABS(ierr))
   ENDIF
       
   !
   ! check side RIGHT
   !
   IF ( TRIM(side_) == 'both' .OR. TRIM(side_) == 'BOTH' .OR. &
        TRIM(side_) == 'right'.OR. TRIM(side_) == 'RIGHT' ) THEN 

       ALLOCATE( result(dim1,dim1), STAT=ierr )
          IF ( ierr /= 0 ) CALL errore(subname,'allocating result',ABS(ierr))
       ! 
       ! matrix mult
       CALL zmat_mul( result, z, 'N', z, 'C', dim1,dim1,dim2)

       DO j=1,dim1
       DO i=1,dim1
           IF ( i==j ) THEN
                IF ( ABS( result(i,j) -CONE ) > toll_ ) zmat_is_unitary = .FALSE.
           ELSE
                IF ( ABS( result(i,j) ) > toll_ ) zmat_is_unitary = .FALSE.
           ENDIF
       ENDDO
       ENDDO

       DEALLOCATE( result, STAT=ierr)
          IF ( ierr /= 0 ) CALL errore(subname,'deallocating result',ABS(ierr))
   ENDIF
   RETURN
END FUNCTION zmat_is_unitary


!**********************************************************
   FUNCTION  zmat_rank( m, n, a, toll )
   !**********************************************************
   IMPLICIT NONE
   INTEGER          :: zmat_rank
   INTEGER          :: m,n
   COMPLEX(DP)     :: a(:,:)
   REAL(DP)        :: toll
   !
   INTEGER :: i,ierr 
   REAL(DP),    ALLOCATABLE :: s(:)
   COMPLEX(DP), ALLOCATABLE :: atmp(:,:), u(:,:), vt(:,:)

   IF ( m > SIZE(a,1) ) CALL errore('zmat_rank','Invalid m',ABS(m)+1)
   IF ( n > SIZE(a,2) ) CALL errore('zmat_rank','Invalid n',ABS(n)+1)

   ALLOCATE( atmp(m,n), u(m,m), vt(n,n), s(MIN(m,n)), STAT=ierr )
   IF ( ierr /=0 ) CALL errore('zmat_rank','allocating atmp--s',ABS(ierr))

   !
   ! local copy
   atmp(:,:) = a(1:m,1:n)
   !
   ! svd decomposition
   CALL mat_svd(m,n,atmp,s,u,vt)
   !
   ! TO BE SUBSTITUTED TO A CALL TO LOCATE
   zmat_rank = MIN(m,n)
   DO i=1, MIN(m,n)
       IF ( ABS(s(i)) < toll ) THEN
          zmat_rank = i-1
          EXIT
       ENDIF
   ENDDO
   !
   DEALLOCATE( atmp, u, vt, s, STAT=ierr )
   IF ( ierr /=0 ) CALL errore('zmat_rank','deallocating atmp--s',ABS(ierr))
   !
END FUNCTION zmat_rank


!**********************************************************
   FUNCTION  dmat_rank( m, n, a, toll )
   !**********************************************************
   IMPLICIT NONE
   INTEGER            :: dmat_rank
   INTEGER            :: m,n
   REAL(DP)          :: a(:,:)
   REAL(DP)          :: toll
   !
   INTEGER :: i,ierr 
   REAL(DP), ALLOCATABLE :: s(:)
   REAL(DP), ALLOCATABLE :: atmp(:,:), u(:,:), vt(:,:)


   IF ( m > SIZE(a,1) ) CALL errore('dmat_rank','Invalid m',ABS(m)+1)
   IF ( n > SIZE(a,2) ) CALL errore('dmat_rank','Invalid n',ABS(n)+1)

   ALLOCATE( atmp(m,n), u(m,m), vt(n,n), s(MIN(m,n)), STAT=ierr )
   IF ( ierr /=0 ) CALL errore('dmat_rank','allocating atmp--s',ABS(ierr))

   !
   ! local copy
   atmp(:,:) = a(1:m,1:n)

   !
   ! svd decomposition
   CALL mat_svd(m,n,atmp,s,u,vt)
   !
   ! TO BE SUBSTITUTED TO A CALL TO LOCATE
   dmat_rank = MIN(m,n)
   DO i=1, MIN(m,n)
       IF ( ABS(s(i)) < toll ) THEN
          dmat_rank = i-1
          EXIT
       ENDIF
   ENDDO
   !
   DEALLOCATE( atmp, u, vt, s, STAT=ierr )
   IF ( ierr /=0 ) CALL errore('dmat_rank','deallocating atmp--s',ABS(ierr))
   !
END FUNCTION dmat_rank


!**********************************************************
   FUNCTION  zmat_ge_dotp( m, n, a, b)
   !**********************************************************
   IMPLICIT NONE
   COMPLEX(DP)       :: zmat_ge_dotp
   INTEGER            :: m, n
   COMPLEX(DP)       :: a(:,:), b(:,:)
   !
   COMPLEX(DP) :: dotp
   INTEGER      :: i, j

   IF ( m > SIZE(a,1) ) CALL errore('zmat_dotp','Invalid m I',ABS(m)+1)
   IF ( n > SIZE(a,2) ) CALL errore('zmat_dotp','Invalid n I',ABS(n)+1)
   IF ( m > SIZE(b,1) ) CALL errore('zmat_dotp','Invalid m II',ABS(m)+1)
   IF ( n > SIZE(b,2) ) CALL errore('zmat_dotp','Invalid n II',ABS(n)+1)

   dotp = CZERO
   !
   DO j = 1, n
   DO i = 1, m
      dotp = dotp + a(i,j) * CONJG ( b(i,j) )
   ENDDO
   ENDDO
   !
   zmat_ge_dotp = dotp
   RETURN
   !
END FUNCTION zmat_ge_dotp


!**********************************************************
   FUNCTION  dmat_ge_dotp( m, n, a, b)
   !**********************************************************
   IMPLICIT NONE
   REAL(DP)       :: dmat_ge_dotp
   INTEGER         :: m, n
   REAL(DP)       :: a(:,:), b(:,:)
   !
   REAL(DP)    :: dotp
   INTEGER      :: i, j

   IF ( m > SIZE(a,1) ) CALL errore('dmat_ge_dotp','Invalid m I',ABS(m)+1)
   IF ( n > SIZE(a,2) ) CALL errore('dmat_ge_dotp','Invalid n I',ABS(n)+1)
   IF ( m > SIZE(b,1) ) CALL errore('dmat_ge_dotp','Invalid m II',ABS(m)+1)
   IF ( n > SIZE(b,2) ) CALL errore('dmat_ge_dotp','Invalid n II',ABS(n)+1)

   dotp = ZERO
   !
   DO j = 1, n
   DO i = 1, m
      dotp = dotp + a(i,j) * b(i,j)
   ENDDO
   ENDDO
   !
   dmat_ge_dotp = dotp
   RETURN
END FUNCTION dmat_ge_dotp


!**********************************************************
   FUNCTION  zmat_he_dotp( m, a, b)
   !**********************************************************
   IMPLICIT NONE
   COMPLEX(DP)       :: zmat_he_dotp
   INTEGER            :: m
   COMPLEX(DP)       :: a(:,:), b(:,:)
   !
   COMPLEX(DP) :: dotp
   INTEGER      :: i, j

   IF ( m > SIZE(a,1) ) CALL errore('zmat_hdotp','Invalid m1',ABS(m)+1)
   IF ( m > SIZE(a,2) ) CALL errore('zmat_hdotp','Invalid m2',ABS(m)+1)
   IF ( m > SIZE(b,1) ) CALL errore('zmat_hdotp','Invalid m3',ABS(m)+1)
   IF ( m > SIZE(b,2) ) CALL errore('zmat_hdotp','Invalid m4',ABS(m)+1)

   dotp = CZERO
   !
   DO j = 1, m
   DO i = 1, j-1
      dotp = dotp + 2.0_DP * CONJG( a(i,j) ) * b(i,j)
   ENDDO
   ENDDO
   !
   DO i = 1, m
      dotp = dotp + 1.0_DP * CONJG( a(i,i) ) * b(i,i)
   ENDDO
   !
   zmat_he_dotp = dotp
   RETURN
   !
END FUNCTION zmat_he_dotp


!**********************************************************
   FUNCTION  dmat_sy_dotp( m, a, b)
   !**********************************************************
   IMPLICIT NONE
   REAL(DP)          :: dmat_sy_dotp
   INTEGER            :: m
   REAL(DP)          :: a(:,:), b(:,:)
   !
   REAL(DP) :: dotp
   INTEGER   :: i, j

   IF ( m > SIZE(a,1) ) CALL errore('dmat_sy_dotp','Invalid m1',ABS(m)+1)
   IF ( m > SIZE(a,2) ) CALL errore('dmat_sy_dotp','Invalid m2',ABS(m)+1)
   IF ( m > SIZE(b,1) ) CALL errore('dmat_sy_dotp','Invalid m3',ABS(m)+1)
   IF ( m > SIZE(b,2) ) CALL errore('dmat_sy_dotp','Invalid m4',ABS(m)+1)

   dotp = ZERO
   !
   DO j = 1, m
   DO i = 1, j-1
      dotp = dotp + 2.0_DP * a(i,j) * b(i,j)
   ENDDO
   ENDDO
   !
   DO i = 1, m
      dotp = dotp + 1.0_DP * a(i,i) * b(i,i)
   ENDDO
   !
   dmat_sy_dotp = dotp
   RETURN
   !
END FUNCTION dmat_sy_dotp


!**********************************************************
   FUNCTION  zmat_hp_dotp( m, uplo, ap, bp)
   !**********************************************************
   !
   ! UPLO: 'U', 'u', provide the upper triangular parts of A, B,
   !       'L', 'l', provide the lower triangular parts
   !
   IMPLICIT NONE
   COMPLEX(DP)       :: zmat_hp_dotp
   INTEGER            :: m
   CHARACTER          :: uplo
   COMPLEX(DP)       :: ap(:), bp(:)
   !
   COMPLEX(DP) :: dotp
   INTEGER      :: i, j, l

   IF ( m*(m+1)/2 > SIZE(ap) ) CALL errore('zmat_hdotp','Invalid m A',ABS(m)+1)
   IF ( m*(m+1)/2 > SIZE(ap) ) CALL errore('zmat_hdotp','Invalid m B',ABS(m)+1)

   dotp = CZERO
   l    = 0
   !
   SELECT CASE ( uplo)
   CASE ( 'U', 'u' )
       !
       DO j = 1, m
           !
           l = (j-1) * j / 2
           !
           DO i = 1, j-1
               l = l+1
               dotp = dotp + 2.0_DP * CONJG( ap(l) ) * bp(l)
           ENDDO
           !
           l = l+1
           dotp = dotp + 1.0_DP * CONJG ( ap(l) ) * bp(l)
           !
       ENDDO
       !
   CASE ( 'L', 'l' )
       !
       DO j = 1, m
           !
           l = (j-1) * ( 2*m -j +2) /2 +1
           dotp = dotp + 1.0_DP * CONJG ( ap(l) ) * bp(l)
           !
           DO i = j+1, m
               l = l+1
               dotp = dotp + 2.0_DP * CONJG( ap(l) ) * bp(l)
           ENDDO
           !
           !
       ENDDO
       !
   CASE DEFAULT
       CALL errore('zmat_hdotp','invalid uplo: '//uplo, 2 )
   END SELECT
    
   !
   zmat_hp_dotp = dotp
   RETURN
   !
END FUNCTION zmat_hp_dotp

END MODULE util_module


