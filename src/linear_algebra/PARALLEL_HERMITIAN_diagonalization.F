!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
! Copyright (C) 2004 WanT Group
!
! Authors (see AUTHORS file for details): AM AF
!
subroutine PARALLEL_HERMITIAN_diagonalization(M, V, N, E)
 !
 ! perform the diagonalization by using scalapack
 !
 use pars,           ONLY:SP
 use parallel_int,   ONLY:PP_redux_wait
 use SLK_m,          ONLY:SLK_POOL,SLK_ORTHO
 use linear_algebra, ONLY:LINEAR_ALGEBRA_error,LINEAR_ALGEBRA_WS_reset,LALGEBRA_WS
 use matrix,         ONLY:PAR_matrix
 use timing_m,       ONLY:timing
!************************* SAVIO NEW *****************************
 use parallel_m,     ONLY:ncpu
!*****************************************************************
#include<memory.h>
 !
 integer           :: N
 type(PAR_matrix)  :: M,V
 real(SP)          :: E(N)
 !
 character(64)     :: subname="PARALLEL_HERMITIAN_diagonalization"
 integer           :: lwork,lrwork
 real(SP)          :: rvar(1)
 complex(SP)       :: cvar(1)
 type(LALGEBRA_WS) :: WS
!************************** SAVIO NEW ****************************
 integer(8)           :: lwork_kind8,liwork
 integer(8),parameter :: max_int_kind4=2147483647
 logical              :: go_to_kind8
 logical,parameter    :: use_pcheevd=.false.
!*****************************************************************
 !
 E=0.0_SP
 !
 if (SLK_ORTHO%coordinate(1)==-1) return
 !
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='start')
 !
!*********************** SAVIO COMMENTED *************************
! 
! The first call to PCHEEV looks like to not have any effect on
! the final result. The conditions lwork=-1 and lrwork=-1 are
! generally used to estimate the minimum size of the work and
! rwork arrays, respectively. However, these minimum size are
! never used in the code after this call. The code simply use a
! default size for these arrays. Some tests shown that commenting
! this call final results are not effected.
!-----------------------------------------------------------------
! lwork=-1
! lrwork=-1
! !
!#if defined _DOUBLE
! call PZHEEV&
!#else
! call PCHEEV&
!#endif
!&  ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1,V%desc,cvar,lwork,rvar,lrwork,WS%i_fail)
! !
! ! AF: A better estimates should be used
! lwork=2*n**2
! lrwork=2*n + 2*n-2
! YAMBOS_ALLOC(WS%v_cmplx,(lwork))
! YAMBOS_ALLOC(WS%v_real,(lrwork))
! !
!#if defined _DOUBLE
! call PZHEEV&
!#else
! call PCHEEV&
!#endif
!& ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1,V%desc,WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
!*****************************************************************
!************************** SAVIO NEW ****************************
 if (use_pcheevd) then
   lwork=n*(1+3*n/ncpu)
   lrwork=1+n*(9+3*n/ncpu)
   liwork=8*n
   !
   YAMBO_ALLOC(WS%v_cmplx,(lwork))
   YAMBO_ALLOC(WS%v_real,(lrwork))
   YAMBO_ALLOC(WS%v_int,(liwork))
   !
   call pzheevd('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1, &
 &              V%desc,WS%v_cmplx,lwork,WS%v_real,lrwork, &
 &              WS%v_int,liwork,WS%i_fail)
   !
 else
   go_to_kind8=.false.
   lwork_kind8=2*int(n,8)**2
   if (lwork_kind8 > max_int_kind4) go_to_kind8=.true.
   !
   lrwork=2*n + 2*n-2
   YAMBO_ALLOC(WS%v_real,(lrwork))
   !
   if (go_to_kind8) then
     YAMBO_ALLOC(WS%v_cmplx,(lwork_kind8))
     !
     ! To be extended to the Double case
     !
     call pcheev_kind8&
 &        ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1, &
 &          V%desc,WS%v_cmplx,lwork_kind8,WS%v_real,lrwork,WS%i_fail)
     !
   else
     lwork=2*n**2
     YAMBO_ALLOC(WS%v_cmplx,(lwork))
     !
#if defined _DOUBLE
     call PZHEEV&
#else
     call PCHEEV&
#endif
 &        ('V','U',N,M%blc(:,:,M%I),1,1,M%desc,E,V%blc(:,:,V%I),1,1, &
 &          V%desc,WS%v_cmplx,lwork,WS%v_real,lrwork,WS%i_fail)
     !
   endif
 endif
!*****************************************************************
 !
 if(WS%i_fail.ne.0) call LINEAR_ALGEBRA_error(subname,'performing P(Z/C)HEEV')
 !
 if (SLK_ORTHO%task/=0) E=0.0_SP
 !
 call PP_redux_wait(E,COMM=SLK_POOL%INTRA_comm)
 !
 call LINEAR_ALGEBRA_WS_reset(WS) 
 !
 call timing('PARALLEL_HERMITIAN_diagonalization',OPR='stop')
 !
end subroutine PARALLEL_HERMITIAN_diagonalization
!************************** SAVIO NEW ****************************
subroutine pcheev_kind8( JOBZ, UPLO, N, A, IA, JA, DESCA, W, Z, IZ, JZ, &
 &                       DESCZ, WORK, LWORK, RWORK, LRWORK, INFO )
!
!  -- ScaLAPACK routine (version 1.7) --
!     University of Tennessee, Knoxville, Oak Ridge National Laboratory,
!     and University of California, Berkeley.
!     August 14, 2001 
!
!     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            IA, INFO, IZ, JA, JZ, LRWORK, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * ), DESCZ( * )
      REAL               RWORK( * ), W( * )
      COMPLEX            A( * ), WORK( * ), Z( * )
!     ..
!
!  Purpose
!  =======
!
!  PCHEEV computes selected eigenvalues and, optionally, eigenvectors
!  of a real Hermitian matrix A by calling the recommended sequence
!  of ScaLAPACK routines.
!
!  In its present form, PCHEEV assumes a homogeneous system and makes
!  only spot checks of the consistency of the eigenvalues across the
!  different processes.  Because of this, it is possible that a
!  heterogeneous system may return incorrect results without any error
!  messages.
!
!  Notes
!  =====
!  A description vector is associated with each 2D block-cyclicly dis-
!  tributed matrix.  This vector stores the information required to
!  establish the mapping between a matrix entry and its corresponding
!  process and memory location.
!
!  In the following comments, the character _ should be read as
!  "of the distributed matrix".  Let A be a generic term for any 2D
!  block cyclicly distributed matrix.  Its description vector is DESCA:
!
!  NOTATION        STORED IN      EXPLANATION
!  --------------- -------------- --------------------------------------
!  DTYPE_A(global) DESCA( DTYPE_) The descriptor type.
!  CTXT_A (global) DESCA( CTXT_ ) The BLACS context handle, indicating
!                                 the BLACS process grid A is distribu-
!                                 ted over. The context itself is glo-
!                                 bal, but the handle (the integer
!                                 value) may vary.
!  M_A    (global) DESCA( M_ )    The number of rows in the distributed
!                                 matrix A.
!  N_A    (global) DESCA( N_ )    The number of columns in the distri-
!                                 buted matrix A.
!  MB_A   (global) DESCA( MB_ )   The blocking factor used to distribute
!                                 the rows of A.
!  NB_A   (global) DESCA( NB_ )   The blocking factor used to distribute
!                                 the columns of A.
!  RSRC_A (global) DESCA( RSRC_ ) The process row over which the first
!                                 row of the matrix A is distributed.
!  CSRC_A (global) DESCA( CSRC_ ) The process column over which the
!                                 first column of A is distributed.
!  LLD_A  (local)  DESCA( LLD_ )  The leading dimension of the local
!                                 array storing the local blocks of the
!                                 distributed matrix A.
!                                 LLD_A >= MAX(1,LOCr(M_A)).
!
!  Let K be the number of rows or columns of a distributed matrix,
!  and assume that its process grid has dimension p x q.
!  LOCr( K ) denotes the number of elements of K that a process
!  would receive if K were distributed over the p processes of its
!  process column.
!  Similarly, LOCc( K ) denotes the number of elements of K that a
!  process would receive if K were distributed over the q processes of
!  its process row.
!  The values of LOCr() and LOCc() may be determined via a call to the
!  ScaLAPACK tool function, NUMROC:
!          LOCr( M ) = NUMROC( M, MB_A, MYROW, RSRC_A, NPROW ),
!          LOCc( N ) = NUMROC( N, NB_A, MYCOL, CSRC_A, NPCOL ).
!
!  Arguments
!  =========
!
!     NP = the number of rows local to a given process.
!     NQ = the number of columns local to a given process.
!
!  JOBZ    (global input) CHARACTER*1
!          Specifies whether or not to compute the eigenvectors:
!          = 'N':  Compute eigenvalues only.
!          = 'V':  Compute eigenvalues and eigenvectors.
!
!  UPLO    (global input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          Hermitian matrix A is stored:
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular
!
!  N       (global input) INTEGER
!          The number of rows and columns of the matrix A.  N >= 0.
!
!  A       (local input/workspace) block cyclic COMPLEX array,
!          global dimension (N, N), local dimension ( LLD_A,
!          LOCc(JA+N-1) )
!
!          On entry, the Hermitian matrix A.  If UPLO = 'U', only the
!          upper triangular part of A is used to define the elements of
!          the Hermitian matrix.  If UPLO = 'L', only the lower
!          triangular part of A is used to define the elements of the
!          Hermitian matrix.
!
!          On exit, the lower triangle (if UPLO='L') or the upper
!          triangle (if UPLO='U') of A, including the diagonal, is
!          destroyed.
!
!  IA      (global input) INTEGER
!          A's global row index, which points to the beginning of the
!          submatrix which is to be operated on.
!
!  JA      (global input) INTEGER
!          A's global column index, which points to the beginning of
!          the submatrix which is to be operated on.
!
!  DESCA   (global and local input) INTEGER array of dimension DLEN_.
!          The array descriptor for the distributed matrix A.
!          If DESCA( CTXT_ ) is incorrect, PCHEEV cannot guarantee
!          correct error reporting.
!
!  W       (global output) REAL array, dimension (N)
!          If INFO=0, the eigenvalues in ascending order.
!
!  Z       (local output) COMPLEX array,
!          global dimension (N, N),
!          local dimension (LLD_Z, LOCc(JZ+N-1))
!          If JOBZ = 'V', then on normal exit the first M columns of Z
!          contain the orthonormal eigenvectors of the matrix
!          corresponding to the selected eigenvalues.
!          If JOBZ = 'N', then Z is not referenced.
!
!  IZ      (global input) INTEGER
!          Z's global row index, which points to the beginning of the
!          submatrix which is to be operated on.
!
!  JZ      (global input) INTEGER
!          Z's global column index, which points to the beginning of
!          the submatrix which is to be operated on.
!
!  DESCZ   (global and local input) INTEGER array of dimension DLEN_.
!          The array descriptor for the distributed matrix Z.
!          DESCZ( CTXT_ ) must equal DESCA( CTXT_ )
!
!  WORK    (local workspace/output) COMPLEX array,
!          dimension (LWORK)
!          On output, WORK(1) returns the workspace needed to guarantee
!          completion.  If the input parameters are incorrect, WORK(1)
!          may also be incorrect.
!
!          If JOBZ='N' WORK(1) = minimal workspace for eigenvalues only.
!          If JOBZ='V' WORK(1) = minimal workspace required to
!             generate all the eigenvectors.
!
!
!  LWORK   (local input) INTEGER
!          See below for definitions of variables used to define LWORK.
!          If no eigenvectors are requested (JOBZ = 'N') then
!             LWORK >= MAX( NB*( NP0+1 ), 3 ) +3*N
!          If eigenvectors are requested (JOBZ = 'V' ) then
!          the amount of workspace required:
!             LWORK >= (NP0 + NQ0 + NB)*NB + 3*N + N^2
!
!          Variable definitions:
!             NB = DESCA( MB_ ) = DESCA( NB_ ) =
!                  DESCZ( MB_ ) = DESCZ( NB_ )
!             NP0 = NUMROC( NN, NB, 0, 0, NPROW )
!             NQ0 = NUMROC( MAX( N, NB, 2 ), NB, 0, 0, NPCOL )
!
!          If LWORK = -1, the LWORK is global input and a workspace
!          query is assumed; the routine only calculates the minimum
!          size for the WORK array.  The required workspace is returned
!          as the first element of WORK and no error message is issued
!          by PXERBLA.
!
!  RWORK   (local workspace/output) COMPLEX array,
!          dimension (LRWORK)
!          On output RWORK(1) returns the
!          REAL workspace needed to
!          guarantee completion.  If the input parameters are incorrect,
!          RWORK(1) may also be incorrect.
!
!  LRWORK  (local input) INTEGER
!          Size of RWORK array.
!          If eigenvectors are desired (JOBZ = 'V') then
!             LRWORK >= 2*N + 2*N-2
!          If eigenvectors are not desired (JOBZ = 'N') then
!             LRWORK >= 2*N
!
!          If LRWORK = -1, the LRWORK is global input and a workspace
!          query is assumed; the routine only calculates the minimum
!          size for the RWORK array.  The required workspace is returned
!          as the first element of RWORK and no error message is issued
!          by PXERBLA.
!
!  INFO    (global output) INTEGER
!          = 0:  successful exit
!          < 0:  If the i-th argument is an array and the j-entry had
!                an illegal value, then INFO = -(i*100+j), if the i-th
!                argument is a scalar and had an illegal value, then
!                INFO = -i.
!          > 0:  If INFO = 1 through N, the i(th) eigenvalue did not
!                converge in CSTEQR2 after a total of 30*N iterations.
!                If INFO = N+1, then PCHEEV has detected heterogeneity
!                by finding that eigenvalues were not identical across
!                the process grid.  In this case, the accuracy of
!                the results from PCHEEV cannot be guaranteed.
!
!  Alignment requirements
!  ======================
!
!  The distributed submatrices A(IA:*, JA:*) and C(IC:IC+M-1,JC:JC+N-1)
!  must verify some alignment properties, namely the following
!  expressions should be true:
!
!  ( MB_A.EQ.NB_A.EQ.MB_Z .AND. IROFFA.EQ.IROFFZ .AND. IROFFA.EQ.0 .AND.
!    IAROW.EQ.IZROW )
!  where
!  IROFFA = MOD( IA-1, MB_A ) and ICOFFA = MOD( JA-1, NB_A ).
!
!     =====================================================================
!     
!  Version 1.4 limitations:
!     DESCA(MB_) = DESCA(NB_)
!     DESCA(M_) = DESCZ(M_)
!     DESCA(N_) = DESCZ(N_)
!     DESCA(MB_) = DESCZ(MB_)
!     DESCA(NB_) = DESCZ(NB_)
!     DESCA(RSRC_) = DESCZ(RSRC_)
!
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, DLEN_, DTYPE_, CTXT_, M_, N_, &
 &                       mb_, nb_, rsrc_, csrc_, lld_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                       ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                       rsrc_ = 7, csrc_ = 8, lld_ = 9 )
      REAL               ZERO, ONE
      parameter( zero = 0.0e+0, one = 1.0e+0 )
      COMPLEX            CZERO, CONE
      parameter( czero = ( 0.0e+0, 0.0e+0 ), &
 &                       cone = ( 1.0e+0, 0.0e+0 ) )
      INTEGER            ITHVAL
      parameter( ithval = 10 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LOWER, WANTZ
      INTEGER            CONTEXTC, CSRC_A, I, IACOL, IAROW, ICOFFA, &
 &                       iinfo, indd, inde, indrd, indre, indrwork, &
 &                       indtau, indwork, indwork2, iroffa, iroffz, &
 &                       iscale, izrow, j, k, ldc, llrwork, &
 &                       lrmin, lrwmin, lwmin, mb_a, mb_z, mycol, &
 &                       mypcolc, myprowc, myrow, nb, nb_a, nb_z, np0, &
 &                       npcol, npcolc, nprocs, nprow, nprowc, nq0, nrc, &
 &                       rsizecsteqr2, rsrc_a, rsrc_z, sizecsteqr2, &
 &                       sizepchetrd, sizepcunmtr
      INTEGER(8)         llwork
      REAL               ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA, &
 &                       smlnum
!     ..
!     .. Local Arrays ..
      INTEGER            DESCQR( 10 ), IDUM1( 3 ), IDUM2( 3 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            INDXG2P, NUMROC, SL_GRIDRESHAPE
      REAL               PCLANHE, PSLAMCH
      EXTERNAL           lsame, indxg2p, numroc, sl_gridreshape, &
 &                       pclanhe, pslamch
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_gridexit, blacs_gridinfo, chk1mat, &
 &                       csteqr2, descinit, pcelget, pcgemr2d, pchetrd, &
 &                       pchk1mat, pchk2mat, pclascl, pclaset, pcunmtr, &
 &                       pxerbla, scopy, sgamn2d, sgamx2d, sscal
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          abs, cmplx, ichar, int, max, min, mod, real, sqrt
!     ..
!     .. Executable Statements ..
!       This is just to keep ftnchek and toolpack/1 happy
      IF( block_cyclic_2d*csrc_*ctxt_*dlen_*dtype_*lld_*mb_*m_*nb_*n_* &
 &        rsrc_.LT.0 )RETURN
!
!     Quick return
!
      IF( n.EQ.0 ) RETURN
!
!     Test the input arguments.
!
      CALL blacs_gridinfo( desca( ctxt_ ), nprow, npcol, myrow, mycol )
      info = 0
!
!     Initialize pointer to some safe value
!
      indtau = 1
      indd = 1
      inde = 1
      indwork = 1
      indwork2 = 1
!
      indre = 1
      indrd = 1
      indrwork = 1
!
      wantz = lsame( jobz, 'V' )
      IF( nprow.EQ.-1 ) THEN
         info = -( 700+ctxt_ )
      ELSE IF( wantz ) THEN
         IF( desca( ctxt_ ).NE.descz( ctxt_ ) ) THEN
            info = -( 1200+ctxt_ )
         END IF
      END IF
      IF( info.EQ.0 ) THEN
         CALL chk1mat( n, 3, n, 3, ia, ja, desca, 7, info )
         IF( wantz ) CALL chk1mat( n, 3, n, 3, iz, jz, descz, 12, info )
!
         IF( info.EQ.0 ) THEN
!
!           Get machine constants.
!
            safmin = pslamch( desca( ctxt_ ), 'Safe minimum' )
            eps = pslamch( desca( ctxt_ ), 'Precision' )
            smlnum = safmin / eps
            bignum = one / smlnum
            rmin = sqrt( smlnum )
            rmax = min( sqrt( bignum ), one / sqrt( sqrt( safmin ) ) )
!
            nprocs = nprow*npcol
            nb_a = desca( nb_ )
            mb_a = desca( mb_ )
            nb = nb_a
            lower = lsame( uplo, 'L' )
!
            rsrc_a = desca( rsrc_ )
            csrc_a = desca( csrc_ )
            iroffa = mod( ia-1, mb_a )
            icoffa = mod( ja-1, nb_a )
            iarow = indxg2p( 1, nb_a, myrow, rsrc_a, nprow )
            iacol = indxg2p( 1, mb_a, mycol, csrc_a, npcol )
            np0 = numroc( n+iroffa, nb, myrow, iarow, nprow )
            nq0 = numroc( n+icoffa, nb, mycol, iacol, npcol )
            IF( wantz ) THEN
               nb_z = descz( nb_ )
               mb_z = descz( mb_ )
               rsrc_z = descz( rsrc_ )
               iroffz = mod( iz-1, mb_a )
               izrow = indxg2p( 1, nb_a, myrow, rsrc_z, nprow )
            ELSE
               iroffz = 0
               izrow = 0
            END IF
!
!           COMPLEX work space for PCHETRD
!
            CALL pchetrd( uplo, n, a, ia, ja, desca, rwork( indd ), &
 &                       rwork( inde ), work( indtau ), &
 &                       work( indwork ), -1, iinfo )
            sizepchetrd = int( abs( work( 1 ) ) )
!
!           COMPLEX work space for PCUNMTR
!
            IF( wantz ) THEN
               CALL pcunmtr( 'L', uplo, 'N', n, n, a, ia, ja, desca, &
 &                           work( indtau ), z, iz, jz, descz, &
 &                           work( indwork ), -1, iinfo )
               sizepcunmtr = int( abs( work( 1 ) ) )
            ELSE
               sizepcunmtr = 0
            END IF
!
!           REAL work space for CSTEQR2
!
            IF( wantz ) THEN
               rsizecsteqr2 = min( 1, 2*n-2 )
            ELSE
               rsizecsteqr2 = 0
            END IF
!
!           Initialize the context of the single column distributed
!           matrix required by CSTEQR2. This specific distribution
!           allows each process to do 1/pth of the work updating matrix
!           Q during CSTEQR2 and achieve some parallelization to an
!           otherwise serial subroutine.
!
            ldc = 0
            IF( wantz ) THEN
               contextc = sl_gridreshape( desca( ctxt_ ), 0, 1, 1, nprocs, 1 )
               CALL blacs_gridinfo( contextc, nprowc, npcolc, myprowc, mypcolc )
               nrc = numroc( n, nb_a, myprowc, 0, nprocs )
               ldc = max( 1, nrc )
               CALL descinit( descqr, n, n, nb, nb, 0, 0, contextc, ldc, info )
            END IF
!
!           COMPLEX work space for CSTEQR2
!
            IF( wantz ) THEN
               sizecsteqr2 = n*ldc
            ELSE
               sizecsteqr2 = 0
            END IF
!
!           Set up pointers into the WORK array
!
            indtau = 1
            indd = indtau + n
            inde = indd + n
            indwork = inde + n
            indwork2 = indwork + n*ldc
            llwork = lwork - indwork + 1
!           Set up pointers into the RWORK array
!
            indre = 1
            indrd = indre + n
            indrwork = indrd + n
            llrwork = lrwork - indrwork + 1
!
!           Compute the total amount of space needed
!
            lrwmin = 2*n + rsizecsteqr2
            lwmin = 3*n + max( sizepchetrd, sizepcunmtr, sizecsteqr2 )
!
         END IF
         IF( info.EQ.0 ) THEN
            IF( .NOT.( wantz .OR. lsame( jobz, 'N' ) ) ) THEN
               info = -1
            ELSE IF( .NOT.( lower .OR. lsame( uplo, 'U' ) ) ) THEN
               info = -2
            ELSE IF( lwork.LT.lwmin .AND. lwork.NE.-1 ) THEN
               info = -14
            ELSE IF( lrwork.LT.lrwmin .AND. lrwork.NE.-1 ) THEN
               info = -16
            ELSE IF( iroffa.NE.0 ) THEN
               info = -5
            ELSE IF( desca( mb_ ).NE.desca( nb_ ) ) THEN
               info = -( 700+nb_ )
            END IF
            IF( wantz ) THEN
               IF( iroffa.NE.iroffz ) THEN
                  info = -10
               ELSE IF( iarow.NE.izrow ) THEN
                  info = -10
               ELSE IF( desca( m_ ).NE.descz( m_ ) ) THEN
                  info = -( 1200+m_ )
               ELSE IF( desca( n_ ).NE.descz( n_ ) ) THEN
                  info = -( 1200+n_ )
               ELSE IF( desca( mb_ ).NE.descz( mb_ ) ) THEN
                  info = -( 1200+mb_ )
               ELSE IF( desca( nb_ ).NE.descz( nb_ ) ) THEN
                  info = -( 1200+nb_ )
               ELSE IF( desca( rsrc_ ).NE.descz( rsrc_ ) ) THEN
                  info = -( 1200+rsrc_ )
               ELSE IF( desca( ctxt_ ).NE.descz( ctxt_ ) ) THEN
                  info = -( 1200+ctxt_ )
               END IF
            END IF
         END IF
         IF( wantz ) THEN
            idum1( 1 ) = ichar( 'V' )
         ELSE
            idum1( 1 ) = ichar( 'N' )
         END IF
         idum2( 1 ) = 1
         IF( lower ) THEN
            idum1( 2 ) = ichar( 'L' )
         ELSE
            idum1( 2 ) = ichar( 'U' )
         END IF
         idum2( 2 ) = 2
         IF( lwork.EQ.-1 ) THEN
            idum1( 3 ) = -1
         ELSE
            idum1( 3 ) = 1
         END IF
         idum2( 3 ) = 3
         IF( wantz ) THEN
            CALL pchk2mat( n, 3, n, 3, ia, ja, desca, 7, n, 3, n, 3, iz, &
 &                         jz, descz, 12, 3, idum1, idum2, info )
         ELSE
            CALL pchk1mat( n, 3, n, 3, ia, ja, desca, 7, 3, idum1, &
 &                         idum2, info )
         END IF
         work( 1 ) = cmplx( lwmin )
         rwork( 1 ) = real( lrwmin )
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( desca( ctxt_ ), 'PCHEEV', -info )
         IF( wantz ) CALL blacs_gridexit( contextc )
         RETURN
      ELSE IF( lwork.EQ.-1 .OR. lrwork.EQ.-1 ) THEN
         IF( wantz ) CALL blacs_gridexit( contextc )
         RETURN
      END IF
!
!     Scale matrix to allowable range, if necessary.
!
      iscale = 0
!
      anrm = pclanhe( 'M', uplo, n, a, ia, ja, desca, rwork( indrwork ) )
!
!
      IF( anrm.GT.zero .AND. anrm.LT.rmin ) THEN
         iscale = 1
         sigma = rmin / anrm
      ELSE IF( anrm.GT.rmax ) THEN
         iscale = 1
         sigma = rmax / anrm
      END IF
!
      IF( iscale.EQ.1 ) THEN
         CALL pclascl( uplo, one, sigma, n, n, a, ia, ja, desca, iinfo )
      END IF
!
!     Reduce Hermitian matrix to tridiagonal form.
!
      CALL pchetrd_kind8( uplo, n, a, ia, ja, desca, rwork( indrd ), &
 &                        rwork( indre ), work( indtau ), work( indwork ), &
 &                        llwork, iinfo )
!
!     Copy the values of D, E to all processes.
!
      DO 10 i = 1, n
         CALL pcelget( 'A', ' ', work( indd+i-1 ), a, i+ia-1, i+ja-1, desca )
         rwork( indrd+i-1 ) = real( work( indd+i-1 ) )
   10 CONTINUE
      IF( lsame( uplo, 'U' ) ) THEN
         DO 20 i = 1, n - 1
            CALL pcelget( 'A', ' ', work( inde+i-1 ), a, i+ia-1, i+ja, desca )
            rwork( indre+i-1 ) = real( work( inde+i-1 ) )
   20    CONTINUE
      ELSE
         DO 30 i = 1, n - 1
            CALL pcelget( 'A', ' ', work( inde+i-1 ), a, i+ia, i+ja-1, desca )
            rwork( indre+i-1 ) = real( work( inde+i-1 ) )
   30    CONTINUE
      END IF
!
      IF( wantz ) THEN
!
         CALL pclaset( 'Full', n, n, czero, cone, work( indwork ), 1, 1, descqr )
!
!        CSTEQR2 is a modified version of LAPACK's CSTEQR.  The
!        modifications allow each process to perform partial updates
!        to matrix Q.
!
         CALL csteqr2( 'I', n, rwork( indrd ), rwork( indre ), &
 &                     work( indwork ), ldc, nrc, rwork( indrwork ), &
 &                     info )
!
         CALL pcgemr2d( n, n, work( indwork ), 1, 1, descqr, z, ia, ja, &
 &                      descz, contextc )
!
         CALL pcunmtr_kind8( 'L', uplo, 'N', n, n, a, ia, ja, desca, &
 &                           work( indtau ), z, iz, jz, descz, &
 &                           work( indwork ), llwork, iinfo )
!
      ELSE
!
         CALL csteqr2( 'N', n, rwork( indrd ), rwork( indre ), &
 &                     work( indwork ), 1, 1, rwork( indrwork ), info )
      END IF
!
!     Copy eigenvalues from workspace to output array
!
      CALL scopy( n, rwork( indd ), 1, w, 1 )
!
!     If matrix was scaled, then rescale eigenvalues appropriately.
!
      IF( iscale.EQ.1 ) THEN
         CALL sscal( n, one / sigma, w, 1 )
      END IF
!
      work( 1 ) = real( lwmin )
!
!     Free up resources
!
      IF( wantz ) THEN
         CALL blacs_gridexit( contextc )
      END IF
!
!     Compare every ith eigenvalue, or all if there are only a few,
!     across the process grid to check for heterogeneity.
!
      IF( n.LE.ithval ) THEN
         j = n
         k = 1
      ELSE
         j = n / ithval
         k = ithval
      END IF
!
      lrmin = int( rwork( 1 ) )
      indtau = 0
      inde = indtau + j
      DO 40 i = 1, j
         rwork( i+indtau ) = w( ( i-1 )*k+1 )
         rwork( i+inde ) = w( ( i-1 )*k+1 )
   40 CONTINUE
!
      CALL sgamn2d( desca( ctxt_ ), 'All', ' ', j, 1, rwork( 1+indtau ), &
 &                  j, 1, 1, -1, -1, 0 )
      CALL sgamx2d( desca( ctxt_ ), 'All', ' ', j, 1, rwork( 1+inde ), &
 &                  j, 1, 1, -1, -1, 0 )
!
      DO 50 i = 1, j
         IF( info.EQ.0 .AND. ( rwork( i+indtau )-rwork( i+inde ).NE. &
 &           zero ) ) THEN
            info = n + 1
         END IF
   50 CONTINUE
      rwork( 1 ) = lrmin
!
      RETURN
!
!     End of PCHEEV
!
end subroutine pcheev_kind8
!===============================================================================
!===============================================================================
subroutine pchetrd_kind8( UPLO, N, A, IA, JA, DESCA, D, E, TAU, WORK, &
 &                        LWORK, INFO )
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            IA, INFO, JA, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * )
      REAL               D( * ), E( * )
      COMPLEX            A( * ), TAU( * ), WORK( * )
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
      REAL               ONE
      parameter( one = 1.0e+0 )
      COMPLEX            CONE
      parameter( cone = ( 1.0e+0, 0.0e+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER
      CHARACTER          COLCTOP, ROWCTOP
      INTEGER            I, IACOL, IAROW, ICOFFA, ICTXT, IINFO, IPW, &
 &                       iroffa, j, jb, jx, k, kk, lwmin, mycol, myrow, &
 &                       nb, np, npcol, nprow, nq
!     ..
!     .. Local Arrays ..
      INTEGER            DESCW( DLEN_ ), IDUM1( 2 ), IDUM2( 2 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_gridinfo, chk1mat, descset, pcher2k, &
 &                       pchetd2, pchk1mat, pclatrd, pb_topget, &
 &                       pb_topset, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            INDXG2L, INDXG2P, NUMROC
      EXTERNAL           lsame, indxg2l, indxg2p, numroc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, ichar, max, min, mod, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(600+ctxt_)
      ELSE
         CALL chk1mat( n, 2, n, 2, ia, ja, desca, 6, info )
         upper = lsame( uplo, 'U' )
         IF( info.EQ.0 ) THEN
            nb = desca( nb_ )
            iroffa = mod( ia-1, desca( mb_ ) )
            icoffa = mod( ja-1, desca( nb_ ) )
            iarow = indxg2p( ia, nb, myrow, desca( rsrc_ ), nprow )
            iacol = indxg2p( ja, nb, mycol, desca( csrc_ ), npcol )
            np = numroc( n, nb, myrow, iarow, nprow )
            nq = max( 1, numroc( n+ja-1, nb, mycol, desca( csrc_ ), npcol ) )
            lwmin = max( (np+1)*nb, 3*nb )
!
            work( 1 ) = cmplx( real( lwmin ) )
            lquery = ( lwork.EQ.-1 )
            IF( .NOT.upper .AND. .NOT.lsame( uplo, 'L' ) ) THEN
               info = -1
            ELSE IF( iroffa.NE.icoffa .OR. icoffa.NE.0 ) THEN
               info = -5
            ELSE IF( desca( mb_ ).NE.desca( nb_ ) ) THEN
               info = -(600+nb_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -11
            END IF
         END IF
         IF( upper ) THEN
            idum1( 1 ) = ichar( 'U' )
         ELSE
            idum1( 1 ) = ichar( 'L' )
         END IF
         idum2( 1 ) = 1
         IF( lwork.EQ.-1 ) THEN
            idum1( 2 ) = -1
         ELSE
            idum1( 2 ) = 1
         END IF
         idum2( 2 ) = 11
         CALL pchk1mat( n, 2, n, 2, ia, ja, desca, 6, 2, idum1, idum2, info )
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCHETRD', -info )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( n.EQ.0 ) RETURN
!
      CALL pb_topget( ictxt, 'Combine', 'Columnwise', colctop )
      CALL pb_topget( ictxt, 'Combine', 'Rowwise',    rowctop )
      CALL pb_topset( ictxt, 'Combine', 'Columnwise', '1-tree' )
      CALL pb_topset( ictxt, 'Combine', 'Rowwise',    '1-tree' )
!
      ipw = np * nb + 1
!
      IF( upper ) THEN
!
!        Reduce the upper triangle of sub( A ).
!
         kk = mod( ja+n-1, nb )
         IF( kk.EQ.0 ) kk = nb
         CALL descset( descw, n, nb, nb, nb, iarow, indxg2p( ja+n-kk, &
 &                     nb, mycol, desca( csrc_ ), npcol ), ictxt, &
 &                     max( 1, np ) )
!
         DO 10 k = n-kk+1, nb+1, -nb
            jb = min( n-k+1, nb )
            i = ia + k - 1
            j = ja + k - 1
!
!           Reduce columns I:I+NB-1 to tridiagonal form and form
!           the matrix W which is needed to update the unreduced part of
!           the matrix
!
            CALL pclatrd( uplo, k+jb-1, jb, a, ia, ja, desca, d, e, tau, &
 &                        work, 1, 1, descw, work( ipw ) )
!
!           Update the unreduced submatrix A(IA:I-1,JA:J-1), using an
!           update of the form:
!           A(IA:I-1,JA:J-1) := A(IA:I-1,JA:J-1) - V*W' - W*V'
!
            CALL pcher2k( uplo, 'No transpose', k-1, jb, -cone, a, ia, &
 &                        j, desca, work, 1, 1, descw, one, a, ia, ja, &
 &                        desca )
!
!           Copy last superdiagonal element back into sub( A )
!
            jx = min( indxg2l( j, nb, 0, iacol, npcol ), nq )
            CALL pcelset( a, i-1, j, desca, cmplx( e( jx ) ) )
!
            descw( csrc_ ) = mod( descw( csrc_ ) + npcol - 1, npcol )
!
   10    CONTINUE
!
!        Use unblocked code to reduce the last or only block
!
         CALL pchetd2_kind8( uplo, min( n, nb ), a, ia, ja, desca, d, e, &
 &                           tau, work, lwork, iinfo )
!
      ELSE
!
!        Reduce the lower triangle of sub( A )
!
         kk = mod( ja+n-1, nb )
         IF( kk.EQ.0 ) kk = nb
         CALL descset( descw, n, nb, nb, nb, iarow, iacol, ictxt, max( 1, np ) )
!
         DO 20 k = 1, n-nb, nb
            i = ia + k - 1
            j = ja + k - 1
!
!           Reduce columns I:I+NB-1 to tridiagonal form and form
!           the matrix W which is needed to update the unreduced part
!           of the matrix
!
            CALL pclatrd( uplo, n-k+1, nb, a, i, j, desca, d, e, tau, &
 &                        work, k, 1, descw, work( ipw ) )
!
!           Update the unreduced submatrix A(I+NB:IA+N-1,I+NB:IA+N-1),
!           using an update of the form: A(I+NB:IA+N-1,I+NB:IA+N-1) :=
!           A(I+NB:IA+N-1,I+NB:IA+N-1) - V*W' - W*V'
!
            CALL pcher2k( uplo, 'No transpose', n-k-nb+1, nb, -cone, a, &
 &                        i+nb, j, desca, work, k+nb, 1, descw, one, a, &
 &                        i+nb, j+nb, desca )
!
!           Copy last subdiagonal element back into sub( A )
!
            jx = min( indxg2l( j+nb-1, nb, 0, iacol, npcol ), nq )
            CALL pcelset( a, i+nb, j+nb-1, desca, cmplx( e( jx ) ) )
!
            descw( csrc_ ) = mod( descw( csrc_ ) + 1, npcol )
!
   20    CONTINUE
!
!        Use unblocked code to reduce the last or only block
!
         CALL pchetd2_kind8( uplo, kk, a, ia+k-1, ja+k-1, desca, d, e, &
 &                           tau, work, lwork, iinfo )
      END IF
!
      CALL pb_topset( ictxt, 'Combine', 'Columnwise', colctop )
      CALL pb_topset( ictxt, 'Combine', 'Rowwise',    rowctop )
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCHETRD
!
end subroutine pchetrd_kind8
!===============================================================================
!===============================================================================
subroutine pcunmtr_kind8( SIDE, UPLO, TRANS, M, N, A, IA, JA, DESCA, &
 &                        TAU, C, IC, JC, DESCC, WORK, LWORK, INFO )
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, UPLO
      INTEGER            IA, IC, INFO, JA, JC, M, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * ), DESCC( * )
      COMPLEX            A( * ), C( * ), TAU( * ), WORK( * )
!     ..
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN, UPPER
      INTEGER            IAA, IAROW, ICC, ICCOL, ICOFFC, ICROW, ICTXT, &
 &                       iinfo, iroffa, iroffc, jaa, jcc, lcm, lcmq, &
 &                       lwmin, mi, mpc0, mycol, myrow, ni, npa0, npcol, &
 &                       nprow, nq, nqc0
!     ..
!     .. Local Arrays ..
      INTEGER            IDUM1( 4 ), IDUM2( 4 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_gridinfo, chk1mat, pchk2mat, pcunmql, &
 &                       pcunmqr, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILCM, INDXG2P, NUMROC
      EXTERNAL           ilcm, indxg2p, lsame, numroc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, ichar, max, mod, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(900+ctxt_)
      ELSE
         left = lsame( side, 'L' )
         notran = lsame( trans, 'N' )
         upper = lsame( uplo, 'U' )
!
         IF( upper ) THEN
            iaa = ia
            jaa = ja+1
            icc = ic
            jcc = jc
         ELSE
            iaa = ia+1
            jaa = ja
            IF( left ) THEN
               icc = ic + 1
               jcc = jc
            ELSE
               icc = ic
               jcc = jc + 1
             END IF
         END IF
!
!        NQ is the order of Q
!
         IF( left ) THEN
            nq = m
            mi = m - 1
            ni = n
            CALL chk1mat( mi, 4, nq-1, 4, iaa, jaa, desca, 9, info )
         ELSE
            nq = n
            mi = m
            ni = n - 1
            CALL chk1mat( ni, 5, nq-1, 5, iaa, jaa, desca, 9, info )
         END IF
         CALL chk1mat( mi, 4, ni, 5, icc, jcc, descc, 14, info )
         IF( info.EQ.0 ) THEN
            iroffa = mod( iaa-1, desca( mb_ ) )
            iroffc = mod( icc-1, descc( mb_ ) )
            icoffc = mod( jcc-1, descc( nb_ ) )
            iarow = indxg2p( iaa, desca( mb_ ), myrow, desca( rsrc_ ), nprow )
            icrow = indxg2p( icc, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
            iccol = indxg2p( jcc, descc( nb_ ), mycol, descc( csrc_ ), npcol )
            mpc0 = numroc( mi+iroffc, descc( mb_ ), myrow, icrow, nprow )
            nqc0 = numroc( ni+icoffc, descc( nb_ ), mycol, iccol, npcol )
!
            IF( left ) THEN
               lwmin = max( ( desca( nb_ ) * ( desca( nb_ ) - 1 ) ) / 2, &
 &                          ( mpc0 + nqc0 ) * desca( nb_ ) ) + &
 &                     desca( nb_ ) * desca( nb_ )
            ELSE
               npa0 = numroc( ni+iroffa, desca( mb_ ), myrow, iarow, &
 &                            nprow )
               lcm = ilcm( nprow, npcol )
               lcmq = lcm / npcol
               lwmin =  max( ( desca( nb_ ) * ( desca( nb_ ) - 1 ) ) &
 &                      / 2, ( nqc0 + max( npa0 + numroc( numroc( &
 &                      ni+icoffc, desca( nb_ ), 0, 0, npcol ), &
 &                      desca( nb_ ), 0, 0, lcmq ), mpc0 ) ) * &
 &                      desca( nb_ ) ) + desca( nb_ ) * desca( nb_ )
            END IF
!
            work( 1 ) = cmplx( real( lwmin ) )
            lquery = ( lwork.EQ.-1 )
            IF( .NOT.left .AND. .NOT.lsame( side, 'R' ) ) THEN
               info = -1
            ELSE IF( .NOT.upper .AND. .NOT.lsame( uplo, 'L' ) ) THEN
               info = -2
            ELSE IF( .NOT.lsame( trans, 'N' ) .AND. .NOT.lsame( trans, 'C' ) ) THEN
               info = -3
            ELSE IF( .NOT.left .AND. desca( mb_ ).NE.descc( nb_ ) ) THEN
               info = -(900+nb_)
            ELSE IF( left .AND. iroffa.NE.iroffc ) THEN
               info = -12
            ELSE IF( left .AND. iarow.NE.icrow ) THEN
               info = -12
            ELSE IF( .NOT.left .AND. iroffa.NE.icoffc ) THEN
               info = -13
            ELSE IF( left .AND. desca( mb_ ).NE.descc( mb_ ) ) THEN
               info = -(1400+mb_)
            ELSE IF( ictxt.NE.descc( ctxt_ ) ) THEN
               info = -(1400+ctxt_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -16
            END IF
         END IF
!
         IF( left ) THEN
            idum1( 1 ) = ichar( 'L' )
         ELSE
            idum1( 1 ) = ichar( 'R' )
         END IF
         idum2( 1 ) = 1
         IF( upper ) THEN
            idum1( 2 ) = ichar( 'U' )
         ELSE
            idum1( 2 ) = ichar( 'L' )
         END IF
         idum2( 2 ) = 2
         IF( notran ) THEN
            idum1( 3 ) = ichar( 'N' )
         ELSE
            idum1( 3 ) = ichar( 'C' )
         END IF
         idum2( 3 ) = 3
         IF( lwork.EQ.-1 ) THEN
            idum1( 4 ) = -1
         ELSE
            idum1( 4 ) = 1
         END IF
         idum2( 4 ) = 16
         IF( left ) THEN
            CALL pchk2mat( mi, 4, nq-1, 4, iaa, jaa, desca, 9, mi, 4, &
 &                         ni, 5, icc, jcc, descc, 14, 4, idum1, idum2, &
 &                         info )
         ELSE
            CALL pchk2mat( ni, 5, nq-1, 5, iaa, jaa, desca, 9, mi, 4, &
 &                         ni, 5, icc, jcc, descc, 14, 4, idum1, idum2, &
 &                         info )
         END IF
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCUNMTR', -info )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( m.EQ.0 .OR. n.EQ.0 .OR. nq.EQ.1 ) RETURN
!
      IF( upper ) THEN
!
!        Q was determined by a call to PCHETRD with UPLO = 'U'
!
         CALL pcunmql_kind8( side, trans, mi, ni, nq-1, a, ia, jaa, desca, &
 &                           tau, c, icc, jcc, descc, work, lwork, iinfo )
!
      ELSE
!
!        Q was determined by a call to PCHETRD with UPLO = 'L'
!
         CALL pcunmqr_kind8( side, trans, mi, ni, nq-1, a, iaa, jaa, desca, &
 &                           tau, c, icc, jcc, descc, work, lwork, iinfo )
!
      END IF
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCUNMTR
!
end subroutine pcunmtr_kind8
!===============================================================================
!===============================================================================
subroutine pchetd2_kind8( UPLO, N, A, IA, JA, DESCA, D, E, TAU, WORK, &
 &                        LWORK, INFO )
!     ..
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            IA, INFO, JA, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * )
      REAL               D( * ), E( * )
      COMPLEX            A( * ), TAU( * ), WORK( * )
!     ..
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
      COMPLEX            HALF, ONE, ZERO
      parameter( half = ( 0.5e+0, 0.0e+0 ), &
 &                       one = ( 1.0e+0, 0.0e+0 ), &
 &                       zero = ( 0.0e+0, 0.0e+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER
      INTEGER            IACOL, IAROW, ICOFFA, ICTXT, II, IK, IROFFA, J, &
 &                       jj, jk, jn, lda, lwmin, mycol, myrow, npcol, &
 &                       nprow
      COMPLEX            ALPHA, TAUI
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_abort, blacs_gridinfo, caxpy, cgebr2d, &
 &                       cgebs2d, chk1mat, chemv, &
 &                       cher2, clarfg, infog2l, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      COMPLEX            CDOTC
      EXTERNAL           lsame, cdotc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(600+ctxt_)
      ELSE
         upper = lsame( uplo, 'U' )
         CALL chk1mat( n, 2, n, 2, ia, ja, desca, 6, info )
         lwmin = 3 * n
!
         work( 1 ) = cmplx( real( lwmin ) )
         lquery = ( lwork.EQ.-1 )
         IF( info.EQ.0 ) THEN
            iroffa = mod( ia-1, desca( mb_ ) )
            icoffa = mod( ja-1, desca( nb_ ) )
            IF( .NOT.upper .AND. .NOT.lsame( uplo, 'L' ) ) THEN
               info = -1
            ELSE IF( iroffa.NE.icoffa ) THEN
               info = -5
            ELSE IF( desca( mb_ ).NE.desca( nb_ ) ) THEN
               info = -(600+nb_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -11
            END IF
         END IF
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCHETD2', -info )
         CALL blacs_abort( ictxt, 1 )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( n.LE.0 ) RETURN
!
!     Compute local information
!
      lda = desca( lld_ )
      CALL infog2l( ia, ja, desca, nprow, npcol, myrow, mycol, ii, jj, iarow, iacol )
!
      IF( upper ) THEN
!
!        Process(IAROW, IACOL) owns block to be reduced
!
         IF( mycol.EQ.iacol ) THEN
            IF( myrow.EQ.iarow ) THEN
!
!              Reduce the upper triangle of sub( A )
!
               ik = ii+n-1+(jj+n-2)*lda
               a( ik ) = real( a( ik ) )
               DO 10 j = n-1, 1, -1
                  ik = ii + j - 1
                  jk = jj + j - 1
!
!                 Generate elementary reflector H(i) = I - tau * v * v'
!                 to annihilate A(IA:IA+J-1,JA:JA+J-1)
!
                  alpha = a( ik+jk*lda )
                  CALL clarfg( j, alpha, a( ii+jk*lda ), 1, taui )
                  e( jk+1 ) = real( alpha )
!
                  IF( taui.NE.zero ) THEN
!
!                    Apply H(i) from both sides to
!                    A(IA:IA+J-1,JA:JA+J-1)
!
                     a( ik+jk*lda ) = one
!
!                    Compute  x := tau * A * v  storing x in TAU(1:i)
!
                     CALL chemv( uplo, j, taui, a( ii+(jj-1)*lda ), &
 &                               lda, a( ii+jk*lda ), 1, zero, &
 &                               tau( jj ), 1 )
!
!                    Compute  w := x - 1/2 * tau * (x'*v) * v
!
                     alpha = -half*taui*cdotc( j, tau( jj ), 1, &
 &                                             a( ii+jk*lda ), 1 )
                     CALL caxpy( j, alpha, a( ii+jk*lda ), 1, &
 &                               tau( jj ), 1 )
!
!                    Apply the transformation as a rank-2 update:
!                       A := A - v * w' - w * v'
!
                     CALL cher2( uplo, j, -one, a( ii+jk*lda ), 1, &
 &                               tau( jj ), 1, a( ii+(jj-1)*lda ), &
 &                               lda )
                  END IF
!
!                 Copy D, E, TAU to broadcast them columnwise.
!
                  a( ik+jk*lda ) = cmplx( e( jk+1 ) )
                  d( jk+1 ) = real( a( ik+1+jk*lda ) )
                  work( j+1 ) = cmplx( d( jk+1 ) )
                  work( n+j+1 ) = cmplx( e( jk+1 ) )
                  tau( jk+1 ) = taui
                  work( 2*n+j+1 ) = tau( jk+1 )
!
   10          CONTINUE
               d( jj ) = real( a( ii+(jj-1)*lda ) )
               work( 1 ) = cmplx( d( jj ) )
               work( n+1 ) = zero
               work( 2*n+1 ) = zero
!
               CALL cgebs2d( ictxt, 'Columnwise', ' ', 1, 3*n, work, 1 )
!
            ELSE
               CALL cgebr2d( ictxt, 'Columnwise', ' ', 1, 3*n, work, 1, iarow, iacol )
               DO 20 j = 2, n
                  jn = jj + j - 1
                  d( jn ) = real( work( j ) )
                  e( jn ) = real( work( n+j ) )
                  tau( jn ) = work( 2*n+j )
   20          CONTINUE
               d( jj ) = real( work( 1 ) )
            END IF
         END IF
!
      ELSE
!
!        Process (IAROW, IACOL) owns block to be factorized
!
         IF( mycol.EQ.iacol ) THEN
            IF( myrow.EQ.iarow ) THEN
!
!              Reduce the lower triangle of sub( A )
!
               a( ii+(jj-1)*lda ) = real( a( ii+(jj-1)*lda ) )
               DO 30 j = 1, n - 1
                  ik = ii + j - 1
                  jk = jj + j - 1
!
!                 Generate elementary reflector H(i) = I - tau * v * v'
!                 to annihilate A(IA+J-JA+2:IA+N-1,JA+J-1)
!
                  alpha = a( ik+1+(jk-1)*lda )
                  CALL clarfg( n-j, alpha, a( ik+2+(jk-1)*lda ), 1, taui )
                  e( jk ) = real( alpha )
!
                  IF( taui.NE.zero ) THEN
!
!                    Apply H(i) from both sides to
!                    A(IA+J-JA+1:IA+N-1,JA+J+1:JA+N-1)
!
                     a( ik+1+(jk-1)*lda ) = one
!
!                    Compute  x := tau * A * v  storing y in TAU(i:n-1)
!
                     CALL chemv( uplo, n-j, taui, a( ik+1+jk*lda ), &
 &                               lda, a( ik+1+(jk-1)*lda ), 1, &
 &                               zero, tau( jk ), 1 )
!
!                    Compute  w := x - 1/2 * tau * (x'*v) * v
!
                     alpha = -half*taui*cdotc( n-j, tau( jk ), 1, &
 &                            a( ik+1+(jk-1)*lda ), 1 )
                     CALL caxpy( n-j, alpha, a( ik+1+(jk-1)*lda ), &
 &                               1, tau( jk ), 1 )
!
!                    Apply the transformation as a rank-2 update:
!                       A := A - v * w' - w * v'
!
                     CALL cher2( uplo, n-j, -one, &
 &                               a( ik+1+(jk-1)*lda ), 1, &
 &                               tau( jk ), 1, a( ik+1+jk*lda ), &
 &                               lda )
                  END IF
!
!                 Copy D(JK), E(JK), TAU(JK) to broadcast them
!                 columnwise.
!
                  a( ik+1+(jk-1)*lda ) = cmplx( e( jk ) )
                  d( jk ) = real( a( ik+(jk-1)*lda ) )
                  work( j ) = cmplx( d( jk ) )
                  work( n+j ) = cmplx( e( jk ) )
                  tau( jk ) = taui
                  work( 2*n+j ) = tau( jk )
   30          CONTINUE
               jn = jj + n - 1
               d( jn ) = real( a( ii+n-1+(jn-1)*lda ) )
               work( n ) = cmplx( d( jn ) )
               tau( jn ) = zero
               work( 2*n ) = zero
!
               CALL cgebs2d( ictxt, 'Columnwise', ' ', 1, 3*n-1, work, 1 )
!
            ELSE
               CALL cgebr2d( ictxt, 'Columnwise', ' ', 1, 3*n-1, work, &
 &                           1, iarow, iacol )
               DO 40 j = 1, n - 1
                  jn = jj + j - 1
                  d( jn ) = real( work( j ) )
                  e( jn ) = real( work( n+j ) )
                  tau( jn ) = work( 2*n+j )
   40          CONTINUE
               jn = jj + n - 1
               d( jn ) = real( work( n ) )
               tau( jn ) = zero
            END IF
         END IF
      END IF
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCHETD2
!
end subroutine pchetd2_kind8
!===============================================================================
!===============================================================================
subroutine pcunmql_kind8( SIDE, TRANS, M, N, K, A, IA, JA, DESCA, TAU, &
 &                        C, IC, JC, DESCC, WORK, LWORK, INFO )
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            IA, IC, INFO, JA, JC, K, M, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * ), DESCC( * )
      COMPLEX            A( * ), C( * ), TAU( * ), WORK( * )
!     ..
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      CHARACTER          COLBTOP, ROWBTOP
      INTEGER            IAROW, ICCOL, ICOFFC, ICROW, ICTXT, IINFO, IPW, &
 &                       iroffa, iroffc, j, j1, j2, j3, jb, lcm, lcmq, &
 &                       lwmin, mi, mpc0, mycol, myrow, ni, npa0, npcol, &
 &                       nprow, nq, nqc0
!     ..
!     .. Local Arrays ..
      INTEGER            IDUM1( 4 ), IDUM2( 4 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_gridinfo, chk1mat, pchk2mat, pclarfb, &
 &                       pclarft, pcunm2l, pb_topget, pb_topset, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ICEIL, ILCM, INDXG2P, NUMROC
      EXTERNAL           iceil, ilcm, indxg2p, lsame, numroc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, ichar, max, min, mod, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(900+ctxt_)
      ELSE
         left = lsame( side, 'L' )
         notran = lsame( trans, 'N' )
!
!        NQ is the order of Q
!
         IF( left ) THEN
            nq = m
            CALL chk1mat( m, 3, k, 5, ia, ja, desca, 9, info )
         ELSE
            nq = n
            CALL chk1mat( n, 4, k, 5, ia, ja, desca, 9, info )
         END IF
         CALL chk1mat( m, 3, n, 4, ic, jc, descc, 14, info )
         IF( info.EQ.0 ) THEN
            iroffa = mod( ia-1, desca( mb_ ) )
            iroffc = mod( ic-1, descc( mb_ ) )
            icoffc = mod( jc-1, descc( nb_ ) )
            iarow = indxg2p( ia, desca( mb_ ), myrow, desca( rsrc_ ), nprow )
            icrow = indxg2p( ic, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
            iccol = indxg2p( jc, descc( nb_ ), mycol, descc( csrc_ ), npcol )
            mpc0 = numroc( m+iroffc, descc( mb_ ), myrow, icrow, nprow )
            nqc0 = numroc( n+icoffc, descc( nb_ ), mycol, iccol, npcol )
!
            IF( left ) THEN
               lwmin = max( ( desca( nb_ ) * ( desca( nb_ ) - 1 ) ) / 2, &
 &                          ( mpc0 + nqc0 ) * desca( nb_ ) ) + &
 &                     desca( nb_ ) * desca( nb_ )
            ELSE
               npa0 = numroc( n+iroffa, desca( mb_ ), myrow, iarow, &
 &                            nprow )
               lcm = ilcm( nprow, npcol )
               lcmq = lcm / npcol
               lwmin =  max( ( desca( nb_ ) * ( desca( nb_ ) - 1 ) ) &
 &                      / 2, ( nqc0 + max( npa0 + numroc( numroc( &
 &                      n+icoffc, desca( nb_ ), 0, 0, npcol ), &
 &                      desca( nb_ ), 0, 0, lcmq ), mpc0 ) ) * &
 &                      desca( nb_ ) ) + desca( nb_ ) * desca( nb_ )
            END IF
!
            work( 1 ) = cmplx( real( lwmin ) )
            lquery = ( lwork.EQ.-1 )
            IF( .NOT.left .AND. .NOT.lsame( side, 'R' ) ) THEN
               info = -1
            ELSE IF( .NOT.notran .AND. .NOT.lsame( trans, 'C' ) ) THEN
               info = -2
            ELSE IF( k.LT.0 .OR. k.GT.nq ) THEN
               info = -5
            ELSE IF( .NOT.left .AND. desca( mb_ ).NE.descc( nb_ ) ) THEN
               info = -(900+nb_)
            ELSE IF( left .AND. iroffa.NE.iroffc ) THEN
               info = -12
            ELSE IF( left .AND. iarow.NE.icrow ) THEN
               info = -12
            ELSE IF( .NOT.left .AND. iroffa.NE.icoffc ) THEN
               info = -13
            ELSE IF( left .AND. desca( mb_ ).NE.descc( mb_ ) ) THEN
               info = -(1400+mb_)
            ELSE IF( ictxt.NE.descc( ctxt_ ) ) THEN
               info = -(1400+ctxt_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -16
            END IF
         END IF
!
         IF( left ) THEN
            idum1( 1 ) = ichar( 'L' )
         ELSE
            idum1( 1 ) = ichar( 'R' )
         END IF
         idum2( 1 ) = 1
         IF( notran ) THEN
            idum1( 2 ) = ichar( 'N' )
         ELSE
            idum1( 2 ) = ichar( 'C' )
         END IF
         idum2( 2 ) = 2
         idum1( 3 ) = k
         idum2( 3 ) = 5
         IF( lwork.EQ.-1 ) THEN
            idum1( 4 ) = -1
         ELSE
            idum1( 4 ) = 1
         END IF
         idum2( 4 ) = 16
         IF( left ) THEN
            CALL pchk2mat( m, 3, k, 5, ia, ja, desca, 9, m, 3, n, 4, ic, &
 &                         jc, descc, 14, 4, idum1, idum2, info )
         ELSE
            CALL pchk2mat( n, 4, k, 5, ia, ja, desca, 9, m, 3, n, 4, ic, &
 &                         jc, descc, 14, 4, idum1, idum2, info )
         END IF
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCUNMQL', -info )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( m.EQ.0 .OR. n.EQ.0 .OR. k.EQ.0 ) RETURN
!
      CALL pb_topget( ictxt, 'Broadcast', 'Rowwise', rowbtop )
      CALL pb_topget( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
      IF( ( left .AND. notran ) .OR. ( .NOT.left .AND. .NOT.notran ) ) THEN
         j1 = min( iceil( ja, desca( nb_ ) )*desca( nb_ ), ja+k-1 ) + 1
         j2 = ja+k-1
         j3 = desca( nb_ )
      ELSE
         j1 = max( ( (ja+k-2) / desca( nb_ ) ) * desca( nb_ ) + 1, ja )
         j2 = min( iceil( ja, desca( nb_ ) )*desca( nb_ ), ja+k-1 ) + 1
         j3 = -desca( nb_ )
      END IF
!
      IF( left ) THEN
         ni = n
         IF( notran ) THEN
            CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'I-ring' )
         ELSE
            CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'D-ring' )
         END IF
         CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', ' ' )
      ELSE
         mi = m
      END IF
!
!     Use unblocked code for the first block if necessary
!
       IF( ( left .AND. notran ) .OR. ( .NOT.left .AND. .NOT.notran ) ) THEN
         jb = j1 - ja
         IF( left ) THEN
            mi = m - k + jb
         ELSE
            ni = n - k + jb
         END IF
         CALL pcunm2l_kind8( side, trans, mi, ni, jb, a, ia, ja, desca, tau, &
 &                           c, ic, jc, descc, work, lwork, iinfo )
      END IF
!
      ipw = desca( nb_ ) * desca( nb_ ) + 1
      DO 10 j = j1, j2, j3
        jb = min( desca( nb_ ), k-j+ja )
!
!        Form the triangular factor of the block reflector
!        H = H(j+jb-1) . . . H(j+1) H(j)
!
         CALL pclarft( 'Backward', 'Columnwise', nq-k+j+jb-ja, jb, &
 &                    a, ia, j, desca, tau, work, work( ipw ) )
         IF( left ) THEN
!
!           H or H' is applied to C(ic:ic+m-k+j+jb-ja-1,jc:jc+n-1)
!
            mi = m - k + j + jb - ja
         ELSE
!
!           H or H' is applied to C(ic:ic+m-1,jc:jc+n-k+j+jb-ja-1)
!
            ni = n - k + j + jb - ja
         END IF
!
!        Apply H or H'
!
         CALL pclarfb( side, trans, 'Backward', 'Columnwise', mi, ni, &
 &                     jb, a, ia, j, desca, work, c, ic, jc, descc, &
 &                     work( ipw ) )
   10 CONTINUE
!
      IF( ( left .AND. .NOT.notran ) .OR. ( .NOT.left .AND. notran ) ) THEN
         jb = j2 - ja
         IF( left ) THEN
            mi = m - k + jb
         ELSE
            ni = n - k + jb
         END IF
         CALL pcunm2l_kind8( side, trans, mi, ni, jb, a, ia, ja, desca, tau, &
 &                           c, ic, jc, descc, work, lwork, iinfo )
      END IF
!
      CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', rowbtop )
      CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCUNMQL
!
end subroutine pcunmql_kind8
!===============================================================================
!===============================================================================
subroutine pcunm2l_kind8( SIDE, TRANS, M, N, K, A, IA, JA, DESCA, TAU, &
 &                        C, IC, JC, DESCC, WORK, LWORK, INFO )
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            IA, IC, INFO, JA, JC, K, M, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * ), DESCC( * )
      COMPLEX            A( * ), C( * ), TAU( * ), WORK( * )
!     ..
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
      COMPLEX            ONE
      parameter( one  = ( 1.0e+0, 0.0e+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      CHARACTER          COLBTOP, ROWBTOP
      INTEGER            IACOL, IAROW, ICCOL, ICOFFC, ICROW, ICTXT, ICC, &
 &                       ii, iroffa, iroffc, j, j1, j2, j3, jcc, jj, &
 &                       lcm, lcmq, lwmin, mi, mp, mpc0, mycol, myrow, &
 &                       ni, npcol, nprow, nq, nqc0
      COMPLEX            AJJ
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_abort, blacs_gridinfo, cgebr2d, &
 &                       cgebs2d, cgerv2d, cgesd2d, chk1mat, &
 &                       cscal, infog2l, pcelset, pcelset2, &
 &                       pclarf, pclarfc, pb_topget, pb_topset, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILCM, INDXG2P, NUMROC
      EXTERNAL           ilcm, indxg2p, lsame, numroc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, conjg, max, mod, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(900+ctxt_)
      ELSE
         left = lsame( side, 'L' )
         notran = lsame( trans, 'N' )
!
!        NQ is the order of Q
!
         IF( left ) THEN
            nq = m
            CALL chk1mat( m, 3, k, 5, ia, ja, desca, 9, info )
         ELSE
            nq = n
            CALL chk1mat( n, 4, k, 5, ia, ja, desca, 9, info )
         END IF
         CALL chk1mat( m, 3, n, 4, ic, jc, descc, 14, info )
         IF( info.EQ.0 ) THEN
            iroffa = mod( ia-1, desca( mb_ ) )
            iroffc = mod( ic-1, descc( mb_ ) )
            icoffc = mod( jc-1, descc( nb_ ) )
            iarow = indxg2p( ia, desca( mb_ ), myrow, desca( rsrc_ ), nprow )
            icrow = indxg2p( ic, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
            iccol = indxg2p( jc, descc( nb_ ), mycol, descc( csrc_ ), npcol )
            mpc0 = numroc( m+iroffc, descc( mb_ ), myrow, icrow, nprow )
            nqc0 = numroc( n+icoffc, descc( nb_ ), mycol, iccol, npcol )
!
            IF( left ) THEN
               lwmin = mpc0 + max( 1, nqc0 )
            ELSE
               lcm = ilcm( nprow, npcol )
               lcmq = lcm / npcol
               lwmin = nqc0 + max( max( 1, mpc0 ), numroc( numroc( &
 &                     n+icoffc, desca( nb_ ), 0, 0, npcol ), &
 &                     desca( nb_ ), 0, 0, lcmq ) )
            END IF
!
            work( 1 ) = cmplx( real( lwmin ) )
            lquery = ( lwork.EQ.-1 )
            IF( .NOT.left .AND. .NOT.lsame( side, 'R' ) ) THEN
               info = -1
            ELSE IF( .NOT.notran .AND. .NOT.lsame( trans, 'C' ) ) THEN
                info = -2
            ELSE IF( k.LT.0 .OR. k.GT.nq ) THEN
               info = -5
            ELSE IF( .NOT.left .AND. desca( mb_ ).NE.descc( nb_ ) ) THEN
               info = -(900+nb_)
            ELSE IF( left .AND. iroffa.NE.iroffc ) THEN
               info = -12
            ELSE IF( left .AND. iarow.NE.icrow ) THEN
               info = -12
            ELSE IF( .NOT.left .AND. iroffa.NE.icoffc ) THEN
               info = -13
            ELSE IF( left .AND. desca( mb_ ).NE.descc( mb_ ) ) THEN
               info = -(1400+mb_)
            ELSE IF( ictxt.NE.descc( ctxt_ ) ) THEN
               info = -(1400+ctxt_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -16
            END IF
         END IF
      END IF
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCUNM2L', -info )
         CALL blacs_abort( ictxt, 1 )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( m.EQ.0 .OR. n.EQ.0 .OR. k.EQ.0 ) RETURN
!
      IF( desca( m_ ).EQ.1 ) THEN
         CALL infog2l( ia, ja, desca, nprow, npcol, myrow, mycol, ii, &
 &                     jj, iarow, iacol )
         CALL infog2l( ic, jc, descc, nprow, npcol, myrow, mycol, icc, &
 &                     jcc, icrow, iccol )
         IF( left ) THEN
            IF( myrow.EQ.iarow ) THEN
               nq = numroc( jc+n-1, descc( nb_ ), mycol, descc( csrc_ ), npcol )
               IF( mycol.EQ.iacol ) THEN
                  IF( notran ) THEN
                     ajj = one - tau( jj )
                  ELSE
                     ajj = one - conjg( tau( jj ) )
                  END IF
                  CALL cgebs2d( ictxt, 'Rowwise', ' ', 1, 1, ajj, 1 )
                  CALL cscal( nq-jcc+1, ajj, &
 &                            c( icc+(jcc-1)*descc( lld_ ) ), &
 &                            descc( lld_ ) )
               ELSE
                  CALL cgebr2d( ictxt, 'Rowwise', ' ', 1, 1, ajj, 1, iarow, iacol )
                  CALL cscal( nq-jcc+1, ajj, &
 &                            c( icc+(jcc-1)*descc( lld_ ) ), &
 &                            descc( lld_ ) )
               END IF
            END IF
         ELSE
            IF( mycol.EQ.iacol ) THEN
               IF( notran ) THEN
                  ajj = one - tau( jj )
               ELSE
                  ajj = one - conjg( tau( jj ) )
               END IF
            END IF
!
            IF( iacol.NE.iccol ) THEN
               IF( mycol.EQ.iacol ) CALL cgesd2d( ictxt, 1, 1, ajj, 1, myrow, iccol )
               IF( mycol.EQ.iccol ) CALL cgerv2d( ictxt, 1, 1, ajj, 1, myrow, iacol )
            END IF
!
            IF( mycol.EQ.iccol ) THEN
               mp = numroc( ic+m-1, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
               CALL cscal( mp-icc+1, ajj, c( icc+(jcc-1)*descc( lld_ ) ), 1 )
            END IF
!
         END IF
!
      ELSE
!
         CALL pb_topget( ictxt, 'Broadcast', 'Rowwise', rowbtop )
         CALL pb_topget( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
         IF( left .AND. notran .OR. .NOT.left .AND. .NOT.notran ) THEN
            j1 = ja
            j2 = ja+k-1
            j3 = 1
         ELSE
            j1 = ja+k-1
            j2 = ja
            j3 = -1
         END IF
!
         IF( left ) THEN
            ni = n
            IF( notran ) THEN
               CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'I-ring' )
            ELSE
               CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'D-ring' )
            END IF
            CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', ' ' )
         ELSE
            mi = m
         END IF
!
         DO 10 j = j1, j2, j3
!
            IF( left ) THEN
!
!              H(j) or H(j)' is applied to C(ic:ic+m-k+j-ja,jc:jc+n-1)
!
               mi = m - k + j - ja + 1
            ELSE
!
!              H(j) or H(j)' is applied to C(ic:ic+m-1,jc:jc+n-k+j-ja)
!
               ni = n - k + j - ja + 1
            END IF
!
!           Apply H(j) or H(j)'
!
            CALL pcelset2( ajj, a, ia+nq-k+j-ja, j, desca, one )
            IF( notran ) THEN
               CALL pclarf( side, mi, ni, a, ia, j, desca, 1, tau, c, &
 &                          ic, jc, descc, work )
            ELSE
               CALL pclarfc( side, mi, ni, a, ia, j, desca, 1, tau, c, &
 &                           ic, jc, descc, work )
            END IF
            CALL pcelset( a, ia+nq-k+j-ja, j, desca, ajj )
!
   10    CONTINUE
!
         CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', rowbtop )
         CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
      END IF
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCUNM2L
!
end subroutine pcunm2l_kind8
!===============================================================================
!===============================================================================
subroutine pcunmqr_kind8( SIDE, TRANS, M, N, K, A, IA, JA, DESCA, TAU, &
 &                        C, IC, JC, DESCC, WORK, LWORK, INFO )
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            IA, IC, INFO, JA, JC, K, M, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * ), DESCC( * )
      COMPLEX            A( * ), C( * ), TAU( * ), WORK( * )
!     ..
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      CHARACTER          COLBTOP, ROWBTOP
      INTEGER            IAROW, ICC, ICCOL, ICOFFC, ICROW, ICTXT, IINFO, &
 &                       ipw, iroffa, iroffc, j, j1, j2, j3, jb, jcc, &
 &                       lcm, lcmq, lwmin, mi, mpc0, mycol, myrow, ni, &
 &                       npa0, npcol, nprow, nq, nqc0
!     ..
!     .. Local Arrays ..
      INTEGER            IDUM1( 4 ), IDUM2( 4 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_gridinfo, chk1mat, pchk2mat, pclarfb, &
 &                       pclarft, pcunm2r, pb_topget, pb_topset, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ICEIL, ILCM, INDXG2P, NUMROC
      EXTERNAL           iceil, ilcm, indxg2p, lsame, numroc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, ichar, max, min, mod, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(900+ctxt_)
      ELSE
         left = lsame( side, 'L' )
         notran = lsame( trans, 'N' )
!
!        NQ is the order of Q
!
         IF( left ) THEN
            nq = m
            CALL chk1mat( m, 3, k, 5, ia, ja, desca, 9, info )
         ELSE
            nq = n
            CALL chk1mat( n, 4, k, 5, ia, ja, desca, 9, info )
         END IF
         CALL chk1mat( m, 3, n, 4, ic, jc, descc, 14, info )
         IF( info.EQ.0 ) THEN
            iroffa = mod( ia-1, desca( mb_ ) )
            iroffc = mod( ic-1, descc( mb_ ) )
            icoffc = mod( jc-1, descc( nb_ ) )
            iarow = indxg2p( ia, desca( mb_ ), myrow, desca( rsrc_ ), nprow )
            icrow = indxg2p( ic, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
            iccol = indxg2p( jc, descc( nb_ ), mycol, descc( csrc_ ), npcol )
            mpc0 = numroc( m+iroffc, descc( mb_ ), myrow, icrow, nprow )
            nqc0 = numroc( n+icoffc, descc( nb_ ), mycol, iccol, npcol )
!
            IF( left ) THEN
               lwmin = max( ( desca( nb_ ) * ( desca( nb_ ) - 1 ) ) / 2, &
 &                          ( mpc0 + nqc0 ) * desca( nb_ ) ) + &
 &                     desca( nb_ ) * desca( nb_ )
            ELSE
               npa0 = numroc( n+iroffa, desca( mb_ ), myrow, iarow, nprow )
               lcm = ilcm( nprow, npcol )
               lcmq = lcm / npcol
               lwmin =  max( ( desca( nb_ ) * ( desca( nb_ ) - 1 ) ) &
 &                      / 2, ( nqc0 + max( npa0 + numroc( numroc( &
 &                      n+icoffc, desca( nb_ ), 0, 0, npcol ), &
 &                      desca( nb_ ), 0, 0, lcmq ), mpc0 ) ) * &
 &                      desca( nb_ ) ) + desca( nb_ ) * desca( nb_ )
            END IF
!
            work( 1 ) = cmplx( real( lwmin ) )
            lquery = ( lwork.EQ.-1 )
            IF( .NOT.left .AND. .NOT.lsame( side, 'R' ) ) THEN
               info = -1
            ELSE IF( .NOT.notran .AND. .NOT.lsame( trans, 'C' ) ) THEN
               info = -2
            ELSE IF( k.LT.0 .OR. k.GT.nq ) THEN
               info = -5
            ELSE IF( .NOT.left .AND. desca( mb_ ).NE.descc( nb_ ) ) THEN
               info = -(900+nb_)
            ELSE IF( left .AND. iroffa.NE.iroffc ) THEN
               info = -12
            ELSE IF( left .AND. iarow.NE.icrow ) THEN
               info = -12
            ELSE IF( .NOT.left .AND. iroffa.NE.icoffc ) THEN
               info = -13
            ELSE IF( left .AND. desca( mb_ ).NE.descc( mb_ ) ) THEN
               info = -(1400+mb_)
            ELSE IF( ictxt.NE.descc( ctxt_ ) ) THEN
               info = -(1400+ctxt_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -16
            END IF
         END IF
!
         IF( left ) THEN
            idum1( 1 ) = ichar( 'L' )
         ELSE
            idum1( 1 ) = ichar( 'R' )
         END IF
         idum2( 1 ) = 1
         IF( notran ) THEN
            idum1( 2 ) = ichar( 'N' )
         ELSE
            idum1( 2 ) = ichar( 'C' )
         END IF
         idum2( 2 ) = 2
         idum1( 3 ) = k
         idum2( 3 ) = 5
         IF( lwork.EQ.-1 ) THEN
            idum1( 4 ) = -1
         ELSE
            idum1( 4 ) = 1
         END IF
         idum2( 4 ) = 16
         IF( left ) THEN
            CALL pchk2mat( m, 3, k, 5, ia, ja, desca, 9, m, 3, n, 4, ic, &
 &                         jc, descc, 14, 4, idum1, idum2, info )
         ELSE
            CALL pchk2mat( n, 4, k, 5, ia, ja, desca, 9, m, 3, n, 4, ic, &
 &                         jc, descc, 14, 4, idum1, idum2, info )
         END IF
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCUNMQR', -info )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( m.EQ.0 .OR. n.EQ.0 .OR. k.EQ.0 ) RETURN
!
      CALL pb_topget( ictxt, 'Broadcast', 'Rowwise', rowbtop )
      CALL pb_topget( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
      IF( ( left .AND. .NOT.notran ) .OR. ( .NOT.left .AND. notran ) ) THEN
         j1 = min( iceil( ja, desca( nb_ ) ) * desca( nb_ ), ja+k-1 ) + 1
         j2 = ja+k-1
         j3 = desca( nb_ )
      ELSE
         j1 = max( ( (ja+k-2) / desca( nb_ ) ) * desca( nb_ ) + 1, ja )
         j2 = min( iceil( ja, desca( nb_ ) ) * desca( nb_ ), ja+k-1 ) + 1
         j3 = -desca( nb_ )
      END IF
!
      IF( left ) THEN
         ni  = n
         jcc = jc
         IF( notran ) THEN
            CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'D-ring' )
         ELSE
            CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'I-ring' )
         END IF
         CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', ' ' )
      ELSE
         mi  = m
         icc = ic
      END IF
!
!     Use unblocked code for the first block if necessary
!
      IF( ( left .AND. .NOT.notran ) .OR. ( .NOT.left .AND. notran ) ) &
 &       CALL pcunm2r_kind8( side, trans, m, n, j1-ja, a, ia, ja, desca, tau, &
 &                           c, ic, jc, descc, work, lwork, iinfo )
!
      ipw = desca( nb_ ) * desca( nb_ ) + 1
      DO 10 j = j1, j2, j3
         jb = min( desca( nb_ ), k-j+ja )
!
!        Form the triangular factor of the block reflector
!        H = H(j) H(j+1) . . . H(j+jb-1)
!
         CALL pclarft( 'Forward', 'Columnwise', nq-j+ja, jb, a, &
 &                     ia+j-ja, j, desca, tau, work, work( ipw ) )
         IF( left ) THEN
!
!           H or H' is applied to C(ic+j-ja:ic+m-1,jc:jc+n-1)
!
            mi  = m - j + ja
            icc = ic + j - ja
         ELSE
!
!           H or H' is applied to C(ic:ic+m-1,jc+j-ja:jc+n-1)
!
            ni  = n - j + ja
            jcc = jc + j - ja
         END IF
!
!        Apply H or H'
!
         CALL pclarfb( side, trans, 'Forward', 'Columnwise', mi, ni, &
 &                    jb, a, ia+j-ja, j, desca, work, c, icc, jcc, &
 &                    descc, work( ipw ) )
   10 CONTINUE
!
!     Use unblocked code for the last block if necessary
!
      IF( ( left .AND. notran ) .OR. ( .NOT.left .AND. .NOT.notran ) ) &
 &       CALL pcunm2r_kind8( side, trans, m, n, j2-ja, a, ia, ja, desca, tau, &
 &                           c, ic, jc, descc, work, lwork, iinfo )
!
      CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', rowbtop )
      CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCUNMQR
!
end subroutine pcunmqr_kind8
!===============================================================================
!===============================================================================
subroutine pcunm2r_kind8( SIDE, TRANS, M, N, K, A, IA, JA, DESCA, TAU, &
 &                        C, IC, JC, DESCC, WORK, LWORK, INFO )
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            IA, IC, INFO, JA, JC, K, M, N
      INTEGER(8)         LWORK
!     ..
!     .. Array Arguments ..
      INTEGER            DESCA( * ), DESCC( * )
      COMPLEX            A( * ), C( * ), TAU( * ), WORK( * )
!
!     .. Parameters ..
      INTEGER            BLOCK_CYCLIC_2D, CSRC_, CTXT_, DLEN_, DTYPE_, &
 &                       lld_, mb_, m_, nb_, n_, rsrc_
      parameter( block_cyclic_2d = 1, dlen_ = 9, dtype_ = 1, &
 &                         ctxt_ = 2, m_ = 3, n_ = 4, mb_ = 5, nb_ = 6, &
 &                         rsrc_ = 7, csrc_ = 8, lld_ = 9 )
      COMPLEX            ONE
      parameter( one  = ( 1.0e+0, 0.0e+0 ) )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      CHARACTER          COLBTOP, ROWBTOP
      INTEGER            IACOL, IAROW, ICCOL, ICOFFC, ICROW, ICTXT, ICC, &
 &                       ii, iroffa, iroffc, j, j1, j2, j3, jcc, jj, &
 &                       lcm, lcmq, lwmin, mi, mp, mpc0, mycol, myrow, &
 &                       ni, npcol, nprow, nq, nqc0
      COMPLEX            AJJ
!     ..
!     .. External Subroutines ..
      EXTERNAL           blacs_abort, blacs_gridinfo, cgebr2d, &
 &                       cgebs2d, cgerv2d, cgesd2d, chk1mat, &
 &                       cscal, infog2l, pcelset, pcelset2, &
 &                       pclarf, pclarfc, pb_topget, pb_topset, pxerbla
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILCM, INDXG2P, NUMROC
      EXTERNAL           ilcm, indxg2p, lsame, numroc
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          cmplx, conjg, max, mod, real
!     ..
!     .. Executable Statements ..
!
!     Get grid parameters
!
      ictxt = desca( ctxt_ )
      CALL blacs_gridinfo( ictxt, nprow, npcol, myrow, mycol )
!
!     Test the input parameters
!
      info = 0
      IF( nprow.EQ.-1 ) THEN
         info = -(900+ctxt_)
      ELSE
         left = lsame( side, 'L' )
         notran = lsame( trans, 'N' )
!
!        NQ is the order of Q
!
         IF( left ) THEN
            nq = m
            CALL chk1mat( m, 3, k, 5, ia, ja, desca, 9, info )
         ELSE
            nq = n
            CALL chk1mat( n, 4, k, 5, ia, ja, desca, 9, info )
         END IF
         CALL chk1mat( m, 3, n, 4, ic, jc, descc, 14, info )
         IF( info.EQ.0 ) THEN
            iroffa = mod( ia-1, desca( mb_ ) )
            iroffc = mod( ic-1, descc( mb_ ) )
            icoffc = mod( jc-1, descc( nb_ ) )
            iarow = indxg2p( ia, desca( mb_ ), myrow, desca( rsrc_ ), nprow )
            icrow = indxg2p( ic, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
            iccol = indxg2p( jc, descc( nb_ ), mycol, descc( csrc_ ), npcol )
            mpc0 = numroc( m+iroffc, descc( mb_ ), myrow, icrow, nprow )
            nqc0 = numroc( n+icoffc, descc( nb_ ), mycol, iccol, npcol )
!
            IF( left ) THEN
               lwmin = mpc0 + max( 1, nqc0 )
            ELSE
               lcm = ilcm( nprow, npcol )
               lcmq = lcm / npcol
               lwmin = nqc0 + max( max( 1, mpc0 ), numroc( numroc( &
 &                     n+icoffc, desca( nb_ ), 0, 0, npcol ), &
 &                     desca( nb_ ), 0, 0, lcmq ) )
            END IF
!
            work( 1 ) = cmplx( real( lwmin ) )
            lquery = ( lwork.EQ.-1 )
            IF( .NOT.left .AND. .NOT.lsame( side, 'R' ) ) THEN
               info = -1
            ELSE IF( .NOT.notran .AND. .NOT.lsame( trans, 'C' ) ) THEN
               info = -2
            ELSE IF( k.LT.0 .OR. k.GT.nq ) THEN
               info = -5
            ELSE IF( .NOT.left .AND. desca( mb_ ).NE.descc( nb_ ) ) THEN
               info = -(900+nb_)
            ELSE IF( left .AND. iroffa.NE.iroffc ) THEN
               info = -12
            ELSE IF( left .AND. iarow.NE.icrow ) THEN
               info = -12
            ELSE IF( .NOT.left .AND. iroffa.NE.icoffc ) THEN
               info = -13
            ELSE IF( left .AND. desca( mb_ ).NE.descc( mb_ ) ) THEN
               info = -(1400+mb_)
            ELSE IF( ictxt.NE.descc( ctxt_ ) ) THEN
               info = -(1400+ctxt_)
            ELSE IF( lwork.LT.lwmin .AND. .NOT.lquery ) THEN
               info = -16
            END IF
         END IF
      END IF
!
      IF( info.NE.0 ) THEN
         CALL pxerbla( ictxt, 'PCUNM2R', -info )
         CALL blacs_abort( ictxt, 1 )
         RETURN
      ELSE IF( lquery ) THEN
         RETURN
      END IF
!
!     Quick return if possible
!
      IF( m.EQ.0 .OR. n.EQ.0 .OR. k.EQ.0 ) RETURN
!
      IF( desca( m_ ).EQ.1 ) THEN
         CALL infog2l( ia, ja, desca, nprow, npcol, myrow, mycol, ii, &
 &                     jj, iarow, iacol )
         CALL infog2l( ic, jc, descc, nprow, npcol, myrow, mycol, icc, &
 &                     jcc, icrow, iccol )
         IF( left ) THEN
            IF( myrow.EQ.iarow ) THEN
               nq = numroc( jc+n-1, descc( nb_ ), mycol, descc( csrc_ ), npcol )
               IF( mycol.EQ.iacol ) THEN
                  IF( notran ) THEN
                     ajj = one - tau( jj )
                  ELSE
                     ajj = one - conjg( tau( jj ) )
                  END IF
                  CALL cgebs2d( ictxt, 'Rowwise', ' ', 1, 1, ajj, 1 )
                  CALL cscal( nq-jcc+1, ajj, &
 &                            c( icc+(jcc-1)*descc( lld_ ) ), &
 &                            descc( lld_ ) )
               ELSE
                  CALL cgebr2d( ictxt, 'Rowwise', ' ', 1, 1, ajj, 1, iarow, iacol )
                  CALL cscal( nq-jcc+1, ajj, &
 &                            c( icc+(jcc-1)*descc( lld_ ) ), &
 &                            descc( lld_ ) )
               END IF
            END IF
         ELSE
            IF( mycol.EQ.iacol ) THEN
               IF( notran ) THEN
                  ajj = one - tau( jj )
               ELSE
                  ajj = one - conjg( tau( jj ) )
               END IF
            END IF
!
            IF( iacol.NE.iccol ) THEN
               IF( mycol.EQ.iacol ) CALL cgesd2d( ictxt, 1, 1, ajj, 1, myrow, iccol )
               IF( mycol.EQ.iccol ) CALL cgerv2d( ictxt, 1, 1, ajj, 1, myrow, iacol )
            END IF
!
            IF( mycol.EQ.iccol ) THEN
               mp = numroc( ic+m-1, descc( mb_ ), myrow, descc( rsrc_ ), nprow )
               CALL cscal( mp-icc+1, ajj, c( icc+(jcc-1)*descc( lld_ ) ), 1 )
            END IF
!
         END IF
!
      ELSE
!
         CALL pb_topget( ictxt, 'Broadcast', 'Rowwise', rowbtop )
         CALL pb_topget( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
         IF( left .AND. .NOT.notran .OR. .NOT.left .AND. notran ) THEN
            j1 = ja
            j2 = ja+k-1
            j3 = 1
         ELSE
            j1 = ja+k-1
            j2 = ja
            j3 = -1
         END IF
!
         IF( left ) THEN
            ni  = n
            jcc = jc
            IF( notran ) THEN
               CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'D-ring' )
            ELSE
               CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', 'I-ring' )
            END IF
            CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', ' ' )
         ELSE
            mi  = m
            icc = ic
         END IF
!
         DO 10 j = j1, j2, j3
            IF( left ) THEN
!
!              H(j) or H(j)' is applied to C(ic+j-ja:ic+m-1,jc:jc+n-1)
!
               mi  = m - j + ja
               icc = ic + j - ja
            ELSE
!
!              H(j) or H(j)' is applied to C(ic:ic+m-1,jc+j-ja:jc+n-1)
!
               ni  = n - j + ja
               jcc = jc + j - ja
            END IF
!
!           Apply H(j) or H(j)'
!
            CALL pcelset2( ajj, a, ia+j-ja, j, desca, one )
            IF( notran ) THEN
               CALL pclarf( side, mi, ni, a, ia+j-ja, j, desca, 1, tau, &
 &                          c, icc, jcc, descc, work )
            ELSE
               CALL pclarfc( side, mi, ni, a, ia+j-ja, j, desca, 1, tau, &
 &                        c, icc, jcc, descc, work )
            END IF
            CALL pcelset( a, ia+j-ja, j, desca, ajj )
!
   10    CONTINUE
!
         CALL pb_topset( ictxt, 'Broadcast', 'Rowwise', rowbtop )
         CALL pb_topset( ictxt, 'Broadcast', 'Columnwise', colbtop )
!
      END IF
!
      work( 1 ) = cmplx( real( lwmin ) )
!
      RETURN
!
!     End of PCUNM2R
!
end subroutine pcunm2r_kind8
!*****************************************************************
