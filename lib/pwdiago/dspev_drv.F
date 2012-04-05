!
! Copyright (C) 2001-2008 Quantum-ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
#include "f_defs.h"
!

MODULE dspev_module

    USE mp_global,   ONLY:local_type,local_type_complex
    IMPLICIT NONE
    SAVE
    PRIVATE

    PUBLIC :: pdspev_drv, dspev_drv

CONTAINS


    SUBROUTINE ptredv( a, lda, d, e, v, ldv, nrl, n, nproc, me, comm )

!
!     Parallel version of the famous HOUSEHOLDER tridiagonalization
!     Algorithm for simmetric matrix.
!
!     AUTHOR : Carlo Cavazzoni - SISSA 1997
!              comments and suggestions to : carlo.cavazzoni@cineca.it
!
! REFERENCES :
!
! NUMERICAL RECIPES, THE ART OF SCIENTIFIC COMPUTING.
! W.H. PRESS, B.P. FLANNERY, S.A. TEUKOLSKY, AND W.T. VETTERLING,
! CAMBRIDGE UNIVERSITY PRESS, CAMBRIDGE.  
!
! PARALLEL NUMERICAL ALGORITHMS,
! T.L. FREEMAN AND C.PHILLIPS,
! PRENTICE HALL INTERNATIONAL (1992). 
!
!
!
!     INPUTS :
!
!     A(NRL,N) Local part of the global matrix A(N,N) to be reduced,
!              only the upper triangle is needed.
!              The rows of the matrix are distributed among processors
!              with blocking factor 1. 
!              Example for NPROC = 4 :
!              ROW | PE
!              1   | 0
!              2   | 1
!              3   | 2
!              4   | 3
!              5   | 0
!              6   | 1
!              ..  | ..
!
!     LDA      LEADING DIMENSION OF MATRIX A.
!
!     LDV      LEADING DIMENSION OF MATRIX V.
!
!     NRL      NUMBER OF ROWS BELONGING TO THE LOCAL PROCESSOR.
!
!     N        DIMENSION OF THE GLOBAL MATRIX.
!
!     NPROC    NUMBER OF PROCESSORS.
!
!     ME       INDEX OF THE LOCAL PROCESSOR (Starting from 0).
!
!
!     OUTPUTS :
!
!     V(NRL,N) Orthogonal transformation that tridiagonalize A,
!              this matrix is distributed among processor
!              in the same way as A.
!
!     D(N)     Diagonal elements of the tridiagonal matrix
!              this vector is equal on all processors.
!
!     E(N)     Subdiagonal elements of the tridiagonal matrix
!              this vector is equal on all processors. 
!
!
      USE kinds,     ONLY : SP,DP

      IMPLICIT NONE
#if defined _MPI
      include 'mpif.h'
#endif

      INTEGER, intent(in) :: N, NRL, LDA, LDV
      INTEGER, intent(in) :: NPROC, ME, comm
      REAL(SP) :: A(LDA,N), D(N), E(N), V(LDV,N)
!
      REAL(SP), external ::DDOT
!
      REAL(SP) :: g, scalef, sigma, kappa, f, h, tmp
      REAL(SP), ALLOCATABLE :: u(:)
      REAL(SP), ALLOCATABLE :: p(:)
      REAL(SP), ALLOCATABLE :: vtmp(:)

      REAL(SP) :: tu, tp, one_over_h
      REAL(SP) :: one_over_scale
      REAL(SP) :: redin(3), redout(3)
      REAL(SP), ALLOCATABLE :: ul(:)
      REAL(SP), ALLOCATABLE :: pl(:)
      integer :: l, i, j, k, t, tl, ierr
      integer :: kl, jl, ks, lloc
      integer, ALLOCATABLE :: is(:)
      integer, ALLOCATABLE :: ri(:)
     
      !     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........

      IF( N == 0 ) THEN
        RETURN
      END IF

      ALLOCATE( u( n+2 ), p( n+1 ), vtmp( n+2 ), ul( n ), pl( n ), is( n ), ri( n ) )

      DO I = N, 1, -1
        IS(I)  = (I-1)/NPROC
        RI(I)  = MOD((I-1),NPROC)  !  owner of I-th row
        IF(ME .le. RI(I) ) then
          IS(I) = IS(I) + 1
        END IF
!        WRITE( stdout,100) 'ME,I,RI,IS',ME,I,RI(I),IS(I)
100   FORMAT(A,2X,5I4)
      END DO

      DO I = N, 2, -1 

         L     = I - 1         ! first element
         H     = 0.0_SP

         IF ( L > 1 ) THEN

           SCALEF = 0.0_SP
           DO K = 1, is(l)
#if defined _DOUBLE
              SCALEF = SCALEF + DABS( A(K,I) )
#else
              SCALEF = SCALEF + ABS( A(K,I) )
#endif
           END DO

#if defined _MPI
           redin(1) = scalef
           CALL MPI_ALLREDUCE(redin, redout, 1, local_type, MPI_SUM, comm, IERR)
           SCALEF = redout(1)
#endif

           IF ( SCALEF .EQ. 0.0_SP )  THEN
             !
             IF (RI(L).EQ.ME) THEN
               E(I) = A(is(L),I) 
             END IF
             !
           ELSE 

             !  ......  CALCULATION OF SIGMA AND H

             ONE_OVER_SCALE = 1.0_SP/SCALEF
             SIGMA = 0.0_SP
             DO k = 1,is(L)
               A(k,I) = A(k,I) * ONE_OVER_SCALE
               SIGMA  = SIGMA + A(k,I)**2 
             END DO

             IF( ri(l) .eq. me ) THEN
               F = A( is(l), i )
             ELSE
               F = 0.0_SP
             END IF

             !  CONSTRUCTION OF VECTOR U

             vtmp( 1:l ) = 0.0_SP 

             k = ME + 1
             DO kl = 1,is(l)          
               vtmp(k)   = A(kl,I)
               k         = k + NPROC
             END DO

             DO kl = 1,is(l)          
               UL(kl)    = A(kl,I)
             END DO

#if defined _MPI
             vtmp( l + 1 ) = sigma
             vtmp( l + 2 ) = f
             CALL MPI_ALLREDUCE( VTMP, U, L+2, local_type, MPI_SUM, comm, IERR )
             sigma = u( l + 1 )
             f     = u( l + 2 )
#else
             u(1:l) = vtmp(1:l)
#endif

             G          = -SIGN(SQRT(SIGMA),F)
             H          = SIGMA - F*G
             ONE_OVER_H = 1.0_SP/H
             E(I)       = SCALEF*G

             U(L)       = F - G 

             IF( RI(L) ==  ME ) THEN
               UL(is(l))  = F - G
               A(is(l),I) = F - G 
             END IF

             !  CONSTRUCTION OF VECTOR P

             DO J = 1,L

               vtmp(j) = 0.0_SP

               DO KL = 1, IS(J)
                 vtmp(J) = vtmp(J) + A(KL,J) * UL(KL)
               END DO

               IF( L > J .AND. ME == RI(J) ) then
                 DO K = J+1,L
                   vtmp(J) = vtmp(J) + A(IS(J),K) * U(K)
                 END DO
               END IF

               vtmp(J) = vtmp(J) * ONE_OVER_H

             END DO 

             KAPPA = 0.5_SP * ONE_OVER_H * DDOT( l, vtmp, 1, u, 1 )

#if defined _MPI
             vtmp( l + 1 ) = kappa
             CALL MPI_ALLREDUCE( vtmp, p, l+1, local_type, MPI_SUM, comm, IERR )
             kappa = p( l + 1 )
#else
             p(1:l) = vtmp(1:l)
#endif

             CALL DAXPY( l, -kappa, u, 1, p, 1 )
             CALL DGER( is(l), l, -1.0_SP, ul, 1, p, 1, a, lda )
             CALL DGER( is(l), l, -1.0_SP, p( me + 1 ), nproc, u, 1, a, lda )

           END IF

         ELSE 

           IF(RI(L).EQ.ME) THEN
             G = A(is(l),I)
           END IF

#if defined _MPI
           CALL MPI_BCAST(G,1,local_type,RI(L),comm,IERR)
#endif
           E(I) = G

         END IF

         D(I) = H

      END DO
      
      E(1) = 0.0_SP
      D(1) = 0.0_SP

      DO J = 1,N
        V(1:nrl,J) = 0.0_SP
        IF(RI(J).EQ.ME) THEN
          V(IS(J),J) = 1.0_SP
        END IF
      END DO

      DO I = 2,N
        L = I - 1
        LLOC = IS(L)
        !
        IF( D(I) .NE. 0.0_SP ) THEN
          !
          ONE_OVER_H = 1.0_SP/D(I)
          !
          IF( lloc > 0 ) THEN
             CALL DGEMV( 't', lloc, l, 1.0_SP, v(1,1), ldv, a(1,i), 1, 0.0_SP, p(1), 1 )
          ELSE
             P(1:l) = 0.0_SP
          END IF
           

#if defined _MPI
          CALL MPI_ALLREDUCE(P,VTMP,L,local_type,MPI_SUM,comm,IERR)
#else
          vtmp(1:l) = p(1:l)
#endif

          IF( lloc > 0 ) THEN
             CALL DGER( lloc, l, -ONE_OVER_H, a(1,i), 1, vtmp, 1, v, ldv )
          END IF

        END IF

      END DO 


      DO I = 1,N
        U(I) = 0.0_SP
        IF(RI(I).eq.ME) then
          U(I) = A(IS(I),I) 
        END IF
      END DO

#if defined _MPI
      CALL MPI_ALLREDUCE(U,D,N,local_type,MPI_SUM,comm,IERR)
#else
      D(1:N) = U(1:N)
#endif

      DEALLOCATE( u, p, vtmp, ul, pl, is, ri )

    RETURN
    END SUBROUTINE ptredv

!==----------------------------------------------==!

    SUBROUTINE ptqliv( d, e, n, z, ldz, nrl, mpime, comm )

!
! Modified QL algorithm for CRAY T3E PARALLEL MACHINE
! calculate the eigenvectors and eigenvalues of a matrix reduced to 
! tridiagonal form by PTREDV. 
!
! AUTHOR : Carlo Cavazzoni - SISSA 1997
!          comments and suggestions to : carlo.cavazzoni@cineca.it
!
! REFERENCES :
!
! NUMERICAL RECIPES, THE ART OF SCIENTIFIC COMPUTING.
! W.H. PRESS, B.P. FLANNERY, S.A. TEUKOLSKY, AND W.T. VETTERLING,
! CAMBRIDGE UNIVERSITY PRESS, CAMBRIDGE.  
!
! PARALLEL NUMERICAL ALGORITHMS,
! T.L. FREEMAN AND C.PHILLIPS,
! PRENTICE HALL INTERNATIONAL (1992). 
!
! NOTE : the algorithm that finds the eigenvalues is not parallelized
!        ( it scales as O(N^2) ), I preferred to parallelize only the
!        updating of the eigenvectors because it is the most costly
!        part of the algorithm ( it scales as O(N^3) ).
!        For large matrix in practice all the time is spent in the updating
!        that in this routine scales linearly with the number of processors,
!        in fact there is no communication at all.
!
!  
!     INPUTS :
!
!     D(N)     Diagonal elements of the tridiagonal matrix
!              this vector is equal on all processors.
!
!     E(N)     Subdiagonal elements of the tridiagonal matrix
!              this vector is equal on all processors.
!
!     N        DIMENSION OF THE GLOBAL MATRIX.
!
!     NRL      NUMBER OF ROWS OF Z BELONGING TO THE LOCAL PROCESSOR.
!
!     LDZ      LEADING DIMENSION OF MATRIX Z. 
!
!     Z(LDZ,N) Orthogonal transformation that tridiagonalizes the original
!              matrix A.
!              The rows of the matrix are distributed among processors
!              with blocking factor 1.
!              Example for NPROC = 4 :
!              ROW | PE
!              1   | 0
!              2   | 1
!              3   | 2
!              4   | 3
!              5   | 0
!              6   | 1
!              ..  | ..
!
!
!
!     OUTPUTS :
!
!     Z(LDZ,N) EIGENVECTORS OF THE ORIGINAL MATRIX.
!              THE Jth COLUMN of Z contains the eigenvectors associated
!              with the jth eigenvalue.
!              The eigenvectors are scattered among processors (4PE examp. )
!              eigenvector | PE
!               elements   |
!                 V(1)     | 0
!                 V(2)     | 1
!                 V(3)     | 2
!                 V(4)     | 3
!                 V(5)     | 0
!                 V(6)     | 1
!                 ....       ..
! 
!     D(N)     Eigenvalues of the original matrix, 
!              this vector is equal on all processors.
!
!
!
!
      USE kinds,     ONLY : DP,SP

      IMPLICIT NONE
#if defined _MPI
      include 'mpif.h'
#endif

      INTEGER, INTENT(IN)  :: n, nrl, ldz, mpime, comm
      REAL(SP) :: d(n), e(n)
      REAL(SP) :: z(ldz,n)

      INTEGER  :: i, iter, mk, k, l, m, ierr
      REAL(SP) :: b, dd, f, g, p, r, c, s
      REAL(SP), ALLOCATABLE :: cv(:,:)
      REAL(SP), ALLOCATABLE :: fv1(:)
      REAL(SP), ALLOCATABLE :: fv2(:)

      ALLOCATE( cv( 2,n ) )
      ALLOCATE( fv1( nrl ) )
      ALLOCATE( fv2( nrl ) )

      do l = 2,n
        e(l-1) = e(l)
      end do

      do l=1,n
        iter=0
1       do m=l,n-1
          dd = abs(d(m))+abs(d(m+1))
          if ( abs(e(m))+dd .eq. dd ) goto 2
        end do
        m=n

2       if ( m /= l ) then
          if ( iter == 200 ) then
             call errore(' tqli ',' too many iterations ', iter)
          end if
          iter=iter+1
          !
          ! iteration is performed on one processor and results broadcast 
          ! to all others to prevent potential problems if all processors
          ! do not behave in exactly the same way (even with the same data!)
          !
          if ( mpime == 0 ) then
             g=(d(l+1)-d(l))/(2.0_SP*e(l))
             r=pythag(g,1.0_SP)
             g=d(m)-d(l)+e(l)/(g+sign(r,g))
             s=1.0_SP
             c=1.0_SP
             p=0.0_SP
             do i=m-1,l,-1
                f=s*e(i)
                b=c*e(i)
                r=pythag(f,g)
                e(i+1)=r
                if ( r == 0.0_SP) then
                   d(i+1)=d(i+1)-p
                   e(m)=0.0_SP
                   goto 1
                endif
                c=g/r
                g=d(i+1)-p
                s=f/r
                r=(d(i)-g)*s+2.0_SP*c*b
                p=s*r
                d(i+1)=g+p
                g=c*r-b
                !
                cv(1,i-l+1) = c
                cv(2,i-l+1) = s
                !cv(1,i) = c
                !cv(2,i) = s
             end do
             !
             d(l)=d(l)-p
             e(l)=g
             e(m)=0.0_SP
           end if
#if defined _MPI
           CALL MPI_BCAST(cv,2*(m-l),local_type,0,comm,IERR)
           !CALL MPI_BCAST(cv,2*n,local_type,0,comm,IERR)
           CALL MPI_BCAST(e,n,local_type,0,comm,IERR)
           CALL MPI_BCAST(d,n,local_type,0,comm,IERR)
#endif

          do i=m-1,l,-1
            do k=1,nrl
              fv2(k)  =z(k,i+1)
            end do
            do k=1,nrl
              fv1(k)  =z(k,i)
            end do
            do k=1,nrl
              z(k,i+1)  =cv(2,i-l+1)*fv1(k) + cv(1,i-l+1)*fv2(k)
              z(k,i)    =cv(1,i-l+1)*fv1(k) - cv(2,i-l+1)*fv2(k)
              !z(k,i+1)  =cv(2,i)*fv1(k) + cv(1,i)*fv2(k)
              !z(k,i)    =cv(1,i)*fv1(k) - cv(2,i)*fv2(k)
            end do
          end do

          goto 1

        endif
      end do

      DEALLOCATE( cv )
      DEALLOCATE( fv1 )
      DEALLOCATE( fv2 )

      RETURN
      END SUBROUTINE ptqliv

!==----------------------------------------------==!


      SUBROUTINE peigsrtv(d,v,ldv,n,nrl)

      USE kinds,     ONLY : SP,DP
!
!     This routine sorts eigenvalues and eigenvectors 
!     generated by PTREDV and PTQLIV.  
!
!     AUTHOR : Carlo Cavazzoni - SISSA 1997
!              comments and suggestions to : carlo.cavazzoni@cineca.it
!

      IMPLICIT NONE
      INTEGER, INTENT (IN) :: n,ldv,nrl
      REAL(SP), INTENT(INOUT) :: d(n),v(ldv,n)

      INTEGER :: i,j,k
      REAL(SP):: p

      do 13 i=1,n-1
        k=i
        p=d(i)
        do j=i+1,n
          if(d(j).le.p)then
            k=j
            p=d(j)
          endif
        end do
        if(k.ne.i)then
          d(k)=d(i)
          d(i)=p
!
!         Exchange local elements of eigenvectors.
!
          do j=1,nrl
            p=v(j,i)
            v(j,i)=v(j,k)
            v(j,k)=p
          END DO

        endif
13    continue
      return
      END SUBROUTINE peigsrtv

   !
   !-------------------------------------------------------------------------
   FUNCTION pythag(a,b)
      USE kinds,     ONLY : SP,DP
      IMPLICIT NONE
      REAL(SP) :: a, b, pythag
      REAL(SP) :: absa, absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*sqrt(1.0_SP+(absb/absa)**2)
      else
        if(absb.eq.0.0_SP)then
          pythag=0.0_SP
        else
          pythag=absb*sqrt(1.0_SP+(absa/absb)**2)
        endif
      endif
      return
   END FUNCTION pythag
   !
!==----------------------------------------------==!

   SUBROUTINE pdspev_drv( jobz, ap, lda, w, z, ldz, &
                          nrl, n, nproc, mpime, comm_in )
     USE kinds,     ONLY : DP,SP
     IMPLICIT NONE
#if defined _MPI
      include 'mpif.h'
#endif
     CHARACTER :: JOBZ
     INTEGER   :: lda, ldz, nrl, n, nproc, mpime
     INTEGER, OPTIONAL, INTENT(IN) :: comm_in
     REAL(SP) :: ap( lda, * ), w( * ), z( ldz, * )
     REAL(SP), ALLOCATABLE :: sd( : )
     INTEGER   :: comm

     !
#if defined _MPI
     IF ( PRESENT( comm_in ) ) THEN
        comm = comm_in
     ELSE
        comm = MPI_COMM_WORLD
     END IF
#endif
     ALLOCATE ( sd (n ) )
     CALL ptredv(ap, lda, w, sd, z, ldz, nrl, n, nproc, mpime, comm)
     CALL ptqliv(w, sd, n, z, ldz, nrl, mpime, comm)
     DEALLOCATE ( sd )
     CALL peigsrtv(w, z, ldz, n, nrl)
     RETURN
   END SUBROUTINE pdspev_drv
 
!==----------------------------------------------==!

      SUBROUTINE dspev_drv( JOBZ, UPLO, N, AP, W, Z, LDZ )
        USE kinds,     ONLY : SP,DP
        IMPLICIT NONE
#if defined _MPI
      include 'mpif.h'
#endif
        CHARACTER ::       JOBZ, UPLO
        INTEGER   ::       IOPT, INFO, LDZ, N
        REAL(SP) ::  AP( * ), W( * ), Z( LDZ, * )
        REAL(SP), ALLOCATABLE :: WORK(:)

        ALLOCATE( work( 3*n ) )

#if defined __ESSL
          IOPT = 0
          IF((JOBZ .EQ. 'V') .OR. (JOBZ .EQ. 'v') ) iopt = iopt + 1
          IF((UPLO .EQ. 'U') .OR. (UPLO .EQ. 'u') ) iopt = iopt + 20
          CALL DSPEV(IOPT, ap, w, z, ldz, n, work, 3*n)
#else 
          CALL DSPEV(jobz, uplo, n, ap(1), w(1), z(1,1), ldz, work, INFO)
          IF( info .NE. 0 ) THEN
            CALL errore( ' dspev_drv ', ' diagonalization failed ',info )
          END IF
#endif

        DEALLOCATE( work )
 
        RETURN
      END SUBROUTINE dspev_drv


END MODULE dspev_module
