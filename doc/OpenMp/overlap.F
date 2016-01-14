!
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!************************************************************
   SUBROUTINE overlap( ik1, ik2, dimw1, dimw2, imin1, imin2, dimwinx, &
                       evc, evc_info, igsort, lnncell, Mkb )
   !************************************************************
   !
   USE kinds
   USE constants,      ONLY : CZERO
   USE timing_module,  ONLY : timing
   USE log_module,     ONLY : log_push, log_pop
   USE ggrids_module,  ONLY : nfft, gamma_only
   USE wfc_info_module 
   !
   IMPLICIT NONE
      !
      ! ... Input Variables
      !
      TYPE(wfc_info), INTENT(in) :: evc_info
      COMPLEX(dbl),   INTENT(in) :: evc( evc_info%npwx, *)

      INTEGER,        INTENT(in) :: ik1, ik2
      INTEGER,        INTENT(in) :: dimw1, dimw2, dimwinx
      INTEGER,        INTENT(in) :: imin1, imin2
      INTEGER,        INTENT(in) :: igsort( evc_info%npwx , *)
      INTEGER,        INTENT(in) :: lnncell( 3 )
      COMPLEX(dbl),   INTENT(out):: Mkb( dimwinx, dimwinx )

      !
      ! ... Local Variables
      !
      INTEGER :: ierr
      INTEGER :: ig
      INTEGER :: npwkx, npwx_g, itmp(3)
      INTEGER :: j1, npwk1, ind1
      INTEGER :: j2, npwk2, ind2
#ifdef __WORKAROUND_ZDOTC
      COMPLEX(dbl) :: ctmp
#endif
      !
      INTEGER,      ALLOCATABLE :: map(:),  map_aux(:)
      COMPLEX(dbl), ALLOCATABLE :: aux1(:), aux2(:)
      !
      COMPLEX(dbl),    EXTERNAL :: ZDOTC
      !
      CHARACTER(7)              :: subname='overlap'
      !
      ! ... end declarations
      !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing(subname,OPR='start')
      CALL log_push(subname)

      !
      ! the maximun number of PW for wfcs
      npwkx = evc_info%npwx
      !
      ! is the total number of G in k-indipendent wfc representation
      ! among ik1 and ik2
      ! the "+1" at the end of the line is used to have one void position
      !
      npwx_g = MAX( MAXVAL(igsort(:,ik1)), MAXVAL(igsort(:,ik2)) ) +1

      IF ( dimw1 > dimwinx ) CALL errore(subname, 'Invalid dimw1', dimw1)
      IF ( dimw2 > dimwinx ) CALL errore(subname, 'Invalid dimw2', dimw2)


      !
      ! local workspace
      !
      ALLOCATE( map(npwkx), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating map',npwkx)
      !
      IF ( gamma_only ) THEN
          ALLOCATE( map_aux(npwkx), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating map_aux',npwkx)
      ENDIF
      !
      ALLOCATE( aux2(npwx_g), STAT=ierr )
      IF (ierr/=0) CALL errore(subname,'allocating aux2',npwx_g)

      !
      ! Calculate cm(i,j)=<u_i k|u_j k+dk> (keeping into account
      ! that if k+dk is outside (or should be outside) the first BZ it must be
      ! brought from there (or there)
      ! with a exp(-iG.r) factor (given the proper convention for the sign):
      ! psi_nk=psi_nk+G -> u_nk exp(ikr)=u_nk+G exp(ikr) exp (iGr) ->
      !                    u_nk+G = u_nk exp(-iGr)
      ! 
      ! Additionally, we might have a exp (iG_0r) that has to be introduced,
      ! if we need a ik2 that lies outside the BZ. In our reciprocal space
      ! products, that means that we have <v_m,k1|v_n,k2>=\sum_G1,G2 c_m,k1,G1
      ! c_n,k2,G2* \int exp [i(G2-G1-G0)r], if G0 is the vector that, say,
      ! brings a k2 inside the BZ just outside it, to be a neighbour of a
      ! k1 that lies just inside the boundary (and the u are now in standard
      ! Bloch notation). The integral gives a delta, and so we take G2s that
      ! are G1s+G0, i.e. nx+nncell, etc...
      !

      !
      ! Here imin* take into account the fact that dimwin 
      ! may not start from the first band
      !
      ind1  = wfc_info_getindex(imin1, ik1, "IK", evc_info)
      ind2  = wfc_info_getindex(imin2, ik2, "IKB", evc_info)
      npwk1 = evc_info%npw( ind1 )
      npwk2 = evc_info%npw( ind2 )

      !
      ! creates the map from the PW of ik2 to those of ik1 
      ! the PWs not found are set to the index npwx_g which is not summed
      !
      ! this mapping takes into account the e^{iGr} factor when k1 and k2 are 
      ! in different Brillouin zones.
      !
      itmp(:) = lnncell
      IF ( gamma_only ) itmp(:) = -itmp(:)
      !
      CALL overlap_setmap( npwk2, npwx_g, nfft(1), nfft(2), nfft(3), igsort(:,ik2), &
                           itmp, 1, map)
      map( npwk2+1: npwkx ) = 0
      !
      !
      IF ( gamma_only ) THEN
          !
          CALL overlap_setmap( npwk2, npwx_g, nfft(1), nfft(2), nfft(3), igsort(:,ik2), &
                               itmp, -1, map_aux)
          map_aux( npwk2+1: npwkx ) = 0
          !
      ENDIF

      !
      ! checks about the maps
      !
      IF ( gamma_only ) THEN
          !
!$omp parallel do
          DO ig = 1, npwk2
             IF ( map(ig) < 0 ) map(ig)     = -npwx_g
             IF ( map(ig) < 0 .AND. map_aux(ig) <= 0 ) CALL errore(subname,'mismatch in maps',10)
             IF ( map(ig) == 0 )                       CALL errore(subname,'map == 0',10)
          ENDDO
!$omp end parallel do
          !
      ELSE
          !
!$omp parallel do
          DO ig = 1, npwk2
              IF ( map(ig) <= 0 ) CALL errore(subname,'invalid index in map', ig)
          ENDDO
!$omp end parallel do
          !
      ENDIF


      !
      ! loops over bands
      !
      DO j2 = 1, dimw2
          !
          aux2(:) = CZERO
          ind2 = wfc_info_getindex(imin2 +j2 -1, ik2, "IKB", evc_info)
          !
          IF ( gamma_only ) THEN
              !
!$omp parallel do
              DO ig = 1, npwk2
                  !
                  IF ( map(ig) < 0 )  THEN 
                      aux2( igsort(ig,ik1) ) = CONJG( evc( map_aux(ig), ind2) )
                  ELSE IF ( map(ig) < npwx_g ) THEN
                      aux2( igsort(ig,ik1) ) = evc( map(ig), ind2)
                  ENDIF
                  !
              ENDDO
!$omp end parallel do
              !
          ELSE
              !
              ! normal wfc representation
              !
!$omp parallel do
              DO ig=1, npwk2
                  aux2( map( ig ) ) = evc( ig, ind2) 
              ENDDO
!$omp end parallel do
              !
          ENDIF


!$omp parallel private(aux1)
          !
!$omp critical
          ALLOCATE( aux1(npwx_g), STAT=ierr )
          IF (ierr/=0) CALL errore(subname,'allocating aux1',npwx_g)
!$omp end critical
          !
!$omp do private(ind1,ig)
          DO j1 = 1, dimw1
              !
              aux1(:) = CZERO
              ind1 = wfc_info_getindex(imin1 +j1 -1, ik1, "IK", evc_info)
              !
              DO ig=1, npwk1
                 aux1( igsort( ig, ik1 ) ) = evc( ig, ind1) 
              ENDDO

              !
              ! last position for ig is dummy
              !
#ifdef __WORKAROUND_ZDOTC
              ctmp = 0.0d0
              !
              DO ig = 1, npwx_g-1
                  ctmp = ctmp + CONJG(aux1(ig)) * aux2(ig) 
              ENDDO
              !
              Mkb( j1, j2) = ctmp
#else
              Mkb( j1, j2) = ZDOTC( npwx_g -1, aux1, 1, aux2, 1) 
#endif
              !
          ENDDO
!$omp end do
          !
!$omp critical
          DEALLOCATE( aux1, STAT=ierr)
          IF (ierr/=0) CALL errore(subname,'deallocating aux1',ABS(ierr))
!$omp end critical
!
!$omp end parallel
          !
      ENDDO


      !
      ! add the missing term (G<0) in the gamma only case
      !
      IF ( gamma_only ) THEN
          !
          itmp(:) =  lnncell(:)
          !
          CALL overlap_setmap( npwk2, npwx_g, nfft(1), nfft(2), nfft(3), igsort(:,ik2), &
                               itmp, 1, map)
          map( npwk2+1: npwkx ) = 0
          !
          CALL overlap_setmap( npwk2, npwx_g, nfft(1), nfft(2), nfft(3), igsort(:,ik2), &
                               itmp, -1, map_aux)
          map_aux( npwk2+1: npwkx ) = 0
          !
!$omp parallel do
          DO ig = 1, npwk2
             IF ( map(ig) < 0 ) map(ig)     = -npwx_g
             IF ( map(ig) < 0 .AND. map_aux(ig) <= 0) CALL errore(subname,'mismatch in maps II',10)
             IF ( map(ig) == 0 )                      CALL errore(subname,'map == 0 II',10)
          ENDDO
!$omp end parallel do
      

          DO j2 = 1, dimw2
              !
              aux2(:) = CZERO
              ind2 = wfc_info_getindex(imin2 +j2 -1, ik2, "IKB", evc_info)
              !
              !
!$omp parallel do
              DO ig = 1, npwk2
                  !
                  IF ( map(ig) < 0 )  THEN 
                      aux2( igsort(ig,ik1) ) = CONJG( evc( map_aux(ig), ind2) )
                  ELSE IF ( map(ig) < npwx_g ) THEN
                      aux2( igsort(ig,ik1) ) = evc( map(ig), ind2)
                  ENDIF
                  !
              ENDDO
!$omp end parallel do


!$omp parallel private(aux1)
              !
!$omp critical
              ALLOCATE( aux1(npwx_g), STAT=ierr )
              IF (ierr/=0) CALL errore(subname,'allocating aux1',npwx_g)
!$omp end critical
              !
!$omp do private(ind1,ig)
              DO j1 = 1, dimw1
                  !
                  aux1(:) = CZERO
                  ind1 = wfc_info_getindex(imin1 +j1 -1, ik1, "IK", evc_info)
                  !
                  DO ig = 1, npwk1
                     aux1( igsort( ig, ik1 ) ) = evc( ig, ind1)
                  ENDDO
    
                  !
                  ! first and last positions for ig are dummy
                  ! The first comes as well because G < 0 (instead of being G<=0 )
                  !
#ifdef __WORKAROUND_ZDOTC
                  Mkb( j1, j2) = Mkb( j1, j2) + CONJG ( DOT_PRODUCT(aux1(2:npwx_g-1), aux2(2:npwx_g-1) ) )
#else
                  Mkb( j1, j2) = Mkb( j1, j2) + CONJG ( ZDOTC ( npwx_g -2, aux1(2), 1, aux2(2), 1) )
#endif
                  !
              ENDDO
!$omp end do
              !
!$omp critical
              DEALLOCATE( aux1, STAT=ierr)
              IF (ierr/=0) CALL errore(subname,'deallocating aux1',ABS(ierr))
!$omp end critical
              !
!$omp end parallel
              !
          ENDDO
          !  
      ENDIF


      !
      ! local cleanup
      !
      DEALLOCATE( aux2, STAT=ierr)
      IF (ierr/=0) CALL errore(subname,'deallocating aux2',ABS(ierr))
      !
      DEALLOCATE( map, STAT=ierr)
      IF (ierr/=0) CALL errore(subname,'deallocating map',ABS(ierr))
      !
      IF ( gamma_only ) THEN
          DEALLOCATE( map_aux, STAT=ierr)
          IF (ierr/=0) CALL errore(subname,'deallocating map_aux',ABS(ierr))
      ENDIF

      CALL timing(subname,OPR='stop')
      CALL log_pop(subname)
      !
   END SUBROUTINE overlap

