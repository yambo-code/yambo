!
! Copyright (C) 2005 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*******************************************************************
   SUBROUTINE projection( ik_g, dimw, imin, dimwinx, evc, evc_info, dimwann, trial, proj)
   !*******************************************************************
   !
   ! ...  Calculate the projection of the gaussians on the bloch eigenstates inside 
   !      energy window: store it in dimwin(ik_g) X dimwann overlap matrix CA
   !
   !      proj(iwann,ib,ik) = < ib, ik | iwann >
   !
   !      The scalar product is directly done in reciprocal space, providing an
   !      analytical form for the FT of the gaussian orbitals.
   !
   USE kinds,                ONLY : dbl
   USE constants,            ONLY : CONE, CZERO, TWO
   USE log_module,           ONLY : log_push, log_pop
   USE timing_module,        ONLY : timing
   USE sph_har_module,       ONLY : sph_har_index 
   USE trial_center_module,  ONLY : trial_center, trial_center_setup
   !
   USE lattice_module,       ONLY : tpiba
   USE kpoints_module,       ONLY : vkpt_g
   USE wfc_data_module,      ONLY : igsort
   USE ggrids_module,        ONLY : g, gamma_only
   USE wfc_info_module

   IMPLICIT NONE
   !
   ! I/O variables
   !
   TYPE(wfc_info),     INTENT(in) :: evc_info
   COMPLEX(dbl),       INTENT(in) :: evc( evc_info%npwx, * )
   INTEGER,            INTENT(in) :: ik_g, dimw, imin
   INTEGER,            INTENT(in) :: dimwinx
   INTEGER,            INTENT(in) :: dimwann
   TYPE(trial_center), INTENT(in) :: trial(dimwann)
   COMPLEX(dbl),    INTENT(inout) :: proj(dimwinx,dimwann)
   !
   ! local variables
   !
#ifdef __INTEL
   INTEGER :: npwx
#endif
   INTEGER :: npwk
   INTEGER :: lmax
   INTEGER :: iwann, ib, ig, ind 
   INTEGER :: ierr
   INTEGER,      ALLOCATABLE :: ylm_info(:,:)
   REAL(dbl),    ALLOCATABLE :: ylm(:,:), vkg(:,:), vkgg(:)
   COMPLEX(dbl), ALLOCATABLE :: trial_vect(:)
   !
   ! End of declaration
   !

!
!------------------------------
! main body
!------------------------------
!
      CALL timing('projection',OPR='start')
      CALL log_push('projection')

      ind = wfc_info_getindex(imin, ik_g, "SPSI_IK", evc_info)
      !
      npwk = evc_info%npw(ind)
      !
#ifdef __INTEL
      npwx = evc_info%npwx
#endif

      ALLOCATE( trial_vect(npwk), STAT = ierr )
      IF( ierr /= 0 ) CALL errore( 'projection', 'allocating trial_vect', npwk )

      !
      ! set the maximum l for the spherical harmonics
      ! Here we use ABS( l ) because l = -1 is used for sp3
      ! hybrid orbitals which are combinations of s and p Y_lm
      !
      lmax = 0
      !
      DO iwann=1,dimwann
         lmax = MAX( lmax, ABS( trial(iwann)%l )  )
      ENDDO

      ALLOCATE( ylm_info(-lmax:lmax, 0:lmax ), STAT=ierr )
      IF( ierr /= 0 ) CALL errore( 'projection', 'allocating ylm_info', ABS(ierr) )
      !
      ALLOCATE( vkg(3,npwk), STAT=ierr )
      IF( ierr /= 0 ) CALL errore( 'projection', 'allocating vkg', ABS(ierr) )
      !
      ALLOCATE( vkgg(npwk), STAT=ierr )
      IF( ierr /= 0 ) CALL errore( 'projection', 'allocating vkgg', ABS(ierr) )
      !
      ALLOCATE( ylm(npwk,(lmax+1)**2), STAT=ierr )
      IF( ierr /= 0 ) CALL errore( 'projection', 'allocating ylm', ABS(ierr) )

      !
      ! compute the needed spherical harmonics
      ! vkg in bohr^-1
      !
!$omp parallel do
      DO ig = 1, npwk
          !
          vkg(:,ig) = - ( vkpt_g(:,ik_g) + g(:, igsort(ig,ik_g))*tpiba )  
          vkgg(ig)  = DOT_PRODUCT( vkg(:,ig) , vkg(:,ig) ) 
          !
      ENDDO
!$omp end parallel do
      !
      CALL ylmr2( (lmax+1)**2, npwk, vkg, vkgg, ylm ) 
      CALL sph_har_index(lmax, ylm_info)


      !
      ! ... wannier trials
      DO iwann = 1, dimwann
          !
          ! set the trial centers in PW represent.
          CALL trial_center_setup(ik_g, npwk, vkgg, lmax, ylm, ylm_info, &
                                  trial(iwann), trial_vect)

#define __OLD_PROJECTIONS

#ifdef __OLD_PROJECTIONS

          !
          ! ... bands 
          !
!$omp parallel do private(ind)
          DO ib = 1, dimw
             !
             ind = wfc_info_getindex(imin +ib -1, ik_g, "SPSI_IK", evc_info)
             !
             proj(ib,iwann) = CZERO    
             !
             DO ig = 1, npwk
                 proj(ib,iwann) = proj(ib,iwann) +  &
                        CONJG( evc(ig,ind) ) * trial_vect(ig)
             ENDDO
             !
          ENDDO 
!$omp end parallel do
          !
#else

          !
          ! get the indexes of the required wfs in the evc workspace
          !
          ind = wfc_info_getindex( imin, ik_g, "SPSI_IK", evc_info )
          !
          ! perform the scalar produce < nk | trial_vect > 
          ! for all the bands in the selected energy window
          !
#ifdef __INTEL
          !
          ! workaround for a strange behavior (bug?) of the intel compiler
          !
          CALL ZGEMV ( 'C', npwk, dimw, CONE, evc(1:npwx, ind:ind+dimw-1 ), npwx, &
                       trial_vect, 1, CZERO, proj(:, iwann), 1 )
#else
          CALL ZGEMV ( 'C', npwk, dimw, CONE, evc(:, ind:ind+dimw-1 ), npwx, &
                       trial_vect, 1, CZERO, proj(:, iwann), 1 )
#endif

#endif
                 
          !
          IF ( gamma_only ) THEN
              !
!$omp parallel do private(ind)
              DO ib = 1, dimw
                  !
                  ind = wfc_info_getindex(imin +ib -1, ik_g, "SPSI_IK", evc_info)
                  !
                  proj(ib,iwann) = proj(ib,iwann) + CONJG( proj(ib,iwann) ) &
                               - CONJG(evc(1, ind )) * trial_vect(1)
              ENDDO
!$omp end parallel do
              !
          ENDIF
          !
      ENDDO    

      !
      ! local cleaning
      ! 
      DEALLOCATE( trial_vect, STAT=ierr )
      IF (ierr/=0) CALL errore('projection','deallocating trial_vect',ABS(ierr))
      !
      DEALLOCATE( ylm_info, STAT=ierr )
      IF (ierr/=0) CALL errore('projection','deallocating ylm_info',ABS(ierr))
      !
      DEALLOCATE( vkg, STAT=ierr )
      IF (ierr/=0) CALL errore('projection','deallocating vkg', ABS(ierr) )
      !
      DEALLOCATE( vkgg, STAT=ierr )
      IF (ierr/=0) CALL errore('projection','deallocating vkgg', ABS(ierr) )
      !
      DEALLOCATE( ylm, STAT=ierr )
      IF (ierr/=0) CALL errore('projection','deallocating ylm', ABS(ierr) )

      CALL timing('projection',OPR='stop')
      CALL log_pop('projection')
   
   END SUBROUTINE projection

