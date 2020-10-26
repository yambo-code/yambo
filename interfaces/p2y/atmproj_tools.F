!
! Copyright (C) 2012 WanT Group
! Adapted for p2y by DS,DV
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License\'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!*********************************************
MODULE atmproj_tools_module
!*********************************************
   !
   USE parameters,          ONLY : nstrx
   !
   IMPLICIT NONE 
   PRIVATE
   SAVE

   !
   ! some default for kinds
   !
   INTEGER,   PARAMETER :: dbl = SELECTED_REAL_KIND( 14, 200 )
   INTEGER,   PARAMETER :: ionode_id=1
   LOGICAL              :: ioneode=.true.

   !
   ! global variables of the module
   !
   CHARACTER(nstrx)   :: savedir
   CHARACTER(nstrx)   :: file_proj
   CHARACTER(nstrx)   :: file_data
   CHARACTER(nstrx)   :: file_fmt
   !
   LOGICAL            :: init = .FALSE.
   !
   ! parameters for the reconstruction 
   ! of the Hamiltonian
   !
   REAL(dbl)          :: atmproj_sh = 10.0d0
   REAL(dbl)          :: atmproj_thr = 0.0d0    ! 0.9d0
   INTEGER            :: atmproj_nbnd = 0
   INTEGER            :: atmproj_nbndmin = 1
   !
   INTEGER, PARAMETER :: nwfcx = 50

   ! contains:
   !
   PUBLIC :: atmproj_read_ext

CONTAINS
!*************************************************
SUBROUTINE atmproj_read_ext ( filein, nbnd, nkpt, nspin, natomwfc, nelec, &
                              efermi, energy_units, vkpt, wk, eig, proj, kovp, ierr )
!*************************************************
   !
   USE iotk_module
   !
   IMPLICIT NONE
   !
   CHARACTER(*),           INTENT(IN)   :: filein
   INTEGER,      OPTIONAL, INTENT(OUT)  :: nbnd, nkpt, nspin, natomwfc
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: nelec, efermi
   CHARACTER(*), OPTIONAL, INTENT(OUT)  :: energy_units
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: vkpt(:,:), wk(:), eig(:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: proj(:,:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: kovp(:,:,:,:)
   INTEGER,                INTENT(OUT)  :: ierr
   !
   !
   CHARACTER(256)    :: attr
   INTEGER           :: iunit
   LOGICAL           :: l_old_atmproj,l_new_atmproj
   !
   INTEGER           :: nbnd_, nkpt_, nspin_, natomwfc_ 
   REAL(dbl)         :: nelec_, efermi_
   CHARACTER(20)     :: energy_units_
   !

   CALL iotk_free_unit( iunit )
   ierr = 0
   CALL iotk_open_read( iunit, FILE=TRIM(filein), IERR=ierr )
   IF ( ierr/=0 ) RETURN

   attr=""
   CALL iotk_scan_empty( iunit, "HEADER", ATTR=attr, IERR=ierr)  
   l_old_atmproj= (ierr/=0)
   l_new_atmproj= (ierr==0)
   
   IF (present(vkpt) .or. present(kovp) ) THEN
     call error(' not coded')
   ENDIF

   IF ( l_old_atmproj ) then
!      WRITE(*,*) "Old atmproj format"
!     CALL atmproj_read_ext_old ( filein, nbnd_, nkpt_, nspin_, natomwfc_, nelec_, &
!                              efermi_, energy_units_, vkpt, wk, eig, proj, kovp, ierr, iunit )
     if (      present(proj) ) CALL atmproj_read_ext_old ( filein, nbnd_, nkpt_, nspin_, natomwfc_, nelec_, &
                              efermi_, energy_units_, proj=proj, ierr=ierr, iunit=iunit )
     if (.not. present(proj) ) CALL atmproj_read_ext_old ( filein, nbnd_, nkpt_, nspin_, natomwfc_, nelec_, &
                              efermi_, energy_units_, ierr=ierr, iunit=iunit )
   ENDIF
                              
   IF ( l_new_atmproj ) THEN
!      WRITE(*,*) "New atmproj format",iunit
!     CALL atmproj_read_ext_new ( filein, nbnd_, nkpt_, nspin_, natomwfc_, nelec_, &
!                              efermi_, energy_units_, vkpt, wk, eig, proj, kovp, ierr, iunit, attr )
     if (      present(proj) ) CALL atmproj_read_ext_new ( filein, nbnd_, nkpt_, nspin_, natomwfc_, nelec_, &
                              efermi_, energy_units_, proj=proj, ierr=ierr, iunit=iunit, attr=attr )
     if (.not. present(proj) ) CALL atmproj_read_ext_new ( filein, nbnd_, nkpt_, nspin_, natomwfc_, nelec_, &
                              efermi_, energy_units_, ierr=ierr, iunit=iunit, attr=attr )
   ENDIF
   
   ! This gives error with new atmproj
   ! if iotk library is compiled with out of bound error check 
   CALL iotk_close_read( iunit, IERR=ierr )
   IF ( ierr/=0 ) RETURN

   IF ( PRESENT( nbnd ) )         nbnd = nbnd_
   IF ( PRESENT( nkpt ) )         nkpt = nkpt_
   IF ( PRESENT( nspin ) )        nspin = nspin_
   IF ( PRESENT( natomwfc ) )     natomwfc = natomwfc_
   IF ( PRESENT( nelec ) )        nelec = nelec_
   IF ( PRESENT( efermi ) )       efermi = efermi_
   IF ( PRESENT( energy_units ) ) energy_units = TRIM(energy_units_)
   !
   RETURN
   !
END SUBROUTINE atmproj_read_ext


!*************************************************
SUBROUTINE atmproj_read_ext_new ( filein, nbnd, nkpt, nspin, natomwfc, nelec, &
                              efermi, energy_units, vkpt, wk, eig, proj, kovp, ierr, iunit, attr )
!*************************************************
   !
   USE iotk_module
   !
   IMPLICIT NONE
   !
   CHARACTER(*),           INTENT(IN)   :: filein
   INTEGER,      OPTIONAL, INTENT(OUT)  :: nbnd, nkpt, nspin, natomwfc
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: nelec, efermi
   CHARACTER(*), OPTIONAL, INTENT(OUT)  :: energy_units
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: vkpt(:,:), wk(:), eig(:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: proj(:,:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: kovp(:,:,:,:)
   INTEGER,                INTENT(OUT)  :: ierr
   INTEGER,                INTENT(IN)   :: iunit
   CHARACTER(256),         INTENT(INOUT):: attr
   !
   CHARACTER(256)    :: str
   INTEGER           :: ik, isp, ias
   !
   INTEGER           :: nbnd_, nkpt_, nspin_, natomwfc_ 
   REAL(dbl)         :: nelec_, efermi_
   CHARACTER(20)     :: energy_units_
   !
   COMPLEX(dbl), ALLOCATABLE :: ztmp(:,:)
   REAL(dbl),    ALLOCATABLE :: rtmp(:,:,:)

     CALL iotk_scan_attr( attr, "NUMBER_OF_BANDS", nbnd_, IERR=ierr)
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_attr( attr, "NUMBER_OF_K-POINTS", nkpt_, IERR=ierr)
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_attr( attr, "NUMBER_OF_SPIN_COMPONENTS", nspin_, IERR=ierr)
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_attr( attr, "NUMBER_OF_ATOMIC_WFC", natomwfc_, IERR=ierr)
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_attr( attr, "NUMBER_OF_ELECTRONS", nelec_, IERR=ierr)
     IF ( ierr/=0 ) RETURN
     energy_units_="Rydberg"
     !
     CALL iotk_scan_attr( attr, "FERMI_ENERGY", efermi_, IERR=ierr)
     IF ( ierr/=0 ) RETURN
   !
   ! reading kpoints, weight, energies and proj
!to be generalized with spin > 1
   !
    CALL iotk_scan_begin( iunit, "EIGENSTATES", IERR=ierr)
     IF ( ierr/=0 ) RETURN
!
     DO isp=1,nspin_
       DO ik = 1, nkpt_
         IF ( PRESENT( vkpt ) ) THEN
           CALL iotk_scan_dat( iunit, "K-POINT", vkpt(:,ik), attr=attr, IERR=ierr)
           IF ( ierr/=0 ) RETURN
           call iotk_scan_attr(attr , "Weight", wk(ik), IERR=ierr)
           IF ( ierr/=0 ) RETURN
         ENDIF
         IF ( PRESENT( eig ) ) THEN
            CALL iotk_scan_dat(iunit, "E" , eig(:,ik, isp ), IERR=ierr)    
            IF ( ierr/=0 ) RETURN
         ENDIF
         !
         IF ( PRESENT( proj ) ) THEN
           !
           ALLOCATE( rtmp(2,nbnd_, natomwfc_) )
           ALLOCATE( ztmp(nbnd_, natomwfc_) )
           !
           CALL iotk_scan_begin( iunit, "PROJS", IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           DO ias = 1, natomwfc_
             !
             str= "ATOMIC_WFC"
             !
             CALL iotk_scan_dat(iunit, TRIM(str) , rtmp( :,:, ias ), IERR=ierr)
             IF ( ierr /= 0 ) RETURN
             !
           ENDDO
           !
           ztmp(1:nbnd_,1:natomwfc_)=cmplx(rtmp(1,1:nbnd_,1:natomwfc_),rtmp(2,1:nbnd_,1:natomwfc_)) 
           proj( 1:natomwfc_, 1:nbnd_, ik, isp ) = TRANSPOSE( ztmp(1:nbnd_,1:natomwfc_) )
           CALL iotk_scan_end( iunit, "PROJS", IERR=ierr )
           IF ( ierr/=0 ) RETURN
           DEALLOCATE( rtmp )
           DEALLOCATE( ztmp )
           !
         ENDIF
       ENDDO
     ENDDO
     
     CALL iotk_scan_end( iunit, "EIGENSTATES", IERR=ierr)
     IF ( ierr/=0 ) RETURN
     
   IF ( PRESENT( nbnd ) )         nbnd = nbnd_
   IF ( PRESENT( nkpt ) )         nkpt = nkpt_
   IF ( PRESENT( nspin ) )        nspin = nspin_
   IF ( PRESENT( natomwfc ) )     natomwfc = natomwfc_
   IF ( PRESENT( nelec ) )        nelec = nelec_
   IF ( PRESENT( efermi ) )       efermi = efermi_
   IF ( PRESENT( energy_units ) ) energy_units = TRIM(energy_units_)


END SUBROUTINE atmproj_read_ext_new

SUBROUTINE atmproj_read_ext_old ( filein, nbnd, nkpt, nspin, natomwfc, nelec, &
                              efermi, energy_units, vkpt, wk, eig, proj, kovp, ierr, iunit )
!*************************************************
   !
   USE iotk_module
   !
   IMPLICIT NONE
   !
   CHARACTER(*),           INTENT(IN)   :: filein
   INTEGER,      OPTIONAL, INTENT(OUT)  :: nbnd, nkpt, nspin, natomwfc
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: nelec, efermi
   CHARACTER(*), OPTIONAL, INTENT(OUT)  :: energy_units
   REAL(dbl),    OPTIONAL, INTENT(OUT)  :: vkpt(:,:), wk(:), eig(:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: proj(:,:,:,:)
   COMPLEX(dbl), OPTIONAL, INTENT(OUT)  :: kovp(:,:,:,:)
   INTEGER,                INTENT(OUT)  :: ierr
   INTEGER,                INTENT(IN)   :: iunit
   !
   CHARACTER(256)    :: attr, str
   INTEGER           :: ik, isp, ias
   !
   INTEGER           :: nbnd_, nkpt_, nspin_, natomwfc_ 
   REAL(dbl)         :: nelec_, efermi_
   CHARACTER(20)     :: energy_units_
   !
   COMPLEX(dbl), ALLOCATABLE :: ztmp(:,:)
   REAL(dbl),    ALLOCATABLE :: rtmp(:,:,:)


     CALL iotk_scan_begin( iunit, "HEADER", IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_dat( iunit, "NUMBER_OF_BANDS", nbnd_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_dat( iunit, "NUMBER_OF_K-POINTS", nkpt_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_dat( iunit, "NUMBER_OF_SPIN_COMPONENTS", nspin_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_dat( iunit, "NUMBER_OF_ATOMIC_WFC", natomwfc_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_dat( iunit, "NUMBER_OF_ELECTRONS", nelec_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     !
     CALL iotk_scan_empty( iunit, "UNITS_FOR_ENERGY", ATTR=attr, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     CALL iotk_scan_attr( attr, "UNITS", energy_units_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     !
     CALL iotk_scan_dat( iunit, "FERMI_ENERGY", efermi_, IERR=ierr) 
     IF ( ierr/=0 ) RETURN
     !
     CALL iotk_scan_end( iunit, "HEADER", IERR=ierr) 
     IF ( ierr/=0 ) RETURN
   !
   ! reading kpoints and weights 
   ! 
     IF ( PRESENT( vkpt ) ) THEN
       !
       CALL iotk_scan_dat( iunit, "K-POINTS", vkpt(:,:), IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
     ENDIF
   !
     IF ( PRESENT (wk) ) THEN
       !
       CALL iotk_scan_dat( iunit, "WEIGHT_OF_K-POINTS", wk(:), IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
     ENDIF
   
   !
   ! Added by Luis to take spinors into accout
   !
     IF ( nspin_ == 4 ) nspin_ = 1
   
   ! 
   ! reading eigenvalues
   ! 

     IF ( PRESENT( eig ) ) THEN
       ! 
       CALL iotk_scan_begin( iunit, "EIGENVALUES", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
       DO ik = 1, nkpt_
           !
           CALL iotk_scan_begin( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           IF ( nspin_ == 1 ) THEN
                !
                isp = 1
                !
                CALL iotk_scan_dat(iunit, "EIG" , eig(:,ik, isp ), IERR=ierr)
                IF ( ierr /= 0 ) RETURN
                !
           ELSE
                !
                DO isp=1,nspin_
                   !
                   str = "EIG"//TRIM(iotk_index(isp))
                   !
                   CALL iotk_scan_dat(iunit, TRIM(str) , eig(:,ik,isp), IERR=ierr)
                   IF ( ierr /= 0 ) RETURN
                   !
                ENDDO
                !
           ENDIF       
           !
           !
           CALL iotk_scan_end( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
       ENDDO
       !
       !
       CALL iotk_scan_end( iunit, "EIGENVALUES", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
     ENDIF


   ! 
   ! reading projections
   ! 
     IF ( PRESENT( proj ) ) THEN
       !
       ALLOCATE( ztmp(nbnd_, natomwfc_) )
       !
       CALL iotk_scan_begin( iunit, "PROJECTIONS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
       !
       DO ik = 1, nkpt_
           !
           !
           CALL iotk_scan_begin( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           DO isp = 1, nspin_
               !
               IF ( nspin_ == 2 ) THEN
                   !
                   CALL iotk_scan_begin( iunit, "SPIN"//TRIM(iotk_index(isp)), IERR=ierr )
                   IF ( ierr/=0 ) RETURN
                   !
               ENDIF
               !
               DO ias = 1, natomwfc_
                   !
                   str= "ATMWFC"//TRIM( iotk_index( ias ) )
                   !
                   CALL iotk_scan_dat(iunit, TRIM(str) , ztmp( :, ias ), IERR=ierr)
                   IF ( ierr /= 0 ) RETURN
                   !
               ENDDO
               !
               proj( 1:natomwfc_, 1:nbnd_, ik, isp ) = TRANSPOSE( ztmp(1:nbnd_,1:natomwfc_) ) 
               !
               !
               IF ( nspin_ == 2 ) THEN
                   !
                   CALL iotk_scan_end( iunit, "SPIN"//TRIM(iotk_index(isp)), IERR=ierr )
                   IF ( ierr/=0 ) RETURN
                   !
               ENDIF
               !
           ENDDO
           !
           !
           CALL iotk_scan_end( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           !
       ENDDO
       !
       DEALLOCATE( ztmp )
       !
       CALL iotk_scan_end( iunit, "PROJECTIONS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
     ENDIF
   ! 
   ! reading overlaps
   ! 
     IF ( PRESENT( kovp ) ) THEN
       !
       CALL iotk_scan_begin( iunit, "OVERLAPS", IERR=ierr )
       IF ( ierr/=0 ) THEN
           call error(' OVERLAPS data not found in file. Crashing ...')
       ENDIF
       !
       DO ik = 1, nkpt_
           CALL iotk_scan_begin( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
           DO isp = 1, nspin_
               !
               CALL iotk_scan_dat(iunit, "OVERLAP"//TRIM(iotk_index(isp)), kovp( :, :, ik, isp ), IERR=ierr)
               IF ( ierr/=0 ) RETURN
               !
           ENDDO
           !
           CALL iotk_scan_end( iunit, "K-POINT"//TRIM(iotk_index(ik)), IERR=ierr )
           IF ( ierr/=0 ) RETURN
           !
       ENDDO
       !
       CALL iotk_scan_end( iunit, "OVERLAPS", IERR=ierr )
       IF ( ierr/=0 ) RETURN
       !
     ENDIF

   !
   IF ( PRESENT( nbnd ) )         nbnd = nbnd_
   IF ( PRESENT( nkpt ) )         nkpt = nkpt_
   IF ( PRESENT( nspin ) )        nspin = nspin_
   IF ( PRESENT( natomwfc ) )     natomwfc = natomwfc_
   IF ( PRESENT( nelec ) )        nelec = nelec_
   IF ( PRESENT( efermi ) )       efermi = efermi_
   IF ( PRESENT( energy_units ) ) energy_units = TRIM(energy_units_)


END SUBROUTINE atmproj_read_ext_old

END MODULE atmproj_tools_module
