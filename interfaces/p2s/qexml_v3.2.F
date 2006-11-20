!
! Copyright (C) 2006 WanT Group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------------------------------------
MODULE qexml_module
  !----------------------------------------------------------------------------
  !
  ! This module contains some common subroutines used to read and write
  ! in XML format the data produced by Quantum-ESPRESSO package.
  !
  ! Written by Andrea Ferretti (2006).
  ! Part of the implementation is taken from xml_io_base.f90
  ! (written by Carlo Sbraccia) in the Quantum-ESPRESSO distribution.
  !
  USE iotk_module
  IMPLICIT NONE
  !
  PRIVATE
  SAVE
  !
  ! definitions for the fmt
  !
  CHARACTER(5), PARAMETER :: fmt_name = "QEXML"
  CHARACTER(5), PARAMETER :: fmt_version = "1.2.0"
  !
  ! some default for kinds
  !
  INTEGER,   PARAMETER :: dbl = SELECTED_REAL_KIND( 14, 200 )
  REAL(dbl), PARAMETER :: e2 = 2.0_dbl
  !
  ! internal data to be set
  !
  CHARACTER(256)   :: datadir
  INTEGER          :: iunpun, rhounit
  LOGICAL          :: rho_binary
  !
  ! end of declarations
  !
  PUBLIC :: fmt_name, fmt_version
  PUBLIC :: iunpun, rhounit, rho_binary
  !
  PUBLIC :: qexml_init,  qexml_openfile, qexml_closefile
  !
  PUBLIC :: qexml_read_header, qexml_read_cell, qexml_read_ions,      &
            qexml_read_symmetry, qexml_read_planewaves,               &
            qexml_read_spin, qexml_read_xc,                           &
            qexml_read_occ, qexml_read_bz, qexml_read_phonon,         &
            qexml_read_bands, qexml_read_bands_info,                  &
            qexml_read_gk, qexml_read_wfc
!            qexml_read_rho

  !
  CHARACTER(iotk_attlenx) :: attr
  !
CONTAINS
  !
!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!
    !------------------------------------------------------------------------
    SUBROUTINE qexml_init( iunpun_, datadir_, rhounit_, rho_binary_ )
      !------------------------------------------------------------------------
      !
      ! just init module data
      !
      IMPLICIT NONE
      INTEGER,           INTENT(IN) :: iunpun_
      INTEGER,           INTENT(IN) :: rhounit_
      CHARACTER(*),      INTENT(IN) :: datadir_    
      LOGICAL, OPTIONAL, INTENT(IN) :: rho_binary_
      !
      iunpun  = iunpun_
      rhounit = rhounit_
      datadir = TRIM(datadir_)
      !
      rho_binary = .TRUE.
      IF ( PRESENT(rho_binary_) ) rho_binary = rho_binary_
      !
    END SUBROUTINE qexml_init


    !------------------------------------------------------------------------
    SUBROUTINE qexml_openfile( filename, action, binary, ierr)
      !------------------------------------------------------------------------
      !
      ! open data file
      !
      IMPLICIT NONE
      !
      CHARACTER(*),       INTENT(IN)  :: filename
      CHARACTER(*),       INTENT(IN)  :: action      ! ("read"|"write")
      LOGICAL, OPTIONAL,  INTENT(IN)  :: binary
      INTEGER,            INTENT(OUT) :: ierr
      !
      LOGICAL :: binary_

      ierr = 0
      binary_ = .FALSE.
      IF ( PRESENT(binary) ) binary_ = binary 
      !
      SELECT CASE ( TRIM(action) )
      CASE ( "read", "READ" )
          !
          CALL iotk_open_read ( iunpun, FILE = TRIM(filename), IERR=ierr )
          !
      CASE ( "write", "WRITE" )
          !
          CALL iotk_open_write( iunpun, FILE = TRIM(filename), BINARY=binary_, IERR=ierr )
          !
      CASE DEFAULT
          ierr = 1
      END SELECT
          
    END SUBROUTINE qexml_openfile
      

    !------------------------------------------------------------------------
    SUBROUTINE qexml_closefile( action, ierr)
      !------------------------------------------------------------------------
      !
      ! close data file
      !
      IMPLICIT NONE
      !
      CHARACTER(*),  INTENT(IN)  :: action      ! ("read"|"write")
      INTEGER,       INTENT(OUT) :: ierr
      !
      ierr = 0
      !
      SELECT CASE ( TRIM(action) )
      CASE ( "read", "READ" )
          !
          CALL iotk_close_read( iunpun, IERR=ierr )
          !
      CASE ( "write", "WRITE" )
          !
          CALL iotk_close_write( iunpun, IERR=ierr )
          !
      CASE DEFAULT
          ierr = 2
      END SELECT
      !
    END SUBROUTINE qexml_closefile

!
!-------------------------------------------
! ... basic (private) subroutines
!-------------------------------------------
!
    !------------------------------------------------------------------------
    FUNCTION int_to_char( int )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: int
      CHARACTER (LEN=6)   :: int_to_char
      !
      !
      IF ( int < 10 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I1)" ) int
         !
      ELSE IF ( int < 100 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I2)" ) int
         !
       ELSE IF ( int < 1000 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I3)" ) int
         !
       ELSE IF ( int < 10000 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I4)" ) int
         !
       ELSE
         !
       WRITE( UNIT = int_to_char , FMT = "(I5)" ) int
       !
      END IF
      !
    END FUNCTION int_to_char

    !------------------------------------------------------------------------
    SUBROUTINE create_directory( dirname )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*), INTENT(IN) :: dirname
      !
      INTEGER                    :: ierr
      INTEGER, EXTERNAL          :: c_mkdir
      !
      ierr = c_mkdir( TRIM( dirname ), LEN_TRIM( dirname ) )
      !
      CALL errore( 'create_directory', &
                   'unable to create directory ' // TRIM( dirname ), ierr )
      !
      ! ... check whether the scratch directory is writable
      !
      OPEN( UNIT = 4, FILE = TRIM( dirname ) // '/test', &
            STATUS = 'UNKNOWN', IOSTAT = ierr )
      CLOSE( UNIT = 4, STATUS = 'DELETE' )
      !
      CALL errore( 'create_directory:', &
                   TRIM( dirname ) // ' non existent or non writable', ierr )
      !
      RETURN
      !
    END SUBROUTINE create_directory
    !
    !------------------------------------------------------------------------
    FUNCTION kpoint_dir( basedir, ik )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=256)           :: kpoint_dir
      CHARACTER(LEN=*), INTENT(IN) :: basedir
      INTEGER,          INTENT(IN) :: ik
      !
      CHARACTER(LEN=256) :: kdirname
      CHARACTER(LEN=5)   :: kindex
      !
      WRITE( kindex, FMT = '( I5.5 )' ) ik     
      !
      kdirname = TRIM( basedir ) // '/K' // kindex
      !
      kpoint_dir = TRIM( kdirname )
      !
      RETURN
      !
    END FUNCTION kpoint_dir
    !
    !------------------------------------------------------------------------
    FUNCTION wfc_filename( basedir, name, ik, ipol, tag, extension )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=256)                 :: wfc_filename
      CHARACTER(LEN=*),       INTENT(IN) :: basedir
      CHARACTER(LEN=*),       INTENT(IN) :: name
      INTEGER,                INTENT(IN) :: ik
      INTEGER,      OPTIONAL, INTENT(IN) :: ipol
      CHARACTER(*), OPTIONAL, INTENT(IN) :: tag
      CHARACTER(*), OPTIONAL, INTENT(IN) :: extension
      !
      CHARACTER(LEN=256) :: filename, tag_, ext_
      !
      !
      filename = ''
      tag_     = ''
      ext_     = '.dat'
      !
      IF ( PRESENT( tag ) )         tag_ = '_'//TRIM(tag)
      IF ( PRESENT( extension ) )   ext_ = '.'//TRIM(extension)
      !
      IF ( PRESENT( ipol ) ) THEN
         !
         WRITE( filename, FMT = '( I1 )' ) ipol
         !
      END IF
      !
      filename = TRIM( kpoint_dir( basedir, ik ) ) // '/' // &
                 & TRIM( name ) // TRIM( filename ) // TRIM( tag_ ) // TRIM( ext_)
      !
      wfc_filename = TRIM( filename )
      !
      RETURN
      !
    END FUNCTION
    !
    !------------------------------------------------------------------------
    SUBROUTINE copy_file( file_in, file_out )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*), INTENT(IN) :: file_in, file_out
      !
      CHARACTER(LEN=256) :: string
      INTEGER            :: iun_in, iun_out, ierr
      !
      !
      CALL iotk_free_unit( iun_in,  ierr )
      CALL iotk_free_unit( iun_out, ierr )
      !
      CALL errore( 'copy_file', 'no free units available', ierr )
      !
      OPEN( UNIT = iun_in,  FILE = file_in,  STATUS = "OLD" )
      OPEN( UNIT = iun_out, FILE = file_out, STATUS = "UNKNOWN" )         
      !
      copy_loop: DO
         !
         READ( UNIT = iun_in, FMT = '(A256)', IOSTAT = ierr ) string
         !
         IF ( ierr < 0 ) EXIT copy_loop
         !
         WRITE( UNIT = iun_out, FMT = '(A)' ) TRIM( string )
         !
      END DO copy_loop
      !
      CLOSE( UNIT = iun_in )
      CLOSE( UNIT = iun_out )
      !
      RETURN
      !
    END SUBROUTINE

!
!-------------------------------------------
! ... read subroutines
!-------------------------------------------
!
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_rho( rho_file_base, rho, &
                              nr1, nr2, nr3, nr1x, nr2x, ipp, npp )
      !------------------------------------------------------------------------
      !
      ! ... Writes charge density rho, one plane at a time.
      ! ... If ipp and npp are specified, planes are collected one by one from
      ! ... all processors, avoiding an overall collect of the charge density
      ! ... on a single proc.
      !
#ifdef __HAVE_RHO_READ
!      USE io_global, ONLY : ionode, ionode_id
!      USE mp_global, ONLY : me_image, intra_image_comm, me_pool, nproc_pool, &
!                            intra_pool_comm, my_pool_id, npool
!      USE mp,        ONLY : mp_put
#endif
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=*),   INTENT(IN)  :: rho_file_base
      INTEGER,            INTENT(IN)  :: nr1, nr2, nr3
      INTEGER,            INTENT(IN)  :: nr1x, nr2x
      REAL(dbl),          INTENT(OUT) :: rho(:)
      INTEGER, OPTIONAL,  INTENT(IN)  :: ipp(:)
      INTEGER, OPTIONAL,  INTENT(IN)  :: npp(:)
      !
      INTEGER                :: ierr, i, j, k, kk, ldr, ip
      INTEGER                :: nr( 3 )
      CHARACTER(LEN=256)     :: rho_file
      REAL(dbl), ALLOCATABLE :: rho_plane(:)
      INTEGER,   ALLOCATABLE :: kowner(:)
      INTEGER                :: iopool_id, ionode_pool
      !
      !
#ifdef __HAVE_RHO_READ

      rho_file = TRIM( rho_file_base ) // '.xml'
      !
      IF ( ionode ) &
         CALL iotk_open_read( rhounit, FILE = rho_file, &
                              BINARY = rho_binary, IERR = ierr )
      !
      CALL mp_bcast( ierr, ionode_id, intra_image_comm )
      !
      CALL errore( 'read_rho', 'cannot open ' // &
                 & TRIM( rho_file ) // ' file for reading', ierr )
      !
      IF ( ionode ) THEN
         !
         CALL iotk_scan_begin( rhounit, "CHARGE-DENSITY" )
         !
         CALL iotk_scan_empty( rhounit, "INFO", attr )
         !
         CALL iotk_scan_attr( attr, "nr1", nr(1) )
         CALL iotk_scan_attr( attr, "nr2", nr(2) )
         CALL iotk_scan_attr( attr, "nr3", nr(3) )
         !
      END IF
      !
      CALL mp_bcast( nr, ionode_id, intra_image_comm )
      !
      IF ( nr1 /= nr(1) .OR. nr2 /= nr(2) .OR. nr3 /= nr(3) ) &
         CALL errore( 'read_rho', 'dimensions do not match', 1 )
      !
      ALLOCATE( rho_plane( nr1*nr2 ) )
      ALLOCATE( kowner( nr3 ) )
      !
      ! ... find the index of the pool that will write rho
      !
      IF ( ionode ) iopool_id = my_pool_id
      !
      CALL mp_bcast( iopool_id, ionode_id, intra_image_comm )
      !
      ! ... find the index of the ionode within its own pool
      !
      IF ( ionode ) ionode_pool = me_pool
      !
      CALL mp_bcast( ionode_pool, ionode_id, intra_image_comm )
      !
      ! ... find out the owner of each "z" plane
      !
      IF ( PRESENT( ipp ) .AND. PRESENT( npp ) ) THEN
         !
         DO ip = 1, nproc_pool
            !
            kowner((ipp(ip)+1):(ipp(ip)+npp(ip))) = ip - 1
            !
         END DO
         !
      ELSE
         !
         kowner = ionode_id
         !
      END IF
      !
      ldr = nr1x*nr2x
      !
      DO k = 1, nr3
         !
         ! ... only ionode reads the charge planes
         !
         IF ( ionode ) &
            CALL iotk_scan_dat( rhounit, "z" // iotk_index( k ), rho_plane )
         !
         ! ... planes are sent to the destination processor
         !
         IF( npool > 1 ) THEN
            !
            !  send to all proc/pools
            !
            CALL mp_bcast( rho_plane, ionode_id, intra_image_comm )
            !
         ELSE
            !
            !  send to the destination proc
            !
            IF ( kowner(k) /= ionode_id ) &
               CALL mp_put( rho_plane, rho_plane, me_image, &
                            ionode_id, kowner(k), k, intra_image_comm )
            !
         END IF
         !
         IF( kowner(k) == me_pool ) THEN
            !
            kk = k
            !
            IF ( PRESENT( ipp ) ) kk = k - ipp(me_pool+1)
            ! 
            DO j = 1, nr2
               !
               DO i = 1, nr1
                  !
                  rho(i+(j-1)*nr1x+(kk-1)*ldr) = rho_plane(i+(j-1)*nr1)
                  !
               END DO
               !
            END DO
            !
         END IF
         !
      END DO
      !
      DEALLOCATE( rho_plane )
      DEALLOCATE( kowner )
      !
      IF ( ionode ) THEN
         !
         CALL iotk_scan_end( rhounit, "CHARGE-DENSITY" )
         !
         CALL iotk_close_read( rhounit )
         !
      END IF
      !
#endif
      RETURN
      !
    END SUBROUTINE qexml_read_rho
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_header( creator_name, creator_version, &
                                  format_name, format_version, ierr )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: creator_name, creator_version
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: format_name, format_version
      INTEGER,           OPTIONAL, INTENT(OUT) :: ierr

      CHARACTER(256) :: creator_name_, creator_version_
      CHARACTER(256) :: format_name_,     format_version_

      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunpun, "HEADER", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "FORMAT", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr(attr, "NAME", format_name_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr, "VERSION", format_version_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "CREATOR", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr(attr, "NAME", creator_name_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr, "VERSION", creator_version_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunpun, "HEADER", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT(creator_name) )     creator_name    = TRIM(creator_name_)
      IF ( PRESENT(creator_version) )  creator_version = TRIM(creator_version_)
      IF ( PRESENT(format_name) )      format_name     = TRIM(format_name_)
      IF ( PRESENT(format_version) )   format_version  = TRIM(format_version_)
      !
    END SUBROUTINE qexml_read_header
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_cell( bravais_latt, symm_type, celldm, alat, &
                                a1, a2, a3, b1, b2, b3, alat_units, a_units, b_units, ierr )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: bravais_latt
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: symm_type
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: celldm(6), alat
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: a1(3), a2(3), a3(3)
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: b1(3), b2(3), b3(3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: alat_units, a_units, b_units
      INTEGER,                     INTENT(OUT) :: ierr
      !
      CHARACTER(256)     :: bravais_latt_, symm_type_
      CHARACTER(256)     :: alat_units_, a_units_, b_units_
      REAL(dbl)          :: celldm_(6), alat_ 
      REAL(dbl)          :: a1_(3), a2_(3), a3_(3)
      REAL(dbl)          :: b1_(3), b2_(3), b3_(3)
      !

      ierr=0
      !
      !
      CALL iotk_scan_begin( iunpun, "CELL" )
      !
      CALL iotk_scan_dat( iunpun, "BRAVAIS_LATTICE", bravais_latt_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      IF ( TRIM( bravais_latt_ ) == "Trigonal R" .OR. &
           TRIM( bravais_latt_ ) == "Hexagonal and Trigonal P" ) THEN
         !
         symm_type_ = 'hexagonal'
         !
      ELSE
         !
         symm_type_ = 'cubic'
         !
      END IF
      !
      CALL iotk_scan_dat( iunpun, "LATTICE_PARAMETER", alat_, ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", alat_units_, IERR=ierr )
      IF ( ierr /= 0 ) RETURN
      !
      CALL iotk_scan_dat( iunpun, "CELL_DIMENSIONS", celldm_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_begin( iunpun, "DIRECT_LATTICE_VECTORS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_DIRECT_LATTICE_VECTORS", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", a_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat(   iunpun, "a1", a1_(:), ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat(   iunpun, "a2", a2_(:), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat(   iunpun, "a3", a3_(:), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_end(   iunpun, "DIRECT_LATTICE_VECTORS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_begin( iunpun, "RECIPROCAL_LATTICE_VECTORS", IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_RECIPROCAL_LATTICE_VECTORS", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", b_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat(   iunpun, "b1", b1_(:), ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat(   iunpun, "b2", b2_(:), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat(   iunpun, "b3", b3_(:), IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_end(   iunpun, "RECIPROCAL_LATTICE_VECTORS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunpun, "CELL", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      ! 
      IF ( PRESENT(bravais_latt) )  bravais_latt = bravais_latt_
      IF ( PRESENT(celldm) )        symm_type    = symm_type_
      IF ( PRESENT(symm_type) )     celldm       = celldm_
      IF ( PRESENT(alat) )          alat         = alat_
      IF ( PRESENT(a1) )            a1           = a1_
      IF ( PRESENT(a2) )            a2           = a2_
      IF ( PRESENT(a3) )            a3           = a3_
      IF ( PRESENT(b1) )            b1           = b1_
      IF ( PRESENT(b2) )            b2           = b2_
      IF ( PRESENT(b3) )            b3           = b3_
      IF ( PRESENT(alat_units) )    alat_units   = TRIM(alat_units_)
      IF ( PRESENT(a_units) )       a_units      = TRIM(a_units_)
      IF ( PRESENT(b_units) )       b_units      = TRIM(b_units_)

    END SUBROUTINE qexml_read_cell

    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_ions( nsp, nat, atm, ityp, psfile, amass, amass_units, &
                                tau, tau_units, if_pos, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,          OPTIONAL, INTENT(OUT) :: nsp, nat
      INTEGER,          OPTIONAL, INTENT(OUT) :: ityp(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: atm(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: psfile(:)
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: amass(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: amass_units
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: tau(:,:)
      INTEGER,          OPTIONAL, INTENT(OUT) :: if_pos(:,:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: tau_units
      INTEGER,                    INTENT(OUT) :: ierr
      !
      INTEGER                     :: nat_, nsp_
      CHARACTER(256)              :: tau_units_, amass_units_
      INTEGER,        ALLOCATABLE :: ityp_(:)
      CHARACTER(3),   ALLOCATABLE :: atm_(:)       
      CHARACTER(256), ALLOCATABLE :: psfile_(:)       
      REAL(dbl),      ALLOCATABLE :: amass_(:)
      REAL(dbl),      ALLOCATABLE :: tau_(:,:)
      INTEGER,        ALLOCATABLE :: if_pos_(:,:)
      !      
      INTEGER            :: i

      !
      ierr=0
      !
      !
      CALL iotk_scan_begin( iunpun, "IONS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NUMBER_OF_ATOMS", nat_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NUMBER_OF_SPECIES", nsp_ )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_ATOMIC_MASSES", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", amass_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT(nat) )   nat = nat_
      IF ( PRESENT(nsp) )   nsp = nsp_
      ! 
      ALLOCATE( atm_(nsp_) ) 
      ALLOCATE( amass_(nsp_) ) 
      ALLOCATE( psfile_(nsp_) ) 
      !
      DO i = 1, nsp_
         !
         CALL iotk_scan_dat( iunpun, "ATOM_TYPE", atm_(i), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat( iunpun, TRIM( atm_(i) ) // "_MASS", amass_(i), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat( iunpun, "PSEUDO_FOR_" // TRIM( atm_(i) ), psfile_(i), IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDDO
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_ATOMIC_POSITIONS", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", tau_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      ALLOCATE( ityp_(nat_) ) 
      ALLOCATE( tau_(3,nat_) ) 
      ALLOCATE( if_pos_(3,nat_) ) 
      !
      DO i = 1, nat_
         !
         CALL iotk_scan_empty( iunpun, &
                               "ATOM" // TRIM( iotk_index(i) ), ATTR=attr, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         CALL iotk_scan_attr( attr, "INDEX",  ityp_(i), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_attr( attr, "tau",    tau_(:,i), IERR=ierr )
         IF (ierr/=0) RETURN
         CALL iotk_scan_attr( attr, "if_pos", if_pos_(:,i), IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDDO
      !
      CALL iotk_scan_end( iunpun, "IONS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT(nsp) )         nsp    = nsp_
      IF ( PRESENT(nat) )         nat    = nat_
      IF ( PRESENT(atm) )         atm(1:nsp_)    = atm_
      IF ( PRESENT(amass) )       amass(1:nsp_)  = amass_
      IF ( PRESENT(amass_units) ) amass_units    = TRIM(amass_units_)
      IF ( PRESENT(psfile) )      psfile(1:nsp_) = psfile_(1:nsp_)
      IF ( PRESENT(ityp) )        ityp(1:nat_)   = ityp_
      IF ( PRESENT(tau_units) )   tau_units      = TRIM(tau_units_)
      IF ( PRESENT(tau) )         tau(1:3, 1:nat_)    = tau_
      IF ( PRESENT(if_pos) )      if_pos(1:3, 1:nat_) = if_pos_
      !
      DEALLOCATE( atm_ )
      DEALLOCATE( amass_ )
      DEALLOCATE( psfile_ )
      DEALLOCATE( ityp_ )
      DEALLOCATE( tau_ )
      DEALLOCATE( if_pos_ )
      ! 
    END SUBROUTINE qexml_read_ions


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_symmetry( nsym, invsym, trasl, s, sname, s_units, t_rev, &
                                    irt, nat, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,          OPTIONAL, INTENT(OUT) :: nsym
      LOGICAL,          OPTIONAL, INTENT(OUT) :: invsym
      INTEGER,          OPTIONAL, INTENT(OUT) :: s(:,:,:)
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: trasl(:,:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: sname(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: s_units
      INTEGER,          OPTIONAL, INTENT(OUT) :: t_rev(:)
      INTEGER,          OPTIONAL, INTENT(OUT) :: irt(:,:), nat
      INTEGER,                    INTENT(OUT) :: ierr
      !
      INTEGER              :: nsym_
      CHARACTER(256)       :: sname_(48), s_units_
      LOGICAL              :: invsym_
      INTEGER              :: s_(3,3,48)
      REAL(dbl)            :: trasl_(3,48)
      INTEGER              :: t_rev_(48)
      INTEGER              :: nat_
      INTEGER, ALLOCATABLE :: irt_(:,:)
      !      
      INTEGER             :: i

      !
      ierr=0
      !
      !
      CALL iotk_scan_begin( iunpun, "SYMMETRIES", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NUMBER_OF_SYMMETRIES", nsym_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "INVERSION_SYMMETRY", invsym_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NUMBER_OF_ATOMS", nat_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      ALLOCATE( irt_(48, nat_) )
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_SYMMETRIES", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", s_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      ! 
      DO i = 1, nsym_
          !
          CALL iotk_scan_begin( iunpun, "SYMM"//TRIM( iotk_index( i ) ), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_empty( iunpun, "INFO", ATTR=attr, IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_attr( attr, "NAME", sname_(i), IERR=ierr )
          IF (ierr/=0) RETURN
          CALL iotk_scan_attr( attr, "T_REV", t_rev_(i), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_dat( iunpun, "ROTATION", s_(1:3,1:3,i), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_dat( iunpun, "FRACTIONAL_TRANSLATION", trasl_(1:3,i), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_dat( iunpun, "EQUIVALENT_IONS", irt_(i,1:nat_), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_end( iunpun, "SYMM"//TRIM( iotk_index( i ) ), IERR=ierr )
          IF (ierr/=0) RETURN
          !
      ENDDO
      !
      CALL iotk_scan_end( iunpun, "SYMMETRIES", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT(nsym) )        nsym          = nsym_
      IF ( PRESENT(invsym) )      invsym        = invsym_
      IF ( PRESENT(nat) )         nat           = nat_
      IF ( PRESENT(trasl) )       trasl(1:3, 1:nsym_)   = trasl_(1:3, 1:nsym_)
      IF ( PRESENT(s) )           s(1:3, 1:3, 1:nsym_)  = s_(1:3, 1:3, 1:nsym_)
      IF ( PRESENT(irt) )         irt(1:nsym_, 1:nat_)  = irt_(1:nsym_, 1:nat_)
      IF ( PRESENT(sname) )  THEN     
          DO i = 1, nsym_
                                  sname( i )            = TRIM( sname_( i ) )
          ENDDO
      ENDIF       
      IF ( PRESENT(s_units) )     s_units               = TRIM( s_units_ )
      IF ( PRESENT(t_rev) )       t_rev( 1:nsym_ )      = t_rev_( 1:nsym_ )
      !
      DEALLOCATE( irt_ )
      !
    END SUBROUTINE qexml_read_symmetry


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_planewaves( ecutwfc, ecutrho, npwx, gamma_only, nr1, nr2,  &
                                      nr3, ngm, nr1s, nr2s, nr3s, ngms, nr1b, &
                                      nr2b, nr3b, igv, cutoff_units, ierr )
      !------------------------------------------------------------------------
      !
      !
      INTEGER,      OPTIONAL, INTENT(OUT) :: npwx, nr1, nr2, nr3, ngm, &
                                             nr1s, nr2s, nr3s, ngms, nr1b, nr2b, nr3b
      INTEGER,      OPTIONAL, INTENT(OUT) :: igv(:,:)
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: ecutwfc, ecutrho
      LOGICAL,      OPTIONAL, INTENT(OUT) :: gamma_only
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: cutoff_units
      INTEGER,                INTENT(OUT) :: ierr
      !
      INTEGER        :: npwx_, nr1_, nr2_, nr3_, ngm_, &
                        nr1s_, nr2s_, nr3s_, ngms_, nr1b_, nr2b_, nr3b_
      REAL(dbl)      :: ecutwfc_, ecutrho_
      CHARACTER(256) :: cutoff_units_
      LOGICAL        :: gamma_only_
      !
      
      ierr = 0
      !
      CALL iotk_scan_begin( iunpun, "PLANE_WAVES", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_CUTOFF", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "UNITS", cutoff_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "WFC_CUTOFF", ecutwfc_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "RHO_CUTOFF", ecutrho_ , IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "MAX_NUMBER_OF_GK-VECTORS", npwx_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "GAMMA_ONLY", gamma_only_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "FFT_GRID", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "nr1", nr1_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr2", nr2_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr3", nr3_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "GVECT_NUMBER", ngm_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "SMOOTH_FFT_GRID", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "nr1s", nr1s_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr2s", nr2s_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr3s", nr3s_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "SMOOTH_GVECT_NUMBER", ngms_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( igv ) ) THEN
          !
          CALL iotk_scan_begin( iunpun, "G-VECTORS", IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_dat( iunpun, "g", igv(1:3,1:ngm_), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_end( iunpun, "G-VECTORS", IERR=ierr )          
          IF (ierr/=0) RETURN
          !
      ENDIF
      !
      !
      CALL iotk_scan_empty( iunpun, "SMALLBOX_FFT_GRID", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "nr1b", nr1b_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr2b", nr2b_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr3b", nr3b_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunpun, "PLANE_WAVES", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( ecutwfc ) )           ecutwfc      = ecutwfc_
      IF ( PRESENT( ecutrho ) )           ecutrho      = ecutrho_
      IF ( PRESENT( npwx ) )              npwx         = npwx_
      IF ( PRESENT( gamma_only ) )        gamma_only   = gamma_only_
      IF ( PRESENT( nr1 ) )               nr1          = nr1_
      IF ( PRESENT( nr2 ) )               nr2          = nr2_
      IF ( PRESENT( nr3 ) )               nr3          = nr3_
      IF ( PRESENT( ngm ) )               ngm          = ngm_
      IF ( PRESENT( nr1s ) )              nr1s         = nr1s_
      IF ( PRESENT( nr2s ) )              nr2s         = nr2s_
      IF ( PRESENT( nr3s ) )              nr3s         = nr3s_
      IF ( PRESENT( ngms ) )              ngms         = ngms_
      IF ( PRESENT( nr1b ) )              nr1b         = nr1b_
      IF ( PRESENT( nr2b ) )              nr2b         = nr2b_
      IF ( PRESENT( nr3b ) )              nr3b         = nr3b_
      IF ( PRESENT( cutoff_units ) )      cutoff_units = TRIM( cutoff_units_ )
      !
    END SUBROUTINE qexml_read_planewaves
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_gk( ik, npwk, npwkx, index, igk, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                INTENT(IN)  :: ik
      INTEGER,      OPTIONAL, INTENT(OUT) :: npwk, npwkx
      INTEGER,      OPTIONAL, INTENT(OUT) :: igk(:,:), index(:)
      INTEGER,                INTENT(OUT) :: ierr
      !
      CHARACTER(256) :: filename
      INTEGER :: iunaux
      INTEGER :: npwk_, npwkx_
      !

      ierr = 0
      !
      CALL iotk_free_unit( iunaux )
      filename = wfc_filename( datadir, 'gkvectors', ik )
      !
      CALL iotk_open_read ( iunaux, FILE = TRIM(filename), IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      CALL iotk_scan_dat( iunaux, 'NUMBER_OF_GK-VECTORS', npwk_, IERR=ierr)
      IF (ierr/=0)  RETURN
      !
      CALL iotk_scan_dat( iunaux, 'MAX_NUMBER_OF_GK-VECTORS', npwkx_, IERR=ierr)
      IF (ierr/=0)  RETURN
      !
      IF ( PRESENT( index ) ) THEN
          !
          CALL iotk_scan_dat( iunaux, 'INDEX', index(1:npwk_), IERR=ierr)
          IF (ierr/=0)  RETURN
          !
      ENDIF
      !
      IF ( PRESENT( igk ) ) THEN
          !
          CALL iotk_scan_dat( iunaux, 'GRID', igk(1:3, 1:npwk_), IERR=ierr)
          IF (ierr/=0)  RETURN
          !
      ENDIF
      !
      CALL iotk_close_read ( iunaux, IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      !
      IF ( PRESENT( npwk ) )       npwk  = npwk_
      IF ( PRESENT( npwkx ) )      npwkx = npwkx_
      !
    END SUBROUTINE qexml_read_gk


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_spin( lsda, noncolin, npol, lspinorb, ierr )
      !------------------------------------------------------------------------
      !
      LOGICAL, OPTIONAL, INTENT(OUT) :: lsda, noncolin, lspinorb
      INTEGER, OPTIONAL, INTENT(OUT) :: npol
      INTEGER,           INTENT(OUT) :: ierr
      !
      LOGICAL   :: lsda_, noncolin_, lspinorb_
      INTEGER   :: npol_
      ! 
     
      ierr = 0
      !
      CALL iotk_scan_begin( iunpun, "SPIN", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "LSDA", lsda_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NON-COLINEAR_CALCULATION", noncolin_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      npol_ = 1
      !
      IF ( noncolin_ ) THEN
          !
          CALL iotk_scan_dat( iunpun, "SPINOR_DIM", npol_, IERR=ierr )
          IF (ierr/=0) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_dat( iunpun, "SPIN-ORBIT_CALCULATION", lspinorb_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunpun, "SPIN", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( lsda ) )       lsda      = lsda_
      IF ( PRESENT( noncolin ) )   noncolin  = noncolin_
      IF ( PRESENT( npol ) )       npol      = npol_
      IF ( PRESENT( lspinorb ) )   lspinorb  = lspinorb_
      !

    END SUBROUTINE qexml_read_spin


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_xc( dft, lda_plus_u,  &
                              Hubbard_lmax, Hubbard_l, nsp, Hubbard_U, Hubbard_alpha, ierr )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: dft
      LOGICAL,          OPTIONAL, INTENT(OUT) :: lda_plus_u
      INTEGER,          OPTIONAL, INTENT(OUT) :: Hubbard_lmax
      INTEGER,          OPTIONAL, INTENT(OUT) :: Hubbard_l(:)
      INTEGER,          OPTIONAL, INTENT(OUT) :: nsp
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: Hubbard_U(:), Hubbard_alpha(:)
      INTEGER,                    INTENT(OUT) :: ierr
      !
      CHARACTER(256) :: dft_
      LOGICAL        :: lda_plus_u_
      INTEGER        :: Hubbard_lmax_, nsp_
      INTEGER,    ALLOCATABLE :: Hubbard_l_(:)
      REAL(dbl),  ALLOCATABLE :: Hubbard_U_(:)
      REAL(dbl),  ALLOCATABLE :: Hubbard_alpha_(:)
      ! 
      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunpun, "EXCHANGE_CORRELATION", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunpun, "DFT", dft_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunpun, "LDA_PLUS_U_CALCULATION", lda_plus_u_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      IF ( lda_plus_u_ ) THEN
         !
         CALL iotk_scan_dat( iunpun, "NUMBER_OF_SPECIES", nsp_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_dat( iunpun, "HUBBARD_LMAX", Hubbard_lmax_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         ALLOCATE( Hubbard_l_(1:Hubbard_lmax_) )
         ALLOCATE( Hubbard_U_(nsp_) )
         ALLOCATE( Hubbard_alpha_(nsp_) )
         !
         CALL iotk_scan_dat( iunpun, "HUBBARD_L", Hubbard_l_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_dat( iunpun, "HUBBARD_U", Hubbard_U_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_dat( iunpun, "HUBBARD_ALPHA", Hubbard_alpha_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
      ENDIF
      !
      CALL iotk_scan_end( iunpun, "EXCHANGE_CORRELATION", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( dft ) )           dft           = dft_
      IF ( PRESENT( lda_plus_u ) )    lda_plus_u    = lda_plus_u_
      !
      IF ( lda_plus_u_ )  THEN
         !
         IF ( PRESENT( nsp ) )             nsp                   = nsp_
         IF ( PRESENT( Hubbard_lmax ) )    Hubbard_lmax          = Hubbard_lmax_
         IF ( PRESENT( Hubbard_l ) )       Hubbard_l(1:Hubbard_lmax_)   = Hubbard_l_(:)
         IF ( PRESENT( Hubbard_U ) )       Hubbard_U(1:nsp_)     = Hubbard_U_(1:nsp_)
         IF ( PRESENT( Hubbard_alpha ) )   Hubbard_alpha(1:nsp_) = Hubbard_alpha_(1:nsp_)
         !
         DEALLOCATE( Hubbard_l_ )
         DEALLOCATE( Hubbard_U_ )
         DEALLOCATE( Hubbard_alpha_ )
         !
      ENDIF 

    END SUBROUTINE qexml_read_xc


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_occ( lgauss, ngauss, degauss, degauss_units, ltetra, ntetra, &
                               tetra, tfixed_occ, input_occ, ierr )
      !------------------------------------------------------------------------
      !
      LOGICAL,      OPTIONAL, INTENT(OUT) :: lgauss, ltetra, tfixed_occ
      INTEGER,      OPTIONAL, INTENT(OUT) :: ngauss, ntetra
      INTEGER,      OPTIONAL, INTENT(OUT) :: tetra(:,:)
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: degauss, input_occ(:,:)
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: degauss_units
      INTEGER,                INTENT(OUT) :: ierr
      !
      LOGICAL        :: lgauss_, ltetra_, tfixed_occ_
      INTEGER        :: ngauss_, ntetra_
      REAL(dbl)      :: degauss_
      CHARACTER(256) :: degauss_units_
      INTEGER,  ALLOCATABLE :: tetra_(:,:)
      INTEGER :: i
      LOGICAL :: lfound
      !
      ierr = 0 
      !
      CALL iotk_scan_begin( iunpun, "OCCUPATIONS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunpun, "SMEARING_METHOD", lgauss_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( lgauss_ ) THEN
         !
         CALL iotk_scan_dat( iunpun, "SMEARING_TYPE", ngauss_, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         CALL iotk_scan_dat( iunpun, "SMEARING_PARAMETER", degauss_ , ATTR=attr, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         CALL iotk_scan_attr( ATTR, "UNITS", degauss_units_ , IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ENDIF
      !
      CALL iotk_scan_dat( iunpun, "TETRAHEDRON_METHOD", ltetra_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( ltetra_ ) THEN
         !
         CALL iotk_scan_dat( iunpun, "NUMBER_OF_TETRAHEDRA", ntetra_, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         ALLOCATE( tetra_(4, ntetra_) )
         !
         DO i = 1, ntetra_
            !
            CALL iotk_scan_dat( iunpun, "TETRAHEDRON"//iotk_index(i), tetra_(1:4,i), IERR=ierr )
            IF (ierr/=0) RETURN
            !
         ENDDO
         !
      ENDIF
      !
      CALL iotk_scan_dat( iunpun, "FIXED_OCCUPATIONS", tfixed_occ_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( tfixed_occ_  .AND. PRESENT( input_occ ) ) THEN
         !
         CALL iotk_scan_dat( iunpun, "INPUT_OCC_UP", input_occ(:,1), IERR=ierr )
         IF (ierr/=0) RETURN
         !
         IF ( SIZE(input_occ, 2) >= 2  ) THEN
            !
            CALL iotk_scan_dat( iunpun, "INPUT_OCC_DOWN", input_occ(:,2), &
                                FOUND=lfound, IERR=ierr )
            IF (ierr/=0) RETURN
            !
         ENDIF
         !
      ENDIF
      !
      CALL iotk_scan_end( iunpun, "OCCUPATIONS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( lgauss ))           lgauss     = lgauss_
      IF ( PRESENT( ltetra ))           ltetra     = ltetra_
      IF ( PRESENT( tfixed_occ ))       tfixed_occ = tfixed_occ_
      IF ( PRESENT( ngauss ))           ngauss     = ngauss_
      IF ( PRESENT( ntetra ))           ntetra     = ntetra_
      IF ( PRESENT( degauss ))          degauss    = degauss
      IF ( PRESENT( degauss_units ))    degauss_units  = TRIM(degauss_units_)
      !
      IF ( ltetra_ ) THEN
         !
         IF ( PRESENT( tetra ) )         tetra(1:4, 1:ntetra_)  = tetra_
         !
         DEALLOCATE( tetra_ )
         !
      ENDIF

    END SUBROUTINE qexml_read_occ


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_bz( num_k_points, xk, wk, k1, k2, k3, nk1, nk2, nk3, k_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,       OPTIONAL, INTENT(OUT) :: num_k_points, k1, k2, k3, nk1, nk2, nk3
      REAL(dbl),     OPTIONAL, INTENT(OUT) :: xk(:,:), wk(:)
      CHARACTER(*),  OPTIONAL, INTENT(OUT) :: k_units
      INTEGER,                 INTENT(OUT) :: ierr
      !
      INTEGER                :: num_k_points_, k1_, k2_, k3_, nk1_, nk2_, nk3_
      CHARACTER(256)         :: k_units_
      REAL(dbl), ALLOCATABLE :: xk_(:,:), wk_(:)
      !
      INTEGER :: ik
      !

      ierr = 0
      !
      CALL iotk_scan_begin( iunpun, "BRILLOUIN_ZONE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NUMBER_OF_K-POINTS", num_k_points_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_K-POINTS", ATTR=attr, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "UNITS", k_units_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_empty( iunpun, "MONKHORST_PACK_GRID", ATTR=attr, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "nk1", nk1_, IERR=ierr  )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "nk2", nk2_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "nk3", nk3_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      CALL iotk_scan_empty( iunpun, "MONKHORST_PACK_OFFSET", ATTR=attr, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_attr( attr, "k1", k1_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "k2", k2_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "k3", k3_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      ALLOCATE( xk_( 3, num_k_points_ ) )
      ALLOCATE( wk_(    num_k_points_ ) )
      !
      DO ik = 1, num_k_points_
         !
         CALL iotk_scan_empty( iunpun, "K-POINT" // TRIM( iotk_index(ik) ), ATTR=attr, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_attr( attr, "XYZ", xk_(:,ik), IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !            
         CALL iotk_scan_attr( attr, "WEIGHT", wk_(ik), IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
      END DO
      !
      CALL iotk_scan_end( iunpun, "BRILLOUIN_ZONE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( num_k_points ) )       num_k_points  = num_k_points_
      IF ( PRESENT( nk1 ) )                nk1           = nk1_
      IF ( PRESENT( nk2 ) )                nk2           = nk2_
      IF ( PRESENT( nk3 ) )                nk3           = nk3_
      IF ( PRESENT( k1 ) )                 k1            =  k1_
      IF ( PRESENT( k2 ) )                 k2            =  k2_
      IF ( PRESENT( k3 ) )                 k3            =  k3_
      IF ( PRESENT( k_units ) )            k_units       =  TRIM(k_units_)
      IF ( PRESENT( xk ) )                 xk(1:3,1:num_k_points_) = xk_(:,:)
      IF ( PRESENT( wk ) )                 wk(1:num_k_points_)     = wk_(:)
      !
      DEALLOCATE( xk_ )
      DEALLOCATE( wk_ )
      !
    END SUBROUTINE qexml_read_bz


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_phonon( modenum, xqq, q_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,       OPTIONAL, INTENT(OUT) :: modenum
      REAL(dbl),     OPTIONAL, INTENT(OUT) :: xqq(:)
      CHARACTER(*),  OPTIONAL, INTENT(OUT) :: q_units
      INTEGER,                 INTENT(OUT) :: ierr
      !
      INTEGER         :: modenum_
      CHARACTER(256)  :: q_units_
      !
     
      ierr = 0
      !
      CALL iotk_scan_begin( iunpun, "PHONON", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunpun, "NUMBER_OF_MODES", modenum_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_Q-POINT", attr, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "UNITS", q_units_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      IF ( PRESENT (xqq) ) THEN
         !
         CALL iotk_scan_dat( iunpun, "Q-POINT", xqq(:), IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
      ENDIF
      !
      CALL iotk_scan_end( iunpun, "PHONON", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT (modenum) )      modenum = modenum_
      IF ( PRESENT (q_units) )      q_units = TRIM(q_units_)
      !
    END SUBROUTINE qexml_read_phonon
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_bands_info( nbnd, num_k_points, nspin, noncolin, natomwfc, & 
                                      nelec, ef, energy_units, k_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,      OPTIONAL, INTENT(OUT) :: nbnd, num_k_points, nspin, natomwfc
      LOGICAL,      OPTIONAL, INTENT(OUT) :: noncolin
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: ef, nelec
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: energy_units, k_units
      INTEGER,                INTENT(OUT) :: ierr
      !
      INTEGER        :: nbnd_, num_k_points_, nspin_, natomwfc_
      LOGICAL        :: noncolin_
      REAL(dbl)      :: ef_, nelec_
      CHARACTER(256) :: energy_units_, k_units_

      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunpun, "BAND_STRUCTURE_INFO", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "NUMBER_OF_BANDS", nbnd_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "NUMBER_OF_K-POINTS", num_k_points_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "NUMBER_OF_SPIN_COMPONENTS", nspin_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "NON-COLINEAR_CALCULATION", noncolin_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "NUMBER_OF_ATOMIC_WFC", natomwfc_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "NUMBER_OF_ELECTRONS", nelec_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_K-POINTS", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr ( attr,   "UNITS", k_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunpun, "UNITS_FOR_ENERGIES", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr ( attr,   "UNITS", energy_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunpun, "FERMI_ENERGY", ef_ , IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunpun, "BAND_STRUCTURE_INFO", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( nbnd ) )             nbnd           = nbnd_
      IF ( PRESENT( num_k_points ) )     num_k_points   = num_k_points_
      IF ( PRESENT( nspin ) )            nspin          = nspin_
      IF ( PRESENT( noncolin ) )         noncolin       = noncolin_
      IF ( PRESENT( natomwfc ) )         natomwfc       = natomwfc_
      IF ( PRESENT( nelec ) )            nelec          = nelec_
      IF ( PRESENT( ef ) )               ef             = ef_
      IF ( PRESENT( energy_units ) )     energy_units   = TRIM( energy_units_ )
      IF ( PRESENT( k_units ) )          k_units        = TRIM( k_units_ )
      !
    END SUBROUTINE qexml_read_bands_info
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_bands( ik, ispin, nbnd, eig, energy_units, occ, &
                                     ef, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                INTENT(IN)  :: ik
      INTEGER,      OPTIONAL, INTENT(IN)  :: ispin
      INTEGER,      OPTIONAL, INTENT(OUT) :: nbnd
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: eig(:)
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: energy_units
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: occ(:)
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: ef
      INTEGER,                INTENT(OUT) :: ierr
      !
      INTEGER        :: iunaux
      INTEGER        :: nbnd_
      CHARACTER(256) :: energy_units_
      CHARACTER(256) :: filename
      REAL(dbl)      :: ef_
      REAL(dbl), ALLOCATABLE :: occ_(:), eig_(:)
      !
      
      ierr = 0
      !
      !
      ! read the main data
      !
      CALL iotk_free_unit( iunaux )
      !
      IF ( PRESENT( ispin) ) THEN
         !
         filename = TRIM( wfc_filename( datadir, 'eigenval', ik, ispin, EXTENSION="xml" ) ) 
         !
      ELSE
         !
         filename = TRIM( wfc_filename( datadir, 'eigenval', ik, EXTENSION="xml" ) ) 
         !
      ENDIF
      !
      !
      CALL iotk_open_read ( iunaux, FILE = TRIM(filename), IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      CALL iotk_scan_empty( iunaux, "INFO", ATTR = attr, IERR=ierr )
      IF (ierr/=0)  RETURN
      CALL iotk_scan_attr( attr, "nbnd", nbnd_, IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      CALL iotk_scan_empty( iunaux, "UNITS_FOR_ENERGIES", ATTR = attr, IERR=ierr )
      IF (ierr/=0)  RETURN
      CALL iotk_scan_attr( attr, "UNITS", energy_units_, IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      CALL iotk_scan_dat( iunaux, "FERMI_ENERGY", ef_, IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      !
      ! Allocations
      !
      ALLOCATE(  eig_ ( nbnd_ ) )
      ALLOCATE(  occ_ ( nbnd_ ) )
      !
      CALL iotk_scan_dat( iunaux, "EIGENVALUES", eig_(:), IERR=ierr)
      IF (ierr/=0)  RETURN
      !
      CALL iotk_scan_dat( iunaux, "OCCUPATIONS", occ_(:), IERR=ierr)
      IF (ierr/=0)  RETURN
      !
      CALL iotk_close_read ( iunaux, IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      !
      IF ( PRESENT( nbnd ) )             nbnd             = nbnd_
      IF ( PRESENT( energy_units ) )     energy_units     = TRIM( energy_units_ )
      IF ( PRESENT( ef ) )                         ef     = ef_
      IF ( PRESENT( occ ) )              occ  (1:nbnd_ )  = occ_(:)
      IF ( PRESENT( eig ) )              eig  (1:nbnd_ )  = eig_(:)
      !
      DEALLOCATE( occ_ )
      DEALLOCATE( eig_ )
      !
    END SUBROUTINE qexml_read_bands


    !------------------------------------------------------------------------
    SUBROUTINE qexml_read_wfc( ibnds, ibnde, ik, ispin, ipol, igk, ngw, igwx, &
                               wf, wf_kindip, ierr )
      !------------------------------------------------------------------------
      !
      ! read wfc from IBNDS to IBNDE, for kpt IK and spin ISPIN
      ! WF is the wfc on itsproper k+g grid, while WF_KINDIP is the same wfc
      ! but on a truncated rho grid (k-point indipendent)
      !
      INTEGER,                 INTENT(IN)  :: ibnds, ibnde, ik
      INTEGER,       OPTIONAL, INTENT(IN)  :: ispin, ipol
      INTEGER,       OPTIONAL, INTENT(IN)  :: igk(:)
      INTEGER,       OPTIONAL, INTENT(OUT) :: ngw, igwx
      COMPLEX(dbl),  OPTIONAL, INTENT(OUT) :: wf(:,:), wf_kindip(:,:)
      INTEGER,                 INTENT(OUT) :: ierr
      !
      INTEGER :: iunaux
      INTEGER :: ngw_, igwx_, ig, ib, lindex
      COMPLEX(dbl),  ALLOCATABLE :: wf_(:)
      CHARACTER(256)             :: filename

      ierr = 0
      !
      !
      ! few check
      !
      IF ( PRESENT( ispin ) .AND. PRESENT( ipol )  ) THEN
         !
         ierr = 1
         RETURN
         !
      ENDIF
      !
      !
      ! read the main data
      !
      CALL iotk_free_unit( iunaux )
      !
      IF ( PRESENT( ispin ) ) THEN
         !
         filename = TRIM( wfc_filename( datadir, 'evc', ik, ispin ) ) 
         !
      ELSEIF ( PRESENT( ipol )  ) THEN
         !
         filename = TRIM( wfc_filename( datadir, 'evc', ik, ipol ) ) 
         !
      ELSE
         !
         filename = TRIM( wfc_filename( datadir, 'evc', ik ) ) 
         !
      ENDIF
      !
      CALL iotk_open_read ( iunaux, FILE = TRIM(filename), IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      !
      CALL iotk_scan_empty( iunaux, "INFO", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "ngw",  ngw_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "igwx", igwx_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( wf )  )  THEN
          !
          lindex = 0
          !
          DO ib = ibnds, ibnde
              !
              lindex = lindex + 1
              !
              CALL iotk_scan_dat( iunaux, "evc"//TRIM(iotk_index(ib)), &
                                  wf( 1:igwx_, lindex ), IERR=ierr )
              IF (ierr/=0) RETURN
              !
          ENDDO
          !
      ENDIF
      !
      IF ( PRESENT( wf_kindip )  )  THEN
          !
          ALLOCATE( wf_(igwx_ ), STAT=ierr )
          IF (ierr/=0) RETURN
          !
          IF ( .NOT. PRESENT( igk ) ) THEN
              ierr = 3
              RETURN
          ENDIF
          !
          IF ( MAXVAL( igk( 1: igwx_ ) ) > SIZE( wf_kindip, 1)  ) THEN
              ierr = 4
              RETURN
          ENDIF
          !
          !
          lindex = 0
          !
          DO ib = ibnds, ibnde
              !
              lindex = lindex + 1
              !
              CALL iotk_scan_dat( iunaux, "evc"//TRIM(iotk_index( ib ) ), &
                                           wf_(1:igwx_), IERR=ierr )
              IF (ierr/=0) RETURN
              !
              ! use the igk map to do the transformation
              !
              wf_kindip(:, lindex) = 0.0_dbl
              !
              DO ig = 1, igwx_
                  !
                  wf_kindip( igk( ig ), lindex ) = wf_( ig )
                  !
              ENDDO
              !
          ENDDO
          !
          DEALLOCATE( wf_, STAT=ierr )
          IF (ierr/=0) RETURN
          !
      ENDIF
      !
      CALL iotk_close_read ( iunaux, IERR=ierr )
      IF (ierr/=0)  RETURN
      !
      !
      IF ( PRESENT( ngw ) )     ngw    = ngw_
      IF ( PRESENT( igwx ) )    igwx   = igwx_
      !
    END SUBROUTINE qexml_read_wfc
    !
    !
END MODULE qexml_module
