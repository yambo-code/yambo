!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
!----------------------------------------------------------------------------
MODULE qexsd_module
  !----------------------------------------------------------------------------
  !
  ! This module contains some common subroutines used to read and write
  ! the QEXSD format produced by Quantum-ESPRESSO package.
  !
  ! Written by Andrea Ferretti (2017) taking parts from 
  !    qexsd.f90 
  !    qexml.f90
  !    qexsd_reader_module.f90
  !
  ! from the QE package (involving contributions from P. Delugas et al).
  !
  USE iotk_module
  IMPLICIT NONE
  !
  PRIVATE
  SAVE
  !
  ! definitions for the fmt
  !
  CHARACTER(5), PARAMETER :: fmt_name = "QEXSD"
  CHARACTER(5), PARAMETER :: fmt_version = "1.0.0"
  !
  ! some default for kinds
  !
  INTEGER,   PARAMETER :: dbl = SELECTED_REAL_KIND( 14, 200 )
  REAL(dbl), PARAMETER :: e2 = 2.0_dbl
  !
  CHARACTER(LEN=2), DIMENSION(2) :: updw = (/ 'up', 'dw' /)
  !
  ! internal data to be set
  !
  CHARACTER(256)   :: datadir_in, datadir_out
  INTEGER          :: iunit, ounit
  !
  ! vars to manage back compatibility
  !
  CHARACTER(10)    :: qexsd_current_version = " "
  CHARACTER(10)    :: qexsd_default_version = TRIM( fmt_version  )
  LOGICAL          :: qexsd_current_version_init = .FALSE.
  !
  CHARACTER(10)    :: qe_current_version = " "
  !
  CHARACTER(iotk_attlenx) :: attr
  
  !
  ! interfaces
  !
  INTERFACE qexsd_scan_logical
    MODULE PROCEDURE qexsd_scan_logical0
  END INTERFACE

  !
  ! public scope
  !
  PUBLIC :: qexsd_current_version, qexsd_default_version
  PUBLIC :: qexsd_current_version_init
  !
  PUBLIC :: qe_current_version
  !
  PUBLIC :: qexsd_init_schema, qexsd_openfile, qexsd_closefile
  PUBLIC :: qexsd_open_output, qexsd_close_output
  !
  PUBLIC :: qexsd_read_header, qexsd_read_cell, qexsd_read_ions, &
            qexsd_read_symmetry, qexsd_read_planewaves, qexsd_read_spin, &
            qexsd_read_hubbard, qexsd_read_bands_info, &
            qexsd_read_band_structure, qexsd_read_xc, qexsd_read_exx, &
            qexsd_read_gk, qexsd_read_wfc, qexsd_igk_map, qexsd_fft2igv_map

CONTAINS

!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_init_schema( unit_in, unit_out, dir, dir_in, dir_out )
      !------------------------------------------------------------------------
      !    
      ! just init module data
      !    
      IMPLICIT NONE 
      INTEGER,                INTENT(in) :: unit_in
      INTEGER,      OPTIONAL, INTENT(in) :: unit_out
      CHARACTER(*), OPTIONAL, INTENT(IN) :: dir
      CHARACTER(*), OPTIONAL, INTENT(IN) :: dir_in, dir_out
      !    
      iunit       = unit_in
      ounit       = unit_in
      IF ( present( unit_out ) ) ounit  = unit_out
      !
      datadir_in  = "./" 
      datadir_out = "./" 
      !
      IF ( PRESENT( dir ) ) THEN
          datadir_in  = TRIM(dir)
          datadir_out = TRIM(dir)
      ENDIF
      !
      IF ( PRESENT( dir_in ) ) THEN
          datadir_in  = TRIM(dir_in)
      ENDIF
      !
      IF ( PRESENT( dir_out ) ) THEN
          datadir_out  = TRIM(dir_out)
      ENDIF
      !
      !    
    END SUBROUTINE qexsd_init_schema
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_openfile( filename, action, binary, ierr)
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
          CALL iotk_open_read ( iunit, FILE = TRIM(filename), IERR=ierr )
          IF ( ierr/=0 ) RETURN
          !
          !CALL qexsd_read_header( FORMAT_VERSION=qexsd_current_version, IERR=ierr )
          !IF ( ierr/=0 ) CALL errore( 'qexsd_read_header', 'reading QEXSD version', 1 )
          !IF ( ierr/=0 ) qexsd_current_version = TRIM( qexsd_default_version )
          !
          !CALL qexsd_read_header( CREATOR_VERSION=qe_current_version, IERR=ierr )
          !IF ( ierr/=0 ) CALL errore( 'qexsd_read_header', 'reading QE version', 1 )
          !IF ( ierr/=0 ) pwscf_current_version = TRIM( pwscf_default_version )
          !
          CALL qexsd_read_header( FORMAT_VERSION=qexsd_current_version, &
                                  CREATOR_VERSION=qe_current_version, IERR=ierr )
          IF ( ierr/=0 ) CALL errore( 'qexsd_read_header', 'reading QE/QEXSD version', 1 )
          !
      CASE DEFAULT
          ierr = 1
      END SELECT
      !
    END SUBROUTINE qexsd_openfile
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_open_output(ierr)
      !------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER :: ierr
      !
      call iotk_scan_begin(iunit,"output",IERR=ierr)
      if (ierr/=0) return
      !
    END SUBROUTINE qexsd_open_output
    !  
    !  
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_close_output(ierr)
      !------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER :: ierr
      !
      call iotk_scan_end(iunit,"output",IERR=ierr)
      if (ierr/=0) return
      !
    END SUBROUTINE qexsd_close_output
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_closefile( action, ierr)
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
          CALL iotk_close_read( iunit, IERR=ierr )
          !
      CASE DEFAULT
          ierr = 2
      END SELECT
      !
    END SUBROUTINE qexsd_closefile
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_scan_logical0( iun, label, lval, attr, lfound, ierr)
      !------------------------------------------------------------------------
      !
      INTEGER      :: iun
      CHARACTER(*) :: label
      LOGICAL      :: lval
      CHARACTER(iotk_attlenx), OPTIONAL :: attr
      LOGICAL,                 OPTIONAL :: lfound
      INTEGER      :: ierr
      !
      CHARACTER(iotk_attlenx) :: str
      !
      ierr=0
      IF (PRESENT(attr)) THEN
        IF (PRESENT(lfound)) THEN
          CALL iotk_scan_dat(iun,label,str,ATTR=attr,FOUND=lfound,IERR=ierr)
        ELSE
          CALL iotk_scan_dat(iun,label,str,ATTR=attr,IERR=ierr)
        ENDIF
      ELSE
        IF (PRESENT(lfound)) THEN
          CALL iotk_scan_dat(iun,label,str,FOUND=lfound,IERR=ierr)
        ELSE
          CALL iotk_scan_dat(iun,label,str,IERR=ierr)
        ENDIF
      ENDIF
      IF (ierr/=0) RETURN
      !
      lval=.false.
      IF (TRIM(str)=="true" .or. TRIM(str)=="True") THEN
         lval=.true.
      ELSE IF (TRIM(str)=="false".or. TRIM(str)=="False") THEN
         lval=.false.
      ELSE
         ierr=1001
         RETURN
      ENDIF
      !
    END SUBROUTINE qexsd_scan_logical0

!
!-------------------------------------------
! ... basic (private) subroutines
!-------------------------------------------
!
    !------------------------------------------------------------------------
    FUNCTION int_to_char( i )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: i
      CHARACTER (LEN=6)   :: int_to_char
      !
      !
      IF ( i < 10 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I1)" ) i
         !
      ELSE IF ( i < 100 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I2)" ) i
         !
       ELSE IF ( i < 1000 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I3)" ) i
         !
       ELSE IF ( i < 10000 ) THEN
         !
         WRITE( UNIT = int_to_char , FMT = "(I4)" ) i
         !
       ELSE
         !
       WRITE( UNIT = int_to_char , FMT = "(I5)" ) i
       !
      END IF
      !
    END FUNCTION int_to_char
    !
    !
    !--------------------------------------------------------------------------
    SUBROUTINE qexsd_basename( str, extension )
      !--------------------------------------------------------------------------
      !
      ! perform the basename operation on the string str, eliminating
      ! any ending (rightmost) occurrence of extension
      !
      CHARACTER(*),  INTENT(INOUT) :: str
      CHARACTER(*),  INTENT(IN)    :: extension
      !
      INTEGER :: ind, strlen, extlen, i
      !
      IF( LEN_TRIM(extension) == 0  .OR. LEN_TRIM(str) == 0 ) RETURN
      !
      strlen = LEN_TRIM( str )
      extlen = LEN_TRIM( extension )
      ind    = INDEX( str, TRIM(extension), BACK=.TRUE. )
      !
      IF ( ind <= 0 .OR. ind > strlen ) RETURN
      !
      ! we want to cut only the last part of the name
      ! any intermediate matching is rejected
      !
      IF ( strlen -ind +1 /= extlen ) RETURN
      !
      DO i = ind, strlen
         str(i:i) = ' '
      ENDDO
      !
    END SUBROUTINE qexsd_basename
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE version_parse(str, major, minor, patch, ierr)
      !--------------------------------------------------------------------------
      !   
      ! Determine the major, minor and patch numbers from 
      ! a version string with the fmt "i.j.k"
      !   
      ! The ierr variable assumes the following values
      !   
      ! ierr < 0     emtpy string
      ! ierr = 0     no problem
      ! ierr > 0     fatal error
      !   
      IMPLICIT NONE
      CHARACTER(*),     INTENT(in)    :: str 
      INTEGER,          INTENT(out)   :: major, minor, patch, ierr
      !   
      INTEGER       :: i1, i2, length
      INTEGER       :: ierrtot
      CHARACTER(10) :: num(3)

      !   
      major = 0 
      minor = 0 
      patch = 0 

      length = LEN_TRIM( str )
      !
      IF ( length == 0 ) THEN
         !
         ierr = -1
         RETURN
         !
      ENDIF
  
      i1 = SCAN( str, ".")
      i2 = SCAN( str, ".", BACK=.TRUE.)
      !
      IF ( i1 == 0 .OR. i2 == 0 .OR. i1 == i2 ) THEN
         !
         ierr = 1
         RETURN
         !
      ENDIF
      !
      num(1) = str(    1 : i1-1 )
      num(2) = str( i1+1 : i2-1 )
      num(3) = str( i2+1 : )
      !
      ierrtot = 0
      !
      READ( num(1), *, IOSTAT=ierr ) major
      IF (ierr/=0) RETURN
      !
      READ( num(2), *, IOSTAT=ierr ) minor
      IF (ierr/=0) RETURN
      !
      READ( num(3), *, IOSTAT=ierr ) patch
      IF (ierr/=0) RETURN
      !
    END SUBROUTINE version_parse
    !
    !--------------------------------------------------------------------------
    FUNCTION version_compare(str1, str2)
      !--------------------------------------------------------------------------
      !   
      ! Compare two version strings; the result is
      ! 
      ! "newer":   str1 is newer that str2    
      ! "equal":   str1 is equal   to str2    
      ! "older":   str1 is older than str2    
      ! " ":       str1 or str2 has a wrong format
      !
      IMPLICIT NONE
      CHARACTER(*)  :: str1, str2
      CHARACTER(10) :: version_compare
      !
      INTEGER   :: version1(3), version2(3)
      INTEGER   :: basis, icheck1, icheck2
      INTEGER   :: ierr
      !
  
      version_compare = " "
      !
      CALL version_parse( str1, version1(1), version1(2), version1(3), ierr)
      IF ( ierr/=0 ) RETURN
      !
      CALL version_parse( str2, version2(1), version2(2), version2(3), ierr)
      IF ( ierr/=0 ) RETURN
      !
      ! 
      basis = 1000
      !
      icheck1 = version1(1) * basis**2 + version1(2)* basis + version1(3)
      icheck2 = version2(1) * basis**2 + version2(2)* basis + version2(3)
      !
      IF ( icheck1 > icheck2 ) THEN
         !
         version_compare = 'newer'
         !
      ELSEIF( icheck1 == icheck2 ) THEN
         !
         version_compare = 'equal'
         !
      ELSE
         !
         version_compare = 'older'
         !
      ENDIF
      !
    END FUNCTION version_compare  
    !
    !
    !------------------------------------------------------------------------
    FUNCTION qexsd_wfc_filename( basedir, name, ik, ipol, tag, extension )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=256)                 :: qexsd_wfc_filename
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
      filename = TRIM(int_to_char(ik))
      !
      IF ( PRESENT( ipol ) ) THEN
         !      
         filename = updw(ipol) // TRIM(filename) 
         !
      END IF
      !
      filename = TRIM( basedir ) // '/' // &
               & TRIM( name ) // TRIM( filename ) // TRIM( tag_ ) // TRIM( ext_)
      !
      qexsd_wfc_filename = TRIM( filename )
      !
      RETURN
      !
    END FUNCTION qexsd_wfc_filename
    !
    !
    !------------------------------------------------------------------------
    FUNCTION restart_dirname( outdir, prefix )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=256)           :: restart_dirname
      CHARACTER(LEN=*), INTENT(IN) :: outdir, prefix
      !
      CHARACTER(LEN=256)         :: dirname
      INTEGER                    :: strlen
      !
      ! ... main restart directory
      !
      dirname = TRIM( prefix ) // '.save'
      !
      IF ( LEN( outdir ) > 1 ) THEN
         !
         strlen = LEN_TRIM( outdir )
         IF ( outdir(strlen:strlen) == '/' ) strlen = strlen -1
         !
         dirname = outdir(1:strlen) // '/' // dirname
         !
      END IF
      !
      restart_dirname = TRIM( dirname )
      !
      RETURN
      !
    END FUNCTION restart_dirname
    !
    !
!
!-------------------------------------------
! ... read subroutines
!-------------------------------------------
!
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_header( creator_name, creator_version, &
                                  format_name, format_version, ierr )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: creator_name, creator_version
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: format_name, format_version
      INTEGER,           OPTIONAL, INTENT(OUT) :: ierr

      CHARACTER(256) :: creator_name_, creator_version_
      CHARACTER(256) :: format_name_, format_version_
      CHARACTER(256) :: str

      ierr = 0
      if (.not.present(creator_name).and..not.present(creator_version).and.&
          .not.present(format_name).and..not.present(format_version) ) return
      !
      CALL iotk_scan_begin(iunit, "general_info", IERR=ierr)
      IF(ierr /= 0) RETURN 
      !
      CALL iotk_scan_dat(iunit, "xml_format", str, ATTR=attr, IERR=ierr) 
      IF (ierr /=0 ) RETURN
      CALL iotk_scan_attr(attr, "NAME", format_name_, IERR=ierr)
      IF (ierr /= 0) RETURN 
      CALL iotk_scan_attr(attr, "VERSION", format_version_, IERR=ierr) 
      IF (ierr /= 0) RETURN 
      !    
      CALL iotk_scan_dat(iunit, "creator", str, ATTR=attr, IERR=ierr)
      IF (ierr /= 0) RETURN 
      CALL iotk_scan_attr(attr, "NAME", creator_name_, IERR=ierr ) 
      IF (ierr /=0 ) RETURN
      CALL iotk_scan_attr(attr, "VERSION", creator_version_, IERR=ierr )
      IF (ierr /=0 ) RETURN
      !
      CALL iotk_scan_end( iunit, "general_info", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT(creator_name) )     creator_name    = TRIM(creator_name_)
      IF ( PRESENT(creator_version) )  creator_version = TRIM(creator_version_)
      IF ( PRESENT(format_name) )      format_name     = TRIM(format_name_)
      IF ( PRESENT(format_version) )   format_version  = TRIM(format_version_)
      !
    END SUBROUTINE qexsd_read_header
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_cell( alat, a1, a2, a3, alat_units, a_units, ierr )
      !------------------------------------------------------------------------
      !
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: alat
      REAL(dbl),         OPTIONAL, INTENT(OUT) :: a1(3), a2(3), a3(3)
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: alat_units, a_units
      INTEGER,                     INTENT(OUT) :: ierr
      !
      ierr=0
      !
      CALL iotk_scan_begin( iunit, "atomic_structure", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF (PRESENT(alat)) THEN
         CALL iotk_scan_attr( attr, "alat", alat, IERR=ierr )
         IF ( ierr /= 0 ) RETURN
      ENDIF
      !
      IF (PRESENT(alat_units)) alat_units="Bohr"
      IF (PRESENT(a_units)) a_units="Bohr"
      !
      CALL iotk_scan_begin(iunit,"cell",IERR=ierr)
      IF (ierr/=0) RETURN
      !
      IF (PRESENT(a1)) THEN
         CALL iotk_scan_dat(iunit, "a1", a1(:), IERR=ierr )
         IF (ierr/=0) RETURN
      ENDIF
      !
      IF (PRESENT(a2)) THEN
         CALL iotk_scan_dat(iunit, "a2", a2(:), IERR=ierr )
         IF (ierr/=0) RETURN
      ENDIF
      !
      IF (PRESENT(a3)) THEN
         CALL iotk_scan_dat(iunit, "a3", a3(:), IERR=ierr )
         IF (ierr/=0) RETURN
      ENDIF
      !
      CALL iotk_scan_end( iunit, "cell", IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_end( iunit, "atomic_structure", IERR=ierr )
      IF (ierr/=0) RETURN
      !
    END SUBROUTINE qexsd_read_cell
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_ions( nsp, nat, atm, ityp, psfile, amass, amass_units, &
                                tau, tau_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,          OPTIONAL, INTENT(OUT) :: nsp, nat
      INTEGER,          OPTIONAL, INTENT(OUT) :: ityp(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: atm(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: psfile(:)
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: amass(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: amass_units
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: tau(:,:)
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
      CHARACTER(3)       :: name_
      INTEGER            :: i,is

      !
      ierr=0
      !
      CALL iotk_scan_begin(iunit, "atomic_species", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr,"ntyp",nsp_, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT(nsp) )   nsp = nsp_
      ! 
      ALLOCATE( atm_(nsp_) ) 
      ALLOCATE( amass_(nsp_) ) 
      ALLOCATE( psfile_(nsp_) ) 
      !
      DO is = 1, nsp_
         CALL iotk_scan_begin(iunit,"species", ATTR=attr, IERR=ierr)
         IF (ierr/=0) RETURN
         CALL iotk_scan_attr(attr,"name",atm_(is),IERR=ierr)
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat(iunit,"mass",amass_(is),IERR=ierr)
         IF (ierr/=0) RETURN
         CALL iotk_scan_dat(iunit,"pseudo_file",psfile_(is),IERR=ierr)
         IF (ierr/=0) RETURN
         CALL iotk_scan_end(iunit,"species", IERR=ierr)
         IF (ierr/=0) RETURN
      ENDDO
      !
      amass_units_="chemistry_units"
      !
      CALL iotk_scan_end(iunit, "atomic_species", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      CALL iotk_scan_begin(iunit, "atomic_structure", ATTR=attr, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr, "nat", nat_, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT(nat) )   nat = nat_
      !
      ALLOCATE( ityp_(nat_) ) 
      ALLOCATE( tau_(3,nat_) ) 
      !
      tau_units_="Bohr"
      !
      CALL iotk_scan_begin(iunit, "atomic_positions", IERR=ierr)
      IF (ierr/=0) RETURN
      !
      DO i = 1, nat_
         !
         CALL iotk_scan_dat(iunit,"atom",tau_(:,i),ATTR=attr, IERR=ierr)
         IF (ierr/=0) RETURN
         CALL iotk_scan_attr(attr, "name",  name_, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         ityp_(i)=0
         sp_loop:&
         DO is = 1, nsp_
            IF (TRIM(name_)==TRIM(atm_(is))) THEN
               ityp_(i)=is
               EXIT sp_loop
            ENDIF
         ENDDO sp_loop
         !
      ENDDO
      !
      IF (any(ityp_(1:nat_)==0)) THEN
         ierr=2
         RETURN
      ENDIF
      !
      CALL iotk_scan_end( iunit, "atomic_positions", IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_end( iunit, "atomic_structure", IERR=ierr )
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
      !
      DEALLOCATE( atm_ )
      DEALLOCATE( amass_ )
      DEALLOCATE( psfile_ )
      DEALLOCATE( ityp_ )
      DEALLOCATE( tau_ )
      ! 
    END SUBROUTINE qexsd_read_ions
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_symmetry( nsym, invsym, trevsym, trasl, s, sname, s_units, t_rev, &
                                    irt, nat, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,          OPTIONAL, INTENT(OUT) :: nsym
      LOGICAL,          OPTIONAL, INTENT(OUT) :: invsym
      LOGICAL,          OPTIONAL, INTENT(OUT) :: trevsym
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: s(:,:,:)
      REAL(dbl),        OPTIONAL, INTENT(OUT) :: trasl(:,:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: sname(:)
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: s_units
      INTEGER,          OPTIONAL, INTENT(OUT) :: t_rev(:)
      INTEGER,          OPTIONAL, INTENT(OUT) :: irt(:,:), nat
      INTEGER,                    INTENT(OUT) :: ierr
      !
      INTEGER              :: nsym_,nrot_
      CHARACTER(256)       :: sname_(48), s_units_
      LOGICAL              :: invsym_, trevsym_
      real(dbl)            :: s_(3,3,48)
      REAL(dbl)            :: trasl_(3,48)
      INTEGER              :: t_rev_(48)
      INTEGER              :: nat_
      INTEGER, ALLOCATABLE :: irt_(:,:)
      !      
      INTEGER   :: i,j
      LOGICAL   :: lfound
      LOGICAL   :: noncolin,domag
      CHARACTER(256) :: t_rev_str,str

      ierr=0
      !
      invsym_=.false.
      trevsym_=.true.
      !
      CALL iotk_scan_begin( iunit, "atomic_structure",ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nat", nat_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_end( iunit, "atomic_structure", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      CALL iotk_scan_begin( iunit, "symmetries", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "nsym", nsym_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "nrot", nrot_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      ALLOCATE( irt_(48, nat_) )
      !
      s_units_="Bohr"
      ! 
      i = 0
      DO j = 1, nsym_
          !
          CALL iotk_scan_begin( iunit, "symmetry", IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_dat( iunit, "info", str, ATTR=attr, IERR=ierr )
          IF (ierr/=0) RETURN
          !
          IF (TRIM(str)=="crystal_symmetry") THEN
             i=i+1
             !
             CALL iotk_scan_attr( attr, "name", sname_(i), IERR=ierr )
             IF (ierr/=0) RETURN
             CALL iotk_scan_attr( attr, "time_reversal", t_rev_str, IERR=ierr, FOUND=lfound )
             IF (ierr/=0) RETURN
             IF (.not.lfound) t_rev_str="false"
             !
             if (trim(sname_(i))=="inversion") invsym_=.true.
             if (trim(t_rev_str)=="false") t_rev_(i)=0
             if (trim(t_rev_str)=="true") t_rev_(i)=1
             !
             CALL iotk_scan_dat( iunit, "rotation", s_(1:3,1:3,i), IERR=ierr )
             IF (ierr/=0) RETURN
             !
             CALL iotk_scan_dat( iunit, "fractional_translation", trasl_(1:3,i), IERR=ierr )
             IF (ierr/=0) RETURN
             !
             CALL iotk_scan_dat( iunit, "equivalent_atoms", irt_(i,1:nat_), IERR=ierr )
             IF (ierr/=0) RETURN
             !
          ENDIF
          !
          CALL iotk_scan_end( iunit, "symmetry", IERR=ierr )
          IF (ierr/=0) RETURN
          !
      ENDDO
      !
      CALL iotk_scan_end( iunit, "symmetries", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_begin( iunit, "magnetization", IERR=ierr)
      IF (ierr/=0) RETURN
      CALL qexsd_scan_logical( iunit, "noncolin", noncolin, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL qexsd_scan_logical( iunit, "do_magnetization", domag, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_end( iunit, "magnetization", IERR=ierr)
      IF (ierr/=0) RETURN
      !
      ! XXX to be checked
      trevsym_=.not.(noncolin.and.domag)
      !
      !
      IF ( PRESENT(nsym) )        nsym          = nsym_
      IF ( PRESENT(invsym) )      invsym        = invsym_
      IF ( PRESENT(trevsym) )     trevsym       = trevsym_
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
    END SUBROUTINE qexsd_read_symmetry
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_planewaves( ecutwfc, ecutrho, npwx, gamma_only, &
                                      nr1, nr2, nr3,  ngm,  nr1s, nr2s, nr3s, ngms, &
                                      igv, cutoff_units, ierr )
      !------------------------------------------------------------------------
      !
      !
      INTEGER,      OPTIONAL, INTENT(OUT) :: npwx, nr1, nr2, nr3, ngm, &
                                             nr1s, nr2s, nr3s, ngms
      INTEGER,      OPTIONAL, INTENT(OUT) :: igv(:,:)
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: ecutwfc, ecutrho
      LOGICAL,      OPTIONAL, INTENT(OUT) :: gamma_only
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: cutoff_units
      INTEGER,                INTENT(OUT) :: ierr
      !
      INTEGER        :: iunit_aux
      CHARACTER(256) :: filename
      
      ierr = 0
      !
      call iotk_scan_begin( iunit, "basis_set", IERR=ierr )
      if (ierr/=0) return
      !
      if (present(cutoff_units)) cutoff_units="Hartree"
      !
      if (present(gamma_only)) then
         call qexsd_scan_logical(iunit, "gamma_only", gamma_only,IERR=ierr)
         if (ierr/=0) return
      endif
      !
      if (present(ecutwfc)) then
         call iotk_scan_dat( iunit, "ecutwfc", ecutwfc, IERR=ierr )
         if (ierr/=0) return
      endif
      if (present(ecutrho)) then
         call iotk_scan_dat( iunit, "ecutrho", ecutrho, IERR=ierr )
         if (ierr/=0) return
      endif
      !
      if (present(nr1).or.present(nr2).or.present(nr3)) then 
         !
         call iotk_scan_empty( iunit, "fft_grid", ATTR=attr, IERR=ierr )
         if (ierr/=0) return
         !
         if (present(nr1)) then
            call iotk_scan_attr( attr, "nr1", nr1, IERR=ierr )
            if (ierr/=0) return
         endif
         if (present(nr2)) then
            call iotk_scan_attr( attr, "nr2", nr2, IERR=ierr )
            if (ierr/=0) return
         endif
         if (present(nr3)) then
            call iotk_scan_attr( attr, "nr3", nr3, IERR=ierr )
            if (ierr/=0) return
         endif
         !
      endif
      !
      if (present(nr1s).or.present(nr2s).or.present(nr3s)) then 
         !
         call iotk_scan_empty( iunit, "fft_smooth", ATTR=attr, IERR=ierr )
         if (ierr/=0) return
         !
         if (present(nr1s)) then
            call iotk_scan_attr( attr, "nr1", nr1s, IERR=ierr )
            if (ierr/=0) return
         endif
         if (present(nr2s)) then
            call iotk_scan_attr( attr, "nr2", nr2s, IERR=ierr )
            if (ierr/=0) return
         endif
         if (present(nr3s)) then
            call iotk_scan_attr( attr, "nr3", nr3s, IERR=ierr )
            if (ierr/=0) return
         endif
         !
      endif
      !
      if (present(ngm)) then
         call iotk_scan_dat( iunit, "ngm", ngm, IERR=ierr )
         if (ierr/=0) return
      endif
      if (present(ngms)) then
         call iotk_scan_dat( iunit, "ngms", ngms, IERR=ierr )
         if (ierr/=0) return
      endif
      if (present(npwx)) then
         call iotk_scan_dat( iunit, "npwx", npwx, IERR=ierr )
         if (ierr/=0) return
      endif
      !
      if ( present( igv ) ) THEN
          !
          call qexsd_read_rhog( igv=igv, ierr=ierr )
          if (ierr/=0) return
          !
      endif
      !
      call iotk_scan_end( iunit, "basis_set", IERR=ierr )
      if (ierr/=0) return
      !
      !
    END SUBROUTINE qexsd_read_planewaves
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_gk( ik, nspin, npwk, xk, igkv, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                INTENT(IN)  :: ik, nspin
      INTEGER,      OPTIONAL, INTENT(OUT) :: npwk
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: xk(3)
      INTEGER,      OPTIONAL, INTENT(OUT) :: igkv(:,:)
      INTEGER,                INTENT(OUT) :: ierr
      !
      INTEGER   :: npwk_
      REAL(dbl) :: xk_(3)

      ierr = 0
      !
      if (present( igkv )) then
         call qexsd_read_wfc(1,1, ik, 1, nspin, XK=xk_, IGKV=igkv, NPWK=npwk_, IERR=ierr)
      else
         call qexsd_read_wfc(1,1, ik, 1, nspin, XK=xk_, NPWK=npwk_, IERR=ierr)
      endif
      if (ierr/=0) return
      !
      if ( present( npwk ) )       npwk    = npwk_
      if ( present( xk ) )         xk(1:3) = xk_(1:3)
      !
    END SUBROUTINE qexsd_read_gk
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_spin( lsda, noncolin, npol, lspinorb, ierr )
      !------------------------------------------------------------------------
      !
      LOGICAL, OPTIONAL, INTENT(OUT) :: lsda, noncolin, lspinorb
      INTEGER, OPTIONAL, INTENT(OUT) :: npol
      INTEGER,           INTENT(OUT) :: ierr
      !
      LOGICAL   :: lsda_, noncolin_, lspinorb_
      INTEGER   :: npol_
     
      ierr = 0
      !
      CALL iotk_scan_begin( iunit, "magnetization", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL qexsd_scan_logical( iunit, "lsda", lsda_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL qexsd_scan_logical( iunit, "noncolin", noncolin_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL qexsd_scan_logical( iunit, "spinorbit", lspinorb_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunit, "magnetization", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      npol_=1
      IF (noncolin_) npol_=2
      !
      IF ( PRESENT( lsda ) )       lsda      = lsda_
      IF ( PRESENT( noncolin ) )   noncolin  = noncolin_
      IF ( PRESENT( npol ) )       npol      = npol_
      IF ( PRESENT( lspinorb ) )   lspinorb  = lspinorb_
      !
    END SUBROUTINE qexsd_read_spin
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_xc( dft, dft_is_hybrid, dft_is_hubbard, dft_is_vdW, ierr)
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: dft
      LOGICAL,          OPTIONAL, INTENT(OUT) :: dft_is_hybrid, dft_is_hubbard, dft_is_vdW
      INTEGER,                    INTENT(OUT) :: ierr
      !
      LOGICAL :: lfound

      ierr = 0
      !
      CALL iotk_scan_begin( iunit, "dft", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      IF (PRESENT(dft)) THEN
         CALL iotk_scan_dat( iunit, "functional", dft, IERR=ierr )
         IF ( ierr/=0 ) RETURN
      ENDIF
      !
      IF (PRESENT(dft_is_hybrid)) THEN
         dft_is_hybrid=.false.
         CALL iotk_scan_begin( iunit, "hybrid", FOUND=lfound, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         IF ( lfound ) THEN
            dft_is_hybrid=.true.
            CALL iotk_scan_end( iunit, "hybrid", IERR=ierr )
            IF ( ierr/=0 ) RETURN
         ENDIF
      ENDIF
      !
      IF (PRESENT(dft_is_hubbard)) THEN
         dft_is_hubbard=.false.
         CALL iotk_scan_begin( iunit, "dftU", FOUND=lfound, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         IF ( lfound ) THEN
            dft_is_hubbard=.true.
            CALL iotk_scan_end( iunit, "dftU", IERR=ierr )
            IF ( ierr/=0 ) RETURN
         ENDIF
      ENDIF
      !
      IF (PRESENT(dft_is_vdW)) THEN
         dft_is_vdW=.false.
         CALL iotk_scan_begin( iunit, "vdW", FOUND=lfound, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         IF ( lfound ) THEN
            dft_is_vdW=.true.
            CALL iotk_scan_end( iunit, "vdW", IERR=ierr )
            IF ( ierr/=0 ) RETURN
         ENDIF
      ENDIF
      !
      CALL iotk_scan_end( iunit, "dft", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
    END SUBROUTINE qexsd_read_xc
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_hubbard( dft, lda_plus_u, &
!                                   Hubbard_lmax, Hubbard_l, nsp, Hubbard_U, Hubbard_alpha, &
                                   ierr )
      !------------------------------------------------------------------------
      !
      CHARACTER(LEN=*), OPTIONAL, INTENT(OUT) :: dft
      LOGICAL,          OPTIONAL, INTENT(OUT) :: lda_plus_u
!      INTEGER,          OPTIONAL, INTENT(OUT) :: Hubbard_lmax
!      INTEGER,          OPTIONAL, INTENT(OUT) :: Hubbard_l(:)
!      INTEGER,          OPTIONAL, INTENT(OUT) :: nsp
!      REAL(dbl),        OPTIONAL, INTENT(OUT) :: Hubbard_U(:), Hubbard_alpha(:)
      INTEGER,                    INTENT(OUT) :: ierr
      !
      CHARACTER(256) :: dft_
      LOGICAL        :: lda_plus_u_, lfound
      INTEGER        :: Hubbard_lmax_, nsp_
      INTEGER,    ALLOCATABLE :: Hubbard_l_(:)
      REAL(dbl),  ALLOCATABLE :: Hubbard_U_(:)
      REAL(dbl),  ALLOCATABLE :: Hubbard_alpha_(:)

      ierr = 0
      !
      CALL iotk_scan_begin( iunit, "dft", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_begin( iunit, "dftU", FOUND=lfound, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      lda_plus_u_=lfound
!      !
!      IF ( lda_plus_u_ ) THEN
!         !
!         CALL iotk_scan_dat( iunit, "NUMBER_OF_SPECIES", nsp_, IERR=ierr )
!         IF ( ierr/=0 ) RETURN
!         !
!         CALL iotk_scan_dat( iunit, "HUBBARD_LMAX", Hubbard_lmax_, IERR=ierr )
!         IF ( ierr/=0 ) RETURN
!         !
!         ALLOCATE( Hubbard_l_(nsp_) )
!         ALLOCATE( Hubbard_U_(nsp_) )
!         ALLOCATE( Hubbard_alpha_(nsp_) )
!         !
!         CALL iotk_scan_dat( iunit, "HUBBARD_L", Hubbard_l_, IERR=ierr )
!         IF ( ierr/=0 ) RETURN
!         !
!         CALL iotk_scan_dat( iunit, "HUBBARD_U", Hubbard_U_, IERR=ierr )
!         IF ( ierr/=0 ) RETURN
!         !
!         CALL iotk_scan_dat( iunit, "HUBBARD_ALPHA", Hubbard_alpha_, IERR=ierr )
!         IF ( ierr/=0 ) RETURN
!         !
!      ENDIF
      !
      IF ( lda_plus_u_ ) THEN
         CALL iotk_scan_end( iunit, "dftU", IERR=ierr )
         IF ( ierr/=0 ) RETURN
      ENDIF
      !
      CALL iotk_scan_end( iunit, "dft", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      IF ( PRESENT( dft ) )           dft           = dft_
      IF ( PRESENT( lda_plus_u ) )    lda_plus_u    = lda_plus_u_
      !
!      IF ( lda_plus_u_ )  THEN
!         !
!         IF ( PRESENT( nsp ) )             nsp                   = nsp_
!         IF ( PRESENT( Hubbard_lmax ) )    Hubbard_lmax          = Hubbard_lmax_
!         IF ( PRESENT( Hubbard_l ) )       Hubbard_l(1:Hubbard_lmax_)   = Hubbard_l_(:)
!         IF ( PRESENT( Hubbard_U ) )       Hubbard_U(1:nsp_)     = Hubbard_U_(1:nsp_)
!         IF ( PRESENT( Hubbard_alpha ) )   Hubbard_alpha(1:nsp_) = Hubbard_alpha_(1:nsp_)
!         !
!         DEALLOCATE( Hubbard_l_ )
!         DEALLOCATE( Hubbard_U_ )
!         DEALLOCATE( Hubbard_alpha_ )
!         !
!      ENDIF 

    END SUBROUTINE qexsd_read_hubbard
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_exx( exx_is_active, nqx1, nqx2, nqx3, ecutfock, exx_fraction, &
                          screening_parameter, exxdiv_treatment, x_gamma_extrapolation, ecutvcut, ierr)
      !------------------------------------------------------------------------
      implicit none
      !
      logical,           optional, intent(out) :: exx_is_active
      integer,           optional, intent(out) :: nqx1, nqx2, nqx3
      real(dbl),         optional, intent(out) :: ecutfock
      real(dbl),         optional, intent(out) :: exx_fraction
      real(dbl),         optional, intent(out) :: screening_parameter
      character(len=*),  optional, intent(out) :: exxdiv_treatment
      logical,           optional, intent(out) :: x_gamma_extrapolation
      real(dbl),         optional, intent(out) :: ecutvcut
      integer,                     intent(out) :: ierr
      !
      logical :: lfound

      ierr = 0
      !
      call iotk_scan_begin(iunit, "hybrid", FOUND=lfound, IERR=ierr)
      if ( ierr/=0 ) return
      !
      if (present(exx_is_active)) exx_is_active=lfound
      !
      if (present(nqx1) .or. present(nqx2) .or. present(nqx3)) then
         !
         call iotk_scan_begin(iunit,"qpoint_grid",ATTR=attr,IERR=ierr)
         if ( ierr/=0 ) return
         !
         if (present(nqx1)) call iotk_scan_attr(attr, "nqx1", nqx1, IERR=ierr )
         if ( ierr/=0 ) return
         if (present(nqx2)) call iotk_scan_attr(attr, "nqx2", nqx2, IERR=ierr )
         if ( ierr/=0 ) return
         if (present(nqx3)) call iotk_scan_attr(attr, "nqx3", nqx3, IERR=ierr )
         if ( ierr/=0 ) return
         !
         call iotk_scan_end(iunit,"qpoint_grid",IERR=ierr)
         if ( ierr/=0 ) return
      endiF
      !
      if (present(ecutfock)) then
         call iotk_scan_dat(iunit, "ecutfock", ecutfock, IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
      if (present(exx_fraction)) then
         call iotk_scan_dat(iunit, "exx_fraction", exx_fraction, IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
      if (present(screening_parameter)) then
         call iotk_scan_dat(iunit, "screening_parameter", screening_parameter, IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
      if (present(exxdiv_treatment)) then
         call iotk_scan_dat(iunit, "exxdiv_treatment", exxdiv_treatment, IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
      if (present(x_gamma_extrapolation)) then
         call iotk_scan_dat(iunit, "x_gamma_extrapolation", x_gamma_extrapolation, IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
      if (present(ecutvcut)) then
         call iotk_scan_dat(iunit, "ecutvcut", ecutvcut, IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
      if (lfound) then
         call iotk_scan_end(iunit, "hybrid", IERR=ierr )
         if ( ierr/=0 ) return
      endif
      !
    end subroutine qexsd_read_exx 
    !
    !
    !------------------------------------------------------------------------
    subroutine qexsd_read_bands_info( nbnd, tot_charge, tot_mag, have_smearing, smearing_type, degauss, &
&                                     occupations_type, have_spin_occ, ierr )
      !------------------------------------------------------------------------
      !
      integer,      optional, intent(out) :: nbnd
      real(dbl),    optional, intent(out) :: tot_charge, tot_mag
      logical,      optional, intent(out) :: have_smearing
      character(*), optional, intent(out) :: smearing_type
      real(dbl),    optional, intent(out) :: degauss
      character(*), optional, intent(out) :: occupations_type
      logical,      optional, intent(out) :: have_spin_occ
      integer,                intent(out) :: ierr
      !
      logical :: lfound
      
      ierr = 0
      !
      call iotk_scan_begin( iunit, "bands", IERR=ierr )
      if (ierr/=0) return
      !
      if (present(nbnd)) then
         call iotk_scan_dat( iunit, "nbnd", nbnd, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      call iotk_scan_begin( iunit, "smearing", FOUND=lfound, IERR=ierr )
      if (ierr/=0) return
      ! 
      if (present(have_smearing)) have_smearing=lfound
      ! 
      if (lfound) then
         if (present(smearing_type)) then
            call iotk_scan_dat( iunit, "smearing", smearing_type, IERR=ierr)
            if (ierr/=0) return
         endif
         !
         if (present(degauss)) then
            call iotk_scan_dat( iunit, "degauss", degauss, IERR=ierr)
           if (ierr/=0) return
         endif
         !
         if (lfound) then
            call iotk_scan_end( iunit, "smearing", IERR=ierr )
            if (ierr/=0) return
         endif
      else
         if (present(smearing_type)) smearing_type=" "
         if (present(degauss))       degauss=0.0
      endif
      !
      if (present(tot_charge)) then
         call iotk_scan_dat( iunit, "tot_charge", tot_charge, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      if (present(tot_mag)) then
         call iotk_scan_dat( iunit, "tot_magnetization", tot_mag, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) tot_mag=0.0
      endif
      !
      if (present(occupations_type)) then
         call iotk_scan_dat( iunit, "occupations", occupations_type, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      if (present(have_spin_occ)) then
         call iotk_scan_dat( iunit, "spin", have_spin_occ, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) have_spin_occ=.FALSE.
      endif
      !
      call iotk_scan_end( iunit, "bands", IERR=ierr )
      IF (ierr/=0) return
      !
    end subroutine qexsd_read_bands_info
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_band_structure( nspin, lsda, noncolin, spinorbit, nbnd, nbnd_up, nbnd_dw, &
                                          nelec, natomwfc, fermi_energy, homo_energy, fermi_energy_updw, &
                                          occupations_type, have_spin_occ, have_smearing, smearing_type, degauss, &
                                          num_k_points, nk, sk, vkpt, wk, eig, occ, npwk, ierr )
      !------------------------------------------------------------------------
      !
      integer,      optional, intent(out) :: nbnd, num_k_points, nspin, natomwfc, nbnd_up, nbnd_dw
      integer,      optional, intent(out) :: nk(3), sk(3)
      logical,      optional, intent(out) :: noncolin, lsda, spinorbit, have_spin_occ, have_smearing
      real(dbl),    optional, intent(out) :: fermi_energy, homo_energy, fermi_energy_updw(2), nelec, degauss
      real(dbl),    optional, intent(out) :: vkpt(:,:), wk(:), eig(:,:,:), occ(:,:,:)
      integer,      optional, intent(out) :: npwk(:)
      character(len=*), optional, intent(out) :: occupations_type, smearing_type 
      integer,                intent(out) :: ierr
      !
      integer :: nbnd_, nkpts_, nspin_, ik, ndim
      logical :: lsda_, noncolin_, lfound
      real(dbl) :: rvec(3)
      real(dbl), allocatable :: eig_(:), occ_(:)
      character(256) :: chval
 
      ierr = 0
      !
      call iotk_scan_begin( iunit, "band_structure", IERR=ierr )
      if (ierr/=0) return
      !
      call iotk_scan_dat( iunit, "lsda", lsda_, IERR=ierr)
      if (ierr/=0) return
      call iotk_scan_dat( iunit, "noncolin", noncolin_, IERR=ierr)
      if (ierr/=0) return
      !
      if (present(lsda))     lsda=lsda_
      if (present(noncolin)) noncolin=noncolin_
      !
      nspin_=1
      if (lsda_.and..not.noncolin_) nspin_=2
      !
      if (present(nspin)) then
         nspin=nspin_
         if (noncolin_) nspin=4    ! back compatibility
      endif
      !
      if (present(spinorbit)) then
         call iotk_scan_dat( iunit, "spinorbit", spinorbit, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      call iotk_scan_dat( iunit, "nbnd", nbnd_, IERR=ierr)
      if (ierr/=0) return
      if (nspin_==2) nbnd_=nbnd_/2
      if (present(nbnd)) nbnd=nbnd_
      !
      if (present(nbnd_up)) then
         call iotk_scan_dat( iunit, "nbnd_up", nbnd_up, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) nbnd_up=-1
      endif
      !
      if (present(nbnd_dw)) then
         call iotk_scan_dat( iunit, "nbnd_dw", nbnd_dw, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) nbnd_dw=-1
      endif
      !
      if (present(nelec)) then
         call iotk_scan_dat( iunit, "nelec", nelec, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      if (present(natomwfc)) then
         call iotk_scan_dat( iunit, "num_of_atomic_wfc", natomwfc, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      if (present(fermi_energy)) then
         call iotk_scan_dat( iunit, "fermi_energy", fermi_energy, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) fermi_energy=-100000
      endif
      !
      if (present(homo_energy)) then
         call iotk_scan_dat( iunit, "highestOccupiedLevel", homo_energy, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) homo_energy=-100000
      endif
      !
      if (present(fermi_energy_updw)) then
         call iotk_scan_dat( iunit, "two_fermi_energies", fermi_energy_updw, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) fermi_energy_updw=-100000
      endif
      !
      if (present(nk) .or. present(sk)) then
         !
         call iotk_scan_begin(iunit,"starting_k_points",IERR=ierr)
         if (ierr/=0) return
         call iotk_scan_dat(iunit,"monkhorst_pack",chval,ATTR=attr,IERR=ierr)
         if (ierr/=0) return
         !
         if (present(nk)) then
            nk(:)=0
            call iotk_scan_attr(attr,"nk1",nk(1),FOUND=lfound,IERR=ierr)
            if (ierr/=0) return
            call iotk_scan_attr(attr,"nk2",nk(2),FOUND=lfound,IERR=ierr)
            if (ierr/=0) return
            call iotk_scan_attr(attr,"nk3",nk(3),FOUND=lfound,IERR=ierr)
            if (ierr/=0) return
         endif
         !
         if (present(sk)) then
            sk(:)=0
            call iotk_scan_attr(attr,"k1",sk(1),FOUND=lfound,IERR=ierr)
            if (ierr/=0) return
            call iotk_scan_attr(attr,"k2",sk(2),FOUND=lfound,IERR=ierr)
            if (ierr/=0) return
            call iotk_scan_attr(attr,"k3",sk(3),FOUND=lfound,IERR=ierr)
            if (ierr/=0) return
         endif
         !
         call iotk_scan_end(iunit,"starting_k_points",IERR=ierr)
         if (ierr/=0) return
         !
      endif
      !
      call iotk_scan_dat( iunit, "nks", nkpts_, IERR=ierr)
      if (ierr/=0) return
      if (present(num_k_points)) num_k_points=nkpts_
      !
      if (present(occupations_type)) then
         call iotk_scan_dat( iunit, "occupations_kind", occupations_type, IERR=ierr)
         if (ierr/=0) return
      endif
      !
      if (present(have_spin_occ)) then
         call iotk_scan_dat( iunit, "spin", have_spin_occ, FOUND=lfound, IERR=ierr)
         if (ierr/=0) return
         if (.not.lfound) have_spin_occ=.FALSE.
      endif
      !
      call iotk_scan_dat( iunit, "smearing", chval, ATTR=attr, FOUND=lfound, IERR=ierr )
      if (ierr/=0) return
      ! 
      if (present(have_smearing)) have_smearing=lfound
      if (present(smearing_type)) then
         if (lfound) then
            smearing_type=trim(chval)
         else
            smearing_type=" "
         endif
      endif
      if (present(degauss)) then
         if (lfound) then
            call iotk_scan_attr( attr, "degauss", degauss, IERR=ierr)
            if (ierr/=0) return
         else
            degauss=0.0
         endif
      endif
      !
      !
      if (present(vkpt).or.present(eig).or.present(occ).or.present(npwk)) then
         !
         ndim = nspin_ * nbnd_ 
         allocate(occ_(ndim),eig_(ndim))
         !
         do ik = 1, nkpts_
            !
            call iotk_scan_begin(iunit,"ks_energies",IERR=ierr)
            if (ierr/=0) return
            !
            if (present(vkpt).or.present(wk)) then
               call iotk_scan_dat( iunit, "k_point", rvec, ATTR=attr, IERR=ierr)
               if (ierr/=0) return
            endif
            if (present(vkpt)) vkpt(1:3,ik) = rvec 
            if (present(wk)) then
               call iotk_scan_attr( attr, "weight", wk(ik), IERR=ierr)
               if (ierr/=0) return
            endif
            !
            if (present(npwk)) then
               call iotk_scan_dat( iunit, "npw", npwk(ik), IERR=ierr)
               if (ierr/=0) return
            endif
            !
            if (present(eig)) then
               call iotk_scan_dat( iunit, "eigenvalues", eig_, IERR=ierr)
               if (ierr/=0) return
               !
               eig(:,ik,1) = eig_(1:nbnd_)
               if (nspin_==2) then
                 eig(:,ik,2) = eig_(nbnd_+1:2*nbnd_)
               endif
               !
            endif
            !
            if (present(occ)) then
               call iotk_scan_dat( iunit, "occupations", occ_, IERR=ierr)
               if (ierr/=0) return
               !
               occ(:,ik,1) = occ_(1:nbnd_)
               if (nspin_==2) then
                 occ(:,ik,2) = occ_(nbnd_+1:2*nbnd_)
               endif
               !
            endif
            !
            call iotk_scan_end(iunit,"ks_energies",IERR=ierr)
            if (ierr/=0) return
            !
         enddo
         !
         deallocate(occ_,eig_)
         !
      endif
      !
      !
      call iotk_scan_end( iunit, "band_structure", IERR=ierr )
      if (ierr/=0) return
      !
    end subroutine qexsd_read_band_structure
    !
    !
    !------------------------------------------------------------------------
    subroutine qexsd_fft2igv_map( ngm, igv, nfft, fft2igv, dims_only)
      !------------------------------------------------------------------------
      !
      ! defines the fft2igv map
      !
      integer,           intent(in)  :: ngm
      integer,           intent(in)  :: igv(3,ngm)
      integer,           intent(inout):: nfft(3)
      integer, optional, intent(out) :: fft2igv(:)
      logical, optional, intent(in)  :: dims_only
      !
      integer :: i, ig, nx,ny,nz, npoint
      integer :: gv_min, gv_max
      logical :: dims_only_

      do i = 1, 3
        gv_min=minval(igv(i,:))
        gv_max=maxval(igv(i,:))
        nfft(i)=gv_max-gv_min+1
      enddo

      dims_only_=.false.
      if (present(dims_only)) dims_only_=dims_only
      !
      if (dims_only_) return
      if (.not.present(fft2igv)) return
    
!$omp parallel do default(shared), private(ig,nx,ny,nz,npoint)
      do ig = 1, ngm
        !
        nx=-100000
        ny=-100000
        nz=-100000
        !
        if ( igv(1,ig) >= 0 ) nx = igv(1,ig) + 1
        if ( igv(1,ig) <  0 ) nx = igv(1,ig) + 1 + nfft(1)
        if ( igv(2,ig) >= 0 ) ny = igv(2,ig) + 1
        if ( igv(2,ig) <  0 ) ny = igv(2,ig) + 1 + nfft(2)
        if ( igv(3,ig) >= 0 ) nz = igv(3,ig) + 1
        if ( igv(3,ig) <  0 ) nz = igv(3,ig) + 1 + nfft(3)
        !
        npoint = nx + (ny-1)*nfft(1) + (nz-1)*nfft(1)*nfft(2)
        !
        fft2igv(npoint) = ig
        !
      enddo
!$omp end parallel do
      !
    end subroutine qexsd_fft2igv_map
    !
    !
    !------------------------------------------------------------------------
    subroutine qexsd_igk_map( nfft, fft2igv, npwk, igkv, igk_map )
      !------------------------------------------------------------------------
      !
      ! needs to fft2igv map
      !
      integer,      intent(in)  :: nfft(3), npwk
      integer,      intent(in)  :: fft2igv(:)
      integer,      intent(in)  :: igkv(3,npwk)
      integer,      intent(out) :: igk_map(npwk)
      !
      integer :: ig, nx,ny,nz, npoint

!$omp parallel do default(shared), private(ig,nx,ny,nz,npoint)
      do ig = 1, npwk
        !
        nx=-100000
        ny=-100000
        nz=-100000
        !
        if ( igkv(1,ig) >= 0 ) nx = igkv(1,ig) + 1
        if ( igkv(1,ig) <  0 ) nx = igkv(1,ig) + 1 + nfft(1)
        if ( igkv(2,ig) >= 0 ) ny = igkv(2,ig) + 1
        if ( igkv(2,ig) <  0 ) ny = igkv(2,ig) + 1 + nfft(2)
        if ( igkv(3,ig) >= 0 ) nz = igkv(3,ig) + 1
        if ( igkv(3,ig) <  0 ) nz = igkv(3,ig) + 1 + nfft(3)
        !
        npoint = nx + (ny-1)*nfft(1) + (nz-1)*nfft(1)*nfft(2)
        !
        igk_map(ig) = fft2igv(npoint)
        !
      enddo
!$omp end parallel do
      !
    end subroutine qexsd_igk_map
    !
!
! Copyright (C) 2016-2017 Quantum ESPRESSO Foundation 
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!----------------------------------------------
  !
  ! ... subroutines used to read and write binary data produced by QE
  ! ... Author: Paolo Giannozzi, based on previous work by Carlo Cavazzoni
  ! ...         modified and adapted to yambo by A. Ferretti (2017)
  !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_wfc( ibnds, ibnde, ik, ispin, nspin, ipol, xk, wfc, &
                               gamma_only, ngw, igkv, npwk, ierr )
      !
      !! "filename.*" (* = dat if fortran binary, * = hdf5 if HDF5),
      !! ierr return 0 if everything is ok, /= 0 if not
      !! read one spinor component at the time
      !------------------------------------------------------------------------
      !
      USE pars,        ONLY : DP
      !
#if defined (_HDF5)
      USE  qeh5_base_module
#endif

      implicit none
      !
      integer,               intent(in)    :: ibnds, ibnde, ik
      integer,               intent(in)    :: ispin, nspin
      integer,     optional, intent(in)    :: ipol
      integer,     optional, intent(out)   :: ngw, npwk
      complex(DP), optional, intent(out)   :: wfc(:,:)
      real(DP),    optional, intent(out)   :: xk(3)
      integer,     optional, intent(out)   :: igkv(:,:)
      logical,     optional, intent(out)   :: gamma_only
      integer,               intent(out)   :: ierr
      !
      complex(DP), allocatable :: wtmp(:)
      integer            :: igwx_, nbnd_, npol_, ik_, ispin_, ngw_
      integer            :: iuni, j, jj, igwx, npwx
      real(DP)           :: xk_(3), scalef, b1(3), b2(3), b3(3)
      logical            :: gamma_only_
      character(256)     :: filename
#if defined(_HDF5)
      type(qeh5_file)    :: h5file
      type(qeh5_dataset) :: h5dset_wfc, h5dset_mill
      character(len=8)   :: char_buf 
#endif  

      !
      ! defs
      !
      ierr=0
      !
      if (present(igkv)) then
         igwx = size(igkv,2) 
      else if (present(wfc)) then
         igwx = size(wfc,1) 
      else
         ierr=71
         return
      endif

      !
      ! set filename
      !
      call iotk_free_unit( iuni )
      !
      if (nspin==2) then
         filename=trim( qexsd_wfc_filename(datadir_in,"wfc",ik, IPOL=ispin, EXTENSION=" ") )
      else
         filename=trim( qexsd_wfc_filename(datadir_in,"wfc",ik, EXTENSION=" ") )
      endif
      !
#if defined(_HDF5)
      call qeh5_openfile( h5file, TRIM(filename)//'hdf5', ACTION = 'read', ERROR = ierr)
#else
      open(iuni, file=trim(filename)//'dat', form='unformatted', status='old', iostat=ierr)
#endif
      if ( ierr /= 0 ) return

      !
#if defined(_HDF5)
      CALL qeh5_read_attribute (h5file%id, "ik", ik_)
      CALL qeh5_read_attribute (h5file%id, "xk",xk_, RANK =1, DIMENSIONS = [3])
      CALL qeh5_read_attribute (h5file%id, "ispin", ispin_)
      CALL qeh5_read_attribute (h5file%id, "gamma_only", char_buf, MAXLEN = len(char_buf) )
      IF (TRIM(char_buf) =='.TRUE.' .OR. TRIM(char_buf)=='.true.') THEN 
         gamma_only_ = .TRUE. 
      ELSE 
         gamma_only_ = .FALSE.
      END IF
      call qeh5_read_attribute (h5file%id, "ngw", ngw_)
      call qeh5_read_attribute (h5file%id, "nbnd",nbnd_)
      call qeh5_read_attribute (h5file%id, "npol",npol_)
      call qeh5_read_attribute (h5file%id, "igwx",igwx_)
#else
      read (iuni,iostat=ierr) ik_, xk_, ispin_, gamma_only_, scalef
      if (ierr/=0) return
      read (iuni,iostat=ierr) ngw_, igwx_, npol_, nbnd_
      if (ierr/=0) return
#endif
      !
      ierr=20
      if (igwx_>igwx) return
      ierr=0
      !
      if (present(ngw))        ngw=ngw_
      if (present(npwk))       npwk=igwx_
      if (present(gamma_only)) gamma_only=gamma_only_
      if (present(xk))         xk=xk_
      !
#if defined(_HDF5)
      !
      if (present(igkv)) then
         !
         CALL qeh5_open_dataset(h5file, h5dset_mill, ACTION='read', NAME='MillerIndices')
         !
         if ( h5dset_mill%filespace%dims(2) > igwx ) then
             ierr= 10 ; return
         endif
         !
         CALL qeh5_read_dataset(igkv, h5dset_mill) 
         CALL qeh5_close(h5dset_mill) 
      endif
      !
#else
      !
      ! skip b1,b2,b3
      !
      read (iuni,iostat=ierr) b1, b2, b3
      if (ierr/=0) return
      !
      if (present(igkv)) then
         read (iuni,iostat=ierr) igkv(1:3,1:igwx_)
         if (ierr/=0) return
      else
         read (iuni,iostat=ierr) jj
         if (ierr/=0) return
      endif
      !
#endif
      if (present(igkv)) then
         if ( igwx > igwx_ ) igkv(1:3,igwx_+1:igwx) = 0
      endif

      !
      ! wfc readin
      !
      if ( PRESENT(wfc) ) then
         !
         !npwx = SIZE( wfc, 1 ) / npol_
         !npwx = igwx
         !
         if (npol_ >2 .and. .not. present(ipol) ) then
            ierr=71 ; return
         endif
         !
         allocate( wtmp(npol_*igwx) )
         !
#if defined(_HDF5) 
         CALL qeh5_open_dataset( h5file, h5dset_wfc, ACTION = 'read', NAME = 'evc')
         CALL qeh5_set_space ( h5dset_wfc, wtmp(1), RANK = 1, DIMENSIONS = [npol_*igwx_], MODE = 'm') 
#endif
         !
         ! few size checks
         !
         if ( ibnde-ibnds+1 > size(wfc,2) .or. ibnds < 1 .or. ibnde > nbnd_ ) then
            ierr=80 ; return
         endif
         !
         ! actual read
         !
         do jj = ibnds, ibnde
            !
            j = jj-ibnds+1 
            !
#if defined(_HDF5)
            !         
            call qeh5_set_file_hyperslab (h5dset_wfc, OFFSET = [0,j-1], COUNT = [2*npol_*igwx_,1] )
            call qeh5_read_dataset (wtmp, h5dset_wfc )  
#else
            read (iuni,iostat=ierr) wtmp(1:npol_*igwx_) 
            if (ierr/=0) return
#endif
            IF ( igwx > igwx_ ) wtmp((npol_*igwx_+1):npol_*igwx) = 0.0_DP
            !
            if ( npol_ == 2 ) then
               !wfc(1:npwx,       j) = wtmp(1:igwx_  )
               !wfc(npwx+1:2*npwx,j) = wtmp(igwx_+1:2*igwx_)
               wfc(1:igwx_,j) = wtmp( (ipol-1)*igwx_+1 : ipol*igwx_)
            else
               wfc(:,j) = wtmp
            endif
            !
         enddo
         !
         deallocate(wtmp)
         !
#if defined(_HDF5)
         CALL qeh5_close(h5dset_wfc) 
         CALL qeh5_close(h5file)
#endif
         ! 
      ENDIF
      !
#if !defined(_HDF5)
      CLOSE ( UNIT = iuni, STATUS = 'keep' )
#endif
      !
      RETURN
      !
    END SUBROUTINE qexsd_read_wfc
    !
    !
    !------------------------------------------------------------------------
    subroutine qexsd_read_rhog ( igv, rho, ierr )
      !------------------------------------------------------------------------
      !! Read and distribute rho(G) from file  'charge-density.*' 
      !! (* = dat if fortran binary, * = hdf5 if HDF5)
      !
      use pars,        ONLY : DP
#if defined (_HDF5) 
      use qeh5_base_module
#endif
      implicit none
      !
      !! g-vector components
      integer,     optional, intent(inout) :: igv(:,:)
      !! read up to nspin components
      complex(DP), optional, intent(inout) :: rho(:,:)
      integer,               intent(out)   :: ierr
      !
      complex(DP), allocatable :: rho_g(:)
      complex(DP)         :: rhoup, rhodw
      real(DP)            :: b1(3), b2(3), b3(3)
      integer             :: nspin, nspin_, ngm_g, isup, isdw
      integer             :: iun, ns, ig
      integer             :: mill_dum
      logical             :: gamma_only
      character(len=320)  :: filename
      character(len=15)   :: subname="qexsd_read_rhog"
      !
#if defined _HDF5
      type( qeh5_file)    :: h5file
      type( qeh5_dataset) :: h5dset_mill, h5dset_rho_g
      character(len=10)   :: tempchar, datasets(2) = ['rhotot_g  ', 'rhodiff_g ']
      
      filename = trim( datadir_in ) // '/charge-density.hdf5'
#else 
      filename = trim( datadir_in ) // '/charge-density.dat'
#endif 
      !
      ngm_g=0
      nspin=0
      if ( present( igv) ) then
         ngm_g  = size(igv, 2)
      else if ( present( rho ) ) then
         ngm_g  = size(rho, 1)
         nspin  = size(rho, 2)
      else 
         ierr=2
         return
      endif
      !
      call iotk_free_unit(iun)
      ierr = 0
      !
#if defined (_HDF5) 
      call qeh5_openfile(h5file, trim(filename), ACTION='read', error=ierr)
      if ( ierr /= 0 ) return
      !
      call qeh5_read_attribute (h5file%id, "gamma_only", tempchar, MAXLEN=len(tempchar)  )
      call qeh5_read_attribute (h5file%id, "ngm_g", ngm_g ) 
      call qeh5_read_attribute (h5file%id, "nspin", nspin_)  
      !
      select case (trim(tempchar) )  
         case ('.true.', '.TRUE.' ) 
             gamma_only = .TRUE.
         case default
             gamma_only = .FALSE.
      end select    
#else
      open (iun, file=trim(filename), form='unformatted', status='old', iostat=ierr )
      if ( ierr /= 0 ) return
      ! 
      read (iun, iostat=ierr) gamma_only, ngm_g, nspin_
      if ( ierr /= 0 ) return
      read (iun, iostat=ierr) b1, b2, b3
      if ( ierr /= 0 ) return
#endif
      !
      if ( nspin > nspin_ ) then
         ierr=71
         return
      endif

      !
      ! read record containing G-vector indices
      !
#if defined(_HDF5)
      if ( present(igv) ) then
         call qeh5_open_dataset ( h5file, h5dset_mill, NAME="MillerIndices" , ACTION='read', ERROR=ierr)
         call qeh5_read_dataset( igv, h5dset_mill )
         call qeh5_close( h5dset_mill)
      endif
#else
      if ( present(igv)) then
         read (iun, iostat=ierr) igv(1:3,1:ngm_g)
      else
         read (iun, iostat=ierr) mill_dum
      endif
#endif
      if ( ierr /= 0 ) return

      !
      ! ... now read the G-vector components
      ! ... of the charge density (one spin at the time to save memory)
      !
      if ( present(rho) ) then
         !
         allocate( rho_g( ngm_g ) )
         !
         do ns = 1, nspin
            !
#if defined(_HDF5)
            call qeh5_open_dataset( h5file, h5dset_rho_g, NAME=datasets(ns), ACTION='read', ERROR=ierr) 
            call qeh5_read_dataset ( rho_g , h5dset_rho_g )
            call qeh5_close ( h5dset_rho_g )  
#else 
            read (iun, iostat=ierr) rho_g(1:ngm_g)
#endif
            if ( ierr /= 0 ) return
            !
            rho(1:ngm_g,ns) = rho_g(1:ngm_g)
            !
            ! Workaround for LSDA, while waiting for much-needed harmonization:
            ! if file contains rhotot=up+dw and rhodif=up-dw (nspin_=2), and
            ! if we want rhoup and rho down (nspin=2), convert 
            ! 
            if ( nspin_ == 2 .and. nspin == 2 .and. ns == 2 ) then
               do ig = 1, ngm_g
                  rhoup = (rho(ig,ns-1) + rho_g(ig)) / 2.0_dp
                  rhodw = (rho(ig,ns-1) - rho_g(ig)) / 2.0_dp
                  rho(ig,ns-1)= rhoup
                  rho(ig,ns  )= rhodw
               enddo
            endif
         enddo
         !
         deallocate(rho_g)
         !
      endif
      !
#if !defined(_HDF5)
      close (iun, status ='keep' )
#endif
      !
      return
      !
    END SUBROUTINE qexsd_read_rhog

end module qexsd_module

