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
  PUBLIC :: qexsd_read_header, qexsd_read_cell, qexsd_read_ions,      &
            qexsd_read_symmetry, &
            qexsd_read_planewaves, qexsd_read_spin, qexsd_read_xc,    &
            qexsd_read_occ, qexsd_read_bz, &
            qexsd_read_bands, qexsd_read_bands_info,qexsd_read_exx,   &
            qexsd_read_gk, qexsd_read_wfc

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
          CALL qexsd_read_header( FORMAT_VERSION=qexsd_current_version, IERR=ierr )
          IF ( ierr/=0 ) qexsd_current_version = TRIM( qexsd_default_version )
          !
          CALL qexsd_read_header( CREATOR_VERSION=qe_current_version, IERR=ierr )
          IF ( ierr/=0 ) CALL errore( 'Read QE version', &
                         ' variables QE_current_version not present', 1 )
          !IF ( ierr/=0 ) pwscf_current_version = TRIM( pwscf_default_version )
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
         filename = TRIM(filename) // updw(ipol)
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
      CHARACTER(256) :: t_rev_str,str

      !
      ierr=0
      !
      invsym_=.false.
      trevsym_=.false.
      !
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
      ! XXXX is this right ???
      IF (any(t_rev_(:)==1)) trevsym_=.true.
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
      INTEGER        :: npwx_, nr1_, nr2_, nr3_, ngm_, &
                        nr1s_, nr2s_, nr3s_, ngms_
      INTEGER        :: iunit_aux
      REAL(dbl)      :: ecutwfc_, ecutrho_
      CHARACTER(256) :: cutoff_units_,str,filename
      LOGICAL        :: gamma_only_
      !
      
      ierr = 0
      !
      CALL iotk_scan_begin( iunit, "basis_set", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      cutoff_units_="Rydberg"
      !
      CALL qexsd_scan_logical(iunit, "gamma_only", gamma_only_,IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "ecutwfc", ecutwfc_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "ecutrho", ecutrho_ , IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "fft_grid", str, ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "nr1", nr1_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr2", nr2_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr3", nr3_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "fft_smooth", str, ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "nr1", nr1s_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr2", nr2s_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr3", nr3s_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "ngm", ngm_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "ngms", ngms_, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_dat( iunit, "npwx", npwx_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT( igv ) ) THEN
          !
          CALL iotk_free_unit(iunit_aux)
          !
          filename = TRIM( datadir_in ) // '/gvectors.dat'
          CALL iotk_open_read(iunit_aux, file=filename, IERR=ierr)
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_begin( iunit_aux, "G-VECTORS", IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_dat( iunit_aux, "g", igv(1:3,1:ngm_), IERR=ierr )
          IF (ierr/=0) RETURN
          !
          CALL iotk_scan_end( iunit_aux, "G-VECTORS", IERR=ierr )          
          IF (ierr/=0) RETURN
          !
          CALL iotk_close_read(iunit_aux, IERR=ierr)
          IF (ierr/=0) RETURN
      ENDIF
      !
      CALL iotk_scan_end( iunit, "basis_set", IERR=ierr )
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
      IF ( PRESENT( cutoff_units ) )      cutoff_units = TRIM( cutoff_units_ )
      !
    END SUBROUTINE qexsd_read_planewaves
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_gk( ik, npwk, npwkx, xk, k_units, index, igk, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                INTENT(IN)  :: ik
      INTEGER,      OPTIONAL, INTENT(OUT) :: npwk, npwkx
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: xk(3)
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: k_units
      INTEGER,      OPTIONAL, INTENT(OUT) :: igk(:,:), index(:)
      INTEGER,                INTENT(OUT) :: ierr
      !
      CHARACTER(256) :: filename, k_units_
      INTEGER   :: npwk_, npwkx_
      REAL(dbl) :: xk_(3)
      INTEGER   :: iunaux
      !

      ierr = 0
      !
      CALL iotk_free_unit( iunaux )
      filename = qexsd_wfc_filename( datadir_in, 'gkvectors', ik )
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
      CALL iotk_scan_dat( iunaux, 'K-POINT_COORDS', xk_, ATTR=attr, IERR=ierr)
      IF (ierr/=0)  RETURN
      CALL iotk_scan_attr( attr, 'UNITS', k_units_, IERR=ierr)
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
      IF ( PRESENT( npwk ) )       npwk    = npwk_
      IF ( PRESENT( npwkx ) )      npwkx   = npwkx_
      IF ( PRESENT( xk ) )         xk(1:3) = xk_(1:3)
      IF ( PRESENT( k_units ) )    k_units = TRIM(k_units_)
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
      ! 
     
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
    SUBROUTINE qexsd_read_xc( dft, lda_plus_u, Hubbard_lmax, Hubbard_l, &
                              nsp, Hubbard_U, Hubbard_alpha, ierr )
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
      LOGICAL        :: lda_plus_u_, lfound
      INTEGER        :: Hubbard_lmax_, nsp_
      INTEGER,    ALLOCATABLE :: Hubbard_l_(:)
      REAL(dbl),  ALLOCATABLE :: Hubbard_U_(:)
      REAL(dbl),  ALLOCATABLE :: Hubbard_alpha_(:)
      ! 
      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunit, "EXCHANGE_CORRELATION", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "DFT", dft_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "LDA_PLUS_U_CALCULATION", lda_plus_u_, FOUND=lfound, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      IF ( .NOT. lfound ) lda_plus_u_=.FALSE.
      !
      IF ( lda_plus_u_ ) THEN
         !
         CALL iotk_scan_dat( iunit, "NUMBER_OF_SPECIES", nsp_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_dat( iunit, "HUBBARD_LMAX", Hubbard_lmax_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         ALLOCATE( Hubbard_l_(nsp_) )
         ALLOCATE( Hubbard_U_(nsp_) )
         ALLOCATE( Hubbard_alpha_(nsp_) )
         !
         CALL iotk_scan_dat( iunit, "HUBBARD_L", Hubbard_l_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_dat( iunit, "HUBBARD_U", Hubbard_U_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
         CALL iotk_scan_dat( iunit, "HUBBARD_ALPHA", Hubbard_alpha_, IERR=ierr )
         IF ( ierr/=0 ) RETURN
         !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "EXCHANGE_CORRELATION", IERR=ierr )
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

    END SUBROUTINE qexsd_read_xc
    !
    !
    SUBROUTINE qexsd_read_exx( x_gamma_extrapolation, nqx1, nqx2, nqx3, &
                          exxdiv_treatment, yukawa, ecutvcut, exx_fraction, &
                          screening_parameter, exx_is_active, ierr )
      !------------------------------------------------------------------------
      !
      LOGICAL,                     INTENT(OUT) :: exx_is_active
      REAL(dbl),                    INTENT(OUT) :: exx_fraction
      INTEGER,                     INTENT(OUT) :: ierr
      LOGICAL,           OPTIONAL, INTENT(OUT) :: x_gamma_extrapolation
      INTEGER,           OPTIONAL, INTENT(OUT) :: nqx1, nqx2, nqx3
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: exxdiv_treatment
      REAL(dbl),          OPTIONAL, INTENT(OUT) :: yukawa, ecutvcut
      REAL(dbl),          OPTIONAL, INTENT(OUT) :: screening_parameter

      CALL iotk_scan_begin(iunit, "EXACT_EXCHANGE", IERR=ierr)
      IF ( ierr/=0 ) RETURN
      ! call iotk_scan_dat(iunit, "x_gamma_extrapolation", x_gamma_extrapolation, IERR=ierr )
      ! call iotk_scan_dat(iunit, "nqx1", nqx1, IERR=ierr )
      ! call iotk_scan_dat(iunit, "nqx2", nqx2, IERR=ierr )
      ! call iotk_scan_dat(iunit, "nqx3", nqx3, IERR=ierr )
      ! call iotk_scan_dat(iunit, "exxdiv_treatment", exxdiv_treatment, IERR=ierr )
      ! call iotk_scan_dat(iunit, "yukawa", yukawa, IERR=ierr )
      ! call iotk_scan_dat(iunit, "ecutvcut", ecutvcut, IERR=ierr )
      call iotk_scan_dat(iunit, "exx_fraction", exx_fraction, IERR=ierr )
      IF ( ierr/=0 ) RETURN
       call iotk_scan_dat(iunit, "screening_parameter", screening_parameter, IERR=ierr )
      call iotk_scan_dat(iunit, "exx_is_active", exx_is_active, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_end(iunit, "EXACT_EXCHANGE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
    END SUBROUTINE qexsd_read_exx 
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_occ( lgauss, ngauss, degauss, degauss_units, &
                               ltetra, ntetra, tetra, tfixed_occ,      &
                               nstates_up, nstates_dw, input_occ, ierr )
      !------------------------------------------------------------------------
      !
      LOGICAL,      OPTIONAL, INTENT(OUT) :: lgauss, ltetra, tfixed_occ
      INTEGER,      OPTIONAL, INTENT(OUT) :: ngauss, ntetra
      INTEGER,      OPTIONAL, INTENT(OUT) :: tetra(:,:)
      INTEGER,      OPTIONAL, INTENT(OUT) :: nstates_up, nstates_dw
      REAL(dbl),    OPTIONAL, INTENT(OUT) :: degauss, input_occ(:,:)
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: degauss_units
      INTEGER,                INTENT(OUT) :: ierr
      !
      LOGICAL        :: lgauss_, ltetra_, tfixed_occ_
      INTEGER        :: ngauss_, ntetra_, nstates_up_, nstates_dw_
      LOGICAL        :: lsda_
      REAL(dbl)      :: degauss_
      CHARACTER(256) :: degauss_units_
      INTEGER,  ALLOCATABLE :: tetra_(:,:)
      INTEGER :: i
      LOGICAL :: lfound
      !
      ierr = 0 
      !
      CALL iotk_scan_begin( iunit, "OCCUPATIONS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "SMEARING_METHOD", lgauss_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( lgauss_ ) THEN
         !
         CALL iotk_scan_dat( iunit, "SMEARING_TYPE", ngauss_, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         CALL iotk_scan_dat( iunit, "SMEARING_PARAMETER", degauss_ , &
                                     ATTR=attr, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         CALL iotk_scan_attr( ATTR, "UNITS", degauss_units_ , IERR=ierr )
         IF (ierr/=0) RETURN
         !
      ELSE
         !
         ngauss_=0
         degauss_=0.0
         degauss_units_=" "
         !
      ENDIF
      !
      CALL iotk_scan_dat( iunit, "TETRAHEDRON_METHOD", ltetra_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( ltetra_ ) THEN
         !
         CALL iotk_scan_dat( iunit, "NUMBER_OF_TETRAHEDRA", ntetra_, IERR=ierr )
         IF (ierr/=0) RETURN
         !
         ALLOCATE( tetra_(4, ntetra_) )
         !
         DO i = 1, ntetra_
            !
            CALL iotk_scan_dat( iunit, "TETRAHEDRON"//iotk_index(i), &
                                        tetra_(1:4,i), IERR=ierr )
            IF (ierr/=0) RETURN
            !
         ENDDO
         !
      ELSE
         ntetra_ = 0
      ENDIF
      !
      CALL iotk_scan_dat( iunit, "FIXED_OCCUPATIONS", tfixed_occ_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( tfixed_occ_  .AND. ( PRESENT( input_occ ) .OR. &
                                PRESENT(nstates_up)  .OR. PRESENT(nstates_dw) ) ) THEN
         !
         CALL iotk_scan_empty( iunit, "INFO", ATTR=attr, IERR=ierr)
         IF (ierr /=0 ) RETURN
         !
         CALL iotk_scan_attr( attr, "lsda", lsda_, IERR=ierr )
         IF (ierr /=0 ) RETURN
         !
            !
            ! current version
            !
            CALL iotk_scan_attr( attr, "nstates_up", nstates_up_, IERR=ierr )
            IF (ierr /=0 ) RETURN
            CALL iotk_scan_attr( attr, "nstates_dw", nstates_dw_, IERR=ierr )
            IF (ierr /=0 ) RETURN
            !
         ! 
         IF ( PRESENT( input_occ ) ) THEN
            !
            CALL iotk_scan_dat( iunit, "INPUT_OCC_UP", input_occ(1:nstates_up_,1), IERR=ierr )
            IF (ierr/=0) RETURN
            !
            IF ( lsda_  ) THEN
               !
               CALL iotk_scan_dat( iunit, "INPUT_OCC_DOWN", input_occ(1:nstates_dw_,2), IERR=ierr )
               IF (ierr/=0) RETURN
               !
            ENDIF
            !
         ENDIF
         !
      ELSE
         nstates_up_=0
         nstates_dw_=0
      ENDIF
      !
      CALL iotk_scan_end( iunit, "OCCUPATIONS", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( lgauss ))           lgauss      = lgauss_
      IF ( PRESENT( ltetra ))           ltetra      = ltetra_
      IF ( PRESENT( tfixed_occ ))       tfixed_occ  = tfixed_occ_
      IF ( PRESENT( ngauss ))           ngauss      = ngauss_
      IF ( PRESENT( degauss_units ))    degauss_units  = TRIM(degauss_units_)
      IF ( PRESENT( degauss ))          degauss     = degauss_
      IF ( PRESENT( ntetra ))           ntetra      = ntetra_
      IF ( PRESENT( nstates_up ))       nstates_up  = nstates_up_
      IF ( PRESENT( nstates_dw ))       nstates_dw  = nstates_dw_
      !
      IF ( ltetra_ ) THEN
         !
         IF ( PRESENT( tetra ) )         tetra(1:4, 1:ntetra_)  = tetra_
         !
         DEALLOCATE( tetra_ )
         !
      ENDIF

    END SUBROUTINE qexsd_read_occ
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_bz( num_k_points, xk, wk, k1, k2, k3, nk1, nk2, nk3, &
                              k_units, ierr )
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
      CALL iotk_scan_begin( iunit, "BRILLOUIN_ZONE", IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_dat( iunit, "NUMBER_OF_K-POINTS", num_k_points_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      !
      CALL iotk_scan_empty( iunit, "UNITS_FOR_K-POINTS", ATTR=attr, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      CALL iotk_scan_attr( attr, "UNITS", k_units_, IERR=ierr )
      IF ( ierr/=0 ) RETURN
      !
      CALL iotk_scan_empty( iunit, "MONKHORST_PACK_GRID", ATTR=attr, IERR=ierr )
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
      CALL iotk_scan_empty( iunit, "MONKHORST_PACK_OFFSET", ATTR=attr, IERR=ierr )
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
         CALL iotk_scan_empty( iunit, "K-POINT" // TRIM( iotk_index(ik) ), &
                               ATTR=attr, IERR=ierr )
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
      CALL iotk_scan_end( iunit, "BRILLOUIN_ZONE", IERR=ierr )
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
    END SUBROUTINE qexsd_read_bz
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_bands_info( nbnd, num_k_points, nspin, noncolin, natomwfc, & 
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
      LOGICAL        :: noncolin_,two_ef_, found
      REAL(dbl)      :: ef_, nelec_
      CHARACTER(256) :: energy_units_, k_units_

      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunit, "BAND_STRUCTURE_INFO", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunit, "NUMBER_OF_BANDS", nbnd_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunit, "NUMBER_OF_K-POINTS", num_k_points_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunit, "NUMBER_OF_SPIN_COMPONENTS", nspin_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunit, "NON-COLINEAR_CALCULATION", noncolin_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunit, "NUMBER_OF_ATOMIC_WFC", natomwfc_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat  ( iunit, "NUMBER_OF_ELECTRONS", nelec_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunit, "UNITS_FOR_K-POINTS", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr ( attr,   "UNITS", k_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunit, "UNITS_FOR_ENERGIES", ATTR = attr, IERR=ierr )
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr ( attr,   "UNITS", energy_units_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_dat( iunit, "TWO_FERMI_ENERGIES", two_ef_, FOUND = found)
      !
      IF ( two_ef_ .and. found ) THEN
        CALL errore('qexsd_read_bands_info',&
&         ' TWO_FERMI_ENERGIES=.true. Action: run pwscf without fixed_magnetization.',1)
      ELSE
        CALL iotk_scan_dat( iunit, "FERMI_ENERGY", ef_, IERR=ierr )
        IF (ierr/=0) RETURN
      ENDIF
      !
      CALL iotk_scan_end( iunit, "BAND_STRUCTURE_INFO", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( nbnd ) )             nbnd           = nbnd_
      IF ( PRESENT( num_k_points ) )     num_k_points   = num_k_points_
      IF ( PRESENT( nspin ) )            nspin          = nspin_
      IF ( PRESENT( noncolin ) )         noncolin       = noncolin_
      IF ( PRESENT( natomwfc ) )         natomwfc       = natomwfc_
      IF ( PRESENT( ef ) )               ef             = ef_
      IF ( PRESENT( nelec ) )            nelec          = nelec_
      IF ( PRESENT( energy_units ) )     energy_units   = TRIM( energy_units_ )
      IF ( PRESENT( k_units ) )          k_units        = TRIM( k_units_ )
      !
    END SUBROUTINE qexsd_read_bands_info
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_bands( ik, ispin, nbnd, eig, energy_units, occ, ef, ierr )
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
         filename= TRIM( qexsd_wfc_filename( datadir_in, 'eigenval', &
                                             ik, ispin, EXTENSION="xml") ) 
         !
      ELSE
         !
         filename= TRIM( qexsd_wfc_filename( datadir_in, 'eigenval', &
                                             ik, EXTENSION="xml") ) 
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
      IF ( PRESENT( ef )) THEN
         !
         CALL iotk_scan_dat( iunaux, "FERMI_ENERGY", ef, IERR=ierr )
         IF (ierr/=0)  RETURN
         !
      ENDIF
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
      IF ( PRESENT( occ ) )              occ  (1:nbnd_ )  = occ_(:)
      IF ( PRESENT( eig ) )              eig  (1:nbnd_ )  = eig_(:)
      !
      DEALLOCATE( occ_ )
      DEALLOCATE( eig_ )
      !
    END SUBROUTINE qexsd_read_bands
    !
    !
    !------------------------------------------------------------------------
    SUBROUTINE qexsd_read_wfc( ibnds, ibnde, ik, ispin, ipol, igk, ngw, igwx, &
                               wf, wf_kindip, ierr )
      !------------------------------------------------------------------------
      !
      ! read wfc from IBNDS to IBNDE, for kpt IK and spin ISPIN
      ! WF is the wfc on its proper k+g grid, while WF_KINDIP is the same wfc
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
      !
      ierr = 0
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
         filename = TRIM( qexsd_wfc_filename( datadir_in, 'evc', ik, ispin ) ) 
         !
      ELSEIF ( PRESENT( ipol )  ) THEN
         !
         filename = TRIM( qexsd_wfc_filename( datadir_in, 'evc', ik, ipol ) ) 
         !
      ELSE
         !
         filename = TRIM( qexsd_wfc_filename( datadir_in, 'evc', ik ) ) 
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
    END SUBROUTINE qexsd_read_wfc
    !
end module qexsd_module

