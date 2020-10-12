!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CH
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
module pw_export_module
  use pw_data
  use pars
  use iotk_module
  implicit none
  private
  save

  integer, parameter :: nstrx = 300

! PWSCF data
  complex(SP),allocatable :: pw_evec(:,:) ! check 
  public                  :: pw_evec
 
  CHARACTER(iotk_attlenx) :: attr

  public :: pw_openindex, pw_closeindex, pw_dimensions
  public :: pw_cell, pw_symmetry, pw_kpoints, pw_other
  public :: pw_gvectors, pw_igkindex, pw_eigenvalues
  public :: pw_wfcstart, pw_wfcstop, pw_wfck, pw_atoms
  public :: qexpt_read_planewaves, qexpt_read_header
  public :: fmt_is_pwexport

contains 

  subroutine pw_openindex(pwunit,pwfilename,ierr)
    implicit none

    integer,      intent(in) :: pwunit  
    character(*), intent(in) :: pwfilename
    integer, optional, intent(out) :: ierr 
    !
    integer :: ierr_

    call iotk_open_read(pwunit,file=pwfilename,ierr=ierr_)
    if (present(ierr)) ierr=ierr_

    return
  end subroutine pw_openindex

  subroutine pw_closeindex(pwunit)
    implicit none

    integer, intent(in) :: pwunit  
    integer :: ierr 

    call iotk_close_read(pwunit,ierr=ierr)

    return
  end subroutine pw_closeindex

  subroutine pw_dimensions(pwunit)
    implicit none
    
    integer :: ierr
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    

    call iotk_scan_begin(pwunit,'Dimensions',ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find tag Dimensions',ABS(ierr))
    call iotk_scan_empty(pwunit,'Kpoints',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Kpoints',ABS(ierr))
    call iotk_scan_attr(attr,'nktot',num_k_points_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nktot',ABS(ierr))
    call iotk_scan_attr(attr,'nspin',n_spin_pw_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nspin',ABS(ierr))
    call iotk_scan_empty(pwunit,'Bands',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Bands',ABS(ierr))
    call iotk_scan_attr(attr,'nbnd',nbnd_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nbnd',ABS(ierr))
    call iotk_scan_empty(pwunit,'Main_grid',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Main_grid',ABS(ierr))
    call iotk_scan_attr(attr,'npw',ngm_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find npw',ABS(ierr))
    call iotk_scan_empty(pwunit,'Wfc_grid',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Wfc_grid',ABS(ierr))
    call iotk_scan_attr(attr,'npwx',npwx_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find npwx',ABS(ierr))

    call iotk_scan_empty(pwunit,'Atoms',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Atoms',ABS(ierr))
    call iotk_scan_attr(attr,'natoms',nat_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find natoms',ABS(ierr))

    call iotk_scan_empty(pwunit,'Symmops',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Symmops',ABS(ierr))
    call iotk_scan_attr(attr,'nsym',nsym_,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nsym',ABS(ierr))
    call iotk_scan_end(pwunit,'Dimensions',ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to end tag Dimensions',ABS(ierr))

    return
  end subroutine pw_dimensions

  subroutine pw_other(pwunit)
    implicit none
    integer :: ierr
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    
    call iotk_scan_begin(pwunit,'Other_parameters',IERR=ierr)
        if (ierr/=0) call errore('pw_other','Unable to find tag Other_p',ABS(ierr))
    call iotk_scan_empty(pwunit,'Charge',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find Charge',ABS(ierr))
    call iotk_scan_attr(attr,'nelec',nelec_,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find nelec',ABS(ierr))
    call iotk_scan_end(pwunit,'Other_parameters',ierr=ierr)
        if (ierr/=0) call errore('pw_other','Unable to end tag Other_p',ABS(ierr))
    return
  end subroutine pw_other

  subroutine pw_atoms(pwunit)
    implicit none
    integer :: ierr, natoms, iat
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    

    call iotk_scan_begin(pwunit,'Atoms',IERR=ierr)
        if (ierr/=0) call errore('pw_atoms','Unable to find tag Atoms',ABS(ierr))
    call iotk_scan_empty(pwunit,'Data',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_atoms','Unable to find Data',ABS(ierr))
    call iotk_scan_attr(attr,'natoms',natoms,IERR=ierr)

    call iotk_scan_attr(attr,'nspecies',nsp_,IERR=ierr)
        if (ierr/=0) call errore('pw_atoms','Unable to find nspecies',ABS(ierr))

    call iotk_scan_begin(pwunit,'Positions',IERR=ierr)
    do iat = 1, natoms
      call iotk_scan_empty(pwunit,'atom'//TRIM(iotk_index(iat)),ATTR=attr,IERR=ierr)
      if (ierr/=0) call errore('pw_atoms','Unable to find atom'//TRIM(iotk_index(iat)),ABS(ierr))
      call iotk_scan_attr(attr,'type',atom_type_(iat),IERR=ierr)
      if (ierr/=0) call errore('pw_atoms','Unable to find type',ABS(ierr))
      call iotk_scan_attr(attr,'xyz',tau_(:,iat),IERR=ierr)
      if (ierr/=0) call errore('pw_atoms','Unable to find xyz',ABS(ierr))
    enddo
    call iotk_scan_end(pwunit,'Positions',IERR=ierr)

    call iotk_scan_begin(pwunit,'Types',IERR=ierr)
    do iat = 1, nsp_
      call iotk_scan_empty(pwunit,'specie'//TRIM(iotk_index(iat)),ATTR=attr,IERR=ierr)
      if (ierr/=0) call errore('pw_atoms','Unable to find atom'//TRIM(iotk_index(iat)),ABS(ierr))
      call iotk_scan_attr(attr,'type',species_type_(iat),IERR=ierr)
      if (ierr/=0) call errore('pw_atoms','Unable to find type',ABS(ierr))
    enddo
    call iotk_scan_end(pwunit,'Types',IERR=ierr)

    call iotk_scan_end(pwunit,'Atoms',IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to end tag Cell',ABS(ierr))
 
    return
  end subroutine pw_atoms


  subroutine pw_cell(pwunit)
    implicit none
    integer :: ierr
    integer, intent(in) :: pwunit  
     real(SP) :: tpiba, bg(3,3)
    CHARACTER(nstrx)    :: attr    

    call iotk_scan_begin(pwunit,'Cell',IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find tag Cell',ABS(ierr))
    call iotk_scan_empty(pwunit,'Data',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find Data',ABS(ierr))
    call iotk_scan_attr(attr,'alat', alat_,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find alat',ABS(ierr))
    call iotk_scan_attr(attr,'tpiba',tpiba,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find tpiba',ABS(ierr))
    call iotk_scan_empty(pwunit,'a1',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find a1',ABS(ierr))
    call iotk_scan_attr(attr,'xyz', a1_(:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'a2',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find a2',ABS(ierr))
    call iotk_scan_attr(attr,'xyz', a2_(:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'a3',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find a3',ABS(ierr))
    call iotk_scan_attr(attr,'xyz', a3_(:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'b1',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find b1',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',bg(1,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'b2',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find b2',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',bg(2,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'b3',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find b3',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',bg(3,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_end(pwunit,'Cell',IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to end tag Cell',ABS(ierr))
 
    return
  end subroutine pw_cell

  subroutine pw_symmetry(pwunit)
    implicit none
    integer :: ierr, is
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    

    call iotk_scan_begin(pwunit,'Symmetry',IERR=ierr)
    if (ierr/=0) call errore('pw_symmetry','Unable to find tag Symmop',ABS(ierr))
    call iotk_scan_empty(pwunit,'symmops',ATTR=attr,ierr=ierr)
    call iotk_scan_attr(attr,'nsym',nsym_,ierr=ierr)
    call iotk_scan_attr(attr,'invsym',invsym_,ierr=ierr)

    DO is = 1, nsym_

       call iotk_scan_dat(pwunit,"sym"//TRIM(iotk_index(is)),isym_(1:3,1:3,is),ierr=ierr)
       IF (ierr/=0) call errore('pw_symmetry','Unable to read sym'//TRIM(iotk_index(is)),ABS(ierr))
    ENDDO

    call iotk_scan_end(pwunit,'Symmetry',IERR=ierr)
    if (ierr/=0) call errore('pw_symmetry','Unable to find tag Symmetry',ABS(ierr))
    return
  end subroutine pw_symmetry

  subroutine pw_gvectors(pwunit)
    implicit none
    integer :: ierr
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    

    call iotk_scan_begin(pwunit,'Main_grid',IERR=ierr)
    if (ierr/=0) call errore('pw_gvectors','Unable to find tag Main_grid',ABS(ierr))
    call iotk_scan_dat(pwunit,'g',igv_(:,:),IERR=ierr)
    if (ierr/=0) call errore('pw_gvectors','Unable to find g',ABS(ierr))
    call iotk_scan_end(pwunit,'Main_grid',IERR=ierr)
    if (ierr/=0) call errore('pw_gvectors','Unable to find tag Main_grid',ABS(ierr))

    return
  end subroutine pw_gvectors

  subroutine pw_eigenvalues(pwunit)
    implicit none
    integer :: ierr,ik
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    
    CHARACTER(nstrx)    :: units    

    if(.not.allocated(eig_)) then
       call errore('pw_eigenvalues','eig not allocated',0)
    endif

    call iotk_scan_begin(pwunit,'Eigenvalues',ATTR=attr,IERR=ierr)
        if (ierr /= 0) call errore('pw_eigenvalues','Unable to find tag Eigenvalues',ABS(ierr))
    call iotk_scan_attr(attr,'units',units,IERR=ierr)
        if (ierr /= 0) call errore('pw_eigenvalues','Unable to find Units',ABS(ierr))
    if(units.ne.'Rydberg') call errore('pw_eigenvalues','Units are not Rydberg.',ABS(ierr))
    do ik=1,num_k_points_
       call iotk_scan_dat(pwunit,"e"//iotk_index(ik),eig_(:,ik),IERR=ierr)
         if (ierr /= 0) call  errore('pw_eigenvalues','Wrong format in e.dat',ABS(ierr))
    enddo

    call iotk_scan_end(pwunit,'Eigenvalues',IERR=ierr)

    return
  end subroutine pw_eigenvalues

  subroutine pw_allocwfc(dimwinx)
    implicit none
    integer :: dimwinx
    integer :: ierr

    allocate(pw_evec(npwx_,dimwinx),stat=ierr) 

    return 
  end subroutine pw_allocwfc

  subroutine pw_igkindex(pwunit)
    implicit none
    integer :: ierr,ik,npw
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr  
    integer, allocatable :: kindex(:) 

    call iotk_scan_begin(pwunit,'Wfc_grids',IERR=ierr)
    if (ierr/=0) call errore('pw_igkindex','Unable to find tag WFC_grids',ABS(ierr))

    if(.not.allocated(pw_igk_)) then
       call errore('pw_igkindex','pw_igk not allocated',0)
    endif
    if(.not.allocated(pw_npwk_)) then
       call errore('pw_igkindex','npwk not allocated',0)
    endif

    do ik=1,num_k_points_
       call iotk_scan_begin(pwunit,"Kpoint"//TRIM(iotk_index(ik)),ATTR=attr,IERR=ierr)
       if ( ierr/= 0) call errore("Unable to find tag Kpoint"//TRIM(iotk_index(ik)),"read_kgrid",ABS(ierr))
       call iotk_scan_attr(attr,"npw",npw,IERR=ierr)
       if ( ierr/= 0) call errore("Wrong input format in NPW","read_kgrid",ABS(ierr) )
       pw_npwk_(ik) = npw
       allocate( kindex(npw), STAT=ierr )
       if ( ierr/=0 ) call errore('pw_igkindex','Unable allocate kindex',ABS(ierr))
       call iotk_scan_dat(pwunit,'index',kindex(:),IERR=ierr)
       if ( ierr/= 0) call errore("Wrong input format in INDEX","read_kgrid",ABS(ierr) )
       pw_igk_(1:npw,ik) = kindex(:) 
       deallocate(kindex)
       call iotk_scan_end(pwunit,"Kpoint"//TRIM(iotk_index(ik)),IERR=ierr)
       if ( ierr/= 0) call errore("end tag Kpoint"//TRIM(iotk_index(ik)), &
                                 "read_kgrid",ABS(ierr))
    enddo
    call iotk_scan_end(pwunit,'Wfc_grids',IERR=ierr)
    if (ierr/=0) call errore('pw_igkindex','Unable to end tag Wfc_grids',ABS(ierr))

    return
  end subroutine pw_igkindex

  subroutine pw_wfcstart(pwunit) 
    implicit none
    integer, intent(in) :: pwunit
    integer :: ierr

    call iotk_scan_begin(pwunit,'Eigenvectors',IERR=ierr)
    if (ierr/=0)  call errore('pw_wfcstart','Unable to find Eigenvector',ABS(ierr))

    return
  end subroutine pw_wfcstart

  subroutine pw_wfcstop(pwunit) 
    implicit none
    integer, intent(in) :: pwunit
    integer :: ierr

    call iotk_scan_end(pwunit,'Eigenvectors',IERR=ierr)
    if (ierr/=0)  call errore('pw_wfcstop','Unable to find Eigenvector',ABS(ierr))

    return
  end subroutine pw_wfcstop

  subroutine pw_wfcscan(pwunit,ik) 
    implicit none
    integer, intent(in) :: pwunit, ik
    CHARACTER(nstrx)   :: attr
    integer :: ierr, idum

    call iotk_scan_begin(pwunit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
    if (ierr/=0)  call errore('pw_wfcscan','Unable to find Kpoint (vectors)',ik)
    call iotk_scan_empty(pwunit,'Info',ATTR=attr,IERR=ierr)
    if (ierr/=0)  call errore('pw_wfcscan','Unable to find Info',ik)
    call iotk_scan_attr(attr,'nbnd',idum,IERR=ierr)
    if (ierr/=0)  call errore('pw_wfcscan','Unable to find nbnd',ik)
    if ( idum /= nbnd_ ) call errore('pw_wfck','Invalid nbnd',6) ! Check nbnd is consistent with header (dimensions).

    return
  end subroutine pw_wfcscan

  subroutine pw_wfcread(pwunit,ib,npw,wtmp) 
    implicit none
    integer, intent(in) :: pwunit, ib
    integer, intent(in) :: npw
    integer :: ierr
    COMPLEX*16, intent(out) :: wtmp(npw)

       call iotk_scan_dat(pwunit,'Wfc'//TRIM(iotk_index(ib)), &
          wtmp(1:npw),IERR=ierr)
       if (ierr/=0)  call errore('pw_wfck','Unable to find Wfc',ABS(ierr))

    return
  end subroutine pw_wfcread
   
  subroutine pw_wfcscanend(pwunit,ik) 
    implicit none
    integer, intent(in) :: pwunit, ik
    integer :: ierr
    call iotk_scan_end(pwunit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
    if (ierr/=0)  call errore('pw_wfck','Unable to end tag Kpoint (vectors)',ik)
    return
  end subroutine pw_wfcscanend

  subroutine pw_wfck(pwunit,ik,npw,ibmin,ibmax) 
    implicit none
    integer, intent(in) :: pwunit
    integer, intent(in) :: npw
    CHARACTER(nstrx)   :: attr
    INTEGER            :: ik,ib, index, idum
    integer :: ierr
    integer :: ibmin,ibmax
    COMPLEX*16, ALLOCATABLE :: wtmp(:)
    COMPLEX*16, parameter:: czero=(0.d0,0.d0)


    if(.not.allocated(pw_evec)) then
       call errore('pw_wfck','evec not allocated',0)
    endif

    call iotk_scan_begin(pwunit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
    if (ierr/=0)  call errore('pw_wfck','Unable to find Kpoint (vectors)',ik)

    call iotk_scan_empty(pwunit,'Info',ATTR=attr,IERR=ierr)
    if (ierr/=0)  call errore('pw_wfck','Unable to find Info',ik)
    call iotk_scan_attr(attr,'nbnd',idum,IERR=ierr)
    if (ierr/=0)  call errore('pw_wfck','Unable to find nbnd',ik)
    if ( idum /= nbnd_ ) call errore('pw_wfck','Invalid nbnd',6) ! Check nbnd is consistent with header (dimensions).

    if(.not.allocated(pw_evec)) then
        write(*,*) "evec not allocated!"
       stop
    endif
    allocate(wtmp(npw),stat=ierr)
    if (ierr/=0) call errore('pw_wfck','allocating wtmp',ABS(ierr))

    DO ib=ibmin,ibmax ! In case of limited range of bands
! This is not implemented in pw_export...
       index = ib - ibmin +1
       call iotk_scan_dat(pwunit,'Wfc'//TRIM(iotk_index(ib)), &
          wtmp(1:npw),IERR=ierr)
       if (ierr/=0)  call errore('pw_wfck','Unable to find Wfc',ABS(ierr))
       pw_evec( 1:npw,index) = wtmp( 1:npw )
       pw_evec( npw+1:npwx_, index) = CZERO
    ENDDO

    call iotk_scan_end(pwunit,'Kpoint'//TRIM(iotk_index(ik)),IERR=ierr)
    if (ierr/=0)  call errore('pw_wfck','Unable to end tag Kpoint (vectors)',ik)
    deallocate( wtmp, STAT=ierr )
    if (ierr/=0) call errore('pw_wfck','deallocating wtmp',ABS(ierr))

    return
  end subroutine pw_wfck


  subroutine pw_kpoints(pwunit)
    implicit none
    integer :: ierr
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    

!   if(.not.allocated(kpt)) then
!      call errore('pw_kpoints','kpt not allocated',0)
!   endif
!   if(.not.allocated(wk)) then
!      call errore('pw_kpoints','wk not allocated',0)
!   endif
!     
    call iotk_scan_begin(pwunit,'Kmesh',ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to find tag Kmesh',ABS(ierr))
!   call iotk_scan_dat(pwunit,'weights',wk(:),ierr=ierr)
!   if (ierr/=0) call errore('pw_kpoints','Unable to find weights',ABS(ierr))
    call iotk_scan_dat(pwunit,'k',xk_(:,:),ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to find kpt',ABS(ierr))
    call iotk_scan_end(pwunit,'Kmesh',ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to end tag Kmesh',ABS(ierr))

    return
  end subroutine pw_kpoints
  !
  logical function fmt_is_pwexport(dirname)
     implicit none
     character(len=*)  :: dirname
     !
     integer :: iun, ierr
     character(256) :: filename, fmt_name
     
     fmt_is_pwexport=.false.
     call iotk_free_unit(iun)
     !
     filename=trim(dirname)//"/index.xml"
     ! 
     call pw_openindex(iun,filename,ierr=ierr)
     if (ierr/=0) return
     !
     call qexpt_read_header(iun, format_name=fmt_name, ierr=ierr)
     if (ierr/=0) return
     !
     if (trim(fmt_name)/="QEXPT") return
     !
     call pw_closeindex(iun)
     !
     fmt_is_pwexport=.true.
     return
  end function

    !------------------------------------------------------------------------
    SUBROUTINE qexpt_read_header( iunit, creator_name, creator_version, &
                                  format_name, format_version, ierr )
      !------------------------------------------------------------------------
      !
      IMPLICIT NONE
      INTEGER,                     INTENT(IN)  :: iunit
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: creator_name, creator_version
      CHARACTER(LEN=*),  OPTIONAL, INTENT(OUT) :: format_name, format_version
      INTEGER,                     INTENT(OUT) :: ierr

      CHARACTER(256) :: creator_name_, creator_version_
      CHARACTER(256) :: format_name_,     format_version_
      CHARACTER(nstrx)    :: attr

      ierr = 0
      !
      !
      CALL iotk_scan_begin( iunit, "Header", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunit, "format", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr(attr, "name", format_name_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr, "version", format_version_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunit, "creator", ATTR=attr, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr(attr, "name", creator_name_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr(attr, "version", creator_version_, IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunit, "Header", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT(creator_name) )     creator_name    = TRIM(creator_name_)
      IF ( PRESENT(creator_version) )  creator_version = TRIM(creator_version_)
      IF ( PRESENT(format_name) )      format_name     = TRIM(format_name_)
      IF ( PRESENT(format_version) )   format_version  = TRIM(format_version_)
      !
    END SUBROUTINE qexpt_read_header

    !------------------------------------------------------------------------
    SUBROUTINE qexpt_read_planewaves( iunit, ecutwfc, ecutrho, npwx, &
                                      nr1, nr2, nr3, ngm, igv, cutoff_units, ierr )
      !------------------------------------------------------------------------
      !
      INTEGER,                INTENT(IN)  :: iunit
      INTEGER,      OPTIONAL, INTENT(OUT) :: npwx, nr1, nr2, nr3, ngm
      INTEGER,      OPTIONAL, INTENT(OUT) :: igv(:,:)
      REAL(DP),     OPTIONAL, INTENT(OUT) :: ecutwfc, ecutrho
      CHARACTER(*), OPTIONAL, INTENT(OUT) :: cutoff_units
      INTEGER,                INTENT(OUT) :: ierr
      !
      INTEGER        :: npwx_, nr1_, nr2_, nr3_, ngm_
      REAL(DP)       :: ecutwfc_, ecutrho_
      CHARACTER(256) :: cutoff_units_
      !

      ierr = 0
      !
      ! ... dimensions
      !
      CALL iotk_scan_begin( iunit, "Other_parameters", IERR=ierr )
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunit, "Cutoff", ATTR=attr, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "wfc", ecutwfc_ ,IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "rho", ecutrho_ ,IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "units", cutoff_units_, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_empty( iunit, "Space_grid", ATTR=attr, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "nr1", nr1_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr2", nr2_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "nr3", nr3_, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_end( iunit, 'Other_parameters', IERR=ierr)
      IF (ierr/=0) RETURN

      !
      !
      ! ... Main G grid (density)
      !
      CALL iotk_scan_begin( iunit, "Main_grid", ATTR=attr, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_attr( attr, "npw", ngm_, IERR=ierr)
      IF (ierr/=0) RETURN
      !
      IF ( PRESENT( igv ) ) THEN
          !
          CALL iotk_scan_dat( iunit, "g", igv(1:3, 1:ngm_) , IERR=ierr)
          IF (ierr/=0) RETURN
          !
      ENDIF
      !
      CALL iotk_scan_end( iunit, "Main_grid", IERR=ierr)
      IF (ierr/=0) RETURN
      !
      CALL iotk_scan_begin( iunit, "Wfc_grids", ATTR=attr, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_attr( attr, "npwx", npwx_, IERR=ierr)
      IF (ierr/=0) RETURN
      CALL iotk_scan_end( iunit, "Wfc_grids", IERR=ierr)
      IF (ierr/=0) RETURN
      !
      !
      IF ( PRESENT( ecutwfc ) )           ecutwfc      = ecutwfc_
      IF ( PRESENT( ecutrho ) )           ecutrho      = ecutrho_
      IF ( PRESENT( npwx ) )              npwx         = npwx_
      IF ( PRESENT( nr1 ) )               nr1          = nr1_
      IF ( PRESENT( nr2 ) )               nr2          = nr2_
      IF ( PRESENT( nr3 ) )               nr3          = nr3_
      IF ( PRESENT( ngm ) )               ngm          = ngm_
      IF ( PRESENT( cutoff_units ) )      cutoff_units = TRIM( cutoff_units_ )
      !
    END SUBROUTINE qexpt_read_planewaves

end module pw_export_module

! pw_export XML format and subroutine output format:
! --------------------------------------------------
! alat             : au                           
! a1(3)/b1(3)      : au, cartesian                
! symmop(3,3,nsym) : units of a1/a2/a3, transposed
! kpt(3,nktot)     : 2pi/a units, cartesian, real 
! igv(3,ngvec)   : integer units of b1/b2/b3    
! en(nbnd,nktot)   : Rydberg
! igk(npwx,nktot)
!
! Output units are kept consistent with PW units (pw_export units)
! Otherwise might have problem with scaling with a parameter which 
! hasn't been read yet (like alat)
