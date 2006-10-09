!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
! pw_export XML format and subroutine output format:
! --------------------------------------------------
! alat             : au                           
! a1(3)/b1(3)      : au, cartesian                
! symmop(3,3,nsym) : units of a1/a2/a3, transposed
! kpt(3,nktot)     : 2pi/a units, cartesian, real 
! igvec(3,ngvec)   : integer units of b1/b2/b3    
! en(nbnd,nktot)   : Rydberg
! igk(npwx,nktot)
!
! Output units are kept consistent with PW units (pw_export units)
! Otherwise might have problem with scaling with a parameter which 
! hasn't been read yet (like alat)
!
module pw_interface
  use pars
  use iotk_module
  implicit none
  save

  integer, parameter, private:: nstrx = 300

  type iotk_dim
     integer :: nbnd, nktot, nspin, ngvec, npwx, nsym
  end type iotk_dim

  type iotk_cell
     real(SP) :: alat, tpiba, at(3,3), bg(3,3)
  end type iotk_cell

  type(iotk_cell)         :: pwcell
  type(iotk_dim)          :: pwdim

!   PWSCF data
  logical                 :: invsym
  real(SP),   allocatable :: kpt(:,:)
  real(SP),   allocatable :: wk(:)
  integer,     allocatable:: igvec(:,:)
  real(SP),   allocatable :: eig(:,:)
  integer,     allocatable:: pw_npwk(:)
  integer,     allocatable:: pw_igk(:,:)
  complex(SP),allocatable :: pw_evec(:,:) ! check 
  integer,  allocatable   :: isym(:,:,:)
  real(SP)                :: nelec, pw_Tel

  type pw_string
     character(nstrx)          :: attr
     character(nstrx)          :: units
  end type pw_string

  public                  :: invsym
  public                  :: kpt,wk,igvec,eig,pw_npwk,pw_igk,isym
  public                  :: nelec, pw_Tel
  public                  :: pw_evec

  public :: pw_openindex
  public :: pw_closeindex
  public :: pw_dimensions
  public :: pw_cell
  public :: pw_symmetry
  public :: pw_gvectors
  public :: pw_igkindex
  public :: pw_eigenvalues
  public :: pw_kpoints
  public :: pw_other
  public :: pw_wfcstart
  public :: pw_wfcstop
  public :: pw_wfck

contains 

  subroutine pw_openindex(pwunit,pwfilename)
    implicit none

    integer, intent(in) :: pwunit  
    integer :: ierr 
    character(*), intent(in) :: pwfilename

    call iotk_open_read(pwunit,file=pwfilename,ierr=ierr)

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
    call iotk_scan_attr(attr,'nktot',pwdim%nktot,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nktot',ABS(ierr))
    call iotk_scan_attr(attr,'nspin',pwdim%nspin,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nspin',ABS(ierr))
    call iotk_scan_empty(pwunit,'Bands',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Bands',ABS(ierr))
    call iotk_scan_attr(attr,'nbnd',pwdim%nbnd,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find nbnd',ABS(ierr))
    call iotk_scan_empty(pwunit,'Main_grid',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Main_grid',ABS(ierr))
    call iotk_scan_attr(attr,'npw',pwdim%ngvec,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find npw',ABS(ierr))
    call iotk_scan_empty(pwunit,'Wfc_grid',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Wfc_grid',ABS(ierr))
    call iotk_scan_attr(attr,'npwx',pwdim%npwx,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find npwx',ABS(ierr))
    call iotk_scan_empty(pwunit,'Symmops',ATTR=attr,ierr=ierr)
        if (ierr/=0) call errore('pw_dimensions','Unable to find Symmops',ABS(ierr))
    call iotk_scan_attr(attr,'nsym',pwdim%nsym,ierr=ierr)
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
    call iotk_scan_attr(attr,'nelec',nelec,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find nelec',ABS(ierr))
    call iotk_scan_end(pwunit,'Other_parameters',ierr=ierr)
        if (ierr/=0) call errore('pw_other','Unable to end tag Other_p',ABS(ierr))
    return
  end subroutine pw_other

  subroutine pw_cell(pwunit)
    implicit none
    integer :: ierr
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr    

    call iotk_scan_begin(pwunit,'Cell',IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find tag Cell',ABS(ierr))
    call iotk_scan_empty(pwunit,'Data',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find Data',ABS(ierr))
    call iotk_scan_attr(attr,'alat',pwcell%alat,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find alat',ABS(ierr))
    call iotk_scan_attr(attr,'tpiba',pwcell%tpiba,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find tpiba',ABS(ierr))
    call iotk_scan_empty(pwunit,'a1',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find a1',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',pwcell%at(1,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'a2',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find a2',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',pwcell%at(2,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'a3',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find a3',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',pwcell%at(3,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'b1',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find b1',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',pwcell%bg(1,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'b2',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find b2',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',pwcell%bg(2,:),IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find xyz',ABS(ierr))
    call iotk_scan_empty(pwunit,'b3',ATTR=attr,IERR=ierr)
        if (ierr/=0) call errore('pw_cell','Unable to find b3',ABS(ierr))
    call iotk_scan_attr(attr,'xyz',pwcell%bg(3,:),IERR=ierr)
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

    if(.not.allocated(isym)) then
       call errore('pw_symmetry','isym not allocated',0)
    endif

    call iotk_scan_begin(pwunit,'Symmetry',IERR=ierr)
    if (ierr/=0) call errore('pw_symmetry','Unable to find tag Symmop',ABS(ierr))
    call iotk_scan_empty(pwunit,'symmops',ATTR=attr,ierr=ierr)
    call iotk_scan_attr(attr,'nsym',pwdim%nsym,ierr=ierr)
    call iotk_scan_attr(attr,'invsym',invsym,ierr=ierr)

    DO is = 1, pwdim%nsym

       call iotk_scan_dat(pwunit,"sym"//TRIM(iotk_index(is)),isym(1:3,1:3,is),ierr=ierr)
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

    if(.not.allocated(igvec)) then
       call errore('pw_gvectors','igvec not allocated',0)
    endif

    call iotk_scan_begin(pwunit,'Main_grid',IERR=ierr)
    if (ierr/=0) call errore('pw_gvectors','Unable to find tag Main_grid',ABS(ierr))
    call iotk_scan_dat(pwunit,'g',igvec(:,:),IERR=ierr)
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

    if(.not.allocated(eig)) then
       call errore('pw_eigenvalues','eig not allocated',0)
    endif

    call iotk_scan_begin(pwunit,'Eigenvalues',ATTR=attr,IERR=ierr)
        if (ierr /= 0) call errore('pw_eigenvalues','Unable to find tag Eigenvalues',ABS(ierr))
    call iotk_scan_attr(attr,'units',units,IERR=ierr)
        if (ierr /= 0) call errore('pw_eigenvalues','Unable to find Units',ABS(ierr))
    if(units.ne.'Rydberg') call errore('pw_eigenvalues','Units are not Rydberg.',ABS(ierr))
    do ik=1,pwdim%nktot
       call iotk_scan_dat(pwunit,"e"//iotk_index(ik),eig(:,ik),IERR=ierr)
         if (ierr /= 0) call  errore('pw_eigenvalues','Wrong format in e.dat',ABS(ierr))
    enddo

    call iotk_scan_end(pwunit,'Eigenvalues',IERR=ierr)

    return
  end subroutine pw_eigenvalues

  subroutine pw_allocwfc(dimwinx)
    implicit none
    integer :: dimwinx
    integer :: ierr

    allocate(pw_evec(pwdim%npwx,dimwinx),stat=ierr) 

    return 
  end subroutine pw_allocwfc

  subroutine pw_alloc
    implicit none
    integer :: ierr

    allocate(kpt     (          3, pwdim%nktot ),stat=ierr)
    allocate(wk      ( pwdim%nktot ),stat=ierr)
    allocate(igvec   (          3, pwdim%ngvec ),stat=ierr)
    allocate(eig     ( pwdim%nbnd, pwdim%nktot ),stat=ierr)
    allocate(pw_igk  ( pwdim%npwx, pwdim%nktot ),stat=ierr) 
    allocate(pw_npwk ( pwdim%nktot ),stat=ierr)
    allocate(isym    ( 3, 3, pwdim%nsym),stat=ierr)

!   allocate(pw_igk  ( pwdim%ngvec,pwdim%nktot ),stat=ierr) 
    return 
  end subroutine pw_alloc
  subroutine pw_dealloc
    implicit none
    integer :: ierr

    deallocate(kpt,wk)
    deallocate(igvec)
    deallocate(eig)
    deallocate(pw_igk) 
    deallocate(pw_npwk)
    deallocate(isym)

    return 
  end subroutine pw_dealloc

  subroutine pw_igkindex(pwunit)
    implicit none
    integer :: ierr,ik,npw
    integer, intent(in) :: pwunit  
    CHARACTER(nstrx)    :: attr  
    integer, allocatable :: kindex(:) 

    call iotk_scan_begin(pwunit,'Wfc_grids',IERR=ierr)
    if (ierr/=0) call errore('pw_igkindex','Unable to find tag WFC_grids',ABS(ierr))

    if(.not.allocated(pw_igk)) then
       call errore('pw_igkindex','pw_igk not allocated',0)
    endif
    if(.not.allocated(pw_npwk)) then
       call errore('pw_igkindex','npwk not allocated',0)
    endif

    do ik=1,pwdim%nktot
       call iotk_scan_begin(pwunit,"Kpoint"//TRIM(iotk_index(ik)),ATTR=attr,IERR=ierr)
       if ( ierr/= 0) call errore("Unable to find tag Kpoint"//TRIM(iotk_index(ik)),"read_kgrid",ABS(ierr))
       call iotk_scan_attr(attr,"npw",npw,IERR=ierr)
       if ( ierr/= 0) call errore("Wrong input format in NPW","read_kgrid",ABS(ierr) )
       pw_npwk(ik) = npw
!DEBUG
!      write(*,*) "CDH:",ik,pw_npwk(ik)
!END DEBUG
       allocate( kindex(npw), STAT=ierr )
       if ( ierr/=0 ) call errore('pw_igkindex','Unable allocate kindex',ABS(ierr))
       call iotk_scan_dat(pwunit,'index',kindex(:),IERR=ierr)
       if ( ierr/= 0) call errore("Wrong input format in INDEX","read_kgrid",ABS(ierr) )
       pw_igk(1:npw,ik) = kindex(:) 
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
    if ( idum /= pwdim%nbnd ) call errore('pw_wfck','Invalid nbnd',6) ! Check nbnd is consistent with header (dimensions).

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
    if ( idum /= pwdim%nbnd ) call errore('pw_wfck','Invalid nbnd',6) ! Check nbnd is consistent with header (dimensions).

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
       pw_evec( npw+1:pwdim%npwx, index) = CZERO
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

    if(.not.allocated(kpt)) then
       call errore('pw_kpoints','kpt not allocated',0)
    endif
    if(.not.allocated(wk)) then
       call errore('pw_kpoints','wk not allocated',0)
    endif
!     
    call iotk_scan_begin(pwunit,'Kmesh',ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to find tag Kmesh',ABS(ierr))
    call iotk_scan_dat(pwunit,'weights',wk(:),ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to find k',ABS(ierr))
    call iotk_scan_dat(pwunit,'k',kpt(:,:),ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to find k',ABS(ierr))
    call iotk_scan_end(pwunit,'Kmesh',ierr=ierr)
    if (ierr/=0) call errore('pw_kpoints','Unable to end tag Kmesh',ABS(ierr))

    return
  end subroutine pw_kpoints

end module pw_interface
