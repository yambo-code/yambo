!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, CH
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
module pw_read_pseudo_m
  !----------------------------------------------------------------------------
  !
  use pars,        ONLY: SP
  use iotk_module
  implicit none
  !
  private
  save
  !
  character(iotk_attlenx) :: attr, root
  integer                 :: iun
  !
  public :: read_pseudo_header, read_pseudo_data

contains

!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!

!---------------------------------------------------------------------------------
subroutine read_pseudo_header(psfile,Z,nmesh,nbeta,psp_type,psp_has_so,psp_has_nlcc) 
  !---------------------------------------------------------------------------------
  implicit none
  character(len=*),  intent(in)  :: psfile
  integer,           intent(out) :: nmesh,nbeta
  real(SP),          intent(out) :: Z
  character(*), optional, intent(out) :: psp_type
  logical,      optional, intent(out) :: psp_has_so
  logical,      optional, intent(out) :: psp_has_nlcc
  !
  character(256) :: psp_type_
  integer :: ierr
  logical :: psp_has_so_,psp_has_nlcc_
  !
  call read_upf_header_v2( psfile, Z, nmesh, nbeta, psp_type_, &
&                          psp_has_so_, psp_has_nlcc_, ierr )
  !
  if(ierr>0) call read_upf_header_v1( psfile, z, nmesh, nbeta, psp_type_, &
&                 psp_has_nlcc_, ierr )
  !
  if(ierr>0) call errore('read_pseudo_header', 'opening file: '//trim(psfile),1)
  !
  if(present(psp_type))     psp_type=trim(psp_type_)
  if(present(psp_has_so))   psp_has_so=psp_has_so_
  if(present(psp_has_nlcc)) psp_has_nlcc=psp_has_nlcc_
  !
end subroutine read_pseudo_header

!---------------------------------------------------------------------------------
subroutine read_upf_header_v2( psfile, z, nmesh, nbeta, psp_type,  psp_has_so, psp_has_nlcc, ierr )
  !---------------------------------------------------------------------------------
  implicit none
  !
  character(len=*), intent(in)  :: psfile
  character(len=*), intent(out) :: psp_type
  integer,          intent(out) :: nmesh,nbeta
  real(SP),         intent(out) :: Z
  logical,          intent(out) :: psp_has_so
  logical,          intent(out) :: psp_has_nlcc
  !
  integer :: ierr,ierr_
  logical :: found, tvanp
  character(20) :: psp_type_    ! Pseudo type ( NCPP | USPP | PAW )
  !
  ierr=0
  !
  call iotk_free_unit(iun)
  call iotk_open_read(iun, FILE=trim(psfile), ATTR=attr, ROOT=root, IERR=ierr_ )
  !
  if( (abs(ierr_)>0) .or. .not. matches('UPF',root) ) THEN
     call iotk_close_read(iun, ierr=ierr_)
     ierr = 1 ! have to force error to make v1 called
     return
  endif
  !
  call iotk_scan_begin(iun,'PP_INFO',found=found)
  if(found) call iotk_scan_end(iun,'PP_INFO')
  !
  call iotk_scan_empty(iun, 'PP_HEADER', attr=attr)
  call iotk_scan_attr(attr, 'pseudo_type', psp_type_)
  !
  psp_type=trim(psp_type_)
  if( trim(psp_type_) == "NC" .or. trim(psp_type_) == "SL" .or. &
&     trim(psp_type_) == "NL") psp_type="NCPP"
  !
  CALL iotk_scan_attr(attr, 'is_ultrasoft',   tvanp) 
  CALL iotk_scan_attr(attr, 'z_valence',      z)
  !
  CALL iotk_scan_attr(attr, 'has_so', psp_has_so)
  CALL iotk_scan_attr(attr, 'core_correction', psp_has_nlcc)
  CALL iotk_scan_attr(attr, 'mesh_size',      nmesh)
  CALL iotk_scan_attr(attr, 'number_of_proj', nbeta)
  !
  call iotk_close_read(iun, ierr=ierr)
  !
end subroutine read_upf_header_v2

!---------------------------------------------------------------------------------
subroutine read_upf_header_v1( psfile, Z, nmesh, nbeta, psp_type, psp_has_nlcc, ierr )
  !---------------------------------------------------------------------------------
  implicit none
  character(*),   intent(in)  :: psfile
  real(SP),       intent(out) :: Z
  integer,        intent(out) :: nmesh,nbeta
  character(*),   intent(out) :: psp_type
  logical,        intent(out) :: psp_has_nlcc
  integer,        intent(out) :: ierr
  !
  character(50) :: dummy
  logical :: ldummy
  integer :: i,idummy

  open(iun,file=trim(psfile),status='old',form='formatted', iostat=ierr)
  if (ierr/=0) return
  !
  rewind(iun)
  call iotk_scan_begin(iun, "PP_HEADER",ierr=ierr)
  if (ierr/=0) return
  !
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) psp_type
  if (ierr/=0) return
  if (trim(psp_type)=="NC") psp_type="NCPP"
  !
  read(iun,*,iostat=ierr) psp_has_nlcc
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) z
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) nmesh
  if (ierr/=0) return
  read(iun,*,iostat=ierr) idummy,nbeta
  if (ierr/=0) return
  read(iun,'(a)') dummy
  !
  do i = 1, idummy
     read (iun, * ) 
  enddo
  !
  call iotk_scan_end(iun, "PP_HEADER",ierr=ierr)
  if (ierr/=0) return
  rewind(iun) 
  close(iun)
  !
end subroutine read_upf_header_v1

!-----------------------------------------------------------------------
logical function matches (string1, string2)
  !-----------------------------------------------------------------------
  !
  ! .true. if string 1 is contained in string2, .false. otherwise
  !
  implicit none
  character (len=*) :: string1, string2
  integer :: len1, len2, l

  len1 = len_trim(string1)
  len2 = len_trim(string2)
  do l = 1, len2 - len1 + 1
    if (string1 (1:len1) .eq.string2 (l:l + len1 - 1) ) then
      matches = .true.
      return
    endif
  enddo

  matches = .false.
  return
end function matches

!-----------------------------------------------------------------------
subroutine read_pseudo_data(psfile,nmesh,nbeta,z,mesh,wmesh,rho_nlcc,vloc,&
&                           lbeta,jbeta,mesh_beta,beta,d,psp_has_so,psp_has_nlcc) 
!-----------------------------------------------------------------------
  IMPLICIT NONE
  !
  CHARACTER(LEN=*), INTENT(IN)    :: psfile
  INTEGER,          INTENT(INOUT) :: nmesh,nbeta
  REAL(SP),         INTENT(OUT)   :: z
  REAL(SP),         INTENT(OUT)   :: mesh(:),wmesh(:),rho_nlcc(:),vloc(:),beta(:,:),d(:),jbeta(:)
  INTEGER,          INTENT(OUT)   :: lbeta(:),mesh_beta(:)
  LOGICAL,          INTENT(INOUT) :: psp_has_so,psp_has_nlcc
  !
  call read_pseudo_header(psfile, Z, nmesh, nbeta, psp_has_so=psp_has_so, psp_has_nlcc=psp_has_nlcc)
  !
  call read_pseudo_parameters(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc, &
&                             wmesh,rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d)
  !
end subroutine read_pseudo_data

!-----------------------------------------------------------------------
subroutine read_pseudo_parameters(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc,wmesh,&
&                                 rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d)
!-----------------------------------------------------------------------
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN)  :: psfile
  LOGICAL,          INTENT(IN)  :: psp_has_so
  LOGICAL,          INTENT(IN)  :: psp_has_nlcc
  INTEGER,          INTENT(IN)  :: nmesh,nbeta
  REAL(SP),         INTENT(OUT) :: mesh(:), wmesh(:), rho_nlcc(:), vloc(:), beta(:,:), d(:), jbeta(:)
  INTEGER,          INTENT(OUT) :: lbeta(:),mesh_beta(:)
  !
  INTEGER :: ierr
  
  CALL read_upf_parameters_v2(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc, &
&                             wmesh,rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d,ierr)
  !
  if(ierr>0) then
    !
    rewind(iun)
    jbeta=0.
    call read_upf_parameters_v1(psfile,nmesh,nbeta,mesh, psp_has_nlcc,wmesh, &
&                               rho_nlcc,vloc,lbeta,mesh_beta,beta,d,ierr)
  endif
  !
  if(ierr>0) CALL errore('read_pseudo_parameters', 'Cannot open file: '//TRIM(psfile),1)
  !
end subroutine

!-----------------------------------------------------------------------
subroutine read_upf_parameters_v1(psfile,nmesh,nbeta,mesh, psp_has_nlcc, &
&                                 wmesh,rho_nlcc,vloc,lbeta,mesh_beta,beta,d,ierr)
  !-----------------------------------------------------------------------
  use interfaces,     ONLY:LINEAR_ALGEBRA_driver
  use linear_algebra, ONLY:DIAGO
  implicit none
  !
  character(len=*), intent(in)  :: psfile
  integer,          intent(in)  :: nmesh,nbeta
  logical,          intent(in)  :: psp_has_nlcc
  real(SP),         intent(out) :: mesh(:), wmesh(:), rho_nlcc(:), vloc(:), beta(:,:), d(:)
  integer,          intent(out) :: lbeta(:),mesh_beta(:)
  Integer,          intent(out) :: ierr
  !
  real(SP)      :: beta_tmp(nmesh,nbeta),d_tmp(nbeta,nbeta),rdummy
  complex(SP)   :: cd_tmp(nbeta,nbeta)
  character(50) :: dummy
  logical :: ldummy
  integer :: idummy,ibeta,ibetap,i_dij,n_dij,i1
  integer :: i, ios
  !
  ierr=0
  !
  call iotk_free_unit(iun)
  open(iun,file=trim(psfile),status='old',form='formatted', iostat=ierr)
  if (ierr/=0) return
  !
  rewind(iun)
  call iotk_scan_begin(iun,"PP_MESH",ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_begin(iun,"PP_R",ierr=ierr)
  if (ierr/=0) return
  read(iun,*,iostat=ierr) mesh
  if (ierr/=0) return
  !
  call iotk_scan_end  (iun,"PP_R",ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_begin(iun,"PP_RAB",ierr=ierr)
  if (ierr/=0) return
  read(iun,*,iostat=ierr) wmesh
  if (ierr/=0) return
  !
  call iotk_scan_end(iun,"PP_RAB",ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_end(iun,"PP_MESH",ierr=ierr)
  if (ierr/=0) return
  !
  ! NLCC
  !
  if (psp_has_nlcc) then
    rewind(iun)
    call iotk_scan_begin(iun,"PP_NLCC",ierr=ierr)
    if (ierr/=0) return
    read(iun,*,iostat=ierr) rho_nlcc
    if (ierr/=0) return
    call iotk_scan_end(iun,"PP_NLCC",ierr=ierr)
    if (ierr/=0) return
  else
    rho_nlcc(:)=0.0
  endif
  !
  ! Local data
  !
  rewind(iun)
  call iotk_scan_begin(iun,"PP_LOCAL",ierr=ierr)
  if (ierr/=0) return
  read(iun,*,iostat=ierr) vloc
  if (ierr/=0) return
  call iotk_scan_end(iun,"PP_LOCAL",ierr=ierr)
  if (ierr/=0) return
  !
  ! Nonlocal data
  !
  if(nbeta==0) then
    rewind(iun)
    close(iun)
    return
  endif 
  !
  rewind(iun)
  call iotk_scan_begin(iun,"PP_NONLOCAL",ierr=ierr)
  if (ierr/=0) return
  !
  do ibeta=1,nbeta
    call iotk_scan_begin(iun,"PP_BETA",ierr=ierr)
    if (ierr/=0) return
    !
    read(iun,*,iostat=ierr) idummy,lbeta(ibeta)
    if (ierr/=0) return
    !
    read(iun,*,iostat=ierr) mesh_beta(ibeta)
    if (ierr/=0) return
    beta(:,ibeta) = 0.0
    read(iun,*,iostat=ierr) beta_tmp(1:mesh_beta(ibeta),ibeta)
    if (ierr/=0) return
    !
    call iotk_scan_end(iun,"PP_BETA",ierr=ierr)
    if (ierr/=0) return
  enddo
  !
  call iotk_scan_begin(iun,"PP_DIJ",ierr=ierr)
  if (ierr/=0) return
  read(iun,*,iostat=ierr) n_dij
  if (ierr/=0) return
  !
  d_tmp=0._SP
  do i_dij=1,n_dij
    !
    read(iun,*,iostat=ierr) ibeta,ibetap,rdummy
    if (ierr/=0) return
    !
    d_tmp(ibeta ,ibetap)=rdummy
    d_tmp(ibetap,ibeta )=rdummy
  enddo
  call iotk_scan_end(iun,"PP_DIJ",ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_end(iun,"PP_NONLOCAL",ierr=ierr)
  if (ierr/=0) return
  !
  if(nbeta>1) then
    cd_tmp=cmplx(d_tmp,0._SP)
    call LINEAR_ALGEBRA_driver(DIAGO,M=cd_tmp,E_real=d)
    d_tmp=real(cd_tmp)
  else
    d=d_tmp(1,1)
    d_tmp(1,1)=1._SP
  endif
  !
  lbeta=nint(matmul(transpose(d_tmp)**2,real(lbeta)))
  forall (i1=1:nmesh) beta(i1,:)=matmul(transpose(d_tmp),beta_tmp(i1,:))
  !
  close(iun)
  !
end subroutine

!-----------------------------------------------------------------------
subroutine read_upf_parameters_v2(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc, &
&                                 wmesh,rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d,ierr)
  !-----------------------------------------------------------------------
  !
  use interfaces,     ONLY:LINEAR_ALGEBRA_driver
  use linear_algebra, ONLY:DIAGO
  use stderr,         ONLY:intc
  implicit none
  !
  CHARACTER(LEN=*), INTENT(IN)  :: psfile
  LOGICAL,          INTENT(IN)  :: psp_has_so
  LOGICAL,          INTENT(IN)  :: psp_has_nlcc
  INTEGER,          INTENT(IN)  :: nmesh,nbeta
  REAL(SP),         INTENT(OUT) :: mesh(:), wmesh(:), rho_nlcc(:), vloc(:), beta(:,:), d(:), jbeta(:)
  INTEGER,          INTENT(OUT) :: lbeta(:),mesh_beta(:)
  INTEGER,          INTENT(OUT) :: ierr
  !
  real(SP)    :: beta_tmp(nmesh,nbeta),d_tmp(nbeta,nbeta)
  complex(SP) :: cd_tmp(nbeta,nbeta)
  INTEGER     :: i,i1,ibeta,ierr_
  !
  ierr=0
  !
  call iotk_free_unit(iun)
  CALL iotk_open_read(iun, FILE = TRIM(psfile), attr=attr, root=root, ierr=ierr_)
  !
  if( ierr_/=0 .or. .not. matches('UPF',root) ) then
     call iotk_close_read(iun, ierr=ierr_)
     ierr = 1
     return
  endif
  ! 
  !  data needed: PP_MESH, PP_R, PP_RAB, PP_LOCAL,PP_NONLOCAL,PP_BETA,PPDJ 
  !
  call iotk_scan_begin(iun, 'PP_MESH', attr=attr,ierr=ierr)
  if (ierr/=0) return
  !
  call iotk_scan_dat(iun, 'PP_R', mesh, ierr=ierr)
  if (ierr/=0) return
  !
  call iotk_scan_dat(iun, 'PP_RAB', wmesh, ierr=ierr)
  if (ierr/=0) return
  !
  call iotk_scan_end(iun, 'PP_MESH', ierr=ierr)
  if (ierr/=0) return
  !
  if (psp_has_nlcc) then
    call iotk_scan_dat(iun, 'PP_NLCC', rho_nlcc, ierr=ierr)
    if (ierr/=0) return
  else 
    rho_nlcc = 0.0
  endif
  !
  call iotk_scan_dat(iun, 'PP_LOCAL', vloc, ierr=ierr)
  if (ierr/=0) return
  !
  if (nbeta>0) then
    !
    CALL iotk_scan_begin(iun, 'PP_NONLOCAL', ierr=ierr)
    !
    do ibeta=1,nbeta
      call iotk_scan_dat(iun, 'PP_BETA.'//intc(ibeta), beta_tmp(:,ibeta), attr=attr,ierr=ierr)
      if (ierr/=0) return
      call iotk_scan_attr(attr, 'size',  mesh_beta(ibeta),ierr=ierr)
      if (ierr/=0) return
      call iotk_scan_attr(attr, 'angular_momentum',  lbeta(ibeta),ierr=ierr)
      if (ierr/=0) return
    enddo
    !
    call iotk_scan_dat(iun, 'PP_DIJ', d_tmp , attr=attr,ierr=ierr)
    if (ierr/=0) return
    !
    if(nbeta>1) then
      cd_tmp=cmplx(d_tmp,0._SP)
      call LINEAR_ALGEBRA_driver(DIAGO,M=cd_tmp,E_real=d)
      d_tmp=real(cd_tmp)
    else
      d=d_tmp(1,1)
      d_tmp(1,1)=1._SP
    endif
    !
    lbeta=nint(matmul(transpose(d_tmp)**2,real(lbeta)))
    forall (i1=1:nmesh) beta(i1,:)=matmul(transpose(d_tmp),beta_tmp(i1,:))
    !
    call iotk_scan_end(iun, 'PP_NONLOCAL', ierr=ierr)
    if (ierr/=0) return
    !
  endif
  !
  if(psp_has_so) then
    !
    call iotk_scan_begin(iun, 'PP_SPIN_ORB', ierr=ierr)
    if (ierr/=0) return
    !
    do ibeta = 1,nbeta
      call iotk_scan_empty(iun, 'PP_RELBETA.'//intc(ibeta),attr=attr, ierr=ierr)
      if (ierr/=0) return
      call iotk_scan_attr(attr, 'jjj', jbeta(ibeta), ierr=ierr)
      if (ierr/=0) return
    enddo
    !
    jbeta=matmul(transpose(d_tmp)**2,jbeta)
    !
    call iotk_scan_end(iun, 'PP_SPIN_ORB', ierr=ierr)
    if (ierr/=0) return
    !
  endif
  !
  call iotk_close_read(iun, ierr=ierr)
  if (ierr/=0) return
  ! 
end subroutine
    !
end module pw_read_pseudo_m

