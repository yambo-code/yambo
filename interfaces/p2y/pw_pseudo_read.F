!
!        Copyright (C) 2000-2019 the YAMBO team
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
module pw_pseudo_read_m
  !----------------------------------------------------------------------------
  !
  use pars,        ONLY: SP
  use iotk_module
  use pw_pseudo_module
  implicit none
  !
  private
  save
  !
  character(iotk_attlenx) :: attr, root
  integer                 :: iun
  !
  public :: pseudo_read_header, pseudo_read_data

contains

!
!-------------------------------------------
! ... basic (public) subroutines
!-------------------------------------------
!

!---------------------------------------------------------------------------------
subroutine pseudo_read_header(psfile,psp)
  !---------------------------------------------------------------------------------
  implicit none
  character(len=*),  intent(in)    :: psfile
  type(pw_pseudo),   intent(inout) :: psp
  !
  integer :: ierr
  !
  ierr=0
  call read_upf_header_v2( psfile, psp, ierr)
  !
  if (ierr/=0) then
     ierr=0
     call read_upf_header_v1( psfile, psp, ierr)
  endif
  !
  if(ierr/=0) call errore('pseudo_read_header', 'opening file: '//trim(psfile),1)
  !
end subroutine pseudo_read_header

!---------------------------------------------------------------------------------
subroutine read_upf_header_v2( psfile, psp, ierr )
  !---------------------------------------------------------------------------------
  implicit none
  !
  character(len=*), intent(in)    :: psfile
  type(pw_pseudo),  intent(inout) :: psp
  integer,          intent(out)   :: ierr
  !
  logical :: lfound
  character(20) :: psp_type    ! Pseudo type ( NCPP | USPP | PAW )
  !
  ierr=0
  !
  call iotk_free_unit(iun)
  call iotk_open_read(iun, FILE=trim(psfile), ATTR=attr, ROOT=root, IERR=ierr)
  !
  if( (abs(ierr)>0) .or. .not. matches('UPF',root) ) THEN
     call iotk_close_read(iun, ierr=ierr)
     ierr = 1 ! have to force error to make v1 called
     return
  endif
  !
  call iotk_scan_begin(iun,'PP_INFO',found=lfound,ierr=ierr)
  if (ierr/=0) return
  if(lfound) call iotk_scan_end(iun,'PP_INFO')
  !
  call iotk_scan_empty(iun, 'PP_HEADER', attr=attr,ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_attr(attr, 'pseudo_type', psp_type,ierr=ierr)
  if (ierr/=0) return
  !
  psp%is_ncpp=.false.
  if( trim(psp_type) == "NC" .or. trim(psp_type) == "SL" .or. &
&     trim(psp_type) == "NL") psp_type="NCPP"
  if (trim(psp_type) == "NCPP") psp%is_ncpp=.true.
  !
  CALL iotk_scan_attr(attr, 'is_ultrasoft',   psp%is_uspp, ierr=ierr) 
  if (ierr/=0) return
  CALL iotk_scan_attr(attr, 'is_paw',         psp%is_paw, ierr=ierr) 
  if (ierr/=0) return
  CALL iotk_scan_attr(attr, 'z_valence',      psp%Z, ierr=ierr)
  if (ierr/=0) return
  CALL iotk_scan_attr(attr, 'l_max',          psp%lmax, ierr=ierr)
  if (ierr/=0) return
  !
  CALL iotk_scan_attr(attr, 'has_so', psp%has_so, ierr=ierr)
  if (ierr/=0) return
  CALL iotk_scan_attr(attr, 'core_correction', psp%has_nlcc, ierr=ierr)
  if (ierr/=0) return
  CALL iotk_scan_attr(attr, 'mesh_size',      psp%nmesh, ierr=ierr)
  if (ierr/=0) return
  CALL iotk_scan_attr(attr, 'number_of_proj', psp%nbeta, ierr=ierr)
  if (ierr/=0) return
  !
  call iotk_close_read(iun, ierr=ierr)
  if (ierr/=0) return
  !
end subroutine read_upf_header_v2

!---------------------------------------------------------------------------------
subroutine read_upf_header_v1( psfile, psp, ierr )
  !---------------------------------------------------------------------------------
  implicit none
  character(len=*), intent(in)    :: psfile
  type(pw_pseudo),  intent(inout) :: psp
  integer,          intent(out)   :: ierr
  !
  character(50) :: dummy, psp_type
  logical :: lfound
  integer :: i,idummy

  ierr=0
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
  !
  select case (trim(psp_type))
  case ("NC","NCPP","nc","ncpp")
     psp%is_ncpp=.true.
  case ("US","USPP","us","uspp")
     psp%is_uspp=.true.
  case default
     call errore("read_upf_header_v1","invalid psp_type = "//trim(psp_type),10)
  end select
  psp%is_paw=.false.
  !
  read(iun,*,iostat=ierr) psp%has_nlcc
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) psp%Z
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) psp%lmax , dummy
  if (ierr/=0) return
  read(iun,*,iostat=ierr) psp%nmesh
  if (ierr/=0) return
  read(iun,*,iostat=ierr) idummy,psp%nbeta
  if (ierr/=0) return
  read(iun,'(a)') dummy
  !
  do i = 1, idummy
     read (iun, * ) 
  enddo
  !
  call iotk_scan_end(iun, "PP_HEADER",ierr=ierr)
  if (ierr/=0) return
  !
  rewind(iun) 
  lfound=.false.
  call iotk_scan_begin(iun, "PP_ADDINFO", found=lfound, ierr=ierr)
  if (.not.lfound) ierr=0
  if (lfound .and. ierr/=0) return
  psp%has_so=lfound
  !
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
subroutine pseudo_read_data(psfile,psp)
!-----------------------------------------------------------------------
  IMPLICIT NONE
  !
  character(len=*), intent(in)    :: psfile
  type(pw_pseudo),  intent(inout) :: psp
  !
  integer :: nmesh,nbeta
  !
  call pseudo_read_header(psfile, psp)
  !
  nmesh=psp%nmesh
  nbeta=psp%nbeta
  !
  allocate(psp%mesh(nmesh))
  allocate(psp%wmesh(nmesh))
  allocate(psp%rho_nlcc(nmesh))
  allocate(psp%vloc(nmesh))
  allocate(psp%lbeta(nbeta))
  allocate(psp%jbeta(nbeta))
  allocate(psp%mesh_beta(nbeta))
  allocate(psp%beta(nmesh,nbeta))
  allocate(psp%d(nbeta))
  allocate(psp%dmat(nbeta,nbeta))
  !
  call pseudo_read_parameters(psfile,psp%nmesh,psp%nbeta,psp%mesh,psp%has_so,psp%has_nlcc, &
&                             psp%wmesh,psp%rho_nlcc,psp%vloc,psp%lbeta,psp%jbeta,psp%mesh_beta,&
                              psp%beta,psp%d,psp%dmat)
  !
  ! Currently USPP data are read natively within
  ! qe_pseudo
  !
  !if (psp%is_uspp) then
  !   !
  !   ! more allocations inside the following routine
  !   !
  !   call pseudo_read_uspp(psfile,psp)
  !endif
  !
end subroutine pseudo_read_data

!-----------------------------------------------------------------------
subroutine pseudo_read_parameters(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc,wmesh,&
&                                 rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d,dmat)
  !-----------------------------------------------------------------------
  implicit none
  character(len=*), intent(in)  :: psfile
  logical,          intent(in)  :: psp_has_so
  logical,          intent(in)  :: psp_has_nlcc
  integer,          intent(in)  :: nmesh,nbeta
  real(SP),         intent(out) :: mesh(:), wmesh(:), rho_nlcc(:), vloc(:), beta(:,:), d(:), dmat(:,:), jbeta(:)
  integer,          intent(out) :: lbeta(:),mesh_beta(:)
  !
  integer :: ierr
  
  call read_upf_parameters_v2(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc, &
&                             wmesh,rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d,dmat,ierr)
  !
  if(ierr/=0) then
    !
    jbeta=0.
    call read_upf_parameters_v1(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc,wmesh, &
&                               rho_nlcc,vloc,lbeta,mesh_beta,beta,d,dmat,ierr)
  endif
  !
  if(ierr/=0) CALL errore('pseudo_read_parameters', 'Cannot read file: '//TRIM(psfile),1)
  !
end subroutine

!-----------------------------------------------------------------------
subroutine read_upf_parameters_v1(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc, &
&                                 wmesh,rho_nlcc,vloc,lbeta,mesh_beta,beta,d,dmat,ierr)
  !-----------------------------------------------------------------------
  use interfaces,     ONLY:LINEAR_ALGEBRA_driver
  use linear_algebra, ONLY:DIAGO
  implicit none
  !
  character(len=*), intent(in)  :: psfile
  integer,          intent(in)  :: nmesh,nbeta
  logical,          intent(in)  :: psp_has_so,psp_has_nlcc
  real(SP),         intent(out) :: mesh(:), wmesh(:), rho_nlcc(:), vloc(:), beta(:,:), d(:), dmat(:,:)
  integer,          intent(out) :: lbeta(:),mesh_beta(:)
  Integer,          intent(out) :: ierr
  !
  real(SP)      :: beta_tmp(nmesh,nbeta),d_tmp(nbeta,nbeta),rdummy
  complex(SP)   :: cd_tmp(nbeta,nbeta)
  character(50) :: dummy
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
  dmat=d_tmp
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
  if (psp_has_so) then
     call errore("read_upf_parameters_v1","SO read not implemented for UPFv1",10)
  endif
  !
  close(iun)
  !
end subroutine read_upf_parameters_v1

!-----------------------------------------------------------------------
subroutine read_upf_parameters_v2(psfile,nmesh,nbeta,mesh,psp_has_so,psp_has_nlcc, &
&                                 wmesh,rho_nlcc,vloc,lbeta,jbeta,mesh_beta,beta,d,dmat,ierr)
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
  REAL(SP),         INTENT(OUT) :: mesh(:), wmesh(:), rho_nlcc(:), vloc(:), beta(:,:), d(:), dmat(:,:), jbeta(:)
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
      call iotk_scan_attr(attr, 'size',  mesh_beta(ibeta), default=size(mesh),ierr=ierr)
      if (ierr/=0) return
      call iotk_scan_attr(attr, 'angular_momentum',  lbeta(ibeta),ierr=ierr)
      if (ierr/=0) return
    enddo
    !
    call iotk_scan_dat(iun, 'PP_DIJ', dmat, attr=attr,ierr=ierr)
    if (ierr/=0) return
    !
    d_tmp=dmat
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
  if (psp_has_nlcc) then
    rewind(iun)
    read(iun,*)
    !
    call iotk_scan_dat(iun, 'PP_NLCC', rho_nlcc, ierr=ierr)
    if (ierr/=0) return
  else
    rho_nlcc = 0.0
  endif
  !
  call iotk_close_read(iun, ierr=ierr)
  if (ierr/=0) return
  ! 
end subroutine read_upf_parameters_v2


!-------------------------------------------
subroutine pseudo_read_uspp(psfile,psp)
  !-------------------------------------------
  implicit none
  character(*),    intent(in) :: psfile
  type(pw_pseudo), intent(inout) :: psp
  !
  integer :: ierr
  !
  ierr=0
  call pseudo_read_uspp_v2(psfile,psp,ierr=ierr)
  !
  if (ierr/=0) then
     ierr=0
     call pseudo_read_uspp_v1(psfile,psp,ierr=ierr)
  endif
  !
  if (ierr/=0) call errore('pseudo_read_uspp','unable to read USPP data',10)
  !
end subroutine pseudo_read_uspp

!-------------------------------------------
subroutine pseudo_read_uspp_v1(psfile,psp,ierr)
  !-------------------------------------------
  implicit none
  character(*),    intent(in)  :: psfile
  type(pw_pseudo), intent(inout) :: psp
  integer,         intent(out) :: ierr
  !
  ierr=0
  if (.not. psp%is_uspp) return
  !
  call errore("pseudo_read_uspp_v1","USPP reading not implemented for UPF v1",10)
  !
end subroutine pseudo_read_uspp_v1

!-------------------------------------------
subroutine pseudo_read_uspp_v2(psfile,psp,ierr)
  !-------------------------------------------
  implicit none
  character(*),    intent(in)  :: psfile
  type(pw_pseudo), intent(inout) :: psp
  integer,         intent(out) :: ierr
  !
  integer :: nb,mb,ln,lm,nmb,l
  !
  ierr=0
  if (.not. psp%is_uspp) return
  !
  call iotk_free_unit(iun)
  call iotk_open_read(iun, file=trim(psfile), ierr=ierr)
  if (ierr/=0) return
  !
  call iotk_scan_begin(iun, 'PP_AUGMENTATION', attr=attr,ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_attr(attr, 'q_with_l', psp%q_with_l,ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_attr(attr, 'nqf',      psp%nqf,ierr=ierr)
  if (ierr/=0) return
  call iotk_scan_attr(attr, 'nqlc',     psp%nqlc, default=2*psp%lmax+1,ierr=ierr)
  if (ierr/=0) return
  !
  ! a negative number means that all qfunc are stored
  call iotk_scan_attr(attr,'augmentation_epsilon',psp%qqq_eps, default=-1.0_SP,ierr=ierr)
  if (ierr/=0) return
  !
  allocate( psp%rinner( psp%nqlc ) )
  allocate( psp%qqq   ( psp%nbeta, psp%nbeta ) )
  if ( psp%q_with_l ) then
    allocate( psp%qfuncl( psp%nmesh, psp%nbeta*(psp%nbeta+1)/2, 0:2*psp%lmax) )
    psp%qfuncl=0._SP
  else
    allocate( psp%qfunc(psp%nmesh, psp%nbeta*(psp%nbeta+1)/2) )
  endif
  !
  ! Read the integrals of the Q functions
  call iotk_scan_dat(iun, 'PP_Q',psp%qqq, ierr=ierr )
  if (ierr/=0) return
  !
  !
  ! Read polinomial coefficients for Q_ij expansion at small radius
  !
  if (psp%nqf <= 0) then
     psp%rinner(:) = 0._SP
     allocate( psp%qfcoef(1,1,1,1) )
     psp%qfcoef = 0._SP
  else
     allocate( psp%qfcoef( MAX( psp%nqf,1 ), psp%nqlc, psp%nbeta, psp%nbeta) )
     call iotk_scan_dat(iun, 'PP_QFCOEF',psp%qfcoef, attr=attr,ierr=ierr)
     if (ierr/=0) return
     call iotk_scan_dat(iun, 'PP_RINNER',psp%rinner, attr=attr, ierr=ierr)
     if (ierr/=0) return
  endif
  !
  ! Read augmentation charge Q_ij
  do nb = 1,psp%nbeta
    ln = psp%lbeta(nb)
    DO mb = nb,psp%nbeta
      lm = psp%lbeta(mb)
      nmb = mb * (mb-1) /2 + nb
      q_with_l : &
      IF( psp%q_with_l ) THEN
         do l = abs(ln-lm),ln+lm,2 ! only even terms
            psp%qfuncl(:,nmb,l)=0.0_SP
            CALL iotk_scan_dat(iun, 'PP_QIJL'//iotk_index((/nb,mb,l/)),&
                               psp%qfuncl(:,nmb,l))
         enddo
      ELSE q_with_l
         psp%qfunc(:,nmb)=0.0_SP
         CALL iotk_scan_dat(iun, 'PP_QIJ'//iotk_index((/nb,mb/)),&
                            psp%qfunc(:,nmb))
      ENDIF q_with_l
    ENDDO
  ENDDO
  !
  call iotk_scan_end(iun, 'PP_AUGMENTATION',ierr=ierr)
  if (ierr/=0) return
  !
  call iotk_close_read(iun,ierr=ierr)
  if (ierr/=0) return
  !
end subroutine pseudo_read_uspp_v2

end module pw_pseudo_read_m

