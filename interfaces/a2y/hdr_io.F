!{\src2tex{textfont=tt}}
!!****f* ABINIT/hdr_io
!! NAME
!! hdr_io
!!
!! FUNCTION
!! This subroutine deals with the I/O of the hdr_type
!! structured variables (read/write/echo).
!! According to the value of rdwr, it reads the header
!! of a file, writes it, or echo the value of the structured
!! variable to a file.
!! Note that, when reading, different records of hdr
!! are allocated here, according to the values of the
!! read variables. Records of hdr should be deallocated
!! correctly by a call to hdr_clean when hdr is not used anymore.
!! Two instances of the hdr_io routines are defined :
!!  hdr_io_int to which only the unit number is given
!!  hdr_io_wfftype to which a wffil datatype is given
!!
!! COPYRIGHT
!! Copyright (C) 2002-2009 ABINIT group (XG,MB)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~abinit/COPYING
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~abinit/doc/developers/contributors.txt.
!!
!! INPUTS
!!  rdwr= if 1, read the hdr structured variable from the header of the file,
!!        if 2, write the header to unformatted file
!!        if 3, echo part of the header to formatted file (records 1 and 2)
!!        if 4, echo the header to formatted file
!!        if 5, read the hdr without rewinding (unformatted)
!!        if 6, write the hdr without rewinding (unformatted)
!!  unitfi=unit number of the file (unformatted if rdwr=1, 2, 5 or 6 formatted if rdwr=3,4)
!!
!! OUTPUT
!!  (see side effects)
!!
!! SIDE EFFECTS
!!  The following variables are both input or output :
!!  fform=kind of the array in the file
!!   if rdwr=1,5 : will be output ; if the reading fail, return fform=0
!!   if rdwr=2,3,4,6 : should be input, will be written or echo to file
!!  hdr <type(hdr_type)>=the header structured variable
!!   if rdwr=1,5 : will be output
!!   if rdwr=2,3,4,6 : should be input, will be written or echo to file
!!
!! NOTES
!! In all cases, the file is supposed to be open already
!! When reading (rdwr=1) or writing (rdwr=2), rewind the file
!! When echoing (rdwr=3) does not rewind the file.
!! When reading (rdwr=5) or writing (rdwr=6), DOES NOT rewind the file
!!
!! PARENTS
!!      conducti,cut3d,initaim,inwffil3,ioarr,macroave,newsp,outkss,outwf,print_ij
!!      rdem1,rdkss,testepsm1,testlda,uderiv,vtorho3,wrem1
!!
!! CHILDREN
!!      leave_new,rhoij_alloc,wrtout
!!
! SOURCE

 subroutine hdr_io_wfftype(fform,hdr,rdwr,wff)

 use defs_datatypes, ONLY:hdr_type,wffile_type

 implicit none

!Arguments ------------------------------------
 integer,intent(inout) :: fform
 integer,intent(in) :: rdwr
 type(hdr_type),intent(inout) :: hdr
 type(wffile_type),intent(inout) :: wff

!Local variables-------------------------------
 integer :: unitfi,ierr

! *************************************************************************

 if( wff%accesswff==0                            .or. &
&   (wff%accesswff==-1 .and.wff%master==wff%me).or. &
&   (wff%accesswff==1  .and.wff%master==wff%me)    ) then
  call hdr_io_int(fform,hdr,rdwr,wff%unwff)
 end if

 end subroutine hdr_io_wfftype

!-----------------------------------------------------------------------------------

 subroutine hdr_io_int(fform,hdr,rdwr,unitfi)

 use defs_datatypes, ONLY:hdr_type,dp,ch10,zero,tol6

 implicit none

!Arguments ------------------------------------
 integer,intent(inout) :: fform
 integer,intent(in) :: rdwr,unitfi
 type(hdr_type),intent(inout) :: hdr

!Local variables-------------------------------
 integer :: bantot,bsize,cplex,headform,iatom,ierr,ii,ikpt,ipsp,ispden,isym
 integer :: jj,lloc,lmax,mmax,natinc,natom,nkpt,npsp,nselect,nspden,nsppol
 integer :: nsym,ntypat
 character(len=500) :: message
 character(len=6) :: codvsn
 integer, allocatable :: ibuffer(:),nsel44(:,:),nsel56(:)
 real(dp) :: acell(3)
 real(dp), allocatable :: buffer(:)

! *************************************************************************

! -------------------------------------------------------------------------
! Reading the header of an unformatted file
! -------------------------------------------------------------------------

 if(rdwr==1 .or. rdwr==5)then

  if (rdwr==1) rewind(unitfi)

! Reading the first record of the file ------------------------------------

  read(unitfi,iostat=ierr)codvsn,fform
  if (ierr /=0) then
   fform=0
   return   ! This is to allow treatment of old epsm1 format
  end if

  if(fform==1   .or. &
&    fform==2   .or. &
&    fform==51  .or. &
&    fform==52  .or. &
&    fform==101 .or. &
&    fform==102       )then
!  This is the old format
   headform=22

  else

!  Format beyond 22 have a different first line, so need reading again the first line
   backspace (unitfi)
   read (unitfi)   codvsn,headform,fform

   if(headform/=23 .and. &
&     headform/=34 .and. &
&     headform/=40 .and. &
&     headform/=41 .and. &
&     headform/=42 .and. &
&     headform/=44 .and. &
&     headform/=53 .and. &
&     headform/=56 .and. &
&     headform/=57         )then
    write(message, '(4a,i3,3a,i8,3a)' ) ch10,&
&    ' hdr_io : ERROR -',ch10,&
&    '  The first line of the (WF, DEN or POT) file read in unit ',unitfi,' is erroneous.',ch10,&
&    '  headform is ',headform,', while it should be 23, 34, 40, 41, 42, 44, 53 or 56 or 57.',ch10,&
&    '  Action : check the correctness of your file.'
    !call wrtout(6,message,'COLL')
    !call leave_new('COLL')
   end if

  end if

  hdr%codvsn=codvsn
  hdr%headform=headform
! fform is not a record of hdr_type

! Reading the second record of the file ------------------------------------

! Initialize the values that are not present for all versions (exception : npsp)
  hdr%nspden=1
  hdr%nspinor=1
  hdr%occopt=1
  hdr%pertcase=1
  hdr%usepaw=0
  hdr%usewvl=0
  hdr%ecut=zero
  hdr%ecutdg=zero
  hdr%ecutsm=zero
  hdr%qptn(1:3)=zero
  hdr%stmbias=zero
  hdr%tphysel=zero
  hdr%tsmear=zero

  if(headform==22)then

    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, nsppol, nsym, ntypat,&
&    acell, hdr%ecut_eff, hdr%rprimd
    npsp=ntypat

  else if(headform==23)then

!   Compared to v2.2, add nspden, nspinor, occopt
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, ntypat, hdr%occopt,&
&    acell, hdr%ecut_eff, hdr%rprimd
    npsp=ntypat

  else if(headform==34)then

!   Compared to v2.3, subtract acell, and add npsp
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, npsp, ntypat, hdr%occopt,&
&    hdr%ecut_eff, hdr%rprimd

  else if(headform==40)then

!   Compared to v3.4, add ecut, ecutsm, tphysel, tsmear
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, npsp, ntypat, hdr%occopt,&
&    hdr%ecut, hdr%ecutsm, hdr%ecut_eff, hdr%rprimd, hdr%tphysel, hdr%tsmear

  else if(headform==41)then

!   Compared to v4.0, add pertcase and qptn(3)
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, npsp, ntypat, hdr%occopt, hdr%pertcase,&
&    hdr%ecut, hdr%ecutsm, hdr%ecut_eff, hdr%qptn(1:3), hdr%rprimd, hdr%tphysel, hdr%tsmear

  else if(headform==42)then

!   Compared to v4.1, add stmbias
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, npsp, ntypat, hdr%occopt, hdr%pertcase,&
&    hdr%ecut, hdr%ecutsm, hdr%ecut_eff, hdr%qptn(1:3), hdr%rprimd,&
&    hdr%stmbias, hdr%tphysel, hdr%tsmear

  else if(headform>=44 .and. headform<57)then

!   Compared to v4.2, add usepaw and ecutdg
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, npsp, ntypat, hdr%occopt, hdr%pertcase,&
&    hdr%usepaw, hdr%ecut, hdr%ecutdg, hdr%ecutsm, hdr%ecut_eff, hdr%qptn(1:3), hdr%rprimd,&
&    hdr%stmbias, hdr%tphysel, hdr%tsmear

  else if(headform>=57)then

!   Compared to v4.4, add usewvl
    read(unitfi) bantot, hdr%date, hdr%intxc, hdr%ixc, natom, hdr%ngfft(1:3),&
&    nkpt, hdr%nspden, hdr%nspinor, nsppol, nsym, npsp, ntypat, hdr%occopt, hdr%pertcase,&
&    hdr%usepaw, hdr%ecut, hdr%ecutdg, hdr%ecutsm, hdr%ecut_eff, hdr%qptn(1:3), hdr%rprimd,&
&    hdr%stmbias, hdr%tphysel, hdr%tsmear, hdr%usewvl

  end if

  hdr%bantot=bantot
  hdr%natom =natom
  hdr%nkpt  =nkpt
  hdr%npsp  =npsp
  hdr%nsppol=nsppol
  hdr%nsym  =nsym
  hdr%ntypat =ntypat

  if(hdr%ecutsm>tol6 .and. headform<44 .and. .not.(fform==51.or.fform==52.or.fform==101.or.fform==102))then
   write(message, '(4a,es16.6,9a)' ) ch10,&
&   ' hdr_io : ERROR -',ch10,&
&   '  The value of ecutsm is',hdr%ecutsm,', while the file has been produced prior to v4.4 .',ch10,&
&   '  The definition of the smearing function has changed, so that you are not allowed',ch10,&
&   '  to restart from a old wavefunction file. By contrast, you can restart from an old',ch10,&
&   '  potential or density file, and perform a self-consistent cycle with a new ABINIT version.',ch10,&
&   '  Action : produce a density or potential file using the old version of ABINIT, and restart from it.'
!   call wrtout(6,message,'COLL')
!   call leave_new('COLL')
  end if

! Allocate all parts of hdr that need to be --------------------------------

  allocate(hdr%istwfk(nkpt))
  allocate(hdr%kptns(3,nkpt))
  allocate(hdr%lmn_size(npsp))
  allocate(hdr%nband(nkpt*nsppol))
  allocate(hdr%npwarr(nkpt)) ! Warning : npwarr here has only one dim
  allocate(hdr%occ(bantot))
  allocate(hdr%pspcod(npsp))
  allocate(hdr%pspdat(npsp))
  allocate(hdr%pspso(npsp))
  allocate(hdr%pspxc(npsp))
  allocate(hdr%so_psp(npsp))
  allocate(hdr%symafm(nsym))
  allocate(hdr%symrel(3,3,nsym))
  allocate(hdr%title(npsp))
  allocate(hdr%tnons(3,nsym))
  allocate(hdr%typat(natom))
  allocate(hdr%wtk(nkpt))
  allocate(hdr%xred(3,natom))
  allocate(hdr%zionpsp(npsp))
  allocate(hdr%znuclpsp(npsp))
  allocate(hdr%znucltypat(ntypat))

! Reading the third record of the file ------------------------------------

! Initialize the values that are not present for all versions
  hdr%istwfk(:)=1
  hdr%so_psp(:)=1
  hdr%symafm(:)=1

  if(headform==22 .and. (fform==1 .or. fform==51 .or. fform==101))then

!   This is very old (pre-2.0) format !
    read(unitfi) hdr%nband(:), hdr%npwarr(:), hdr%symrel(:,:,:), &
&    hdr%typat(:), hdr%kptns(:,:), hdr%occ(:), &
&    hdr%tnons(:,:), hdr%znucltypat(:)

  else if(headform==22 .or. headform==23 .or. headform==34)then

!   Compared to pre v2.0, add istwfk
    read(unitfi) hdr%nband(:), hdr%npwarr(:), hdr%symrel(:,:,:), &
&    hdr%typat(:), hdr%istwfk(:), hdr%kptns(:,:), hdr%occ(:), &
&    hdr%tnons(:,:), hdr%znucltypat(:)

  else if(headform>=40 .and. headform < 50)then

!   Compared to pre v4.0, add so_psp and symafm, and switch istwfk

    read(unitfi)  hdr%istwfk(:), hdr%nband(:), hdr%npwarr(:), &
&    hdr%so_psp(:), hdr%symafm(:), hdr%symrel(:,:,:), &
&    hdr%typat(:), hdr%kptns(:,:), hdr%occ(:), &
&    hdr%tnons(:,:), hdr%znucltypat(:)

  else if(headform>=50)then

!   Compared to pre v5.0, add wtk
    read(unitfi)  hdr%istwfk(:), hdr%nband(:), hdr%npwarr(:), &
&    hdr%so_psp(:), hdr%symafm(:), hdr%symrel(:,:,:), &
&    hdr%typat(:), hdr%kptns(:,:), hdr%occ(:), &
&    hdr%tnons(:,:), hdr%znucltypat(:), hdr%wtk(:)

  end if

! Reading the records with psp information ---------------------------------

! Initialize the values that are not present for all versions
  hdr%pspso(:)=1
  hdr%lmn_size(:)=0

  if(headform==22)then

   do ipsp=1,npsp
    read(unitfi) hdr%title(ipsp), hdr%znuclpsp(ipsp), &
&    hdr%zionpsp(ipsp), hdr%pspdat(ipsp), hdr%pspcod(ipsp), &
&    hdr%pspxc(ipsp), lmax, lloc, mmax
   end do

  else if(headform==23)then

!  Compared to 2.2, add pspso
   do ipsp=1,npsp
    read(unitfi) hdr%title(ipsp), hdr%znuclpsp(ipsp), &
&    hdr%zionpsp(ipsp), hdr%pspso(ipsp), hdr%pspdat(ipsp), &
&    hdr%pspcod(ipsp), hdr%pspxc(ipsp), lmax, lloc, mmax
   end do

  else if(headform==34 .or. headform==40 .or. headform==41 &
&                      .or. headform==42)then

!  Compared to 2.3, suppress lmax, lloc, mmax
   do ipsp=1,npsp
    read(unitfi) hdr%title(ipsp), hdr%znuclpsp(ipsp), &
&    hdr%zionpsp(ipsp), hdr%pspso(ipsp), hdr%pspdat(ipsp), &
&    hdr%pspcod(ipsp), hdr%pspxc(ipsp)
   end do

  else if(headform>=44)then

!  Compared to 4.2, add lmn_size
   do ipsp=1,npsp
    read(unitfi) hdr%title(ipsp), hdr%znuclpsp(ipsp), &
&    hdr%zionpsp(ipsp), hdr%pspso(ipsp), hdr%pspdat(ipsp), &
&    hdr%pspcod(ipsp), hdr%pspxc(ipsp), hdr%lmn_size(ipsp)
   end do

  end if

! Reading the final record of the header  ---------------------------------

! Initialize the values that are not present for all versions
  hdr%fermie=zero

  if(headform==22)then
   read(unitfi) hdr%residm, hdr%xred(:,:), hdr%etot
  else if(headform==23 .or. headform==34 .or. headform>=40)then
   read(unitfi) hdr%residm, hdr%xred(:,:), hdr%etot, hdr%fermie
  end if

! -------------------------------------------------------------------------
! Writing the header of an unformatted file
! -------------------------------------------------------------------------

 else if(rdwr==2 .or. rdwr==6)then

! natom,nkpt,npsp,ntypat... are not defined in this section :
! always address them from hdr

  if(rdwr==2) rewind(unitfi)

! Writing always use last format version
  headform=57
  write(unitfi) hdr%codvsn, headform, fform

  write(unitfi) hdr%bantot, hdr%date, hdr%intxc, hdr%ixc, &
&  hdr%natom, hdr%ngfft(1:3), hdr%nkpt, &
&  hdr%nspden, hdr%nspinor, &
&  hdr%nsppol, hdr%nsym, hdr%npsp, hdr%ntypat, hdr%occopt, hdr%pertcase,&
&  hdr%usepaw, hdr%ecut, hdr%ecutdg, hdr%ecutsm, hdr%ecut_eff, &
&  hdr%qptn, hdr%rprimd, hdr%stmbias, hdr%tphysel, hdr%tsmear, &
&  hdr%usewvl

  write(unitfi) hdr%istwfk(:), hdr%nband(:), hdr%npwarr(:),&
&   hdr%so_psp(:), hdr%symafm(:), hdr%symrel(:,:,:), &
&   hdr%typat(:), hdr%kptns(:,:), hdr%occ(:), &
&   hdr%tnons(:,:), hdr%znucltypat(:)

  do ipsp=1,hdr%npsp

   write(unitfi) hdr%title(ipsp), hdr%znuclpsp(ipsp), &
&   hdr%zionpsp(ipsp), hdr%pspso(ipsp), hdr%pspdat(ipsp), &
&   hdr%pspcod(ipsp), hdr%pspxc(ipsp), hdr%lmn_size(ipsp)

  end do

  write(unitfi) hdr%residm, hdr%xred(:,:), hdr%etot, hdr%fermie


 end if ! choice read/write/echo

 return

end subroutine hdr_io_int
!!***
