!{\src2tex{textfont=tt}}
!!****m* ABINIT/defs_datatypes
!! NAME
!! defs_datatypes
!!
!! FUNCTION
!! This module contains definitions of all structured datatypes for the
!! ABINIT package.
!! If you want to add one new datatype, please, examine first whether
!! another datatype might meet your need (e.g. adding some records to it).
!! Then, if you are sure your new structured datatype is needed,
!! write it here, and DOCUMENT it properly (not all datastructure here are
!! well documented, it is a shame ...).
!! Do not forget : you will likely be the major winner if you document
!! properly.
!! Proper documentation of a structured datatype means :
!!  (1) Mention it in the list just below
!!  (2) Describe it in the NOTES section
!!  (3) Put it in alphabetical order in the the main section of this module
!!  (4) Document each of its records, except if they are described elsewhere
!!      (this exception is typically the case of the dataset associated with
!!      input variables, for which there is a help file)
!!
!! List of datatypes :
!! * hdr_type   : the header of wf, den and pot files
!! * wffile_type : a handler for dealing with the IO of a wavefunction file
!!
!! COPYRIGHT
!! Copyright (C) 2001-2009 ABINIT group (XG)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~abinit/COPYING
!! or http://www.gnu.org/copyleft/gpl.txt .
!!
!! SOURCE

module defs_datatypes

 implicit none

 integer, parameter :: dp=kind(1.0d0)
 integer, parameter :: fnlen=264     ! maximum length of file name variables
 integer, parameter :: abinit_offset = 8

 character(1), parameter :: ch10 = char(10)
 real(dp),     parameter :: zero=0._dp
 real(dp), parameter :: tol6= 0.000001_dp

 type hdr_type
  integer :: bantot        ! total number of bands (sum of nband on all kpts and spins)
  integer :: date          ! starting date
  integer :: headform      ! format of the header
  integer :: intxc,ixc,natom,nkpt,npsp,nspden        ! input variables
  integer :: nspinor,nsppol,nsym,ntypat,occopt        ! input variables
  integer :: pertcase      ! the index of the perturbation, 0 if GS calculation
  integer :: usepaw        ! input variable (0=norm-conserving psps, 1=paw)
  integer :: usewvl        ! input variable (0=plane-waves, 1=wavelets)
  integer :: ngfft(3)      ! input variable

! This record is not a part of the hdr_type, although it is present in the
! header of the files. This is because it depends on the kind of file
! that is written, while all other information does not depend on it.
! It was preferred to let it be initialized or defined outside of hdr_type.
! integer :: fform         ! file descriptor (or file format)

  integer, pointer :: istwfk(:)    ! input variable istwfk(nkpt)
  integer, pointer :: lmn_size(:)  ! lmn_size(npsp) from psps
  integer, pointer :: nband(:)     ! input variable nband(nkpt*nsppol)
  integer, pointer :: npwarr(:)    ! npwarr(nkpt) array holding npw for each k point
  integer          :: nwvlarr(2)   ! nwvlarr(2) array holding the number of wavelets
                                   ! for each resolution.
  integer, pointer :: pspcod(:)    ! pscod(npsp) from psps
  integer, pointer :: pspdat(:)    ! psdat(npsp) from psps
  integer, pointer :: pspso(:)     ! pspso(npsp) from psps
  integer, pointer :: pspxc(:)     ! pspxc(npsp) from psps
  integer, pointer :: so_psp(:)    ! input variable so_psp(npsp)
  integer, pointer :: symafm(:)    ! input variable symafm(nsym)
  integer, pointer :: symrel(:,:,:)! input variable symrel(3,3,nsym)
  integer, pointer :: typat(:)     ! input variable typat(natom)

  real(dp) :: ecut                  ! input variable
  real(dp) :: ecutdg                ! input variable (ecut for NC psps, pawecutdg for paw)
  real(dp) :: ecutsm                ! input variable
  real(dp) :: ecut_eff              ! ecut*dilatmx**2 (dilatmx is an input variable)
  real(dp) :: etot,fermie,residm    ! EVOLVING variables
  real(dp) :: qptn(3)               ! the wavevector, in case of a perturbation
  real(dp) :: rprimd(3,3)           ! EVOLVING variables
  real(dp) :: stmbias               ! input variable
  real(dp) :: tphysel               ! input variable
  real(dp) :: tsmear                ! input variable
  real(dp), pointer :: kptns(:,:)   ! input variable kptns(3,nkpt)
  real(dp), pointer :: occ(:)       ! EVOLVING variable occ(bantot)
  real(dp), pointer :: tnons(:,:)   ! input variable tnons(3,nsym)
  real(dp), pointer :: wtk(:)       ! weight of kpoints wtk(nkpt)
  real(dp), pointer :: xred(:,:)    ! EVOLVING variable xred(3,natom)
  real(dp), pointer :: zionpsp(:)   ! zionpsp(npsp) from psps
  real(dp), pointer :: znuclpsp(:)  ! znuclpsp(npsp) from psps
                                    ! Note the difference between znucl and znuclpsp !!
  real(dp), pointer :: znucltypat(:)! znucltypat(ntypat) from alchemy

  character(len=6) :: codvsn              ! version of the code
  character(len=132), pointer :: title(:) ! title(npsp) from psps

 end type hdr_type

!----------------------------------------------------------------------

!!****t* defs_datatypes/wffile_type
!! NAME
!! wffile_type
!!
!! FUNCTION
!! This structure datatype is a handler for dealing with the IO of a
!! wavefunction file.
!! It contains, among other things, the method of access to the file
!! (standard F90 read/write, or NetCDF call, or MPI IO), the unit number
!! if applicable, the filename, the information on the
!! parallelism, etc ...
!!
!! SOURCE


 type wffile_type

! Integer scalar
  integer :: unwff
   ! unwff  unit number of unformatted wavefunction disk file
  integer :: accesswff
   ! Method to access the wavefunction file
   ! =0 if usual Fortran IO routines
   ! =1 if MPI/IO routines (this access method is only available in parallel)
   ! =2 if NetCDF routines (not used yet)
   ! =-1 if usual Fortran IO routines, but only the master node in the parallel case
  integer :: formwff
   ! formwff=format of the eigenvalues
   !   -1 => not used
   !    0 => vector of eigenvalues
   !    1 => hermitian matrix of eigenvalues
  integer ::  kgwff
   ! kgwff  if 1 , read or write kg_k ; if 0, do not care about kg_k
  character(len=fnlen) :: fname
   ! filename (if available)

! In case of MPI parallel use
  integer :: master
   ! master = number of the processor master of the IO procedure when the WffOpen call is issued
  integer :: me
   ! me = number of my processor
  integer :: nproc
   ! nproc = number of processors that will have access to the file
  integer :: spaceComm
   ! spaceComm = space communicator of the IO procedure when the WffOpen call is issued

! In case of MPI/IO : additional information
  integer :: fhwff
   ! fhwff  file handle of unformatted wavefunction disk file (use in MPI/IO only)
  integer :: nbOct_int,nbOct_dp,nbOct_ch,lght_recs
   ! nbOct_int octet number of int value
   ! nbOct_dp octet number of dp value
   ! nbOct_ch octet number of character value
   ! lght_recs length of record

  integer(abinit_offset)  :: offwff,off_recs
   ! offwff  offset position of unformatted wavefunction disk file
   ! off_recs  offset position of start record
   !             (use in parallel)

 end type wffile_type
!!***

end module defs_datatypes
!!***
