!{\src2tex{textfont=tt}}
!!****f* ABINIT/outkss
!! NAME
!! outkss
!!
!! FUNCTION
!!  This routine creates an output file containing the Kohn-Sham electronic Structure 
!!  for a large number of eigenstates (energies and eigen-functions).
!!  The resulting file (_KSS) is needed for a GW post-treatment.
!!
!! The routine drives the following operations:
!!  - Re-ordering G-vectors according to stars (sets of Gs related by symmetry operations).
!!    A set of g for all k-points is created.
!!  - Creating and opening the output "_KSS'" file
!!  - Printing out output file header information...
!! ... and, for each k-point:
!!    According to 'kssform', either
!!      - Re-computing <G|H|G_prim> matrix elements for all (G, G_prim).
!!        Diagonalizing H in the plane-wave basis.
!!   or - Taking eigenvalues/vectors from congugate-gradient ones.
!!  - Writing out eigenvalues and eigenvectors.
!!
!! COPYRIGHT
!! Copyright (C) 2000-2009 ABINIT group (MT, VO, AR, MG)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~abinit/COPYING
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~abinit/doc/developers/contributors.txt.
!!
!! INPUTS
!!  atindx(natom)=index table for atoms (see scfcv.f)
!!  atindx1(natom)=index table for atoms, inverse of atindx (see scfcv.f)
!!  cg(2,mpw*nspinor*mband*mkmem*nsppol)=planewave coefficients of wavefunctions.
!!  usecprj=1 if cprj datastructure has been allocated (ONLY PAW)
!!  Cprj(natom,nspinor*mband*mkmem*nsppol*Psps%usepaw) <type(cprj_type)>=
!!    projected input wave functions <Proj_i|Cnk> with all NL projectors (only for PAW)
!!    NOTE that Cprj are unsorted, see ctoprj.F90
!!  Dtfil <type(datafiles_type)>=variables related to files
!!  Dtset <type(dataset_type)>=all input variables for this dataset
!!  ecut=cut-off energy for plane wave basis sphere (Ha)
!!  eigen(mband*nkpt*nsppol)=array for holding eigenvalues (hartree)
!!  gmet(3,3)=reciprocal space metric tensor in bohr**-2.
!!  gprimd(3,3)=dimensional reciprocal space primitive translations
!!  Hdr <type(hdr_type)>=the header of wf, den and pot files
!!  kg(3,mpw*mkmem)=reduced planewave coordinates.
!!  kssform=govern the Kohn-Sham Structure file format
!!  mband=maximum number of bands
!!  mgfft=maximum size of 1D FFTs
!!  mkmem =number of k points which can fit in memory; set to 0 if use disk
!!  MPI_enreg=information about MPI parallelization
!!  mpsang= 1+maximum angular momentum for nonlocal pseudopotentials
!!  mpw=maximum dimensioned size of npw.
!!  natom=number of atoms in cell.
!!  nattyp(ntypat)= # atoms of each type.
!!  nfft=(effective) number of FFT grid points (for this processor)
!!  nkpt=number of k points.
!!  npwarr(nkpt)=number of planewaves in basis at this k point
!!  nspinor=number of spinorial components of the wavefunctions
!!  nsppol=1 for unpolarized, 2 for spin-polarized
!!  nspden=number of density components
!!  nsym=number of symmetries in space group
!!  ntypat=number of types of atoms in unit cell.
!!  occ(mband*nkpt*nsppol)=occupation number for each band (usually 2) for each k.
!!  Pawrad(Psps%ntypat*Psps%usepaw) <type(pawrad_type)>=paw radial mesh and related data
!!  Pawtab(Psps%ntypat*Psps%usepaw) <type(pawtab_type)>=paw tabulated starting data
!!  ph1d(2,3*(2*mgfft+1)*natom)=one-dimensional structure factor information
!!  prtvol=control print volume and debugging output
!!  Psps <type(pseudopotential_type)>=variables related to pseudopotentials
!!  rmet(3,3)=real space metric (bohr**2)
!!  rprimd(3,3)=dimensional primitive translations for real space (bohr)
!!  ucvol=unit cell volume (bohr**3)
!!  Wffnow=information about wf disk file
!!  vtrial(nfft,nspden)=the trial potential
!!  xred(3,natom)=reduced dimensionless atomic coordinates
!!  ylm(mpw*mkmem,mpsang*mpsang*useylm)=real spherical harmonics for each G and k point
!!
!! OUTPUT
!!  (only writing)
!!
!! NOTES
!! * The routine can be time consuming (in particular when computing
!!   <G|H|G_prim> elements for all (G, G_prim)) (kssform=1).
!!   So, it is recommended to call it once per run...
!!
!! * The routine RE-compute all Hamiltonian terms.
!!   So it is equivalent to an additional electronic SC cycle.
!!   (This has no effect is convergence was reach... 
!!   If not, eigenvalues/vectors may differs from the congugaste gradient ones)
!!
!! * The routine does not work with nspinor=2 or mpspso=2 (mpssoang>mpsang).
!!
!! * The routine works only for norm-conserving pseudopotentials or PAW but only if kssform==3.
!!
!! * The routine gives correct _KSS files, only with one projector in each angular momentum channel.
!!   (If kssform=3, the wavefunctions are correct and only the KB form factors are not calculated correctly.
!!    The results are, however, completely wrong in case of kssform=1)
!!
!! * In the ETSF output format (Dtset%accesswff == 3), the complete symmetry set
!!   is output. So, if reading programs need only the symmorphic symmetries, they
!!   will need to remove themselves the non-symmorphic ones.
!!
!! * The routine gives strange file formats if ((kssform==0 or kssform==1) and
!!   real(dp) means single precision). It is needed to correct all the write
!!   in such a way to force however real(dp) on the output file.
!!
!! * There exists two file formats:
!!    kssform==1 diagonalized file _KSS in real(dp) is generated.
!!    kssform==3 same as kssform=1 but the wavefunctions are not diagonalized
!!               (they are taken from conjugate-gradient ones)
!!    Old kssform=0 and kssform=2 are obsolete - no longer available
!!    The recommended form is 1.
!!
!! * Here are used mpsang and Psps%mpsang??? which ones?
!!   We need 1+maximum angular momentum for nonlocal pseudopotentials
!!   to dimensionate variables and write correctly the files.
!!
!! TESTS
!! * ETSF_IO output is tested in tests/etsf_io/t02.
!!
!! PARENTS
!!      outscfcv
!!
!! CHILDREN
!!      wrtout
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

subroutine outkss(atindx,atindx1,Dtfil,Dtset,ecut,gmet,gprimd,Hdr,kg,&
& kssform,mband,mgfft,mkmem,MPI_enreg,mpsang,mpw,natom,&
& nattyp,nfft,nkpt,npwarr,nspinor,nspden,nsppol,nsym,ntypat,occ,Pawrad,Pawtab,&
& ph1d,prtvol,Psps,rmet,rprimd,ucvol,Wffnow,vtrial,xred,ylm,cg,usecprj,Cprj,eigen)

 use defs_basis
 use defs_datatypes
 !£use m_numeric_tools, only : bisect
 use defs_wvltypes
 use m_io_tools, only : flush_unit
 use m_errors, only : assert

#if defined HAVE_ETSF_IO
 use etsf_io
#endif

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
 use interfaces_01manage_mpi
 use interfaces_11util
 use interfaces_12ffts
 use interfaces_12geometry
 use interfaces_13io_mpi
 use interfaces_13ionetcdf
 use interfaces_13iovars
 use interfaces_13recipspace
 use interfaces_14iowfdenpot
 use interfaces_15nonlocal
 use interfaces_lib01hidempi
 use interfaces_lib03numeric
 use interfaces_linalg
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: kssform,mband,mgfft,mkmem,mpsang,mpw,natom,usecprj
 integer,intent(in) :: nfft,nkpt,nsppol,nspden,nsym,ntypat,prtvol
 integer,intent(inout) :: nspinor ! INOUT due to rdnpw
 real(dp),intent(in) :: ecut,ucvol
 type(MPI_type),intent(inout) :: MPI_enreg
 type(Datafiles_type),intent(in) :: Dtfil
 type(Dataset_type),intent(in) :: Dtset
 type(Hdr_type),intent(inout) :: Hdr
 type(Pseudopotential_type),intent(in) :: Psps
 type(Wffile_type),intent(inout) :: Wffnow
!arrays
 integer,intent(in) :: atindx(natom),atindx1(natom)
 integer,intent(in) :: kg(3,mpw*mkmem),nattyp(ntypat)
 integer,intent(in),target :: npwarr(nkpt)
 real(dp),intent(in) :: gmet(3,3),gprimd(3,3),occ(mband*nkpt*nsppol)
 real(dp),intent(in) :: ph1d(2,3*(2*mgfft+1)*natom),rmet(3,3),rprimd(3,3)
 real(dp),intent(inout) :: vtrial(nfft,nspden)
 real(dp),intent(in) :: xred(3,natom),ylm(mpw*mkmem,mpsang*mpsang*Psps%useylm)
 real(dp),intent(in) :: cg(2,mpw*nspinor*mband*mkmem*nsppol),eigen(mband*nkpt*nsppol)
 type(Cprj_type),intent(in) :: Cprj(natom,nspinor*mband*mkmem*nsppol*Psps%usepaw*usecprj)
 type(Pawrad_type),intent(in) :: Pawrad(Psps%ntypat*Psps%usepaw)
 type(Pawtab_type),intent(in) :: Pawtab(Psps%ntypat*Psps%usepaw)

!Local variables-------------------------------
!scalars
 integer,parameter :: untkss=75
 integer :: bdtot_index,choice,cplex,cpopt,dimekb1,dimekb2,dimffnl,fform,i,ia,iatom,iband,ib,ibp,ider,idir,ier
 integer :: ierr,ibsp,ibsp1,ibsp2,ibg,ig,igp,igspinor,ii,iinv,ikg,ik,ikpt,il,il0,ilim,ilm,ilmn,in,ind1,ind2
 integer :: master,receiver,sender,spinor_shift1,spinor_shift2
 integer :: inp,ipw,is,is1,is2,isdiscarded,isretained,ish,ishm,ispinor,ispinorp,isppol,itypat,istwf_k
 integer :: isym,j,jj,jlmn,k,j0lmn
 integer :: k_index,kk,klmn,lmn2_size,matblk,maxpw,mcg_disk,mproj,n1,n2,n2dim,n3,n4,n5,n6,nband_k,nb
 integer :: nbandkss_k,nbandksseff,nbase,negv,ngdiago,nghg,nkpg,nnlout,nprocs,npw_k,npwkss
 integer :: nrst1,nrst2,nsym2,ntemp,bantot_save,option,paw_opt,pinv,rdwr,signs,sizepw,spaceComm,istat
 integer :: tim_fourwf,tim_nonlop,tim_rwwf,unwfnow,pad1,pad2
 integer :: bufnb=20,bufrt,bufsz
 real(dp) :: arg,cinf=1.0e24,csup=0.0_dp,dtnons,dum,einf=1.0e24,eps,esup=0.0_dp
 real(dp) :: norm,weight,sij,cfact
 complex(dpc) :: cdum
 logical :: diago,found,ltest,lhack
 character(len=fnlen) :: filekss,filevkb
 character(len=500) :: msg
 character(len=80) :: title,frmt1,frmt2
 character(len=50),parameter :: sub_name='outkss.F90'
 character(len=10) :: stag(2)=(/'          ','          '/)
!arrays 
 integer :: flag(16),gbas(3),gcur(3),geq(3),nbandkssk(nkpt)
 integer,pointer :: symrel2(:,:,:)
 integer,allocatable :: symrec2t(:,:,:)
 integer :: inversion(3,3)
 integer,allocatable,target :: vkbsign_int(:,:)
 integer,allocatable :: gbase(:,:),gbasek(:,:,:),gbig(:,:),gbound(:,:)
 integer,allocatable :: gcurr(:,:),gshell(:,:),ifail(:),insort(:),iwork(:),gtmp(:,:)
 integer,allocatable :: kg_dum(:,:),nbasek(:),nshell(:),shlim(:)
 integer,allocatable :: nband_save(:)
 integer,allocatable,target :: kg_k(:,:)
 integer,allocatable :: trsl(:)
 integer, allocatable :: npwarr_save(:)
 integer,allocatable :: dimlmn(:)
 real(dp) :: nonlop_dum(1,1),xcart(3,natom),paw_ovlp(2),ovlp(2)
 real(dp) :: enlout(1),gcar(3),kpoint(3),tsec(2),ylmgr_dum(1),pij_bks(2)
 real(dp),pointer :: tnons2(:,:)
 real(dp),allocatable :: cg_disk(:,:),cnorm(:),cnormk(:,:),cwork(:,:),ctmp(:)
 real(dp),allocatable :: eig_dum(:),eigval(:),eigvec(:,:,:),ekb(:,:,:),en(:),ffnl(:,:,:,:)
 real(dp),allocatable :: ghg(:,:),gvnlg(:,:),kinpw(:),kpg_dum(:,:),modkplusg(:),oc(:),occ_dum(:),occ_save(:)
 real(dp),allocatable :: occ_k(:),ph3d(:,:,:),phkxred(:,:),pwave(:,:),pwave_so(:,:)
 real(dp),allocatable :: rwork(:),subghg(:,:),subghg_so(:,:),vkb(:,:,:),vkbd(:,:,:)
 real(dp),allocatable,target :: vkb_tgt(:, :, :), vkbd_tgt(:, :, :)
 real(dp),allocatable :: vkbsign(:,:)
 real(dp),allocatable :: vlocal(:,:,:),work(:,:,:,:),ylm_k(:,:)
 real(dp),allocatable,target :: wfg(:,:,:)
 real(dp),pointer :: wfg1(:,:),wfg2(:,:)
 type(Cprj_type) :: Cprj_dum(1)
 type(Cprj_type),allocatable :: Cprjnk_k(:,:)
 type(Wvl_wf_type) :: Dummy_wfs

#if defined HAVE_ETSF_IO
 type(etsf_dims) :: dims
 logical :: lstat
 type(etsf_io_low_error) :: error
 character(len=etsf_io_low_error_len) :: errmess
 type(dataset_type) :: dtset_cpy
 type(etsf_gwdata) :: gwdata
 type(wffile_type) :: wff
 integer :: ncid
 !integer, allocatable :: tmp_npwarr(:)
 !real(dp), pointer :: occ_pt(:)
#endif
! *********************************************************************

#ifdef VMS
!DEC$ ATTRIBUTES ALIAS:'ZHPEV' :: zhpev
!DEC$ ATTRIBUTES ALIAS:'ZHPEVX' :: zhpevx
#endif

#if defined DEBUG_MODE
 write(msg,'(a)') 'outkss: enter'
 call wrtout(std_out,msg,'COLL') 
 call flush_unit(std_out)
#endif

 call timab(234,1,tsec)
 call xcomm_init(MPI_enreg,spaceComm)
 call xmaster_init(MPI_enreg,master)
 call xproc_max(nprocs,ierr)

!MG: since in seq case MPI_enreg%proc_distrb is not defined
!we hack a bit the data type in order to get rid of MPI preprocessing options.
!The previous status of %proc_distrb is restored before exiting.
!Note that in case of seq run MPI_enreg%proc_distrb is nullified at the very beginning of abinit.F90
!
!FIXME this is a design flaw that should be solved: proc_distrb should always 
!be allocated and filled with %me in case of sequential run otherwise checks like
!if (nprocs>1.and.MPI_enreg%proc_distrb(ii)==me) leads to SIGFAULT under gfortran.
!as the second array is not allocated.
 lhack=.FALSE.
 if (nprocs==1) then 
  ltest=ASSOCIATED(MPI_enreg%proc_distrb)
! call assert(ltest,'%proc_distrb not associated',__FILE__,__LINE__)
  if (.not.ltest) then 
   allocate(MPI_enreg%proc_distrb(nkpt,mband,nsppol))
   MPI_enreg%proc_distrb=MPI_enreg%me
   lhack=.TRUE.
  end if
  ltest=ALL(MPI_enreg%proc_distrb(:,:,:)==MPI_enreg%me)
  call assert(ltest,'wrong values in %proc_distrb',__FILE__,__LINE__)
 end if
!
!=== Write preliminary message ===
 if (kssform==3) then
  write(msg,'(a,70("="),4a)')ch10,ch10,&
&  ' Calculating and writing out Kohn-Sham electronic Structure file',ch10, &
&  ' Using conjugate gradient wavefunctions and energies (kssform=3)'
 else
  write(msg,'(a,70("="),4a,i1,a)') ch10,ch10, &
&  ' Calculating and writing out Kohn-Sham electronic Structure file',ch10, &
&  ' Using diagonalized wavefunctions and energies (kssform=',kssform,')'
 end if
 call wrtout(std_out,msg,'COLL') 
 call wrtout(ab_out,msg,'COLL')
!
!=== Perform some tests ===
!* Check whether nband is a constant for all k point
 if (Dtset%occopt>=2.and.Dtset%occopt<=7) then
  do isppol=1,nsppol
   do ikpt=1,nkpt
    if (Dtset%nband(ikpt+(isppol-1)*nkpt)/=Dtset%nband(1)) then
     write(msg,'(6a,i4,a,i3,a,i4,3a)')ch10,&
&     ' outkss: ERROR -',ch10,&
&     '  The number of bands must be the same for all k-points ',ch10,&
&     '  but nband(1)=',Dtset%nband(1),' is different of nband(',&
&     ikpt+(isppol-1)*nkpt,')=',Dtset%nband(ikpt+(isppol-1)*nkpt),'.',ch10,&
&     '  Program does not stop but _KSS file will not be created...'
     call wrtout(std_out,msg,'COLL') 
     call wrtout(ab_out,msg,'COLL') ; RETURN
    end if
   end do
  end do
 end if
!* istwfk must be 1 for each k-point
 if (ANY(Dtset%istwfk(1:nkpt)/=1)) then
  write(msg,'(10a)') ch10,&
&  ' outkss: ERROR -',ch10,&
&  '  nbandkss<>0 and istwfk<>1 not allowed:',ch10,&
&  '  States output not programmed for time-reversal symmetry.',ch10,&
&  '  Action : change istwfk in input file (put it to 1 for all kpt).',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!* Check nspinor
!NOTE : at least the use of fftpac should be modified to cope with nspinor==2
 if (nspinor/=1.and.kssform/=3) then
  write(msg,'(6a)')ch10,&
&  ' outkss: ERROR -',ch10,&
&  '  Variable nspinor should be 1 !',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!* Check spin-orbit
 if (Psps%mpssoang/=mpsang) then
  write(msg,'(6a)')ch10,&
&  ' outkss: ERROR -',ch10,&
&  '  Variable mpspso should be 1 !',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!* Check mproj
 mproj=MAXVAL(Psps%indlmn(3,:,:))
 if (mproj>1) then
  if (kssform/=3) then
   write(msg,'(8a)')ch10,&
&   ' outkss : ERROR -',ch10,&
&   '  The present version of outkss does not accept pseudopotentials',ch10,&
&   '  with more than one projector per angular momentum channel when kssform is different from 3.',ch10,&
&   '  Program does not stop but _KSS file will not be created...'
   call wrtout(std_out,msg,'COLL') 
   call wrtout(ab_out,msg,'COLL')
   write(*,*)Psps%indlmn(3,:,:) ; write(*,*)Psps%indlmn(1,:,:) ; RETURN
  else if (Psps%usepaw/=1) then
!  TODO
   write(msg,'(8a)')ch10,&
&   ' outkss : COMMENT - ',ch10,&
&   ' At least one NC pseudopotential has more that one projector per angular channel',ch10,&
&   ' Note that inclvkb==0 should be used in screening, since the evaluation of the commutator',ch10,&
&   ' for this particular case is not implemented yet'
   call wrtout(std_out,msg,'COLL') 
   call wrtout(ab_out,msg,'COLL') 
  end if
 end if
!* Check max angular momentum
 if (MAXVAL(Psps%indlmn(1,:,:))+1 >= 5) then 
  write(msg,'(6a)')ch10,&
&  ' outkss : ERROR -',ch10,&
&  '  Pseudopotentials with f-projectors not implemented',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  write(*,*)Psps%indlmn(1,:,:) ; RETURN
 end if
!* Check parareel
 if (Dtset%parareel/=0) then
  write(msg,'(6a)')ch10,&
&  ' outkss: ERROR -',ch10,&
&  '  outkss cannot be used with parareel/=0 !',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!* Check useylm
 if (Psps%useylm/=0.and.Psps%usepaw==0) then
  write(msg,'(6a)')ch10,&
&  ' outkss : ERROR -',ch10,&
&  '  The present version of outkss does not work with useylm/=0 !',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!* Check PAW and kssform value 
 if (Psps%usepaw/=0) then 
  if (kssform/=3) then
   write(msg,'(6a)')ch10,&
&   ' outkss : ERROR -',ch10,&
&   '  The present version of outkss does not work in PAW mode if kssform/=3!',ch10,&
&   '  Program does not stop but _KSS file will not be created...'
   call wrtout(std_out,msg,'COLL') 
   call wrtout(ab_out,msg,'COLL') ; RETURN
  end if
  if (usecprj/=1) then
   write(msg,'(6a)')ch10,&
&   ' outkss ERROR -',ch10,&
&   '  If PAW and kssform=1, usecprj must be 1',ch10,&
&   '  Program does not stop but _KSS file will not be created...'
   call wrtout(std_out,msg,'COLL') 
   call wrtout(ab_out,msg,'COLL') ; RETURN
  end if
  if (mkmem==0) then
   write(msg,'(6a)')ch10,&
&   ' outkss ERROR -',ch10,&
&   '  PAW with mkmem==0 not yet implemented ',ch10,&
&   '  Program does not stop but _KSS file will not be created...'
   call wrtout(std_out,msg,'COLL') 
   call wrtout(ab_out,msg,'COLL') ; RETURN
  end if
 end if
!* Check bands parallelization
 if (MPI_enreg%paralbd/=0) then
  write(msg,'(6a)')ch10,&
&  ' outkss: ERROR -',ch10,&
&  '  outkss cannot be used with parallelization on bands (paralbd/=0) !',ch10,&
&  '  Program does not stop but _KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!
!Estimate required memory in case of diagonalization.
!TODO to be modified to take into account the case nsppol=2
 if (kssform/=3) then 
  call memkss(mband,mgfft,mkmem,MPI_enreg,mproj,Psps%mpssoang,mpw,natom,Dtset%ngfft,nkpt,nspinor,nsym,ntypat)
 end if
!
!=== Initialize some variables ===
 if (nsppol==2) stag(:)=(/'SPIN UP:  ','SPIN DOWN:'/)
 n1=Dtset%ngfft(1) ; n2=Dtset%ngfft(2) ; n3=Dtset%ngfft(3)
 n4=Dtset%ngfft(4) ; n5=Dtset%ngfft(5) ; n6=Dtset%ngfft(6)

 sizepw=2*mpw ; diago=(kssform/=3)
 allocate(dimlmn(natom*Psps%usepaw))
 if (Psps%usepaw==1) then 
  do iatom=1,natom
   itypat=Dtset%typat(iatom)
   dimlmn(iatom)=Pawtab(itypat)%lmn_size
  end do
 end if
!
!============================================================
!=== Prepare set containing all G-vectors sorted by stars ===
!============================================================
 write(msg,'(2a)')ch10,' Sorting g-vecs for an output of states on an unique "big" PW basis.'
 call wrtout(std_out,msg,'COLL')
!
!=== Analyze symmetry operations ===
 if (Dtset%symmorphi==0) then 
! Old (Obsolete) implementation: Suppress inversion from symetries list:
  nullify(symrel2,tnons2)
  call remove_inversion(nsym,Dtset%symrel,Dtset%tnons,nsym2,symrel2,tnons2,pinv)
  if (ANY(ABS(tnons2(:,1:nsym2))>tol8)) then
   write(msg,'(5a)')' - outkss - ERROR:',ch10,&
&   '   Non-symmorphic operations still remain in the symmetries list ',ch10,&
&   '   Program does not stop but _KSS file will not be created...'
   call wrtout(std_out,msg,'COLL') ; RETURN
  end if
 else if (Dtset%symmorphi==1) then 
! If in the input file symmorphi==1 all the symmetry operations are retained:
! both identity and inversion (if any) as well as non-symmorphic operations.
  nsym2=nsym ; pinv=1
  allocate(symrel2(3,3,nsym),tnons2(3,nsym))
  symrel2(:,:,:)=Dtset%symrel(:,:,1:nsym)
  tnons2(:,:)   =Dtset%tnons(:,1:nsym)
 else  
  write(msg,'(3a,i4,4a)')ch10,&
&  ' outkss: BUG -',ch10,&
&  ' symmorphi = ',Dtset%symmorphi,ch10,' while it must be 0 or 1',ch10,&
&  ' Program does not stop but KSS file will not be created...'
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL') ; RETURN
 end if
!* Here we use TRANSPOSE(symrel2) instead of the more intuitive symrel2^{-1t} for historical reasons
!It does not affect the results since in the code below we only check the module of G
 allocate(symrec2t(3,3,nsym2))
 do isym=1,nsym2
  symrec2t(:,:,isym)=TRANSPOSE(symrel2(:,:,isym))
 end do
!
!=== Search for base g-vectors generating all G-stars ===
 ikg=0 
 if (mkmem==0) rewind(unit=Dtfil%unkg)
 allocate(nbasek(nkpt),gbasek(3,mpw,nkpt),cnormk(mpw,nkpt),gcurr(3,mpw))
 nbasek(:)=0 ; cnormk(:,:)=zero ; gbasek(:,:,:)=0
!
!Loop over k-points, in case of k-point parallelism each processor 
!takes care of its own set of g-vectors. In this case, indeed, kg 
!contains only mkmem G-spheres, where mkmem<nkpt.
 do ikpt=1,nkpt

  if (MPI_enreg%paral_compil_kpt==1) then
   if (MINVAL(ABS(MPI_enreg%proc_distrb(ikpt,1,1:nsppol)-MPI_enreg%me))/=0) CYCLE
  end if

  if (mkmem==0) then
!  * Read g-vectors corresponding to this k-point
   read(Dtfil%unkg) npw_k
   read(Dtfil%unkg)
   read(Dtfil%unkg) ((gcurr(ii,ig),ii=1,3),ig=1,npw_k)
  else
   npw_k=npwarr(ikpt)
   do ig=1,npw_k
    gcurr(:,ig)=kg(:,ikg+ig)
   end do
   if (ANY(gcurr(:,1)/=0)) stop 'bug gcurr in outkss'
  end if
  ikg=ikg+npw_k
! 
! === Search for the G"s generating the others by symmetry ===
! * Here symrec2t=TRANSPOSE(symrel2) for historical reasons, see note above
  call get_irredg(npw_k,nsym2,pinv,gprimd,symrec2t,gcurr,nbasek(ikpt),gbasek(:,:,ikpt),cnormk(:,ikpt))
 end do ! k-points

 if (MPI_enreg%paral_compil_kpt==1) then
  call xsum_mpi(nbasek,spaceComm,ierr)
  call xsum_mpi(cnormk,spaceComm,ierr)
  call xsum_mpi(gbasek,spaceComm,ierr)
 end if
!
!=== Reduce info over k-points ===
 nbase=0
 allocate(gbase(3,sizepw),cnorm(sizepw))
!* Here symrec2t=TRANSPOSE(symrel2) for historical reasons, see note above
 call merge_kgirr(nsym2,pinv,nkpt,mpw,sizepw,symrec2t,nbasek,cnormk,gbasek,nbase,gbase,cnorm,ierr)
 if (ierr/=0) RETURN 

 deallocate(nbasek,cnormk,gbasek)
!
!=== Reorder base g-vectors in order of increasing module ===
!
!Generate all shells of G-vectors: star of a g==set of all symetrics of this g
 allocate(gbig(3,sizepw),gshell(3,2*nsym2),shlim(nbase))
!
!* Here symrec2t=TRANSPOSE(symrel2) for historical reasons, see note above
!call getfullg(nbase,nsym2,pinv,sizepw,gbase,symrec2t,cnorm,maxpw,gbig,shlim,ierr)
!if (ierr/0) RETURN
!
#if 1
 allocate(insort(nbase),nshell(nbase))
 do in=1,nbase
  insort(in)=in
 end do
 call sort_dp(nbase,cnorm,insort,tol14)
!
!Loop over all different modules of g''s (=shells):
 maxpw=0
 do in=1,nbase
  nshell(in)=0
  gcur(:)=gbase(:,insort(in))
! Loop over all symetries:
  do is=1,nsym2
   do iinv=pinv,1,2
    geq(:)=iinv*(symrel2(1,:,is)*gcur(1)+symrel2(2,:,is)*gcur(2)+symrel2(3,:,is)*gcur(3))
!   Search for symetric of g and eventually add it:
    found=.FALSE. ; ish=1
    do while ((.not.found) .and. (ish<=nshell(in)))
     found=ALL(geq(:)==gshell(:,ish))
     ish=ish+1
    end do
    if (.not.found) then
     nshell(in)=nshell(in)+1
     gshell(:,nshell(in))=geq(:)
    end if
   end do
  end do
! 
! Store this shell of g''s in a big array of g (gbig):
  if ((maxpw+nshell(in)) > sizepw) then
!  We need to increase the size of the gbase, gbig and cnorm arrays while still keeping their content.
!  This is done using two temporary arrays gtmp and ctmp
   allocate(ctmp(sizepw),gtmp(3,sizepw))
   sizepw=maxpw+nshell(in)

   ctmp(:)=cnorm(:)
   gtmp(:,:)=gbase(:,:)

   deallocate(cnorm) ; allocate(cnorm(sizepw))
   cnorm(:)=ctmp(:)
   deallocate(ctmp)

!  MG why this? gbase should not be changed!
   deallocate(gbase) ; allocate(gbase(3,sizepw))
   gbase(:,:)=gtmp(:,:)
   gtmp(:,:)=gbig(:,:)

   deallocate(gbig)  ; allocate(gbig(3,sizepw))
   gbig(:,:)=gtmp(:,:)
   deallocate(gtmp)
  end if

  do ig=1,nshell(in)
   gbig(:,ig+maxpw)=gshell(:,ig)
  end do
  maxpw=maxpw+nshell(in)

 end do ! End loop over shells
!
!Compute shell limits
 ilim=0
 do in=1,nbase
  ilim=ilim+nshell(in)
  shlim(in)=ilim
 end do
!Print out shell limits
 write(msg,'(3a)')&
& ' Shells found:',ch10,&
& ' number of shell    number of G vectors      cut-off energy'
 call wrtout(std_out,msg,'COLL')
 do in=1,nbase
  write(msg,'(12x,i4,17x,i6,12x,f8.3)')in,shlim(in),2*pi**2*cnorm(in)
  call wrtout(std_out,msg,'COLL')
 end do
 write(msg,'(a)')ch10 
 call wrtout(std_out,msg,'COLL')
 deallocate(gshell,insort,nshell)
#endif 

 deallocate(gcurr,gbase,cnorm) 
!
!=== Determine optimal number of bands and G"s to be written ===
 npwkss=Dtset%npwkss
 if ((npwkss==0).or.(npwkss>=maxpw)) then
  npwkss=maxpw
  write(msg,'(5a)')&
&  ' Since the number of g''s to be written on file',ch10,&
&  ' was 0 or too large, it has been set to the max. value.,',ch10,&
&  ' computed from the union of the sets of G vectors for the different k-points.'
  call wrtout(std_out,msg,'COLL')
 end if

 ishm=0
 do ii=1,nbase
  if (shlim(ii)<=npwkss) then
   ishm=ii
  else
   EXIT
  end if
 end do
!£ ishm=bisect(shlim,npwkss)

 if (shlim(ishm)/=npwkss) then
  nrst1=shlim(ishm)
  nrst2=MIN0(shlim(MIN0(ishm+1,nbase)),maxpw)
  if (IABS(npwkss-nrst2)<IABS(npwkss-nrst1)) nrst1=nrst2
  npwkss=nrst1 
  if (shlim(ishm)<npwkss) ishm=ishm+1
  write(msg,'(3a)')&
&  ' The number of G''s to be written on file is not a whole number of stars ',ch10,&
&  ' the program set it to the nearest star limit.'
  call wrtout(std_out,msg,'COLL')
 end if
 write(msg,'(a,i5)')' Number of g-vectors written on file is: ',npwkss
 call wrtout(std_out,msg,'COLL')
!
!=== Check on the number of stored bands ===
 if (diago) then

  if (Dtset%nbandkss==-1.or.Dtset%nbandkss>=maxpw) then
   nbandkssk(1:nkpt)=npwarr(1:nkpt)
   write(msg,'(6a)')ch10,&
&   ' Since the number of bands to be computed was (-1) or',ch10,&
&   ' too large, it has been set to the max. value. allowed for each k,',ch10,&
&   ' thus, the minimum of the number of plane waves for each k point.'
   call wrtout(std_out,msg,'COLL')
  else
   nbandkssk(1:nkpt)=Dtset%nbandkss
   found=.FALSE.
   do ikpt=1,nkpt
    if (Dtset%nbandkss>npwarr(ikpt)) then
     nbandkssk(ikpt)=npwarr(ikpt)
     found=.TRUE.
    end if
   end do
   if (found) then
    write(msg,'(10a)')&
&    ' outkss:  WARNING -',ch10,&
&    ' The value choosen for the number of bands in file',ch10,&
&    ' (nbandkss) was greater than at least one number of plane waves ',ch10,&
&    ' for a given k-point (npw_k).',ch10,' It has been modified consequently.'
    call wrtout(std_out,msg,'COLL')
   end if
  end if
  found=.FALSE.
  do ikpt=1,nkpt
   if (nbandkssk(ikpt)>npwkss) then
    nbandkssk(ikpt)=npwkss
    found=.TRUE.
   end if
  end do
  if (found) then
   write(msg,'(7a)')' outkss: WARNING -',ch10,&
&   ' The number of bands to be computed (for one k) was',ch10,&
&   ' greater than the number of g-vectors to be written.',ch10,&
&   ' It has been modified consequently.'
   call wrtout(std_out,msg,'COLL')
  end if
  nbandksseff=MINVAL(nbandkssk)

 else ! .not. diago
  do ikpt=1,nkpt
   do isppol=1,nsppol
    nbandkssk(ikpt)=Dtset%nband(ikpt+(isppol-1)*nkpt)
   end do
  end do
  nbandksseff=MINVAL(nbandkssk)
  if (Dtset%nbandkss>0 .and. Dtset%nbandkss<nbandksseff) then 
   write(msg,'(3a,i5,a,i5,2a)')&
&   ' outkss : COMMENT ',ch10,&
&   ' Number of bands calculated=',nbandksseff,', greater than nbandkss=',Dtset%nbandkss,ch10,&
&   ' will write nbandkss bands on the KSS file'
   call wrtout(std_out,msg,'COLL')
   nbandksseff=Dtset%nbandkss
  end if 
 end if

 write(msg,'(a,i5)')' Number of bands written on file is: ',nbandksseff
 call wrtout(std_out,msg,'COLL')

 found=.FALSE.
 if (ANY(nbandkssk(1:nkpt)<npwarr(1:nkpt))) found=.TRUE.

 if (diago) then
  if (found) then
   write(msg,'(6a)')ch10,&
&   ' Since the number of bands to be computed',ch10,&
&   ' is less than the number of G-vectors found,',ch10,&
&   ' the program will perform partial diagonalizations.'
  else
   write(msg,'(6a)')ch10,&
&   ' Since the number of bands to be computed',ch10,&
&   ' is equal to the nb of G-vectors found for each k-pt,',ch10,&
&   ' the program will perform complete diagonalizations.'
  end if
  call wrtout(std_out,msg,'COLL')
 end if
!
!==========================================================================
!=== Open KSS file for output, write header with dimensions and kb sign ===
!==========================================================================
!
!* Output required disk space.
 call dsksta(ishm,Psps%usepaw,nbandksseff,mpsang,natom,ntypat,npwkss,nkpt,nspinor,nsppol,nsym2,dimlmn)

 if (MPI_enreg%me==master) then
  filekss=TRIM(Dtfil%filnam_ds(4))//'_KSS'
  write(msg,'(3a)')ch10,' Opening file for KS structure output: ',TRIM(filekss)
  call wrtout(std_out,msg,'COLL')

  if (Dtset%accesswff==0) then
   open(untkss,file=filekss,form='unformatted')
#if defined HAVE_ETSF_IO
  else if (Dtset%accesswff==3) then
!  The ETSF output is a first basic attempt, branching all write calls.
   call dtsetcopy(dtset_cpy, Dtset)
   dtset_cpy%mpw   = npwkss
   dtset_cpy%mband = nbandksseff
!  We currently use the dataset symmetries, as defined in the Hdr structure
!  instead of the symmetries recomputed here.
   call abi_etsf_init(dtset_cpy, filekss, 4, .false., Hdr%lmn_size, Psps, Dummy_wfs)
!  Complete the geometry information with missing values from hdr_io().
   call abi_etsf_geo_put(dtset_cpy, filekss, Psps, rprimd, xred)
!  We open again for further additions
   call etsf_io_low_open_modify(ncid, trim(filekss)//"-etsf.nc", lstat, error_data = error)
   if (.not. lstat) then
    call etsf_io_low_error_to_str(errmess, error)
    write(msg,'(4a)')ch10,' outkss: ERROR -',ch10,errmess(1:min(475, len(errmess)))
    call wrtout(std_out, msg, 'COLL') 
    call leave_new('COLL')
   end if
#endif
  end if
! 
! Update some values in Hdr, write the header on the KSS file then restore previous values
! Note that nsym and symrel might have been changed this has to be fixed
! carefully in the next patch since in the new implementation symmorphy=0 should be dafault
  allocate(occ_save(Hdr%bantot)) ; occ_save(:)=Hdr%occ
  bantot_save=Hdr%bantot ; Hdr%bantot=nbandksseff*nkpt*nsppol
  deallocate(Hdr%occ) ; allocate(Hdr%occ(Hdr%bantot))
! Copy the occ number in the new header, fill with zero the rest since mband can be < nbandksseff 
  Hdr%occ=zero ; nb=MIN(mband,nbandksseff)
  do is=1,nsppol
   do ik=1,nkpt
    ind1=1+(ik-1)*nbandksseff+(is-1)*nkpt*nbandksseff 
    ind2=1+(ik-1)*mband      +(is-1)*nkpt*mband
    Hdr%occ(ind1:ind1+nb-1) = occ(ind2:ind2+nb-1)
   end do
  end do

  allocate(npwarr_save(nkpt)) ; npwarr_save=Hdr%npwarr ; Hdr%npwarr=npwkss
  allocate(nband_save(nkpt*nsppol)) ; nband_save=Hdr%nband ; Hdr%nband=nbandksseff

  fform=502 ; rdwr=2
  if (Dtset%accesswff == 0) then
   call hdr_io(fform,Hdr,rdwr,untkss)
   title='Results from ABINIT code'          ; write(untkss) title(1:80)
   title='Ab-initio plane waves calculation' ; write(untkss) title(1:80)
!  To be modified to deal with more than one projector
   write(untkss) nsym2,nbandksseff,npwkss,ishm,mpsang

#if defined HAVE_ETSF_IO
  else if (Dtset%accesswff == 3) then
!  Since we are not k_dependent here, we hack a little npwarr
!  to have the right values for not k_dependent case.
!  MG Mon Oct 22 hacking moved above, so fortran and etsf have the same header
   call hdr_io_etsf(fform,Hdr,rdwr,ncid)
!  MG THIS comes from Damien branch, anyway now Hdr%occ should have the correct size bantot
!  occ_pt => Hdr%occ
!  nullify(Hdr%occ)
!  call hdr_io_etsf(fform,Hdr,rdwr,ncid)
!  Hdr%npwarr = tmp_npwarr
!  deallocate(tmp_npwarr)
!  Hdr%occ => occ_pt
!  END MG
#endif
  end if
! Restore old values in the header since Hdr% can be used by other routines
  Hdr%bantot=bantot_save ; deallocate(Hdr%occ) ; allocate(Hdr%occ(Hdr%bantot)) 
  Hdr%occ=occ_save       ; deallocate(occ_save)
  Hdr%npwarr=npwarr_save ; deallocate(npwarr_save)
  Hdr%nband=nband_save   ; deallocate(nband_save)

  write(msg,'(a,i6)') ' number of Gamma centered plane waves ',npwkss
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL')
  write(msg,'(a,i6)') ' number of Gamma centered shells ',ishm
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL')
  write(msg,'(a,i6)') ' number of bands ',nbandksseff
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL')
  write(msg,'(a,i6)') ' maximum angular momentum components ',mpsang
  call wrtout(std_out,msg,'COLL') 
  call wrtout(ab_out,msg,'COLL')
  write(msg,'(a,i2,a)')' number of symmetry operations ',nsym2,' (without inversion)'
  call wrtout(std_out,msg,'COLL')
  if (Dtset%accesswff==0) then
   write(untkss) (((symrel2(ii,jj,kk),ii=1,3),jj=1,3),kk=1,nsym2)
   write(untkss) ((tnons2(ii,kk),ii=1,3),kk=1,nsym2)
   write(untkss) ((gbig(ii,ig),ii=1,3),ig=1,npwkss)
   write(untkss) (shlim(in),in=1,ishm)
  end if
! 
! === For NC pseudos, write vkbsign ===
! FIXME : The present version of outkss deals only with one projector in each angular 
! momentum channel (XG 010927) The allocation is done in the wrong order for dimensions...
  if (Psps%usepaw==0) then
   if (Dtset%accesswff==0) then
    allocate(vkbsign(ntypat,mpsang)) ; vkbsign(:,:)=zero
    do is=1,ntypat
     il0=0 
     do ilmn=1,Psps%lmnmax
      il=1+Psps%indlmn(1,ilmn,is)
      in=Psps%indlmn(3,ilmn,is)
      if (il/=il0 .and. in==1) then
       il0=il
       vkbsign(is,il)=DSIGN(one,Psps%ekb(ilmn,is))
      end if
     end do
    end do
    write(untkss) ((vkbsign(is,il),il=1,mpsang),is=1,ntypat)
    deallocate(vkbsign)
#if defined HAVE_ETSF_IO
   else if (Dtset%accesswff == 3) then
    allocate(vkbsign_int(mpsang,ntypat)) ; vkbsign_int(:,:)=0
    do is=1,ntypat
     il0=0
     do ilmn=1,Psps%lmnmax
      il=1+Psps%indlmn(1,ilmn,is)
      in=Psps%indlmn(3,ilmn,is)
      if ((il/=il0).and.(in==1)) then
       il0=il
       vkbsign_int(il,is)=NINT(DSIGN(one,Psps%ekb(ilmn,is)))
      end if
     end do
    end do
    gwdata%kb_formfactor_sign%data2D => vkbsign_int
!   We write it now to be able to deallocate quickly.
    call etsf_io_gwdata_put(ncid, gwdata, lstat, error)
    if (.not.lstat) then
     call etsf_io_low_error_to_str(errmess, error)
     write(msg, '(4a)')ch10,' outkss: ERROR -',ch10,errmess(1:min(475, len(errmess)))
     call wrtout(std_out, msg, 'COLL') 
     call leave_new('COLL')
    end if
    nullify(gwdata%kb_formfactor_sign%data2D)
    deallocate(vkbsign_int)
#endif
   end if
  end if ! usepaw=0

  do ig=1,MIN(8,npwkss)
   write(msg,'(a,i2,a,3i3)')'   *   g(',ig,')=',gbig(:,ig)
   call wrtout(std_out,msg,'COLL')
  end do
  do ig=MAX(MIN(8,npwkss),npwkss-6),npwkss
   write(msg,'(a,i4,a,3i3)')'   *   g(',ig,')=',gbig(:,ig)
   call wrtout(std_out,msg,'COLL')
  end do

 end if ! master wrote the header

 deallocate(shlim)

 if (diago)      write(msg,'(a)')' Diagonalized eigenvalues'
 if (.not.diago) write(msg,'(a)')' Conjugate gradient eigenvalues'
 call wrtout(ab_out,msg,'COLL')
 if (Dtset%enunit==1) then
  write(msg,'(a)') '   k    eigenvalues [eV]'
 else
  write(msg,'(a)') '   k    eigenvalues [Hartree]'
 end if
 call wrtout(ab_out,msg,'COLL')
!
!=== Prepare WF file for reading ===
 if ((.not.diago).and.mkmem==0) then
  mcg_disk=mpw*nspinor*mband
  allocate(eig_dum(mband),kg_dum(3,0),occ_dum(mband),cg_disk(2,mcg_disk))
  call clsopn(Wffnow)
  call hdr_skip(Wffnow,ierr)
! Define offsets, in case of MPI I/O
! call WffKg(Wffnow,0)
! call xdefineOff(0,Wffnow,MPI_enreg,Dtset%nband,npwarr,nspinor,nsppol,nkpt)
 end if
 call timab(234,2,tsec)
!
!Non-local factors:
!Norm-conserving: kleimann-Bylander energies
 if (Psps%usepaw==0) then
  dimekb1=Psps%dimekb ; dimekb2=ntypat
  allocate(ekb(Psps%dimekb,ntypat,nspinor**2))
  ekb(:,:,1)=Psps%ekb(:,:)
  if (nspinor==2) then
   ekb(:,:,2)=Psps%ekb(:,:)
   ekb(:,:,3:4)=zero
  end if
 else
! Not available within PAW
  allocate(ekb(Psps%dimekb,natom,nspinor**2))
 end if

 k_index=0 ; bdtot_index=0 ; ibg=0
 allocate(vlocal(n4,n5,n6))
!
!=== Loop over spin ===
 do isppol=1,nsppol
  ikg=0
  if (mkmem==0) rewind (unit=Dtfil%unkg)
  if (mkmem==0.and.Psps%useylm==1) rewind Dtfil%unylm
! 
! * Set up local potential vlocal_sigma with proper dimensioning, from vtrial
! FIXME this should be modified to cope with nspinor==2 if kssform/=3
  call fftpac(isppol,nsppol,n1,n2,n3,n4,n5,n6,Dtset%ngfft,vtrial,vlocal,2)
! 
! === Loop over k-points ===
  do ikpt=1,nkpt
   call timab(235,1,tsec)

   nband_k=Dtset%nband(ikpt+(isppol-1)*nkpt)
   npw_k=npwarr(ikpt)
   istwf_k=Dtset%istwfk(ikpt)
   kpoint(:)=Dtset%kpt(:,ikpt)
   nbandkss_k=nbandkssk(ikpt)
   if (diago) ngdiago=npw_k
   allocate(ylm_k(npw_k,mpsang*mpsang*Psps%useylm))
   allocate(trsl(maxpw))

   if (MPI_enreg%proc_distrb(ikpt,1,isppol)==MPI_enreg%me) then
    write(msg,'(2a,i3,3x,a)')ch10,' k-point ',ikpt,stag(isppol)
    call wrtout(std_out,msg,'PERS')
!   
!   === Load G-vectors, for this k-point ===
    allocate(gbound(2*mgfft+8,2),kg_k(3,npw_k))
    if (mkmem==0) then
     call rdnpw(ikpt,isppol,nband_k,npw_k,nspinor,0,Dtfil%unkg)
     read (Dtfil%unkg) kg_k(1:3,1:npw_k)
     call sphereboundary(gbound,istwf_k,kg_k,mgfft,npw_k)
     if (Psps%useylm==1) then
      read(Dtfil%unylm)
      read(Dtfil%unylm) ((ylm_k(ipw,ilm),ipw=1,npw_k),ilm=1,mpsang*mpsang)
     end if
    else
     kg_k(:,1:npw_k)=kg(:,1+ikg:npw_k+ikg)
     call sphereboundary(gbound,istwf_k,kg_k,mgfft,npw_k)
     if (Psps%useylm==1) then
      do ilm=1,mpsang*mpsang
       ylm_k(1:npw_k,ilm)=ylm(1+ikg:npw_k+ikg,ilm)
      end do
     end if
     ikg=ikg+npw_k
    end if
!   
!   I-Associate set of g-vectors with the big array of g''s:
!   The array gbig(3,ig=1,maxpw) contains all g-vectors used for all k-points, in order of 
!   increasing shells. For a each k-point, the wave-functions are defined only on a particular set 
!   of g-vectors g''(k) (included in gbig). This set is defined by array trsl:
!   The array trsl(ig=1,maxpw) translates the index of the gbig (from 1 to maxpw) into the corresponding
!   index in array g''(k). If gbig(ig) does not exist in  g''(k), trsl(ig) contains npw(k)+1.
!   
!   Initialize array trsl
    trsl(:)=npw_k+1
!   
!   Loop over g-vectors, for this k point:
    do ig=1,npw_k
     gcur(:)=kg_k(:,ig)
!    Search selected vector in array gbig:
     igp=0 ; found=.FALSE.
     do while ((.not.found) .and. igp<maxpw)
      igp=igp+1
      found=ALL(gcur(:)==gbig(:,igp))
     end do
!    Store it if found:
     if (found) then
      trsl(igp)=ig
     else
      write(msg,'(6a)')ch10,&
&      ' outkss: BUG -',ch10,&
&      '  The set of g vectors is inconsistent ! Check source.',ch10,&
&      '  Program does not stop but _KSS file will not be created...'
      call wrtout(std_out,msg,'COLL') 
      call wrtout(ab_out,msg,'COLL') ; RETURN
     end if
    end do
!   
!   === Re-evaluate some useful quantities ===
    allocate(phkxred(2,natom))
    dimffnl=2 ;  if (Psps%usepaw==1) dimffnl=1+3
    allocate(ffnl(npw_k,dimffnl,Psps%lmnmax,ntypat))
    allocate(kinpw(npw_k),modkplusg(npw_k))
    
    nkpg=0
    do ia=1,natom
     iatom=atindx(ia)
     arg=two_pi*(kpoint(1)*xred(1,ia)+kpoint(2)*xred(2,ia)+kpoint(3)*xred(3,ia))
     phkxred(1,iatom)=COS(arg)
     phkxred(2,iatom)=SIN(arg)
    end do

    ider=1 ; idir=0
    allocate(kpg_dum(npw_k,nkpg)) !this is only used if useylm==1
    call mkffnl(Psps%dimekb,dimffnl,Psps%ekb,ffnl,Psps%ffspl,gmet,gprimd,ider,idir,Psps%indlmn,&
&    kg_k,kpg_dum,kpoint,Psps%lmnmax,Psps%lnmax,Psps%mpsang,Psps%mqgrid_ff,nkpg,npw_k,& 
&    ntypat,Psps%pspso,Psps%qgrid_ff,rmet,Psps%usepaw,Psps%useylm,ylm_k,ylmgr_dum)
    deallocate(kpg_dum)

    call mkkin(ecut,Dtset%ecutsm,Dtset%effmass,gmet,kg_k,kinpw,kpoint,npw_k)

    modkplusg(:)=SQRT(0.5_dp/pi**2*kinpw(:))
    modkplusg(:)=MAX(modkplusg(:),tol10)
    matblk=Dtset%nloalg(4)
    if (Dtset%nloalg(1)>0) matblk=natom
    allocate(ph3d(2,npw_k,matblk),stat=istat)
    if (istat/=0) call memerr(sub_name,'ph3d ',2*npw_k*matblk,'dp')
    if (Dtset%nloalg(1)>0) then
     call ph1d3d(1,natom,kg_k,kpoint,matblk,natom,npw_k,n1,n2,n3,phkxred,ph1d,ph3d)
    end if
!   
!   NOTE These definitions are only valid if useylm==0 and not PAW
    if (Psps%usepaw==0) then
     allocate(vkb(npw_k+1,ntypat,mpsang),stat=istat)
     if (istat/=0) call memerr(sub_name,'vkb ',(npw_k+1)*ntypat*mpsang,'dp')
     allocate(vkbd(npw_k+1,ntypat,mpsang),stat=istat)
     if (istat/=0) call memerr(sub_name,'vkbd',(npw_k+1)*ntypat*mpsang,'dp')
     vkb(:,:,:)=zero ; vkbd(:,:,:)=zero
     do is=1,ntypat
      il0=0
      do ilmn=1,Psps%lmnmax
       il=1+Psps%indlmn(1,ilmn,is)
       in=Psps%indlmn(3,ilmn,is)
       if ((il/=il0).and.(in==1)) then
        il0=il
        if (ABS(Psps%ekb(ilmn,is))>1.0d-10) then
         if (il==1) then
          vkb (1:npw_k,is,il) = ffnl(:,1,ilmn,is)
          vkbd(1:npw_k,is,il) = ffnl(:,2,ilmn,is)*modkplusg(:)/two_pi
         else if (il==2) then
          vkb(1:npw_k,is,il)  = ffnl(:,1,ilmn,is)*modkplusg(:)
          do ig=1,npw_k
           vkbd(ig,is,il) = ((ffnl(ig,2,ilmn,is)*modkplusg(ig)*modkplusg(ig))+&
&           ffnl(ig,1,ilmn,is) )/two_pi
          end do
         else if (il==3) then
          vkb (1:npw_k,is,il) =  ffnl(:,1,ilmn,is)*modkplusg(:)**2
          vkbd(1:npw_k,is,il) = (ffnl(:,2,ilmn,is)*modkplusg(:)**3+&
&          2*ffnl(:,1,ilmn,is)*modkplusg(:) )/two_pi
         else if (il==4) then
          vkb (1:npw_k,is,il) =  ffnl(:,1,ilmn,is)*modkplusg(:)**3
          vkbd(1:npw_k,is,il) = (ffnl(:,2,ilmn,is)*modkplusg(:)**4+&
&          3*ffnl(:,1,ilmn,is)*modkplusg(:)**2 )/two_pi
         end if
         vkb (:,is,il) = SQRT(4*pi/ucvol*(2*il-1)*ABS(Psps%ekb(ilmn,is)))*vkb (:,is,il)
         vkbd(:,is,il) = SQRT(4*pi/ucvol*(2*il-1)*ABS(Psps%ekb(ilmn,is)))*vkbd(:,is,il)
        else
         vkb (:,is,il)=zero
         vkbd(:,is,il)=zero
        end if
       end if
      end do
     end do
    end if !PAW
    deallocate(modkplusg)

    if (diago) then
!    /** K-Compute <G|H|G_prim> matrix elements for all (G, G_prim)  **/
     write(msg,'(a)') ' Calculating <G|H|G''> elements'
     call wrtout(std_out,msg,'PERS')
!    BE CAREFULL : DOES NOT WORK WITH nspinor=2 !!!!
     allocate(pwave (2,npw_k*nspinor))
     allocate(subghg(2,npw_k*nspinor))
     allocate(gvnlg (2,npw_k*nspinor))
     if (nspinor==2) allocate(pwave_so(2,npw_k),subghg_so(2,npw_k))
     allocate(ghg(2,(ngdiago*nspinor*(ngdiago*nspinor+1))/2))
     allocate(eigval(npw_k*nspinor))
     allocate(eigvec(2,ngdiago*nspinor,nbandkssk(ikpt)))
!    Initialize plane-wave array:
     pwave(:,:)=zero
!    Initialize index for packed storage of <G|H|G''>:
     nghg=0
!    Loop over G'':
     do ispinorp=1,nspinor
      do igp=1,ngdiago
       pwave(1,igp)=1._dp
!      
!      Compute <G|Vlocal|G''>:
!      
       allocate(work(2,n4,n5,n6))
       cplex=1 ; option=2 ; tim_fourwf=15 ; weight=1._dp
       call fourwf(cplex,vlocal,pwave,subghg,work,gbound,gbound,istwf_k,kg_k,kg_k,&
&       mgfft,MPI_enreg,1,Dtset%ngfft,npw_k,npw_k,n4,n5,n6,option,Dtset%paral_kgb,tim_fourwf,weight,weight)
       if (nspinor==2) then
        pwave_so(:,1:npw_k)=pwave(:,npw_k+1:2*npw_k)
        call fourwf(cplex,vlocal,pwave_so,subghg_so,work,gbound,gbound,istwf_k,kg_k,&
&        kg_k,mgfft,MPI_enreg,1,Dtset%ngfft,npw_k,npw_k,n4,n5,n6,option,Dtset%paral_kgb,tim_fourwf,weight,weight)
        subghg(:,npw_k+1:2*npw_k)=subghg_so(:,1:npw_k)
       end if
       deallocate(work)
!      
!      Compute <G|Vnonlocal|G''>:
!      
       signs=2; choice=1 ; nnlout=1 ; idir=0 ; tim_nonlop=9 ; paw_opt=0 ; cpopt=-1

       allocate(kpg_dum(npw_k,nkpg)) !this is only used if useylm==1
       call nonlop(atindx1,choice,cpopt,Cprj_dum,dimekb1,dimekb2,dimffnl,dimffnl,ekb,&
&       enlout,ffnl,ffnl,gmet,gprimd,idir,psps%indlmn,istwf_k,kg_k,kg_k,kpg_dum,kpg_dum,kpoint,kpoint,&
&       dum,psps%lmnmax,matblk,mgfft,mpi_enreg,psps%mpsang,psps%mpssoang,natom,nattyp,dtset%ngfft,nkpg,nkpg,&
&       dtset%nloalg,nnlout,npw_k,npw_k,nspinor,ntypat,0,paw_opt,phkxred,phkxred,ph1d,ph3d,ph3d,psps%pspso,&
&       signs,nonlop_dum,nonlop_dum,tim_nonlop,ucvol,psps%useylm,pwave,gvnlg)
       deallocate(kpg_dum)

!      Assemble modified kinetic, local and nonlocal contributions to <G|H|G''>.
       do ispinor=1,nspinor
        do ig=1,igp
         igspinor=ig+npw_k*(ispinor-1)
         if (kinpw(ig)<huge(0.0_dp)*1.d-11) then
          subghg(1,igspinor)= kinpw(ig)*pwave(1,igspinor) + subghg(1,igspinor)+gvnlg(1,igspinor)
          subghg(2,igspinor)= kinpw(ig)*pwave(2,igspinor) + subghg(2,igspinor)+gvnlg(2,igspinor)
         else
          subghg(1,igspinor)=zero
          subghg(2,igspinor)=zero
         end if
        end do
!       
!       Store <G|H|G''> in packed storage:
        do ig=1,igp
         nghg=nghg+1
         ghg(:,nghg)=subghg(:,ig)
        end do

       end do ! loop over G''
       pwave(1,igp)=zero
      end do
     end do
     deallocate(pwave,subghg,gvnlg)
     if (nspinor==2) deallocate(pwave_so,subghg_so)
!    
!    === Diagonalize <G|H|G''> matrix ===
     call timab(236,1,tsec)

!    Initialize non-defined <G|H|G''> to zero:  OBSOLETE !
!    eigvec(:,npw_k+1,:)=0._dp
!    if (nspinor==2) eigvec(:,(npw_k+1)*nspinor,:)=0._dp

     if (nbandkss_k>=ngdiago*nspinor) then
!     * Complete diagonalization
      nbandkss_k=ngdiago*nspinor
      write(msg,'(2a,i3,3x,3a,i5)')ch10,&
&      ' Begin complete diago for ikpt=',ikpt,stag(isppol),ch10,&
&      ' - Size of mat.=',ngdiago*nspinor
      call wrtout(std_out,msg,'PERS')
      allocate(cwork(2,2*npw_k*nspinor),rwork(3*npw_k*nspinor))
#if defined T3E
      call chpev('v','u',ngdiago*nspinor,ghg,eigval,eigvec,ngdiago*nspinor,cwork,rwork,ier)
#else
      call zhpev('v','u',ngdiago*nspinor,ghg,eigval,eigvec,ngdiago*nspinor,cwork,rwork,ier)
#endif
      deallocate(rwork,cwork)
     else
!     * Partial diagonalization
      write(msg,'(2a,i3,3x,2a,i5,a,i5)')ch10,&
&      ' Begin partial diago for ikpt=',ikpt,stag(isppol),&
&      ' - Size of mat.=',ngdiago*nspinor,' - # bnds=',nbandkss_k
      call wrtout(std_out,msg,'PERS')

      allocate(cwork(2,2*npw_k*nspinor),rwork(7*npw_k*nspinor))
      allocate(iwork(  5*npw_k*nspinor),ifail(  npw_k*nspinor))
#if defined T3E
      call chpevx('v','i','u',ngdiago*nspinor,ghg,0.,0.,1,nbandkss_k,&
&      -1.e-8,negv,eigval,eigvec,ngdiago*nspinor,cwork,rwork,iwork,ifail,ier)
#else
      call zhpevx('v','i','u',ngdiago*nspinor,ghg,0._dp,0._dp,1,nbandkss_k,&
&      -1.d-8,negv,eigval,eigvec,ngdiago*nspinor,cwork,rwork,iwork,ifail,ier)
#endif
      deallocate(rwork,cwork,iwork,ifail)
     end if

     if (ier/=0) then
      write(msg,'(3(a,i3),a)')&
&      ' outkss: WARNING - at ikpt= ',ikpt,' spin= ',isppol,' - error in diago (ier=',ier,')'
      call wrtout(std_out,msg,'PERS') 
      call wrtout(ab_out,msg,'PERS')
     end if

     deallocate(ghg)
     call timab(236,2,tsec)
    end if ! diago

    deallocate(gbound,phkxred,ph3d,ffnl,kinpw)
    call timab(237,1,tsec)
   end if ! kpt+spin parallelism, proc_distrb(ikpt,1,isppol)==MPI_enreg%me
!  
!  ===================================
!  === Transfer data between procs ===
!  ===================================
   if (nprocs==1) then 
    if (Psps%usepaw==1.and.kssform==3) then 
!    Copy projectors for this k-point
     allocate(Cprjnk_k(natom,nband_k*nspinor))
     call cprj_alloc(Cprjnk_k,0,dimlmn)
     ibsp=0
     do iband=1,nband_k
      do ispinor=1,nspinor
       ibsp=ibsp+1
       do iatom=1,natom
        Cprjnk_k(iatom,ibsp)%cp(:,:)=Cprj(iatom,ibsp+ibg)%cp(:,:)
       end do
      end do
     end do
    end if
   else !parallel case
    bufsz=nbandksseff/bufnb;bufrt=nbandksseff-bufnb*bufsz
    nband_k=Dtset%nband(ikpt+(isppol-1)*nkpt)

    if (MPI_enreg%me==master.or.MPI_enreg%proc_distrb(ikpt,1,isppol)==MPI_enreg%me) then

!    Allocate arrays if not done yet.
     if (MPI_enreg%me==master.and.MPI_enreg%proc_distrb(ikpt,1,isppol)/=MPI_enreg%me) then
      if (Psps%usepaw==0) then
       allocate(vkb(npw_k+1,ntypat,mpsang),vkbd(npw_k+1,ntypat,mpsang))
      end if
      if (diago) then
       allocate(eigval(npw_k*nspinor),eigvec(2,ngdiago*nspinor,nbandkssk(ikpt)))
      end if
     end if

!    Transmit data.
     call xexch_mpi(trsl,npwkss,MPI_enreg%proc_distrb(ikpt,1,isppol),trsl,0,spaceComm,ierr)
     if (Psps%usepaw==0) then
      call xexch_mpi(vkb,(npw_k+1)*mpsang*ntypat,MPI_enreg%proc_distrb(ikpt,1,isppol),vkb,0,spaceComm,ierr)
      call xexch_mpi(vkbd,(npw_k+1)*mpsang*ntypat,MPI_enreg%proc_distrb(ikpt,1,isppol),vkbd,0,spaceComm,ierr)
     end if

     if (.not.diago) then
      ngdiago=npw_k
      allocate(eigvec(2,ngdiago*nspinor,nbandkssk(ikpt)))
      if (MPI_enreg%proc_distrb(ikpt,1,isppol)==MPI_enreg%me) then
       do ib=1,nbandksseff
        do ig=1,ngdiago
         eigvec(:,ig,ib)=cg(:,ig+(ib-1)*npw_k*nspinor+k_index)
        end do
       end do
      end if
!     
!     In case of PAW and kssform==3, retrieve matrix elements of the PAW projectors for this k-point
!     TODO add the mkmem==0 case
      if (Psps%usepaw==1.and.kssform==3) then 
       sender=MPI_enreg%proc_distrb(ikpt,1,isppol)
       receiver=master
       allocate(Cprjnk_k(natom,nband_k*nspinor))
       call cprj_alloc(Cprjnk_k,0,dimlmn)
       ibsp=0
       if (MPI_enreg%me==sender) then
        do iband=1,nband_k
         do ispinor=1,nspinor
          ibsp=ibsp+1
!         if (ibsp+ibg>nspinor*mband*mkmem*nsppol) then 
!         write(msg,*)'BOOM' ; call wrtout(std_out,msg,'PERS')
!         call leave_new('COLL')
!         end if
          do iatom=1,natom
           Cprjnk_k(iatom,ibsp)%cp(:,:)=Cprj(iatom,ibsp+ibg)%cp(:,:)
          end do
         end do
        end do
       end if
       if (sender/=receiver) then
        n2dim=nband_k*nspinor
        call cprj_exch(natom,n2dim,dimlmn,0,Cprjnk_k,Cprjnk_k,sender,receiver,spaceComm,ierr)
       end if
      end if

     else
      call xexch_mpi(eigval,nbandksseff,MPI_enreg%proc_distrb(ikpt,1,isppol),eigval,0,spaceComm,ierr)
     end if
     if (bufsz>0) then
      do i=0,bufnb-1
       call xexch_mpi(eigvec(:,:,i*bufsz+1:(i+1)*bufsz),2*ngdiago*nspinor*bufsz,MPI_enreg%proc_distrb(ikpt,1,isppol),&
&       eigvec(:,:,i*bufsz+1:(i+1)*bufsz),0,spaceComm,ierr)
      end do
     end if
     if (bufrt>0) then
      call xexch_mpi(eigvec(:,:,bufnb*bufsz+1:bufnb*bufsz+bufrt),2*ngdiago*nspinor*bufrt,MPI_enreg%proc_distrb(ikpt,1,isppol),&
&      eigvec(:,:,bufnb*bufsz+1:bufnb*bufsz+bufrt),0,spaceComm,ierr)
     end if
     call timab(48,2,tsec)
    end if
   end if !nprocs > 1

   if (MPI_enreg%me==master) then
!   === Prepare data for writing on disk ===
    allocate(en(nbandksseff),wfg(2,npwkss*nspinor,nbandksseff))
    en(:)=zero ; wfg(:,:,:)=zero

    if (.not.diago) then
     en(1:nbandksseff)=eigen(1+bdtot_index:nbandksseff+bdtot_index)
     if (mkmem==0) then
      tim_rwwf=0
      call rwwf(cg_disk,eig_dum,0,0,0,ikpt,isppol,kg_dum,mband,mcg_disk,MPI_enreg,&
&      nband_k,nband_k,npw_k,nspinor,occ_dum,-2,0,tim_rwwf,Wffnow)
      do ib=1,nbandksseff
       do ispinor=1,nspinor
        spinor_shift1=(ispinor-1)*npwkss
        spinor_shift2=(ispinor-1)*npw_k
        do ig=1,npwkss
         if (trsl(ig)>npw_k) CYCLE
         wfg(:,ig+spinor_shift1,ib)=cg_disk(:,trsl(ig)+spinor_shift2+(ib-1)*npw_k*nspinor)
        end do
       end do
      end do
     else 
      do ib=1,nbandksseff
       do ispinor=1,nspinor
        spinor_shift1=(ispinor-1)*npwkss
        spinor_shift2=(ispinor-1)*npw_k
        do ig=1,npwkss
         if (trsl(ig)>npw_k) CYCLE
!        Here we have to retrieve the correct components
         if (nprocs>1) then
          wfg(:,ig+spinor_shift1,ib)=eigvec(:,trsl(ig)+spinor_shift2,ib)
         else 
          wfg(:,ig+spinor_shift1,ib)=cg(:,trsl(ig)+spinor_shift2+(ib-1)*npw_k*nspinor+k_index)
         end if
        end do
       end do
      end do
     end if
    else ! Full diagonalization.
     en(1:nbandksseff)=eigval(1:nbandksseff)
     do ib=1,nbandksseff
      do ig=1,npwkss
       if (trsl(ig)>npw_k .or. trsl(ig)>ngdiago) CYCLE
       wfg(:,ig,ib)=eigvec(:,trsl(ig),ib)
      end do
     end do
!    * Check diagonalized eigenvalues with respect to conjugate gradient ones
     ntemp=MIN(nbandksseff,nband_k)
     if (ANY(ABS(en(1:ntemp)-eigen(1+bdtot_index:ntemp+bdtot_index))>1.d-3)) then
      write(msg,'(6a)')ch10,&
&      ' outkss: WARNING -',ch10,&
&      '  The diagonalized eigenvalues differ by more than 10^-3 Hartree',ch10,&
&      '  with respect to the conjugated gradient values.'
      call wrtout(std_out,msg,'COLL')
     end if
    end if
!   
!   === Write out results ===
    if (Dtset%enunit==1) then
     cfact=Ha_eV ; frmt1='(i4,4x,9(1x,f7.2))' ; frmt2='(8x,9(1x,f7.2))'
     write(msg,'(a,i3,3x,a)')' Eigenvalues in eV for ikpt= ',ikpt,stag(isppol)
    else
     cfact=one   ; frmt1='(i4,4x,9(1x,f7.4))' ; frmt2='(8x,9(1x,f7.4))'
     write(msg,'(a,i3,3x,a)')' Eigenvalues in Hartrees for ikpt= ',ikpt,stag(isppol)
    end if
    call wrtout(std_out,msg,'COLL')
    write(msg,frmt1)ikpt,(en(ib)*cfact,ib=1,MIN(9,nbandksseff))
    call wrtout(std_out,msg,'COLL') 
    call wrtout(ab_out,msg,'COLL')
    if (nbandksseff>9) then
     do j=10,nbandksseff,9
      write(msg,frmt2) (en(ib)*cfact,ib=j,MIN(j+8,nbandksseff))
      call wrtout(std_out,msg,'COLL') 
      call wrtout(ab_out,msg,'COLL')
     end do
    end if
!   
!   * Test on the normalization of wavefunctions.
    ibsp=0
    do ib=1,nbandksseff
     norm=zero
     do ispinor=1,nspinor
      ibsp=ibsp+1
      spinor_shift1=(ispinor-1)*npwkss
      wfg1 => wfg(:,1+spinor_shift1:npwkss+spinor_shift1,ib)
      ovlp=overlap_real(wfg1,wfg1,Psps%usepaw,Cprjnk_k(:,ibsp),Cprjnk_k(:,ibsp),Dtset%typat,Pawtab)
      norm=norm+ABS(ovlp(1))
!     norm=norm+SQRT(ovlp(1)**2+ovlp(2)**2)
     end do
     if (norm<einf) einf=norm
     if (norm>esup) esup=norm
    end do
!   * Test on the orthogonalization of wavefunctions.
    do ib=1,nbandksseff
     pad1=(ib-1)*nspinor
     do ibp=ib+1,nbandksseff
      pad2=(ibp-1)*nspinor
      ovlp(:)=zero
      do ispinor=1,nspinor
       ibsp1=pad1+ispinor
       ibsp2=pad2+ispinor
       spinor_shift1=(ispinor-1)*npwkss
       wfg1 => wfg(:,1+spinor_shift1:npwkss+spinor_shift1,ib )
       wfg2 => wfg(:,1+spinor_shift1:npwkss+spinor_shift1,ibp)
       ovlp= ovlp + overlap_real(wfg1,wfg2,Psps%usepaw,Cprjnk_k(:,ibsp1),Cprjnk_k(:,ibsp2),Dtset%typat,Pawtab)
      end do
      norm=SQRT(ovlp(1)**2+ovlp(2)**2)
      if (norm<cinf) cinf=norm
      if (norm>csup) csup=norm
     end do
    end do
!   
!   === For NC pseudo, write pseudopotential information on disk ===
    if (Psps%usepaw==0) then
     if (Dtset%accesswff==0) then
      do is=1,ntypat
       do il=1,mpsang
        write(untkss) (vkb (trsl(ig),is,il),ig=1,npwkss)
        write(untkss) (vkbd(trsl(ig),is,il),ig=1,npwkss)
       end do
      end do
#if defined HAVE_ETSF_IO
     else if (Dtset%accesswff==3) then
      allocate(vkb_tgt (npwkss,mpsang,ntypat))
      allocate(vkbd_tgt(npwkss,mpsang,ntypat))
      do is=1,ntypat
       do il=1,mpsang
        do ig=1,npwkss
         vkb_tgt (ig,il,is)=vkb (trsl(ig),is,il)
         vkbd_tgt(ig,il,is)=vkbd(trsl(ig),is,il)
        end do
       end do
      end do
      gwdata%kb_formfactors%data3D => vkb_tgt
      gwdata%kb_coeff__kpoint_access = ikpt
      gwdata%kb_formfactor_derivative%data3D => vkbd_tgt
      gwdata%kb_coeff_der__kpoint_access = ikpt
      call etsf_io_gwdata_put(ncid, gwdata, lstat, error)
      if (.not.lstat) then
       call etsf_io_low_error_to_str(errmess,error)
       write(msg,'(4a)')ch10,&
&       ' outkss: ERROR -',ch10,errmess(1:min(475,len(errmess)))
       call wrtout(std_out,msg,'COLL') 
       call leave_new('COLL')
      end if
      deallocate(vkb_tgt,vkbd_tgt)
!     FIXME here we have dangling pointers since %kb_formfactors and 
!     kb_formfactor_derivative% are not nullified
#endif
     end if
    end if ! usepaw==0
!   
!   === Write eigenvalues on disk ===
    write(msg,'(a,i3,3x,a)')' Writing out eigenvalues/vectors for ikpt=',ikpt,stag(isppol)
    call wrtout(std_out,msg,'COLL')
    if (Dtset%accesswff==0) write(untkss) (en(ib),ib=1,nbandksseff)
!   
!   === Write occupation numbers on disk ===
    allocate(occ_k(MAX(nband_k,nbandksseff)))
    occ_k(1:nband_k)=occ(1+bdtot_index:nband_k+bdtot_index)
    if (nband_k < nbandksseff) occ_k(nband_k+1:nbandksseff)=zero

    write(msg,'(a,i3,3x,a)')' Occupation numbers for ikpt=',ikpt,stag(isppol)
    call wrtout(std_out,msg,'COLL')
    write(msg,'(i4,4x,9(1x,f7.4))')ikpt,(occ_k(ib),ib=1,MIN(9,nbandksseff))
    call wrtout(std_out,msg,'COLL')
    if (nbandksseff>9) then
     do j=10,nbandksseff,9
      write(msg,'(8x,9(1x,f7.4))') (occ_k(ib),ib=j,MIN(j+8,nbandksseff))
      call wrtout(std_out,msg,'COLL')
     end do
    end if
!   
!   === Write wavefunctions and PAW matrix elements on disk ===
    if (Dtset%accesswff==0) then
     ibsp=0
     do ib=1,nbandksseff
      write(untkss) (wfg(:,ig,ib),ig=1,npwkss*nspinor)
!     write(77,'(2f8.4)')(CMPLX(wfg(1,1:374*nspinor,ib),wfg(2,1:374*nspinor,ib)))
      if (Psps%usepaw==1.and.kssform==3) then 
!      FIXME here I should used dimlmn and check ordering used in ctocprj
       do ispinor=1,nspinor
        ibsp=ibsp+1
        do ia=1,natom
         ii=Cprjnk_k(ia,ibsp)%nlmn
         write(untkss) (Cprjnk_k(ia,ibsp)%cp(:,1:ii))
!        write(77,'(f8.4)')(Cprjnk_k(ia,ibsp)%cp(:,1:ii))
        end do
       end do
      end if
     end do
    end if
!   
!   When ETSF, use the rwwf routine (should always be done like that)
    if (Dtset%accesswff==3) then
#if defined HAVE_ETSF_IO
     if (Psps%usepaw==1.and.kssform==3) call not_implemented() 
     wff%master=0
     wff%me=MPI_enreg%me
     wff%unwff=ncid
     wff%accesswff=3
!    MG Tue Oct 23 occ_k is passed to writewf instead of occ to treat correctly metals
     call writewf(RESHAPE(wfg(:, 1:npwkss, :),(/2,nbandksseff*npwkss /)),en,0,Hdr%headform,&
&     0,ikpt,isppol,gbig(:,1:npwkss),nbandksseff,nbandksseff*npwkss,MPI_enreg,&
&     nbandksseff,nbandksseff,npwkss,nspinor,occ_k(1:nbandksseff),2,1,wff)
#endif
    end if
    deallocate(occ_k)
    deallocate(en,wfg)
   end if ! MPI_enreg%me==master

   if (MPI_enreg%me==master.or.MPI_enreg%proc_distrb(ikpt,1,isppol)==MPI_enreg%me) then

    if (allocated(eigval)) deallocate(eigval)
    if (allocated(eigvec)) deallocate(eigvec)
    if (allocated(kg_k  )) deallocate(kg_k  )
    if (Psps%usepaw==0) then
     deallocate(vkb,vkbd)
    end if
    if (Psps%usepaw==1) then 
     call cprj_free(Cprjnk_k)
     deallocate(Cprjnk_k)
    end if
   end if
!  
!  === End loop over k-points ===
   deallocate(trsl,ylm_k)
!  if (MPI_enreg%paral_compil_kpt==1) then !cannot be used in seq run!
   if (MINVAL(ABS(MPI_enreg%proc_distrb(ikpt,:,isppol)-MPI_enreg%me))==0) then 
    k_index=k_index+npw_k*nband_k*nspinor
    ibg=ibg+nspinor*nband_k
   end if
!  end if
!  ibg=ibg+nspinor*nband_k
   bdtot_index=bdtot_index+nband_k
   call leave_test(MPI_enreg)
   call timab(237,2,tsec)

  end do ! k-point
 end do ! spin

 write(msg,'(3a,f9.6,2a,f9.6,4a,f9.6,2a,f9.6,a)')&
& ' Test on the normalization of the wavefunctions',ch10,&
& '  min sum_G |a(n,k,G)| = ',einf,ch10,&
& '  max sum_G |a(n,k,G)| = ',esup,ch10,&
& ' Test on the orthogonalization of the wavefunctions',ch10,&
& '  min sum_G a(n,k,G)* a(n'',k,G) = ',cinf,ch10,&
& '  max sum_G a(n,k,G)* a(n'',k,G) = ',csup,ch10
 call wrtout(std_out,msg,'COLL') 
 call wrtout(ab_out,msg,'COLL')

 deallocate(ekb)
 deallocate(vlocal,gbig)
 deallocate(symrel2,tnons2,symrec2t)
 if (Psps%usepaw==1) deallocate(dimlmn)
 if ((.not.diago).and.(mkmem==0)) deallocate(eig_dum,kg_dum,occ_dum,cg_disk)
!
!=== Close files ===
 call leave_test(MPI_enreg)
 if (MPI_enreg%me==master) then
  if (Dtset%accesswff==0) then
   close(unit=untkss)
#if defined HAVE_ETSF_IO
  else if (Dtset%accesswff==3) then
   call dtsetfree(dtset_cpy)
   call etsf_io_low_close(ncid, lstat, error)
   if (.not.lstat) then
    call etsf_io_low_error_to_str(errmess, error)
    write(msg,'(4a)')ch10,&
&    ' outkss: ERROR -',ch10,errmess(1:min(475, len(errmess)))
    call wrtout(std_out,msg,'COLL') 
    call leave_new('COLL')
   end if
#endif
  end if
 end if

 if (lhack) deallocate(MPI_enreg%proc_distrb)

#if defined DEBUG_MODE
 write(msg,'(a)')' outkss: exit'
 call wrtout(std_out,msg,'COLL') 
 call flush_unit(std_out)
#endif

 CONTAINS  !===========================================================

!!***

!!****f* outkss/not_implemented
!! NAME
!! not_implemented
!!
!! FUNCTION
!!
!! INPUTS
!!
!! OUTPUT
!!
!! PARENTS
!!      outkss
!!
!! CHILDREN
!!      leave_new,wrtout
!!
!! SOURCE
 subroutine not_implemented()


!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
 use interfaces_01manage_mpi
!End of the abilint section

 implicit none

 write(msg,'(a)')' Calculation not yet implemented'
 call wrtout(std_out, msg, 'COLL') 
 call leave_new('COLL')

 end subroutine not_implemented

end subroutine outkss
!!***
