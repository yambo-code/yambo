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
!!
!! * crystal_structure : information on the unit cell, symmetries
!! * dataset_type : the "dataset" for the main abinit code
!! * datafil_type : the data (units,filenames) related to files
!! * dens_sym_operator_type : information for symmetrizing the density
!! * efield_type : First-principles calculations in a finite electric field
!! * energies_type : simple datastructure to store parts of total energy.
!! * epsilonm1_parameters : for GW part of ABINIT, parameters for epsilon-1
!! * epsilonm1_results : for GW part of ABINIT, results of screening
!! * gipaw_type : structures related to magnetic field response
!! * gs_hamiltonian_type : datastructure describing an Hamiltonian
!! * bz_mesh : information on the sampling of the BZ used in the GW part
!! * MPI_type : the data related to MPI parallelization
!! * nuclear_type : data (esp. related to different nspden) at each nuclear site
!! * pawang_type : for PAW, ANGular mesh discretization and related data
!! * pawfgr_type : for PAW, Fine rectangular GRid parameters and related data
!! * pawfgrtab_type : for PAW, various arrays giving data related to fine grid for a given atom
!! * pawrad_type : for PAW, RADial mesh discretization and related data
!! * pawtab_type : for PAW, TABulated data initialized at start
!! * paw_an_type : for PAW, various arrays given on ANgular mesh or ANgular moments
!! * paw_ij_type : for PAW, various arrays given on (i,j) (partial waves) channels
!! * pawrhoij_type : for PAW, rhoij quantities and related data
!! * pseudopotential_type : for norm-conserving pseudopotential, all the
!!   information
!! * pspheader_paw_type : for PAW, the header of the atomic file
!! * pspheader_type : for norm-conserving pseudopotentials, the header of
!!   the file
!! * rdm_parameters : contains the parameters used during a RDM calculation
!! * results_gs_type : contains the results of a GS calculation
!! * results_out_type : contains a subset of the results, for internal
!!   tests and timing analysis
!! * scf_history : contains an history of previous SCF cycles (densities...)
!! * sigma_parameters : for GW part of ABINIT, parameters for sigma
!! * sigma_results : for GW part of ABINIT, results of sigma
!! * vecteur_type : 
!! * WannierData_type : Object used to store and handle Wannier90 results inside Abinit
!! * wffile_type : a handler for dealing with the IO of a wavefunction file
!! * wvl_internalVars_type : all internal input variables used by wavelets.
!!
!! COPYRIGHT
!! Copyright (C) 2001-2009 ABINIT group (XG)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~abinit/COPYING
!! or http://www.gnu.org/copyleft/gpl.txt .
!!
!! NOTES
!! PAW structured datatypes to be described ...
!! * pawang_type : ANGular mesh discretization and related data
!! * pawfgr_type : Fine rectangular GRid parameters and related data
!! * pawfgrtab_type : various arrays giving data related to fine grid for a given atom
!! * pawrad_type :  RADial mesh discretization and related data
!! * pawtab_type : TABulated data initialized at start
!! * paw_an_type : various arrays given on ANgular mesh or
!! * paw_ij_type : various arrays given on (i,j) (partial waves) channels
!! * pawrhoij_type : for PAW, rhoij quantities and related data
!! * pspheader_paw_type: the header of the atomic file
!!
!! TODO
!!
!! SOURCE

module defs_datatypes

 use defs_basis

 implicit none

  integer, pointer ::  istwfk(:)     ! istwfk(nkpt)
  integer, pointer ::  kberry(:,:)   ! kberry(3,nberry)
  integer, pointer ::  lexexch(:)    ! lexexch(ntypat)
  integer, pointer ::  lpawu(:)      ! lpawu(ntypat)
  integer, pointer ::  ltypeorb(:)   ! ltypeorb(norb)
  integer, pointer ::  nband(:)      ! nband(nkpt*nsppol)
  integer, pointer ::  numorb(:)     ! numorb(ncenter)
  integer, pointer ::  so_psp(:)     ! so_psp(npsp)
  integer, pointer ::  symafm(:)     ! symafm(nsym)
  integer, pointer ::  symrel(:,:,:) ! symrel(3,3,nsym)
  integer, pointer ::  typat(:)      ! typat(natom)
  integer, pointer ::  w90lplot(:)   ! w90lplot(w90nplot)

! Real
  real(dp) :: alpha,bmass,boxcutmin,bxctmindg,charge,cpus,dedlnn,diecut,diegap,dielam,&
&  dielng,diemac,diemix,diemixmag,dilatmx,dosdeltae,dtion,&
&  ecut,ecuteps,ecutsigx,ecutsm,ecutwfn,effmass,&
&  eshift,exchmix,fband,fixmom,freqremax,freqspmax,freqsusin,freqsuslo,friction,gwencomp,&
&  kptnrm,kptrlen,mdftemp,mditemp,mdwall,nelect,noseinert,&
&  omegasimax,omegasrdmax,pawecutdg,pawovlp,ppmfrq,qptnrm,recrcut,recefermi,rectolden,rhoqpmix,rcut,&
&  sciss,slabwsrad,slabzbeg,slabzend,soenergy,spbroad,spnorbscl,stmbias,strfact,strprecon,&
&  td_maxene,tfnewton,tl_radius,toldfe,toldff,tolrff,&
&  tolmxf,tolvrs,tolwfr,tphysel,tsmear,userra,userrb,userrc,userrd,&
&  userre,vacwidth,vis,vmass,wvl_hgrid,wvl_crmult,wvl_frmult,wvl_cpmult,wvl_fpmult,&
&  zcut
! Types
  type(wvl_internalVars_type) :: wvl
! Real arrays
  real(dp) :: acell_orig(3),angdeg_orig(3),boxcenter(3),&
&  efield(3),genafm(3),qpt(3),qptn(3),rprim_orig(3,3),&
&  rprimd_orig(3,3),strtarget(6),vcutgeo(3),vprtrb(2)
! Real pointers
  real(dp), pointer :: amu(:)            ! amu(ntypat)
  real(dp), pointer :: atvshift(:,:,:)   ! atvshift(16,nsppol,natom)
  real(dp), pointer :: corecs(:)         ! corecs(ntypat)
  real(dp), pointer :: densty(:,:)       ! densty(ntypat,4)
  real(dp), pointer :: dmatpawu(:,:,:,:) ! dmatpawu(2*lpawu+1,2*lpawu+1,nsppol*nspinor,natpu) where natpu=number of atoms with lpawu/=1
  real(dp), pointer :: jpawu(:)       ! jpawu(ntypat)
  real(dp), pointer :: kpt(:,:)       ! kpt(3,nkpt)
  real(dp), pointer :: kptgw(:,:)     ! kptgw(3,nkptgw)
  real(dp), pointer :: kptns(:,:)     ! kptns(3,nkpt)
  real(dp), pointer :: mixalch(:,:)   ! mixalch(npspalch,ntypalch)
  real(dp), pointer :: occ_orig(:)    ! occ_orig(mband*nkpt*nsppol)
  real(dp), pointer :: ptcharge(:)    ! ptcharge(ntypat)
  real(dp), pointer :: qmass(:)       ! qmass(nnos)
  real(dp), pointer :: qptdm(:,:)     ! qptdm(3,nqptdm)
  real(dp), pointer :: quadmom(:)     ! quadmom(ntypat)
  real(dp), pointer :: ratsph(:)      ! ratsph(ntypat)
  real(dp), pointer :: rcoord(:,:)    ! rcoord(3,ncenter)
  real(dp), pointer :: rtheta(:,:)    ! rtheta(3,norb)
  real(dp), pointer :: shiftk(:,:)    ! shiftk(3,nshiftk)
  real(dp), pointer :: spinat(:,:)    ! spinat(3,natom)
  real(dp), pointer :: tnons(:,:)     ! tnons(3,nsym)
  real(dp), pointer :: upawu(:)       ! upawu(ntypat)
  real(dp), pointer :: vel_orig(:,:)  ! vel_orig(3,natom)
  real(dp), pointer :: wtatcon(:,:,:) ! wtatcon(3,natom,nconeq)
  real(dp), pointer :: wtk(:)         ! wtk(nkpt)
  real(dp), pointer :: xred_orig(:,:) ! xred_orig(3,natom)
  real(dp), pointer :: ziontypat(:)   ! ziontypat(ntypat)
  real(dp), pointer :: znucl(:)       ! znucl(npsp)
 end type dataset_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/dens_sym_operator_type
!! NAME
!! dens_sym_operator_type
!!
!! FUNCTION
!! Information for symmetrizing the density
!! This datastructure contains the information needed for symmetrizing
!! the density : number of symmetry operations, location of related
!! points in reciprocal space, phases, etc ...
!!
!! TODO 
!!  MG It seems this structure is never used, can we remove it?
!!  One should use more General object describing the crystal
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type dens_sym_operator_type

! Integer scalar

  integer :: flagdensymop
   ! if 1, the density symmetrization operator is to be applied,
   ! if 0, do not apply it

  integer :: nfft
   ! number of FFT grid points

  integer :: nspdensymop
   ! number of spin-density components of irrzon and phnons

  integer :: nsym
   ! number of symmetries

! Integer arrays

! integer, pointer :: irrzon(:,:,:)
   ! irrzon(nfft*flagdensymop,2,nspdensymop)
   ! contains the locations of related
   ! grid points and the repetition number for each symmetry class.

! integer, pointer :: symafm(:)
   ! symafm(nsym)
   ! anti-ferromagnetic character of the symmetry operations (+ if the
   ! magnetisation is not conserved, -1 if the magnetisation is reversed)

! Real (real(dp)) arrays

! real(dp), pointer :: phnons(:,:,:)
   ! phnons(2,nfft*flagdensymop,nspdensymop)
   ! phases associated with nonsymmorphic translations

 end type dens_sym_operator_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/efield_type
!! NAME
!! efield_type
!!
!! FUNCTION
!! First-principles calculations in a finite electric field
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type efield_type

! Integer variables
  integer :: fmkmem              ! number of k-points in the FBZ per cpu
  integer :: fmkmem_max          ! max of fmkmem
  integer :: fnkpt               ! number of k-points in the FBZ
  integer :: maxnstr             ! max number of strings along idir=1,2,3
  integer :: maxnkstr            ! max number of k-points per string
  integer :: mkmem_max           ! max of mkmem
  integer :: nband_occ           ! number of occupied bands
                                 ! this number must be the same for every k

! Integer arrays
  integer :: nstr(3)             ! nstr(idir) = number of strings along idir
  integer :: nkstr(3)            ! nkstr(idir) = number of k-points per string

! Real(dp) scalars
  real(dp) :: sdeg               ! spin degeneracy: sdeg = 2 if nsppol = 1
                                 !                         1 if nsppol = 2

! Real(dp) arrays
  real(dp) :: dkvecs(3,3)        ! dkvec(:,idir) = vector between a k-poinit
                                 ! and its nearest neighbour along idir
  real(dp) :: efield_dot(3)      ! reciprocal lattice coordinates of the
                                 ! electric field

! Integer pointers
  integer, pointer :: cgindex(:,:)    ! cgindex(nkpt,nsppol)
                                      ! for each k-point, stores the location
                                      ! of the WF in the cg array
  integer, pointer :: cgqindex(:,:,:) ! cgqindex(3,6,nkpt*nsppol)
                                      ! for each k-point, stores the location
                                      ! of the WF in the cgq and pwnsfacq
                                      ! arrays
                                      ! (see vtorho.f and initberry.f)
  integer, pointer :: ikpt_dk(:,:,:)  ! ikpt_dk(nkpt,2,3)
                                      ! ikpt_dp(ikpt,ii,idir) = index of the
                                      ! k-point at k+dk (ii=1) and k-dk (ii=2)
  integer, pointer :: idxkstr(:,:,:)  ! idxkstr(maxnkstr,maxnstr,3)
                                      ! idxkstr(ikstr,istr,idir) index (ikpt) of
                                      ! k-point ikstr on string istr along idir
  integer, pointer :: indkk_f2ibz(:,:)   ! indkk_f2ibz(1:dtefield%fnkpt,1:6)
                                         ! information needed to fold a
                                         ! k-point in the FBZ into the IBZ;
                                         ! the second index (1:6)
                                         ! is as described in listkk
  integer, pointer :: i2fbz(:)           ! i2fbz(1:nkpt) gives index of IBZ
                                         ! k-points in the FBZ k-point list
  integer, pointer :: nneigh(:)          ! nneigh(nkpt)
                                         ! for each k-point, nneigh stores
                                         ! the number of its nearest neighbours
                                         ! that are not related by symmetry
  integer, pointer :: kgindex(:)      ! kgind(nkpt)
                                      ! kgind(ikpt) = ikg
  integer, pointer :: fkgindex(:)     ! same as kgindex, but defined
                                      ! for the FBZ and intended to use
                                      ! with pwindf
  integer, pointer :: sflag(:,:,:,:)  ! sflag(nband_occ,nkpt*nsppol,2,3)
                                      ! sflag = 0 : compute the whole row of
                                      !             smat
                                      ! sflag = 1 : the row is up to date

! Real(dp) pointers
  real(dp), pointer :: fkptns(:,:)       ! fkptns(3,1:dtefield%fnkpt)
                                         ! k-points in FBZ

  real(dp), pointer :: smat(:,:,:,:,:,:)
! smat(2,nband_occ,nband_occ,nkpt*nsppol,2,3)
! Overlap matrix for every k-point. In an electric field calculation,
! smat is updated at every iteration.


 end type efield_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/energies_type
!! NAME
!! energies_type
!!
!! FUNCTION
!! Simple datastructure to gather all part of total energy. Not all
!! attributes may have a value, depending on the scheme used to
!! compute the total energy and several options read from dtset.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type energies_type
!!  local psp energy (hartree)
  real(dp) :: e_localpsp
!!  Sum of the eigenvalues - Band energy (Hartree)
!!  (valid for double-counting scheme dtset%optene == 1)
  real(dp) :: e_eigenvalues
!!  Ewald energy (hartree), store also the ion/ion energy for free
!!  boundary conditions.
  real(dp) :: e_ewald
!!  Hartree part of total energy (hartree units)
  real(dp) :: e_hartree
!!  psp core-core energy
  real(dp) :: e_corepsp
!!  kinetic energy part of total energy.
!!  (valid for direct scheme, dtset%optene == 0)
  real(dp) :: e_kinetic
!!  nonlocal pseudopotential part of total energy.
  real(dp) :: e_nonlocalpsp
!!  entropy energy due to the occupation number smearing (if metal)
!!  Value is multiplied by dtset%tsmear, see %entropy for the entropy alone.
!!  (valid for metals, dtset%occopt>=3 .and. dtset%occopt<=7)
  real(dp) :: e_entropy
  real(dp) :: entropy
!!  correction energy for basis set error (Francis-Payne)
  real(dp) :: e_pulay
!!  exchange-correlation energy (hartree)
  real(dp) :: e_xc
!!  potential exchange-correlation energy (hartree)
  real(dp) :: e_vxc
!!  enxcdc=exchange-correlation double-counting energy (hartree)
  real(dp) :: e_xcdc
!!  PAW spherical part energy
  real(dp) :: e_paw
!!  PAW spherical part double-counting energy
  real(dp) :: e_pawdc
!!  Electric enthalpy, by adding both ionic and electronic contributions
  real(dp) :: e_elecfield
!!  Fermie energy
  real(dp) :: e_fermie
!!  h0=e_kinetic+e_localpsp+e_nonlocalpsp
  real(dp) :: h0
 end type energies_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/epsilonm1_parameters
!! NAME
!! epsilonm1_parameters
!!
!! FUNCTION
!! For the GW part of ABINIT, the  epsilonm1_parameters structured datatype
!! gather different parameters used to calculate the inverse dielectric matrices.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type epsilonm1_parameters

!scalars

  integer :: awtr                   ! If 1 the Adler-Wiser expression for Chi_0 is evaluated
                                    !  taking advantage of time-reversal symmetry
  integer :: gwcalctyp              ! Calculation type (see input variable)
  integer :: inclvkb                ! Integer flag related to the evaluation of the commutator for q-->0
  integer :: spmeth                 ! Method used to approximate the delta function in the expression for Im Chi_0
  integer :: nI                     ! Number of components (rows) in the chi0 matrix.
  integer :: nJ                     ! Number of components (columns) in the chi0 matrix.
  integer :: npwvec                 ! Max between npwe and npwwfn, used to pass the dimension of arrays e.g gvec
  integer :: npwwfn                 ! Number of planewaves for wavefunctions
  integer :: npwe                   ! Number of planewaves for $\tilde \epsilon$
  integer :: npwepG0                ! Number of planewaves in the enlarged sphere G-G0, to account for umklapp G0 vectors
  integer :: nbnds                  ! Number of bands used to evaluate $\tilde \epsilon$
  integer :: nkibz                  ! Number of k-points in the IBZ
  integer :: nsppol                 ! 1 for spin unpolarized, 2 for collinear spin polarized
  integer :: nqcalc                 ! Number of q-points that are calculated (subset of qibz)
  integer :: nqibz                  ! Number of q-points in the IBZ
  integer :: nqlwl                  ! Number of directions to analyze the non analytical behavior for q-->0
  integer :: nomega                 ! Number of frequencies where evaluate $\tilde \epsilon (\omega)$
  integer :: nomegaer,nomegaei      ! Number of real and imaginary frequencies, respectively
  integer :: nomegasf               ! Number of frequencies used for the spectral function
  integer :: symchi                 ! 0 ==> do not use symmetries to reduce the k-points summed over in chi0
                                    ! 1 ==> take advantage of point group symmetries as well as time-reversal

  real(dp) :: omegaermax            ! Maximum real frequency used in the contour deformation method
  real(dp) :: soenergy              ! Scissor energy used in chi0
  real(dp) :: spsmear               ! Smearing of the delta in case of spmeth==2
  real(dp) :: zcut                  ! Small imaginary shift to avoid poles in chi0

  logical :: analytic_continuation  ! if true calculate chi0 only along the imaginary axis
  logical :: contour_deformation    ! if true calculated chi0 both along the real and the imaginary axis
  logical :: plasmon_pole_model     ! if true a plasmonpole model is used (only 1 or 2 frequencies are calculated)

!arrays
  integer :: mG0(3)                 
  ! For each reduced direction gives the max G0 component to account for umklapp processes

  real(dp),pointer :: qcalc(:,:)    
  ! qcalc(3,nqcalc) 
  ! q-points that are explicitely calculated (subset of qibz).

  real(dp),pointer :: qibz(:,:)    
  ! qibz(3,nqibz)
  ! q-points in the IBZ.

  real(dp),pointer :: qlwl(:,:)     
  ! qlwl(3,nqlwl)
  ! q-points used for the long-wavelength limit.

  real(dp),pointer :: omegasf(:)    
  ! omegasf(nomegasf)
  ! real frequencies used to calculate the imaginary part of chi0.

  complex(dpc),pointer :: omega(:)
  ! omega(nomegasf)
  ! real and imaginary frequencies in chi0,epsilon and epsilonm1.

 end type
!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/ wavefunctions_information
!! NAME
!! wavefunctions_information
!!
!! FUNCTION
!! For the GW part of ABINIT, the wavefunctions_informations structured datatype
!! gather the information concerning the storage of the wavefunctions
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type wavefunctions_information

  integer :: gwmem                             ! Storage strategy: =0 do not store, =1 store everything
  integer :: my_minb,my_maxb                   ! min and Max band index stored and treated by this processor
  integer :: nfft                              ! Number of FFT points treated by this processor
  integer :: nfftot                            ! Total number of points in the FFT grid
  integer :: nk                                ! Number of irreducible k-points
  integer :: npwwfn                            ! Number of G vectors for wavefunctions
  integer :: nspden                            ! Number of independent spin-density components
  integer :: nspinor                           ! Number of spinorial components
  integer :: nsppol                            ! Number of independent spin polarizations
  integer :: paral_kgb                         ! Option for kgb parallelism

!arrays
  integer :: ngfft(18)                         
  ! All needed information about 3D FFT, 
  ! see ~abinit/doc/input_variables/vargs.htm#ngfft

  integer,pointer :: igfft0(:)                 
  ! igfft0(npwwfn)
  ! Index of G vectors in the FFT array

  logical,pointer :: is_already_stored(:,:,:)  
  ! is_already_stored(my_minb:my_maxb,nk,nsppol)
  ! .TRUE. if \psi(r) is in memory

  complex(gwpc),pointer :: wfg(:,:,:,:)        
  ! wfg(npwwfn*nspinor,my_minb:my_maxb,nk,nsppol)
  !  Wavefunctions in reciprocal space on the G-sphere.

  complex(gwpc),pointer :: wfr(:,:,:,:)        
  ! wfr(nfft*nspinor,my_minb:my_maxb,nk,nsppol)
  !  Wavefunctions in real space on the FFT mesh.

 end type wavefunctions_information

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/gs_hamiltonian_type
!! NAME
!! gs_hamiltonian_type
!!
!! FUNCTION
!! This datastructure contains the information about one Hamiltonian,
!! needed in the "getghc" routine, that apply the Hamiltonian
!! on a wavefunction.
!! About the non-local part of the Hamiltonian
!! The operator Onl has the following general form:
!! $Onl=sum_{R,lmn,l''m''n''} {|P_{Rlmn}> Enl^{R}_{lmn,l''m''n''} <P_{Rl''m''n''}|}$
!! Operator Onl is -- in the typical case -- the nonlocal potential.
!! - In a classical plane-wave calculation, $Enl^{R}_{lmn,l''m''n''}$ is the
!!   Kleinmann-Bylander energy $Ekb^{R}_{ln}$.
!! - In a PAW calculation, $Enl^{R}_{lmn,l''m''n''}$ can either be the nonlocal
!!   contribution to the Hamiltonian $D_{ij}$ or the overlap matrix $S_{ij}$.
!! - The |P_{Rlmn}> are the projector functions.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type gs_hamiltonian_type

! Integer scalar

  integer :: dimekb1
   ! First dimension of Ekb (see ekb in this file)
   ! Same as psps%dimekb
   ! ->Norm conserving : Max. number of Kleinman-Bylander energies
   !                     for each atom type
   !                     dimekb1=lnmax
   ! ->PAW : Max. number of Dij coefficients connecting projectors
   !                     for each atom
   !                     dimekb1=lmnmax*(lmnmax+1)/2

  integer :: dimekb2
   ! Second dimension of Ekb (see ekb in this file)
   ! ->Norm conserving psps: dimekb2=ntypat
   ! ->PAW                 : dimekb2=natom

  integer :: istwf_k
   ! option parameter that describes the storage of wfs

  integer :: lmnmax
   ! Maximum number of different l,m,n components over all types of psps.
   ! same as dtset%lmnmax

  integer :: matblk
   ! dimension of the array ph3d

  integer :: mgfft
   ! maximum size for 1D FFTs (same as dtset%mgfft)

  integer :: mproj  ! TO BE SUPPRESSED LATER
   ! Maximum number of non-local projectors over all angular momenta
   !  and type of psps
   ! 0 only if all psps are local
   ! same as psps%mproj

  integer :: mpsang
   ! Highest angular momentum of non-local projectors over all type of psps.
   ! shifted by 1 : for all local psps, mpsang=0; for largest s, mpsang=1,
   ! for largest p, mpsang=2; for largest d, mpsang=3; for largest f, mpsang=4
   ! This gives also the number of non-local "channels"
   ! same as psps%mpsang

  integer :: mpssoang
   ! Maximum number of channels, including those for treating the spin-orbit coupling
   ! when mpspso=1, mpssoang=mpsang
   ! when mpspso=2, mpssoang=2*mpsang-1
   ! same as psps%mpssoang

  integer :: natom
   ! The number of atoms for this dataset
   ! same as dtset%natom

  integer :: nfft
   ! number of FFT grid points
   ! same as dtset%nfft

  integer :: npw
   ! number of plane waves

  integer:: nspinor
   ! Number of spinorial components

  integer :: ntypat
   ! Number of types of pseudopotentials
   ! same as dtset%ntypat

  integer :: nvloc
   ! Number of components of vloc
   ! usually, nvloc=1, except in the non-collinear magnetism case, where nvloc=4

  integer :: n4,n5,n6
   ! same as ngfft(4:6)

  integer :: usepaw
   ! if usepaw=0 , use norm-conserving psps part of the code
   ! is usepaw=1 , use paw part of the code

  integer :: useylm
   ! governs the way the nonlocal operator is to be applied:
   !   1=using Ylm, 0=using Legendre polynomials

! Integer arrays

  integer, pointer :: atindx(:)
   ! atindx(natom)
   ! index table for atoms (see scfcv.f)

  integer, pointer :: atindx1(:)
   ! atindx1(natom)
   ! index table for atoms, inverse of atindx (see scfcv.f)

  integer, pointer :: gbound(:,:)
   ! gbound(2*mgfft+8,2)
   ! G sphere boundary

  integer, pointer :: indlmn(:,:,:)
   ! indlmn(6,lmnmax,ntypat)
   ! For each type of psp,
   ! array giving l,m,n,lm,ln,spin for i=ln  (if useylm=0)
   !                                or i=lmn (if useylm=1)

! integer, pointer :: indpw_k(:,:)
   ! indpw_k(4,npw)
   ! array which gives fft box index for given basis sphere
   ! This component was taken away : CPU time problem !

! integer, pointer :: kg_k(:,:)
   ! kg_k(3,npw)
   ! G vector coordinates with respect to reciprocal lattice translations
   ! This component was taken away : CPU time problem !

  integer, pointer :: nattyp(:)
   ! nattyp(ntypat)
   ! # of atoms of each type

  integer :: ngfft(18)
   ! ngfft(1:3)=integer fft box dimensions
   ! ngfft(4:6)=integer fft box dimensions, might be augmented for CPU speed
   ! ngfft(7)=fftalg
   ! ngfft(8)=fftalg
   ! same as dtset%ngfft

  integer :: nloalg(5)
   ! governs the choice of the algorithm for non-local operator
   ! same as dtset%nloalg

  integer, pointer :: pspso(:)
   ! pspso(ntypat)
   ! For each type of psp, 1 if no spin-orbit component is taken
   ! into account, 2 if a spin-orbit component is used

! Real (real(dp)) scalar

  real(dp) :: ucvol
   ! unit cell volume (Bohr**3)

! Real (real(dp)) arrays

  real(dp), pointer :: ekb(:,:,:)
   ! ekb(dimekb1,dimekb2,nspinor**2)
   !  ->Norm conserving : (Real) Kleinman-Bylander energies (hartree)
   !          for number of basis functions (l,n) (lnmax)
   !          and number of atom types (ntypat)
   !          dimekb1=lnmax ; dimekb2=ntypat
   !  ->PAW : (Real, symmetric) Frozen part of Dij coefficients
   !                            to connect projectors
   !          for number of basis functions (l,m,n) (lmnmax)
   !          and number of atom (natom)
   !          dimekb1=lmnmax*(lmnmax+1)/2 ; dimekb2=natom
   ! NOTE (MT) : ekb (norm-conserving) is now diagonal (one dimension
   !             lnmax); it would be easy to give it a second
   !             (symmetric) dimension by putting
   !             dimekb1=lnmax*(lnmax+1)/2
   !             in the place of dimekb1=lnmax.

  real(dp), pointer :: sij(:,:)
   ! sij(dimekb1,ntypat*usepaw) = overlap matrix for paw calculation

! real(dp), pointer :: ffnl(:,:,:,:)
   ! ffnl(npw,2,lmnmax,ntypat)
   ! nonlocal form factors
   ! This component was taken away : CPU time problem !

  real(dp) :: gmet(3,3)
   ! reciprocal space metric tensor in Bohr**-2

  real(dp) :: gprimd(3,3)
   ! dimensional reciprocal space primitive translations (Bohr^-1)

! real(dp), pointer :: kinpw(:)
   ! kinpw(npw)
   ! (modified) kinetic energy for each plane wave (Hartree)
   ! This component was taken away : CPU time problem !

  real(dp) :: kpoint(3)
   ! dimensionless k point coordinates wrt reciprocal lattice vectors

  real(dp), pointer :: phkxred(:,:)
   ! phkxred(2,natom)
   ! phase factors exp(2 pi k.xred)

  real(dp), pointer :: ph1d(:,:)
   ! ph1d(2,3*(2*mgfft+1)*natom)
   ! 1-dim phase arrays for structure factor (see getph.f).

! real(dp), pointer :: ph3d(:,:,:)
   ! ph3d(2,npw,matblk)
   ! 3-dim structure factors, for each atom and plane wave
   ! This component was taken away : CPU time problem !

! real(dp), pointer :: vlocal(:,:,:,:)
   ! vlocal(n4,n5,n6,nvloc)
   ! local potential in real space, on the augmented fft grid
   ! This component was taken away : CPU time problem !

  real(dp),pointer :: xred(:,:)
   ! xred(3,natom)
   ! reduced coordinates of atoms (dimensionless)

! real(dp),pointer :: ylm(:,:)
   ! ylm(npw,mpsang*mpsang*useylm)
   ! Real spherical harmonics for each G
   ! This component was taken away : CPU time problem !

 end type gs_hamiltonian_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/Gpairs_type
!! NAME
!! Gpairs_type
!!
!! FUNCTION
!! The Gpairs_type data type contains information useful to symmetrize in reciprocal
!! space any two-point function, f_q(G1,G2), which has the same symmetry of the crystal.
!! In particular this structure reports:
!!
!! 1) The List of the irreducible G pairs
!! 4) Tables giving, for each G1 G2 pair in reciprocal space, the corresponding
!!    irreducible pair and the reqired symmetry.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type Gpairs_type

  integer :: ng                      ! The number of G vectors
  integer :: ngpairs                 ! Total number of G pairs ie ng**2, used to dimension gptab and gptabo
  integer :: niggp                   ! Number of irreducible G-Gp pairs for this q-point, used to dimension gptab and gptabo
  integer :: nsym                    ! Number of operations
  integer :: timrev                  ! If time-reversal has been considered to generate the G-sphere
  logical :: can_use_timrev          ! .TRUE. If time-reversal can be used (actually only at Gamma)

  integer,pointer :: fp2ip(:,:,:)    ! fp2ip(2,T1,T2) gives the sequential index, in the array gvec, of the
                                     ! indendent pair (G1,G2) such as (T1,T2)= S (G1,G2)
  integer,pointer :: fptabo(:,:)     ! fptabo(ng,ng)=index of the symmetry operation in the array symrec such as (T1,T2)= S (G1,G2)
                                     ! At Gamma, if time-reversal is used, the index is negative
  integer,pointer :: ip2fp(:,:)      ! ip2fp(2,niggp)= index of G1 and G2 in the array gvec for each niggp independent pair.

  real(dp) :: qpt(3)                 ! The point in the BZ where the two-point function has to be symmetrized

 end type Gpairs_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/Gvectors_type
!! NAME
!! Gvectors_type
!!
!! FUNCTION
!! The Gvectors_type data type contains information related to the set of G vectors
!! used during a screening or a GW calculation, as well as symmetry tables relating
!! these vectors. Presently the following quantities are stored
!!
!! 1) The reduced coordinates of the G vectors (arrays gvec)
!! 2) Tables giving the correspondence between a G-vector and its rotated image
!!    through a symmetry operation in reciprocal space.
!! 3) List of the irreducible G pairs
!! 4) Tables giving, for each pair in the full reciprocal space, the corresponding
!!    irreducible pair as well as the symmetry operation in reciprocal space
!!
!! Note that, unlike the GS part, the basis set does not depend on the k-point.
!!
!! NOTES
!! To indicate the indices in the arrays grottb, grottbm1 we use the following notation :
!!
!!  g defines the index of the reciprocal lattice vector in the array gvec
!!  s  indicates the index of the symmetry operation in reciprocal space
!!  i  can  be one or two. 1 is used to indicate the identity operator
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type Gvectors_type

  integer :: ng                              ! Number of G vectors in the array gvec
  integer :: nsh                             ! Number of shells
  integer :: nsym                            ! The number of symmetry operations
  integer :: timrev                          ! 2 if time-reversal is used, 1 otherwise

  integer,pointer :: g2sh(:)                 
  ! g2sh(ng)
  ! For each G gives the shell to which it belongs.

  integer,pointer :: gvec(:,:)               
  ! gvec(3,ng)
  ! Reduced coordinates of G vectors.

  integer,pointer :: rottb(:,:,:)            
  ! rottb(ng,timrev,nsym)
  ! grottb(G,I,S) is the index of (SI) G in the array gvec 
  ! where I is either the identity or the inversion.

  integer,pointer :: rottbm1(:,:,:)          
  ! grottb(ng,timrev,nsym) 
  ! grottbm1(G,I,S) is the index of IS{^-1} G in the array gvec

  integer,pointer :: shlim(:)
  ! shlim(nsh+1) 
  ! Index of the first G vector in each shell, =ng+1 for nsh+1

  real(dp) :: gmet(3,3)                      
  ! Reciprocal space metric ($\textrm{bohr}^{-2}$).

  real(dp) :: gprimd(3,3)                    
  ! Dimensional reciprocal space primitive translations (bohr^{-1})

  real(dp),pointer :: shlen(:)               
  ! shlen(nsh)
  ! Radius of each shell.

  !TODO switch to dpc
  complex(gwpc),pointer :: phmGt(:,:)        
  ! phmGt(ng,nsym)
  ! Phase factor e^{-i2\pi(G.\tau)} where $\tau$ is the fractional translation associated to isym.

  complex(gwpc),pointer :: phmSGt(:,:)       
  ! phmSGt(ng,nsym)
  ! Phase factor e^{-i2\pi(SG.\tau)} where S is one of the symmetry properties in reciprocal space.

 end type Gvectors_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/hdr_type
!! NAME
!! hdr_type
!!
!! FUNCTION
!! It contains all the information needed to write a header for a
!! wf, den or pot file.
!! The structure of the header is explained in the abinis_help.html file.
!! The datatype is considered as an object, to which are attached a whole
!! set of "methods", actually, different subroutines.
!! A few of these subroutines are : hdr_init, hdr_update, hdr_clean,
!! hdr_check, hdr_io, hdr_skip.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

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

  type(pawrhoij_type), pointer :: pawrhoij(:) ! EVOLVING variable, only for paw

!Should make a list of supplementary infos
! MG: For postprocessing purposes, it is quite useful to
!  have kptrlatt as well as nshiftk and shiftk. also kptopt is useful 
!  to know if time reversal can be employed

 end type hdr_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/bz_mesh_type
!! NAME
!! bz_mesh_type
!!
!! FUNCTION
!! The BZ_mesh structured datatype contains different information
!! on the grid used to sample the BZ : the k-points in the
!! full Brillouin zone BZ, the irreducible wedge IBZ as well
!! as tables describing the symmetry relationship between these points.
!! This structure can be easily created and desctructed using the methods
!! bz_mesh_type_init and bz_mesh_type_free described in the bz_mesh_method subroutine
!!
!! NOTES
!! Presently used only for GW calculations
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

type BZ_mesh_type

 integer :: nshift=0
 real(dp),pointer :: shift(:,:)
  !  shift(3,nshift)
  !  shift for k-points, usually nshift=1

 integer :: kptrlatt(3,3)=RESHAPE((/0,0,0,0,0,0,0,0,0/),(/3,3/))
  ! Coordinates of three vectors in real space, expressed in reduced coordinates. 
  ! They defines a super-lattice in real space. The k point lattice is the reciprocal of 
  ! this super-lattice, eventually shifted by shift.

! For tetrahedrons
 integer :: ntetra_irr     ! No. of tetrahedrons, 0 if tetrahedrons are not stored
 real(dp) ::tetra_vol      ! Volume of each tetrahedron 

 integer,pointer :: tetra_full(:,:,:)
  ! tetra_full(4,2,ntetra_irr)
  ! For each irred tetrahedron, the list of k-point vertices.
  !  tetra_full(:,1,it) contains the index of the irreducible k-points
  !  tetra_full(:,2,it) contains the index of the full k-points

 integer,pointer :: tetra_mult(:)
  ! tetra_mult(ntetra_irr)
  ! For each irreducible tetrahedron, its multiplicity.

  integer,pointer :: tetra_wrap(:,:,:) 
  ! tetra_wrap(3,4,ntetra_irr)
  ! Flag to wrap q-points outside the IBZ (+-1) to get the irreducible tetrahedra

!scalars
 integer :: nbz                    ! Number of points in the BZ. 
 integer :: nibz                   ! Number of points in the IBZ.
 integer :: nsym                   ! Number of symmetry operations
 integer :: timrev                 ! 2 if time reversal symmetry can be used, 1 otherwise

!arrays
 integer,pointer :: rottb(:,:,:)   
 ! rottb(nbz,timrev,nsym), 
 ! Index of (IS)k in the BZ arraywhere S is a sym operation in rec. space, 
 ! I is the identity or the inversion operator (1,2 resp) 

 integer,pointer :: rottbm1(:,:,:) 
 ! rottbm1(nbz,timrev,nsym) 
 ! Index of IS^{-1} k in the BZ array.

 integer,pointer :: tab(:)         
 ! tab(nbz)
 ! For each point in the BZ, give the index of the symmetric 
 ! irreducible point in the array ibz.

 integer,pointer :: tabi(:)        
 ! tabi(nbz)
 ! For each point in the BZ, tabi tells if time-reversal has to be 
 ! used to obtain k_BZ starting from the correponding point in ibz  (1=>no, -1=>yes)

 integer,pointer :: tabo(:)        
 ! tabo(nbz)
 ! For each point in the BZ, give the index, in the array symrec, of the 
 ! symmetry operation in reciprocal space which rotates k_IBZ onto \pm k_BZ (depending on tabi)

 real(dp) :: gmet(3,3)             
 ! Reciprocal space metric ($\textrm{bohr}^{-2}$).

 real(dp) :: gprimd(3,3)           
 ! Dimensional primitive translations for reciprocal space ($\textrm{bohr}^{-1}$)
 
 real(dp),pointer :: bz(:,:)       
 ! bz(3,nbz)
 ! Points in the BZ in reduced coordinates.

 real(dp),pointer :: ibz(:,:)      
 ! ibz(3,nibz)
 ! Points in the IBZ in reduced coordinates.

 real(dp),pointer :: wt(:)         
 ! wt(nibz)
 ! Weights for each point in the IBZ.

 ! TODO should be dpc
 complex(gwpc),pointer :: tabp(:)  
 ! tabp(nkbz)
 ! For each point in the BZ, give the phase factors associated 
 ! to non-symmorphic operations, i.e e^{-i2\pi k_IBZ.R{^-1}t}=e^{-i2\pi k_BZ cdot t}
 ! where \transpose R{-1}=S and  (S k_IBZ)=\pm k_BZ (depending on tabi)

end type BZ_mesh_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/vecteur_type
!! NAME
!! vecteur_type
!!
!! FUNCTION
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type vecteur_type

  integer,pointer :: indice(:)

 end type vecteur_type
!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/MPI_type
!! NAME
!! MPI_type
!!
!! FUNCTION
!! The MPI_type structured datatype gather different information
!! about the MPI parallelisation : number of processors,
!! the index of my processor, the different groups of processors, etc ...
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type MPI_type

! Integer scalar

!***********************************************************************************

!Set of variables for parallelism, that do NOT depend on input variables.
!These are independent of the dataset, and are initialized at the beginning of
!an ABINIT run. The other should be initialized only inside a dataset.

  integer :: world_comm       ! number of the world communicator MPI_COMM_WORLD
  integer :: world_group      ! number of the world group of processor MPI_GROUP_NULL
  integer :: me               ! number of my processor in the group of all processors
  integer :: nproc            ! number of processors

  integer :: paral_compil
   ! paral_compil =0 : no -DMPI flag was activated in the compiling procedure
   ! paral_compil =1 : the -DMPI flag was activated in the compiling procedure
  integer :: paral_compil_mpio
   ! paral_compil_mpio =0 : no -DMPIO flag was activated in the compiling procedure
   ! paral_compil_mpio =1 : the -DMPIO flag was activated in the compiling procedure

!***********************************************************************************

! The other should be initialized only inside a dataset.
  integer :: paral_compil_kpt
  integer :: paral_compil_fft

  integer :: paral_level
   ! level of parallelization at a moment in the code
   ! level = 1 : level parareel
   ! level = 2 : level nkpt
   ! level = 3 : level FFT

  integer :: paralbd
   ! relevant only if paral_compil_kpt=1 . So, in addition to the kpt parallelization :
   ! paralbd=0 : (no //ization on bands)
   ! paralbd=1 : (//ization on bands)
   ! paralbd>1 : (//ization on blocks of bands)

  integer :: gwpara
   ! level of parallelization at a moment in the GW code
   ! level = 0 : no parallelization (seq run)
   ! level = 1 : kpoints
   ! level = 2 : bands
   ! level = 3 : in the future, maybe : mixed (kpoints+bands)

  integer :: me_group         ! number of my processor in my group of kpt
  integer :: nproc_group      ! number of processors in my group of kpt
  integer :: me_fft           ! number of my processor in my group of FFT
  integer :: me_band           ! number of my processor in my group of bands
  integer :: nproc_fft        ! number of processors in my group of FFT
  integer :: master_fft       ! number of master of my fft group (in the world_group)
  integer :: paral_fft        ! set to 1 if the FFT parallelisation is active
  integer :: me_g0            ! if set to 1, means that the current processor is taking care of the G(0 0 0) planewave.
  integer :: num_group_fft    ! number of FFT group of my processor. 0 if my processor is not in a group
  integer :: num_group        ! number of group of my processor. 0 if my processor is not in a group
  integer :: nproc_per_kpt    ! number of processors per kpt

  integer :: fft_master_group
   ! fft_master_group
   ! group of processors of fft_master_comm
   ! exists only when paral_fft = 1

  integer :: fft_master_comm
   ! fft_master_comm
   ! communicator on master processors
   ! (one processor per fft_group or all processors when paral_fft = 0)

  integer :: fft_option_lob
   ! fft_option_lob
   ! option for lob
   ! fft_option_lob=1 : old version of lob
   ! fft_option_lob=2 : new version of lob
   ! exists only when paral_fft = 1

  integer :: has_band_comm
   ! has_band_comm
   ! 1 if mpi_enreg%band_comm(:) is allocated

! Integer arrays

  integer, pointer :: band_comm(:)
   ! band_comm(nproc_per_kpt)
   ! tab of communicators of processors which treat one set ogf bands
   ! exists only when paralbd = 1 and has_band_comm=1

  integer, pointer :: fft_group(:)
   ! fft_group(nkpt*nsppol)
   ! tab of groups of processors which treat ffts
   ! exists only when paral_fft = 1

  integer, pointer :: fft_comm(:)
   ! fft_comm(nkpt*nsppol)
   ! tab of communicators of processors which treat ffts of a kpt
   ! exists only when paral_fft = 1

  integer, pointer :: proc_distrb(:,:,:)
   ! proc_distrb(nkpt,mband,nsppol)
   ! number of the processor that will treat
   ! each band in each k point.

  integer, pointer :: kpt_group(:)
   ! kpt_group(nproc_per_kpt)
   ! tab of groups of processors which treat one nkpt/nsppol
   ! exists only when paralbd > 1

  integer, pointer :: kpt_comm(:)
   ! kpt_comm(nproc_per_kpt)
   ! tab of communicators of processors which treat one nkpt/nsppol
   ! exists only when paralbd > 1

  integer, pointer :: kptdstrb(:,:,:)
   ! kptdstrb(me,ineigh,ikptloc)
   ! tab of processors required for mv_3dte.f and berryphase_new.f

  integer, pointer :: kptdstrbi(:,:,:)
   ! same as kptdstrb, but for k-points in the iBZ
   ! required for MPI // of the finite electric field (see vtorho.f)

  integer, pointer :: nplanes_fft(:)
   ! nplanes_fft(nkpt)
   ! number of planes for my proc me_fft
   ! exists only if mpi_enreg%paral_compil_fft==1

  integer, pointer :: ind_fft_planes(:,:)
   ! ind_fft_planes(nkpt,nplanes_fft)
   ! indice of planes for each kpoint for my proc me_fft
   ! exists only if mpi_enreg%paral_compil_fft==1


  type(vecteur_type), pointer :: ind_kg_mpi_to_seq(:)
   ! ind_kg_mpi_to_seq(nkpt) 
   ! in case of //band and //fft, for each processor,
   ! indice of kg in the numerotation of the sequentiel mode

  integer           :: flag_ind_kg_mpi_to_seq
   ! flag to activate the building of ind_kg_mpi_to_seq

! Adds for parallelization over perturbations
  integer :: paral_compil_respfn
   ! paral_compil_respfn =0 : no -DMPI flag was activated in the compiling procedure
   ! paral_compil_respfn =1 : the -DMPI flag was activated in the compiling procedure

  integer :: me_respfn           ! number of my processor in my group of perturbations
  integer :: nproc_respfn        ! number of processors in my group of perturbations
  integer :: my_respfn_group     ! my group for calculating perturbations
  integer :: my_respfn_comm      ! my communicator of my_respfn_group
  integer :: respfn_master_group ! groups for masters of respfn_groups
  integer :: respfn_master_comm  ! communicator for masters of respfn_groups
  integer :: ngroup_respfn       ! number of groups for calculating perturbations
  integer :: spaceComm           ! communicator for calculating responsefunction
                                 ! default is MPI_COMM_WORLD but may be changed in 08seqpar/loper3.F90

  integer, pointer :: respfn_group(:) ! groups for calculating perturbations
  integer, pointer :: respfn_comm(:)  ! communicators for respfn_group

  ! Wavelet paralelisation, use when %paral_compil_fft == 1
  ! Array to store the description of the scaterring in real space of
  ! the potentials and density. It is allocated to (0:nproc-1,4).
  ! The four values are:
  ! - the density size in z direction ( = ngfft(3)) ;
  ! - the potential size in z direction ( <= ngfft(3)) ;
  ! - the position of the first value in the complete array ;
  ! - the shift for the potential in the array.
  integer, pointer :: nscatterarr(:,:)
  ! Array to store the total size (of this proc) of the potentails arrays when
  ! the memory is distributed following nscatterarr.
  integer, pointer :: ngatherarr(:,:)
  ! Store the ionic potential size in z direction.
  integer :: ngfft3_ionic
  ! End wavelet additions

!This is for the bandFFT case
   character :: mode_para
   !If mode_para=='bandFFT', we are in bandFFT mode
   integer :: commcart
   !This is the communicator for the full cartesian array
   integer :: comm_band, comm_fft
   !The communicators over bands and fft respectively
   integer :: me_cart
   !This is the rank of the proc in the full cartesian array
   integer :: dimcart
   !This is the dimension of the cartesian array (2 for 2-dim)
   integer :: nproc_band
   !This is the number of procs on which we distribute bands
   integer, pointer :: sizecart(:)
   !The first dimension is the number of fft processors, the second the number of bands
   integer, pointer :: coords(:)
   !The coordinate of the proc in the cartesian array

!This is for the kpt & bandFFt case
   integer :: commcart_3d      ! 3D communicator
   integer :: comm_kpt         ! communicator of kpt
   integer :: me_kpt           ! number of my processor in my group of kpt
   integer :: nproc_kpt        ! number of procs on which we distribute kpt
   integer :: me_cart_2d       ! This is the rank of the proc in the commcart

! Adds for parareel
  integer :: parareel
   ! parareel = 0 default
   ! parareel = 1 if treats parareel case

! All the following data exist only in the parareel=1 case
  integer :: npara                 ! number of loops on gstate
  integer :: ipara                 ! number of actual internal loop on gstate
  integer :: jpara                 ! number of actual external loop on gstate
  integer :: me_group_para         ! number of my processor in my group of para
  integer :: nproc_group_para      ! number of processors in my group of para
  integer :: num_group_para        ! number of group of my processor. 0 if my processor is not in a group
  integer :: nproc_per_para        ! number of processors per para
  integer :: master_group_para     ! number of the master processor (in the world group) of my group of para

  integer, pointer :: proc_distrb_para(:,:)
   ! proc_distrb_para(npara,nkpt)
   ! exists only when parareel = 1
   ! number of the processor that will treat
   ! each kpt in each para.

  integer, pointer :: kpt_group_para(:)
   ! kpt_group_para(npara)
   ! tab of groups of processors which treat one npara
   ! exists only when parareel = 1

  integer, pointer :: kpt_comm_para(:)
   ! kpt_comm_para(npara)
   ! tab of communicators of processors which treat one npara
   ! exists only when parareel = 1

  integer :: bandpp

 end type MPI_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/mttk_type
!! NAME
!! mttk_type
!!
!! FUNCTION
!! For Martyna et al. (TTK) reversible MD integration scheme and related data
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type mttk_type

!Real (double precision) scalars

   real(dp) :: glogv
    !Logarithm of the volume

   real(dp) :: vlogv
    !Derivative of logv

!Real (double precision) arrays

  real(dp) :: gboxg(3,3)
   !Imbalance in pressure (see paper)

  real(dp) :: vboxg(3,3)
   !Velocity of log rprimd (see paper)

  real(dp), pointer :: glogs(:)
   ! glogs(nnos)
   ! Imbalance of kinetic energy

  real(dp), pointer :: vlogs(:)
   ! vlogs(nnos)
   ! Velocities of thermostat variables

  real(dp), pointer :: xlogs(:)
   ! xlogs(nnos)
   ! Positions of thermostat variables

 end type mttk_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/nuclear_type
!! NAME
!! nuclear_type
!!
!! FUNCTION
!! Property results typically at each atom for each nspden. This appears to
!! be necessary because in PAW calculations there can be different nspden values
!! at each nuclear site.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type nuclear_type

!Real (real(dp)) arrays

  real(dp), pointer :: spden(:)
   ! spden(nspden)
   ! data for each ispden value; note that nspden for a given nucleus will 
   ! typically be retrieved from pawrhoij(iatom)%nspden and hence is nucleus
   ! specific

 end type nuclear_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pawang_type
!! NAME
!! pawang_type
!!
!! FUNCTION
!! For PAW, ANGular mesh discretization and related data
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pawang_type

!Integer scalars

  integer :: angl_size
   ! Dimension of paw angular mesh
   ! angl_size=ntheta*nphi

  integer :: l_max
   ! Maximum value of angular momentum l+1

  integer :: l_size_max
   ! Maximum value of angular momentum +1
   ! leading to non-zero Gaunt coefficients
   ! l_size_max = 2*l_max-1

  integer :: ngnt
   ! Number of non zero Gaunt coefficients

  integer :: ntheta, nphi
   ! Dimensions of paw angular mesh

  integer :: nsym
   ! Number of symmetry elements in space group

  integer :: gnt_option
   ! Option for Gaunt coefficients:
   !  gnt_option/=2, Gaunt coeffs are computed up to l_size_max
   !  gnt_option==1, Gaunt coeffs are computed up to 2*l_size_max-1

  integer :: use_ls_ylm
   ! Flag: use_ls_ylm=1 if pawang%ls_ylm pointer is allocated

!Integer arrays

  integer, pointer :: gntselect(:,:)
   ! gntselect(l_size_max**2,l_max**2*(l_max**2+1)/2)
   ! Selection rules for Gaunt coefficients
   ! (if gntselect>0, Gaunt coeff. is non-zero)

!Real (real(dp)) arrays

  real(dp), pointer :: anginit(:,:)
   ! anginit(3,angl_size)
   ! For each point of the angular mesh, gives the coordinates
   ! of the corresponding point on an unitary sphere
   ! Not used in present version (5.3)

  real(dp), pointer :: angwgth(:)
   ! angwgth(angl_size)
   ! For each point of the angular mesh, gives the weight
   ! of the corresponding point on an unitary sphere

  real(dp), pointer :: ls_ylm(:,:,:)
   ! ls_ylm(2,l_max**2*(l_max**2+1)/2,2)
   ! LS operator in the real spherical harmonics basis
   ! ls_ylm(ilm1m2,ispin)= <sigma, y_lm1| LS |y_lm2, sigma_prime>

  real(dp), pointer :: realgnt(:)
   ! realgnt(ngnt)
   ! Non zero real Gaunt coefficients

  real(dp), pointer :: ylmr(:,:)
   ! ylmr(l_size_max**2,angl_size)
   ! Real Ylm calculated in real space

  real(dp), pointer :: zarot(:,:,:,:)
   !  zarot(l_size_max,l_size_max,l_max,nsym)
   !  Coeffs of the transformation of real spherical
   !  harmonics under the symmetry operations

 end type pawang_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pawfgr_type
!! NAME
!! pawfgr_type
!!
!! FUNCTION
!! For PAW, Fine rectangular GRid parameters and related data
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pawfgr_type

!Integer scalars

  integer :: mgfft, nfft
   ! Values of mffft and nfft for the fine rectangular grid:
   !   mgfft= max(ngfft(i)) [max. size of 1D FFT grid]
   !   nfft=ngfft1*ngfft2*ngfft3 [number of pts in the FFT box]

  integer :: mgfftc, nfftc
   ! Values of mffft and nfft for the COARSE rectangular grid:
   !   mgfftc= max(ngfftc(i)) [max. size of 1D FFT grid]
   !   nfftc=ngfftc1*ngfftc2*ngfftc3 [number of pts in the FFT box]

  integer :: usefinegrid
   ! Flag: =1 if a double-grid is used to convert spherical data
   !       to Fourier grid. =0 otherwise

  integer :: natom
   ! Number of atoms in the unit cell

!Integer arrays

  integer, pointer :: coatofin(:)
   ! coatofin(nfftc)
   ! Index of the points of the coarse grid on the fine grid

  integer, pointer :: fintocoa(:)
   ! fintocoa(nfft)
   ! Index of the points of the fine grid on the coarse grid
   !  (=0 if the point of the fine grid does not belong to the coarse grid)

  integer :: ngfft(18)
   ! ngfft(1:18)=integer array with FFT box dimensions and other
   ! information on FFTs, for the fine rectangular grid

  integer :: ngfftc(18)
   ! ngfft(1:18)=integer array with FFT box dimensions and other
   ! information on FFTs, for the COARSE rectangular grid

!Real (real(dp))

  real(dp) :: gsqcut
   ! Fourier cutoff on G^2 for "large sphere" of radius double
   ! that of the basis sphere corresponding to paw_ecutdg
   ! (concerns the fine rectangular grid)

 end type pawfgr_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pawfgrtab_type
!! NAME
!! pawfgrtab_type
!!
!! FUNCTION
!! For PAW, various arrays giving data related to fine grid for a given atom
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pawfgrtab_type

!Integer scalars

  integer :: l_size
   ! 1+maximum value of l leading to non zero Gaunt coeffs
   ! for the considered atom type

  integer :: gylm_allocated
   ! 1 if gylm() is allocated (and computed)

  integer :: gylmgr_allocated
   ! 1 if gylmgr() is allocated (and computed)

  integer :: gylmgr2_allocated
   ! 1 if gylmgr2() is allocated (and computed)

  integer :: nfgd
   ! Number of Fine rectangular GriD points
   ! in the paw sphere around considered atom

  integer :: rfgd_allocated
   ! 1 if rfgd() is allocated (and computed)

  integer :: vlocgr_allocated
   ! 1 if vlocgr() is allocated (and computed)

!Integer arrays

  integer, pointer :: ifftsph(:)
   ! ifftsph(nfgd)
   ! Array giving the FFT index (fine grid) of a point in the paw
   ! sphere around considered atom (ifftsph=ix+n1*(iy-1+n2*(iz-1))

!Real (real(dp)) arrays

  real(dp), pointer :: gylm(:,:)
   ! gylm(nfgd,l_size*l_size)
   ! Gives g_l(r)*Y_lm(r) on the fine rectangular grid
   ! around considered atom

  real(dp), pointer :: gylmgr(:,:,:)
   ! gylmgr(3,nfgd,l_size*l_size)
   ! Gives the gradient of g_l(r)*Y_lm(r) wrt cart. coordinates
   ! on the fine rectangular grid around considered atom

  real(dp), pointer :: gylmgr2(:,:,:)
   ! gylmgr(6,nfgd,l_size*l_size)
   ! Gives the second gradient of g_l(r)*Y_lm(r) wrt cart. coordinates
   ! on the fine rectangular grid around considered atom

  real(dp), pointer :: rfgd(:,:)
   ! r(3,nfgd)
   ! Gives all R vectors (r-r_atom) on the Fine rectangular GriD
   ! around considered atom

  real(dp), pointer :: vlocgr(:,:)
   ! vlocgr(3,nfgd)
   ! Gives the gradient of local potential wrt cart. coordinates
   ! on the fine rectangular grid around considered atom
   ! Only use in response function calculations

 end type pawfgrtab_type

!!***

!-------------------------------------------------------------------------

!!****t* defs_datatypes/pawrad_type
!! NAME
!! pawrad_type
!!
!! FUNCTION
!! For PAW, RADial mesh discretization and related data
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pawrad_type

!Integer scalars

  integer :: int_meshsz
   ! Mesh size used in integrals computation
   ! Integrals will be computed up to r(int_meshsz)

  integer :: mesh_size
   ! Dimension of radial mesh

  integer :: mesh_type
   ! Type of mesh
   !     1=regular grid: r(i)=(i-1)*AA
   !     2=logarithmic grid: r(i)=AA*(exp[BB*(i-1)]-1)
   !     3=logarithmic grid: r(i>1)=AA*exp[BB*(i-1)] and r(1)=0
   !     4=logarithmic grid: r(i)=-AA*ln[1-BB*(i-1)] with BB=1/n

!Real (real(dp)) scalars

  real(dp) :: lstep
   ! Exponential step of the mesh (BB parameter above)
   ! Defined only if mesh type is logarithmic

  real(dp) :: rmax
   ! Max. value of r = rad(mesh_size)

  real(dp) :: rstep
   ! Radial step of the mesh (AA parameter above)

  real(dp) :: stepint
   ! Radial step used to convert any function from the
   ! present grid onto a regular grid in order to
   ! integrate it using trapeze method

!Real (real(dp)) arrays

  real(dp), pointer :: rad(:)
   ! rad(mesh_size)
   ! Coordinates of all the points of the mesh

  real(dp), pointer :: radfact(:)
   ! radfact(mesh_size)
   ! Factor used to compute radial integrals
   ! Before being integrated on the present mesh,
   ! any function is multiplied by this factor

  real(dp), pointer :: simfact(:)
   ! radfact(mesh_size)
   ! Factor used to compute radial integrals by the a Simpson scheme
   ! Integral[f] = Sum_i [simfact(i)*f(i)]

 end type pawrad_type

!!***

!!****t* defs_datatypes/pawtab_type
!! NAME
!! pawtab_type
!!
!! FUNCTION
!! For PAW, TABulated data initialized at start
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pawtab_type

!Integer scalars

  integer :: basis_size
   ! Number of elements for the paw nl basis on the considered atom type

  integer :: has_nabla
   ! if /=0, onsite matrix elements of the nabla operator are stored and calculated

  integer :: ij_proj
   ! Number of (i,j) elements for the orbitals on which U acts (PAW+U only)
   ! on the considered atom type (ij_proj=1 (1 projector), 3 (2 projectors)...)
   ! Also used for local exact-exchange

  integer :: ij_size
   ! Number of (i,j) elements for the symetric paw basis
   ! on the considered atom type (ij_size=basis_size*(basis_size+1)/2)

  integer :: lcut_size
   ! Maximum value of l+1 leading to non zero Gaunt coeffs
   ! modified by dtset%pawlcutd
   ! lcut_size=min(2*l_max,dtset%pawlcutd)+1

  integer :: l_size
   ! Maximum value of l+1 leading to non zero Gaunt coeffs
   ! l_size=2*l_max+1

  integer :: lexexch
   ! lpawu gives l on which local exact-exchange is applied for a given type of atom.

  integer :: lmn_size
   ! Number of (l,m,n) elements for the paw basis

  integer :: lmn2_size
   ! lmn2_size=lmn_size*(lmn_size+1)/2
   ! where lmn_size is the number of (l,m,n) elements for the paw basis

  integer :: lmnmix_sz
   ! lmnmix_sz=number of klmn=(lmn,lmn_prime) verifying l<=lmix and l_prime<=lmix

  integer :: lpawu
   ! lpawu gives l on which U is applied for a given type of atom.

  integer :: nproju
   ! nproju is the number of projectors for orbitals on which paw+u acts.
   ! Also used for local exact-exchange

  integer :: mesh_size
   ! Dimension of radial mesh

  integer :: mqgrid
   ! Number of points in the reciprocal space grid on which
   ! the radial functions (tcorespl, tvalespl...) are specified
   ! Same as psps%mqgrid_vl

  integer :: shape_lambda
   ! Lambda parameter in gaussian shapefunction (shape_type=2)

  integer :: shape_type
   ! Radial shape function type
   ! shape_type=-1 ; g(r)=numeric (read from psp file)
   ! shape_type= 1 ; g(r)=[sin(pi*r/rshp)/(pi*r/rshp)]**2 if r<=rshp, zero if r>rshp
   ! shape_type= 2 ; g(r)=exp[-(r/sigma)**lambda]
   ! shape_type= 3 ; gl(r)=Alpha(1,l)*jl(q(1,l)*r)+Alpha(2,l)*jl(q(2,l)*r) for each l

  integer :: useexexch
   ! useexexch=0 ; do not use local exact-exchange
   ! useexexch=1 ; use local exact-exchange

  integer :: usepawu
   ! usepawu=0 ; do not use PAW+U formalism
   ! usepawu=1 ; use PAW+U formalism (Full localized limit)
   ! usepawu=2 ; use PAW+U formalism (Around Mean Field)

  integer :: usetcore
   ! Flag controling use of pseudized core density (0 if tncore=zero)

  integer :: usetvale
   ! Flag controling use of pseudized valence density (0 if tnval is unknown)

  integer :: vlocopt
   ! 0 if Vloc in atomic data is Vbare    (Blochl s formulation)
   ! 1 if Vloc in atomic data is VH(tnzc) (Kresse s formulation)

!Real (real(dp)) scalars

  real(dp) :: dncdq0
   ! Gives 1/q d(tNcore(q))/dq for q=0
   ! (tNcore(q) = FT of pseudo core density)

  real(dp) :: dnvdq0
   ! Gives 1/q d(tNvale(q))/dq for q=0
   ! (tNvale(q) = FT of pseudo valence density)

  real(dp) :: exccore
   ! Exchange-correlation energy for the core density

  real(dp) :: exchmix
   ! mixing of exact exchange; default is 0.25 (PBE0)
   
  real(dp) :: jpawu
   ! jpawu
   ! Value of J parameter for paw+u for a given type.

  integer :: mqgrid_shp
   ! Number of points in the reciprocal space grid on which
   ! the radial shape functions (shapefncg) are given

  real(dp) :: rpaw
   ! Radius of PAW sphere

  real(dp) :: rshp
   ! Compensation charge radius (if r>rshp, g(r)=zero)

  real(dp) :: shape_sigma
   ! Sigma parameter in gaussian shapefunction (shape_type=2)

  real(dp) :: upawu
   ! upawu
   ! Value of U parameter for paw+u for a given type.


!Integer arrays

  integer, pointer :: indklmn(:,:)
   ! indklmn(6,lmn2_size)
   ! Array giving klm, kln, abs(il-jl), (il+jl), ilm and jlm for each klmn=(ilmn,jlmn)
   ! Note: ilmn=(il,im,in) and ilmn<=jlmn

  integer, pointer :: klmntomn(:,:)
   ! klmntomn(4,lmn2_size)
   ! Array giving im, jm ,in, and jn for each klmn=(ilmn,jlmn)
   ! Note: ilmn=(il,im,in) and ilmn<=jlmn
   ! NB: klmntomn is an application and not a bijection

  integer, pointer :: kmix(:)
   ! kmix(lmnmix_sz)
   ! Indirect array selecting the klmn=(lmn,lmn_prime) verifying l<=lmix and l_prime<=lmix

  integer, pointer :: lnproju(:)
   ! lnproju(nproju) gives ln (index for phi) for each projectors on which U acts (PAW+U only)
   ! nproju is 1 or 2 and  is the number of projectors for correlated orbitals
   ! Also used for local exact-exchange

!Real (real(dp)) arrays

  real(dp), pointer :: coredens(:)
   ! coredens(mesh_size)
   ! Gives the core density of the atom

  real(dp), pointer :: dij0(:)
   ! dij0(lmn2_size)
   ! Part of the Dij term (non-local operator) completely
   ! calculated in the atomic data part

  real(dp), pointer :: dltij(:)
   ! dltij(lmn2_size)
   ! Factor used to compute sums over klmn=(ilmn,jlmn)
   ! ((ilmn,ilmn) term has to be added once)
   ! dltij(klmn)=1 if ilmn=jlmn, else dltij(klmn)=2

  real(dp), pointer :: dshpfunc(:,:,:)
   ! shapefunc(mesh_size,l_size,4)
   ! Gives the 4 first derivatives of  radial shape function
   ! for each l component; used only if shape_type=-1

  real(dp), pointer :: eijkl(:,:)
   ! eijkl(lmn2_size,lmn2_size)
   ! Part of the Dij term (non-local operator) that depends only from
   ! the projected occupation coeffs in the self-consistent loop

 real(dp), pointer :: fk(:,:)
   ! fk(6, 4)
   ! Slater integrals used for local exact exchange

  real(dp), pointer :: gnorm(:)
   ! gnorm(l_size)
   ! Give the the normalization factor of each radial shape function

  real(dp),pointer :: nabla_ij(:,:,:)
   ! Onsite matrix elements <phi|\nabla|phj>-<tphi|\nabla|tphj>

  real(dp), pointer :: phi(:,:)
   ! phi(mesh_size, basis_size)
   ! Gives, on the radial grid, the paw all electron wavefunctions

  real(dp), pointer :: phiphj(:,:)
   ! phiphj(mesh_size,ij_size)
   ! Useful product Phi(:,i)*Phi(:,j)

  real(dp), pointer :: phiphjint(:)
   ! phiphjint(ij_proj)
   ! Integration of Phi(:,i)*Phi(:,j) for LDA+U/local exact-exchange occupation matrix

  real(dp), pointer :: qgrid_shp(:)
   ! qgrid_shp(mqgrid_shp)
   ! Grid of points in reciprocal space on which the shape functions are given

  real(dp), pointer :: qijl(:,:)
   ! qijl(l_size**2,lmn2_size)
   ! The qijl are the moments of the charge density difference between
   ! the AE and PS partial wave for each channel (i,j). They take part
   ! to the building of the compensation charge

  real(dp), pointer :: rad_for_spline(:)
   ! rad_for_spline(mesh_size)
   ! Radial mesh used to spline quantities on radial mesh;
   ! Allocated and used only when
   !     shape_type=-1 (numerical shape function)
   !  or usedvloc=1 (use of vloc derivative)

  real(dp), pointer :: rhoij0(:)
   ! rhoij0(lmn2_size)
   ! Initial guess for rhoij

  real(dp), pointer :: shape_alpha(:,:)
   ! shape_alpha(2,l_size)
   ! Alpha_i parameters in Bessel shapefunctions (shape_type=3)

  real(dp), pointer :: shape_q(:,:)
   ! shape_q(2,l_size)
   ! Q_i parameters in Bessel shapefunctions (shape_type=3)

  real(dp), pointer :: shapefunc(:,:)
   ! shapefunc(mesh_size,l_size)
   ! Gives the normalized radial shape function for each l component

  real(dp), pointer :: shapefncg(:,:,:)
   ! shapefncg(mqgrid_shp,2,l_size)
   ! Gives the spherical Fourier transform of the radial shape function
   ! for each l component (for each qgrid_shp(i)) + second derivative

  real(dp), pointer :: sij(:)
   ! sij(lmn2_size)
   ! Nonlocal part of the overlap operator

  real(dp), pointer :: tcoredens(:)
   ! tcoredens(mesh_size)
   ! Gives the pseudo core density of the atom

  real(dp), pointer :: tcorespl(:,:)
   ! tcorespl(mqgrid,2)
   ! Gives the pseudo core density in reciprocal space on a regular grid

  real(dp), pointer :: tphi(:,:)
   ! tphi(mesh_size,basis_size)
   ! Gives, on the radial grid, the paw atomic pseudowavefunctions

  real(dp), pointer :: tphitphj(:,:)
   ! tphitphj(mesh_size,ij_size)
   ! Useful product tPhi(:,i)*tPhi(:,j)

  real(dp), pointer :: tvalespl(:,:)
   ! tvalespl(mqgrid,2)
   ! Gives the pseudo valence density in reciprocal space on a regular grid

  real(dp), pointer :: Vee(:,:,:,:)
   ! PAW+U:
   ! Screened interaction matrix deduced from U and J parameters
   ! computed on the basis of orbitals on which U acts.

  real(dp), pointer :: Vex(:,:,:,:,:)
   ! Local exact-exchange:
   ! Screened interaction matrix deduced from calculation of Slater integrals
   ! computed on the basis of orbitals on which local exact exchange acts.

 end type pawtab_type

!!***

!-------------------------------------------------------------------------

!!****t* defs_datatypes/paw_an_type
!! NAME
!! paw_an_type
!!
!! FUNCTION
!! For PAW, various arrays given on ANgular mesh or ANgular moments
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type paw_an_type

!Integer scalars

  integer :: angl_size
   ! Dimension of paw angular mesh (angl_size=ntheta*nphi)

  integer :: cplex
   ! cplex=1 if potentials/densities are real, 2 if they are complex

  integer :: has_vhartree
   ! set to 1 if vh1 and vht1 are allocated and used
   !        2 if they are already computed

  integer :: has_vxc
   ! set to 1 if vxc1 and vxct1 are allocated and used
   !        2 if they are already computed

  integer :: has_vxcval
   ! set to 1 if vxc1_val and vxct1_val are allocated and used
   !        2 if they are already computed

  integer :: lm_size
   ! lm_size=(l_size)**2
   ! l is Maximum value of l+1 leading to non zero Gaunt coeffs (l_size=2*l_max+1)

  integer :: mesh_size
   ! Dimension of radial mesh

  integer :: nspden
   ! Number of spin-density components

!Logical arrays

  logical, pointer :: lmselect(:)
   ! lmselect(lm_size)
   ! lmselect(ilm)=select the non-zero LM-moments of "one-center" densities/potentials

!Real (real(dp)) arrays

  real(dp), pointer :: vh1 (:,:,:)
   ! vh1(cplex*mesh_size,lm_size,nspden)
   ! Gives Hartree potential LM-moments inside the sphere

  real(dp), pointer :: vht1 (:,:,:)
   ! vht1(cplex*mesh_size,lm_size,nspden)
   ! Gives Hartree tilde potential LM-moments inside the sphere

  real(dp), pointer :: vxc1 (:,:,:)
   ! vxc1(cplex*mesh_size,lm_size or angl_size,nspden)
   ! Gives xc potential inside the sphere
   !   (theta,phi) values of potential if pawxcdev=0
   !   LM-moments of potential if pawxcdev/=0

  real(dp), pointer :: vxc1_val (:,:,:)
   ! vxc1_val(cplex*mesh_size,lm_size or angl_size,nspden) (Usually real, Mainly used for GW)
   ! Gives xc potential inside the sphere arising from valence only electrons
   !   (theta,phi) values of potential if pawxcdev=0
   !   LM-moments of potential if pawxcdev/=0

  real(dp), pointer :: vxct1 (:,:,:)
   ! vxct1(cplex*mesh_size,angl_size,nspden)
   ! Gives xc tilde potential inside the sphere
   !   (theta,phi) values of potential if pawxcdev=0
   !   LM-moments of potential if pawxcdev/=0

  real(dp), pointer :: vxct1_val (:,:,:)
   ! vxct1_val(cplex*mesh_size,angl_size,nspden) (Usually real, Mainly used for GW)
   ! Gives xc tilde potential inside the sphere
   !   (theta,phi) values of potential if pawxcdev=0
   !   LM-moments of potential if pawxcdev/=0

  real(dp), pointer :: vxc_ex (:,:,:)
   ! vxc_ex(cplex*mesh_size,angl_size,nspden)
   ! Gives xc  potential for local exact exchange inside the sphere
   !   (theta,phi) values of potential if pawxcdev=0
   !   LM-moments of potential if pawxcdev/=0

 end type paw_an_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/gipaw_type
!! NAME
!! gipaw_type
!!
!! FUNCTION
!! For GIPAW, augmentation fields due to diamagnetic (scalar field) and
!! paramagnetic (vector field) terms, and on-site angular moment
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type gipaw_type

!Real (real(dp)) arrays

  real(dp), pointer :: dia(:,:)
   ! dia(ngfd,lmn2_size)
   ! scalar field at each point on the fine grid around an atom, for
   ! each ij pair of basis functions: <phi_i|r'><r'|phi_j>-<tphi_i|r'><r'|tphi_j>

  real(dp), pointer :: para(:,:,:)
   ! para(idir,ngfd,lmn2_size)
   ! vector field with components idir =1..3 at each point on the fine grid
   ! around an atom, for each ij pair of basis functions
   ! note that the actual paramagnetic augmentation vector field is (-i)
   ! times what is stored here; in order to save memory we only store this
   ! part and then multiply by -i when the components are used.
   ! 0.5*(<phi_i|p|r'><r'|phi_j>+<phi_i|r'><r'|p|phi_j>) -
   !  0.5*(<tphi_i|p|r'><r'|tphi_j>+<tphi_i|r'><r'|p|tphi_j>)

  real(dp), pointer :: onsiteangmom(:,:,:)
   ! onsiteangmom(2,idir,lmn2_size)
   ! on-site expectation values of angular momentum (complex quantities)
   ! <phi_j|L_idir|phi_i> - <tphi_j|L_idir|tphi_i>

 end type gipaw_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/paw_ij_type
!! NAME
!! paw_ij_type
!!
!! FUNCTION
!! For PAW, various arrays given on (i,j) (partial waves) channels
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type paw_ij_type

!Integer scalars

  integer :: cplex
   ! cplex=1 if all on-site PAW quantities are real, 2 if they are complex

  integer :: cplex_dij
   ! cplex=1 if dij are real, 2 if they are complex

  integer :: has_dijhartree
   ! 1 if dijhartree is allocated
   ! 2 if dijhartree is already computed

  integer :: has_dijhat
   ! 1 if dijhat is allocated
   ! 2 if dijhat is already computed

  integer :: has_dijso
   ! 1 if dijso is associated and used, 0 otherwise
   ! 2 if dijso is already computed

  integer :: has_dijU
   ! 1 if dijU is associated and used, 0 otherwise
   ! 2 if dijU is already computed

  integer :: has_dijxc
   ! 1 if dijxc is associated and used, 0 otherwise
   ! 2 if dijxc is already computed

  integer :: has_dijxc_val
   ! 1 if dijxc_val is associated and used, 0 otherwise
   ! 2 if dijxc_val is already computed

  integer :: lmn_size
   ! Number of (l,m,n) elements for the paw basis

  integer :: lmn2_size
   ! lmn2_size=lmn_size*(lmn_size+1)/2
   ! where lmn_size is the number of (l,m,n) elements for the paw basis

  integer :: ndij
   ! Number of components of dij
   ! Usually ndij=nspden, except for spin-orbit (where ndij=nspinor**2)

  integer :: nspden
   ! Number of spin-density components (may be different from dtset%nspden if spin-orbit)

  integer :: nsppol
   ! Number of independant spin-components

!Real (real(dp)) arrays

  real(dp), pointer :: dij(:,:)
   ! dij(cplex_dij*lmn2_size,ndij)
   ! Dij term (non-local operator)
   ! May be complex if cplex_dij=2
   !  dij(:,:,1) contains Dij^up-up
   !  dij(:,:,2) contains Dij^dn-dn
   !  dij(:,:,3) contains Dij^up-dn (only if nspinor=2)
   !  dij(:,:,4) contains Dij^dn-up (only if nspinor=2)

  real(dp), pointer :: dijhartree(:)
   ! dijhartree(cplex*lmn2_size)
   ! Dij_hartree term
   ! Contains all contributions to Dij from hartree
   ! Warning: Dimensioned by cplex, not cplex_dij
   ! Same storage as Dij (see above)

  real(dp), pointer :: dijhat(:,:)
   ! dijhat(cplex_dij*lmn2_size,ndij) 
   ! Dij_hat term (non-local operator) i.e \sum_LM \int_FFT Q_{ij}^{LM} vtrial
   ! Same storage as Dij (see above)

  real(dp), pointer :: dijU(:,:)
   ! dijU(cplex_dij*lmn2_size,ndij)
   ! Onsite matrix elements of the U part of the PAW Hamiltonian.
   ! Same storage as Dij (see above)

  real(dp), pointer :: dijso(:,:)
   ! dijso(cplex_dij*lmn2_size,ndij)
   ! Onsite matrix elements of L.S i.e <phi_i|L.S|phi_j>
   ! Same storage as Dij (see above)

  real(dp), pointer :: dijxc(:,:)
   ! dijxc(cplex_dij*lmn2_size,ndij)
   ! Onsite matrix elements of vxc i.e
   ! <phi_i|vxc[n1+nc]|phi_j> - <tphi_i|vxc(tn1+nhat+tnc]|tphi_j>
   ! Same storage as Dij (see above)

  real(dp), pointer :: dijxc_val(:,:)
   ! dijxc_val(cplex_dij*lmn2_size,ndij)
   ! Onsite matrix elements of valence-only vxc i.e
   ! <phi_i|vxc[n1]|phi_j> - <tphi_i|vxc(tn1+nhat]|tphi_j>
   ! Same storage as Dij (see above)

  real(dp), pointer :: noccmmp(:,:,:)
   ! noccmmp(2*lpawu+1,2*lpawu+1,nocc_nspden)
   ! gives occupation matrix for lda+u (computed in setnoccmmp)
   ! Stored as: noccmmp(:,:,1)=   n^{up,up}_{m,mp}
   !            noccmmp(:,:,2)=   n^{dn,dn}_{m,mp}
   !            noccmmp(:,:,3)=Re[n^{up,dn}_{m,mp}]
   !            noccmmp(:,:,4)=Im[n^{up,dn}_{m,mp}]

  real(dp), pointer :: nocctot(:)
   ! nocctot(nspden)
   ! gives trace of occupation matrix for lda+u (computed in pawdenpot)
   ! for each value of ispden (1 or 2)

  real(dp), pointer :: vpawx(:,:,:)
   ! vpawx(2*lexexch+1,2*lexexch+1,nspden)
   ! exact exchange potential

 end type paw_ij_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pawrhoij_type
!! NAME
!! pawrhoij_type
!!
!! FUNCTION
!! For PAW, rhoij quantities and related data
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pawrhoij_type

!Integer scalars

  integer :: cplex
   ! cplex=1 if rhoij are real, 2 if rhoij are complex

  integer :: lmn_size
   ! Number of (l,m,n) elements for the paw basis

  integer :: lmn2_size
   ! lmn2_size=lmn_size*(lmn_size+1)/2
   ! where lmn_size is the number of (l,m,n) elements for the paw basis

  integer :: lmnmix_sz
   ! lmnmix_sz=number of (lmn,lmn_prime) verifying l<=lmix and l_prime<=lmix
   !           i.e. number of rhoij elements being mixed during SCF cycle
   ! lmnmix_sz=0 if mixing data are note used

  integer :: ngrhoij
   ! First dimension of array grhoij

  integer :: nrhoijsel
   ! nrhoijsel
   ! Number of non-zero value of rhoij
   ! This is the size of rhoijp(:,:) (see below in this datastructure)

  integer :: nspden
   ! Number of spin-density components for rhoij (may be different from nspden for density)

  integer :: nsppol
   ! Number of independant spin-components

  integer :: use_rhoij_
   ! 1 if pawrhoij%rhoij_ is allocated

  integer :: use_rhoijres
   ! 1 if pawrhoij%rhoijres is allocated

!Integer arrays

  integer, pointer :: kpawmix(:)
   ! kpawmix(lmnmix_sz)
   ! Indirect array selecting the elements of rhoij
   ! being mixed during SCF cycle

  integer, pointer :: rhoijselect(:)
   ! rhoijselect(lmn2_size)
   ! Indirect array selecting the non-zero elements of rhoij:
   ! rhoijselect(isel,ispden)=klmn if rhoij(klmn,ispden) is non-zero

!Real (real(dp)) arrays

  real(dp), pointer :: grhoij (:,:,:)
   ! grhoij(ngrhoij,cplex*lmn2_size,nspden)
   ! Gradients of Rho_ij wrt xred, strains, ... (non-packed storage)

  real(dp), pointer :: rhoij_ (:,:)
   ! rhoij_(cplex*lmn2_size,nspden)
   ! Array used to (temporary) store Rho_ij in a non-packed storage mode

  real(dp), pointer :: rhoijp (:,:)
   ! rhoijp(cplex*lmn2_size,nspden)
   ! Augmentation waves occupancies Rho_ij
   ! in PACKED STORAGE (only non-zero elements are stored)

  real(dp), pointer :: rhoijres (:,:)
   ! rhoijres(cplex*lmn2_size,nspden)
   ! Rho_ij residuals during SCF cycle (non-packed storage)

 end type pawrhoij_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/cprj_type
!! NAME
!! cprj_type
!!
!! FUNCTION
!! <p_lmn|Cnk> projected scalars and derivatives
!!             where |p_lmn> are non-local projectors for a given atom
!!                   |Cnk> is a wave function
!! Used only when useylm=1
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type cprj_type

!Integer scalars

  integer :: ncpgr
   ! Number of gradients of cp=<p_lmn|Cnk>

  integer :: nlmn
   ! Number of (l,m,n) non-local projectors

!Real (real(dp)) arrays

  real(dp), pointer :: cp (:,:)
   ! cp(2,nlmn)
   ! <p_lmn|Cnk> projected scalars for a given atom and wave function

  real(dp), pointer :: dcp (:,:,:)
   ! dcp(2,ncpgr,nlmn)
   ! derivatives of <p_lmn|Cnk> projected scalars for a given atom and wave function

 end type cprj_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pseudopotential_gth_type
!! NAME
!! pseudopotential_gth_type
!!
!! FUNCTION
!! This structure is a sub-structure of pseudopotential_type used to
!! store parameters from the GTH pseudo-potentials. All arrays have
!! indices running on 1:npsp for each read pseudo-file. The 'set' array
!! is a check array, since several different pseudo can be used in a simulation
!! it set a flag for each npsp if params have been set or not. This is
!! redundant with psps%pspcod in the way that when psps%pspcod(i) is 2,
!! then gth_params%set(i) is .true.. GTH pseudo previous to wavelets introduction
!! doesn't have geometric informations. These have been added on the last line.
!! It is three radius informations, the %hasGeometry flag is there to know
!! which kind of pseudo has been read.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pseudopotential_gth_type

  ! These are {rloc, C(1...4)} coefficients for psppar(0, :, :) indices,
  ! Followed by the h coefficients for psppar(1:2, :, :) indices.
  !  size (0:2, 0:4, npsp)
  real(dp), pointer :: psppar(:, :, :)
  ! The covalence radii for each pseudo (?)
  !  size (npsp)
  real(dp), pointer :: radii_cov(:)
  ! Cut-off radii for core part and long-range part.
  ! radii_cf(:, 1) is for the long-range cut-off and
  ! radii_cf(:, 2) is for the core cut-off.
  !  size (npsp, 2)
  real(dp), pointer :: radii_cf(:, :)
  ! The semicore code, indicated as an integer.
  ! The integer is the n_s + 4*n_p + 16* n_d + 64* n_f
  ! where n_l are the number of semicore orbitals for a given angular momentum
  ! starting from the lower level of course
  integer, pointer :: semicore(:)

  ! Spin orbit coefficients in HGH/GTH formats: k11p etc... see psp3ini.F90
  !   dimension = num l channels, 3 coeffs, num psp = (1:lmax+1,1:3,npsp)
  real(dp), pointer :: psp_k_par(:, :, :)

  ! Flag for geometric informations in the pseudo
  !  size (npsp)
  logical, pointer :: hasGeometry(:)

  ! Consistency array, used for checking
  !  size (npsp)
  logical, pointer :: set(:)

 end type pseudopotential_gth_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pseudopotential_type
!! NAME
!! pseudopotential_type
!!
!! FUNCTION
!! This structured datatype contains all the information about one
!! norm-conserving pseudopotential, including the description of the local
!! and non-local parts, the different projectors, the non-linear core
!! correction ...
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pseudopotential_type

! Integer scalars

  integer :: dimekb
   ! Dimension of Ekb
   ! ->Norm conserving : Max. number of Kleinman-Bylander energies
   !                     for each atom type
   !                     dimekb=lnmax (lnmax: see this file)
   ! ->PAW : Max. number of Dij coefficients connecting projectors
   !                     for each atom type
   !                     dimekb=lmnmax*(lmnmax+1)/2 (lmnmax: see this file)

  integer :: lmnmax
   !  If useylm=0, max number of (l,m,n) comp. over all type of psps (lnproj)
   !  If useylm=1, max number of (l,n)   comp. over all type of psps (lmnproj)
   !  If mpspso is 2, lmnmax takes into account the spin-orbit projectors,
   !  so, it is equal to the max of lmnprojso or lnprojso, see pspheader_type

  integer :: lnmax
   !  Max. number of (l,n) components over all type of psps
   !  If mpspso is 2, lmnmax takes into account the spin-orbit projectors,
   !  so, it is equal to the max of lnprojso, see pspheader_type

  integer :: mproj    ! TO BE SUPPRESSED
   ! Maximum number of non-local projectors over all angular momenta
   !  and type of psps
   ! 0 only if all psps are local

  integer :: mpsang
   ! Highest angular momentum of non-local projectors over all type of psps.
   ! shifted by 1 : for all local psps, mpsang=0; for largest s, mpsang=1,
   ! for largest p, mpsang=2; for largest d, mpsang=3; for largest f, mpsang=4
   ! This gives also the number of non-local "channels"

  integer :: mpspso
   ! mpspso is set to 1 if none of the psps is used with a spin-orbit part (that
   !  is, if the user input variable so_psp is not equal
   !  to 1 in at least one case
   ! otherwise, it is set to 2

  integer :: mpssoang
   ! Maximum number of channels, including those for treating the spin-orbit coupling
   ! when mpspso=1, mpssoang=mpsang
   ! when mpspso=2, mpssoang=2*mpsang-1

  integer :: mqgrid_ff
   ! Number of points in the reciprocal space grid on which
   ! the radial functions ffspl are specified

  integer :: mqgrid_vl
   ! Number of points in the reciprocal space grid on which
   ! the radial functions vlspl are specified

  integer :: mtypalch
   ! Maximum number of alchemical pseudo atoms. If non-zero,
   ! the mechanism to generate mixing of pseudopotentials is activated

  integer :: npsp
   ! Number of types of pseudopotentials

  integer :: npspalch
   ! Number of types of pseudopotentials use for alchemical purposes

  integer :: ntypat
   ! Number of types of atoms (might be alchemy wrt pseudopotentials)

  integer :: ntypalch
   ! Number of types of alchemical pseudoatoms

  integer :: ntyppure
   ! Number of types of pure pseudoatoms

  integer :: n1xccc
   ! Number of radial points for the description of the pseudo-core charge
   ! (in the framework of the non-linear XC core correction)

  integer :: optnlxccc
   ! Option for the choice of non-linear XC core correction treatment (see the input variable)

  integer :: positron
   ! Option for the choice of type of GS calculation (electron or positron)

  integer :: usepaw
   ! if usepaw=0 , use norm-conserving psps part of the code
   ! is usepaw=1 , use paw part of the code

  integer :: useylm
   ! governs the way the nonlocal operator is to be applied:
   !   1=using Ylm, 0=using Legendre polynomials

! Logical scalars

  logical :: vlspl_recipSpace
   ! governs if vlspl is compute in reciprocal space or in real
   ! space (when available).

! Integer arrays

  integer, pointer :: algalch(:)   ! algalch(ntypalch)
   ! For each type of pseudo atom, the algorithm to mix the pseudopotentials

  integer, pointer :: indlmn(:,:,:)
   ! indlmn(6,lmnmax,ntypat)
   ! For each type of psp,
   ! array giving l,m,n,lm,ln,spin for i=ln  (if useylm=0)
   !                                or i=lmn (if useylm=1)

  integer, pointer :: pspdat(:)
   ! pspdat(ntypat)
   ! For each type of psp, the date of psp generation, as given by the psp file

  integer, pointer :: pspcod(:)
   ! pspcod(npsp)
   ! For each type of psp, the format -or code- of psp generation,
   !  as given by the psp file

  integer, pointer :: pspso(:)
   ! pspso(ntypat)
   ! For each type of psp, 1 if no spin-orbit component is taken
   ! into account, 2 if a spin-orbit component is used

  integer, pointer :: pspxc(:)
   ! pspxc(ntypat)
   ! For each type of psp, the XC functional that was used to generate it,
   ! as given by the psp file

! Real (real(dp)) arrays

  real(dp), pointer :: ekb(:,:)
   ! ekb(dimekb,ntypat*(1-usepaw))
   !  ->NORM-CONSERVING PSPS ONLY:
   !    (Real) Kleinman-Bylander energies (hartree)
   !           for number of basis functions (l,n) (lnmax)
   !           and number of atom types (ntypat)
   ! NOTE (MT) : ekb (norm-conserving) is now diagonal (one dimension
   !             lnmax); it would be easy to give it a second
   !             (symmetric) dimension by putting
   !             dimekb=lnmax*(lnmax+1)/2
   !             in the place of dimekb=lmnmax.

  real(dp), pointer :: ffspl(:,:,:,:)
   ! ffspl(mqgrid_ff,2,lnmax,ntypat)
   ! Gives, on the radial grid, the different non-local projectors,
   ! in both the norm-conserving case, and the PAW case

  real(dp), pointer :: mixalch(:,:)
   ! mixalch(npspalch,ntypalch)
   ! Mixing coefficients to generate alchemical pseudo atoms

  real(dp), pointer :: qgrid_ff(:)
   ! qgrid_ff(mqgrid_ff)
   ! The coordinates of all the points of the radial grid for the nl form factors

  real(dp), pointer :: qgrid_vl(:)
   ! qgrid_vl(mqgrid_vl)
   ! The coordinates of all the points of the radial grid for the local part of psp

  real(dp), pointer :: vlspl(:,:,:)
   ! vlspl(mqgrid_vl,2,ntypat)
   ! Gives, on the radial grid, the local part of each type of psp.

  real(dp), pointer :: dvlspl(:,:,:)
   ! dvlspl(mqgrid_vl,2,ntypat)
   ! Gives, on the radial grid, the first derivative of the local
   ! part of each type of psp (computed when the flag 'vlspl_recipSpace'
   ! is true).

  real(dp), pointer :: xcccrc(:)
   ! xcccrc(ntypat)
   ! Gives the maximum radius of the pseudo-core charge, for each type of psp.

  real(dp), pointer :: xccc1d(:,:,:)
   ! xccc1d(n1xccc*(1-usepaw),6,ntypat)
   ! Norm-conserving psps only
   ! The component xccc1d(n1xccc,1,ntypat) is the pseudo-core charge
   ! for each type of atom, on the radial grid. The components
   ! xccc1d(n1xccc,ideriv,ntypat) give the ideriv-th derivative of the
   ! pseudo-core charge with respect to the radial distance.

  real(dp), pointer :: zionpsp(:)
   ! zionpsp(npsp)
   ! For each pseudopotential, the ionic pseudo-charge
   ! (giving raise to a long-range coulomb potential)

  real(dp), pointer :: ziontypat(:)
   ! ziontypat(ntypat)
   !  For each type of atom (might be alchemy wrt psps), the ionic pseudo-charge
   ! (giving raise to a long-range coulomb potential)

  real(dp), pointer :: znuclpsp(:)
   ! znuclpsp(npsp)
   ! The atomic number of each pseudopotential

  real(dp), pointer :: znucltypat(:)
   ! znucltypat(ntypat)
   ! The atomic number of each type of atom (might be alchemy wrt psps)

! Character arrays

  character(len=fnlen), pointer :: filpsp(:)
   ! filpsp(ntypat)
   ! The filename of the pseudopotential

  character(len=fnlen), pointer :: title(:)
   ! title(ntypat)
   ! The content of first line read from the psp file

! Types for pseudo-potentials that are based on parameters. Currently, only
! GTH are supported (see pseudopotential_gth_type). To add one, one should
! create an initialisation method and a destruction method in 02psp (see
! psp2params.F90). These methods are called in driver().
  type(pseudopotential_gth_type) :: gth_params

 end type pseudopotential_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pspheader_paw_type
!! NAME
!! pspheader_paw_type
!!
!! FUNCTION
!! The pspheader_paw_type structured datatype gather additional information
!! about a PAW pseudopotential file, from its header.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pspheader_paw_type
  integer :: basis_size    ! Number of elements of the wf basis ((l,n) quantum numbers)
  integer :: l_size        ! Maximum value of l+1 leading to a non zero Gaunt coefficient
  integer :: lmn_size      ! Number of elements of the paw basis
  integer :: mesh_size     ! Dimension of (main) radial mesh
  integer :: pawver        ! Version number of paw psp format
  integer :: shape_type    ! Type of shape function
  real(dp) :: rpaw         ! Radius for paw spheres
  real(dp) :: rshp         ! Cut-off radius of shape function
 end type pspheader_paw_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/pspheader_type
!! NAME
!! pspheader_type
!!
!! FUNCTION
!! The pspheader_type structured datatype gather different information
!! about a pseudopotential file, from its header.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type pspheader_type
  integer :: nproj(0:3) ! number of scalar projectors for each angular momentum
  integer :: nprojso(3) ! number of spin-orbit projectors for each angular momentum
  integer :: lmax       ! maximum l quantum number (-1 if only local)
                        ! Example : s only       -> lmax=0
                        !           s and p      -> lmax=1
                        !           d only       -> lmax=2
  integer :: pspcod     ! code number of the pseudopotential
  integer :: pspdat     ! date of generation of the pseudopotential
  integer :: pspxc      ! exchange-correlation functional
  integer :: pspso      ! spin-orbit characteristics
  integer :: xccc       ! =0 if no XC core correction, non-zero if XC core correction
  real(dp) :: zionpsp     ! charge of the ion made of core electrons only
  real(dp) :: znuclpsp    ! atomic number of the nuclei
  real(dp) :: GTHradii(0:4) ! Radii values for GTH (and HGH) family potentials
  character(len=fnlen) :: filpsp   ! name of the psp file
  character(len=fnlen) :: title    ! content of first line read from the psp file
  type(pspheader_paw_type) :: pawheader ! only for PAW psps. See above
 end type pspheader_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/rdm_parameters
!! NAME
!! rdm_parameters
!!
!! FUNCTION
!! For the RDM part of ABINIT, the rdm_parameters structured datatype
!! gather different parameters that characterize a RDM calculation.
!!
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type rdm_parameters

  integer :: npwvec                      ! Max between npwe and npwwfn, used to pass the dimension of arrays e.g gvec
  integer :: npwwfn                      ! Number of planewaves for wavefunctions
  integer :: npwx                        ! Number of planewaves for the exchange part
  integer :: npwc                        ! Number of planewaves for $\Sigma_c$ and W
  integer :: nbnds                       ! Number of bands kept in the calculation
  integer :: nkibz                       ! Number of k-points in the IBZ
  integer :: nqibz                       ! Number of q-points in the IBZ
  integer :: nkbz                        ! Number of k-points in the BZ
  integer :: nqbz                        ! Number of q-points in the BZ
  integer :: nsym                        ! Number of symmetry operations
                                         ! (operations related through the inversion symmetry are not considered)
  integer :: nsppol                      ! 1 for unpolarized, 2 for spin-polarized calculations
  integer :: time_reversal               ! 2 if time-reversal symmetry is used, 1 otherwise

  integer :: mG0(3)                      ! For each reduced direction gives the max G0 component to account for umklapp processes

 end type rdm_parameters

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/results_gs_type
!! NAME
!! results_gs_type
!!
!! FUNCTION
!! This structured datatype contains the results of a GS calculation :
!! energy and its decomposition, forces and their decompositions, stresses
!! and their decompositions
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type results_gs_type

! Integer scalar

  integer :: natom
   ! The number of atoms for this dataset

! Real (real(dp)) scalars

! All the energies are in Hartree, obtained "per unit cell".
  type(energies_type) :: energies
!!$  real(dp) :: eei      ! local pseudopotential energy (Hartree)
!!$  real(dp) :: eeig     ! sum of eigenvalue energy (Hartree)
!!$  real(dp) :: eew      ! Ewald energy (Hartree)
!!$  real(dp) :: ehart    ! Hartree part of total energy (Hartree)
!!$  real(dp) :: eii      ! pseudopotential core-core energy
!!$  real(dp) :: ek       ! kinetic energy (Hartree)
!!$  real(dp) :: enefield ! the term of the energy functional that depends
!!$                       ! explicitely on the electric field
!!$                       ! enefield = -ucvol*E*P
!!$  real(dp) :: enl      ! nonlocal pseudopotential energy (Hartree)
  real(dp) :: entropy  ! entropy (Hartree)
!!$  real(dp) :: enxc     ! exchange-correlation energy (Hartree)
!!$  real(dp) :: enxcdc   ! exchange-correlation double-counting energy (Hartree)
!!$  real(dp) :: epaw     ! PAW spherical energy (Hartree)
!!$  real(dp) :: epawdc   ! PAW spherical double-counting energy (Hartree)
  real(dp) :: etotal   ! total energy (Hartree)
                       ! for fixed occupation numbers (occopt==0,1,or 2):
                       !   etotal=ek+ehart+enxc+eei+eew+eii+enl+PAW_spherical_part
                       ! for varying occupation numbers (occopt>=3):
                       !   etotal=ek+ehart+enxc+eei+eew+eii+enl - tsmear*entropy +PAW_spherical_part
  real(dp) :: fermie   ! Fermi energy (Hartree)
  real(dp) :: residm   ! maximum value for the residual over all bands, all k points,
                       !   and all spins (Hartree or Hartree**2, to be checked !)
  real(dp) :: vxcavg   ! Average of the exchange-correlation energy. The average
                       ! of the local psp pot and the Hartree pot is set to zero (due
                       ! to the usual problem at G=0 for Coulombic system, so vxcavg
                       ! is also the average of the local part of the Hamiltonian

! Real (real(dp)) arrays

  real(dp), pointer :: fcart(:,:)
   ! fcart(3,natom)
   ! Cartesian forces (Hartree/Bohr)

  real(dp), pointer :: fred(:,:)
   ! fred(3,natom)
   ! Forces in reduced coordinates (Hartree)
   ! Actually, gradient of the total energy with respect
   ! to change of reduced coordinates

  real(dp), pointer :: gresid(:,:)
   ! gresid(3,natom)
   ! Part of the gradient of the total energy (Hartree) with respect
   ! to change of reduced coordinates, that comes from the residual
   ! of the potential

  real(dp), pointer :: grewtn(:,:)
   ! grewtn(3,natom)
   ! Part of the gradient of the total energy (Hartree) with respect
   ! to change of reduced coordinates, that comes from the Ewald energy

  real(dp), pointer :: grxc(:,:)
   ! grxc(3,natom)
   ! Part of the gradient of the total energy (Hartree) with respect
   ! to change of reduced coordinates, that comes from the XC energy

  real(dp) :: pel(3)
   ! ucvol times the electronic polarization in reduced coordinates

  real(dp) :: strten(6)
   ! Stress tensor in cartesian coordinates (Hartree/Bohr^3)
   ! 6 unique components of this symmetric 3x3 tensor:
   ! Given in order (1,1), (2,2), (3,3), (3,2), (3,1), (2,1).

  real(dp), pointer :: synlgr(:,:)
   ! synlgr(3,natom)
   ! Part of the gradient of the total energy (Hartree) with respect
   ! to change of reduced coordinates, that comes from the non-local energy
   ! The "sy" prefix refer to the fact that this gradient has been
   ! symmetrized.

 end type results_gs_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/results_out_type
!! NAME
!! results_out_type
!!
!! FUNCTION
!! This structured datatype contains a subset of the results of a GS
!! calculation, needed to perform the so-called "internal tests", and
!! to perform the timing analysis
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type results_out_type

! Integer scalar

  integer :: natom ! The number of atoms for this dataset

! Integer arrays

  integer, pointer :: npwtot(:)      ! npw(mxnkpt) Full number of plane waves for each
                                     ! k point, computed with the "true" rprimd
                                     ! Not taking into account the decrease due to istwfk
                                     ! Not taking into account the spread of pws on different procs
! Real (real(dp)) scalars

! All the energies are in Hartree, obtained "per unit cell".
  real(dp) :: etotal  ! total energy (Hartree)

! Real (real(dp)) arrays

  real(dp) :: acell(3),rprim(3,3),rprimd(3,3),strten(6)
  real(dp), pointer :: fcart(:,:) ! fcart(3,natom) Cartesian forces (Hartree/Bohr)
  real(dp), pointer :: fred(:,:)  ! fred(3,natom)
   ! Forces in reduced coordinates (Hartree)
   ! Actually, gradient of the total energy with respect
   ! to change of reduced coordinates
  real(dp), pointer :: occ(:)     ! occ(mxmband_upper*mxnkpt*mxnsppol)
  real(dp), pointer :: vel(:,:)   ! vel(3,natom)
  real(dp), pointer :: xred(:,:)  ! xred(3,natom)

 end type results_out_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/scf_history_type
!! NAME
!! scf_history_type
!!
!! FUNCTION
!! This structured datatype contains various arrays obtained from
!! previous SCF cycles (density, positions...)
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type scf_history_type

! Integer scalar

  integer :: history_size
   ! Number of previous SCF cycles stored in history

  integer :: natom
   ! Number of atoms in cell

  integer :: nfft
   ! Size of FFT grid (for density)

  integer :: nspden
   ! Number of independant spin components for density

! Integer arrays

  integer,pointer :: hindex(:)
   ! hindex(history_size)
   ! Indexes of SCF cycles in the history
   ! hindex(1) is the newest SCF cycle
   ! hindex(history_size) is the oldest SCF cycle

! Real (real(dp)) arrays

   real(dp),pointer :: deltarhor(:,:,:)
    ! deltarhor(nfft,nspden,history_size)
    ! Diference between electronic density (in real space)
    ! and sum of atomic densities at the end of each SCF cycle of history

   real(dp),pointer :: atmrho_last(:)
    ! atmrho_last(nfft)
    ! Sum of atomic densities at the end of the LAST SCF cycle

   real(dp),pointer :: xreddiff(:,:,:)
    ! xreddiff(3,natom,history_size)
    ! Difference of reduced coordinates of atoms between a
    ! SCF cycle and the previous

! Structured datatypes arrays

  type(pawrhoij_type), pointer :: pawrhoij(:,:)
    ! pawrhoij(natom,history_size)
    ! PAW only: occupancies matrix at the end of each SCF cycle of history

 end type scf_history_type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/sigma_parameters
!! NAME
!! sigma_parameters
!!
!! FUNCTION
!! For the GW part of ABINIT, the sigma_parameters structured datatype
!! gather different parameters that characterize the calculation of the matrix
!! elements of the self-energy operator.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type sigma_parameters

  integer :: gwcalctyp                   ! Calculation type
  integer :: minbdgw,maxbdgw             ! Minimum and maximum band index (considering the spin) defining
                                         ! The set of bands where GW corrections are evaluated
  integer :: gwgamma                     ! If 1 include vertex correction (GWGamma)
  integer :: npwvec                      ! Max betwenn npwe and npwwfn, used to pass the dimension of arrays e.g gvec
  integer :: npwwfn                      ! No. of planewaves for wavefunctions
  integer :: npwx                        ! No. of planewaves for $\Sigma_x$
  integer :: npwc                        ! No. of planewaves for $\Sigma_c$ and W
  integer :: nbnds                       ! No. of bands summed over.
  integer :: nomegasr                    ! No. of frequencies on the real axis to evaluate the spectral function
  integer :: nomegasrd                   ! No. of frequencies on the real axis to evaluate $\Sigma(E)$
  integer :: nomegasi                    ! No. of frequencies along the imaginary axis for Sigma in case of AC
  integer :: nsig_ab                     ! No. of components in the self-energy operator (1 if nspinor==1, 4 if nspinor==2)
  integer :: nspinor                     ! No. of spinorial components.
  integer :: nsppol                      ! 1 for unpolarized, 2 for spin-polarized calculation
  integer :: nkcalc                      ! No. of k-points where GW corrections have been calculated
  integer :: ppmodel                     ! Integer defining the plasmon pole model used, 0 for None.
  integer :: symsigma                    ! 0 ==> do not use symmetries to reduce the k-points summed over in sigma
                                         ! 1 ==> take advantage of space group symmetries as well as time-reversal
  integer :: splitsigc                   ! See related input variable

  real(dp) :: soenergy                   ! Scissor energy used in G0

  integer :: mG0(3)                      ! For each reduced direction gives the max G0 component 
                                         ! to account for umklapp processes

  real(dp) :: deltae                     ! Energy step used to evaluate numerically the derivative of the self energy
                                         ! $\frac{\partial \Re \Sigma(E)}{\partial E_o}$
  real(dp) :: maxomega_r                 ! Maximum real frequency for the evaluation of the spectral function
  real(dp) :: maxomega4sd                ! Maximum displacement around the KS energy where evaluate the diagonal
                                         ! Elements of $ \Sigma(E)$
  real(dp) :: omegasimax                 ! Max omega for Sigma along the imag axis in case of analytic continuation
  real(dp) :: omegasimin                 ! min omega for Sigma along the imag axis in case of analytic continuation
  real(dp) :: zcut                       ! Value of $\delta$ used to avoid the divergences (see related input variable)

  integer,pointer :: kcalc(:)            
  ! kcalc(nkcalc)
  ! For each k-point where GW corrections are calculated, the corresponding index in the BZ.

  integer,pointer :: minbnd(:),maxbnd(:) 
  ! minbnd(nkcalc), maxbnd(nkcalc)
  ! For each k-point at which GW corrections are calculated, the min and Max band index considered
  ! (see also input variable dtset%bdgw).

  real(dp),pointer :: xkcalc(:,:)        
  ! xkcalc(3,nkcalc)
  ! k-points for the GW corrections in reduced coordinates.

  !TODO should be removed, everything should be in Sr%

  complex(dpc),pointer :: omegasi(:)     
  ! omegasi(nomegasi)
  ! Frequencies along the imaginary axis used for the analytical continuation.

  complex(dpc),pointer :: omega_r(:)    
  ! omega_r(nomegasr)
  ! Frequencies used to evaluate the spectral function.

 end type

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/sigma_results
!! NAME
!! sigma_results
!!
!! FUNCTION
!! For the GW part of ABINIT, the sigma_results structured datatype
!! gather the results of a sigma calculation.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type sigma_results

  integer :: b1gw,b2gw      ! min and Max gw band indeces over spin and k-points (used to dimension)
  integer :: gwcalctyp     ! Flag defining the calculation type.
  integer :: nkcalc        ! No. of points calculated
  integer :: nkibz          ! No. of irreducible k-points.
  integer :: nbnds          ! Total number of bands 
  integer :: nomega_r       ! No. of real frequencies for the spectral function.
  integer :: nomega_i       ! No. of frequencies along the imaginary axis.
  integer :: nomega4sd      ! No. of real frequencies to evaluate the derivative of $\Sigma(E)$.
  integer :: nsig_ab        ! 1 if nspinor=1,4 for noncollinear case. 
  integer :: nsppol         ! No. of spin polarizations.
  integer :: usepawu        ! 1 if we are using LDA+U as starting point (only for PAW)

  real(dp) :: deltae       ! Frequency step for the calculation of d\Sigma/dE
  real(dp) :: maxomega4sd  ! Max frequency around E_ks for d\Sigma/dE.
  real(dp) :: maxomega_r   ! Max frequency for spectral function. 
  real(dp) :: scissor_ene  ! Scissor energy value. zero for None.

  integer,pointer :: maxbnd(:) 
  ! maxbnd(nkcalc)
  ! Max band index considered in GW for this k-point.

  integer,pointer :: minbnd(:)
  ! minbnd(nkcalc)
  ! Min band index considered in GW for this k-point.

  !real(dp),pointer :: ame(:,:,:)               
  ! ame(nbnds,nkibz,nomega))
  ! Diagonal matrix elements of the spectral function.
  ! Commented out, it can be calculated from the other quantities

  real(dp),pointer :: degwgap(:,:)             
  ! degwgap(nkibz,nsppol)
  ! Difference btw the QP and the KS optical gap.

  real(dp),pointer :: egwgap(:,:)              
  ! egwgap(nkibz,nsppol))
  ! QP optical gap at each k-point and spin.

  real(dp),pointer :: en_qp_diago(:,:,:)       
  ! en_qp_diago(nbnds,nkibz,nsppol))
  ! QP energies obtained from the diagonalization of the Hermitian approximation to Sigma (QPSCGW)

  real(dp),pointer :: e0(:,:,:)                
  ! e0(nbnds,nkibz,nsppol)
  ! KS eigenvalues for each band, k-point and spin. In case of self-consistent? 

  real(dp),pointer :: e0gap(:,:)               
  ! e0gap(nkibz,nsppol),
  ! KS gap at each k-point, for each spin.

  real(dp),pointer :: omega_r(:)    
  ! omega_r(nomega_r)
  ! real frequencies used for the self energy.

  real(dp),pointer :: xkcalc(:,:) ! TODO this should be replaced by a table (nkibz)
  ! xkcalc(3,nkcalc)       
  ! List of calculated k-points

  real(dp),pointer :: sigxme(:,:,:)            
  ! sigxme(b1gw:b2gw,nkibz,nsppol*nsig_ab))
  ! Diagonal matrix elements of $\Sigma_x$ i.e $\<nks|\Sigma_x|nks\>$

  real(dp),pointer :: vxcme(:,:,:)             
  ! vxcme(b1gw:b2gw,nkibz,nsppol*nsig_ab))
  ! $\<nks|v_{xc}[n_val]|nks\>$ matrix elements of vxc (valence-only contribution).

  real(dp),pointer :: vUme(:,:,:)             
  ! vUme(b1gw:b2gw,nkibz,nsppol*nsig_ab))
  ! $\<nks|v_{U}|nks\>$ for LDA+U.

  complex(dpc),pointer :: degw(:,:,:)         
  ! degw(b1gw:b2gw,nkibz,nsppol))
  ! Difference between the QP and the KS energies.

  complex(dpc),pointer :: dsigmee0(:,:,:)     
  ! dsigmee0(b1gw:b2gw,nkibz,nsppol*nsig_ab))
  ! Derivative of $\Sigma_c(E)$ calculated at the KS eigenvalue.

  complex(dpc),pointer :: egw(:,:,:)          
  ! degw(nbnds,nkibz,nsppol))
  ! QP energies, $\epsilon_{nks}^{QP}$.

  complex(dpc),pointer :: eigvec_qp(:,:,:,:)  
  ! eigvec_qp(nbnds,nbnds,nkibz,nsppol)) 
  ! Expansion of the QP amplitude in the KS basis set.

  complex(dpc),pointer :: hhartree(:,:,:,:)   
  ! hhartree(b1gw:b2gw,b1gw:b2gw,nkibz,nsppol*nsig_ab)
  ! $\<nks|T+v_H+v_{loc}+v_{nl}|mks\>$

  complex(dpc),pointer :: sigcme(:,:,:,:)     
  ! sigcme(b1gw:b2gw,nkibz,nomega_r,nsppol*nsig_ab)) 
  ! $\<nks|\Sigma_{c}(E)|nks\>$ at each nomega_r frequency

  complex(dpc),pointer :: sigmee(:,:,:)       
  ! sigmee(b1gw:b2gw,nkibz,nsppol*nsig_ab))
  ! $\Sigma_{xc}E_{KS} + (E_{QP}- E_{KS})*dSigma/dE_KS

  complex(dpc),pointer :: sigcmee0(:,:,:)     
  ! sigcmee0(b1gw:b2gw,nkibz,nsppol*nsig_ab))
  ! Diagonal mat. elements of $\Sigma_c(E)$ calculated at the KS energy $E_{KS}$

  complex(dpc),pointer :: sigcmesi(:,:,:,:)   
  ! sigcmesi(b1gw:b2gw,nkibz,nomega_i,nsppol*nsig_ab))
  ! Matrix elements of $\Sigma_c$ along the imaginary axis. 
  ! Only used in case of analytical continuation.

  complex(dpc),pointer :: sigcme4sd(:,:,:,:)  
  ! sigcme4sd(b1gw:b2gw,nkibz,nomega4sd,nsppol*nsig_ab))
  ! Diagonal matrix elements of \Sigma_c around the zeroth order eigenvalue (usually KS).

  complex(dpc),pointer :: sigxcme(:,:,:,:)    
  ! sigxme(b1gw:b2gw,nkibz,nomega_r,nsppol*nsig_ab))
  ! $\<nks|\Sigma_{xc}(E)|nks\>$ at each real frequency frequency.

  complex(dpc),pointer :: sigxcmesi(:,:,:,:)  
  ! sigxcmesi(b1gw:b2gw,nkibz,nomega_i,nsppol*nsig_ab))
  ! Matrix elements of $\Sigma_{xc}$ along the imaginary axis.
  ! Only used in case of analytical continuation.

  complex(dpc),pointer :: sigxcme4sd(:,:,:,:) 
  ! sigxcme4sd(b1gw:b2gw,nkibz,nomega4sd,nsppol*nsig_ab))
  ! Diagonal matrix elements of \Sigma_xc for frequencies around the zeroth order eigenvalues.

  complex(dpc),pointer :: ze0(:,:,:)          
  ! ze0(b1gw:b2gw,nkibz,nsppol))
  ! renormalization factor. $(1-\dfrac{\partial\Sigma_c} {\partial E_{KS}})^{-1}$

  complex(dpc),pointer :: omega_i(:)     
  ! omegasi(nomega_i)
  ! Frequencies along the imaginary axis used for the analytical continuation.

  complex(dpc),pointer :: omega4sd(:,:,:,:) 
  ! omega4sd(b1gw:b2gw,nkibz,nomega4sd,nsppol).
  ! Frequencies used to evaluate the Derivative of Sigma.

 end type sigma_results

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/vardims_type
!! NAME
!!  vardims_type
!!
!! FUNCTION
!!  Stores dimensions of dataset variables.
!!
!! SOURCE

 type vardims_type

  integer :: mband,mproj,mpsang,mpw,ngrid1,ngrid2,ngrid3,&
&            ntypat,natom,natsph,nkpt,nkptgw,nshiftk,nsppol,nberry,&
&            nsym,npsp,nconeq,ntypalch,npspalch,nfft,nspden,wfs_dim1,wfs_dim2,&
&            nfreqsus,npw_tiny,nqptdm,norb,ncenter,nspinor

 end type vardims_type

!!***

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

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

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

!----------------------------------------------------------------------

!!****t* defs_datatypes/little_group
!! NAME
!! little_group
!!
!! FUNCTION
!! For the GW part of ABINIT. The little_group structured datatype gather information on
!! the little group associated to an external vector q. The little group associated to q
!! is defined as the subset of the space group that preserves q, modulo a G0 vector
!! (also called umklapp vector). Namely
!!
!!  Sq = q +G0,  where S is an operation in reciprocal space.
!!
!! If time reversal symmetry holds true, it is possible to enlarge the little group by
!! including the operations such as
!!  -Sq = q+ G0.
!!
!! The operations belongin to the little group define an irriducible wedge in the Brillouin zone
!! that is, usually, larger than the irredubile zone defined by the space group.
!! The two zone coincide when q=0
!!
!! TODO
!! Rationalize most of the arrays, in particular the tables
!! This structre shoud be rewritten almost from scratch, thus avoid using it 
!! for your developments.
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type little_group

  integer :: npw             ! No. of planewaves used to describe the wavefuntion, used to dimension igmG0
  integer :: nsym_sg         ! No. of operations in the space group (*NOT* the little group)
  integer :: nsym_ltg        ! No. of symmetry operations in the little group (time-reversal is included, if can be used)
  integer :: timrev          ! 2 if time-reversal is considered, 1 otherwise
  integer :: nbz             ! No. of kpoints in the full BZ
  integer :: nibz_ltg        ! No. of points in the irreducible wedge defined by the little group
  !integer :: use_umklp      ! 1 if umklapp processes are included 

  real(dp) :: max_kin_gmG0
  ! Max kinetic energy of G-G0 in case of umklapp

  integer,pointer :: G0(:,:,:)         
  ! g0(3,timrev,nsym_sg) 
  ! Reduced coordinates of the umklapp G0 vector

  integer,pointer :: ibzq(:)           
  ! ibzq(nbz) 
  ! 1 if the point belongs to the IBZ_q defined by ext_pt, 0 otherwise

  integer,pointer :: bz2ibz(:)         
  ! bz2ibz(nbz) 
  ! Index of the point in the irreducible wedge defined by the little group, 0 otherwise

  integer,pointer :: ibz2bz(:)         
  ! ibz2bz(nibz_ltg)
  ! The correspondind index in the BZ array

  integer,pointer :: igmG0(:,:,:)      
  ! iumklp(npw,timrev,nsym_sg) 
  ! Index of G-G0 in the FFT array for each operations IS (I=\pm 1)

  integer,pointer :: flag_umklp(:,:)   
  ! flag_umklp(timrev,nsym_sg) 
  ! 1 if the operation IS requires a non null G0 vector to preserve q, 0 otherwise

  integer,pointer :: preserve(:,:)     
  ! preserve(timrev,nsym_sg)
  ! preserve(1,S) is 1 if the operation S in rec space preserves the external q-point i.e Sq=q+G0 
  ! preserve(2,S) is 1 if -Sq=q+G0. G0 is a reciprocal lattice vector also called "umklapp vector"

  integer,pointer :: tab(:)
  ! tab(nbz) 
  ! For each point in BZ, the index of the irreducible point (kIBZ_q) in the irreducible 
  ! wedge defined by the little group of q. kBZ= (IS) kIBZ where I is the inversion or the identity

  integer,pointer :: tabo(:)           
  ! tabo(nbz) 
  ! The index of the operation S in the little group that rotates ! kIBZ_q into \pm kBZ

  integer,pointer :: tabi(:)  
  ! tabi(nbz) 
  ! for each k-point in the BZ defines whether inversion has to be
  ! considered in the relation kBZ= IS kIBZ_q (1 => only S; -1 => -S)

  integer,pointer :: wtksym(:,:,:)     
  ! wtksym(timrev,nsym_sg,kbz) 
  ! 1 if IS belongs to the little group, 0 otherwise !(should invert firt dimensions

  real(dp) :: ext_pt(3)
  ! The external point defining the little group

 end type little_group
!!***

!----------------------------------------------------------------------

!!****t* m_numeric_tools/transitions_type
!! NAME
!!  transitions_type
!!
!! FUNCTION
!!
!! INPUTS
!!
!! OUTPUT
!!
!! SOURCE


 type transitions_type
  !TODO replace ntrans with nkibz,nkbz,nsppol
  !add a logical flag for transitions in nbv

  integer :: nbnds                          ! Total number of bands
  integer :: nbvw                           ! Number of valence states allocated on each proc (used only if gwpara==2)
  integer :: nomega                         ! Number of frequencies in num_w
  integer :: nkbz                           ! Number of k-points in the BZ
  integer :: nsppol                         ! 2 for spin polarized, 1 otherwise
  integer :: ntrans                         ! Total number of transitions for this q-point
  integer :: my_ntrans                      ! Number of transitions treated by this processor

  real(dp) :: my_min_res,my_max_res         ! min and Max resonant transition for this processor

  integer,pointer :: bands(:,:)             ! Left and right band index for this transition
  integer,pointer :: distrb(:)              ! equal to me if this processor treats this transition
  integer,pointer :: ik_ibz(:),ikmq_ibz(:)  ! For each ntrans, the index in the IBZ of k and k-q
  integer,pointer :: ik_bz(:),ikmq_bz(:)    ! For each ntrans, the index in the  BZ of k and k-q
  integer,pointer :: G0(:,:)                ! Umklapp vector required to bring k-q back to the first BZ ie kp = k-q-G0
  integer,pointer :: spin(:,:)              ! The spin associated to this transition TODO another dimension?

  real(dp),pointer :: delta_occ(:)          ! occ1-occ2 for this transition
  real(dp),pointer :: qpoint(:)             ! The external q-point of chi0

  complex(dpc),pointer :: delta_ene(:)      ! ene1-ene2 for this transition
  complex(dpc),pointer :: num_w(:,:)        ! The frequency dependent part of chi0 (imaginary part if spectral)

 end type transitions_type
!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/Bands_Symmetries
!! NAME
!!  Bands_Symmetries
!!
!! FUNCTION
!! This dataype gathers information need to analize the symmetries of electronic
!! bands using Group Theory
!!
!! SOURCE

type Bands_Symmetries

 integer :: nspinor                      ! No. of spinorial components.
 integer :: nsppol                       ! No. of independent spin polarizations
 integer :: nbnds                        ! No. of bands for this k-point (same for up and down spin)

 integer :: timrev                       ! 2 if time-reversal is used, 1 otherwise
 integer :: nclass                       ! The number of classes in the small group of k.
 integer :: nsym_sgk                     ! No. of symmetries in the small/little group of k

 real(dp) :: tol_deg                     ! Energy tolerance below which two bands are considered degenerate

 logical :: only_trace                   ! if .TRUE. only the trace of a single matrix per class is calculated
                                         ! this is the standard way used to analyze bands symmetries. If .FALSE.
                                         ! the full matrices of the irreducible representations are calculated and stored
 logical :: has_inversion                ! .TRUE. if the inversion belongs to the space group
 logical :: is_symmorphic                ! if .TRUE. analysis cannot be performed since kpt is
                                         ! at border zone and non-zero fractional translations are present in the space group

 real(dp) :: kpt(3)                      
 ! The k-point under investigation

 integer,pointer :: G0(:,:)              
 ! G0(3,nsym_sgk)
 ! The umklapp G0 vector associated to each little group operation.

 integer,pointer :: nclasses_found(:)
 ! nclasses_found(nsppol)
 ! Number of classes found for each spin 
 ! Needed in the present implementation since I still do not have a lookup table
 ! defining the irred. represetantion for each lattice.

 integer,pointer :: ncplx(:)             
 ! ncplx(nsppol)
 ! Number of degenerate states, for each spin

 integer,pointer :: nelements(:)         
 ! nelements(nclass) 
 ! Number of symmetry operations in each class.

 integer,pointer :: sgk2symrec(:)        
 ! sgk2symrec(nsym_sgk) 
 ! The index of each symmetry in the symrec array. Arrays is packed by classes.

 integer,pointer :: which_irred(:,:)
 ! which_irred(nbnds,nsppol)
 ! Index of the irreducible represenation to which this state belongs to.

 complex(dpc),pointer :: irred_repr(:,:,:)
 ! irred_repr(nclass,MAXVAL(nclasses_found),nsppol)
 ! Irreducible representations at this k-point calculated from the wavefunctions

 type(Degenerate_Bands),pointer :: Cplx(:,:)
 ! Cplx(ncplx_MAX,nsppol)) 

end type Bands_Symmetries
!!***

!!****t* defs_datatypes/Degenerate_Bands
!! NAME
!! Degenerate_Bands 
!!
!! FUNCTION
!!
!! INPUTS
!!
!! OUTPUT
!!
!! SOURCE

type Degenerate_Bands

 integer :: dim_cplx                    ! No of degenerate bands. 
                                        ! It is the dimension of representation if no accidental degeneracy occurs.
 integer :: ib_start,ib_end             ! Starting and final band index for the set of degenerate states

 real(dp),pointer :: ene(:)             
 ! The energies (supposed to be almost generate)

 complex(dpc),pointer :: Rirr(:,:,:)    
 ! Rirr(dim_cplx,dim_cplx,nsym_sgk) 
 ! The irreducible representations.

 complex(dpc),pointer :: trace(:)       
 ! trace(nclass) 
 ! The trace of the irreducible representations.

end type Degenerate_Bands
!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/crystal_structure
!! NAME
!! crystal_structure
!!
!! FUNCTION
!! For the GW part of ABINIT. This structure contains information on the unit cell
!! (geometry, atomic positions and symmetry operations in real and reciprocal space)
!!
!! SOURCE

type Crystal_structure

!scalars
  !integer :: point_group                    ! Point group 
  !integer :: bravais,crystsys               ! Bravais lattice, Crystal system
  !integer :: nptsym                         ! No of point symmetries of the Bravais lattice
  !integer :: bravais(11)                    ! bravais(1)=iholohedry, bravais(2)=center
                                             ! bravais(3:11)=coordinates of rprim in the axes of the conventional 
                                             ! bravais lattice (*2 if center/=0)
  !integer,pointer ptsymrel(:,:,:) 
  !ptsymrel(3,3,nptsym) 
  ! nptsym point-symmetry operations of the Bravais lattice in real space in terms of primitive translations.

  integer :: natom                           ! Number of atoms
  integer :: nsym                            ! Number of symmetry operations
  integer :: ntypat                          ! Number of type of atoms
  !integer :: ntypalch,ntyppure
  integer :: npsp                            ! No. of pseudopotentials
  integer :: space_group                     ! Space group
  integer :: timrev                          ! 2 => take advantage of time-reversal symmetry
                                             ! 1 => do not use time-reversal symmetry
  real(dp) :: ucvol                          ! Unit cell in real space

  logical :: use_antiferro                   ! .TRUE. if AFM symmetries are present and used.
  logical :: has_inversion                   ! .TRUE. if inversion symmetry is present
  logical :: isymmorphic                     ! .TRUE, if all fractional translations are zero

!arrays
  real(dp) :: angdeg(3)                      
  ! Angles among rprim (degree).

  real(dp) :: gmet(3,3)
  ! Reciprocal space metric ($\textrm{bohr}^{-2}$).

  real(dp) :: gprimd(3,3)           
  ! Dimensional primitive translations for reciprocal space ($\textrm{bohr}^{-1}$)

  real(dp) :: rmet(3,3)                      
  ! Metric in real space.

  real(dp) :: rprimd(3,3)                    
  ! Direct lattice vectors, Bohr units.

  integer,pointer :: indsym(:,:,:)           
  ! indsym(4,nsym,natom) 
  ! indirect indexing array for atoms, see symatm.F90.

  integer,pointer :: symafm(:)               
  ! symafm(nsym)
  ! (Anti)Ferromagnetic symmetries.

  integer,pointer :: symrec(:,:,:)           
  ! symrec(3,3,nsym)
  ! Symmetry operation in reciprocal space (reduced coordinates)

  integer,pointer :: symrel(:,:,:)           
  ! symrel(3,3,nsym)
  ! Symmetry operations in direct space (reduced coordinates).

  integer,pointer :: atindx(:),atindx1(:)    
  ! atindx(natom), atindx1(natom)
  ! Index tables for atoms useful to treat atoms type after type.

  integer,pointer :: typat(:),nattyp(:)      
  ! typat(natom), nattyp(ntypat)
  ! Type of each natom and number of atoms of each type.

  real(dp),pointer :: tnons(:,:)             
  ! tnons(3,nsym)
  ! Fractional translations (reduced coordinates)

  real(dp),pointer :: xcart(:,:),xred(:,:)   
  ! xcart(3,natom), xred(3,natom)
  ! Cartesian and Reduced coordinates.

  real(dp),pointer :: spinrot(:,:)
  ! spinrot(4,nsym)
  ! spinor rotation matrices.

! Useful quantities that might be added in the future
  !real(dp),pointer :: amu(:)                ! amu(ntypat)

  real(dp),pointer :: ziontypat(:)     
  ! ziontypat(ntypat)
  ! Charge of the pseudo-ion (No of valence electrons needed to screen exactly the pseudopotential).

  !real(dp),pointer :: znucltypat(:)         ! znucltypat(ntypat) from alchemy

  real(dp),pointer :: znucl(:)              
  ! znucl(npsp)
  ! Nuclear charge for each type of pseudopotential

  character(len=132),pointer :: title(:)  
   ! title(ntypat)
   ! The content of first line read from the psp file

end type Crystal_structure
!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/PPmodel_type
!! NAME
!! PPmodel_type
!!
!! FUNCTION
!!  For the GW part of ABINIT, the PPmodel_type structured datatype
!!  gather information related to the plasmonpole technique
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

type PPmodel_type

!integers
  integer :: dm2_botsq                          ! =npwc if ppmodel=1,2; =1 if ppmodel=3,4
  integer :: dm_eig                             ! =npwc if ppmodel=3;   =0 if ppmodel=1,2,4
  integer :: dm2_otq                            ! =npwc if ppmodel=1,2; =1 if ppmodel=3,4
  integer :: model                              ! The type of Plasmonpole model
  integer :: mqmem                              ! =nqibz if in-core solutions, =0 for out-of-core
                                                ! (In the former case the kast dimension in PPm arrays has size 1)
  integer :: nqibz                              ! Number of q-points in the IBZ
  integer :: npwc                               ! Number of G vectors in $\tilde \epsilon $

  real(dp) :: drude_plsmf                       ! Drude plasma frequency
  !£real(dp) :: zcut
  !£real(dp),pointer :: qibz(:,:)

  !logical :: has_inversion
  !logical :: has_time_reversal

!arrays
  complex(gwpc),pointer :: bigomegatwsq(:,:,:)  
  ! bigomegatwsq(npwc,dm2_botsq,nqibz)
  ! Plasmon pole parameters $\tilde\Omega^2_{G Gp}(q)$.

  complex(gwpc),pointer :: omegatw(:,:,:)       
  ! omegatw(npwc,dm2_otq,nqibz)
  ! Plasmon pole parameters $\tilde\omega_{G Gp}(q)$.

  complex(gwpc),pointer :: eigpot(:,:,:)        
  ! eigpot(dm_eig,dm_eig,nqibz)
  ! Eigvectors of the symmetrized inverse dielectric matrix
 
end type
!!***

!!****t* defs_datatypes/ScrHdr_type
!! NAME
!!  ScrHdr_type
!!
!! FUNCTION
!!
!! INPUTS
!!
!! OUTPUT
!!
!! SOURCE


!----------------------------------------------------------------------

 !FIXME: 
 ! this is just an hack to cheat the build system.
 ! the following data type should be defined in the appropriated module (m_io_screening.F90)

 type ScrHdr_type

  !Other variables that can be added are, for the moment, commented out. 
  !Most of them are related to the Abinit implementation  and are not specified in the ETSF specs.

  !Index of the qlwl section?
  !gwcomp, gwencomp  ! Info on the extrapolar algorithm

  integer :: ID           ! Matrix identifier: O if not yet defined, 1 for chi0, 
                          ! 2 for chi, 3 for epsilon, 4 for espilon^{-1}
  integer :: ikxc         ! Kxc kernel used, 0 for None (RPA), >0 for static TDDFT (=ixc), <0 for frequency-dependent TDDFT 
  integer :: inclvkb      ! q-->0 treatment, 0 for None, 1-2 for transversal gauge, 3 for longitudinal
  integer :: headform     ! format of the SCR header
  integer :: fform        ! File format:
  integer :: gwcalctyp    ! Calculation type (G0W0, G0W, GW ...)
  integer :: nI,nJ        ! Number of spin components (rows,columns) in chi|eps^-1. (1,1) if collinear. 
                          !  The internal representation of the matrix is eps(nI*npwe,nJ*npwe) 
  integer :: nqibz        ! Number of q-points in the IBZ.
  integer :: nqlwl        ! Number of points for the treatment of the long wavelength limit.
  integer :: nomega       ! Total number of frequencies.
  integer :: nbnds_used   ! Number of bands used during the screening calculation (only for info)
  integer :: npwe         ! Number of G vectors reported on the file.
  integer :: npwwfn_used  ! Number of G vectors for wavefunctions used during the screening calculation (only for info)
  integer :: spmeth       ! Method used to approximate the delta function in the expression for Im Chi_0
  integer :: test_type    ! 0 for None, 1 for TEST-PARTICLE, 2 for TEST-ELECTRON (only for TDDFT)
  integer :: tordering    ! 0 if not defined, 1 for Time-Ordered, 2 for Advanced, 3 for Retarded.

  real(dp) :: soenergy    ! Scissor Energy, zero if not used
  real(dp) :: spsmear     ! Smearing of the delta in case of spmeth==2
  real(dp) :: zcut        ! Imaginary shift to avoid the poles along the real axis.

  type(Hdr_type) :: Hdr   ! The abinit header.

!arrays
  character(len=80) :: title(2)
  ! Title describing the content of the file.

  integer,pointer :: gvec(:,:)                 
  ! gvec(3,npwe) 
  ! G vectors in r.l.u.

  real(dp),pointer :: qibz(:,:)
  ! qibz(3,nqibz)
  ! q-points in r.l.u.

  real(dp),pointer :: qlwl(:,:)                 
  ! qlwl(3,nqlwl)
  ! q-points for the long wave-length limit treatment (r.l.u)

  complex(dpc),pointer :: lwing(:,:,:)         
  ! lwing(npwe,nomega,nqlwl)
  ! Lower wings for the different q"s -->0 

  complex(dpc),pointer :: omega(:)             
  ! omega(nomega) 
  ! All frequencies calculated both along the real and the imaginary axis.

  complex(dpc),pointer :: uwing(:,:,:)   
  ! uwing(npwe,nomega,nqlwl)
  ! Upper wings for the different q"s -->0 

 end type ScrHdr_type
!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/epsilonm1_results
!! NAME
!! epsilonm1_results
!!
!! FUNCTION
!! For the GW part of ABINIT, the epsilonm1_results structured datatype
!! gather the results of screening : the inverse dielectric matrix,
!! and the omega matrices .
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type Epsilonm1_results

  integer :: ID                          ! Matrix identifier: O if not yet defined, 1 for chi0, 
                                         ! 2 for chi, 3 for epsilon, 4 for espilon^{-1}
  integer :: ikxc                        ! Kxc kernel used, 0 for None (RPA), >0 for static TDDFT (=ixc), <0 for TDDFT 
  integer :: fform                       ! File format: 1002 for SCR|SUSC files.
  integer :: mqmem                       ! =0 for out-of-core solution, =nqibz if entire matrix is stored in memory.
  integer :: nI,nJ                       ! Number of components (rows,columns) in chi|eps^-1. (1,1) if collinear.
  integer :: nqibz                       ! Number of q-points in the IBZ used.
  integer :: nqlwl                       ! Number of point used for the treatment of the long wave-length limit.
  integer :: nomega                      ! Number of frequencies used.
  integer :: nomega_i                    ! Number of purely imaginary frequencies used.
  integer :: nomega_r                    ! Number of real frequencies used.
  integer :: npwe                        ! Number of G vectors used.
  integer :: test_type                   ! 0 for None, 1 for TEST-PARTICLE, 2 for TEST-ELECTRON (only for TDDFT)
  integer :: Tordering                   ! 0 if not defined, 1 for Time-Ordered, 2 for Advanced, 3 for Retarded.
  
  character(len=fnlen) :: fname          ! Name of the file from which epsm1 is read.

!arrays
  integer,pointer  :: gvec(:,:)                 
  ! gvec(3,npwe) 
  ! G vectors used to describr the two-point function i(reciprocal lattice units)

  real(dp),pointer :: qibz(:,:)
  ! qibz(3,nqibz)
  ! q-points in reduced coordinates

  real(dp),pointer :: qlwl(:,:)                 
  ! qlwl(3,nqlwl)
  ! q-points used for the long wave-length limit treatment.

  complex(gwpc),pointer :: epsm1(:,:,:,:)       
  ! epsm1(npwe,npwe,nomega,nqibz)
  ! Contains the two-point function $\epsilon_{G,Gp}(q,omega)$ in frequency and reciprocal space.

  complex(gwpc),pointer :: lwing(:,:,:)         
  ! lwing(npwe,nomega,nqlwl)
  ! Lower wings for the different q"s -->0 

  complex(dpc),pointer :: omega(:)             
  ! omega(nomega) 
  ! Frequencies used both along the real and the imaginary axis.

  complex(gwpc),pointer :: uwing(:,:,:)   
  ! uwing(npwe,nomega,nqlwl)
  ! Upper wings for the different q"s -->0 

  type(ScrHdr_type) :: Hscr                         
  ! The header reported in the _SCR of _SUSC file. 
  ! This object contains information on the susceptibility or the inverse dielectric matrix
  ! as stored in the external file. These quantities do *NOT* correspond to the quantities
  ! used during the GW calculation since some parameters might differ, actually they might be smaller.
  ! For example, the number of G-vectors used can be smaller than the number of G"s stored on file.

 end type Epsilonm1_results

!!***

!----------------------------------------------------------------------

!!****t* defs_datatypes/WannierData
!! NAME
!! WannierData
!!
!! FUNCTION
!! Object used to store and handle Wannier90 results inside Abinit
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

 type WannierData

  integer :: mband            ! Total number of bands to be processed.
  integer :: mwan             ! Max number of Wannier functions over spin, i.e MAXVAL(nwan) (to dimension arrays).
  integer :: nntot            ! Number of k-point neighbour.
  integer :: nkpt             ! Number of k-points.
  integer :: nsppol           ! Number of independent spin polarizations (presently only nsppol=1 is implemented).
  integer :: WDversion        ! Version of the WannierData file.

  real(dp) :: W90version      ! Wannier90 version.

  character(len=800) :: title

!Arrays
  integer,pointer :: nwan(:)
   ! nwan(nsppol)
   ! Number of wannier functions (read in wannier90.win).

  real(dp),pointer :: eigen(:,:,:)
   ! eigen(mband,nkpt,nsppol)

!TODO convert Everything to Bohr to be consistent with Abinit internal conventions.
! inside the creation method
  real(dp),pointer :: spreadw(:,:)
   ! spreadw(3,nsppol)

  real(dp),pointer :: wann_centres(:,:,:)
   ! wann_centres(3,mwan,nsppol)

  real(dp),pointer :: wann_spreads(:,:)
   ! wann_spreads(mwan,nsppol)

  complex(dp),pointer :: U_matrix(:,:,:,:)
   ! U_matrix(mwan,mwan,nkpt,nsppol)

  complex(dp),pointer :: U_matrix_opt(:,:,:,:)
   ! U_matrix_opt(mband,mwan,nkpt,nsppol) 

  logical,pointer :: band_in(:,:)
   ! band_in(mband,nsppol)
   ! .TRUE. if the band is included in the calculation.   

  logical,pointer :: lwindow(:,:,:)
   ! lwindow(mband,nkpt,nsppol)
   ! Only if disentanglement, .TRUE. if this band at this k-point lies within the outer window

  logical,pointer :: have_disentangled(:)
   ! have_disentangled(nsppol)
   ! Whether a disentanglement has been performed

  character(len=100) :: cut_mode='None'
   ! Wheter the Hamiltonian in real space in the Wannier gauge has to be truncated.

  type(Hdr_type) :: Hdr
   ! The abinit header

  ! ==================================================================
  ! Variable and arrays used to perform the Wannier interpolation
  ! * NB: The quantities below are initialized and defined during the 
  !   interpolation, They are _not_ part of the WAN file.
  ! ==================================================================

  integer :: nrpts
  ! Number of points in the Wigner-Seitz cell

  integer,pointer :: irvec(:,:)
  ! irvec(3,nrpts) 
  ! Lattice vectors in the WS cell in the basis of the lattice vectors 
  ! defining the unit cell

  integer,pointer :: ndegen(:)
  ! ndegen(nrpts)
  ! Degeneracy of each point. It will be weighted using 1/ndegen(ii)

  integer,pointer :: ndimwin(:,:)
  ! ndimwin(nkpt,nsppol)
  ! Number of bands inside outer window at nkpt-th k point

  complex(dpc),pointer :: hamWR(:,:,:,:)
  ! hamWR(mwan,mwan,nrpts,nsppol))
  ! Hamiltonian in k-space (ab-initio grid) in the Wannier gauge.

 end type WannierData

end module defs_datatypes
!!***
