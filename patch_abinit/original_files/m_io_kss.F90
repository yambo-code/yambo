!{\src2tex{textfont=tt}}
!!****m* ABINIT/m_io_kss
!! NAME
!!  m_io_kss
!!
!! FUNCTION
!!  This module contains procedured dealing with the IO of the KSS file.
!!
!! COPYRIGHT
!! Copyright (C) 1999-2013 ABINIT group (MG, GMR, VO, LR, RWG, MM, XG, RShaltaf)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~abinit/COPYING
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~abinit/doc/developers/contributors.txt .
!!
!! PARENTS
!!
!! CHILDREN
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

#include "abi_common.h"

MODULE m_io_kss

 use defs_basis
 use defs_datatypes
 use defs_abitypes
 use defs_wvltypes
 use m_profiling
 use m_wffile
 use m_xmpi
 use m_errors
 use m_timer
 use m_abi_etsf
#ifdef HAVE_TRIO_ETSF_IO
 use etsf_io
#endif

 use m_io_tools,         only : get_unit
 use m_dtset,            only : dtset_copy, dtset_free
 use m_blas,             only : xdotc
 use m_header,           only : hdr_get_nelect_byocc, isknown_headform, hdr_copy, hdr_clean, hdr_comm, hdr_io_etsf, hdr_io
 use m_gsphere,          only : table_gbig2kg, get_kg,  merge_and_sort_kg
 use m_wfs,              only : wfs_descriptor, wfd_ihave_ug, wfd_mybands, wfd_update_bkstab, &
&                               WFD_STORED, WFD_ALLOCATED, wfd_set_mpicomm
 use m_commutator_vkbr,  only : calc_vkb

 implicit none

 private

 public :: testkss             ! Test a KSS file reporting basic quantities and dimensions.
 public :: wfd_read_kss        ! Read energies, wavefunctions and KB form factors from a KSS file.
 public :: write_kss_header    ! Writes the header of the KSS file.
 public :: read_kss_header     ! Read the head of the KSS file.
 public :: write_vkb           ! Writes the KB form factors and derivates on file for a single k-point.
 public :: write_kss_wfgk      ! Write the Gamma-centered wavefunctions and energies on the KSS file for a single k-point.
 public :: k2gamma_centered    ! Convert a set of wavefunctions from the k-centered to the gamma-centered basis set.
 public :: make_gvec_kss       ! Build the list of G-vectors for the KSS file.

CONTAINS  !===========================================================
!!***

!!****f* m_io_kss/testkss
!! NAME
!! testkss
!!
!! FUNCTION
!! Test the KSS type.
!!
!! INPUTS
!!  accesswff=Define the access mode (plain FORTRAN or NETCDF with ETSF-IO).
!!  filkss=Name of the KSS file. "-etsf.nc" will be appended in case of ETSF-IO.
!!  (TODO: remove this kind of a hack, using a module to store units and filenames
!!  Comm=MPI Communicator.
!!
!! OUTPUT
!!  mpsang=1+maximum angular momentum for nonlocal pseudopotential.
!!  nbnds_kss=Number of bands contained in the KSS file
!!  ng_kss=Number of plane waves in KSS file.
!!  nsym_out=Number of symmetries reported in the KSS, warning might differ from the symmetries found by abinit.
!!  Hdr<Hdr_type>=The abinit header.
!!
!! SIDE EFFECTS
!!  gvec_p(3,ng_kss)=
!!   In input : pointer to integers (not associated)
!!   In output: allocated array containing the G vectors reported in the KSS file.
!!  energies_p(nbnds_kss,Hdr%nkpt,Hdr%nsppol)=
!!   In input : pointer to real (not associated)
!!   In output: allocated array with the KS energies.
!!
!! NOTES
!!  Starting version 5.6, KSS files in single precision are not supported anymore.
!!
!! PARENTS
!!      setup_screening,setup_sigma
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine testkss(filkss,accesswff,nsym_out,nbnds_kss,ng_kss,mpsang,gvec_p,energies_p,Hdr,comm)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'testkss'
 use interfaces_14_hidewrite
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: accesswff,comm
 integer,intent(out) :: mpsang,nbnds_kss,ng_kss,nsym_out
 character(len=fnlen),intent(in) :: filkss
 type(Hdr_type),intent(out) :: Hdr
!arrays
 integer,pointer :: gvec_p(:,:)
 real(dp),pointer :: energies_p(:,:,:)

!Local variables-------------------------------
#if defined HAVE_TRIO_ETSF_IO
 type(ETSF_io_low_error) :: Error_data
 type(ETSF_electrons),target :: Electrons_folder
 logical :: lstat
 real(dp),allocatable,target :: eigen(:)
#endif
!scalars
 integer :: iatom,iband,ierr,ikpt,il,ispinor,isppol,itypat,master
 integer :: my_rank,kss_unt,nprocs,my_prtvol
 real(dp) :: nelect
 character(len=500) :: msg
 character(len=fnlen) :: fname
!arrays
 real(dp),allocatable :: tmp_enek(:)

! *************************************************************************

 DBG_ENTER("COLL")
 ABI_TIMER_START("")

 if (accesswff==IO_MODE_ETSF) then
   write(msg,'(3a)')&
&   ' when accesswff==3, support for the ETSF I/O library ',ch10,&
&   ' must be compiled. Use --enable-etsf-io when configuring '
#if !defined HAVE_TRIO_ETSF_IO
   MSG_ERROR(msg)
#endif
 end if

 ! === MPI info ===
 my_rank = xcomm_rank(comm)
 nprocs  = xcomm_size(comm)
 master=0

 if (my_rank==master) then
!  === Read the header of the GS wavefunction file ===
!  TODO: remove this kind of a hack, using a module to store units and filenames.
   if (accesswff==IO_MODE_FORTRAN) fname=filkss
   if (accesswff==IO_MODE_ETSF   ) fname=TRIM(filkss)//'-etsf.nc'

   my_prtvol=0
   call read_kss_header(kss_unt,fname,accesswff,my_prtvol,nsym_out,nbnds_kss,ng_kss,mpsang,nelect,gvec_p,Hdr)

!  In case of parallelism over bands or Adler-Wiser with time reversal find the band
!  index separating the occupied and partially occupied from the empty states (for each spin)
!  Each processor will store in memory the occupied states while the conduction
!  states will be shared between different processors

!  TODO this part can be completely avoided if we read Hdr%occ.
!  The only problem is that Hdr%occ has to be correctly calculated in outkss in case of NSCF run.

   call wrtout(std_out,' testkss: reading occupation numbers ...','COLL')

!  NOTE : In old version of the code, the number of bands defined in the header was different
!  from the value reported in in the first section of a KSS file generated using kssform 3 (if nbandkss<nband).
!  NOW the BUG has been fixed but we keep these tests.
   ABI_CHECK(ALL(Hdr%nband==Hdr%nband(1)),'nband must be constant')
   ABI_CHECK(ALL(Hdr%nband==nbnds_kss),'nband must be equal to nbnds_kss')

   ABI_ALLOCATE(energies_p,(nbnds_kss,Hdr%nkpt,Hdr%nsppol))
   energies_p(:,:,:)=zero
   ABI_ALLOCATE(tmp_enek,(1:nbnds_kss))

   if (accesswff==IO_MODE_FORTRAN) then  !  Read eigenvalues from the KSS file in the FORTRAN format

     do isppol=1,Hdr%nsppol
       do ikpt=1,Hdr%nkpt

         if (Hdr%usepaw==0) then
           do itypat=1,Hdr%ntypat
            do il=1,mpsang
              read(kss_unt) !vkbdb(:,itypat,il)
              read(kss_unt) !vkbdd(:,itypat,il)
            end do
           end do
         end if

         read(kss_unt) tmp_enek(1:nbnds_kss)
         energies_p(1:nbnds_kss,ikpt,isppol)=tmp_enek(1:nbnds_kss)

         do iband=1,nbnds_kss
           read(kss_unt) !kss_ugd(kss_npw*nspinor)
           if (Hdr%usepaw==1) then
             do ispinor=1,Hdr%nspinor
               do iatom=1,Hdr%natom
                read(kss_unt) !(cprjnk_k(iatom,ibsp)%cp(:,1:cprjnk_k(iatom,ibsp)%nlmn))
               end do
             end do
           end if
         end do

       end do !ikpt
     end do !isppol

#if defined HAVE_TRIO_ETSF_IO
   else if (accesswff==IO_MODE_ETSF) then
     ABI_ALLOCATE(eigen,(nbnds_kss))
     eigen(:)=zero
!    allocate(occ_vec(nbnds_kss))
     do isppol=1,Hdr%nsppol
       do ikpt=1,Hdr%nkpt
         Electrons_folder%eigenvalues%data1D         => eigen
         Electrons_folder%eigenvalues__kpoint_access =  ikpt
         Electrons_folder%eigenvalues__spin_access   =  isppol
!        NOTE : occupation numbers have been read from Hdr, and are recalculated in fermi.
!        Electrons_folder%occupations%data1D         => occ_vec
!        Electrons_folder%occupations__kpoint_access = ikpt
!        Electrons_folder%occupations__spin_access   = isppol
         call etsf_io_electrons_get(kss_unt,Electrons_folder,lstat,Error_data)
         ETSF_CHECK_ERROR(lstat,Error_data)

         energies_p(1:nbnds_kss,ikpt,isppol)=eigen(1:nbnds_kss)
         !write(std_out,*)isppol,ikpt,eigen(:)*Ha_eV
       end do
     end do
     nullify(Electrons_folder%eigenvalues%data1D)
     ABI_DEALLOCATE(eigen)
!    nullify(Electrons_folder%occupations%data1D)
!    deallocate(occ_vec)
#endif
   end if

   ABI_DEALLOCATE(tmp_enek)

   if (accesswff==IO_MODE_FORTRAN) close(kss_unt)
   if (accesswff==IO_MODE_ETSF) then
#if defined HAVE_TRIO_ETSF_IO
     call etsf_io_low_close(kss_unt,lstat,Error_data)
     ETSF_CHECK_ERROR(lstat,Error_data)
#endif
   end if
 end if ! (my_rank==master)
 !
 !==========================================
 !=== Cast data if KSS file is not local ===
 !==========================================
 if (nprocs>1) then
   call xcast_mpi(nsym_out, master,comm,ierr)
   call xcast_mpi(nbnds_kss,master,comm,ierr)
   call xcast_mpi(ng_kss,   master,comm,ierr)
   call xcast_mpi(mpsang,   master,comm,ierr)
   call xcast_mpi(nelect,   master,comm,ierr)
   call hdr_comm(Hdr,master,my_rank,comm)
   if (my_rank/=master) then ! this proc did not read.
     ABI_ALLOCATE(gvec_p,(3,ng_kss))
     ABI_ALLOCATE(energies_p,(nbnds_kss,Hdr%nkpt,Hdr%nsppol))
   end if
   call xcast_mpi(gvec_p,    master,comm,ierr)
   call xcast_mpi(energies_p,master,comm,ierr)
   call xbarrier_mpi(comm)
 end if

 ABI_TIMER_STOP("")
 DBG_EXIT("COLL")

end subroutine testkss
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/wfd_read_kss
!! NAME
!! wfd_read_kss
!!
!! FUNCTION
!! Read a KSS file
!!
!! INPUTS
!!  ikss_name=Name of the KSS file.
!!  accesswff=1 for plain Fortran IO, 3 for NETCDF with ETSF-IO.
!!  mpsang=1+maximum angular momentum for nonlocal pseudopotential.
!!  nbndsA=Number of bands required (<=nbandkss)
!!
!! OUTPUT
!!  nelect=Number of electrons
!!  Wfd<wfs_descriptor>=The ug treated by this node are read from file and store in Wfd%Wave.
!!
!! NOTES
!!  For historical reasons, in case of Fortran files, symmetry operations and fractional translations
!!  are not read from the Hdr but directly from the KSS file.
!!  This behavior should be changed but most of the automatic tests should be updated TODO
!!
!! PARENTS
!!      screening,sigma
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine wfd_read_kss(Wfd,ikss_name,nbndsA,accesswff,nelect)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'wfd_read_kss'
 use interfaces_14_hidewrite
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: accesswff,nbndsA
 real(dp),intent(out) :: nelect
 character(len=*),intent(in) :: ikss_name
 type(wfs_descriptor),intent(inout) :: Wfd
!arrays

!Local variables-------------------------------
#if defined HAVE_TRIO_ETSF_IO
 integer :: formeig,mband,mcg
 type(ETSF_main),target :: Main_folder
 type(ETSF_io_low_error) :: Error_data
 type(ETSF_electrons),target :: Electrons_folder
 logical :: lstat
 real(dp),allocatable,target :: cg(:,:),eigen(:),occ_vec(:)
#endif
!scalars
 integer :: band,ib,ierr,iatom,itypat,comm
 integer :: ik_ibz,il,ispinor,spin,my_prtvol,ig_k,ig_kss,npw_k,npw_kso
 integer :: mpsang_,nbandkss,mpw,jj
 integer :: kss_npw,nsym2,spad_kss
 integer :: spad_wfn,kss_unt,bpad
 logical :: found
 character(len=500) :: msg
 character(len=fnlen) :: fname
 type(Hdr_type) :: Hdr
!arrays
 integer :: gk_search(3)
 integer,pointer :: gvec_p(:,:),kg_k(:,:)
 integer,allocatable :: k2gamma(:,:)
 real(dp),allocatable :: energyd(:)
 complex(dpc),allocatable :: kss_ugd(:)
 logical,allocatable :: my_readmask(:,:,:)
 character(len=6) :: tag_spin(2)

! *************************************************************************

 DBG_ENTER("COLL")

 ABI_TIMER_START("")

#ifndef HAVE_TRIO_ETSF_IO
 if (accesswff==IO_MODE_ETSF) then
   write(msg,'(3a)')&
&   ' When accesswff==3, support for the ETSF I/O library ',ch10,&
&   ' must be compiled. Use --enable-etsf-io when configuring '
   MSG_ERROR(msg)
 end if
#endif

 if (accesswff==IO_MODE_ETSF) then
   ABI_CHECK(Wfd%usepaw==0,'PAW+ETSF-IO not yet coded')
 end if
 !
 ! === MPI environment ===
 comm= Wfd%comm
 !
 ! TODO: remove this kind of a hack, using a module to store units and filenames.
 if (accesswff==IO_MODE_FORTRAN) fname=ikss_name
 if (accesswff==IO_MODE_ETSF   ) fname=TRIM(ikss_name)//'-etsf.nc'

 my_prtvol=1
 call read_kss_header(kss_unt,fname,accesswff,my_prtvol,nsym2,nbandkss,kss_npw,mpsang_,nelect,gvec_p,Hdr)

 call hdr_clean(Hdr)

 mpw = MAXVAL(Wfd%npwarr)

 if (.not.Wfd%gamma_centered) then ! Table with correspondence k_sphere --> G_sphere.
   ABI_ALLOCATE(k2gamma,(mpw,Wfd%nkibz))
   k2gamma=kss_npw+1
   do ik_ibz=1,Wfd%nkibz
     npw_k = Wfd%npwarr(ik_ibz)
     kg_k => Wfd%Kdata(ik_ibz)%kg_k

     do ig_k=1,npw_k
       gk_search(:) = kg_k(:,ig_k)
       ig_kss=0; found=.FALSE.
       do while (ig_kss<kss_npw.and..not.found)
         ig_kss=ig_kss+1
         if (ALL(gvec_p(:,ig_kss)==gk_search)) then
           found=.TRUE.
           k2gamma(ig_k,ik_ibz)=ig_kss
         end if
       end do
     end do

   end do
 end if

 ABI_DEALLOCATE(gvec_p)

 ! Each node will read the waves whose status if (WFD_ALLOCATED|WFD_STORED).
 ABI_ALLOCATE(my_readmask,(nbandkss,Wfd%nkibz,Wfd%nsppol))
 my_readmask=.FALSE.
 do spin=1,Wfd%nsppol
   do ik_ibz=1,Wfd%nkibz
     do band=1,Wfd%nband(ik_ibz,spin)

       if (wfd_ihave_ug(Wfd,band,ik_ibz,spin)) then
         my_readmask(band,ik_ibz,spin) = .TRUE.
         if (band>nbndsA) then
           write(std_out,*)Wfd%Wave(:,ik_ibz,spin)%has_ug
           MSG_ERROR("band>nbndsA")
         end if
         if (wfd_ihave_ug(Wfd,band,ik_ibz,spin,"Stored")) then
           MSG_WARNING("Wavefunction is already stored!")
         end if
       end if

     end do
   end do
 end do

 write(msg,'(a,i0,a)')" rdkss: will read ",COUNT(my_readmask)," states"
 call wrtout(std_out,msg,"PERS")
 !
 ! =========================================
 ! ==== Read wavefunctions and energies ====
 ! =========================================
 ABI_ALLOCATE(energyd,(nbandkss))
 ABI_ALLOCATE(kss_ugd,(kss_npw*Wfd%nspinor))

 tag_spin(:)=(/'      ','      '/); if (Wfd%nsppol==2) tag_spin(:)=(/' UP   ',' DOWN '/)
 !
 ! Here it is possible to reduce the number of BCAST
 write(msg,'(2a)')ch10,' k       eigenvalues [eV]'
 call wrtout(ab_out,msg,'COLL')
 call wrtout(std_out,msg,'COLL')

 ! === Each proc stores only the sub-block of eigenvalues required by the user ===

 SELECT CASE  (accesswff)

 CASE  (IO_MODE_FORTRAN)

 do spin=1,Wfd%nsppol
   do ik_ibz=1,Wfd%nkibz

     npw_k   = Wfd%npwarr(ik_ibz)
     npw_kso = npw_k*Wfd%nspinor

     if (Wfd%usepaw==0) then  ! Skip Kleynmann-Bylander form factor and derivatives for this k.
       do itypat=1,Wfd%ntypat
         do il=1,mpsang_
           read(kss_unt) !vkbdb(:,itypat,il)
           read(kss_unt) !vkbdd(:,itypat,il)
         end do
       end do
     end if

     read(kss_unt) energyd(1:nbandkss)

     if (Wfd%my_rank==Wfd%master) then ! TODO, too much output, should write to std_out.
       if (Wfd%nsppol==2) then
         write(ab_out,'(i3,a,10f7.2/50(10x,10f7.2/))') ik_ibz,tag_spin(spin),(energyd(ib)*Ha_eV,ib=1,nbndsA)
       else
         write(ab_out,'(i3,7x,10f7.2/50(10x,10f7.2/))')ik_ibz,(Ha_eV*energyd(ib),ib=1,nbndsA)
       end if
     end if

     ! Here we have to loop over nbndsA because we still need to read the cprj.
     do ib=1,nbndsA
       if (my_readmask(ib,ik_ibz,spin)) then
         !
         ! Read and store ug_{bks}.
         read(kss_unt) kss_ugd(1:kss_npw*Wfd%nspinor)

         if (Wfd%gamma_centered) then ! No conversion is needed.
           do ispinor=1,Wfd%nspinor
             spad_kss=(ispinor-1)*kss_npw
             spad_wfn=(ispinor-1)*npw_k
             Wfd%Wave(ib,ik_ibz,spin)%ug(spad_wfn+1:spad_wfn+npw_k)=kss_ugd(spad_kss+1:spad_kss+npw_k)
           end do

         else ! Convert from gamma- to k-centered G-spheres.
           do ispinor=1,Wfd%nspinor
             spad_kss=(ispinor-1)*kss_npw
             spad_wfn=(ispinor-1)*npw_k
             do ig_k=1,npw_k
               ig_kss=k2gamma(ig_k,ik_ibz)
               if (ig_kss/=kss_npw+1) then
                 Wfd%Wave(ib,ik_ibz,spin)%ug(ig_k+spad_wfn)=kss_ugd(ig_kss+spad_kss)
               else
                 Wfd%Wave(ib,ik_ibz,spin)%ug(ig_k+spad_wfn)=czero
               end if
             end do
           end do
         end if
         Wfd%Wave(ib,ik_ibz,spin)%has_ug = WFD_STORED
         !
         ! Read and store cprj.
         if (Wfd%usepaw==1) then
           do ispinor=1,Wfd%nspinor
             do iatom=1,Wfd%natom
               read(kss_unt) Wfd%Wave(ib,ik_ibz,spin)%Cprj(iatom,ispinor)%cp
             end do
           end do
           Wfd%Wave(ib,ik_ibz,spin)%has_cprj = WFD_STORED
         end if

       else
         ierr = skip_kss_record(kss_unt,1,Wfd%usepaw,Wfd%nspinor,Wfd%natom)
         if (ierr/=0) then
           write(msg,'(a,3(i0,1x))')" IO-Error reading KSS file records for (b,k,s)= ",ib,ik_ibz,spin
           MSG_ERROR(msg)
         end if
       end if
     end do ! ib
     !
     ! Skip the remaining (nbandkss-nbndsA) records.
     ierr = skip_kss_record(kss_unt,nbandkss-nbndsA,Wfd%usepaw,Wfd%nspinor,Wfd%natom)
     if (ierr/=0) then
       write(msg,'(a,3(i0,1x))')" IO-Error while skipping KSS file records after (b,k,s)= ",ib,ik_ibz,spin
       MSG_ERROR(msg)
     end if

   end do !ik_ibz
 end do !spin

 CASE (IO_MODE_ETSF)

#if defined HAVE_TRIO_ETSF_IO

 ! Read full block. TODO: Can be opimized since we usually need a much smaller block.
 formeig=0; mcg=Wfd%nspinor*kss_npw*nbandkss; mband=nbandkss
 ABI_ALLOCATE(cg,(2,mcg))
 ABI_ALLOCATE(eigen,((2*mband)**formeig*mband))
 ABI_ALLOCATE(occ_vec,(mband))

 do spin=1,Wfd%nsppol
   do ik_ibz=1,Wfd%nkibz
     npw_k   = Wfd%npwarr(ik_ibz)
     npw_kso = npw_k*Wfd%nspinor

     if (ALL(.not.my_readmask(:,ik_ibz,spin))) CYCLE

     ! Get eigenvalues, occupations and coefficients
     Electrons_folder%eigenvalues%data1D         => eigen
     Electrons_folder%eigenvalues__kpoint_access =  ik_ibz
     Electrons_folder%eigenvalues__spin_access   =  spin
     Electrons_folder%occupations%data1D         => occ_vec
     Electrons_folder%occupations__kpoint_access =  ik_ibz
     Electrons_folder%occupations__spin_access   =  spin

     call etsf_io_electrons_get(kss_unt,Electrons_folder,lstat,Error_data)
     ETSF_CHECK_ERROR(lstat,Error_data)

     ! === Get the coefficients_of_wavefunctions ===
     Main_folder%coefficients_of_wavefunctions%data2D => cg(:,:)
     Main_folder%wfs_coeff__kpoint_access             =  ik_ibz
     Main_folder%wfs_coeff__spin_access               =  spin

     call etsf_io_main_get(kss_unt,Main_folder,lstat,Error_data)
     ETSF_CHECK_ERROR(lstat,Error_data)
     !write(std_out,*)' occ_vec    ik_ibz = ',ik_ibz,occ_vec
     !write(std_out,*)' eigen [eV] ik_ibz = ',ik_ibz,eigen*Ha_eV
     !write(std_out,*)' cg         ik_ibz = ',ik_ibz,cg
     !ene_out(ik_ibz,1:nbndsA,spin)=eigen(1:nbndsA)
     !
     ! Convert double precision real to complex quantities i.e cg ==> kss_ugd
     do ib=1,nbndsA
       if (my_readmask(ib,ik_ibz,spin)) then
         bpad=kss_npw*Wfd%nspinor*(ib-1)
         kss_ugd(1:npw_k) =CMPLX(cg(1,bpad+1:bpad+npw_k),cg(2,bpad+1:bpad+npw_k))
         if (Wfd%nspinor==2) then
           jj=bpad+kss_npw
           kss_ugd(npw_k+1:2*npw_k)=CMPLX(cg(1,jj+1:jj+npw_k),cg(2,jj+1:jj+npw_k))
         end if
         if (Wfd%gamma_centered) then
           ! No conversion is needed.
           Wfd%Wave(ib,ik_ibz,spin)%ug=kss_ugd(1:npw_k*Wfd%nspinor)
         else
           ! Convert from gamma- to k-centered G-spheres.
           do ispinor=1,Wfd%nspinor
             spad_kss=(ispinor-1)*npw_k
             spad_wfn=(ispinor-1)*npw_k
             do ig_k=1,npw_k
               ig_kss=k2gamma(ig_k,ik_ibz)
               if (ig_kss/=kss_npw+1) then
                 Wfd%Wave(ib,ik_ibz,spin)%ug(ig_k+spad_wfn)=kss_ugd(ig_kss+spad_kss)
               else
                 Wfd%Wave(ib,ik_ibz,spin)%ug(ig_k+spad_wfn)=czero
               end if
             end do
           end do
         end if
         Wfd%Wave(ib,ik_ibz,spin)%has_ug = WFD_STORED
       end if
     end do

     nullify(Main_folder%coefficients_of_wavefunctions%data2D)
     nullify(Electrons_folder%occupations%data1D)
     nullify(Electrons_folder%eigenvalues%data1D)

   end do !ik_ibz
 end do !spin

 ABI_DEALLOCATE(cg)
 ABI_DEALLOCATE(eigen)
 ABI_DEALLOCATE(occ_vec)

#endif

 CASE DEFAULT
   write(msg,'(a,i0)')" Unsupported value for accesswff: ",accesswff
   MSG_ERROR(msg)
 END SELECT
 !
 ! === Close the file ===
 if (accesswff==IO_MODE_FORTRAN) close(kss_unt)
#if defined HAVE_TRIO_ETSF_IO
 if (accesswff==IO_MODE_ETSF) then
   call etsf_io_low_close(kss_unt,lstat,Error_data)
   ETSF_CHECK_ERROR(lstat,Error_data)
 end if
#endif
 !
 ! ============================================================
 ! ==== Reading completed, now test the orthonormalization ====
 ! ============================================================

 ! Update the kbs table storing the distribution of the ug.
 call wfd_update_bkstab(Wfd)

 ! Initialize the MPI communicators.
 call wfd_set_mpicomm(Wfd)
 !
 ! === Free memory ===
 if (allocated(energyd))  then
   ABI_DEALLOCATE(energyd)
 end if
 if (allocated(kss_ugd))  then
   ABI_DEALLOCATE(kss_ugd)
 end if
 if (allocated(k2gamma))  then
   ABI_DEALLOCATE(k2gamma)
 end if

 ABI_DEALLOCATE(my_readmask)

 ABI_TIMER_STOP("")

 DBG_EXIT("COLL")

end subroutine wfd_read_kss
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/write_kss_header
!! NAME
!!  write_kss_header
!!
!! FUNCTION
!!  Write the header of the KSS file either using plain Fortran-IO or ETSF-IO.
!!  Returns the unit number to be used for further writing.
!!  It should be executed by master node only.
!!
!! INPUTS
!!  filekss(len=fnlen)=The name of the KSS file.
!!  kss_npw=Number of planewaves used for the wavefunctions in the KSS files.
!!  ishm=Max number of shells written on file
!!  shlim(ishm)=The cumulative number of G"s in each shell.
!!  nbandksseff=Number of bands to be written.
!!  mband=The maximum number of bands treated by abinit.
!!  nsym2=Number of symmetry operations to be written on the header.
!!  symrel2(3,3,nsym2)=The symmetry operations in real space to be written.
!!  tnons2(3,nsym2)=The fractional translations associateed to symrel2.
!!  gbig(3,kss_npw)=The set of G-vectors for the KSS wavefunctions (Gamma-centered)
!!  Hdr<hdr_type>=The abinit header.
!!  Dtset <dataset_type>=all input variables for this dataset
!!  Psps<pseudopotential_type>=Structure gathering info on the pseudopotentials.
!!  accesswff=Input variables specifying the fileformat. (0-->Fortran,3-->ETSF-IO).
!!  occ(mband*nkpt*nsppol)=The occupation factors.
!!
!! OUTPUT
!!  kss_unt=The unit number of the opened file.
!!
!! SIDE EFFECTS
!!  The KSS Header is written on file.
!!
!! PARENTS
!!      outkss
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine write_kss_header(filekss,kss_npw,ishm,nbandksseff,mband,nsym2,symrel2,tnons2,occ,gbig,shlim,&
&  Dtset,Hdr,Psps,accesswff,kss_unt)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'write_kss_header'
 use interfaces_14_hidewrite
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: accesswff,kss_npw,nbandksseff,ishm,nsym2,mband
 integer,intent(out) :: kss_unt
 character(len=fnlen),intent(in) :: filekss
 type(pseudopotential_type),intent(in) :: Psps
 type(Hdr_type),intent(in) :: Hdr
 type(Dataset_type),intent(in) :: Dtset
!arrays
 integer,intent(in) :: symrel2(3,3,nsym2)
 integer,intent(in) :: gbig(3,kss_npw),shlim(ishm)
 real(dp),intent(in) :: tnons2(3,nsym2)
 real(dp),intent(in) :: occ(mband*Dtset%nkpt*Dtset%nsppol)

!Local variables-------------------------------
!scalars
 integer :: nspinor,nsppol,nkpt,itypat
 integer :: nb,isppol,ik,fform,rdwr,ii,jj,kk,ig
 integer :: il,il0,ilmn,in,ind1,ind2
 character(len=80) :: title
 character(len=500) :: msg
 type(hdr_type) :: my_Hdr
 type(dataset_type) :: Dtset_cpy
#if defined HAVE_TRIO_ETSF_IO
 logical :: lstat
 type(etsf_gwdata) :: GW_data
 type(wvl_wf_type) :: Dummy_wfs
 type(etsf_io_low_error) :: Error_data
#endif
!arrays
 integer,allocatable,target :: vkbsign_int(:,:)
 real(dp),allocatable :: vkbsign(:,:)

! *********************************************************************

 DBG_ENTER("COLL")

 nsppol = Dtset%nsppol
 nkpt   = Dtset%nkpt
 nspinor= Dtset%nspinor

 write(msg,'(3a)')ch10,' Opening file for KS structure output: ',TRIM(filekss)
 call wrtout(std_out,msg,'COLL')
 !% call wrtout(ab_out,msg,'COLL')

 write(msg,'(a,i6)') ' number of Gamma centered plane waves ',kss_npw
 call wrtout(std_out,msg,'COLL')
 call wrtout(ab_out,msg,'COLL')
 write(msg,'(a,i6)') ' number of Gamma centered shells ',ishm
 call wrtout(std_out,msg,'COLL')
 call wrtout(ab_out,msg,'COLL')
 write(msg,'(a,i6)') ' number of bands ',nbandksseff
 call wrtout(std_out,msg,'COLL')
 call wrtout(ab_out,msg,'COLL')
 write(msg,'(a,i6)') ' maximum angular momentum components ',Psps%mpsang
 call wrtout(std_out,msg,'COLL')
 call wrtout(ab_out,msg,'COLL')
 write(msg,'(a,i2,a)')' number of symmetry operations ',nsym2,' (without inversion)'
 call wrtout(std_out,msg,'COLL')

!Copy the header so that we can change some basic dimensions using the KSS values:
!(bantot, npwarr, nband) and the occupation factors

!Note that nsym and symrel might have been changed this has to be fixed
!carefully in the next patch since in the new implementation symmorphy=0 should be dafault

 call hdr_copy(Hdr,my_Hdr)

 my_Hdr%npwarr =kss_npw
 my_Hdr%nband  =nbandksseff
 my_Hdr%bantot =nbandksseff*nkpt*nsppol

 my_Hdr%istwfk = 1  ! KSS file does not support istwfk/=1 even though the GS run
                    ! can take advantage of time-reversal symmetry.

!Copy the occ number in the new header with correct dimensions
!fill with zero the rest since mband can be < nbandksseff
 !write(std_out,*)associated(my_Hdr%occ)
 !if (associated(my_Hdr%occ)) deallocate(my_Hdr%occ)
 ABI_DEALLOCATE(my_Hdr%occ)
 ABI_ALLOCATE(my_Hdr%occ,(my_Hdr%bantot))
 !mband = MAXVAL(Hdr%nband)

 my_Hdr%occ=zero; nb=MIN(mband,nbandksseff)
 do isppol=1,nsppol
   do ik=1,nkpt
     ind1=1+(ik-1)*nbandksseff+(isppol-1)*nkpt*nbandksseff
     ind2=1+(ik-1)*mband      +(isppol-1)*nkpt*mband
     my_Hdr%occ(ind1:ind1+nb-1) = occ(ind2:ind2+nb-1)
   end do
 end do

!Change dimension in the local Dtset_cpy as well.
 call dtset_copy(Dtset_cpy, Dtset)
 Dtset_cpy%mpw   = kss_npw
 Dtset_cpy%mband = nbandksseff

 fform=502; rdwr=2

 SELECT CASE (accesswff)

 CASE (IO_MODE_FORTRAN)

   kss_unt=get_unit()
   open(kss_unt,file=filekss,form='unformatted')

   call hdr_io(fform,my_Hdr,rdwr,kss_unt)

   title='Results from ABINIT code';          write(kss_unt) title(1:80)
   title='Ab-initio plane waves calculation'; write(kss_unt) title(1:80)

   write(kss_unt) nsym2,nbandksseff,kss_npw,ishm,Psps%mpsang ! To be modified to deal with more than one projector
   write(kss_unt) (((symrel2(ii,jj,kk),ii=1,3),jj=1,3),kk=1,nsym2)
   write(kss_unt) ((tnons2(ii,kk),ii=1,3),kk=1,nsym2)
   write(kss_unt) ((gbig(ii,ig),ii=1,3),ig=1,kss_npw)
   write(kss_unt) (shlim(in),in=1,ishm)
   !
   ! Write vkbsign for NC pseudos.
   ! FIXME : only one projector in each angular is treated.
   ! Moreover the allocation is done in the wrong order for dimensions...
   if (Psps%usepaw==0) then
     ABI_ALLOCATE(vkbsign,(Psps%ntypat,Psps%mpsang))
     vkbsign(:,:)=zero
     do itypat=1,Psps%ntypat
       il0=0
       do ilmn=1,Psps%lmnmax
         il=1+Psps%indlmn(1,ilmn,itypat)
         in=Psps%indlmn(3,ilmn,itypat)
         if (il/=il0 .and. in==1) then
           il0=il
           vkbsign(itypat,il)=DSIGN(one,Psps%ekb(ilmn,itypat))
         end if
       end do
     end do
     write(kss_unt) ((vkbsign(itypat,il),il=1,Psps%mpsang),itypat=1,Psps%ntypat)
     ABI_DEALLOCATE(vkbsign)
   end if

#if defined HAVE_TRIO_ETSF_IO
 CASE (IO_MODE_ETSF)
!  We currently use the dataset symmetries, as defined in the Hdr structure instead of the symmetries recomputed in outkss.
   call abi_etsf_init(Dtset_cpy, filekss, 4, .false., my_Hdr%lmn_size, Psps, Dummy_wfs)

   call abi_etsf_geo_put(Dtset_cpy, filekss, Psps)
   ! Open again for further additions
   call etsf_io_low_open_modify(kss_unt, TRIM(filekss)//"-etsf.nc", lstat, Error_data = Error_data)
   ETSF_CHECK_ERROR(lstat,Error_data)

   ! Add additional info from abinit header.
   call hdr_io_etsf(fform,my_Hdr,rdwr,kss_unt)

   ! If NC pseudos, write vkbsign. Here ordering of dimensions is OK but multi-projectors not supported.
   if (Psps%usepaw==0) then
     ABI_ALLOCATE(vkbsign_int,(Psps%mpsang,Psps%ntypat))
     vkbsign_int=0
     do itypat=1,Psps%ntypat
       il0=0
       do ilmn=1,Psps%lmnmax
         il=1+Psps%indlmn(1,ilmn,itypat)
         in=Psps%indlmn(3,ilmn,itypat)
         if ((il/=il0).and.(in==1)) then
           il0=il
           vkbsign_int(il,itypat)=NINT(DSIGN(one,Psps%ekb(ilmn,itypat)))
         end if
       end do
     end do
     ! Write it now to be able to deallocate quickly.
     GW_data%kb_formfactor_sign%data2D => vkbsign_int
     call etsf_io_gwdata_put(kss_unt, GW_data, lstat, Error_data) 
     ETSF_CHECK_ERROR(lstat,Error_data)
     nullify(GW_data%kb_formfactor_sign%data2D)
     ABI_DEALLOCATE(vkbsign_int)
   end if
#endif

 CASE DEFAULT
   write(msg,'(a,i0)')" Unsupported value for accesswff: ",accesswff
   MSG_ERROR(msg)
 END SELECT

 call dtset_free(Dtset_cpy)
 call hdr_clean(my_Hdr)

 DBG_EXIT("COLL")

end subroutine write_kss_header
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/read_kss_header
!! NAME
!!  read_kss_header
!!
!! FUNCTION
!!  Read the header of the KSS file either using plain Fortran-IO or ETSF-IO.
!!
!! INPUTS
!!  filkss(len=fnlen)=The name of the KSS file.
!!  accesswff=Input variables specifying the fileformat. (0-->Fortran,3-->ETSF-IO).
!!  prtvol=Flag governing verbosity output.
!!
!! OUTPUT
!!  kss_unt=The unit number of the opened file.
!!  nsym_out=Number of symmetry operations read from the KSS file (not from the abinit header)
!!  nbnds_kss=Number of bands stored.
!!  ng_kss=Number of planewaves used for the wavefunctions in the KSS files.
!!  mpsang=Max angular momentum + 1
!!  nelect=Number of electrons (including a possible charge in the unit cell)
!!  Hdr<hdr_type>=The abinit header.
!!
!! SIDE EFFECTS
!!  gvec_p(3,ng_kss)=Input:  Pointer to null()
!!                   Output: The set of G-vectors for the KSS wavefunctions (Gamma-centered)
!!
!! PARENTS
!!      kss2wfk,m_io_kss
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine read_kss_header(kss_unt,filkss,accesswff,prtvol,nsym_out,nbnds_kss,ng_kss,mpsang,nelect,gvec_p,Hdr)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'read_kss_header'
 use interfaces_14_hidewrite
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: accesswff,prtvol
 integer,intent(out) :: nsym_out,nbnds_kss,ng_kss,mpsang,kss_unt
 real(dp),intent(out) :: nelect
 character(len=fnlen),intent(in) :: filkss
 type(Hdr_type),intent(out) :: Hdr
!arrays
 integer,pointer :: gvec_p(:,:)

!Local variables-------------------------------
!scalars
 integer :: ios,nshells
 integer :: fform,rdwr,ii,ig,nsym_kss
 character(len=80) :: title(2)
 character(len=500) :: msg
 logical :: ltest
#if defined HAVE_TRIO_ETSF_IO
 logical :: lstat
 type(etsf_io_low_error) :: Error_data
 type(ETSF_dims) :: Dims
 type(ETSF_basisdata),target :: Wave_folder
!arrays
 integer,allocatable,target :: kg_k(:,:)
#endif

! *************************************************************************

 DBG_ENTER("COLL")

#if !defined HAVE_TRIO_ETSF_IO
 if (accesswff==IO_MODE_ETSF) then
   write(msg,'(3a)')&
&   ' When accesswff==3, support for the ETSF I/O library ',ch10,&
&   ' must be compiled. Use --enable-etsf-io when configuring '
   MSG_ERROR(msg)
 end if
#endif

 kss_unt = get_unit()
 !
 ! ===============================================
 ! ==== Read file according to the fileformat ====
 ! ===============================================
 SELECT CASE (accesswff)

 CASE (IO_MODE_FORTRAN) ! * Formatted Fortran File
   write(msg,'(2a)')' reading Fortran Kohn-Sham structure file ',TRIM(filkss)
   call wrtout(std_out,msg,'COLL')

   open(unit=kss_unt,file=filkss,form='unformatted',status='old',iostat=ios)
   write(msg,'(3a)')'Opening file: ',TRIM(filkss),' as old'
   ABI_CHECK(ios==0,msg)

   rdwr=1
   call hdr_io(fform,Hdr,rdwr,kss_unt)

 CASE (IO_MODE_ETSF) ! * NETCDF-ETSF file format

#if defined HAVE_TRIO_ETSF_IO
   write(msg,'(2a)')' wfd_read_kss : reading NETCDF-ETSF Kohn-Sham structure file ',TRIM(filkss)
   call wrtout(std_out,msg,'COLL')

   call etsf_io_low_open_read(kss_unt,filkss,lstat,Error_data=Error_data)
   ETSF_CHECK_ERROR(lstat,Error_data)

   rdwr=1
   call hdr_io_etsf(fform,Hdr,rdwr,kss_unt)

   ABI_CHECK(fform/=602,' Single precision KSS + ETSF-IO not implemented')
   ABI_CHECK(Hdr%usepaw==0,'PAW+ETSF-IO not yet coded')
#endif

 CASE DEFAULT
   write(msg,'(a,i4)')'Wrong value for accesswff = ',accesswff
   MSG_ERROR(msg)
 END SELECT

 if (fform==602) then ! * fform must be 502.
   write(msg,'(3a)')&
&   ' Starting v5.6, KSS files in single precision are not supported anymore,',ch10,&
&   ' Please, use an older version of abinit.'
   MSG_ERROR(msg)
 end if
 if (fform>=1.and.fform<=2) then
   write(msg,'(a,i4)')' (STA|QPLDA) format are obsolete and not supported anymore; fform= ',fform
   MSG_ERROR(msg)
 end if
 if (fform/=502) then
   write(msg,'(a,i4)')' Found unknown file format; fform= ',fform
   MSG_ERROR(msg)
 end if
 !
 ! === Output the header of the GS wavefunction file ===
 if (prtvol>0) then
   rdwr=4
   if (accesswff==IO_MODE_FORTRAN) then
     call hdr_io(fform,Hdr,rdwr,std_out)
   else if (accesswff==IO_MODE_ETSF) then
#if defined HAVE_TRIO_ETSF_IO
     call hdr_io_etsf(fform,Hdr,rdwr,std_out)
#endif
   end if
 end if

 write(msg,'(1x,47a)')('-',ii=1,47)
 call wrtout(std_out,msg,'COLL')
 write(msg,'(3a,a6,a,i3)')&
&  ' KSS abinit double precision form',ch10,&
&  ' generated by ABINIT ',Hdr%codvsn,' header version ',Hdr%headform
 call wrtout(std_out,msg,'COLL')

 if ( .not.isknown_headform(Hdr%headform) ) then
   write(msg,'(a,i5)')' Unknown header version = ',Hdr%headform
   MSG_ERROR(msg)
 end if

! === Test spin-orbit characteristic ===
 if (Hdr%headform<53) then  ! Format previous to version 5.5, now pspo is obsolete and has been substituted by so_psp.
   ltest=ALL(Hdr%pspso(1:Hdr%ntypat)==1)
   ABI_CHECK(ltest,'pspso/=1 value not programmed')
 else  ! New format containing so_psp
   ltest=ALL(Hdr%so_psp(1:Hdr%npsp)==1)
   ABI_CHECK(ltest,'so_psp/=1 value not programmed')
 end if

 ! There might be extra charge thus we use occ_out. factors to calculate nelect.
 ! Besides note that nelect is real
 nelect = hdr_get_nelect_byocc(Hdr)
 !
 ! === Abinit Header successfully read ===
 ! * Now read basic dimensions.
 ! * Note that, in the case of Fortran file, nsym_out is read from the second record
 nsym_out=Hdr%nsym

 SELECT CASE (accesswff)

 CASE (IO_MODE_FORTRAN)
   read(kss_unt) title(1)
   read(kss_unt) title(2)
   write(msg,'(2a,1x,a79,a,1x,a79,a)')' title of file: ',ch10,title(1)(:79),ch10,title(2)(:79),ch10
   call wrtout(std_out,msg,'COLL')
   read(kss_unt) nsym_kss,nbnds_kss,ng_kss,nshells,mpsang
   read(kss_unt) !(((symrel2(jj,ii,isym),ii=1,3),jj=1,3),isym=1,nsym_kss)
   read(kss_unt) !((tnons(i,isym),i=1,3),isym=1,nsym_kss)

   ABI_ALLOCATE(gvec_p,(3,ng_kss))
   read(kss_unt)((gvec_p(ii,ig),ii=1,3),ig=1,ng_kss)
   nsym_out=nsym_kss

   read(kss_unt)                      !(shlim(i),i=1,nshells)
   if (Hdr%usepaw==0) read(kss_unt)   !((vkbsignd(il,is),il=1,mpsang),is=1,Hdr%ntypat)

 CASE (IO_MODE_ETSF) ! TODO spin-orbit not treated, number of projectors not treated
#if defined HAVE_TRIO_ETSF_IO
   call etsf_io_dims_get(kss_unt,Dims,lstat,Error_data)
   nsym_kss =Dims%number_of_symmetry_operations
   nbnds_kss=Dims%max_number_of_states
   ng_kss   =Dims%max_number_of_coefficients
   mpsang   =Dims%max_number_of_angular_momenta

   ABI_ALLOCATE(gvec_p,(3,ng_kss))
   ABI_ALLOCATE(kg_k,(3,ng_kss))
   Wave_folder%reduced_coordinates_of_plane_waves%data2D => kg_k(:,:)
   call etsf_io_basisdata_get(kss_unt,Wave_folder,lstat,Error_data)
   gvec_p(:,:)=kg_k(:,:)
   ABI_DEALLOCATE(kg_k)
   nshells=0 ! nshells is not defined in the ETSF spefications but it is not used
#endif

 CASE DEFAULT
   MSG_ERROR("Unsupported value for accesswff")
 END SELECT

 ABI_CHECK(ALL(gvec_p(1:3,1)==0),'First G must be 0')

 if (prtvol>0) then ! Output important dimensions on the log file.
   write(msg,'(a,f8.2)')' number of electrons                    ',nelect
   call wrtout(std_out,msg,'COLL')
   write(msg,'(a,i8)')' number of symmetries without inversion ',nsym_out
   call wrtout(std_out,msg,'COLL')
   write(msg,'(a,i8)')' number of bands                        ',nbnds_kss
   call wrtout(std_out,msg,'COLL')
   write(msg,'(a,i8)')' number of plane waves                  ',ng_kss
   call wrtout(std_out,msg,'COLL')
   write(msg,'(a,i8)')' number of shells                       ',nshells
   call wrtout(std_out,msg,'COLL')
   write(msg,'(a,i8,a)')' maximum angular momentum +1          ',mpsang,ch10
   call wrtout(std_out,msg,'COLL')
   write(msg,'(1x,47a)')('-',ii=1,47)
   call wrtout(std_out,msg,'COLL')
 end if
 !
 ! === Check the value of some variables ===
 ! This is due to the awful treatment of symmetries done in outkss
 if (Hdr%nsym/=nsym_kss) then
   write(msg,'(2a,2(a,i3))')&
&    ' Code does not use the original set of symmetries.',ch10,&
&    ' Hdr%nsym= ',Hdr%nsym,' /= nsym_kss= ',nsym_kss
   MSG_COMMENT(msg)
 end if

 DBG_EXIT("COLL")

end subroutine read_kss_header
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/write_vkb
!! NAME
!!  write_vkb
!!
!! FUNCTION
!!  Writes the KB form factors and derivates on file for a single k-point.
!!  Supports plain Fortran IO and ETSF-IO.
!!
!! INPUTS
!!  kss_unt=The unit number of the file
!!  ikpt=The index of the k-point
!!  kpoint(3)=The k-point in reduced coordinates.
!!  kss_npw=Number of planewaves used for the wavefunctions in the KSS files.
!!  npw_k=Number of planewaves at this k-point in the k-centered basis set used in abinit (ecut).
!!  trsl(kss_npw)=Mapping between the G-sphere used for the KSS wavefunctions and the
!!   Abinit G-sphere (npw_k). As kss_npw>=npw_k, trsl=npw_k+1 is the "KSS" G-vector
!!   is not contained in the abinit one.
!!  ecut=cutoff energy used in abinit.
!!  rprimd(3,3)=dimensional primitive translations for real space (bohr).
!!  Psps<Pseudopotential_type>=Datatype gathering data on the Pseudopotentials.
!!  accesswff=Input variables specifying the fileformat. (0-->Fortran,3-->ETSF-IO).
!!  gbig(3,kss_npw)=Set of G-vectors used in the KSS file.
!!
!! OUTPUT
!!  Only writing.
!!
!! PARENTS
!!      m_io_kss
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine write_vkb(kss_unt,ikpt,kpoint,kss_npw,gbig,trsl,rprimd,Psps,accesswff)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'write_vkb'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: ikpt,accesswff,kss_npw,kss_unt
 type(Pseudopotential_type),intent(in) :: Psps
!arrays
 integer,intent(in) :: trsl(kss_npw),gbig(3,kss_npw)
 real(dp),intent(in) :: kpoint(3),rprimd(3,3)

!Local variables-------------------------------
!scalars
 integer :: istat,itypat,il,ig,mpsang,ntypat
 character(len=500) :: msg
!array
 real(dp),allocatable :: vkb(:,:,:),vkbd(:,:,:)
 real(dp),allocatable :: dum_vkbsign(:,:)
#if defined HAVE_TRIO_ETSF_IO
 logical :: lstat
 real(dp),allocatable,target :: vkb_tgt(:,:,:), vkbd_tgt(:,:,:)
 type(etsf_io_low_error) :: Error_data
 type(etsf_gwdata) :: GW_data
#endif

! *********************************************************************

 mpsang = Psps%mpsang
 ntypat = Psps%ntypat

 ABI_ALLOCATE(vkb ,(kss_npw,ntypat,mpsang))
 istat = ABI_ALLOC_STAT
 ABI_ALLOCATE(vkbd,(kss_npw,ntypat,mpsang))
 istat = ABI_ALLOC_STAT
 ABI_ALLOCATE(dum_vkbsign,(ntypat,mpsang))

 call calc_vkb(Psps,kpoint,kss_npw,gbig,rprimd,dum_vkbsign,vkb,vkbd)
 ABI_DEALLOCATE(dum_vkbsign)

 SELECT CASE (accesswff)

 CASE (IO_MODE_FORTRAN)
  do itypat=1,ntypat
    do il=1,mpsang
      !write(kss_unt) (vkb (trsl(ig),itypat,il),ig=1,kss_npw)
      !write(kss_unt) (vkbd(trsl(ig),itypat,il),ig=1,kss_npw)
      write(kss_unt) (vkb (ig,itypat,il),ig=1,kss_npw)
      write(kss_unt) (vkbd(ig,itypat,il),ig=1,kss_npw)
    end do
  end do

#if defined HAVE_TRIO_ETSF_IO
 CASE (IO_MODE_ETSF)
   ABI_ALLOCATE(vkb_tgt ,(kss_npw,mpsang,ntypat))
   ABI_ALLOCATE(vkbd_tgt,(kss_npw,mpsang,ntypat))
   do itypat=1,ntypat
     do il=1,mpsang
       do ig=1,kss_npw
         !vkb_tgt (ig,il,itypat)=vkb (trsl(ig),itypat,il)
         !vkbd_tgt(ig,il,itypat)=vkbd(trsl(ig),itypat,il)
         vkb_tgt (ig,il,itypat)=vkb (ig,itypat,il)
         vkbd_tgt(ig,il,itypat)=vkbd(ig,itypat,il)
       end do
     end do
   end do
   GW_data%kb_coeff__kpoint_access         = ikpt
   GW_data%kb_coeff_der__kpoint_access     = ikpt
   GW_data%kb_formfactors%data3D           => vkb_tgt
   GW_data%kb_formfactor_derivative%data3D => vkbd_tgt

   call etsf_io_gwdata_put(kss_unt, GW_data, lstat, Error_data)
   ETSF_CHECK_ERROR(lstat,Error_data)

   nullify(GW_data%kb_formfactors%data3D          ) ! Avoid dangling pointers
   nullify(GW_data%kb_formfactor_derivative%data3D)
   ABI_DEALLOCATE(vkb_tgt)
   ABI_DEALLOCATE(vkbd_tgt)
#endif

 CASE DEFAULT
   write(msg,'(a,i0)')" Unsupported value for accesswff: ",accesswff
   MSG_ERROR(msg)
 END SELECT

 ABI_DEALLOCATE(vkb)
 ABI_DEALLOCATE(vkbd)

 RETURN
 ABI_UNUSED(trsl(1)) ! just to keep trsl as an argument while in development

end subroutine write_vkb
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/write_kss_wfgk
!! NAME
!!  write_kss_wfgk
!!
!! FUNCTION
!!  Write the Gamme-centered wavefunctions and energies on the KSS file for a single k-point.
!!  (Only the master node should call this routine).
!!
!! INPUTS
!!  kss_unt=The unit number of the file
!!  ikpt=The index of the k-point
!!  isppol=The spin index.
!!  nspinor=number of spinorial components (on current proc)
!!  kss_npw=Number of planewaves used for the wavefunctions in the KSS files.
!!  npw_k=Number of plane-waves in the k-centered basis set.
!!  nbandksseff=Number of bands to be written.
!!  natom=Number of atoms.
!!  Psps<Pseudopotential_type>=Structure gathering pseudopotential data.
!!  kpoint(3)=The k-points in reduced coordinates.
!!  ene_k(nbandksseff)=Energies at this k-point
!!  occ_k(nbandksseff)=Occupation factors at this k-point.
!!  rprimd(3,3)=dimensional primitive translations for real space (bohr).
!!  kg_k(3,npw_k)=The G-vectors in the k-centered basis set.
!!  gbig(3,kss_npw)=The set of G-vectors for the KSS wavefunctions (Gamma-centered)
!!  wfg(2,kss_npw*nspinor,nbandksseff)=The wavefunction Fourier coefficients.
!!  accesswff=Input variables specifying the fileformat. (0-->Fortran,3-->ETSF-IO).
!!
!! OUTPUT
!!  Only writing.
!!
!! PARENTS
!!      outkss
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine write_kss_wfgk(kss_unt,ikpt,isppol,kpoint,nspinor,kss_npw,npw_k,kg_k,&
&          nbandksseff,natom,Psps,ene_k,occ_k,rprimd,gbig,wfg,Cprjnk_k,accesswff)

 use defs_basis
 use defs_datatypes
 use m_pawcprj, only : cprj_type

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'write_kss_wfgk'
 use interfaces_51_manage_mpi
 use interfaces_59_io_mpi
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: ikpt,isppol,accesswff,kss_npw,nspinor,kss_unt,nbandksseff
 integer,intent(in) :: natom,npw_k
 type(pseudopotential_type),intent(in) :: Psps
!arrays
 integer,intent(in) :: gbig(3,kss_npw),kg_k(3,npw_k)
 real(dp),intent(in) :: kpoint(3),rprimd(3,3)
 real(dp),intent(in) :: ene_k(nbandksseff),occ_k(nbandksseff)
 real(dp),intent(in) ::  wfg(2,kss_npw*nspinor,nbandksseff)
 type(Cprj_type),intent(in) :: Cprjnk_k(natom,nspinor*nbandksseff*Psps%usepaw)

!Local variables-------------------------------
!scalars
 integer :: ib,ibsp,ig,ispinor,iatom,ii,master,my_rank,ierr
 type(MPI_type) :: MPI_enreg_seq
 character(len=500) :: msg
#if defined HAVE_TRIO_ETSF_IO
 type(wffile_type) :: Wff
#endif
!arrays
 integer,allocatable :: trsl(:)

! *********************************************************************

!* Fake MPI_type for the sequential part.
 call initmpi_seq(MPI_enreg_seq)
 master=0; my_rank=master

 if (Psps%usepaw==0) then ! Calculate and write KB form factors and derivative at this k-point.
  ! The array trsl translates the index of gbig into the corresponding
  ! index in array kg_k. If gbig(ig) does not exist in kg_k, trsl(ig) is set to npw_k+1.
  ABI_ALLOCATE(trsl,(kss_npw))
  call table_gbig2kg(npw_k,kg_k,kss_npw,gbig,trsl,ierr)
  if (ierr/=0.and.(kss_npw>=npw_k)) then
    MSG_ERROR(' The set of g vectors is inconsistent ! Check source.')
  end if
  call write_vkb(kss_unt,ikpt,kpoint,kss_npw,gbig,trsl,rprimd,Psps,accesswff)
  ABI_DEALLOCATE(trsl)
 end if

 ! ============================================================
 ! ==== Write wavefunctions and PAW matrix elements on disk ====
 ! ============================================================
 SELECT CASE (accesswff)

 CASE (IO_MODE_FORTRAN)
   write(kss_unt) (ene_k(ib),ib=1,nbandksseff)

   ibsp=0
   do ib=1,nbandksseff
     write(kss_unt) (wfg(:,ig,ib),ig=1,kss_npw*nspinor)
     if (Psps%usepaw==1) then ! Remember that cprj are unsorted.
       do ispinor=1,nspinor
         ibsp=ibsp+1
         do iatom=1,natom
           ii=Cprjnk_k(iatom,ibsp)%nlmn
           write(kss_unt) (Cprjnk_k(iatom,ibsp)%cp(:,1:ii))
         end do
       end do
     end if
   end do

#if defined HAVE_TRIO_ETSF_IO
 CASE (IO_MODE_ETSF) ! When ETSF, use the rwwf routine (should always be done like that)
   if (Psps%usepaw==1) then
     MSG_ERROR("PAW output with ETSF-IO not implemented")
   end if
   Wff%master   =master
   Wff%me       =my_rank
   Wff%unwff    =kss_unt
   Wff%accesswff=IO_MODE_ETSF
!  MG Tue Oct 23 occ_k is passed to writewf instead of occ to treat correctly metals
!  MG FIXME The RESHAPE below is ugly. Some compiler will likely create a buffer on the stack and then BOOM!
   call writewf(RESHAPE(wfg(:, 1:kss_npw, :),(/2,nbandksseff*kss_npw /)),ene_k,0,&
&   0,ikpt,isppol,gbig(:,1:kss_npw),nbandksseff,nbandksseff*kss_npw,MPI_enreg_seq,&
&   nbandksseff,nbandksseff,kss_npw,nspinor,occ_k(1:nbandksseff),2,1,Wff)
#endif

 CASE DEFAULT
   write(msg,'(a,i0)')" Unsupported accesswff: ",accesswff
   MSG_ERROR(msg)
 END SELECT

 call destroy_mpi_enreg(MPI_enreg_seq)

end subroutine write_kss_wfgk
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/k2gamma_centered
!! NAME
!!  k2gamma_centered
!!
!! FUNCTION
!!  Helper function to translate a set of wavefunctions from the k-centered G-sphere
!!  to the Gamma-centered G-sphere used for GW calculations.
!!
!! INPUTS
!!  npw_k=Number of planewaves in the k-centered basis set.
!!  kss_npw=Number of planewaves in the Gamma-centered G-sphere.
!!  nspinor=Number of spinorial component.
!!  nbandksseff=Number of bands in input-output arrays.
!!  [icg]=Shift to be used when accessing the cg array. 0 if not specified (usually k_index).
!!  [eig_vec(2,npw_k*nspinor,nbandksseff)]=wavefunctions defined on the k-centered G-sphere.
!!  [cg(2,ikg+1:ikg+npw_k*nspinor*nbandksseff)]=wavefunctions defined on the k-centered G-sphere.
!!  ngfft(18)=Info on the FFT.
!!  MPI_enreg<MPI_type>=Structure gathering info on the parallelization.
!!  istwf_k
!!  ecut
!!  gbig(3,kss_npw)
!!  kg_k(3,npw_k)
!!  gmet(3,3)
!!  kpoint(3)
!!
!! OUTPUT
!!  wfg(2,kss_npw*nspinor,nbandksseff)=Wavefunctions in the Gamma-centered representation.
!!
!! NOTES
!!  1) icg is used only if cg is present.
!!  2) cg and eig_vec are mutually exclusive. One and only one can be passed to the routine.
!!
!! PARENTS
!!      outkss
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine k2gamma_centered(kpoint,npw_k,istwf_k,ecut,kg_k,kss_npw,nspinor,nbandksseff,ngfft,gmet,&
&  MPI_enreg,gbig,ug,icg,cg,eig_vec)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'k2gamma_centered'
 use interfaces_52_fft_mpi_noabirule
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: nbandksseff,nspinor,kss_npw,npw_k,istwf_k
 integer,optional,intent(in) :: icg
 real(dp),intent(in) :: ecut
 type(MPI_type),intent(inout) :: MPI_enreg
!arrays
 integer,intent(in) :: gbig(3,kss_npw)
 integer,intent(in) :: kg_k(3,npw_k)
 integer,intent(in) :: ngfft(18)
 real(dp),intent(in) :: gmet(3,3),kpoint(3)
 real(dp),intent(out) :: ug(2,kss_npw*nspinor,nbandksseff)
 real(dp),optional,intent(in) :: eig_vec(2,npw_k*nspinor,nbandksseff)
 real(dp),optional,intent(in) :: cg(:,:)

!Local variables-------------------------------
!scalars
 integer,parameter :: tobox=1,tosph=-1
 integer :: band,ispinor,spinor_shift2,spinor_shift1,ig,my_icg,ierr
 integer :: n1,n2,n3,n4,n5,n6,ndat,full_npw_k,ii
 character(len=500) :: msg
!arrays
 integer :: identity(3,3)=RESHAPE((/1,0,0,0,1,0,0,0,1/),(/3,3/))
 integer :: no_shift(3)=(/0,0,0/)
 integer,pointer :: full_kg_k(:,:)
 integer,allocatable :: trsl(:)
 real(dp),allocatable :: cfft(:,:,:,:)
 real(dp),allocatable :: full_cg(:,:),tmp_cg(:,:)

! *********************************************************************

 if (PRESENT(cg).and.PRESENT(eig_vec)) then
   MSG_ERROR("Both cg and eig_vec are present!")
 end if

! Mapping between the gamma-centered basis set and the k-centered one.
! trsl(ig)=npw_k+1 if vector ig is not inside the k-centered G-sphere.
 ABI_ALLOCATE(trsl,(kss_npw))
 nullify(full_kg_k)

 n1=ngfft(1); n2=ngfft(2); n3=ngfft(3)
 n4=ngfft(4); n5=ngfft(5); n6=ngfft(6)

 if (istwf_k==1) then ! Full k-centered G-sphere.
   call table_gbig2kg(npw_k,kg_k,kss_npw,gbig,trsl,ierr)
   if (ierr/=0.and.(kss_npw>=npw_k)) then
     MSG_ERROR(' The set of G vectors is inconsistent')
   end if

 else  ! Calculate full kg with istwf_k=1 then do the mapping.
   call get_kg(kpoint,1,ecut,gmet,full_npw_k,full_kg_k)

   call table_gbig2kg(full_npw_k,full_kg_k,kss_npw,gbig,trsl,ierr)
   if (ierr/=0.and.(kss_npw>=npw_k)) then
     MSG_ERROR(' The set of G vectors is inconsistent')
   end if
 end if
 !
 ! Branching, depending on optional arguments.
 if (PRESENT(cg)) then
   my_icg=0; if (PRESENT(icg)) my_icg=icg

   SELECT CASE (istwf_k)

   CASE (1)
     do band=1,nbandksseff
       do ispinor=1,nspinor
         spinor_shift1=(ispinor-1)*kss_npw
         spinor_shift2=(ispinor-1)*npw_k
         do ig=1,kss_npw ! Retrieve the correct components
           if (trsl(ig)<=npw_k) then
             ug(:,ig+spinor_shift1,band)=cg(:,trsl(ig)+spinor_shift2+(band-1)*npw_k*nspinor+my_icg)
           else
             ug(:,ig+spinor_shift1,band)=zero
           end if
         end do
       end do
     end do

   CASE (2:9)

     ABI_CHECK(nspinor==1,"nspinor/=1!")
     !
     ! Convert input wfs from reduced to full G-sphere.
     ndat=1
     ABI_ALLOCATE(cfft,(2,n4,n5,n6*ndat))
     ABI_ALLOCATE(full_cg,(2,full_npw_k*ndat))
     ABI_ALLOCATE(tmp_cg,(2,npw_k*ndat))

     !write(std_out,*)"npw_k, full_kg_k",npw_k,full_npw_k

     do band=1,nbandksseff
       ii = (band-1)*npw_k
       tmp_cg = cg(:,my_icg+ii+1:my_icg+ii+npw_k)
       !write(776,*)"band= ",band,tmp_cg !cg(1:,my_icg+1+ii:my_icg+ii+npw_k)

       call sphere(tmp_cg,ndat,npw_k,cfft,n1,n2,n3,n4,n5,n6,kg_k,istwf_k,tobox,MPI_enreg%me_g0,no_shift,identity,one)

       call sphere(full_cg,ndat,full_npw_k,cfft,n1,n2,n3,n4,n5,n6,full_kg_k,1,tosph,MPI_enreg%me_g0,no_shift,identity,one)
       !write(777,*)"band= ",band,full_cg(:,:)

       do ig=1,kss_npw ! Retrieve the correct components
         if (trsl(ig)<=full_npw_k) then
           ug(:,ig,band)=full_cg(:,trsl(ig))
         else
           ug(:,ig,band)=zero
         end if
       end do
     end do !band

     ABI_DEALLOCATE(cfft)
     ABI_DEALLOCATE(tmp_cg)
     ABI_DEALLOCATE(full_cg)

   CASE DEFAULT
     MSG_BUG("Wrong istwf_k")
   END SELECT

 else if (PRESENT(eig_vec)) then

   SELECT CASE (istwf_k)

   CASE (1)
     do band=1,nbandksseff
       do ispinor=1,nspinor
         spinor_shift1=(ispinor-1)*kss_npw
         spinor_shift2=(ispinor-1)*npw_k
         do ig=1,kss_npw ! Retrieve the correct components
           if (trsl(ig)<=npw_k) then
             ug(:,ig+spinor_shift1,band)=eig_vec(:,trsl(ig)+spinor_shift2,band)
           else
             ug(:,ig+spinor_shift1,band)=zero
           end if
         end do
       end do
     end do

   CASE DEFAULT
     write(msg,'(a,i0)')" Unsupported value for istwf_k: ",istwf_k
     MSG_ERROR(msg)
   END SELECT

 else
   MSG_ERROR("neither cg not eig_vec are in input")
 end if

 ABI_DEALLOCATE(trsl)
 if (associated(full_kg_k))  then
   ABI_DEALLOCATE(full_kg_k)
 end if

end subroutine k2gamma_centered
!!***


!----------------------------------------------------------------------

!!****f* m_io_kss/skip_kss_record
!! NAME
!!  skip_kss_record
!!
!! FUNCTION
!!  Skip one or more wavefunction records of the KSS file.
!!
!! INPUTS
!!  kss_unt=Unit of the KSS file.
!!  nrec=Number of records to be skipped.
!!  usepaw=1 if PAW
!!  nspinor=Number of spinor components.
!!  natom=Nuber of atom
!!
!!  OUTPUT
!!   Error status reported by Fortran read statement.
!!
!! SIDE EFFECTS
!!  See description.
!!
!! PARENTS
!!
!! CHILDREN
!!
!! SOURCE

function skip_kss_record(kss_unt,nrec,usepaw,nspinor,natom) result(ios)

 use defs_basis

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'skip_kss_record'
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: kss_unt,usepaw,nspinor,natom,nrec
 integer :: ios

!Local variables-------------------------------
 integer :: ispinor,iatom,irec

! *************************************************************************

 ios=0
 do irec=1,nrec
   read(kss_unt,err=10) ! kss_ugd(1:kss_npw*nspinor)
   if (usepaw==1) then
     do ispinor=1,nspinor
       do iatom=1,natom
         read(kss_unt,err=10) !(Cprj_ibz(iatom,ibsp)%cp
       end do
     end do
   end if
 end do

 RETURN

 10 ios=-1

end function skip_kss_record
!!***

!----------------------------------------------------------------------

!!****f* m_io_kss/make_gvec_kss
!! NAME
!! make_gvec_kss
!!
!! FUNCTION
!!   Build the list of G-vectors using the KSS convention.
!!
!! INPUTS
!!  nkpt=Number of k-points.
!!  nsym=Number of symmetries.
!!  prtvol=Verbosity option.
!!  symmorphi= 
!!    0 : Old (Obsolete) implementation => Suppress inversion from symmetries list
!!    1 : Use input symrel, tnons.
!!  ecut_eff=Effective cutoff
!!  symrel(3,3,nsym)= Symmetry operation in real space.
!!  tnons(3,nsym)=Fractional translations
!!  kptns(3,nkpt)=K-points in reduced coordinates.
!!
!! OUTPUT
!!  npwkss = Input: Initial guess for the number of G-vectors required. Use 0 to have the 
!!           full list of G-vectors that form a closed shell.
!!           Output: Actual number of G-vectors that form a set of closed shells
!!  gvec_kss(:,:) = Input: null pointer. Output: gvec_kss(3,npwkss), list of G-vectors (closed shells)
!!  ierr=Status error
!!
!! PARENTS
!!      setup_screening,setup_sigma
!!
!! CHILDREN
!!      merge_and_sort_kg,remove_inversion,wrtout
!!
!! SOURCE

subroutine make_gvec_kss(nkpt,kptns,ecut_eff,symmorphi,nsym,symrel,tnons,gprimd,prtvol,npwkss,gvec_kss,ierr)

 use defs_basis
 use m_profiling

!This section has been created automatically by the script Abilint (TD).
!Do not modify the following lines by hand.
#undef ABI_FUNC
#define ABI_FUNC 'make_gvec_kss'
 use interfaces_14_hidewrite
 use interfaces_41_geometry
!End of the abilint section

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: nkpt,nsym,prtvol,symmorphi
 integer,intent(out) :: ierr
 integer,intent(inout) :: npwkss
 real(dp),intent(in) :: ecut_eff
!arrays
 integer,intent(in) :: symrel(3,3,nsym)
 integer,pointer :: gvec_kss(:,:)
 real(dp),intent(in) :: tnons(3,nsym),kptns(3,nkpt)
 real(dp),intent(in) :: gprimd(3,3)

!Local variables-------------------------------
!scalars
 integer :: ii,ishm,maxpw,nbase
 integer :: nrst1,nrst2,nsym2,pinv
 integer,pointer :: gbig(:,:)
 character(len=500) :: msg
!arrays
 integer,pointer :: symrel2(:,:,:),shlim(:)
 real(dp),pointer :: tnons2(:,:)
! *********************************************************************

 ierr = 0
 write(msg,'(2a)')ch10,' Sorting g-vecs for an output of states on an unique "big" PW basis.'
 call wrtout(std_out,msg,'COLL')

 !ecut_eff = ecut * Dtset%dilatmx**2  ! Use ecut_eff instead of ecut_eff since otherwise
 !
 !============================================================
 !=== Prepare set containing all G-vectors sorted by stars ===
 !============================================================
 !
 !=== Analyze symmetry operations ===
 if (symmorphi==0) then  ! Old (Obsolete) implementation: Suppress inversion from symmetries list:
   nullify(symrel2,tnons2)
   call remove_inversion(nsym,symrel,tnons,nsym2,symrel2,tnons2,pinv)
   if (ANY(ABS(tnons2(:,1:nsym2))>tol8)) then
     write(msg,'(3a)')&
&     ' Non-symmorphic operations still remain in the symmetries list ',ch10,&
&     ' Program does not stop but _KSS file will not be created...'
     MSG_WARNING(msg)
     ierr=ierr+1 ; RETURN
   end if
 else if (symmorphi==1) then
!  If in the input file symmorphi==1 all the symmetry operations are retained:
!  both identity and inversion (if any) as well as non-symmorphic operations.
   nsym2=nsym ; pinv=1
   ABI_MALLOC(symrel2,(3,3,nsym))
   ABI_MALLOC(tnons2,(3,nsym))
   symrel2(:,:,:)=symrel(:,:,1:nsym)
   tnons2(:,:)   =tnons(:,1:nsym)
 else
   write(msg,'(a,i4,3a)')&
&   ' symmorphi = ',symmorphi,' while it must be 0 or 1',ch10,&
&   ' Program does not stop but KSS file will not be created...'
   MSG_WARNING(msg)
   ierr=ierr+1 ; RETURN
 end if
 !
 !===================================================================
 !==== Merge the set of k-centered G-spheres into a big set gbig ====
 !===================================================================
 !* Vectors in gbig are ordered by shells
 !
 nullify(gbig,shlim)
 call merge_and_sort_kg(nkpt,kptns,ecut_eff,nsym2,pinv,symrel2,gprimd,gbig,prtvol,shlim_p=shlim)

 nbase = SIZE(shlim)   ! Number of independent G in the big sphere.
 maxpw = shlim(nbase)  ! Total number of G"s in the big sphere.
 !
 ! * Determine optimal number of bands and G"s to be written.
 !npwkss=Dtset%npwkss
 if ((npwkss==0).or.(npwkss>=maxpw)) then
   npwkss=maxpw
   write(msg,'(5a)')&
&   ' Since the number of g''s to be written on file',ch10,&
&   ' was 0 or too large, it has been set to the max. value.,',ch10,&
&   ' computed from the union of the sets of G vectors for the different k-points.'
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
 !ishm=bisect(shlim,npwkss)

 if (shlim(ishm)/=npwkss) then
   nrst1=shlim(ishm)
   nrst2=MIN0(shlim(MIN0(ishm+1,nbase)),maxpw)
   if (IABS(npwkss-nrst2)<IABS(npwkss-nrst1)) nrst1=nrst2
   npwkss=nrst1
   if (shlim(ishm)<npwkss) ishm=ishm+1
   write(msg,'(3a)')&
&   ' The number of G''s to be written on file is not a whole number of stars ',ch10,&
&   ' the program set it to the nearest star limit.'
   call wrtout(std_out,msg,'COLL')
 end if

 write(msg,'(a,i5)')' Number of G-vectors is: ',npwkss
 call wrtout(std_out,msg,'COLL')

 ABI_MALLOC(gvec_kss,(3,npwkss))
 gvec_kss = gbig(:,1:npwkss)

 ABI_FREE(gbig)
 ABI_FREE(symrel2)
 ABI_FREE(tnons2)
 ABI_FREE(shlim)

end subroutine make_gvec_kss
!!***

END MODULE m_io_kss
!!***
