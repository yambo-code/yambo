!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF, DV
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
module P2Ym
 !
 use pw_export_module
 use qexml_module
 use qexsd_module
 use pw_data
 use pars,                  ONLY : lchlen,SP,DP
 use electrons,             ONLY : levels
 use R_lattice,             ONLY : bz_samp
 use mod_com2y,             ONLY : verboseIO
 use parallel_m,            ONLY : myid,PP_bcast   
 ! 
#include<memory.h>
 
 !
 character(lchlen) :: index_filename
 character(lchlen) :: dftdata_fmt
 !
 integer, public  :: pw_unit 
 integer, private :: i1,i2,ierr
 !
 ! Memory saving tools
 !
 integer, parameter       :: max_blocksize = 9
 integer                  :: blocksize(max_blocksize)
 !
contains
 !
 !---------------------------------------------------------------------*
 !    Select and open XML files                                        *
 !---------------------------------------------------------------------*
 !
 subroutine pw_init(instr,inf)
   !
   use pars,  ONLY:lchlen
   use com,   ONLY:msg
   character(*)  :: instr,inf
   !
   ! Work Space
   !
   integer           :: rhounit_ 
   character(lchlen) :: lch
   logical           :: lexist

   !
   ! data fmt detection
   !
   call p2y_get_dftdata_fmt(dftdata_fmt)
   !
   select case(trim(dftdata_fmt))
   case("pw_export","qexml","qexsd","qexsd-hdf5")
      !
      call msg('s','detected QE data format: ',trim(dftdata_fmt))
      !
   case default
      call msg('s','detected QE data format: ',trim(dftdata_fmt))
      call errore("p2y_init","QE format not recognised/compatible (was wf_collect set to true?)",1)
   end select
      
   !
   ! settings
   !
   pw_unit = 10
   rhounit_ = 12
   !
   select case (trim(dftdata_fmt))
   case("pw_export")
      !
      index_filename = 'index.xml'
      write(lch,'(a)') '== pw_export generated data =='
      !
   case("qexml")   
      !
      index_filename = 'data-file.xml'
      write(lch,'(a)') '== PWscf > v5.x generated data (QEXML fmt) =='
      !
   case("qexsd-hdf5")   
      !
      index_filename = 'data-file-schema.xml'
      write(lch,'(a)') '== PWscf v.6.x generated data (QEXSD-HDF5 fmt) =='
      !
#if !defined _P2Y_QEXSD_HDF5
      call errore('p2y_init','p2y compiled without QE-HDF5 support',10)
#endif
   case ("qexsd")
      !
      index_filename = 'data-file-schema.xml'
      write(lch,'(a)') '== PWscf v.6.x generated data (QEXSD fmt) =='
      !
   case default
      call errore("p2y_init"," Invalid dftdata fmt "//trim(dftdata_fmt),2)
   end select
   !
   call msg('s',lch)
   !
   ! check again for file existence ?

   !
   ! file opening
   !
   select case (trim(dftdata_fmt))
   case("pw_export")
      ! 
      call pw_openindex(pw_unit,index_filename,ierr=ierr) 
      if (ierr/=0) call errore('p2y_init','opening '//trim(index_filename),10)
      !
   case("qexml")
      !
      call qexml_init(pw_unit) 
      call qexml_openfile( index_filename, "read", .false., ierr)
      if (ierr/=0) call errore('p2y_init','opening '//trim(index_filename),10)
      !
   case("qexsd","qexsd-hdf5")
      !
      call qexsd_init(pw_unit) 
      call qexsd_openfile( index_filename, "read", .false., ierr)
      if (ierr/=0) call errore('p2y_init','opening '//trim(index_filename),10)
      !
      call qexsd_open_section("output",ierr)
      if (ierr/=0) call errore('p2y_init','opening output tag',10)
      !
   case default
      call errore("p2y_init"," Invalid dftdata fmt II "//trim(dftdata_fmt),3)
   end select
   !
   return
 end subroutine pw_init
 !
 !---------------------------------------------------------------------*
 !    Close the XML files                                              *
 !---------------------------------------------------------------------*
 !
 subroutine pw_close
   !
   select case(trim(dftdata_fmt))
   case("pw_export")
      !
      call pw_closeindex(pw_unit)
      !
   case("qexml")
      !
      call qexml_closefile("read",ierr)
      !
   case("qexsd","qexsd-hdf5")
      !
      call qexsd_close_section("output",ierr)
      call qexsd_closefile("read",ierr)
      !
   case default
      call errore("pw_close"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select
   !
   return
 end subroutine pw_close

 !
 !---------------------------------------------------------------------*
 !    Read dimensions                                                  *
 !---------------------------------------------------------------------*
 !
 subroutine get_dimensions(en,k)
   !
   use electrons,             ONLY : default_nel, n_spin, n_sp_pol, &
&                                    n_spinor, n_spin_den, l_spin_orbit
   use R_lattice,             ONLY : ng_vec
   use D_lattice,             ONLY : input_GS_Tel, n_atomic_species
   use wave_func,             ONLY : wf_ncx 
   use com,                   ONLY : msg, error
   use LIVE_t,                ONLY : live_timing_is_on
   use units,                 ONLY : HA2KEL
   type(levels),     intent(out)  :: en     ! Energies
   type(bz_samp),    intent(out)  :: k      ! K/Q points
   !
   ! Call the version dependent routines
   !
   gamma_only_ = .false.
   !
   select case(trim(dftdata_fmt))
   case ("pw_export")
     !
     call pw_dimensions(pw_unit)
     !   
     call qexpt_read_planewaves(pw_unit, ngm=ngm_, nr1=nr1_,nr2=nr2_,nr3=nr3_, ierr=ierr)
     if (ierr/=0) call errore('qexpt_read_planewaves','fmt problem',ABS(ierr))
     !
     ! data not read
     nelec_=0
     nsp_=0
     !
     nr1s_=nr1_
     nr2s_=nr2_
     nr3s_=nr3_
     ngms_=ngm_
     !
   case ("qexml")
     !
     call qexml_read_bands_info(nbnd=nbnd_, num_k_points=num_k_points_, &
&                               nspin=n_spin_pw_, nelec=nelec_, ierr=ierr)
     if (ierr/=0) call errore('qexml_read_bands_info','fmt problem (dims I)',ABS(ierr))
     !
     call qexml_read_symmetry(nsym=nsym_, ierr=ierr)
     if (ierr/=0) call errore('qexml_read_symmetry','fmt problem',ABS(ierr))
     call qexml_read_spin(lspinorb=l_spin_orbit, ierr=ierr)
     if (ierr/=0) call errore('qexml_read_spin','fmt problem',ABS(ierr))
     call qexml_read_planewaves(gamma_only=gamma_only_, npwx=npwx_,&
&                               ngm=ngm_, nr1=nr1_,nr2=nr2_,nr3=nr3_,&
&                               ngms=ngms_, nr1s=nr1s_,nr2s=nr2s_,nr3s=nr3s_, ierr=ierr)
     if (ierr/=0) call errore('qexml_read_planewaves','fmt problem',ABS(ierr))
     call qexml_read_ions(nat=nat_, nsp=nsp_, ierr=ierr)
     if (ierr/=0) call errore('qexml_read_ions','fmt problem',ABS(ierr))
     !
   case ("qexsd","qexsd-hdf5")
     !
     call qexsd_read_band_structure(nbnd=nbnd_, num_k_points=num_k_points_, nspin=n_spin_pw_, &
&                                   nelec=nelec_, ierr=ierr)
     if (ierr/=0) call errore('qexsd_read_band_structure','fmt problem (dims I)',ABS(ierr))
     !
     call qexsd_read_ions(nat=nat_, nsp=nsp_, ierr=ierr)
     if (ierr/=0) call errore('qexsd_read_ions','fmt problem I',ABS(ierr))
     call qexsd_read_symmetry(nsym=nsym_, ierr=ierr)
     if (ierr/=0) call errore('qexsd_read_symmetry','fmt problem',ABS(ierr))
     call qexsd_read_planewaves(gamma_only=gamma_only_, npwx=npwx_,&
&                               ngm=ngm_, nr1=nr1_,nr2=nr2_,nr3=nr3_,&
&                               ngms=ngms_, nr1s=nr1s_,nr2s=nr2s_,nr3s=nr3s_, ierr=ierr)
     if (ierr/=0) call errore('qexsd_read_planewaves','fmt problem I',ABS(ierr))
     call qexsd_read_spin(lspinorb=l_spin_orbit, noncolin=noncolin_, ierr=ierr)
     if (ierr/=0) call errore('qexsd_read_spin','fmt problem',ABS(ierr))
     !
   case default
     call errore("pw_dimensions"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select

   !
   ! other settings
   !
   default_nel = nelec_
   n_atomic_species = nsp_     

   k%nibz      = num_k_points_
   en%nb       = nbnd_
   !
   ! YAMBO presently does not make use of GAMMA_ONLY option, hence
   ! the wfc's and G's must be doubled in this case.
   ! Note: the quantities in the PW files are still dimensioned ngm_ and npwx_
   !
   if(gamma_only_) then
     ng_vec      = 2*(ngm_ -1) + 1
     wf_ncx      = 2*(npwx_-1) + 1
   else
     ng_vec      = ngm_
     wf_ncx      = npwx_ 
   endif
   !
   ! Set miscellanous YAMBO data: dimensions
   !
   n_spin_den = n_spin_pw_
   select case(n_spin_pw_)
   case(1)
     n_sp_pol  = 1
     n_spinor  = 1
     n_spin    = 1
   case(2)
     n_sp_pol  = 2
     n_spinor  = 1
     n_spin    = 2
   case(4)
     n_sp_pol  = 1
     n_spinor  = 2
     n_spin    = 2
   end select
   !
   input_GS_Tel = 0.d0
   !
   select case(trim(dftdata_fmt))
   case ("qexml")
     !
     call qexml_read_occ(lgauss=lgauss_,ngauss=ngauss_,degauss=degauss_,&
&                               degauss_units=degauss_units_, ierr=ierr)
     if (ierr/=0) call errore('qexml_read_occ','IOTK problem',ABS(ierr))  
     !
     if (lgauss_) input_GS_Tel=300._SP/HA2KEL
     !
   case ("qexsd","qexsd-hdf5")
     !
     call qexsd_read_band_structure(have_smearing=lgauss_,smearing_type=smearing_type_,&
&                                   degauss=degauss_,ierr=ierr)
     if (ierr/=0) call errore('qexsd_read_band_structure','fmt problem II',ABS(ierr))  
     !
     degauss_units_="Hartree"
     !
     if (lgauss_) input_GS_Tel=300._SP/HA2KEL
     !
   end select
   !
   return
 end subroutine get_dimensions
 !
 !---------------------------------------------------------------------*
 !    Read atomic data                                                 *
 !---------------------------------------------------------------------*
 !
 subroutine get_atoms
   use D_lattice,   ONLY:n_atoms_species_max,n_atomic_species,n_atoms_species, &
&                        atom_pos, Z_species,atomic_number
   ! 
#include<memory.h>
   !
   ! Work Space
   !
   real(DP)                       :: tau_units
   real(SP)                       :: z
   integer                        :: i1,i2,u
   !
   YAMBO_ALLOC(ityp_,(nat_))
   YAMBO_ALLOC(tau_,(3,nat_))

   select case(trim(dftdata_fmt))
   case("pw_export")
      !
      allocate(atom_type_(nat_))
      allocate(species_type_(nat_))
      !
      call pw_atoms(pw_unit)
      !
      n_atomic_species = nsp_     ! n_atom_species only read here
      allocate(atm_(n_atomic_species))
      atm_(1:n_atomic_species) = species_type_(1:n_atomic_species)
      do i1=1, nat_
         ityp_(i1) = -1
         do i2=1,n_atomic_species
            if(trim(atom_type_(i1))==trim(atm_(i2))) then
              ityp_(i1) = i2
            endif
         enddo
      enddo
      tau_units = alat_
      !
      deallocate(atom_type_)
      deallocate(species_type_)
      !
   case("qexml")
      !
      tau_units = 1.0_DP
      allocate(atm_(n_atomic_species))
      allocate(psfile(n_atomic_species))
      YAMBO_ALLOC(nmesh,(n_atomic_species))
      YAMBO_ALLOC(nbeta,(n_atomic_species))
      !
      call qexml_read_ions( atm=atm_, ityp=ityp_, psfile=psfile, tau=tau_, ierr=ierr)
      if (ierr/=0) call errore('qexml_read_ions','fmt problem',ABS(ierr))  
      !
   case("qexsd","qexsd-hdf5")
      !
      tau_units = 1.0_DP
      allocate(atm_(n_atomic_species))
      allocate(psfile(n_atomic_species))
      YAMBO_ALLOC(nmesh,(n_atomic_species))
      YAMBO_ALLOC(nbeta,(n_atomic_species))
      !
      call qexsd_read_ions( atm=atm_, ityp=ityp_, psfile=psfile, tau=tau_, ierr=ierr)
      if (ierr/=0) call errore('qexsd_read_ions','fmt problem II',ABS(ierr))
      !
   case default
     call errore("get_atoms"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select
   !
   ! init
   !
   YAMBO_ALLOC(n_atoms_species,(n_atomic_species))
   n_atoms_species(:)=0
   do i1 = 1, nat_
     n_atoms_species( ityp_(i1) ) = n_atoms_species( ityp_(i1) ) +1
   enddo
   n_atoms_species_max = maxval(n_atoms_species)
   YAMBO_ALLOC(atom_pos,(3,n_atoms_species_max,n_atomic_species))
   n_atoms_species(:)=0
   do i1 = 1, nat_
     n_atoms_species( ityp_(i1) ) = n_atoms_species( ityp_(i1) ) +1
     atom_pos(:, n_atoms_species( ityp_(i1) ) , ityp_(i1) ) = tau_(:,i1)*tau_units
   enddo
   !
   YAMBO_ALLOC(Z_species,(n_atomic_species))
   do i1 = 1, n_atomic_species
     Z_species(i1) = atomic_number(atm_(i1))
   enddo
   !
   return
 end subroutine get_atoms
 !
 !---------------------------------------------------------------------*
 !    Read cell data                                                   *
 !---------------------------------------------------------------------*
 !
 subroutine get_cell
   use pars,                  only : pi
   use R_lattice,             ONLY : bz_samp, ng_vec
   use D_lattice,             ONLY : DL_vol, a, alat,lattice
   use mod_com2y,             ONLY : alat_mult_factor
   use vec_operate,           ONLY :  define_b_and_DL_vol
    
   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      call pw_cell(pw_unit)
      !
   case ("qexml")
      !
      call qexml_read_cell(alat=alat_, a1=a1_, a2=a2_, a3=a3_,ierr=ierr)
      if (ierr/=0) call errore('qexml_read_cell','fmt problem',ABS(ierr))
      ! 
   case ("qexsd","qexsd-hdf5")
      !
      call qexsd_read_cell(alat=alat_, a1=a1_, a2=a2_, a3=a3_,ierr=ierr)
      if (ierr/=0) call errore('qexsd_read_cell','fmt problem',ABS(ierr))
      !
   case default
     call errore("get_cell"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select
   !
   a(1,:) = a1_(:) ! assumes always atomic units
   a(2,:) = a2_(:)
   a(3,:) = a3_(:)
   !
   ! Set related YAMBO data: cell
   !
   alat(1) = maxval(abs(a(1,:)))*alat_mult_factor
   alat(2) = maxval(abs(a(2,:)))*alat_mult_factor
   alat(3) = maxval(abs(a(3,:)))*alat_mult_factor
   call crystal_lattice()
   call define_b_and_DL_vol()
   !
   return
 end subroutine get_cell
 !
 !---------------------------------------------------------------------*
 !    Read symmetries                                                  *
 !---------------------------------------------------------------------*
 !
 subroutine get_symmetries
   use pars
   use mod_com2y,  ONLY : symmetries_check_and_load,force_noWFs
   use com,        ONLY : warning,error
   use vec_operate,ONLY : v_is_zero
   use D_lattice,  ONLY : a
   use R_lattice,  ONLY : b

   real(DP) :: trasl_(3,48)
   real     :: rmat1(3,3),rmat2(3,3)
   logical  :: warn=.FALSE.

   trasl_=0._SP

   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      call pw_symmetry(pw_unit)
      trevsym_=.true.
      t_rev_=0
      !
   case ("qexml")
      !
      call qexml_read_symmetry(invsym=invsym_, trevsym=trevsym_, trasl=trasl_(:,1:nsym_), &
&                              s=isym_(:,:,1:nsym_), t_rev=t_rev_ , ierr=ierr)
      if (ierr/=0) call errore('qexml_read_symmetry','fmt problem',ABS(ierr))
      !
   case ("qexsd","qexsd-hdf5")
      !
      call qexsd_read_symmetry(invsym=invsym_, trevsym=trevsym_, trasl=trasl_(:,1:nsym_), &
&                              s=rsym_(:,:,1:nsym_), t_rev=t_rev_ , ierr=ierr)
      if (ierr/=0) call errore('qexml_read_symmetry','fmt problem',ABS(ierr))
      !
      ! convert to integers
      !
      do i1 = 1, nsym_
         isym_(:,:,i1) = nint(rsym_(:,:,i1))
      enddo
      !
      ! definition of t_rev & trevsym  need to be further checked in qexsd_p2y.F
      !
      call qexsd_rewind(ierr)
      if (ierr/=0) call errore('qexml_read_symmetry','iduring qexsd_rewind',ABS(ierr))
      !
      call qexsd_open_section("input",ierr) 
      if (ierr/=0) call errore('qexml_read_symmetry','opening sec input locally',ABS(ierr))
      !
      call qexsd_read_symmetry_input(noinv=noinv_,nosym=nosym_,ierr=ierr)
      if (ierr/=0) call errore('qexml_read_symmetry','reading input data',ABS(ierr))
      !
      call qexsd_close_section("input",ierr) 
      if (ierr/=0) call errore('qexml_read_symmetry','closing sec input locally',ABS(ierr))
      !
      call qexsd_open_section("output",ierr) 
      if (ierr/=0) call errore('qexml_read_symmetry','opening sec output locally',ABS(ierr))
      !
      trevsym_=trevsym_.and..not.noinv_
      !
   case default
     call errore("get_symmetries"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select
   !
   ! postporoc
   !
   do i1=1,nsym_
     if (.not.v_is_zero(real(trasl_(:,i1),SP)) ) then 
       warn=.TRUE.
       if (.not.force_noWFs) then
         call error(' Non-symmorphic symmetry operations are not supported! Use force_symmorphic=.true. in PWSCF')
       endif
     endif
   enddo
   !
   ! Note that invsym_ is well defined here, could be used for checks.
   do i1 = 1,nsym_
     isym_(:,:,i1) = transpose(isym_(:,:,i1))
   enddo
   call symmetries_check_and_load(isym_(:,:,1:nsym_),nsym_,trevsym=trevsym_, t_rev=t_rev_)
   !
   if (warn) call warning(' Non-symmorphic symmetry operations are not supported!')
   !
 end subroutine get_symmetries
 !
 !---------------------------------------------------------------------*
 !    Read K-point data                                                *
 !---------------------------------------------------------------------*
 !
 subroutine get_k_points(k)
   ! 
   use R_lattice,   ONLY:bz_samp
   use D_lattice,   ONLY:alat
   use vec_operate, ONLY:v_is_zero
   use com,         ONLY:warning
#include<memory.h>
   type(bz_samp) :: k
   !
   YAMBO_ALLOC(xk_,(3,k%nibz))

   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      call pw_kpoints(pw_unit)
      !
   case ("qexml")
      !
      call qexml_read_bz(xk=xk_, ierr=ierr)
      if (ierr/=0) call errore('qexml_read_bz','IOTK error',ABS(ierr))
      !
   case ("qexsd","qexsd-hdf5")
      !
      call qexsd_read_band_structure(vkpt=xk_, ierr=ierr)
      if (ierr/=0) call errore('qexsd_read_band_structure','fmt error (KPT)',ABS(ierr))
      !
   case default
     call errore("get_k_points"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select

   !
   ! PW k in units of [cart, tpiba] -> units of [cart, 2*pi/alat(:)]
   ! PW cart tpiba/cart au/cart alat/RLU units
   !
   YAMBO_ALLOC(k%pt,(k%nibz,3))
   do i1=1,k%nibz
     k%pt(i1,:)=xk_(:,i1) * alat(:)/alat_ 
   enddo
   !
   if (trim(dftdata_fmt)=="pw_export") then
     ! 
     if(k%nibz==1.and.v_is_zero(k%pt(1,:))) then
        call warning(' GAMMA_ONLY calculations are not supported in pw_export. ')
     endif
     !
   endif
   !
   return
 end subroutine get_k_points
 !
 !---------------------------------------------------------------------*
 !    Read miscellaneous data                                          *
 !---------------------------------------------------------------------*
 !
 subroutine get_more
   use electrons,             ONLY : default_nel
   !
   if (trim(dftdata_fmt)=="pw_export") then
     call pw_other(pw_unit)
     default_nel = nelec_ 
   endif
   !
   return
 end subroutine get_more
 !
 !---------------------------------------------------------------------*
 !    Read reciprocal lattice vectors                                  *
 !---------------------------------------------------------------------*
 !
 subroutine get_R_vectors
   ! 
   use pars,        only: pi
   use R_lattice,   ONLY: b, ng_vec, g_vec
   use D_lattice,   ONLY: alat
#include<memory.h>
   !
   YAMBO_ALLOC(g_vec,(ng_vec,3)) ! The YAMBO array
   !
   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      YAMBO_ALLOC(igv_,(3,ng_vec))
      call pw_gvectors(pw_unit)
      !
   case ("qexml")
      !
      YAMBO_ALLOC(igv_,(3,ngm_)) ! The PWscf array (ngm = ng_vec if not gamma_only)
      call qexml_read_planewaves(ecutwfc=ecutwfc_, ecutrho=ecutrho_,igv=igv_, ierr=ierr)
      if (ierr/=0) call errore('qexml_read_planewaves','fmt error',ABS(ierr))
      !
   case ("qexsd","qexsd-hdf5")
      !
      YAMBO_ALLOC(igv_,(3,ngm_)) ! The PWscf array (ngm = ng_vec if not gamma_only)
      call qexsd_read_planewaves(ecutwfc=ecutwfc_, ecutrho=ecutrho_, igv=igv_, ierr=ierr)
      if (ierr/=0) call errore('qexsd_read_planewaves','fmt error II',ABS(ierr))
      !
   case default
     call errore("get_R_vectors"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select
   
   !
   ! PW integer units of b1/b2/b3    -> 2pi/alat(:) units, cartesian, real
   ! b(:,:) is in a.u.
   !
   if(gamma_only_) then
     !
     g_vec(1,:)=matmul(transpose(b),igv_(:,1))*alat(:)/2.0_SP/pi
     do i1 = 2,ngm_
        g_vec(2*i1-2,:)  = matmul(transpose(b),igv_(:,i1))*alat(:)/2.0_SP/pi
        g_vec(2*i1-1,:)  =-matmul(transpose(b),igv_(:,i1))*alat(:)/2.0_SP/pi
     enddo
     !
   else
     !
     do i1 = 1, ng_vec
       g_vec(i1,:)=matmul(transpose(b),igv_(:,i1))*alat(:)/2.0_SP/pi ! ok
     enddo
     !
   endif
   !
 end subroutine get_R_vectors

 !
 !---------------------------------------------------------------------*
 !    Read IGK arrays                                                  *
 !---------------------------------------------------------------------*
 !
 subroutine get_IGK(k)
   use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng
   use R_lattice,             ONLY : bz_samp, ng_vec
   use mod_com2y,             ONLY : force_noWFs
   use com,                   ONLY : msg, error
   character(lchlen) :: lch
   type(bz_samp) :: k
   integer i1,ik,nfft_tmp(3)
   !
   YAMBO_ALLOC(wf_nc_k,(k%nibz))

   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      YAMBO_ALLOC(pw_igk_,(wf_ncx,k%nibz))
      YAMBO_ALLOC(pw_npwk_,(k%nibz))
      call pw_igkindex(pw_unit)
      wf_nc_k(:) = pw_npwk_(:)
      YAMBO_FREE(pw_npwk_)
      !
   case ("qexml")
      !
      YAMBO_ALLOC(pw_igk_,(npwx_,k%nibz))
      YAMBO_ALLOC(index_,(npwx_))
      do ik = 1, k%nibz
        call qexml_read_gk(ik, npwk=npwk_, index=index_, ierr=ierr)
        if (ierr/=0) call errore('qexml_read_gk','IOTK error',ABS(ierr))
        !
        pw_igk_(1:npwk_, ik)= index_(1:npwk_) 
        wf_nc_k(ik) = npwk_
      enddo
      YAMBO_FREE(index_)
      ! 
   case ("qexsd","qexsd-hdf5")
      !
      YAMBO_ALLOC(pw_igk_,(npwx_,k%nibz))
      YAMBO_ALLOC(index_,(npwx_))
      YAMBO_ALLOC(igkv_,(3,npwx_))
      !
      if (.not.allocated(igv_)) call errore('qexsd_read_gk','igv_ not alloc',10)
      !
      nfft_tmp=0   ! redefined in qexsd_fft2igv_map
      !
      call qexsd_fft2igv_map(ngm_, igv_, nfft_tmp, dims_only=.TRUE.)
      YAMBO_ALLOC(fft2igv_,(product(nfft_tmp)))
      call qexsd_fft2igv_map(ngm_, igv_, nfft_tmp, fft2igv_)
      !
      if (n_spin_pw_/=1.and.n_spin_pw_/=2.and.n_spin_pw_/=4) &
&               call errore('qexsd_read_gk','invalid n_spin_pw_',10)
      !
      do ik = 1, k%nibz
        !
        call qexsd_read_gk(ik, n_spin_pw_, npwk=npwk_, igkv=igkv_, ierr=ierr)
        if (ierr/=0) call errore('qexsd_read_gk','fmt error',ABS(ierr))
        !
        call qexsd_igk_map( nfft_tmp, fft2igv_, npwk_, igkv_, index_ )
        !
        pw_igk_(1:npwk_, ik)= index_(1:npwk_) 
        wf_nc_k(ik) = npwk_
        !
      enddo
      !
      YAMBO_FREE(index_)
      YAMBO_FREE(igkv_)
      YAMBO_FREE(fft2igv_)
      ! 
   case default
     call errore("get_igk"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select

   YAMBO_ALLOC(wf_igk,(wf_ncx,k%nibz))
   wf_igk(:,:)=-1 ! for checking

   if(gamma_only_) then
     do i1 = 1,k%nibz
        wf_igk(1,i1)=pw_igk_(1,i1)
        do i2=2,wf_nc_k(i1)
           wf_igk(2*i2-2,i1)=pw_igk_(i2,i1)*2-2
           wf_igk(2*i2-1,i1)=pw_igk_(i2,i1)*2-1
        enddo
        !
        ! NB: Extension of wf_nc_k array also done here.
        !
        wf_nc_k(i1)=2*(wf_nc_k(i1)-1)+1
     enddo
   else
     do i1 = 1,k%nibz
       wf_igk(1:wf_nc_k(i1),i1)= pw_igk_(1:wf_nc_k(i1),i1) 
     enddo   
   endif
   YAMBO_FREE(pw_igk_)

   wf_ng = maxval(wf_igk)
   !
   ! Inportant check on wf_ncx.
   !
   if(maxval(wf_nc_k)/=wf_ncx) then
     call error('maxval(wf_nc_k) /= wf_ncx ! Check MAX_NPW/MAX_NUMBER_GK_VECTORS ')
   endif
   if(any(wf_nc_k==0)) call error('At least one npw(k) = 0!')
    
 end subroutine get_IGK
 !
 !---------------------------------------------------------------------*
 !    Read eigenvalues                                                 *
 !---------------------------------------------------------------------*
 !
 subroutine get_energies(en,k)
   !
   use electrons,  ONLY : levels, n_sp_pol
   type(bz_samp) :: k
   integer      :: ik_,ispin_
   type(levels) :: en
   !
   YAMBO_ALLOC(en%E,(en%nb,k%nibz,n_sp_pol))

   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      YAMBO_ALLOC(eig_,(en%nb,k%nibz))
      call pw_eigenvalues(pw_unit)
      !
      ! pw_export in Hartree
      en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )/2.0_DP
      ! 
   case ("qexml")
      ! 
      YAMBO_ALLOC(eigb_,(en%nb))
      select case(n_sp_pol)
      case(2)
        do ik_ = 1,k%nibz
          do ispin_ = 1, n_sp_pol
            call qexml_read_bands(ik=ik_,ispin=ispin_,eig=eigb_, ierr=ierr)
            if (ierr/=0) call errore('qexml_read_bands','IOTK error 2',ABS(ierr))
            en%E(:,ik_,ispin_) = eigb_( 1:en%nb )
          enddo
        enddo
      case(1)
        do ik_ = 1,k%nibz
           call qexml_read_bands(ik=ik_,eig=eigb_, ierr=ierr)
           if (ierr/=0) call errore('qexml_read_bands','IOTK error 1',ABS(ierr))
           en%E(:,ik_,1) = eigb_( 1:en%nb )
        enddo
      end select
      YAMBO_FREE(eigb_)
      !
   case ("qexsd","qexsd-hdf5")
      ! 
      YAMBO_ALLOC(eig_s_,(en%nb,k%nibz,n_sp_pol))
      !
      call qexsd_read_band_structure(eig=eig_s_,ierr=ierr)
      if (ierr/=0) call errore('qexsd_read_band_structure','fmt error',ABS(ierr))
      !
      en%E(:,:,:)=eig_s_(:,:,:)
      YAMBO_FREE(eig_s_)
      !
   case default
     call errore("get_energies"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select
   !  
 end subroutine get_energies
 !
 !---------------------------------------------------------------------*
 !    Read XC functional                                               *
 !---------------------------------------------------------------------*
 !
 subroutine get_xc
   use com,            ONLY: warning,error
   use xc_functionals, ONLY: GS_xc_FUNCTIONAL, GS_xc_KIND, GS_exx_FRACTION, GS_exx_SCREENING,&
&                            XC_FACTOR,XC_HYB_GGA_XC_GAUPBE,XC_HYB_GGA_XC_PBEH,XC_HYB_GGA_XC_B3LYP      
   use mod_xc2y,       ONLY: XC_yamboID,  XC_yamboID2kind
   implicit none
   select case (trim(dftdata_fmt))
   case ("pw_export")
      !
      ! doing nothing here (for the moment)
      !
   case ("qexml")
      !
      call qexml_read_xc(dft=pw_dft, lda_plus_u=pw_lda_plus_u, ierr=ierr)
      if (ierr/=0) call errore('qexml_read_xc','IOTK error',abs(ierr))
      !
      if (trim(pw_dft)=="B3LYP") call error('For B3LYP calculation use input_dft=B3LYP-V1R')
      if(pw_lda_plus_u) call warning(' LDA+U. Hubbard correction is not considered in yambo.')
      GS_xc_FUNCTIONAL = XC_yamboID('pwscf_',pw_func=pw_dft)
      if (GS_xc_FUNCTIONAL.eq.XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) then
        GS_xc_KIND = 3
      else
      !
        GS_xc_KIND       = XC_yamboID2kind(GS_xc_FUNCTIONAL)
      endif
      call qexml_read_exx(exx_fraction=pw_exx_fraction, &
&                         screening_parameter=pw_screening_parameter, & 
&                         exx_is_active=pw_exx_is_active, exxdiv_treatment=pw_exxdiv_treatment,ierr=ierr)
      if (ierr==0) then
         if (pw_exx_is_active) then
           if(GS_xc_FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR.or.&
           &  GS_xc_FUNCTIONAL==XC_HYB_GGA_XC_PBEH*XC_FACTOR.or.& 
           &  GS_xc_FUNCTIONAL==XC_HYB_GGA_XC_B3LYP*XC_FACTOR.or.GS_xc_FUNCTIONAL==0) then
             if(pw_exxdiv_treatment/='vcut_ws'.and.pw_exxdiv_treatment/='vcut_spherical') &
                &  call error('Hybrids and HF allowed only with vcut_ws/spherical divergence treatment')
             GS_exx_FRACTION = pw_exx_fraction
             GS_exx_SCREENING= pw_screening_parameter
             if (GS_xc_FUNCTIONAL.eq.XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) GS_exx_SCREENING=0.15 
           else
             call error('Hybrid functional: Only HF,PBE0, B3LYP and GAUPBE allowed')
           endif
         endif
      endif
      !
   case ("qexsd","qexsd-hdf5")
      !
      call qexsd_read_xc(dft=pw_dft, dft_is_hubbard=pw_lda_plus_u, &
&                        dft_is_hybrid=pw_exx_is_active, ierr=ierr)
      if (ierr/=0) call errore('qexsd_read_xc','fmt error',abs(ierr))
      !
      if(pw_lda_plus_u) call warning(' LDA+U. Hubbard correction is not considered in yambo.')
      GS_xc_FUNCTIONAL = XC_yamboID('pwscf_',pw_func=pw_dft)
      if (GS_xc_FUNCTIONAL.eq.XC_HYB_GGA_XC_GAUPBE*XC_FACTOR) then
        GS_xc_KIND = 3
      else
        GS_xc_KIND       = XC_yamboID2kind(GS_xc_FUNCTIONAL)
      endif
      !
      if (pw_exx_is_active) then
         !
         if(GS_xc_FUNCTIONAL==XC_HYB_GGA_XC_GAUPBE*XC_FACTOR.or.&
         &  GS_xc_FUNCTIONAL==XC_HYB_GGA_XC_PBEH*XC_FACTOR.or.&
         &  GS_xc_FUNCTIONAL==XC_HYB_GGA_XC_B3LYP*XC_FACTOR.or.GS_xc_FUNCTIONAL==0) then
           call qexsd_read_exx(exx_fraction=pw_exx_fraction,screening_parameter=pw_screening_parameter,&
           &                   exxdiv_treatment=pw_exxdiv_treatment, ierr=ierr)
           if (ierr/=0) call errore('qexsd_read_exx','fmt error',abs(ierr))
           if(pw_exxdiv_treatment/='vcut_ws'.and.pw_exxdiv_treatment/='vcut_spherical') &
             &  call error('Hybrids and HF allowed only with vcut_ws/spherical divergence treatment')
           GS_exx_FRACTION = pw_exx_fraction
           GS_exx_SCREENING= pw_screening_parameter
           if (GS_xc_FUNCTIONAL.eq.XC_HYB_GGA_XC_GAUPBE*XC_FACTOR)  GS_exx_SCREENING=0.15
         !
         else
           call error('Hybrid functional: Only HF,PBE0,B3LYP and GAUPBE allowed')
         endif
      endif
      !
   case default
     call errore("get_xc"," Invalid dftdata fmt "//trim(dftdata_fmt),1)
   end select

 end subroutine get_xc
 !
 subroutine p2y_get_dftdata_fmt(dftdata_fmt)
    implicit none
    character(len=*) :: dftdata_fmt

    dftdata_fmt="unknown"
    !
    if (myid==0) then
      !
      if ( fmt_is_qexml(dirname="./") ) then
        !
        dftdata_fmt="qexml"
        !
      elseif ( fmt_is_qexsd(dirname="./") ) then
        !
        dftdata_fmt="qexsd"
        !
      elseif ( fmt_is_qexsd_hdf5(dirname="./") ) then
        !
        dftdata_fmt="qexsd-hdf5"
        !
      elseif ( fmt_is_pwexport(dirname="./") ) then
        !
        dftdata_fmt="pw_export"
        !
      endif
    endif
    !  
    call PP_bcast(dftdata_fmt,0)
    return 
    !
 end subroutine p2y_get_dftdata_fmt
 !
end module P2Ym
