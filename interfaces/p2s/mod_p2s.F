!
! Copyright (C) 2000-2005 A. Marini and the SELF team
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
module P2S
 !
#if defined _P2S_EXPORT
 use pw_export
#else 
 use qexml_module
#endif
 use pw_data
 use pars,                  ONLY : lchlen,SP
 use electrons,             ONLY : levels
 use R_lattice,             ONLY : bz_samp
 ! 
 implicit none
 !
 logical :: verboseIO
 character(lchlen) :: index_filename
 !
 integer, public :: pw_unit 
 integer, private :: i1,ierr
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
   ! index filenames
   !
#if defined _P2S_EXPORT

   index_filename = 'index.xml'

#elif defined _P2S_V31

   index_filename = 'data-file.xml'
   pwversion = 311 ! default
   if(index(instr,'31').ne.0)  pwversion = 31
   if(index(instr,'311').ne.0) pwversion = 311
   write(lch,'(a,i3,a)') '== PWscf v.',pwversion,' generated data =='

#elif defined _P2S_V32

   index_filename = 'data-file.xml'
   write(lch,'(a,i3,a)') '== PWscf v.3.2 generated data =='

#endif

   if (len_trim(inf)>0) index_filename = inf

   call msg('s','Index file set to ',trim(index_filename))
   inquire(file=trim(index_filename),exist=lexist)
   if(.not.lexist) then
     call msg('s','Index file not found! Check p2s version...')
     stop ' '
   endif
   !
   ! Open XML index files and units
   !
   pw_unit = 10
#if defined _P2S_EXPORT
    
   call msg('s','== pw_export generated data ==')
   call pw_openindex(pw_unit,index_filename) ! open index.xml file
    
#elif defined _P2S_V31 

   call msg('s',trim(lch))
   rhounit_ = 12
   call qexml_init(pw_unit,rhounit_,.true.) ! sets unit numbers
   call qexml_openfile( index_filename, "read", .false., ierr)

#elif defined _P2S_V32

   rhounit_ = 12
   call qexml_init(pw_unit,"./", rhounit_,.true.) ! sets unit numbers
   call qexml_openfile( index_filename, "read", .false., ierr)
    
#endif
   !
   !  qexml_init(_,_,.true.)               : rho file is binary
   !  qexml_openfile(_,"read"/"write",_,_) : read only from existing files
   !  qexml_openfile(_,_,.false.,_)        : data-file.xml is not binary
   !
 end subroutine pw_init
 !
 !---------------------------------------------------------------------*
 !    Close the XML files                                              *
 !---------------------------------------------------------------------*
 !
 subroutine pw_close
   !
#if defined _P2S_EXPORT

   call pw_closeindex(pw_unit) ! close index.xml file

#elif defined _P2S_V31 || defined _P2S_V32

   call qexml_closefile("read",ierr) ! close index.xml file

#endif
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
&                                    n_spinor
   use R_lattice,             ONLY : ng_vec
   use D_lattice,             ONLY : default_Tel, n_atom_species
   use wave_func,             ONLY : wf_ncx 
   use com,                   ONLY : msg, error
   type(levels),     intent(out)  :: en     ! Energies
   type(bz_samp),    intent(out)  :: k      ! K/Q points
   !
   ! Call the version dependent routines
   !
#if defined _P2S_EXPORT

   call pw_dimensions(pw_unit)
    
#elif defined _P2S_V31

   call qexml_read_bands(nbnd=nbnd_, num_k_points=num_k_points_, &
&                        nspin=n_spin_pw_, nelec=nelec_, ierr=ierr)

#elif defined _P2S_V32

   call qexml_read_bands_info(nbnd=nbnd_, num_k_points=num_k_points_, &
&                        nspin=n_spin_pw_, nelec=nelec_, ierr=ierr)

#endif

   if (ierr.ne.0) then
     call msg('s','Error reading data: most likely you are using an incompatible')
     call msg('s','version of p2s with your data (3.1/3.1.1/3.2).')
     call msg('s','Try -q 31, or compile a compatible version of p2s.')
     call errore('qexml_read_bands.','IOTK error:',ABS(ierr)) 
   endif

#if defined _P2S_V31 || _P2S_V32

   call qexml_read_symmetry(nsym=nsym_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))
   call qexml_read_spin(lspinorb=lspinorb_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_spin','IOTK error',ABS(ierr))
   call qexml_read_planewaves(npwx=npwx_,ngm=ngm_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))
   call qexml_read_ions(nat=nat_, nsp=nsp_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_ions','IOTK error',ABS(ierr))
   !
   default_nel = nelec_
   n_atom_species = nsp_     

#endif

   k%nibz      = num_k_points_
   en%nb       = nbnd_
   ng_vec      = ngm_
   wf_ncx      = npwx_ 
   !
   ! Set miscellanous SELF data: dimensions
   !
   select case(n_spin_pw_)
   case(1)
     n_sp_pol  = 1
     n_spinor  = 1
     n_spin    = 1
! -- GPL_END --
   case(2)
     n_sp_pol  = 2
     n_spinor  = 1
     n_spin    = 2
   case(4)
     n_sp_pol  = 1
     n_spinor  = 2
     n_spin    = 2
! -- GPL_START --
   end select
   default_Tel = 0.d0

   !
   return
 end subroutine get_dimensions
 !
 !---------------------------------------------------------------------*
 !    Read atomic data                                                 *
 !---------------------------------------------------------------------*
 !
 subroutine get_atoms
   use D_lattice,   ONLY:n_atoms_max,n_atom_species,n_atoms, &
&                        atom_pos, znucl
   !
   ! Work Space
   !
   integer, allocatable           :: ityp_(:)
   integer, external              :: atomic_number
   real(DP)                       :: tau_units
   integer                        :: i1,i2

   allocate( ityp_(nat_) )
   allocate( tau_(3,nat_) )  

#if defined _P2S_EXPORT

   allocate(atom_type_ ( nat_), stat=ierr)
   allocate(species_type_ ( nat_), stat=ierr) 

   call pw_atoms(pw_unit)

   n_atom_species = nsp_     ! n_atom_species only read here
   allocate( atm_(n_atom_species)) 
   atm_(1:n_atom_species) = species_type_(1:n_atom_species)
   do i1=1, nat_
     ityp_(i1) = -1
     do i2=1,n_atom_species
       if(trim(atom_type_(i1))==trim(atm_(i2))) then
         ityp_(i1) = i2
       endif
     enddo
   enddo
   tau_units = alat_

   deallocate(atom_type_,species_type_)

#elif defined _P2S_V31 || _P2S_V32

   tau_units = 1.0_DP
   allocate( atm_(n_atom_species))
   call qexml_read_ions( tau=tau_, ityp=ityp_, atm=atm_, ierr=ierr)

#endif

   allocate(n_atoms(n_atom_species))
   n_atoms(:)=0
   do i1 = 1, nat_
     n_atoms( ityp_(i1) ) = n_atoms( ityp_(i1) ) +1
   enddo
   n_atoms_max = maxval(n_atoms)
   allocate(atom_pos(3,n_atoms_max,n_atom_species))
   n_atoms(:)=0
   do i1 = 1, nat_
     n_atoms( ityp_(i1) ) = n_atoms( ityp_(i1) ) +1
     atom_pos(:, n_atoms( ityp_(i1) ) , ityp_(i1) ) = tau_(:,i1)*tau_units
   enddo

   allocate(znucl(n_atom_species))
   do i1 = 1, n_atom_species
     znucl(i1) = atomic_number(atm_(i1))
   enddo

   deallocate(ityp_, tau_)

   return
 end subroutine get_atoms
 !
 !---------------------------------------------------------------------*
 !    Read cell data                                                   *
 !---------------------------------------------------------------------*
 !
 subroutine get_cell
   use pars,                  only : pi
   use R_lattice,             ONLY : bz_samp, ng_vec, b
   use D_lattice,             ONLY : DL_vol, a, alat
   use interfaces,            ONLY : alat_mult_factor
   use vec_operate,           ONLY : cross_product
   real(SP) :: cp(3)
    
#if defined _P2S_EXPORT

   call pw_cell(pw_unit)

#elif defined _P2S_V31 || _P2S_V32

   call qexml_read_cell(alat=alat_, a1=a1_, a2=a2_, a3=a3_,ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_cell','IOTK error',ABS(ierr))
    
#endif

   a(1,:) = a1_(:) ! assumes always atomic units
   a(2,:) = a2_(:)
   a(3,:) = a3_(:)
   !
   ! Set related SELF data: cell
   !
   alat(1) = maxval(abs(a(1,:)))*alat_mult_factor
   alat(2) = maxval(abs(a(2,:)))*alat_mult_factor
   alat(3) = maxval(abs(a(3,:)))*alat_mult_factor
   cp = cross_product(a(2,:),a(3,:))
   do i1=1,3
     DL_vol= DL_vol+a(1,i1)*cp(i1)
   enddo
   b(1,:)=cross_product(a(2,:),a(3,:))*2.0_SP*pi/DL_vol
   b(2,:)=cross_product(a(3,:),a(1,:))*2.0_SP*pi/DL_vol
   b(3,:)=cross_product(a(1,:),a(2,:))*2.0_SP*pi/DL_vol

   return
 end subroutine get_cell
 !
 !---------------------------------------------------------------------*
 !    Read symmetries                                                  *
 !---------------------------------------------------------------------*
 !
 subroutine get_symmetries
   use interfaces,  ONLY : symmetries_check_and_load

#if defined _P2S_EXPORT

   call pw_symmetry(pw_unit)

#elif defined _P2S_V31 || defined _P2S_V32

   call qexml_read_symmetry(invsym=invsym_, s=isym_(:,:,1:nsym_), ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))

#endif

   ! Note that invsym_ is well defined here, could be used for checks.
   do i1 = 1,nsym_
     isym_(:,:,i1) = transpose(isym_(:,:,i1))
   enddo
   call symmetries_check_and_load(isym_(:,:,1:nsym_),nsym_)

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
   type(bz_samp) :: k
   !
   allocate(xk_(3, k%nibz))
#if defined _P2S_EXPORT

   call pw_kpoints(pw_unit)

#elif defined _P2S_V31 || _P2S_V32

   call qexml_read_bz(xk=xk_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))

#endif
   !
   ! PW k in units of [cart, tpiba] -> units of [cart, 2*pi/alat(:)]
   ! PW cart tpiba/cart au/cart alat/RLU units
   !
   allocate(k%pt(k%nibz,3))
   do i1=1,k%nibz
     k%pt(i1,:)=xk_(:,i1) * alat(:)/alat_ 
   enddo
   deallocate(xk_)
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
#if defined _P2S_EXPORT
   call pw_other(pw_unit)
   default_nel = nelec_ 
#endif
   !
   return
 end subroutine get_more
 !
 !---------------------------------------------------------------------*
 !    Read reciprocal lattice vectors                                  *
 !---------------------------------------------------------------------*
 !
 subroutine get_R_vectors
   use pars,        only : pi
   use R_lattice,   ONLY:b, ng_vec, g_vec
   use D_lattice,   ONLY:alat
   !
   allocate(g_vec(ng_vec,3))
   allocate(igv_(3,ng_vec))
#if defined _P2S_EXPORT

   call pw_gvectors(pw_unit)

#elif defined _P2S_V31 || _P2S_V32

   call qexml_read_planewaves(igv=igv_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))

#endif
   !
   ! PW integer units of b1/b2/b3    -> 2pi/alat(:) units, cartesian, real
   ! b(:,:) is in a.u.
   !
   do i1 = 1, ng_vec
     g_vec(i1,:)=matmul(transpose(b),igv_(:,i1))*alat(:)/2./pi ! ok
   enddo
   deallocate(igv_)
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
   use interfaces,            only : force_noWFs
   use com,                   ONLY : msg, error
   character(lchlen) :: lch
   type(bz_samp) :: k
   integer i1,ik
   !
   allocate( wf_nc_k(k%nibz) )

#if defined _P2S_EXPORT

   allocate( pw_igk_(wf_ncx, k%nibz))
   allocate( pw_npwk_(k%nibz) )
   call pw_igkindex(pw_unit)
   wf_nc_k(:) = pw_npwk_(:)
   deallocate(pw_npwk_)

#elif defined _P2S_V31  

   allocate( pw_igk_(ng_vec, k%nibz))
   allocate(index_(ng_vec))     ! avoid allocate(index_(wf_ncx)) in case of v3.1 bug
   do ik = 1, k%nibz
     !
     pw_igk_(1:ng_vec, ik)= 0
     wf_nc_k(ik) =0
     !
     if (force_noWFs) cycle
     !
     ! Print verbose wfc read message
     !
     if(verboseIO.and.(any( (/1,2,k%nibz/)-ik.eq.0 ) &
&                      .or.mod(ik,k%nibz/4).eq.0)) then
       write(lch,'(" :: K-point:",i5,"/",i5)' ) ik,k%nibz
       call msg('s',trim(lch))
     endif

     call qexml_read_gk(ik, npwk=npwk_, index=index_, ierr=ierr)
     if (ierr.ne.0) call errore('qexml_read_gk','IOTK error',ABS(ierr))

!    if(npwk_.gt.wf_ncx) then
!      call msg('s','PWscf v3.1 contains a bug for parallel jobs.')
!      call msg('s','The tag <MAX_NPW> is incorrect      :',wf_ncx)
!      call msg('s','as it is less than npwk for some k  :',npwk_)
!      call msg('s','Action: fix MAX_NPW in data-file.xml manually,')
!      call msg('s','or use pw_export, 3.1.1 or 3.2 instead.')
!      call error('MAX_NPW < gkvectors size. See report file for details.')
!    endif
     pw_igk_(1:npwk_, ik)= index_(1:npwk_) 
     wf_nc_k(ik) = npwk_
   enddo
   deallocate(index_)
   wf_ncx = maxval(wf_nc_k)  ! correct definition of wf_ncx 

#elif defined _P2S_V32

   allocate( pw_igk_(wf_ncx, k%nibz))
   allocate( index_(wf_ncx) )     ! avoid allocate(index_(wf_ncx)) in case of v3.1 bug
   do ik = 1, k%nibz
     if (force_noWFs) cycle
     wf_nc_k(ik) =0
     call qexml_read_gk(ik, npwk=npwk_, index=index_, ierr=ierr)
     if (ierr.ne.0) call errore('qexml_read_gk','IOTK error',ABS(ierr))
     pw_igk_(1:npwk_, ik)= index_(1:npwk_) 
     wf_nc_k(ik) = npwk_
   enddo
   deallocate(index_)
     
#endif

   allocate( wf_igk(wf_ncx, k%nibz) )  ! this will be incorrect for parallel 3.1
   wf_igk(:,:)=-1 ! for checking
   do i1 = 1,k%nibz
     wf_igk(1:wf_nc_k(i1),i1)= pw_igk_(1:wf_nc_k(i1),i1) 
   enddo   
   deallocate(pw_igk_)

   wf_ng = maxval(wf_igk)
   if(maxval(wf_nc_k).ne.wf_ncx) then
     call error('maxval(wf_nc_k).ne.wf_ncx ! Check MAX_NPW ')
   endif
   if(any(wf_nc_k.eq.0)) call error('At least one npw(k) = 0!')
    
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
   allocate( en%E(en%nb, k%nibz, n_sp_pol) )

#if defined _P2S_EXPORT

   allocate(eig_(en%nb, k%nibz))

   call pw_eigenvalues(pw_unit)
    
   en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )/2.0_DP ! pw_export in Hartree
    
#elif defined _P2S_V31

   select case(n_sp_pol)
   case(1)
     allocate(eig_(en%nb, k%nibz))
     call qexml_read_bands(eig=eig_, ierr=ierr)
     if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
     en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )
     deallocate(eig_)
   case(2)
     allocate(eig_s_(en%nb, k%nibz,n_sp_pol))
     call qexml_read_bands(eig_s=eig_s_, ierr=ierr)
     if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
     en%E(:,:,:) = eig_s_( 1:en%nb, 1:k%nibz, 1:n_sp_pol )
     deallocate(eig_s_)
   end select

#elif defined _P2S_V32

   allocate(eigb_(en%nb))
   select case(n_sp_pol)
   case(2)
     do ik_ = 1,k%nibz
       do ispin_ = 1, n_sp_pol
         call qexml_read_bands(ik=ik_,ispin=ispin_,eig=eigb_, ierr=ierr)
         if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
         en%E(:,ik_,ispin_) = eigb_( 1:en%nb )
       enddo
     enddo
   case(1)
     do ik_ = 1,k%nibz
        call qexml_read_bands(ik=ik_,eig=eigb_, ierr=ierr)
        if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
        en%E(:,ik_,1) = eigb_( 1:en%nb )
     enddo
   end select
   deallocate(eigb_)

#endif
    
 end subroutine get_energies
 !
 !---------------------------------------------------------------------*
 !    Read XC functional                                               *
 !---------------------------------------------------------------------*
 !
 subroutine get_xc
   use xc_functionals,        ONLY : GS_xc_KIND,GS_xc_FUNCTIONAL
   implicit none

#if defined _P2S_V31 || _P2S_V32
   call qexml_read_xc(dft=pw_dft, ierr=ierr)
   call scan_xc(pw_dft,GS_xc_KIND,GS_xc_FUNCTIONAL)
#endif

   return
 end subroutine get_xc

 subroutine scan_xc(pw_dft, GS_xc_KIND_,GS_xc_FUNCTIONAL_)
   !
   ! Default: GS_xc_KIND_ = -1 , GS_xc_FUNCTIONAL_ = -1
   !
   use xc_functionals,        ONLY : XC_EXCHANGE_CORRELATION, XC_EXCHANGE, &
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,   &
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
   implicit none
   character(256), intent(in)    :: pw_dft
   integer, intent(out)          :: GS_xc_KIND_,GS_xc_FUNCTIONAL_
   !
   ! No support for GGA yet
   !
   if(index(pw_dft,'NOGX')==0.or.index(pw_dft,'NOGC')==0) then
     return
   endif
   !
   ! This list mimics the support for the Abinit functionals only.
   !
   if(index(pw_dft,'PZ')/=0.and.index(pw_dft,'SLA')/=0) then
     GS_xc_FUNCTIONAL_ = XC_LDA_C_PZ
     GS_xc_KIND_ = XC_EXCHANGE_CORRELATION
   else if(index(pw_dft,'PW')/=0.and.index(pw_dft,'SLA')/=0) then
     GS_xc_FUNCTIONAL_ = XC_LDA_C_PW
     GS_xc_KIND_ = XC_EXCHANGE_CORRELATION
   else if(index(pw_dft,'WIG')/=0.and.index(pw_dft,'SLA')/=0) then
     GS_xc_FUNCTIONAL_ = XC_LDA_C_WIGNER
     GS_xc_KIND_ = XC_EXCHANGE_CORRELATION
   else if(index(pw_dft,'HL')/=0.and.index(pw_dft,'SLA')/=0) then
     GS_xc_FUNCTIONAL_ = XC_LDA_C_HL
     GS_xc_KIND_ = XC_EXCHANGE_CORRELATION
!  else if(index(pw_dft,'SL1')/=0) then ! Have to check this...
!    GS_xc_FUNCTIONAL_ = XC_LDA_C_XALPHA
!    GS_xc_KIND_ = XC_EXCHANGE
   endif
   !
   return
   !
 end subroutine scan_xc
 !
end module P2S
