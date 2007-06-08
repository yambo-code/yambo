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
 subroutine p2s_qexml_db1(pwunit,verboseIO,en,k)
 !
 use pars,                  ONLY : SP, PI, lchlen
 use qexml_module
 use qexml_data
 use com,                   ONLY : msg, error
 use electrons,             ONLY : levels, default_nel, n_spin, n_sp_pol, n_spinor, spin_orbit
 use R_lattice,             ONLY : bz_samp, ng_vec, g_vec,b
 use D_lattice,             ONLY : DL_vol, a, alat, default_Tel, &
&                                  n_atoms_max,n_atom_species,n_atoms, atom_pos, &
&                                  znucl
 use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng
 use vec_operate,           ONLY : cross_product
 use xc_functionals,        ONLY : GS_xc_KIND,GS_xc_FUNCTIONAL, xc_string
 use interfaces,            ONLY : print_interface_dimensions,symmetries_check_and_load,&
&                                  alat_mult_factor,force_noWFs
 implicit none
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
 integer,          intent(in)   :: pwunit ! unit of index.dat
 logical,          intent(in)   :: verboseIO
 !
 ! Work Space
 !
 real(SP)                       :: acopy(3,3),cp(3)
 integer                        :: i1, ik, ierr, isym(3,3,48), nspinpw
 integer, external              :: atomic_number
 character(lchlen)              :: lch
 character(256)                 :: pw_dft
 character(3), allocatable      :: atom_species_names(:)
 !---------------------------------------------------------------------*
 !    Read dimensions                                                  *
 !---------------------------------------------------------------------*
 call msg('s',' Header/K-points/Energies...')
 !
 call qexml_read_bands(nbnd=nbnd_, num_k_points=num_k_points_, &
&                      nspin=nspinpw, nelec=nelec_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
 call qexml_read_symmetry(nsym=nsym_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))
 call qexml_read_spin(lspinorb=lspinorb_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_spin','IOTK error',ABS(ierr))
 call qexml_read_planewaves(npwx=npwx_,ngm=ngm_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))
 call qexml_read_ions(nat=nat_, nsp=nsp_, ierr=ierr)  
 if (ierr.ne.0) call errore('qexml_read_ions','IOTK error',ABS(ierr))
 !
 en%nb  = nbnd_
 k%nibz = num_k_points_
 select case(nspinpw)
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
 spin_orbit = lspinorb_
 ng_vec = ngm_
 default_nel = nelec_
 default_Tel = 0.0_SP 
 wf_ncx  = npwx_ ! max for writing, allocation
 n_atom_species = nsp_
 !
 call msg("l","done")
 !
 !---------------------------------------------------------------------*
 !    Read cell data                                                   *
 !---------------------------------------------------------------------*
 call msg('s',' Cell data...')
 call qexml_read_cell(alat=alat_, a1=a1_, a2=a2_, a3=a3_,ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_cell','IOTK error',ABS(ierr))
 !
 a(1,:) = a1_(:) ! assumes always atomic units
 a(2,:) = a2_(:)
 a(3,:) = a3_(:)
 alat(1) = maxval(abs(a(1,:)) )*alat_mult_factor
 alat(2) = maxval(abs(a(2,:)) )*alat_mult_factor
 alat(3) = maxval(abs(a(3,:)) )*alat_mult_factor
 !
 ! Use acopy to avoid passing non-contiguous arrays (just for fun)
 !
 acopy = transpose(a)
 cp = cross_product(acopy(:,2),acopy(:,3))
 do i1=1,3
   DL_vol= DL_vol+a(1,i1)*cp(i1)
 enddo
 b(1,:)=cross_product(acopy(:,2),acopy(:,3))*2.*PI/DL_vol
 b(2,:)=cross_product(acopy(:,3),acopy(:,1))*2.*PI/DL_vol
 b(3,:)=cross_product(acopy(:,1),acopy(:,2))*2.*PI/DL_vol
 call msg("l","done")
 !
 !---------------------------------------------------------------------*
 !    Read atomic positions (for version > 3.0.4)                      *
 !---------------------------------------------------------------------*
 call msg('s',' Atomic data...')
 allocate( tau_(3,nat_), ityp_(nat_) )  ! tau is read in cartesian coords
 allocate( atm_(n_atom_species))
 call qexml_read_ions( tau=tau_, ityp=ityp_, atm=atm_, ierr=ierr)
 !
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
   atom_pos(:, n_atoms( ityp_(i1) ) , ityp_(i1) ) = tau_(:,i1) 
 enddo
 !
 allocate(atom_species_names(n_atom_species))
 allocate(znucl(n_atom_species))
 do i1 = 1, n_atom_species
   atom_species_names(i1) = atm_(i1)    
   znucl(i1) = atomic_number(atom_species_names(i1))
 enddo
 !
 if (ierr.ne.0) call errore('qexml_read_ions','IOTK error',ABS(ierr))
 !
 call msg("l","done")
 !---------------------------------------------------------------------*
 !    Read symmetries                                                  *
 !---------------------------------------------------------------------*
 !
 call msg('s',' Symmetries...')
 !
 allocate(s_(3,3,nsym_))
 call qexml_read_symmetry(invsym=invsym_, s=s_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))
 do i1 = 1,nsym_
   isym(:,:,i1) = transpose(s_(:,:,i1)) 
 enddo
 deallocate(s_)
 call symmetries_check_and_load(isym(:,:,:nsym_),nsym_)
 !---------------------------------------------------------------------*
 !    Read XC functional                                               *
 !---------------------------------------------------------------------*
 call qexml_read_xc(dft=pw_dft, ierr=ierr)
 call msg("s"," XC functional found: "//trim(pw_dft))
 call scan_xc(pw_dft,GS_xc_KIND,GS_xc_FUNCTIONAL)
 !---------------------------------------------------------------------*
 !    Read band structure                                              *
 !---------------------------------------------------------------------*
 call msg('s',' Bandstructure (K-pts, E)...')
 allocate(xk_(3, num_k_points_))
 !
 if(nspinpw.eq.1.or.nspinpw.eq.4) then
   !
   allocate(eig_(nbnd_, num_k_points_))
   call qexml_read_bands(xk=xk_, eig=eig_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
   !
 else if(nspinpw.eq.2) then
   !
   allocate(eig_s_(nbnd_, num_k_points_,n_sp_pol))
   call qexml_read_bands(xk=xk_, eig_s=eig_s_, ierr=ierr)
   !
 endif
 !
 ! PW k in units of [cart, tpiba] -> units of [cart, 2*pi/alat(:)]
 ! PW cart tpiba/cart au/cart alat/RLU units
 !
 allocate(k%pt(k%nibz,3))
 do i1 = 1,k%nibz
    !
    ! These are equivalent:
    ! k%pt(i1,:)=kpt(:,i1) * pwcell%tpiba * alat(:)/2.0_SP/PI
    !
    k%pt(i1,:)=xk_(:,i1) * alat(:)/alat_
 enddo
 deallocate(xk_)
 !
 ! PW energies (3.1) in HARTREE
 !
 allocate( en%E(en%nb, k%nibz, n_sp_pol) )
 select case(nspinpw)
 case(1)
   en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )
   deallocate(eig_)
 case(4)
   en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )
   deallocate(eig_)
 case(2)
   en%E(:,:,:) = eig_s_( 1:en%nb, 1:k%nibz, 1:n_sp_pol )
   deallocate(eig_s_)
 end select
 !
 call msg('l','done')
 !---------------------------------------------------------------------*
 !    Read G-vectors                                                   *
 !---------------------------------------------------------------------*
 allocate(g_vec(ng_vec,3))
 !
 call msg('s',' RL vectors...')
 allocate(igv_(3,ngm_))
 call qexml_read_planewaves(igv=igv_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))
 !
 ! PW integer units of b1/b2/b3    -> 2pi/alat(:) units, cartesian, real
 ! b(:,:) is in a.u.
 !
 do i1 = 1, ng_vec
    g_vec(i1,:)=matmul(transpose(b),igv_(:,i1))*alat(:)/2./pi ! ok
 enddo
 deallocate(igv_)
 call msg('l','done')
 !---------------------------------------------------------------------*
 !    Read wf_ncx and igk arrays                                   *
 !---------------------------------------------------------------------*
 !
 ! This whole qexml is terribly slow as it reads and rereads the data.xml
 ! file searching for tags. A single subroutine "read bandstructure" would
 ! be much more efficient such that the BAND_STRUCTURE tag would be searched
 ! only once. It would also get around the MAX_NPW 3.1 bug....
 !
 call msg('s',' IGK arrays...')
 !
 allocate( wf_nc_k(k%nibz) )
 allocate( wf_igk(wf_ncx, k%nibz) ) ! in ioDB1: wf_igk(wf_ncx,k%nibz))
 wf_igk(:,:)=-1 ! for checking
 !
 !allocate(index_(wf_ncx)) ! avoid in case of v3.1 bug
 !
 allocate(index_(ng_vec))     ! overkill...
 do ik = 1,k%nibz
   !
   wf_igk(1:npwk_, ik)= 0
   wf_nc_k(ik) =0
   !
   if (force_noWFs) cycle
   !
   ! Print verbose wfc read message
   !
   if(verboseIO.and.(any( (/1,2,k%nibz/)-ik.eq.0 ) &
&  .or.mod(ik,k%nibz/4).eq.0)) then
     write(lch,'(" :: K-point:",i5,"/",i5)' ) ik,k%nibz
     call msg('s',trim(lch))
   endif
   call qexml_read_gk(ik, npwk=npwk_, index=index_, ierr=ierr)

!>>>DEBUG
! These lines are needed if using gkvector files from multiple runs,
! patched using Perl script and modified qexml.F.
! Superceded by new script using iotk_copy.x
!  kshift = k%nibz ! default
!  kshift = 10
!  if(ik.gt.kshift) then
!    call qexml_read_gk(ik, ikgk=ik-kshift, npwk=npwk_, index=index_, ierr=ierr)   else
!    call qexml_read_gk(ik, npwk=npwk_, index=index_, ierr=ierr)
!  endif
!<<<DEBUG

   if (ierr.ne.0) call errore('qexml_read_gk','IOTK error',ABS(ierr))
   if(npwk_.gt.wf_ncx) then
      call msg('s',' :: RL vectors        (WF):',wf_ng)
      call msg('s','PWscf v3.1 contains a bug in parallel jobs: npw(k) is ',npwk_)
      call msg('s','MAX_NPW < gkvectors size. Fix data-file.xml manually!')
      call error('MAX_NPW < gkvectors size. Fix data-file.xml manually! ')
   endif
   wf_igk(1:npwk_, ik)= index_(1:npwk_) ! CHECK!
   wf_nc_k(ik) = npwk_
 enddo
 deallocate(index_)
 wf_ng = maxval(wf_igk)
 if(maxval(wf_nc_k).ne.wf_ncx) then
    call error('maxval(wf_nc_k).ne.wf_ncx ! Check MAX_NPW ')
 endif
 if(any(wf_nc_k.eq.0)) call error('At least one npw(k) = 0!')
 !
 call msg('l','done')
 !
 call print_interface_dimensions(en,k)
 !
 end subroutine p2s_qexml_db1
 !
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

