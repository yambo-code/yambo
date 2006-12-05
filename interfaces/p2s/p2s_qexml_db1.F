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
!=====================================
 use pars,                  ONLY : SP, PI, lchlen
 use qexml_module
 use qexml_data
 use com,                   ONLY : msg, error
 use electrons,             ONLY : levels, default_nel, n_spin, n_sp_pol, n_spinor
 use R_lattice,             ONLY : bz_samp, ng_vec, g_vec
 use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, default_Tel, &
&                                  n_atoms_max,n_atom_species,n_atoms, atom_pos, &
&                                  znucl
 use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk, wf_ng
 use vec_operate,           ONLY : cross_product
 use xc_functionals,        ONLY : GS_xc_KIND,GS_xc_FUNCTIONAL, xc_string
 implicit none
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
 integer,          intent(in)   :: pwunit ! unit of index.dat
 logical,          intent(in)   :: verboseIO
!ws
 real(SP)                       :: lsop(3,3), acopy(3,3),cp(3)
 real(SP)                       :: b(3,3)
 integer                        :: i1, ik, ierr, isym(3,3,48), nspinpw
 logical                        :: l_identity, l_inversion
 character(lchlen)              :: lch
 character(256)                 :: pw_dft
 character(3), allocatable      :: atom_species_names(:)

!---------------------------------------------------------------------*
!    Read dimensions                                                  *
!---------------------------------------------------------------------*
 call msg('s',' Header/K-points/Energies...')

 call qexml_read_bands(nbnd=nbnd_, num_k_points=num_k_points_, &
&                      nspin=nspinpw, nelec=nelec_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))
 call qexml_read_symmetry(nsym=nsym_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))
 call qexml_read_planewaves(npwx=npwx_,ngm=ngm_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))
 call qexml_read_ions(nat=nat_, nsp=nsp_, ierr=ierr)  
 if (ierr.ne.0) call errore('qexml_read_ions','IOTK error',ABS(ierr))

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
 nsym   = nsym_
 ng_vec = ngm_
 default_nel = nelec_
 default_Tel = 0.0_SP 
 wf_ncx  = npwx_ ! max for writing, allocation
 wf_ng   = wf_ncx ! this is written
 n_atom_species = nsp_

 call msg("l","done")

!DEBUG<
!call msg('s',' ')
!call msg('s','  :: Electrons             :',default_nel)
!call msg('s','  :: Temperature       [ev]:',default_Tel)
!call msg('s','  :: K-points              :',k%nibz)
!call msg('s','  :: Bands                 :',en%nb)
!call msg('s','  :: Spin                  :',n_spin)
!call msg('s','  :: RL vectors        (WF):',wf_ng)
!call msg('s','  :: RL vectors    (CHARGE):',ng_vec)
!call msg('s',' ')
!DEBUG>

!---------------------------------------------------------------------*
!    Read cell data                                                   *
!---------------------------------------------------------------------*

 call msg('s',' Cell data...')
 call qexml_read_cell(alat=alat_, a1=a1_, a2=a2_, a3=a3_,ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_cell','IOTK error',ABS(ierr))

 a(1,:) = a1_(:) ! assuming always atomic units?
 a(2,:) = a2_(:)
 a(3,:) = a3_(:)
 alat(1) = 1.0_SP*maxval(abs(a(1,:)) )
 alat(2) = 1.0_SP*maxval(abs(a(2,:)) )
 alat(3) = 1.0_SP*maxval(abs(a(3,:)) )

! Use acopy to avoid passing non-contiguous arrays (just for fun)
 acopy = transpose(a)
 cp = cross_product(acopy(:,2),acopy(:,3))
 do i1=1,3
   DL_vol= DL_vol+a(1,i1)*cp(i1)
 enddo
 b(1,:)=cross_product(acopy(:,2),acopy(:,3))*2.*PI/DL_vol
 b(2,:)=cross_product(acopy(:,3),acopy(:,1))*2.*PI/DL_vol
 b(3,:)=cross_product(acopy(:,1),acopy(:,2))*2.*PI/DL_vol
 call msg("l","done")

!---------------------------------------------------------------------*
!    Read atomic positions (for version > 3.0.4)                      *
!---------------------------------------------------------------------*
 call msg('s',' Atomic data...')
 allocate( tau_(3,nat_), ityp_(nat_) )  ! tau is read in cartesian coords
 allocate( atm_(n_atom_species))
 call qexml_read_ions( tau=tau_, ityp=ityp_, atm=atm_, ierr=ierr)

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

 allocate(atom_species_names(n_atom_species))
 allocate(znucl(n_atom_species))
 do i1 = 1, n_atom_species
   atom_species_names(i1) = atm_(i1)    
   znucl(i1) = atomic_number(atom_species_names(i1))
 enddo

 if (ierr.ne.0) call errore('qexml_read_ions','IOTK error',ABS(ierr))

 call msg("l","done")
!---------------------------------------------------------------------*
!    Read symmetries                                                  *
!---------------------------------------------------------------------*

 call msg('s',' Symmetries...')

 allocate(s_(3,3,nsym_))
 call qexml_read_symmetry(invsym=invsym_, s=s_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))

 do i1 = 1,nsym 
   isym(:,:,i1) = transpose(s_(:,:,i1)) 
 enddo
 deallocate(s_)

 l_identity  = .false.
 l_inversion = .false.
 do i1=1,nsym
   if (isym(1,1,i1)==1.and.isym(1,2,i1)==0.and.isym(1,3,i1)==0.and.&
&      isym(2,1,i1)==0.and.isym(2,2,i1)==1.and.isym(2,3,i1)==0.and.&
&      isym(3,1,i1)==0.and.isym(3,2,i1)==0.and.isym(3,3,i1)==1) l_identity=.true.
   if (isym(1,1,i1)==-1.and.isym(1,2,i1)==0.and.isym(1,3,i1)==0.and.&
&      isym(2,1,i1)==0.and.isym(2,2,i1)==-1.and.isym(2,3,i1)==0.and.&
&      isym(3,1,i1)==0.and.isym(3,2,i1)==0.and.isym(3,3,i1)==-1) l_inversion=.true.
 enddo
 if (.not.l_identity) then
   call msg("l","failed. Identity not found.")
   stop 
 else
   call msg("l","done")
 endif
 if(l_inversion) call msg("s","Inversion ... found")
 if(.not.l_inversion) call msg("s","Inversion ... not found.")

 if(n_spinor.eq.1) then
   i_time_rev = 1  ! Turn time reversal on
   nsym=nsym/2
   call msg("s","Time reversal being used.")
 else ! spinor for now: really means spin orbit coupling...
   i_time_rev = 0
   nsym=nsym
   call msg("s","Time reversal not being used.")
 endif

!DEBUG<
!if (l_inversion) then
!
! Now switch off time reversal ( i_time_rev -> 0 )
! But if any of the nsym/2+1:nsym symmetries are not simply I * the :nsym/2
! then switch it back on.
!
!  i_time_rev = 0
!  do i1=1,nsym/2
!    if (.not.all((/isym(:,:,i1)==-isym(:,:,i1+nsym/2)/))) i_time_rev=1
!  enddo
!  if (i_time_rev==0) call msg("l","found. Time reversal turned off.")
!  if (i_time_rev==1) then
!    call msg("l","not found")
!    nsym=nsym/2
!  endif
!  if(n_spin.eq.4) call msg("s"," Inversion + spin-orbit calculation!")
!else
!  call msg("l","not found")
!endif
!DEBUG<

   if(l_inversion.and.nspinpw.eq.4) call msg("s"," Inversion + spinor calculation!")
!
! PWscf: R is in fact transpose(R) ? See pw2gw
!
 allocate(dl_sop(3,3,nsym*(1+i_time_rev)))
 do i1=1,nsym
   lsop=matmul(transpose(a),isym(:,:,i1))
   dl_sop(:,:,i1)=matmul(lsop,b)/2.0_SP/PI
   if (i_time_rev==1.and.n_spin.eq.1) dl_sop(:,:,i1+nsym)=-dl_sop(:,:,i1)
 enddo
 if (i_time_rev==1.and.n_spin.eq.1) nsym=2*nsym

!---------------------------------------------------------------------*
!    Read XC functional                                               *
!---------------------------------------------------------------------*

 call qexml_read_xc(dft=pw_dft, ierr=ierr)
 call msg("s","XC functional found: "//trim(pw_dft))
 call scan_xc(pw_dft,GS_xc_KIND,GS_xc_FUNCTIONAL)

!---------------------------------------------------------------------*
!    Read band structure                                              *
!---------------------------------------------------------------------*

 call msg('s',' Bandstructure (K-pts, E)...')
 allocate(xk_(3, num_k_points_))

 if(nspinpw.eq.1.or.nspinpw.eq.4) then

   allocate(eig_(nbnd_, num_k_points_))
   call qexml_read_bands(xk=xk_, eig=eig_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_bands','IOTK error',ABS(ierr))

 else if(nspinpw.eq.2) then

   allocate(eig_s_(nbnd_, num_k_points_,n_sp_pol))
   call qexml_read_bands(xk=xk_, eig_s=eig_s_, ierr=ierr)

 endif

! PW k in units of [cart, tpiba] -> units of [cart, 2*pi/alat(:)]
! PW cart tpiba/cart au/cart alat/RLU units
 allocate(k%pt(k%nibz,3))
 do i1 = 1,k%nibz
!   k%pt(i1,:)=kpt(:,i1) * pwcell%tpiba * alat(:)/2.0_SP/PI
    k%pt(i1,:)=xk_(:,i1) * alat(:)/alat_
 enddo
 deallocate(xk_)

!PW energies (3.1) in HARTREE
 allocate( en%E(en%nb, k%nibz, n_spin) )
 select case(nspinpw)
 case(1)
   en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )
   deallocate(eig_)
 case(4)
   en%E(:,:,1) = eig_( 1:en%nb, 1:k%nibz )
   en%E(:,:,2) = eig_( 1:en%nb, 1:k%nibz )
   deallocate(eig_)
 case(2)
   en%E(:,:,:) = eig_s_( 1:en%nb, 1:k%nibz, 1:n_spin )
   deallocate(eig_s_)
 end select

 call msg('l','done')

!---------------------------------------------------------------------*
!    Read G-vectors                                                   *
!---------------------------------------------------------------------*

 allocate(g_vec(ng_vec,3))

 call msg('s',' RL vectors...')
 allocate(igv_(3,ngm_))
 call qexml_read_planewaves(igv=igv_, ierr=ierr)
 if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))

! PW integer units of b1/b2/b3    -> 2pi/alat(:) units, cartesian, real
! b(:,:) is in a.u.
 do i1 = 1, ng_vec
    g_vec(i1,:)=matmul(transpose(b),igv_(:,i1))*alat(:)/2./pi ! ok
 enddo
 deallocate(igv_)
 call msg('l','done')

!---------------------------------------------------------------------*
!    Read wf_ncx and igk arrays                                   *
!---------------------------------------------------------------------*

! This whole qexml is terribly slow as it reads and rereads the data.xml
! file searching for tags. A single subroutine "read bandstructure" would
! be much more efficient such that the BAND_STRUCTURE tag would be searched
! only once. It would also get around the MAX_NPW 3.1 bug....

 call msg('s',' IGK arrays...')

 allocate( wf_nc_k(k%nibz) )
 allocate( wf_igk(wf_ncx, k%nibz) ) ! in ioDB1: wf_igk(wf_ncx,k%nibz))
 wf_igk(:,:)=-1 ! for checking

!allocate(index_(wf_ncx)) ! avoid in case of v3.1 bug
 allocate(index_(ng_vec))     ! overkill...
 do ik = 1,k%nibz
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

 if(any(wf_nc_k.eq.0)) call error('At least one npw(k) = 0!')

 call msg('l','done')

!---------------------------------------------------------------------*
!    Print dimensions
!---------------------------------------------------------------------*

 call msg('s',' :: Electrons             :',default_nel)
 call msg('s',' :: Temperature       [ev]:',default_Tel)
 call msg('s',' :: Lattice factors [a.u.]:',alat)
 call msg('s',' :: K-points              :',k%nibz)
 call msg('s',' :: Bands                 :',en%nb)
 call msg('s',' :: Spinor components     :',n_spinor)
 call msg('s',' :: Spin polarizations    :',n_sp_pol)
 if(i_time_rev==1) call msg('s',' :: Symmmetries  [spatial]:',nsym/2)
 if(i_time_rev==1) call msg('s',' :: Symmmetries    [T-rev]:',nsym/2*i_time_rev)
 if(i_time_rev==0) call msg('s',' :: Symmmetries  [spatial]:',nsym)
 if(i_time_rev==0) call msg('s',' :: Symmmetries    [T-rev]:',nsym*i_time_rev)
 call msg('s',' :: RL vectors        (WF):',wf_ng)
 call msg('s',' :: RL vectors    (CHARGE):',ng_vec)
 call msg('s',' :: XC potential          :',xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
 call msg('s',' :: Atomic species        :',n_atom_species)
 call msg('s',' :: Max atoms/species     :',n_atoms_max)

   return
 
 contains
 
  integer function atomic_number(atom_name)
    implicit none
    character(len=*), intent(in) :: atom_name
    character(len=2)             :: elements(103)
    integer                      :: i
            
    elements(1:2)=(/'H',                              'He'/)
    elements(3:10)=(/'Li','Be','B','C','N','O','F','Ne'/)
    elements(11:18)=(/'Na','Mg','Al','Si','P','S','Cl','Ar'/)
    elements(19:25)=(/'K','Ca','Sc','Ti','V','Cr','Mn'/)
    elements(26:30)=(/'Fe','Co','Ni','Cu','Zn'/)
    elements(31:36)=(/'Ga','Ge','As','Se','Br','Kr'/)
    elements(37:43)=(/'Rb','Sr','Y','Zr','Nb','Mo','Tc'/)
    elements(44:48)=(/'Ru','Rh','Pd','Ag','Cd'/)
    elements(49:54)=(/ 'In','Sn','Sb','Te','I','Xe'/)
    elements(55:63)=(/'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu'/)
    elements(64:71)=(/'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu'/)
    elements(72:76)=(/'Hf','Ta','W','Re','Os'/)
    elements(77:80)=(/'Ir','Pt','Au','Hg'/)
    elements(81:86)=(/'Tl','Pb','Bi','Po','At','Rn'/)
    elements(87:94)=(/'Fr','Ra','Ac','Th','Pa','U','Np','Pu'/)
    elements(95:103)=(/'Am','Cm','Bk','Cf','Es','Fm','Md','No', 'Lr'/)
      
    do i = 1, 103
      if ( trim(atom_name) == trim(elements(i)) ) then
        atomic_number = i
        return
      end if
    end do
    znucl = 0

    return 
  end function atomic_number

 end subroutine p2s_qexml_db1

 subroutine scan_xc(pw_dft, GS_xc_KIND_,GS_xc_FUNCTIONAL_)
   ! Default: GS_xc_KIND_ = -1 , GS_xc_FUNCTIONAL_ = -1
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

   return
 end subroutine scan_xc

