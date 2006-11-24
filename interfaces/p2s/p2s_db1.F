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
 subroutine p2s_db1(pwunit,en,k)
!===============================
 use pw_interface
 use com,                   ONLY : msg,error
 use electrons,             ONLY : levels, default_nel, n_spin, n_sp_pol, n_spinor
 use R_lattice,             ONLY : bz_samp, ng_vec, g_vec
 use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, default_Tel
 use wave_func,             ONLY : wf_nc_k, wf_ncx, wf_igk,wf_ng
 use vec_operate,           ONLY : cross_product
 implicit none
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
 integer,          intent(in)   :: pwunit ! unit of index.dat
!ws
 real(SP)                       :: lsop(3,3), acopy(3,3),cp(3)
 real(SP)                       :: b(3,3)
 integer                        :: i1, ik, nspinpw 
 logical                        :: l_identity, l_inversion
 character(schlen)              :: lch

!---------------------------------------------------------------------*
!    Read dimensions                                                  *
!---------------------------------------------------------------------*
 call msg('s','Header/K-points/Energies...')
 call pw_dimensions(pwunit)

 ng_vec  = pwdim%ngvec
 nsym   = pwdim%nsym 
 en%nb  = pwdim%nbnd
 nspinpw = pwdim%nspin
 select case(nspinpw)
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
 k%nibz = pwdim%nktot

 default_Tel = 0.d0  
 call msg("l","done")
!
!---------------------------------------------------------------------*
!    i/o allocations                                                  *
!---------------------------------------------------------------------*

 call pw_alloc

!---------------------------------------------------------------------*
!    Read cell data                                                   *
!---------------------------------------------------------------------*

 call msg('s','Cell data...')

 call pw_cell(pwunit)
 a(1,:) = pwcell%at(1,:)
 a(2,:) = pwcell%at(2,:)
 a(3,:) = pwcell%at(3,:)
 alat(1) = maxval(abs(a(1,:)))
 alat(2) = maxval(abs(a(2,:)))
 alat(3) = maxval(abs(a(3,:)))

! Use acopy to avoid passing non-contiguous arrays (just for fun)
 acopy = transpose(a)
 cp = cross_product(acopy(:,2),acopy(:,3))
 do i1=1,3
   DL_vol= DL_vol+a(1,i1)*cp(i1)
 enddo
 b(1,:)=cross_product(acopy(:,2),acopy(:,3))*2.*pi/DL_vol
 b(2,:)=cross_product(acopy(:,3),acopy(:,1))*2.*pi/DL_vol
 b(3,:)=cross_product(acopy(:,1),acopy(:,2))*2.*pi/DL_vol
 call msg("l","done")

!---------------------------------------------------------------------*
!    Read symmetries                                                  *
!---------------------------------------------------------------------*

 call msg('s','Symmetries...')

 call pw_symmetry(pwunit)

! pw_export gives the transpose of the matrix
 do i1 = 1,nsym 
   isym(:,:,i1) = transpose(isym(:,:,i1)) 
 enddo

 l_identity  = .false.
 l_inversion = .false.
 do i1=1,nsym
    if (isym(1,1,i1)==1.and.isym(1,2,i1)==0.and.isym(1,3,i1)==0.and.&
&       isym(2,1,i1)==0.and.isym(2,2,i1)==1.and.isym(2,3,i1)==0.and.&
&       isym(3,1,i1)==0.and.isym(3,2,i1)==0.and.isym(3,3,i1)==1) l_identity=.true.
    if (isym(1,1,i1)==-1.and.isym(1,2,i1)==0.and.isym(1,3,i1)==0.and.&
&       isym(2,1,i1)==0.and.isym(2,2,i1)==-1.and.isym(2,3,i1)==0.and.&
&       isym(3,1,i1)==0.and.isym(3,2,i1)==0.and.isym(3,3,i1)==-1) l_inversion=.true.
 enddo
 if (.not.l_identity) then
   call msg("l","failed. Identity not found.")
   stop 
 else
   call msg("l","done")
 endif

 if(l_inversion) call msg("s","Inversion ... found")
 if(.not.l_inversion) call msg("s","Inversion ... not found.")

 nsym=nsym
 if(n_spinor.eq.1) then
   i_time_rev = 1  ! Turn time reversal on
   if (l_inversion) nsym=nsym/2
   call msg("s","Time reversal being used.")
 else ! spinor for now: really spin orbit present
   i_time_rev = 0
   call msg("s","Time reversal not being used.")
 endif
!DEBUG>
!if (l_inversion) then
!
! Now switch off time reversal ( i_time_rev -> 0 )
! But if any of the nsym/2+1:nsym symmetries are not simply I * the :nsym/2
! then switch it back on (just in case the symmetries are not generated in 
! the order (I,R,-I,-R), etc)
!
!  i_time_rev = 0
!  do i1=1,nsym/2
!    if (.not.all((/isym(:,:,i1)==-isym(:,:,i1+nsym/2)/))) i_time_rev=1
!  enddo
!  if (i_time_rev==0) then
!     call msg("l","found. Time reversal turned off.")
!  else if (i_time_rev==1) then
!    call msg("l","not found. Time reversal turned on.")
! here inversion not found means that the order (above) is not followed.
!    nsym=nsym/2
!  endif
!  if(n_spin==4) call msg("s"," Inversion + spin-orbit calculation!")
!else
!  call msg("l","not found")
!endif
!DEBUG<

 if(l_inversion.and.nspinpw==4) call msg("s"," Inversion + spinor calculation!")
!
! Copy symmetries into real db1 array format
! PWscf: R is in fact transpose(R) ? See pw2gw
!
 allocate(dl_sop(3,3,nsym*(1+i_time_rev)))
 do i1=1,nsym
   lsop=matmul(transpose(a),isym(:,:,i1))
   dl_sop(:,:,i1)=matmul(lsop,b)/2./pi
   if (i_time_rev==1.and.n_spin==1) dl_sop(:,:,i1+nsym)=-dl_sop(:,:,i1)
 enddo
 if (i_time_rev==1.and.n_spin==1) nsym=2*nsym

!---------------------------------------------------------------------*
!    Read k-point mesh                                                *
!---------------------------------------------------------------------*

! k-points
 call msg('s','K-points mesh...')
 call pw_kpoints(pwunit)

! PW k in units of [cart, tpiba] -> units of [cart, 2*pi/alat(:)]
! PW cart tpiba/cart au/cart alat/RLU units
 allocate(k%pt(k%nibz,3))
 do i1=1,k%nibz
    k%pt(i1,:)=kpt(:,i1) * pwcell%tpiba * alat(:)/2.0_DP/PI
 enddo
 call msg('l','done')

!---------------------------------------------------------------------*
!    Read electrons                                                   *
!---------------------------------------------------------------------*

! Electrons
 call pw_other(pwunit)
 default_nel = nelec ! check 
!---------------------------------------------------------------------*
!    Read G-vectors                                                   *
!---------------------------------------------------------------------*

 allocate(wf_nc_k(k%nibz),g_vec(ng_vec,3))

 call msg('s','RL vectors...')
 call pw_gvectors(pwunit)
 call pw_igkindex(pwunit)

! PW integer units of b1/b2/b3    -> 2pi/alat(:) units, cartesian, real
! b(:,:) is in a.u.
 do i1 = 1, ng_vec
    g_vec(i1,:)=matmul(transpose(b),igvec(:,i1))*alat(:)/2./pi ! ok
 enddo
 call msg('l','done')

!---------------------------------------------------------------------*
!    Read igk arrays                                                  *
!---------------------------------------------------------------------*

 call msg('s','IGK arrays...')
 wf_nc_k(:) = pw_npwk(:)
 wf_ncx     = maxval(wf_nc_k) ! max for writing, allocation
 allocate(wf_igk(wf_ncx,k%nibz))
 forall(i1=1:wf_ncx) wf_igk(i1,:)=i1

 do i1 = 1,k%nibz
   wf_igk(1:wf_nc_k(i1),i1)= pw_igk(1:wf_nc_k(i1),i1) ! same as f2s
 enddo
 wf_ng   = maxval(wf_igk)

 call msg('l','done')

!---------------------------------------------------------------------*
!    Read eigenvalues                                                 *
!---------------------------------------------------------------------*

 call msg('s','Energies...')

 call pw_eigenvalues(pwunit)

 allocate( en%E(en%nb,k%nibz,n_spin) )
! PW energies in Ry
 en%E(:,:,1) = eig( 1:en%nb, 1:k%nibz )/2.0_DP
 if(n_spinor.eq.2) en%E(:,:,2) = en%E(:,:,1)
 call msg('l','done')

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
 
 end subroutine
