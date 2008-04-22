!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 subroutine f2y_db1(en,k,fhi_inp,fhi_wvf,file_name)
!===================================================
 use pars,                  ONLY : SP, DP, PI, schlen, HARTREE
 use com,                   ONLY : msg
 use electrons,             ONLY : levels, default_nel,n_bands,n_spin,n_sp_pol, spin_orbit
 use R_lattice,             ONLY : bz_samp, ng_vec, g_vec,nkibz
 use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, default_Tel,&
&                                  n_atom_species,n_atoms,n_atoms_max,atom_pos
 use wave_func,             ONLY : wf_nc_k, wf_igk,wf_ncx,wf_ng
 use vec_operate,           ONLY : cross_product
 use xc_functionals,        ONLY : XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
 use f2y_datatypes
 use mod_com2y,            ONLY : print_interface_dimensions
 implicit none
 character(*)                   :: file_name
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
! FHI variables
 type(finp_type),  intent(out)  :: fhi_inp
 type(fwvf_type),  intent(out)  :: fhi_wvf
 type(fgg_type)                 :: fhi_gg
 ! 
 ! Work Space
 !
 real(SP)                       :: lsop(3,3), b(3,3),cp(3)
 integer                        :: i1, ik, i2, i3, inel, ii
 logical                        :: l_identity, l_inversion
 character(schlen)              :: filename
 character(schlen)              :: xcname, cdummy
 integer, allocatable           :: asop(:,:,:)

 spin_orbit = .false.

 call msg('nl',' Header/K-points/Energies...')
! open and read inp.ini 
!
!      - lattice vectors
!      - k-points
!      - symmetry operations
!
!  other data is read only to check for consistency
!  atomic coordinates are read from wavefunction file      
 write(fhi_inp%file,'(2a)') trim(file_name),'.inp'
 call read_inp_ini(fhi_inp)
 call msg('l','done')

 call msg('nl',' XC-functional ...')
 
 open(11,file=trim(file_name),form='formatted')
 do i1=1,6
  read (11,*)
 enddo
 read (11,'(2x,a9,1x,a)') cdummy,xcname
 close(11)
 if (trim(cdummy)/="Exchange:") then
  print*,":",trim(cdummy),":"
  call msg('l',' inconsistency in main output file')
  stop
 endif
 if (trim(xcname)=="LDA") then
   GS_xc_FUNCTIONAL=XC_LDA_C_PZ
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
 else
  write(cdummy,'(3a)') "XC-functional ",trim(xcname)," not implemented"
  call msg('nl',cdummy)
  stop
 endif

 call msg('l','done')

 call msg('nl',' Reading G-points...')
 write(filename,'(2a)') trim(file_name),'.rl'
 call read_gg(fhi_inp,fhi_gg,filename)
 call msg('l','done')
! open and read first part of wvf file
!
!      - k-points and related data
!      - Kohn-Sham eigenvalues
!      - perform consistency checks
 call msg('nl',' Read wave function file...')
 write(fhi_wvf%file,'(2a)') trim(file_name),'.wvf'
 call read_wvf(fhi_inp,fhi_gg,fhi_wvf)
!  transfer lattice vectors to YAMBO
 i_time_rev=1
 a(1,:)=fhi_inp%a(:,1)
 a(2,:)=fhi_inp%a(:,2)
 a(3,:)=fhi_inp%a(:,3)

 alat(1)=2*maxval(abs(a(1,:)))
 alat(2)=2*maxval(abs(a(2,:)))
 alat(3)=2*maxval(abs(a(3,:)))
 DL_vol=0.0
 cp=cross_product(a(2,:),a(3,:))
 do i1=1,3
   DL_vol=DL_vol+a(1,i1)*cp(i1)
 enddo
 b(1,:)=cross_product(a(2,:),a(3,:))*2.*pi/DL_vol
 b(2,:)=cross_product(a(3,:),a(1,:))*2.*pi/DL_vol
 b(3,:)=cross_product(a(1,:),a(2,:))*2.*pi/DL_vol

 nsym    = fhi_inp%nrot
 en%nb   = fhi_wvf%nx
 n_bands = en%nb
 ng_vec  = fhi_gg%ngx
 k%nibz  = fhi_wvf%nkptx
 nkibz   = k%nibz
 default_Tel = fhi_inp%ekt/HARTREE
!
 n_spin=1
 n_sp_pol=1

! pretending the system is neutral ...
 default_nel = 0.
 do i1 = 1, fhi_wvf%nsx
   inel = fhi_inp%zv(i1)*fhi_inp%na(i1)
   default_nel = default_nel + inel
 enddo
 call msg('l','done')

! transfer k-points to YAMBO
!
 allocate(k%pt(k%nibz,3))
 do i1=1,k%nibz
  k%pt(i1,:)= fhi_wvf%xk(:,i1)*alat(:)/fhi_inp%alat
 enddo
!
 call msg('nl',' Atoms positions...')
 call msg('l','not done')
!
 allocate(wf_nc_k(k%nibz),g_vec(ng_vec,3),en%E(en%nb,k%nibz,n_spin),asop(3,3,nsym))

 call msg('nl',' Symmetries...')
 do i3=1,fhi_inp%nrot
   do i2=1,3
     do i1=1,3
       asop(i1,i2,i3)=fhi_inp%srot(i2,i1,i3)
     enddo
   enddo
 enddo
 l_identity=.false.
 l_inversion=.false.
 do i1=1,nsym
   if (asop(1,1,i1)==1.and.asop(1,2,i1)==0.and.asop(1,3,i1)==0.and.&
&      asop(2,1,i1)==0.and.asop(2,2,i1)==1.and.asop(2,3,i1)==0.and.&
&      asop(3,1,i1)==0.and.asop(3,2,i1)==0.and.asop(3,3,i1)==1) l_identity=.true.
   if (asop(1,1,i1)==-1.and.asop(1,2,i1)==0.and.asop(1,3,i1)==0.and.&
&      asop(2,1,i1)==0.and.asop(2,2,i1)==-1.and.asop(2,3,i1)==0.and.&
&      asop(3,1,i1)==0.and.asop(3,2,i1)==0.and.asop(3,3,i1)==-1) l_inversion=.true.
 enddo
 if (.not.l_identity) then
   call msg("l","failed. Identity not found.")
   stop
 else
   call msg("l","done")
 endif
 call msg("s","Inversion ...")
 if (l_inversion) then
   !
   ! Note that this will make YAMBO run slower....
   !
   call msg("l","found. Time reversal turned off.")
   i_time_rev=0
 else
   call msg("l","not found")
 endif
!

!-------------------------------------------------------------------------
!From abinit help ...
!
!The relation between the above symmetry matrices symrel,
!expressed in the basis of primitive translations, and the same symmetry
!matrices expressed in cartesian coordinates, is as follows.
!Denote the matrix whose columns are the primitive
!translations as A, and denote the cartesian symmetry matrix as R. Then
!R_rlu = A(inverse) * R * A 
!
!In my case A=transpose(a)
!
!R= transpose(a) R_rlu inverse[transpose(a)]
!
!but inverse[transpose(a)]= b/2./pi
!
!-------------------------------------------------------------------------
 allocate(dl_sop(3,3,nsym*(1+i_time_rev)))
 do i1=1,nsym
   lsop=matmul(transpose(a),asop(:,:,i1))
   dl_sop(:,:,i1)=matmul(lsop,b)/2./pi
   if (i_time_rev==1) dl_sop(:,:,i1+nsym)=-dl_sop(:,:,i1)
 enddo
 if (i_time_rev==1) nsym=2*nsym

! transfer reciprocal lattice vectors and igk tables
 call msg('nl',' RL vectors...')
 do i1 = 1,ng_vec
    g_vec(i1,:)= fhi_gg%gg(i1,:)*alat(:)/fhi_inp%alat
 enddo
 wf_nc_k(:)=fhi_wvf%ngw(:)
 wf_ncx=maxval(fhi_wvf%ngw)
 allocate(wf_igk(wf_ncx,k%nibz))
 forall(i1=1:wf_ncx) wf_igk(i1,:)=i1
 do i1=1,fhi_wvf%nkptx
  wf_igk(:wf_nc_k(i1),i1)=fhi_wvf%igk(:wf_nc_k(i1),i1)
 enddo
 wf_ng=maxval(wf_igk)
! transfer KS-energies to YAMBO
 call msg('l','done')
!
 call msg('nl',' Energies...')
 do ik=1,k%nibz
  forall (i1=1:n_spin) en%E(:,ik,i1)=fhi_wvf%eig(:,ik)
 enddo
!
 call msg('l','done')
!call msg('s',':: Electrons             :',default_nel)
!call msg('s',':: Temperature       [ev]:',default_Tel*HARTREE)
!call msg('s',':: Lattice factors [a.u.]:',alat)
!call msg('s',':: K-points              :',k%nibz)
!call msg('s',':: Bands                 :',en%nb)
!call msg('s',':: Symmetries   [spatial]:',nsym)
!call msg('s','::                [T-rev]:',nsym*i_time_rev)
!call msg('s',':: Spin  [components/pol]:',(/n_spin,n_sp_pol/))
!call msg('s',':: (WF)     RL vectors   :',wf_ng)
!call msg('s',':: (Charge) RL vectors   :',ng_vec)
!call msg('s',' ')
!
 call deallocate_fgg(fhi_gg)

 call print_interface_dimensions(en,k)

 end subroutine

