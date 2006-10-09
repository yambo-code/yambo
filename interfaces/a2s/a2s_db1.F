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
 subroutine a2s_db1(en,k,KSS_file_name)
!======================================
 use pars,                  ONLY : SP, DP, PI, schlen
 use com,                   ONLY : msg
 use electrons,             ONLY : levels, default_nel,n_bands,n_spin,n_sp_pol,n_spinor
 use R_lattice,             ONLY : bz_samp, ng_vec, g_vec,nkibz
 use D_lattice,             ONLY : nsym, i_time_rev, i_space_inv, dl_sop, &
&                                  DL_vol, a, alat, default_Tel,&
&                                  pp_n_l_comp_max,n_atom_species,n_atoms,pp_n_l_comp,&
&                                  n_atoms_max,atom_pos
 use wave_func,             ONLY : ng_wf, wf_igk,wf_ng_disk,wf_ng
 use vec_operate,           ONLY : internal_product
 use defs_datatypes,        ONLY : hdr_type, wffile_type
 use xc_functionals,        ONLY : XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                  xc_string,XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                  XC_LDA_C_HL,XC_LDA_C_XALPHA
 implicit none
 character(*)                   :: KSS_file_name
 type(levels),     intent(out)  :: en
 type(bz_samp),    intent(out)  :: k  
!
!ws
 real(SP)                       :: lsop(3,3), b(3,3)
 integer                        :: i1, ik, i2, i3, inel
 logical                        :: l_identity, l_inversion
!
!ABINIT
!
 type(hdr_type)                 :: ahdr
 type(wffile_type)              :: wff
 integer                        :: fform,ishm,a_nsym
 integer, allocatable           :: agvec(:,:),asop(:,:,:)
 real(DP), allocatable          :: rstr(:)
!
 call msg('s','KSS Header...')
!===========================================
!
! ABINIT KSS FILE
!
 open(unit=11,file=KSS_file_name,form='unformatted')
 wff%unwff=11
 wff%accesswff=0
 fform=1
 call hdr_io_wfftype(fform,ahdr,5,wff)
 call msg('l','abinit version ',ahdr%codvsn)
!
! XC KIND/FUNCTIONAL
!--------------------
 select case (ahdr%ixc)
  case (1,7)
   GS_xc_FUNCTIONAL=XC_LDA_C_PW
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
  case (2)
   GS_xc_FUNCTIONAL=XC_LDA_C_PZ
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
  case (3)
   GS_xc_FUNCTIONAL=XC_LDA_C_PZ
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
  case (4)
   GS_xc_FUNCTIONAL=XC_LDA_C_WIGNER
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
  case (5)
   GS_xc_FUNCTIONAL=XC_LDA_C_HL
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
  case (6)
   GS_xc_FUNCTIONAL=XC_LDA_C_XALPHA
   GS_xc_KIND=XC_EXCHANGE_CORRELATION
 end select

 read(11)
 read(11) 
 read(11) a_nsym, en%nb, ng_vec, ishm, pp_n_l_comp_max
 n_bands= en%nb
!
 n_spin=1
 n_spinor=1
 n_sp_pol=1
!
 k%nibz      = ahdr%nkpt
 nkibz       = k%nibz
 default_Tel = ahdr%tphysel 
 i_time_rev  = 1
 a(1,:)  = ahdr%rprimd(:,1)
 a(2,:)  = ahdr%rprimd(:,2)
 a(3,:)  = ahdr%rprimd(:,3)
 alat(1) = 2.*maxval(abs(a(1,:)))
 alat(2) = 2.*maxval(abs(a(2,:)))
 alat(3) = 2.*maxval(abs(a(3,:)))
 default_nel = 0.
 do i1 = 1, ahdr%natom
   i2 = ahdr%typat(i1)
   inel = ahdr%znucltypat(i2)
   do i3 = 1, ahdr%npsp
     if(ahdr%znuclpsp(i3) == ahdr%znucltypat(i2)) inel = ahdr%zionpsp(i3)
   enddo
   default_nel = default_nel + inel
 enddo
!
 do i1 = 1,3
   DL_vol = DL_vol+a(1,i1)*internal_product(a(2,:),a(3,:),i1)
 enddo
 do i1 = 1,3
   b(1,i1) = internal_product(a(2,:),a(3,:),i1)*2.*pi/DL_vol
   b(2,i1) = internal_product(a(3,:),a(1,:),i1)*2.*pi/DL_vol
   b(3,i1) = internal_product(a(1,:),a(2,:),i1)*2.*pi/DL_vol
 enddo
 allocate(k%pt(k%nibz,3))
!
 do i1 = 1,k%nibz
   k%pt(i1,:)=matmul(transpose(b),ahdr%kptns(:,i1))*alat(:)/2./pi
 enddo
!
 call msg('s',":: Atoms positions...")
!=====================================
!
 n_atom_species=ahdr%ntypat
 allocate(n_atoms(n_atom_species),pp_n_l_comp(n_atom_species))
 pp_n_l_comp=pp_n_l_comp_max
 n_atoms(:)=0
 do i1=1,ahdr%natom
   n_atoms( ahdr%typat(i1) ) = n_atoms( ahdr%typat(i1) ) +1
 enddo
 n_atoms_max=maxval(n_atoms)
 allocate(atom_pos(3,n_atoms_max,n_atom_species))
 n_atoms(:)=0
 do i1=1,ahdr%natom
   n_atoms( ahdr%typat(i1) ) = n_atoms( ahdr%typat(i1) ) +1
   atom_pos(:, n_atoms( ahdr%typat(i1) ) ,ahdr%typat(i1) )=matmul(transpose(a),ahdr%xred(:,i1))
 enddo
 call msg('l','done')
!
! ALLOCATION
!
 allocate(ng_wf(k%nibz),g_vec(ng_vec,3),rstr(en%nb))
 allocate(en%E(en%nb,k%nibz,n_spin),agvec(3,ng_vec),asop(3,3,a_nsym))
!
 call msg('s','Symmetries...')
!===============================
 read(11) (((asop(i1,i2,i3),i1=1,3),i2=1,3),i3=1,a_nsym)
 read(11) 
 l_identity=.false.
 l_inversion=.false.
 do i1=1,a_nsym
   if (asop(1,1,i1)==1.and.asop(1,2,i1)==0.and.asop(1,3,i1)==0.and.&
&      asop(2,1,i1)==0.and.asop(2,2,i1)==1.and.asop(2,3,i1)==0.and.&
&      asop(3,1,i1)==0.and.asop(3,2,i1)==0.and.asop(3,3,i1)==1) l_identity=.true.
   if (asop(1,1,i1)==-1.and.asop(1,2,i1)==0.and.asop(1,3,i1)==0.and.&
&      asop(2,1,i1)==0.and.asop(2,2,i1)==-1.and.asop(2,3,i1)==0.and.&
&      asop(3,1,i1)==0.and.asop(3,2,i1)==0.and.asop(3,3,i1)==-1) l_inversion=.true.
 enddo
 if (.not.l_identity) call msg("l","[ID not found]...")
 if (l_identity) call msg("l","[ID found]...")
 if (l_inversion) then
   if (l_identity) then
     call msg("l","[INV found. Time rev is off]")
     i_time_rev=0
   else
     call msg("l","[INV found]")
     asop(:,:,:)=-asop(:,:,:)
   endif
 else 
   call msg("l","[INV not found]")
 endif
!
 if (n_spin==2) then
   i_time_rev=0
   call msg("l","...[Time rev off]")
   nsym=a_nsym*2
 else
   nsym=a_nsym
   if (i_time_rev==1) nsym=a_nsym*2
 endif
!
 call msg('s',':: Electrons             :',default_nel)
 call msg('s',':: Temperature       [ev]:',default_Tel)
 call msg('s',':: Lattice factors [a.u.]:',alat)
 call msg('s',':: K-points              :',k%nibz)
 call msg('s',':: Bands                 :',en%nb)
 call msg('s',':: Spin  [components/pol]:',(/n_spin,n_sp_pol/))
 call msg('s',':: Symmetries   [spatial]:',nsym)
 call msg('s','::                [T-rev]:',i_time_rev==1)
 call msg('s',':: (Charge) RL vectors   :',ng_vec)
 call msg('s',':: XC potential          :',xc_string(GS_xc_KIND,GS_xc_FUNCTIONAL))
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
 allocate(dl_sop(3,3,nsym))
 do i1=1,a_nsym
   lsop=matmul(transpose(a),asop(:,:,i1))
   dl_sop(:,:,i1)=matmul(lsop,b)/2./pi
   if (i_time_rev==1.or.n_spin==2) dl_sop(:,:,i1+a_nsym)=-dl_sop(:,:,i1)
 enddo
!
 call msg('s','RL vectors...')
!=============================
 read(11) (agvec(:,i1),i1=1,ng_vec)
 read(11) 
 do i1=1,ng_vec
   g_vec(i1,:)=matmul(transpose(b),agvec(:,i1))*alat(:)/2./pi
 enddo
 ng_wf=ng_vec
 wf_ng_disk=maxval(ng_wf)
 wf_ng=wf_ng_disk
 allocate(wf_igk(wf_ng_disk,k%nibz))
 forall(i1=1:ng_vec) wf_igk(i1,:)=i1
 call msg('l','done')
!
 call msg('s','Energies...')
!===========================
 read(11) 
 do ik=1,k%nibz
   do i1=1,n_atom_species
     do i2=1,pp_n_l_comp_max
       read(11)
       read(11) 
     enddo
   enddo
   read(11) rstr
   en%E(:,ik,1)=rstr(:)
   do i1=1,en%nb
     read(11) 
   enddo
 enddo
 call msg('l','done')
 close(11)
 end subroutine

