!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine a2y_db1(en,k,KSS_file_name)
 !
 use pars,                  ONLY: SP, DP,pi
 use com,                   ONLY: msg,warning,error
 use electrons,             ONLY: levels, default_nel,n_bands,n_spin,n_sp_pol,&
&                                 n_spinor 
 use R_lattice,             ONLY: bz_samp, ng_vec, g_vec,nkibz, b
 use D_lattice,             ONLY: DL_vol, a, alat, input_GS_Tel,&
&                                 n_atomic_species,n_atoms_species,&
&                                 n_atoms_species_max,atom_pos,Z_species,lattice
 use pseudo,                ONLY: pp_n_l_comp_max,pp_n_l_comp
 use wave_func,             ONLY: wf_nc_k, wf_igk,wf_ncx,wf_ng
 use vec_operate,           ONLY: cross_product,v_is_zero
 use defs_datatypes,        ONLY: hdr_type, wffile_type
 use xc_functionals,        ONLY: XC_EXCHANGE_CORRELATION,GS_xc_KIND,GS_xc_FUNCTIONAL,&
&                                 XC_LDA_C_PW,XC_LDA_C_PZ,XC_LDA_C_WIGNER,&
&                                 XC_LDA_C_HL,XC_LDA_C_XALPHA,NONE,XC_LDA_X,&
&                                  XC_EXCHANGE
 use mod_com2y,             ONLY: print_interface_dimensions,symmetries_check_and_load,&
&                                 alat_mult_factor,artificial_spin_pol
 implicit none
 character(*)                   :: KSS_file_name
 type(levels),     intent(out)  :: en
 type(bz_samp),    intent(out)  :: k  
 !
 !Work Space
 !
 real(SP)                       :: cp(3)
 integer                        :: i1, ik, i2, i3, inel,i_spin
 !
 !ABINIT
 !
 type(hdr_type)                 :: ahdr
 type(wffile_type)              :: wff
 integer                        :: fform,ishm
 integer,  allocatable          :: rel_gvec(:,:)
 real(DP), allocatable          :: rstr(:)
 !
 call msg('s','KSS Header...')
 !
#if !defined _ELK
 !
 ! ABINIT KSS FILE
 !
 open(unit=11,file=KSS_file_name,form='unformatted')
 wff%unwff=11
 wff%accesswff=0
 fform=1
 call hdr_io_wfftype(fform,ahdr,5,wff)
 call msg('l','abinit version ',ahdr%codvsn)
 read(11)
 read(11) 
 read(11) ahdr%nsym, n_bands, ng_vec, ishm, pp_n_l_comp_max
 en%nb=n_bands
 !
#else
 !
 ! ELK level 0
 !============
 !
 call elk_db1(ahdr,en,k,KSS_file_name,0)
 call msg('l','ELK version ',ahdr%codvsn)
 !
#endif
 !
 ! XC KIND/FUNCTIONAL
 !===================
 !
 select case (ahdr%ixc)
  case (0)
   GS_xc_FUNCTIONAL=NONE
   GS_xc_KIND=NONE
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
  case (8)
   GS_xc_FUNCTIONAL=XC_EXCHANGE
   GS_xc_KIND=XC_LDA_X
  case (9:)
   call warning ('GGA not supported: switching to PZ')
   GS_xc_FUNCTIONAL = XC_LDA_C_PZ
   GS_xc_KIND = XC_EXCHANGE_CORRELATION
 end select
 !
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 !n_spinor=1
 !n_sp_pol=1
 ! GPL_INCLUDE_END SPIN_IGNORE
 !
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 n_spinor=ahdr%nspinor
 n_sp_pol=ahdr%nsppol
 !
 if (n_sp_pol==1.and.artificial_spin_pol) n_sp_pol=2
 !
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
 n_spin=max(n_sp_pol,n_spinor)
 !
 input_GS_Tel = ahdr%tphysel 
 a(1,:)  = ahdr%rprimd(:,1)
 a(2,:)  = ahdr%rprimd(:,2)
 a(3,:)  = ahdr%rprimd(:,3)
 !
 alat(1) = maxval(abs(a(1,:)))*alat_mult_factor
 alat(2) = maxval(abs(a(2,:)))*alat_mult_factor
 alat(3) = maxval(abs(a(3,:)))*alat_mult_factor
 call crystal_lattice()
 if (trim(lattice)=='Unknown'.and.alat_mult_factor==1.) then
   alat(1) = maxval(abs(a(1,:)))*2.
   alat(2) = maxval(abs(a(2,:)))*2.
   alat(3) = maxval(abs(a(3,:)))*2.
   call crystal_lattice()
 endif
 !
 cp=cross_product(a(2,:),a(3,:))
 do i1 = 1,3
   DL_vol = DL_vol+a(1,i1)*cp(i1)
 enddo
 b(1,:) = cross_product(a(2,:),a(3,:))*2.*pi/DL_vol
 b(2,:) = cross_product(a(3,:),a(1,:))*2.*pi/DL_vol
 b(3,:) = cross_product(a(1,:),a(2,:))*2.*pi/DL_vol
 !
#if defined _ELK
 !
 ! ELK 
 !######
 !
 ! I/O level 1
 !============
 !
 call elk_db1(ahdr,en,k,KSS_file_name,1)
 !
 call msg('s','Symmetries...')
 !============================
 call symmetries_check_and_load(ahdr%symrel,ahdr%nsym)
 !
#else
 !
 k%nibz      = ahdr%nkpt
 nkibz       = k%nibz
 allocate(k%pt(k%nibz,3))
 do i1 = 1,k%nibz
   k%pt(i1,:)=matmul(transpose(b),ahdr%kptns(:,i1))*alat(:)/2./pi
 enddo
 !
 ! ABINIT 
 !#########
 !
 call msg('s',":: Atom's informations...")
 !=========================================
 !
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
 n_atomic_species=ahdr%ntypat
 allocate(n_atoms_species(n_atomic_species),pp_n_l_comp(n_atomic_species))
 pp_n_l_comp=pp_n_l_comp_max
 n_atoms_species(:)=0
 do i1=1,ahdr%natom
   n_atoms_species( ahdr%typat(i1) ) = n_atoms_species( ahdr%typat(i1) ) +1
 enddo
 n_atoms_species_max=maxval(n_atoms_species)
 allocate(atom_pos(3,n_atoms_species_max,n_atomic_species),Z_species(n_atomic_species))
 n_atoms_species(:)=0
 do i1=1,ahdr%natom
   n_atoms_species( ahdr%typat(i1) ) = n_atoms_species( ahdr%typat(i1) ) +1
   atom_pos(:, n_atoms_species( ahdr%typat(i1) ) ,ahdr%typat(i1) )=matmul(transpose(a),ahdr%xred(:,i1))
 enddo
 do i1=1,ahdr%npsp
   Z_species(i1) = ahdr%znuclpsp(i1)
 enddo 
 !
 call msg('l','done')
 !
 ! ALLOCATION
 !
 allocate(wf_nc_k(k%nibz),g_vec(ng_vec,3),rstr(en%nb))
 allocate(en%E(en%nb,k%nibz,n_spin),rel_gvec(3,ng_vec))
 !
 call msg('s','Symmetries...')
 !============================
 !
 read(11) (((ahdr%symrel(i1,i2,i3),i1=1,3),i2=1,3),i3=1,ahdr%nsym)
 read(11) 
 !
 do i1=1,ahdr%nsym
   if (.not.v_is_zero( real(ahdr%tnons(:,i1),SP) ) ) then
     call error(' Non-symmorphic symmetry operations are not supported! Use "symmorphi 0" in ABINIT')
   endif
 enddo
 !
 call symmetries_check_and_load(ahdr%symrel,ahdr%nsym)
 !
 call msg('s','RL vectors...')
 !============================
 !
 read(11) (rel_gvec(:,i1),i1=1,ng_vec)
 read(11) 
 read(11) 
 do i1=1,ng_vec
   g_vec(i1,:)=matmul(transpose(b),rel_gvec(:,i1))*alat(:)/2./pi
 enddo
 wf_nc_k=ng_vec
 wf_ncx=maxval(wf_nc_k)
 wf_ng=wf_ncx
 allocate(wf_igk(wf_ncx,k%nibz))
 forall(i1=1:ng_vec) wf_igk(i1,:)=i1
 call msg('l','done')
 !
 call msg('s','Energies...')
 !==========================
 !
 ! GPL_INCLUDE_START SPIN_IGNORE
 ! i_spin=1
 ! GPL_INCLUDE_END SPIN_IGNORE
 ! 
 ! GPL_EXCLUDE_START SPIN_IGNORE
 !
 do i_spin=1,n_spin
   !
   if (i_spin==2.and.artificial_spin_pol) then
     en%E(:,:,2)=en%E(:,:,1)
     cycle
   endif
   ! 
   ! GPL_EXCLUDE_END SPIN_IGNORE
   !
   do ik=1,k%nibz
     do i1=1,n_atomic_species
       do i2=1,pp_n_l_comp_max
         read(11)
         read(11) 
       enddo
     enddo
     read(11) rstr
     en%E(:,ik,i_spin)=rstr(:)
     do i1=1,en%nb
       read(11) 
     enddo
   enddo
   ! 
   ! GPL_EXCLUDE_START SPIN_IGNORE
   !
 enddo
 ! 
 ! GPL_EXCLUDE_END SPIN_IGNORE
 !
 ! CLEAN
 !
 deallocate(rel_gvec,rstr)
 !
 close(11)
 !
#endif
 !
 call msg('l','done')
 !
 call msg('s','Report:')
 !======================
 !
 call print_interface_dimensions(en,k)
 !
end subroutine
