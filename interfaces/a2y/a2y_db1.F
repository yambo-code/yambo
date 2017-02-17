!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
&                                 n_spinor,spin_orbit 
 use R_lattice,             ONLY: bz_samp, ng_vec, g_vec,nkibz, b
 use D_lattice,             ONLY: DL_vol, a, alat, input_GS_Tel,&
&                                 n_atomic_species,n_atoms_species,&
&                                 n_atoms_species_max,atom_pos,Z_species,lattice
 use pseudo,                ONLY: pp_n_l_times_proj_max,pp_n_l_comp,&
&                                 pp_n_l_max,pp_table,l_many_proj
 use wave_func,             ONLY: wf_nc_k, wf_igk,wf_ncx,wf_ng
 use vec_operate,           ONLY: cross_product,v_is_zero
 use defs_datatypes,        ONLY: hdr_type, wffile_type
 use xc_functionals,        ONLY: GS_xc_KIND,GS_xc_FUNCTIONAL
 use mod_xc2y,              ONLY: XC_Abinit2Yambo,XC_Libxc2Yambo
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
 integer                        :: psp_so
 integer                        :: fform,ishm
 integer,  allocatable          :: rel_gvec(:,:)
 real(DP), allocatable          :: rstr(:)
 !
 call msg('s','KSS Header...')
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
 read(11) ahdr%nsym, n_bands, ng_vec, ishm, pp_n_l_times_proj_max
 !
 l_many_proj= (ishm==-1)
 !
 if(     l_many_proj) then
   read(11)  pp_n_l_max, psp_so
   read(11) 
   read(11) 
   read(11) 
   !
   spin_orbit=(psp_so>1)
 else
   pp_n_l_max=abs(pp_n_l_times_proj_max)
 endif
 en%nb=n_bands
 !
 call msg('s','Pseudo Potential KB form factors...')
 call msg('l','yes')
 !
 ! XC KIND/FUNCTIONAL
 !===================
 !
 if (ahdr%ixc.ge. 0) then
   call XC_Abinit2Yambo(ahdr%ixc,GS_xc_FUNCTIONAL,GS_xc_KIND)
 else !negative ahdr%ixc means abinit used libxc
   call XC_Libxc2Yambo(ahdr%ixc,GS_xc_FUNCTIONAL,GS_xc_KIND)
 end if
 !
 n_spinor=1
 n_sp_pol=1
 !
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
 call msg('s',"Atom's informations...")
 !=====================================
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
 !
 allocate(n_atoms_species(n_atomic_species),pp_n_l_comp(n_atomic_species),&
&         pp_table(3,n_atomic_species,pp_n_l_times_proj_max))
 pp_n_l_comp(:)=pp_n_l_max
 !
 ! if "l_many_proj" the table is readen in a2y_wf
 if(.not.l_many_proj) then
   do i1=1,pp_n_l_times_proj_max
     pp_table(1,:,i1)=i1  !  l+1
     pp_table(2,:,i1)=1   !  n_proj
     pp_table(3,:,i1)=1   !  i_spin
   enddo
 endif
 !
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
  i_spin=1
 ! 
   !
   do ik=1,k%nibz
     do i1=1,n_atomic_species
       do i2=1,pp_n_l_times_proj_max
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
 !
 ! CLEAN
 !
 deallocate(rel_gvec,rstr)
 !
 close(11)
 !
 call msg('l','done')
 !
 call msg('s','Report:')
 !======================
 !
 call print_interface_dimensions(en,k)
 !
end subroutine
