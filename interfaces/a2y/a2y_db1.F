!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
&                                 n_spinor,l_spin_orbit,n_spin_den 
 use R_lattice,             ONLY: bz_samp, ng_vec, g_vec,nkibz, b
 use D_lattice,             ONLY: DL_vol, a, alat, input_GS_Tel,i_space_inv,&
&                                 n_atomic_species,n_atoms_species,i_time_rev,&
&                                 n_atoms_species_max,atom_pos,Z_species,lattice
 use pseudo,                ONLY: pp_n_l_times_proj_max,pp_n_l_comp,&
&                                 pp_n_l_max,pp_table,l_many_proj
 use wave_func,             ONLY: wf_nc_k, wf_igk,wf_ncx,wf_ng
 use vec_operate,           ONLY: cross_product,v_is_zero
 use defs_datatypes,        ONLY: hdr_type, wffile_type
 use xc_functionals,        ONLY: GS_xc_KIND,GS_xc_FUNCTIONAL
 use mod_xc2y,              ONLY: XC_yamboID, XC_yamboID2kind
 use mod_com2y,             ONLY: print_interface_dimensions,symmetries_check_and_load,&
&                                 alat_mult_factor,artificial_spin_pol,ng_vec_abinit
 use vec_operate,           ONLY: v_is_zero
 !
#include<memory.h>
 character(*)                   :: KSS_file_name
 type(levels),     intent(out)  :: en
 type(bz_samp),    intent(out)  :: k  
 !
 !Work Space
 !
 real(SP)                       :: cp(3)
 integer                        :: i1,ik,i2,i3,inel,i_spin,i_spinor,ic,ig
 complex(DP),allocatable        :: wf_disk_DP(:,:)
 integer, allocatable           :: wf_igk_tmp(:,:)
 logical                        :: gamma_only
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
 read(11) ahdr%nsym, n_bands, ng_vec_abinit, ishm, pp_n_l_times_proj_max
 !
 l_many_proj= (ishm==-1)
 !
 if(     l_many_proj) then
   read(11)  pp_n_l_max, psp_so, n_spin_den 
   read(11) 
   read(11) 
   read(11) 
   !
   l_spin_orbit=(psp_so>1)
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
 GS_xc_FUNCTIONAL = XC_yamboID('abinit',abinit_func=ahdr%ixc)
 GS_xc_KIND = XC_yamboID2kind(GS_xc_FUNCTIONAL)
 !
 n_spinor=ahdr%nspinor
 n_sp_pol=ahdr%nsppol
 !
 if (n_sp_pol==1.and.artificial_spin_pol) n_sp_pol=2
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
 !
 cp=cross_product(a(2,:),a(3,:))
 do i1 = 1,3
   DL_vol = DL_vol+a(1,i1)*cp(i1)
 enddo
 b(1,:) = cross_product(a(2,:),a(3,:))*2._SP*pi/DL_vol
 b(2,:) = cross_product(a(3,:),a(1,:))*2._SP*pi/DL_vol
 b(3,:) = cross_product(a(1,:),a(2,:))*2._SP*pi/DL_vol
 !
 k%nibz      = ahdr%nkpt
 nkibz       = k%nibz
 YAMBO_ALLOC(k%pt,(k%nibz,3))
 do ik = 1,k%nibz
   k%pt(ik,:)=matmul(transpose(b),ahdr%kptns(:,ik))*alat(:)/2._SP/pi
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
 YAMBO_ALLOC(pp_n_l_comp,(n_atomic_species))
 YAMBO_ALLOC(n_atoms_species,(n_atomic_species))
 YAMBO_ALLOC(pp_table,(3,n_atomic_species,pp_n_l_times_proj_max))
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
 YAMBO_ALLOC(atom_pos,(3,n_atoms_species_max,n_atomic_species))
 YAMBO_ALLOC(Z_species,(n_atomic_species))
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
 YAMBO_ALLOC(wf_nc_k,(k%nibz))
 YAMBO_ALLOC(rel_gvec,(3,ng_vec_abinit))
 YAMBO_ALLOC(en%E,(en%nb,k%nibz,n_sp_pol))
 YAMBO_ALLOC(g_vec,(ng_vec_abinit,3))
 YAMBO_ALLOC(rstr,(en%nb))
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
 read(11) (rel_gvec(:,ig),ig=1,ng_vec_abinit)
 read(11) 
 read(11) 
 do ig=1,ng_vec_abinit
   g_vec(ig,:)=matmul(transpose(b),real(rel_gvec(:,ig),SP))*alat(:)/2._SP/pi
 enddo
 call msg('l','done')
 !
 ng_vec=ng_vec_abinit
 !
 gamma_only=(nkibz==1 .and. all(k%pt(1,:)==0._SP) )
 !
 if( (i_time_rev==1 .or. i_space_inv==1) .and. .not.gamma_only) then
   !
   call msg('s','Closing shells against inversion...')
   !============================
   !
   call G_rot_grid(-1,'extend_grid')
   !
   call msg('l','done')
   !
   if(ng_vec/=ng_vec_abinit) call msg('s',':: ng_vec was increased to close the G-shells')
   !
 endif
 !
 call msg('s','Energies...')
 !==========================
 !
 YAMBO_ALLOC(wf_disk_DP,(ng_vec_abinit,n_spinor))
 YAMBO_ALLOC(wf_igk_tmp,(ng_vec_abinit,k%nibz))
 wf_igk_tmp=-1
 WF_disk_DP=0._DP
 !
 do i_spin=1,n_sp_pol
   !
   if (i_spin==2.and.artificial_spin_pol) then
     en%E(:,:,2)=en%E(:,:,1)
     cycle
   endif
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
       if(i1==1.and.i_spin==1) read(11) (wf_disk_DP(:,i_spinor),i_spinor=1,n_spinor)
       if(i1/=1.or. i_spin==2) read(11)
     enddo
     if(i_spin>1) cycle
     ic=0
     do ig=1,ng_vec_abinit
       if(all(wf_disk_DP(ig,:)==0)) cycle
       ic=ic+1
       wf_igk_tmp(ic,ik)=ig
     enddo
     wf_nc_k(ik)=ic
   enddo
   ! 
   if(i_spin>1) cycle
   !
   wf_ncx=maxval(wf_nc_k)
   wf_ng=maxval(wf_igk_tmp)
   !
   YAMBO_ALLOC(wf_igk,(wf_ncx,k%nibz))
   do ik=1,k%nibz
     wf_igk(1:wf_ncx,ik)=wf_igk_tmp(1:wf_ncx,ik)
   enddo
 enddo
 !
 ! CLEAN
 !
 YAMBO_FREE(wf_igk_tmp)
 YAMBO_FREE(wf_disk_DP)
 YAMBO_FREE(rel_gvec)
 YAMBO_FREE(rstr)
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
