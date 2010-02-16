!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
! 
! Copyright (C) 2002-2005 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.
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
subroutine elk_db1(ahdr,en,k,KSS_file_name,read_level)
 !
 use pars,            ONLY:pi,SP
 use defs_datatypes,  ONLY:hdr_type
 use R_lattice,       ONLY:bz_samp,b,g_vec,ng_vec
 use D_lattice,       ONLY:a,n_atomic_species,n_atoms_species,atom_pos,alat,&
&                          Z_species,n_atoms_species_max,input_GS_Tel
 use electrons,       ONLY:n_bands,levels,default_nel,n_spin
 use wave_func,       ONLY:wf_ng,wf_nc_k,wf_ncx,wf_igk
 use vec_operate,     ONLY:sort,v_norm
 use com,             ONLY:error
 implicit none
 !
 integer                        :: read_level
 type(hdr_type)                 :: ahdr
 character(*)                   :: KSS_file_name
 type(levels),     intent(out)  :: en
 type(bz_samp),    intent(out)  :: k  
 !
 integer              ::version(3),is,ia,ik,ic,ig,ng_vec_no_G_centered,ng_vec_to_order,&
&                       g_found
 integer, allocatable ::G_index(:)
 real(8), allocatable ::k_plus_G(:,:,:)
 real(SP),allocatable ::g_vec_to_order(:,:),g_vec_mod_to_order(:)
 real(8)              ::spzn,cv,t,Z_val,atom_pos_tmp(3,100,100),real_string(10000),&
&                       real_v(3)
 !
 open(unit=11,file=KSS_file_name,form='unformatted')
 !
 read(11,err=10) version
 write (ahdr%codvsn,'(I1.1,".",I1.1,".",I2.2)') version
 read(11,err=10) ahdr%rprimd
 read(11,err=10) n_atomic_species
 !
 if (read_level==0) allocate(n_atoms_species(n_atomic_species),Z_species(n_atomic_species))
 !
 default_nel=0.
 do is=1,n_atomic_species
   read(11,err=10) 
   read(11,err=10) spzn
   Z_species(is)=-spzn
   read(11,err=10) Z_val
   read(11,err=10) n_atoms_species(is)
   do ia=1,n_atoms_species(is)
     default_nel=default_nel+Z_val
     read(11,err=10) atom_pos_tmp(:,is,ia)
   end do
 end do
 !
 n_atoms_species_max=maxval(n_atoms_species)
 if (read_level==0) then
   allocate(atom_pos(3,n_atoms_species_max,n_atomic_species))
   atom_pos(:,:,:)=atom_pos_tmp(:,:n_atoms_species_max,:n_atomic_species)
 endif
 !
 read(11,err=10) 
 read(11,err=10) ahdr%ixc
 read(11,err=10) ahdr%tphysel
 read(11,err=10) ahdr%nspinor
 read(11,err=10) ahdr%nsym
 input_GS_Tel=ahdr%tphysel
 ahdr%nsppol=1
 !
 if (read_level==0) allocate(ahdr%symrel(3,3,ahdr%nsym))
 !
 do is=1,ahdr%nsym
   read(11,err=10) ahdr%symrel(:,:,is)
 end do
 !
 read(11,err=10) k%nibz
 read(11,err=10) n_bands
 en%nb=n_bands
 read(11,err=10) 
 read(11,err=10) ng_vec_no_G_centered
 !
 if (read_level==0) allocate(k%pt(k%nibz,3),wf_nc_k(k%nibz))
 if (read_level==1) then
   wf_ncx=maxval(wf_nc_k)
   allocate(k_plus_G(3,ng_vec_no_G_centered,k%nibz))
   allocate(wf_igk(wf_ncx,k%nibz))
   allocate(en%E(en%nb,k%nibz,n_spin))
   wf_igk=0
 endif
 !
 do ik=1,k%nibz
   read(11,err=10) real_v
   read(11,err=10) wf_nc_k(ik)
   if (read_level==1) then
     read(11,err=10) k_plus_G(:,:,ik)
     forall (ic=1:3) k_plus_G(ic,:,ik)=k_plus_G(ic,:,ik)-real_v(ic)
     k%pt(ik,:)=matmul(transpose(b),real_v)*alat(:)/2./pi
     read(11,err=10) real_string(:n_bands)
     en%E(:,ik,1)=real_string(:n_bands)
     read(11,err=10) 
   else
     read(11,err=10) 
     read(11,err=10) 
     read(11,err=10) 
   endif
 end do
 !
 if (read_level==0) then
   close(11)
   return
 endif
 !
 ! Here I need to fill and order the Gamma-Centered list of G-vectors
 !
 allocate(g_vec_to_order(ng_vec_no_G_centered*10,3),g_vec_mod_to_order(ng_vec_no_G_centered*10),&
&         G_index(ng_vec_no_G_centered*10))
 !
 ng_vec_to_order=0
 g_vec_mod_to_order=0.
 do ik=1,k%nibz
   do ic=1,wf_nc_k(ik)
     g_found=0
     do ig=1,ng_vec_to_order
       if (all((/real(k_plus_G(:,ic,ik))==g_vec_to_order(ig,:)/))) wf_igk(ic,ik)=ig
     enddo
     if (wf_igk(ic,ik)==0) then
       ng_vec_to_order=ng_vec_to_order+1
       if (ng_vec_to_order>size(g_vec_to_order,1)) call error('G_VEC_to_order not big enough')
       g_vec_to_order(ng_vec_to_order,:)=k_plus_G(:,ic,ik)
       real_v=matmul(transpose(b),g_vec_to_order(ng_vec_to_order,:))
       g_vec_mod_to_order(ng_vec_to_order)=v_norm( real(real_v,SP) ) 
       wf_igk(ic,ik)=ng_vec_to_order
     endif
   enddo
 enddo
 !
 call sort(arrin=g_vec_mod_to_order(:ng_vec_to_order),indx=G_index(:ng_vec_to_order))
 !
 ng_vec=ng_vec_to_order
 wf_ng =ng_vec
 !
 allocate(g_vec(ng_vec,3))
 !
 do ig=1,ng_vec
   g_vec(ig,:)=matmul(transpose(b),g_vec_to_order(G_index(ig),:))*alat(:)/2./pi
 enddo
 do ik=1,k%nibz
   do ic=1,wf_nc_k(ik)
     wf_igk(ic,ik)=G_index( wf_igk(ic,ik) )
   enddo
 enddo
 !
 close(11)
 !
 deallocate(k_plus_G,G_index,g_vec_to_order,g_vec_mod_to_order)
 !
 return
 !
 10 call error("Reading from YAMBO")
 !
end subroutine
