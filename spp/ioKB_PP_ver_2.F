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
 integer function ioKB_PP_ver_2(ID)
!==================================
!
 use pars,             ONLY:schlen,IP,SP
 use D_lattice,        ONLY:n_atoms_max,n_atom_species,n_atoms,&
&                           atom_pos,pp_n_l_comp_max,&
&                           pp_n_l_comp,pp_kbs,pp_kb,PP_alloc,pp_kbd
 use memory_m,         ONLY:mem_est
 use wave_func,        ONLY:wf_ng_disk,wf_igk,ng_wf_k,wf_ng
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_bulk, io_status, io_bulk,io_header,&
&                           read_is_on,write_is_on
 implicit none
 integer,       intent(in) :: ID
 character(schlen)         :: VAR_NAME
!
 integer :: ik,i1,i2
 integer, allocatable :: I_on_disk(:)
 real(SP),allocatable :: R_on_disk(:)

 ik=maxval(io_sec(ID,:))-1

 ioKB_PP_ver_2 =io_connect(desc="s.kbpp",type=-1,ID=ID)
 if (ioKB_PP_ver_2/=0) goto 1

 if (any ((/io_sec(ID,:)==1/)) ) then
!
! H_versionID
!
   allocate(I_on_disk(4))
   call io_bulk(ID,VAR="H_versionID",VAR_SZ=(/4/))
   call io_bulk(ID,I1=I_on_disk)
   deallocate(I_on_disk)
!
! H1ID 
!
   allocate(I_on_disk(5))
   call io_bulk(ID,VAR="H1ID",VAR_SZ=(/size(I_on_disk)/))
   call io_bulk(ID,I1=I_on_disk)
   n_atom_species=I_on_disk(1)
   n_atoms_max=I_on_disk(2)
   pp_n_l_comp_max=I_on_disk(3)
   deallocate(I_on_disk)
!
   if (read_is_on(ID).and..not.allocated(n_atoms)) then
     allocate(n_atoms(n_atom_species),pp_n_l_comp(n_atom_species))
     allocate(atom_pos(3,n_atoms_max,n_atom_species))
   endif
!
! H2ID
!
   allocate(I_on_disk(2*n_atom_species))
   call io_bulk(ID,VAR="H2ID",VAR_SZ=(/size(I_on_disk)/))
   call io_bulk(ID,I1=I_on_disk)
   n_atoms=I_on_disk(:n_atom_species)
   pp_n_l_comp=I_on_disk(n_atom_species+1:)
   deallocate(I_on_disk)
!
! H2RD
!
   allocate(R_on_disk(3*n_atom_species*n_atoms_max))
   call io_bulk(ID,VAR="H2RD",VAR_SZ=(/size(R_on_disk)/))
   call io_bulk(ID,R1=R_on_disk)
   atom_pos=reshape(R_on_disk,(/3,n_atoms_max,n_atom_species/))
   deallocate(R_on_disk)

 endif

 if (ik==0) goto 1

 if (ik==1) call PP_alloc()
!
 allocate(R_on_disk(2*wf_ng+1))
 write (VAR_NAME,'(a,i5.5,a)') 'Components-',ik,'RD'
 call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=(/n_atom_species*pp_n_l_comp_max/))

 do i1=1,n_atom_species
   do i2=1,pp_n_l_comp(i1)
     call io_bulk(ID,R1=R_on_disk) 
     pp_kb (:,i1,i2)=R_on_disk(:wf_ng)
     pp_kbd(:,i1,i2)=R_on_disk(wf_ng+1:2*wf_ng)
     pp_kbs(i1,i2)=R_on_disk(2*wf_ng+1)
   enddo
 enddo
 deallocate(R_on_disk)
!
1 call io_disconnect(ID=ID)

 end function ioKB_PP_ver_2
