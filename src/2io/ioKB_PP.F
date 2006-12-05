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
 integer function ioKB_PP(ID)
!============================
 use pars,             ONLY:schlen,IP,SP
 use D_lattice,        ONLY:n_atoms_max,n_atom_species,n_atoms,&
&                           atom_pos,pp_n_l_comp_max,&
&                           pp_n_l_comp,pp_kbs,pp_kb,PP_alloc,pp_kbd
 use memory_m,         ONLY:mem_est
 use wave_func,        ONLY:wf_ncx,wf_igk,wf_nc_k,wf_ng
 use IO_m,             ONLY:io_connect, io_disconnect, io_sec, &
&                           io_elemental, io_status, io_bulk,io_header,&
&                           read_is_on,write_is_on
 implicit none
 integer,       intent(in) :: ID
 character(schlen)         :: VAR_NAME
 real(SP),      allocatable:: pp_disk(:,:,:)
!
 integer :: ik,i1
 ik=maxval(io_sec(ID,:))-1

 ioKB_PP=io_connect(desc="kb_pp",type=0,ID=ID)
 if (ioKB_PP/=0) goto 1
!
! Dimensions
!
 if (any((/io_sec(ID,:)==1/))) then
 
   ioKB_PP=io_header(ID,IMPOSE_SN=.true.)
   if (ioKB_PP/=0) goto 1

   call io_elemental(ID,VAR="PARS",VAR_SZ=3,MENU=0)
   !
   ! n_atoms_max and n_atom_species are redundant as they are
   ! already read from the DB1.
   ! In the case when kb_pp is not present these variables
   ! are available anyway.
   !
   call io_elemental(ID,I0=n_atoms_max)
   call io_elemental(ID,I0=n_atom_species)
   !
   call io_elemental(ID,I0=pp_n_l_comp_max)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)

   if (read_is_on(ID)) then

     if (.not.allocated(n_atoms)) then
       allocate(n_atoms(n_atom_species))
       allocate(atom_pos(3,n_atoms_max,n_atom_species))
       call mem_est("n_atoms atom_pos",&
&                   (/n_atom_species,size(atom_pos)/),(/IP,SP/))
     endif
     if (.not.allocated(pp_n_l_comp)) then
       allocate(pp_n_l_comp(n_atom_species))
       call mem_est("pp_n_l_comp",(/n_atom_species/),(/IP/))
     endif

   endif

   call io_bulk(ID,VAR="N_ATOMS",VAR_SZ=(/n_atom_species/))
   call io_bulk(ID,I1=n_atoms)
   call io_bulk(ID,VAR="ATOM_L_COMP",VAR_SZ=(/n_atom_species/))
   call io_bulk(ID,I1=pp_n_l_comp)
   call io_bulk(ID,VAR="ATOM_POS",VAR_SZ=shape(atom_pos))
   call io_bulk(ID,R3=atom_pos)

   if (ioKB_PP/=0) goto 1
 endif

 if (ik==0) goto 1

 if (ik==1) then
   if (read_is_on(ID)) call PP_alloc()
   call io_bulk(ID,VAR="PP_KBS",VAR_SZ=shape(pp_kbs))
   call io_bulk(ID,R2=pp_kbs)
 endif
!

 allocate(pp_disk(wf_ncx,n_atom_species,pp_n_l_comp_max))

 if (write_is_on(ID)) pp_disk=pp_kb

 write (VAR_NAME,'(a,i4.4)') 'PP_KB_IK',ik
 call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=shape(pp_disk))
 call io_bulk(ID,R3=pp_disk)

 if (read_is_on(ID)) then
   pp_kb=0._SP
   do i1=1,wf_nc_k(ik)
     if (wf_igk(i1,ik) > wf_ng) cycle
     pp_kb( wf_igk(i1,ik) ,:,:) = pp_disk(i1,:,:)
   enddo
 endif

 if (write_is_on(ID)) pp_disk=pp_kbd

 write (VAR_NAME,'(a,i4.4)') 'PP_KBD_IK',ik
 call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=shape(pp_disk))
 call io_bulk(ID,R3=pp_disk)

 if (read_is_on(ID)) then
   pp_kbd=0._SP
   do i1=1,wf_nc_k(ik)
     if (wf_igk(i1,ik) > wf_ng) cycle
     pp_kbd( wf_igk(i1,ik) ,:,:) = pp_disk(i1,:,:)
   enddo
 endif

 deallocate(pp_disk)

1 call io_disconnect(ID=ID)

 end function ioKB_PP
