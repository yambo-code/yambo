!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function io_kb_abinit_old(ID)
 !
 use pars,             ONLY:schlen,SP
 use stderr,           ONLY:intc
 use D_lattice,        ONLY:n_atoms_species_max,n_atomic_species,n_atoms_species,&
&                           atom_pos
 use R_lattice,        ONLY:ng_vec
 use pseudo,           ONLY:pp_n_l_times_proj_max,pp_table,pp_n_l_max,&
&                           pp_n_l_comp,pp_kbs_old,pp_kb_old,pp_kbd_old, &
&                           PP_alloc_abinit_old
 use IO_m,             ONLY:io_connect,io_disconnect,io_sec,io_fragment, &
&                           io_elemental,io_bulk,io_header,&
&                           read_is_on,write_is_on,ver_is_gt_or_eq,frag_WF
 use electrons,        ONLY:n_sp_pol
 !
#include<memory.h>
 !
 integer,       intent(in) :: ID
 character(schlen)         :: VAR_NAME
 real(SP),      allocatable:: pp_disk(:,:,:)
 ! 
 ! Work Space
 !
 integer :: ik,i1,i_spin,var_size
 ik=maxval(io_sec(ID,:))-1
 !
 io_kb_abinit_old=io_connect(desc="kb_pp",type=0,ID=ID)
 if (io_kb_abinit_old/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_kb_abinit_old=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   if (io_kb_abinit_old/=0) goto 1
   !
   var_size=3
   if (ver_is_gt_or_eq(ID,revision=898)) var_size=4
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=var_size,MENU=0)
   !
   ! n_atoms_max and n_atom_species are redundant as they are
   ! already read from the DB1.
   ! In the case when kb_pp is not present these variables
   ! are available anyway.
   !
   call io_elemental(ID,I0=n_atoms_species_max)
   call io_elemental(ID,I0=n_atomic_species)
   if (ver_is_gt_or_eq(ID,revision=898)) &
&    call io_elemental(ID,I0=pp_n_l_times_proj_max)
   call io_elemental(ID,I0=pp_n_l_max)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   if (read_is_on(ID)) then
     ! 
     if (.not.allocated(n_atoms_species)) then
       YAMBO_ALLOC(n_atoms_species,(n_atomic_species))
       YAMBO_ALLOC(atom_pos,(3,n_atoms_species_max,n_atomic_species))
     endif
     if (.not.allocated(pp_n_l_comp)) then
       YAMBO_ALLOC(pp_n_l_comp,(n_atomic_species))
     endif
     !
   endif
   !
   call io_bulk(ID,VAR="N_ATOMS",VAR_SZ=(/n_atomic_species/))
   call io_bulk(ID,I1=n_atoms_species)
   call io_bulk(ID,VAR="ATOM_L_COMP",VAR_SZ=(/n_atomic_species/))
   call io_bulk(ID,I1=pp_n_l_comp)
   !
   if (.not.ver_is_gt_or_eq(ID,revision=898)) pp_n_l_times_proj_max=maxval(pp_n_l_comp)
   if (read_is_on(ID)) then
     YAMBO_FREE(pp_table)
     YAMBO_ALLOC(pp_table,(3,n_atomic_species,pp_n_l_times_proj_max))
   endif
   !
   if (ver_is_gt_or_eq(ID,revision=898)) then
     call io_bulk(ID,VAR="PP_TABLE",VAR_SZ=(/3,n_atomic_species,pp_n_l_times_proj_max/))
     call io_bulk(ID,I3=pp_table)
   else
     do i1=1,pp_n_l_times_proj_max
       pp_table(1,:,i1)=i1
       pp_table(2,:,i1)=1
       pp_table(3,:,i1)=1
     enddo
   endif
   !
   call io_bulk(ID,VAR="ATOM_POS",VAR_SZ=shape(atom_pos))
   call io_bulk(ID,R3=atom_pos)
   !
   if (io_kb_abinit_old/=0) goto 1
   !
 endif
 !
 if (ik==0) goto 1
 !
 if (ik==1) then
   if (read_is_on(ID)) call PP_alloc_abinit_old()
   call io_bulk(ID,VAR="PP_KBS",VAR_SZ=shape(pp_kbs_old))
   call io_bulk(ID,R2=pp_kbs_old)
 endif
 !
 YAMBO_ALLOC(pp_disk,(ng_vec,n_atomic_species,pp_n_l_times_proj_max))
 !
 call io_fragment(ID,i_fragment=ik)
 !
 if (read_is_on(ID)) then
   pp_kb_old=0._SP
   pp_kbd_old=0._SP
 endif
 ! 
 do i_spin=1,n_sp_pol
   ! 
   if (write_is_on(ID)) pp_disk=pp_kb_old(:,:,:,i_spin)
   !
   if (ver_is_gt_or_eq(ID,revision=374)) then
     call io_bulk(ID,VAR='PP_KB_IK'//trim(intc(ik))//'_SPIN'//trim(intc(i_spin)),VAR_SZ=shape(pp_disk)) 
   else
     write (VAR_NAME,'(a,i4.4)') 'PP_KB_IK',ik
     call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=shape(pp_disk)) 
   endif
   call io_bulk(ID,R3=pp_disk)
   !
   if (read_is_on(ID)) pp_kb_old(:,:,:,i_spin) = pp_disk(:,:,:)
   !
   if (write_is_on(ID)) pp_disk=pp_kbd_old(:,:,:,i_spin)
   !
   if (ver_is_gt_or_eq(ID,revision=374)) then
     call io_bulk(ID,VAR='PP_KBD_IK'//trim(intc(ik))//'_SPIN'//trim(intc(i_spin)),VAR_SZ=shape(pp_disk)) 
   else
     write (VAR_NAME,'(a,i4.4)') 'PP_KBD_IK',ik
     call io_bulk(ID,VAR=trim(VAR_NAME),VAR_SZ=shape(pp_disk)) 
   endif
   call io_bulk(ID,R3=pp_disk)
   !
   if (read_is_on(ID)) pp_kbd_old(:,:,:,i_spin) = pp_disk(:,:,:)
   !
 enddo
 !
 YAMBO_FREE(pp_disk)
 !
1 call io_disconnect(ID=ID)
 !
end function io_kb_abinit_old
