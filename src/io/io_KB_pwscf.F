!
!        Copyright (C) 2000-2018 the YAMBO team
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
integer function io_KB_pwscf(ID)
 !
 use pars,             ONLY:SP
 use stderr,           ONLY:intc
 use R_lattice,        ONLY:nkibz
 use D_lattice,        ONLY:n_atomic_species,atom_pos,n_atoms_species, &
&                           n_atoms_species_max
 use IO_m,             ONLY:io_connect,io_disconnect,io_sec, &
&                           io_elemental,io_bulk,io_header,&
&                           read_is_on,io_fragment,frag_WF
 use pseudo,           ONLY:pp_n_l_times_proj_max,pp_table,pp_n_l_max,&
&                           pp_n_l_comp,pp_kbs,pp_kb,pp_kbd, &
&                           PP_alloc_pwscf
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
#include<memory.h>
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 integer :: ifrag,ik,i_sp_pol
 !
#if defined _TIMING
 call timing('io_KB_pwscf',OPR='start')
#endif
 !
 ifrag=maxval(io_sec(ID,:))-1
 if(ifrag<=nkibz) then
   ik=ifrag
   i_sp_pol=1
 else
   ik=ifrag-nkibz
   i_sp_pol=2
 endif
 !
 io_KB_pwscf=io_connect(desc="kb_pp_pwscf",type=0,ID=ID)
 if (io_KB_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_KB_pwscf=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   if (io_KB_pwscf/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=4,MENU=0)
   !
   ! n_atoms_max and n_atom_species are redundant as they are
   ! already read from the DB1.
   ! In the case when kb_pp is not present these variables
   ! are available anyway.
   !
   call io_elemental(ID,I0=n_atoms_species_max)
   call io_elemental(ID,I0=n_atomic_species)
   call io_elemental(ID,I0=pp_n_l_times_proj_max)
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
   if (read_is_on(ID)) then
     YAMBO_FREE(pp_table)
     YAMBO_ALLOC(pp_table,(3,n_atomic_species,pp_n_l_times_proj_max))
   endif
   !
   call io_bulk(ID,VAR="PP_TABLE",VAR_SZ=(/3,n_atomic_species,pp_n_l_times_proj_max/))
   call io_bulk(ID,I3=pp_table)
   !
   call io_bulk(ID,VAR="ATOM_POS",VAR_SZ=shape(atom_pos))
   call io_bulk(ID,R3=atom_pos)
   !
   if (io_KB_pwscf/=0) goto 1
   !
 endif
 !
 if (ik==0) goto 1
 !
 if (ik==1.and.i_sp_pol==1) then
   if (read_is_on(ID)) call PP_alloc_pwscf()
   call io_bulk(ID,VAR="PP_KBS",VAR_SZ=shape(pp_kbs))
   call io_bulk(ID,R2=pp_kbs)
 endif
 !
 call io_fragment(ID,i_fragment=ik)
 !
 if (read_is_on(ID)) then
   pp_kb=0._SP
   pp_kbd=0._SP
 endif
 !
 call io_bulk(ID,VAR="PP_KB_K"//trim(intc(ik)),VAR_SZ=shape(pp_kb(:,:,:,1)))
 call io_bulk(ID,R3=pp_kb(:,:,:,1))
 !
 call io_bulk(ID,VAR="PP_KBD_K"//trim(intc(ik)),VAR_SZ=shape(pp_kbd(:,:,:,1)))
 call io_bulk(ID,R3=pp_kbd(:,:,:,1))
 !
1 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_KB_pwscf',OPR='stop')
#endif
 !
end function io_KB_pwscf
