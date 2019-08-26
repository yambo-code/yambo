!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF
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
integer function io_NLCC_pwscf(ID)
 !
 use pars,             ONLY:SP
 use D_lattice,        ONLY:n_atomic_species
 use R_lattice,        ONLY:ng_vec
 use IO_m,             ONLY:io_connect,io_disconnect,io_sec, &
&                           io_elemental,io_bulk,io_header,&
&                           read_is_on
 use pseudo,           ONLY:pp_rhog_nlcc,pp_has_nlcc_atom,PP_nlcc_alloc
 !
 implicit none
 !
 integer,intent(in) :: ID
 ! 
 io_nlcc_pwscf=io_connect(desc="nlcc_pp_pwscf",type=0,ID=ID)
 if (io_nlcc_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_NLCC_pwscf=io_header(ID,IMPOSE_SN=.true.)
   if (io_NLCC_pwscf/=0) goto 1
   !
   ! n_atoms_max and n_atom_species are redundant as they are
   ! already read from the DB1.
   !
   call io_elemental(ID,VAR="NUM_ATOMIC_SPECIES",VAR_SZ=1,MENU=0)
   call io_elemental(ID,I0=n_atomic_species,VAR=" PP Number of Atomic Species")
   call io_elemental(ID,VAR="NUM_RL_RHO_NLCC",VAR_SZ=1,MENU=0)
   call io_elemental(ID,I0=ng_vec,VAR=" NLCC RL compoments")
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
 endif
 !
 if (read_is_on(ID)) call PP_nlcc_alloc()
 !
 call io_bulk(ID,VAR="PP_HAS_NLCC_ATOM",VAR_SZ=shape(pp_has_nlcc_atom))
 call io_bulk(ID,L1=pp_has_nlcc_atom)
 call io_bulk(ID,VAR="PP_RHOG_NLCC",VAR_SZ=(/2,shape(pp_rhog_nlcc)/))
 call io_bulk(ID,C1=pp_rhog_nlcc)
 !
1 call io_disconnect(ID=ID)
 !
end function io_NLCC_pwscf
