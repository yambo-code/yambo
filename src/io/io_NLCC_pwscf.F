!
! License-Identifier: GPL
!
! Copyright (C) 2017 The Yambo Team
!
! Authors (see AUTHORS file for details): AF
!
integer function io_NLCC_pwscf(ID)
 !
 use pars,             ONLY:SP
 use D_lattice,        ONLY:n_atomic_species
 use R_lattice,        ONLY:ng_vec
 use IO_m,             ONLY:io_sec,read_is_on
 use IO_int,           ONLY:io_connect,io_disconnect, &
&                           io_elemental,io_bulk,io_header
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
