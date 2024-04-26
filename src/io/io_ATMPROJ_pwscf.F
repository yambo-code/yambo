!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
integer function io_ATMPROJ_pwscf(ID)
 !
 use atom_proj
 use pars,             ONLY:IP,SP,schlen,cZERO,IP_YIO
 use electrons,        ONLY:n_sp_pol
 use stderr,           ONLY:intc
 use R_lattice,        ONLY:nkibz
 use IO_m,             ONLY:io_sec,read_is_on,frag_WF
 use IO_int,           ONLY:io_connect,io_disconnect, &
&                           io_elemental,io_variable_bulk,def_variable_bulk, &
&                           io_header,io_fragment
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 integer           :: ifrag,ik,i_sp_pol,ID_frag
 character(schlen) :: varname
 !
 call timing('io_ATMPROJ_pwscf',OPR='start')
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
 io_ATMPROJ_pwscf=io_connect(desc="atom_proj_pwscf",type=0,ID=ID)
 if (io_ATMPROJ_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_ATMPROJ_pwscf=io_header(ID,IMPOSE_SN=.false.,FRAG=frag_WF)
   if (io_ATMPROJ_pwscf/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=2,MENU=0)
   call io_elemental(ID,I0=n_atom_wfc)
   call io_elemental(ID,I0=n_proj_bands)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   if ( (.not.allocated(atomic_table)) .and. read_is_on(ID) ) then
     YAMBO_ALLOC(atomic_table,(6,n_atom_wfc))
     atomic_table=0
   endif
   !
   varname="ATOM_PROJ_TABLE"
   call def_variable_bulk(ID,trim(varname),1,shape(atomic_table),IP_YIO)
   call  io_variable_bulk(ID,1,I2=atomic_table)
   !
 endif
 !
 if (ik==0) goto 1
 !
 if (ik==1.and.i_sp_pol==1.and.read_is_on(ID)) then
   if (.not.allocated(atomic_projections)) then
     YAMBO_ALLOC(atomic_projections,(n_atom_wfc,n_proj_bands,nkibz,n_sp_pol))
     atomic_projections=cZERO
   endif
 endif
 !
 call io_fragment(ID,ID_frag,i_fragment=ik)
 !
 varname="ATOM_PROJ_IK"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol))
 call def_variable_bulk(ID_frag,trim(varname),1,(/2,n_atom_wfc,n_proj_bands/),SP)
 call  io_variable_bulk(ID_frag,1,C2=atomic_projections(:,:,ik,i_sp_pol))
 !
 call io_fragment_disconnect(ID,ID_frag)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_ATMPROJ_pwscf',OPR='stop')
 !
end function io_ATMPROJ_pwscf
