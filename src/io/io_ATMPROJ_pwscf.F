!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function io_ATMPROJ_pwscf(ID)
 !
 use atom_proj
 use pars,             ONLY:IP,SP,schlen,cZERO
 use electrons,        ONLY:n_sp_pol
 use stderr,           ONLY:intc
 use R_lattice,        ONLY:nkibz
 use IO_m,             ONLY:io_connect,io_disconnect,io_sec, &
&                           io_elemental,io_variable_bulk,def_variable_bulk, &
&                           io_header,read_is_on,io_fragment,frag_WF
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 integer           :: ifrag,ik,i_sp_pol
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
   io_ATMPROJ_pwscf=io_header(ID,IMPOSE_SN=.true.,FRAG=frag_WF)
   if (io_ATMPROJ_pwscf/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=2,MENU=0)
   call io_elemental(ID,I0=n_atom_wfc)
   call io_elemental(ID,I0=n_proj_bands)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   varname="ATOM_PROJ_TABLE"
   call def_variable_bulk(ID,trim(varname),1,shape(atomic_table),IP+100)
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
 call io_fragment(ID,i_fragment=ik)
 !
 varname="ATOM_PROJ_IK"//trim(intc(ik))//"_SP_POL"//trim(intc(i_sp_pol))
 call def_variable_bulk(ID,trim(varname),1,(/2,n_atom_wfc,n_proj_bands/),SP)
 call  io_variable_bulk(ID,1,C2=atomic_projections(:,:,ik,i_sp_pol))
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_ATMPROJ_pwscf',OPR='stop')
 !
end function io_ATMPROJ_pwscf
