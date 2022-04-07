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
integer function io_EXCPH_gkkp(ID,what)
 !
 use pars,           ONLY:SP,schlen
 use IO_m,           ONLY:io_sec,read_is_on,RD_CL,RD_CL_IF_END,io_action,frag_ELPH,io_status
 use IO_int,         ONLY:io_connect,io_disconnect,io_elemental,io_bulk,io_header,&
&                         def_variable_bulk,io_variable_bulk,io_fragment
 use EXCPH,          ONLY:EXCPH_Gkkp,EXCPH_Gkkp_sq,EXCPH_kind,EXCPH_sum,EXCPH_states,&
&                         BS_E_in,BS_Sat_E,BS_Sat_WEIGHT,BS_EXCPH_E,ph_EXCPH_E,&
&                         BS_Sat_E_PH_abs,BS_Sat_WEIGHT_PH_abs,L_kind_in,L_kind_out,Sat_alloc, &
&                         EXCPH_gkkp_alloc
 use IO_int,         ONLY:io_variable_elemental,def_variable_elemental
 use ELPH,           ONLY:ph_modes
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:nqbz
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 integer      ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                :: iq,ierr,VAR_size(4),VAR_size_sq(3),ID_frag
 character(schlen)      :: db_name,VAR_name,dim_names(4),dim_names_sq(3)
 logical                :: l_excph_gkkp,l_excph_satellites
 !
 call timing('io_EXCPH_gkkp',OPR='start')
 !
 l_excph_gkkp      =.FALSE.
 l_excph_satellites=.FALSE.
 !
 if (index(what,'gkkp')>0)       l_excph_gkkp      =.TRUE.
 if (index(what,'satellites')>0) l_excph_satellites=.TRUE.
 if (index(what,'all')>0) then
   l_excph_gkkp=.TRUE.
   l_excph_satellites=.TRUE.
 endif
 !
 ID_frag=0
 io_EXCPH_gkkp=0
 !
 ! DB name
 !
 db_name='excph_gkkp'
 !
 io_EXCPH_gkkp=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_EXCPH_gkkp/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_EXCPH_gkkp=io_header(ID,R_LATT=.true.,KPTS=.true.,FRAG=frag_ELPH)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=4,MENU=0)
   !
   call io_elemental(ID,&
&       VAR=" Exciton states           :",I1=EXCPH_states,CHECK=.true.,OP=(/"==","=="/))
   !
   call io_elemental(ID,&
&       VAR=" Exciton sum (scattered):",I1=EXCPH_sum,CHECK=.true.,OP=(/"==","=="/))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   io_EXCPH_gkkp=io_status(ID)
   if (io_EXCPH_gkkp/=0) goto 1
   !
   call io_bulk(ID,VAR="EXCITON_STATES",VAR_SZ=(/2/))
   call io_bulk(ID,I1=EXCPH_states)
   !
   call io_bulk(ID,VAR="EXCITON_SUM",VAR_SZ=(/2/))
   call io_bulk(ID,I1=EXCPH_sum)
   !
   call def_variable_elemental(ID,"L_kind_in",1,0,0)
   call io_variable_elemental(ID,VAR="[K] L_kind in",CH0=L_kind_in,CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(ID,"L_kind_out",1,0,0)
   call io_variable_elemental(ID,VAR="[K] L_kind out",CH0=L_kind_in,CHECK=.true.,OP=(/"=="/))
   !
 endif
 !
 io_EXCPH_gkkp=io_status(ID)
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
 !
 if(.not.l_excph_gkkp.and..not.l_excph_satellites) goto 1
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=iq,ierr=ierr)
 if (ierr<0.and.read_is_on(ID)) goto 1
 !
 ! When the DB is fragmented I allow a partial reading checking
 ! if the fragment exists or not.
 ! If the fragment does not exist (ierr<0) I return an error code
 !
 if (ierr<0.and.read_is_on(ID)) then
   io_EXCPH_gkkp=-1
   goto 1
 endif
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.iq==nqbz) io_action(ID)=RD_CL
 !
 ! Excitonic Gkkp (and SE) fragments
 !=========================
 !
 if (read_is_on(ID)) then
   if(.not.allocated(EXCPH_gkkp).and.l_excph_gkkp      ) call EXCPH_gkkp_alloc()
   if(.not.allocated(BS_Sat_E)  .and.l_excph_satellites) call Sat_alloc()
 endif
 !
 if(l_excph_gkkp) then
   !
   VAR_name="EXCITON_PH_GKKP_Q"//trim(intc(iq))
   VAR_size=(/2,ph_modes,EXCPH_sum(2),EXCPH_states(2)/)
   dim_names = [character(schlen) :: 'complex','PH_modes','N_exc_sum','N_exc_states']
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size,SP,dim_names)
   call io_variable_bulk(ID_frag,1,C3=EXCPH_gkkp)
   !
   VAR_name="EXCITON_PH_GKKP_SQUARED_Q"//trim(intc(iq))
   VAR_size_sq=(/ph_modes,EXCPH_sum(2),EXCPH_states(2)/)
   dim_names_sq = [character(schlen) :: 'PH_modes','N_exc_sum','N_exc_states']
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size_sq,SP,dim_names_sq)
   call io_variable_bulk(ID_frag,1,R3=EXCPH_gkkp_sq)
   !
 endif
 !
 if(l_excph_satellites) then
   !      
   VAR_name="EXCITON_SATELLITE_ENERGY_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size,SP,dim_names)
   call io_variable_bulk(ID_frag,1,C3=BS_Sat_E)
   !
   VAR_name="EXCITON_SATELLITE_ENERGY_ph_abs_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size,SP,dim_names)
   call io_variable_bulk(ID_frag,1,C3=BS_Sat_E_PH_abs)
   !
   VAR_name="EXCITON_SATELLITE_WEIGHT_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size_sq,SP,dim_names_sq)
   call io_variable_bulk(ID_frag,1,R3=BS_Sat_WEIGHT)
   !
   VAR_name="EXCITON_SATELLITE_WEIGHT_ph_abs_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size_sq,SP,dim_names_sq)
   call io_variable_bulk(ID_frag,1,R3=BS_Sat_WEIGHT_PH_abs)
   !
 endif
 !
 io_EXCPH_gkkp=io_status(ID)
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID=ID)
 !
 call timing('io_EXCPH_gkkp',OPR='stop')
 !
end function
