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
integer function io_EXCPH_SE(ID,what)
 !
 use pars,           ONLY:SP,schlen
 use IO_m,           ONLY:io_sec,read_is_on,frag_ELPH,io_status
 use IO_int,         ONLY:io_connect,io_disconnect,io_elemental,io_bulk,io_header,&
&                         def_variable_bulk,io_variable_bulk,io_fragment
 use EXCPH,          ONLY:EXCPH_Gkkp,EXCPH_Gkkp_sq,EXCPH_kind,EXCPH_sum,EXCPH_states,&
&                         BS_E_in,BS_Sat_E,BS_Sat_WEIGHT,BS_EXCPH_E,ph_EXCPH_E,&
&                         BS_Sat_E_PH_abs,BS_Sat_WEIGHT_PH_abs
 use ELPH,           ONLY:ph_modes,elph_branches
 use stderr,         ONLY:intc
 use R_lattice,      ONLY:nqibz
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer      ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                ::iq,ierr,VAR_size(4),VAR_size_sq(3),scat_type,ID_frag
 real(SP)               ::v(3)
 character(schlen)      ::db_name,VAR_name,dim_names(4),dim_names_sq(3)
 logical                ::l_exc_SE
 !
 call timing('io_EXCPH_gkkp',OPR='start')
 !
 ID_frag=0
 !
 ! DB name
 !
 db_name='excph_gkkp'
 !
 if (index(what,'excitonic_SE')>0) l_exc_SE = .true.
 !
 io_EXCPH_gkkp=io_connect(desc=trim(db_name),type=2,ID=ID)
 if (io_EXCPH_gkkp/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   scat_type = kind_to_integers(EXCPH_kind)
   !
   io_EXCPH_gkkp=io_header(ID,R_LATT=.true.,KPTS=.true.,FRAG=frag_ELPH)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=6,MENU=0)
   !
   call io_elemental(ID,&
&       VAR=" Phonon modes           :",I0=ph_modes,CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,&
&       VAR=" Exciton states           :",I1=EXCPH_states,CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,&
&       VAR=" Exciton sum (scattered):",I1=EXCPH_sum,CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,&
&       VAR=" Scattering type [1=TT,2=LT,3=TL,4=LL]:",I0=scat_type,CHECK=.true.,OP=(/"=="/))
   !
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   io_EXCPH_gkkp=io_status(ID)
   if (io_EXCPH_gkkp/=0) goto 1
   !
   call io_bulk(ID,VAR="PH_BRANCHES",VAR_SZ=(/2/))
   call io_bulk(ID,I1=elph_branches)
   !
   call io_bulk(ID,VAR="EXCITON_STATES",VAR_SZ=(/2/))
   call io_bulk(ID,I1=EXCPH_states)
   !
   call io_bulk(ID,VAR="EXCITON_SUM",VAR_SZ=(/2/))
   call io_bulk(ID,I1=EXCPH_sum)
   !
   call io_bulk(ID,VAR="SCATTERING_TYPE",VAR_SZ=(/1/))
   call io_bulk(ID,I0=scat_type)
   !
   if (l_exc_SE) then
     !
     call io_bulk(ID,VAR="BS_initial_states_E",VAR_SZ=(/2,EXCPH_states(2)/))
     call io_bulk(ID,C1=BS_E_in)
     !
   endif
   !
 endif
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
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
 ! Excitonic Gkkp (and SE) fragments
 !=========================
 !
 VAR_name="EXCITON_PH_GKKP_Q"//trim(intc(iq))
 VAR_size=(/2,ph_modes,EXCPH_sum(2),EXCPH_states(2)/)
 dim_names = [character(schlen) :: 'complex','PH_modes','N_exc_sum','N_exc_states']
 call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size,SP,dim_names)
 call io_variable_bulk(ID_frag,1,C3=EXCPH_Gkkp)
 !
 VAR_name="EXCITON_PH_GKKP_SQUARED_Q"//trim(intc(iq))
 VAR_size_sq=(/ph_modes,EXCPH_sum(2),EXCPH_states(2)/)
 dim_names_sq = [character(schlen) :: 'PH_modes','N_exc_sum','N_exc_states']
 call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size_sq,SP,dim_names_sq)
 call io_variable_bulk(ID_frag,1,R3=EXCPH_Gkkp_sq)
 !
 if (l_exc_SE) then
   !
   VAR_name="EXCITON_SATELLITE_ENERGY_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size,SP,dim_names)
   call io_variable_bulk(ID_frag,1,C3=BS_Sat_E)
   !
   VAR_name="EXCITON_SATELLITE_ENERGY_phonon_absorption_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size,SP,dim_names)
   call io_variable_bulk(ID_frag,1,C3=BS_Sat_E_PH_abs)
   !
   VAR_name="EXCITON_SATELLITE_WEIGHT_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size_sq,SP,dim_names_sq)
   call io_variable_bulk(ID_frag,1,R3=BS_Sat_WEIGHT)
   !
   VAR_name="EXCITON_SATELLITE_WEIGHT_phonon_absorption_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,VAR_size_sq,SP,dim_names_sq)
   call io_variable_bulk(ID_frag,1,R3=BS_Sat_WEIGHT_PH_abs)
   !
   VAR_name="PHONON_ENERGY_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,(/VAR_size(2)/),SP,(/dim_names(2)/))
   call io_variable_bulk(ID_frag,1,R1=ph_EXCPH_E)
   !
   VAR_name="EXCITON_BS_ENERGY_Q"//trim(intc(iq))
   call def_variable_bulk(ID_frag,trim(VAR_name),1,(/VAR_size(1),VAR_size(3)/),SP,(/dim_names(1),dim_names(3)/))
   call io_variable_bulk(ID_frag,1,C1=BS_EXCPH_E)
   !
 endif
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID=ID)
 !
 call timing('io_EXCPH_gkkp',OPR='stop')
 !
 contains
     integer function kind_to_integers(SCATTERING_TYPE)
         !
         integer :: l2int
         character(schlen), intent(in) :: SCATTERING_TYPE
         !
         if (trim(SCATTERING_TYPE)=='TT') l2int=1
         !
         if (trim(SCATTERING_TYPE)=='LT') l2int=2
         !
         if (trim(SCATTERING_TYPE)=='TL') l2int=3
         !
         if (trim(SCATTERING_TYPE)=='LL') l2int=4
         !
         if (trim(SCATTERING_TYPE)=='IL') l2int=5
         !
         if (trim(SCATTERING_TYPE)=='IT') l2int=6
         !
         kind_to_integers=l2int
         !
     end function
     !
end function
