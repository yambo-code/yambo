!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
integer function variables_DIPOLES(Dip,ID)
 !
 use pars,          ONLY:lchlen
 use units,         ONLY:HA2EV
 use DIPOLES,       ONLY:DIPOLE_t
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:q0_def_norm
 use IO_m,          ONLY:io_elemental,io_status
 use global_XC,     ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 !
 implicit none
 !
 type(DIPOLE_t) :: Dip
 integer        :: ID
 !
 ! Workspace
 !
 character(lchlen)  ::local_description
 !
 call io_elemental(ID,VAR="PARS",VAR_SZ=11,MENU=0)
 !
 call io_elemental(ID,DB_I1=Dip%db_ib,&
&     VAR=" X band range           :",I1=Dip%ib    ,CHECK=.true.,OP=(/">=","<="/))
 call io_elemental(ID,DB_I1=Dip%db_ib_lim,&
&     VAR=" X band range limits    :",I1=Dip%ib_lim,CHECK=.true.,OP=(/"<=",">="/))
 call io_elemental(ID,UNIT=HA2EV,&
&     VAR=" X e/h energy range [ev]:",R1=Dip%ehe,CHECK=.true.,OP=(/">=","<="/))
 call io_elemental(ID,&
&     VAR=" RL vectors in the sum  :",I0=Dip%ng,WARN=.true.,OP=(/"<="/))
 call io_elemental(ID,&
&     VAR=" [r,Vnl] included       :",L0=Dip%Vnl_included,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" Bands ordered          :",L0=Dip%bands_ordered,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" Direct v evaluation    :",L0=Dip%force_v_g_space,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" Field momentum norm    :",R0=q0_def_norm,CHECK=.true.,OP=(/"=="/))
 !
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 ! Character variables
 !
 call io_elemental(ID,VAR="APPROACH",VAR_SZ=1,CH0="",MENU=0)
 call io_elemental(ID,&
&     VAR=" Approach used          :",CH0=Dip%approach,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,VAR="",VAR_SZ=0)
 !
 ! Wavefunctions xc 
 !
 call io_elemental(ID,VAR='WAVE_FUNC_XC',CH0="",VAR_SZ=1,MENU=0)
 call io_elemental(ID,DB_CH0=Dipole_WF_xc_string,CH0=loaded_WF_xc_string,&
&     VAR=' Wavefunctions          :',CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,VAR="",VAR_SZ=0)
 !
 variables_DIPOLES=io_status(ID)
 !
end function variables_DIPOLES
