!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
integer function variables_DIPOLES(X,Xen,ID,db_nbm,db_nb)
 !
 use units,         ONLY:HA2EV
 use X_m,           ONLY:X_t,Dipole_bands_ordered,&
&                        Dipole_uses_shifted_grids
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:q0_def_norm
 use IO_m,          ONLY:io_elemental,io_status
 use global_XC,     ONLY:Dipole_WF_xc_string,loaded_WF_xc_string
 !
 implicit none
 type(X_t)   ::X
 type(levels)::Xen
 integer     ::ID
 !
 integer :: db_nbm,db_nb(2)
 !
 ! Work Space
 !
 integer :: sec_size
 integer :: db_nbf
 !
 sec_size=16
 !
 call io_elemental(ID,VAR="PARS",VAR_SZ=sec_size,MENU=0)
 call io_elemental(ID,DB_I1=db_nb,&
&     VAR=" X band range           :",I1=X%ib,CHECK=.true.,OP=(/">=","<="/))
 call io_elemental(ID,UNIT=HA2EV,&
&     VAR=" X e/h energy range [ev]:",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/))
 call io_elemental(ID,DB_I0=db_nbm,&
&     VAR=" Metallic bands         :",I0=Xen%nbm,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,DB_I0=db_nbf,&
&     VAR=" Filled bands           :",I0=Xen%nbf,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" RL vectors in the sum  :",I0=X%ngostnts,WARN=.true.,OP=(/"<="/))
 call io_elemental(ID,&
&     VAR=" [r,Vnl] included       :",L0=X%Vnl_included,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" Transitions ordered    :",L0=Dipole_bands_ordered,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" Using shifted grids    :",L0=Dipole_uses_shifted_grids,CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,&
&     VAR=" Field momentum norm    :",R0=q0_def_norm,CHECK=.true.,OP=(/"=="/))
 !
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
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
