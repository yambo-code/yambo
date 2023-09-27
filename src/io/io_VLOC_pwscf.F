!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
integer function io_VLOC_pwscf(ID)
 !
 use pars,             ONLY: DP,IP
 use stderr,           ONLY: intc
 use IO_m,             ONLY: io_sec,read_is_on,write_is_on
 use IO_int,           ONLY: io_connect,io_disconnect, &
&                            io_variable_elemental,io_variable_bulk,io_header, &
&                            def_variable_elemental,def_variable_bulk
 use qe_pseudo_m,      ONLY: nsp
 use gvect,            ONLY: ngm,ngl
 use vlocal,           ONLY: vloc_yambo
 !
 implicit none
 !
 integer,intent(in) :: ID
 !
 ! Work Space
 !
 io_VLOC_pwscf=io_connect(desc="vloc_pp_pwscf",type=0,ID=ID)
 if (io_VLOC_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ! g-vector variables
   !
   call def_variable_elemental(ID,VAR="ngm",VAR_SZ=1,VAR_KIND=IP+100,MENU=0)
   call  io_variable_elemental(ID,I0=ngm,VAR="Number of G-vectors",CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(ID,VAR="ngl",VAR_SZ=1,VAR_KIND=IP+100,MENU=0)
   call  io_variable_elemental(ID,I0=ngl,VAR="Number of G-shells",CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(ID,VAR="nsp",VAR_SZ=1,VAR_KIND=IP+100,MENU=0)
   call  io_variable_elemental(ID,I0=nsp,VAR="Number of at. species",CHECK=.true.,OP=(/"=="/))
   !
   call def_variable_elemental(ID,VAR="",VAR_SZ=0,VAR_KIND=0,MENU=1)
   !
 endif
 !
 ! vloc
 !
 call def_variable_bulk(ID,"vloc",1,shape(vloc_yambo),DP)
 call  io_variable_bulk(ID,1,D2=vloc_yambo)
 !
1 call io_disconnect(ID=ID)
 !
end function io_VLOC_pwscf
