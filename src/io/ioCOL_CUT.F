!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
integer function ioCOL_CUT(ID)
 !
 use pars,           ONLY:SP
 use wave_func,      ONLY:wf_ng
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 use R_lattice,      ONLY:RIM_ng,RIM_n_rand_pts,CUTOFF_plus_RIM,&
&                         cyl_ph_radius,box_length,cyl_length,cut_geometry,&
&                         ng_closed,nqibz,bare_qpg,cut_description
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,io_header,&
&                         io_elemental,io_status,io_bulk,read_is_on,write_is_on
 implicit none
 integer :: ID
 ! 
 ! Work Space
 !
 real(SP),allocatable::bare_qpg_disk(:,:,:)
 integer             ::CUT_RIM_ng,CUT_RIM_npts,wf_ncx
 !
 ioCOL_CUT=io_connect(desc="cutoff",type=1,ID=ID)
 !
 if (ioCOL_CUT/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ioCOL_CUT=io_header(ID,QPTS=.true.,R_LATT=.true.,IMPOSE_SN=.true.)
   if (ioCOL_CUT/=0) goto 1   
   !
   call io_elemental(ID,VAR='GEOMETRY',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=cut_geometry,&
&       VAR=' CutOff Geometry                 :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR='DESCRIPTION',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=cut_description,&
&       VAR=' Coulomb cutoff potential        :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=10,MENU=0)
   call io_elemental(ID,R1=box_length,&
&       VAR=' Box sides length            [au]:',CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,R0=cyl_ph_radius,&
&       VAR=' Sphere/Cylinder radius      [au]:',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=cyl_length,&
&       VAR=' Cylinder length             [au]:',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=wf_ng,DB_I0=wf_ncx,&
&       VAR=' RL components                   :',CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,I0=ng_closed,&
&       VAR=' RL components used in the sum   :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,L0=CUTOFF_plus_RIM,&
&       VAR=' RIM corrections included        :',CHECK=.true.,OP=(/"=="/))
   CUT_RIM_ng=0
   CUT_RIM_npts=0
   if (CUTOFF_plus_RIM) then
     CUT_RIM_ng=RIM_ng
     CUT_RIM_npts=RIM_n_rand_pts
   endif
   call io_elemental(ID,I0=CUT_RIM_ng,&
&       VAR=' RIM RL components               :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=CUT_RIM_npts,&
&       VAR=' RIM random points               :',CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   ioCOL_CUT=io_status(ID)
   if (ioCOL_CUT/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   ! 
   allocate(bare_qpg_disk(nqibz,wf_ncx,2))
   ! 
   if (write_is_on(ID)) call mat_c2r(bare_qpg,bare_qpg_disk)
   !
   call io_bulk(ID,VAR='CUT_BARE_QPG',VAR_SZ=shape(bare_qpg_disk))
   call io_bulk(ID,R3=bare_qpg_disk)
   !   
   if (read_is_on(ID)) call mat_r2c(bare_qpg_disk,bare_qpg)
   !
   deallocate(bare_qpg_disk)
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function
