!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
integer function io_COL_CUT(ID)
 !
 use wave_func,      ONLY:wf_ng
 use R_lattice,      ONLY:RIM_ng,RIM_n_rand_pts,CUTOFF_plus_RIM,&
&                         cyl_ph_radius,box_length,cyl_length,cut_geometry,&
&                         ng_closed,nqibz,bare_qpg,cut_description
 use IO_m,           ONLY:io_sec,io_status
 use IO_int,         ONLY:io_connect,io_disconnect,io_header,io_elemental,io_bulk
 use timing_m,       ONLY:timing
#include<memory.h>
 !
 integer :: ID
 ! 
 ! Work Space
 !
 integer             ::CUT_RIM_ng,CUT_RIM_npts,wf_ng_db
 !
 call timing('io_COL_CUT',OPR='start')
 io_COL_CUT=io_connect(desc="cutoff",type=2,ID=ID)
 !
 if (io_COL_CUT/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   io_COL_CUT=io_header(ID,QPTS=.true.,R_LATT=.true.,IMPOSE_SN=.true.)
   if (io_COL_CUT/=0) goto 1   
   !
   call io_elemental(ID,VAR='GEOMETRY',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=cut_geometry,&
&       VAR=' CutOff Geometry                 ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='DESCRIPTION',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=cut_description,&
&       VAR=' Coulomb cutoff potential        ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=10,MENU=0)
   call io_elemental(ID,R1=box_length,&
&       VAR=' Box sides length            [au]',CHECK=.true.,OP=(/"==","==","=="/))
   call io_elemental(ID,R0=cyl_ph_radius,&
&       VAR=' Sphere/Cylinder radius      [au]',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,R0=cyl_length,&
&       VAR=' Cylinder length             [au]',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=wf_ng,DB_I0=wf_ng_db,&
&       VAR=' RL components                   ',CHECK=.true.,OP=(/"<="/))
   call io_elemental(ID,I0=ng_closed,&
&       VAR=' RL components used in the sum   ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,L0=CUTOFF_plus_RIM,&
&       VAR=' RIM corrections included        ',CHECK=.true.,OP=(/"=="/))
   CUT_RIM_ng=0
   CUT_RIM_npts=0
   if (CUTOFF_plus_RIM) then
     CUT_RIM_ng=RIM_ng
     CUT_RIM_npts=RIM_n_rand_pts
   endif
   call io_elemental(ID,I0=CUT_RIM_ng,&
&       VAR=' RIM RL components               ',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=CUT_RIM_npts,&
&       VAR=' RIM random points               ',CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,VAR="",VAR_SZ=0)
   io_COL_CUT=io_status(ID)
   if (io_COL_CUT/=0) goto 1
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   call io_bulk(ID,VAR='CUT_BARE_QPG',VAR_SZ=(/2,nqibz,wf_ng/))
   call io_bulk(ID,C2=bare_qpg(:,:wf_ng))
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_COL_CUT',OPR='stop')
 !
end function
