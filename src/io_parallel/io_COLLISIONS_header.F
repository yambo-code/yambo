!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
integer function io_COLLISIONS_header(ID,COLL)
 !
 use pars,          ONLY:schlen,IP,IP_YIO
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use collision_ext, ONLY:COLLISIONS_cutoff,COLL_bands,COLLISIONS_group,COLLISIONS_CV_only
 use IO_m,          ONLY:io_status,IO_OUTDATED_DB,read_is_on
 use IO_int,        ONLY:io_connect,io_header,io_elemental,ver_is_gt_or_eq, &
&                        io_variable_bulk,def_variable_bulk,io_disconnect
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED,&
&                        H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_HARTREE
 use plasma,        ONLY:N_plasma_poles_global,PLASMA_redux_percent
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH,SC_band_mixing,QP_QED_ng
 use X_m,           ONLY:X_desc
 use drivers,       ONLY:l_elel_scatt,l_elphoton_scatt
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer, intent(inout)            ::ID
 type(COLLISIONS_group)            ::COLL
 !
 ! Work Space
 !
 character(schlen)     :: db_name
 integer               :: V_kind_disk,SIZE_
 !
 integer, external     :: io_descriptors
 !
 call timing('io_COLLISIONS',OPR='start')
 !
 call COLLISIONS_dbname(COLL%kind,db_name)
 !
 io_COLLISIONS_header=io_connect(desc=trim(db_name)//"_header",type=2,ID=ID)
 if(io_COLLISIONS_header/=0) goto 1
 !
 io_COLLISIONS_header=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.)
 !
 if (.not.ver_is_gt_or_eq(ID,revision=5453)) then
   io_COLLISIONS_header=IO_OUTDATED_DB
   goto 1
 endif
 !
 call io_elemental(ID,VAR="COLLISION_DESCRIPTION",VAR_SZ=1,CH0="",MENU=0)
 call io_elemental(ID,CH0=COLL%name,VAR=' COLLISIONS name                 ')
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 SIZE_=6
 if (COLL%kind==QP_SE_GW_NEQ) SIZE_=SIZE_+3
 if (COLL%kind==QP_SE_GW_QED) SIZE_=SIZE_+2
 if (ver_is_gt_or_eq(ID,revision=21128)) then
   SIZE_ = SIZE_+1
 endif
 call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=SIZE_,MENU=0)
 !
 call io_elemental(ID,I0=COLL%N,&
&       VAR='Total number of COLLISIONS      ',CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,I0=COLL%kind,&
&       VAR='COLLISIONS kind                 ',CHECK=.true.,OP=(/"=="/))
 !
 call io_elemental(ID,I1=COLL_bands,&
&       VAR='Bands                           ',CHECK=.true.,OP=(/"==","=="/))
 !
 if (COLL%kind==QP_SE_GW_NEQ) then
   call io_elemental(ID,L0=l_elel_scatt,&
&         VAR='Electron-Electron scattering    ',WARN=.true.,OP=(/"=="/))
   if(l_elel_scatt) then
    call io_elemental(ID,I0=N_plasma_poles_global,&
&          VAR='Max N plasma poles (q-resolved) ',CHECK=.true.,OP=(/"=="/))
    call io_elemental(ID,R0=PLASMA_redux_percent,&
&          VAR='Plasma redux percent            ',CHECK=.true.,OP=(/"=="/))
   endif
 endif
 !
 if (COLL%kind==QP_SE_GW_QED) then
   call io_elemental(ID,L0=l_elphoton_scatt,&
&         VAR='Electron-Photon   scattering    ',WARN=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=QP_QED_ng,&
&         VAR='Electron-Photon   RL vectors    ',CHECK=.true.,OP=(/"=="/))
 endif
 !
 call io_elemental(ID,R0=SC_band_mixing,&
&       VAR='Bands mixing                    ',CHECK=.true.,OP=(/"=="/))
 call io_elemental(ID,I0=COLL%kind,DB_I0=V_kind_disk,CHECK=.true.,OP=(/"=="/))
 if (ver_is_gt_or_eq(ID,revision=21128)) then
   call io_elemental(ID,L0=COLLISIONS_CV_only,VAR='CV only scattering    ',WARN=.true.,OP=(/"=="/))
 endif
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if(COLL%kind==H_SE_HARTREE.or.COLL%kind==H_SE_FOCK.or.COLL%kind==H_SE_SEX.or.COLL%kind>=H_SE_HARTREE) then
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   SIZE_=1
   if (V_kind_disk>=H_SE_HARTREE) SIZE_=SIZE_+1
   if (V_kind_disk==             H_SE_FOCK.or.V_kind_disk==             H_SE_SEX.or.&
&        V_kind_disk==H_SE_HARTREE+H_SE_FOCK.or.V_kind_disk==H_SE_HARTREE+H_SE_SEX     ) SIZE_=SIZE_+1
   call io_elemental(ID,VAR="HXC_PARAMETERS",VAR_SZ=SIZE_,MENU=0)
   call io_elemental(ID,R0=COLLISIONS_cutoff,&
&         VAR='Collisions Cutoff                 ',CHECK=.true.,OP=(/"=="/))
   !
   if (V_kind_disk>=H_SE_HARTREE) then
     call io_elemental(ID,I0=QP_ng_SH,&
&         VAR='Hartree  RL vectors             ',CHECK=.true.,OP=(/"=="/))
   endif
   if (V_kind_disk==             H_SE_FOCK.or.V_kind_disk==             H_SE_SEX.or.&
&        V_kind_disk==H_SE_HARTREE+H_SE_FOCK.or.V_kind_disk==H_SE_HARTREE+H_SE_SEX) then
     call io_elemental(ID,I0=QP_ng_Sx,&
&         VAR='Exchange RL vectors             ',CHECK=.true.,OP=(/"=="/))
   endif
 endif
 !
 if (V_kind_disk==QP_SE_GW_NEQ   .or.V_kind_disk==H_SE_COH  .or.V_kind_disk==H_SE_SEX) then
   !
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! Global Response Descriptions
   !==============================
   !
   io_COLLISIONS_header=io_descriptors(X_desc,"X",ID,.FALSE.)
   !
   call io_elemental(ID,VAR="RIM_PARAMETERS",VAR_SZ=2,MENU=0)
   call io_elemental(ID,I0=RIM_ng,VAR=        ' RIM RL components      ',WARN=.true.)
   call io_elemental(ID,I0=RIM_n_rand_pts,VAR=' RIM random points      ',WARN=.true.)
   !
 endif
 !
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 ! States
 !
 if (read_is_on(ID).and..not.allocated(COLL%state)) then
   YAMBO_ALLOC(COLL%state,(COLL%N,4))
 endif
 call def_variable_bulk(ID,'COLLISIONS_STATE',1,(/COLL%N,4/),IP_YIO)
 call  io_variable_bulk(ID,1,I2=COLL%state)
 !
 io_COLLISIONS_header=io_status(ID)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_COLLISIONS',OPR='stop')
 !
end function io_COLLISIONS_header
