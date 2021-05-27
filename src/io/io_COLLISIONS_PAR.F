!
!        Copyright (C) 2000-2021 the YAMBO team
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
 use pars,          ONLY:schlen,IP,SP
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use collision_ext, ONLY:COLLISIONS_cutoff,COLLISIONS_element,COLL_bands,COLLISIONS_group
 use IO_m,          ONLY:io_sec,io_status,IO_OUTDATED_DB,read_is_on,IO_mode,&
&                        VERIFY,io_extension,io_raw_extension
 use IO_int,        ONLY:io_connect,io_header,io_elemental,ver_is_gt_or_eq,io_fragment, &
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
 integer               :: V_kind_disk,io_err,SIZE_
 !
 integer, external     :: io_descriptors
 !
 call timing('io_COLLISIONS',OPR='start')
 !
 call get_collisions_dbname(COLL%kind,db_name)
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
&         VAR='Exchange Cutoff                 ',CHECK=.true.,OP=(/"=="/))
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
 call def_variable_bulk(ID,'COLLISIONS_STATE',1,(/COLL%N,4/),IP+100)
 call  io_variable_bulk(ID,1,I2=COLL%state)
 !
 io_COLLISIONS_header=io_status(ID)
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_COLLISIONS',OPR='stop')
 !
end function io_COLLISIONS_header
!
!
integer function io_COLLISIONS(ID,COLL,COLL_element,i_coll_,COMM)
 !
 use pars,          ONLY:schlen,IP,SP
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use collision_ext, ONLY:COLLISIONS_cutoff,COLLISIONS_element,COLL_bands,COLLISIONS_group
 use IO_m,          ONLY:io_sec,io_status,IO_OUTDATED_DB,read_is_on,IO_mode,&
&                        VERIFY,io_extension,io_raw_extension,write_is_on
 use IO_int,        ONLY:io_connect,io_header,io_elemental,ver_is_gt_or_eq,io_fragment, &
&                        io_variable_bulk,def_variable_bulk,io_disconnect
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED
 use plasma,        ONLY:N_plasma_poles_global,PLASMA_redux_percent
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH,SC_band_mixing,QP_QED_ng
 use X_m,           ONLY:X_desc
 use drivers,       ONLY:l_elel_scatt,l_elphoton_scatt
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer, intent(inout)            ::ID
 type(COLLISIONS_group)            ::COLL
 type(COLLISIONS_element),optional, target :: COLL_element
 integer, intent(in)     ,optional         :: i_coll_,COMM
 !
 real(SP),   pointer :: tmp_link_real(:,:)
 complex(SP),pointer :: tmp_link_complex(:,:)
 !
 character(schlen) :: db_name
 integer           :: i_coll
 logical           :: def,io2,io3
 !
 call timing('io_COLLISIONS',OPR='start')
 !
 ! Collision element...
 !
 i_coll=0
 if (present(COLL_element)) i_coll=COLL_element%I
 if (present(i_coll_))      i_coll=i_coll_
 !
 call get_collisions_dbname(COLL%kind,db_name)
 !
 if(any(io_sec(ID,:)==1).and.write_is_on(ID)) then
   if(.not.present(COMM)) call error(" [IO COLLs] Parallel I/O requires COMM for initialization")
   io_COLLISIONS=io_connect(desc=trim(db_name),type=2,ID=ID,COMM=COMM)
 else
   io_COLLISIONS=io_connect(desc=trim(db_name),type=2,ID=ID)
 endif
 !
 if(io_COLLISIONS/=0) goto 1
 !
 def=any(io_sec(ID,:)==1)
 io3=any(io_sec(ID,:)==3)
 !
 if(.not.(def.or.io3)) goto 1
 !
 ! This should be used for restart like in BSE ...
 if (io3.and.read_is_on(ID)) COLL_element%table='t'
 !
 if(io3.or.def) then
   !
   if(def) COLL_element%N=(COLL%D(3,2)-COLL%D(3,1)+1)*(COLL%D(2,2)-COLL%D(2,1)+1)*(COLL%D(1,2)-COLL%D(1,1)+1)
   !
   if(COLL%kind==QP_SE_GW_NEQ.or.COLL%kind==QP_SE_GW_QED) then
     ! Real collisions
     if(def) call def_variable_bulk(ID,'COLLISIONS_v',1,(/COLL_element%N,COLL%N/),SP,par_io_kind='independent')
     if(io3) then
       call C_F_POINTER(C_LOC(COLL_element%v_r),tmp_link_real,(/COLL_element%N,1/))
       call  io_variable_bulk(ID,1,R2=tmp_link_real,IPOS=(/1,1,i_coll/))
       nullify(tmp_link_real)
     endif
   else
     ! Complex collisions
     if(def) call def_variable_bulk(ID,'COLLISIONS_v',1,(/2,COLL_element%N,COLL%N/),SP,par_io_kind='independent')
     if(io3) then
       call C_F_POINTER(C_LOC(COLL_element%v_c),tmp_link_complex,(/COLL_element%N,1/))
       call  io_variable_bulk(ID,1,C2=tmp_link_complex,IPOS=(/1,1,i_coll/))
       nullify(tmp_link_complex)
     endif
   endif
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
 call timing('io_COLLISIONS',OPR='stop')
 !
end function io_COLLISIONS
!
!
subroutine get_collisions_dbname(coll_kind,db_name)
 !
 use pars,          ONLY:schlen
 use xc_functionals,ONLY:GS_xc_kind
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED,&
&                        H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_HARTREE
 !
 implicit none
 !
 integer,           intent(in)  :: coll_kind
 character(schlen), intent(out) :: db_name
 !
 if(coll_kind==H_SE_COH) then
   db_name = "COLLISIONS_COH"
 elseif(coll_kind==H_SE_HARTREE.or.coll_kind==H_SE_FOCK.or.coll_kind==H_SE_SEX.or.coll_kind==GS_xc_kind.or.&
&       coll_kind>=H_SE_HARTREE) then
   db_name = "COLLISIONS_HXC"
 elseif(coll_kind==QP_SE_GW_NEQ) then
   db_name = "COLLISIONS_GW_NEQ"
 elseif(coll_kind==QP_SE_GW_QED) then
   db_name = "COLLISIONS_P"
 else
   call error("COLLISIONS I/O : Unknown database type ")
 endif
 !
end subroutine get_collisions_dbname
