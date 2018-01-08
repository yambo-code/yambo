!
!        Copyright (C) 2000-2017 the YAMBO team
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
integer function io_COLLISIONS(ID,COLL,COLL_element,i_coll_)
 !
 use pars,          ONLY:schlen,lchlen
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use collision_ext, ONLY:COLLISIONS_cutoff,COLLISIONS_element,COLL_bands,COLLISIONS_group
 use IO_m,          ONLY:io_sec,io_connect,io_header,io_status,io_elemental, &
&                        io_bulk,io_disconnect,&
&                        ver_is_gt_or_eq,IO_OUTDATED_DB,read_is_on,io_fragment,IO_mode,&
&                        VERIFY,io_extension,io_raw_extension
 use xc_functionals,ONLY:GS_xc_kind
 use global_XC,     ONLY:QP_SE_GW_NEQ,QP_SE_GW_QED,&
&                        H_SE_FOCK,H_SE_COH,H_SE_SEX,H_SE_HARTREE
 use com,           ONLY:error
 use plasma,        ONLY:N_plasma_poles_global,PLASMA_redux_percent
 use QP_m,          ONLY:QP_ng_Sx,QP_ng_SH,SC_band_mixing,QP_QED_ng
 use X_m,           ONLY:n_X_descs,X_description
 use stderr,        ONLY:intc
 use drivers,       ONLY:l_elel_scatt,l_elphoton_scatt
#if defined _TIMING
 use timing_m,      ONLY:timing
#endif
 !
#include<memory.h>
 !
 integer, intent(inout)            ::ID
 type(COLLISIONS_group)            ::COLL
 type(COLLISIONS_element),optional ::COLL_element
 integer, intent(in)     ,optional ::i_coll_
 !
 ! Work Space
 !
 character(schlen)     :: db_name
 integer               :: V_kind_disk,i_desc,i_coll_loop,io_err,i_coll,SIZE_
 character(lchlen)     :: X_string
 !
 ! Collision element...
 !
 i_coll=0
 if (present(COLL_element)) i_coll=COLL_element%I
 if (present(i_coll_))      i_coll=i_coll_
 !
#if defined _TIMING
 call timing('io_COLLISIONS',OPR='start')
#endif
 !
 if(COLL%kind==H_SE_COH) then
   db_name = "COLLISIONS_COH"
 elseif(COLL%kind==H_SE_HARTREE.or.COLL%kind==H_SE_FOCK.or.COLL%kind==H_SE_SEX.or.COLL%kind==GS_xc_kind.or.&
&       COLL%kind>=H_SE_HARTREE) then
   db_name = "COLLISIONS_HXC"
 elseif(COLL%kind==QP_SE_GW_NEQ) then
   db_name = "COLLISIONS_GW_NEQ"
 elseif(COLL%kind==QP_SE_GW_QED) then
   db_name = "COLLISIONS_P"
 else
   call error("COLLISIONS I/O : Unknown database type ")
 endif
 !
 io_COLLISIONS=io_connect(desc=db_name,type=2,ID=ID)
 if(io_COLLISIONS/=0) goto 1
 !
 if (any(io_sec(ID,:)==1)) then
   !
   io_COLLISIONS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.)
   !
   if (.not.ver_is_gt_or_eq(ID,revision=5453)) then
     io_COLLISIONS=IO_OUTDATED_DB
     goto 1
   endif
   !
   call io_elemental(ID,VAR="COLLISION DESCRIPTION",VAR_SZ=1,CH0="",MENU=0)
   call io_elemental(ID,CH0=COLL%name,VAR=' COLLISIONS name                 :')
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   SIZE_=6
   if (COLL%kind==QP_SE_GW_NEQ) SIZE_=SIZE_+3
   if (COLL%kind==QP_SE_GW_QED) SIZE_=SIZE_+2
   call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=SIZE_,MENU=0)
   !
   call io_elemental(ID,I0=COLL%N,&
&       VAR=' Total number of COLLISIONS      :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=COLL%kind,&
&       VAR=' COLLISIONS kind                 :',CHECK=.true.,OP=(/"=="/))
   !
   call io_elemental(ID,I1=COLL_bands,&
&       VAR=' Bands                           :',CHECK=.true.,OP=(/"==","=="/))
   !
   if (COLL%kind==QP_SE_GW_NEQ) then
     call io_elemental(ID,L0=l_elel_scatt,&
&         VAR=' Electron-Electron scattering    :',WARN=.true.,OP=(/"=="/))
     if(l_elel_scatt) then
      call io_elemental(ID,I0=N_plasma_poles_global,&
&          VAR=' Max N plasma poles (q-resolved) :',CHECK=.true.,OP=(/"=="/))
      call io_elemental(ID,R0=PLASMA_redux_percent,&
&          VAR=' Plasma redux percent            :',CHECK=.true.,OP=(/"=="/))
     endif
   endif
   !
   if (COLL%kind==QP_SE_GW_QED) then
     call io_elemental(ID,L0=l_elphoton_scatt,&
&         VAR=' Electron-Photon   scattering    :',WARN=.true.,OP=(/"=="/))
     call io_elemental(ID,I0=QP_QED_ng,&
&         VAR=' Electron-Photon   RL vectors    :',CHECK=.true.,OP=(/"=="/))
   endif
   !
   call io_elemental(ID,R0=SC_band_mixing,&
&       VAR=' Bands mixing                    :',CHECK=.true.,OP=(/"=="/))
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
&         VAR=' Exchange Cutoff                 :',CHECK=.true.,OP=(/"=="/))
     !
     if (V_kind_disk>=H_SE_HARTREE) then
       call io_elemental(ID,I0=QP_ng_SH,&
&         VAR=' Hartree  RL vectors             :',CHECK=.true.,OP=(/"=="/))
     endif
     if (V_kind_disk==             H_SE_FOCK.or.V_kind_disk==             H_SE_SEX.or.&
&        V_kind_disk==H_SE_HARTREE+H_SE_FOCK.or.V_kind_disk==H_SE_HARTREE+H_SE_SEX) then
       call io_elemental(ID,I0=QP_ng_Sx,&
&         VAR=' Exchange RL vectors             :',CHECK=.true.,OP=(/"=="/))
     endif
   endif
   !
   if (V_kind_disk==QP_SE_GW_NEQ   .or.V_kind_disk==H_SE_COH  .or.V_kind_disk==H_SE_SEX.or.&
&      V_kind_disk==H_SE_HARTREE.or.V_kind_disk==H_SE_FOCK .or.V_kind_disk>=H_SE_HARTREE) then
     !
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     ! Global Response Descriptions
     !==============================
     call io_elemental(ID,VAR="N_X_descriptions",VAR_SZ=1,MENU=0)
     if (n_X_descs> 0) call io_elemental(ID,I0=n_X_descs,CHECK=.true.,OP=(/"=="/))
     if (n_X_descs==0) then
       call io_elemental(ID,DB_I0=n_X_descs,I0=n_X_descs)
       n_X_descs=-n_X_descs
     endif
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     ! In this case I have loaded the em1s DB and I know the parameters.
     ! I can, therefore, write/read and/or VERIFIY the values
     !
     do i_desc=1,iabs(n_X_descs)
       !
       call io_elemental(ID,VAR='X_descriptions_'//trim(intc(i_desc)),CH0="",VAR_SZ=1,MENU=0)
       call io_elemental(ID,DB_CH0=X_string,CH0=X_description(i_desc),VAR='',CHECK=n_X_descs>0,OP=(/"=="/))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       !
     enddo
     !
     call io_elemental(ID,VAR="RIM_PARAMETERS",VAR_SZ=2,MENU=0)
     call io_elemental(ID,I0=RIM_ng,VAR=        ' RIM RL components      :',WARN=.true.)
     call io_elemental(ID,I0=RIM_n_rand_pts,VAR=' RIM random points      :',WARN=.true.)
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
   call io_bulk(ID,VAR='COLLISIONS_STATE',VAR_SZ=(/COLL%N,4/))
   call io_bulk(ID,I2=COLL%state)
   !
   io_COLLISIONS=io_status(ID)
   if (io_COLLISIONS/=0) goto 1
   !
 endif
 !
 ! Check the restart point
 !
 if (IO_mode(ID)==VERIFY.and.allocated(COLL%IO_status)) then
   !
   COLL%IO_status=-1
   do i_coll_loop=1,COLL%N
     if (.not.COLL%PAR_IND%element_1D(i_coll_loop)) cycle
     io_extension(ID)=io_raw_extension(ID)
     call io_fragment(ID,i_fragment=i_coll_loop,ierr=io_err)
     if (io_err==0) then
       COLL%IO_status(i_coll_loop)=1
     else
       COLL%IO_status(i_coll_loop)=-1
       exit
     endif
   enddo
   !
 endif
 !
 if (any(io_sec(ID,:)==2)) then     
   !
   ! QP-based fragmentation
   !
   call io_fragment(ID,i_fragment=i_coll)
   !
   call io_bulk(ID,VAR='N_COLLISIONS_STATES',VAR_SZ=(/1/))
   call io_bulk(ID,I0=COLL_element%N)
   !
 endif
 !
 if (any(io_sec(ID,:)==3)) then     
   !
   ! Fragmentation
   !
   if (.not.any(io_sec(ID,:)==2)) call io_fragment(ID,i_fragment=i_coll)
   !
   call io_bulk(ID,VAR='COLLISIONS_TABLE',&
&                    VAR_SZ=(/COLL%D(1,2)-COLL%D(1,1)+1,&
&                             COLL%D(2,2)-COLL%D(2,1)+1,&
&                             COLL%D(3,2)-COLL%D(3,1)+1/))
   !
   call io_bulk(ID,L3=COLL_element%table(COLL%D(1,1):COLL%D(1,2),&
&                                        COLL%D(2,1):COLL%D(2,2),&
&                                        COLL%D(3,1):COLL%D(3,2)))
   !
   if(COLL%kind==QP_SE_GW_NEQ.or.COLL%kind==QP_SE_GW_QED) then
     ! Real collisions
     call io_bulk(ID,VAR='COLLISIONS_v',VAR_SZ=(/COLL_element%N/))
     call io_bulk(ID,R1=COLL_element%v_r)
   else
     ! Complex collisions
     call io_bulk(ID,VAR='COLLISIONS_v',VAR_SZ=(/2,COLL_element%N/))
     call io_bulk(ID,C1=COLL_element%v_c)
   endif
   !
 endif
 !
1 continue
 !
 call io_disconnect(ID=ID)
 !
#if defined _TIMING
 call timing('io_COLLISIONS',OPR='stop')
#endif
 !
end function io_COLLISIONS
