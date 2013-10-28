!
! Copyright (C) 2000-2013 C. Attaccalite, A. Marini and the YAMBO team 
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
integer function io_COLLISIONS(i_qp,ID,X)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use collisions_IO, ONLY:COLLISIONS_table,n_collisions,V_kind,COLLISIONS_v,COLLISIONS_cutoff,&
&                        COLLISIONS_path,COLLISIONS_range
 use IO_m,          ONLY:io_sec,io_connect,io_header,io_status,io_elemental, &
&                        io_bulk,write_is_on,io_disconnect,io_restart_point,&
&                        ver_is_gt_or_eq,IO_OUTDATED_DB,read_is_on
 use global_XC,     ONLY:SC_HF,global_XC_string,EXT_NONE,SE_NONE,SC_COHSEX
 use com,           ONLY:error
 use SC,            ONLY:SC_bands,SC_X_size,SC_X_bands
 use QP_m,          ONLY:QP_ng_Sx,QP_n_states,SC_band_mixing
 use fragments,     ONLY:io_fragment,Fragments_Restart,Fragments_Synchronize
 use X_m,           ONLY:X_t,n_X_descs,X_description
 use stderr,        ONLY:intc
 use drivers,       ONLY:l_elel_scatt
 !
 implicit none
 !
 integer, intent(inout)::ID
 integer, intent(in)   ::i_qp
 type(X_t),optional    ::X
 !
 ! Work Space
 !
 character(schlen)     :: db_name
 real(SP), allocatable :: COLLISIONS_v_disk(:,:)
 integer               :: V_kind_disk,i_desc,VAR_size
 character(lchlen)     :: V_kind_string,X_string
 character(schlen)     :: desc,db_string
 !
 db_string=" "
 if (len_trim(COLLISIONS_path)>1) db_string="db."
 !
 if(V_kind==SC_HF) then
   db_name     = trim(db_string)//"COLLISIONS_Vbare"
 elseif(V_kind==SC_COHSEX) then
   db_name     = trim(db_string)//"COLLISIONS_Wstatic"
 else
   call error("COLLISIONS I/O : Unknown database type ")
 endif
 !
 if (len_trim(COLLISIONS_path)>1) then
   io_COLLISIONS=io_connect(desc=db_name,subfolder=trim(COLLISIONS_path),type=-1,ID=ID)
 else
   io_COLLISIONS=io_connect(desc=db_name,type=2,ID=ID)
 endif
 if(io_COLLISIONS/=0) goto 1
 !
 if (.not.ver_is_gt_or_eq(ID,revision=891)) then
   io_COLLISIONS=IO_OUTDATED_DB
   goto 1
 endif
 !
 ! Previous Fragments are syncronized (copied in the MORE_IO)
 !
 call Fragments_Synchronize(ID,db_name,i_fragment=-QP_n_states)
 !
 ! Check the restart point
 !
 call Fragments_Restart(ID,fragments_todo=QP_n_states)
 !
 if (any(io_sec(ID,:)==1)) then
   !
   io_COLLISIONS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.)
   !
   VAR_size=4
   if (ver_is_gt_or_eq(ID,revision=1428)) VAR_size=5
   call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=VAR_size,MENU=0)
   !
   call io_elemental(ID,I1=SC_bands,&
&       VAR=' Bands                           :',CHECK=.true.,OP=(/"==","=="/))
   !
   if (ver_is_gt_or_eq(ID,revision=1428)) then
     call io_elemental(ID,L0=l_elel_scatt,&
&         VAR=' Electron-Electron scattering    :',CHECK=.true.,OP=(/"=="/))
   endif
   !
   call io_elemental(ID,R0=SC_band_mixing,&
&       VAR=' Bands mixing                    :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=V_kind,DB_I0=V_kind_disk,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   V_kind_string     =global_XC_string(V_kind,SE_NONE,0,EXT_NONE)
   !
   call io_elemental(ID,VAR='INTERACTION_KIND',CH0="",VAR_SZ=1,MENU=0)
   call io_elemental(ID,CH0=V_kind_string,&
&       VAR=' Interaction Kind                :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   if (V_kind_disk==SC_HF) then
     call io_elemental(ID,VAR="INTERACTION_PARAMETERS",VAR_SZ=4,MENU=0)
     call io_elemental(ID,I0=QP_ng_Sx,&
&         VAR=' Exchange RL vectors             :',CHECK=.true.,OP=(/"=="/))
     call io_elemental(ID,R0=COLLISIONS_cutoff,&
&         VAR=' Exchange Cutoff                 :',CHECK=.true.,OP=(/"=="/))
   else
     !
     if (ver_is_gt_or_eq(ID,revision=955)) then
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
         call io_elemental(ID,VAR='X_descriptions_'//trim(intc(i_desc)),CH0="",VAR_SZ=1,MENU=0)
         call io_elemental(ID,DB_CH0=X_string,CH0=X_description(i_desc),VAR='',CHECK=n_X_descs>0,OP=(/"=="/))
         call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
         !
         ! For the real-time databases I need to extract from the X_descriptions few parameters which describe
         ! the response function properties
         !
         if (read_is_on(ID).and.i_desc<=2) then
           if (i_desc==1) read (X_string(26:40),*) SC_X_size
           if (i_desc==2) read (X_string(26:40),'(i2,1x,i3)') SC_X_bands
         endif
       enddo
       !
     endif
     !
     call io_elemental(ID,VAR="INTERACTION_PARAMETERS",VAR_SZ=2,MENU=0)
     !
   endif
   !
   call io_elemental(ID,I0=RIM_ng,VAR=' RIM RL components               :',WARN=.true.)
   call io_elemental(ID,I0=RIM_n_rand_pts,VAR=' RIM random points               :',WARN=.true.)
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
   io_COLLISIONS=io_status(ID)
   if (io_COLLISIONS/=0) goto 1
   !
 endif
 !
 if (any(io_sec(ID,:)==2)) then     
   !
   ! QP-based fragmentation
   !
   call io_fragment(ID,i_fragment=i_qp)
   !
   call io_bulk(ID,VAR='N_COLLISIONS_STATES',VAR_SZ=(/1/))
   call io_bulk(ID,I0=n_collisions)
   call io_bulk(ID,VAR='COLLISIONS_TABLE',&
&                  VAR_SZ=(/COLLISIONS_range(1,2)-COLLISIONS_range(1,1)+1,&
&                           COLLISIONS_range(2,2)-COLLISIONS_range(2,1)+1,&
&                           COLLISIONS_range(3,2)-COLLISIONS_range(3,1)+1/))
   !
   call io_bulk(ID,L3=COLLISIONS_table(COLLISIONS_range(1,1):COLLISIONS_range(1,2),&
&                                      COLLISIONS_range(2,1):COLLISIONS_range(2,2),&
&                                      COLLISIONS_range(3,1):COLLISIONS_range(3,2)))
   !
   allocate(COLLISIONS_v_disk(n_collisions,2))      
   !
   call io_bulk(ID,VAR='COLLISIONS_v',VAR_SZ=(/n_collisions,2/))
   if(write_is_on(ID)) then
     COLLISIONS_v_disk(:,1) =real(COLLISIONS_v(1:n_collisions))
     COLLISIONS_v_disk(:,2)=aimag(COLLISIONS_v(1:n_collisions))
     call io_bulk(ID,R2=COLLISIONS_v_disk)
   else
     call io_bulk(ID,R2=COLLISIONS_v_disk)
     COLLISIONS_v(1:n_collisions)=CMPLX(COLLISIONS_v_disk(:,1),COLLISIONS_v_disk(:,2))
   endif
   !
   deallocate(COLLISIONS_v_disk)
   !
   ! Fragments are syncronized (copied in the CORE_IO)
   !
   call Fragments_Synchronize(ID,db_name,i_fragment=i_qp)
   !
 endif
 !
1 continue
 !
 if(io_COLLISIONS==0) io_COLLISIONS=io_restart_point(ID)
 !
 call io_disconnect(ID=ID)
 !
end function io_COLLISIONS
