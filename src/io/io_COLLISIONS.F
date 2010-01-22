!
! Copyright (C) 2000-2009 C. Attaccalite, A. Marini and the YAMBO team 
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
integer function io_COLLISIONS(i_qp,ID,QP_COLLISIONS_done)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use collisions_IO, ONLY:COLLISIONS_table,n_collisions,V_kind,COLLISIONS_v,COLLISIONS_cutoff
 use IO_m,          ONLY:io_sec,io_connect,io_header,io_status,io_elemental, &
&                        io_bulk,write_is_on,io_disconnect,IO_write,         &
&                        file_is_present,synchronize_db,io_fragment
 use global_XC,     ONLY:SC_HF,global_XC_string,EXT_NONE,SE_NONE,SC_CHOSEX
 use com,           ONLY:error,more_io_path,core_io_path
 use stderr,        ONLY:intc
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_ng_Sx,QP_n_states,QP_nb,QP_nk,SC_band_mixing
 use par_proc_m,    ONLY:master_cpu,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 !
 implicit none
 !
 integer, intent(inout)         :: ID
 integer, intent(in)            :: i_qp
 character(schlen)              :: desc
 logical, intent(out), optional :: QP_COLLISIONS_done(QP_n_states)
 !
 ! Work Space
 !
 character(schlen)     :: db_name
 real(SP), allocatable :: COLLISIONS_v_disk(:,:)
 integer               :: V_kind_disk,i_qp_db
 character(lchlen)     :: V_kind_string
 logical               :: synchronize_files
 type(pp_indexes)      :: px
 !
 if (present(QP_COLLISIONS_done)) then 
   QP_COLLISIONS_done=.FALSE.
 endif
 !
 if(V_kind==SC_HF) then
   db_name     = "COLLISIONS_Vbare"
 elseif(V_kind==SC_CHOSEX) then
   db_name     = "COLLISIONS_Wstatic"
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
   call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=3,MENU=0)
   call io_elemental(ID,I0=SC_bands,&
&       VAR=' Bands                           :',CHECK=.true.,OP=(/"=="/))
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
     call io_elemental(ID,VAR="INTERACTION_PARAMETERS",VAR_SZ=2,MENU=0)
   endif
   call io_elemental(ID,I0=RIM_ng,&
&       VAR=' RIM RL components               :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=RIM_n_rand_pts,&
&       VAR=' RIM random points               :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
   io_COLLISIONS=io_status(ID)
   if (io_COLLISIONS/=0) goto 1
   !
!   if (present(QP_COLLISIONS_done)) then 
!     do i_qp_db=1,QP_n_states 
!       QP_COLLISIONS_done(i_qp_db) =  file_is_present(desc=trim(db_name)//"_QP_"//trim(intc(i_qp_db)),type=2)
!     enddo
!   endif
!   !
!   ! Distribute databases among all processors
!   !
!   synchronize_files=.false.
!   !
!   if(trim(core_io_path)/=trim(more_io_path)) then
!     !
!     if(present(QP_COLLISIONS_done))  then
!        if(all(QP_COLLISIONS_done(:))) synchronize_files=.true.
!     endif
!     !
!     if(.not.present(QP_COLLISIONS_done).and.write_is_on(ID))    synchronize_files=.true.
!     ! 
!   endif
!   !
!   if(synchronize_files) then
!     !
!     call pp_indexes_reset(px)
!     call par_indexes(px,(/QP_n_states/))
!     !
!     do i_qp_db=1,QP_n_states 
!       if (.not.px%i1p(i_qp_db)) cycle
!       call synchronize_db(desc=trim(db_name)//"_QP_"//trim(intc(i_qp_db)))
!     enddo
!     !
!     call pp_indexes_reset(px)
!     !
!   endif
   !
 endif
 !
 if (.not.any(io_sec(ID,:)==2)) then     
   !
   goto 1
   !
 else 
   !
   ! All cpu write on disk
   !
   if (write_is_on(ID)) IO_write=.true.
   !
   ! QP-based fragmentation
   !
   call io_fragment(ID,i_pointer=i_qp)
   !
   !io_COLLISIONS=io_connect(desc=trim(db_name)//"_QP_"//trim(intc(i_qp)),type=2,ID=ID)
   !
   if(io_COLLISIONS/=0) goto 1
   !
   call io_bulk(ID,VAR='N_COLLISIONS_STATES',VAR_SZ=(/1/))
   call io_bulk(ID,I0=n_collisions)
   call io_bulk(ID,VAR='COLLISIONS_TABLE',VAR_SZ=(/QP_nb,QP_nb,QP_nk/))
   call io_bulk(ID,L3=COLLISIONS_table)
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
   ! restore master-only write premission
   !
   IO_write=master_cpu
   !
!   if (write_is_on(ID)) call synchronize_db(desc=trim(db_name)//"_QP_"//trim(intc(i_qp)))
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function io_COLLISIONS
