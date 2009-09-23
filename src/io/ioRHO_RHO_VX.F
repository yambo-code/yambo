!
! Copyright (C) 2000-2008 C. Attaccalite, A. Marini and the YAMBO team 
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
integer function ioRHO_RHO_VX(i_qp,ID,QP_DB_done)
 !
 use pars,          ONLY:SP,schlen,lchlen
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts
 use oscillators_IO,ONLY:rho_table,n_rho_states,V_kind,rho_VX_rho
 use IO_m,          ONLY:io_sec,io_connect,io_header,io_status,io_elemental, &
&                        io_bulk,write_is_on,io_disconnect,IO_write,file_is_present
 use global_XC,     ONLY:SC_HF,global_XC_string,EXT_NONE,SE_NONE
 use stderr,        ONLY:intc
 use SC,            ONLY:SC_bands
 use QP_m,          ONLY:QP_ng_Sx,QP_n_states
 use par_proc_m,    ONLY:master_cpu
 !
 implicit none
 integer, intent(inout)         :: ID
 integer, intent(in)            :: i_qp
 logical, intent(out), optional :: QP_DB_done(QP_n_states)
 !
 ! Work Space
 !
 character(schlen)     :: db_name
 real(SP), allocatable :: rho_VX_rho_disk(:,:)
 integer               :: V_kind_disk,i_qp_db
 character(lchlen)     :: V_kind_string
 !
 if (present(QP_DB_done)) then 
   QP_DB_done=.FALSE.
 endif
 !
 if (io_sec(ID,1)==1) then
   !
   db_name     = "RHO_RHO_VX"
   ioRHO_RHO_VX=io_connect(desc=db_name,type=2,ID=ID)
   !
   if(ioRHO_RHO_VX/=0) goto 1
   !
   ioRHO_RHO_VX=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.)
   !
   call io_elemental(ID,VAR="PARAMETERS",VAR_SZ=2,MENU=0)
   call io_elemental(ID,I0=SC_bands,&
&       VAR=' Bands                           :',CHECK=.true.,OP=(/"=="/))
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
   call io_elemental(ID,VAR="INTERACTION_PARAMETERS",VAR_SZ=3,MENU=0)
   if (V_kind_disk==SC_HF) then
     call io_elemental(ID,I0=QP_ng_Sx,&
&         VAR=' Exchange RL vectors             :',CHECK=.true.,OP=(/"=="/))
   endif
   call io_elemental(ID,I0=RIM_ng,&
&       VAR=' RIM RL components               :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=RIM_n_rand_pts,&
&       VAR=' RIM random points               :',CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0)
   !
   ioRHO_RHO_VX=io_status(ID)
   if (ioRHO_RHO_VX/=0) goto 1
   !
   if (present(QP_DB_done)) then 
     do i_qp_db=1,QP_n_states 
       QP_DB_done(i_qp_db) =  file_is_present(desc="RHO_RHO_VX_"//trim(intc(i_qp_db)),type=2)
     enddo
   endif
   !
 endif
 !
 if (io_sec(ID,1)/=1) then     
   !
   ! All cpu write on disk
   !
   if(write_is_on(ID)) IO_write=.true.
   !
   db_name     = "RHO_RHO_VX_"//trim(intc(i_qp))
   ioRHO_RHO_VX=io_connect(desc=db_name,type=2,ID=ID)
   !
   if(ioRHO_RHO_VX/=0) goto 1
   !
   call io_bulk(ID,VAR='N_RHO_STATES',VAR_SZ=(/1/))
   call io_bulk(ID,I0=n_rho_states)
   call io_bulk(ID,VAR='RHO_TABLE',VAR_SZ=(/3,n_rho_states/) )
   call io_bulk(ID,I2=rho_table(:,1:n_rho_states))
   !
   allocate(rho_VX_rho_disk(n_rho_states,2))      
   !
   call io_bulk(ID,VAR='RHO_VX_RHO',VAR_SZ=(/n_rho_states,2/))
   if(write_is_on(ID)) then
     rho_VX_rho_disk(:,1) =real(rho_VX_rho(1:n_rho_states))
     rho_VX_rho_disk(:,2)=aimag(rho_VX_rho(1:n_rho_states))
     call io_bulk(ID,R2=rho_VX_rho_disk)
   else
     call io_bulk(ID,R2=rho_VX_rho_disk)
     rho_VX_rho(1:n_rho_states)=CMPLX(rho_VX_rho_disk(:,1),rho_VX_rho_disk(:,2))
   endif
   !
   deallocate(rho_VX_rho_disk)      
   !
   IO_write=master_cpu
   !
 endif
 !
1 call io_disconnect(ID=ID)
 !
end function ioRHO_RHO_VX
